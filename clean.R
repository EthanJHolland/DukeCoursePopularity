# libs
library(dplyr)

# internal functions
standardize.meetings <- function(meetings){
  case_when(
    meetings %in% c('MoWeTuThFr', 'TuThMoWeFr', 'MoWeFrTuTh') ~ 'MoTuThWeFr',
    meetings %in% c('MoWeFrTu', 'TuMoWeFr') ~ 'MoTuWeFr',
    meetings %in% c('MoWeTuTh', 'TuThMoWe', 'MoTuThWe') ~ 'MoTuWeTh',
    meetings == 'MoWeFrWe' ~ 'MoWeWeFr',
    meetings == 'TUThTuMo' ~ 'MoTuTuTh',
    meetings %in% c('MoFrTuTh', 'TuThMoFr') ~ 'MoTuThFr',
    meetings %in% c('TuThWe', 'WeTuTh') ~ 'TuThWe',
    meetings %in% c('MoFrWe', 'FrMoWe') ~ 'MoWeFr',
    meetings == 'WeFrTuTh' ~ 'TuThWeFr',
    meetings == 'TuThTu' ~ 'TuTuTh',
    meetings == 'WeFrWe' ~ 'WeWeFr',
    meetings == 'MoWeTu' ~ 'MoTuWe',
    meetings == 'TuThMo' ~ 'MoTuTh',
    meetings == 'WeFrTu' ~ 'TuWeFr',
    meetings == 'MoFrMo' ~ 'MoMoFr',
    meetings == 'MoFrTu' ~ 'MoTuFr',
    meetings == 'FrTu' ~ 'TuFr',
    meetings == 'ThTu' ~ 'TuTh',
    meetings == 'ThMo' ~ 'MoTh',
    meetings == 'ThWe' ~ 'WeTh',
    meetings == 'TBA' || is.na(meetings) ~ NA_character_,
    T ~ as.character(meetings)
  ) %>% return()
}

times.to.mins <- function(times){
  split <- strsplit(as.character(times), ':', fixed=T)
  if(length(split[[1]]) < 2) return(0) 
  sapply(split, function(x) as.numeric(x[1])*60 + as.numeric(x[2])) %>% return()
}

mins.to.time <- function(mins){
  paste0(mins %/% 60, ':', if(mins %% 60 < 10) '0' else '', mins %% 60) %>% return()
}

remove.missing <- function(data) data[complete.cases(data),]

#external functions
clean.data <- function(data.raw){
  data.raw  %>% 
    mutate(
      term = match(term, terms),  # convert term to continuous
      crosslistings = num.depts - 1,
      is.sem = grepl('S', number, fixed=T),
      is.pratt = dept %in% c('EGR', 'ME', 'BME', 'ECE', 'CEE', 'ENRGYEGR'),
      number = gsub('-.*|[A-Z]+', '', number),  # 89-1 => 89, 412S => 412
      units = if_else(units %in% c('0.25 - 1', '0.5 - 1'), NA_character_, as.character(units)) %>% 
        as.factor() %>% relevel('1'),
      level = case_when(
        nchar(number) == 2 ~ '<100',
        startsWith(number, '1') ~ '100',
        startsWith(number, '2') ~ '200',
        startsWith(number, '3') ~ '300',
        startsWith(number, '4') ~ '400'
      ) %>% as.factor() %>% relevel('100'),
      building.char = if_else(building %in% c('', 'OTHERS', 'SEE INSTR'), NA_character_, as.character(building)),
      building = building.char %>% as.factor() %>% relevel('7710'),
      campus = case_when(  # based on https://wiki.duke.edu/download/attachments/75171635/Campus%20Map.pdf?version=1&modificationDate=1470765220000&api=v2
        startsWith(building.char, '77') ~ 'west',
        startsWith(building.char, '70') ~ 'central',
        startsWith(building.char, '71') ~ 'central',
        startsWith(building.char, '72') ~ 'east',
        T ~ 'other'
      ) %>% as.factor() %>% relevel('west'),
      meetings = standardize.meetings(meetings) %>% as.factor() %>% relevel('TuTh'),
      meetings.num = nchar(as.character(meetings)) / 2,
      time.start = times.to.mins(time.start),
      time.end = times.to.mins(time.end),
      time.lecture = (time.end - time.start) * meetings.num,
      full.prop = pmin(enrolled / capacity, 1)
    ) %>% 
    select(
      full.prop, term, crosslistings, has.disc, has.lab, capacity, dept, level, units, time.start,
      time.lecture, building, campus, meetings, meetings.num, is.sem, is.pratt, number
    ) %>% remove.missing() %>% return()
}
