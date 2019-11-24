#libraries
library(glue)
library(httr)
library(dplyr)


# general scraping funtctions
# make a get request to a given url and parse response body as JSON
make.get <- function(url){
  Sys.sleep(runif(1)*3)  # sleep between requests
  GET(url, accept_json()) %>% 
    content('parsed') %>% 
    return()
}


# methods for getting api endpoints
url.courses <- function(term, dept=''){
  return(glue(
    'https://beta.dukehub.duke.edu/psc/CSPRD01/EMPLOYEE/SA/s/WEBLIB_HCX_CM.H_BROWSE_CLASSES.FieldFormula.IScript_BrowseCourses?institution=DUKEU&term={term}&institution=DUKEU&acad_career=UGRD&subject={dept}'
  ))
}

url.sections <- function(term, courseid){
  return(glue(
    'https://beta.dukehub.duke.edu/psc/CSPRD01/EMPLOYEE/SA/s/WEBLIB_HCX_CM.H_BROWSE_CLASSES.FieldFormula.IScript_BrowseSections?institution=DUKEU&campus=&location=&course_id={courseid}&institution=DUKEU&acad_career=UGRD&term={term}'
  ))
}

url.description <- function(term, section.id){
  return(glue(
    'https://beta.dukehub.duke.edu/psc/CSPRD01/EMPLOYEE/SA/s/WEBLIB_HCX_CM.H_CLASS_SEARCH.FieldFormula.IScript_ClassDetails?institution=DUKEU&term=1700&class_nbr={section.id}'
  ))
}


# static constants
terms <- c(  # based on https://registrar.duke.edu/faculty-staff-resources/dukehub
  '2014 Fall' = 1500,
  '2015 Spring' = 1510,
  '2015 Fall' = 1540,
  '2016 Spring' = 1550,
  '2016 Fall' = 1580,
  '2017 Spring' = 1590,
  '2017 Fall' = 1620,
  '2018 Spring' = 1630,
  '2018 Fall' = 1660,
  '2019 Spring' = 1670,
  '2019 Fall' = 1700#,
  # '2020 Spring' = 1710
)

#get courses from terms
scrape.courses <- function(terms){
  courses <- lapply(terms, function(term){
    res <- make.get(url.courses(term))
    
    courses.notabroad <- res$courses[!sapply(res$courses, function(x) grepl('A', x$catalog_nbr, fixed=T) || x$subject == "REG")]  # ignore abroad classes
    sapply(courses.notabroad, function(x) c('term' = term, 'courseid' = x$crse_id)) %>% t() %>% 
      as.data.frame(stringsAsFactors = F) %>% 
      group_by(courseid) %>% mutate(num.depts = n()) %>% unique() %>% ungroup() %>% 
      return()
  }) %>% bind_rows() %>% return()
}


#get data from courses
scrape.sections <- function(courses){
  apply(courses, 1, function(vec){
    res <- make.get(url.sections(vec['term'], vec['courseid']))
    
    components <- sapply(res$sections, function(section) section$component)
    lecture <- NULL
    for(section in res$sections){  # find lecture/seminar in primary department
      if(section$component %in% c('LEC', 'SEM')) lecture <- section
    }
    if(is.null(lecture)) return(data.frame())
    
    meeting.1 <- if(length(lecture$meetings) >= 1) lecture$meetings[[1]] else NULL
    
    data.frame(
      term = vec['term'],
      courseid = vec['courseid'],
      num.depts = vec['num.depts'],
      sectionid = lecture$class_nbr,
      has.disc = any(components == 'DIS'),
      has.lab = any(components == 'LAB'),
      enrolled = lecture$enrollment_total,
      capacity = lecture$class_capacity,
      component = lecture$component,
      dept = lecture$subject,
      number = lecture$catalog_nbr,
      title = lecture$descr,
      units = lecture$units,
      time.start = if(is.null(meeting.1) || is.null(meeting.1$meeting_time_start)) NA else meeting.1$meeting_time_start,
      time.end = if(is.null(meeting.1) || is.null(meeting.1$meeting_time_end)) NA else meeting.1$meeting_time_end,
      building = if(is.null(meeting.1) || is.null(meeting.1$bldg_cd)) NA else meeting.1$bldg_cd,
      meetings = if(is.null(meeting.1)) NA else sapply(lecture$meetings, function(meeting) meeting$stnd_mtg_pat) %>% paste(collapse=''),
      stringsAsFactors = F
    ) %>% return()
  }) %>% bind_rows() %>% return()
}

scrape.data <- function(){
  scrape.courses(terms) %>% scrape.sections() %>% return()
}

load.data <- function(){
  return(read.csv('data.csv'))
}


