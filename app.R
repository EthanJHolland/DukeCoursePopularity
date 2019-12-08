library(shiny)
library(shinyjs)
library(shinyTime)
library(shinythemes)
library(shinydashboard)
source('scrape.R')
source('clean.R')

# to deploy
# library(rsconnect)
# deployApp('.')

# data
load('model.rda')
data <- load.data() %>% clean.data()
buildings <- read.csv('buildings.csv', header = T)

# constants
vars <- data.frame(
  var = c('term', 'dept', 'level', 'building', 'units', 'crosslistings', 'capacity', 'timestart', 'timeend', 'meetingsnum', 'issem', 'hasdisc', 'haslab'),
  margin = c(40, 40, 40, 40, 40, 45, 50, 45, 50, 5, 2, 2, 0)
)

# helper variables and functions
last.valid.starttimes <- rep('10:05', 2)
last.valid.endtimes <- rep('11:20', 2)

is.valid.time <- function(time){
  return(
    grepl('^[0-2]?\\d:[0-5]\\d$', time) && 
    abs(times.to.mins(time) - 12*60) < 12*60 # verify 0 < time < mins in a day
  )
}

choices <- function(vec){
  vec %>% as.character() %>% table() %>% sort(decreasing = T) %>% names() %>% return()
}

search <- function(get.val){
  data %>% filter(
    term == which(names(terms) == get.val('term')),
    dept == get.val('dept'),
    number == as.character(get.val('number'))
  ) %>% return()
}

get.pred <- function(get.val, index){
  # response ~ term + (1+term|dept) + level + is.sem + units + crosslistings + scale(capacity) + has.disc + has.lab + poly(time.start, 2) + meetings.num + scale(time.lecture) + (1|building) + campus
  if(is.valid.time(get.val('timestart'))){
    last.valid.starttimes[index] <<- get.val('timestart')
  }
  if(is.valid.time(get.val('timeend'))){
    last.valid.endtimes[index] <<- get.val('timeend')
  }
  
  df <- data.frame(
    term = if(get.val('term') == '2020 Spring') length(terms) + 1 else which(names(terms) == get.val('term')),
    dept = get.val('dept'),
    level = get.val('level'),
    is.sem = get.val('issem'),
    units = get.val('units'),
    crosslistings = get.val('crosslistings'),
    capacity = get.val('capacity'),
    has.disc = get.val('hasdisc'),
    has.lab = get.val('haslab'),
    time.start = times.to.mins(last.valid.starttimes[index]),
    time.end = times.to.mins(last.valid.endtimes[index]),
    meetings.num = get.val('meetingsnum'),
    building = buildings[buildings$name == get.val('building'),]$id

  ) %>% mutate(
    time.lecture = get.val('meetingsnum') * abs(time.end - time.start),
    campus = case_when(
      startsWith(building %>% as.character(), '77') ~ 'west',
      startsWith(building %>% as.character(), '70') ~ 'central',
      startsWith(building %>% as.character(), '71') ~ 'central',
      startsWith(building %>% as.character(), '72') ~ 'east',
      T ~ 'other'
    )
  )
  
  predict(model, df, type='response') %>% return()
}

add.form <- function(title, label){
  #response ~ term + (1+term|dept) + level + is.sem + units + crosslistings + scale(capacity) + has.disc + has.lab + poly(time.start, 2) + meetings.num + scale(time.lecture) + (1|building) + campus
  box(title = title, width = 4,
      # search
      h4('Search'),
      
      selectInput(paste0('term', label, 'Search'),
                  label = 'Term',
                  choices = names(terms) %>% rev()
      ),
      
      selectInput(paste0('dept', label, 'Search'),
                  label = 'Primary Department',
                  choices = choices(data$dept)
      ),
      
      numericInput(paste0('number', label, 'Search'),
                   label = 'Number',
                   value = 101,
                   min = 1,
                   max = 499
      ),
      
      actionButton(paste0('search', label), 'search'),
      
      hr(),
      
      # manual
      h4('Enter Manually'),
      
      selectInput(paste0('term', label),
                  label = 'Term',
                  choices = c(names(terms), '2020 Spring') %>% rev(), # additionally allow for next semester
                  selected = '2019 Fall'
      ),
      
      selectInput(paste0('dept', label),
                  label = 'Primary Department',
                  choices = choices(data$dept)
      ),
      
      selectInput(paste0('level', label),
                  label = 'Level',
                  choices = choices(data$level) %>% sort() %>% as.factor(),
                  selected = as.factor('100')
      ),
      
      selectInput(paste0('building', label),
                  label = 'Location',
                  choices = sapply(choices(data$building), function(id) buildings[buildings$id == id,]$name) %>% unlist() %>% unname(),
                  selected = 'East Duke'
      ),
      
      selectInput(paste0('units', label),
                  label = 'Credits',
                  choices = choices(data$units) %>% sort() %>% as.factor(),
                  selected = as.factor('1')
      ),
      
      numericInput(paste0('crosslistings', label),
                   label = 'Number of crosslistings',
                   value = 0,
                   min = 0,
                   max = data$dept %>% unique() %>% length()
      ),
      
      numericInput(paste0('capacity', label),
                   label = 'Capacity',
                   value = 24,
                   min = 1,
                   max = max(data$capacity)
      ),
      
      textInput(paste0('timestart', label),
                label='Start time',
                value = '10:05'
      ),
      
      textInput(paste0('timeend', label),
                label='End time',
                value = '11:20'
      ),
      
      numericInput(paste0('meetingsnum', label),
                   label = 'Lectures per week',
                   value = 2,
                   min = 1,
                   max = 5
      ),
      
      checkboxInput(paste0('issem', label),
                    label = 'Seminar?',
                    value = F
      ),
      
      checkboxInput(paste0('hasdisc', label),
                    label = 'Has discussion?',
                    value = F
      ),
      
      checkboxInput(paste0('haslab', label),
                    label = 'Has lab?',
                    value = F
      )
  ) %>% return()
}

update.fields <- function(label, input, session){
  get.val <- function(name) input[[paste0(name, label,'Search')]]
  
  res <- search(get.val)
  if(nrow(res) > 0){
    res <- head(res, 1)  # will never be more than one result
    
    for(var in c('dept', 'level', 'units', 'crosslistings', 'capacity', 'meetings.num', 'is.sem', 'has.disc', 'has.lab')){
      updateTextInput(session, paste0(gsub('.', '', var, fixed=T), label), value=res[1, var])
    }
    updateTextInput(session, paste0('term', label), value=get.val('term'))  # get term as text rather than index
    updateTextInput(session, paste0('building', label), value=buildings[buildings$id == res$building,]$name)
    updateTextInput(session, paste0('timestart', label), value=mins.to.time(res$time.start))
    updateTextInput(session, paste0('timeend', label), value=mins.to.time(res$time.start + (res$time.lecture / res$meetings.num)))
  }
}

# define UI
ui <- navbarPage(
  shinyjs::useShinyjs(),
  theme = shinytheme('sandstone'),
  tags$head(tags$style(HTML("hr {border-top: 1px solid #888888;}"))),  # make dividing lines visible
  title = 'Predicting Duke Course Popularity - Course Comparer',
  
  h4('Welcome to the Course Comparer! Search for a past course or manually enter course information to see how changing aspects of a course changes the predicted probability that the course reaches full capacity. Predictions based on a mixed-effects logistic model. Based on data scraped from the beta version of DukeHub 2.0.'),
  hr(),
  fluidRow(
    add.form('Course A', 'A'),
    add.form('Course B', 'B'),
    box(width = 4,
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      h4(textOutput('probA')),
      h4(textOutput('probB')),
      apply(vars, 1, function(vec){
        column(12, style = paste0('text-align: center; margin-bottom: ', vec['margin'], 'px;'), id = paste0(vec['var'], 'Col'),
               h4(textOutput(paste0(vec['var'], 'Mult')))
        ) %>% return()
      })
    )
  )
)

# define server logic
server <- function(input, output, session) {
  # on search, update fields to match search results
  observeEvent(input$searchA, { update.fields('A', input, session) })
  observeEvent(input$searchB, { update.fields('B', input, session) })
  
  # get probabilities for each class
  output$probA <- renderText({
    get.val <- function(name) input[[paste0(name, 'A')]]
    paste('Predicted probability that Course A fills up: ', round(get.pred(get.val, 1), 4))
  })
  
  output$probB <- renderText({
    get.val <- function(name) input[[paste0(name, 'B')]]
    paste('Predicted probability that course B fills up: ', round(get.pred(get.val, 2), 4))
  })
  
  # indicate favors
  apply(vars, 1, function(vec){
    var <- vec['var']
    output[[paste0(var, 'Mult')]] <- renderText({
      get.val.A <- function(name) input[[paste0(name, 'A')]]
      get.val.B <- function(name) if(name == var) input[[paste0(name, 'B')]] else input[[paste0(name, 'A')]]
      
      predA <- get.pred(get.val.A, 1)
      predB <- get.pred(get.val.B, 2)
      
      if(predA < predB){
        return('favors B')
      } else if(predA > predB){
        return('favors A')
      }
      return('-')
    })
  })
  
  # set box color (on A change)
  apply(vars, 1, function(vec){
    var <- vec['var']
    observeEvent(input[[paste0(var, 'A')]], {
      get.val.A <- function(name) input[[paste0(name, 'A')]]
      get.val.B <- function(name) if(name == var) input[[paste0(name, 'B')]] else input[[paste0(name, 'A')]]
      
      predA <- get.pred(get.val.A, 1)
      predB <- get.pred(get.val.B, 2)

      color <- '#ffffff'
      if(predA < predB){
        color <- '#cddccd'
      } else if(predA > predB){
        color <- '#eccdcd'
      }
      runjs(paste0('$("#', var, 'Col").css("background-color", "', color, '")'))
    })
  })
  
  # set box color (on B change)
  apply(vars, 1, function(vec){
    var <- vec['var']
    observeEvent(input[[paste0(var, 'B')]], {
      get.val.A <- function(name) input[[paste0(name, 'A')]]
      get.val.B <- function(name) if(name == var) input[[paste0(name, 'B')]] else input[[paste0(name, 'A')]]

      predA <- get.pred(get.val.A, 1)
      predB <- get.pred(get.val.B, 2)

      color <- '#ffffff'
      if(predA < predB){
        color <- '#cddccd'
      } else if(predA > predB){
        color <- '#eccdcd'
      }
      runjs(paste0('$("#', var, 'Col").css("background-color", "', color, '")'))
    })
  })
}

# launch application 
shinyApp(ui = ui, server = server)
