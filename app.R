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

load('model.rda')
data <- load.data() %>% clean.data()
buildings <- read.csv('buildings.csv', header = T)

# constants
vars <- data.frame(
  var = c('term', 'dept', 'level', 'building', 'units', 'crosslistings', 'capacity', 'timestart', 'meetingsnum'), #'timeend',
  margin = c(30, 30, 30, 25, 40
             , 35, 40, 35, 0)
)

# helper functions
choices <- function(vec){
  vec %>% as.character() %>% table() %>% sort(decreasing = T) %>% names() %>% return()
}

get.pred <- function(get.val){
  # response ~ term + (1+term|dept) + level + is.sem + units + crosslistings + scale(capacity) + has.disc + has.lab + poly(time.start, 2) + meetings.num + scale(time.lecture) + (1|building) + campus
  building <- buildings[buildings$name == get.val('building'),]$id
  
  df <- data.frame(
    term = which(names(terms) == get.val('term')),
    dept = get.val('dept'),
    level = get.val('level'),
    is.sem = T,
    units = get.val('units'),
    crosslistings = get.val('crosslistings'),
    capacity = get.val('capacity'),
    has.disc = F,
    has.lab = F,
    time.start = times.to.mins(get.val('timestart')),
    meetings.num = get.val('meetingsnum'),
    time.lecture = get.val('meetingsnum') * (75), #TODO: fix
    building = building,
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
      selectInput(paste0('term', label),
                  label = 'Term',
                  choices = names(terms) %>% rev()
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
                  choices = sapply(choices(data$building), function(id) buildings[buildings$id == id,]$name) %>% unlist() %>% unname()
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
                   value = 18,
                   min = 1,
                   max = max(data$capacity)
      ),
      
      textInput(paste0('timestart', label),
                label='Start time',
                value = '10:05'
      ),
      
      # textInput(paste0('timeend', label),
      #           label='End time',
      #           value = '11:20'
      # ),
      
      numericInput(paste0('meetingsnum', label),
                   label = 'Lectures per week',
                   value = 2,
                   min = 1,
                   max = 5
      )
      
      # checkboxGroupInput(paste0('misc', label), 
      #                    label = 'Miscellaneous', 
      #                    choices = list('has a discussion?', 'has a lab?', 'is a seminar?')
      # )
  ) %>% return()
}

# define UI
ui <- navbarPage(
  shinyjs::useShinyjs(),
  theme = shinytheme('sandstone'),
  title = 'Predicting Duke Course Popularity - Course Comparer',
  
  fluidRow(
    add.form('Course A', 'A'),
    add.form('Course B', 'B'),
    box(width = 4,
      h5(textOutput('probA')),
      h5(textOutput('probB')),
      apply(vars, 1, function(vec){
        column(12, style = paste0('text-align: center; margin-top: 10px; margin-bottom: ', vec['margin'], 'px;'), id = paste0(vec['var'], 'Col'),
               h4(textOutput(paste0(vec['var'], 'Mult')))
        ) %>% return()
      })
    )
  )
) #eccdcd #cddccd

# define server logic
server <- function(input, output) {
  output$probA <- renderText({
    get.val <- function(name) input[[paste0(name, 'A')]]
    paste('Course A probability: ', round(get.pred(get.val), 4))
  })
  
  output$probB <- renderText({
    get.val <- function(name) input[[paste0(name, 'B')]]
    paste('Course B probability: ', round(get.pred(get.val), 4))
  })
  
  apply(vars, 1, function(vec){
    var <- vec['var']
    output[[paste0(var, 'Mult')]] <- renderText({
      get.val.A <- function(name) input[[paste0(name, 'A')]]
      get.val.B <- function(name) if(name == var) input[[paste0(name, 'B')]] else input[[paste0(name, 'A')]]
      
      predA <- get.pred(get.val.A)
      predB <- get.pred(get.val.B)
      
      if(predA < predB){
        return('favors B')
      } else if(predA > predB){
        return('favors A')
      }
      return('-')
    })
  })
  
  apply(vars, 1, function(vec){
    var <- vec['var']
    observeEvent(input[[paste0(var, 'A')]], {
      get.val.A <- function(name) input[[paste0(name, 'A')]]
      get.val.B <- function(name) if(name == var) input[[paste0(name, 'B')]] else input[[paste0(name, 'A')]]
      
      predA <- get.pred(get.val.A)
      predB <- get.pred(get.val.B)

      color <- '#ffffff'
      if(predA < predB){
        color <- '#cddccd'
      } else if(predA > predB){
        color <- '#eccdcd'
      }
      runjs(paste0('$("#', var, 'Col").css("background-color", "', color, '")'))
    })
  })
  
  apply(vars, 1, function(vec){
    var <- vec['var']
    observeEvent(input[[paste0(var, 'B')]], {
      get.val.A <- function(name) input[[paste0(name, 'A')]]
      get.val.B <- function(name) if(name == var) input[[paste0(name, 'B')]] else input[[paste0(name, 'A')]]

      predA <- get.pred(get.val.A)
      predB <- get.pred(get.val.B)

      color <- '#ffffff'
      if(predA < predB){
        color <- '#cddccd'
      } else if(predA > predB){
        color <- '#eccdcd'
      }
      runjs(paste0('$("#', var, 'Col").css("background-color", "', color, '")'))
    })
  })

  # observeEvent(input$termA, {
  #   color <- if(input$termA == input$termB) '#eccdcd' else '#cddccd'
  #   runjs(paste0('$("#', var, 'Col").css("background-color", "', color, '")'))
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

