

library(tidyverse)
library(tibble)
library(purrr) 
library(lubridate)

df <- tibble(
  a = c(1,-2,3),
  b = c(4,-5,4),
  c = c(10,9,8)
)

map_dbl(df, sum)

d1 <- "January 1, 2010"

mdy(d1)

modify(df, abs)

library(shiny)

# counterButton <- function(id, label = "Counter") {
#   ns <- NS(id)
#   tagList(
#     actionButton(ns("button"), label = label),
#     verbatimTextOutput(ns("out"))
#   )
# }

# counter <- function(input, output, session) {
#   count <- reactiveVal(0)
#   observeEvent(input$button, {
#     count(count() + 1)
#   })
#   output$out <- renderText({
#     count()
#   })
#   count
# }

ui <- fluidPage(
fileInput('file_id', label = 'file_label')
)

server <- function(input, output, session) {
  observe(
    list(
      print(str(input$file_id))
    )
  )
}

shinyApp(ui, server)
