library(ggvis)
library(rworldmap)

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

shinyUI(fluidPage(
  titlePanel("Environmental and Language Variables"),
  fluidRow(
    column(6,
      wellPanel(
        selectInput("xvar", "X-axis variable", axis_vars, selected = "pop_log"),
        selectInput("yvar", "Y-axis variable", axis_vars, selected = "complexity.bias"),
        tags$small(paste0(
          "Color on the scatter plot indicates language family. Line shows simple linear regression."
        ))
      ),
      wellPanel(
        span("Number of languages:",
             textOutput("n_languages")
        )
      ),
      wellPanel(
        span("Model Fits",
             textOutput("xx_languages")
        )
      ),
      plotOutput("mapPlot")
    ),
    column(6, 
      ggvisOutput("scatterPlot")
    )
  )
))
