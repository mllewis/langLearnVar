library(ggvis)
library(rworldmap)
library(tidyr)
library(ggplot2)
library(shinythemes)
library(lme4)
library(citr)

shinyUI(fluidPage(
  theme = shinytheme("spacelab"),
  
  titlePanel("Linguistic Niche Browser"),
  p("Explore an aggregation of datasets that test relationships between features of the environment and elements of language.
    This browser is supplemental material to",
    a("Lewis and Frank (2016, CogSci Proceedings).", 
      href = "https://langcog.stanford.edu/papers_new/lewis-2016-underrev.pdf", target = "_blank")),
  br(),
  
  fluidRow(
    column(6,
      wellPanel(
        selectInput("xvar", "Predictor", axis_vars, selected = "pop_log"),
        selectInput("yvar", "Outcome", axis_vars, selected = "complexity.bias"),
        tags$small(paste0(
          "Color on the scatter plot indicates language family.
          Hover over points to identifiy individual languages."
        ))
      ),

      wellPanel(radioButtons("model", "Model fit:",
                                     selected = 1,                   
                                     c("simple linear regression" = 1,
                                       "random intercept of country" = 2,
                                       "random intercept of family" = 3,
                                       "random intercepts of country and family" = 4,
                                       "random intercept and slope of family, intercept of country" = 5),
                                    width = '500px')
      )
      
    ),
    column(6, 
    tabsetPanel(
      tabPanel("Plot",
                 br(),
                 h4(""),
                 div(style = "height: 300px;",                   
                     ggvisOutput("scatterPlot")
                  )
               ),  
      
      tabPanel("Model Summary",
               br(), 
               h4("R Regression model output"),                  
               verbatimTextOutput("summary")
              ),
      
      tabPanel("Data Summary",
               br(), 
               h4("Predictor data source:"),
               htmlOutput("citation_x"),
               h4("Outcome data source:"),
               htmlOutput("citation_y"),
               br(), 
               h4("Number of languages:", textOutput("n_languages", inline = TRUE)),
               h4("Number of families:", textOutput("n_families", inline = TRUE)),
               br(), 
               h4("Geographical distribution of languages:"),                  
               plotOutput("mapPlot")
          )
      #,tabPanel("Data Source",
        #       br(),
       #        h4("Number of langua")
       #   )
        )
      )
    )
  )
)
