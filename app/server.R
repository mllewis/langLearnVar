library(ggvis)
library(dplyr)

# Set up handles to database tables on app start
d <- read.csv("data/langLearnVar_data_clean.csv")
#d <- read.csv("../data/langLearnVar_data_clean.csv")
# (when running locally)

# filter variables
#"Language" = "stock"                         
#"Language" = "region"                         
#"Language Family" = "lang.family",                
#"Language Genus" = "lang.genus"                    
#"Language Genus" = "native.country"          
#"Language Genus" = "native.country.area"     
#"Language Genus" = "language"

mapTheme = theme(plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_blank(), 
                 axis.title = element_blank(),
                 axis.ticks = element_blank(),
                 axis.text = element_blank())

shinyServer(function(input, output, session) {
  
  # filter out NAs
  d.sub <- reactive({
    d[!is.na(d[,input$xvar])  & !is.na(d[,input$yvar]),]
  })
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Labels for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    d.sub %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_model_predictions(model = "lm", se = TRUE) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.7, fill = ~lang.family, fillOpacity.hover := 0.5, key := ~language) %>%
      add_tooltip(function(df) df$language) %>%
      add_axis("x", title = xvar_name, title_offset = 50) %>%
      add_axis("y", title = yvar_name, title_offset = 50) %>% 
      set_options(width = 500, height = 500) %>%
      hide_legend('fill')
  })
  
  vis %>% bind_shiny("scatterPlot")
  
  # Generate a summary of the dataset
  output$summary <- renderPrint({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    yy = input$yvar
    #m = d[!is.na(d[,input$xvar])  & !is.na(d[,input$yvar]),]
    m = d.sub %>% select(yy)
    summary(m)
   #lmer(input$yvar ~ input$xvar + (input$xvar|lang.family) +
              # (1|native.country), m)
  })
  
  output$mapPlot <- renderPlot({
    d.sub() %>%
      ggplot() +   
        borders("world", colour="gray68", fill="gray68") +
        geom_point(aes(x = lon, y = lat), size = 3) +
        ggtitle("Geographical Distribution of Languages") + 
      mapTheme
  }, height = 275)

  output$n_languages<- renderText({ 
        nrow(d.sub())
    })
})

#shinyapps::deployApp('/Documents/GRADUATE_SCHOOL/Projects/langLearnVar/app/',account = "mlewis", appName="lhnn")
