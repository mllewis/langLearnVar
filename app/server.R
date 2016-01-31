library(ggvis)
library(dplyr)

# Set up handles to database tables on app start
d <- read.csv("../data/langLearnVar_data_clean.csv")

# filter variables
#"Language" = "stock"                         
#"Language" = "region"                         
#"Language Family" = "lang.family",                
#"Language Genus" = "lang.genus"                    
#"Language Genus" = "native.country"          
#"Language Genus" = "native.country.area"     
#"Language Genus" = "language",    


shinyServer(function(input, output, session) {


  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))

    # get rid of missing values
    d.sub <- d[!is.na(d[,input$xvar])  & !is.na(d[,input$yvar]),]
    
    d.sub %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.7, fill = ~lang.family, fillOpacity.hover := 0.5, key := ~language) %>%
      add_tooltip(function(df) df$language) %>%
      add_axis("x", title = xvar_name, title_offset = 50) %>%
      add_axis("y", title = yvar_name, title_offset = 50) %>% 
      set_options(width = 500, height = 500) %>%
      layer_model_predictions(model = "lm", se = TRUE) %>%
      hide_legend('fill')
  })
  
  
  vis %>% bind_shiny("scatterPlot")
  
  output$mapPlot <- renderPlot({
    d[!is.na(d[,input$xvar])  & !is.na(d[,input$yvar]),] %>%
    ggplot() +   
      borders("world", colour="gray50", fill="gray50") +
      geom_point(aes(x =lon, y=lat), size=3) +
      ggtitle("Geographical Distribution of Languages") + 
      mapTheme
  }, height = 275)

  output$n_languages<- renderText({ 
    d[!is.na(d[,input$xvar])  & !is.na(d[,input$yvar]),]%>%
        nrow()
    })
})
