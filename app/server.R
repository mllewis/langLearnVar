library(ggvis)
library(dplyr)
#library(shinyapps)
library(broom)
library(R.utils)


# Set up handles to database tables on app start
d <- read.csv("data/langLearnVar_data_clean.csv")
citations <- read.csv("data/bibliography.csv")

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
  
  # model
  mod <- reactive({
    if (input$model == 1) { # simple linear regression
      lm(as.formula(paste(input$yvar," ~ ",input$xvar)), data = d.sub())
      
    } else if (input$model == 2) { # random intercept of country
      lmer(as.formula(paste(input$yvar,  "~" , input$xvar,  "+ (1|native.country)")), 
           d.sub(), control=lmerControl(optimizer = "bobyqa"))
      
    } else if (input$model == 3) { # random intercept of family
      lmer(as.formula(paste(input$yvar,  "~" , input$xvar,  "+ (1|lang.family)")), 
           d.sub(), control=lmerControl(optimizer = "bobyqa"))
      
    } else if (input$model == 4) { # both random interecepts
      lmer(as.formula(paste(input$yvar,  "~" , input$xvar,  "+ (1|lang.family) + (1|native.country)")), 
           d.sub(), control=lmerControl(optimizer = "bobyqa"))
      
    } else if (input$model == 5) {  # random intercept and slope of family, intercept of country
      lmer(as.formula(paste(input$yvar,  "~" , input$xvar,  "+(", input$xvar, "|lang.family) +
                                (1|native.country)")), d.sub(), control=lmerControl(optimizer = "bobyqa"))
      
    } # full model doesn't have enough levels
  })
  
  # this is necessary to create the line fit on the plot because ggvis doesn't have an abline equivalent
  data_line <- reactive({
    xmin = min(d.sub()[,input$xvar])
    xmax = max(d.sub()[,input$xvar])
    
    line.params = tidy(mod())%>%
      filter(term == input$xvar | term == "(Intercept)") %>%
      select(term, estimate) %>%
      spread(term, estimate) %>%
      rename_(m = input$xvar) %>%
      rename(b = `(Intercept)`)
    
    m = line.params$m 
    b = line.params$b
             
    data.frame(
      x_rng = c(xmin, xmax), 
      y_rng = c(m*xmin + b, m*xmax + b))     
  }) 
  
  # function for generating tooltip text
  language_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$language)) return(NULL)
    
    # pick out the language with this ID
    all_d <- isolate(d.sub())
    lang <- all_d[all_d$language == x$language, ]
    lang = lang %>% 
            filter(!is.na(language)) %>%
           slice(1)

    paste0("<b>", capitalize(lang$language), "</b><br>",
           lang$lang.family, "<br>",
           lang$native.country)
  }
  
  # scatter plot
  vis <- reactive({
    # axes labels
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    d.sub() %>%
      ggvis(x = xvar, y = yvar) %>%
      #layer_model_predictions(model = "lm", se = TRUE) %>%
      layer_points(size := 50, size.hover := 200,
        fillOpacity := 0.7, fill = ~lang.family, 
        fillOpacity.hover := 0.5, key := ~language) %>%
      layer_paths(x = ~x_rng, y = ~y_rng, stroke := "black",strokeWidth := 5, data = data_line) %>%
      #add_tooltip(function(df) df$language) %>%
      add_tooltip(language_tooltip, "hover") %>%
      add_axis("x", title = xvar_name, title_offset = 50) %>%
      add_axis("y", title = yvar_name, title_offset = 50) %>% 
      set_options(width = 500, height = 500) %>%
      hide_legend('fill')
  })
  vis %>% bind_shiny("scatterPlot")
  
  # summary of model
  output$summary <- renderPrint({ 
    summary(mod())
  }, width = "600")

  # number of languages
  output$n_languages<- renderText({ 
    nrow(d.sub())
  })
  
  # number of families
  output$n_families<- renderText({ 
    m = droplevels(d.sub()$lang.family)
    length(summary(m))
  }) 
  
  # citation_x
  output$citation_x<- renderUI({ 
    m = filter(citations, var_name == input$xvar) %>%
      select(long_source) 
    HTML(as.character(m[1,1]))
  })
  
  # citation_y
  output$citation_y<- renderUI({ 
   m = filter(citations, var_name == input$yvar) %>%
     select(long_source) 
   HTML(as.character(m[1,1]))
  })
  
  # link
  output$citation_y<- renderUI({ 
    m = filter(citations, var_name == input$yvar) %>%
      select(long_source) 
    HTML(as.character(m[1,1]))
  })
  
  # notes
  output$citation_y<- renderUI({ 
    m = filter(citations, var_name == input$yvar) %>%
      select(long_source) 
    HTML(as.character(m[1,1]))
  })
  
  
  # map plot
  output$mapPlot <- renderPlot({
    d.sub() %>%
      ggplot() +   
      borders("world", colour="gray68", fill="gray68") +
      geom_point(aes(x = lon, y = lat, color = lang.family), size = 3) +
      mapTheme +
      theme(legend.position="none") 
     #theme(legend.position="bottom", legend.title=element_blank())
  }, height = 275)
  
})



#shinyapps::deployApp('/Documents/GRADUATE_SCHOOL/Projects/langLearnVar/app/',account = "mlewis", appName="lhnn")
