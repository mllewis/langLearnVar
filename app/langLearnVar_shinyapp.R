SHINY APP
```{r, eval = F}
lmeasure <- "lexical.diversity"
pmeasure <- "log.RatioL2"
subdata <- filter(d.scatter.fam2,
                  lang_measure == "lmeasure", pop_measure == "pmeasure")
```

```{r, eval = F}
shinyApp(
  
  ui = fluidPage(
    selectInput("lmeasure", "Language Measure:", 
                choices = as.character(unique(d.scatter.fam2$lang_measure)),
                selected = "lexical.diversity"),
    selectInput("pmeasure", "Population Measure:", 
                choices = as.character(unique(d.scatter.fam2$pop_measure)),
                selected = "log.RatioL2"),
    plotOutput("scatterPlot"), 
    verbatimTextOutput("lmer")
  ),
  
  server = function(input, output) {
    output$scatterPlot <- renderPlot({
      qplot(pop_value, lang_value, 
            data = filter(d.scatter.fam2, lang_measure == input$lmeasure & 
                            pop_measure == input$pmeasure)) + 
        geom_smooth(method="lm") + 
        theme_bw()
    })
    
    output$lmer <- textOutput({
      summary(lmer(lang_value ~ pop_value + (pop_value | langFamily), 
                   data = filter(d.scatter.fam2, 
                                 lang_measure == input$lmeasure & 
                                   pop_measure == input$pmeasure)))
    })
  },
  
  options = list(height = 500)
)
```