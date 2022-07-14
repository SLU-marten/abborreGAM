library(shiny)

source("func.R")

shinyServer(function(input, output){
  mod <- reactiveValues(
    ne = NULL,
    nl = NULL,
    se = NULL,
    sl = NULL
    )
  
  observeEvent(input$modellknapp,{
    mod$ne <- gamFunc("data_N_e", input$variabler)
    mod$nl <- gamFunc("data_N_l", input$variabler)
    mod$se <- gamFunc("data_S_e", input$variabler)
    mod$sl <- gamFunc("data_S_l", input$variabler)
  })
  
  output$ne_summary <- renderPrint({
    req(input$modellknapp)
    summary(mod$ne)
  })
  output$nl_summary <- renderPrint({
    summary(mod$nl)
  })
  output$se_summary <- renderPrint({
    summary(mod$se)
  })
  output$sl_summary <- renderPrint({
    summary(mod$sl)
  })
})
