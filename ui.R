library(shiny)
library(usdm)
library(mgcv)
library(fitdistrplus)
library(raster)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(rnaturalearth)
library(dplyr)
library(ROCR)
library(GGally)
library(RColorBrewer)
library(shinycssloaders)

shinyUI(fluidPage(

    # Application title
    titlePanel("Abborre GAM"),

    sidebarLayout(
        sidebarPanel(
        checkboxGroupInput(
          "variabler", 
          "Variabler",
          names(rst)[-c(3,6)]
          ),
        actionButton(
          "modellknapp",
          "Kör modell"
        )
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Norr 1978-2004", 
              tabsetPanel(
                tabPanel("Summary",
                         withSpinner(verbatimTextOutput("ne_summary"))
                ),
                tabPanel("Validering")
              )
            ),
            tabPanel(
              "Norr 2005-2019", 
              tabsetPanel(
                tabPanel("Summary",
                         withSpinner(verbatimTextOutput("nl_summary"))
                ),
                tabPanel("Validering")
              )
            ),
            tabPanel(
              "Syd 1978-2004", 
              tabsetPanel(
                tabPanel("Summary",
                         withSpinner(verbatimTextOutput("se_summary"))
                ),
                tabPanel("Validering")
              )
            ),
            tabPanel(
              "Syd 2005-2019", 
              tabsetPanel(
                tabPanel("Summary",
                         withSpinner(verbatimTextOutput("sl_summary"))
                ),
                tabPanel("Validering")
              )
            )
          )
        )
    )
))
