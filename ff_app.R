library(shiny)
library(ggplot2)
library(reshape2)
library(plyr)
library(zoo)
library(quantmod)
library(rmarkdown)
library(TTR)
library(gridExtra)
library(grid)
library(DT)
library(kableExtra)
library(tables)
library(knitr)
library(rsconnect)
library(shinythemes)
library(shinydashboard)

#setwd("C:/Users/micha/Dropbox/Files/Projects/")

source("ff_interactive_mod.r", local = TRUE) 

ui <- shinyUI(
  fluidPage(theme=shinytheme('cosmo'),
      
            
            navbarPage("Fantasy Football",
                       navbarMenu("Research",
                                  sandbox.UI(id="sandbox")
                       )
            )
  )
)


server <- function(input, output, session){
  
  callModule(sandbox.server,id="sandbox",data=rush_plyr_game.df,data2=rec_plyr_game.df)
  
}

shinyApp(ui = ui, server = server)
