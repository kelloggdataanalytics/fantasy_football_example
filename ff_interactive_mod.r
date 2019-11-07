

source("C:/Users/micha/Dropbox/Files/Projects/nfl_data.R")

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
library(plotly)
library(ggthemes)
library(stargazer)

sandbox.UI <- function(id) {
  
  ns <- NS(id)
  
  tabPanel("Data Sandbox",
           #customHeader(title = "Data 'Sandbox' Manipulator"),
           column(12,
                  sidebarLayout(
                    sidebarPanel(h3("Data Selection - Rushing"),
                                 selectInput(ns('playstat'),
                                                label = 'Select statistic',
                                                choices=c("total_yards","touchdown","fumble","first_downs","mean_ydstogo","mean_ScoreDiff","mean_qtr","mean_down","yards_per_carry","max_run","med_run",'first_downs_share',"avg_v_median_yards_carry"),
                                             selected="total_yards"
                                 ),
                                 radioButtons(ns("radio"),
                                              label = "Select Data Transformation", 
                                              choices = c("Levels" = "levels", "Log Levels" = "log","Z-Score"="z", "Percent Change (QoQ)" = "qoq", "Percent Change (YoY)" = "yoy","Annualized Percent Change"="annual"),
                                              selected = "levels"),
                                 
                                 selectInput(ns("name"),
                                             label = "Select data:",
                                             choices = c(unique(rush_plyr_game.df$Rusher)[-c(1,2)]),
                                             selected = c("S.Barkley"),
                                             multiple = TRUE
                                 ),
                                 
                                 downloadButton(ns("RusherData"), label = "Download Data")
                                 
                    ),
                    
                    
                    
                    #Main panel for displaying outputs ----
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot", plotlyOutput(ns("plot"))),
                                  tabPanel("Summary", tableOutput(ns("summary"))),
                                  tabPanel("Correlations", tableOutput(ns("corr"))),
                                  tabPanel("Table", tableOutput(ns("table"))))
                    )
                    
                  ) #,div(style = "width:5000px;height:10px")#style='padding:100px;'
           ),
           
           #Receiving
           
           column(12,
                  sidebarLayout(
                    sidebarPanel(h3("Data Selection - Receiving"),
                                 selectInput(ns('playstat2'),
                                             label = 'Select statistic',
                                             choices=c("total_yards","touchdown","fumble","receptions","yac","mean_ScoreDiff","mean_qtr","mean_down","pass_attempts","long_pass","short_pass","first_downs","mean_ydstogo","max_rec","med_rec","catch_rate","yards_per_catch","first_downs_share","short_pass_share","long_pass_share","avg_v_median_yards_rec"),
                                             selected="total_yards"
                                 ),
                                 radioButtons(ns("radio2"),
                                              label = "Select Data Transformation", 
                                              choices = c("Levels" = "levels", "Log Levels" = "log","Z-Score"="z", "Percent Change (QoQ)" = "qoq", "Percent Change (YoY)" = "yoy","Annualized Percent Change"="annual"),
                                              selected = "levels"),
                                 
                                 selectInput(ns("name2"),
                                             label = "Select data:",
                                             choices = c(unique(rec_plyr_game.df$Receiver)[-c(1,2)]),
                                             selected = c("T.Hill"),
                                             multiple = TRUE
                                 ),
                                 
                                 downloadButton(ns("ReceiverData"), label = "Download Data")
                                 
                    ),
                    
                    
                    
                    #Main panel for displaying outputs ----
                    mainPanel(
                      tabsetPanel(type = "tabs",
                                  tabPanel("Plot", plotlyOutput(ns("plot2"))),
                                  tabPanel("Summary", tableOutput(ns("summary2"))),
                                  tabPanel("Correlations", tableOutput(ns("corr2"))),
                                  tabPanel("Table", tableOutput(ns("table2"))))
                    )
                    
                  ) #,div(style = "width:5000px;height:10px")#style='padding:100px;'
           )
           
           
  )
}
           
           


sandbox.server <- function(input, output, session,data,data2){
             
             module_data=data
             
             data_input <- reactive({
               if (input$radio=="levels"){
                 module_data.levels=module_data[,c("Rusher","GameID",input$playstat)]
                 df <- melt(module_data.levels,id.vars=c('GameID',"Rusher"))
                 df <- df[df$Rusher %in% input$name,]
                 units="Levels"
               }
               
               if (input$radio=="log"){
                 module_data.log=module_data[,input$playstat]
                 for (i in 2:ncol(module_data.log)){
                   module_data.log[,i]=log(module_data.log[,i])
                 }
                 df <- melt(module_data.log,id.vars='GameID')
                 df <- df[df$variable %in% input$name,]
                 units="Log Level"
               }
               
               if (input$radio=="z"){
                 module_data.z=module_data[,input$playstat]
                 for (i in 2:ncol(module_data.z)){
                   module_data.z[,i]=(module_data.z[,i]-mean(module_data.z[,i],na.rm=T))/sd(module_data.z[,i],na.rm=T)
                 }
                 df <- melt(module_data.z,id.vars='GameID')
                 df <- df[df$variable %in% input$name,]
                 units="Standard Deviation from the Mean"
               }
               
               if (input$radio=="qoq"){
                 module_data.qoq=module_data
                 for (i in 2:ncol(module_data.qoq)){
                   module_data.qoq[,i]=as.numeric(as.vector(Delt(module_data.qoq[,i],k=1)))*100
                 }
                 module_data.qoq=module_data.qoq[module_data.qoq$GameID>=input$dateRange[1] & module_data.qoq$GameID<=input$dateRange[2],]
                 df <- melt(module_data.qoq,id.vars='GameID')
                 df <- df[df$variable %in% input$name,]
                 units="Percent"
               }
               
               if (input$radio=="yoy"){
                 module_data.yoy=module_data
                 for (i in 2:ncol(module_data.yoy)){
                   module_data.yoy[,i]=as.numeric(as.vector(Delt(module_data.yoy[,i],k=4)))*100
                 }
                 module_data.yoy=module_data.yoy[module_data.yoy$GameID>=input$dateRange[1] & module_data.yoy$GameID<=input$dateRange[2],]
                 df <- melt(module_data.yoy,id.vars='GameID')
                 df <- df[df$variable %in% input$name,]
                 units="Percent"
               }
               
               if (input$radio=="annual"){
                 module_data.annual=module_data
                 for (i in 2:ncol(module_data.annual)){
                   module_data.annual[,i]=as.numeric(as.vector(Delt(module_data.annual[,i],k=1)))*400
                 }
                 module_data.annual=module_data.annual[module_data.annual$GameID>=input$dateRange[1] & module_data.annual$GameID<=input$dateRange[2],]
                 df <- melt(module_data.annual,id.vars='GameID')
                 df <- df[df$variable %in% input$name,]
                 units="Percent"
               }
               
               
               df
               
             })
             
             
             output$plot <- renderPlotly({
               
               # plot <-   ggplot(data_input()) +              
               #   geom_line(aes(x = GameID, y = value, colour = Rusher)) + scale_colour_discrete(name = NULL) + labs(x = NULL, y = input$radio, title = "Rusher Data",cap=c(paste0("Data last updated on ",format(Sys.Date(),"%b %d, %Y"))))   + theme(legend.position = "bottom", legend.margin = margin(t = -.1, unit='cm')) + theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank()) + theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"))
               # 
               # print(plot)
               
              plot_ly(data_input(), x = ~GameID,y= ~value, color = ~Rusher, type = 'scatter', mode = 'lines') %>%
                 layout(title = "Rusher Statistics",
                        xaxis = list(title = "Game"),
                        yaxis = list (title = "Units"))
               
            
               #,text = paste('Value:', value,'<br>Date: ', as.Date(date,format='%b-%Y'),  '<br>Variable: ', variable)
               
               #print(df.ggplot)
               
               
             }#,height=400,width=1000
             )
             
             output$summary <- renderTable({
               
               df.sum=data_input()
               df.sum=na.omit(df.sum)
               
               summary=data.frame(series=numeric(length(input$name)),l1=numeric(length(input$name)),l2=numeric(length(input$name)),l3=numeric(length(input$name)),year=numeric(length(input$name)),five=numeric(length(input$name)),whole=numeric(length(input$name)),total=numeric(length(input$name)),min=numeric(length(input$name)),max=numeric(length(input$name)),sd=numeric(length(input$name)),twentyfive=numeric(length(input$name)),seventyfive=numeric(length(input$name)))
               
               for (i in 1:length(input$name)){
                 summary[i,'series']=input$name[i]
                 summary[i,'l1']=subset(df.sum,Rusher==input$name[i])[nrow(subset(df.sum,Rusher==input$name[i])),"value"]
                 summary[i,'l2']=subset(df.sum,Rusher==input$name[i])[nrow(subset(df.sum,Rusher==input$name[i]))-1,"value"]
                 summary[i,'l3']=subset(df.sum,Rusher==input$name[i])[nrow(subset(df.sum,Rusher==input$name[i]))-2,"value"]
                 summary[i,'year']=mean(tail(subset(df.sum,Rusher==input$name[i]),4)[,"value"],na.rm=T)
                 summary[i,'five']=mean(tail(subset(df.sum,Rusher==input$name[i]),8)[,"value"],na.rm=T)
                 summary[i,'whole']=mean(subset(df.sum,Rusher==input$name[i])[,"value"],na.rm=T)
                 summary[i,'total']=sum(subset(df.sum,Rusher==input$name[i])[,"value"],na.rm=T)
                 summary[i,'min']=min(subset(df.sum,Rusher==input$name[i])[,"value"],na.rm=T)
                 summary[i,'max']=max(subset(df.sum,Rusher==input$name[i])[,"value"],na.rm=T)
                 summary[i,'sd']=sd(subset(df.sum,Rusher==input$name[i])[,"value"],na.rm=T)
                 summary[i,'twentyfive']=quantile(subset(df.sum,Rusher==input$name[i])[,"value"],seq(0,1,0.25))[2]
                 summary[i,'seventyfive']=quantile(subset(df.sum,Rusher==input$name[i])[,"value"],seq(0,1,0.25))[4]
               }
               colnames(summary)=c("Series",as.character(as.Date(df.sum[nrow(df.sum),"GameID"])),as.character(as.Date(df.sum[nrow(df.sum)-1,"GameID"])),as.character(as.Date(df.sum[nrow(df.sum)-2,"GameID"])),"4-Game Avg.","8-Game Avg.",'Season Avg.','Season Total',"Min","Max","Stdev.","25th Percentile","75th Percentile")
               #colnames(summary)[1:4]=c("Series",as.Date(df.sum[nrow(df.sum),"date"]),df.sum[nrow(df.sum)-1,"date"],df.sum[nrow(df.sum)-2,"date"])
               #colnames(summary)[5:ncol(summary)]=c("Three Month Avg","Six Month Avg","One Year Avg",'Series Avg.',"Min","Max","Stdev.","25th Percentile","75th Percentile")
               summary
               
             },caption="Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"))
             
             output$corr <- renderTable({
               
               df.corr=data_input()
               df.corr=dcast(df.corr,date~variable,value.var="value",mean)
               df.corr=cor(na.omit(as.matrix(df.corr[,-1])))
               rownames(df.corr)=colnames(df.corr)
               df.corr
               
             },caption="Correlation Matrix",caption.placement = getOption("xtable.caption.placement", "top"),rownames = TRUE)
             
             output$table <- renderTable({
               df1=data_input()
               df1$GameID=as.character(df1$GameID)
               df1=dcast(df1,date~variable,value.var='value',mean)
               df1
             })
             
             output$QuarterlyData <- downloadHandler(
               filename = function() {
                 paste('stats', 'csv', sep='.')
               },
               content = function(file) {
                 
                 write.csv(data_input(), file, row.names = FALSE)
               }
             )
             
             ##############
             #SECOND CHART#
             ##############
             
             module_data2=data2
             
             data_input2 <- reactive({
               if (input$radio2=="levels"){
                 module_data2.levels=module_data2[,c("Receiver","GameID",input$playstat2)]
                 df <- melt(module_data2.levels,id.vars=c('GameID',"Receiver"))
                 df <- df[df$Receiver %in% input$name2,]
                 units="Levels"
               }
               
               if (input$radio2=="log"){
                 module_data2.log=module_data2[,input$playstat2]
                 for (i in 2:ncol(module_data2.log)){
                   module_data2.log[,i]=log(module_data2.log[,i])
                 }
                 df <- melt(module_data2.log,id.vars='GameID')
                 df <- df[df$variable %in% input$name2,]
                 units="Log Level"
               }
               
               if (input$radio2=="z"){
                 module_data2.z=module_data2[,input$playstat2]
                 for (i in 2:ncol(module_data2.z)){
                   module_data2.z[,i]=(module_data2.z[,i]-mean(module_data2.z[,i],na.rm=T))/sd(module_data2.z[,i],na.rm=T)
                 }
                 df <- melt(module_data2.z,id.vars='GameID')
                 df <- df[df$variable %in% input$name2,]
                 units="Standard Deviation from the Mean"
               }
               
               if (input$radio2=="qoq"){
                 module_data2.qoq=module_data2
                 for (i in 2:ncol(module_data2.qoq)){
                   module_data2.qoq[,i]=as.numeric(as.vector(Delt(module_data2.qoq[,i],k=1)))*100
                 }
                 module_data2.qoq=module_data2.qoq[module_data2.qoq$GameID>=input$dateRange[1] & module_data2.qoq$GameID<=input$dateRange[2],]
                 df <- melt(module_data2.qoq,id.vars='GameID')
                 df <- df[df$variable %in% input$name2,]
                 units="Percent"
               }
               
               if (input$radio2=="yoy"){
                 module_data2.yoy=module_data2
                 for (i in 2:ncol(module_data2.yoy)){
                   module_data2.yoy[,i]=as.numeric(as.vector(Delt(module_data2.yoy[,i],k=4)))*100
                 }
                 module_data2.yoy=module_data2.yoy[module_data2.yoy$GameID>=input$dateRange[1] & module_data2.yoy$GameID<=input$dateRange[2],]
                 df <- melt(module_data2.yoy,id.vars='GameID')
                 df <- df[df$variable %in% input$name2,]
                 units="Percent"
               }
               
               if (input$radio2=="annual"){
                 module_data2.annual=module_data2
                 for (i in 2:ncol(module_data2.annual)){
                   module_data2.annual[,i]=as.numeric(as.vector(Delt(module_data2.annual[,i],k=1)))*400
                 }
                 module_data2.annual=module_data2.annual[module_data2.annual$GameID>=input$dateRange[1] & module_data2.annual$GameID<=input$dateRange[2],]
                 df <- melt(module_data2.annual,id.vars='GameID')
                 df <- df[df$variable %in% input$name2,]
                 units="Percent"
               }
               
               
               df
               
             })
             
             
             output$plot2 <- renderPlotly({
               # 
               # plot <-   ggplot(data_input2()) +              
               #   geom_line(aes(x = GameID, y = value, colour = Receiver)) + scale_colour_discrete(name = NULL) + labs(x = NULL, y = input$radio2, title = "Receiver Data",cap=c(paste0("Data last updated on ",format(Sys.Date(),"%b %d, %Y"))))   + theme(legend.position = "bottom", legend.margin = margin(t = -.1, unit='cm')) + theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank()) + theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"))
               # 
               # print(plot)
               #,text = paste('Value:', value,'<br>Date: ', as.Date(date,format='%b-%Y'),  '<br>Variable: ', variable)
               
               #print(df.ggplot)
               
               plot_ly(data_input2(), x = ~GameID,y= ~value, color = ~Receiver, type = 'scatter', mode = 'lines') %>%
                 layout(title = "Receiver Statistics",
                        xaxis = list(title = "Game"),
                        yaxis = list (title = "Units"))
               
             }#,height=400,width=1000
             )
             
             output$summary2 <- renderTable({
               
               df.sum=data_input2()
               df.sum=na.omit(df.sum)
               
               summary=data.frame(series=numeric(length(input$name2)),l1=numeric(length(input$name2)),l2=numeric(length(input$name2)),l3=numeric(length(input$name2)),year=numeric(length(input$name2)),five=numeric(length(input$name2)),whole=numeric(length(input$name2)),total=numeric(length(input$name2)),min=numeric(length(input$name2)),max=numeric(length(input$name2)),sd=numeric(length(input$name2)),twentyfive=numeric(length(input$name2)),seventyfive=numeric(length(input$name2)))
               
               for (i in 1:length(input$name2)){
                 summary[i,'series']=input$name2[i]
                 summary[i,'l1']=subset(df.sum,Receiver==input$name2[i])[nrow(subset(df.sum,Receiver==input$name2[i])),"value"]
                 summary[i,'l2']=subset(df.sum,Receiver==input$name2[i])[nrow(subset(df.sum,Receiver==input$name2[i]))-1,"value"]
                 summary[i,'l3']=subset(df.sum,Receiver==input$name2[i])[nrow(subset(df.sum,Receiver==input$name2[i]))-2,"value"]
                 summary[i,'year']=mean(tail(subset(df.sum,Receiver==input$name2[i]),4)[,"value"],na.rm=T)
                 summary[i,'five']=mean(tail(subset(df.sum,Receiver==input$name2[i]),8)[,"value"],na.rm=T)
                 summary[i,'whole']=mean(subset(df.sum,Receiver==input$name2[i])[,"value"],na.rm=T)
                 summary[i,'total']=sum(subset(df.sum,Receiver==input$name2[i])[,"value"],na.rm=T)
                 summary[i,'min']=min(subset(df.sum,Receiver==input$name2[i])[,"value"],na.rm=T)
                 summary[i,'max']=max(subset(df.sum,Receiver==input$name2[i])[,"value"],na.rm=T)
                 summary[i,'sd']=sd(subset(df.sum,Receiver==input$name2[i])[,"value"],na.rm=T)
                 summary[i,'twentyfive']=quantile(subset(df.sum,Receiver==input$name2[i])[,"value"],seq(0,1,0.25))[2]
                 summary[i,'seventyfive']=quantile(subset(df.sum,Receiver==input$name2[i])[,"value"],seq(0,1,0.25))[4]
               }
               colnames(summary)=c("Series",as.character(as.Date(df.sum[nrow(df.sum),"GameID"])),as.character(as.Date(df.sum[nrow(df.sum)-1,"GameID"])),as.character(as.Date(df.sum[nrow(df.sum)-2,"GameID"])),"4-Game Avg.","8-Game Avg.",'Season Avg.','Season Total',"Min","Max","Stdev.","25th Percentile","75th Percentile")
               #colnames(summary)[1:4]=c("Series",as.Date(df.sum[nrow(df.sum),"date"]),df.sum[nrow(df.sum)-1,"date"],df.sum[nrow(df.sum)-2,"date"])
               #colnames(summary)[5:ncol(summary)]=c("Three Month Avg","Six Month Avg","One Year Avg",'Series Avg.',"Min","Max","Stdev.","25th Percentile","75th Percentile")
               summary
               
             },caption="Summary Statistics",caption.placement = getOption("xtable.caption.placement", "top"))
             
             output$corr2 <- renderTable({
               
               df.corr=data_input2()
               df.corr=dcast(df.corr,date~variable,value.var="value",mean)
               df.corr=cor(na.omit(as.matrix(df.corr[,-1])))
               rownames(df.corr)=colnames(df.corr)
               df.corr
               
             },caption="Correlation Matrix",caption.placement = getOption("xtable.caption.placement", "top"),rownames = TRUE)
             
             output$table2 <- renderTable({
               df1=data_input2()
               df1$GameID=as.character(df1$GameID)
               df1=dcast(df1,date~variable,value.var='value',mean)
               df1
             })
             
             output$ReceiverData <- downloadHandler(
               filename = function() {
                 paste('stats', 'csv', sep='.')
               },
               content = function(file) {
                 
                 write.csv(data_input2(), file, row.names = FALSE)
               }
             )
             
}