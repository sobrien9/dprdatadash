# description -------------------------------------------------------------
# 11/10/17 10:12 AM - still need to write a script to pull the EIA 914 production series ###
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(lubridate)
library(DBI)
library(rsconnect)

# dbconnect ---------------------------------------------------------------

#Connect to DB
setwd('/srv/shiny-server')
dw <- config::get("datawarehouse")

con <- DBI::dbConnect(odbc::odbc(),
                      Driver = dw$driver,
                      Server = dw$server,
                      UID    = dw$uid,
                      PWD    = dw$pwd,
                      Port   = dw$port,
                      Database = dw$database
)
setwd('/srv/shiny-server/Shiny-DPRTesting')


# load, modify data ---------------------------------------------------------------


TotalRigs <- dbReadTable(con, "RigCountUSBasins")

#Import rigs and production csvs, reformat dates to match
TotalRigs$Report_Period <- ymd(TotalRigs$Report_Period)
prod <- dbReadTable(con, "PET.MCRFPUS2.M")
prod$X1 <- paste0(prod$X1, "01")
prod$Report_Period <- ymd(prod$X1)
prod$X2 <- as.numeric(prod$X2)
prod$USProd <- prod$X2 / 1000
prod <- select(prod, Report_Period, USProd)
prod <- arrange(prod, desc(Report_Period))
TotalRigs <- arrange(TotalRigs, desc(Report_Period))

#combine prod and rigs data frames
df <- merge(prod, TotalRigs, by="Report_Period", all=TRUE)
df <- df[ which(df$Report_Period > '2010-12-31'), ]
df <- arrange(df, desc(Report_Period))
df$TotalUS <- as.numeric(df$TotalUS)

#Pull and arrange DPR data with rig count data
DPR_Prod <- dbReadTable(con, "DPR_Prod")
TotalRigs <- dbReadTable(con, "RigCountUSBasins")
TotalRigs$Report_Period <- ymd(TotalRigs$Report_Period)
DPR_Prod$Report_Period <- ymd(DPR_Prod$Report_Period)
DPR_Prod <- DPR_Prod %>% filter(Product == "Oil") %>% select(-Product) %>% arrange(desc(Report_Period), Basin)

basin_list <- c("Anadarko", "Appalachia", "Bakken", "EagleFord", "Haynesville", "Niobrara", "Permian")
DPR_RIGS <- TotalRigs
DPR_Prod_temp <- DPR_Prod

for(i in 1:length(basin_list)){
  DPR_Prod_temp2 <- DPR_Prod_temp %>% filter(Basin == basin_list[i]) %>% select(Report_Period, Perrig, Legacychg, Total)
  colnames(DPR_Prod_temp2) <- c("Report_Period", paste0(basin_list[i], "Perrig"), paste0(basin_list[i], "Legacychg"), paste0(basin_list[i], "Total"))
  
  DPR_RIGS <- merge(DPR_RIGS, DPR_Prod_temp2, by=c("Report_Period"), all=TRUE)
}
DPR_RIGS <- DPR_RIGS %>% arrange(desc(Report_Period))


# shiny server ------------------------------------------------------------

shinyServer(function(input,output){
  
  output$histogram <- renderPlot({
    hist(faithful$eruptions,breaks = input$bins)
  })
  
  #Total US rigs plot
  # output$plot <- renderPlotly({
  #   plot_ly(TotalRigs, x = ~Report_Period, y = ~TotalUS, type = 'scatter', mode = 'lines') 
  # })
  #total US prod and total rigs plot
  output$plot <- renderPlotly({
    plot_ly(df) %>%
      add_trace(x = ~Report_Period, y = ~USProd, type = 'bar', name = 'Production',
                marker = list(color = '#C9EFF9'),
                hoverinfo = "text",
                text = ~paste(Report_Period, " - ", USProd, 'mill b/d')) %>%
      add_trace(x = ~Report_Period, y = ~TotalUS, type = 'scatter', mode = 'lines', name = 'Rig Count', yaxis = 'y2',
                line = list(shape = "linear", color = '#45171D'),
                hoverinfo = "text",
                text = ~paste(TotalUS, 'Rigs'),
                connectgaps = TRUE) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(side = 'left', title = 'Production in millions of bbl/d', showgrid = FALSE, zeroline = FALSE),
        yaxis2 = list(side = 'right', overlaying = "y", title = 'Rig count', showgrid = FALSE, zeroline = FALSE))
  })
  
  #Permian prod and total rigs plot
  output$permianoil <- renderPlotly({
    plot_ly(DPR_RIGS) %>%
      add_trace(x = ~Report_Period, y = ~PermianTotal, type = 'bar', name = 'Production',
                marker = list(color = '#C9EFF9'),
                hoverinfo = "text",
                text = ~paste(Report_Period, " - ", PermianTotal, 'bbl/d')) %>%
      add_trace(x = ~Report_Period, y = ~Permian, type = 'scatter', mode = 'lines', name = 'Rig Count', yaxis = 'y2',
                line = list(shape = "linear", color = '#45171D'),
                hoverinfo = "text",
                text = ~paste(Permian, 'Rigs'),
                connectgaps = TRUE) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(side = 'left', title = 'Production in bbl/d', showgrid = FALSE, zeroline = FALSE),
        yaxis2 = list(side = 'right', overlaying = "y", title = 'Rig count', showgrid = FALSE, zeroline = FALSE))
  })
  
  #Anadarko prod and total rigs plot
  output$Anadarkooil <- renderPlotly({
    plot_ly(DPR_RIGS) %>%
      add_trace(x = ~Report_Period, y = ~AnadarkoTotal, type = 'bar', name = 'Production',
                marker = list(color = '#C9EFF9'),
                hoverinfo = "text",
                text = ~paste(Report_Period, " - ", AnadarkoTotal, 'bbl/d')) %>%
      # add_trace(x = ~Report_Period, y = ~Anadarko, type = 'scatter', mode = 'lines', name = 'Rig Count', yaxis = 'y2',
      #           line = list(shape = "linear", color = '#45171D'),
      #           hoverinfo = "text",
      #           text = ~paste(Anadarko, 'Rigs'),
      #           connectgaps = TRUE) %>%
      # This section is commented out because the DB rigs table doesnt have an entry for Anadarko. Will need to check BHI
      layout(
        xaxis = list(title = ""),
        yaxis = list(side = 'left', title = 'Production in bbl/d', showgrid = FALSE, zeroline = FALSE),
        yaxis2 = list(side = 'right', overlaying = "y", title = 'Rig count', showgrid = FALSE, zeroline = FALSE))
  })
  
  #Bakken prod and total rigs plot
  output$Bakkenoil <- renderPlotly({
    plot_ly(DPR_RIGS) %>%
      add_trace(x = ~Report_Period, y = ~BakkenTotal, type = 'bar', name = 'Production',
                marker = list(color = '#C9EFF9'),
                hoverinfo = "text",
                text = ~paste(Report_Period, " - ", BakkenTotal, 'bbl/d')) %>%
      add_trace(x = ~Report_Period, y = ~Bakken, type = 'scatter', mode = 'lines', name = 'Rig Count', yaxis = 'y2',
                line = list(shape = "linear", color = '#45171D'),
                hoverinfo = "text",
                text = ~paste(Bakken, 'Rigs'),
                connectgaps = TRUE) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(side = 'left', title = 'Production in bbl/d', showgrid = FALSE, zeroline = FALSE),
        yaxis2 = list(side = 'right', overlaying = "y", title = 'Rig count', showgrid = FALSE, zeroline = FALSE))
  })
  
  #Product select checkboxes
  output$productselect <- renderUI({
    
  })
  
  #Rig count week over week
  output$WeeklyChangeBox <- renderInfoBox({
    infoBox(
      "Week over week", -10, icon = icon("dashboard"), #will need to modify this with logic to change values
      color = "purple"
    )})
  
  #date range filter
  output$dateRangeText  <- renderText({
    paste("input$dateRange is", 
          paste(as.character(input$dateRange), collapse = " to ")
    )
  })
  
})


