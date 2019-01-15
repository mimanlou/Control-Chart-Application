options(warn=-1)
options(java.parameters = "-Xmx8g" )
options(tibble.width = Inf)
options(tibble.print_max = 30)

library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(qicharts2)
library(qcc)
library(RDCOMClient)





###check whether input data points are out of control, ooc
ooc <- function(counts,sizes,band){
  p_bar <- sum(counts)/sum(sizes)
  UCL <- p_bar+band*sqrt(p_bar*(1-p_bar))/sqrt(sizes)
  LCL <- p_bar-band*sqrt(p_bar*(1-p_bar))/sqrt(sizes)
  flags <- ifelse(counts>0,counts/sizes > UCL | counts/sizes<LCL,TRUE)
  
  return(flags)
}





###find reasonable calibration date used for comparison base of new data 
###please decision rules in the flow chart of control chart
findCandidate <- function(historical_data,
                          counts,
                          sizes,
                          band=3,
                          interval=20,
                          oocFlag=NULL){
  nsample <- nrow(historical_data) 
  if(nsample<=interval){
    return(1:nsample)
  }
  candidate <- (nsample-interval+1):nsample
  flags <-ooc(historical_data[candidate,counts],
              historical_data[candidate,sizes],3)
  if(sum(flags)>5 | sum(flags)==0){
    return(candidate)
  }
  else{
    start_point <- max((nsample-interval-sum(flags)+1),1)
    candidate <- c(start_point:(nsample-interval),
                   candidate[!flags])
    print(paste0("Second:",candidate))
    flags2<-  ooc(historical_data[candidate,counts],
                  historical_data[candidate,sizes],3)
    if(sum(flags2)==0){
      return(candidate)
    }
    else{
      if(sum(flags)+sum(flags2)<=5){
        start_point <- max((start_point-sum(flags2)),1)
        print(paste0("Third:",candidate[!flags2]))
        candidate <- c(start_point:(candidate[1]-1),
                       candidate[!flags2])
        return(candidate)
      }
      else{
        start_point <- max((nsample-interval-5+1),1)
        print(paste0("Third:",candidate))
        candidate<- start_point:nsample
        return(candidate)
      }
      
    }
  }
}





###plotting the control chart
plotCC <- function(data,
                   station.x,
                   new.data.start,
                   new.data.end,
                   counts,
                   sizes,
                   dates,
                   band=3,
                   interval=20, 
                   plot.type){
  if(!is.null(station.x)){
    old_data <- data %>% filter(station==station.x,
                                datedate<new.data.start)
    new_data <- data %>% filter(station==station.x,datedate>=new.data.start)
    if(!is.null(new.data.end)){
      new_data <- new_data %>% filter(datedate<new.data.end)
    }
  }
  candidate <- findCandidate(old_data,counts,sizes,band,interval)
  skip <- setdiff(candidate[1]:nrow(old_data),candidate)
  
  if(plot.type == "xbar.one"){
    cc_bar.title<-paste0("xbar.one chart for ",
                         counts,
                         " over ",
                         sizes," in ",
                         station,
                         " from ",
                         new.data.start,
                         " till ",
                         new.data.end)
    cc_bar <- qcc(data=old_data[candidate,counts],
                  sizes=pull(old_data[candidate,n]),
                  type="xbar.one",
                  labels=pull(old_data[candidate,dates]),
                  newdata=new_data[,counts],
                  newsizes=pull(new_data[,n]),
                  newlabels=pull(new_data[,dates]), 
                  plot = TRUE, 
                  title = cc_bar.title, 
                  add.stats=TRUE, 
                  ylab="PinSaved",
                  xlab="Date")
    violations <- cc_bar$violations
    return(cc_bar)
  } else if(plot.type == "p_hat"){
    cc.title<-paste0("p_hat chart for ",
                     counts, 
                     " over ",
                     sizes,
                     " in ",
                     station,
                     " from ",
                     new.data.start,
                     " till ",
                     new.data.end)
    cc <- qcc(data=old_data[candidate,counts],
              sizes=pull(old_data[candidate,n]),
              type="p",
              labels=pull(old_data[candidate,dates]),
              newdata=new_data[,counts],
              newsizes=pull(new_data[,n]),
              newlabels=pull(new_data[,dates]), 
              plot = TRUE, 
              title = cc.title, 
              add.stats=TRUE, 
              ylab="Proportion",xlab="Date")
    return(cc)
    violations <- cc$violations
  }
  
  
  candidate_skip <- pull(old_data[skip,dates])
}

### Current Data Address
tables.path <- rstudioapi::getActiveProject()
tables.path <- paste0(tables.path, "/tables")
Daily <- readRDS(paste0(tables.path,"/daily.data.Rds"))
Weekly <- readRDS(paste0(tables.path,"/weekly.data.Rds"))
Monthly <- readRDS(paste0(tables.path,"/monthly.data.Rds"))


counts <- "total.defected"
n <- "total.produced"
dates <- "datedate"
band<- 3
interval <- 20
datasetInput <- Daily
station.x <- "A1"
new.data.start.date <- ymd(20180720)
new.data.end.date<- ymd(20180731)
plot.type <- "p_hat"
cc_results<-plotCC(datasetInput,station.x,new.data.start.date,
                   new.data.end.date,counts,n,dates,band,interval, plot.type)

TW <- c("Daily", "Weekly", "Monthly")
Measurement <- c("defected.ratio", "total.produced", "total.defected")
plotchoices  <-  c("xbar.one", "p_hat")
Stations <- sort(unique(Daily$station))

ui <- fluidPage(
  titlePanel("Control Chart for product Defection"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Station",
                  label = "Choose Station:",
                  choices = Stations, 
                  selected = "A1"),
      tags$hr(),
      selectInput(inputId = "chart.type",
                  label = "Choose a Type:",
                  choices = plotchoices, 
                  selected = "xbar.one"),
      tags$hr(),
      dateRangeInput('dateRange',
                     label = 'Date:',
                     format = "yyyy-mm-dd",
                     startview = "month",
                     start = "2018-07-01",
                     end = "2018-07-15"
      ),
      tags$hr(),
      selectInput(inputId = "Measure", 
                  label = "Measure",
                  choices = Measurement,
                  selected = "PinSavedTotal"),
      tags$hr(),
      selectInput(inputId = "Freq", 
                  label = "Frequency",
                  choices = TW,
                  selected = "Weekly"),
      tags$hr(),
      downloadButton(outputId = "plot.png", 
                     label = "Download Plot")
    ),
    
    mainPanel(
      textOutput("DateRange"),
      plotOutput("Control_Chart")
    )
  )
)

server <- function(input, output){
  Stations <- unique(Daily$station)
  datasetInput <- reactive({
    switch(input$Freq,
           "Daily" = Daily,
           "Weekly" = Weekly,
           "Monthly" = Monthly)
  })
  ccp <- function() {
    plotCC(datasetInput(), input$Station, 
           new.data.start = ymd(input$dateRange[1]), 
           new.data.end = ymd(input$dateRange[2]), counts = input$Measure, 
           sizes = "total.produced", dates = "datedate", 
           band = 3,interval = 20, plot.type = input$chart.type)
  }
  output$Control_Chart <- renderPlot({
    qcp <- plotCC(datasetInput(), input$Station, 
                  new.data.start = ymd(input$dateRange[1]), 
                  new.data.end = ymd(input$dateRange[2]), 
                  counts = input$Measure, sizes = "total.produced", 
                  dates = "datedate", band = 3,interval = 20, 
                  plot.type = input$chart.type)
    
    # date.filter.email.new <- datasetInput() %>% 
    #     select(datedate, station) %>%
    #     filter(datedate >= new.data.start.date & 
    #                datedate <= new.data.end.date)
    # date.filter.email <- 
    #     append(as.character(unique(date.filter.email.new$datedate)), 
    #               as.character(names(qcp$statistics)))
    # out.of.control.dates <- list()
    # rule.violatation.dates <- list()
    
    # if (length(unlist(qcp$violations$beyond.limits)) != 0) {
    #     for (i in 1:length(date.filter.email)) {
    #         for (j in 1:length(unlist(qcp$violations$beyond.limits))) {
    #             if (i == unlist(qcp$violations$beyond.limits)[j]) {
    #                 out.of.control.dates[j] <- date.filter.email[[i]]
    #             }
    #         }
    #         
    #     }
    # }
    # 
    # if (length(unlist(qcp$violations$violating.runs)) != 0) {
    #     for (i in 1:length(date.filter.email)) {
    #         for (j in 1:length(unlist(qcp$violations$violating.runs))) {
    #             if (i == unlist(qcp$violations$violating.runs)[j]) {
    #                 rule.violatation.dates[j] <- date.filter.email[[i]]
    #             }
    #         }
    #         
    #     }
    # }
    # 
    # 
    # OutApp <- COMCreate("Outlook.Application")
    # outMail = OutApp$CreateItem(0)
    # outMail[["To"]]  <-  "mohamad.imanlou@bnsf.com"
    # outMail[["subject"]]  <-  input$Station
    # outMail[["body"]] <- 
    #     as.character(paste("Out of Control: " , 
    #         paste(unlist(out.of.control.dates), collapse = ", "), "     ",
    #         "Rule Violations: ", paste(unlist(rule.violatation.dates), 
    #                                    collapse = ", ")))
    # if (length(unlist(ccp()$violations$beyond.limits))!=0) {
    #     outMail$Send()
    # }
    
    output$plot.png <- downloadHandler(
      filename <-  "plot.png",
      content <-  function(file) {
        png(file, width=612, height=400, type = "windows")
        ccp()
        dev.off()
      })
  })
  
  output$DateRange <- renderText({
    # make sure end date later than start date
    validate(
      need(input$dateRange[2] > input$dateRange[1], 
           "end date is earlier than start date")
    )
    paste("Your date range is", 
          difftime(input$dateRange[2], input$dateRange[1], units="days"), 
          "days")
  })
}

shinyApp(ui = ui, server = server)
