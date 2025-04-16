### SERVER ###

# This file determines the mappings between the inputs from the UI file and the output characteristics.

library(dplyr)
library(rdrop2)

# Define server logic required to the visualization plots
shinyServer(function(input, output, session) {
  
  ####################################################
  # Controls forward and back button actions
  value <- reactiveVal(0)
  activate <- reactiveVal(0)
  plotInMain <- reactiveVal(plot.dict$ID[plot.dict$Name == viz.default])
  condition <- reactiveVal(condition.default)
  plotHR <- reactiveValues()
  plotSEC <- reactiveValues()
  plotOX <- reactiveValues()
  plotBP <- reactiveValues()
  plotTEMP <- reactiveValues()
  plotRESP <- reactiveValues()
  plotLABS <- reactiveValues()
  plotMEDS <- reactiveValues()
  plotDEMO <- reactiveValues()
  plotLABS_Badge <- reactiveValues()
  plotMEDS_Badge <- reactiveValues()
  plotBP_Badge <- reactiveValues()
  plotDEMO_Badge <- reactiveValues()
  buttonLabs <- ggplot() + xlim(limits=c(0,1)) + ylim(limits=c(0,2)) +
    geom_rect(aes(xmin=0, xmax=1, ymin=0,ymax=2),color="black",fill = "white") +
    geom_text(aes(x=0.5, y=1), label="Labs", size = 5, angle = 270, fontface = "bold") +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank())
  buttonMeds <- ggplot() + xlim(limits=c(0,1)) + ylim(limits=c(0,2)) +
    geom_rect(aes(xmin=0, xmax=1, ymin=0,ymax=2),color="black",fill = "white") +
    geom_text(aes(x=0.5, y=1), label="Meds", size = 5, angle = 270, fontface = "bold") +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank())
  
  record_clicks <- function(timewindow = input$timeframe) {
    pID <- get_participantID(input$participantID)
    if (pID %in% ord$participantID & pID != "default") {
      app.dat <- read.csv(dat.file)
      app.dat <- app.dat[,c("participantID", "caseID", "conditionID", "timestamp", "plotID", "windowStart", "windowEnd")]
      app.dat[nrow(app.dat)+1,] <- c(pID, isolate(value()), isolate(condition()), Sys.time(), plot.dict$Name[plot.dict$ID == isolate(plotInMain())], timewindow[1], timewindow[2])
      write.csv(app.dat, dat.file)
    }
  }
  
  load_plots <- function(x) {
    if(isolate(activate()) == 0) {
      time.window <- c(-4,0)
    } else {
      time.window <- input$timeframe
    }
    
    plots <- make_graphs(isolate(value()), isolate(condition()), time.window)
    
    # Set Middle Plots
    output$viz_hr <- renderPlot({
      plots[[1]] # HR
    })
    plotHR$list <- plots[[1]] # HR
    if (isolate(plotInMain()) == plot.dict$ID[plot.dict$Name == "SpO2"]) {
      output$viz <- renderPlot({
        plots[[2]] # SpO2
      })
      plotSEC$list <- plots[[2]] # SpO2
    } else if (isolate(plotInMain()) == plot.dict$ID[plot.dict$Name == "BP"]) {
      output$viz <- renderPlot({
        plots[[9]][[1]] # BP1
      })
      plotSEC$list <- plots[[9]][[1]] # BP1
    } else if (isolate(plotInMain()) == plot.dict$ID[plot.dict$Name == "Temp"]) {
      output$viz <- renderPlot({
        plots[[4]] # Temp
      })
      plotSEC$list <- plots[[4]] # Temp
    } else if (isolate(plotInMain()) == plot.dict$ID[plot.dict$Name == "Resp"]) {
      output$viz <- renderPlot({
        plots[[3]] # Resp
      })
      plotSEC$list <- plots[[3]] # Resp
    } else if (isolate(plotInMain()) == plot.dict$ID[plot.dict$Name == "Labs"]) {
      output$viz <- renderPlot({
        plots[[5]][[1]] # Labs
      })
      plotSEC$list <- plots[[5]][[1]] # Labs
    } else if (isolate(plotInMain()) == plot.dict$ID[plot.dict$Name == "Meds"]) {
      output$viz <- renderPlot({
        plots[[6]][[1]] # Meds
      })
      plotSEC$list <- plots[[6]][[1]] # Meds
    } else if (isolate(plotInMain()) == plot.dict$ID[plot.dict$Name == "Info"]) {
      output$viz <- renderPlot({
        plots[[11]][[1]] # Demographics
      })
      plotSEC$list <- plots[[11]][[1]] # Demographics
    }
    
    # Set Current Case ID and Time
    vitals_sub <- plotHR$list$data
    timeframe <- c(-24,0)
    xmax <- max(vitals_sub$Time) + 0*60*60 
    xmin <- as.POSIXct(xmax) + (0-24)*60*60
    output$currentCase <- renderText({
      get_case(isolate(value()), isolate(condition()))
    })
    
    # Render All Badges
    output$viz_hrbadge <- renderPlot({
      plots[[10]] + # HR Badge
        scale_y_continuous(limits = c(0, 170), breaks = seq(50,150,50), expand = expand_scale(mult = c(0, 0))) +
        scale_x_datetime(limits = c(xmin, xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin))),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0))) +
        geom_rect(xmin=xmax, xmax = xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin)), ymin = 0, ymax = 170, fill = "white", color = "black") + 
        geom_text(x= xmax+ ((1.16666666667*(xmax-xmin)) - (xmax-xmin))/2, y = 170/2, label="HR", size = 5, angle = 270, fontface = "bold") + ylab(NULL)
    })
    output$viz_ox <- renderPlot({
      plots[[2]] + scale_y_continuous(limits = c(39.6, 102), breaks = seq(60,100,20), expand = expand_scale(mult = c(0, 0))) +
        scale_x_datetime(limits = c(xmin, xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin))),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0))) +
        geom_rect(xmin=xmax, xmax = xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin)), ymin = 39.6, ymax = 102, fill = "white", color = "black") + 
        geom_text(x= xmax+ ((1.16666666667*(xmax-xmin)) - (xmax-xmin))/2, y = (102+39)/2, label="SpO2", size = 5, angle = 270) + ylab(NULL)
    })
    plotOX$list <- plots[[2]]
    output$viz_bp <- renderPlot({
      plots[[9]][[2]] + scale_y_continuous(limits = c(0,250), breaks = seq(60,250,60), expand = expand_scale(mult = c(0,0))) + 
        scale_x_datetime(limits = c(xmin,xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin))),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0))) +
        geom_rect(xmin=xmax, xmax = xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin)), ymin = 0, ymax = 250, fill = "white", color="black") + 
        geom_text(x= xmax+ ((1.16666666667*(xmax-xmin)) - (xmax-xmin))/2, y = (250)/2, label="BP", size = 5, angle = 270) + ylab(NULL)
    })
    plotBP$list <- plots[[9]][[1]]
    plotBP_Badge$list <- plots[[9]][[2]]
    # output$viz_temp <- renderPlot({
    #   plots[[4]] + scale_y_continuous(limits = c(80,110), breaks = seq(80,110, 10), minor_breaks = seq(80,110,5), expand = expand_scale(mult = c(0,0))) + 
    #     scale_x_datetime(limits = c(xmin,xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin))),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0))) +
    #     geom_rect(xmin=xmax, xmax = xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin)), ymin = 80, ymax = 110, fill = "white", color = "black") + 
    #     geom_text(x= xmax+ ((1.16666666667*(xmax-xmin)) - (xmax-xmin))/2, y = (110+80)/2, label="Expand", size = 5, angle=90)
    # })
    #  plotTEMP$list <- plots[[4]]
    output$viz_labs <- renderPlot({
      if (length(plots[[5]][[1]]$data) == 0) {
        print("here")
        grid.arrange(plots[[5]][[2]] + theme(axis.title.y = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()), buttonLabs + theme(plot.margin =unit(c(5.5,5.5,5.5,-7.5), "pt")), ncol = 2,widths = c(1,.14))
      } else {
        grid.arrange(plots[[5]][[2]] + theme(axis.text.y = element_blank()), buttonLabs + theme(plot.margin =unit(c(5.5,5.5,5.5,-7.5), "pt")), ncol = 2,widths = c(1,.16))
      }
    })
    plotLABS$list <- plots[[5]][[1]]
    plotLABS_Badge$list <- plots[[5]][[2]]
    output$viz_meds <- renderPlot({
      grid.arrange(plots[[6]][[2]] + 
                     scale_y_continuous(breaks = c(0.5), labels = c("100"), expand = expand_scale(mult = c(0, 0))) +
                     theme(axis.text = element_text(size = 14),
                           axis.ticks.x = element_blank(), 
                           axis.text.x = element_blank(), 
                           axis.title.x = element_blank(),
                           axis.ticks.y = element_line(colour = "white"),
                           axis.text.y = element_text(colour = "white"),
                           strip.text.y = element_blank()) + 
                     scale_x_datetime(limits = c(xmin, xmax), labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0))), buttonMeds + theme(plot.margin =unit(c(5.5,5.5,5.5,-7.5), "pt")), ncol = 2,widths = c(1,.16))
    })
    plotMEDS$list <- plots[[6]][[1]]
    plotMEDS_Badge$list <- plots[[6]][[2]]
    output$viz_concern <- renderPlot({
      plots[[8]]
    })
    output$viz_resp <- renderPlot({
      plots[[3]] + scale_y_continuous(limits = c(0, 65), breaks = seq(0,65,20), labels = paste0("  ",seq(0,65,20)), minor_breaks = seq(0,65,5),  expand = expand_scale(mult = c(0, 0))) + 
        scale_x_datetime(limits = c(xmin, xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin))),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0))) +
        geom_rect(xmin=xmax, xmax = xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin)), ymin = 0, ymax = 65, fill = "white", color = "black") + 
        geom_text(x= xmax+ ((1.16666666667*(xmax-xmin)) - (xmax-xmin))/2, y = (65)/2, label="Resp", size = 5, angle = 270) + ylab(NULL)
    })
    plotRESP$list <- plots[[3]]
    output$viz_demo <- renderPlot({
      plots[[11]][[2]] + scale_y_continuous(limits = c(-1,1), expand = expand_scale(mult = c(0, 0))) + 
        scale_x_continuous(limits = c(-1, 1.33333333333), expand = expand_scale(mult = c(0, 0))) +
        geom_rect(xmin = 1, xmax = 1.33333333333, ymin = -1, ymax = 1, fill = "white", color = "black") + 
        geom_text(x = 1.16666666667, y = 0, label = "Info", size = 5, angle = 270, fontface = "bold") + ylab(NULL)
    })
    plotDEMO$list <- plots[[11]][[1]]
    plotDEMO_Badge$list <- plots[[11]][[2]]
  }
  
  # Render Vitals
  renderVitals <- function(x) {
    vitals_sub <- plotHR$list$data
    timeframe <- input$timeframe
    xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60 
    xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
    output$viz_hr <- renderPlot({
      plotHR$list + scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
    })
    if (plot.dict$Name[plot.dict$ID == isolate(plotInMain())] %in% c("SpO2","BP","Temp","Meds")) {
      output$viz <- renderPlot({
        plotSEC$list + scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
      })
    } else if (plot.dict$Name[plot.dict$ID == isolate(plotInMain())] %in% c("Resp")) {
      output$viz <- renderPlot({
        plotSEC$list + scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0))) +
          theme(plot.margin=unit(c(5.5,13.5,5.5,13.5), "pt"))
      })
    }
  }
  
  # BACK Button
  observeEvent(input$backButton, {
    newValue <- match(paste0(value(), condition()), ord[ord$participantID == get_participantID(input$participantID), 2:ncol(ord)]) - 1
    if (is.na(newValue) | newValue < 1) {
      newValue <- 1
    }
    expID <- ord[ord$participantID == get_participantID(input$participantID), 2:ncol(ord)][newValue]
    value(get_caseID(expID))
    condition(get_conditionID(expID))
    plotInMain(plot.dict$ID[plot.dict$Name == viz.default])
    # Updates the slider value and displays the plot if the timeframe did not change
    if(isolate(input$timeframe[1]) == -4 & isolate(input$timeframe[2]) == 0) {
      load_plots(1)
    } else {
      activate(0)
      updateSliderInput(session, "timeframe", value = c(-4,0))
    }
    valueList$df <- data.frame()
    record_clicks(c(-4,0))
  })
  
  # NEXT Button
  observeEvent(input$nextButton, {
    newValue <- match(paste0(value(), condition()), ord[ord$participantID == get_participantID(input$participantID), 2:ncol(ord)]) + 1
    if (is.na(newValue) | newValue > nrow(ids)) {
      newValue <- 1
    }
    expID <- ord[ord$participantID == get_participantID(input$participantID), 2:ncol(ord)][newValue]
    value(get_caseID(expID))
    condition(get_conditionID(expID))
    plotInMain(plot.dict$ID[plot.dict$Name == viz.default])
    # Updates the slider value and displays the plot if the timeframe did not change
    if(isolate(input$timeframe[1]) == -4 & isolate(input$timeframe[2]) == 0) {
      load_plots(1)
    } else {
      activate(0)
      updateSliderInput(session, "timeframe", value = c(-4,0))
    }
    valueList$df <- data.frame()
    record_clicks(c(-4,0))
  })
  
  # Record Reactive Values
  valueList <- reactiveValues()
  valueList$df <- data.frame()
  allValue <- reactiveValues()
  allValue$df <- data.frame()
  
  ####################################################
  
  # Tester output from select input labeled 'inPatient'
  output$outPatient <- renderPrint({
    input$inPatient
  }) 
  
  # Observe Badge Clicks
  observeEvent(input$ox_plot,{
    case <- as.numeric(isolate(value()))
    id <- ids[which(ids$case == case), 2]
    vitals_sub <- vitals_and_alarms[which(vitals_and_alarms$Identity == id),]
    timeframe <- input$timeframe
    plotInMain(plot.dict$ID[plot.dict$Name == "SpO2"])
    record_clicks()
    xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60 
    xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
    output$viz <- renderPlot({
      plotOX$list + scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
    })
    plotSEC$list <- plotOX$list
  })
  observeEvent(input$bp_plot,{
    case <- as.numeric(isolate(value()))
    id <- ids[which(ids$case == case), 2]
    vitals_sub <- vitals_and_alarms[which(vitals_and_alarms$Identity == id),]
    timeframe <- isolate(input$timeframe)
    plotInMain(plot.dict$ID[plot.dict$Name == "BP"])
    record_clicks()
    xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60
    xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
    output$viz <- renderPlot({
      plotBP$list + scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
    })
    plotSEC$list <- plotBP$list
  })  
  observeEvent(input$temp_plot,{
    case <- as.numeric(isolate(value()))
    id <- ids[which(ids$case == case), 2]
    vitals_sub <- vitals_and_alarms[which(vitals_and_alarms$Identity == id),]
    timeframe <- isolate(input$timeframe)
    xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60
    xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
    plotInMain(plot.dict$ID[plot.dict$Name == "Temp"])
    record_clicks()
    output$viz <- renderPlot({
      plotTEMP$list + scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
    })
    plotSEC$list <- plotTEMP$list
  })
  observeEvent(input$resp_plot,{
    case <- as.numeric(isolate(value()))
    id <- ids[which(ids$case == case), 2]
    vitals_sub <- vitals_and_alarms[which(vitals_and_alarms$Identity == id),]
    timeframe <- isolate(input$timeframe)
    plotInMain(plot.dict$ID[plot.dict$Name == "Resp"])
    record_clicks()
    xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60
    xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
    output$viz <- renderPlot({
      plotRESP$list + scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0))) +
        theme(plot.margin=unit(c(5.5,13.5,5.5,13.5), "pt"))
    })
    plotSEC$list <- plotRESP$list
  })
  observeEvent(input$labs_plot,{
    plotInMain(plot.dict$ID[plot.dict$Name == "Labs"])
    record_clicks()
    if(length(plotLABS$list$data) > 0) {
      if(length(unique(plotLABS$list$data$COMPONENT_NAME)) > 13) {
        height= 300 + 15*(length(unique(plotLABS$list$data$COMPONENT_NAME)) - 13)
      }else {
        height = 300
      }
    }else {
      height = 300
    }
    output$viz <- renderPlot({
      plotLABS$list
    }, height = height)
    plotSEC$list <- plotLABS$list
  })
  observeEvent(input$meds_plot,{
    case <- as.numeric(isolate(value()))
    id <- ids[which(ids$case == case), 2]
    vitals_sub <- vitals_and_alarms[which(vitals_and_alarms$Identity == id),]
    timeframe <- isolate(input$timeframe)
    xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60 
    xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
    plotInMain(plot.dict$ID[plot.dict$Name == "Meds"])
    record_clicks()
    output$viz <- renderPlot({
      plotMEDS$list + scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
    })
    plotSEC$list <- plotMEDS$list
  })
  observeEvent(input$demo_plot,{
    plotInMain(plot.dict$ID[plot.dict$Name == "Info"])
    record_clicks()
    output$viz <- renderPlot({
     plotDEMO$list
    })
    plotSEC$list <- plotDEMO$list
  })
  
  ####################################################
  # Render Time Slider
  output$timeSlider <- renderPrint({
    input$timeframe
  })
  
  # SUBMIT Button
  observeEvent(input$submitButton, {
    if(isolate(activate()) > 0) {
      if (get_caseID(input$experimentID) != "" & get_caseID(input$experimentID) %in% ids$case) {
        value(as.numeric(get_caseID(input$experimentID)))
        condition(get_conditionID(input$experimentID))
        plotInMain(plot.dict$ID[plot.dict$Name == viz.default])
        load_plots(1)
        updateTextInput(session = session, "experimentID", value = "")
      } else if (get_caseID(input$experimentID) != "") {
        updateTextInput(session = session, "experimentID", value = "")
      }
    }
    record_clicks()
  })
  
  # Observe TIMESLIDER
  observeEvent(input$timeframe, {
    if(isolate(activate()) != 1) {
      load_plots(1)
      activate(1)
    } else {
      renderVitals(1)
    }
    record_clicks()
  })
  
  ####################################################
  # Stores the text input 'experimentID' when submitted
  participantIDinput <- eventReactive(input$submitButton, {
    get_caseID(input$experimentID)
  })
  
  output$partID <- renderText({
    participantIDinput()
  })
  
  wiped <- reactiveVal(1)
  clearedPoint <- reactiveVal(0)
  clearedTime <- reactiveVal(0)
  ####################################################
  
  # Observe Hover Events
  observeEvent(input$hover_hr ,{
    hover <- input$hover_hr
    x <- get_data(isolate(value()), get_caseID(input$experimentID), plotHR$list)
    if (length(x[[1]]) > 0 ) {
      point <- nearPoints(x, hover,xvar = "Time", yvar = "Alarm_val", threshold = 30, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0 & isolate(wiped()) == 0) {
        vitals_sub <- plotHR$list$data
        timeframe <- input$timeframe
        xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60 
        xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
        wiped(1)
        output$viz_hr <- renderPlot({
          plotHR$list + 
            scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
        })
        clearedPoint(0)
        clearedTime(0)
      }else if(nrow(point) > 0) {
        if(isolate(clearedTime()) == point$Time & isolate(clearedPoint()) == point$Label) {
          return(NULL)
        }else {
          clearedTime(point$Time)
          clearedPoint(point$Label)
        }
        wiped(0)
        vitals_sub <- plotHR$list$data
        timeframe <- input$timeframe
        xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60 
        xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
        Time <- seq(xmin, xmax, by = 60)
        Time <- as.data.frame(Time)
        Time$val <- 0
        x <- xmin
        yaxis <- data.frame(x,c(0:200))
        colnames(yaxis) <- c("Time","yaxis")
        point1 <- nearPoints(Time, hover,xvar = "Time", yvar = "val", threshold = 500, maxpoints = 1, addDist = TRUE)
        point2 <- nearPoints(yaxis, hover,xvar = "Time", yvar = "yaxis",threshold = 50000, maxpoints = 1)
        pos <- point2$yaxis
        output$viz_hr <- renderPlot({
          plotHR$list + annotate(geom = "label", x = point1$Time, y = pos, label = sprintf("Start Time: %s\nLabel: %s\nDuration: %s", point$Time, point$Label,paste(point$Duration, "Seconds", sep = " ")), hjust = "inward", vjust = "inward",
                                 fill= "#F0F0F0", alpha=.85, size = 5) +
            scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
        })
      }
    }
  })
  wipedOX <- reactiveVal(0)
  observeEvent(input$hover_ox ,{
    hover <- input$hover_ox
    if (isolate(plotInMain()) == plot.dict$ID[plot.dict$Name == "SpO2"]) {
      x <- get_data_ox(isolate(value()), get_caseID(input$experimentID), plotOX$list)
      if (length(x[[1]]) > 0 ) {
        point <- nearPoints(x, hover, xvar = "Time", yvar = "Alarm_val", threshold = 30, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0 & isolate(wipedOX()) == 0) {
          wipedOX(1)
          vitals_sub <- plotHR$list$data
          timeframe <- input$timeframe
          xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60 
          xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
          output$viz <- renderPlot({
            plotOX$list + 
              scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
          })
          clearedPoint(0)
          clearedTime(0)
        } else if(nrow(point) > 0) {
          if(isolate(clearedTime()) == point$Time & isolate(clearedPoint()) == point$Label) {
            return(NULL)
          }else {
            clearedTime(point$Time)
            clearedPoint(point$Label)
          }
          wipedOX(0)
          vitals_sub <- plotHR$list$data
          timeframe <- input$timeframe
          xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60 
          xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
          Time <- seq(xmin, xmax, by = 60)
          Time <- as.data.frame(Time)
          Time$val <- 0
          x <- xmin
          yaxis <- data.frame(x,c(0:400))
          colnames(yaxis) <- c("Time","yaxis")
          point1 <- nearPoints(Time, hover,xvar = "Time", yvar = "val", threshold = 500, maxpoints = 1, addDist = TRUE)
          point2 <- nearPoints(yaxis, hover,xvar = "Time", yvar = "yaxis",threshold = 50000, maxpoints = 1)
          pos <- point2$yaxis
          output$viz <- renderPlot({
            plotOX$list + annotate(geom = "label", x = point1$Time, y = pos, label = sprintf("Start Time: %s\nLabel: %s\nDuration: %s", point$Time, point$Label,paste(point$Duration, "Seconds", sep = " ")), hjust = "inward", vjust = "inward",
                                   fill= "#F0F0F0", alpha=.85, size = 5) +
              scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
          })
        }
      } 
    }
  })
})