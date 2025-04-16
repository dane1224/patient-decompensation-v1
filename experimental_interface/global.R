#### LIBRARIES ####
library(shiny)
library(carData)
library(stats)
library(readxl)
library(stringr)
library(plyr)
library(car)
library(scales)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(astsa)
library(zoo)
library(randomForest)
library(caret)
library(lubridate)
library(tidyverse)
library(rlist)
library(ggplotify)
source('meds.R')
source('observability.R')
source('baseline.R')
source('alarms.R')
source('labs.R')
source('predictions.R')
source('prepare.R')
source('demographics.R')

#### GLOBAL SETTINGS ####
condition.default <- "C"
case.default <- 0
baseline.on <- TRUE
RF.default <- FALSE
LR.default <- TRUE
viz.default <- "Info"

#### DATA PREP ####

model <- prep_model("model.csv")
labs <- prep_labs("labs.csv")
meds <- prep_meds("meds.csv")
vitals_and_alarms <- prep_vitals_and_alarms("vitals.csv")
ord <- prep_order("orders.csv")
ids <- prep_ids("cases.csv")
dat.file <- "clicks.csv"
plot.dict <- read.csv("dictionary.csv")
patients <- prep_patients("patients.csv")
diagnosis <- prep_diagnosis("diagnosis.csv")
comorbids <- prep_comorbids("comorbidities.csv")

#### FUNCTIONS ####
make_graphs <- function(identity = case.default, conditionID = condition.default, timeframe = c(-4,0)) {
  
  ## Data Preparation
  case <- as.numeric(identity)
  id <- ids[which(ids$case == case), 2]
  vitals_sub <- vitals_and_alarms[which(vitals_and_alarms$Identity == id),]
  endtime <- max(vitals_sub$Time)
  xmax <- endtime + (timeframe[2])*60*60 
  xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
  extra_row <- data.frame(NA, NA, NA, NA, NA, NA, NA , NA, NA , NA, NA)
  colnames(extra_row) <- colnames(vitals_sub)
  
  for (pos in 2:length(vitals_sub[[1]]))  {
    if (vitals_sub$Time[pos] - vitals_sub$Time[pos - 1] != 1 & vitals_sub$Time[pos] - vitals_sub$Time[pos - 1] != 0) {
      Time <- vitals_sub$Time[pos - 1] + 60
      while (vitals_sub$Time[pos] - Time != 1) {
        extra_row$Time <- Time
        vitals_sub <- rbind(vitals_sub, extra_row)
        Time <- Time + 60
      }
    }
  }
  vitals_sub <- vitals_sub[order(vitals_sub$Time),]
  vitals_sub$Temperature <- as.numeric(vitals_sub$Temperature)
  bp <- prep_bp(vitals_sub)
  train <- model[which(model$Identity != id),]
  test <- model[which(model$Identity == id),]
  mod.predictions <- make_predictions(id, model, train, test)
  
  ## Create Base Vitals Plots
  gplot_resp <- ggplot(vitals_sub, aes(Time, Respirations)) + geom_line(colour = "grey",size = 1) + geom_point(size = 0.75, alpha = 0.5) + 
    labs(y = "Respirations", x = NULL) +
    scale_y_continuous(limits = c(0, 65), breaks = seq(0,65,10), minor_breaks = seq(0,65,5),  expand = expand_scale(mult = c(0, 0)))
  gplot_temp <- ggplot(vitals_sub, aes(Time, Temperature)) + geom_line(colour = "grey", size = 1) + geom_point() +
    labs(y = "Temperature", x = NULL) +
    scale_y_continuous(limits = c(80,110), breaks = seq(80,110, 10), minor_breaks = seq(80,110,5), expand = expand_scale(mult = c(0,0))) 
  gplot_bp <- ggplot(bp, aes(Time, BP, group=group)) + geom_line(colour="grey",size=1.5, na.rm = TRUE) + geom_point(size=3, alpha=0.5, na.rm = TRUE) +
    labs(y = "Blood Pressure", x = NULL) +
    geom_text(data=bp[which(bp$group2 == 1),], aes(Time, BP, label = BP), vjust=1.6, alpha = .75) +
    geom_text(data=bp[which(bp$group2 == 2),], aes(Time, BP, label = BP), vjust=-.6, alpha = .75) +
    scale_y_continuous(limits = c(0,250), breaks = seq(0,250, 25), expand = expand_scale(mult = c(0,0)), sec.axis = sec_axis(~., breaks=c(100,bp$BP[length(bp[[1]])-1], bp$BP[length(bp[[1]])]))) +
    theme(axis.text.y.right = element_text(colour = c("white","black","black")),
          axis.ticks.y.right = element_line(colour = "white"))
  gplot_bp2 <- ggplot(bp, aes(Time, BP, group=group)) + geom_line(colour="grey",size=1.5, na.rm = TRUE) + geom_point(size=3, alpha=0.5, na.rm = TRUE) +
    labs(y = "Blood Pressure", x = NULL) +
    #geom_text(data=bp[which(bp$group2 == 1),], aes(Time, BP, label = BP), vjust=1.6, alpha = .75) +
    #geom_text(data=bp[which(bp$group2 == 2),], aes(Time, BP, label = BP), vjust=-.6, alpha = .75) +
    scale_y_continuous(limits = c(0,250), breaks = seq(0,250, 25), expand = expand_scale(mult = c(0,0)),sec.axis = sec_axis(~., breaks=c(100))) 
  gplot_labs <- plot_labs(xmin, xmax, id, labs)
  gplot_meds <- make_meds_graph(meds, id, xmax, xmin)
  gplot_demo <- make_demo_table(id, endtime)
  
  ## Create Predictions
  if (get_predictionsON(conditionID)) {
    gplot_concern <- plot_predictions(mod.predictions, get_predictionsON(conditionID))
  } else {
    gplot_concern <- NULL
  }
  
  ## Create HR & SpO2 Plots
  hrox <- observability(id, model, vitals_and_alarms, train, test, xmin, xmax, 0, conditionID)
  gplot_hr <- hrox[[2]]
  gplot_oximetry <- hrox[[1]]
  
  ## Format Plots
  # gplot_hr <- format_plots(gplot_hr)
  gplot_oximetry <- format_plots(gplot_oximetry)
  gplot_resp <- format_plots(gplot_resp)
  gplot_temp <- format_plots(gplot_temp)
  gplot_bp <- format_plots(gplot_bp)
  gplot_bp2 <- format_plots(gplot_bp2)
  gplot_meds[[1]] <- format_plots(gplot_meds[[1]])
  gplot_demo[[1]] <- format_plots(gplot_demo[[1]])
  
  ## Add Alarms to HR & SpO2 Plots
  l <- Add_Alarms(prep_alarms(vitals_sub), gplot_hr, gplot_oximetry)
  gplot_hr <- l[[1]]
  gplot_oximetry <- l[[2]]
  
  ## Add Baseline
  if (length(vitals_sub[!is.na(vitals_sub$Respirations),][[1]]) > 0) {
    # vitals_subR <- baseline(vitals_sub, 5, .08, 2, "R")
    vitals_subR <- vitals_sub
    # RESP BASELINE = Median of 1st 60 Data Points
    vitals_subR$baseline <- median(vitals_subR[!is.na(vitals_subR$Respirations),]$Respirations[1:60], na.rm = TRUE)
    last_time <- vitals_subR[which(vitals_subR$Time <= xmax),]
    last_time <- last_time[!is.na(last_time$Respirations),]
    most_recent <- last_time$Respirations[length(last_time$Respirations)]
    most_recent2 <- last_time$baseline[length(last_time$Respirations)]
    most_recent2 <- round(most_recent2, 0)
    gplot_resp <- gplot_resp + 
      geom_line(data = vitals_subR, aes(x = Time, y = baseline, group = baseline),color = "blue", size = .5, alpha = if (baseline.on) {1} else {0}) +
      geom_ribbon(data = vitals_subR, aes(ymin = Respirations, ymax = baseline,x = Time, group = baseline), fill = "royalblue3", alpha = if (baseline.on) {0.3} else {0}) +
      scale_y_continuous(limits = c(0, 65), breaks = seq(0,65,10), minor_breaks = seq(0,65,5),  expand = expand_scale(mult = c(0, 0)), sec.axis = sec_axis(~., breaks = c(most_recent2,most_recent))) +
      theme(axis.text.y.right = element_text(color = if (baseline.on) {c("blue","black")} else {c("white","black")}),
            axis.ticks.y.right = element_blank())
  } else {
    gplot_resp <- gplot_resp +
      scale_y_continuous(limits = c(0, 65), breaks = seq(0,65,10), minor_breaks = seq(0,65,5),  expand = expand_scale(mult = c(0, 0)), sec.axis = sec_axis(~., breaks = c(100))) +
      theme(axis.text.y.right = element_text(color = c("white")),
            axis.ticks.y.right = element_blank())
  }
  
  vitals_sub1 <- vitals_sub
  # HR BASELINE = Median of 1st 60 Data Points
  vitals_sub1$baseline <- median(vitals_sub1[!is.na(vitals_sub1$Heartrate),]$Heartrate[1:60], na.rm = TRUE)
  last_time <- vitals_sub1[which(vitals_sub1$Time <= xmax),]
  most_recent <- last_time$Heartrate[length(last_time$Heartrate)]
  most_recent2 <- last_time$baseline[length(last_time$baseline)]
  most_recent2 <- round(most_recent2, 0)
  gplot_hr <- gplot_hr + 
    geom_line(data = vitals_sub1, aes(x = Time, y = baseline, group = baseline), color = "blue", size = .5, alpha = if (baseline.on) {1} else {0}) +
    geom_ribbon(data = vitals_sub1, aes(ymin = Heartrate, ymax = baseline,x = Time, group = baseline), fill = "royalblue3", alpha = if (baseline.on) {0.3} else {0}) +
    scale_y_continuous(limits = c(0, 170), breaks= seq(30,170,20), minor_breaks = seq(30,170,5),expand = expand_scale(mult = c(0, 0)),sec.axis = sec_axis(~., breaks=c(100,most_recent2,most_recent))) + 
    theme(axis.text.y.right = element_text(color = if (baseline.on) {c("white","blue","black")} else {c("white","white","black")}),
          axis.ticks.y.right = element_blank())
  # SPO2 BASELINE = Mode of All Data Points ???
  vitals_sub2 <- baseline(vitals_sub, 10 ,.01,5, "S")
  # vitals_sub2 <- vitals_subR
  last_time <- vitals_sub2[which(vitals_sub2$Time <= xmax),]
  last_time <- last_time[!is.na(last_time$Ox),]
  most_recent <- last_time$Ox[length(last_time$Ox)]
  most_recent2 <- last_time$baseline[length(last_time$baseline)]
  most_recent2 <- round(most_recent2, 0)
  gplot_oximetry <- gplot_oximetry + 
    geom_line(data = vitals_sub2, aes(x = Time, y = baseline, group = baseline), color = "blue", size = .5, alpha = if (baseline.on) {1} else {0}) +
    geom_ribbon(data = vitals_sub2, aes(ymin = Ox, ymax = baseline,x = Time, group = baseline), fill = "royalblue3", alpha = if (baseline.on) {0.3} else {0}) +
    scale_y_continuous(limits = c(39.6, 102), breaks = seq(50,100,10), expand = expand_scale(mult = c(0, 0)),sec.axis = sec_axis(~., breaks=c(100,most_recent2,most_recent))) + 
    theme(axis.text.y.right = element_text(color = if (baseline.on) {c("white","blue","black")} else {c("white","white","black")}),
          axis.ticks.y.right = element_blank())
  if (length(vitals_sub[!is.na(vitals_sub$Temperature),][[1]]) > 0) {
    vitals_subR <- vitals_sub
    # TEMP BASELINE = 98.7
    vitals_subR$baseline <- 98.7
    last_time <- vitals_subR[which(vitals_subR$Time <= xmax),]
    last_time <- last_time[!is.na(last_time$Temperature),]
    most_recent <- last_time$Temperature[length(last_time$Temperature)]
    most_recent2 <- last_time$baseline[length(last_time$Temperature)]
    most_recent <- round(most_recent,1)
    most_recent2 <- round(most_recent2, 1)
    gplot_temp <- gplot_temp + 
      geom_line(data = vitals_subR, aes(x = Time, y = baseline, group = baseline), color = "blue", size = .5, alpha = if (baseline.on) {1} else {0}) +
      geom_ribbon(data = vitals_subR, aes(ymin = Temperature, ymax = baseline,x = Time, group = baseline), fill = "royalblue3", alpha = if (baseline.on) {0.3} else {0}) +
      scale_y_continuous(limits = c(80, 110), breaks = seq(80,110,10), minor_breaks = seq(80,110,5),  expand = expand_scale(mult = c(0, 0)), sec.axis = sec_axis(~., breaks = c(100,most_recent2,most_recent))) +
      theme(axis.text.y.right = element_text(color = if (baseline.on) {c("white","blue","black")} else {c("white","white","black")}),
            axis.ticks.y.right = element_blank())
  } else {
    gplot_temp <- gplot_temp + scale_y_continuous(limits = c(80, 110), breaks = seq(80,110,10), minor_breaks = seq(80,110,5),  expand = expand_scale(mult = c(0, 0)), sec.axis = sec_axis(~., breaks = c(100))) +
      theme(axis.text.y.right = element_text(color = c("white")),
            axis.ticks.y.right = element_blank())   
  }
  
  ## Scale Plots by Time Scale 
  gplot_hr <- gplot_hr + scale_x_datetime(limits = c(xmin, xmax),labels = time_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
  gplot_oximetry <- gplot_oximetry + scale_x_datetime(limits = c(xmin, xmax),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
  gplot_resp <- gplot_resp + scale_x_datetime(limits = c(xmin, xmax),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
  gplot_temp <- gplot_temp + scale_x_datetime(limits = c(xmin, xmax),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
  gplot_bp2 <- gplot_bp2 + scale_x_datetime(limits = c(xmin, xmax),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
  gplot_bp <- gplot_bp + scale_x_datetime(limits = c(xmin, xmax),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
  gplot_meds[[1]] <- gplot_meds[[1]] + scale_x_datetime(limits = c(xmin, xmax),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
  gplot_meds[[2]] <- gplot_meds[[2]] + scale_x_datetime(limits = c(xmin, xmax),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
  if (length(gplot_labs[[2]]$data) == 2) {
    gplot_labs[[1]] <- gplot_labs[[1]] + scale_x_datetime(limits = c(xmin, xmax),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
    gplot_labs[[2]] <- gplot_labs[[2]] + scale_x_datetime(limits = c(xmin, xmax),labels = date_format("%H:%M", tz = ""), date_breaks = paste(-10*(timeframe[1]-timeframe[2]), "min"), expand = expand_scale(mult = c(0, 0)))
  }
  gplot_hrbadge <- gplot_hr + 
    scale_y_continuous(limits = c(0, 170), breaks = seq(50, 150, 50), minor_breaks = seq(50, 150, 50),  expand = expand_scale(mult = c(0, 0))) + 
    labs(x = "Past 24 Hours") + 
    theme(axis.ticks.x = element_blank(), 
          axis.text.x = element_blank())
  
  ## Make Tables based on Table Choice
  p_list <- list(gplot_hr , gplot_oximetry , gplot_resp , gplot_temp, gplot_labs , gplot_meds, mod.predictions, gplot_concern, list(gplot_bp, gplot_bp2), gplot_hrbadge, gplot_demo)
  return(p_list)
}