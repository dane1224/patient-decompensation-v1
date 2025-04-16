check.err <- function (code, silent = FALSE) {
  # returns NULL if any models fail to fit
  tryCatch(code, error = function(c) {if (!silent) {NULL} else {code}})
}
get_data <- function(identity, part, gplot_hr) {
  case <- as.numeric(identity) 
  id <- ids[which(ids$case == case), 2]
  vitals_sub <- vitals_and_alarms[which(vitals_and_alarms$Identity == id),]
  vitals_sub$Alarm_val <- 25
  vitals_sub <- vitals_sub[!is.na(vitals_sub$Label),]
  vitals_sub <- vitals_sub[which(!grepl("SPO2", vitals_sub$Label)),]
  vitals_sub$Label <- as.character(vitals_sub$Label)
  if (length(vitals_sub[[1]]) > 0) {
    split <- strsplit(vitals_sub$Label, split = " ")
    for (i in 1:length(vitals_sub[[1]])) {
      if (grepl("[[:digit:]]", vitals_sub[i,9])) {
        vitals_sub$Alarm_val[i] <- as.numeric(split[[i]][length(split[[i]])])
      }
    }
  }
  stop <- 0
  for(i in 1:length(gplot_hr$layers)) {
    if(length(gplot_hr$layers[[i]]$data) != 0) {
      if(colnames(gplot_hr$layers[[i]]$data)[1] == "TimeFake") {
        stop <- i
      }  
    }
  }
  if(stop != 0) {
    fake_stuff <- gplot_hr$layers[[stop]]$data
    print(fake_stuff)
    colnames(fake_stuff)[1] <- "Time"
    fake_stuff$Alarm_val <- 25
    for(i in 1:length(fake_stuff[[1]])) {
      st <- 1
      for(j in 1:length(vitals_sub)) {
        if(colnames(vitals_sub)[j] %in% colnames(fake_stuff)) {
          vitals_sub[length(vitals_sub[[1]]) + st, j] <- 
            fake_stuff[i, colnames(vitals_sub)[j]]
          st <- 0
        }else {
          if(length(vitals_sub[[1]]) > 1) {
            vitals_sub[length(vitals_sub[[1]]) + st, j] <- 
              vitals_sub[length(vitals_sub[[1]]) - 1, j]
          }else {
            vitals_sub[length(vitals_sub[[1]]) + st, j] <- 0
          }
          st <- 0
        }
      }
    }
  }
  return(vitals_sub)
}
get_data_ox <- function(identity, part, gplot_oximetry) {
  case <- as.numeric(identity) 
  id <- ids[which(ids$case == case), 2]
  vitals_sub <- vitals_and_alarms[which(vitals_and_alarms$Identity == id),]
  vitals_sub$Alarm_val <- 45
  vitals_sub <- vitals_sub[!is.na(vitals_sub$Label),]
  vitals_sub <- vitals_sub[which(grepl("SPO2", vitals_sub$Label)),]
  vitals_sub$Label <- as.character(vitals_sub$Label)
  if (length(vitals_sub[[1]]) > 0) {
    split <- strsplit(vitals_sub$Label, split = " ")
    for (i in 1:length(split)) {
      if (length(split[[i]]) > 2) {
        vitals_sub$Alarm_val[i] <- as.numeric(split[[i]][length(split[[i]])])
      }
    }
  }
  stop <- 0
  for(i in 1:length(gplot_oximetry$layers)) {
    if(length(gplot_oximetry$layers[[i]]$data) != 0) {
      if(colnames(gplot_oximetry$layers[[i]]$data)[1] == "TimeFake") {
        stop <- i
      }  
    }
  }
  if(stop != 0) {
    fake_stuff <- gplot_oximetry$layers[[stop]]$data
    colnames(fake_stuff)[1] <- "Time"
    fake_stuff$Alarm_val <- 45
    for(i in 1:length(fake_stuff[[1]])) {
      st <- 1
      for(j in 1:length(vitals_sub)) {
        if(colnames(vitals_sub)[j] %in% colnames(fake_stuff)) {
          vitals_sub[length(vitals_sub[[1]]) + st, j] <- 
            fake_stuff[i, colnames(vitals_sub)[j]]
          st <- 0
        }else {
          if(length(vitals_sub[[1]]) > 1) {
            vitals_sub[length(vitals_sub[[1]]) + st, j] <- 
              vitals_sub[length(vitals_sub[[1]]) - 1, j]
          }else {
            vitals_sub[length(vitals_sub[[1]]) + st, j] <- 0
          }
          st <- 0
        }
      }
    }
  }
  return(vitals_sub)
}
get_case <- function(identity, condition) {
  return(paste0("Case: ", identity, condition))
}
get_click_data <- function(identity, part, plotInMain) {
  case <- as.numeric(identity) 
  id <- ids[which(ids$case == case), 2]
  vitals_sub <- vitals_and_alarms[which(vitals_and_alarms$Identity == id),]
  vitals_sub$Row <- 1:length(vitals_sub[[1]])
  if (plotInMain == 0) {
    colnames(vitals_sub)[4] <- "READING"
  }
  if (plotInMain == 1) {
    colnames(vitals_sub)[5] <- "READING"
  }
  if (plotInMain == 2) {
    colnames(vitals_sub)[6] <- "READING"
  }
  if (plotInMain == 3) {
    colnames(vitals_sub)[7] <- "READING"
  }
  if (plotInMain == 10) {
    colnames(vitals_sub)[3] <- "READING"
  }
  return(vitals_sub)
}
get_click_badge <- function(identity, part, timeframe) {
  case <- as.numeric(identity) 
  id <- ids[which(ids$case == case), 2]
  timeframe <- c(-24,0)
  vitals_sub <- vitals_and_alarms[which(vitals_and_alarms$Identity == id),]
  xmax <- max(vitals_sub$Time) + (timeframe[2])*60*60 
  xmin <- as.POSIXct(xmax) + (timeframe[1] - timeframe[2])*60*60
  xmax <- xmax + ((1.16666666667*(xmax-xmin)) - (xmax-xmin))
  time <- seq(xmin, xmax, by = 60)
  time <- as.data.frame(time)
  time$val <- 0
  return(time)
}
# get_valid_input <- function(x) {
#   x <- as.character(unique(ord$Participant.ID))
#   x2 <- paste(x, "-2",sep ="")
#   x3 <- paste(x, "-3",sep ="")
#   x4 <- paste(x, "-4",sep ="")
#   x <- append(x,x2)
#   x <- append(x,x3)
#   x <- append(x,x4)
#   return(x)
# }
get_caseID <- function(x) {
  if (is.na(as.numeric(substr(x,nchar(x),nchar(x))))) {
    x <- substr(x,1,nchar(x)-1)
  }
  return(x)
}
get_conditionID <- function(x) {
  if (is.null(x) | x == "") {
    return(condition.default)
  }
  if (is.na(as.numeric(substr(x,nchar(x),nchar(x))))) {
    return(toupper(substr(x,nchar(x),nchar(x))))
  } else {
    return(condition.default)
  }
}
get_participantID <- function(x) {
  x <- check.err(tolower(x))
  if (x %in% ord$participantID) {
    return(x)
  } else {
    return("default")
  }
}
get_predictionsON <- function(x) {
  if (x == "B" | x == "C") {
    return(TRUE)
  } else if (x == "A" | x == "D") {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
get_annotationsON <- function(x) {
  if (x == "C" | x == "D") {
    return(TRUE)
  } else if (x == "A" | x == "B") {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
get_modelsON <- function(x) {
  if (x == "A" | x == "B" | x == "C" | x == "D") {
    RF.on <- FALSE
    LR.on <- TRUE
  } else {
    RF.on <- RF.default
    LR.on <- LR.default
  }
  models.on <- c(RF.on, LR.on)
  return(c(RF.on, LR.on))
}
format_plots <- function(x) {
  x <- x + theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    plot.background = element_blank(),
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )
}
prep_bp <- function(x) {
  bp <- subset(x, select= c(Time, BP))
  bp <- bp[!is.na(bp$BP),]
  bp <- bp[order(bp$Time, bp$BP),]
  bp <- unique(bp)
  st <- 1
  if(length(bp[[1]]) > 0) {
    while(st < length(bp[[1]])) {
      bp$group[st] <- st
      bp$group2[st] <- 1
      bp$group[st+1] <- st
      bp$group2[st+1] <- 2
      st <- st + 2
    }
  } else {
    bp <- x
    bp$group <- 0
    bp$group2 <- 1
  }
  return(bp)
}
prep_alarms <- function(x) {
  alarms <- subset(x, select = c(Time, Label, Duration))
  alarms <- alarms[!is.na(alarms$Label),]
  alarms$Start <- alarms$Time
  alarms$End <- alarms$Time + alarms$Duration
  alarms <- unique(alarms)
  alarms$Label <- as.character(alarms$Label)
  return(alarms)
}
prep_model <- function(x) {
  model <- read.csv(x)
  model <- model[,c(2:length(model))]
  model$Identity <- as.character(model$Identity)
  return(model)
}
prep_labs <- function(x) {
  labs <- read.csv(x)
  labs <- labs[,c(2:25)]
  labs$Time <- as.character(labs$Time)
  labs$Time <- as.POSIXct(labs$Time, format = "%Y-%m-%d %H:%M:%S")
  labs$Identity <- as.character(labs$Identity)
  labs$RESULT_TIME_S <- as.character(labs$RESULT_TIME_S)
  labs$RESULT_TIME_S <- as.POSIXct(labs$RESULT_TIME_S, format = "%Y-%m-%d %H:%M:%S")
  labs$FIRST_DATE <- as.character(labs$FIRST_DATE)
  labs$LAST_DATE <- as.POSIXct(labs$LAST_DATE, format = "%Y-%m-%d %H:%M:%S")
  return(labs)
}
prep_meds <- function(x) {
  meds <- read.csv(x)
  meds <- meds[,c(2:5)]
  meds$Identity <- as.character(meds$Identity)
  meds$Time_Start <- as.character(meds$Time_Start)
  meds$Time_Start <- as.POSIXct(meds$Time_Start, format = "%Y-%m-%d %H:%M:%S")
  meds$Time_End <- as.character(meds$Time_End)
  meds$Time_End <- as.POSIXct(meds$Time_End, format = "%Y-%m-%d %H:%M:%S")
  meds$Medication <- as.character(meds$Medication)
  colnames(meds) <- c("Identity", "Time_Start", "Time_End", "Medication")
  return(meds)
}
prep_vitals_and_alarms <- function(x) {
  vitals_and_alarms <- read.csv(x)
  vitals_and_alarms <- vitals_and_alarms[,c(2:length(vitals_and_alarms))]
  vitals_and_alarms$Identity <- as.character(vitals_and_alarms$Identity)
  vitals_and_alarms$Time <- as.character(vitals_and_alarms$Time)
  vitals_and_alarms$Time <- as.POSIXct(vitals_and_alarms$Time, format = "%Y-%m-%d %H:%M:%S")
  return(vitals_and_alarms)
}
prep_order <- function(x) {
  ord <- read.csv(x)
  ord[nrow(ord)+1,1] <- "default"
  for (i in 2:ncol(ord)) {
    ord[ord[,1] == "default",i] <- paste0(i-1, condition.default)
  }
  ord <- data.frame(ord[,1], rep(paste0(case.default, condition.default),nrow(ord)), ord[,2:ncol(ord)])
  names(ord) <- c("participantID",0:(length(names(ord))-2))
  ord$participantID <- tolower(ord$participantID)
  for (i in 2:ncol(ord)) {
    ord[,i] <- toupper(ord[,i])
  }
  return(ord)
}
prep_ids <- function(y) {
  ids <- read.csv(y)
  ids <- ids[ids$use,c("case","id")]
  ids <- ids[order(ids$case),]
  rownames(ids) <- seq(1,nrow(ids),1)
  return(ids)
}