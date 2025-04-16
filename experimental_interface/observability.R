observability <- function(id, new_model, for_model, train, test, xmin, xmax, mi, conditionID) {
  
  ## Data Preparation
  new_model <- model
  modelLM <- glm(Event ~ avg_hb + streak_low_resid + neg_rate + Mean_Loess_Rate, data = train, family = binomial)
  one_model <- test
  one_model <- subset(one_model, select = c(avg_hb, neg_rate, Mean_Loess_Rate, streak_low_resid))
  x <- modelLM$coefficients[[1]] + modelLM$coefficients[[2]]*one_model$avg_hb + modelLM$coefficients[[3]]*one_model$streak_low_resid +
    modelLM$coefficients[[4]]*one_model$neg_rate + modelLM$coefficients[[5]]*one_model$Mean_Loess_Rate 
  p <- exp(x)/(exp(x) + 1)
  one_model2 <- one_model
  weights <- data.frame()
  for (j in 1:length(one_model)) {
    c <- names(modelLM$coefficients[j + 1])
    if (modelLM$coefficients[[c]] > 0) {
      one_model[,c] <- min(model[,c])
      x <- modelLM$coefficients[[1]] + modelLM$coefficients[[2]]*one_model$avg_hb + modelLM$coefficients[[3]]*one_model$streak_low_resid +
        modelLM$coefficients[[4]]*one_model$neg_rate + modelLM$coefficients[[5]]*one_model$Mean_Loess_Rate 
    } else {
      one_model[,c] <- max(model[,c])
      x <- modelLM$coefficients[[1]] + modelLM$coefficients[[2]]*one_model$avg_hb + modelLM$coefficients[[3]]*one_model$streak_low_resid +
        modelLM$coefficients[[4]]*one_model$neg_rate + modelLM$coefficients[[5]]*one_model$Mean_Loess_Rate 
    }
    p2 <- exp(x)/(exp(x) + 1)
    weights[j,1] <- c
    weights[j,2] <- p - p2 
    one_model <- one_model2
  }
  weights <- weights[order(weights$V1),]
  colnames(weights) <- c("Feature", "Weight")
  maxHR <- 238 #max(for_model$Heartrate)  ## 30 min = 238
  minHR <- 0 #min(for_model$Heartrate) ## 30 min = 0
  maxOX <- 100 #max(for_model$Ox, na.rm = TRUE) ## 30 min = 100
  minOX <- 7 #min(for_model$Ox, na.rm = TRUE) ## 30 min = 7
  ##for_model <- vitals_and_alarms[which(vitals_and_alarms$Identity == i),]
  #one_model <- new_model[which(new_model$Identity == i),]
  #one_model <- subset(one_model, select = c(avg_hb, neg_rate, Mean_Loess_Rate, streak_low_resid))
  try <- for_model[which(for_model$Identity == id),]
  try <- subset(try, select = c(Identity, Time, Heartrate, Ox, Event))
  try <- unique(try) 
  try <- try[which(try$Time >= max(try$Time) - 7*60*60),]
  min_time <- min(try$Time)
  heartbeat <- try[,3]
  oximetry <- try[,4]
  oximetry[is.na(oximetry)] <- max(oximetry, na.rm = TRUE)
  oximetry <- (oximetry - minOX) / (maxOX - minOX)
  heartbeat <- as.data.frame(heartbeat)
  heartbeat <- heartbeat[complete.cases(heartbeat),]
  heartbeat <- as.data.frame(heartbeat)
  heartbeat[, 1] <- (heartbeat[, 1] - minHR) / (maxHR - minHR)
  loess.fit <- loess(heartbeat[[1]] ~ c(1:length(heartbeat[[1]])), span = .12, degree = 2)
  loess.fitOX <- loess(oximetry ~ c(1:length(oximetry)), span = .11, degree = 2)
  loess_ratesOX <- rep(NA, length(loess.fitOX$fitted)-1)
  loess_rates <- rep(NA,length(loess.fit$fitted)-1)
  for (j in 2:length(loess.fit$fitted)) {
    loess_rates[j - 1] <- round(loess.fit$fitted[j],4) - round(loess.fit$fitted[j - 1],4)
    loess_ratesOX[j - 1] <- round(loess.fitOX$fitted[j], 4) - round(loess.fitOX$fitted[j - 1], 4)
  }
  oxPulse <- cbind(loess_rates, loess_ratesOX,heartbeat[2:length(heartbeat[,1]),1], oximetry[2:length(oximetry)])
  oxPulse <- as.data.frame(oxPulse)
  oxPulse$Time <- try$Time[2:length(try$Time)]
  colnames(oxPulse) <- c("loess", "loessOx", "hr", "or", "Time")
  oxPulse[,6] <- NA
  oxPulse[,7] <- NA
  pos <- 1
  j <- 1
  time_resid <- as.data.frame(loess.fit$residuals)
  time_resid$Time <- try$Time
  colnames(time_resid) <- c("resid", "time")
  time_resid$streak <- NA
  while (j < length(time_resid[[1]])) {
    if (abs(time_resid$resid[j]) < .007) {
      pos <- j
      while (abs(time_resid$resid[j]) < .007 & j < length(time_resid[[1]])) {
        j <- j + 1
      }
      time_resid$streak[pos:j] <- j - pos
    } else {
      j <- j + 1
      time_resid$streak[j] <- 1
    }
  }
  time_resid <- time_resid[which(time_resid$streak > mean(new_model$streak_low_resid)),]
  if (weights[which(weights$Feature == "streak_low_resid"),2] > .05) {
    oxPulse[which(oxPulse$Time %in% time_resid$time), 6] <- "red"
  }
  if (weights[which(weights$Feature == "Mean_Loess_Rate"),2] >= .12) {
    oxPulse[which(abs(oxPulse$loess) > mean(oxPulse$loess) + 2*sd(oxPulse$loess)), 6] <- "red"
  } 
  #if (weights[which(weights$Feature == "avg_hb"),2] > .4) {
  #  oxPulse[which(oxPulse$hr > mean(new_model$avg_hb) + 1.5*sd(new_model$avg_hb)), 6] <- "green"
  #}
  if (weights[which(weights$Feature == "neg_rate"),2] >= .24) {
    oxPulse[which(abs(oxPulse$loessOx) > abs(mean(new_model$neg_rate)) + sd(new_model$neg_rate) ), 7] <- "red"
  }
  colnames(oxPulse)[6] <- "color"
  times <- oxPulse[!is.na(oxPulse$color),c(5,6)]
  colnames(oxPulse)[7] <- "colorOx"
  timesOx <- oxPulse[!is.na(oxPulse$colorOx),c(5,7)]
  try2 <- for_model[which(for_model$Identity == id),]
  extra_row <- data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  colnames(extra_row) <- colnames(try2)
  for (pos in 2:length(try2[[1]]))  {
    if (try2$Time[pos] - try2$Time[pos - 1] != 1 & try2$Time[pos] - try2$Time[pos - 1] != 0) {
      Time <- try2$Time[pos - 1] + 60
      while (try2$Time[pos] - Time != 1) {
        extra_row$Time <- Time
        try2 <- rbind(try2, extra_row)
        Time <- Time + 60
      }
    }
  }
  try2 <- try2[order(try2$Time),]
  try <- subset(try2, select = c(Identity, Time, Heartrate, Ox, Event ))
  try <- unique(try)
  try <- merge(try, times, by = "Time", all.x = TRUE)
  try$color[is.na(try$color)] <- "none"
  try <- merge(try, timesOx, by = "Time", all.x = TRUE)
  try$colorOx[is.na(try$colorOx)] <- "none"
  #try$loess <- NA
  #try$loessOx <- NA
  try <- unique(try)
  try <- merge(try, oxPulse[,c(1,5)], by = "Time", all.x = TRUE)
  try <- merge(try, oxPulse[,c(2,5)], by = "Time", all.x = TRUE)
  try$loess[!is.na(try$loess)] <- loess.fit$fitted[2:length(loess.fit$fitted)]*(maxHR - minHR) + minHR
  try$loessOx[!is.na(try$loessOx)] <- loess.fitOX$fitted[2:length(loess.fit$fitted)]*(maxOX - minOX) + minOX
  try[is.na(try$Heartrate), 8] <- NA
  try[is.na(try$Ox), 9] <- NA
  try[which(try$colorOx != "red"), 9] <- NA
  try[which(try$color != "red"), 8] <- NA
  if(mi > 2) {
    try <- try[which(try$Time >= (max(try$Time) - 5*60*60)),] 
  }
  
  ## Create HR & OX Plots
  ox_plot <- ggplot(try, aes(Time,Ox))  + labs(y = "SpO2", x = NULL) + geom_line(colour = "grey",size = 1, na.rm=TRUE) + 
    geom_point(size = .75, alpha = .5, na.rm = TRUE) +
    scale_y_continuous(limits = c(39.6, 102), breaks = seq(50,100,10), expand = expand_scale(mult = c(0, 0))) +
    geom_hline(yintercept = 88, linetype = 5, colour = "grey") +
    geom_hline(yintercept = 50, linetype = 1, colour = "black", size = 2)
  hr_plot <- ggplot(try, aes(Time,Heartrate)) + labs(y = "Heart Rate", x = paste("Date:", date(xmax))) + geom_line(colour = "grey",size = 1, na.rm=TRUE) +
    geom_point(size = .75, alpha = .5,na.rm = TRUE) + 
    scale_y_continuous(limits = c(0, 170), breaks = seq(30,170,20), minor_breaks = seq(30,170,5),  expand = expand_scale(mult = c(0, 0))) +
    geom_hline(yintercept = 50, linetype = 5, colour = "grey") + 
    geom_hline(yintercept = 150, linetype = 5, colour = "grey") +
    geom_hline(yintercept = 30, linetype = 1, colour = "black", size = 2) +
    theme(
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 16, face = "bold"),
      plot.background = element_blank(),
      panel.grid.major = element_line(),
      panel.grid.minor = element_line(),
      panel.border = element_blank(),
      axis.text.x = element_text(),
      axis.ticks.x = element_line(),
      legend.position = "none"
    )
  
  ## Add Annotations
  if (get_annotationsON(conditionID)) {
    loessD <- try[!is.na(try$loessOx),]
    if (length(loessD[[1]]) > 0) {
      ox_plot <- ox_plot + geom_line(aes(Time, loessOx), colour = "red", size = .75, na.rm = TRUE) +
        geom_point(data = try[!is.na(try$loessOx),], aes(Time, Ox, color = "red"), size = .5, alpha = .5, na.rm = TRUE) + 
        geom_segment(aes(x  = Time, y = loessOx , yend = Ox, xend = Time), color = "red")
      
    }
    if (weights[which(weights$Feature == "avg_hb"),2] > .4 & length(try[which(try$Heartrate > 95),][[1]]) > 0) {
      xmin <- min_time
      hr_plot <- hr_plot + geom_line(data=try[which(try$Time >= xmin),],aes(x=Time, y = 95), colour = "red")
      #geom_point(data = try[which(try$Heartrate > 95),], aes(Time, Heartrate, color = "red")) 
      if (length(try[which(try$Heartrate > 95 & try$Time >= xmin),][[1]]) > 0) {
        hr_plot <- hr_plot + geom_segment(data = try[which(try$Heartrate > 95 & try$Time >= xmin),], aes(x = Time, y = 95, yend = Heartrate - .8, xend = Time),color = "red")
      }
    }
    if (length(try[!is.na(try$loess),][[1]]) > 0) {
      hr_plot <- hr_plot + geom_line(aes(Time, loess), colour = "red", size = .75, na.rm = TRUE) +
        geom_point(data = try[!is.na(try$loess),], aes(Time, Heartrate, color = "red"), size = .5, alpha = .5, na.rm = TRUE) + 
        geom_segment(aes(x  = Time, y = loess , yend = Heartrate, xend = Time), color = "red", na.rm = TRUE)
    }
    try <- merge(try, oxPulse[,c(1,5)], by = "Time", all.x = TRUE)
    try$loess.y[!is.na(try$loess.y)] <- loess.fit$fitted[2:length(loess.fit$fitted)]*(maxHR - minHR) + minHR
    try[which(try$color != "blue"), 10] <- NA
    hr_plot <- hr_plot + geom_line(data = try, aes(Time, loess.y), colour = "red", size = .75, na.rm = TRUE)
  }
  
  ## Return HR & OX Plots
  list <- c()
  list[[1]] <- ox_plot
  list[[2]] <- hr_plot 
  return(list)
}