baseline <- function(vitals_one_patient, x, thres, s, vital) {
  one_patient <- vitals_one_patient
  getmode <- function(v) {
    uniqv <- unique(v)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
  }
  if (vital == "H") {
    colnames(one_patient)[which(colnames(one_patient) == "Heartrate")] <- "READING"
    one_patient <- one_patient[!is.na(one_patient$READING),]
    baseline <- c()
    baseline[1] <- median(one_patient$READING[1:5])
  }else if(vital == "S"){
    colnames(one_patient)[which(colnames(one_patient) == "Ox")] <- "READING"
    baseline <- rep(NA, length(one_patient[[1]]))
    vitals_one_patient$baseline <- getmode(one_patient$READING[!is.na(one_patient$READING)])
    return(vitals_one_patient)
  }else if (vital == "R") {
    colnames(one_patient)[which(colnames(one_patient) == "Respirations")] <- "READING"
    baseline <- c()
    one_patient[is.na(one_patient$READING), 7] <- mean(one_patient$READING, na.rm = TRUE)
    brnona <- one_patient[!is.na(one_patient$READING),]
    baseline[1] <- median(brnona$READING[1:5])
  }
  
  
  ## initial baseline

  group <- c()
  g <- 1
  group[1] <- g
  bl <- baseline[1]
  n_old <- 1
  ## calculate moving average
  temp1 <- data.frame(one_patient$READING, one_patient$Time)
  mav1 <- rollmean(temp1$one_patient.READING, 10, fill = list(NA, NULL, NA), align=c("center"))
  one_patient$fitted <- mav1
  one_patient[which(is.na(one_patient$fitted)),length(one_patient)] <- mav1[which(!is.na(mav1))][1]
  
  loess_rates <- c()
  loess_rates[1] <- 0
  for(j in 2:length(one_patient$fitted)) {
    loess_rates[j] <- one_patient$fitted[j] - one_patient$fitted[j-1]
  }
  loess_rates[1] <- loess_rates[2]
  one_patient$rates <- loess_rates
  ## find periods of stability and unstability based off chosen threshold
  temp <- length(one_patient)+1
  one_patient[which(one_patient$rates > -thres & one_patient$rates < thres), temp] <- "stable"
  one_patient[which(one_patient$rates <= -thres | one_patient$rates >= thres),temp] <- "unstable"
  colnames(one_patient)[temp] <- "change"
  one_patient[is.na(one_patient$change), temp] <- "unstable"
  ## only keep periods of stability that are sustained for x data points
  start <- as.character(one_patient$change[[1]])
  i <- 1
  k <- 1
  temp <- length(one_patient)+1
  for(j in 2:length(one_patient$change)) {
    nextj <- as.character(one_patient$change[[j]])
    if(start == nextj) {
      k <- k + 1
    }else {
      one_patient[c(i:(i + k - 1)),temp] <- k
      i <- i + k
      k <- 1
      start <- nextj
    }
  }
  one_patient[c(i:(i + k - 1)),temp] <- k
  colnames(one_patient)[temp] <- "streak"
  one_patient[which(one_patient$streak < x & one_patient$change == "stable"),temp - 1] <- "unstable"
  
  
  one_patient$change <- factor(one_patient$change, levels = c("stable", "unstable"))
  one_patient[is.na(one_patient$READING),4] <- 100
  i <- 1
  while (i <= length(one_patient[[1]])) {
    if (one_patient$change[[i]] == "stable") {
      k <- i
      while (!(is.na(one_patient$change[k])) & one_patient$change[k] == "stable") {
        k <- k + 1
      }
      one_patient2 <- one_patient[i:k-1,]
      avg <- median(one_patient2$READING, na.rm = TRUE)
      n_new <- length(one_patient2$READING)
      sd <- sd(one_patient2$READING, na.rm = TRUE)
      
      
      ## See if it is statistically different from the previous
      
      if (is.na(avg)) {
        avg <- "No baseline found"
      }else {
        if (abs(avg - bl) / (sd^2/(n_new/n_old) + .0000001)  > 2) {
          if (avg %in% baseline) {
            baseline[i:(k - 1)] <- avg + .1
            bl <- avg + .1
          }else{
            baseline[i:(k - 1)] <- avg
            bl <- avg
          }
          g <- g + 1
          group[i:(k - 1)] <- g
          n_old <- n_new
          sd_old <- sd
        }else {
          baseline[i:(k - 1)] <- bl
          group[i:(k - 1)] <- g
        }
      }
      i <- k
    }else{
      baseline[i] <- bl
      group[i] <- g
      i <- i + 1
    }
  }
  one_patient$baseline <- baseline
  one_patient <- subset(one_patient, select=c(Time, baseline))
  vitals_one_patient <- merge(vitals_one_patient, one_patient, by=c("Time", "Time"),  all.x = TRUE)
  vitals_one_patient <- unique(vitals_one_patient)
  vitals_one_patient <- vitals_one_patient[,c(2,1,3:length(vitals_one_patient))]
  for (i in 1:length(vitals_one_patient[[1]])) {
    if (is.na(vitals_one_patient$baseline[[i]]) & i == 1) {
      vitals_one_patient$baseline[[i]] <- unique(vitals_one_patient$baseline[[i]])[1]
    }
    if (is.na(vitals_one_patient$baseline[[i]]) & i != 1) {
      vitals_one_patient$baseline[[i]] <- vitals_one_patient$baseline[i-1]
    }
  }
  return(vitals_one_patient)
}
