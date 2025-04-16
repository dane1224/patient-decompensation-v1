Add_Alarms <- function(alarms, gplot_hr, gplot_oximetry) {
  if (length(alarms[[1]]) > 0) {
    alarms_hr <-  alarms[which(!grepl("SPO2",alarms$Label)),]
    if (length(alarms_hr[[1]]) > 0) {
      split <- strsplit(alarms_hr$Label, split = " ")
      inds <- c()
      alarms_hr$Value <- NA
      for (ind in 1:length(alarms_hr[[1]])) {
        if (grepl("[[:digit:]]", alarms_hr[ind,2])) {
          inds <- append(inds,ind)
          alarms_hr$Value[ind] <- as.numeric(split[[ind]][length(split[[ind]])])
        }
      }
      if (!is.null(inds)) {
        withNum <- alarms_hr[inds, ]
        withOutNum <- alarms_hr[-inds, ]
      } else {
        withNum <- alarms_hr[which(alarms_hr$Label == "Wacky Tacky"),]
        withOutNum <- alarms_hr
      }
      if (length(withOutNum[[1]]) > 0) {
        gplot_hr <- gplot_hr + geom_rect(data = withOutNum, aes(xmin = Start, xmax = End, ymin = 0, ymax = 28), fill = "royalblue3", color = "blue", alpha = .4, inherit.aes = FALSE) +
          geom_segment(data = withOutNum, aes(x = Start, y = 0, yend = 28, xend = Start), color = "blue") 
      }
      if (length(withNum[[1]]) > 0) {
        withNum$Value <- as.numeric(withNum$Value)
        gplot_hr <- gplot_hr + geom_point(data = withNum, aes(Time, Value), shape = 8, size = 3 , color = "blue", alpha = .5) +
          geom_segment(data = withNum, aes(x = Start, y = Value, yend = Value, xend = End), color = "blue", size = 1)
      }
    }
  }
  if (length(alarms[[1]]) > 0) {
    alarms_sp <-  alarms[which(grepl("SPO2",alarms$Label)),]
    if (length(alarms_sp[[1]]) > 0) {
      split <- strsplit(alarms_sp$Label, split = " ")
      inds <- c()
      for (ind in 1:length(split)) {
        if (length(split[[ind]]) > 2) {
          inds <- append(inds, ind)
        }
      }
      if (!is.null(inds)) {
        withNum <- alarms_sp[inds, ]
        withOutNum <- alarms_sp[-inds, ]
      } else {
        withNum <- alarms_hr[which(alarms_hr$Label == "Wacky Tacky"),]
        withOutNum <- alarms_sp
      }
      if (length(withOutNum[[1]]) > 0) {
        gplot_oximetry <- gplot_oximetry + geom_rect(data = withOutNum, aes(xmin = Start, xmax = End, ymin = 39.6, ymax = 49.25), fill = "royalblue3", color = "blue", alpha = .4, inherit.aes = FALSE) +
          geom_segment(data = withOutNum, aes(x = Start, y = 39.6, yend = 49.25, xend = Start), color = "blue") 
      }
      if (length(withNum[[1]]) > 0) {
        withNum$Value <- t(as.data.frame(strsplit(withNum$Label, split = " "))[3, ])
        withNum$Value <- as.numeric(as.character(withNum$Value))
        gplot_oximetry <- gplot_oximetry + geom_point(data = withNum, aes(Time, Value), shape = 8, size = 3 , color = "blue", alpha = .5) +
          geom_segment(data = withNum, aes(x = Start, y = Value, yend = Value, xend = End), color = "blue", size = 1)
      }
    }
  }
  list <- c()
  list[[1]] <- gplot_hr
  list[[2]] <- gplot_oximetry 
  return(list)
}