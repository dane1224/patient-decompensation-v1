make_demo_table <- function(id, endtime) {
  
  this.patient <- patients[which(patients$Identity == strsplit(id,split = " ")[[1]][1]),]
  this.LOS <- round(endtime - this.patient$HOSP_ADMSN_TIME_S, 1)
  this.comorbid <- comorbids[comorbids$CaseID == id,]
  this.comorbid <- this.comorbid[order(this.comorbid$Date, decreasing = TRUE),]
  this.comorbid <- this.comorbid[this.comorbid$Date < endtime,]
  for (i in 1:nrow(this.comorbid)) {
    this.comorbid[i,"id"] <- i
  }
  this.diagnosis <- diagnosis[diagnosis$CaseID == id,]
  print(this.diagnosis)
  print(this.comorbid)
  this.comorbid[1+nrow(this.comorbid),] <- c(this.diagnosis, 0)
  print(this.comorbid)
  this.comorbid$Date[this.comorbid$id == 0] <- NA
  this.comorbid$Label <- NA
  this.comorbid$Label[this.comorbid$id == 0] <- "Primary Diagnosis:"
  
  demo.summary <- ggplot(data.frame(x = 0, y = 0, lab = paste("Age:", this.patient$AGE_AT_ADM, ", LOS:", this.LOS)), aes(x,y, label = lab)) + 
    geom_text(size = 5) + 
    labs(x = NULL, y = "Info", size = 14) +
    scale_x_continuous(limits = c(-1,1), expand = expand_scale(mult = c(0,0))) + 
    scale_y_continuous(limits = c(-1,1), expand = expand_scale(mult = c(0,0))) +
    theme(axis.title = element_text(size = 16, face = "bold"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  demo.table <- ggplot(data = this.comorbid[this.comorbid$id <= 15,], aes(x = NULL, y = 50)) +
    facet_grid(id~., switch = "y") +
    geom_hline(yintercept = 0, size = 1) +
    geom_text(aes(x = -0.1, y = 50, label = Date), hjust = 1) +
    geom_text(aes(x = -0.1, y = 50, label = Label), hjust = 1) +
    geom_text(aes(x = 0, y = 50, label = Comorbidity), hjust = 0) +
    labs(y = "Comorbidities") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_text(colour = "white"),
          axis.text.y.right = element_text(colour = "white"),
          strip.text.y.left = element_blank(),
          axis.ticks.y = element_line(colour = "white"),
          axis.ticks.y.right = element_line(colour = "white"),
          panel.spacing.y = unit(0,"lines"), 
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_x_continuous(limits = c(-0.5, 2)) +
    scale_y_continuous(breaks = c(50,100), expand = c(0,0), limits = c(0, 100), sec.axis = sec_axis(~., breaks = c(50,100))) 
  
  return(list(demo.table, demo.summary))
}
prep_patients <- function(file.id) {
  patients <- read.csv(file.id)
  patients$HOSP_ADMSN_TIME_S <- as.POSIXct(patients$HOSP_ADMSN_TIME_S, format = "%Y-%m-%d %H:%M:%S")
  patients <- patients[,2:ncol(patients)]
  return(patients)
}
prep_comorbids <- function(file.id) {
  comorbids <- read.csv(file.id)
  comorbids$Date <- as.POSIXct(comorbids$Date, format = "%Y-%m-%d")
  return(comorbids)
}
prep_diagnosis <- function(file.id) {
  diagnosis <- read.csv(file.id)
  diagnosis$Date <- as.POSIXct(diagnosis$Date, format = "%Y-%m-%d")
  return(diagnosis)
}