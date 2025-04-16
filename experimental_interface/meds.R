make_meds_graph <- function(meds, id, xmax, xmin) {
  meds2 <- meds[which(meds$Identity == strsplit(id,split = " ")[[1]][1]),]
  meds2 <- meds2[which(meds2$Time_Start <= xmax & meds2$Time_Start >= xmax -24*60*60),]
  meds2 <- meds2[order(meds2$Time_Start, decreasing = TRUE),]
  for (i in 1:nrow(meds2)) {
    meds2[i,"mlab"] <- paste(gsub("[\r\n]","",meds2$Medication[i])," ")
  }
  
  # Artificially set Time_End 60*x-minutes after Time_Start
  meds2$Time_End <- meds2$Time_Start + 60
  meds2$Time_End_Badge <- meds2$Time_Start + 60*10
  
  # n <- nrow(as.data.frame(meds2$Medication))
  
  #Set number of points on distribution - 30 makes a relativly smooth curve
  # dist_length <- 30
  # graph_primer <- function(meds, n, dist_length) {
  #   #Meds2 should be fed into this function for shiny
  #   graph_data <- data.frame()
  #   temp <- data.frame(hold = numeric(30))
  #   diff <- c()
  #   for (i in 1:n) {
  #     diff[i] <- as.numeric(abs(meds$Time_Start[i]-meds$Time_End[i]))
  #     assign(paste("x_", i, sep = ""), seq(0, diff[i], length=dist_length))
  #     assign(paste("y_", i, sep = ""), dnorm(get(paste("x_", i, sep = "")), mean=diff[i]/2, sd=1))
  #     assign(paste("y_", i, sep = ""), (get(paste("y_", i, sep = ""))-min(get(paste("y_", i, sep = ""))))/(max(get(paste("y_", i, sep = "")))-min(get(paste("y_", i, sep = "")))))
  #     assign(paste("p_", i, sep = ""), as.numeric((abs(meds$Time_Start[i]-meds$Time_End[i]))/dist_length))
  #     assign(paste("df_", i, sep = ""), data.frame(hold = numeric(dist_length)))
  #     for (j in 1:dist_length) {
  #       temp$Time[j] <- meds$Time_Start[i] + 3600*(j-1)*get(paste("p_", i, sep = ""))
  #       temp$Value[j] <- get(paste("y_", i, sep = ""))[j]
  #       temp$Medication[j] <- meds$Medication[i]
  #       temp$Patient[j] <- meds$Identity[i]
  #       temp$hold <- NULL
  #     }
  #     temp$Time <- as.POSIXct(temp$Time, origin = "1970-01-01")
  #     assign(paste("df_", i, sep = ""), temp)
  #     if (i == 1) {
  #       for (j in 1:dist_length) {
  #         graph_data[j,1] <- meds$Time_Start[i] + 3600*(j-1)*get(paste("p_", i, sep = ""))
  #         graph_data[j,2] <- get(paste("y_", i, sep = ""))[j]
  #         graph_data[j,3] <- meds$Medication[i]
  #         graph_data[j,4] <- meds$Identity[i]
  #       }
  #     } else {
  #       for (j in ((i-1)*dist_length+1):(i*dist_length)) {
  #         graph_data[j,1] <- meds$Time_Start[i] + 3600*(j-((i-1)*dist_length+1))*get(paste("p_", i, sep = ""))
  #         graph_data[j,2] <- get(paste("y_", i, sep = ""))[(j-((i-1)*dist_length))]
  #         graph_data[j,3] <- meds$Medication[i]
  #         graph_data[j,4] <- meds$Identity[i]
  #       }
  #     }
  #   }
  #   colnames(graph_data) <- c("Time", "Value", "Medication", "Patient")
  #   graph_data$Time <- as.POSIXct(graph_data$Time, origin = "1970-01-01")
  #   graphs <- list(graph_data)
  #   for (i in 1:n) {
  #     graphs <- list.append(graphs, get(paste("df_", i, sep = "")))
  #   }
  #   return (graphs)
  # }
  
  #Check if there are meds for time given / meds2 should be used here in shiny
  if (nrow(meds2) > 0) {
    # meds3 <- graph_primer(meds2,n,dist_length)
    p <- ggplot(meds2, aes(Time_Start, NULL)) + 
      facet_grid(Medication~., switch = "y") + 
      geom_hline(yintercept = 0, size = 1) + 
      # geom_vline(aes(xintercept = Time_Start), size = 10) + 
      geom_rect(aes(xmin = Time_Start, xmax = Time_End, ymin = 0, ymax = 100), fill = "black") +
      geom_text(aes(x = Time_Start, y = 50, label = mlab), hjust = 1) +
      labs(y = "Medications") +
      theme(axis.title.x = element_blank(),
            axis.text.y = element_text(colour = "white"),
            axis.text.y.right = element_text(colour = "white"),
            # strip.text.y.left = element_text(face = "bold", angle = 0),
            strip.text.y.left = element_blank(),
            axis.ticks.y = element_blank(),
            axis.ticks.y.right = element_blank(),
            panel.spacing.y = unit(0,"lines"), 
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) + 
      scale_y_continuous(breaks = c(100, 200), expand = c(0,0), limits = c(0, 110), sec.axis = sec_axis(~., breaks = c(100, 200))) 
    
    p_badge <- ggplot(meds2, aes(Time_Start, NULL)) + 
      facet_grid(Medication~., switch = "y") + 
      geom_hline(yintercept = 0, size = 1) + 
      # geom_vline(aes(xintercept = Time_Start), size = 10) + 
      geom_rect(aes(xmin = Time_Start, xmax = Time_End_Badge, ymin = 0, ymax = 1), fill = "black") +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            # strip.text.y.left = element_text(face = "bold", angle = 0),
            strip.text.y.left = element_blank(),
            panel.spacing.y = unit(0,"lines"), 
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank()) + 
      scale_y_continuous(expand = c(0,0), limits = c(0,1.1)) 
    
    # for (i in 2:(n+1)) {
    #   p <- p + geom_ribbon(data = meds3[[i]], aes(x = Time, ymax = Value), ymin=0, fill="steelblue1", alpha=0.5)
    # }
    # 
    # for (i in 2:(n+1)) {
    #   p <- p + geom_line(data = meds3[[i]], aes(x=Time, y = Value))
    # }
  } else {
    x <- seq(xmin, xmax, by = 60)
    x <- as.data.frame(x)
    x$val <- 0
    p <- ggplot(x , aes(x, val)) + 
      labs(y = "MEDS", x = NULL) +
      scale_y_continuous(limits = c(80,110), breaks = seq(80,110,10), minor_breaks = seq(80,110,5), expand = expand_scale(mult = c(0,0))) +
      theme(
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.background = element_blank(),
        #plot.margin=unit(c(1,1,-0.5,1), "cm"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(color="white"),
        legend.position = "none"
      ) 
    
    p_badge <- ggplot(x , aes(x, val)) + 
      labs(y = "MEDS", x = NULL) +
      scale_y_continuous(limits = c(80,110), breaks = seq(80,110,10), minor_breaks = seq(80,110,5), expand = expand_scale(mult = c(0,0))) +
      theme(
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.background = element_blank(),
        #plot.margin=unit(c(1,1,-0.5,1), "cm"),
        panel.grid.major = element_line(),
        panel.grid.minor = element_line(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(color="white"),
        legend.position = "none"
      ) 
  }
  
  return(list(p, p_badge))
}