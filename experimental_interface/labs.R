get_previous_date_reading <- function(labs_df,comp_name, reading_date) {
  labs_df_temp <- labs_df %>%
    filter(COMPONENT_NAME == comp_name) %>%
    arrange(RESULT_TIME_S)
  
  #Will be NA if no previous reading
  return_reading <- NA
  
  if(reading_date != min(labs_df_temp$RESULT_TIME_S)) {
    return_date <- labs_df_temp$RESULT_TIME_S[which(labs_df_temp$RESULT_TIME_S == reading_date) - 1]
    return_reading <- labs_df_temp %>%
      filter(RESULT_TIME_S == return_date) %>%
      pull(ORD_VALUE)
  }
  
  return(return_reading)
}


#####HELPER FUNCTION#####
most_recent_reading <- function(labs_df) {
  labs_df <- labs_df %>%
    group_by(COMPONENT_NAME) %>%
    filter(RESULT_TIME_S == max(RESULT_TIME_S)) %>%
    ungroup()
}

plot_labs <- function(MIN_TIME ,TIME, PATIENT_ID, DATA) {
  p <- ggplot() + geom_point(aes(x=1,y=1)) +
    labs(y = "LABS", x = NULL) +
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
  p1 <- ggplot() + geom_point(aes(x=1,y=1)) +
    labs(y = "LABS", x = NULL) +
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
  #Filter for the Specific Patient ID
  plot_df <- DATA %>%
    filter(Identity == strsplit(PATIENT_ID, split = " ")[[1]][1])
  
  if (length(plot_df[[1]]) > 0) {
    #Subset for specific result time desired - less than or equal to time desired
    plot_df <- plot_df %>%
      filter(RESULT_TIME_S <= TIME )
    if (length(plot_df[[1]]) > 0) {
      
      #Subset for most recent reading for each
      plot_df <- most_recent_reading(plot_df)
      
      #Subset relevant columns
      plot_df <- plot_df %>%
        select(PROC_NAME, COMPONENT_NAME, ORD_VALUE, RESULT_TIME_S, LOWER.LIMIT, UPPER.LIMIT, CRITICAL.LOWER.LIMIT, CRITICAL.UPPER.LIMIT, COMPONENT_MIN, COMPONENT_MAX)
      
      #Rename "ORD_VALUE" to "CURRENT_ORD_VALUE"
      colnames(plot_df)[which(colnames(plot_df) == "ORD_VALUE")] <- "CURRENT_ORD_VALUE"
      
      #Define "PREVIOUS_ORD_VALUE" to be the previous reading
      plot_df$PREVIOUS_ORD_VALUE <- rep(NA, nrow(plot_df))
      for(component in unique(plot_df$COMPONENT_NAME)) {
        
        previous_ord <- get_previous_date_reading(labs_df = labs%>%filter(Identity == strsplit(PATIENT_ID, split = " ")[[1]][1]), comp_name =  component, reading_date =  plot_df[which(plot_df$COMPONENT_NAME == component),]$RESULT_TIME_S)
        if(length(previous_ord) > 0) {
          plot_df[which(plot_df$COMPONENT_NAME == component),]$PREVIOUS_ORD_VALUE <- previous_ord[1]
        }
      }
      
      #Factor Component_Name
      plot_df$COMPONENT_NAME <- factor(plot_df$COMPONENT_NAME)
      
      #Make COMPONENT_NAME numeric
      component_num_seq <- seq(0,(nrow(plot_df)-1)*60, 60)
      plot_df <- plot_df %>%
        mutate(COMPONENT_NAME_NUM = component_num_seq)
      
      #Determine categories of previous and current ord values
      plot_df <- plot_df %>%
        mutate(PREVIOUS_CATEGORY = NA) %>%
        mutate(PREVIOUS_CATEGORY = ifelse(PREVIOUS_ORD_VALUE <= CRITICAL.LOWER.LIMIT, "CRITICAL_LOW", PREVIOUS_CATEGORY)) %>%
        mutate(PREVIOUS_CATEGORY = ifelse(((PREVIOUS_ORD_VALUE <= LOWER.LIMIT) & (PREVIOUS_ORD_VALUE > CRITICAL.LOWER.LIMIT)), "LOW", PREVIOUS_CATEGORY)) %>%
        mutate(PREVIOUS_CATEGORY = ifelse(((PREVIOUS_ORD_VALUE > LOWER.LIMIT) & (PREVIOUS_ORD_VALUE < UPPER.LIMIT)), "NORMAL", PREVIOUS_CATEGORY)) %>%
        mutate(PREVIOUS_CATEGORY = ifelse(((PREVIOUS_ORD_VALUE >= UPPER.LIMIT) & (PREVIOUS_ORD_VALUE < CRITICAL.UPPER.LIMIT)), "HIGH", PREVIOUS_CATEGORY)) %>%
        mutate(PREVIOUS_CATEGORY = ifelse(PREVIOUS_ORD_VALUE >= CRITICAL.UPPER.LIMIT, "CRITICAL_HIGH", PREVIOUS_CATEGORY)) %>%
        mutate(CURRENT_CATEGORY = NA) %>%
        mutate(CURRENT_CATEGORY = ifelse(CURRENT_ORD_VALUE <= CRITICAL.LOWER.LIMIT, "CRITICAL_LOW", CURRENT_CATEGORY)) %>%
        mutate(CURRENT_CATEGORY = ifelse(((CURRENT_ORD_VALUE <= LOWER.LIMIT) & (CURRENT_ORD_VALUE > CRITICAL.LOWER.LIMIT)), "LOW", CURRENT_CATEGORY)) %>%
        mutate(CURRENT_CATEGORY = ifelse(((CURRENT_ORD_VALUE > LOWER.LIMIT) & (CURRENT_ORD_VALUE < UPPER.LIMIT)), "NORMAL", CURRENT_CATEGORY)) %>%
        mutate(CURRENT_CATEGORY = ifelse(((CURRENT_ORD_VALUE >= UPPER.LIMIT) & (CURRENT_ORD_VALUE < CRITICAL.UPPER.LIMIT)), "HIGH", CURRENT_CATEGORY)) %>%
        mutate(CURRENT_CATEGORY = ifelse(CURRENT_ORD_VALUE >= CRITICAL.UPPER.LIMIT, "CRITICAL_HIGH", CURRENT_CATEGORY))
  
      #Normalize values
      plot_df <- plot_df  %>%
        mutate(NORMALIZED_PREVIOUS_ORD_VALUE = NA) %>%
        mutate(NORMALIZED_PREVIOUS_ORD_VALUE = ifelse(PREVIOUS_CATEGORY == "CRITICAL_LOW", yes = 10*(-3 + ((PREVIOUS_ORD_VALUE - (COMPONENT_MIN)) / ((CRITICAL.LOWER.LIMIT+0.0001) - (COMPONENT_MIN)))), no = NORMALIZED_PREVIOUS_ORD_VALUE)) %>%
        mutate(NORMALIZED_PREVIOUS_ORD_VALUE = ifelse(PREVIOUS_CATEGORY == "LOW", yes = 10*(-2 + ((PREVIOUS_ORD_VALUE - (CRITICAL.LOWER.LIMIT)) / ((LOWER.LIMIT+0.0001) - (CRITICAL.LOWER.LIMIT + 0.0001)))), no = NORMALIZED_PREVIOUS_ORD_VALUE)) %>%
        mutate(NORMALIZED_PREVIOUS_ORD_VALUE = ifelse(PREVIOUS_CATEGORY == "NORMAL", yes = 10*(0 + ((PREVIOUS_ORD_VALUE - (LOWER.LIMIT)) / ((UPPER.LIMIT+0.0001) - (LOWER.LIMIT + 0.0001)))), no = NORMALIZED_PREVIOUS_ORD_VALUE)) %>%
        mutate(NORMALIZED_PREVIOUS_ORD_VALUE = ifelse(PREVIOUS_CATEGORY == "HIGH", yes = 10*(1 + ((PREVIOUS_ORD_VALUE - (UPPER.LIMIT)) / ((CRITICAL.UPPER.LIMIT+0.0001) - (UPPER.LIMIT + 0.0001)))), no = NORMALIZED_PREVIOUS_ORD_VALUE)) %>%
        mutate(NORMALIZED_PREVIOUS_ORD_VALUE = ifelse(PREVIOUS_CATEGORY == "CRITICAL_HIGH", yes = 10*(2 + ((PREVIOUS_ORD_VALUE - (CRITICAL.UPPER.LIMIT)) / ((COMPONENT_MAX+0.0001) - (CRITICAL.UPPER.LIMIT + 0.0001)))), no = NORMALIZED_PREVIOUS_ORD_VALUE)) %>%
        mutate(NORMALIZED_CURRENT_ORD_VALUE = NA) %>%
        mutate(NORMALIZED_CURRENT_ORD_VALUE = ifelse(CURRENT_CATEGORY == "CRITICAL_LOW", yes = 10 * (-3 + ((CURRENT_ORD_VALUE - (COMPONENT_MIN)) / ((CRITICAL.LOWER.LIMIT+0.0001) - (COMPONENT_MIN)))), no = NORMALIZED_CURRENT_ORD_VALUE))%>%
        mutate(NORMALIZED_CURRENT_ORD_VALUE = ifelse(CURRENT_CATEGORY == "LOW", yes = 10*(-2 + ((CURRENT_ORD_VALUE - (CRITICAL.LOWER.LIMIT)) / ((LOWER.LIMIT+0.0001) - (CRITICAL.LOWER.LIMIT + 0.0001)))), no = NORMALIZED_CURRENT_ORD_VALUE)) %>%
        mutate(NORMALIZED_CURRENT_ORD_VALUE = ifelse(CURRENT_CATEGORY == "NORMAL", yes =10*( 0 + ((CURRENT_ORD_VALUE - (LOWER.LIMIT)) / ((UPPER.LIMIT+0.0001) - (LOWER.LIMIT + 0.0001)))), no = NORMALIZED_CURRENT_ORD_VALUE)) %>%
        mutate(NORMALIZED_CURRENT_ORD_VALUE = ifelse(CURRENT_CATEGORY == "HIGH", yes = 10*(1 + ((CURRENT_ORD_VALUE - (UPPER.LIMIT)) / ((CRITICAL.UPPER.LIMIT+0.0001) - (UPPER.LIMIT + 0.0001)))), no = NORMALIZED_CURRENT_ORD_VALUE)) %>%
        mutate(NORMALIZED_CURRENT_ORD_VALUE = ifelse(CURRENT_CATEGORY == "CRITICAL_HIGH", yes = 10*(2 + ((CURRENT_ORD_VALUE - (CRITICAL.UPPER.LIMIT)) / ((COMPONENT_MAX+0.0001) - (CRITICAL.UPPER.LIMIT + 0.0001)))), no = NORMALIZED_CURRENT_ORD_VALUE))
      
      #This is a 1 or -1 to determine if next ord value less than or greater than ord_value for geom_rect purposes
      plot_df <- plot_df %>%
        mutate(ORIGINAL_RECT_FACTOR = ifelse(test = NORMALIZED_CURRENT_ORD_VALUE > NORMALIZED_PREVIOUS_ORD_VALUE, yes = 1, no = -1)) %>%
        mutate(ORIGINAL_RECT_FACTOR = ifelse(test = NORMALIZED_CURRENT_ORD_VALUE == NORMALIZED_PREVIOUS_ORD_VALUE, yes = 0, no = ORIGINAL_RECT_FACTOR))
      
      #If the difference doesn't meet the min threshold, don't draw a rectangle, add an adjustment for text
      min_threshold <- 1.0
      plot_df <- plot_df %>%
        mutate(ADJUSTED_RECT_FACTOR = ifelse(test = (abs(NORMALIZED_CURRENT_ORD_VALUE - NORMALIZED_PREVIOUS_ORD_VALUE) >= min_threshold), yes = ORIGINAL_RECT_FACTOR, no = 0))
      
      #Text adjust
      plot_df <- plot_df %>%
        mutate(text_adjust = ifelse(test = ADJUSTED_RECT_FACTOR == 0, yes = 3*ORIGINAL_RECT_FACTOR, no = 0)) %>%
        mutate(text_adjust = ifelse(test = (ADJUSTED_RECT_FACTOR == 0) & (text_adjust == 0), yes = -2, no = text_adjust))
      
      #Reorder based on PROC_NAME
      
      #Add a column for text colour depending on CURRENT_CATEGORY
      plot_df <- plot_df %>%
        mutate(text_color = ifelse(CURRENT_CATEGORY == "NORMAL", "black", ifelse(CURRENT_CATEGORY == "LOW" | CURRENT_CATEGORY == "HIGH", "blue", "white")))
      text_color <- plot_df$text_color
      
      ################################################
      
      fill.values <- c("blue", "grey50", "grey50", "grey50", "blue")
      names(fill.values) <- c("CRITICAL_LOW", "LOW", "NORMAL", "HIGH", "CRITICAL_HIGH")
      color.values <- c("blue", "blue", "grey50", "blue", "blue")
      names(color.values) <- c("CRITICAL_LOW", "LOW", "NORMAL", "HIGH", "CRITICAL_HIGH")
      alpha.values <- c(1, 0, 0, 0, 1)
      names(alpha.values) <- c("CRITICAL_LOW", "LOW", "NORMAL", "HIGH", "CRITICAL_HIGH")
      
      #Plot
      p <- ggplot(data = plot_df, aes(x = COMPONENT_NAME_NUM, y = NORMALIZED_CURRENT_ORD_VALUE)) +
        scale_fill_manual(values = fill.values) +
        scale_color_manual(values = color.values) +
        scale_alpha_manual(values = alpha.values) +
        geom_hline(yintercept = -20, linetype = "longdash") +
        geom_hline(yintercept = -10, linetype = "dotted") +
        geom_hline(yintercept = 10, linetype = "dotted") +
        geom_hline(yintercept = 20, linetype = "longdash") +
        #This geom_rect is for current_value
        geom_rect(aes(xmin = (COMPONENT_NAME_NUM - 25), xmax = (COMPONENT_NAME_NUM + 25), ymin = NORMALIZED_CURRENT_ORD_VALUE-1.5, ymax= (NORMALIZED_CURRENT_ORD_VALUE +1.5), fill = CURRENT_CATEGORY, color = CURRENT_CATEGORY, alpha = CURRENT_CATEGORY), linetype=1) +
        geom_text(aes(label = CURRENT_ORD_VALUE), colour = text_color, fontface = "bold") +
        #This geom_rect is segment to previous value
        geom_rect(aes(xmin = (COMPONENT_NAME_NUM-5)*abs(ADJUSTED_RECT_FACTOR), xmax = (COMPONENT_NAME_NUM+5)*abs(ADJUSTED_RECT_FACTOR), ymin = (NORMALIZED_CURRENT_ORD_VALUE-ADJUSTED_RECT_FACTOR*1.5)*abs(ADJUSTED_RECT_FACTOR), ymax = (NORMALIZED_PREVIOUS_ORD_VALUE)*abs(ADJUSTED_RECT_FACTOR), fill = "grey50"),  color = "grey50") +
        geom_text(aes(x = COMPONENT_NAME_NUM, y = NORMALIZED_PREVIOUS_ORD_VALUE-1.5*ADJUSTED_RECT_FACTOR - text_adjust, label = PREVIOUS_ORD_VALUE)) +
        coord_flip() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_blank()) +
        guides(fill = FALSE, color = FALSE, alpha = FALSE) +
        xlab("Lab Name") +
        scale_x_continuous(breaks = component_num_seq, labels = unique(as.character(plot_df$COMPONENT_NAME))) +
        ylim(-45,35) +
        scale_y_continuous(expand = c(0,0)) +
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor.x = element_blank()) +
        theme(panel.grid.minor.y = element_line(colour = "white")) +
        theme(axis.text.y = element_text(face = "bold")) +
        theme(panel.background = element_rect(color = "grey10")) +
        geom_text(x = max(plot_df$COMPONENT_NAME_NUM)+1.5*(plot_df$COMPONENT_NAME_NUM[2] - plot_df$COMPONENT_NAME_NUM[1]), y = 20, label = "Critically High", fontface = "bold") +
        geom_text(x = max(plot_df$COMPONENT_NAME_NUM)+1.5*(plot_df$COMPONENT_NAME_NUM[2] - plot_df$COMPONENT_NAME_NUM[1]), y = 10, label = "High", fontface = "bold") +
        geom_text(x = max(plot_df$COMPONENT_NAME_NUM)+1.5*(plot_df$COMPONENT_NAME_NUM[2] - plot_df$COMPONENT_NAME_NUM[1]), y = -10, label = "Low", fontface = "bold") +
        geom_text(x = max(plot_df$COMPONENT_NAME_NUM)+1.5*(plot_df$COMPONENT_NAME_NUM[2] - plot_df$COMPONENT_NAME_NUM[1]), y = -20, label = "Critically Low", fontface = "bold") +
        #Geom_rect - fill as same critical/normal/low/high color category
        geom_rect(aes(xmin = COMPONENT_NAME_NUM - 30, xmax = COMPONENT_NAME_NUM + 30, ymin = -45, ymax = -35, fill = CURRENT_CATEGORY, alpha = CURRENT_CATEGORY)) +
        #Geom_text for current reading
        geom_text(aes(x = COMPONENT_NAME_NUM, y = -40, label = CURRENT_ORD_VALUE), colour = text_color, fontface = "bold") +
        #Geom_text for 'Current Category'
        geom_text(aes(x = max(plot_df$COMPONENT_NAME_NUM)+1.5*(plot_df$COMPONENT_NAME_NUM[2] - plot_df$COMPONENT_NAME_NUM[1]),y = -40, label = "Reading")) +
        #Just to fix x boundaries
        geom_text(aes(x = COMPONENT_NAME_NUM, y = 35, label = "")) +
        #geom_rect and text for current ord_value display
        geom_hline(yintercept = -35, linetype = "solid", color = "darkgrey")
      
      p1 <- ggplot(data = plot_df, aes(x = COMPONENT_NAME_NUM, y = NORMALIZED_CURRENT_ORD_VALUE)) +
        scale_fill_manual(values = fill.values) +
        scale_color_manual(values = color.values) +
        scale_alpha_manual(values = alpha.values) +
        geom_hline(yintercept = -20, linetype = "longdash") +
        geom_hline(yintercept = -10, linetype = "dotted") +
        geom_hline(yintercept = 10, linetype = "dotted") +
        geom_hline(yintercept = 20, linetype = "longdash") +
        #This geom_rect is for current_value
        geom_rect(aes(xmin = (COMPONENT_NAME_NUM - 25), xmax = (COMPONENT_NAME_NUM + 25), ymin = NORMALIZED_CURRENT_ORD_VALUE-1.5, ymax= (NORMALIZED_CURRENT_ORD_VALUE +1.5), fill=CURRENT_CATEGORY, color = CURRENT_CATEGORY, alpha = CURRENT_CATEGORY), linetype=1) +
        #This geom_rect is segment to previous value
        geom_rect(aes(xmin = (COMPONENT_NAME_NUM-5)*abs(ADJUSTED_RECT_FACTOR), xmax = (COMPONENT_NAME_NUM+5)*abs(ADJUSTED_RECT_FACTOR), ymin = (NORMALIZED_CURRENT_ORD_VALUE-ADJUSTED_RECT_FACTOR*1.5)*abs(ADJUSTED_RECT_FACTOR), ymax = (NORMALIZED_PREVIOUS_ORD_VALUE)*abs(ADJUSTED_RECT_FACTOR), fill = "grey50"),  color = "grey50") +
        coord_flip() +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        guides(fill = FALSE, color = FALSE, alpha = FALSE) +
        xlab("Lab Name") +
        scale_x_continuous(breaks = component_num_seq,labels = unique(as.character(plot_df$COMPONENT_NAME))) +
        ylim(-45,35) +
        scale_y_continuous(expand = c(0,0)) +
        theme(panel.grid.major = element_blank()) +
        theme(panel.grid.minor.x = element_blank()) +
        theme(panel.grid.minor.y = element_line(colour = "white"))+
        theme(axis.text.y = element_text(face = "bold")) +
        theme(panel.background = element_rect(color = "grey")) +
        #geom_rect and text for current ord_value display
        geom_hline(yintercept = -35, linetype = "solid", color = "darkgrey") +
        #Just to fix x boundaries
        geom_text(aes(x = COMPONENT_NAME_NUM, y = 35, label = "")) 
    }
  }
  return(list(p,p1))
}
