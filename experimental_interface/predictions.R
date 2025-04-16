make_predictions <- function (id, model, train, test) {
  mod.predictions <- data.frame()
  featureg <- NA
  gplot_concern <- NA
  modelRF <- randomForest(as.factor(Event) ~ longest_streak +  Mean_Loess_Rate +  range_loess_rate + range_hb +  avg_hb + degree1 + avg_ox + mean_ox_rate + range_ox_rate + degree+ var_ox + pos_rate +  neg_rate +  dips2 + streak_low_resid + variance_of_fit ,  data = train)
  result <- predict(modelRF, newdata = test, type = "prob")
  mod.predictions <- cbind(as.numeric(result[[2]]), "RF")
  colnames(mod.predictions) <- c("Prob Ert", "Mod")
  mod.predictions <- as.data.frame(mod.predictions)
  mod.predictions$`Prob Ert` <- as.numeric(as.character(mod.predictions$`Prob Ert`))
  mod.predictions$Mod <- as.character(mod.predictions$Mod)
  modelLM <- glm(Event ~avg_hb + streak_low_resid   + neg_rate  + Mean_Loess_Rate  , data = train, family = binomial)
  mod.predictions[2,1] <- predict(modelLM, newdata = test, type = "response")[[1]]
  mod.predictions[2,2] <- "LR"
  mod.predictions$Mod <- factor(mod.predictions$Mod, levels= c("RF", "LR"))
  mod.predictions$`Prob Ert` <- round(mod.predictions$`Prob Ert`, 2)
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
  weights$V1 <- c("Avg HB", "Delta HR", "Delta Oximetry", "Low SD (Loess) HR")
  colnames(weights) <- c("Feature", "Weight")
  table <- weights
  return(mod.predictions)
}
plot_predictions <- function(mod.predictions, conditionID) {
  fillcolor <- c("blue","red")
  names(fillcolor) <- c("RF","LR")
  gplot_concern <- ggplot(data = mod.predictions[get_modelsON(conditionID),]) + geom_bar(aes(x = Mod, y = `Prob Ert`, fill = Mod), stat = "identity", width = 1)  +
    scale_y_continuous(limits = c(0, 1), expand = expand_scale(mult = c(0, 0)), labels = scales::percent_format()) +
    #scale_x_continuous(limits = c(0, 1), expand = expand_scale(mult = c(0, 0))) +
    geom_hline(aes(yintercept=.50), size = 2) +
    scale_fill_manual(values = fillcolor) +
    ggtitle("Prediction") +
    ylab("Probability of ERT") + xlab("Algorithm") +
    theme(
      axis.text=element_text(size = 14),
      axis.title=element_text(size = 16,face = "bold"),
      plot.background = element_blank(),
      #plot.margin=unit(c(1,1,-0.5,1), "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      #axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    ) 
  if (get_modelsON(conditionID)[1]) {
    if (mod.predictions$`Prob Ert`[[1]] >  .05)  {
      gplot_concern <- gplot_concern +  geom_text(data = mod.predictions[1,], aes(x = Mod,y = `Prob Ert` - .04 ,label = scales::percent(`Prob Ert`)),vjust = 0, col = "white", cex = 6) 
    }else {
      gplot_concern <- gplot_concern +  geom_text(data = mod.predictions[1,], aes(x = Mod,y = `Prob Ert` + .01 ,label = scales::percent(`Prob Ert`)),vjust = 0, col = "black", cex = 6) 
    }
  }
  if (get_modelsON(conditionID)[2]) {
    if (mod.predictions$`Prob Ert`[[2]] > .05 ) {
      gplot_concern <- gplot_concern +  geom_text(data = mod.predictions[2,], aes(x = Mod,y = `Prob Ert` - .04 ,label = scales::percent(`Prob Ert`)),vjust = 0, col = "white", cex = 6) 
    }else {
      gplot_concern <- gplot_concern +  geom_text(data = mod.predictions[2,], aes(x = Mod,y = `Prob Ert` + .01 ,label = scales::percent(`Prob Ert`)),vjust = 0, col = "black", cex = 6) 
    }
  }
  return(gplot_concern)
}