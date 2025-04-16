library(stringr)
library(stats)
library(admisc)

anova.plus <- function(mod.anova) {
  for (i in rownames(mod.anova)) {
    is.parent <- FALSE
    for (j in rownames(mod.anova)) {
      if (i != j) {
        is.match <- TRUE
        for (k in strsplit(i,":")[[1]]) {
          is.match <- is.match & grepl(k,j)
        }
        is.parent <- is.parent | is.match
      }
    }
    mod.anova[i,"parent"] <- is.parent
  }
  return(mod.anova)
}

is.parent <- function(term.list) {
  parent.list <- NULL
  for (i in term.list) {
    this.parent <- FALSE
    for (j in term.list) {
      if (i != j) {
        is.match <- TRUE
        for (k in strsplit(i,":")[[1]]) {
          is.match <- is.match & grepl(k,j)
        }
        this.parent <- this.parent | is.match
      }
    }
    parent.list <- c(parent.list, this.parent)
  }
  return(parent.list)
}

backward.step <- function (data, mod.function, dependent, independents) {
  dat <- data
  mod <- mod.function(as.formula(paste(dependent,"~",format.terms(independents))), data = dat)
  mod <- step(mod, direction = "backward")
  print(mod)
  mod <- mod.function(as.formula(get_model(mod)@call$formula), data = dat)
  return(mod)
}

backward.hybrid <- function(data, mod.function, dependent, fixed, random, left, right, alpha = 0.05) {
  n.terms <- length(fixed)
  rerun <- TRUE
  remove.order <- as.data.frame(NULL)
  while (rerun) {
    mod <- mod.function(as.formula(paste(dependent,"~",format.terms(fixed, add.plus = TRUE),format.terms(random, add.plus = FALSE))), data = data, REML = TRUE)
    p.mod <- anova.plus(car::Anova(mod))
    term.drop <- rownames(p.mod[p.mod$Df == 0,])
    fixed <- fixed[!(fixed %in% term.drop)]
    p.mod <- p.mod[p.mod$Df > 0,]
    if (nrow(p.mod[p.mod$`Pr(>Chisq)` > alpha & p.mod$parent == 0,] > 0)) {
      term.remove <- rownames(p.mod[p.mod$`Pr(>Chisq)` == max(p.mod$`Pr(>Chisq)`[p.mod$`Pr(>Chisq)` > alpha & p.mod$parent == 0]),])
    } else {
      term.remove <- NULL
    }
    if (length(term.remove) == 0) {
      rerun <- FALSE
      cat("\nModel found uses ",length(fixed)," of ",n.terms," fixed effects terms.\n")
    } else {
      for (i in fixed) {
        is.match <- TRUE
        for (j in strsplit(term.remove,":")[[1]]) {
          is.match <- is.match & grepl(j,i)
        }
        if (is.match) {
          fixed.remove <- i
        }
      }
      remove.order[nrow(remove.order)+1,"term"] <- fixed.remove
      remove.order[nrow(remove.order),"p-value"] <- p.mod[term.remove,"Pr(>Chisq)"]
      cat("\nUsing ",length(fixed)," of ",n.terms," fixed effects terms.\n")
      fixed <- fixed[fixed != fixed.remove]
      fixed <- c(fixed,term.drop)
    }
    censored.mod <- censReg(as.formula(paste(dependent,"~",format.terms(fixed, add.plus = FALSE))), left = left, right = right, data = data)
    mod.comp <- compare.models(truth.model = censored.mod, comparison.model = mod)
    any.difference <- FALSE
    for (i in 1:nrow(mod.comp)) {
      if (mod.comp$`Pr(>Chisq)`[i] < alpha) {
        cat(mod.comp$Term[i],"is different, p =",mod.comp$`Pr(>Chisq)`[i],"\n")
        any.difference <- TRUE
      }
    }
    if (!any.difference) {
      cat("All fixed effects are similar between models.\n")
    }
  }
  cat("\nUnused Terms:\n")
  if (length(term.drop) > 0) {
    print(term.drop)
  } else {
    cat("None\n")
  }
  cat("\nOrder of Removal:\n")
  print(remove.order)
  return(mod)
}

beta.transform <- function (data.column, left = min(data.column, na.rm = TRUE), right = max(data.column, na.rm = TRUE), granularity = 10^-numdec(data.column)) {
  data.column[data.column < left + granularity & !is.na(data.column)] <- left + granularity
  data.column[data.column > right - granularity & !is.na(data.column)] <- right - granularity
  data.column <- data.column - left
  data.column <- data.column/(right-left)
  return(data.column)
}

check.terms <- function (this.term, term.list) {
  keep.term <- NULL
  full.match <- NULL
  for (n in 1:length(this.term)) {
    keep.term[n] <- TRUE
    for (i in 1:length(str_split(term.list,":"))) {
      if (length(str_split(term.list,":")[[i]]) > length(str_split(this.term[n],":")[[1]])) {
        full.match[n] <- TRUE
        for (j in 1:length(str_split(this.term[n],":")[[1]])) {
          full.match[n] <- full.match[n] & str_split(this.term[n],":")[[1]][j] %in% str_split(term.list,":")[[i]]
        }
        keep.term[n] <- keep.term[n] & !full.match[n]
      }
    }
  }
  return(keep.term)
}

compare.models <- function (truth.model, comparison.model) {
  mod.comp <- as.data.frame(NULL)
  for (i in rownames(summary(comparison.model)$coefficients)) {
    mod.comp[nrow(mod.comp)+1,"Term"] <- i
    mod.comp[nrow(mod.comp),"Pr(>Chisq)"] <- linearHypothesis(comparison.model, paste(i,"=",coef(truth.model)[names(coef(truth.model)) == i]))$`Pr(>Chisq)`[2]
    mod.comp[nrow(mod.comp),"Sig"] <- if (mod.comp[nrow(mod.comp),"Pr(>Chisq)"] < 0.001) {"***"} else if (mod.comp[nrow(mod.comp),"Pr(>Chisq)"] < 0.01) {"**"} else if (mod.comp[nrow(mod.comp),"Pr(>Chisq)"] < 0.05) {"*"} else {""}
  }
  return(mod.comp)
}

correlation.plot <- function (x.name, y.name, data, granularity = 30) {
  data[,y.name] <- as.numeric(data[,y.name])
  dsum <- as.data.frame(NULL)
  if (length(unique(data[!is.na(data[,x.name]),x.name])) > granularity) {
    step.size <- (max(data[,x.name], na.rm = TRUE)-min(data[,x.name], na.rm = TRUE))/granularity
    for (i in seq(min(data[,x.name], na.rm = TRUE),max(data[,x.name], na.rm = TRUE),step.size)) {
      for (j in c("No","Yes")) {
        for (k in c("No","Yes")) {
          for (t in c("nonERT","ERT")) {
            n <- nrow(data[!is.na(data[,x.name]) & !is.na(data[,y.name]) & data[,x.name] >= i & data[,x.name] <= i + step.size & data$pred.ON == j & data$anno.ON == k & data$Truth == t,])
            if (n > 0) {
              dsum[nrow(dsum)+1,x.name] <- i
              dsum[nrow(dsum),"pred.ON"] <- j
              dsum[nrow(dsum),"anno.ON"] <- k
              dsum[nrow(dsum),"Truth"] <- t
              dsum[nrow(dsum),"Count"] <- n
              dsum[nrow(dsum),y.name] <- mean(data[data[,x.name] >= i & data[,x.name] <= i + step.size & data$pred.ON == j & data$anno.ON == k & data$Truth == t, y.name], na.rm = TRUE)
              dsum[nrow(dsum),c("lower.CI","upper.CI")] <- tryCatch(t.test(data[data[,x.name] >= i & data[,x.name] <= i + step.size & data$pred.ON == j & data$anno.ON == k & data$Truth == t, y.name])$conf.int[1:2], error = function(e) NA)
            }
          }
        }
      }
    }
  } else {
    for (i in unique(data[!is.na(data[,x.name]),x.name])) {
      for (j in c("No","Yes")) {
        for (k in c("No","Yes")) {
          for (t in c("nonERT","ERT")) {
            n <- nrow(data[!is.na(data[,x.name]) & !is.na(data[,y.name]) & data[,x.name] == i & data$pred.ON == j & data$anno.ON == k & data$Truth == t,])
            if (n > 0) {
              dsum[nrow(dsum)+1,x.name] <- i
              dsum[nrow(dsum),"pred.ON"] <- j
              dsum[nrow(dsum),"anno.ON"] <- k
              dsum[nrow(dsum),"Truth"] <- t
              dsum[nrow(dsum),"Count"] <- n
              dsum[nrow(dsum),y.name] <- mean(data[data[,x.name] == i & data$pred.ON == j & data$anno.ON == k & data$Truth == t, y.name], na.rm = TRUE)
              dsum[nrow(dsum),c("lower.CI","upper.CI")] <- tryCatch(t.test(data[data[,x.name] == i & data$pred.ON == j & data$anno.ON == k & data$Truth == t, y.name])$conf.int[1:2], error = function(e) NA)
            }
          }
        }
      }
    }
  }
  dsum$lower.CI[is.na(dsum$lower.CI) | dsum$lower.CI < min(data[,y.name], na.rm = TRUE)] <- min(data[,y.name], na.rm = TRUE)
  dsum$upper.CI[is.na(dsum$upper.CI) | dsum$upper.CI > max(data[,y.name], na.rm = TRUE)] <- max(data[,y.name], na.rm = TRUE)
  ggplot(data = data, aes_string(x = x.name, y = y.name, group = "Truth")) +
    geom_point(aes(fill = Truth, color = Truth), alpha = 0.05) +
    geom_ribbon(data = dsum, aes(ymin = lower.CI, ymax = upper.CI, fill = Truth), alpha = 0.2) +
    geom_line(data = dsum, aes(color = Truth), linewidth = 1) +
    labs(x = x.name, y = y.name) +
    scale_fill_manual(values = colors, name = "Patient Type", labels = c("Urgent","Non-Urgent")) +
    scale_color_manual(values = colors, name = "Patient Type", labels = c("Urgent","Non-Urgent")) +
    theme_linedraw(base_size = 16) +
    facet_grid(anno.ON ~ pred.ON, labeller = labeller(pred.ON = prediction.labels, anno.ON = annotation.labels))
}

custom.contrast <- function (object, comparison = list(A = NULL, B = NULL), difference = list(Top = NULL, Bottom = NULL), common = NULL) {
  A <- TRUE
  if (!is.null(comparison[["A"]])) {
    for (i in 1:length(comparison[["A"]])) {
      A <- A & object@grid[,names(comparison[["A"]])[i]] %in% comparison[["A"]][[i]]
    }
  } 
  if (is.null(comparison[["B"]])) {
    B <- FALSE
  } else {
    B <- TRUE
    for (i in 1:length(comparison[["B"]])) {
      B <- B & object@grid[,names(comparison[["B"]])[i]] %in% comparison[["B"]][[i]]
    }
  }
  if (!is.null(common)) {
    for (i in 1:length(common)) {
      A <- A & object@grid[,names(common)[i]] %in% common[[i]]
      B <- B & object@grid[,names(common)[i]] %in% common[[i]]
    }
  }
  TopA <- A
  TopB <- B
  BottomA <- A
  BottomB <- B
  if (!is.null(difference[["Top"]]) & !is.null(difference[["Bottom"]])) {
    for (i in 1:length(difference[["Top"]])) {
      TopA <- TopA & object@grid[,names(difference[["Top"]])[i]] %in% difference[["Top"]][[i]]
      TopB <- TopB & object@grid[,names(difference[["Top"]])[i]] %in% difference[["Top"]][[i]]
    }
    for (i in 1:length(difference[["Bottom"]])) {
      BottomA <- BottomA & object@grid[,names(difference[["Bottom"]])[i]] %in% difference[["Bottom"]][[i]]
      BottomB <- BottomB & object@grid[,names(difference[["Bottom"]])[i]] %in% difference[["Bottom"]][[i]]
    }
    A <- TopA - BottomA
    B <- TopB - BottomB
  }
  A <- A/nrow(object@grid[TopA,])
  B <- B/nrow(object@grid[TopB,])
  print(contrast(regrid(object), method = list("custom" = A - B)))
}

pass.back <- function (data, mod.function, dependent, independents) {
  dat <- data
  mod <- mod.function(as.formula(paste(dependent,"~",format.terms(independents))), data = dat)
  return(mod)
}

extract.order <- function (formula.string) {
  len.vector <- NULL
  formula.string <- extract.terms(formula.string)
  for (i in 1:length(formula.string)) {
    len.vector[i] <- length(str_split(formula.string,":")[[i]])
  }
  return(len.vector)
}

extract.terms <- function (formula.string) {
  formula.string <- str_split(formula.string,"~")[[1]][length(str_split(formula.string,"~")[[1]])]
  formula.string <- str_split(formula.string,"\\+")[[1]]
  formula.string <- str_replace_all(formula.string," ","")
  return(formula.string)
}

format.terms <- function (terms, add.plus = FALSE) {
  fx.sum <- NULL 
  for (i in terms) {
    if (!add.plus & i == terms[length(terms)]) {
      fx.sum <- paste(fx.sum, i)
    } else {
      fx.sum <- paste(fx.sum, i, "+")
    }
  }
  return(fx.sum)
}

get.interactions <- function(base.terms, term.list = NULL, int.order = length(base.terms)) {
  term.list <- if (is.null(term.list)) {base.terms} else {term.list}
  if (int.order > 1) {
    loop.list <- term.list[str_count(term.list,":") == max(str_count(term.list,":"))]
    for (i in loop.list) {
      if (match(strsplit(i,":")[[1]][length(strsplit(i,":")[[1]])], base.terms) != length(base.terms)) {
        for (j in (match(strsplit(i,":")[[1]][length(strsplit(i,":")[[1]])], base.terms)+1):length(base.terms)) {
          term.list <- c(term.list, paste0(i,":",base.terms[j]))
        }
      }
    }
    get.interactions(base.terms = base.terms, term.list = term.list, int.order = int.order - 1)
  } else {
    return(term.list[!(term.list %in% base.terms)])
  }
}

get.terms <- function(base.terms, term.list = NULL, int.order = length(base.terms)) {
  term.list <- if (is.null(term.list)) {base.terms} else {term.list}
  if (int.order > 1) {
    loop.list <- term.list[str_count(term.list,":") == max(str_count(term.list,":"))]
    for (i in loop.list) {
      if (match(strsplit(i,":")[[1]][length(strsplit(i,":")[[1]])], base.terms) != length(base.terms)) {
        for (j in (match(strsplit(i,":")[[1]][length(strsplit(i,":")[[1]])], base.terms)+1):length(base.terms)) {
          term.list <- c(term.list, paste0(i,":",base.terms[j]))
        }
      }
    }
    get.terms(base.terms = base.terms, term.list = term.list, int.order = int.order - 1)
  } else if (int.order == 1) {
    return(term.list)
  } else if (int.order == 0) {
    return(NULL)
  } else {
    cat("\nError: int.order somehow below 0\n")
    return(NULL)
  }
}

mfx.model <- function(mod.function, data, output, fixed, random, alpha = 0.05, check.order = FALSE) {
  n.terms <- length(fixed)
  rerun <- TRUE
  remove.order <- as.data.frame(NULL)
  while (rerun) {
    mod <- mod.function(as.formula(paste(output,"~",format.terms(fixed, add.plus = TRUE),format.terms(paste0("(",random,")"), add.plus = FALSE))), data = data, REML = TRUE)
    p.mod <- anova.plus(car::Anova(mod))
    term.drop <- rownames(p.mod[p.mod$Df == 0,])
    fixed <- fixed[!(fixed %in% term.drop)]
    p.mod <- p.mod[p.mod$Df > 0,]
    term.remove <- rownames(p.mod[p.mod$`Pr(>Chisq)` == max(p.mod$`Pr(>Chisq)`[p.mod$`Pr(>Chisq)` > alpha & p.mod$parent == 0]),])
    if (check.order) {
      nonordinal <- check.ordinal(data = data, fixed = fixed[fixed %in% dict$Name[dict$Category == "demographic" & dict$Granularity == "ordinal"]], mod.effects = summary(mod)$coefficients)
      if (!is.null(nonordinal)) {
        term.remove <- rownames(p.mod[p.mod$`Pr(>Chisq)` == max(p.mod$`Pr(>Chisq)`[rownames(p.mod) %in% nonordinal]),])
      }
    }
    if (length(term.remove) == 0) {
      rerun <- FALSE
    } else {
      for (i in fixed) {
        is.match <- TRUE
        for (j in strsplit(term.remove,":")[[1]]) {
          is.match <- is.match & grepl(j,i)
        }
        if (is.match) {
          fixed.remove <- i
        }
      }
      remove.order[nrow(remove.order)+1,"term"] <- fixed.remove
      remove.order[nrow(remove.order),"p-value"] <- p.mod[term.remove,"Pr(>Chisq)"]
      cat("\nUsing ",length(fixed)," of ",n.terms," fixed effects terms.\n")
      fixed <- fixed[fixed != fixed.remove]
      fixed <- c(fixed,term.drop)
    }
  }
  cat("\nUnused Terms:\n")
  if (length(term.drop) > 0) {
    print(term.drop)
  } else {
    cat("None\n")
  }
  cat("\nOrder of Removal:\n")
  print(remove.order)
  return(mod)
}

control.clmm <- function (formula.string, data, crit = "AIC", retain = NULL, control = list(NULL)) {
  if (!(crit %in% c("AIC","logLik","BIC"))) {
    cat("\nError: invalid ordering criteria.\nMust be 'AIC' or 'logLik'\n")
    return(NULL)
  }
  if (crit == "BIC") {
    BIC <- TRUE
    crit <- "AIC"
  } else {
    BIC <- FALSE
  }
  left.formula <- str_split(formula.string,"~")[[1]][1]
  iter <- 0
  rerun <- TRUE
  removal.order <- as.data.frame(NULL)
  while(rerun) {
    iter <- iter + 1
    cat("\nIteration",iter,"\n")
    if (grepl("\\|",formula.string)) {
      mod.null <- tryCatch(clmm(formula = paste(formula.string, collapse = " "), data = data, control = do.call(clmm.control, control)), error = function(e) e)
    } else {
      mod.null <- tryCatch(clm(formula = paste(formula.string, collapse = " "), data = data), error = function(e) e)
    }
    if (inherits(mod.null,'error')) {
      cat("\nError: null model produced an error.\n")
      return(NULL)
    } else {
      if (grepl("\\|",formula.string)) {
        convergence.code <- mod.null$optRes$convergence
      } else {
        convergence.code <- mod.null$convergence$code
      }
    }
    if (convergence.code != 0) {
      cat("\nError: null model failed to converge with controls.\n")
      return(NULL)
    }
    add.tab <- as.data.frame(NULL)
    for (i in extract.terms(formula.string)) {
      add.tab[nrow(add.tab)+1,"Term"] <- i
      add.tab[nrow(add.tab),"Evaluate"] <- if (i == 1 | i %in% retain) {FALSE} else {check.terms(i, extract.terms(formula.string))}
      add.tab[nrow(add.tab),"Iteration"] <- iter
      if (add.tab$Evaluate[nrow(add.tab)]) {
        new.formula <- paste(left.formula,"~",format.terms(extract.terms(formula.string)[extract.terms(formula.string) != i]), collapse = " ")
        if (grepl("\\|",new.formula)) {
          mod.add <- tryCatch(clmm(formula = paste(left.formula,"~",format.terms(extract.terms(formula.string)[extract.terms(formula.string) != i]), collapse = " "), data = data, control = do.call(clmm.control, control)), error = function(e) e)
        } else {
          mod.add <- tryCatch(clm(formula = paste(left.formula,"~",format.terms(extract.terms(formula.string)[extract.terms(formula.string) != i]), collapse = " "), data = data), error = function(e) e)
        }
        if (inherits(mod.add,'error')) {
          add.tab$Evaluate[nrow(add.tab)] <- FALSE
        } else {
          if (grepl("\\|",new.formula)) {
            convergence.code <- mod.add$optRes$convergence
          } else {
            convergence.code <- mod.add$convergence$code
          }
        }
        if (convergence.code == 0) {
          if (BIC) {
            # reminder: clmm does not appear to have an intercept (baked into connecting to ordinal scale)
            add.tab[nrow(add.tab),crit] <- (as.numeric(mod.null$info[crit]) - 2*length(extract.terms(formula.string)) + log(as.numeric(mod.null$info["nobs"]))*length(extract.terms(formula.string))) - (as.numeric(mod.add$info[crit]) - 2*(length(extract.terms(formula.string))-1) + log(as.numeric(mod.add$info["nobs"]))*(length(extract.terms(formula.string))-1))
          } else {
            add.tab[nrow(add.tab),crit] <- as.numeric(mod.null$info[crit]) - as.numeric(mod.add$info[crit])
          }
        } else {
          add.tab[nrow(add.tab),crit] <- NA
        }
      }
    }
    if (BIC) {
      names(add.tab) <- c("Term","Evaluate","Iteration","BIC")
    } 
    print(add.tab)
    if (BIC) {
      names(add.tab) <- c("Term","Evaluate","Iteration","AIC")
    }
    if (max(add.tab[,crit], na.rm = TRUE) < 0) {
      rerun <- FALSE
      cat("\nAll terms are significant.\n")
      if (BIC) {
        names(removal.order) <- c("Term","Iteration","BIC")
      }
      print(removal.order)
      return(mod.null)
    } else {
      cat("\nRemoving ...",add.tab$Term[!is.na(add.tab[,crit]) & add.tab[,crit] == max(add.tab[,crit], na.rm = TRUE)],"\n")
      formula.string <- paste(left.formula,"~",format.terms(extract.terms(formula.string)[extract.terms(formula.string) != add.tab$Term[!is.na(add.tab[,crit]) & add.tab[,crit] == max(add.tab[,crit], na.rm = TRUE)]]))
      removal.order[nrow(removal.order)+1,"Term"] <- add.tab$Term[!is.na(add.tab[,crit]) & add.tab[,crit] == max(add.tab[,crit], na.rm = TRUE)]
      removal.order[nrow(removal.order),"Iteration"] <- iter
      removal.order[nrow(removal.order),crit] <- max(add.tab[,crit], na.rm = TRUE)
      cat("\nNew Formula:\n",formula.string,"\n")
    }
  }
}

control.glmmTMB <- function (formula.string, family = gaussian(), data, crit = "AIC", control = glmmTMBControl(), retain = NULL, impute.col = NULL, censored = FALSE, iterations = 5, seed = Sys.time(), convergence.stop = FALSE) {
  if (!(crit %in% c("AIC","BIC","logLik"))) {
    cat("\nError: invalid ordering criteria.\nMust be 'AIC', 'BIC', or 'logLik'\n")
    return(NULL)
  }
  left.formula <- str_split(formula.string,"~")[[1]][1]
  iter <- 0
  rerun <- TRUE
  removal.order <- as.data.frame(NULL)
  while(rerun) {
    iter <- iter + 1
    cat("\nIteration",iter,"\n")
    if (convergence.stop) {
      mod.null <- tryCatch(glmmTMB(formula = as.formula(formula.string), family = family, data = data), error = function(e) e)
      if (inherits(mod.null,'error')) {
        cat("\nError: null model produced an error.\n")
        return(NULL)
      } else if (mod.null$fit$convergence == 0) {
        cat("\nSuccess: current model converged with default controls!\n")
        rerun <- FALSE
        return(mod.null)
      }
    }
    mod.null <- tryCatch(glmmTMB(formula = as.formula(formula.string), family = family, data = data, control = control), error = function(e) e)
    if (inherits(mod.null,'error')) {
      cat("\nError: null model produced an error.\n")
      return(NULL)
    } else if (mod.null$fit$convergence != 0) {
      cat("\nError: null model failed to converge with controls.\n")
      return(NULL)
    }
    if (!is.null(impute.col)) {
      impute.check <- FALSE
      cat("\nImputation Check ... ")
      pool.summary <- impute.pool(impute.glmm(formula = mod.null$call$formula, family = family, data = data, control = control, impute.col = impute.col, censored = censored, iterations = iterations, seed = seed))
      mod.comparison <- data.frame(summary(mod.null)$coefficients$cond)
      names(mod.comparison) <- c("Estimate","SE","z.value","p.value")
      for (i in 1:nrow(mod.comparison)) {
        mod.comparison[i,"PooledEstimate"] <- pool.summary$Estimate[pool.summary$Term == rownames(mod.comparison)[i]]
        mod.comparison[i,"PooledSE"] <- pool.summary$`Std. Error`[pool.summary$Term == rownames(mod.comparison)[i]]
        mod.comparison[i,"z.comparison"] <- (mod.comparison$Estimate[i] - mod.comparison$PooledEstimate[i])/sqrt(mod.comparison$SE[i]^2 + mod.comparison$PooledSE[i]^2)
        mod.comparison[i,"p.comparison"] <- 2*pnorm(min(mod.comparison$z.comparison[i], -mod.comparison$z.comparison[i]))
        if (mod.comparison[i,"p.comparison"] < 0.05) {
          impute.check <- TRUE
          cat("WARNING! Null model and pooled imputed model differ in",rownames(mod.comparison)[i],"with p-value =",mod.comparison$p.comparison[i],"\n\n")
        }
      }
      if (nrow(mod.comparison[mod.comparison$p.comparison < 0.05,]) == 0) {
        cat("no disagreements detected.\n\n")
      }
    }
    add.tab <- as.data.frame(NULL)
    for (i in extract.terms(formula.string)) {
      add.tab[nrow(add.tab)+1,"Term"] <- i
      add.tab[nrow(add.tab),"Evaluate"] <- if (i == 1 | i %in% retain) {FALSE} else {check.terms(i, extract.terms(formula.string))}
      add.tab[nrow(add.tab),"Iteration"] <- iter
      if (add.tab$Evaluate[nrow(add.tab)]) {
        mod.add <- tryCatch(glmmTMB(formula = as.formula(paste(left.formula,"~",format.terms(extract.terms(formula.string)[extract.terms(formula.string) != i]))), family = family, data = data, control = control), error = function(e) e)
        if (inherits(mod.add,'error')) {
          add.tab$Evaluate[nrow(add.tab)] <- FALSE
        } else if (mod.add$fit$convergence == 0) {
          anova.test <- anova(mod.null, mod.add)
          add.tab[nrow(add.tab),crit] <- anova.test[,crit][2] - anova.test[,crit][1]
        } else {
          add.tab[nrow(add.tab),crit] <- NA
        }
      }
    }
    print(add.tab)
    if (max(add.tab[,crit], na.rm = TRUE) < 0) {
      rerun <- FALSE
      cat("\nAll terms are significant.\n")
      print(removal.order)
      return(mod.null)
    } else {
      cat("\nRemoving ...",add.tab$Term[!is.na(add.tab[,crit]) & add.tab[,crit] == max(add.tab[,crit], na.rm = TRUE)],"\n")
      formula.string <- paste(left.formula,"~",format.terms(extract.terms(formula.string)[extract.terms(formula.string) != add.tab$Term[!is.na(add.tab[,crit]) & add.tab[,crit] == max(add.tab[,crit], na.rm = TRUE)]]))
      removal.order[nrow(removal.order)+1,"Term"] <- add.tab$Term[!is.na(add.tab[,crit]) & add.tab[,crit] == max(add.tab[,crit], na.rm = TRUE)]
      removal.order[nrow(removal.order),"Iteration"] <- iter
      removal.order[nrow(removal.order),crit] <- max(add.tab[,crit], na.rm = TRUE)
      if (!is.null(impute.col)) {
        removal.order[nrow(removal.order),"Imputation"] <- if (impute.check) {"FAIL"} else {"pass"}
      }
      cat("\nNew Formula:\n",formula.string,"\n")
    }
  }
}

stepwise.glmmTMB <- function (start.formula, max.formula = start.formula, family = gaussian(), data, direction = "bidirectional", crit = "AIC", control = glmmTMBControl(), retain = NULL, save.path = NULL, impute.col = NULL, impute.freq = "always", censored = FALSE, iterations = 5, seed = Sys.time()) {
  
  # record start time.
  start.time <- Sys.time()
  
  # check for valid inputs.
  if (!(crit %in% c("AIC","BIC","logLik"))) {
    cat("\nError: invalid ordering criteria.\nMust be 'AIC', 'BIC', or 'logLik'\n")
    return(NULL)
  }
  if (!(direction %in% c("forward","backward","bidirectional"))) {
    cat("\nError: invalid selection direction.\nMust be 'forward', 'backward', or 'bidirectional'\n")
    return(NULL)
  }
  if (!(impute.freq %in% c("always","first","last","ends"))) {
    cat("\nError: invalid imputation frequency.\nMust be 'always', 'first', 'last', or 'ends'\n")
  }
  
  # check maximal model and extract data frame for consistent use throughout.
  cat("\nInitializing ... ")
  mod.full <- tryCatch(glmmTMB(formula = as.formula(max.formula), family = family, data = data, control = control), error = function(e) e)
  if (inherits(mod.full,'error')) {
    cat("\n\nError: full model produced an error.\n")
    return(NULL)
  } else if (mod.full$fit$convergence != 0) {
    cat("\n\nError: full model failed to converge with controls.\n")
    return(NULL)
  } else {
    keep.list <- TRUE
    for (i in names(mod.full$frame)) {
      keep.list <- keep.list & !is.na(data[,i])
    }
    data <- data[keep.list,]
  }
  cat("complete.\n")
  
  # initialize variables.
  formula.string <- start.formula
  left.formula <- str_split(formula.string,"~")[[1]][1]
  iter <- 0
  rerun <- TRUE
  selection.order <- as.data.frame(NULL)
  selected.term <- 1
  if (direction == "forward") {
    forward <- TRUE
    backward <- FALSE
  } else if (direction == "backward") {
    forward <- FALSE
    backward <- TRUE
  } else if (direction == "bidirectional") {
    forward <- TRUE
    backward <- TRUE
  }
  if (impute.freq == "always") {
    impute.first <- TRUE
    impute.middle <- TRUE
    impute.last <- TRUE
  } else if (impute.freq == "first") {
    impute.first <- TRUE
    impute.middle <- FALSE
    impute.last <- FALSE
  } else if (impute.freq == "last") {
    impute.first <- FALSE
    impute.middle <- FALSE
    impute.last <- TRUE
  } else if (impute.freq == "ends") {
    impute.first <- TRUE
    impute.middle <- FALSE
    impute.last <- TRUE
  }
  if (is.character(censored)) {
    censored <- data[,censored]
  }
  
  # stepwise selection procedure.
  while(rerun) {
    
    # start new iteration.
    iter <- iter + 1
    step.tab <- data.frame(Term = extract.terms(max.formula), Iteration = iter)
    cat("\nIteration",iter,"\n")
    
    # fit and check null model.
    if (iter == 1 & start.formula == max.formula) {
      mod.null <- mod.full
    } else {
      mod.null <- tryCatch(glmmTMB(formula = as.formula(formula.string), family = family, data = data, control = control), error = function(e) e)
    }
    if (inherits(mod.null,'error')) {
      cat("\nError: null model produced an error.\n")
      return(NULL)
    } else if (mod.null$fit$convergence != 0) {
      cat("\nError: null model failed to converge with controls.\n")
      return(NULL)
    }
    
    # check all terms in maximal model, except for previously selected term.
    for (i in 1:nrow(step.tab)) {
      if (step.tab$Term[i] != selected.term & step.tab$Term[i] != 1) {
        
        # check for backwards removal.
        if (backward) {
          if (!(step.tab$Term[i] %in% retain) & step.tab$Term[i] %in% extract.terms(formula.string)) {
            if (check.terms(step.tab$Term[i], extract.terms(formula.string))) {
              step.tab[i,"Evaluate"] <- "Remove"
              mod.new <- tryCatch(glmmTMB(formula = as.formula(paste(left.formula,"~",format.terms(extract.terms(formula.string)[extract.terms(formula.string) != step.tab$Term[i]]))), family = family, data = data, control = control), error = function(e) e)
              if (inherits(mod.new,'error')) {
                step.tab[i,crit] <- NA
              } else if (mod.new$fit$convergence == 0) {
                anova.test <- anova(mod.null, mod.new)
                step.tab[i,crit] <- anova.test[,crit][2] - anova.test[,crit][1]
              } else {
                step.tab[i,crit] <- NA
              }
            }
          }
        }
        
        # check for forwards addition.
        if (forward) {
          if (!(step.tab$Term[i] %in% extract.terms(formula.string))) {
            if (sum((!(get.terms(str_split(step.tab$Term[i],":")[[1]], int.order = length(str_split(step.tab$Term[i],":")[[1]])-1) %in% extract.terms(formula.string)))) == 0) {
              step.tab[i,"Evaluate"] <- "Add"
              mod.new <- tryCatch(glmmTMB(formula = as.formula(paste(left.formula,"~",format.terms(c(extract.terms(formula.string), step.tab$Term[i])))), family = family, data = data, control = control), error = function(e) e)
              if (inherits(mod.new,'error')) {
                step.tab[i,crit] <- NA
              } else if (mod.new$fit$convergence == 0) {
                anova.test <- anova(mod.null, mod.new)
                step.tab[i,crit] <- anova.test[,crit][1] - anova.test[,crit][2]
              } else {
                step.tab[i,crit] <- NA
              }
            }
          }
        }
      }
    }
    
    # check if any term meets selection criteria.
    if (max(step.tab[,crit], na.rm = TRUE) < 0) {
      rerun <- FALSE
      cat("\nAll terms are significant.\n")
    } else if (nrow(step.tab[!is.na(step.tab$Evaluate),]) == 0) {
      rerun <- FALSE
      cat("\nNo further evaluations are possible.\n")
    } 
    
    # check null model against imputed model.
    if (!is.null(impute.col)) {
      if ((impute.first & iter == 1) | (impute.middle) | (impute.last & !rerun)) {
        impute.check <- FALSE
        cat("\nImputation Check ... ")
        pool.summary <- impute.pool(impute.glmm(formula = mod.null$call$formula, family = family, data = data, control = control, impute.col = impute.col, censored = censored, iterations = iterations, seed = seed))
        mod.comparison <- data.frame(summary(mod.null)$coefficients$cond)
        names(mod.comparison) <- c("Estimate","SE","z.value","p.value")
        for (i in 1:nrow(mod.comparison)) {
          mod.comparison[i,"PooledEstimate"] <- pool.summary$Estimate[pool.summary$Term == rownames(mod.comparison)[i]]
          mod.comparison[i,"PooledSE"] <- pool.summary$`Std. Error`[pool.summary$Term == rownames(mod.comparison)[i]]
          mod.comparison[i,"z.comparison"] <- (mod.comparison$Estimate[i] - mod.comparison$PooledEstimate[i])/sqrt(mod.comparison$SE[i]^2 + mod.comparison$PooledSE[i]^2)
          mod.comparison[i,"p.comparison"] <- 2*pnorm(min(mod.comparison$z.comparison[i], -mod.comparison$z.comparison[i]))
          if (mod.comparison[i,"p.comparison"] < 0.05) {
            impute.check <- TRUE
            cat("\nWARNING! Null model and pooled imputed model differ in",rownames(mod.comparison)[i],"with p-value =",mod.comparison$p.comparison[i],"\n\n")
          }
        }
        if (nrow(mod.comparison[mod.comparison$p.comparison < 0.05,]) == 0) {
          cat("no disagreements detected.\n\n")
        }
      } else {
        impute.check <- NA
      }
    }
    
    # print summary of stepwise iteration.
    print(step.tab)
    
    # select term.
    if (rerun) {
      
      # record location of selected term.
      max.step <- match(max(step.tab[,crit], na.rm = TRUE), step.tab[,crit])
      cat("\n",step.tab$Evaluate[max.step],"...",step.tab$Term[max.step],"\n")
      
      # remove term from formula.
      if (step.tab$Evaluate[max.step] == "Remove") {
        formula.string <- paste(left.formula,"~",format.terms(extract.terms(formula.string)[extract.terms(formula.string) != step.tab$Term[max.step]]))
      }
      
      # add term to formula.
      if (step.tab$Evaluate[max.step] == "Add") {
        formula.string <- paste(left.formula,"~",format.terms(c(extract.terms(formula.string), step.tab$Term[max.step])))
      }
      
      # record selection order and print new formula.
      selection.order[nrow(selection.order)+1,"Term"] <- step.tab$Term[max.step]
      selection.order[nrow(selection.order),"Iteration"] <- iter
      selection.order[nrow(selection.order),"Evaluate"] <- step.tab$Evaluate[max.step]
      selection.order[nrow(selection.order),crit] <- step.tab[max.step,crit]
      if (!is.null(impute.col)) {
        selection.order[nrow(selection.order),"Imputation"] <- if (is.na(impute.check)) {NA} else if (impute.check) {"FAIL"} else {"pass"}
      }
      selected.term <- step.tab$Term[max.step]
      cat("\nNew Formula:\n",formula.string,"\n")
    }
  }
  
  # record end time.
  end.time <- Sys.time()
  
  # print summary of selection order.
  cat("\n\nSummary of Selection Order:\n")
  print(selection.order)
  if (!is.null(save.path)) {
    write.csv(selection.order, save.path)
  }
  cat("\n\nElapsed Time:",round(difftime(end.time, start.time, units = "mins"), digits = 2),"minutes\n")
  return(mod.null)
}

compare.glmmTMB <- function (base.formula, term.list, family = gaussian(), data, crit = "AIC", control = glmmTMBControl(), skip.terms = NULL, remove.check = FALSE, lrt.check = FALSE, save.path = NULL) {
  start.time <- Sys.time()
  left.formula <- str_split(base.formula,"~")[[1]][1]
  comparison <- data.frame(Term = "BASE MODEL", Formula = base.formula)
  cat("\nFitting base model.")
  null.mod <- tryCatch(glmmTMB(as.formula(base.formula), family = family, data = data, control = control), error = function(e) e)
  if (inherits(null.mod,'error')) {
    cat("\nError: base model produced an error.\n")
    return(NULL)
  } else if (null.mod$fit$convergence != 0) {
    cat("\nError: base model failed to converge with controls.\n")
    return(NULL)
  }
  comparison[comparison$Formula == base.formula, c("AIC","BIC","logLik","deviance","df.resid")] <- summary(null.mod)$AICtab
  intermediate.time <- Sys.time()
  for (i in term.list) {
    if (!(i %in% extract.terms(base.formula) | i %in% skip.terms)) {
      comparison[nrow(comparison)+1,"Term"] <- i
      this.terms <- get.terms(str_split(i,":")[[1]])
      comparison[nrow(comparison),"Formula"] <- paste(left.formula,"~",format.terms(remove.duplicates(c(extract.terms(base.formula),this.terms))), collapse = " ")
      this.mod <- tryCatch(glmmTMB(as.formula(comparison$Formula[nrow(comparison)]), family = family, data = data, control = control), error = function(e) e)
      if (!inherits(this.mod,'error') & this.mod$fit$convergence == 0) {
        comparison[nrow(comparison),c("AIC","BIC","logLik","deviance","df.resid")] <- summary(this.mod)$AICtab
      }
      if (remove.check) {
        if (comparison[nrow(comparison),crit] < comparison[comparison$Formula == base.formula,crit]) {
          if (length(this.terms) > 1) {
            remove.mod <- tryCatch(glmmTMB(as.formula(paste(left.formula,"~",format.terms(remove.duplicates(c(extract.terms(base.formula),this.terms[this.terms != i]))), collapse = " ")), family = family, data = data, control = control), error = function(e) e)
          } else {
            remove.mod <- null.mod
          }
          if (!inherits(remove.mod,'error') & remove.mod$fit$convergence == 0) {
            comparison[nrow(comparison),paste0("removed_",c("AIC","BIC","logLik","deviance","df.resid"))] <- summary(remove.mod)$AICtab
            comparison[nrow(comparison),"Removal"] <- comparison[nrow(comparison),crit] - comparison[nrow(comparison),paste0("removed_",crit)]
          }
        }
      }
      if (lrt.check) {
        this.lrt <- lrtest(null.mod, this.mod)
        comparison[nrow(comparison),c("#Df","Df","Chisq","Pr(>Chisq)")] <- this.lrt[2,c("#Df","Df","Chisq","Pr(>Chisq)")]
        comparison[nrow(comparison),"Sig"] <- if (comparison[nrow(comparison),"Pr(>Chisq)"] < 0.001) {"***"} else if (comparison[nrow(comparison),"Pr(>Chisq)"] < 0.01) {"**"} else if (comparison[nrow(comparison),"Pr(>Chisq)"] < 0.05) {"*"} else {""}
      }
    }
    previous.time <- intermediate.time
    intermediate.time <- Sys.time()
    cat("\nFitted",match(i,term.list),"of",length(term.list),"models in",round(difftime(intermediate.time, previous.time, units = "secs"), digits = 0),"seconds.")
  }
  comparison <- comparison[order(comparison[,crit]),]
  if (!is.null(save.path)) {
    write.csv(comparison, save.path)
  }
  end.time <- Sys.time()
  cat("\n\nElapsed Time:",round(difftime(end.time, start.time, units = "mins"), digits = 2),"minutes\n")
  return(comparison)
}

impute.glmm <- function (formula, family = gaussian(), data, control = glmmTMBControl(), impute.col, censored, iterations = 5, seed) {
  impute.summary <- as.data.frame(NULL)
  for (i in 1:iterations) {
    mod.impute <- glmmTMB(formula = formula, family = family, control = control, 
                          data = impute.interval(data = data, impute.col = impute.col, censored = censored, replace = TRUE, seed = seed + 1))
    impute.summary <- rbind(impute.summary, cbind(Term = rownames(summary(mod.impute)$coefficients$cond), summary(mod.impute)$coefficients$cond))
  }
  rownames(impute.summary) <- 1:nrow(impute.summary)
  impute.summary$Estimate <- as.numeric(impute.summary$Estimate)
  impute.summary$`Std. Error` <- as.numeric(impute.summary$`Std. Error`)
  impute.summary$`z value` <- as.numeric(impute.summary$`z value`)
  impute.summary$`Pr(>|z|)` <- as.numeric(impute.summary$`Pr(>|z|)`)
  return(impute.summary)
}

impute.pool <- function(impute.summary) {
  pool.summary <- as.data.frame(NULL)
  for (i in unique(impute.summary$Term)) {
    pool.summary[nrow(pool.summary)+1,"Term"] <- i
    pool.summary[nrow(pool.summary),"Estimate"] <- mean(impute.summary$Estimate[impute.summary$Term == i])
    pool.summary[nrow(pool.summary),"Std. Error"] <- sqrt(mean(impute.summary$`Std. Error`[impute.summary$Term == i]^2) + var(impute.summary$Estimate[impute.summary$Term == i]) + var(impute.summary$Estimate[impute.summary$Term == i])/nrow(impute.summary[impute.summary$Term == i,]))
  }
  return(pool.summary)
}

impute.interval <- function(data, impute.col, censored = FALSE, replace = FALSE, seed = Sys.time()) {
  for (i in unique(data[censored & !is.na(data[,impute.col]), impute.col])) {
    set.seed(seed + 1)
    i.pdf <- density(data[!censored & !is.na(data[,impute.col]) & data[,impute.col] >= max(min(data[censored & !is.na(data[,impute.col]), impute.col]), i-0.5) & data[,impute.col] <= min(max(data[censored & !is.na(data[,impute.col]), impute.col]), i+0.5), impute.col], from = max(min(data[censored & !is.na(data[,impute.col]), impute.col]), i-0.5), to = min(max(data[censored & !is.na(data[,impute.col]), impute.col]), i+0.5))
    data[censored & !is.na(data[,impute.col]) & data[,impute.col] == i, impute.col] <- approx(cumsum(i.pdf$y)/sum(i.pdf$y), i.pdf$x, runif(nrow(data[censored & !is.na(data[,impute.col]) & data[,impute.col] == i,])))$y
  }
  if (replace) {
    return(data)
  } else {
    return(data[,impute.col])
  }
}

max.glmmTMB <- function (formula.string, family = gaussian(), data, crit = "AIC", jump.to.order = max(extract.order(str_split(formula.string,"~")[[1]][2]))) {
  if (!(crit %in% c("AIC","BIC","logLik"))) {
    cat("\nError: invalid ordering criteria.\nMust be 'AIC', 'BIC', or 'logLik'\n")
    break
  }
  max.formula <- str_split(formula.string,"~")[[1]][2]
  left.formula <- str_split(formula.string,"~")[[1]][1]
  this.formula <- paste(left.formula,"~ 1")
  iter <- 0
  rerun <- TRUE
  for (n.int in 1:jump.to.order) {
    cat("\nFitting interactions of order",n.int,"...\n")
    try.formula <- paste(left.formula,"~",format.terms(extract.terms(max.formula)[extract.order(max.formula) <= n.int]))
    mod.null <- tryCatch(glmmTMB(formula = as.formula(try.formula), family = family, data = data), error = function(e) e)
    if (inherits(mod.null,'error')) {
      cat("Model with full interaction terms produced an error.\n")
      break
    } else if (mod.null$fit$convergence == 0) {
      this.formula <- try.formula
      cat("Model successfully fitted!\n")
      cat("\nNew Formula:\n",this.formula,"\n")
    } else {
      cat("Model with full interaction terms failed to converge.\n")
      cat("\nNow adding terms one-by-one.\n")
      break
    }
  }
  while(rerun) {
    iter <- iter + 1
    cat("\nIteration",iter,"\n")
    mod.null <- tryCatch(glmmTMB(formula = as.formula(this.formula), family = family, data = data), error = function(e) e)
    if (inherits(mod.null,'error')) {
      cat("\nError: null model produced an error.\n")
      return(NULL)
    } else if (mod.null$fit$convergence != 0) {
      cat("\nError: null model failed to converge.\n")
      return(NULL)
    }
    add.tab <- as.data.frame(NULL)
    for (i in extract.terms(max.formula)) {
      add.tab[nrow(add.tab)+1,"Term"] <- i
      if (i %in% extract.terms(this.formula)) {
        add.tab[nrow(add.tab),"Add"] <- FALSE
      } else {
        if (length(str_split(i,":")[[1]]) > 1) {
          add.tab[nrow(add.tab),"Add"] <- sum((!(get.terms(str_split(i,":")[[1]], int.order = length(str_split(i,":")[[1]])-1) %in% extract.terms(this.formula)))) == 0
        } else {
          add.tab[nrow(add.tab),"Add"] <- sum((i %in% extract.terms(this.formula))) == 0
        }
      }
      add.tab[nrow(add.tab),"Iteration"] <- iter
      if (add.tab$Add[nrow(add.tab)]) {
        mod.add <- tryCatch(glmmTMB(formula = as.formula(paste(this.formula,"+",add.tab$Term[nrow(add.tab)])), family = family, data = data), error = function(e) e)
        if (inherits(mod.add,'error')) {
          add.tab$Add[nrow(add.tab)] <- FALSE
        } else if (mod.add$fit$convergence == 0) {
          anova.test <- anova(mod.null, mod.add)
          add.tab[nrow(add.tab),crit] <- anova.test[,crit][2] - anova.test[,crit][1]
        } else {
          add.tab$Add[nrow(add.tab)] <- FALSE
        }
      }
    }
    if (nrow(add.tab[add.tab$Add,]) == 0) {
      rerun <- FALSE
      cat("\nModel is at maximum capacity.\n")
    } else if (nrow(add.tab[!is.na(add.tab[,crit]),]) == 0) {
      rerun <- FALSE
      cat("\nModel is at maximum capacity.\n")
    } else {
      this.formula <- paste(this.formula,"+",add.tab$Term[add.tab$Add & add.tab[,crit] == min(add.tab[,crit], na.rm = TRUE)])
      print(add.tab)
      cat("\nAdding ...",add.tab$Term[add.tab$Add & add.tab[,crit] == min(add.tab[,crit], na.rm = TRUE)],"\n")
    }
    cat("\nNew Formula:\n",this.formula,"\n")
  }
  return(as.formula(this.formula))
}

normal.transform <- function (input) {
  return((input - min(input, na.rm = TRUE))/(max(input, na.rm = TRUE) - min(input, na.rm = TRUE)))
}

noninclusive.transform <- function (input, s = mean(range(input, na.rm = TRUE))) {
  N <- sum(!is.na(input))
  return((input*(N - 1) + s)/N)
}

remove.duplicates <- function (term.list) {
  if (length(term.list) > 1) {
    remove.terms <- FALSE
    for (i in 2:length(term.list)) {
      remove.terms <- c(remove.terms, term.list[i] %in% term.list[1:(i-1)])
    }
  }
  return(term.list[!remove.terms])
}

rescale <- function (data, ref = data, left = 0, right = 1) {
  data <- (data - min(ref, na.rm = TRUE))/(max(ref, na.rm = TRUE) - min(ref, na.rm = TRUE))
  data <- data*(right - left) + left
  return(data)
}

normalize <- function (data, ref = data) {
  return(rescale(data = data, ref = ref, left = 0, right = 1))
}

unnormalize <- function (data, ref = data) {
  return(rescale(data = data, ref = ref, left = min(ref, na.rm = TRUE), right = max(ref, na.rm = TRUE)))
}

standardize <- function (data, ref = data) {
  data <- (data - mean(ref, na.rm = TRUE))/sd(ref, na.rm = TRUE)
  return(data)
}

unstandardize <- function (data, ref = data) {
  data <- data*sd(ref, na.rm = TRUE) + mean(ref, na.rm = TRUE)
  return(data)
}

center <- function (data, ref = data) {
  data <- data - mean(ref, na.rm = TRUE)
  return(data)
}

uncenter <- function (data, ref = data) {
  data <- data + mean(ref, na.rm = TRUE)
  return(data)
}

shift <- function (data, ref = data) {
  data <- data - mean(range(ref, na.rm = TRUE))
  return(data)
}

unshift <- function (data, ref = data) {
  data <- data + mean(range(ref, na.rm = TRUE))
  return(data)
}

correct <- function (data, crit) {
  if (length(data) == length(crit)) {
    for (i in 1:length(data)) {
      data[i] <- if (crit[i]) {data[i]} else {1-data[i]}
    }
  } else {
    cat("\nError: data and criteria have different lengths.\n")
  }
  return(data)
}

multistandardize <- function (data, cols) {
  for (i in cols) {
    data[,i] <- (data[,i] - mean(data[,i], na.rm = TRUE))/sd(data[,i], na.rm = TRUE)
  }
  return(data)
}

transform <- function (data, transformations = list(NULL)) {
  for (i in names(transformations)) {
    data[,i] <- transformations[[i]](data[,i])
  }
  return(data)
}

tryModel <- function(code) {
  tryCatch(code, error = function(e) e, warning = function(w) {
    message(conditionMessage(w))
    if (!grepl("converge",w)) {
      cat("\nNot a convergence warning\n")
      code
      } else {w}})
}

variance.pie.old <- function (object, response, est.resid = FALSE) {
  pop.var <- var(response)*(length(response)-1)/length(response)
  mod.summary <- summary(object)
  prop.var <- as.data.frame(NULL)
  for (i in 1:length(mod.summary$varcor$cond)) {
    prop.var[nrow(prop.var)+1,"Category"] <- names(mod.summary$varcor$cond)[i]
    prop.var[nrow(prop.var),"Variance"] <- mod.summary$varcor$cond[[i]][1]
  }
  prop.var[nrow(prop.var)+1,"Category"] <- "Error"
  if (est.resid) {
    prop.var[nrow(prop.var),"Variance"] <- var(residuals(object))
  } else {
    prop.var[nrow(prop.var),"Variance"] <- mod.summary$sigma^2
  }
  prop.var[nrow(prop.var)+1,"Category"] <- "Fixed Effects"
  prop.var[nrow(prop.var),"Variance"] <- pop.var-sum(prop.var$Variance, na.rm = TRUE)
  if (prop.var$Variance[prop.var$Category == "Fixed Effects"] < 0) {prop.var$Variance[prop.var$Category == "Fixed Effects"] <- 0}
  prop.var[,"Proportion"] <- prop.var$Variance/pop.var
  pieplot <- ggplot(data = prop.var, aes(x = "", y = Variance, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = scales::percent(Proportion)), position = position_stack(vjust = 0.5), color = "white", size = 6) +
    coord_polar("y", start = 0) +
    scale_fill_brewer(palette="Set1") +
    theme_void(base_size = 24)
  return(pieplot)
}

variance.pie <- function (object) {
  this.variance <- get_variance(object)
  var.table <- as.data.frame(NULL)
  for (i in names(this.variance$var.intercept)) {
    var.table[nrow(var.table)+1,"Category"] <- i
    var.table[nrow(var.table),"Variance"] <- this.variance$var.intercept[i]
  }
  var.table[nrow(var.table)+1,"Category"] <- "Fixed Effects"
  var.table[nrow(var.table),"Variance"] <- this.variance$var.fixed
  var.table[nrow(var.table)+1,"Category"] <- "Error"
  var.table[nrow(var.table),"Variance"] <- this.variance$var.residual
  var.table[,"Proportion"] <- var.table$Variance/sum(this.variance$var.fixed, this.variance$var.random, this.variance$var.residual)
  pieplot <- ggplot(data = var.table, aes(x = "", y = Variance, fill = Category)) +
    geom_bar(stat = "identity", width = 1) +
    geom_text(aes(label = scales::percent(Proportion)), position = position_stack(vjust = 0.5), color = "white", size = 6) +
    coord_polar("y", start = 0) +
    scale_fill_brewer(palette="Set1") +
    theme_void(base_size = 24)
  return(pieplot)
}

