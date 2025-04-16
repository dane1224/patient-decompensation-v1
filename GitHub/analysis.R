#### GLOBAL SETTINGS ####

# data files.
data.file <- "data.csv" # not publicly available because of stipulations in informed consent process.
cases.file <- "cases.csv"
training.file <- "all_predictions.csv"

# plot parameters.
condition.labels <- c("Without AI Assistance","AI Recommendation Only","AI Recommendation + Explanation","AI Explanation Only")
names(condition.labels) <- c("A","B","C","D")
condition.alt.labels <- c("Without\nAugmentation","Recommendation\nOnly","Recommendation\n+ Explanation","Explanation\nOnly")
names(condition.alt.labels) <- c("A","B","C","D")
condition.order <- c("A","B","C","D")
truth.labels <- c("Emergency Patients","Non-Emergency Patients")
names(truth.labels) <- c("ERT","nonERT")
assisted.label <- "With AI Augmentation"
unassisted.label <- "Without AI Augmentation"
assistance.linetypes <- c("dashed","solid")
names(assistance.linetypes) <- c(unassisted.label,assisted.label)
assistance.shapes <- c(1,19)
names(assistance.shapes) <- c(unassisted.label,assisted.label)
prediction.labels <- c("Very Low\nPredicted Probability","Low\nPredicted Probability","Moderate\nPredicted Probability","High\nPredicted Probability","Very High\nPredicted Probability")
names(prediction.labels) <- c("Low","Medium-Low","Medium","Medium-High","High")
program.labels <- c("2nd Year\nUndergrad","3rd Year\nUndergrad","4th Year\nUndergrad","1st Year\nGraduate","2nd Year\nGraduate","Licensed\nNurse")
names(program.labels) <- c("Undergraduate Sophomore","Undergraduate Junior","Undergraduate Senior","Graduate Sophomore","Graduate Junior","Practitioner")
degree.labels <- c("Undergraduate\nStudent","Graduate\nStudent","Licensed\nNurse")
names(degree.labels) <- c("BSN","MSN","Practitioner")
pred.prior.labels <- c("No Change\n(No Recommendation)","Recommendation\nRemoved","Recommendation\nAdded","No Change\n(Recommendation)")
names(pred.prior.labels) <- c("No No","No Yes","Yes No","Yes Yes")
anno.prior.labels <- c("No Change\n(No Explanation)","Explanation\nRemoved","Explanation\nAdded","No Change\n(Explanation)")
names(anno.prior.labels) <- c("No No","No Yes","Yes No","Yes Yes")

#### LIBRARIES ####

source("functions.R")
library(ggplot2)
library(grid)
library(gridExtra)
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(performance)
library(insight)

#### DATA PREP ####

# import files and basic data cleaning.
dat <- read.csv(data.file)
dat <- dat[,2:ncol(dat)]
cases <- read.csv(cases.file)
cases <- cases[cases$case != 0,]
predictions <- read.csv(training.file)
predictions <- predictions[,2:ncol(predictions)]

# data exclusions.
dat <- dat[dat$SurveyID %in% c("AU22","SP23","AU23","AU22extra","SU24"),]
dat <- dat[dat$PredictionFormat == "percentage",]

# exclusions based on time.
summary(dat$AdjustedTime)/60
dat <- dat[(!is.na(dat$AdjustedTime) & dat$AdjustedTime < 6*60) | (is.na(dat$AdjustedTime) & dat$Duration < 6*10*60),]

# add display condition fields.
dat[dat$ConditionID == "A" | dat$ConditionID == "D","pred.ON"] <- "No"
dat[dat$ConditionID == "B" | dat$ConditionID == "C","pred.ON"] <- "Yes"
dat[dat$ConditionID == "A" | dat$ConditionID == "B","anno.ON"] <- "No"
dat[dat$ConditionID == "C" | dat$ConditionID == "D","anno.ON"] <- "Yes"
dat[dat$PriorConditionID == "A" | dat$PriorConditionID == "D", "pred.prior"] <- "No"
dat[dat$PriorConditionID == "B" | dat$PriorConditionID == "C", "pred.prior"] <- "Yes"
dat[dat$PriorConditionID == "A" | dat$PriorConditionID == "B", "anno.prior"] <- "No"
dat[dat$PriorConditionID == "C" | dat$PriorConditionID == "D", "anno.prior"] <- "Yes"

# add case characteristics.
dat[dat$CaseID == cases$case[cases$truth == "ERT"][order(cases$prediction[cases$truth == "ERT"])][1] | dat$CaseID == cases$case[cases$truth == "nonERT"][order(cases$prediction[cases$truth == "nonERT"])][1],"pred.order"] <- "Low"
dat[dat$CaseID == cases$case[cases$truth == "ERT"][order(cases$prediction[cases$truth == "ERT"])][2] | dat$CaseID == cases$case[cases$truth == "nonERT"][order(cases$prediction[cases$truth == "nonERT"])][2],"pred.order"] <- "Medium-Low"
dat[dat$CaseID == cases$case[cases$truth == "ERT"][order(cases$prediction[cases$truth == "ERT"])][3] | dat$CaseID == cases$case[cases$truth == "nonERT"][order(cases$prediction[cases$truth == "nonERT"])][3],"pred.order"] <- "Medium"
dat[dat$CaseID == cases$case[cases$truth == "ERT"][order(cases$prediction[cases$truth == "ERT"])][4] | dat$CaseID == cases$case[cases$truth == "nonERT"][order(cases$prediction[cases$truth == "nonERT"])][4],"pred.order"] <- "Medium-High"
dat[dat$CaseID == cases$case[cases$truth == "ERT"][order(cases$prediction[cases$truth == "ERT"])][5] | dat$CaseID == cases$case[cases$truth == "nonERT"][order(cases$prediction[cases$truth == "nonERT"])][5],"pred.order"] <- "High"

# add years of experience.
dat[dat$License == "Prelicensure","ExperienceCategory"] <- dat$EducationCategory[dat$License == "Prelicensure"]
dat[dat$License == "Prelicensure","YearsExperience"] <- dat$YearsEducation[dat$License == "Prelicensure"]
dat[dat$License == "Licensed","ExperienceCategory"] <- "Practitioner"
dat[dat$License == "Licensed","YearsExperience"] <- dat$YearsEducation[dat$License == "Licensed"] + dat$YearsWork[dat$License == "Licensed"]
dat[dat$ExperienceCategory == "Practitioner","ProgramCategory"] <- "Practitioner"
dat[dat$ExperienceCategory != "Practitioner","ProgramCategory"] <- paste(dat$Program[dat$ExperienceCategory != "Practitioner"], dat$EducationCategory[dat$ExperienceCategory != "Practitioner"])
dat[dat$Student == "Yes" & as.numeric(dat$CourseID) < 6000,"Degree"] <- "BSN"
dat[dat$Student == "Yes" & as.numeric(dat$CourseID) >= 6000,"Degree"] <- "MSN"
dat[dat$Student == "No","Degree"] <- "Practitioner"

# add data variations.
dat[,"LogYearsExperience"] <- log(dat$YearsExperience) # supported by Takase (2013)

# add censored indicator.
dat[,"CensoredID"] <- dat$DataType == "Categorical"

# format data types.
dat$CaseID <- factor(dat$CaseID)
dat$Truth <- factor(dat$Truth)
dat$ParticipantID <- factor(dat$ParticipantID)
dat$ConditionID <- factor(dat$ConditionID)
dat$pred.ON <- factor(dat$pred.ON)
dat$anno.ON <- factor(dat$anno.ON)
dat$PriorConditionID <- factor(dat$PriorConditionID)
dat$pred.prior <- factor(dat$pred.prior)
dat$anno.prior <- factor(dat$anno.prior)
dat$pred.order <- factor(dat$pred.order, levels = c("High","Medium-High","Medium","Medium-Low","Low"), ordered = TRUE)

# summarize by case and condition.
basic.summary <- as.data.frame(NULL)
for (i in 1:10) {
  for (j in c("No","Yes")) {
    for (k in c("No","Yes")) {
      basic.summary[nrow(basic.summary)+1,"CaseID"] <- i
      if (j == "No" & k == "No") {
        basic.summary[nrow(basic.summary),"ConditionID"] <- "A"
      } else if (j == "Yes" & k == "No") {
        basic.summary[nrow(basic.summary),"ConditionID"] <- "B"
      } else if (j == "Yes" & k == "Yes") {
        basic.summary[nrow(basic.summary),"ConditionID"] <- "C"
      } else if (j == "No" & k == "Yes") {
        basic.summary[nrow(basic.summary),"ConditionID"] <- "D"
      }
      basic.summary[nrow(basic.summary),"pred.ON"] <- j
      basic.summary[nrow(basic.summary),"anno.ON"] <- k
      basic.summary[nrow(basic.summary),"Truth"] <- unique(dat$Truth[dat$CaseID == i])
      basic.summary[nrow(basic.summary),"Prediction"] <- unique(dat$Prediction[dat$CaseID == i])
      basic.summary[nrow(basic.summary),"Average"] <- mean(dat$Concern[dat$CaseID == i & dat$pred.ON == j & dat$anno.ON == k], na.rm = TRUE)
      basic.summary[nrow(basic.summary),"SD"] <- sd(dat$Concern[dat$CaseID == i & dat$pred.ON == j & dat$anno.ON == k], na.rm = TRUE)
      basic.summary[nrow(basic.summary),"n"] <- nrow(dat[!is.na(dat$Concern) & dat$CaseID == i & dat$pred.ON == j & dat$anno.ON == k,])
      basic.summary[nrow(basic.summary),c("Lower.CI","Upper.CI")] <- t.test(dat$Concern[dat$CaseID == i & dat$pred.ON == j & dat$anno.ON == k])$conf.int[1:2]
      basic.summary[nrow(basic.summary),"Error"] <- if (basic.summary$Truth[nrow(basic.summary)] == "ERT") {1-basic.summary$Prediction[nrow(basic.summary)]} else {basic.summary$Prediction[nrow(basic.summary)]}
      basic.summary[nrow(basic.summary),"Performance"] <- if (basic.summary$Truth[nrow(basic.summary)] == "ERT") {basic.summary$Average[nrow(basic.summary)]/10} else {1-basic.summary$Average[nrow(basic.summary)]/10}
    }
  }
}

# reformat prediction events.
predictions$Event <- c("nonERT","ERT")[predictions$Event+1]
predictions$Event <- factor(predictions$Event)

# create summary of participants (used in the model).
participants <- as.data.frame(NULL)
for (i in unique(dat$ParticipantID)) {
  if (nrow(dat[!is.na(dat$Concern) & dat$ParticipantID == i,]) > 0) {
    participants[nrow(participants)+1,"ParticipantID"] <- i
    participants[nrow(participants),"Age"] <- unique(dat$Age[dat$ParticipantID == i])
    participants[nrow(participants),"Student"] <- unique(dat$Student[dat$ParticipantID == i])
    participants[nrow(participants),"Degree"] <- unique(dat$Degree[dat$ParticipantID == i])
    participants[nrow(participants),"License"] <- unique(dat$License[dat$ParticipantID == i])
    participants[nrow(participants),"EducationCategory"] <- unique(dat$EducationCategory[dat$ParticipantID == i])
    participants[nrow(participants),"YearsEducation"] <- unique(dat$YearsEducation[dat$ParticipantID == i])
    participants[nrow(participants),"ExperienceCategory"] <- unique(dat$ExperienceCategory[dat$ParticipantID == i])
    participants[nrow(participants),"YearsExperience"] <- unique(dat$YearsExperience[dat$ParticipantID == i])
    participants[nrow(participants),"Program"] <- unique(dat$Program[dat$ParticipantID == i])
    participants[nrow(participants),"YearsWork"] <- unique(dat$YearsWork[dat$ParticipantID == i])
    participants[nrow(participants),"ProgramCategory"] <- unique(dat$ProgramCategory[dat$ParticipantID == i])
  }
}

# print finished message.
cat("\nFinished data preparation.\n")

# end data prep.

#### DATA SUMMARY ####

# summarize number of participants available for model.
cat("\nFor concern model,",length(unique(dat$ParticipantID[!is.na(dat$Concern) & dat$ExperienceCategory != "Practitioner"])),"students and",length(unique(dat$ParticipantID[!is.na(dat$Concern) & dat$ExperienceCategory == "Practitioner"])),"nurses completed",nrow(dat[!is.na(dat$Concern),]),"cases, including:")
cat("\n",length(unique(dat$ParticipantID[!is.na(dat$Concern) & dat$ExperienceCategory == "Sophomore"])),"Sophomores completing",nrow(dat[!is.na(dat$Concern) & dat$ExperienceCategory == "Sophomore",]),"cases.")
cat("\n",length(unique(dat$ParticipantID[!is.na(dat$Concern) & dat$ExperienceCategory == "Junior"])),"Juniors completing",nrow(dat[!is.na(dat$Concern) & dat$ExperienceCategory == "Junior",]),"cases.")
cat("\n",length(unique(dat$ParticipantID[!is.na(dat$Concern) & dat$ExperienceCategory == "Senior"])),"Seniors completing",nrow(dat[!is.na(dat$Concern) & dat$ExperienceCategory == "Senior",]),"cases.")
cat("\n",length(unique(dat$ParticipantID[!is.na(dat$Concern) & dat$ExperienceCategory == "Practitioner"])),"Licensed Nurses completing",nrow(dat[!is.na(dat$Concern) & dat$ExperienceCategory == "Practitioner",]),"cases.")

# summarize ages of participants.
range(dat$Age[!is.na(dat$Concern) & dat$Student == "Yes"], na.rm = TRUE) # all students
range(dat$Age[!is.na(dat$Concern) & dat$ExperienceCategory == "Practitioner"], na.rm = TRUE) # any licensed nurse

# summarize years of education and years of experience.
range(dat$YearsEducation[!is.na(dat$Concern) & dat$Student == "Yes"]) # all students
range(dat$YearsWork[!is.na(dat$Concern) & dat$ExperienceCategory == "Practitioner"]) # any licensed nurse

# summarize proportion of students who were interval censored.
length(unique(dat$ParticipantID[!is.na(dat$Concern) & dat$DataType == "Categorical"]))
length(unique(dat$ParticipantID[!is.na(dat$Concern) & dat$DataType == "Categorical"]))/length(unique(dat$ParticipantID[!is.na(dat$Concern)]))

# data summary.

#### PREDICTION GRAPHS ####

# density plot of algorithm training data.
ggplot(data = predictions, aes(x = prediction, group = Event)) +
  geom_histogram(aes(y = ..density..), breaks = seq(0,1,0.05), fill = "gray", color = "black") +
  geom_line(aes(y = ..density..), stat = 'density', size = 2) +
  scale_x_continuous(breaks = seq(0,1,0.1), labels = scales::percent_format()) +
  labs(x = "AI Recommendation (Predicted Probability)", y = "Density") +
  theme_linedraw(base_size = 16) +
  facet_grid(rows = vars(Event), labeller = labeller(Event = truth.labels))

# descriptive statistics about algorithm predictions.
summary(predictions$prediction[predictions$Event == "ERT"])
summary(predictions$prediction[predictions$Event == "nonERT"])

# algorithm predictions.

#### CALIBRATION GRAPHS ####

# compute calibration by deciles.
calibration <- as.data.frame(NULL)
prediction.deciles <- quantile(predictions$prediction, seq(0,1,0.1))
for (i in 1:(length(prediction.deciles)-1)) {
  calibration[nrow(calibration)+1,"DecileOrder"] <- i
  calibration[nrow(calibration),"BinStart"] <- prediction.deciles[i]
  calibration[nrow(calibration),"BinEnd"] <- prediction.deciles[i+1]
  calibration[nrow(calibration),"MeanPredicted"] <- mean(predictions$prediction[predictions$prediction >= prediction.deciles[i] & predictions$prediction <= prediction.deciles[i+1]])
  calibration[nrow(calibration),"CountPredicted"] <- nrow(predictions[predictions$prediction >= prediction.deciles[i] & predictions$prediction <= prediction.deciles[i+1],])
  calibration[nrow(calibration),"CountERT"] <- nrow(predictions[predictions$Event == "ERT" & predictions$prediction >= prediction.deciles[i] & predictions$prediction <= prediction.deciles[i+1],])
  calibration[nrow(calibration),"PropERT"] <- calibration[nrow(calibration),"CountERT"]/calibration[nrow(calibration),"CountPredicted"]
  this.test <- prop.test(x = calibration[nrow(calibration),"CountERT"], 
                         n = calibration[nrow(calibration),"CountPredicted"],
                         p = calibration[nrow(calibration),"MeanPredicted"])
  calibration[nrow(calibration),c("lower.CI","upper.CI")] <- this.test$conf.int
  calibration[nrow(calibration),"p.value"] <- this.test$p.value
}

# calibration plot.
ggplot(data = calibration, aes(x = MeanPredicted, y = PropERT)) +
  annotate("segment", x = 0, xend = 1, y = 0, yend = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = lower.CI, ymax= upper.CI), alpha = 0.2) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2), labels = scales::percent_format()) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2), labels = scales::percent_format()) +
  labs(x = "Mean AI Recommendation (Predicted Probability) within Decile", y = "Proportion of Emergency Patients") +
  theme_linedraw(base_size = 16)

# calibration plot.

#### EXPERIENCE GRAPHS ####

# violin plot for age.
plot.age <- ggplot(data = participants, 
                   aes(x = factor(ProgramCategory, levels = c("Undergraduate Sophomore","Undergraduate Junior","Undergraduate Senior","Graduate Sophomore","Graduate Junior","Practitioner"), ordered = TRUE), 
                       y = Age)) +
  geom_violin(scale = "area", adjust = 2, fill = "gray", alpha = 0.5) +
  geom_boxplot(width = 0.05, fill = NA) +
  scale_x_discrete(labels = program.labels) +
  labs(x = NULL, y = "Age (in Years)") +
  theme_linedraw(base_size = 16)

# violin plot for student education.
plot.education <- ggplot(data = participants[participants$Student == "Yes",], aes(x = Degree, y = YearsEducation)) +
  geom_violin(scale = "area", adjust = 2, fill = "gray", alpha = 0.5) +
  geom_boxplot(width = 0.05, fill = NA) +
  scale_x_discrete(labels = degree.labels) +
  scale_y_continuous(limits = c(0,4.25)) +
  labs(x = NULL, y = "Years of Nursing Education") +
  theme_linedraw(base_size = 16)

# violin plot for nurse years of work experience.
plot.work <- ggplot(data = participants[participants$ProgramCategory == "Practitioner",], aes(x = ProgramCategory, y = YearsWork)) + 
  geom_violin(scale = "area", adjust = 1, fill = "gray", alpha = 0.5) +
  geom_boxplot(width = 0.05, fill = NA) + 
  scale_x_discrete(labels = program.labels) +
  scale_y_continuous(limits = c(0,25)) +
  labs(x = NULL, y = "Years of Work Experience") +
  theme_linedraw(base_size = 16)

# combined plot.
grid.arrange(plot.age, plot.education, plot.work, layout_matrix = rbind(c(1,1,1),
                                                                        c(2,2,3)))

# descriptive plots.

#### CONCERN GRAPHS ####

# violin plot of experimental condition.
ggplot(data = dat[!is.na(dat$Concern),], aes(x = ConditionID, y = Concern)) +
  geom_hline(aes(yintercept = 10*Prediction), linetype = "dashed") +
  geom_violin(scale = "area", fill = "gray", alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = NA) + 
  scale_x_discrete(labels = condition.alt.labels) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  labs(x = "Experimental Condition", y = "Nurse Concern") +
  theme_linedraw(base_size = 12) +
  facet_grid(rows = vars(pred.order), cols = vars(Truth), labeller = labeller(Truth = truth.labels, pred.order = prediction.labels))

# violin plot of trial number.
ggplot(data = dat[!is.na(dat$Concern),], aes(x = factor(OrderID), y = Concern)) +
  geom_hline(aes(yintercept = 10*Prediction), linetype = "dashed") +
  geom_violin(scale = "area", fill = "gray", alpha = 0.5) +
  geom_boxplot(width = 0.15, fill = NA) + 
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  labs(x = "Trial Number", y = "Nurse Concern") +
  theme_linedraw(base_size = 12) +
  facet_grid(rows = vars(pred.order), cols = vars(Truth), labeller = labeller(Truth = truth.labels, pred.order = prediction.labels))

# violin plot of participant groups.
ggplot(data = dat[!is.na(dat$Concern),], aes(x = factor(ProgramCategory, levels = c("Undergraduate Sophomore","Undergraduate Junior","Undergraduate Senior","Graduate Sophomore","Graduate Junior","Practitioner"), ordered = TRUE), y = Concern)) +
  geom_hline(aes(yintercept = 10*Prediction), linetype = "dashed") +
  geom_violin(scale = "area", fill = "gray", alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = NA) + 
  scale_x_discrete(labels = program.labels) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  labs(x = "Experience Category", y = "Nurse Concern") +
  theme_linedraw(base_size = 12) +
  facet_grid(rows = vars(pred.order), cols = vars(Truth), labeller = labeller(Truth = truth.labels, pred.order = prediction.labels))

# violin plot of switching recommendation conditions.
ggplot(data = dat[!is.na(dat$Concern),], aes(x = paste(pred.ON, pred.prior), y = Concern)) +
  geom_hline(aes(yintercept = 10*Prediction), linetype = "dashed") +
  geom_violin(scale = "area", fill = "gray", alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = NA) + 
  scale_x_discrete(labels = pred.prior.labels) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  labs(x = "Change from Previous Experimental Condition", y = "Nurse Concern") +
  theme_linedraw(base_size = 12) +
  facet_grid(rows = vars(pred.order), cols = vars(Truth), labeller = labeller(Truth = truth.labels, pred.order = prediction.labels))

# violin plot of switching explanation conditions.
ggplot(data = dat[!is.na(dat$Concern),], aes(x = paste(anno.ON, anno.prior), y = Concern)) +
  geom_hline(aes(yintercept = 10*Prediction), linetype = "dashed") +
  geom_violin(scale = "area", fill = "gray", alpha = 0.5) +
  geom_boxplot(width = 0.1, fill = NA) + 
  scale_x_discrete(labels = anno.prior.labels) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,2)) +
  labs(x = "Change from Previous Experimental Condition", y = "Nurse Concern") +
  theme_linedraw(base_size = 12) +
  facet_grid(rows = vars(pred.order), cols = vars(Truth), labeller = labeller(Truth = truth.labels, pred.order = prediction.labels))

# ConditionID, OrderID, ProgramCategory, pred.prior, anno.prior

#### T-TESTS ####

# one-sample t-tests comparing to algorithm prediction.
t.test(dat$Concern[dat$ConditionID == "A" & dat$CaseID == 1], mu = 10*cases$prediction[cases$case == 1])
t.test(dat$Concern[dat$ConditionID == "A" & dat$CaseID == 2], mu = 10*cases$prediction[cases$case == 2])
t.test(dat$Concern[dat$ConditionID == "A" & dat$CaseID == 3], mu = 10*cases$prediction[cases$case == 3])
t.test(dat$Concern[dat$ConditionID == "A" & dat$CaseID == 4], mu = 10*cases$prediction[cases$case == 4])
t.test(dat$Concern[dat$ConditionID == "A" & dat$CaseID == 5], mu = 10*cases$prediction[cases$case == 5])
t.test(dat$Concern[dat$ConditionID == "A" & dat$CaseID == 6], mu = 10*cases$prediction[cases$case == 6])
t.test(dat$Concern[dat$ConditionID == "A" & dat$CaseID == 7], mu = 10*cases$prediction[cases$case == 7])
t.test(dat$Concern[dat$ConditionID == "A" & dat$CaseID == 8], mu = 10*cases$prediction[cases$case == 8])
t.test(dat$Concern[dat$ConditionID == "A" & dat$CaseID == 9], mu = 10*cases$prediction[cases$case == 9])
t.test(dat$Concern[dat$ConditionID == "A" & dat$CaseID == 10], mu = 10*cases$prediction[cases$case == 10])

# condition A tests.

#### COMPLEMENTARITY GRAPHS ####

A.vs.AI <- as.data.frame(NULL)
for (i in 1:10) {
  A.vs.AI[nrow(A.vs.AI)+1,"CaseID"] <- i
  A.vs.AI[nrow(A.vs.AI),"ConditionID"] <- "A"
  A.vs.AI[nrow(A.vs.AI),"Truth"] <- cases$truth[cases$case == i]
  A.vs.AI[nrow(A.vs.AI),"Prediction"] <- cases$prediction[cases$case == i]
  this.test <- t.test(dat$Concern[dat$ConditionID == A.vs.AI$ConditionID[nrow(A.vs.AI)] & dat$CaseID == i], mu = 10*cases$prediction[cases$case == i])
  A.vs.AI[nrow(A.vs.AI),"mean"] <- this.test$estimate
  A.vs.AI[nrow(A.vs.AI),names(this.test$statistic)] <- this.test$statistic
  A.vs.AI[nrow(A.vs.AI),names(this.test$paramter)] <- this.test$paramter
  A.vs.AI[nrow(A.vs.AI),"p.value"] <- this.test$p.value
  A.vs.AI[nrow(A.vs.AI),c("lower.CI","upper.CI")] <- this.test$conf.int
  A.vs.AI[nrow(A.vs.AI),"significance"] <- if(this.test$p.value < 0.05/10) {"*"} else {""}
}

ggplot(data = A.vs.AI, aes(x = Prediction)) +
  annotate("segment", x = 0, xend = 1, y = 0, yend = 10, linetype = "dashed", size = 0.5) + 
  geom_text(data = data.frame(x = c(0.3,0.7,0.3,0.7), 
                              y = c(9.5,0.5,9.5,0.5), 
                              label = c("Better than AI","Worse than AI","Worse than AI","Better than AI"), 
                              Truth = c("ERT","ERT","nonERT","nonERT")), 
            aes(x = x, y = y, label = label), size = 8) + 
  geom_errorbar(aes(ymin = lower.CI, ymax = upper.CI), size = 0.5) +
  geom_text(aes(y = upper.CI + 0.1, label = significance), size = 8) +
  geom_point(aes(x = Prediction, y = 10*Prediction), size = 4, shape = 23, fill = "black") +
  geom_point(aes(x = Prediction, y = mean), size = 4, shape = 21) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2), labels = scales::percent) +
  scale_y_continuous(limits = c(0,10), breaks = seq(0,10,1)) + 
  labs(x = "AI Recommendation (Predicted Probability)", y = "Nurse Concern Without AI Assistance") +
  theme_linedraw(base_size = 20) +
  facet_grid(cols = vars(Truth), labeller = labeller(Truth = truth.labels))

#### MODEL EXPLORATION ####

# Iteration 1:

# set new base formula.
base.formula <- paste("Concern ~",format.terms(remove.duplicates(c(
  get.terms(c("Truth","Prediction","pred.ON","anno.ON")), # base experiment.
  "(1|CaseID)","(1|ParticipantID)"))), collapse = " ") # model foundation.

# explore model extensions ... 
basic.extension1 <- compare.glmmTMB(
  base.formula = paste(base.formula, collapse = " "),
  term.list = remove.duplicates(c(
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","OrderID","ExperienceCategory")),
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","OrderID","ProgramCategory")),
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","OrderID","Degree")),
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","OrderID","YearsExperience")),
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","OrderID","LogYearsExperience")),
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","pred.prior","anno.prior"))
  )),
  family = ordbeta(),
  data = transform(
    dat[!is.na(dat$Concern),], 
    list(Concern = normalize,
         Prediction = shift,
         OrderID = shift,
         YearsExperience = standardize,
         LogYearsExperience = center)),
  crit = "BIC",
  control = glmmTMBControl(optCtrl = list(iter.max = 2000, eval.max = 2000))
)

# explore most promising model extension ... 
basic.step1 <- stepwise.glmmTMB(
  start.formula = paste(
    "Concern ~",
    format.terms(remove.duplicates(c(
      get.terms(c("Truth","Prediction","pred.ON","anno.ON")), # base experiment.
      get.terms(c("Truth","Prediction","pred.ON","OrderID")), # promising model extension.
      "(1|CaseID)","(1|ParticipantID)" # model foundation.
    ))), collapse = " "),
  family = ordbeta(),
  data = transform(
    dat[!is.na(dat$Concern),], 
    list(Concern = normalize,
         Prediction = shift,
         OrderID = shift,
         YearsExperience = standardize,
         LogYearsExperience = center)),
  direction = "bidirectional",
  crit = "BIC",
  retain = remove.duplicates(c(
    get.terms(c("Truth","Prediction","pred.ON","anno.ON")),
    "(1|CaseID)","(1|ParticipantID)"
  )),
  control = glmmTMBControl(optCtrl = list(iter.max = 2000, eval.max = 2000)),
  impute.col = "Concern",
  impute.freq = "last",
  censored = "CensoredID",
  iterations = 5
)

# conclusion ... add pred.ON:OrderID to model.

# Iteration 2:

# set new base formula.
base.formula <- paste("Concern ~",format.terms(remove.duplicates(c(
  get.terms(c("Truth","Prediction","pred.ON","anno.ON")), # base experiment.
  get.terms(c("pred.ON","OrderID")), # iteration 1.
  "(1|CaseID)","(1|ParticipantID)"))), collapse = " ") # model foundation.

# explore model extensions ... 
basic.extension2 <- compare.glmmTMB(
  base.formula = paste(base.formula, collapse = " "),
  term.list = remove.duplicates(c(
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","OrderID","ExperienceCategory")),
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","OrderID","ProgramCategory")),
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","OrderID","Degree")),
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","OrderID","YearsExperience")),
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","OrderID","LogYearsExperience")),
    get.terms(c("Truth","Prediction","pred.ON","anno.ON","pred.prior","anno.prior"))
  )),
  family = ordbeta(),
  data = transform(
    dat[!is.na(dat$Concern),], 
    list(Concern = normalize,
         Prediction = shift,
         OrderID = shift,
         YearsExperience = standardize,
         LogYearsExperience = center)),
  crit = "BIC",
  control = glmmTMBControl(optCtrl = list(iter.max = 2000, eval.max = 2000))
)

# explore most promising model extension ... 
basic.step2 <- stepwise.glmmTMB(
  start.formula = paste(
    "Concern ~",
    format.terms(remove.duplicates(c(
      get.terms(c("Truth","Prediction","pred.ON","anno.ON")), # base experiment.
      get.terms(c("pred.ON","OrderID")), # iteration 1.
      get.terms(c("Truth","Prediction","OrderID","LogYearsExperience")), # promising model extension.
      "(1|CaseID)","(1|ParticipantID)" # model foundation.
    ))), collapse = " "),
  family = ordbeta(),
  data = transform(
    dat[!is.na(dat$Concern),], 
    list(Concern = normalize,
         Prediction = shift,
         OrderID = shift,
         YearsExperience = standardize,
         LogYearsExperience = center)),
  direction = "bidirectional",
  crit = "BIC",
  retain = remove.duplicates(c(
    get.terms(c("Truth","Prediction","pred.ON","anno.ON")),
    "(1|CaseID)","(1|ParticipantID)"
  )),
  control = glmmTMBControl(optCtrl = list(iter.max = 2000, eval.max = 2000)),
  impute.col = "Concern",
  impute.freq = "last",
  censored = "CensoredID",
  iterations = 5
)

# conclusion ... add no more terms.

#### FINAL MODEL ####

# mixed effects model of concern.
mod.Concern <- glmmTMB(
  formula = formula(paste(
    "Concern ~",
    format.terms(remove.duplicates(c(
      get.terms(c("Truth","Prediction","pred.ON","anno.ON")),
      get.terms(c("pred.ON","OrderID")),
      "(1|CaseID)","(1|ParticipantID)"
    ))), collapse = " ")),
  family = ordbeta(),
  data = transform(
    dat[!is.na(dat$Concern),], 
    list(Concern = normalize,
         Prediction = shift,
         OrderID = shift)),
  control = glmmTMBControl(optCtrl = list(iter.max = 2000, eval.max = 2000))
)
if (mod.Concern$fit$convergence == 0) {cat("\nModel successfully converged!\n")} else {cat("\nModel failed to converge.\n")}
summary(mod.Concern)

# check collinearities ... 
check_collinearity(mod.Concern)

# check DHARMa ... 
simulationOutput <- simulateResiduals(fittedModel = mod.Concern, plot = T)
plot(simulationOutput)

#### JOINT PERFORMANCE GRAPHS ####

# calculate estimated marginal means.
emm <- emmeans(mod.Concern, ~ Truth + Prediction + pred.ON + anno.ON + OrderID, type = "response",
               data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
               at = list(Prediction = shift(seq(0,1,0.01), dat$Prediction)))
emm <- summary(emm)
emm[emm$pred.ON == "No" & emm$anno.ON == "No","ConditionID"] <- "A"
emm[emm$pred.ON == "Yes" & emm$anno.ON == "No","ConditionID"] <- "B"
emm[emm$pred.ON == "Yes" & emm$anno.ON == "Yes","ConditionID"] <- "C"
emm[emm$pred.ON == "No" & emm$anno.ON == "Yes","ConditionID"] <- "D"
emm$Prediction <- unshift(emm$Prediction, dat$Prediction)
emm$OrderID <- unshift(emm$OrderID, dat$OrderID)
emm$ConditionID <- factor(emm$ConditionID, levels = condition.order, ordered = TRUE)
emm[emm$Truth == "ERT","Performance"] <- emm$response[emm$Truth == "ERT"]
emm[emm$Truth == "nonERT","Performance"] <- 1-emm$response[emm$Truth == "nonERT"]
emm[emm$Truth == "ERT","Performance.LCL"] <- emm$asymp.LCL[emm$Truth == "ERT"]
emm[emm$Truth == "nonERT","Performance.LCL"] <- 1-emm$asymp.LCL[emm$Truth == "nonERT"]
emm[emm$Truth == "ERT","Performance.UCL"] <- emm$asymp.UCL[emm$Truth == "ERT"]
emm[emm$Truth == "nonERT","Performance.UCL"] <- 1-emm$asymp.UCL[emm$Truth == "nonERT"]
emm[emm$Truth == "ERT","Error"] <- 1-emm$Prediction[emm$Truth == "ERT"]
emm[emm$Truth == "nonERT","Error"] <- emm$Prediction[emm$Truth == "nonERT"]
emm[emm$ConditionID == "A","Assistance"] <- unassisted.label
emm[emm$ConditionID != "A","Assistance"] <- assisted.label

# calculate effect size.
for (i in 1:nrow(emm)) {
  emm[i,"EffectSize"] <- (emm$response[i] - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == emm$Truth[i] & emm$Prediction == emm$Prediction[i]])/(emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "ERT" & emm$Prediction == emm$Prediction[i]] - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "nonERT" & emm$Prediction == emm$Prediction[i]])
  emm[i,"EffectSize.LCL"] <- (emm$asymp.LCL[i] - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == emm$Truth[i] & emm$Prediction == emm$Prediction[i]])/(emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "ERT" & emm$Prediction == emm$Prediction[i]] - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "nonERT" & emm$Prediction == emm$Prediction[i]])
  emm[i,"EffectSize.UCL"] <- (emm$asymp.UCL[i] - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == emm$Truth[i] & emm$Prediction == emm$Prediction[i]])/(emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "ERT" & emm$Prediction == emm$Prediction[i]] - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "nonERT" & emm$Prediction == emm$Prediction[i]])
}
emm$EffectSize[emm$Truth == "nonERT"] <- -emm$EffectSize[emm$Truth == "nonERT"]
emm$EffectSize.LCL[emm$Truth == "nonERT"] <- -emm$EffectSize.LCL[emm$Truth == "nonERT"]
emm$EffectSize.UCL[emm$Truth == "nonERT"] <- -emm$EffectSize.UCL[emm$Truth == "nonERT"]

# split into two data frames.
emm.A <- emm[emm$ConditionID == "A",]
emm.BCD <- emm[emm$ConditionID != "A",]
emm.BCD[,"PlotID"] <- emm.BCD$ConditionID

# calculate effect size for unmodeled means.
basic.summary[basic.summary$ConditionID == "A","Assistance"] <- unassisted.label
basic.summary[basic.summary$ConditionID != "A","Assistance"] <- assisted.label
for (i in 1:nrow(basic.summary)) {
  basic.summary[i,"EffectSize"] <- (basic.summary$Average[i]/10 - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == basic.summary$Truth[i] & round(emm$Prediction, 2) == basic.summary$Prediction[i]])/(emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "ERT" & round(emm$Prediction, 2) == basic.summary$Prediction[i]] - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "nonERT" & round(emm$Prediction, 2) == basic.summary$Prediction[i]])
  basic.summary[i,"EffectSize.Lower"] <- (basic.summary$Lower.CI[i]/10 - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == basic.summary$Truth[i] & round(emm$Prediction, 2) == basic.summary$Prediction[i]])/(emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "ERT" & round(emm$Prediction, 2) == basic.summary$Prediction[i]] - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "nonERT" & round(emm$Prediction, 2) == basic.summary$Prediction[i]])
  basic.summary[i,"EffectSize.Upper"] <- (basic.summary$Upper.CI[i]/10 - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == basic.summary$Truth[i] & round(emm$Prediction, 2) == basic.summary$Prediction[i]])/(emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "ERT" & round(emm$Prediction, 2) == basic.summary$Prediction[i]] - emm$response[emm$pred.ON == "No" & emm$anno.ON == "No" & emm$Truth == "nonERT" & round(emm$Prediction, 2) == basic.summary$Prediction[i]])
}
basic.summary$EffectSize[basic.summary$Truth == "nonERT"] <- -basic.summary$EffectSize[basic.summary$Truth == "nonERT"]
basic.summary$EffectSize.Lower[basic.summary$Truth == "nonERT"] <- -basic.summary$EffectSize.Lower[basic.summary$Truth == "nonERT"]
basic.summary$EffectSize.Upper[basic.summary$Truth == "nonERT"] <- -basic.summary$EffectSize.Upper[basic.summary$Truth == "nonERT"]
summary.A <- basic.summary[basic.summary$ConditionID == "A",]
summary.BCD <- basic.summary[basic.summary$ConditionID != "A",]
summary.BCD[,"PlotID"] <- summary.BCD$ConditionID

# 2x3 joint performance graph with percent difference
ggplot(data = emm.BCD, aes(group = ConditionID)) +
  geom_errorbar(data = summary.A, aes(x = Error, ymin = EffectSize.Lower, ymax = EffectSize.Upper)) +
  geom_errorbar(data = summary.BCD, aes(x = Error, ymin = EffectSize.Lower, ymax = EffectSize.Upper)) +
  geom_point(data = summary.A, aes(x = Error, y = EffectSize, shape = Assistance), size = 3) +
  geom_point(data = summary.BCD, aes(x = Error, y = EffectSize, shape = Assistance), size = 3) +
  geom_ribbon(data = emm.A, aes(x = Error, ymin = EffectSize.LCL, ymax = EffectSize.UCL), alpha = 0.2, color = NA) + 
  geom_ribbon(aes(x = Error, ymin = EffectSize.LCL, ymax = EffectSize.UCL), alpha = 0.2, color = NA) + 
  geom_line(data = emm.A, aes(x = Error, y = EffectSize, linetype = Assistance), linewidth = 1) +
  geom_line(aes(x = Error, y = EffectSize, linetype = Assistance), linewidth = 1) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.2), labels = scales::percent) +
  scale_y_continuous(limits = range(c(emm.BCD$EffectSize.LCL, emm.BCD$EffectSize.UCL)), breaks = seq(-2,1.5,0.5), labels = scales::percent) +
  scale_linetype_manual(values = assistance.linetypes) + 
  scale_shape_manual(values = assistance.shapes) +
  labs(y = "Impacts of AI Augmentation on Human-AI Performance", x = "Magnitude of AI Error", linetype = "Experimental Condition", shape = "Experimental Condition") +
  theme_linedraw(base_size = 16) +
  theme(legend.position = "top", legend.key.width = unit(2, "cm")) +
  facet_grid(rows = vars(Truth), cols = vars(PlotID), labeller = labeller(Truth = truth.labels, PlotID = condition.labels))

# main results figure.


#### RESULTS ####

# emmeans for each case.
emm.cases <- emmeans(mod.Concern, ~ Truth + Prediction + pred.ON + anno.ON + OrderID, type = "response",
                     data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
                     at = list(Prediction = shift(cases$prediction, dat$Prediction)))

# 1st vs. 10th trial
emmeans(mod.Concern, pairwise ~ OrderID | Truth + pred.ON, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(OrderID = shift(c(1,10), dat$OrderID)))

1.323 - qnorm(0.975)*0.0812
1.323 + qnorm(0.975)*0.0812

# slope of A for ERT
custom.contrast(emm.cases,
                comparison = list(A = list(Prediction = shift(min(cases$prediction[cases$truth == "ERT"]), dat$Prediction)),
                                  B = list(Prediction = shift(max(cases$prediction[cases$truth == "ERT"]), dat$Prediction))),
                common = list(pred.ON = "No",
                              anno.ON = "No",
                              Truth = "ERT"))

# slope of A for non-ERT
custom.contrast(emm.cases,
                comparison = list(A = list(Prediction = shift(min(cases$prediction[cases$truth == "nonERT"]), dat$Prediction)),
                                  B = list(Prediction = shift(max(cases$prediction[cases$truth == "nonERT"]), dat$Prediction))),
                common = list(pred.ON = "No",
                              anno.ON = "No",
                              Truth = "nonERT"))

# ERT vs. non-ERT for A
emmeans(mod.Concern, pairwise ~ Truth | pred.ON + anno.ON, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(Prediction = shift(cases$prediction, dat$Prediction), pred.ON = "No", anno.ON = "No"))

0.684 - 0.489
2.27 - qnorm(0.975)*0.251
2.27 + qnorm(0.975)*0.251

# A vs. B, true positive
emmeans(mod.Concern, revpairwise ~ pred.ON + anno.ON | Truth + Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(anno.ON = "No",
                  Truth = cases$truth[cases$case == 8],
                  Prediction = shift(c(cases$prediction[cases$case == 8]), dat$Prediction)))

emmeans(mod.Concern, ~ pred.ON + anno.ON + Truth | Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  anno.ON = "No",
                  Prediction = shift(c(cases$prediction[cases$case == 8]), dat$Prediction)))

(0.807-0.692)/(0.692-0.474)
1.86 - qnorm(0.975)*0.189
1.86 + qnorm(0.975)*0.189

# A vs. B, true negative
emmeans(mod.Concern, pairwise ~ pred.ON + anno.ON | Truth + Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(anno.ON = "No",
                  Truth = cases$truth[cases$case == 6],
                  Prediction = shift(c(cases$prediction[cases$case == 6]), dat$Prediction)))

emmeans(mod.Concern, ~ pred.ON + anno.ON + Truth | Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  anno.ON = "No",
                  Prediction = shift(c(cases$prediction[cases$case == 6]), dat$Prediction)))

(0.384-0.502)/(0.677-0.502)
1.62 - qnorm(0.975)*0.14
1.62 + qnorm(0.975)*0.14

# A vs. B, false negative
emmeans(mod.Concern, revpairwise ~ pred.ON + anno.ON | Truth + Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(anno.ON = "No",
                  Truth = cases$truth[cases$case == 2],
                  Prediction = shift(c(cases$prediction[cases$case == 2]), dat$Prediction)))

emmeans(mod.Concern, ~ pred.ON + anno.ON + Truth | Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  anno.ON = "No",
                  Prediction = shift(c(cases$prediction[cases$case == 2]), dat$Prediction)))

(0.465-0.677)/(0.677-0.501)
0.414 - qnorm(0.975)*0.0442
0.414 + qnorm(0.975)*0.0442

# A vs. B, false positive
emmeans(mod.Concern, pairwise ~ pred.ON + anno.ON | Truth + Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(anno.ON = "No",
                  Truth = cases$truth[cases$case == 4],
                  Prediction = shift(c(cases$prediction[cases$case == 4]), dat$Prediction)))

emmeans(mod.Concern, ~ pred.ON + anno.ON + Truth | Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  anno.ON = "No",
                  Prediction = shift(c(cases$prediction[cases$case == 4]), dat$Prediction)))

(0.684-0.475)/(0.692-0.475)
0.418 - qnorm(0.975)*0.0434
0.418 + qnorm(0.975)*0.0434

# false positive vs. true positive
(0.684-0.475)/(0.807-0.692)

# false negative vs. true negative
(0.465-0.677)/(0.384-0.502)

# A ERT vs. B false positive
custom.contrast(object = emm.cases,
                common = list(anno.ON = "No"),
                comparison = list(A = list(Truth = "ERT",
                                           pred.ON = "No",
                                           Prediction = shift(cases$prediction[cases$case == 4], dat$Prediction)),
                                  B = list(Truth = "nonERT",
                                           pred.ON = "Yes",
                                           Prediction = shift(cases$prediction[cases$case == 4], dat$Prediction))))

# A non-ERT vs. B false negative
custom.contrast(object = emm.cases,
                common = list(anno.ON = "No"),
                comparison = list(A = list(Truth = "nonERT",
                                           pred.ON = "No",
                                           Prediction = shift(cases$prediction[cases$case == 2], dat$Prediction)),
                                  B = list(Truth = "ERT",
                                           pred.ON = "Yes",
                                           Prediction = shift(cases$prediction[cases$case == 2], dat$Prediction))))

# B vs. C for ERT cases
emmeans(mod.Concern, pairwise ~ pred.ON + anno.ON | Truth + Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "Yes",
                  Truth = "ERT",
                  Prediction = shift(c(cases$prediction[cases$truth == "ERT"]), dat$Prediction)))

# B vs. C for non-ERT cases
emmeans(mod.Concern, pairwise ~ pred.ON + anno.ON | Truth + Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "Yes",
                  Truth = "nonERT",
                  Prediction = shift(c(cases$prediction[cases$truth == "nonERT"]), dat$Prediction)))

# A vs. D, true positive
emmeans(mod.Concern, revpairwise ~ pred.ON + anno.ON | Truth + Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  Truth = cases$truth[cases$case == 8],
                  Prediction = shift(c(cases$prediction[cases$case == 8]), dat$Prediction)))

emmeans(mod.Concern, ~ pred.ON + anno.ON + Truth | Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  anno.ON = "No",
                  Prediction = shift(c(cases$prediction[cases$case == 8]), dat$Prediction)))

(0.744-0.692)/(0.692-0.474)
1.29 - qnorm(0.975)*0.125
1.29 + qnorm(0.975)*0.125

# A vs. D, false positive
emmeans(mod.Concern, pairwise ~ pred.ON + anno.ON | Truth + Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  Truth = cases$truth[cases$case == 4],
                  Prediction = shift(c(cases$prediction[cases$case == 4]), dat$Prediction)))

emmeans(mod.Concern, ~ pred.ON + anno.ON + Truth | Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  anno.ON = "No",
                  Prediction = shift(c(cases$prediction[cases$case == 4]), dat$Prediction)))

(0.548-0.475)/(0.692-0.475)
0.746 - qnorm(0.975)*0.0751
0.746 + qnorm(0.975)*0.0751

# A vs. D, true negative
emmeans(mod.Concern, pairwise ~ pred.ON + anno.ON | Truth + Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  Truth = cases$truth[cases$case == 6],
                  Prediction = shift(c(cases$prediction[cases$case == 6]), dat$Prediction)))

emmeans(mod.Concern, ~ pred.ON + anno.ON + Truth | Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  anno.ON = "No",
                  Prediction = shift(c(cases$prediction[cases$case == 6]), dat$Prediction)))

# A vs. D, false negative
emmeans(mod.Concern, pairwise ~ pred.ON + anno.ON | Truth + Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  Truth = cases$truth[cases$case == 2],
                  Prediction = shift(c(cases$prediction[cases$case == 2]), dat$Prediction)))

emmeans(mod.Concern, ~ pred.ON + anno.ON + Truth | Prediction, type = "response",
        data = transform(dat, list(Concern = normalize, Prediction = shift, OrderID = shift)),
        at = list(pred.ON = "No",
                  anno.ON = "No",
                  Prediction = shift(c(cases$prediction[cases$case == 2]), dat$Prediction)))
