# Subset Labs, Meds, and Vitals to Selected Cases
# Run this script if case set changes

# Import Case IDs
cases <- read.csv("cases.csv")
for (i in 1:nrow(cases)) {
  cases[i,"shortID"] <- strsplit(cases$id[i],split = " ")[[1]][1]
}

# Import Labs, Meds, and Vitals
labs <- read.csv("all_labs.csv")
meds <- read.csv("all_meds.csv")
vitals <- read.csv("all_vitals.csv")
patients <- read.csv("all_patients.csv")

# Check Data
length(unique(labs$Identity))
length(unique(meds$Identity))
length(unique(vitals$Identity))
length(unique(patients$Identity))

# Subset Data
labs_subset <- labs[labs$Identity %in% cases$shortID,]
meds_subset <- meds[meds$Identity %in% cases$shortID,]
vitals_subset <- vitals[vitals$Identity %in% cases$id,]
patients_subset <- patients[patients$Identity %in% cases$shortID,]

# Check Data Subset
length(unique(labs_subset$Identity))
length(unique(meds_subset$Identity))
length(unique(vitals_subset$Identity))
length(unique(patients_subset$Identity))

# Write Data Files
write.csv(labs_subset[,2:ncol(labs_subset)], "labs.csv")
write.csv(meds_subset[,2:ncol(meds_subset)], "meds.csv")
write.csv(vitals_subset[,2:ncol(vitals_subset)], "vitals.csv")
write.csv(patients_subset[,2:ncol(patients_subset)], "patients.csv")