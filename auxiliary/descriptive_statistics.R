###################################################################
###           Descriptive statistics in the paper               ###
###################################################################

# 2023 Aug: FDA reviewed the fent list and suggested to exclude "4-Allyloxy-3,5-dimethoxyphenethylamine"
# check the records with excluded fent drug
check <- dat_rev[dat_rev$SubstanceDetected=="4-Allyloxy-3,5-dimethoxyphenethylamine",]
# 9 drug reports/samples
check.allsamples <- dat_rev[dat_rev$AnalysisID %in% check$AnalysisID,]
# 13 drug reports involved in these samples
# They were actually not included in the final analytical sample, descriptive stats should not be affected



druglist <- c("Methamphetamine", "Cannabinoids", "Cocaine", "Heroin", "Prescription opioids",
              "Prescription benzodiazepines", "Prescription stimulants", "Hallucinogens", "Club drugs")


# how many drug reports do not have specific drugs reported
flag.nodrug <- c("No Analysis", "No Controlled Drug Identified", "No Drug Found", "Result Not Reported","Unknown", "Unspecified Pharmacuetical Preparation","Negative Results - Tested For Specific Drugs")
dat_rev$flag.unknown <- ifelse(dat_rev$SubstanceDetected %in% flag.nodrug, 1, 0)
cat("Number of drug reports without specific drugs:", nrow(dat_rev[dat_rev$flag.unknown==1,]),"\n")
cat("proportion of drug reports without specific drugs:", nrow(dat_rev[dat_rev$flag.unknown==1,])/nrow(dat_rev),"\n")


# Frequency for drug categories
table(dat_rev$Category.aggregate)
cat("Number of fentanyl:",nrow(dat_rev[dat_rev$Category.aggregate=="Fentanyl and fentanyl-related",]),"\n")
cat("Number of fentanyl among known drugs reports:",nrow(dat_rev[dat_rev$Category.aggregate=="Fentanyl and fentanyl-related",])/nrow(dat_rev[dat_rev$flag.unknown==0,]),"\n")

cat("Number of all other categories:",nrow(dat_rev[dat_rev$Category.aggregate %in% druglist,]),"\n")
cat("Number of all other drugs among known drugs reports:",nrow(dat_rev[dat_rev$Category.aggregate %in% druglist,])/nrow(dat_rev[dat_rev$flag.unknown==0,]),"\n")


# Check how frequent fentanly are involved in other drugs in general
ID.fent <- distinct(dat_rev[dat_rev$Category.aggregate=="Fentanyl and fentanyl-related","AnalysisID"])
ID.fent$flag.fent <- 1
ID.other <- distinct(dat_rev[dat_rev$Category.aggregate %in% druglist,"AnalysisID"])
ID.compare <- merge(ID.other, ID.fent, all.x=TRUE)
table(ID.compare$flag.fent, useNA="always")

cat("Proportion of fent in other drugs in general:", nrow(ID.compare[ID.compare$flag.fent==1,])/nrow(ID.compare),"\n")


# For SUPPLEMENT FIGURE S1
# Level 1
# unique number of seizures and samples
cat("Number of drug reports across 50 states and DC:", nrow(dat_rev), "\n")
cat("Number of samples:",length(unique(dat_rev$AnalysisID)),"\n")
cat("Number of seizures:",length(unique(dat_rev$NFLISID)),"\n")

# Level 2
# Number of sample per seizure
dat.seizure <- distinct(dat_rev[,c("NFLISID","AnalysisID")])
Sample.per.seizure <- dat.seizure %>% 
  group_by(NFLISID) %>% 
  summarise(Freq = n()) 
cat("Number of seizures had one sample:",nrow(Sample.per.seizure[Sample.per.seizure$Freq==1,]),"\n")
cat("Proportion of seizures had one sample:",nrow(Sample.per.seizure[Sample.per.seizure$Freq==1,])/nrow(Sample.per.seizure),"\n")
cat("Number of seizures had multiple samples:",nrow(Sample.per.seizure[Sample.per.seizure$Freq>1,]),"\n")
cat("Proportion of seizures had multiple samples:",nrow(Sample.per.seizure[Sample.per.seizure$Freq>1,])/nrow(Sample.per.seizure),"\n")

dat.onesampleperseizure <- dat_rev[dat_rev$NFLISID %in% Sample.per.seizure[Sample.per.seizure$Freq==1,]$NFLISID,]
cat("Proportion of drug reports in one sample per seizure:",nrow(dat.onesampleperseizure)/nrow(dat_rev),"\n")
cat("Proportion of samples in one sample per seizure:",length(unique(dat.onesampleperseizure$AnalysisID))/length(unique(dat_rev$AnalysisID)),"\n")

dat.multiplesamples <- dat_rev[dat_rev$NFLISID %in% Sample.per.seizure[Sample.per.seizure$Freq>1,]$NFLISID,]
cat("Proportion of drug reports in one sample per seizure:",nrow(dat.multiplesamples)/nrow(dat_rev),"\n")
cat("Proportion of samples in one sample per seizure:",length(unique(dat.multiplesamples$AnalysisID))/length(unique(dat_rev$AnalysisID)),"\n")

# Level 3

Report.sample1 <- dat.onesampleperseizure %>% 
  group_by(AnalysisID) %>% 
  summarise(Freq = n()) 
cat("Proportion of samples had one report:",nrow(Report.sample1[Report.sample1$Freq==1,])/length(unique(dat_rev$AnalysisID)),"\n")
cat("Proportion of samples had multiple reports:",nrow(Report.sample1[Report.sample1$Freq>1,])/length(unique(dat_rev$AnalysisID)),"\n")

sample1 <- dat_rev[dat_rev$AnalysisID %in% Report.sample1[Report.sample1$Freq==1,]$AnalysisID,]
cat("Proportion of seizures had one report:",length(unique(sample1$NFLISID))/length(unique(dat_rev$NFLISID)),"\n")
cat("Proportion of drug reports:",nrow(sample1)/nrow(dat_rev),"\n")
cat("Proportion of samples had one report:",length(unique(sample1$AnalysisID))/length(unique(dat_rev$AnalysisID)),"\n")

sample1.2 <- dat_rev[dat_rev$AnalysisID %in% Report.sample1[Report.sample1$Freq>1,]$AnalysisID,]
cat("Proportion of seizures had one report:",length(unique(sample1.2$NFLISID))/length(unique(dat_rev$NFLISID)),"\n")
cat("Proportion of drug reports:",nrow(sample1.2)/nrow(dat_rev),"\n")
cat("Proportion of samples had one report:",length(unique(sample1.2$AnalysisID))/length(unique(dat_rev$AnalysisID)),"\n")

Report.sample2 <- dat.multiplesamples %>% 
  group_by(AnalysisID) %>% 
  summarise(Freq = n()) 
cat("Proportion of samples had one report:",nrow(Report.sample2[Report.sample2$Freq==1,])/length(unique(dat_rev$AnalysisID)),"\n")
cat("Proportion of samples had multiple reports:",nrow(Report.sample2[Report.sample2$Freq>1,])/length(unique(dat_rev$AnalysisID)),"\n")

sample2 <- dat_rev[dat_rev$AnalysisID %in% Report.sample2[Report.sample2$Freq==1,]$AnalysisID,]
cat("Proportion of seizures had one report:",length(unique(sample2$NFLISID))/length(unique(dat_rev$NFLISID)),"\n")
cat("Proportion of drug reports:",nrow(sample2)/nrow(dat_rev),"\n")
cat("Proportion of samples had one report:",length(unique(sample2$AnalysisID))/length(unique(dat_rev$AnalysisID)),"\n")

sample2.2 <- dat_rev[dat_rev$AnalysisID %in% Report.sample2[Report.sample2$Freq>1,]$AnalysisID,]
cat("Proportion of seizures had one report:",length(unique(sample2.2$NFLISID))/length(unique(dat_rev$NFLISID)),"\n")
cat("Proportion of drug reports:",nrow(sample2.2)/nrow(dat_rev),"\n")
cat("Proportion of samples had one report:",length(unique(sample2.2$AnalysisID))/length(unique(dat_rev$AnalysisID)),"\n")




# Get final sample
# Total number of seizures
length(unique(dat_rev$NFLISID))
# 9584522
# Total number of samples
length(unique(dat_rev$AnalysisID))
# 16658920
length(unique(dat_rev[dat_rev$Category.aggregate %in% druglist,]$AnalysisID))
# 11940207

# mark included sample ID
includedsample <- unique(dat_rev[dat_rev$Category.aggregate %in% druglist,]$AnalysisID)
length(includedsample)
# 11940207
length(includedsample)/length(unique(dat_rev$AnalysisID))
# 71.7%
# at sample level
length(includedsample)/length(unique(dat_rev[dat_rev$flag.unknown==0,]$AnalysisID))

dat.included <- dat_rev[dat_rev$AnalysisID %in% includedsample,]
nrow(dat.included)
# 12793592 drug reports
length(unique(dat.included$AnalysisID))
# 11940207 samples
length(unique(dat.included[dat.included$fent==1,]$AnalysisID))
# 214899 fent involved samples

# Number of substances per sample
Drug.per.sample <- dat.included %>% 
  group_by(AnalysisID) %>% 
  summarise(Freq = n()) 

nrow(Drug.per.sample[Drug.per.sample$Freq==1,])
nrow(Drug.per.sample[Drug.per.sample$Freq>1,])
mean(Drug.per.sample[Drug.per.sample$Freq>1,]$Freq)
sd(Drug.per.sample[Drug.per.sample$Freq>1,]$Freq)







# For SUPPLEMENT FIGURE S1
# Final included sample
# Level 1
# unique number of seizures and samples
cat("Number of drug reports :", nrow(dat.included), "\n")
cat("Number of samples:",length(unique(dat.included$AnalysisID)),"\n")
cat("Number of seizures:",length(unique(dat.included$NFLISID)),"\n")

# Level 2
# Number of sample per seizure
dat.seizure <- distinct(dat.included[,c("NFLISID","AnalysisID")])
Sample.per.seizure <- dat.seizure %>% 
  group_by(NFLISID) %>% 
  summarise(Freq = n()) 
cat("Number of seizures had one sample:",nrow(Sample.per.seizure[Sample.per.seizure$Freq==1,]),"\n")
cat("Proportion of seizures had one sample:",nrow(Sample.per.seizure[Sample.per.seizure$Freq==1,])/nrow(Sample.per.seizure),"\n")
cat("Number of seizures had multiple samples:",nrow(Sample.per.seizure[Sample.per.seizure$Freq>1,]),"\n")
cat("Proportion of seizures had multiple samples:",nrow(Sample.per.seizure[Sample.per.seizure$Freq>1,])/nrow(Sample.per.seizure),"\n")

dat.onesampleperseizure <- dat.included[dat.included$NFLISID %in% Sample.per.seizure[Sample.per.seizure$Freq==1,]$NFLISID,]
cat("Proportion of drug reports in one sample per seizure:",nrow(dat.onesampleperseizure)/nrow(dat.included),"\n")
cat("Proportion of samples in one sample per seizure:",length(unique(dat.onesampleperseizure$AnalysisID))/length(unique(dat.included$AnalysisID)),"\n")

dat.multiplesamples <- dat.included[dat.included$NFLISID %in% Sample.per.seizure[Sample.per.seizure$Freq>1,]$NFLISID,]
cat("Proportion of drug reports in multiple samples per seizure:",nrow(dat.multiplesamples)/nrow(dat.included),"\n")
cat("Proportion of samples in multiple samples per seizure:",length(unique(dat.multiplesamples$AnalysisID))/length(unique(dat.included$AnalysisID)),"\n")

# Level 3

Report.sample1 <- dat.onesampleperseizure %>% 
  group_by(AnalysisID) %>% 
  summarise(Freq = n()) 
cat("Proportion of samples had one report:",nrow(Report.sample1[Report.sample1$Freq==1,])/length(unique(dat.included$AnalysisID)),"\n")
cat("Proportion of samples had multiple reports:",nrow(Report.sample1[Report.sample1$Freq>1,])/length(unique(dat.included$AnalysisID)),"\n")

sample1 <- dat.included[dat.included$AnalysisID %in% Report.sample1[Report.sample1$Freq==1,]$AnalysisID,]
cat("Proportion of seizures had one report:",length(unique(sample1$NFLISID))/length(unique(dat.included$NFLISID)),"\n")
cat("Proportion of drug reports:",nrow(sample1)/nrow(dat.included),"\n")
cat("Proportion of samples had one report:",length(unique(sample1$AnalysisID))/length(unique(dat.included$AnalysisID)),"\n")

sample1.2 <- dat.included[dat.included$AnalysisID %in% Report.sample1[Report.sample1$Freq>1,]$AnalysisID,]
cat("Proportion of seizures had one report:",length(unique(sample1.2$NFLISID))/length(unique(dat.included$NFLISID)),"\n")
cat("Proportion of drug reports:",nrow(sample1.2)/nrow(dat.included),"\n")
cat("Proportion of samples had one report:",length(unique(sample1.2$AnalysisID))/length(unique(dat.included$AnalysisID)),"\n")

Report.sample2 <- dat.multiplesamples %>% 
  group_by(AnalysisID) %>% 
  summarise(Freq = n()) 
cat("Proportion of samples had one report:",nrow(Report.sample2[Report.sample2$Freq==1,])/length(unique(dat.included$AnalysisID)),"\n")
cat("Proportion of samples had multiple reports:",nrow(Report.sample2[Report.sample2$Freq>1,])/length(unique(dat.included$AnalysisID)),"\n")

sample2 <- dat.included[dat.included$AnalysisID %in% Report.sample2[Report.sample2$Freq==1,]$AnalysisID,]
cat("Proportion of seizures had one report:",length(unique(sample2$NFLISID))/length(unique(dat.included$NFLISID)),"\n")
cat("Proportion of drug reports:",nrow(sample2)/nrow(dat.included),"\n")
cat("Proportion of samples had one report:",length(unique(sample2$AnalysisID))/length(unique(dat.included$AnalysisID)),"\n")

sample2.2 <- dat.included[dat.included$AnalysisID %in% Report.sample2[Report.sample2$Freq>1,]$AnalysisID,]
cat("Proportion of seizures had multiple reports:",length(unique(sample2.2$NFLISID))/length(unique(dat.included$NFLISID)),"\n")
cat("Proportion of drug reports:",nrow(sample2.2)/nrow(dat.included),"\n")
cat("Proportion of samples had multiple reports:",length(unique(sample2.2$AnalysisID))/length(unique(dat.included$AnalysisID)),"\n")










# Total substance samples and fentanyl involved samples
aggregate(dat_rev[dat_rev$Category.aggregate %in% druglist,]$Category.aggregate,list(dat_rev[dat_rev$Category.aggregate %in% druglist,]$Category.aggregate), FUN=length)
aggregate(alldrugs_month$Total, list(alldrugs_month$Drug.sample), FUN=sum) 
aggregate(alldrugs_month$Count_fent, list(alldrugs_month$Drug.sample), FUN=sum) 

# Missing in form variable
nrow(dat_rev[(is.na(dat_rev$Form) | dat_rev$Form %in% c("unspecified","UNKNOWN","Not Provided")),])/nrow(dat_rev)

# Total fent drug reports and samples
check=dat_rev[dat_rev$Category.aggregate=="Fentanyl and fentanyl-related" &
                !is.na(dat_rev$Category.aggregate),]
table(check$Category.aggregate, useNA="always")
length(unique(check$AnalysisID))
