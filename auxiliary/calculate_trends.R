################################################################################
###              Generate data for figures and tables                        ###
################################################################################

if (update_analysis == TRUE) {  # Analysis can only be updated with access to data
  if (update_clean_data != TRUE) {
    dat_rev  <- read_csv(str_glue("{dat_folder}/combined_data_categorised.2013-2023.fullyears.csv"))
  }
} else {  # If not updating analysis, read in output files instead
  alldrugs_year <- read_csv("./outputs/processed/Fentanyl_cooccurrence_year_state.csv")
  fent_samples <- read_csv("./outputs/processed/Fentanyl_samples_year_state.csv")
  alldrugs_month <- read_csv("./outputs/processed/Fentanyl_cooccurrence_month_state.csv")
}

##########################################
###         Yearly trend               ###
##########################################

if (update_analysis == TRUE) {
  # Initialise container for output
  alldrugs_year <- vector("list", length(druglist))
  
  # calculate the yearly proportion of fentanyl at national level and state level 
  for (i in 1:length(druglist)) {
    cat("Calculating co-occurrences for", druglist[i], "\n")
    subsampleID <- dat_rev %>% filter(Category.aggregate == druglist[i]) %>% select(AnalysisID) %>% distinct()
    # get all the relevant samples
    subsample <- dat_rev %>% filter(AnalysisID %in% subsampleID$AnalysisID) %>% arrange(YYYY, State, AnalysisID, fent)
    
    # keep the last record for each AnalysisID, which should have indicator if fent=1
    subsample_single <- subsample %>% group_by(AnalysisID) %>% arrange(AnalysisID) %>% filter(row_number()==n())
    
    # double check
    # nrow(subsample_single)
    # nrow(subsampleID) # these two numbers should match
    # length(unique(subsample[fent==1,]$AnalysisID))
    # table(subsample_single$fent) # these two numbers should match
    
    # proportion of fent sample by year 
    fent.time <- subsample_single %>% 
      group_by(YYYY) %>% 
      summarise(Count_fent = sum(fent),
                Total = n()) %>% 
      mutate(P100_fent = Count_fent/Total*100,
             State = "0.US")
    
    # double check
    # sum(fent.time$Count_fent)
    # table(subsample_single$fent) # should match
    # sum(fent.time$Total)
    # nrow(subsample_single)# should match
    
    # proportion of fent sample by year and state
    fent.time.state <- subsample_single %>% 
      group_by(YYYY, State) %>% 
      summarise(Count_fent = sum(fent),
                Total = n()) %>% 
      mutate(P100_fent = Count_fent/Total*100)
    
    # double check
    # sum(fent.time.state$Count_fent)
    # table(subsample_single$fent) # should match
    # sum(fent.time.state$Total)
    # nrow(subsample_single)# should match
    
    # combine national result and state level result
    fent.all <- bind_rows(fent.time, fent.time.state) %>% 
      mutate(Drug.sample=druglist[i]) %>% 
      rename(Time = YYYY) %>% 
      select(c("State","Time","Drug.sample","Total","Count_fent","P100_fent"))
    
    # combine all drugs
    alldrugs_year[[i]] <- fent.all
  }
  alldrugs_year <- bind_rows(alldrugs_year)  # Combine output into single tibble
  
  # double check
  aggregate(Total~Time+Drug.sample,data=alldrugs_year[alldrugs_year$State!="0.US",],sum)
  aggregate(Count_fent~Time+Drug.sample,data=alldrugs_year[alldrugs_year$State!="0.US",],sum)
  check=alldrugs_year[alldrugs_year$State=="0.US",]
  
  # save the results to csv file & clean up
  alldrugs_year %>% write_csv("./outputs/processed/Fentanyl_cooccurrence_year_state.csv")
  remove(subsample, subsampleID, subsample_single, fent.time, fent.time.state, fent.all)
}

##########################################
###      All substances by state       ###
##########################################

if (update_analysis == TRUE) {
  cat("Calculating fentanyl sample sizes\n")

  # Get IDs of samples containing substances on the list of substances examined
  subsampleID <- dat_rev %>% filter(Category.aggregate %in% druglist) %>% select(AnalysisID) %>% distinct()
  
  # Subsample data to records for samples containing or NOT containing any substances examined
  subsample <- dat_rev %>% filter(AnalysisID %in% subsampleID$AnalysisID) %>% arrange(YYYY, State, AnalysisID, fent)
  subsample_other <- dat_rev %>% filter(!(AnalysisID %in% subsampleID$AnalysisID)) %>% arrange(YYYY, State, AnalysisID, fent)

  # keep the last record for each AnalysisID, which should have indicator if fent=1
  fent_listed_single <- subsample %>% group_by(AnalysisID) %>% arrange(AnalysisID) %>% filter(row_number()==n())
  fent_other_single <- subsample_other %>% group_by(AnalysisID) %>% arrange(AnalysisID) %>% filter(row_number()==n())
  
  # Proportion of fent samples across all substances by year 
  fent.time.all <- fent_listed_single %>% 
    group_by(YYYY) %>% 
    summarise(Count_fent_listed = sum(fent),
              Total_listed = n()) %>% 
    mutate(State = "0.US") %>% 
    left_join(fent_other_single %>%  # Add number of fent-only samples
                group_by(YYYY) %>% 
                summarise(Count_fent_only = sum(fent_prop == 1),
                          Count_fent_other = sum(fent_prop > 0 & fent_prop < 1),
                          Total_other = n()))

  # Proportion of fent samples across all substances by year and state
    fent.time.state.all <- fent_listed_single %>% 
      group_by(YYYY, State) %>% 
      summarise(Count_fent_listed = sum(fent),
                Total_listed = n()) %>% 
      left_join(fent_other_single %>%  # Add number of fent-only samples
                  group_by(YYYY, State) %>% 
                  summarise(Count_fent_only = sum(fent_prop == 1),
                            Count_fent_other = sum(fent_prop > 0 & fent_prop < 1),
                            Total_other = n()))
  
  fent_samples <- bind_rows(fent.time.all, fent.time.state.all) %>% 
    rename(Time = YYYY) %>% 
    mutate(Listed_only = Total_listed - Count_fent_listed, 
           Other_only = Total_other - Count_fent_other - Count_fent_only, 
           P100_fent = Count_fent_listed / Total_listed * 100,
           P100_fent_only = Count_fent_only / (Total_listed + Total_other) * 100, 
           P100_fent_all = (Count_fent_listed + Count_fent_other + Count_fent_only) / (Total_listed + Total_other) * 100) %>% 
    select(c("State", "Time", "Total_listed", "Listed_only", "Count_fent_listed", "P100_fent", 
             "Total_other", "Other_only", "Count_fent_only","Count_fent_other", "P100_fent_only", "P100_fent_all"))
  
  # save the results to csv file & clean up
  fent_samples %>% write_csv("./outputs/processed/Fentanyl_samples_year_state.csv")
  remove(subsample, subsample_other, subsampleID, fent_listed_single, fent_other_single, fent.time.all, fent.time.state.all)
}

##########################################
###         Monthly trend              ###
##########################################

if (update_analysis == TRUE) {
  # Initialise container for output
  alldrugs_month <- vector("list", length(druglist))
  
  # calculate the monthly proportion of fentanyl at national level
  for (i in 1:length(druglist)) {
    cat("Calculating co-occurrences for", druglist[i], "\n")
    subsampleID <- dat_rev %>% filter(Category.aggregate == druglist[i]) %>% select(AnalysisID) %>% distinct()
    # get all the relevant samples
    subsample <- dat_rev %>% filter(AnalysisID %in% subsampleID$AnalysisID) %>% arrange(YYYY, MM, State, AnalysisID, fent)
    
    # keep the last record for each AnalysisID, which should have indicator if fent=1
    subsample_single <- subsample %>% group_by(AnalysisID) %>% arrange(AnalysisID) %>% filter(row_number()==n())
    
    # double check
    # nrow(subsample_single)
    # nrow(subsampleID) # these two numbers should match
    # length(unique(subsample[fent==1,]$AnalysisID))
    # table(subsample_single$fent) # these two numbers should match
    
    # proportion of fent sample by month 
    fent.time <- subsample_single %>% 
      group_by(Month) %>% 
      summarise(Count_fent = sum(fent),
                Total = n()) %>% 
      mutate(P100_fent = Count_fent/Total*100,
             State = "0.US", 
             Drug.sample=druglist[i]) %>% 
      rename(Time = Month) %>% 
      select(c("State","Time","Drug.sample","Total","Count_fent","P100_fent"))
    
    # double check
    # sum(fent.time$Count_fent)
    # table(subsample_single$fent) # should match
    # sum(fent.time$Total)
    # nrow(subsample_single)# should match
    
    # combine all drugs
    alldrugs_month[[i]] <- fent.time
  }
  alldrugs_month <- bind_rows(alldrugs_month)  # Combine output into single tibble
  
  # double check
  check2=alldrugs_month
  check2$year = substr(check2$Time,1,4)
  aggregate(Total~year+Drug.sample,data=check2,sum)
  aggregate(Count_fent~year+Drug.sample,data=check2,sum)
  check=alldrugs_year[alldrugs_year$State=="0.US",]
  
  # save the results to csv file & clean up
  alldrugs_month %>% write_csv("./outputs/processed/Fentanyl_cooccurrence_month_state.csv")
  remove(subsample, subsampleID, subsample_single, fent.time)
}

##########################################
###         Rolling averages           ###
##########################################

# exclude drugs with very low fentanyl prevalence
alldrugs_month %>% group_by(Drug.sample) %>% summarise(sum(P100_fent<1))

excludelist = c()  # UPDATE THIS WITH DRUGS TO EXCLUDE LATER
alldrugs_month <- alldrugs_month %>% 
  filter(!(Drug.sample %in% excludelist)) %>% 
  arrange(Drug.sample, Time)

# calculate 12-month rolling average for fentanyl count 
alldrugs_month <- alldrugs_month %>% 
  arrange(desc(Drug.sample)) %>% 
  group_by(Drug.sample) %>% 
  mutate(fent_12mon = zoo::rollmean(Count_fent, k = 12, fill = NA, align = "right"),
         total_12mon = zoo::rollmean(Total, k = 12, fill = NA, align = "right"),
         fent_12mon.V2 = zoo::rollmean(Count_fent, k = 12, fill = NA),
         total_12mon.V2 = zoo::rollmean(Total, k = 12, fill = NA)) %>% 
  ungroup() %>% 
  mutate(P100_fent_roll12mon = fent_12mon/total_12mon*100, 
         P100_fent_roll12mon.V2 = fent_12mon.V2/total_12mon.V2 *100, 
         Time = ym(Time)) %>% 
  arrange(Drug.sample, Time)

# double check that December value represents the yearly estimate using the first approach
cat("\nRolling average vs. annual total check:\n")
print(near(alldrugs_month %>% filter(str_detect(Time, "-12")) %>% pull(P100_fent_roll12mon), 
           alldrugs_year %>% 
             filter(State=="0.US" & !(Drug.sample %in% excludelist) & Total >= 5) %>%  
             arrange(Drug.sample) %>% 
             pull(P100_fent)))


##########################################
###         Mann-Kendall tests         ###
##########################################

# Calculate Mann-Kendall trend test statistics by year and month
trend_year <- alldrugs_year %>% 
  group_by(State, Drug.sample) %>% 
  drop_na(P100_fent) %>%  # mk.test fails with NA values
  summarise(mk = list(tryCatch(trend::mk.test(P100_fent), error = function(e) NA))) %>% 
  mutate(mk = map(mk, unlist)) %>%
  hoist(mk, mk_tau = "estimates.tau", mk_p = "p.value") %>% 
  select(-mk) %>% 
  mutate(across(mk_tau:mk_p, as.numeric), 
         across(mk_tau:mk_p, ~ signif(.x, 5)))


trend_month <- alldrugs_month %>% 
  group_by(State, Drug.sample) %>% 
  drop_na(P100_fent) %>%  # mk.test fails with NA values
  summarise(mk = list(tryCatch(trend::mk.test(P100_fent), error = function(e) NA))) %>% 
  mutate(mk = map(mk, unlist)) %>% 
  hoist(mk, mk_tau = "estimates.tau", mk_p = "p.value") %>% 
  select(-mk) %>% 
  mutate(across(mk_tau:mk_p, as.numeric), 
         across(mk_tau:mk_p, ~ signif(.x, 5)))

# Merge back into main results tables
alldrugs_year <- alldrugs_year %>% 
  left_join(trend_year, by=c("State", "Drug.sample"))

alldrugs_month <- alldrugs_month %>% 
  left_join(trend_month, by=c("State", "Drug.sample"))

# Save monthly trend MK result to supplementary table
trend_month %>% 
  ungroup() %>% 
  select(-State) %>% 
  rename(`Substance category` = Drug.sample, 
         Tau = mk_tau, 
         `p value` = mk_p) %>% 
  write_csv("./outputs/Monthly MK trend test.csv")

# Create appendix table
# p-values
trend.result0 <- trend_year %>%
  filter(State!="0.US") %>%
  mutate(p.value=ifelse(is.na(mk_p), 9999,
                        ifelse(mk_p>0.001, round(mk_p,3),0))) %>%
  select(State, Drug.sample, p.value)

trend.result0$P = ifelse(trend.result0$p.value==9999, "NaN",
                 ifelse(trend.result0$p.value==0, "<0.001",trend.result0$p.value))
trend.result0 = trend.result0[,-3]

trend.result<-spread(trend.result0, key = Drug.sample, value = P)

# save the data
trend.result %>% write_csv("./outputs/MK trend test p values.csv")


# re-formatting the output p-value table based on journal requirements
format.trend.result0 <- trend_year %>%
  filter(State!="0.US") %>%
  mutate(mk_p_formatted = ifelse(is.na(mk_p), "NaN", 
                                 ifelse(mk_p >= 0.05, format(round(mk_p, 2), nsmall = 2, scientific = FALSE),
                                        ifelse(mk_p >= 0.001 & mk_p < 0.05, format(round(mk_p, 3), nsmall = 3, scientific = FALSE),
                                               ifelse(mk_p >= 0.0001 & mk_p < 0.001, format(round(mk_p, 4), nsmall = 4, scientific = FALSE),
                                                      "<0.0001"))))) %>%
  select(State, Drug.sample, mk_p_formatted)

format.trend.result<-spread(format.trend.result0, key = Drug.sample, value = mk_p_formatted)

# save the data
format.trend.result %>% write_csv("./outputs/MK trend test p values_reformat.csv")



# mk_tau
tau.result0 <- trend_year %>%
  filter(State!="0.US") %>%
  mutate(mk.tau=round(mk_tau,3)) %>%
  select(State, Drug.sample, mk.tau)

tau.result<-spread(tau.result0, key = Drug.sample, value = mk.tau)

# save the data
tau.result %>% write_csv("./outputs/MK trend test tau values.csv")
