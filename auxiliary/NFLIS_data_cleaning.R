################################################################################
###                      Read in the datasets                                ###
################################################################################

# Read & combine raw data files
if (update_raw_data == TRUE) {
  # Initialise data container
  # 2013-2021, 2 files per year; 2022, 4 files; 2023 full year, 4 files
  dat_list <- vector("list", ((length(dat_years)-2) * 2 + 4 +4))
  
  # Read in data files from data folder
  # Read 2013-2021
  for (i in 1:(length(dat_years)-2)) {
    year <- dat_years[[i]]
    dat_list[[i*2-1]] <- read_csv(str_glue("{dat_folder}/{year}_1.csv"), col_select=c("NFLISID","AnalysisID","SubstanceDetected","State","YYYY","MM","Form","SEQNUM"), show_col_types = FALSE) 
    dat_list[[i*2]] <- read_csv(str_glue("{dat_folder}/{year}_2.csv"), col_select=c("NFLISID","AnalysisID","SubstanceDetected","State","YYYY","MM","Form","SEQNUM"), show_col_types = FALSE) 
  }
  
  # Read 2022, 4 files
  year <- 2022
  dat_list[[(length(dat_years)-1)*2-1]] <- read_csv(str_glue("{dat_folder}/{year}_1.csv"), col_select=c("NFLISID","AnalysisID","SubstanceDetected","State","YYYY","MM","Form","SEQNUM"), show_col_types = FALSE)
  dat_list[[(length(dat_years)-1)*2]] <- read_csv(str_glue("{dat_folder}/{year}_2.csv"), col_select=c("NFLISID","AnalysisID","SubstanceDetected","State","YYYY","MM","Form","SEQNUM"), show_col_types = FALSE)
  dat_list[[(length(dat_years)-1)*2+1]] <- read_csv(str_glue("{dat_folder}/{year}_3.csv"), col_select=c("NFLISID","AnalysisID","SubstanceDetected","State","YYYY","MM","Form","SEQNUM"), show_col_types = FALSE)
  dat_list[[(length(dat_years)-1)*2+2]] <- read_csv(str_glue("{dat_folder}/{year}_4.csv"), col_select=c("NFLISID","AnalysisID","SubstanceDetected","State","YYYY","MM","Form","SEQNUM"), show_col_types = FALSE)
  
  # Read 2023, 4 files
  year <- 2023
  dat_list[[length(dat_years)*2+1]] <- read_csv(str_glue("{dat_folder}/{year}_1.csv"), col_select=c("NFLISID","AnalysisID","SubstanceDetected","State","YYYY","MM","Form","SEQNUM"), show_col_types = FALSE)
  dat_list[[length(dat_years)*2+2]] <- read_csv(str_glue("{dat_folder}/{year}_2.csv"), col_select=c("NFLISID","AnalysisID","SubstanceDetected","State","YYYY","MM","Form","SEQNUM"), show_col_types = FALSE)
  dat_list[[length(dat_years)*2+3]] <- read_csv(str_glue("{dat_folder}/{year}_3.csv"), col_select=c("NFLISID","AnalysisID","SubstanceDetected","State","YYYY","MM","Form","SEQNUM"), show_col_types = FALSE)
  dat_list[[length(dat_years)*2+4]] <- read_csv(str_glue("{dat_folder}/{year}_4.csv"), col_select=c("NFLISID","AnalysisID","SubstanceDetected","State","YYYY","MM","Form","SEQNUM"), show_col_types = FALSE)
  
  
  # Combine raw data files, clean up, and save combined file
  datall <- bind_rows(dat_list) %>% filter(!is.na(NFLISID))
  remove(dat_list)
  datall %>% write_csv(str_glue("{dat_folder}/combined_data.2013-2023.fullyears.csv"))
}

################################################################################
###               Categorisation & sample restrictions                       ###
################################################################################

if (update_clean_data == TRUE) {
  # If raw data cleaning not repeated, read in combined file
  if (update_raw_data != TRUE) {
    datall <- read_csv(str_glue("{dat_folder}/combined_data.2013-2023.fullyears.csv"))
  }
  cat("\nTotal rows:", nrow(datall), "\n")
  # 15,319,396 from 2013-2021
  # 17,079,514 from 2013-2023 June
  # 17,786,207 from 2013-2023 full year
 
  # check whether there are duplicated records
  cat("No duplicated records:", nrow(distinct(datall)) == nrow(datall), "\n")
  # TRUE -> no duplicated records
  
  # double check records for each year
  cat("Number of drug reports per year:", table(datall$YYYY),"\n")
  
  
  # check states
  cat("Records by state:")
  print(table(datall$State, useNA="always"))
  # 50 States and DC + 
  # GU: Guam
  # PR: Puerto Rico
  # VI: Virgin Islands
  # AS: American Samoa 
  # MP: Northern Mariana Islands
  
  # read in the drug category mapping file
  dat.cat <- read_excel(str_glue("./data-public/NFLIS_substance_categories_annual_top_60 from reports 2013 to 2022.xlsx"), sheet="Drug categories") %>% 
    setNames(c("Category.aggregate","Substance")) 
  
  # merge back the categories
  # change all substance names to lower case
  datall <- datall %>%  mutate(Substance=tolower(SubstanceDetected))
  dat <- datall %>% left_join(dat.cat, by="Substance")
  remove(datall)

  # flag all records with fentanyl and fentanyl-related records
  dat <- dat %>% 
    mutate(fent = ifelse(Category.aggregate %in% c("Fentanyl and fentanyl-related"), 1, 0), 
           MM = str_pad(MM, 2, pad = "0"),
           Month = paste(YYYY,MM,sep="-")) %>%  # generate year-month variable
    arrange(YYYY, MM, NFLISID, AnalysisID)  # sort the dataset
  
  cat("Fentanyl records:")
  print(table(dat$fent, useNA="always"))
  
  # double check drug categories by year
  cat("Drug categories by year:")
  print(table(dat$Category.aggregate, dat$YYYY, useNA="always"))

  # Exclude:
  # GU: Guam
  # PR: Puerto Rico
  # VI: Virgin Islands
  # AS: American Samoa 
  # MP: Northern Mariana Islands
  
  dat_rev <- dat %>% 
    filter(!(State %in% c("GU","PR","VI","AS","MP"))) %>% 
    group_by(AnalysisID) %>% 
    mutate(fent_prop = mean(fent)) %>% 
    ungroup()
  # 15,294,836 obs from 2013-2021
  # 17,053,758 obs from 2013-2023 June
  # 17,759,878 obs from 2013-2023 full years
  
  
  cat("Records by state:")
  print(table(dat_rev$State))
  cat("Number of states included:", length(unique(dat_rev$State)), "\n")
  remove(dat)
  
  # check frequency of drug records
  freqtable <- dat_rev %>% 
    group_by(Category.aggregate) %>% 
    summarise(Freq = n()) %>% 
    arrange(-Freq) %>% 
    mutate(Percent_p100 = Freq/sum(Freq)*100, 
           Cumsum_p100 = cumsum(Percent_p100))
  freqtable
  
  # check frequency of drug records by year
  freqtable.year <- dat_rev %>% 
    group_by(YYYY,SubstanceDetected) %>% 
    summarise(Freq = n()) %>% 
    arrange(YYYY,-Freq) %>% 
    mutate(Percent_p100 = Freq/sum(Freq)*100, 
           Cumsum_p100 = cumsum(Percent_p100))
  freqtable.year %>% write_csv("./outputs/processed/Drug_frequency_annual.csv")
  
  freqtable.year.90 <- freqtable.year[freqtable.year$Cumsum_p100<=91,]
  freqtable.year.90 %>% write_csv("./outputs/processed/Drug_frequency_annual_top90.csv")
  
  # Save cleaned file with categorisation & sample restrictions
  dat_rev %>% write_csv(str_glue("{dat_folder}/combined_data_categorised.2013-2023.fullyears.csv"))
} else {
  cat("Reading saved data, skipping checks.\n")
  if (exists("datall")) remove(datall)
}