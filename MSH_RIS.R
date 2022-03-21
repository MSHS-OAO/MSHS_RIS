library(tidyverse)
library(xlsx)
library(readxl)
library(lubridate)

#MSH RIS directory
RIS_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                  "Productivity/Volume - Data/MSH Data/RIS/")

#month and year of charge detail
month_year <- "FEB2022"

#read in charge detail
RIS <- read.csv(paste0(RIS_dir, "Charge Detail/MSH_RIS_", month_year, ".csv"), 
                colClasses = c(rep("character", 21)))
#read in OR Diagnostic
RIS_OR <- read.csv(paste0(RIS_dir,"Charge Detail/OR Diagnostic/",
                                    "MSH_RIS_OR Diagnostic_",month_year,".csv"), 
                             colClasses = c(rep("character",9))) %>%
  select(MRN, Name, ACC, Date, Exam, Exam.Modifier, Org, Resource)
#read in neuro detail
RIS_neuro <- read.csv(paste0(RIS_dir,"Charge Detail/Neurosurgery/",
                             "MSH_Neuro_",month_year,".csv"), 
                colClasses = c(rep("character",21)))

#Read in modality mapping file
modality <- read_xlsx(paste0(RIS_dir,"Mapping/Modality_Mapping.xlsx"))
#Read in PAT mapping
PAT <- read_xlsx(paste0(RIS_dir,"Mapping/PAT_Mapping.xlsx"))
#Premier Dep ID for CPT upload
Premier_Dep <- read_xlsx(paste0(RIS_dir,"Mapping/Premier_ID.xlsx"))

#remove whitespaces
RIS[,1:21] <- sapply(RIS[,1:21], trimws)
RIS_neuro[,1:21] <- sapply(RIS_neuro[,1:21], trimws)

#modifiers we want to keep
acceptable_modifiers <- c("26","50","53","tc")

#pivot charge columns longer into a single column
RIS_charge <- RIS %>% 
  pivot_longer(cols = c(10:17), names_to = "Charge", values_to = "charge_code") %>%
  filter(charge_code != "") %>%
  select(-Charge)%>%
  mutate(
    Charge.Mod.1 = case_when(
      Charge.Mod.1 %in% acceptable_modifiers ~ Charge.Mod.1,
      TRUE ~ ""),
    Charge.Mod.2 = case_when(
      Charge.Mod.2 %in% acceptable_modifiers ~ Charge.Mod.2,
      TRUE ~ ""),
    Charge.Mod.3 = case_when(
      Charge.Mod.3 %in% acceptable_modifiers ~ Charge.Mod.3,
      TRUE ~ ""),
    Charge.Mod.4 = case_when(
      Charge.Mod.4 %in% acceptable_modifiers ~ Charge.Mod.4,
      TRUE ~ ""),
    Modifier = paste0(Charge.Mod.1,Charge.Mod.2,Charge.Mod.3,Charge.Mod.4)) %>%
  unite(CPT4, 
        charge_code, Charge.Mod.1, Charge.Mod.2, Charge.Mod.3, Charge.Mod.4, 
        sep = "") %>%
left_join(modality) %>%
  left_join(PAT, by = c("Pat.Type" = "Code")) %>%
  mutate(Identifier = paste0(Org,"-",Modality,"-",`IP or OP`)) %>%
  left_join(Premier_Dep) %>%
  mutate(Partner = "729805",
         Hosp = "NY0014",
         Start = paste0(substr(Date,6,7), "/",
                        substr(Date,9,10), "/",
                        substr(Date,1,4)),
         End = paste0(substr(Date,6,7), "/",
                      substr(Date,9,10), "/",
                      substr(Date,1,4))) %>%
  select(Partner, Hosp, DepID, Start, End, CPT4)

#prepare OR diagnostic volume
RIS_OR_upload <- RIS_OR %>%
  filter(Org == "RM") %>%
  mutate(Partner = "729805",
         Hosp = "NY0014",
         DepID = "MSHRIS21008OR",
         Start = Date,
         End = Date,
         CPT4 = "71045") %>%
  select(Partner, Hosp, DepID, Start, End, CPT4) 

#prepare neuro volume
neuro <- RIS_neuro %>%
  #pivot longer to combine charge columns into one
  pivot_longer(cols = c(10:17), 
               names_to = "charge", 
               values_to = "code") %>%
  #remove blank charges from upload prep
  filter(code != "") %>%
  #drop charge column
  select(-charge) %>%
  #reome unwated modifier
  mutate(
    Charge.Mod.1 = case_when(
      Charge.Mod.1 %in% acceptable_modifiers ~ Charge.Mod.1,
      TRUE ~ ""),
    Charge.Mod.2 = case_when(
      Charge.Mod.2 %in% acceptable_modifiers ~ Charge.Mod.2,
      TRUE ~ ""),
    Charge.Mod.3 = case_when(
      Charge.Mod.3 %in% acceptable_modifiers ~ Charge.Mod.3,
      TRUE ~ ""),
    Charge.Mod.4 = case_when(
      Charge.Mod.4 %in% acceptable_modifiers ~ Charge.Mod.4,
      TRUE ~ "")) %>%
  #unite charge and 
  unite(CPT4, 
        code, Charge.Mod.1, Charge.Mod.2, Charge.Mod.3, Charge.Mod.4, 
        sep = "") %>%
  #Add in necessary columns for upload
  mutate(Partner = "729805",
         Hosp = "NY0014",
         DepID = "MSHRIS21050",
         Start = paste0(substr(Date,6,7), "/",
                       substr(Date,9,10), "/",
                       substr(Date,1,4))) %>%
  #Create second date column
  mutate(End = Start) %>%
  #select column order for upload
  select(Partner, Hosp, DepID, Start, End, CPT4)

#bind both files for upload
upload <- rbind(RIS_charge, neuro 
                #, RIS_OR_upload) 
                )%>%
  group_by(Partner, Hosp, DepID, Start, End, CPT4) %>%
  summarise(volume = n()) %>%
  mutate(budget = "0")
  

####################################################  
old_master <- readRDS(paste0(RIS_dir,"Master/Master.rds"))
new_master <- rbind(old_master,upload)
saveRDS(new_master,paste0(RIS_dir,"Master/Master.rds"))
####################################################

#Trend Check
pp_mapping <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                               "Productivity/Universal Data/Mapping/",
                               "MSHS_Pay_Cycle.xlsx"))

pp_mapping$DATE <- format(as.Date(pp_mapping$DATE), "%m/%d/%Y")
pp_mapping$END.DATE <- format(as.Date(pp_mapping$END.DATE), "%m/%d/%Y")

pp_mapping[, 1] <- sapply(pp_mapping[, 1], as.character)

new_master$End <- mdy(new_master$End)
new_master$End <- format((new_master$End), "%m/%d/%Y")

trend <- new_master %>%
  left_join(pp_mapping, by = c("End" = 'DATE')) %>% 
  ungroup() %>%
  group_by(DepID,END.DATE) %>%
  summarise(Vol = sum(Volume, na.rm = T)) %>%
  pivot_wider(id_cols = c(DepID),names_from = END.DATE, values_from = Vol)

View(trend)

#save upload
write.table(upload,paste0(RIS_dir,"Uploads/MSH_RIS_",month_year,".csv"),
            sep = ",", row.names = F, col.names = F)
