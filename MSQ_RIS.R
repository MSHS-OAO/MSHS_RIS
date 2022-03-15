library(readxl)
library(here)
library(tidyverse)
library(xlsx)

#MSQ RIS directory
RIS_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                  "Productivity/Volume - Data/MSQ Data/RIS/")

#month and year of charge detail
month_year <- "JAN2022"

#read in charge detail
RIS <- read.csv(paste0(RIS_dir,"Charge Detail/",
                       "MSQ_RIS_",month_year,".csv"), 
                colClasses = c(rep("character",21)))   
#read in OR Diagnostic
RIS_OR <- read.csv(paste0(RIS_dir,"Charge Detail/OR Diagnostic/",
                          "MSQ_RIS_OR Diagnostic_",month_year,".csv"), 
                   colClasses = c(rep("character",9))) %>%
  select(MRN, Name, ACC, Date, Exam, Exam.Modifier, Org, Resource)

#read in CDM
CDM <- read_xlsx(file.choose(), col_types = rep("text",55))
#read in PAT Mapping
PAT <- read_xlsx(paste0(RIS_dir,"Mapping/PAT_Mapping.xlsx"))
#read in Dep ID mapping
Dep_ID <- read_xlsx(paste0(RIS_dir,"Mapping/Dep_ID.xlsx"),
                    col_types = c("text","text"))
#Premier Dep ID for CPT upload
Premier_Dep <- read_xlsx(paste0(RIS_dir,"Mapping/Premier_Dep.xlsx"))

#remove whitespaces
RIS[,1:21] <- sapply(RIS[,1:21], trimws)
#pivot charge columns longer into a single column
RIS_charge <- RIS %>% 
  pivot_longer(cols = c(10:17), names_to = "Charge", values_to = "charge_code") %>%
  filter(charge_code != "") %>%
  select(-Charge)
#modifiers we want to keep
acceptable_modifiers <- c("26","50","53","tc")
#remove unwanted modifiers and concatenate remaining
RIS_charge_mod <- RIS_charge %>%
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
    Modifier = paste0(Charge.Mod.1,Charge.Mod.2,Charge.Mod.3,Charge.Mod.4))

#select columns from CDM for join
CDM_join <- CDM %>%
  select(`CHARGE CODE`,`CHARGE DESC`,`DEPARTMENT CODE`,`GENERAL CPT4 CODE`) %>%
  rename(charge_code = `CHARGE CODE`) %>%
  distinct()
#join charge detail with CDM, PAT, and DEP IT
RIS_cpt4 <- left_join(RIS_charge_mod, CDM_join) %>%
  filter(!is.na(`GENERAL CPT4 CODE`)) %>%
  left_join(PAT, by = c("Pat.Type" = "Code")) %>%
  left_join(Dep_ID, by = c("DEPARTMENT CODE" = "Dept")) %>%
  mutate(Identifier = paste0(Org,"-",`DEPARTMENT CODE`,"-",`IP or OP`)) %>%
  mutate(CPT = paste0(`GENERAL CPT4 CODE`,Modifier)) %>%
  left_join(Premier_Dep) %>%
  mutate(Start = paste0(
    substr(Date,6,7),"/",
    substr(Date,9,10),"/",
    substr(Date,1,4))) %>%
  mutate(End = Start,
         Partner = "729805",
         Hosp = "NY0014")

#prepare OR diagnostic volume
RIS_OR_upload <- RIS_OR %>%
  filter(Org == "QN") %>%
  mutate(Partner = "729805",
         Hosp = "NY0014",
         DepID = "MSQRIS21008OR",
         Start = Date,
         End = Date,
         CPT = "71045") %>%
  group_by(Partner,Hosp,DepID,Start,End,CPT) %>%
  summarise(Volume = n()) %>%
  mutate(Budget = "0")

#prepare upload
RIS_cpt4_upload <- RIS_cpt4 %>%
  group_by(Partner,Hosp,DepID,Start,End,CPT) %>%
  summarise(Volume = n()) %>%
  mutate(Budget = "0")

#combine upload files
upload <- rbind(RIS_cpt4_upload, RIS_OR_upload)

##################need to create master append code  
old_master <- readRDS(paste0(RIS_dir,"Master/Master.rds"))
new_master <- rbind(old_master, upload)
saveRDS(new_master, paste0(RIS_dir,"Master/Master.rds"))
####################################################

#Trend Check
pp_mapping <- read_xlsx(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                               "Productivity/Universal Data/Mapping/",
                               "MSHS_Pay_Cycle.xlsx"))

pp_mapping$DATE <- format(as.Date(pp_mapping$DATE), "%m/%d/%Y")
pp_mapping$END.DATE <- format(as.Date(pp_mapping$END.DATE), "%m/%d/%Y")

pp_mapping[, 1] <- sapply(pp_mapping[, 1], as.character)

trend <- new_master %>%
  left_join(pp_mapping, by = c("End" = 'DATE')) %>% 
  na.omit(trend) %>% #DELETE once date issue is fixed
  ungroup() %>%
  group_by(DepID,END.DATE) %>%
  summarise(Vol = sum(Volume, na.rm = T)) %>%
  pivot_wider(id_cols = c(DepID),names_from = END.DATE, values_from = Vol)

View(trend)

#save upload
write.table(upload,paste0(RIS_dir,"Uploads/MSQ_RIS_",month_year,".csv"),
            sep = ",", row.names = F, col.names = F)
