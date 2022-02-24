library(tidyverse)
library(xlsx)
library(readxl)

#MSH RIS directory
RIS_dir <- paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                  "Productivity/Volume - Data/MSH Data/RIS/")

#month and year of charge detail
month_year <- "JAN2022"

#read in charge detail
RIS <- read.csv(paste0(RIS_dir, "Charge Detail/MSH_RIS_", month_year, ".csv"), 
                colClasses = c(rep("character", 21)))
#read in OR Diagnostic
RIS_OR <- read.csv(paste0(RIS_dir,"Charge Detail/OR Diagnostic/",
                                    "MSH_RIS_OR Diagnostic_",month_year,".csv"), 
                             colClasses = c(rep("character",9))) %>%
  select(MRN, Name, ACC, Date, Exam, Exam.Modifier, Org, Resource)

#Read in modality mapping file
modality <- read_xlsx(paste0(RIS_dir,"Mapping/Modality_Mapping.xlsx"))
#Read in PAT mapping
PAT <- read_xlsx(paste0(RIS_dir,"Mapping/PAT_Mapping.xlsx"))
#Premier Dep ID for CPT upload
Premier_Dep <- read_xlsx(paste0(RIS_dir,"Mapping/Premier_ID.xlsx"))

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

#Bring in modality
RIS_modality <- RIS_charge_mod %>%
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
                      substr(Date,1,4)))

#prepare OR diagnostic volume
RIS_OR_upload <- RIS_OR %>%
  filter(Org == "RM") %>%
  mutate(Partner = "729805",
         Hosp = "NY0014",
         DepID = "MSHRIS21008OR",
         Start = Date,
         End = Date,
         charge_code = "71045") %>%
  group_by(Partner,Hosp,DepID,Start,End,charge_code) %>%
  summarise(Volume = n()) %>%
  mutate(Budget = "0")


#prepare upload
RIS_modality_upload <- RIS_modality %>%
  group_by(Partner,Hosp,DepID,Start,End,charge_code) %>%
  summarise(Volume = n()) %>%
  mutate(Budget = "0")

#bind both files for upload
upload <- rbind(RIS_modality_upload, RIS_OR_upload)

####################################################  
old_master <- readRDS(paste0(RIS_dir,"Master/Master.rds"))
new_master <- rbind(old_master,upload)
saveRDS(new_master,paste0(RIS_dir,"Master/Master.rds"))
####################################################

#save upload
write.table(upload,paste0(RIS_dir,"Uploads/MSH_RIS_",month_year,".csv"),
            sep = ",", row.names = F, col.names = F)
