library(tidyverse)
library(lubridate)

#read in charge detail and cdm
RIS <- read.csv(file.choose(), 
                colClasses = c(rep("character",21)))

#modifiers we want to keep
acceptable_modifiers <- c("26","50","53","tc")

#Trim whitespace in charge detail
RIS[,1:21] <- sapply(RIS[,1:21], trimws)

neuro <- RIS %>%
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
  #create grouping columns
  group_by(Date, CPT4) %>%
  #summarize and remove unnecessary columsn
  summarise(volume = n()) %>%
  #Add in necessary columns for upload
  mutate(Facility = "729805",
         Site = "NY0014",
         DeptID = "MSHRIS21050",
         Date = paste0(substr(Date,6,7), "/",
                       substr(Date,9,10), "/",
                       substr(Date,1,4)),
         Budget = "0") %>%
  #Create second date column
  mutate(Date2 = Date) %>%
  #select column order for upload
  select(Facility, Site, DeptID, Date, Date2, CPT4, volume, Budget)

##################################
old_master <- readRDS(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                          "Productivity/Volume - Data/MSH Data/RIS/Master/",
                          "Neurosurgery/Master.RDS"))

new_master <- rbind(old_master, neuro)

saveRDS(new_master, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                           "Productivity/Volume - Data/MSH Data/RIS/Master/",
                           "Neurosurgery/Master.RDS"))
##################################

#save upload
min_date <- min(as.Date(neuro$Date, format = "%m/%d/%Y"))
min_mon <- toupper(month.abb[month(min_date)])
min_date_save <- paste0(substr(min_date,9,10),
                        min_mon,
                        substr(min_date,1,4))
max_date <- max(as.Date(neuro$Date, format = "%m/%d/%Y"))
max_mon <- toupper(month.abb[month(max_date)])
max_date_save <- paste0(substr(max_date,9,10),
                        max_mon,
                        substr(max_date,1,4))
write.table(neuro, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                          "Productivity/Volume - Data/MSH Data/RIS/Uploads/",
                          "Neurosurgery/MSH_Neurosurgery_RIS_",
                          min_date_save, " to ",max_date_save, ".csv"),
            row.names = F, col.names = F, sep = ",")
