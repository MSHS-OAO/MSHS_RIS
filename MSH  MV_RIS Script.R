library(tidyverse)
library(lubridate)

#read in charge detail 
RIS <- read.csv(file.choose()
                  , colClasses = c(rep("character",21)))

#Trim whitespace in charge detail

RIS[,1:21] <- sapply(RIS[,1:21], trimws)

mobile_van <- RIS %>%
  
  #pivot longer to combine charge columns into one 
  pivot_longer(cols=c(10:17),
               names_to = "charge",
               values_to = "code") %>%
  
  #remove blanks
  filter(code != "") %>%
  
  #unite columns for final CPT4 code for upload
  unite(CPT4,code, Charge.Mod.1, Charge.Mod.2, Charge.Mod.3, Charge.Mod.4, sep ="") %>%
  
  # create grouping columns  - marc look at this and summarise 
  group_by(Date,CPT4) %>%
  
  #summarize and remove un-necessary columns 
  summarise(volume = n()) %>% 

mutate(Facility = "729805",
       Site = "NY0014",
       DeptID = "MSHRIS21009",
       Date = paste0(substr(Date,6,7), "/",
                     substr(Date,9,10), "/",
                     substr(Date,1,4)),
       Budget = "0") %>%
  #Create second date column
  mutate(Date2 = Date) %>%
  #select column order for upload
  select(Facility, Site, DeptID, Date, Date2, CPT4, volume, Budget)


#saveRDS(mobile_van, file = "Master.rds")


#----------------MASTER (Daniel hold off on Running this section until line 61)!!!!!!!----------------

old_master <- readRDS(paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                             "Productivity/Volume - Data/MSH Data/RIS/Master/",
                             "Mobile Van/Master.rds"))


#append current upload to master
new_master <- rbind(old_master, mobile_van)

#save new master 
saveRDS(new_master, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                           "Productivity/Volume - Data/MSH Data/RIS/Master/",
                           "Mobile Van/Master.rds"))

#save upload

min_date <- min(as.Date(mobile_van$Date, format = "%m/%d/%Y"))
min_mon <- toupper(month.abb[month(min_date)])
min_date_save <- paste0(substr(min_date,9,10),
                        min_mon,
                        substr(min_date,1,4))

max_date <- max(as.Date(mobile_van$Date, format = "%m/%d/%Y"))
max_mon <- toupper(month.abb[month(min_date)])
max_date_save <- paste0(substr(max_date,9,10),
                        min_mon,
                        substr(max_date,1,4))
write.csv(mobile_van, paste0("J:/deans/Presidents/SixSigma/MSHS Productivity/",
                          "Productivity/Volume - Data/MSH Data/RIS/Uploads/",
                          "Mobile Van/MSH_Mobile Van RIS_",
                          min_date_save, " to ",max_date_save, ".csv"),
          sep=",",row.names = F, col.names = F) #--- ask Greg about col names issue 



