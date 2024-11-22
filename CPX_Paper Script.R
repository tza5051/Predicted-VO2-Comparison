# Making a new script to re-organize the orginal 
# using only the Access section. 


#github change 

# Packages  ---------------------------------------------------------------



library(tidyverse)
library(labelled)
library(easystats)
library(naniar)
#library(readr)
library(readxl)
library(writexl)
library(patchwork)
#library(forcats)
library(RColorBrewer)
library(ggsci)
library(ggrepel)
library(Hmisc)
library(ggbeeswarm)
library(skimr)
library(janitor)
library(ggridges)
library(ggpubr)
library(rstatix)
library(datarium)
library(ez)
library(gmodels)
library(effectsize)
library(data.table)
library(ggstatsplot)
library(reshape2)
library(REDCapR)
#library(stringr)
library(haven)
library(DataExplorer)
library(httr)
library(jsonlite)
library(gghighlight)
library(gtsummary)
library(knitr)
library(haven)
#library(plotly)
library(rlang)
#library(esquisse)
library(stringi)
library(data.table)
library(lubridate)
library(psych)
library(DT)
library(tableone)
library(irr)
library(PMCMRplus)
library(lme4)
library(broom.mixed)
library(ggalluvial)
library(NatParksPalettes)

# Bringing in Data --------------------------------------------------------


AccessCPET <- read_excel("R:/WRIISC/Databases/Pulmonary/PDCEN/Downloads/CPET_ODC_7_1_24.xlsx")
AccessCPET <- clean_names(AccessCPET)

AccessCPET <- AccessCPET %>% 
  filter(!is.na(wriiscid)) %>% 
  select(
    wriiscid	,
    watts_peak,
    cycle	,
    treadmill	,
    exercise	,
    eperf_peakvo2	,
    eperf_vo2kg_peak) %>% 
  rename(
    Y_N_cycle	=	cycle	,
    Y_N_tredmill	=	treadmill	,
    VO2.peak	=	eperf_peakvo2	,
    VO2_kg.peak	=	eperf_vo2kg_peak)

Access_IDS <- AccessCPET$wriiscid

AccessPatient2023 <-  read_excel("R:/WRIISC/Databases/Pulmonary/PDCEN/Downloads/Patient_ODC_7_1_24.xlsx")
AccessPatient2023 <- clean_names(AccessPatient2023)

AccessPFT2023 <-  read_excel("R:/WRIISC/Databases/Pulmonary/PDCEN/Downloads/Pulmonary_ODC_7_1_24.xlsx")
AccessPFT2023 <- clean_names(AccessPFT2023)


AccessPatient2023 <- AccessPatient2023 %>% 
  filter(
    wriiscid %in% Access_IDS
  )

AccessPFT2023 <- AccessPFT2023 %>% 
  select(
    wriiscid,
    race
  )

AccessPatient2023 <- 
  merge(
    AccessPatient2023,
    AccessPFT2023,
    by = "wriiscid"
  ) 

AccessPatient2023 <- AccessPatient2023 %>% 
  mutate(
    race = case_when(
      grepl("Asia", race) ~ "SEA",
      TRUE ~ race
    )
  )

rm(AccessPFT2023
)


#write.csv(colnames(PDCEN_CPET), "Variables_CPET.csv",row.names = FALSE)

AccessCPET <-
  merge(
    AccessPatient2023,
    AccessCPET,
    by = "wriiscid"
  ) %>% 
  rename(
    Subject_ID = wriiscid
  )

AccessCPET <- AccessCPET %>% 
  filter(
    !is.na(VO2.peak)
  )

# Setting Up dataset for predicted equations

AccessCPET <- AccessCPET %>% 
  mutate(
    Mode = case_when(
      Y_N_cycle == TRUE ~ "Bike",
      Y_N_tredmill == TRUE ~ "Treadmill",
      str_detect(exercise, regex("bike|watt|cycle|ergometry|ergo|w", ignore_case = TRUE)) ~ "Bike",
      str_detect(exercise, regex("Tread|bruce|Gervino", ignore_case = TRUE)) ~ "Treadmill",   
      TRUE ~ NA_character_
    )
  )

AccessCPET$Mode <- as.factor(AccessCPET$Mode)

AccessCPET <-  AccessCPET %>% 
  select(
    Subject_ID, pdcen_site, Mode, everything()
  )


AccessCPET <- AccessCPET %>% 
  filter(!is.na(VO2.peak) & !is.na(height) & !is.na(age) & !is.na(bmi) & !is.na(race) &!is.na(Mode))


AccessCPET %>% 
  filter(duplicated(Subject_ID)) %>% 
  pull(Subject_ID)

AccessCPET <- AccessCPET %>% 
  distinct(Subject_ID, .keep_all = TRUE) 

AccessCPET <- AccessCPET %>% 
  rename(
    weight_lbs = weight,
    height_in = height
  )

AccessCPET <- AccessCPET %>% 
  mutate(
    weight_kg = weight_lbs * 0.4546,
    height_cm = height_in * 2.54,
    bmi = (weight_lbs/(height_in * height_in) * 703),
    gender = factor(case_when(
      gender == 0 ~ 2,
      TRUE ~ gender
    ))
  )

AccessCPET <- AccessCPET %>% 
  rename(
    VO2_peak.actual = VO2.peak,
    VO2_kg_peak.actual = VO2_kg.peak,
    Watts_peak = watts_peak
  )

AccessCPET <- AccessCPET %>% 
  mutate(
    VO2_peak.actual = case_when(
      VO2_peak.actual < 10 ~ VO2_peak.actual * 1000,
      VO2_peak.actual >= 10 ~ VO2_peak.actual, 
      TRUE ~ NA_real_
    ))

AccessCPET <- AccessCPET %>% 
  mutate(VO2_kg_peak.actual = VO2_peak.actual/weight_kg)


AccessCPET <- AccessCPET %>% 
  mutate(
    weight_ideal = case_when(
      gender == 1 ~ 0.79 * height_cm - 60.7,
      gender == 2 ~ 0.65 * height_cm - 42.8,
      TRUE ~ NA_real_),
    cycle_factor = case_when(
      gender == 1 ~ 50.72 - 0.372 * (age),
      gender == 2 ~ 22.78 - 0.17 * (age),
      TRUE ~ NA_real_)
  )

#QCing

#removing Vet who did arm and crazy vo2 value if any (assuming typo)
# removing test subjects, lastname = Sandhya

#Lookiing for vets that did arm 
AccessCPET %>% 
  filter(
    str_detect(exercise, regex("arm", ignore_case = TRUE))
  ) %>% 
  pull(Subject_ID)


# 35700 35977 36815

AccessCPET <- AccessCPET %>% 
  filter(Subject_ID != 36815 & Subject_ID != 35700 & Subject_ID != 35977)


# removing vets below 500ml/min and above 10,000 min ()
AccessCPET <- AccessCPET %>% 
  filter(
    VO2_peak.actual > 500 & VO2_peak.actual < 10000
  )
#filtering out test files
AccessCPET <- AccessCPET %>% 
  filter(
    lastname != "Bandi")
#filtering out super low bmi of 2.3
AccessCPET <- AccessCPET %>% 
  filter(
    bmi > 8)


#final = 305 so far


#assessing subjects based on age and bmi

#categorizing BMI

quantile(AccessCPET$bmi)
#       0%      25%      50%      75%     100% 
# 16.72362 28.01578 30.84592 34.82191 46.85737  

quantile(AccessCPET$age)
# 0%  25%  50%  75% 100% 
# 24   36   44   51   67 


AccessCPET <- AccessCPET %>% 
  mutate(
    BMI_cat = factor(case_when(
      bmi <= 28 ~ 1,
      bmi > 28 & bmi <= 35 ~ 2,
      bmi > 35 ~ 3, 
      TRUE ~ NA_real_
    )),
    Age_cat = factor(case_when(
      age <= 36 ~ 1,
      age > 36 & age <= 51 ~ 2,
      age > 51 ~ 3,
      TRUE ~ NA_real_
    ))
  )


#bring in redcap data for race
IDS <- AccessCPET %>% 
  pull(Subject_ID)

Redcap_PDCEN <- read.csv("//r04.med.va.gov/v03/EAS/Research/WRIISC/Databases/Pulmonary/PDCEN/Downloads/PDCEN_Database_files/PDCEN_REDCAP_8_22_24.csv")
Redcap_PDCEN <- Redcap_PDCEN %>% 
  filter(Subject_ID %in% IDS) %>% 
  select(
    Subject_ID, 
    race_redcap = race, 
    ethncity_redcap = ethncity, 
    ethnic_gli
  )


# Access_CPET is the base file used to create everything 

# Setting up equations ----------------------------------------------------



# Getting all equations for all ODC subjects
# Same process as above and then making the same plots

# Setting up 2 different data sets
# 1) Uncorrected = all normal, did not calculate if wrong mode is used. 
# 2) Corrected = use all equations but correct it by a factor of 1.11. Bike to Tread --> * 1.11, Tread to Bike --> * .89 (only if no corrections is already there)

#### 1)----------------------------------------------------------------------------
AccessCPET_Uncorrected <- AccessCPET %>% 
    mutate(
        FRIEND_Predicted = case_when(
          gender == 1 & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 2)) * weight_kg,
          gender == 2 & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 2)) * weight_kg,
          gender == 1 & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 1)) * weight_kg,
          gender == 2 & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 1)) * weight_kg,
          TRUE ~ NA_real_),
        
        Wasserman_Predicted = case_when(
          gender == 1 & Mode == "Bike" ~ (weight_kg * (50.72 - (0.372 * age))), 
          gender == 2 & Mode == "Bike" ~ (weight_kg + 42.8) * (22.78 - (0.17 * age)),
          TRUE ~ NA_real_),
        
        Hansen_Predicted = case_when(
          gender == 1 & Mode == "Bike" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg) / 2) * cycle_factor),
          gender == 1 & Mode == "Bike" & weight_kg == weight_ideal ~ (weight_kg * cycle_factor),
          gender == 1 & Mode == "Bike" & weight_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_kg - weight_ideal)),
          
          # gender == 1 & Mode == "Treadmill" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg) / 2) * cycle_factor) * 1.11,
          # gender == 1 & Mode == "Treadmill" & weight_kg == weight_ideal ~ (weight_kg * cycle_factor) * 1.11,
          # gender == 1 & Mode == "Treadmill" & weight_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_kg - weight_ideal)) * 1.11,
          # 
          gender == 2 & Mode == "Bike" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg + 86) / 2) * cycle_factor),
          gender == 2 & Mode == "Bike" & weight_kg == weight_ideal ~ ((weight_kg + 43) * cycle_factor),
          gender == 2 & Mode == "Bike" & weight_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_kg - weight_ideal)),
          
          # gender == 2 & Mode == "Treadmill" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg + 86) / 2) * cycle_factor) * 1.11,
          # gender == 2 & Mode == "Treadmill" & weight_kg == weight_ideal ~ ((weight_kg + 43) * cycle_factor) * 1.11,
          # gender == 2 & Mode == "Treadmill" & weight_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_kg - weight_ideal)) * 1.11,
          # 
          TRUE ~ NA_real_),
        
        
        Bruce_Predicted = case_when(
          gender == 1 & Mode == "Treadmill" ~ ((60 - (0.55* age)) * (weight_kg)), 
          gender == 2 & Mode == "Treadmill" ~ ((48 - (0.37 * age)) * (weight_kg)),
          TRUE ~ NA_real_),
        
        Jones_Predicted = case_when(
          gender == 1 & Mode == "Bike" ~ (-3.76 + 0.034 * height_cm + 0.022 * weight_kg - 0.028 * age) * 1000, 
          gender == 2 & Mode == "Bike" ~ (-2.26 + 0.025 * height_cm + 0.01 * weight_kg - 0.018 * age) * 1000,
          TRUE ~ NA_real_),
        
        Neder_Predicted = case_when(
          gender == 1 & Mode == "Bike" ~ ((-24.3 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 1125), 
          gender == 2 & Mode == "Bike" ~ ((-13.7 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 60),
          TRUE ~ NA_real_)
      )


AccessCPET_Uncorrected <- AccessCPET_Uncorrected %>% 
  mutate(
    Friend_pp = (VO2_peak.actual/FRIEND_Predicted) * 100,
    Wasserman_pp = (VO2_peak.actual/Wasserman_Predicted) * 100,
    Hansen_pp = (VO2_peak.actual/Hansen_Predicted) * 100,
    Bruce_pp = (VO2_peak.actual/Bruce_Predicted) * 100,
    Jones2_pp = (VO2_peak.actual/Jones_Predicted) * 100,
    Neder_pp = (VO2_peak.actual/Neder_Predicted) * 100
  )



Access_Percent_predicted_Uncorrected <- AccessCPET_Uncorrected %>%  
  select(
    Subject_ID,
    Mode,
    VO2_peak.actual,
    FRIEND_Predicted,
    Wasserman_Predicted,
    Hansen_Predicted,
    Bruce_Predicted,
    Jones_Predicted,
    Neder_Predicted,
    Friend_pp,
    Wasserman_pp,
    Hansen_pp,
    Bruce_pp,
    Jones2_pp,
    Neder_pp
  ) %>%  
  rename(
    Measured_Predicted = VO2_peak.actual,
    FRIEND_Percent.Predicted = Friend_pp,
    Wasserman_Percent.Predicted = Wasserman_pp,
    Hansen_Percent.Predicted = Hansen_pp,
    Bruce_Percent.Predicted = Bruce_pp,
    Jones_Percent.Predicted = Jones2_pp,
    Neder_Percent.Predicted = Neder_pp
  )

AccessCPET_Uncorrected <-  AccessCPET_Uncorrected  %>% 
  rename(
    FRIEND_Percent.Predicted = Friend_pp,
    Wasserman_Percent.Predicted = Wasserman_pp,
    Hansen_Percent.Predicted = Hansen_pp,
    Bruce_Percent.Predicted = Bruce_pp,
    Jones_Percent.Predicted = Jones2_pp,
    Neder_Percent.Predicted = Neder_pp
  )


Access_Percent_predicted_tidy_Uncorrected <- Access_Percent_predicted_Uncorrected %>%  
  pivot_longer(
    cols = -(c(Subject_ID, Mode)),
    names_to = c("Equation", ".value"),
    names_pattern = "(Measured|FRIEND|Wasserman|Hansen|Bruce|Jones|Neder)_(Predicted|Percent.Predicted)"
  )

Access_Percent_predicted_tidy_Uncorrected <- Access_Percent_predicted_tidy_Uncorrected %>%  
  mutate(
    Equation = as_factor(Equation)
  )

Access_Percent_predicted_tidy_Uncorrected <- Access_Percent_predicted_tidy_Uncorrected %>%  
  mutate(
    Clinical_Interpretation = case_when(
      Percent.Predicted < 80 ~ 1, #low VO2
      Percent.Predicted >= 80 ~ 0, #normal 
      TRUE ~ NA_real_
    )
  )

Access_Percent_predicted_tidy_Uncorrected$Clinical_Interpretation <- factor(Access_Percent_predicted_tidy_Uncorrected$Clinical_Interpretation)


Access_Interpertation_wide_Uncorrected <- Access_Percent_predicted_tidy_Uncorrected %>% 
  pivot_wider(id_cols = Subject_ID, 
              names_from = Equation, 
              values_from = Clinical_Interpretation)

Access_Interpertation_wide_Uncorrected <- Access_Interpertation_wide_Uncorrected %>% 
  select(
    -(Measured)
  )


Access_Percent_predicted_tidy_Uncorrected <- 
  merge(
    AccessCPET_Uncorrected[,c("Subject_ID", "age", "gender", "bmi", "race")],
    Access_Percent_predicted_tidy_Uncorrected,
    by = "Subject_ID"
  )

Access_Percent_predicted_tidy_Uncorrected$Equation <- factor(Access_Percent_predicted_tidy_Uncorrected$Equation,
                                                             levels =  c("Measured","Wasserman", "FRIEND","Hansen","Bruce","Jones", "Neder")) 

Access_Percent_predicted_tidy_Uncorrected$Subject_ID <- factor(Access_Percent_predicted_tidy_Uncorrected$Subject_ID)
Access_Percent_predicted_tidy_Uncorrected$race <- factor(Access_Percent_predicted_tidy_Uncorrected$race)
Access_Percent_predicted_tidy_Uncorrected$gender <- factor(Access_Percent_predicted_tidy_Uncorrected$gender)


#### 2)---------------------------------------------------------------------------


#Corrected values based on Mode:
# Wasserman, Jones, Neder: for Treadmill: x1.11
# Bruce: for Bike: x 0.89
AccessCPET_Corrected <- AccessCPET %>% 
  mutate(
    FRIEND_Predicted = case_when(
      gender == 1 & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 2)) * weight_kg,
      gender == 2 & Mode == "Bike" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 2)) * weight_kg,
      gender == 1 & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 1) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 1)) * weight_kg,
      gender == 2 & Mode == "Treadmill" ~ (45.2 - (0.35 * age) - (10.9 * 2) - (0.15 * weight_lbs) + (0.68 * height_in) - (0.46 * 1)) * weight_kg,
      TRUE ~ NA_real_),
    
    Wasserman_Predicted = case_when(
      gender == 1 & Mode == "Treadmill" ~ ((weight_kg * (50.72 - (0.372 * age))) * 1.11), 
      gender == 2 & Mode == "Treadmill" ~ (((weight_kg + 42.8) * (22.78 - (0.17 * age))) * 1.11),
      gender == 1 & Mode == "Bike" ~ (weight_kg * (50.72 - (0.372 * age))), 
      gender == 2 & Mode == "Bike" ~ ((weight_kg + 42.8) * (22.78 - (0.17 * age))),
      TRUE ~ NA_real_),
    
    Hansen_Predicted = case_when(
      gender == 1 & Mode == "Bike" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg) / 2) * cycle_factor),
      gender == 1 & Mode == "Bike" & weight_kg == weight_ideal ~ (weight_kg * cycle_factor),
      gender == 1 & Mode == "Bike" & weight_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_kg - weight_ideal)),
      
      gender == 1 & Mode == "Treadmill" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg) / 2) * cycle_factor) * 1.11,
      gender == 1 & Mode == "Treadmill" & weight_kg == weight_ideal ~ (weight_kg * cycle_factor) * 1.11,
      gender == 1 & Mode == "Treadmill" & weight_kg > weight_ideal ~ ((weight_ideal * cycle_factor) + 6 * (weight_kg - weight_ideal)) * 1.11,
      
      gender == 2 & Mode == "Bike" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg + 86) / 2) * cycle_factor),
      gender == 2 & Mode == "Bike" & weight_kg == weight_ideal ~ ((weight_kg + 43) * cycle_factor),
      gender == 2 & Mode == "Bike" & weight_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_kg - weight_ideal)),
      
      gender == 2 & Mode == "Treadmill" & weight_kg < weight_ideal ~ (((weight_ideal + weight_kg + 86) / 2) * cycle_factor) * 1.11,
      gender == 2 & Mode == "Treadmill" & weight_kg == weight_ideal ~ ((weight_kg + 43) * cycle_factor) * 1.11,
      gender == 2 & Mode == "Treadmill" & weight_kg > weight_ideal ~ (((weight_ideal + 43) * cycle_factor) + 6 * (weight_kg - weight_ideal)) * 1.11,
      TRUE ~ NA_real_),
    
    Bruce_Predicted = case_when(
      gender == 1 & Mode == "Treadmill" ~ ((60 - (0.55* age)) * (weight_kg)), 
      gender == 2 & Mode == "Treadmill" ~ ((48 - (0.37 * age)) * (weight_kg)),
      gender == 1 & Mode == "Bike" ~ (((60 - (0.55* age)) * (weight_kg)) * 0.89), 
      gender == 2 & Mode == "Bike" ~ (((48 - (0.37 * age)) * (weight_kg)) * 0.89),
      TRUE ~ NA_real_),
    
    Jones_Predicted = case_when(
      gender == 1 & Mode == "Bike" ~ (-3.76 + 0.034 * height_cm + 0.022 * weight_kg - 0.028 * age) * 1000, 
      gender == 2 & Mode == "Bike" ~ (-2.26 + 0.025 * height_cm + 0.01 * weight_kg - 0.018 * age) * 1000,
      gender == 1 & Mode == "Treadmill" ~ (((-3.76 + 0.034 * height_cm + 0.022 * weight_kg - 0.028 * age) * 1000) * 1.11), 
      gender == 2 & Mode == "Treadmill" ~ (((-2.26 + 0.025 * height_cm + 0.01 * weight_kg - 0.018 * age) * 1000) * 1.11),
      TRUE ~ NA_real_),
    
    Neder_Predicted = case_when(
      gender == 1 & Mode == "Bike" ~ ((-24.3 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 1125), 
      gender == 2 & Mode == "Bike" ~ ((-13.7 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 60),
      gender == 1 & Mode == "Treadmill" ~ ((((-24.3 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 1125)) * 1.11), 
      gender == 2 & Mode == "Treadmill" ~ ((((-13.7 * age) + (10.2 * weight_kg) + (8.3 * height_cm) + 60)) * 1.11),
      TRUE ~ NA_real_)
  )

AccessCPET_Corrected <- AccessCPET_Corrected %>% 
  mutate(
    Friend_pp = (VO2_peak.actual/FRIEND_Predicted) * 100,
    Wasserman_pp = (VO2_peak.actual/Wasserman_Predicted) * 100,
    Hansen_pp = (VO2_peak.actual/Hansen_Predicted) * 100,
    Bruce_pp = (VO2_peak.actual/Bruce_Predicted) * 100,
    Jones2_pp = (VO2_peak.actual/Jones_Predicted) * 100,
    Neder_pp = (VO2_peak.actual/Neder_Predicted) * 100
  )



Access_Percent_predicted_Corrected <- AccessCPET_Corrected %>%  
  select(
    Subject_ID,
    Mode,
    VO2_peak.actual,
    FRIEND_Predicted,
    Wasserman_Predicted,
    Hansen_Predicted,
    Bruce_Predicted,
    Jones_Predicted,
    Neder_Predicted,
    Friend_pp,
    Wasserman_pp,
    Hansen_pp,
    Bruce_pp,
    Jones2_pp,
    Neder_pp
  ) %>%  
  rename(
    Measured_Predicted = VO2_peak.actual,
    FRIEND_Percent.Predicted = Friend_pp,
    Wasserman_Percent.Predicted = Wasserman_pp,
    Hansen_Percent.Predicted = Hansen_pp,
    Bruce_Percent.Predicted = Bruce_pp,
    Jones_Percent.Predicted = Jones2_pp,
    Neder_Percent.Predicted = Neder_pp
  )

AccessCPET_Corrected <-  AccessCPET_Corrected  %>% 
  rename(
    FRIEND_Percent.Predicted = Friend_pp,
    Wasserman_Percent.Predicted = Wasserman_pp,
    Hansen_Percent.Predicted = Hansen_pp,
    Bruce_Percent.Predicted = Bruce_pp,
    Jones_Percent.Predicted = Jones2_pp,
    Neder_Percent.Predicted = Neder_pp
  )


Access_Percent_predicted_tidy_Corrected <- Access_Percent_predicted_Corrected %>%  
  pivot_longer(
    cols = -(c(Subject_ID, Mode)),
    names_to = c("Equation", ".value"),
    names_pattern = "(Measured|FRIEND|Wasserman|Hansen|Bruce|Jones|Neder)_(Predicted|Percent.Predicted)"
  )

Access_Percent_predicted_tidy_Corrected <- Access_Percent_predicted_tidy_Corrected %>%  
  mutate(
    Equation = as_factor(Equation)
  )

Access_Percent_predicted_tidy_Corrected <- Access_Percent_predicted_tidy_Corrected %>%  
  mutate(
    Clinical_Interpretation = case_when(
      Percent.Predicted < 80 ~ 1, #low VO2
      Percent.Predicted >= 80 ~ 0, #normal 
      TRUE ~ NA_real_
    )
  )

Access_Percent_predicted_tidy_Corrected$Clinical_Interpretation <- factor(Access_Percent_predicted_tidy_Corrected$Clinical_Interpretation)


Access_Interpertation_wide_Corrected <- Access_Percent_predicted_tidy_Corrected %>% 
  pivot_wider(id_cols = Subject_ID, 
              names_from = Equation, 
              values_from = Clinical_Interpretation)

Access_Interpertation_wide_Corrected <- Access_Interpertation_wide_Corrected %>% 
  select(
    -(Measured)
  )


Access_Percent_predicted_tidy_Corrected <- 
  merge(
    AccessCPET_Corrected[,c("Subject_ID", "age", "gender", "bmi", "race")],
    Access_Percent_predicted_tidy_Corrected,
    by = "Subject_ID"
  )

Access_Percent_predicted_tidy_Corrected$Equation <- factor(Access_Percent_predicted_tidy_Corrected$Equation,
                                                             levels =  c("Measured","Wasserman", "FRIEND","Hansen","Bruce","Jones", "Neder")) 

Access_Percent_predicted_tidy_Corrected$Subject_ID <- factor(Access_Percent_predicted_tidy_Corrected$Subject_ID)
Access_Percent_predicted_tidy_Corrected$race <- factor(Access_Percent_predicted_tidy_Corrected$race)
Access_Percent_predicted_tidy_Corrected$gender <- factor(Access_Percent_predicted_tidy_Corrected$gender)


# Corrected Dataset (x1.11 or x.89) ---------------------------------------------------------
#### Plots based on corrected dataset --------------------------------------------------------

# plots to look at difference in percent predicted and predicted VO2
# Using the Corrected dataset below

Access_Percent_predicted_tidy_Corrected %>% 
  filter(Equation != "Measured") %>% 
  ggplot(aes(x = Percent.Predicted, color = Equation)) +
  geom_density()


AccessCPET_Corrected %>% 
  ggplot(aes(x = age)) +
  geom_histogram()

AccessCPET_Corrected %>% 
  ggplot(aes(x = bmi)) +
  geom_histogram()


#Figure 1 of paper: Violin plot of d Predicted Peak VȮ2 using the 5 equations.
Access_Percent_predicted_tidy_Corrected %>% 
  filter(Equation != "Measured") %>% 
  ggplot(aes(x = Equation, y = Percent.Predicted)) +
  geom_violindot(aes(fill = Equation), binwidth = 5, dots_size = 0.1, color_dots ="black", fill_dots = "black") +
  theme_classic() +
  scale_fill_jco() +
  labs(y = "Percent Predicted", x = "") +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

# ggsave("Violin_VO2_PeakPercentPredicited.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )

#Figure 3 of paper: Violin plot of % Predicted VȮ2 across equations.
Access_Percent_predicted_tidy_Corrected %>% 
  ggplot(aes(x = Equation, y = Predicted)) +
  geom_violindot(aes(fill = Equation), binwidth = 100, dots_size = 0.1, color_dots ="black", fill_dots = "black") +
  # stat_summary(fun = mean, geom = "point", size = 3) +
  # stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2) +
  theme_classic() +
  scale_fill_jco() +
  labs(y = expression(Peak~VO[2]~(ml%*%min^-1)), x = "") +
  theme(
   
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

# ggsave("Violin_VO2_predicted.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )



#plot to look at differences in classification between each equation

Access_Percent_predicted_tidy_Corrected %>% 
  mutate(
    Clinical_Interpretation = case_when(
      Clinical_Interpretation == 0 ~ "Preserved Exercise Capacity",
      Clinical_Interpretation == 1 ~ "Reduced Exercise Capacity",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(Equation != "Measured") %>% 
  ggplot(aes(x = Equation, fill = Clinical_Interpretation)) +
  geom_bar(position = "fill") +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  theme_classic() +
  labs(y = "Proportion of Subjects", x = "", fill = "Clinical Interpretation") 

  # ggsave("ClinicalInterpretation.png",
  #      path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )


#agreement plots----------------------------

# Making a plot dataset for these plots to make it easier to deal with

AgreementPlots <- AccessCPET_Corrected %>% 
  select(
    Sex = gender,
    bmi,
    BMI_cat,
    Wasserman_Percent.Predicted,
    FRIEND_Percent.Predicted,
    Hansen_Percent.Predicted,
    Jones_Percent.Predicted,
    Bruce_Percent.Predicted,
    Neder_Percent.Predicted
  )

AgreementPlots <- AgreementPlots %>%  
  mutate(
    Sex = case_when(
      Sex == 1 ~ "Male",
      Sex == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    BMI_cat = as.factor(case_when(
      bmi <= 28 ~ "BMI: < 25%",
      
      bmi > 28 & bmi <= 35 ~ "BMI: 25-75%",
      
      bmi > 35 ~ "BMI: > 75%",
      
      TRUE ~ NA_character_)))

AgreementPlots$BMI_cat <- factor(AgreementPlots$BMI_cat ,
                                 levels = c("BMI: < 25%", "BMI: 25-75%","BMI: > 75%"))


AgreementPlots$Sex_BMI <- 
  with(AgreementPlots, interaction(Sex, BMI_cat, sep = ", "))

# setting up colors
# Define color palette 
Agreement_colors <- c("Male, BMI: < 25%" = "#B9741F", "Male, BMI: 25-75%" = "#213958", "Male, BMI: > 75%" = "#990006", 
                      "Female, BMI: < 25%" = "#B9741F", "Female, BMI: 25-75%" = "#213958", "Female, BMI: > 75%" = "#990006")

Agreement_shape <- c("Male, BMI 1" = "#B9741F", "Male, BMI 2" = "#213958", "Male, BMI 3" = "#990006", 
                      "Female, BMI 1" = "#B9741F", "Female, BMI 2" = "#213958", "Female, BMI 3" = "#990006")


Agreement_colors <- c("BMI: < 25%" = "#B9741F", "BMI: 25-75%" = "#213958", "BMI: > 75%" = "#990006")

Agreement_shape <- c("Male, BM" = "#B9741F", "Male, BMI 2" = "#213958", "Male, BMI 3" = "#990006", 
                     "Female, BMI 1" = "#B9741F", "Female, BMI 2" = "#213958", "Female, BMI 3" = "#990006")


# FRIEND to Wasserman
table(Access_Interpertation_wide_Corrected$FRIEND, Access_Interpertation_wide_Corrected$Wasserman)

  
F_vs_W <- 
AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Wasserman_Percent.Predicted, y = FRIEND_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: FRIEND", x = "% Predicted: Wasserman", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))


F_vs_W

# ggsave("Agreement_FvsW.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )



# FRIEND to Hansen

table(Access_Interpertation_wide_Corrected$FRIEND, Access_Interpertation_wide_Corrected$Hansen)

F_vs_H <- 
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Hansen_Percent.Predicted, y = FRIEND_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: FRIEND", x = "% Predicted: Hansen", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

F_vs_H
# ggsave("Agreement_FvsH.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )


# FRIEND to Bruce

table(Access_Interpertation_wide_Corrected$FRIEND, Access_Interpertation_wide_Corrected$Bruce)

F_vs_B <-  
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Bruce_Percent.Predicted, y = FRIEND_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: FRIEND", x = "% Predicted: Bruce", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

F_vs_B
# ggsave("Agreement_FvsB.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )



# FRIEND to Jones

table(Access_Interpertation_wide_Corrected$FRIEND, Access_Interpertation_wide_Corrected$Jones)

F_vs_J <-  
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Jones_Percent.Predicted, y = FRIEND_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: FRIEND", x = "% Predicted: Jones", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

F_vs_J
# ggsave("Agreement_FvsJ.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )


#FRIEND to Neder
table(Access_Interpertation_wide_Corrected$FRIEND, Access_Interpertation_wide_Corrected$Neder)


F_vs_N <-  
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Wasserman_Percent.Predicted, y = FRIEND_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: FRIEND", x = "% Predicted: Neder", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

F_vs_N




 #Wasserman to Hansen

table(Access_Interpertation_wide_Corrected$Wasserman, Access_Interpertation_wide_Corrected$Hansen)

W_vs_H <-  
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Hansen_Percent.Predicted, y = Wasserman_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: Wasserman", x = "% Predicted: Hansen", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

W_vs_H
# ggsave("Agreement_WvsH.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )


#Wasserman to Bruce

table(Access_Interpertation_wide_Corrected$Wasserman, Access_Interpertation_wide_Corrected$Bruce)

W_vs_B <-
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Bruce_Percent.Predicted, y = Wasserman_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: Wasserman", x = "% Predicted: Bruce", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

W_vs_B
# ggsave("Agreement_WvsB.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )


#Wasserman to Jones

table(Access_Interpertation_wide_Corrected$Wasserman, Access_Interpertation_wide_Corrected$Jones)

w_vs_J <- 
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Jones_Percent.Predicted, y = Wasserman_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: Wasserman", x = "% Predicted: Jones", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

w_vs_J

# ggsave("Agreement_WvsJ.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )



# Wasserman to Neder

table(Access_Interpertation_wide_Corrected$Wasserman, Access_Interpertation_wide_Corrected$Neder)

W_vs_N <-  
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Neder_Percent.Predicted, y = Wasserman_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: Wasserman", x = "% Predicted: Neder", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

W_vs_N


#Hansen to Bruce

table(Access_Interpertation_wide_Corrected$Hansen, Access_Interpertation_wide_Corrected$Bruce)

H_vs_B <-   
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Bruce_Percent.Predicted, y = Hansen_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: Hansen", x = "% Predicted: Bruce", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

H_vs_B

# 
# ggsave("Agreement_HvsB.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )


#Hansen to Jones

table(Access_Interpertation_wide_Corrected$Hansen, Access_Interpertation_wide_Corrected$Jones)

H_vs_J <-   
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Jones_Percent.Predicted, y = Hansen_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: Hansen", x = "% Predicted: Jones", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

H_vs_J
# ggsave("Agreement_HvsJ.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )


#Hansen to Neder

table(Access_Interpertation_wide_Corrected$Hansen, Access_Interpertation_wide_Corrected$Neder)

H_vs_N <- 
AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Neder_Percent.Predicted, y = Hansen_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: Hansen", x = "% Predicted: Neder", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

H_vs_N

#Bruce to Jones

table(Access_Interpertation_wide_Corrected$Bruce, Access_Interpertation_wide_Corrected$Jones)

B_vs_J <- 
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Jones_Percent.Predicted, y = Bruce_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: Bruce", x = "% Predicted: Jones", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

B_vs_J

#Bruce to Neder

table(Access_Interpertation_wide_Corrected$Bruce, Access_Interpertation_wide_Corrected$Neder)

B_vs_N <-   
  AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Neder_Percent.Predicted, y = Bruce_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: Bruce", x = "% Predicted: Neder", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

B_vs_N


#Jones to Neder

table(Access_Interpertation_wide_Corrected$Jones, Access_Interpertation_wide_Corrected$Neder)

J_vs_N <-   AgreementPlots %>%  
  ggplot() +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "lightgrey", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "lightgrey", alpha = 0.02) +
  
  geom_point(aes(x = Neder_Percent.Predicted, y = Jones_Percent.Predicted, color = BMI_cat, shape = Sex)) +
  
  #adding labs
  labs(y = "% Predicted: Jones", x = "% Predicted: Neder", color = "BMI", shape = "Sex") +    
  geom_text(label = "Reclassified", x = 20, y = 150, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Reclassified", x = 140, y = 10, color = "black", size = 4.5, alpha = 0.02, family = "serif") +
  geom_text(label = "Line of Identity", x= 5, y = 5, angle = 45, hjust = 0, vjust = -0.5, size = 4.5, alpha = 0.02, family = "serif") +
  # geom_text(label = "0 -> 0", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  # geom_text(label = "1 -> 1", x = 40, y = 25, color = "black", size = 4.5, alpha = 0.02) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  guides(color = guide_legend(override.aes = list(shape = 22, fill = c("#B9741F", "#213958", "#990006")))) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  
  theme_classic() +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    text = element_text(family = "serif"))

J_vs_N


# ggsave("Agreement_BvsJ.png",
#        path = "R:/AirHazardsCenter/AHBPCE-PDCEN_site data/Working Analyses/CPET Equations/Plots" )

# organizing plots

#(F_vs_W + F_vs_H) / (F_vs_B + F_vs_J) / (W_vs_H + W_vs_B) / (w_vs_J + H_vs_B) / (H_vs_J + B_vs_J) + legend.position = "collect" +  plot_annotation(title = "Classification of Exercise Tolerance:")



# (F_vs_W + F_vs_H) / (F_vs_B + F_vs_J) / (F_vs_N + plot_spacer()) + plot_layout(guides = "collect") +  plot_annotation(title = "A")
# 
# (W_vs_H + W_vs_B) / ((w_vs_J) + W_vs_N) / (plot_spacer() + plot_spacer()) + plot_layout(guides = "collect") +  plot_annotation(title = "B")
# 
# ((H_vs_B) + (H_vs_J)) / (H_vs_N + plot_spacer()) / (plot_spacer() + plot_spacer()) + plot_layout(guides = "collect") +  plot_annotation(title = "C")
# 
# ((B_vs_J) + (B_vs_N)) / ((plot_spacer() + plot_spacer()) / (plot_spacer() + plot_spacer()) + plot_layout(guides = "collect") +  plot_annotation(title = "D"))





#### Analysis Corrected Dataset----------------------------------------------------------------

#assess normality


shapiro.test(Access_Percent_predicted_tidy_Corrected$Predicted)

shapiro.test(AccessCPET_Corrected$FRIEND_Percent.Predicted)
shapiro.test(AccessCPET_Corrected$FRIEND_Predicted)

shapiro.test(AccessCPET_Corrected$Wasserman_Percent.Predicted)
shapiro.test(AccessCPET_Corrected$Wasserman_Predicted)

shapiro.test(AccessCPET_Corrected$Hansen_Percent.Predicted)
shapiro.test(AccessCPET_Corrected$Hansen_Predicted)

shapiro.test(AccessCPET_Corrected$Jones_Percent.Predicted)
shapiro.test(AccessCPET_Corrected$Jones_Predicted)

shapiro.test(AccessCPET_Corrected$Bruce_Percent.Predicted)
shapiro.test(AccessCPET_Corrected$Bruce_Predicted)

shapiro.test(AccessCPET_Corrected$Neder_Percent.Predicted)
shapiro.test(AccessCPET_Corrected$Neder_Predicted)

shapiro.test(AccessCPET_Corrected$weight_kg)
shapiro.test(AccessCPET_Corrected$height_cm)
shapiro.test(AccessCPET_Corrected$age)



# mean values for paper

AccessCPET_Corrected %>% 
  select(c(
    gender,
    Mode,
    VO2_peak.actual,
    age,
    bmi,
    race
  )) %>% 
  tbl_summary(    
    statistic = list(
    all_categorical() ~ "{n} / {N} ({p}%)"),
  digits = all_continuous() ~ 2,) %>% 
  add_n()



# 1 comparing raw predicted values

#Making a dataset without the measured values
Access_Corrected_Tidy_FORanalaysis <- Access_Percent_predicted_tidy_Corrected %>% 
  filter(
    Equation != "Measured"
  )

Access_Percent_predicted_Corrected %>% 
  select(-(Subject_ID))%>% 
  tbl_summary(    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,) %>% 
  add_n()

Access_Corrected_Tidy_FORanalaysis %>% 
  group_by(Equation,Clinical_Interpretation ) %>% 
  summarise(count = n())

Access_Interpertation_wide_Corrected %>% 
  select(
    FRIEND,
    Wasserman,
    Hansen,
    Jones,
    Bruce,
    Neder
  ) %>% 
  tbl_summary(    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,) %>% 
  add_n()


Access_Corrected_Tidy_FORanalaysis$Equation <- factor(Access_Corrected_Tidy_FORanalaysis$Equation)

# non-parametric test for repeated measures (Friedman)

friedman.test(Predicted ~ Equation | Subject_ID, data = Access_Corrected_Tidy_FORanalaysis)
friedman_effsize(Predicted ~ Equation | Subject_ID, data = Access_Corrected_Tidy_FORanalaysis)




conover <- frdAllPairsExactTest(y = Access_Corrected_Tidy_FORanalaysis$Predicted,
                                groups = Access_Corrected_Tidy_FORanalaysis$Equation,
                                blocks = Access_Corrected_Tidy_FORanalaysis$Subject_ID,
                                p.adjust.methods = "bonferroni")
conover


# Calculating Cohen's Kappa for each pair


# pairwise analysis 
# need to use wide format for this



Access_equations <- colnames(Access_Interpertation_wide_Corrected)[-1]  #  first column is Subject_ID

for (i in 1:(length(Access_equations)-1)){
  for (j in (i+1):length(Access_equations)){
    
    #creating the pair name ( F vs W)
    pair_name <- paste(Access_equations[i], Access_equations[j], sep = "_")
    
    #creating a new variable for the pair
    
    Access_Interpertation_wide_Corrected <- Access_Interpertation_wide_Corrected %>% 
      mutate(!!pair_name := factor(case_when(
        .data[[Access_equations[i]]] == 0 & .data[[Access_equations[j]]] == 0 ~ 0, # both normal 
        .data[[Access_equations[i]]] == 1 & .data[[Access_equations[j]]] == 1 ~ 0, # both abnormal 
        .data[[Access_equations[i]]] == 1 & .data[[Access_equations[j]]] == 0 ~ -1, # reclassified as normal
        .data[[Access_equations[i]]] == 0 & .data[[Access_equations[j]]] == 1 ~ 1, # reclassified as abnormal
        TRUE ~ NA_real_))
      )
  }
}



#Kappa analysis

# Initialize an empty matrix to store kappa values
kappa_matrix <- matrix(NA, nrow = length(Access_equations), ncol = length(Access_equations), dimnames = list(Access_equations, Access_equations))



# Loop through each pair of equations and calculate Kappa


for (i in 1:(length(Access_equations)-1)) {
  for (j in (i+1):length(Access_equations)) {
    
    eq1 <- Access_Interpertation_wide_Corrected[[Access_equations[i]]]
    eq2 <- Access_Interpertation_wide_Corrected[[Access_equations[j]]]
    
    Access_kappa_results <- kappa2(cbind(eq1, eq2))
    cat("Kappa for", Access_equations[i], "vs", Access_equations[j], ":\n")
    
    print(Access_kappa_results)
    cat("\n")
    
    kappa_matrix[i, j ] <- round(Access_kappa_results$value, digits = 2)
    kappa_matrix[j, i ] <- round(Access_kappa_results$value, digits = 2)
    
    
  }
}




#Plotting heat map

ggplot(melt(kappa_matrix), aes(x = Var1, y = Var2, fill = value))+
  geom_tile(color = "black",
            lwd = 0.5,
            linetype = 1) +
  scale_fill_gradientn(colors = c("red", "white", "green"),
                       values = scales::rescale(c(0, 0.5, 1)),
                       limits = c(0, 1),
                       name = "Kappa",
                       breaks = c(0, 0.5, 1),
                       labels = c("0", "0.5", "1")) +
  geom_text(aes(label =value), color = "black") +
  labs(
    title = "Pairwise Cohen's Kappa Among Equations",
    fill = "Kappa",
    x = "",
    y = "") +
  theme_minimal() +
  guides(fill = guide_colourbar(
    barwidth = 0.5,                            
    barheight = 20))



#### Trying to understand what drives the differences:  ------------------------

# goal here is to see what is different 
# all possible pairwise comparisons

# 0 = no change
# -1 = changed to normal
# 1 = changed to abnormal

Classifications_Corrected <- Access_Interpertation_wide_Corrected %>% 
  select(Subject_ID, FRIEND, Wasserman, Hansen, Bruce, Jones, Neder) %>% 
  mutate(
    FvW = case_when(
      FRIEND == "1" & Wasserman == "1" ~ 0,
      FRIEND == "0" & Wasserman == "0" ~ 0,
      FRIEND == "1" & Wasserman == "0" ~ -1,
      FRIEND == "0" & Wasserman == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    FvH = case_when(
      FRIEND == "1" & Hansen == "1" ~ 0,
      FRIEND == "0" & Hansen == "0" ~ 0,
      FRIEND == "1" & Hansen == "0" ~ -1,
      FRIEND == "0" & Hansen == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    FvB = case_when(
      FRIEND == "1" & Bruce == "1" ~ 0,
      FRIEND == "0" & Bruce == "0" ~ 0,
      FRIEND == "1" & Bruce == "0" ~ -1,
      FRIEND == "0" & Bruce == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    FvJ = case_when(
      FRIEND == "1" & Jones == "1" ~ 0,
      FRIEND == "0" & Jones == "0" ~ 0,
      FRIEND == "1" & Jones == "0" ~ -1,
      FRIEND == "0" & Jones == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    FvN = case_when(
      FRIEND == "1" & Neder == "1" ~ 0,
      FRIEND == "0" & Neder == "0" ~ 0,
      FRIEND == "1" & Neder == "0" ~ -1,
      FRIEND == "0" & Neder == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    WvH = case_when(
      Wasserman == "1" & Hansen == "1" ~ 0,
      Wasserman == "0" & Hansen == "0" ~ 0,
      Wasserman == "1" & Hansen == "0" ~ -1,
      Wasserman == "0" & Hansen == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    WvB = case_when(
      Wasserman == "1" & Bruce == "1" ~ 0,
      Wasserman == "0" & Bruce == "0" ~ 0,
      Wasserman == "1" & Bruce == "0" ~ -1,
      Wasserman == "0" & Bruce == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    WvJ = case_when(
      Wasserman == "1" & Jones == "1" ~ 0,
      Wasserman == "0" & Jones == "0" ~ 0,
      Wasserman == "1" & Jones == "0" ~ -1,
      Wasserman == "0" & Jones == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    WvN = case_when(
      Wasserman == "1" & Neder == "1" ~ 0,
      Wasserman == "0" & Neder == "0" ~ 0,
      Wasserman == "1" & Neder == "0" ~ -1,
      Wasserman == "0" & Neder == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    HvB = case_when(
      Hansen == "1" & Bruce == "1" ~ 0,
      Hansen == "0" & Bruce == "0" ~ 0,
      Hansen == "1" & Bruce == "0" ~ -1,
      Hansen == "0" & Bruce == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    HvJ = case_when(
      Hansen == "1" & Jones == "1" ~ 0,
      Hansen == "0" & Jones == "0" ~ 0,
      Hansen == "1" & Jones == "0" ~ -1,
      Hansen == "0" & Jones == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    HvN = case_when(
      Hansen == "1" & Neder == "1" ~ 0,
      Hansen == "0" & Neder == "0" ~ 0,
      Hansen == "1" & Neder == "0" ~ -1,
      Hansen == "0" & Neder == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    BvJ = case_when(
      Bruce == "1" & Jones == "1" ~ 0,
      Bruce == "0" & Jones == "0" ~ 0,
      Bruce == "1" & Jones == "0" ~ -1,
      Bruce == "0" & Jones == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    BvN = case_when(
      Bruce == "1" & Neder == "1" ~ 0,
      Bruce == "0" & Neder == "0" ~ 0,
      Bruce == "1" & Neder == "0" ~ -1,
      Bruce == "0" & Neder == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    
    JvN = case_when(
      Jones == "1" & Neder == "1" ~ 0,
      Jones == "0" & Neder == "0" ~ 0,
      Jones == "1" & Neder == "0" ~ -1,
      Jones == "0" & Neder == "1" ~ 1,
      TRUE ~ NA_real_
    )
  )


columns_check <- c("FvW", "FvH", "FvB", "FvJ", "FvN", "WvH", "WvB", "WvJ", "WvN", "HvB", "HvJ", "HvN", "BvJ",
                   "BvN", "JvN")

Classifications_Corrected <- Classifications_Corrected %>% 
  mutate(
    count_NoChange = rowSums(Classifications_Corrected[,columns_check] == 0, na.rm = TRUE),
    count_Reduced = rowSums(Classifications_Corrected[,columns_check] == 1, na.rm = TRUE),
    count_Normal = rowSums(Classifications_Corrected[,columns_check] == -1, na.rm = TRUE)
  )




Classifications_Corrected %>%
  count(count_NoChange == 15) 



#comparing (age + sex + weight + height) between two  groups


# 0) No change
# 1) has some changes

Classifications_Corrected <- Classifications_Corrected %>% 
  mutate(
    Status = factor(case_when(
      count_NoChange == 15 ~ 0,
      count_NoChange != 15 ~ 1
    ))
  )

summary(Classifications_Corrected$Status)
# 0   1 
# 143 162 

#making factors
Classifications_Corrected <- Classifications_Corrected %>% 
  mutate(across(-c("Subject_ID", "count_NoChange", "count_Reduced", "count_Normal"), as.factor))

Classifications_Corrected %>% 
tbl_summary(    
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"),
  digits = all_continuous() ~ 2,) %>% 
  add_n()


Classifications_Corrected <- 
  merge(
    AccessCPET_Corrected[,c("Subject_ID", "gender",  "Mode" , "race" ,"age", "weight_kg", "height_cm", "bmi", "VO2_peak.actual", "FRIEND_Predicted", "Wasserman_Predicted",
                  "Hansen_Predicted", "Bruce_Predicted", "Jones_Predicted", "Neder_Predicted")],
    Classifications_Corrected,
    by = "Subject_ID"
  )

AccessCPET_Corrected <- 
  merge(
    AccessCPET_Corrected,
    Classifications_Corrected[,c("Subject_ID", "Status")],
    by = "Subject_ID"
  )


# mean values based on grouping  (no change vs change) for paper:
Classifications_Corrected %>% 
  tbl_summary(    
    by = Status,
    statistic = list(
      all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,) %>% 
  add_n()


#plots:
(Classifications_Corrected %>% 
    ggplot() +
    geom_violin(aes(x = Status, y = age, color = Status))) +
  (Classifications_Corrected %>%   
     ggplot() +
     geom_violin(aes(x = Status, y = weight_kg, color = Status))) +
  (Classifications_Corrected %>% 
     ggplot() +
     geom_violin(aes(x = Status, y = height_cm, color = Status))) +
  (Classifications_Corrected %>% 
     ggplot() +
     geom_violin(aes(x = Status, y = bmi, color = Status))) +
  plot_layout(guides = "collect")


Classifications_Corrected %>% 
  ggplot() +
  geom_bar(aes(x = gender, fill = Status), position = position_dodge())

Classifications_Corrected %>% 
  ggplot() +
  geom_bar(aes(x = Mode, fill = Status), position = position_dodge())


# Mannwhitneyfor height, weight, and age between two 
# looping it in 

Anaylsis_Variables <- c("age", "weight_kg", "height_cm", "bmi", "VO2_peak.actual","FRIEND_Predicted", "Wasserman_Predicted", "Hansen_Predicted","Bruce_Predicted","Jones_Predicted","Neder_Predicted")

# Assuming your data frame my_data has 'group', 'age', 'height', and 'weight' 
# Function to perform Kruskal-Wallis and then Dunn's test 
perform_tests <- function(Anaylsis_Variables, data) { 
  
  # Perform Kruskal-Wallis Test
  MW_test <- wilcox.test(as.formula(paste(Anaylsis_Variables, "~ Status")), data = Classifications_Corrected) 
  MW_effectsize <- wilcox_effsize(as.formula(paste(Anaylsis_Variables, "~ Status")), data = Classifications_Corrected) 
  
  return(list(Wilcox = MW_test, wilcox_effect = MW_effectsize)) }




# Apply the function to each variable and collect results
results_wilcox <- lapply(Anaylsis_Variables, perform_tests, data = Classifications_Corrected) 
# Name the list elements based on variables for easier identification 
names(results_wilcox) <- Anaylsis_Variables

results_wilcox


chisq.test(Classifications_Corrected$gender, Classifications_Corrected$Status)
cramers_v(Classifications_Corrected$gender, Classifications_Corrected$Status)

chisq.test(Classifications_Corrected$race, Classifications_Corrected$Status)
cramers_v(Classifications_Corrected$race, Classifications_Corrected$Status)

chisq.test(Classifications_Corrected$Mode, Classifications_Corrected$Status)
cramers_v(Classifications_Corrected$Mode, Classifications_Corrected$Status)


#specif numbers for paper.
# Looking at effect of sex with WvsB

Classifications_Corrected %>% 
  group_by(gender, WvB) %>% 
  summarise(Count = n())

Access_Corrected_Tidy_FORanalaysis %>% 
  filter(
    Equation == "Wasserman" | Equation == "Bruce"
  ) %>% 
  group_by(gender, Equation) %>% 
  summarise(mean1 = mean(Percent.Predicted))


  merge(
    Access_Percent_predicted_Corrected,
    AccessCPET_Corrected[,c("Subject_ID", "gender")],
    by = "Subject_ID"
  ) %>% 
  group_by(gender) %>% 
  summarise(meanW = mean(Wasserman_Predicted), meanB = mean(Bruce_Predicted, na.rm = TRUE))

  AccessCPET %>% 
    group_by(gender) %>% 
    summarise(mean1 = mean(age), mean2 = mean(weight_kg), mean3 = mean(height_cm))
  

  
  AccessCPET %>% 
    summarise(mean1 = mean(age), mean2 = mean(weight_kg), mean3 = mean(height_cm))
  
  
# seeing if percent predicted is different between the groups
Access_Corrected_Tidy_FORanalaysis <- 
  merge(
    Access_Corrected_Tidy_FORanalaysis,
    Classifications_Corrected[,c("Subject_ID", "Status")],
    by = "Subject_ID"
  )
  
wilcox.test(Predicted ~ Status, data = Access_Corrected_Tidy_FORanalaysis) 
wilcox_effsize(Predicted ~ Status, data = Access_Corrected_Tidy_FORanalaysis) 

# Spiderweb plot/analysis ----------------------------------------------------------

#need to make groups


quantile(Access_Corrected_Tidy_FORanalaysis$bmi)
#       0%      25%      50%      75%     100% 
# 16.72362 28.17020 31.00780 34.90000 46.85737 

quantile(Access_Corrected_Tidy_FORanalaysis$age)
# 0%  25%  50%  75% 100% 
# 24   36   44   51   67 


Access_Corrected_Tidy_FORanalaysis <- Access_Corrected_Tidy_FORanalaysis %>% 
  mutate(
    Spider_Grouping = factor(case_when(
      bmi <= 28 &  age <= 36 ~ "BMI 1 | Age 1",
      bmi <= 28 &  age > 36 & age <= 51 ~ "BMI 1 | Age 2",
      bmi <= 28 &  age > 51 ~ "BMI 1 | Age 3",
      
      bmi > 28 & bmi <= 35 &  age <= 36 ~ "BMI 2 | Age 1",
      bmi > 28 & bmi <= 35 & age > 36 & age <= 51 ~ "BMI 2 | Age 2",
      bmi > 28 & bmi <= 35 & age > 51 ~ "BMI 2 | Age 3",
      
      bmi > 35 & age <= 36 ~ "BMI 3 | Age 1",
      bmi > 35 & age > 36 & age <= 51 ~ "BMI 3 | Age 2",
      bmi > 35 & age > 51 ~ "BMI 3 | Age 3",
      
      TRUE ~ NA_character_)))

# Making separate spider plots for age and BMI 

SpiderPlot_Gender <- Access_Corrected_Tidy_FORanalaysis %>% 
  group_by(gender, Equation) %>% 
  summarise(Mean_Predicted = mean(Percent.Predicted), .groups = "drop")

# Differences in gender:
SpiderPlot_Gender %>% 
  mutate(gender = 
    case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female"
    )) %>% 
  ggplot(aes(x = gender, y = Mean_Predicted, group = Equation, color = Equation)) +
  geom_line(size = 1) + 
  geom_point() +
  labs(title = "Mean Percent Predicted Per Gender", y = "Percent Predicted", x = "") +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)

  ) +
  scale_fill_jco() +
  scale_color_jco()


#Difference in Mode

SpiderPlot_Mode <- Access_Corrected_Tidy_FORanalaysis %>% 
  group_by(Mode, Equation) %>% 
  summarise(Mean_Predicted = mean(Percent.Predicted), .groups = "drop")


SpiderPlot_Mode %>% 
  ggplot(aes(x = Mode, y = Mean_Predicted, group = Equation, color = Equation)) +
  geom_line(size = 1) + 
  geom_point() +
  labs(title = "Mean Percent Predicted Per Gender", y = "Percent Predicted", x = "") +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
    
  ) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_color_manual(values = natparks.pals("Triglav"))


# spider plot, gender vs BMI

SpiderPlot_GendervsBMI <- Access_Corrected_Tidy_FORanalaysis %>% 
  mutate(
    Spider_Grouping = factor(case_when(
      bmi <= 28 ~ "BMI: < 25%",
      
      bmi > 28 & bmi <= 35 ~ "BMI: 25-75%",

      bmi > 35 ~ "BMI: > 75%",

      TRUE ~ NA_character_)))

SpiderPlot_GendervsBMI <- SpiderPlot_GendervsBMI %>% 
  group_by(gender, Equation, Spider_Grouping) %>% 
  summarise(Mean_Predicted = mean(Percent.Predicted), .groups = "drop")

 
SpiderPlot_maleBMI <- SpiderPlot_GendervsBMI %>% 
  filter(gender == 1) %>% 
  ggplot() +
  geom_polygon(aes(x = Spider_Grouping, y = Mean_Predicted, color = Equation, group = Equation, fill = Equation), 
               linewidth = 1, alpha = 0.05) +
  coord_radar(clip = "off") +
  theme_radar() +
  
  
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = seq(50, 120, by = 10), limits = c(50, 120), expand = c(0,0)) +
  
  
  geom_text(data = data.frame(x = rep("BMI: 25-75%", 6), y = seq(60, 110, by = 10)), aes(x = x, y = y, label = y), 
            position = position_nudge(x = 0.5), angle = 0, vjust = 0.5, hjust = 0.5) +
  labs(title = "Male") +
  
  theme(
    plot.margin = unit(c(0,30,0,0), "pt"),
    axis.text.x = element_text(size = 12),
    axis.ticks = element_line(color = 2,
                              linewidth = 2),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"  
  ) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_color_manual(values = natparks.pals("Triglav"))

SpiderPlot_maleBMI


#BMI plot for females
SpiderPlot_femaleBMI <- SpiderPlot_GendervsBMI %>% 
  filter(gender == 2) %>% 
  ggplot() +
  geom_polygon(aes(x = Spider_Grouping, y = Mean_Predicted, color = Equation, group = Equation, fill = Equation), 
               linewidth = 1, alpha = 0.05) +
  coord_radar(clip = "off") +
  theme_radar() +
  labs(title = "Female") +
  
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = seq(50, 120, by = 10), limits = c(50, 120), expand = c(0,0)) +
  
  
  geom_text(data = data.frame(x = rep("BMI: 25-75%", 6), y = seq(60, 110, by = 10)), aes(x = x, y = y, label = y), 
            position = position_nudge(x = 0.5), angle = 0, vjust = 0.5, hjust = 0.5) +

  theme(
    plot.margin = unit(c(0,30,0,0), "pt"),
    axis.text.x = element_text(size = 12),
    axis.ticks = element_line(color = 2,
                              linewidth = 2),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"  
  ) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_color_manual(values = natparks.pals("Triglav"))


SpiderPlot_femaleBMI


# spider plot, gender vs Age

SpiderPlot_GendervAge <- Access_Corrected_Tidy_FORanalaysis %>% 
  mutate(
    Spider_Grouping = factor(case_when(
      age <= 36 ~ "Age: < 25%",
      age > 36 & age <= 51 ~ "Age: 25-75%",
      age > 51 ~ "Age: >75%",
      TRUE ~ NA_character_)))

SpiderPlot_GendervAge <- SpiderPlot_GendervAge %>% 
  group_by(gender, Equation, Spider_Grouping) %>% 
  summarise(Mean_Predicted = mean(Percent.Predicted), .groups = "drop")


SpiderPlot_maleAge <- SpiderPlot_GendervAge %>% 
  filter(gender == 1) %>% 
  ggplot() +
  geom_polygon(aes(x = Spider_Grouping, y = Mean_Predicted, color = Equation, group = Equation, fill = Equation), 
               linewidth = 1, alpha = 0.05) +
  coord_radar(clip = "off") +
  theme_radar() +
  labs(title = "Male") +
  
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = seq(50, 120, by = 10), limits = c(50, 120), expand = c(0,0)) +
  
  
  geom_text(data = data.frame(x = rep("Age: 25-75%", 6), y = seq(60, 110, by = 10)), aes(x = x, y = y, label = y), 
            position = position_nudge(x = 0.5), angle = 0, vjust = 0.5, hjust = 0.5) +
  
  theme(
    plot.margin = unit(c(0,30,0,0), "pt"),
    axis.text.x = element_text(size = 12),
    axis.ticks = element_line(color = 2,
                              linewidth = 2),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"  
  ) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_color_manual(values = natparks.pals("Triglav"))

SpiderPlot_maleAge

SpiderPlot_femaleAge <- SpiderPlot_GendervAge %>% 
  filter(gender == 2) %>% 
  ggplot() +
  geom_polygon(aes(x = Spider_Grouping, y = Mean_Predicted, color = Equation, group = Equation, fill = Equation), 
               linewidth = 1, alpha = 0.05) +
  coord_radar(clip = "off") +
  theme_radar() +
  
  
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(breaks = seq(50, 120, by = 10), limits = c(50, 120), expand = c(0,0)) +
  
  
  geom_text(data = data.frame(x = rep("Age: 25-75%", 6), y = seq(60, 110, by = 10)), aes(x = x, y = y, label = y), 
            position = position_nudge(x = 0.5), angle = 0, vjust = 0.5, hjust = 0.5) +
  labs(title = "Female") +
  theme(
    plot.margin = unit(c(0,30,0,0), "pt"),
    axis.text.x = element_text(size = 12),
    axis.ticks = element_line(color = 2,
                              linewidth = 2),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom"  
  ) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_color_manual(values = natparks.pals("Triglav"))


SpiderPlot_femaleAge



#running analysis on our spider groups

Access_Percent_predicted_tidy_Corrected <- Access_Percent_predicted_tidy_Corrected %>% 
  mutate(
    BMI_Grouping = factor(case_when(
      bmi <= 28 ~ "BMI: < 25%",
      
      bmi > 28 & bmi <= 35 ~ "BMI: 25-75%",
      
      bmi > 35 ~ "BMI: > 75%",
      
      TRUE ~ NA_character_)),
    
    Age_Grouping = factor(case_when(
      age <= 36 ~ "Age: < 25%",
      age > 36 & age <= 51 ~ "Age: 25-75%",
      age > 51 ~ "Age: >75%",
      TRUE ~ NA_character_)))


#1) BMI
# List of equations


# Initialize a list to store the results
kruskal_results <- list()

# Loop through each equation and perform the Kruskal-Wallis test
for (eq in Access_equations) {
  # Subset the data for the specific equation
  eq_data <- Access_Percent_predicted_tidy_Corrected %>% filter(Equation == eq)
  
  # Perform the Kruskal-Wallis test
  test_result <- kruskal.test(Percent.Predicted ~ BMI_Grouping, data = eq_data)
  
  # Store the result in the list
  kruskal_results[[eq]] <- test_result
}

# Display the results
kruskal_results


#2) AGE
# List of equations


# Initialize a list to store the results
kruskal_results <- list()

# Loop through each equation and perform the Kruskal-Wallis test
for (eq in Access_equations) {
  # Subset the data for the specific equation
  eq_data <- Access_Percent_predicted_tidy_Corrected %>% filter(Equation == eq)
  
  # Perform the Kruskal-Wallis test
  test_result <- kruskal.test(Percent.Predicted ~ Age_Grouping, data = eq_data)
  
  # Store the result in the list
  kruskal_results[[eq]] <- test_result
}

# Display the results
kruskal_results


#3) Gender
# List of equations


# Initialize a list to store the results
kruskal_results <- list()

# Loop through each equation and perform the Kruskal-Wallis test
for (eq in Access_equations) {
  # Subset the data for the specific equation
  eq_data <- Access_Percent_predicted_tidy_Corrected %>% filter(Equation == eq)
  
  # Perform the Kruskal-Wallis test
  test_result <- wilcox.test(Percent.Predicted ~ gender, data = eq_data)
  
  # Store the result in the list
  kruskal_results[[eq]] <- test_result
}

# Display the results
kruskal_results


# ALL PLOTS FOR PAPER 1.0 -----------------------------------------------------

# Figure 1 of paper: Violin plot of % Predicted VȮ2 across equations.
Violin_Pred <- Access_Percent_predicted_tidy_Corrected %>% 
  filter(Equation != "Measured") %>%
  ggplot(aes(x = Equation, y = Predicted)) +
  geom_violindot(aes(fill = Equation), binwidth = 100, dots_size = 0.1, color_dots ="black", fill_dots = "black") +
  # stat_summary(fun = mean, geom = "point", size = 3) +
  # stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2) +
  theme_classic() +
  scale_fill_jco() +
  labs(y = expression(Predicted~Peak~VO[2]~(ml%*%min^-1)), x = "") +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 16),
    axis.text.x =  element_blank(),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


# Figure 1 of paper: Violin plot 
Violin_Percent <- Access_Percent_predicted_tidy_Corrected %>% 
  filter(Equation != "Measured") %>% 
  ggplot(aes(x = Equation, y = Percent.Predicted)) +
  geom_violindot(aes(fill = Equation), binwidth = 5, dots_size = 0.1, color_dots ="black", fill_dots = "black") +
  theme_classic() +
  scale_fill_jco() +
  labs(y = "Percent Predicted", x = "") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )




#Figure 2 trying to look at them seperatly and grouping on word myself
emptyplot <- 
AgreementPlots %>%  
  ggplot(color = "white") +
  #adding line for agreements + shades    
  geom_vline(xintercept = 80, linetype = "dashed", color = "white", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "white", size = 0.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "white", size = 0.8, alpha = 0.5) +
  geom_rect(aes(xmin = 80, xmax = 160, ymin = 0, ymax = 80), fill = "white", alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = 80, ymin = 80, ymax = 160), fill = "white", alpha = 0.02) +
  
  geom_point(aes(x = Wasserman_Percent.Predicted, y = FRIEND_Percent.Predicted, color = BMI_cat, shape = Sex), alpha = 0.0) +
  
  #color and legends
  scale_color_manual(values = Agreement_colors) +
  scale_fill_manual(values = Agreement_colors) +
  #scale_shape_manual(values = c(16,17,16,17,16,17)) +
  coord_equal(xlim = c(0,160), ylim = c(0,160) ) +
  scale_x_continuous(breaks = seq(0,160,by = 20)) +
  scale_y_continuous(breaks = seq(0,160,by = 20)) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.grid = element_blank() )



P1 <- (F_vs_W + F_vs_H ) & theme(legend.position = "none") 
P2 <- (F_vs_B + F_vs_J) & theme(legend.position = "none") 
P3 <- (F_vs_N + emptyplot)  & theme(legend.position = "none")




(W_vs_H + W_vs_B) + plot_layout(guides = "collect") 
(w_vs_J + W_vs_N) + plot_layout(guides = "collect") 

(H_vs_B + H_vs_J) + plot_layout(guides = "collect") 
(H_vs_N + plot_spacer()) + plot_layout(guides = "collect") 

(B_vs_J + B_vs_N) + plot_layout(guides = "collect") 
(J_vs_N + plot_spacer()) + plot_layout(guides = "collect") 


#Figure one:
(Violin_Pred / Violin_Percent) 

# Figure 2, Spider Plot:
(SpiderPlot_maleBMI | SpiderPlot_femaleBMI) + plot_layout(guides = "collect") & theme(legend.position = "bottom")



# Figure 2: Spider Plot separated 
((SpiderPlot_maleBMI | SpiderPlot_femaleBMI) + plot_layout(tag_level = 'new')) / ((SpiderPlot_maleAge | SpiderPlot_femaleAge) + plot_layout(tag_level = 'new')) + plot_layout(guides = "collect") & theme(legend.position = "bottom") 


# Figure 4 of paper: agreement plots for each pair using the corrected dataset 

(F_vs_W + F_vs_H) / (F_vs_B + F_vs_J) / (F_vs_N + plot_spacer()) + plot_layout(guides = "collect") +  plot_annotation(title = "A")

(W_vs_H + W_vs_B) / ((w_vs_J) + W_vs_N) / (plot_spacer() + plot_spacer()) + plot_layout(guides = "collect") +  plot_annotation(title = "B")

(H_vs_B + H_vs_J) / (H_vs_N + plot_spacer()) / (plot_spacer() + plot_spacer()) + plot_layout(guides = "collect") +  plot_annotation(title = "C")

(B_vs_J + B_vs_N) / (plot_spacer() + plot_spacer()) / (plot_spacer() + plot_spacer()) + plot_layout(guides = "collect") + plot_annotation(title = "D")

(J_vs_N + plot_spacer()) / (plot_spacer() + plot_spacer()) / (plot_spacer() + plot_spacer()) + plot_layout(guides = "collect") +  plot_annotation(title = "E")


#Analaysis for paper only
#getting numbrs for changes in BMI in our populations

Access_Corrected_Tidy_FORanalaysis %>% 
  mutate(
    Spider_Grouping = factor(case_when(
      bmi <= 28 ~ "BMI 1",
      
      bmi > 28 & bmi <= 35 ~ "BMI 2",
      
      bmi > 35 ~ "BMI 3",
      
      TRUE ~ NA_character_))) %>% 
  group_by(Equation, Spider_Grouping) %>% 
  summarise(Mean_Predicted = mean(Percent.Predicted), .groups = "drop")






#-------------------------------------------------------------------------------
# Splitting up Tread vs Bike: Possible supplemental? (Plots + Analysis) ----------------------------------------------

# Need to split up the data set for supplement. 
# Using the uncorrected dataset

#used these because the orginal data used bike or tread or both (FRIEND)

#There is a correction factor for Hansen, but ignoring that since its just the 1.11 
Tread_equations <- c("FRIEND", "Bruce")
Bike_equations <-  c("FRIEND", "Wasserman", "Hansen", "Jones", "Neder")

#getting datasets

#### Tread--------------------------------------------------------------------


#Setting up Dataset for just Treadmill data

Tread_Dataset <- Access_Percent_predicted_tidy_Uncorrected %>% 
  filter(Equation %in% Tread_equations) %>% 
  filter(Mode == "Treadmill")


Tread_Dataset$Equation <-  factor(Tread_Dataset$Equation,
                                  levels =  c("FRIEND","Bruce")) 

Tread_Dataset$Subject_ID <- factor(Tread_Dataset$Subject_ID)

Tread_Dataset_wide <- Tread_Dataset %>% 
  pivot_wider(
    names_from = Equation, 
    values_from = c(Percent.Predicted, Predicted, Clinical_Interpretation)
  )


Tread_Interpretation_wide <- Tread_Dataset %>% 
  pivot_wider(id_cols = Subject_ID, 
              names_from = Equation, 
              values_from = Clinical_Interpretation)


#Plots for Tread

Tread_Dataset %>% 
  ggplot(aes(x = Equation, y = Predicted)) +
  geom_violindot(aes(fill = Equation), binwidth = 100, dots_size = 0.1, color_dots ="black", fill_dots = "black") +
  # stat_summary(fun = mean, geom = "point", size = 3) +
  # stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2) +
  theme_classic() +
  scale_fill_jco() +
  labs(y = expression(Peak~VO[2]~(ml%*%min^-1)), x = "") +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


#plot to look at differences in classification between each equation

#Tread
Tread_Dataset %>% 
  mutate(
    Clinical_Interpretation = case_when(
      Clinical_Interpretation == 0 ~ "Preserved Exercise Capacity",
      Clinical_Interpretation == 1 ~ "Reduced Exercise Capacity",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(Equation != "Measured") %>% 
  ggplot(aes(x = Equation, fill = Clinical_Interpretation)) +
  geom_bar(position = "fill") +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  theme_classic() +
  labs(y = "Proportion of Subjects", x = "", fill = "Clinical Interpretation") 




# plots for agreement:

# FvB:


AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Mode == "Treadmill") %>% 
  ggplot() +
  geom_point(aes(y = FRIEND_Percent.Predicted, x = Bruce_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "FRIEND to Bruce", y = "% Predicted: FRIEND ", 
       x = "% Predicted: Bruce", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


# All Analayis for Tread


shapiro.test(Tread_Dataset$Predicted)


#Runnig a wilcox test since its just two variables 
wilcox.test(Tread_Dataset_wide$Predicted_FRIEND, Tread_Dataset_wide$Predicted_Bruce, paired = TRUE)
wilcox_effsize(data = Tread_Dataset, Predicted ~ Equation, paired = TRUE)


#Interpretation comparison 

Access_equations_Tread <- colnames(Tread_Interpretation_wide)[-1]  #  first column is Subject_ID

#Pairwise comparison: code below will create a new variable for each pair of equations and show change per subject

for (i in 1:(length(Access_equations_Tread)-1)){
  for (j in (i+1):length(Access_equations_Tread)){
    
    #creating the pair name ( F vs W)
    pair_name <- paste(Access_equations_Tread[i], Access_equations_Tread[j], sep = "_")
    
    #creating a new variable for the pair
    
    Tread_Interpretation_wide <- Tread_Interpretation_wide %>% 
      mutate(!!pair_name := factor(case_when(
        .data[[Access_equations_Tread[i]]] == 0 & .data[[Access_equations_Tread[j]]] == 0 ~ 0, # both normal 
        .data[[Access_equations_Tread[i]]] == 1 & .data[[Access_equations_Tread[j]]] == 1 ~ 0, # both abnormal 
        .data[[Access_equations_Tread[i]]] == 1 & .data[[Access_equations_Tread[j]]] == 0 ~ -1, # reclassified as normal
        .data[[Access_equations_Tread[i]]] == 0 & .data[[Access_equations_Tread[j]]] == 1 ~ 1, # reclassified as abnormal
        TRUE ~ NA_real_))
      )
  }
}

#Shows Percent change:
# 0 = no change
# -1 = # reclassified as normal
# 1 = # reclassified as adnormal
Tread_Interpretation_wide %>% 
  select(-(Subject_ID))%>% 
  tbl_summary(    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,) %>% 
  add_n()

#Kappa analysis for tread: Agreement between the two

# Initialize an empty matrix to store kappa values
kappa_matrix <- matrix(NA, nrow = length(Access_equations_Tread), ncol = length(Access_equations_Tread), dimnames = list(Access_equations_Tread, Access_equations_Tread))



# Loop through each pair of equations and calculate Kappa


for (i in 1:(length(Access_equations_Tread)-1)) {
  for (j in (i+1):length(Access_equations_Tread)) {
    
    eq1 <- Access_Interpertation_wide_Uncorrected[[Access_equations_Tread[i]]]
    eq2 <- Access_Interpertation_wide_Uncorrected[[Access_equations_Tread[j]]]
    
    Access_kappa_results <- kappa2(cbind(eq1, eq2))
    cat("Kappa for", Access_equations_Tread[i], "vs", Access_equations_Tread[j], ":\n")
    
    print(Access_kappa_results)
    cat("\n")
    
    kappa_matrix[i, j ] <- round(Access_kappa_results$value, digits = 2)
    kappa_matrix[j, i ] <- round(Access_kappa_results$value, digits = 2)
    
    
  }
}

# table(Tread_Dataset_wide$FRIEND, Tread_Dataset_wide$Wasserman)
# table(Tread_Dataset_wide$FRIEND, Tread_Dataset_wide$Bruce)
# table(Tread_Dataset_wide$Wasserman, Tread_Dataset_wide$Bruce)

#Assesment of what is driving the difference for just Tread

#Shows Percent change:
# 0 = no change
# -1 = # reclassified as normal
# 1 = # reclassified as adnormal

Classifications_Tread <- Tread_Interpretation_wide %>% 
  select(-FRIEND_Bruce) %>% 
  mutate(
    FvB = case_when(
      FRIEND == "1" & Bruce == "1" ~ 0,
      FRIEND == "0" & Bruce == "0" ~ 0,
      FRIEND == "1" & Bruce == "0" ~ -1,
      FRIEND == "0" & Bruce == "1" ~ 1,
      TRUE ~ NA_real_
    )) %>% 
  mutate(
    Status = case_when(
      FvB == 0 ~ 0,
      TRUE ~ 1
    )
  )


Classifications_Tread <- 
  merge(
    AccessCPET_Uncorrected[,c("Subject_ID", "gender",  "Mode" , "race" ,"age", "weight_kg", "height_cm", "bmi", "FRIEND_Predicted", "Bruce_Predicted")],
    Classifications_Tread,
    by = "Subject_ID"
  )


#plots:
(Classifications_Tread %>% 
    ggplot() +
    geom_violin(aes(x = Status, y = age, color = Status))) +
  (Classifications_Tread %>%   
     ggplot() +
     geom_violin(aes(x = Status, y = weight_kg, color = Status))) +
  (Classifications_Tread %>% 
     ggplot() +
     geom_violin(aes(x = Status, y = height_cm, color = Status))) +
  (Classifications_Tread %>% 
     ggplot() +
     geom_violin(aes(x = Status, y = bmi, color = Status))) +
  plot_layout(guides = "collect")


Classifications_Tread %>% 
  ggplot() +
  geom_bar(aes(x = gender, fill = Status), position = position_dodge())


# Mannwhitneyfor height, weight, and age between two 
# looping it in 

Anaylsis_Variables <- c("age", "weight_kg", "height_cm")


# Function to perform Kruskal-Wallis and then Dunn's test 
perform_tests <- function(Anaylsis_Variables, data) { 
  
  # Perform Kruskal-Wallis Test
  MW_test <- wilcox.test(as.formula(paste(Anaylsis_Variables, "~ Status")), data = Classifications_Tread) 
  MW_effectsize <- wilcox_effsize(as.formula(paste(Anaylsis_Variables, "~ Status")), data = Classifications_Tread) 
  
  return(list(Wilcox = MW_test, wilcox_effect = MW_effectsize)) }


# Check if significant to proceed with Dunn's test 

# Apply the function to each variable and collect results
results_wilcox <- lapply(Anaylsis_Variables, perform_tests, data = Classifications_Tread) 
# Name the list elements based on variables for easier identification 
names(results_wilcox) <- Anaylsis_Variables

results_wilcox


chisq.test(Classifications_Tread$gender, Classifications_Tread$Status)
chisq.test(Classifications_Tread$race, Classifications_Tread$Status)


Classifications_Tread %>% 
  select(-(Subject_ID))%>% 
  tbl_summary(    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,) %>% 
  add_n()

#### Bike-------------------------------------------------------------------------


#Getting dataset for Bike
Bike_Dataset <- Access_Percent_predicted_tidy_Uncorrected %>% 
  filter(Equation %in% Bike_equations) %>% 
  filter(Mode == "Bike")

Bike_Dataset$Equation <-  factor(Bike_Dataset$Equation,
                                 levels =  c("FRIEND", "Wasserman", "Hansen", "Jones", "Neder"))

Bike_Dataset$Subject_ID <- factor(Bike_Dataset$Subject_ID)

Bike_Dataset_wide <- Bike_Dataset %>% 
  pivot_wider(
    names_from = Equation, 
    values_from = c(Percent.Predicted, Predicted, Clinical_Interpretation)
  )


Bike_Interpretation_wide <- Bike_Dataset %>% 
  pivot_wider(id_cols = Subject_ID, 
              names_from = Equation, 
              values_from = Clinical_Interpretation)



#Plot for Bike
Bike_Dataset %>% 
  ggplot(aes(x = Equation, y = Predicted)) +
  geom_violindot(aes(fill = Equation), binwidth = 100, dots_size = 0.1, color_dots ="black", fill_dots = "black") +
  # stat_summary(fun = mean, geom = "point", size = 3) +
  # stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = 0.2) +
  theme_classic() +
  scale_fill_jco() +
  labs(y = expression(Peak~VO[2]~(ml%*%min^-1)), x = "") +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

#Bike
Bike_Dataset %>% 
  mutate(
    Clinical_Interpretation = case_when(
      Clinical_Interpretation == 0 ~ "Preserved Exercise Capacity",
      Clinical_Interpretation == 1 ~ "Reduced Exercise Capacity",
      TRUE ~ NA_character_
    )
  ) %>% 
  filter(Equation != "Measured") %>% 
  ggplot(aes(x = Equation, fill = Clinical_Interpretation)) +
  geom_bar(position = "fill") +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  theme_classic() +
  labs(y = "Proportion of Subjects", x = "", fill = "Clinical Interpretation") 


# Agreement plots for BIKE

# "FvW, FvH, FvJ, FvN, WvH, WvJ, WvN, HvJ, HvN, JvN"

# FvW:

AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Mode == "Bike") %>% 
  ggplot() +
  geom_point(aes(y = FRIEND_Percent.Predicted, x = Wasserman_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "FRIEND to Wasserman", y = "% Predicted: FRIEND ", 
       x = "% Predicted: Wasserman", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

------------------------
  # FvH:
  
  AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )) %>%
  filter(Mode == "Bike") %>% 
  ggplot() +
  geom_point(aes(y = FRIEND_Percent.Predicted, x = Hansen_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "FRIEND to HANSEN", y = "% Predicted: FRIEND ", 
       x = "% Predicted: Hansen", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )



# FvJ:


AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Mode == "Bike") %>% 
  ggplot() +
  geom_point(aes(y = FRIEND_Percent.Predicted, x = Jones_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "FRIEND to Jones", y = "% Predicted: FRIEND ", 
       x = "% Predicted: Jones", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )



# FvN:


AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Mode == "Bike") %>% 
  ggplot() +
  geom_point(aes(y = FRIEND_Percent.Predicted, x = Neder_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "FRIEND to Neder", y = "% Predicted: FRIEND ", 
       x = "% Predicted: Neder", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

#WvH

AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Mode == "Bike") %>% 
  ggplot() +
  geom_point(aes(x = Hansen_Percent.Predicted, y = Wasserman_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "Wasserman to Hansen", y = "% Predicted: Wasserman ", 
       x = "% Predicted: Hansen", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

#WvJ

AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Mode == "Bike") %>% 
  ggplot() +
  geom_point(aes(x = Jones_Percent.Predicted, y = Wasserman_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "Wasserman to Jones", y = "% Predicted: Wasserman ", 
       x = "% Predicted: Jones", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )

#WvN

AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Mode == "Bike") %>% 
  ggplot() +
  geom_point(aes(x = Neder_Percent.Predicted, y = Wasserman_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "Wasserman to Neder", y = "% Predicted: Wasserman ", 
       x = "% Predicted: Neder", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


#HvJ

AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Mode == "Bike") %>% 
  ggplot() +
  geom_point(aes(x = Jones_Percent.Predicted, y = Hansen_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "Hansen to Jones", y = "% Predicted: Hansen ", 
       x = "% Predicted: Jones", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


#HvsN
AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Mode == "Bike") %>% 
  ggplot() +
  geom_point(aes(y = Hansen_Percent.Predicted, x = Neder_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "Hansen to Neder", y = "% Predicted: Neder ", 
       x = "% Predicted: Hansen", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )


#JvsN
AccessCPET_Uncorrected %>%  
  mutate(
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(Mode == "Bike") %>% 
  ggplot() +
  geom_point(aes(y = Jones_Percent.Predicted, x = Neder_Percent.Predicted, color = BMI_cat, shape = gender)) +
  geom_vline(xintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "black", size = 0.8, alpha = 0.5) +
  labs(title = "Jones to Neder", y = "% Predicted: Jones ", 
       x = "% Predicted: Neder", color = "BMI Category", shape = "Gender") +
  theme_classic() +
  scale_color_manual(values = natparks.pals("Triglav")) +
  scale_fill_manual(values = natparks.pals("Triglav")) +
  scale_x_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  scale_y_continuous(limits = (c(20,160)), breaks = seq(0,160,by = 20)) +
  geom_text(label = "+ , -", x = 40, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , +", x = 140, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "+ , +", x = 140, y = 140, color = "black", size = 4.5, alpha = 0.02) +
  geom_text(label = "- , -", x = 40, y = 20, color = "black", size = 4.5, alpha = 0.02) +
  theme(
    
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 14)
  )




# Analayis for Bike


friedman.test(Predicted ~ Equation | Subject_ID, data = Bike_Dataset)
friedman_effsize(Predicted ~ Equation | Subject_ID, data = Bike_Dataset)




conover_Bike <- frdAllPairsExactTest(y = Bike_Dataset$Predicted,
                                     groups = Bike_Dataset$Equation,
                                     blocks = Bike_Dataset$Subject_ID,
                                     p.adjust.methods = "bonferroni")
conover_Bike


Access_equations_Bike <- colnames(Bike_Interpretation_wide)[-1]  #  first column is Subject_ID

for (i in 1:(length(Access_equations_Bike)-1)){
  for (j in (i+1):length(Access_equations_Bike)){
    
    #creating the pair name ( F vs W)
    pair_name <- paste(Access_equations_Bike[i], Access_equations_Bike[j], sep = "_")
    
    #creating a new variable for the pair
    
    Bike_Interpretation_wide <- Bike_Interpretation_wide %>% 
      mutate(!!pair_name := factor(case_when(
        .data[[Access_equations_Bike[i]]] == 0 & .data[[Access_equations_Bike[j]]] == 0 ~ 0, # both normal 
        .data[[Access_equations_Bike[i]]] == 1 & .data[[Access_equations_Bike[j]]] == 1 ~ 0, # both abnormal 
        .data[[Access_equations_Bike[i]]] == 1 & .data[[Access_equations_Bike[j]]] == 0 ~ -1, # reclassified as normal
        .data[[Access_equations_Bike[i]]] == 0 & .data[[Access_equations_Bike[j]]] == 1 ~ 1, # reclassified as abnormal
        TRUE ~ NA_real_))
      )
  }
}

Bike_Interpretation_wide %>% 
  select(-(Subject_ID))%>% 
  tbl_summary(    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,) %>% 
  add_n()



#Kappa analysis


# Initialize an empty matrix to store kappa values
kappa_matrix <- matrix(NA, nrow = length(Access_equations_Bike), ncol = length(Access_equations_Bike), dimnames = list(Access_equations_Bike, Access_equations_Bike))



# Loop through each pair of equations and calculate Kappa


for (i in 1:(length(Access_equations_Bike)-1)) {
  for (j in (i+1):length(Access_equations_Bike)) {
    
    eq1 <- Access_Interpertation_wide_Uncorrected[[Access_equations_Bike[i]]]
    eq2 <- Access_Interpertation_wide_Uncorrected[[Access_equations_Bike[j]]]
    
    Access_kappa_results <- kappa2(cbind(eq1, eq2))
    cat("Kappa for", Access_equations_Bike[i], "vs", Access_equations_Bike[j], ":\n")
    
    print(Access_kappa_results)
    cat("\n")
    
    kappa_matrix[i, j ] <- round(Access_kappa_results$value, digits = 2)
    kappa_matrix[j, i ] <- round(Access_kappa_results$value, digits = 2)
    
    
  }
}

# table(Bike_Dataset_wide$FRIEND, Bike_Dataset_wide$Hansen)
# table(Bike_Dataset_wide$FRIEND, Bike_Dataset_wide$Jones)
# table(Bike_Dataset_wide$FRIEND, Bike_Dataset_wide$Neder)
# table(Bike_Dataset_wide$Hansen, Bike_Dataset_wide$Jones)
# table(Bike_Dataset_wide$Hansen, Bike_Dataset_wide$Neder)
# table(Bike_Dataset_wide$Jones, Bike_Dataset_wide$Neder)


#Assesment of what is driving the difference for just Bike

# "FvW, FvH, FvJ, FvN, WvH, WvJ, WvN, HvJ, HvN, JvN"

Classifications_Bike <- Bike_Interpretation_wide %>% 
  select(c(Subject_ID, FRIEND, Wasserman, Hansen, Jones, Neder)) %>% 
  mutate(
    FvW = case_when(
      FRIEND == "1" & Wasserman == "1" ~ 0,
      FRIEND == "0" & Wasserman == "0" ~ 0,
      FRIEND == "1" & Wasserman == "0" ~ -1,
      FRIEND == "0" & Wasserman == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    FvH = case_when(
      FRIEND == "1" & Hansen == "1" ~ 0,
      FRIEND == "0" & Hansen == "0" ~ 0,
      FRIEND == "1" & Hansen == "0" ~ -1,
      FRIEND == "0" & Hansen == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    FvJ = case_when(
      FRIEND == "1" & Jones == "1" ~ 0,
      FRIEND == "0" & Jones == "0" ~ 0,
      FRIEND == "1" & Jones == "0" ~ -1,
      FRIEND == "0" & Jones == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    FvN = case_when(
      FRIEND == "1" & Neder == "1" ~ 0,
      FRIEND == "0" & Neder == "0" ~ 0,
      FRIEND == "1" & Neder == "0" ~ -1,
      FRIEND == "0" & Neder == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    WvH = case_when(
      Wasserman == "1" & Hansen == "1" ~ 0,
      Wasserman == "0" & Hansen == "0" ~ 0,
      Wasserman == "1" & Hansen == "0" ~ -1,
      Wasserman == "0" & Hansen == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    WvJ = case_when(
      Wasserman == "1" & Jones == "1" ~ 0,
      Wasserman == "0" & Jones == "0" ~ 0,
      Wasserman == "1" & Jones == "0" ~ -1,
      Wasserman == "0" & Jones == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    WvN = case_when(
      Wasserman == "1" & Neder == "1" ~ 0,
      Wasserman == "0" & Neder == "0" ~ 0,
      Wasserman == "1" & Neder == "0" ~ -1,
      Wasserman == "0" & Neder == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    HvJ = case_when(
      Hansen == "1" & Jones == "1" ~ 0,
      Hansen == "0" & Jones == "0" ~ 0,
      Hansen == "1" & Jones == "0" ~ -1,
      Hansen == "0" & Jones == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    HvN = case_when(
      Hansen == "1" & Neder == "1" ~ 0,
      Hansen == "0" & Neder == "0" ~ 0,
      Hansen == "1" & Neder == "0" ~ -1,
      Hansen == "0" & Neder == "1" ~ 1,
      TRUE ~ NA_real_
    ),
    JvN = case_when(
      Jones == "1" & Neder == "1" ~ 0,
      Jones == "0" & Neder == "0" ~ 0,
      Jones == "1" & Neder == "0" ~ -1,
      Jones == "0" & Neder == "1" ~ 1,
      TRUE ~ NA_real_
    ))


Classifications_Bike <- 
  merge(
    AccessCPET_Uncorrected[,c("Subject_ID", "gender",  "Mode" , "race" ,"age", "weight_kg", "height_cm", "bmi", "FRIEND_Predicted", "Bruce_Predicted")],
    Classifications_Bike,
    by = "Subject_ID"
  )


columns_check <- c("FvW", "FvH", "FvJ", "FvN", "WvH", "WvJ", "WvN", "HvJ", "HvN", "JvN")

Classifications_Bike <- Classifications_Bike %>% 
  mutate(
    count_NoChange = rowSums(Classifications_Bike[,columns_check] == 0, na.rm = TRUE),
    count_Reduced = rowSums(Classifications_Bike[,columns_check] == -1, na.rm = TRUE),
    count_Normal = rowSums(Classifications_Bike[,columns_check] == 1, na.rm = TRUE)
  )

Classifications_Bike %>%
  count(count_NoChange == 10) 

#comparing (age + sex + weight + height) between two  groups


# 0) No change (Sum = 10)
# 1) has some changes

Classifications_Bike <- Classifications_Bike %>% 
  mutate(
    Status = factor(case_when(
      count_NoChange == 10 ~ 0,
      count_NoChange != 10 ~ 1
    ))
  )


Classifications_Bike %>% 
  ggplot() +
  geom_bar(aes(x = gender, fill = Status), position = position_dodge())




#plots:
(Classifications_Bike %>% 
    ggplot() +
    geom_violin(aes(x = Status, y = age, color = Status))) +
  (Classifications_Bike %>%   
     ggplot() +
     geom_violin(aes(x = Status, y = weight_kg, color = Status))) +
  (Classifications_Bike %>% 
     ggplot() +
     geom_violin(aes(x = Status, y = height_cm, color = Status))) +
  (Classifications_Bike %>% 
     ggplot() +
     geom_violin(aes(x = Status, y = bmi, color = Status))) +
  plot_layout(guides = "collect")


# Mannwhitneyfor height, weight, and age between two 
# looping it in 

Anaylsis_Variables <- c("age", "weight_kg", "height_cm")


# Function to perform Kruskal-Wallis and then Dunn's test 
perform_tests <- function(Anaylsis_Variables, data) { 
  
  # Perform Kruskal-Wallis Test
  MW_test <- wilcox.test(as.formula(paste(Anaylsis_Variables, "~ Status")), data = Classifications_Bike) 
  MW_effectsize <- wilcox_effsize(as.formula(paste(Anaylsis_Variables, "~ Status")), data = Classifications_Bike) 
  
  return(list(Wilcox = MW_test, wilcox_effect = MW_effectsize)) }


# Check if significant to proceed with Dunn's test 

# Apply the function to each variable and collect results
results_wilcox <- lapply(Anaylsis_Variables, perform_tests, data = Classifications_Bike) 
# Name the list elements based on variables for easier identification 
names(results_wilcox) <- Anaylsis_Variables

results_wilcox


chisq.test(Classifications_Bike$gender, Classifications_Bike$Status)
chisq.test(Classifications_Bike$race, Classifications_Bike$Status)


# Mean values
Classifications_Bike %>% 
  select(-(Subject_ID))%>% 
  tbl_summary(    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,) %>% 
  add_n()





##-------------------------------------------------------------------------------
# AUC Dataset -------------------------------------------------------------


#bringing in all SOB questions
Clincal_Demos <- read_csv("Clincal_Demos_2024-03-01.csv")
PDCEN_REDCAP <- read_csv("PDCEN_REDCAP.csv")

Clincal_Demos <- Clincal_Demos %>% 
  select(
    Subject_ID = WRIISCID,
    phq_sob
  )

Clincal_Demos <- Clincal_Demos %>% filter(!(duplicated(Subject_ID))) 

PDCEN_REDCAP <- PDCEN_REDCAP %>% 
  select(
    Subject_ID,
    phq_sob 
  )

PDCEN_REDCAP <- PDCEN_REDCAP %>% filter(!(duplicated(PDCEN_REDCAP))) 


AWARE_REDCAP <- 
  redcap_read_oneshot(
    redcap_uri = "https://varedcap.rcp.vaec.va.gov/redcap/api/",
    token = "7F2EF38183ED3B2F7ADA01A4A70248B3",
    raw_or_label_headers = "raw",
    fields = 'wriisc_id',
    forms = 'section_13_phq15'
  )

AWARE_REDCAP <- as_tibble(AWARE_REDCAP$data)

AWARE_REDCAP <- AWARE_REDCAP %>% 
  select(
    Subject_ID = wriisc_id,
    phq_sob = phq_j
  )

AWARE_REDCAP <- AWARE_REDCAP %>% 
  filter(!(duplicated(Subject_ID))) 


Clincal_Demos <-
  rows_patch(
    Clincal_Demos,
    PDCEN_REDCAP,
    by = "Subject_ID",
    unmatched = "ignore"
  )


Clincal_Demos <-
  rows_patch(
    Clincal_Demos,
    AWARE_REDCAP,
    by = "Subject_ID",
    unmatched = "ignore"
  )



Clincal_Demos <- Clincal_Demos %>% 
  filter(
    Subject_ID %in% AccessCPET$Subject_ID
  )

AccessCPET <- 
  merge(
    AccessCPET,
    Clincal_Demos,
    by = "Subject_ID",
    all.x = TRUE
  )

AccessCPET <- AccessCPET %>% 
  replace_with_na_all(~.x %in% c(-999, "NA", "missing", 999)) 

AccessCPET$phq_sob <- factor(AccessCPET$phq_sob)
# 0 = not bothered
# 1 = Bothered a little
# 2 = Bothered a lot


AccessCPET %>% 
  select(phq_sob) %>% 
  tbl_summary(    
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} / {N} ({p}%)"),
    digits = all_continuous() ~ 2,) %>% 
  add_n()



# New Equation ------------------------------------------------------------

#   
# 
# #making new dataset to make this easier to deal with
# 
# New_Equation <- AccessCPET %>% 
#   select(
#     Subject_ID,
#     Mode,
#     age,
#     gender,
#     weight_kg,
#     height_cm,
#     bmi,
#     VO2_peak.actual
#   )
# 
# 
# New_Equation_Bike <- New_Equation %>% 
#   filter(
#     Mode == "Bike")
# 
# New_Equation_Tread <- New_Equation %>% 
#   filter(
#     Mode == "Treadmill")
# 
# 
# 
# # `Making Equation: Multivariate: weight/height
# 
# # all
# Multivariate_weight_height <- 
#   lm(data = New_Equation, VO2_peak.actual ~ age + gender + Mode + height_cm + weight_kg)
# 
# summary(Multivariate_weight_height)
# parameters(Multivariate_weight_height)
# check_model(Multivariate_weight_height)
# model_performance(Multivariate_weight_height)
# 
# 
# 
# plot(parameters(Multivariate_weight_height))
# estimate_prediction(Multivariate_weight_height)
# 
# 
# # Just Bike
# Multivariate_weight_height.Bike <- 
#   lm(data = New_Equation_Bike, VO2_peak.actual ~ age + gender + height_cm + weight_kg)
# 
# summary(Multivariate_weight_height.Bike)
# parameters(Multivariate_weight_height.Bike)
# check_model(Multivariate_weight_height.Bike)
# model_performance(Multivariate_weight_height.Bike)
# 
# 
# 
# plot(parameters(Multivariate_weight_height.Bike))
# estimate_prediction(Multivariate_weight_height.Bike)
# 
# 
# # Just Tread
# Multivariate_weight_height.Tread <- 
#   lm(data = New_Equation_Tread, VO2_peak.actual ~ age + gender + height_cm + weight_kg)
# 
# summary(Multivariate_weight_height.Tread)
# parameters(Multivariate_weight_height.Tread)
# check_model(Multivariate_weight_height.Tread)
# model_performance(Multivariate_weight_height.Tread)
# 
# 
# 
# 
# # Case Example ------------------------------------------------------------
# 
# # selecting veterans to do a case review
# 
# #FRIEND to wasserman
# 
# # vets who went from normal to abnormal 
# # only selecting PDCEN folks
# 
# 
# # Pulling in all Redcap Data ---------------------------------------------------
# 
# PDCEN_REDCAP <- 
#   redcap_read_oneshot(
#     redcap_uri = "https://varedcap.rcp.vaec.va.gov/redcap/api/",
#     token = "32D4DD3A904549F2A755EDFFB19A53A9",
#     raw_or_label_headers = "raw",
#     events = "initial_contact_arm_1"
#   )
# 
# PDCEN_REDCAP <-  PDCEN_REDCAP$data
# PDCEN_REDCAP <- as_tibble(PDCEN_REDCAP)
# PDCEN_REDCAP <- clean_names(PDCEN_REDCAP)
# 
# 
# #write.csv(colnames(PDCEN_REDCAP), "Variables.csv")
# 
# 
# #coding back in race
# PDCEN_REDCAP <- PDCEN_REDCAP %>% 
#   mutate(
#     race = case_when(
#       demo_race_0 == 1 ~ "Native American",
#       demo_race_1 == 1 ~ "Asian",
#       demo_race_2 == 1 ~ "Black",
#       demo_race_3 == 1 ~ "Pacific Islander",
#       demo_race_4 == 1 ~ "White",
#       demo_race_5 == 1 ~ "Unknown",
#       demo_race_6 == 1 ~ "Other",
#       demo_race_0 == 0 & demo_race_1 == 0 & demo_race_2 == 0 & demo_race_3 == 0 & demo_race_4 == 0 & demo_race_5 == 0 & demo_race_6 == 0 ~ "Other",
#       TRUE ~ "NA"
#     )
#   )
# 
# 
# PDCEN_REDCAP <- PDCEN_REDCAP %>%  
#   mutate(pdcen_site = case_when(
#     pdcen_site == 1 ~ "Ann Arbor",
#     pdcen_site == 2 ~ "Baltimore",
#     pdcen_site == 3 ~ "Colorado",
#     pdcen_site == 4 ~ "Nashville",
#     pdcen_site == 5 ~ "New Jersey",
#     pdcen_site == 6 ~ "San Francisco",
#     pdcen_site == 7 ~ "Other",
#     TRUE ~ "None"
#   ))
# 
# 
# 
# PDCEN_REDCAP$pdcen_site <- factor(PDCEN_REDCAP$pdcen_site,
#                                   levels = c("Ann Arbor", "Baltimore", "Colorado", "Nashville", "New Jersey", "San Francisco", "Other", "None"))
# 
# 
# PDCEN_REDCAP <- PDCEN_REDCAP %>%  replace_with_na_all(~.x %in% c(-999, "NA", "missing")) 
# 
# 
# #  DEMO  -------------------------------------------------------
# 
# REDCAP_DEMO <- PDCEN_REDCAP %>%  
#   mutate(
#     Redcap_1 = "DEMO"
#   ) %>%  
#   select(
#     Redcap_1,
#     wriisc_id,
#     pdcen_site,
#     last_name,
#     first_name,
#     race,
#     pdcen_site,
#     ethnic_gli,
#     demo_age,
#     demo_gender,
#     mmrc_dyspnea
#   ) %>%  
#   rename(
#     Subject_ID = wriisc_id,
#     age = demo_age,
#     gender = demo_gender
#   )  %>% 
#   mutate(
#     mmrc_dyspnea = case_when(
#       mmrc_dyspnea == 1 ~ 0,
#       mmrc_dyspnea == 2 ~ 1,
#       mmrc_dyspnea == 3 ~ 2,
#       mmrc_dyspnea == 4 ~ 3,
#       mmrc_dyspnea == 5 ~ 4,
#       TRUE ~ NA_real_
#     ),
#     Redcap_5 = "Respiratory"
#   ) %>% 
#   select(
#     Redcap_5, everything()
#   ) %>% 
#   filter(!is.na(Subject_ID)) 
# 
# 
# 
# #getting and ID list
# PDCEN_IDS <-
#   REDCAP_DEMO$Subject_ID
# 
# PDCEN_IDS <- PDCEN_IDS[!(PDCEN_IDS %in% c("**TEST**", "***test2.0***"))]
# 
# 
# FriendvsWasserman_Normal <-  Classifications %>% 
#   filter(Subject_ID %in% PDCEN_IDS & FvW == 1)
# 
# FriendvsWasserman_Reduced <-  Classifications %>% 
#   filter(Subject_ID %in% PDCEN_IDS & FvW == -1)
# 
# #
# # IDs that need chart review:
# 
#   #Reduced to normal: 37751 38053 38280 38344
#   #Reduced to abnormal: 37331 37389 37462 37529 37584 37607 37657 37676 37842 37918 38168 38229 38244
# 
# 
# 


#getting Race information

AccessCPET <- 
  merge(
    AccessCPET,
    Redcap_PDCEN[,c("Subject_ID", "race_redcap" )],
    by = "Subject_ID",
    all.x = TRUE
  )

Pre_Demo <- read.csv("Clincal_Demo.csv")

Pre_Demo <- Pre_Demo %>% 
  rename(
    Subject_ID = WRIISCID
  )

AccessCPET <- 
  merge(
    AccessCPET,
    Pre_Demo[,c("Subject_ID", "race_ethnicity" )],
    by = "Subject_ID",
    all.x = TRUE
  )


AccessCPET <- AccessCPET %>% 
  mutate(
    Race_Combined = case_when(
      is.na(race_redcap) ~ race_ethnicity,
      TRUE ~ race_redcap
    )
  )


AccessCPET <- AccessCPET %>% 
  mutate(
    Race_Combined = case_when(
      is.na(Race_Combined) ~ race,
      TRUE~ Race_Combined))

AccessCPET <- AccessCPET %>% 
  mutate(
    Race_Combined = case_when(
      Race_Combined == "White" ~ "Caucasian",
      Race_Combined == "African-American" ~ "Black",
      Race_Combined == "Black or African-American" ~ "Black",
      Race_Combined == "American Indian or Alaskan tive" ~ "Native American",
      Race_Combined == "Mixed" ~ "Other",
      TRUE ~Race_Combined))


AccessCPET %>% 
  select(
    race,
    Race_Combined
  ) %>% 
  tbl_summary()

AccessCPET <- 
  merge(
    AccessCPET,
    AccessCPET_Corrected[,c("Subject_ID", "Status")],
    by = "Subject_ID"
  )

AccessCPET %>% 
  select(
    age,
    gender,
    bmi,
    weight_kg,
    height_cm,
    Mode,
    VO2_peak.actual,
    race,
    Race_Combined,
    Status
  ) %>% 
  tbl_summary(
    by = "Status"
  )


#######TABELS

#trying to use gt tables to get better looking tables

# Demographic table:

AccessCPET_Corrected %>% 
  select(
    Gender = gender, 
    Age = age, 
    BMI = bmi, 
    "Mode of Testing" = Mode, 
    Race = race,
    "Measured VO2" =
         VO2_peak.actual) %>%
  tbl_summary(
    statistic = all_continuous() ~ "{median} ({IQR})"
  )

AccessCPET_Corrected %>% 
  select(gender, age, bmi, Mode, race, 
         VO2_peak.actual, Status) %>% 
  tbl_summary(by = Status)


AccessCPET_Corrected %>% 
  select(gender, age, bmi, Mode, race, Status, ) %>% 
  tbl_summary(
    by = Status
  )



# table 4:
Access_Corrected_Tidy_FORanalaysis %>% 
  select(Equation, Predicted, Percent.Predicted, Clinical_Interpretation) %>% 
  mutate(
    Clinical_Interpretation = case_when(
      Clinical_Interpretation == "0" ~ "Normal Exercise Tolerance",
      Clinical_Interpretation == "1" ~ "Reduced Exercise Tolerance"
    )
  ) %>% 
  tbl_summary(
    by = Equation,
    label = list(
      Clinical_Interpretation = "Clinical Interpertation",
      Predicted = "Predicted VO<sub>2</sub>",
      Percent.Predicted = "Percent Predicted (%)"),
    digits = all_continuous() ~ 2
  ) 

