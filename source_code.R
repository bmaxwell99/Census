library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)


#put your file path to the  into this function to filter it to the appropriate years of interest for this project

df1 <- read.csv("AL_NY_Census.csv", stringsAsFactors = FALSE)  %>% 
  select(GEO.display.label, HC02_EST_VC01)

df2 <- read.csv("NC_END_CENSUS.csv", stringsAsFactors = FALSE) %>% 
  select(GEO.display.label, HC02_EST_VC01)

uni_data <- rbind(df1, df2)

disability_by_state <- 
    uni_data %>% 
    filter(GEO.display.label != 'Geography') %>% 
    separate(GEO.display.label, c('tract', 'County', 'state'), sep = ',') %>% 
    separate(tract, c('drop', 'drop2', "tract"), sep = ' ') %>%
    mutate(Tract10 = as.numeric(tract)*100) %>% 
    select(-drop, -drop2, -tract) %>% 
    mutate(County = trimws(as.character(County))) %>% 
    rename(pop_w_disab = HC02_EST_VC01) %>% 
    unite(Tract10, state, County, Tract10, sep = ",") %>% 
    arrange(Tract10) %>% 
    mutate(Tract10 = trimws(as.character(Tract10)))
    
    

HTC_by_state <- 
  read.csv("all_state_HTC.csv", stringsAsFactors = FALSE) %>% 
  select(State_name, County_name10, MRR20pctthreshold, TotPopACS17, Tract10) %>% 
  rename(Flag = MRR20pctthreshold) %>% 
  mutate(County_name10 = replace(County_name10, County_name10 == 'DoÃ±a Ana County', 'Doña Ana County')) %>% 
  unite(Tract10, State_name, County_name10, Tract10, sep = ",") %>% 
  arrange(Tract10) %>% 
  mutate(Tract10 = trimws(as.character(Tract10)))

joined_data <- inner_join(HTC_by_state, disability_by_state, by = c("Tract10"))

HTC_Joined_Data <-
  joined_data %>% 
  filter(Flag == 1) %>%
  select(-Flag) 
  
HTC_summary_stats <-
  HTC_Joined_Data %>% 
  separate(Tract10, c('State', 'County', "Tract"), sep = ',') %>% 
  group_by(State) %>% 
  summarise(HTC_W_Disabilty = sum(as.numeric(pop_w_disab))) %>% 
  mutate(State = trimws(as.character(State)))

df3 <-
  read.csv("All_States_Total_Disab.csv", stringsAsFactors = FALSE) %>% 
  rename(Total_Pop_W_Disab = HC02_EST_VC01) %>% 
  rename(State = GEO.display.label) %>% 
  filter(State != 'Puerto Rico') %>% 
  filter(State != 'Geography') %>% 
  select(State, Total_Pop_W_Disab) %>% 
  mutate(State = trimws(as.character(State)))

final_data <- inner_join(df3, HTC_summary_stats, by = 'State') 

final_data <-
  final_data %>% 
  mutate(percentage = round(as.numeric(HTC_W_Disabilty) / as.numeric(Total_Pop_W_Disab) * 100, digits = 1))

write.csv(final_data, 'Final_Data.csv')
