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
    rename(pop_w_disab = HC02_EST_VC01) %>% 
    unite(Tract10, County, Tract10) %>% 
    arrange(Tract10)
    
lapply(disability_by_state[1], as.String())

HTC_by_state <- 
  read.csv("all_state_HTC.csv", stringsAsFactors = FALSE) %>% 
  select(State_name, County_name10, MRR20pctthreshold, TotPopACS17, Tract10) %>% 
  rename(Flag = MRR20pctthreshold) %>% 
  unite(Tract10, County_name10, Tract10) %>% 
  arrange(Tract10)


HTC_by_state %>% View()

disability_by_state[1]

disable_nrows<- nrow(disability_by_state)
htc_nrows<- nrow(HTC_by_state)


joined_data <- right_join(HTC_by_state, disability_by_state, by = c("Tract10"))
joined_data %>% View()
