library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)



#Part one of source data pulled from ACS 5 year estimate, broken down by census tract in each county 
df1 <- read.csv("AL_NY_Census.csv", stringsAsFactors = FALSE)  %>% 
  #removes all columns except geography data and estimated population with a disability
  select(GEO.display.label, HC02_EST_VC01)

#Part two of source data pulled from ACS 5 year estimate, broken down by census tract in each county
df2 <- read.csv("NC_END_CENSUS.csv", stringsAsFactors = FALSE) %>% 
  #removes all columns except geography data and estimated population with a disability
  select(GEO.display.label, HC02_EST_VC01)

#combines the above two data sets into one
uni_data <- rbind(df1, df2)

disability_by_state <- 
    uni_data %>% 
    filter(GEO.display.label != 'Geography') %>% 
    #breaks apart the Geo column into it's three components
    separate(GEO.display.label, c('tract', 'County', 'state'), sep = ',') %>% 
    #seperates the superfluous "Census Tract" string from the tract number
    separate(tract, c('drop', 'drop2', "tract"), sep = ' ') %>%
    #multiplies the tract number by 100 to make it compatable with the HTC formatting
    mutate(Tract10 = as.numeric(tract)*100) %>%
    select(-drop, -drop2, -tract) %>% 
    mutate(County = trimws(as.character(County))) %>% 
    rename(pop_w_disab = HC02_EST_VC01) %>% 
    #I realized later on the census tracts were not unique accross the nation, the following joins the reformatted tract number 
    #with county and state
    unite(Tract10, state, County, Tract10, sep = ",") %>% 
    arrange(Tract10) %>% 
    mutate(Tract10 = trimws(as.character(Tract10)))
    
    
#Source data taken from HTC 2020 website, that references the 2010 mail return rates from Census
HTC_by_state <- 
  read.csv("all_state_HTC.csv", stringsAsFactors = FALSE) %>% 
  select(State_name, County_name10, MRR20pctthreshold, TotPopACS17, Tract10) %>% 
  rename(Flag = MRR20pctthreshold) %>% 
  #Dona Ana County was labelled using different characters in the two data sets, this normalized them one spelling
  mutate(County_name10 = replace(County_name10, County_name10 == 'DoÃ±a Ana County', 'Doña Ana County')) %>% 
  #I realized later on the census tracts were not unique accross the nation, the following joins the tract number 
  #with county and state
  unite(Tract10, State_name, County_name10, Tract10, sep = ",") %>% 
  arrange(Tract10) %>% 
  mutate(Tract10 = trimws(as.character(Tract10)))

joined_data <- inner_join(HTC_by_state, disability_by_state, by = c("Tract10"))

HTC_Joined_Data <-
  joined_data %>% 
  #filters to show only the tracts which were listed as Hard to Count(%73 or lower mail response rate)
  filter(Flag == 1) %>%
  select(-Flag) 
  
HTC_summary_stats <-
  HTC_Joined_Data %>% 
  #Seperating the geographical data so I can group by state
  separate(Tract10, c('State', 'County', "Tract"), sep = ',') %>% 
  #sums the population with a disability that lives in HTC census tracts, within each state
  group_by(State) %>%
  summarise(HTC_W_Disabilty = sum(as.numeric(pop_w_disab))) %>% 
  mutate(State = trimws(as.character(State)))

#source data pulled from ACS 5 year estimate, the population of people with a disability in each state
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
  #Divides the population with a disability that lives in HTC tracts within each state by the total population with a disability 
  #within each state
  mutate(percentage = round(as.numeric(HTC_W_Disabilty) / as.numeric(Total_Pop_W_Disab) * 100, digits = 1))

write.csv(final_data, 'Final_Data.csv')
