# Loading Packages and Data
library(tidyverse)
library(usmap)
county_health_rankings <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/health/optum_projects/county_rankings.csv")

# Hypothesis - States with higher Unemployment ratings will have more Uninsured people resulting in
#large percentages of people wth poor, or fair health. 


## Deleting the US row
county_health_sel <- county_health_rankings[-1,]


## Selecting my variables
county_health_sel <- county_health_sel %>%
  select(`State Abbreviation`,`Uninsured raw value`,`Unemployment raw value`,`Poor or fair health raw value`)

##Averaging the varaibles values 
county_health_sel <- county_health_sel %>% 
  group_by(`State Abbreviation`) %>%
  mutate("new_unemployment" = mean(`Unemployment raw value`)) %>%
  mutate("new_uninsured" = mean(`Uninsured raw value`)) %>%
  mutate("new_poor_health" = mean(`Poor or fair health raw value`)) 

##Selecting my variables of new data set 
county_health_sel <- county_health_sel %>%
  select(`State Abbreviation`, new_unemployment, new_uninsured, new_poor_health) 


##Grouping variables by State Abb, and with the 3 variables 
county_health_sel <- county_health_sel %>%
group_by(`State Abbreviation`) %>%
  summarize(totaL_new_unemployment = mean(new_unemployment),totaL_new_uninsured = mean(new_uninsured),
            totaL_new_poor_health = mean(new_poor_health))


county_health_sel <- na.omit(county_health_sel)

## Covnerting the variables into percentages
county_health_sel <- county_health_sel %>%
  mutate(totaL_new_unemployment = totaL_new_unemployment * 100,totaL_new_uninsured = totaL_new_uninsured * 100, 
         totaL_new_poor_health = totaL_new_poor_health * 100)

county_health_sel <- county_health_sel %>%
  rename(values = totaL_new_unemployment, state = `State Abbreviation`)

plot_usmap(data = county_health_sel, color = "blue") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Unemployment Ratings (%)", label = scales::comma
  ) + theme(legend.position = "bottom")