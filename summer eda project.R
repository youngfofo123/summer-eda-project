library(tidyverse)
hospital_ratings_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/health/eda_projects/hospitals.csv")
head(hospital_ratings_data)



#### Question: Does Facility Type affect the Rating Overall? 
### Hypothesis:  Government has the lowest average Rating Overall and Church has the highest.  --------------------------------------------------------


# Creating a new data new sheet, which is the average Rating.Overall for each Facility.Type 




new_FT_RO1 <- hospital_ratings_data %>%
  group_by(Facility.Type) %>%
  summarize( new_Rating = mean(Rating.Overall))




# Forming my geom_bar plot from this new data

new_FT_RO1 %>%
  ggplot(aes(x = Facility.Type, y = new_Rating, fill= Facility.Type)) +
  geom_col(width = 0.5 ) + theme(legend.position = "bottom")





## This graph shows that average Rating Overall for each Facility Type
## We see that the Government has the lowest average Rating Overall and Church has the highest. 




### Question: Will the population of each Facility.State affect the Procedure.Cost? 
###  Hypothesis: In states, with bigger population there will be higher cost of procedure.


# Creating a new column, that averages the total of cost of all procedures for every hospital. 




hospital_ratings_data <- hospital_ratings_data %>%
  mutate(all_cost = (`Procedure.Heart Attack.Cost`+ `Procedure.Heart Failure.Cost`+ Procedure.Pneumonia.Cost +
           `Procedure.Hip Knee.Cost`/4))

# Forming my new data sheet, which only groups the Facility.State and the Average Cost in each Facility.State. 

hospital_ratings_data1 <- hospital_ratings_data
  group_by(Facility.State) %>%
summarize(mean(all_cost))

# forming my new data sheet for my US Plot Map 


  
  
  data_5_USmap <- hospital_ratings_data1 %>%
  mutate("state" = Facility.State) %>%
  colnames(data_5_USmap)[2] <- "values"

# plotting US Map for the average cost for all procedure in each state. 

  
  
  plot_usmap(data = data_5_USmap)+
  scale_fill_continuous(
    low = "white", high = "red", name = "Avg.Cost", label = scales::comma
  ) + theme(legend.position = "right")

#plotting US Map for by population in 2019 for each state 


  
  colnames(newUS_Pop2019)[12] <- "values"
colnames(newUS_Pop2019)[1] <- "state"


#comparing the two data sets for each 


plot_usmap(data=newUS_Pop2019, values = "pop_2019")


## Results - As you see that the states with lowest population has the highest avergae cost procedures. 
## Our hypothesis is not true. 




  
  
  
  
  
  
# Question: Does Facility State dictate the Facility Type in each state? 
# Hypotheses:Facilities in the southern states will be predominately owned by the Government or Proprietary. 





state_by_type <- state.by %>%
  select(Facility.State:Facility.Type)

colnames(state_by_type)[1] <- "state" 
  colnames(state_by_type)[2] <- "values"
  
plot_usmap(data = state_by_type)

  





             

                                                           