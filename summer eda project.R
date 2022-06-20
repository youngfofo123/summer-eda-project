library(tidyverse)
hospital_ratings_data <- read_csv("http://www.stat.cmu.edu/cmsac/sure/2022/materials/data/health/eda_projects/hospitals.csv")
head(hospital_ratings_data)



# Hypotheses Trial --------------------------------------------------------

new_FT_RO1 <- hospital_ratings_data %>%
  group_by(Facility.Type) %>%
  summarize( new_Rating = mean(Rating.Overall))
            


# Mutating  ---------------------------------------------------------------


hospital_ratings_data %>%
  ggplot(aes(x = Facility.Type, y = stat(Rating.Overall)) +
  geom_bar()+
  theme_bw()

# Facility Type vs. Rating.Overall.  -----------------------------------------------------------------
new_FT_RO1 %>%
  ggplot(aes(x = Facility.Type, y = new_Rating, fill= Facility.Type)) +
  geom_col(width = 0.5 ) + theme(legend.position = "bottom")
  

str(hospital_ratings_data)
# Rating Overall vs. Procedure Heart Cost of Each Category 


hospital_ratings_data %>%
  mutate(hospital_ratings_data, "Average Heart Cost Church" = mean()  )


# Install US MAP and other packages  ----------------------------------------------------------

install.packages("usmap")
library(usmap)
library(ggplot2)
library(tidyverse)
# Using US MAP ------------------------------------------------------------

state.by <- hospital_ratings_data %>% mutate("state" = Facility.State, "values" = Rating.Overall)

plot_usmap(data = state.by)

             

                                                           