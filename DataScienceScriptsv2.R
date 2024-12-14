library(dplyr)
library(ggplot2)
library(tidyverse)
library(purrr)
install.packages("nortest")
library(nortest)
#predict the gas emissions 
####Visualisation to check linear relationship

#for all sectors
top10ghg %>% 
  ggplot(aes(year,Emissions/1000))+
  geom_point()+
  geom_smooth(method = "lm", colour="blue")+
  facet_wrap(~Sector, scales = "free_y")+
  theme_minimal()+
  labs(title = "Emissions Over Time for Manufacturing Sector",
       x = "Year", y = "Emissions (Million Tonnes")

## model

top10ghg %>%
  group_by(Sector) %>%
  nest() %>%
  mutate(model = map(data, ~lm(Emissions ~ year+0, data = .x))) %>%
  view()# Fit linear model

  
  # Define the future years for predictions (Explanatory variables (independent))
future_years <- data.frame(year = 2024:2050)

# Predict for each sector
predictions <- top10ghg %>%
  group_by(Sector) %>%
  nest() %>%
  mutate(model = map(data, ~lm(Emissions ~ year, data = .x)), # Fit linear model
    predicted = map(model, ~data.frame(
      year = future_years$year,
      Emissions = predict(.x, newdata = future_years)))) %>%
  select(Sector, predicted) %>%
  unnest(predicted)


#Explort prediction model
glimpse(predictions)

# combinind predicated data with hostrical
predictions_sector <- bind_rows(top10ghg, predictions)

#Ploting the historical and the predicted values >0
predictions_sector %>% 
  filter(Emissions>0) %>%
  mutate(Emissions=Emissions/1000) %>% # converted to million tonnes
  ggplot(aes(x = year,
             y = Emissions, 
             fill = Sector)) +
  geom_area(position = "stack") +
  #adding labels
  labs(title = "Top Emitting Sectors: Historical and Projected Emissions",
       x = "Year",
       fill="Sector",
       y = "Emissions (Million Tonnes)",
       caption = "Data source: UK Office for National Statistics (ONS) - UK Environmental Accounts") +
  #Specifying the scale range
  scale_x_continuous(breaks = seq(1990, 2050, by = 5)) +  # Breaks every 5 years
  scale_y_continuous(breaks = seq(0, 1000, by = 100))+   # Breaks every 100 million tonnes
  #Themes
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(size=12),
        axis.title = element_text(size = 13),
        plot.title = element_text(hjust = 0.5),
        title = element_text(size = 15,
                             face="bold"),
        axis.text.x = element_text(angle = 45,
                                   size=10,
                                   face="bold"),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0, size = 9))+
  #colouring Scheme
  scale_fill_viridis_d() +
  #geom_vline to used for annotations indication
  geom_vline(xintercept = 2023, 
             linetype = "dashed",
             color = "red") +
  #annotations
  annotate("text", x = 2023, 
           y = max(predictions_sector$Emissions/1000, na.rm = TRUE),
           label = "←Historical",
           size=4,
           fontface="bold",
           hjust = 1.07, 
           vjust = -29)+
    annotate("text", x = 2037, 
           y = max(predictions_sector$Emissions / 1000, na.rm = TRUE),
           label = "Projected →", 
           color = "black", 
           size = 4, 
           fontface = "bold", 
           hjust = 2.25,
           vjust=-29)
#---------------------------------------------------------------
#evaluating the quality of the regression model by calucation
#R square value

predictions_rSquare <- top10ghg_train %>%
  group_by(Sector) %>%
  nest() %>%
  mutate(
    model = map(data, ~lm(Emissions ~ year, data = .x)), # Fit linear model
    r_squared = map_dbl(model, ~summary(.x)$r.squared),  # Extract R-squared
    predicted = map(model, ~data.frame(
      year = future_years$year,
      Emissions = predict(.x, newdata = future_years)))
  ) %>%
  select(Sector, r_squared, predicted) %>%
  unnest(predicted) %>% 
  view()


predictions_rSquare %>%
  distinct(Sector, r_squared) %>%
  ggplot(aes(x = Sector, y = r_squared)) +
  geom_col(fill = "blue") +
  labs(title = "R-squared for Each Sector's Regression Model",
       x = "Sector",
       y = "R-squared Value") +
  theme_minimal()

### make a line chart showing transportation trend-line





#----------------------------Training and test data --------------------------
#--------checking data for linear relationship
#manufacturing
top10ghg %>%
  filter(Sector == "Manufacturing") %>%
  ggplot(aes(x = year, y = Emissions)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Emissions Over Time for Manufacturing Sector",
       x = "Year", y = "Emissions") +
  theme_minimal()

#for all sectors
top10ghg %>% 
  ggplot(aes(year,Emissions/1000))+
  geom_point()+
  geom_smooth(method = "lm", colour="blue")+
  facet_wrap(~Sector, scales = "free_y")+
  theme_minimal()+
  labs(title = "Emissions Over Time for Manufacturing Sector",
       x = "Year", y = "Emissions (Million Tonnes")



#--------Creating the Training and Test Datasets

top10ghg_train<-top10ghg[1:341,] #training data
top10ghg_test<-top10ghg[341:374,] #Test Data


#-------Correlation between year and emissions
#correlation check
top10ghg_train %>%
  group_by(Sector) %>%
  summarise(correlation = cor(year, 
                              Emissions, 
                              use = "complete.obs"))


#Fitting the linear regression model
mod_year <- lm(formula = Emissions ~ year, 
               data = top10ghg_train)
summary(mod_year)

library(tidyverse)

# Fit a separate linear model for each sector
sector_models <- top10ghg_train %>%
  group_by(Sector) %>%
  nest() %>%  # Nest data for each sector into its own tibble
  mutate(
    model = map(data, ~lm(Emissions ~ year, data = .x)),  # Fit a linear model for each sector
    model_summary = map(model, summary)  # Optionally store the summary of each model
  )


top10ghg %>%
  filter(Sector == "Transport") %>%
  ggplot(aes(x = year, y = Emissions)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Emissions Over Time for Manufacturing Sector",
       x = "Year", y = "Emissions") +
  theme_minimal()

#####
top10ghg %>%
  filter(Sector == "Manufacturing") %>%
  ggplot(aes(x = year, y = Emissions)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Emissions Over Time for Manufacturing Sector",
       x = "Year", y = "Emissions") +
  theme_minimal()


##############


#exploring the training dataset
top10ghg[1:11,]
summary(top10ghg_train)


ggplot(
  data=top10ghg_train,
  aes(x=year, y=Emissions)
) + geom_point()


#--------------------------------------------


#Correlaction check between sector(Dependent) & Time(Independent)



#Fitting the first linear regression
#We’ll use R’s lm() function to fit the linear regression model to the data. 
#lm()’s first argument takes an Rstyle
#definition of the dependent vs. independent variables: <dependent> ~ <independent>.