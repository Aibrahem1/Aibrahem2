#Intial data exploration and cleaning

str(top10ghg)

#_______________Data exploratory 
#1.---------------- Setting up training and test data
nrow(top10ghg)
top10ghg_train<-top10ghg[1:300,]  # to train the model
top10ghg_test<-top10ghg[301:374,] # to test the model


analysis1<- top10ghg_train %>% 
  select(year,Sector,Emissions) %>%
  mutate(Emissions=Emissions/1000) %>% #to convert the values to Million Tonnes
  view()

#2.-----------------Descriptive statistics for each sector

analysis1_summary <- analysis1 %>%
  group_by(Sector) %>% 
  summarise(
    min_emissions = min(Emissions, na.rm = TRUE),
    max_emissions = max(Emissions, na.rm = TRUE),
    mean_emissions = mean(Emissions, na.rm = TRUE),
    median_emissions = median(Emissions, na.rm = TRUE),
    sd_emissions = sd(Emissions, na.rm = TRUE))

write.csv(analysis1_summary, "analysis1_summary.csv", row.names = FALSE)

#3.-----------------Exploring linear relationship using GGPLOT
library(tidyverse)
#for all sectors
top10ghg_train %>% 
  ggplot(aes(year,Emissions/1000))+
  geom_point()+
  geom_smooth(method = "lm", colour="blue")+
  facet_wrap(~Sector, scales = "free_y")+
  theme_minimal()+
  labs(title = "Emissions Over Time for Manufacturing Sector",
       x = "Year", y = "Emissions (Million Tonnes")
#4.----------------Correlation Testing--------------------

Correlation1_analysis<-
  top10ghg_train %>%
  group_by(Sector) %>%
  summarise(correlation = cor(year, 
                              Emissions, 
                              use = "complete.obs")) %>% #insures NA values are removed
  write.csv("correlation1_analysis.csv", row.names = FALSE)


#5.-----------Fit Linear Regression Models- R2 and Residuals----
#Required Libraries
library(tidyverse)
library(broom)

# _______calculate and extract residuals for a linear regression model 

residuals0 <- analysis1 %>% #using the train dataset
  group_by(Sector) %>%
  nest() %>%
  mutate(
    model = map(data, ~lm(Emissions ~ year, data = .x)), # Fit the linear model
    residuals = map(model, ~augment(.x))) %>% 
  view()# Get residuals


# Extract residuals and save to CSV
analysis1 %>%
  group_by(Sector) %>%
  nest() %>%
  mutate(
    model = map(data, ~lm(Emissions ~ year, data = .x)), # Fit the linear model
    residuals = map(model, ~augment(.x))                 # Extract residuals and predictions
  ) %>%
  unnest(cols = residuals) %>%                           # Unnest the residuals
  select(-data, -model) %>%                              # Drop unnecessary list columns
  write.csv("Residuals_values.csv", row.names = FALSE)   # Save to CSV

# _______ Extracting coefficients (intercept & Slope)

coefficients <- residuals0 %>%
  mutate(
    coefficients = map(model, tidy)  # Extract coefficients using broom::tidy
  ) %>%
  unnest(coefficients)

# View coefficients
print(coefficients)

# Export cofficients
coefficients %>%
  select(-data, -model, -residuals) %>% 
  write.csv("coefficients.csv", row.names = FALSE)

#________________Residuals_______________________
#. Unnest Residuals To visualize residuals, unnest the residuals into a single data frame:
residuals1 <- residuals0 %>%
  unnest(residuals)

#Residuals Visualize Residuals Per Sector
#. Plot Residuals Per Sector Use ggplot2 to visualize residuals for each sector:
residuals1 %>%
  ggplot(aes(x = year, y = .resid, color = Sector)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Sector, scales = "free_y") +  # Separate plot for each sector
  labs(
    title = "Residuals of Emissions per Sector",
    x = "Year",
    y = "Residuals"
  ) +
  theme_minimal()

#______________R-Square________________
#Extract R-squared and coefficients for each sector
model_summaries <- residuals0 %>%
  mutate(
    summary = map(model, glance),  # Model-level metrics (e.g., R-squared)
    coefficients = map(model, tidy)) # Coefficients (intercept and slope)

# Unnest summaries to view R-squared and other metrics
r_squared_table <- model_summaries %>%
  unnest(summary) %>%
  select(Sector, r.squared, adj.r.squared, p.value)

print(r_squared_table)
write.csv(r_squared_table,"R_square_table.csv", row.names = FALSE)

# Plot of R-Square values
r_squared_table %>%
  distinct(Sector, r.squared) %>%
  ggplot(aes(x = Sector, y = r.squared)) +
  geom_col(fill = "blue") +
  labs(title = "R-squared for Each Sector's Regression Model",
       x = "Sector",
       y = "R-squared Value") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, face = "bold"), # Rotate x-axis labels and make them bold
    axis.text.y = element_text(face = "bold"), # Make y-axis labels bold
    axis.title = element_text(face = "bold"), # Make axis titles bold
    plot.title = element_text(hjust = 0.5, face = "bold") # Center and bold the plot title
  )

# Extract R-squared and coefficients for each sector
residuals0 %>%
  mutate(summary = map(model, glance),     # Model-level metrics (e.g., R-squared)
         coefficients = map(model, tidy)) # Coefficients (intercept and slope)


# ------------------Making predictions with the linear regression model  ----
# ___________Define the future years for prediction
future_years <- data.frame(year = 2024:2050) #net Zero by 2050

#____________Fitting the linear regression model using test data
models <- top10ghg_test %>% #using a model of test data (unseen)
  group_by(Sector) %>%
  nest() %>%
  mutate(model = map(data,
                     ~lm(Emissions ~ year, 
                         data = .x)))  # Fit linear models

#___________Predict Future Emissions Using Test Data

#1. Make predictions for future years  (Strong, Moderate, Weak) with test data using pipe
predictions_lr <- top10ghg_test %>%
  group_by(Sector) %>%
  nest() %>%
  mutate(model = map(data, ~lm(Emissions ~ year, data = .x)), # Fit linear model
         predicted = map(model, ~data.frame(
           year = future_years$year,
           Emissions = predict(.x, newdata = future_years)))) %>%
  select(Sector, predicted) %>%
  unnest(predicted) %>% 
  view()

#2. exploring
glimpse(predictions_lr)

#3. Sectors Strong and moderate sectors (excluding Weak)
predictions_lr_f <- top10ghg_test %>%
  filter(!Sector %in% c("	Consumer Exp", "Transport")) %>% # Replace "Sector1" and "Sector2" with sectors to exclude
  group_by(Sector) %>%
  nest() %>%
  mutate(
    model = map(data, ~lm(Emissions ~ year, data = .x)), # Fit linear model
    predicted = map(model, ~data.frame(
      year = future_years$year,
      Emissions = predict(.x, newdata = future_years)))) %>%
  select(Sector, predicted) %>%
  unnest(predicted)

#exploring
glimpse(predictions_lr_f)

#------Plotting Historical vs. Predicted Trends from LR Model for Strong & Moderate Sectors----
#Required Library:
library(tidyverse)
#1. Combine predictions with actual test data
test_predictions_lr <- bind_rows(top10ghg, predictions_lr)
test_predictions_lr_f<-top10ghg %>% 
  filter(!Sector %in% c("	Consumer Exp", "Transport")) %>%
  bind_rows(predictions_lr_f)

#2. Ploting the historical and the predicted values >0 (All sectors)
test_predictions_lr %>% 
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
  annotate("text", x = 2023, 
           y = max(predictions_sector$Emissions / 1000, na.rm = TRUE),
           label = "Projected →", 
           color = "black", 
           size = 4, 
           fontface = "bold", 
           hjust = -0.11,
           vjust=-29)

#----Ploting the historical and the predicted values >0 (exclude weak)
test_predictions_lr_f%>% 
  filter(Emissions>0, !Sector%in%c("Consumer Exp","Transport")) %>%
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
  annotate("text", x = 2023, 
           y = max(predictions_sector$Emissions / 1000, na.rm = TRUE),
           label = "Projected →", 
           color = "black", 
           size = 4, 
           fontface = "bold", 
           hjust = -0.11,
           vjust=-29)

# Plot predicted vs. actual emissions
test_predictions_lr %>%
  filter(Emissions>0) %>% 
  ggplot(aes(x = year, y = Emissions, color = Sector)) +
  geom_point() +
  geom_vline(xintercept = 2024, lty="dashed")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Predicted vs. Actual Emissions for Strong and Moderate Sectors",
    x = "Predicted Emissions",
    y = "Actual Emissions"
  ) +
  facet_wrap(~Sector, scales = "free") +
  theme_minimal()

# Evaluation for strong and moderate sectors
library(broom)

evaluation_metrics <- top10ghg_test %>%
  #filter(!Sector %in% c("Consumer Exp", "Transport")) %>% # Exclude weak sectors
  group_by(Sector) %>%
  nest() %>%
  mutate(
    model = map(data, ~lm(Emissions ~ year, data = .x)), # Fit linear model
    predictions = map2(model, data, ~augment(.x, newdata = .y)), # Pass the original data explicitly
    metrics = map(predictions, ~tibble(
      MAE = mean(abs(.x$.fitted - .x$Emissions), na.rm = TRUE),
      RMSE = sqrt(mean((.x$.fitted - .x$Emissions)^2, na.rm = TRUE)),
      R2 = cor(.x$.fitted, .x$Emissions)^2
    ))
  ) %>%
  unnest(metrics)


evaluation_metrics %>%
  unnest(data) %>%
  group_by(Sector) %>% 
  select(Sector, MAE, RMSE, R2) %>%
  distinct() %>%
  write.csv("lr_evaluation_metrics.csv", row.names = FALSE)
  #view()
