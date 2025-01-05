library(readr)#
library(janitor)
library(readxl)
# Identify the working Directory
setwd("~/Rdirectory") #set the working director for the file



#read the data from the downloaded dataset
GHG_total <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "GHG total", 
                        range = "A8:Y42")


#Exploring the data
View(GHG_total)
str(GHG_total)

#importing data using Readxl package
GHG_total<- GHG_total %>% 
  mutate(gas_type="Combined") %>% #Creaing Unique variable to indicate the gas type
  rename(year=`Industry name`) %>% 
  select(year, gas_type, everything())

GHG_total<-clean_names(GHG_total)

#Co2
Co2_total <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "Co2", 
                        range = "A7:Y41")
Co2_total<- Co2_total %>% 
  mutate(gas_type="Co2") %>% 
  rename(year=`Industry name`) %>% 
  select(year, gas_type, everything())
Co2_total<-clean_names(Co2_total)
#CH4
CH4_total <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "CH4", 
                        range = "A7:Y41")
CH4_total<- CH4_total %>% 
  mutate(gas_type="CH4") %>% 
  rename(year=`Industry name`) %>% 
  select(year, gas_type, everything())
CH4_total<-clean_names(CH4_total)
#N2O
N2O_total <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "N2O", 
                        range = "A7:Y41")
N2O_total<- N2O_total %>% 
  mutate(gas_type="N2O") %>% 
  rename(year=`Industry name`) %>% 
  select(year, gas_type, everything())
N2O_total<-clean_names(N2O_total)
#HFC
HFC_total <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "HFC", 
                        range = "A7:Y41")
HFC_total<- HFC_total %>% 
  mutate(gas_type="HFC") %>% 
  rename(year=`Industry name) %>% 
  select(year, gas_type, everything())
HFC_total<-clean_names(HFC_total)
#PFC
PFC_total <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "PFC", 
                        range = "A7:Y41")
PFC_total<- PFC_total %>% 
  mutate(gas_type="PFC") %>% 
  rename(year=`Industry name`) %>% 
  select(year, gas_type, everything())
PFC_total<-clean_names(PFC_total)
#SF6
SF6_total <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "SF6", 
                        range = "A7:Y41")
SF6_total<- SF6_total %>% 
  mutate(gas_type="SF6") %>% 
  rename(year=`Industry name`) %>% 
  select(year, gas_type, everything())
SF6_total<-clean_names(SF6_total)
#NF3
NF3_total <- read_excel("GHG_rawdata.xlsx", 
                        sheet = "NF3", 
                        range = "A7:Y41")
NF3_total<- NF3_total %>% 
  mutate(gas_type="NF3") %>% 
  rename(year=`Industry name`) %>% 
  select(year, gas_type, everything())
NF3_total<-clean_names(NF3_total)

# combining into one single dataframe using dplyr
#------1. Match variable headers
#GHG
GHG_total<-rename(GHG_total,
                  "total_gas_emissions"="total_greenhouse_gas_emissions" )
#Co2
Co2_total<-rename(Co2_total,
                  "total_gas_emissions"="total_co2_gas_emissions" )

#CH4
CH4_total<-rename(CH4_total,
                  "total_gas_emissions"="total_ch4_gas_emissions" )
#HFC
HFC_total<-rename(HFC_total,
                  "total_gas_emissions"="total_hfc_gas_emissions" )
#N2O
N2O_total<-rename(N2O_total,
                  "total_gas_emissions"="total_n2o_gas_emissions" )
#NF3
NF3_total<-rename(NF3_total,
                  "total_gas_emissions"="total_nf3_gas_emissions" )
#PFC
PFC_total<-rename(PFC_total,
                  "total_gas_emissions"="total_pfc_gas_emissions" )
#SF6
SF6_total<-rename(SF6_total,
                  "total_gas_emissions"="total_sf6_gas_emissions" )

# -----2. Combining the data by using bind
all_gases_emission<-bind_rows(GHG_total,
                              Co2_total,
                              CH4_total,
                              HFC_total,
                              N2O_total,
                              NF3_total,
                              PFC_total,
                              SF6_total)
#-----3. Exploring the combined data
str(all_gases_emission)
glimpse(all_gases_emission)
head(all_gases_emission)

# creating COPY OF data for vis
emissions_data<-all_gases_emission
#renaming variables for visual clarity purposes
emissions_data<-emissions_data %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

CH4_total<-CH4_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")
Co2_total<-Co2_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")
GHG_total<-GHG_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")
HFC_total<-HFC_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")
N2O_total<-N2O_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")
NF3_total<-NF3_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")
PFC_total<-PFC_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")
SF6_total<-SF6_total %>% 
  rename("Agriculture"="agriculture_forestry_and_fishing",
         "Mining"="mining_and_quarrying",
         "Manufacturing"="manufacturing",
         "Energy"="electricity_gas_steam_and_air_conditioning_supply",
         "Water & Waste"="water_supply_sewerage_waste_management_and_remediation_activities",
         "Construction"="construction",
         "Retail & Auto"="wholesale_and_retail_trade_repair_of_motor_vehicles_and_motorcycles",
         "Transport"="transport_and_storage",
         "Hospitality"="accommodation_and_food_services",
         "ICT"="information_and_communication",
         "Finance"="financial_and_insurance_activities",
         "Real Estate"="real_estate_activities",
         "Professional"="professional_scientific_and_technical_activities",
         "Admin Services"="administrative_and_support_service_activities",
         "Public Admin"="public_administration_and_defence_compulsory_social_security",
         "Education"="education",
         "Healthcare"="human_health_and_social_work_activities",
         "Arts & Rec"="arts_entertainment_and_recreation",
         "Other Services"="other_service_activities",
         "Households"="activities_of_households_as_employers_undifferentiated_goods_and_services_producing_activities_of_households_for_own_use",
         "Consumer Exp"="consumer_expenditure_note_4",
         "Consumer Non-Travel"="consumer_expenditure_not_travel",
         "Consumer Travel"="consumer_expenditure_travel",
         "Total"="total_gas_emissions")

#-----------------------Removing Unwanted dataframes
# can be removed because subsets can be made from Emissions data created
rm(CH4_total)
rm(Co2_total)
rm(GHG_total)
rm(HFC_total)
rm(N2O_total)
rm(NF3_total)
rm(PFC_total)
rm(SF6_total)
#------------------------Consumer Data ---------------------
# making subset Consumer expenditure
# removed zero emission gases from consumer EXP
# Calculate percentage

Consumer_data<-emissions_data %>%
  filter(`Consumer Exp`>0) %>%
  select(year,gas_type, 
         `Consumer Non-Travel`,
         `Consumer Travel`,
         `Consumer Exp`) %>% 
  mutate(
    Non_travel_percentage = `Consumer Non-Travel` * 100 / `Consumer Exp`,
    Travel_percentage = `Consumer Travel` * 100 / `Consumer Exp`)
# Creating long data from consumer_data

Consumer_data_long<-Consumer_data %>% 
  select(year,gas_type,
         Non_travel_percentage,
         Travel_percentage) %>% 
  pivot_longer(
    cols = 3:4,
    names_to = "Consumer EXP",
    values_to = "Percentage"
  )


#------------- pivot long ---------------

# Pivot of emission data and calculate the percentage 
sector_data<-emissions_data %>% 
  select(year,gas_type, Total, everything(),
         -`Consumer Non-Travel`,-`Consumer Travel`) %>%
  pivot_longer(
    cols = 4:24,
    names_to = "sector",
    values_to="emissions") %>%
  mutate(Percentage= round(emissions*100/Total,2)) %>% 
  select(year,gas_type,sector,emissions ,Total, Percentage) %>% 
  view()

#------test------subset gas----------------
#Using filter
sector_data %>% 
  filter(gas_type == "Co2" & sector == "Manufacturing") %>%
  View()

#Using subset
subset(sector_data, gas_type%in%c("Co2","CH4") & 
         sector=="Manufacturing") %>% 
  view()

#GHG_data_long subset
GHG_data_long<-subset(sector_data, gas_type=="Combined") %>%
  rename("Emissions"="emissions", "Sector"="sector") %>% 
  view()

#Stacked data with total long format

emissions_data %>% 
  select(year,gas_type,Total) %>% 
  filter(gas_type!="Combined", year%in%c("1990",
                                         "2000",
                                         "2010",
                                         "2020")) %>% 
  view()

##---------------- data mutation


#i chose a stacked area chart#
#to avoid over crowding the graph I dedcide to aggregate data
#I will present top 10 sectors & others
#Others is the sum of lowest 10 sectors

#-------------------------Top10ghg data---------------------
#I have rerrange data to identify the top 10 sectors and the lowest 10
GHG_data_long %>% 
  arrange(year,desc(Emissions)) %>% # arrange by year ascending order and emissions descending order
  write.csv("top10ghg.csv", row.names = FALSE ) #save for exploration

#I conducted data exploration
GHG_data_long %>% 
  arrange(year,desc(Emissions)) %>% # arrange by year asce and emissions desc
  view()

#using dplyr I filter out other sectors to aggregate
other_sectors_row <- GHG_data_long %>% #the dataset
  arrange(year,desc(Emissions)) %>% #additional and not really needed
  filter(Sector %in% c("Education", "Healthcare", "Hospitality", 
                       "Admin Services", "Professional", "Arts & Rec", 
                       "ICT", "Other Services", "Real Estate", 
                       "Finance", "Households")) %>% #filtered out sectors to add
  group_by(year, gas_type) %>% #grouping by year and gas_type
  summarize(
    Sector = "Other Sectors",
    Emissions = sum(Emissions, na.rm = TRUE), #sum emissions
    Total = first(Total),  # Assuming Total is consistent within each year
    Percentage = sum(Percentage, na.rm = TRUE)) %>% #sum percentage
  view()

#I filtered out other the aggregated sectors from the GHG_Data_long
GHG_filtered <- GHG_data_long %>%
  arrange(year,desc(Emissions)) %>% 
  filter(!Sector %in% c("Education", "Healthcare", "Hospitality", 
                        "Admin Services", "Professional", "Arts & Rec", 
                        "ICT", "Other Services", "Real Estate", 
                        "Finance", "Households")) %>% 
  view()
#combined the 2 data frames using rowbind

top10ghg<- 
  bind_rows(GHG_filtered, other_sectors_row) %>%
  arrange(year, desc(Emissions)) %>%
  view()

#Removed the mutated data as it will not be using the visualisation or the analysis
rm(GHG_filtered)
rm(other_sectors_row)