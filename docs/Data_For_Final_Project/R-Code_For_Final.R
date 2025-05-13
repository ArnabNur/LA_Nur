
install.packages(c("tidycensus", "dplyr", "readr", "stringr", "tidyr"))
library(tidycensus)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Census API Key
census_api_key("3788f98d3edc52a98069477dbd56e4933585a0ac", install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

#2022 Poverty Data for All Iowa Counties
acs_data <- get_acs(
  geography = "county",
  state = "IA",
  year = 2022,
  variables = c(
    "B17001_002",  # People below poverty
    "B17001_001"   # Total population
  ),
  survey = "acs5",
  output = "wide"
)

# Clean and Calculate Poverty Rate
poverty_data <- acs_data %>%
  transmute(
    County = gsub(" County, Iowa", "", NAME),
    poverty_rate = B17001_002E / B17001_001E
  ) %>%
  mutate(
    County = str_trim(str_to_title(County)),
    County = str_replace(County, "O Brien", "O'Brien")
  )

#Load Nutrient Dataset
nutrient_data <- read_csv("Iowa_Nutrient_Data_2002_2023_Final_With_2022.csv") %>%
  mutate(
    County = str_trim(str_to_title(County)),
    County = str_replace(County, "O Brien", "O'Brien")
  )

#Extract 2021 Nutrient Values
nutrient_2021 <- nutrient_data %>%
  select(County, `2021`) %>%
  rename(nutrient_2021 = `2021`) %>%
  drop_na()

# Merge Nutrient + Poverty and Compute Score
risk_data <- inner_join(nutrient_2021, poverty_data, by = "County") %>%
  mutate(
    nutrient_pct = percent_rank(nutrient_2021),
    poverty_pct = percent_rank(poverty_rate),
    risk_score = (nutrient_pct + poverty_pct) / 2
  )

#Save Final Score to CSV
write_csv(risk_data, "Clean_Water_Risk_Score_2021_Final.csv")

# Preview Results
head(risk_data)
