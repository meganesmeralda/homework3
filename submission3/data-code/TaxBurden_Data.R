# Meta --------------------------------------------------------------------

## Title:         CDC Tax Burden on Tobacco
## Author:        Megan Zheng
## Date Created:  3/2/2025
## Date Edited:   3/2/2025
## Description:   Data cleaning of CDC data 


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

cig.data <- read_csv("data/input/CDC_1970-2018.csv", col_names = TRUE)
cpi.data <- read_xlsx("data/input/CPI_1913_2019.xlsx", skip = 11)

# Clean tobacco data --------------------------------------------------------------
cig.data <- cig.data %>%
  mutate(measure = case_when(
    SubMeasureDesc == "Average Cost per pack" ~ "cost_per_pack",
    SubMeasureDesc == "Cigarette Consumption (Pack Sales Per Capita)" ~ "sales_per_capita",
    SubMeasureDesc == "Federal and State tax as a Percentage of Retail Price" ~ "tax_percent",
    SubMeasureDesc == "Federal and State Tax per pack" ~ "tax_dollar",
    SubMeasureDesc == "Gross Cigarette Tax Revenue" ~ "tax_revenue",
    SubMeasureDesc == "State Tax per pack" ~ "tax_state"
  )) %>%
  select(state_abb = LocationAbbr, 
         state = LocationDesc, 
         Year, 
         value=Data_Value, 
         measure)
         
final.data <- pivot_wider(cig.data, 
                         id_cols = c("state","Year"),
                         names_from = "measure",
                         values_from = "value") %>%
  arrange(state, Year)

# Clean CPI data ----------------------------------------------------------
cpi.data <- pivot_longer(cpi.data, 
                         cols=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                         names_to="month",
                         values_to="index")
cpi.data <- cpi.data %>%
  group_by(Year) %>%
  summarize(index=mean(index, na.rm=TRUE))

cpi.2012 <- cpi.data %>%
  filter(Year==2012) %>%
  select(index) %>% as.numeric

# Form final dataset ------------------------------------------------------
# adjust to 2010 dollars
final.data <- final.data %>%
  mutate(cpi_2012=cpi.2012) %>%
  left_join(cpi.data, by="Year")

write_tsv(final.data,"data/output/TaxBurden_Data.txt",append=FALSE,col_names=TRUE)
write_rds(final.data,"data/output/TaxBurden_Data.rds")

