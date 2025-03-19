# Meta --------------------------------------------------------------------

## Title:         CDC Tax Burden on Tobacco
## Author:        Megan Zheng
## Date Created:  3/2/2025
## Date Edited:   3/2/2025
## Description:   Analysis of CDC data 


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

if (!require("fixest")) install.packages("fixest")
library(fixest)

final.data = readRDS('data/output/TaxBurden_Data.rds')
cig.data <- read_csv("data/input/CDC_1970-2018.csv", col_names = TRUE)
cpi.data <- read_xlsx("data/input/CPI_1913_2019.xlsx", skip = 11)


# Question 1: Bar Graph
## Filter data for the years 1970 to 1985
filtered_data <- final.data %>%
  filter(between(Year,1970, 1985)) %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = tax_state != lag(tax_state, default = first(tax_state))) %>%
  ungroup()

## Calculate the proportion of states with a change in cigarette tax each year
proportion_data <- filtered_data %>%
     group_by(Year) %>%
     summarize(proportion_change = mean(tax_change, na.rm = TRUE))

## Create the bar graph
q1 = ggplot(proportion_data, aes(x = Year, y = proportion_with_change)) +
    geom_bar(stat = "identity") +
    labs(title = "Proportion of States with Cigarette Tax Change (1970-1985)",
             x = "Year",
             y = "Proportion of States") +
    theme_minimal()

# Question 2: avg tax and price for cigarretes 
## Adjust the price and tax to 2012 dollars
final.data <- final.data %>%
  mutate(price_real = cost_per_pack * (230/index),
         tax_real = tax_dollar * (230/index))

filtered_data_2018 <- final.data %>%
  filter(between(Year,1970, 2018)) %>%
  group_by(Year) %>%
  summarize(avg_tax = mean(tax_real, na.rm = TRUE),
            avg_price = mean(price_real, na.rm = TRUE))

## Create the line plot
q2 = ggplot(filtered_data_2018, aes(x = Year, y = Value, color = Type)) +
  geom_line(size = 1.2) +
  labs(title = "Average Cigarette Tax and Price (1970-2019)",
       x = "Year",
       y = "Dollars (2012 Adjusted)") +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Average Tax (2012 Dollars)", "Average Price (2012 Dollars)")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Question 3: 5 states with highest increase in cig prices
## Calculate the increase in cigarette price per state
price_increase <- filtered_data_2018 %>%
  group_by(state) %>%
  summarize(price_change = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE)) %>%
  arrange(desc(price_change))

## Identify the top 5 states with the highest increase in cigarette prices
top_5_states <- price_increase %>% slice_head(n = 5) %>% pull(state)

## Filter data for the top 5 states
top_states_data <- filtered_data_2018 %>% filter(state %in% top_5_states)

## Compute the average number of packs sold per capita for each year
avg_packs_sold <- top_states_data %>%
  group_by(Year, state) %>%
  summarize(avg_packs = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

## Create the line plot
q3 = ggplot(avg_packs_sold, aes(x = Year, y = avg_packs, color = state)) +
  geom_line(size = 1.2) +
  labs(title = "Average Packs Sold Per Capita in Top 5 States (1970-2018)",
       x = "Year",
       y = "Average Packs Sold Per Capita") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Question 4: 5 states with lowest increase in cig prices
## Calculate the increase in cigarette price per state
price_increase <- filtered_data_2018 %>%
  group_by(state) %>%
  summarize(price_change = max(cost_per_pack, na.rm = TRUE) - min(cost_per_pack, na.rm = TRUE)) %>%
  arrange(price_change) 

## Identify the top 5 states with the lowest increase in cigarette prices
bottom_5_states <- price_increase %>% slice_head(n = 5) %>% pull(state)

## Filter data for the bottom 5 states
bottom_states_data <- filtered_data_2018 %>% filter(state %in% bottom_5_states)

## Compute the average number of packs sold per capita for each year
avg_packs_sold <- bottom_states_data %>%
  group_by(Year, state) %>%
  summarize(avg_packs = mean(sales_per_capita, na.rm = TRUE), .groups = "drop")

## Create the line plot
q4 = ggplot(avg_packs_sold, aes(x = Year, y = avg_packs, color = state)) +
  geom_line(size = 1.2) +
  labs(title = "Average Packs Sold Per Capita in 5 States with Lowest Price Increase (1970-2018)",
       x = "Year",
       y = "Average Packs Sold Per Capita") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Question 5: written response

# Estimating ATEs

# Filter data for the years 1970 to 1990
data_1970_1990 <- final.data %>%
  filter(between(Year, 1970, 1990))

# Question 6: Price Elasticity of Demand for Cigs in 1970 - 1990
# Compute log of sales and log of prices
data_1970_1990 <- data_1970_1990 %>%
  mutate(ln_sales = log(sales_per_capita),
         ln_price_2012 = log(price_real)
         ) %>%
  drop_na(ln_sales, ln_price_2012)
q6 = feols(ln_sales ~ ln_price_2012, data = data_1970_1990)

q6 <- feols(ln_sales ~ ln_price_2012 | state + Year, data = data_1970_1990)


data_1970_1990 <- data_1970_1990 %>%
  drop_na(ln_sales, ln_price_2012, state, Year)

q6 <- feols(ln_sales ~ ln_price_2012 | state + Year, data = data_1970_1990)

q6 <- feols(ln_sales ~ 1 | ln_price_2012 ~ ln_total_tax, data = data_1970_1990)


# Regress log sales on log prices
q6 <- feols(ln_sales ~ ln_price | state + Year, data = data_1970_1990)

q6 = feols(ln_sales~ln_price_2012, data=data_1970_1990)

final.data <- final.data %>%
  filter(between(Year, 1991, 2015)) %>%
  drop_na(cost_per_pack, sales_per_capita, price_cpi)

final.data <- final.data %>%
  mutate(ln_sales = log(sales_per_capita),
         ln_price_2012 = log(cost_per_pack * (218 / price_cpi)))

q6 = feols(ln_sales~ln_price_2012, data=final.data %>% filter(Year<1991))

# Display the summary of the regression model
summary(q6)

### Interpretation:
### The coefficient of log_price represents the price elasticity of demand.
### A negative coefficient indicates that an increase in price leads to a decrease in sales, which is expected for normal goods.

# Question 7: with instrument
# Regress log sales on log prices using total cigarette tax as an instrument for log prices
q7 <- feols(ln_sales ~ 1 | state + Year | ln_price ~ tax_dollar, data = data_1970_1990)

# Display the summary of the regression model
summary(q7)

### Interpretation:
### The coefficient of log_price represents the price elasticity of demand when using the total cigarette tax as an instrument.
### Compare the coefficients from q6 and q7 to see if they are different.
### If the estimates are different, it could be due to the instrument accounting for endogeneity in the price variable.

# Question 8: First Stage and reduced form of instrument
## First Stage: ln_price on tax_dollar with fixed effects
q8_firststage <- feols(ln_price ~ tax_dollar | state + Year, data = data_1970_1990)
summary(q8_firststage)

## Reduced-form regression: ln_sales on tax_dollar with fixed effects
q8_reduced_orm <- feols(ln_sales ~ tax_dollar | state + Year, data = data_1970_1990)
summary(q8_reducedform)

# Question 9: Repeat of Questions 6-8, but for 1991 - 2015
## Question 9.6: Price Elasticity of Demand for Cigs in 1991 - 2015
### Filter data for the years 1991 to 2015
data_1991_2015 <- final.data %>%
  filter(between(Year, 1991, 2015)) %>%
  drop_na(cost_per_pack, sales_per_capita)

### Compute log of sales and log of prices
data_1991_2015 <- data_1991_2015 %>%
  mutate(ln_sales = log(sales_per_capita),
         ln_price = log(cost_per_pack))

### Regress log sales on log prices with state and year fixed effects
q9_6 <- feols(ln_sales ~ ln_price | state + Year, data = data_1991_2015)

## Question 9.7: Instrumental Variable Regression: ln_sales on ln_price using tax_dollar as IV
### Instrumental Variable Regression: ln_sales on ln_price using tax_dollar as IV
q9_7 <- feols(ln_sales ~ 1 | state + Year | ln_price ~ tax_dollar, data = data_1991_2015)

## Question 9.8: First Stage and Reduced Form of Instrument
### First-stage regression: ln_price on tax_dollar with fixed effects
q9_8_firststage <- feols(ln_price ~ tax_dollar | state + Year, data = data_1991_2015)

## Question 9.8: Reduced-form regression: ln_sales on tax_dollar with fixed effects
### Reduced-form regression: ln_sales on tax_dollar with fixed effects
q9_8_reducedform <- feols(ln_sales ~ tax_dollar | state + Year, data = data_1991_2015)
summary(q9_8_reducedform)

# Workspace
rm(list=c("final.data"))
save.image("submission1/hwk3_workspace.Rdata")

