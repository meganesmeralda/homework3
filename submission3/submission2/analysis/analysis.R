# Meta --------------------------------------------------------------------

## Title:         CDC Tax Burden on Tobacco
## Author:        Megan Zheng
## Date Created:  3/19/2025
## Date Edited:   3/19/2025
## Description:   Analysis of CDC data 


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

if (!require("fixest")) install.packages("fixest")
library(fixest)

final.data = readRDS('data/output/TaxBurden_Data.rds')

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
q1 = ggplot(proportion_data, aes(x = Year, y = proportion_change)) +
    geom_bar(stat = "identity") +
    labs(title = "Proportion of States with Cigarette Tax Change (1970-1985)",
             x = "Year",
             y = "Proportion of States") +
    theme_minimal()

# Question 2: avg tax and price for cigarretes 
## Adjust the price and tax to 2012 dollars
cpi_2012 <- cpi.data %>% filter(Year == 2012) %>% pull(index)

final.data <- final.data %>%
  mutate(price_real = cost_per_pack * (cpi_2012/index),
         tax_real = tax_dollar * (cpi_2012/index))

filtered_data_2018 <- final.data %>%
  filter(between(Year,1970, 2018)) %>%
  group_by(Year) %>%
  summarize(avg_tax = mean(tax_real, na.rm = TRUE),
            avg_price = mean(price_real, na.rm = TRUE))

## Create the line plot
q2 = ggplot(filtered_data_2018, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Average Tax (2012 Dollars)"), linewidth = 1.2) +
  geom_line(aes(y = avg_price, color = "Average Price (2012 Dollars)"), linetype = "dashed", linewidth = 1.2) +
  geom_line(size = 1.2) +
  labs(title = "Average Cigarette Tax and Price (1970-2019)",
       x = "Year",
       y = "Dollars (2012 Adjusted)") +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Average Tax (2012 Dollars)", "Average Price (2012 Dollars)")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# Question 3 and 4: 5 states with highest increase in cig prices, and 5 lowest
## Calculate the increase in cigarette price per state 
final.data <- final.data %>%
  mutate(Year=as.integer(Year))

price_increase <- final.data %>%
  filter(Year == 1970) %>%
    select(state, price_1970=price_real) %>%
  left_join(final.data %>%
  filter(Year == 2018) %>%
  select(state, price_2018=price_real), by = "state") %>%
  mutate(price_change = price_2018 - price_1970) 

highest_change <- price_increase %>% slice_max(price_change, n = 5) %>% mutate(change_group = "high")
lowest_change <- price_increase %>% slice_min(price_change, n = 5) %>% mutate(change_group = "low")
change <- rbind(highest_change, lowest_change)

change_price <- final.data %>% ungroup()%>%
inner_join(change %>% select(state, change_group), 
            by = c("state"))


## Question 3: Highest state change, create the line plot
q3 <- change_price %>% filter(change_group=="high") %>% 
  ggplot(aes(x = Year, y = sales_per_capita, color = state)) +
  stat_summary(fun="mean", geom="line") +
  labs(title = "Cigarette Sales Per Capita",
       subtitle = "For the 5 States with the Highest Increase in Cigarette Prices",
       x = "Year",
       y = "Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

## Question 4: Lowest state change, create the line plot
q3 <- change_price %>% filter(change_group=="low") %>% 
  ggplot(aes(x = Year, y = sales_per_capita, color = state)) +
  stat_summary(fun="mean", geom="line") +
  labs(title = "Cigarette Sales Per Capita",
       subtitle = "For the 5 States with the Lowest Increase in Cigarette Prices",
       x = "Year",
       y = "Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

# Question 5: written response

# Estimating ATEs

# Filter data for the years 1970 to 1990
data_1970_1990 <- final.data %>%
  filter(between(Year, 1970, 1990))

# Question 6: Price Elasticity of Demand for Cigs in 1970 - 1990
# Compute log of sales and log of prices
data_1970_1990 <- data_1970_1990 %>%
  mutate(ln_sales = log(sales_per_capita),
         ln_price_2012 = log(price_real))

q6 <- feols(ln_sales ~ ln_price_2012, data = data_1970_1990)

# Question 7: with instrument
q7 <- feols(ln_sales ~ 1 | state + Year | ln_price_2012 ~ tax_dollar, data = data_1970_1990)
q7


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
save.image("submission2/hwk3_workspace.Rdata")

