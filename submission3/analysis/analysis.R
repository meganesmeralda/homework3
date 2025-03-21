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
final.data <- final.data %>%
  mutate(price_real = cost_per_pack * (cpi_2012/index),
         tax_real = tax_dollar * (cpi_2012/index))

filtered_data_2018 <- final.data %>%
  filter(between(Year,1970, 2018)) %>%
  group_by(Year) %>%
  summarize(avg_tax = mean(tax_real, na.rm = TRUE),
            avg_price = mean(price_real, na.rm = TRUE))

## Create the line plot
q2 <- ggplot(filtered_data_2018, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Average Tax (2012 Dollars)"), 
            linewidth = 1.2) +
  geom_line(aes(y = avg_price, color = "Average Price (2012 Dollars)"), 
            linetype = "dashed", linewidth = 1.2) +
  labs(title = "Average Cigarette Tax and Price (1970-2019)",
       x = "Year",
       y = "Dollars (2012 Adjusted)") +
  scale_color_manual(values = c("red", "blue"),
                     labels = c("Average Tax (2012 Dollars)", 
                                "Average Price (2012 Dollars)")) +
  theme_minimal() +
  theme(legend.title = element_blank())
print(q2)

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
q4 <- change_price %>% filter(change_group=="low") %>% 
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
final.data <- final.data %>%
  mutate(ln_sales = log(sales_per_capita),
         ln_price = log(price_real),
         ln_tax_dollar = log(tax_real))

q6 <- feols(ln_sales ~ ln_price, data = final.data %>% filter(between(Year, 1970, 1990)))

# Question 7: with instrument
q7 <- feols(ln_sales ~ 1 | ln_price ~ ln_tax_dollar, data = final.data %>% filter(between(Year, 1970, 1990)))

# Question 8: First Stage and reduced form of instrument
## First Stage: ln_price on tax_dollar with fixed effects
q8_firststage <- feols(ln_price ~ ln_tax_dollar, data = final.data %>% filter(between(Year, 1970, 1990)))

## Reduced-form regression: ln_sales on tax_dollar with fixed effects
q8_reduced_form <- feols(ln_sales ~ ln_tax_dollar, data = final.data %>% filter(between(Year, 1970, 1990)))

# Question 9: Repeat of Questions 6-8, but for 1991 - 2015
q9.6 <- feols(ln_sales ~ ln_price, data = final.data %>% filter(between(Year, 1991, 2015)))
q9.7 <- feols(ln_sales ~ 1 | ln_price ~ ln_tax_dollar, data = final.data %>% filter(between(Year, 1991, 2015)))
q9.8_firststage <- feols(ln_price ~ ln_tax_dollar, data = final.data %>% filter(between(Year, 1991, 2015)))
q9.8_reduced_form <- feols(ln_sales ~ ln_tax_dollar, data = final.data %>% filter(between(Year, 1991, 2015)))



# Workspace
rm(list = setdiff(ls(), c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8_firststage", "q8_reduced_form", "q9.6", "q9.7", "q9.8_firststage", "q9.8_reduced_form")))
save.image("submission3/results/hwk3_workspace.Rdata")
