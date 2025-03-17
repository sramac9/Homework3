# Meta --------------------------------------------------------------------
# Author:        Sammy Ramacher
# Date Created:  3/15/2025
# Date Edited:   3/15/2025
# Homework 3-1

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)

final.data <- readRDS("data/output/TaxBurden_Data.rds")

# Question 1: Change in Cigarette Tax 
final.data <- final.data %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = ifelse(lag(tax_dollar) != tax_dollar, 1, 0)) %>%
  ungroup()

tax_change_data <- final.data %>%
  filter(Year >= 1970 & Year <= 1985)

tax_change_proportion <- tax_change_data %>%
  group_by(Year) %>%
  summarize(proportion_with_change = mean(tax_change, na.rm = TRUE))

q1 <- ggplot(tax_change_proportion, aes(x = Year, y = proportion_with_change)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Proportion of States that Changed Cigarette Tax, 1970-1985",
       x = "Year",
       y = "Proportion with Tax Change") +
  theme_minimal()
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Question 2: Average Tax and Cigarette Price
avg_tax_price_data <- final.data %>%
  group_by(Year) %>%
  summarize(
    avg_tax = mean(tax_dollar, na.rm = TRUE),
    avg_price = mean(cost_per_pack, na.rm = TRUE))

q2 <- ggplot(avg_tax_price_data, aes(x = Year)) +
  geom_line(aes(y = avg_tax), color = "blue", size = 1.2, linetype = "solid") +
  geom_line(aes(y = avg_price), color = "red", size = 1.2, linetype = "solid") +
  scale_y_continuous(
    name = "Average Tax (2012 Dollars)",
    sec.axis = sec_axis(~ ., name = "Average Price (2012 Dollars)")) +
  labs(
    title = "Average Cigarette Tax and Price (1970 - 2018)",
    x = "Year") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.title.y.right = element_text(size = 14, face = "bold")) +
  scale_color_manual(values = c("blue", "red"))

# Question 3: Highest Cigarette Prices
price_changes <- final.data %>%
  filter(Year %in% c(1970, 2018)) %>%
  group_by(state, Year) %>%
  summarize(cost_per_pack = mean(cost_per_pack, na.rm = TRUE), .groups = 'drop')

price_changes_wide <- price_changes %>%
  pivot_wider(names_from = Year, values_from = cost_per_pack)

price_changes_wide <- price_changes_wide %>%
  mutate(price_change = `2018` - `1970`)

top5_states <- price_changes_wide %>%
  arrange(desc(price_change)) %>%
  slice_head(n = 5) %>%
  pull(state)

top_5_data <- final.data %>%
  filter(state %in% top5_states) %>%
  group_by(state, Year) %>%
  summarize(avg_sales_per_capita = mean(sales_per_capita, na.rm = TRUE), .groups = 'drop')

q3 <- ggplot(top_5_data, aes(x = Year, y = avg_sales_per_capita, color = state, group = state)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average Cigarette Packs Sold per Capita for Top 5 States with Highest Price Increases (1970-2018)",
    x = "Year",
    y = "Average Packs Sold per Capita"
  ) +
  scale_color_brewer(palette = "Set1", name = "State") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"))

# Question 4: Lowest Cigarette Prices
price_changes <- final.data %>%
  filter(Year %in% c(1970, 2018)) %>%
  group_by(state, Year) %>%
  summarize(cost_per_pack = mean(cost_per_pack, na.rm = TRUE), .groups = 'drop')

price_changes <- final.data %>%
  filter(Year %in% c(1970, 2018)) %>%
  group_by(state, Year) %>%
  summarize(cost_per_pack = mean(cost_per_pack, na.rm = TRUE), .groups = 'drop')

price_changes_wide <- price_changes_wide %>%
  mutate(price_change = `2018` - `1970`)

bottom5_states <- price_changes_wide %>%
  arrange(price_change) %>%
  slice_head(n = 5) %>%
  pull(state)

bottom_5_data <- final.data %>%
  filter(state %in% bottom5_states) %>%
  group_by(state, Year) %>%
  summarize(avg_sales_per_capita = mean(sales_per_capita, na.rm = TRUE), .groups = 'drop')

q4 <- ggplot(bottom_5_data, aes(x = Year, y = avg_sales_per_capita, color = state, group = state)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average Number of Packs Sold per Capita for Bottom 5 States with Lowest Price Increases (1970-2018)",
    x = "Year",
    y = "Average Packs Sold per Capita") +
  scale_color_brewer(palette = "Set1", name = "State") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"))

# Question 5: written
# Question 6: Price Elasticity of Demand
data_1970_1990 <- final.data %>%
  filter(Year >= 1970 & Year <= 1990) %>%
  drop_na(cost_per_pack, sales_per_capita)

data_1970_1990 <- data_1970_1990 %>%
  mutate(
    log_sales = log(sales_per_capita),
    log_price = log(cost_per_pack))

q6 <- lm(log_sales ~ log_price, data = data_1970_1990)
summary(q6)

# Question 7: Regression with Tax as an Instrument
if (!require("AER")) install.packages("AER")
library(AER)

data_1970_1990 <- final.data %>%
  filter(Year >= 1970 & Year <= 1990) %>%
  drop_na(cost_per_pack, sales_per_capita, tax_dollar)

data_1970_1990 <- data_1970_1990 %>%
  mutate(
    log_sales = log(sales_per_capita),
    log_price = log(cost_per_pack),
    log_tax = log(tax_dollar))

q7 <- ivreg(log_sales ~ log_price | log_tax, data = data_1970_1990)
summary(q7)

# Question 8: First Stage & Reduced Form
first_stage_8 <- lm(log_price ~ log_tax, data = data_1970_1990)
first_stage_8

reduced_form_8 <- lm(log_sales ~ log_tax, data = data_1970_1990)
summary(reduced_form_8)

# Question 9: Repeat for 1991-2015
## a) Price Elasticity for Demand
data_1991_2015 <- final.data %>%
  filter(Year >= 1991 & Year <= 2015) %>%
  drop_na(cost_per_pack, sales_per_capita)

data_1991_2015 <- data_1991_2015 %>%
  mutate(
    log_sales = log(sales_per_capita),
    log_price = log(cost_per_pack))

q9a <- lm(log_sales ~ log_price, data = data_1991_2015)
summary(q9a)

## b) Regression with Tax as an Instrument
data_1991_2015 <- final.data %>%
  filter(Year >= 1991 & Year <= 2015) %>%
  drop_na(cost_per_pack, sales_per_capita, tax_dollar)

data_1991_2015 <- data_1991_2015 %>%
  mutate(
    log_sales = log(sales_per_capita),
    log_price = log(cost_per_pack),
    log_tax = log(tax_dollar))

q9b <- ivreg(log_sales ~ log_price | log_tax, data = data_1991_2015)
summary(q9b)

## c) First Stage & Reduced Form
first_stage_9 <- lm(log_price ~ log_tax, data = data_1991_2015)
summary(first_stage_9)

reduced_form_9 <- lm(log_sales ~ log_tax, data = data_1991_2015)
summary(reduced_form_9)

## CREATE WORKSPACE
rm(list=c("final.data"))
save.image("submission1/hwk3_workspace.Rdata")

