# Meta --------------------------------------------------------------------
# Author:        Sammy Ramacher
# Date Created:  3/15/2025
# Date Edited:   3/17/2025
# Homework 3-1

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)

final.data <- readRDS("data/output/TaxBurden_Data.rds")

# Question 1 
final.data <- final.data %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = tax_state - lag(tax_state),
  tax_change_d = ifelse(tax_change==0,0,1),
  price_cpi_2012 = cost_per_pack*(cpi_2012/index),
  total_tax_cpi_2012=tax_dollar*(cpi_2012/index),
  ln_tax_2012=log(total_tax_cpi_2012),
  ln_sales=log(sales_per_capita),
  ln_price_2012=log(price_cpi_2012))
 

tax_change_d <- final.data %>%
  filter(Year >= 1970 & Year <= 1985)

tax_change_proportion <- tax_change_d %>%
  group_by(Year) %>%
  summarize(proportion_with_change = mean(tax_change, na.rm = TRUE))

q1 <- ggplot(tax_change_proportion, aes(x = Year, y = proportion_with_change)) +
  geom_bar(stat = "identity", fill = "#96eb87") +
  labs(title = "Proportion of States that Changed Cigarette Tax, 1970-1985",
       x = "Year",
       y = "Proportion with Tax Change") +
  theme_minimal()
 

# Question 2
avg_tax_price_data <- final.data %>%
  group_by(Year) %>%
  summarize(
    avg_tax = mean(tax_dollar, na.rm = TRUE),
    avg_price = mean(cost_per_pack, na.rm = TRUE))

q2 <- ggplot(avg_tax_price_data, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Average Tax"), size = 1.2, linetype = "solid") +
  geom_line(aes(y = avg_price, color = "Average Price"), size = 1.2, linetype = "solid") +
  scale_y_continuous(
    name = "Average Tax (2012 Dollars)",
    sec.axis = sec_axis(~ ., name = "Average Price, in 2012 Dollars")) +
  labs(
    title = "Average Cigarette Tax and Price, 1970-2018",
    x = "Year",
    color = "Legend") +  # Adds a legend title
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"),
    axis.title.y.right = element_text(size = 11, face = "bold")) +
  scale_color_manual(
    name = "Legend",  # Legend title
    values = c("Average Tax" = "#00aaff", "Average Price" = "#ffae00")
  )

# Question 3
cig.data.change <- final.data %>% ungroup() %>%
  filter(Year==1970) %>% 
  select(state, price_1970 = price_cpi_2012) %>%
  left_join(final.data %>% 
  filter(Year==2018) %>% 
  select(state, price_2018 = price_cpi_2012),
          by=c('state')) %>% 
  mutate(price_change = price_2018-price_1970)

high.change <- cig.data.change %>% slice_max(price_change, n=5) %>% mutate(change_group="high")
low.change <- cig.data.change %>% slice_min(price_change, n=5) %>% mutate(change_group="low")
change.group <- rbind(high.change, low.change)

top.bottom.price <- final.data %>% ungroup() %>%
  inner_join(change.group %>% select(state, change_group),
             by=c("state"))

q3 <- top.bottom.price %>% filter(change_group=="high") %>%
ggplot(aes(x = Year, y = sales_per_capita, color = state, group = state)) +
  stat_summary(fun="mean",geom="line")+ 
  labs(
    title = "Average Packs Sold per Capita for 5 Highest Price-Increasing States",
    x = "Year",
    y = "Average Packs Sold (per Capita)"
  ) +
  scale_color_brewer(palette = "Set1", name = "State") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"))
q3

# Question 4
q4 <- top.bottom.price %>% filter(change_group=="low") %>%
ggplot(aes(x = Year, y = sales_per_capita, color = state, group = state)) +
  stat_summary(fun="mean",geom="line")+ 
  labs(
    title = "Average Packs Sold per Capita for 5 Lowest Price-Increasing States",
    x = "Year",
    y = "Average Packs Sold (per Capita)"
  ) +
  scale_color_brewer(palette = "Set1", name = "State") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.title.y = element_text(size = 11, face = "bold"))
q4

# Question 5: written (or could include graph if wanted)
# Question 6
data_1970_1990 <- final.data %>%
  filter(Year >= 1970 & Year <= 1990) %>%
  drop_na(cost_per_pack, sales_per_capita)

data_1970_1990 <- data_1970_1990 %>%
  mutate(
    log_sales = log(sales_per_capita),
    log_price = log(cost_per_pack))

q6 <- lm(log_sales ~ log_price, data = data_1970_1990)
summary(q6)

# Question 7
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

# Question 8
first_stage_8 <- lm(log_price ~ log_tax, data = data_1970_1990)
first_stage_8

reduced_form_8 <- lm(log_sales ~ log_tax, data = data_1970_1990)
summary(reduced_form_8)

# Question 9 (a)

data_1991_2015 <- final.data %>%
  filter(Year >= 1991 & Year <= 2015) %>%
  drop_na(cost_per_pack, sales_per_capita)

data_1991_2015 <- data_1991_2015 %>%
  mutate(
    log_sales = log(sales_per_capita),
    log_price = log(cost_per_pack))

q9a <- lm(log_sales ~ log_price, data = data_1991_2015)
summary(q9a)

#(b)
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

#(c)
first_stage_9 <- lm(log_price ~ log_tax, data = data_1991_2015)
summary(first_stage_9)

reduced_form_9 <- lm(log_sales ~ log_tax, data = data_1991_2015)
summary(reduced_form_9)

## CREATE WORKSPACE
rm(list=c("final.data"))
save.image("submission1/hwk3_workspace.Rdata")

