# DS4B 101-R: R FOR BUSINESS ANALYSIS ---- 
# ADVANCED BUSINESS PLOTS ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("~/Desktop/desktop/R/Bus_science/M1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Lollipop Chart: Top N Customers ----
# - Great for showing order

# Question: How much purchasing power is in top 5 customers?
# Goal: Visualize top N customers in terms of Revenue, include cumulative percentage

n <- 10

# Data Manipulation

bike_orderlines_tbl %>% select(bikeshop_name, total_price) %>% 
  mutate(bikeshop_name = as_factor(bikeshop_name) %>% fct_lump(n = n, w = total_price)) %>% 
  
  group_by(bikeshop_name) %>% 
  summarize(revenue = sum(total_price))

# Data Visualization





# 2.0 Heatmaps ----
# - Great for showing details in 3 dimensions

# Question: Do specific customers have a purchasing prefernce?
# Goal: Visualize heatmap of proportion of sales by Secondary Product Category

# Data Manipulation


# Data Visualization




