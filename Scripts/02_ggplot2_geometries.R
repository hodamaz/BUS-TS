# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# Types of Graphs: ggplot2 Geometries ----


library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("~/Desktop/desktop/R/Bus_science/M1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Point / Scatter Plots ----
# - Great for Continuous vs Continuous
# - Also good for Lollipop Charts (more on this in advanced plots)

# Goal: Explain relationship between order value and quantity of bikes sold

# Data Manipulation


# Scatter Plot




# 2.0 Line Plots ----
# - Great for time series
# Goal: Describe revenue by Month, expose cyclic nature
# Data Manipulation
bike_orderline_month_tbl <- bike_orderlines_tbl %>% 
  select(order_date, total_price) %>% 
  mutate(year = ymd(order_date),
         month = floor_date(year, unit = 'month')) %>% 
  group_by(month) %>% 
  summarize(revenue = sum(total_price))

# Line Plot

bike_orderline_month_tbl %>%  ggplot(aes(month, revenue)) +
  geom_line() +
  geom_smooth(method = 'loess', span = 0.2) +
  expand_limits(y=0) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = 'M'))


# 3.0 Bar / Column Plots ----
# - Great for categories

# Goal: Sales by Descriptive Category

# Data Manipulation
revenue_by_category2_tbl <- bike_orderlines_tbl %>% 
  select(category_2, total_price) %>% 
  group_by(category_2) %>% 
  summarize(revenue = sum(total_price))


# Bar Plot
revenue_by_category2_tbl %>% 
  mutate(category_2 = category_2 %>% as_factor() %>% fct_reorder(revenue)) %>% 
  ggplot(aes(category_2, revenue)) +
  geom_col() +
  coord_flip()


# 4.0 Histogram / Density Plots ----
# - Great for inspecting the distribution of a variable


# Goal: Unit price of bicycles
# Histogram


# Goal: Unit price of bicylce, segmenting by frame material
# Histogram


# Density
bike_orderlines_tbl %>%  
  ggplot(aes(price)) +
  geom_histogram()
  



# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions


# Goal: Unit price of models, segmenting by category 2

# Data Manipulation


# Box Plot


# Violin Plot & Jitter Plot







# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation


# Adding text to bar chart


# Filtering labels to highlight a point





