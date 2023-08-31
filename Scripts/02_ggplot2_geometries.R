# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# Types of Graphs: ggplot2 Geometries ----
# Main author: Matt Dancho, some code is added: hodamaz

library(tidyverse)
library(lubridate)
library(tidyquant)
library(patchwork)

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
bike_orderlines_tbl %>%  
  ggplot(aes(price)) +
  geom_histogram()

bike_orderlines_tbl %>% 
  distinct(price, frame_material) %>% 
  ggplot(aes(price, fill = frame_material))+
  geom_histogram()+
  facet_wrap(~frame_material, ncol = 1)+
  scale_fill_tq()+
  theme_tq()

# Density and diff btw select() and distinct()
p1 <- bike_orderlines_tbl %>% 
  select(price, frame_material) %>% 
  ggplot(aes(price, fill = frame_material))+
  geom_density(alpha = 0.5)+
  scale_fill_tq()+
  theme_tq() +
  labs(title = "select function")
  
p2 <- bike_orderlines_tbl %>% 
  distinct(price, frame_material) %>% 
  ggplot(aes(price, fill = frame_material))+
  geom_density(alpha = 0.5)+
  scale_fill_tq()+
  theme_tq()+
  labs(title = "distinct function")

p1 + p2
# 5.0 Box Plot / Violin Plot ----
# - Great for comparing distributions


# Goal: Unit price of models, segmenting by category 2

# Data Manipulation
unit_price_by_cat_2_tbl <- bike_orderlines_tbl %>% 
  distinct(category_2, price, model) %>% 
  mutate(category_2 = as_factor(category_2) %>% fct_reorder(price)) 
  

# Box Plot
p1 <- unit_price_by_cat_2_tbl %>% 
  
  ggplot(aes(price, category_2)) +
  geom_boxplot()+
  theme_tq()

p2 <- unit_price_by_cat_2_tbl %>% 
  
  ggplot(aes(category_2, price)) +
  geom_boxplot()+
  coord_flip()+
  theme_tq() +
  labs(title = "coord_flip")
p1 + p2

# Violin Plot & Jitter Plot

unit_price_by_cat_2_tbl %>% 
  ggplot(aes(category_2, price))+
  
  geom_violin() +
  geom_jitter(alpha = 0.5, width = 0.15, color = '#2c3e50') +
  
  coord_flip() +
  theme_tq()





# 6.0 Adding Text & Labels ----

# Goal: Exposing sales over time, highlighting outlier

# Data Manipulation
bike_orderline_per_year <- bike_orderlines_tbl %>% 
  select(order_date, total_price) %>% 
  mutate(year = year(order_date)) %>% 
  group_by(year) %>% 
  summarise(revenue = sum(total_price))
  
# Adding text to bar chart
bike_orderline_per_year %>% 
  ggplot(aes(year, revenue)) +
  
  geom_col(fill = '#2c3e50') +
  geom_smooth(method = 'lm', se = FALSE) +
  
  geom_label(aes(label = scales::dollar(revenue, scale = 1e-6, suffix = 'M')),
             vjust = -0.5,
             size = 5)+
  geom_label(label = "Major demand this year",
             vjust = -4,
             size = 4,
             fontface = 'italic',
             fill = '#1f78b4',
             color = 'white',
             data = bike_orderline_per_year %>% 
               filter(year %in% c('2013', '2015')))+
  
  expand_limits(y = 2e7) +
  theme_tq() 
  


# Filtering labels to highlight a point





