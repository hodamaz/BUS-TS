# main author: Matt Duncho 
# as part of learning wiht ggpurrr

install.packages('ggrepel')
library(tidyverse)
library(lubridate) # functions: ymd()
library(tidyquant) # zoo:: for rolling()
library(ggrepel)
library(fs)
# 0.0 data ----
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

# 1.0 Anatomy of a function ----
# 1.1 Examining the mean() function ----
x <- c(0:10, 50, NA_real_)
x
mean(x, na.rm = TRUE)

# 1.2 Customizing a mean function ---- 
# ... when making a function, gives more flexibility to your function
mean_na_remove <- function(x, na.rm = TRUE, ...){
  avg <- mean(x, na.rm = na.rm, ...)
  return(avg)
  
}
mean_na_remove(x)

# 2.0 The two types of functions: Vectors and Data ----
# Calculate a 3 month rolling average for category_1 & category_2
# with dates aligned at last day of the month
rolling_avg_3_tbl <- bike_orderlines_tbl %>% 
  select(order_date, category_1, category_2, total_price) %>% 
  mutate(order_date = ymd(order_date)) %>% 
  
  # make a new column and to align each date to the last day of the month
  mutate(month_end = ceiling_date(order_date, unit = 'month') - period(1, unit = 'day')) %>% 
  group_by(category_1, category_2, month_end) %>% 
  
  # aggregate for the grouped data
  summarise(
    total_price = sum(total_price)
  ) %>% 
  
  # Calculate the rolling avg for the created groups
  mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = 'right')) %>% 
  ungroup() %>% 
  
  mutate(categor_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price))

rolling_avg_3_tbl

# visualize the averages
rolling_avg_3_tbl %>%  
  ggplot(aes(month_end, total_price, color = category_2)) +
  geom_point() +
  geom_line(aes(y = rolling_avg_3), size = 1) +
  facet_wrap(~ category_2, scales = 'free_y') +
  # Formatting
  theme_tq()+ 
  scale_color_tq()

# 2.1 Vector Functions ---- 
# 2.2 Data Functions ----

# 3.0 Controlling flow: if statements, messages, warnings, stops ----

# 4.0 ----
# 5.0 ----
# 6.0 ----