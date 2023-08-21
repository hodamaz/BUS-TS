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
class_detect <- function(x){
  if (is.numeric(x)){
    message('Value is numeric')
    print(x)
  } else if (is.character(x)){
    warning('Value is character, please change type!', call. = FALSE)
    print(x)
  } else if(is.logical(x)){
    stop('In class_detect(): Value is logical!!! Not accepted!', call. = FALSE)
    print(x)
  } else {
    message('Unknown class')
    print(x)
  }
}

1 %>% class_detect()
'a' %>% class_detect()
TRUE %>% class_detect()
formula(y ~ x)
# 4.0 Vectorised detect outliers function ----
# -Box plot diagram to identify outliers
# -Goal: Use box plot approach to identify outliers

# Make bikes_tbl
bikes_tbl <- bike_orderlines_tbl %>% 
  distinct(model, category_1, price)

bikes_tbl

# Visualize 
bikes_tbl %>% 
  ggplot(aes(category_1, price)) +
  geom_boxplot()

# carete detect_outliers()
x <- c(0:10, 50, NA_real_)
x

detect_outliers <- function(x){
  
  if(missing(x)) stop('The argument x needs a vector.')
  
  if(!is.numeric(x)) stop(' The argument x must be numeric')
  
  data_tbl <- tibble(data = x)
  
  limits_tbl <- data_tbl %>% 
    summarise(
      quantile_lo = quantile(data, probs = 0.25, na.rm = TRUE),
      quantile_hi = quantile(data, probs = 0.75, na.rm = TRUE),
      iqr         = IQR(data, na.rm = TRUE),
      limit_lo    = quantile_lo - 1.5 * iqr,
      limit_hi    = quantile_hi + 1.5 * iqr
  )
  output_tbl <- data_tbl %>% 
    mutate(outlier = case_when(
      data < limits_tbl$limit_lo ~ TRUE,
      data > limits_tbl$limit_hi ~ TRUE,
      TRUE ~ FALSE
    ))
  return(output_tbl$outlier)
}

detect_outliers()
detect_outliers('a')
detect_outliers(x)

tibble(x = x) %>% 
 mutate(outliers = detect_outliers(x))

# apply the detect_outliers() to bikes_tbl
bike_outliers_tbl <- bikes_tbl %>% 
  
  group_by(category_1) %>% 
  mutate(outlier = detect_outliers(price)) %>% 
  ungroup()
bike_outliers_tbl

# Visualize with detect_outliers
bike_outliers_tbl %>% 
  
  ggplot(aes(category_1, price))+
  geom_boxplot() +
  ggrepel::geom_label_repel(aes(label = model),
                            color = 'red',
                            size  = 3,
                            data = . %>% 
                              filter(outlier))

# 5.0 DATA FUNCTION: FEATURE ENGINEERING ----

#  - Goal: Want to simplify the text feature engineering steps to convert model name to features



# Pipeline Comes From 02_data_wrangling/04_text.R
bikes_tbl %>%
  
  select(model) %>%
  
  # Fix typo
  mutate(model = case_when(
    model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
    model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
    model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
    TRUE ~ model
  )) %>%
  
  # separate using spaces
  separate(col     = model, 
           into    = str_c("model_", 1:7), 
           sep     = " ", 
           remove  = FALSE, 
           fill    = "right") %>%
  
  # creating a "base" feature
  mutate(model_base = case_when(
    
    # Fix Supersix Evo
    str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
    
    # Fix Fat CAAD bikes
    str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
    
    # Fix Beast of the East
    str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
    
    # Fix Bad Habit
    str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
    
    # Fix Scalpel 29
    str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
    
    # catch all
    TRUE ~ model_1)
  ) %>%
  
  # Get "tier" feature
  mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
  
  # Remove unnecessary columns
  select(-matches("[0-9]")) %>%
  
  # Create Flags
  mutate(
    black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
    hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
    team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
    red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
    ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
    dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
    disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
  )

bikes_tbl

separats_bike_model <- function(data, keep_model_column = TRUE, append = TRUE){
  # Append
  if (!append){
    data <- data %>% select(model)
  }
  # Pipeline
  output_tbl <- data %>%
    
    # select(model) %>%
    
    # Fix typo
    mutate(model = case_when(
      model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
      model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
      model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
      TRUE ~ model
    )) %>%
    
    # separate using spaces
    separate(col     = model, 
             into    = str_c("model_", 1:7), 
             sep     = " ", 
             remove  = FALSE, 
             fill    = "right") %>%
    
    # creating a "base" feature
    mutate(model_base = case_when(
      
      # Fix Supersix Evo
      str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
      
      # Fix Fat CAAD bikes
      str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
      
      # Fix Beast of the East
      str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
      
      # Fix Bad Habit
      str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
      
      # Fix Scalpel 29
      str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
      
      # catch all
      TRUE ~ model_1)
    ) %>%
    
    # Get "tier" feature
    mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
    
    # Remove unnecessary columns
    select(-matches("[0-9]")) %>%
    
    # Create Flags
    mutate(
      black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
      hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
      team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
      red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
      ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
      dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
      disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
    )
  
}

# 6.0 ----














