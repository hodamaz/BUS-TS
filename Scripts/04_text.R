# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TEXT MANIPULATION ----

library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)

bike_orderlines_tbl <- read_rds("~/Desktop/desktop/R/Bus_science/M1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl

bikes_tbl <- readxl::read_excel("~/Desktop/desktop/R/Bus_science/M1/DS4B_101_R_Business_Analysis/00_data/bike_sales/data_raw/bikes.xlsx")

bikes_tbl


# 1.0 Basics ----

# 1.1 Detection: Used with filter() ----

# Vector (vectorized approach)

c("Supersix Evo Black Inc.", "Supersix Evo Hi-Mod Team") %>% 
    str_detect(pattern = "Supersix")

# Tibble (approach doing with tible) (This is part of feature engineering)
bikes_tbl %>% 
    select(model) %>% 
    mutate(supersix = model %>% str_detect("Supersix") %>% # flagged
               as.numeric()) %>% 
    mutate(black = model %>% str_detect("Black") %>%  as.numeric())  
    
# bikes_tbl %>% 
#   select(model) %>% 
#   mutate(red = model %>% str_detect(pattern = 'Red') %>% as.numeric()) %>% view()

# 1.2 Case & Concatenation ----


# Case
bikeshop_name <- "Ithaca Mountain Climbers"
str_to_upper(bikeshop_name)
str_to_lower(bikeshop_name)
str_to_title(bikeshop_name)

# Concatenation

# Vector
order_id <- 1
order_line <- 1

str_c("Order Line:", order_id, ".", order_line, 
      " sent to Customer: ", bikeshop_name)

str_glue("Order Line:{order_id}.{order_line} sent to Customer: {str_to_upper(bikeshop_name)}")

# Tibble
bike_orderlines_tbl %>% 
    select(bikeshop_name, order_id, order_line) %>% 
    mutate(purchase_statement = str_glue(
        "Order Line:{order_id}.{order_line} sent to Customer: {str_to_upper(bikeshop_name)}" 
    )%>% as.character()) 

# 1.3 Separating Text: See tidyr::separate() ----

# Vector
c("Road - Elite Road - Carbon", "Road - Elite Road") %>% 
    str_split(pattern = " - ", simplify = TRUE) # simplify returns a matrix, with empty strings when no value is given

# Tibble
bikes_tbl %>% 
    separate(col    = description, # from tidyr
             into   = c("category_1", "category_2", "category_3"),
             sep    = " - ",
             remove = FALSE)


# 1.4 Trimming Text ----

" text with space   " %>% str_trim(side = "both") # "both" and "left"

# 1.5 Replacement: Used with mutate() [and optionally case_when()] ----

# Vector
c("CAAD12", "CAAD", "CAAD8") %>% str_replace(pattern = "[0-9]", replacement = "") # finds the FIRST pattern
# one number was retained, it only looks for one instance
c("CAAD12", "CAAD", "CAAD8") %>% str_replace_all(pattern = "[0-9]", replacement = "X")

# Tibble
bikes_tbl %>% 
    select(model) %>% 
    mutate(model_num_removed = model %>% str_replace_all("[0-9]", ""))
# as you can see there are some spaces at the end where num are removed
bikes_tbl %>% 
    select(model) %>% 
    mutate(model_num_removed = model %>% str_replace_all("[0-9]", "") %>% str_trim())

# 1.6 Formatting Numbers ----
# 
value <- 1e6
(value) %>% scales::number(prefix = "$", suffix = "M")
(value / 1e6) %>% scales::number(prefix = "$", suffix = "M")
value %>% scales::number(prefix = "$", big.mark = ",")
value %>% scales::dollar(scale = 1/1e6, suffix = "M") # scale value will be multiplied

pct <- 0.15
pct %>% scales::number(scale = 100, suffix = "%")
pct %>% scales::percent()

# 1.7 Formatting Column Names ----

# Replacing text in column names
bike_orderlines_tbl %>% 
    set_names(names(.) %>% str_replace("_", ".") %>% str_to_upper())

# Appending text to column names
bike_orderlines_tbl %>% 
    set_names(str_glue("{names(.)}_bike")) # from purrr

# Appending text to specific column names
bike_orderlines_columns_tbl <- 
    bike_orderlines_tbl %>% 
    rename_at(.vars = vars(model:frame_material), # select from model to frame_material/ from dplyr
              .funs = ~ str_c("prod_", .)) %>% 
    rename_at(.vars = vars(bikeshop_name:state),
              ~ str_c("cust_", .)) # pass anonymouse function

bike_orderlines_columns_tbl

bike_orderlines_columns_tbl %>% 
    select(contains("cust_"), total_price, price)
# 2.0 Feature Engineering with Text ----- (real world experience)
# Investigating "model" and extracting well-formatted features

bikes_tbl %>% 
  select(model) %>% 
  #fix a typo
  mutate(model = case_when(
    model == 'CAAD Disc Ultegra'    ~ 'CAAD12 Disc Ultegra',
    model == 'Syapse Carbon Tiagra' ~ 'Synapse Carbon Tiagra',
    model == 'Hi-Mod Utegra' ~ 'Hi-Mod Ultegra',
    TRUE ~ model
  )) %>% 
  
  # Separate using spaces
  separate(col = model, 
           into = str_c('model_', 1:7), # all features have been broken out, str_c() for col names
           sep = ' ', remove = FALSE, 
           fill = 'right', extra = 'drop') %>%  # Fill: anything to the right of the columns is gonna be filled with NAs
  
  mutate(model_base = case_when(
    # fix Supersix Evo
    # detect (supersix) ~ combine (model_1 , model_2, sep = ' ')
    str_detect(str_to_lower(model_1), 'supersix') ~ str_c(model_1, model_2, sep = ' '),
    
    # fix fat cadd 
    str_detect(str_to_lower(model_1), 'fat') ~ str_c(model_1, model_2, sep = ' '),
    
    # fix beast of the east
    str_detect(str_to_lower(model_1), 'beast') ~ str_c(model_1, model_2, model_3, model_4, sep = ' '),
    
    # fix bad habbit
    str_detect(str_to_lower(model_1), 'bad') ~ str_c(model_1, model_2, sep = ' '),
    
    # fix bad Scalpel
    str_detect(str_to_lower(model_2), '29') ~ str_c(model_1, model_2, sep = ' '),
    
    # catch all
    TRUE ~ model_1
  )) %>% 
  # Get 'tier' feature
  mutate(model_tier = model %>% str_replace(model_base, '') %>% str_trim()) %>% 
  
  # remove un-neccessary columns
  select(- matches('[0-9]')) %>%  # matches() let's you use 'regex', any column with numbers is eliminated
  
  # Create flags
  mutate(
    # detect black in tier %>% numeric flag
    black    = model_tier %>% str_to_lower() %>% str_detect("black") %>%  as.numeric(),
    hi_mode  = model_tier %>% str_to_lower() %>% str_detect('hi-mod') %>%  as.numeric(),
    team     = model_tier %>% str_to_lower() %>% str_detect('team') %>%  as.numeric(),
    red      = model_tier %>% str_to_lower() %>% str_detect('red') %>%  as.numeric(),
    ultegra  = model_tier %>% str_to_lower() %>% str_detect('ultegra') %>%  as.numeric(),
    dura_ace = model_tier %>% str_to_lower() %>% str_detect('dura ace') %>%  as.numeric(),
    disc     = model_tier %>% str_to_lower() %>% str_detect('disc') %>%  as.numeric()
    ) %>% 
  
  view()


















