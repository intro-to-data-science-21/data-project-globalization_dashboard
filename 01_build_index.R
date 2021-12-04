source("00_load_data.R")
p_load(tidyverse, magrittr)

### normalize data:

# by population:
data_pc <- data_raw %>% 
  select(country, iso3c, date, area, pop, internet, everything(), -iso2c) %>% 
  mutate(across(import:int_meetings, ~ .x / pop))



### panel normalization:

# exclude small states for normalization
data_pc %<>% 
  mutate(small = if_else(pop < 1000000 | area < 3000,
                         T,
                         F))

# per variable: maximum <- 100, minimum <- 0

# exclude extreme values outside mean +/- 3*IQR <- 100 / 0

# identify maximum & minimum

