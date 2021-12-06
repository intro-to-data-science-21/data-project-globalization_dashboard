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


### TODO:

# exclude extreme values outside mean +/- 3*IQR <- 100 / 0

# identify maximum & minimum




# per variable: maximum <- 100, minimum <- 0

# THIS IS NOT THE FINAL INDEX ONLY A PLACHOLDER TO BUILD THE DASHBOARD!!!

panel_normalize <- function(variable) {
  ((variable - min(variable, na.rm = T)) / (max(variable, na.rm = T) - min(variable, na.rm = T))) * 100
}

data_normalized <- data_pc %>% 
  mutate(across(.cols = import:int_meetings, 
                .fns = panel_normalize))


### Combining to index:
# all variables are theoretically valid, load strongly on a common factor and are highly intercorrelated (Kessler 2016, Schröder 2020)
# therfore the index can be constructed simply by taking the average of all available normalized variables:

# TODO: 
# insert variables this way (easier to make changes!)
index_variables <- list("internet", 
                        "fdi", 
                        "trade", 
                        "tourism", 
                        "Int_Departures", 
                        "int_phone_minutes", 
                        "int_meetings")

index <- data_normalized %>% 
  rowwise() %>% 
  mutate(KGI = mean(c(internet, 
                     fdi, 
                     trade, 
                     tourism, 
                     Int_Departures, 
                     int_phone_minutes, 
                     int_meetings),
                    na.rm = T)) %>% 
  bind_cols(., 
            data_pc %>% select(internet, 
                                  fdi, 
                                  trade, 
                                  tourism, 
                                  Int_Departures, 
                                  int_phone_minutes, 
                                  int_meetings) %>% 
              mutate(N_vars = rowSums(!is.na(.))) %>% 
              select(N_vars))

### save processed data:
dir.create("data_processed")
save(index, file = "data_processed/KGI.Rdata")
