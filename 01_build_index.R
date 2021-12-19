source("00_load_data.R")
p_load(tidyverse, 
       magrittr, 
       stats, 
       magrittr)
options(scipen = 999)

### normalize data:

# by population:
data_pc <- data_raw %>% 
  select(country, 
         iso3c, 
         date, 
         area, 
         pop, 
         import,
         int_rpk:comtech,
         everything(),
         -iso2c) %>% 
  mutate(across(import:int_meetings, 
                ~ .x / pop)) %>% 
  filter(!is.na(iso3c) && !is.na(country)) # excluding rows with no information
  



### panel normalization:

# exclude small states (as defined by Kessler 2016) for normalization
data_pc %<>% 
  mutate(small = if_else(pop < 1000000 | area < 3000,
                         T,
                         F),
# There are some NAs for variable small:
  # NAs <- filter(data_pc, is.na(small))
  # some years for Kosovo, South Sudan, Eritrea, and Kuwait, none of them being a small state according to the definition
        small = ifelse(is.na(small),
                F,
                small))

# get long df:
data_pc_long <- data_pc %>% 
  pivot_longer(cols = import:int_meetings, 
               names_to = "variable", 
               values_to = "value")


# only relevant variables 
normal_range <- data_pc %>% 
  # for now, we use all years. This is to allow to adapt code later to avoid biases induced by skewed patterns of data availability:
  filter(., between(date, 1990, 2020)) %>% 
  select(import:int_meetings)

distribution_step1 <- as.data.frame(apply(normal_range, 2, summary)) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(iqr = `3rd Qu.`- `1st Qu.`) %>% 
  select(lower_quartile = `1st Qu.`, 
         upper_quartile = `3rd Qu.`, 
         iqr) %>% 
  rownames_to_column(var = "variable") %>% 
  right_join(., data_pc_long,
             by = "variable")

# exclude small states and extreme outliers before defining max/min: 
distribution_step2 <- distribution_step1 %>% 
  mutate(value = ifelse(value > upper_quartile + 3 * iqr | 
                        value < lower_quartile - 3 * iqr |
                        small == T,
                        NA,
                        value)) %>% 
  select(variable, value) %>% 
  rownames_to_column(var = "unique_identifier_i_actually_dont_need") %>% 
  pivot_wider(names_from = "variable",
              values_from = "value") %>% 
  select(-unique_identifier_i_actually_dont_need) # not pretty but works as well

# join data with max/min
distribution <- as.data.frame(apply(distribution_step2, 2, summary)) %>% 
  t() %>% 
  as.data.frame() %>% 
  select(minimum = Min., 
         maximum = Max.) %>% 
  rownames_to_column(var = "variable")

data_pc_long %<>% left_join(., distribution, 
                            by = "variable")

# exclude extreme outlier values outside between(25% quantile - 3 * IQR, 75% quantile + 3 * IQR)(see Schröder 2020):
data_normalized_long <- data_pc_long %>%  
  mutate(normalized = ((value - minimum) / (maximum - minimum)) * 100) %>% 
         # set outliers to 0 / 100:
         mutate(normalized = case_when(normalized > 100 ~ 100,
                                       normalized < 0 ~ 0,
                                       TRUE ~ normalized))


indicators <- c("internet", "fdi", "trade", "tourism", "Int_Departures", "int_phone_minutes", "int_meetings", "comtech", "int_rpk", "trade_g_s_pi")
data_normalized <- data_pc %>% 
  select(country:pop, small)

for (i in 1:length(indicators)){
  data_normalized %<>% 
    bind_cols(., data_normalized_long %>% 
                  filter(variable == indicators[i]) %>% 
                  select(normalized))
  }

data_normalized %<>% rename("internet" = normalized...7, 
                            "fdi" = normalized...8, 
                            "trade" = normalized...9, 
                            "tourism" = normalized...10, 
                            "Int_Departures" = normalized...11, 
                            "int_phone_minutes" = normalized...12, 
                            "int_meetings" = normalized...13,
                            "comtech" = normalized...14, 
                            "int_rpk" = normalized...15, 
                            "trade_g_s_pi" = normalized...16)

### Combining to index ----

# all variables are theoretically valid, load strongly on a common factor and are highly intercorrelated (Kessler 2016, Schröder 2020)
# therefore the index can be constructed simply by taking the average of all available normalized variables:

index <- data_normalized %>% 
  rowwise() %>% 
  mutate(KGI_original = mean(c(internet, 
                               fdi, 
                               trade, 
                               tourism, 
                               int_rpk, 
                               int_phone_minutes, 
                               int_meetings),
                              na.rm = T),
         KGI_new = mean(c(comtech, 
                          fdi, 
                          trade_g_s_pi, 
                          tourism,
                          int_rpk,
                          int_meetings),
                        na.rm = T)) %>% 
  bind_cols(., 
            apply(data_normalized %>% 
              select(internet:int_meetings), 
              1, 
              function(x) sum(!is.na(x))), 
            apply(data_normalized %>% 
              select(fdi, tourism, int_meetings:trade_g_s_pi), 
                     1, 
                     function(x) sum(!is.na(x)))) %>% 
  rename(n_vars_original = ...19,
         n_vars_new = ...20)

### save processed data ----
dir.create("data_processed")
save(index, file = "data_processed/KGI.Rdata")
