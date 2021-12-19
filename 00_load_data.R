library(pacman)
p_load(wbstats, rio, tidyverse, magrittr)


### get data from WDI: ----
from <- 1990
to <- 2020

wdi <- wb_data(indicator = c(pop = "SP.POP.TOTL",
                             area = "AG.LND.TOTL.K2",
                             internet = "IT.NET.USER.ZS",
                             import = "BM.GSR.GNFS.CD",
                             export = "BX.GSR.GNFS.CD",
                             fdi_in = "BX.KLT.DINV.CD.WD",
                             fdi_out = "BM.KLT.DINV.CD.WD",
                             tourism_in = "ST.INT.ARVL",
                             tourism_out = "ST.INT.DPRT"),
                      start_date = from, end_date = to, 
                      return_wide = T) %>% 
  rowwise() %>% 
  # if only in OR out is not reported, it's assumed that the other is neglectable (see Schröder 2020)
  mutate(fdi = ifelse(is.na(fdi_in) && is.na(fdi_out),
                      NA,
                      sum(abs(fdi_in), abs(fdi_out), na.rm = T)),
         trade = ifelse(is.na(import) && is.na(export),
                        NA,
                        sum(import, export, na.rm = T)),
         tourism = ifelse(is.na(tourism_in) && is.na(tourism_out),
                          NA, 
                          sum(tourism_in, tourism_out, na.rm = T)))

### Indicators not available via WDI: ----
#   - International telephone traffic (ITU)
#   - International Meetings/Conferences (UIA)
#   - International aircraft passengers (ICAO)

### join data from other sources:
icao <- rio::import("data/ICAO.xlsx")

uia <- rio::import("data/UIA.xlsx",
                   na.strings = c("NA", "..")) %>% 
  pivot_longer(., cols = `1990`:`2018`,
               names_to = "Year",
               values_to = "int_meetings",
               names_transform = list(Year = as.integer))

phone <- rio::import("data/phone.xlsx", 
                     which = "total total", 
                     na.strings = c("NA", "..")) %>% 
  pivot_longer(., cols = `1990_value`:`2017_value`, 
             # string removal not very elegant!
               names_to = c("Year", "drop"), 
               values_to = "int_phone_minutes", 
               names_sep = 4, names_transform = list(Year = as.integer))


# join local data:
other_sources <- right_join(uia, full_join(phone, icao,
                                          by = c("Year", "Country" = "Name")),
                           by = c("Year", "Code" = "State")) %>% 
  select(Code, Year, Int_Departures, int_phone_minutes, int_meetings)


### join wdi and others: ----
data_raw <- full_join(wdi, other_sources,
                      by = c("date" = "Year", 
                             "iso3c" = "Code"))



### Even more valid indicators (see Schröder 2020): ----

#   - Replace air passengers with international revenue passenger kilometres (ICAO)
RPK <- rio::import("data/ICAO_RPK.xlsx", 
                   which = "Int. RPK clean", 
                   na.strings = c("NA", "..")) %>% 
  pivot_longer(., cols = `1990`:`2017`,
               names_to = "date", 
               values_to = "int_rpk",
               names_transform = list(date = as.integer))

data_raw %<>% left_join(., RPK,
                        by = c("iso3c" = "Code", "date"))

#   - Replace number of internet users with internationally transferred bandwidth (ITU)
international_internet <- rio::import("data/ITU.xlsx", 
                                      which = "int. IT bandwidth", 
                                      na.strings = c("NA", "..")) %>% 
  pivot_longer(., cols = `1990`:`2017`,
               names_to = "date", 
               values_to = "int_mbits",
               names_transform = list(date = as.integer)) %>% 
  select(-`1988`, -`1989`)

data_raw %<>% left_join(., international_internet,
                        by = c("country" = "Country", "date"))

#   - Extend Trade in goods & services with primary income (WDI)
data_raw <- wbstats::wb_data(indicator = c(
                 import_g_s_pi = "BM.GSR.TOTL.CD",
                 export_g_s_pi = "BX.GSR.TOTL.CD"),
                 start_date = from, end_date = to, 
                 return_wide = T) %>% 
  select(-iso2c, -country) %>% 
  rowwise() %>% 
    # if only in OR out is not reported, the other is assumed to be neglectable (see Schröder 2020)
  mutate(trade_g_s_pi = ifelse(is.na(import_g_s_pi) && is.na(export_g_s_pi),
                        NA,
                        sum(import_g_s_pi, export_g_s_pi, na.rm = T))) %>% 
  right_join(., data_raw,
             by = c("iso3c", "date"))


#   - create communication technology indicator reflecting technological change relevant for globalization:
#     - until 2006: phone traffic correlates highly with all other globalization indicators while internet does not
#     - from 2006: other way round, hence include:
#       - telephone traffic prior to 2006
#       - internet traffic from 2006
data_raw %<>% mutate(comtech = ifelse(date < 2006,
                                      int_phone_minutes,
                                      int_mbits))

  # however, coverage (esp. in terms of years) are not that good for some indicators
