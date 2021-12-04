library(pacman)
p_load(wbstats, rio)


### get data:
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
  mutate(fdi = abs(fdi_in) + abs(fdi_out),
         trade = import + export,
         tourism = tourism_in + tourism_out)

### Indicators not available via WDI:
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


# join wdi and others:
data_raw <- full_join(wdi, other_sources,
                      by = c("date" = "Year", "iso3c" = "Code"))



### Even more valid indicators could be:
#   - Exclude internet prior to 2006
#   - Exclude phone from 2006
#   - Replace air passengers with international revenue passenger kilometres (ICAO)
#   - Extend Trade to primary income (WDI)
#   - Replace number of internet users with internationally transferred bandwidth (ITU)
