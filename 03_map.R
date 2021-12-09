#Do not use this, see 03_plotlymap

library(leaflet)
library(tidyverse)
library(sp)
library(raster)
library(stringr)
library(pacman)
p_load(rio)
KGI <- rio::import(file = "data_processed/KGI.Rdata") 

getData("ISO3")
# chi(?), kxk(kosovo)
w_iso <- c(ABW="ABW", AFG="AFG", AGO="AGO", ALB="ALB", AND="AND",
           ARE="ARE", ARG="ARG", ARM="ARM", ASM="ASM", ATG="ATG",
           AUS="AUS", AUT="AUT", AZE="AZE", BDI="BDI", BEL="BEL",
           BEN="BEN", BFA="BFA", BGD="BGD", BGR="BGR", BHR="BHR",
           BHS="BHS", BIH="BIH", BLR="BLR", BLZ="BLZ", BMU="BMU",
           BOL="BOL", BRA="BRA", BRB="BRB", BRN="BRN", BTN="BTN",
           BWA="BWA", CAF="CAF", CAN="CAN", CHE="CHE", 
           CHL="CHL", CHN="CHN", CIV="CIV", CMR="CMR", COD="COD",
           COG="COG", COL="COL", COM="COM", CPV="CPV", CRI="CRI",
           CUB="CUB", CUW="CUW", CYM="CYM", CYP="CYP", CZE="CZE",
           DEU="DEU", DJI="DJI", DMA="DMA", DNK="DNK", DOM="DOM",
           DZA="DZA", ECU="ECU", EGY="EGY", ERI="ERI", ESP="ESP",
           EST="EST", ETH="ETH", FIN="FIN", FJI="FJI", FRA="FRA",
           FRO="FRO", FSM="FSM", GAB="GAB", GBR="GBR", GEO="GEO",
           GHA="GHA", GIB="GIB", GIN="GIN", GMB="GMB", GNB="GNB",
           GNQ="GNQ", GRC="GRC", GRD="GRD", GRL="GRL", GTM="GTM",
           GUM="GUM", GUY="GUY", HKG="HKG", HND="HND", HRV="HRV",
           HTI="HTI", HUN="HUN", IDN="IDN", IMN="IMN", IND="IND",
           IRL="IRL", IRN="IRN", IRQ="IRQ", ISL="ISL", ISR="ISR",
           ITA="ITA", JAM="JAM", JOR="JOR", JPN="JPN", KAZ="KAZ",
           KEN="KEN", KGZ="KGZ", KHM="KHM", KIR="KIR", KNA="KNA",
           KOR="KOR", KWT="KWT", LAO="LAO", LBN="LBN", LBR="LBR",
           LBY="LBY", LCA="LCA", LIE="LIE", LKA="LKA", LSO="LSO",
           LTU="LTU", LUX="LUX", LVA="LVA", MAC="MAC", MAF="MAF",
           MAR="MAR", MCO="MCO", MDA="MDA", MDG="MDG", MDV="MDV",
           MEX="MEX", MHL="MHL", MKD="MKD", MLI="MLI", MLT="MLT",
           MMR="MMR", MNE="MNE", MNG="MNG", MNP="MNP", MOZ="MOZ",
           MRT="MRT", MUS="MUS", MWI="MWI", MYS="MYS", NAM="NAM",
           NCL="NCL", NER="NER", NGA="NGA", NIC="NIC", NLD="NLD",
           NOR="NOR", NPL="NPL", NRU="NRU", NZL="NZL", OMN="OMN",
           PAK="PAK", PAN="PAN", PER="PER", PHL="PHL", PLW="PLW",
           PNG="PNG", POL="POL", PRI="PRI", PRK="PRK", PRT="PRT",
           PRY="PRY", PSE="PSE", PYF="PYF", QAT="QAT", ROU="ROU",
           RUS="RUS", RWA="RWA", SAU="SAU", SDN="SDN", SEN="SEN",
           SGP="SGP", SLB="SLB", SLE="SLE", SLV="SLV", SMR="SMR",
           SOM="SOM", SRB="SRB", SSD="SSD", STP="STP", SUR="SUR",
           SVK="SVK", SVN="SVN", SWE="SWE", SWZ="SWZ", SXM="SXM",
           SYC="SYC", SYR="SYR", TCA="TCA", TCD="TCD", TGO="TGO",
           THA="THA", TJK="TJK", TKM="TKM", TLS="TLS", TON="TON",
           TTO="TTO", TUN="TUN", TUR="TUR", TUV="TUV", TZA="TZA",
           UGA="UGA", UKR="UKR", URY="URY", USA="USA", UZB="UZB",
           VCT="VCT", VEN="VEN", VGB="VGB", VIR="VIR", VNM="VNM",
           VUT="VUT", WSM="WSM", YEM="YEM", ZAF="ZAF",
           ZMB="ZMB", ZWE="ZWE")
world <- list()

for(country_code in w_iso){
  world[[country_code]] <- getData("GADM", country=w_iso[country_code], level=0)
}

countries_SPDF <- rbind(world$ABW,world$AFG,world$AGO,world$ALB,world$AND,world$ARE,
                        world$ARG,world$ARM,world$ASM,world$ATG,world$AUS,world$AUT,
                        world$AZE,world$AZE,world$BDI,world$BEL,world$BEN,world$BFA,
                        world$BGD,world$BGR,world$BHR,world$BHS,world$BIH,world$BLR,
                        world$BLZ,world$BMU,world$BOL,world$BRA,world$BRB,world$BRN,
                        world$BTN,world$BWA,world$CAF,world$CAN,world$CHE,world$CHL,
                        world$CHL,world$CHN,world$CIV,world$CMR,world$COD,world$COL,
                        world$COM,world$CPV,world$CRI,world$CUB,world$CUW,world$CYM,
                        world$CYP,world$CZE,world$DEU,world$DJI,world$DMA,world$DNK,
                        world$DOM,world$DZA,world$ECU,world$EGY,world$ERI,world$ESP,
                        world$EST,world$ETH,world$FIN,world$FJI,world$FRA,world$FRO,
                        world$FSM,world$GAB,world$GBR,world$GEO,world$GHA,world$GIB,
                        world$GIN,world$GMB,world$GNB,world$GNQ,world$GRC,world$GRD,
                        world$GRL,world$GTM,world$GUM,world$GUY,world$HKG,world$HND,
                        world$HRV,world$HTI,world$HUN,world$IDN,world$IMN,world$IND,
                        world$IRL,world$IRN,world$IRQ,world$ISL,world$ISR,world$ITA,
                        world$JAM,world$JOR,world$JPN,world$KAZ,world$KEN,world$KGZ,
                        world$KHM,world$KIR,world$KNA,world$KOR,world$KWT,world$LAO,
                        world$LBN,world$LBR,world$LBY,world$LCA,world$LIE,world$LKA,
                        world$LSO,world$LTU,world$LUX,world$LVA,world$MAC,world$MAF,
                        world$MAR,world$MCO,world$MDA,world$MDG,world$MDV,world$MEX,
                        world$MHL,world$MKD,world$MLI,world$MLT,world$MMR,world$MNE,
                        world$MNG,world$MNP,world$MOZ,world$MRT,world$MUS,world$MWI,
                        world$MYS,world$NAM,world$NCL,world$NER,world$NGA,world$NIC,
                        world$NLD,world$NOR,world$NPL,world$NRU,world$NZL,world$OMN,
                        world$PAK,world$PAN,world$PER,world$PHL,world$PLW,world$PNG,
                        world$POL,world$PRI,world$PRK,world$PRT,world$PRY,world$PSE,
                        world$PYF,world$QAT,world$ROU,world$RUS,world$RWA,world$SAU,
                        world$SDN,world$SEN,world$SGP,world$SLB,world$SLE,world$SLV,
                        world$SMR,world$SOM,world$SRB,world$SSD,world$STP,world$SUR,
                        world$SVK,world$SVN,world$SWE,world$SWZ,world$SXM,world$SYC,
                        world$SYR,world$TCA,world$TCD,world$TGO,world$THA,world$TJK,
                        world$TKM,world$TLS,world$TON,world$TTO,world$TUN,world$TUR,
                        world$TUV,world$TZA,world$UGA,world$UKR,world$URY,world$USA,
                        world$UZB,world$VCT,world$VEN,world$VGB,world$VIR,world$VNM,
                        world$VUT,world$WSM,world$YEM,world$ZAF,world$ZMB,world$ZWE)

#color numeric map
col_pal_q <- colorNumeric(palette = "Blues", domain = KGI$KGI)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = countries_SPDF,
              stroke = FALSE,
              fillColor = ~col_pal_q(KGI$KGI),
              popup = paste("Country:", KGI$country, "<br>",
                            "KGI:", KGI$KGI, "<br>"),
              fillOpacity = 0.9,
              color = "white",
              weight = 0.3) %>% 
  addLegend(position= "bottomleft", pal = col_pal_q, values = KGI$KGI, title = "KGI by country")

#basic map
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = countries_SPDF, stroke = FALSE)

# Colors quantile map
col_pal_q <- colorQuantile(palette = "BuPu", domain = KGI$KGI)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = countries_SPDF,
              stroke = FALSE,
              fillColor = ~col_pal_q(KGI$KGI),
              popup = paste("Country:", KGI$country, "<br>",
                            "KGI:", KGI$KGI, "<br>"),
              fillOpacity = 0.9,
              color = "white",
              weight = 0.3) %>% 
  addLegend(position= "bottomleft", pal = col_pal_q, values = KGI$KGI, title = "KGI by country")

