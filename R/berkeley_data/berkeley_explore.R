library(tidyverse)


temp_data <- readr::read_delim(file="./data/data.txt", skip = 111, delim = "\t",
                  col_names = F, col_types = "iicnnii", na = "-99") %>% 
  set_names(c("station_id","series_number","date","temperature",
              "uncertainty","observations","time")) %>% 
  separate(date, into = c("year","month"), sep="\\.", convert = T, remove=F) %>% 
  mutate(month=round(month*12/1000+.5))


# Station ID, Station Name, Latitude, Longitude, Elevation (m), Lat. Uncertainty, 
# Long. Uncertainty, Elev. Uncertainty (m), Country, State / Province Code, County, 
# Time Zone, WMO ID, Coop ID, WBAN ID, ICAO ID, # of Relocations, # Suggested Relocations,
# of Sources, Hash

readr::read_delim(file="./data/site_detail.txt", skip=149, delim="\t", 
                  col_names = F, na=c("-99","-9999"), 
                  col_types = "icnnnnnncccciiiiiiic") %>% 
  set_names(c("station_id","station_name","latitude","longitudo","elevation","lat_uncert",
              "lon_uncert","elev_uncert","country","state","county","timezone","wmo_id",
              "coop_id","wban_id","icao_id","relocations","suggested_relocations","sources","hash")) %>% 
  glimpse()


