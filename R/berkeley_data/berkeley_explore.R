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

site_data <- readr::read_delim(file="./data/site_detail.txt", skip=149, delim="\t", 
                  col_names = F, 
                  na=c("-99","-9999","[Missing]                               ",""), 
                  col_types = "icnnnnnncccciiiiiiic") %>% 
  set_names(c("station_id","station_name","latitude","longitude","elevation","lat_uncert",
              "lon_uncert","elev_uncert","country","state","county","timezone","wmo_id",
              "coop_id","wban_id","icao_id","relocations","suggested_relocations","sources","hash")) %>% 
  mutate_if(is.character, str_trim) %>% 
  mutate_at(vars(country, state, county), as.factor)

brazil_sites <- site_data %>% 
  filter(country=="Brazil")

library(yaml)
library(ggmap)
library(ggrepel)

google <- yaml::read_yaml("./config/google_keys.yaml")
ggmap::register_google(google$key)

bbox <- make_bbox(lon = brazil_sites$longitude, lat = brazil_sites$latitude, f = .1)
gmap <- get_map(location=bbox, source="google", maptype="terrain")

ggmap(gmap) +
  geom_point(data=brazil_sites, aes(x=longitude, y=latitude)) +
  geom_text_repel(data=brazil_sites, aes(x=longitude, y=latitude, label=station_name), 
                  size=3) +
  theme_void()
  +#geom_text(data=brazil_sites, aes()) +
  


geom_text
