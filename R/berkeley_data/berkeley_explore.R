library(tidyverse)
library(broom)
library(ggrepel)

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
                  size=2) +
  theme_void()


br_temp <- brazil_sites %>% 
  inner_join(temp_data, by = "station_id") %>% 
  select(station_id, station_name, latitude, longitude, 
         elevation, country, year, temperature)

br_temp %>% 
  filter(station_name=="GOIANIA") %>% 
  group_by(country, year) %>% 
  summarise( temp_avg = mean(temperature, na.rm = T)) %>% 
  ungroup() %>% 
  filter(year>=max(year)-100) %>% 
  mutate(temp_avg=temp_avg-mean(temp_avg)) %>% 
  # ggplot(aes(x=year, y=temp_avg)) +
  # geom_point() + 
  # theme_minimal()
  ggplot(aes(x=year, fill=temp_avg, color=temp_avg)) +
    geom_bar(aes(y=1), stat="identity",
             position = position_dodge2(width = 0, padding=0)) +
    scale_fill_gradient2(low="darkblue", mid="lightblue", high="red") +
    scale_color_gradient2(low="darkblue", mid="lightblue", high="red") +
    theme_void() +
    theme(
      legend.position = "none",
      panel.border = element_blank()
    )

br_temp %>% 
  group_by(station_id, station_name, year) %>%
  top_n(year,100) %>% 
  summarise(temp_avg=mean(temperature, rm.na=T)) %>% 
  nest() %>% 
  mutate(model=map(data, function(.x){
    lm(temp_avg~year, .x)
  })) %>% 
  mutate(m.values=map(model, function(.x){
    bind_cols(
      .x %>% 
        tidy() %>% 
        filter(term=="year") %>% 
        select(slope=estimate),
      .x %>% 
        glance() %>% 
        select(r.squared)  
    ) %>% return()
  })) %>% 
  unnest(m.values) %>% 
  ungroup() %>% 
  ggplot(aes(y=slope, x=r.squared)) +
  geom_point() +
  geom_text_repel(aes(label=station_name), size=1.75) +
  theme_minimal()


br_temp %>% 
  filter(station_name %in% c("RIO DE JANEIR", "SAO PAULO", "BRASILEIRA (AER", 
                             "SALVADOR","MANAUS")) %>% 
  group_by(country, station_name, year) %>% 
  summarise( temp_avg = mean(temperature, na.rm = T)) %>% 
  ungroup() %>% 
  filter(year>=max(year)-100) %>% 
  mutate(temp_avg=temp_avg-mean(temp_avg)) %>% 
  # ggplot(aes(x=year, y=temp_avg)) +
  # geom_point() + 
  # theme_minimal()
  ggplot(aes(x=year, fill=temp_avg, color=temp_avg)) +
  geom_bar(aes(y=1), stat="identity",
           position = position_dodge2(width = 0, padding=0)) +
  scale_fill_gradient2(low="darkblue", mid="lightblue", high="red") +
  scale_color_gradient2(low="darkblue", mid="lightblue", high="red") +
  facet_wrap(.~station_name, ncol = 1)+
  theme_void() +
  theme(
    legend.position = "none",
    panel.border = element_blank()
  )




