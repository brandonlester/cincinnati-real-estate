library(tidyverse)
library(lubridate)

zhvi_import <- read_csv("Data/Neighborhood_zhvi_uc_sfr_sm_sa_mon.csv")
skimr::skim(zhvi_import)
head(zhvi_import)

zhvi_import %>% filter(is.na(Metro)) %>% count(State)
zhvi_import %>% count(RegionID, RegionName, RegionType) %>% arrange(RegionID) #%>% arrange(desc(n))
zhvi_import %>% count(State, StateName)
zhvi_import %>% filter(State == "OH" | StateName == "OH") %>% count(State, StateName)
zhvi_import %>% filter(State == "KY" | StateName == "KY") %>% count(State, StateName)
zhvi_import %>% filter(State == "OH") %>% count(Metro, CountyName)
zhvi_import %>% filter(State == "KY") %>% count(Metro, CountyName)

neighborhoods <- zhvi_import %>% 
  filter(Metro == "Cincinnati")  %>% 
  select(RegionID, SizeRank_US = SizeRank, RegionName, City, CountyName) %>% 
  arrange(SizeRank_US) %>% 
  mutate(SizeRank_Cin = row_number()) #%>% count(Rank) %>% arrange(desc(n))

zhvi <- zhvi_import %>% 
  filter(RegionID %in% neighborhoods$RegionID) %>% 
  select(RegionID, contains("-")) %>% #names()
  pivot_longer(cols = -RegionID, names_to = "Date", values_to = "ZHVI") %>% 
  mutate(Date = ymd(Date))

skimr::skim(zhvi)

#missing values
zhvi %>% 
  filter(is.na(ZHVI)) %>% 
  #count(RegionID, yr = year(Date)) %>% print(n=Inf) %>% 
  group_by(RegionID) %>% 
  summarise(
    na = n(),
    minDate = min(Date),
    maxDate = max(Date)
  ) %>% 
  mutate(month_diff = round(time_length(maxDate - minDate, unit = "month"), digits = 0)+1) %>% 
  left_join(neighborhoods, by = "RegionID")

zhvi <- zhvi %>% filter(!is.na(ZHVI))

library(xml2)

# geocode <- function(q, a) {
#   stopifnot(is.character(q), length(q)==1, a %in% c("lat","lon"))
#   
#   u <- paste0("https://nominatim.openstreetmap.org/search?q=", q, ",ohio&format=xml")
#   x <- tryCatch(read_xml(u), error = function(cond) NA)
#   if(is.na(x)) NA_character_ else xml_attr(xml_child(x), attr = a)
# }
# 
# for(i in 1:length(neighborhoods$RegionName)) print(paste(i, geocode(neighborhoods$RegionName[[i]], a = "lat")))
# for(i in 54:length(neighborhoods$RegionName)) print(paste(i, geocode(neighborhoods$RegionName[[i]], a = "lat")))
# neighborhoods[53,]

geocode <- function(q, a) {
  stopifnot(is.character(q), length(q)==1, a %in% c("lat","lon"))
  
  u <- paste0("https://nominatim.openstreetmap.org/search?q=", q, ",ohio&format=xml")
  x <- tryCatch(read_xml(u), error = function(cond) NA_character_)
  if(is.na(x)) return(NA_character_) else c <- tryCatch(xml_child(x), error = function(cond) NA_character_)
  if(is.na(c)) return(NA_character_) else xml_attr(c, attr = a)
}

neighborhoods$lat <- map_chr(neighborhoods$RegionName, geocode, a = "lat")
neighborhoods$lon <- map_chr(neighborhoods$RegionName, geocode, a = "lon")

neighborhoods %>% filter(is.na(lat) | is.na(lon))


missing_coordinates <- list(
  `West Price HIll` = c(39.112192,-84.605955),
  `College Hill` = c(39.1953178,-84.5578255),
  `East Price Hill` = c(39.106142,-84.5807041),
  `Walnut Hills` = c(39.1272573,-84.4992359),
  `Bond Hill` = c(39.179925,-84.4801268),
  `Winton Hills` = c(39.1860023,-84.5193982),
  `Kennedy Heights`= c(39.1831258,-84.4183891),
  `Fruit Hill` = c(39.0711943,-84.3763343),
  `Paddock Hills` = c(39.1626188,-84.4814705),
  `Mid Heights` = c(39.2649861,-84.2757216),
  `Lower Price Hill` = c(39.1053532,-84.5603652),
  `Pheasant Hills` = c(39.2611843,-84.3019982),
  `Loveland-Madeira Corridor` = c(39.2584893,-84.2880393),
  `Henry Hannah's Farm` = c(39.2602237,-84.2618913),
  `Pheasant Hills on the Lake` = c(39.2643791,-84.2966167)
) %>% 
  map(as.character) %>% 
  bind_rows() %>% t() %>% as.data.frame()

missing_coordinates$name <- rownames(missing_coordinates)
rownames(missing_coordinates) <- NULL
names(missing_coordinates) <- c("lat", "lon", "RegionName")

neighborhoods <- neighborhoods %>% 
  left_join(missing_coordinates, by = "RegionName", suffix = c("", ".manual")) %>% 
  mutate(
    lat = ifelse(is.na(lat), lat.manual, lat),
    lat = as.numeric(lat),
    lon = ifelse(is.na(lon), lon.manual, lon),
    lon = as.numeric(lon)
  ) %>% 
  select(-contains(".manual"))

coord_ranges <- c(range(neighborhoods$lon), range(neighborhoods$lat))
neighborhoods %>% 
  filter(lon %in% coord_ranges | lat %in% coord_ranges) %>% 
  View()

neighborhoods$lat[neighborhoods$RegionName == "Central Business District"] <- 39.1028148
neighborhoods$lon[neighborhoods$RegionName == "Central Business District"] <- -84.5246515

neighborhoods$lat[neighborhoods$RegionName == "Huntington"] <- 39.2517002
neighborhoods$lon[neighborhoods$RegionName == "Huntington"] <- -84.2493667

neighborhoods$lat[neighborhoods$RegionName == "Brandywine"] <- 39.2784178
neighborhoods$lon[neighborhoods$RegionName == "Brandywine"] <- -84.2574159

neighborhoods$lat[neighborhoods$RegionName == "Downtown"] <- 39.2697192
neighborhoods$lon[neighborhoods$RegionName == "Downtown"] <- -84.2609599

neighborhoods$lat[neighborhoods$RegionName == "Clifton"] <- 39.1496151
neighborhoods$lon[neighborhoods$RegionName == "Clifton"] <- -84.5387535

neighborhoods$lat[neighborhoods$RegionName == "Mt. Lookout"] <- 39.1279522
neighborhoods$lon[neighborhoods$RegionName == "Mt. Lookout"] <- -84.4369616

neighborhoods$lat[neighborhoods$RegionName == "Riverside" & neighborhoods$City == "Cincinnati"] <- 39.0775756
neighborhoods$lon[neighborhoods$RegionName == "Riverside" & neighborhoods$City == "Cincinnati"] <- -84.6117549

neighborhoods$lat[neighborhoods$RegionName == "Mt. Auburn"] <- 39.1192592
neighborhoods$lon[neighborhoods$RegionName == "Mt. Auburn"] <- -84.5174844

neighborhoods$lat[neighborhoods$RegionName == "Riverside" & neighborhoods$City == "Cincinnati"] <- 39.0775756
neighborhoods$lon[neighborhoods$RegionName == "Riverside" & neighborhoods$City == "Cincinnati"] <- -84.6117549

neighborhoods$lat[neighborhoods$RegionName == "Riverside" & neighborhoods$City == "Loveland"] <- 39.2629983
neighborhoods$lon[neighborhoods$RegionName == "Riverside" & neighborhoods$City == "Loveland"] <- -84.2694719


write_csv(neighborhoods, "Output/neighborhoods.csv")
write_csv(zhvi, "Output/zhvi.csv")
