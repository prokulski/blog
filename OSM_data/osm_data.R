library(tidyverse)
library(sf)
library(osmdata)
library(ggmap)
library(htmlwidgets)
library(leaflet)


# obszar
q0 <- getbb("Wetlina, Poland") %>% opq(timeout = 60)

# co pobieramy - jakie cechy, jakie wartości - trzeba pogrzebać :)
q1 <- add_osm_feature(q0, key = 'name', key_exact = TRUE) # any named objects

# wczytujemy do obiektu sf
data_sf <- osmdata_sf(q1)

data_sf

# punkty - tylko 1%
data_sf$osm_points %>%
  select(geometry) %>%
  sample_frac(0.01) %>%
  ggplot() +
  geom_sf()


# jesli punkty sa poza interesujacym nas obszarem mozemy je przyciac
data_sf <- trim_osmdata(data_sf, getbb("Wetlina, Poland", format_out = "polygon"))

data_sf


# punkty
data_sf$osm_points %>%
  select(geometry) %>%
  ggplot() +
  geom_sf()


mapa <- get_map(location = "Wetlina, Poland", zoom = 12, maptype = "hybrid")

# polygony
ggmap(mapa) + geom_sf(data =data_sf$osm_polygons,
                      inherit.aes = FALSE,
                      color = "white", size = 1)


# linie
ggmap(mapa) + geom_sf(data = data_sf$osm_lines,
                      inherit.aes = FALSE,
                      aes(color = highway), size = 2)


# punkty z nazwami
ggmap(mapa, darken = 0.7) + geom_sf(data = data_sf$osm_points %>% filter(!is.na(name)),
                      inherit.aes = FALSE,
                      color = "white", size = 0.1)


gps_coords <- data_sf$bbox %>% str_split(",") %>% unlist() %>% as.numeric()
lng_coord <- (gps_coords[2] + gps_coords[4])/2
lat_coord <- (gps_coords[1] + gps_coords[3])/2


leaflet() %>%
  addTiles() %>%
  setView(lng = lng_coord, lat = lat_coord, zoom = 12) %>%
  addMarkers(data = data_sf$osm_points %>%
               filter(!is.na(name)) %>%
               select(name, geometry),
             label = ~as.character(name))





#### FUNKCJE ----
getOSMData <- function(f_localisation, f_key, f_value) {

  data_sf <- getbb(f_localisation) %>%
    opq() %>%
    add_osm_feature(key = f_key, value = f_value) %>%
    osmdata_sf()

  return(data_sf)
}

getLngLat <- function(f_data_sf) {

  gps_coords <- f_data_sf$bbox %>%
    str_split(",") %>%
    unlist() %>%
    as.numeric()

  lng_coord <- (gps_coords[2] + gps_coords[4])/2
  lat_coord <- (gps_coords[1] + gps_coords[3])/2

  return(list(lng = lng_coord, lat = lat_coord))
}

plotLeafletMap_points <- function(f_data_sf) {

  if(is.null(f_data_sf$osm_points$category)) f_data_sf$osm_points$category <- NA

  gps_coords <- getLngLat(f_data_sf)

  leaflet() %>%
    addTiles() %>%
    setView(lng = gps_coords$lng, lat = gps_coords$lat, zoom = 11) %>%
    addMarkers(data = f_data_sf$osm_points,
               label = ~paste0(if_else(is.na(name),
                                       "",
                                       as.character(name)
               ),
               if_else(is.na(category),
                       "",
                       paste0(" (", category, ")")
               )
               )
    )
}

plotLeafletMap_polygons <- function(f_data_sf) {

  gps_coords <- getLngLat(f_data_sf)

  # workaround konieczny, żeby polygony zadziałały
  # via https://github.com/ropensci/osmdata/issues/100
  names(f_data_sf$osm_polygons$geometry) <- NULL

  leaflet() %>%
    addTiles() %>%
    setView(lng = gps_coords$lng, lat = gps_coords$lat, zoom = 11) %>%
    addPolygons(data = f_data_sf$osm_polygons,
                opacity = 1,
                color = 'red',
                weight = 1,
                fillOpacity = 0.2,
                fillColor ='yellow',
                smoothFactor = 0.9,
                label = ~as.character(name))
}

plotLeafletMap_lines <- function(f_data_sf) {

  names(f_data_sf$osm_lines$geometry) <- NULL

  gps_coords <- getLngLat(f_data_sf)

  leaflet() %>%
    addTiles() %>%
    setView(lng = gps_coords$lng, lat = gps_coords$lat, zoom = 11) %>%
    addPolylines(data = f_data_sf$osm_lines,
                 color = 'red')
}

#### UŻYCIE FUNKCJI ----

# Gdzie w Szczecinie są sklepy spożywcze?
getOSMData("Szczecin, Poland", "shop", "supermarket") %>% plotLeafletMap_points()


# noclegi w Poznaniu
noclegi <- c(getOSMData("Poznań, Poland", f_key = 'tourism', f_value = 'hotel'),
             getOSMData("Poznań, Poland", f_key = 'tourism', f_value = 'motel'),
             getOSMData("Poznań, Poland", f_key = 'tourism', f_value = 'hostel'),
             getOSMData("Poznań, Poland", f_key = 'tourism', f_value = 'guest_house'),
             getOSMData("Poznań, Poland", f_key = 'tourism', f_value = 'apartment'))

noclegi$osm_points$category <- noclegi$osm_points$tourism

plotLeafletMap_points(noclegi)


# Gdzie w Krakowie są ścieżki rowerowe?
c(getOSMData("Kraków, Poland", "route", "bicycle"),
  getOSMData("Kraków, Poland", "highway", "cycleway")) %>% plotLeafletMap_lines()
