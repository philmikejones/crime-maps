# Process crime data
library("dplyr")
library("sf")
library("leaflet")

if(!file.exists("lincoln/england_lad_2011.shp")) {
    city = "lincoln"
    zips = list.files(city, pattern = ".zip", full.names = TRUE)
    lapply(zips, unzip, exdir = city)
}

csvs = list.files("lincoln", recursive = TRUE, full.names = TRUE, pattern = ".csv")
lincoln_crimes = lapply(csvs, readr::read_csv)
lincoln_crimes = bind_rows(lincoln_crimes)

lincoln_crimes = 
    lincoln_crimes %>%
    mutate(id = row_number()) %>%
    select(
        id,
        Month,
        Longitude,
        Latitude,
        `Crime type`
    ) %>%
    rename(
        month = `Month`,
        long = `Longitude`,
        lat = `Latitude`,
        type = `Crime type`
    ) %>%
    filter(!is.na(long), !is.na(lat)) %>%
    st_as_sf(coords = c("long", "lat")) %>%
    st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

lincoln = 
    read_sf("lincoln/england_lad_2011.shp") %>%
    st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

lincoln_crimes = filter(lincoln_crimes, st_within(lincoln_crimes, lincoln, sparse = FALSE))
glimpse(lincoln_crimes)

crime_types = unique(lincoln_crimes$type)

for (crime in crime_types) {
    dat = 
        lincoln_crimes %>%
        filter(type == crime)
    
    map =
        leaflet(dat) %>%
        addTiles() %>%
        addMarkers(
            clusterOptions = markerClusterOptions(),
            popup = ~ paste0(
            htmltools::htmlEscape(type),
            "<br />",
            "Date: ",
            htmltools::htmlEscape(month))
        )
    
    crime = stringr::str_replace_all(crime, " ", "-")
    withr::with_dir("./docs/", htmlwidgets::saveWidget(map, file = paste0(crime, ".html")))
}
