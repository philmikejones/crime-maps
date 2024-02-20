# Process crime data
# Uses one year of data
# Produces a map of each crime type in leaflet
library("dplyr")
library("sf")
library("leaflet")

normanton =
    sf::read_sf("data/normanton-neighbourhood.shp") %>%
    st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

csvs = list.files("data/normanton/2023", recursive = TRUE, full.names = TRUE, pattern = ".csv")
normanton_crimes = lapply(csvs, readr::read_csv, show_col_types = FALSE)
normanton_crimes = bind_rows(normanton_crimes)

normanton_crimes = 
    normanton_crimes %>%
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

normanton_crimes = filter(normanton_crimes, st_within(normanton_crimes, normanton, sparse = FALSE))

crime_types = unique(normanton_crimes$type)

for (crime in crime_types) {
    dat = 
        normanton_crimes %>%
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
    withr::with_dir("./docs/normanton", htmlwidgets::saveWidget(map, file = paste0(crime, ".html")))
}
