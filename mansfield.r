# Process crime data
# 1/10/2017 - 30/9/2019
# ASB, drug dealing, theft, indecent exposure (sexual offences?), threatening behaviour (violence)

library("dplyr")
library("sf")
library("leaflet")


# Load Mansfield boundary

mansfield =
    sf::read_sf("data/mansfield/mansfield.gpkg") %>%
    st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


# Load Nottinghamshire crimes

csvs = list.files("data/mansfield/notts-crimes", recursive = TRUE, full.names = TRUE, pattern = ".csv")
crimes = lapply(csvs, readr::read_csv, show_col_types = FALSE)
crimes = bind_rows(crimes)

# Clip Nottinghamshire crimes to Mansfield

crimes = 
    crimes %>%
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

crimes = crimes[mansfield, ]


# Produce maps by crime type

crime_types = unique(crimes$type)

for (crime in crime_types) {
    dat = 
        crimes %>%
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
    withr::with_dir("./docs/mansfield", htmlwidgets::saveWidget(map, file = paste0(crime, ".html")))
}
