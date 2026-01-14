# Produce leaflet interactive crime maps of
# Derby Allestree and Darley with Mackworth and Morley
library("dplyr")
library("sf")
library("leaflet")

dir.create("docs/cannock", showWarnings = FALSE)

boundary =
    sf::read_sf("data/boundaries/cannock-south-and-west.gpkg") %>%
    st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

crimes = list.files("data/crimes/staffordshire", recursive = TRUE, full.names = TRUE, pattern = ".csv")
crimes = crimes[!grepl("2021", crimes)]
crimes = crimes[!grepl("2022", crimes)]

crimes = lapply(crimes, readr::read_csv, show_col_types = FALSE)
crimes = bind_rows(crimes)

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
    mutate(month = paste0(month, "-01")) |>
    mutate(month = lubridate::as_date(month)) |>
    filter(month > "2023-11-01") |>
    filter(!is.na(long), !is.na(lat)) %>%
    st_as_sf(coords = c("long", "lat")) %>%
    st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

crimes = crimes[boundary, ]

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
    withr::with_dir("./docs/cannock", htmlwidgets::saveWidget(map, title = paste0("Cannock South and West - ", crime), file = paste0(crime, ".html")))
}
