library("dplyr")
library("sf")
library("leaflet")

normanton =
    sf::read_sf("data/normanton-neighbourhood.shp") %>%
    st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

csvs = list.files("data/normanton/2023", recursive = TRUE, full.names = TRUE, pattern = ".csv")
vehicle_crime = lapply(csvs, readr::read_csv, show_col_types = FALSE)
vehicle_crime = bind_rows(vehicle_crime)

vehicle_crime = 
    vehicle_crime %>%
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
    filter(type == "Vehicle crime") %>%
    filter(month == "2023-07") %>%
    filter(!is.na(long), !is.na(lat)) %>%
    st_as_sf(coords = c("long", "lat")) %>%
    st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

map =
    leaflet(vehicle_crime) %>%
    addTiles() %>%
    addMarkers(
        clusterOptions = markerClusterOptions(),
        popup = ~ paste0(
        htmltools::htmlEscape(type),
        "<br />",
        "Date: ",
        htmltools::htmlEscape(month))
    )

withr::with_dir("./docs/normanton", htmlwidgets::saveWidget(map, file = "vehicle-crime-july-derbyshire.html"))
