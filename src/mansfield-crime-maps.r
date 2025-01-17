# Produce leaflet interactive crime maps of
# Mansfield Town Centre
library("dplyr")
library("sf")
library("leaflet")

dir.create("docs/mansfield", showWarnings = FALSE)

boundary =
    sf::read_sf("data/boundaries/mansfield-town-centre.kml") %>%
    st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# The scenario takes places between 1 October 2017 to 30 September 2019
crimes = list("data/crimes/nottinghamshire/2017-10-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2017-11-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2017-12-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-01-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-02-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-03-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-04-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-05-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-06-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-07-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-08-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-09-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-10-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-11-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2018-12-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2019-01-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2019-02-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2019-03-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2019-04-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2019-05-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2019-06-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2019-07-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2019-08-nottinghamshire-street.csv", "data/crimes/nottinghamshire/2019-09-nottinghamshire-street.csv")
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
    withr::with_dir("./docs/mansfield", htmlwidgets::saveWidget(map, file = paste0(crime, ".html")))
}
