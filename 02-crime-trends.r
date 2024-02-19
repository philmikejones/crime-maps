library("dplyr")
library("lubridate")
library("sf")
library("ggplot2")

normanton =  # to clip crimes to normanton ward
    sf::read_sf("data/normanton-neighbourhood.shp") %>%
    st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

crimes = list.files("data/normanton", recursive = TRUE, full.names = TRUE, pattern = ".csv")
crimes = lapply(crimes, readr::read_csv)
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
    mutate(month = paste0(month, "-01")) %>%
    mutate(month = lubridate::as_date(month)) %>%
    filter(!is.na(long), !is.na(lat)) %>%
    st_as_sf(coords = c("long", "lat")) %>%
    st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")    

crimes = filter(crimes, st_within(crimes, normanton, sparse = FALSE))

types = unique(crimes$type)

b = crimes[crimes$type == "Burglary", ]
b = as_tibble(b)
b = count(b, month)

p = 
    ggplot(b, aes(month, n)) + 
    geom_vline(xintercept = ymd('2021-01-01'), linetype = "dashed") +
    geom_vline(xintercept = ymd('2022-01-01'), linetype = "dashed") +
    geom_vline(xintercept = ymd('2023-01-01'), linetype = "dashed") +
    geom_line() +
    geom_smooth(method = "loess", formula = "y ~ x") +
    labs(x = "Month", y = "Number of offences", caption = "Burglary") +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +  # https://stackoverflow.com/a/42929948
    theme(axis.text.x=element_text(angle = 60, hjust = 1))

ggsave(p, width = 297, height = 210, units = "mm", file = "burglary-trend.pdf")
