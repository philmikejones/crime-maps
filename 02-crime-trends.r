library("dplyr")
library("lubridate")
library("sf")
library("ggplot2")

normanton =  # to clip crimes to normanton ward
    sf::read_sf("data/normanton-neighbourhood.shp") %>%
    st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

n2021 = list.files("data/normanton/2021", recursive = TRUE, full.names = TRUE, pattern = ".csv")
n2021 = lapply(n2021, readr::read_csv)
n2021 = bind_rows(n2021)
n2021 = mutate(n2021, year = 2021)

n2022 = list.files("data/normanton/2022", recursive = TRUE, full.names = TRUE, pattern = ".csv")
n2022 = lapply(n2022, readr::read_csv)
n2022 = bind_rows(n2022)
n2022 = mutate(n2022, year = 2022)

n2023 = list.files("data/normanton/2023", recursive = TRUE, full.names = TRUE, pattern = ".csv")
n2023 = lapply(n2023, readr::read_csv)
n2023 = bind_rows(n2023)
n2023 = mutate(n2023, year = 2023)

normanton_crimes = bind_rows(n2021, n2022, n2023)

normanton_crimes = 
    normanton_crimes %>%
    mutate(id = row_number()) %>%
    select(
        id,
        year,
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

normanton_crimes = filter(normanton_crimes, st_within(normanton_crimes, normanton, sparse = FALSE))

print(unique(normanton_crimes$type))

types = unique(normanton_crimes$type)

b = normanton_crimes[normanton_crimes$type == "Burglary", ]

b = b[b$year == 2021, ]

b = as_tibble(b)

b = count(b, month)
print(b)

p = 
    ggplot(b, aes(month, n)) + 
    geom_line() +
    labs(x = "Month", y = "Number of offences", caption = "Burglary") +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +  # https://stackoverflow.com/a/42929948
    theme(axis.text.x=element_text(angle = 60, hjust = 1))

ggsave(p, width = 297, height = 210, units = "mm", file = "burglary-trend.pdf")
