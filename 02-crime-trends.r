library("dplyr")
library("sf")

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

print(normanton_crimes)

stop()

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
