library("dplyr")
library("lubridate")
library("sf")
library("ggplot2")

boundary =
    sf::read_sf("data/boundaries/sherwood.kml") %>%
    st_transform(crs = st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

crimes = list.files("data/crimes/nottinghamshire", recursive = TRUE, full.names = TRUE, pattern = ".csv")
crimes = crimes[!grepl("2017", crimes)]
crimes = crimes[!grepl("2018", crimes)]
crimes = crimes[!grepl("2019", crimes)]

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
    mutate(month = paste0(month, "-01")) %>%
    mutate(month = lubridate::as_date(month)) %>%
    # Streetaid device was installed in August 2023
    filter(month <= "2023-08-01") |>
    filter(!is.na(long), !is.na(lat)) %>%
    st_as_sf(coords = c("long", "lat")) %>%
    st_set_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

crimes = crimes[boundary, ]

types = unique(crimes$type)

for(type in types) {
    dat = crimes[crimes$type == type, ]
    dat = as_tibble(dat)
    dat = count(dat, month)

    p = 
        ggplot(dat, aes(month, n)) + 
        # geom_vline(xintercept = ymd('2023-08-01'), linetype = "dashed") +
        geom_line() +
        geom_smooth(method = "loess", formula = "y ~ x") +
        labs(x = "Month", y = "Number of offences", caption = type) +
        scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") +  # https://stackoverflow.com/a/42929948
        theme(axis.text.x=element_text(angle = 60, hjust = 1))

    ggsave(
        p, width = 297, height = 210, units = "mm",
        file = paste0("docs/sherwood/", type, "-trend.pdf"), title = paste0(type, " trend, Sherwood, 2022-2024")
    )

    print(paste0("Saved: ", type, "-trend.pdf"))

}
