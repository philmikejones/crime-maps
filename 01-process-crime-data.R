# Process crime data

city = "lincoln"
zips = list.files(city, pattern = ".zip", full.names = TRUE)
lapply(zips, unzip, exdir = city)
