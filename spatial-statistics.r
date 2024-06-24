library("sf")
library("ape")

crimes = sf::read_sf("data/normanton/2023/2023-12-derbyshire-street.csv")
crimes$num = 1:nrow(crimes)

# https://stats.oarc.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
crimes_dists <- as.matrix(dist(cbind(crimes$Longitude, crimes$Latitude)))
crimes_dists_inv <- 1 / crimes_dists
diag(crimes_dists_inv) <- 0

print(crimes$num)

print(Moran.I(crimes$num, crimes_dists_inv))
