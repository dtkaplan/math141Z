# Make the chapter-by-chapter data

Coal_prices <- tibble::tribble(
  ~ year, ~ price,
  1980, 31.36,
  1981, 30.11,
  1983, 27.46,
  1987, 22.25
)
Birth_weight <- tibble::tribble(
  ~ week, ~ weight,
  25, 777,
  26, 888,
  30, 1435,
  31, 1633,
  33, 2058
)
Prozac <- tibble::tribble(
  ~ day, ~ concentration,
  0, 79,
  5, 40,
  10, 19.6,
  22, 4.3,
  27, 2.5
)

save(Prozac, Birth_weight, Coal_prices, file="data/Chapter1.rda")
