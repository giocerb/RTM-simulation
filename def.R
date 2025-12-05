# --- 1. SETUP & LIBRARIES ---
library(tidyverse)
library(sf)
library(viridis)
library(spdep)
library(FNN)
library(grid)
library(patchwork) 
library(here)
library(MASS)


# Helper function: Nearest Neighbor Distance
nn_function <- function(measureFrom, measureTo, k) {
  measureFrom_Matrix <- st_coordinates(measureFrom)
  measureTo_Matrix <- st_coordinates(measureTo)
  nn <- get.knnx(data = measureTo_Matrix, query = measureFrom_Matrix, k = k)
  nndists <- as.data.frame(nn$nn.dist)
  return(rowMeans(nndists))
}

# --- 2. DATA LOADING & PREPROCESSING ---

# Load Grid
squares_paris <- st_read(here("data", "grid_paris.geojson"), quiet = TRUE)

# Load Arrondissements (Mask)
arrondissement_shape <- st_read(here("data", "arrondissements.geojson"), quiet = TRUE)
arrondissement_shape1 <- st_transform(arrondissement_shape, 2154)

# Filter Grid
squares_section <- st_filter(squares_paris, arrondissement_shape1)

# Load Risk Factors
gares_raw <- st_read(here("data", "emplacement-des-gares-idf.geojson"), quiet = TRUE)
gares_paris <- st_transform(gares_raw, 2154)
gares_paris_vz <- st_filter(gares_paris, arrondissement_shape1)

shops_general_brut <- st_read(here("data", "shops_filtered.geojson"), quiet = TRUE) %>% 
  st_transform(2154)

# Filter Shops
disco <- shops_general_brut[shops_general_brut$codact == "SA404", ]
fastfood <- shops_general_brut[shops_general_brut$codact %in% c("CH302", "CH303"), ]

# Load Police
police_base <- st_read(here("data", "carte-des-points-daccueil-police-a-paris.geojson"), quiet = TRUE) %>% 
  st_transform(2154)

# Load Target Variable
marue_sf <- readRDS(here("data", "dans_ma_rue_thin.rds")) %>%
  st_transform(2154)


# --- 3. FEATURE ENGINEERING ---

# A. Counts
squares_section$count_gares <- lengths(st_intersects(squares_section, gares_paris))
squares_section$count_disco <- lengths(st_intersects(squares_section, disco))
squares_section$count_fastfood <- lengths(st_intersects(squares_section, fastfood))
squares_section$police <- lengths(st_intersects(squares_section, police_base))
squares_section$marue_count <- lengths(st_intersects(squares_section, marue_sf))

# B. Density
grid_area_km2 <- as.numeric(st_area(squares_section)) / 1e6 # Convert m2 to km2

squares_section <- squares_section %>%
  mutate(
    density_gares = count_gares / grid_area_km2,
    density_disco = count_disco / grid_area_km2,
    density_fastfood = count_fastfood / grid_area_km2,
    density_police = police / grid_area_km2,
    densitymarue = marue_count / grid_area_km2
  )

# C. Distances (Nearest Neighbor)
grid_centroids <- st_centroid(squares_section)

squares_section$dist_gares <- nn_function(grid_centroids, gares_paris, k = 1) / 1000
squares_section$dist_disco <- nn_function(grid_centroids, disco, k = 1) / 1000
squares_section$dist_police <- nn_function(grid_centroids, police_base, k = 1) / 1000
squares_section$dist_fastfood <- nn_function(grid_centroids, fastfood, k = 1) / 1000

# --- 4. MODELING ---

df_model <- st_drop_geometry(squares_section)
df_model$grid_area_km2 <- grid_area_km2

# Model 1: Distance
model_dist <- glm.nb(
  marue_count ~ dist_disco + dist_gares + dist_fastfood + dist_police + offset(log(grid_area_km2)),
  data = df_model
)
summary(model_dist)

# Model 2: Count
model_count <- glm.nb(
  marue_count ~ count_disco + count_fastfood + count_gares + police + offset(log(grid_area_km2)),
  data = df_model
)
summary(model_count)

# Model 3: Density
model_dens <- glm.nb(
  marue_count ~ density_gares + density_disco + density_fastfood + density_police + offset(log(grid_area_km2)),
  data = df_model
)
summary(model_dens)

# --- 5. PREDICTION & RRS CALCULATION ---

# Predict Risk
squares_section$predicted_risk <- predict(model_dist, newdata = squares_section, type = "response")

# Calculate RRS (Moved up so plots work)
avg_risk <- mean(squares_section$predicted_risk, na.rm = TRUE)
squares_section$RRS <- squares_section$predicted_risk / avg_risk
max_rrs <- max(squares_section$RRS, na.rm = TRUE)

# --- 6. VISUALIZATION ---

# A. Risk Maps
risk_map <- ggplot(data = squares_section) +
  geom_sf(aes(fill = RRS), color = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       name = "Relative Risk\nScore", 
                       limits = c(1, max_rrs), 
                       na.value = "grey90") +
  labs(title = "Risk Terrain Map: Dans Ma Rue Reports",
       subtitle = "Predicted risk of reporting",
       caption = "RRS > 1.0 indicates higher than average risk") +
  theme_void() +
  theme(legend.position = "right")

real_map <- ggplot(data = squares_section) +
  geom_sf(aes(fill = marue_count), color = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Number of reporting") +
  labs(title = "Real reporting on Dans Ma Rue", subtitle = "RTM Grid Analysis") + 
  theme_void()

combined <- risk_map + real_map
print(combined)

# B. Distance to Police
squares_section$dist_pol_cap <- pmin(squares_section$dist_police, 2000)

ggplot(data = squares_section) +
  geom_sf(aes(fill = dist_pol_cap), color = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1, 
                       labels = c("0", "500", "1000", "1500", "2000+"), 
                       name = "Avg Dist (m)") +
  labs(title = "Distance to nearest police station", caption = "Data Source: Région Île-de-France") +
  scale_alpha(range = c(0.1, 0.9), guide = "none")

# C. Density Plots (Kernel Density)
# Fast Food
fastfood_coord <- data.frame(st_coordinates(fastfood)) %>% rename(X = X, Y = Y)
ggplot() +
  geom_sf(data = arrondissement_shape1, fill = NA, color = "gray50") +
  stat_density_2d(data = fastfood_coord, aes(x = X, y = Y, fill = after_stat(level), alpha = after_stat(level)), geom = "polygon") +
  scale_fill_viridis_c(option = "magma", name = "Density") +
  scale_alpha(range = c(0.1, 0.9), guide = "none") +
  labs(title = "Density of fast food locations", caption = "Data Source: Apur") +
  theme_minimal()

# Metro
metro_coord <- data.frame(st_coordinates(gares_paris_vz)) %>% rename(X = X, Y = Y)
ggplot() +
  geom_sf(data = arrondissement_shape1, fill = NA, color = "gray50") +
  stat_density_2d(data = metro_coord, aes(x = X, y = Y, fill = after_stat(level), alpha = after_stat(level)), geom = "polygon") +
  scale_fill_viridis_c(option = "magma", name = "Density") +
  scale_alpha(range = c(0.1, 0.9), guide = "none") +
  labs(title = "Density of metro stops", caption = "Data Source: RATP") +
  theme_void()

# Nightclubs (Points)
disco_coord <- data.frame(st_coordinates(disco)) %>% rename(X = X, Y = Y)
ggplot() +
  geom_sf(data = arrondissement_shape1, fill = NA, color = "gray50") +
  geom_point(data = metro_coord, aes(x = X, y = Y), color = "red", size = 2, alpha = 0.7) +
  scale_fill_viridis_c(option = "magma", name = "Density") +
  labs(title = "Nightclubs in Paris", caption = "Data Source: Apur") +
  theme_void()

# Club Density (Grid) - Note: Uses marue_count as fill
ggplot(data = squares_section) +
  geom_sf(aes(fill = marue_count), color = NA) +
  scale_fill_viridis_c(option = "cividis", name = "Club Count") +
  labs(title = "Density of Clubs in Paris", subtitle = "RTM Grid Analysis") +
  theme_void()

# Dans Ma Rue (Density)
marue_coords <- data.frame(st_coordinates(marue_sf)) %>% rename(X = X, Y = Y)
ggplot() +
  geom_sf(data = arrondissement_shape1, fill = NA, color = "gray50") +
  stat_density_2d(data = marue_coords, aes(x = X, y = Y, fill = after_stat(level), alpha = after_stat(level)), geom = "polygon") +
  scale_fill_viridis_c(option = "magma", name = "Density") +
  scale_alpha(range = c(0.1, 0.9), guide = "none") +
  labs(title = "Density of anomalies reported trough Dans Ma Rue (Oct-Dec 2025)", caption = "Data Source: Ville de Paris") +
  theme_minimal()
