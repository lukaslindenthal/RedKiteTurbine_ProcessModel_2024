
library(sf)
library(raster)
library(ggplot2)

# 1. Laden und Umwandeln der Projektion von Baden-Württemberg
bw_shape <- st_read("data/BW_kreise/AX_Gebiet_Kreis.shp")
plot(bw_shape)

# Überprüfen der aktuellen CRS (Koordinatenreferenzsystem)
st_crs(bw_shape)

# Umwandeln in ein geeignetes CRS, z.B. UTM für Baden-Württemberg (EPSG:25832)
bw_shape <- st_transform(bw_shape, crs = 25832)

# Begrenzungen des Shapefiles extrahieren
bounds <- st_bbox(bw_shape)

# Raster proportional zur tatsächlichen Geometrie von Baden-Württemberg
aspect_ratio <- (bounds$xmax - bounds$xmin) / (bounds$ymax - bounds$ymin)

# Dimensionen entsprechend anpassen
x_dim <- 100
y_dim <- round(x_dim / aspect_ratio)

timesteps <- 10

# Erstellen eines Rasters für Baden-Württemberg mit der gleichen Projektion
bw_raster <- raster(extent(bw_shape), ncol = x_dim, nrow = y_dim)
bw_raster <- rasterize(bw_shape, bw_raster, field = 1, background = 0)

region <- array(0, dim = c(x_dim, y_dim, timesteps))
region[, , ] <- as.matrix(bw_raster)

# 2. Initialisierung der Turbinen
turbine <- array(FALSE, dim = c(x_dim, y_dim, timesteps))
valid_coords <- which(region[, , 1] == 1, arr.ind = TRUE)
n_turb_1 <- 10
random_indices <- sample(1:nrow(valid_coords), n_turb_1, replace = TRUE)

for (i in random_indices) {
  x <- valid_coords[i, 1]
  y <- valid_coords[i, 2]
  turbine[x, y, ] <- TRUE
}

# 3. Visualisierung
region_layer <- region[, , 1]
turbine_layer <- turbine[, , 1]

df <- data.frame(
  x = rep(1:x_dim, each = y_dim),
  y = rep(1:y_dim, times = x_dim),
  region = as.vector(region_layer),
  turbine = as.vector(turbine_layer)
)

p <- ggplot(df, aes(x = x, y = y)) +
  geom_tile(aes(fill = region), show.legend = FALSE) +
  scale_fill_gradient(low = "white", high = "blue", name = "Region") +
  geom_point(data = subset(df, turbine == TRUE), aes(x = x, y = y), color = "red", size = 2) +
  labs(title = paste("Timestep = ", 1), x = "X", y = "Y") +
  theme_minimal()

print(p)

