```r
library(sf)
library(centerline)
library(dplyr)
library(ggplot2)
library(parallel)

# Read your GMBA 300 shapefile
```{r echo=FALSE, eval=FALSE}
mountains <- st_read("./gmba_inventory_v20_standard_300_dissolved.gpkg")

# Cast to POLYGON and filter by area (> 5000 km²)
mountains_P <- st_cast(mountains, "POLYGON")
mountains_P <- mountains_P[as.numeric(st_area(mountains_P)) > 5e8, ]

# Parallelized centerline guess function
run_guess <- function(i, sf_poly) {
  library(centerline)
  cnt_path_guess(sf_poly[i, ], keep = 0.5, method = "voronoi")
}

# Apply in parallel (adjust cores as needed)
cline_listV <- mclapply(
  X             = seq_len(nrow(mountains_P)),
  FUN           = run_guess,
  mc.cores      = 5,
  mc.preschedule = FALSE,
  sf_poly       = mountains_P
)

# Filter out failed guesses and combine
valid <- sapply(cline_listV, function(x) !is.null(x) && !is.na(st_crs(x)$input))
cline_all <- do.call(rbind, cline_listV[valid])

# Keep only centerlines longer than 50 km
cline_50km <- cline_all[which(as.numeric(st_length(cline_all$geometry)) > 50e3), ]

# Merge attributes back to mountains
mountains_cl <- merge(mountains_P, cline_50km, by = "GMBA_V2_ID")

# Plot with ggplot2
g1 <- ggplot() +
geom_sf(data = mountains_P, fill = "lightgrey", color = NA) +
geom_sf(data = cline_all,  color = "darkred", size = 1) +
geom_sf(data = cline_50km, color = "green",   size = 1) +
theme_minimal() +
theme(
  panel.background = element_rect(fill = "white"),
  panel.grid       = element_line(color = "grey90")
  ) +
labs(
  title    = "Mountains with Centerline Paths",
  subtitle = "Red = all paths, Green = 50 km subset"
  )
```

```{r echo=FALSE, out.width='100%', fig.cap="World mountain centerlines (green ≥50 km)"}
knitr::include_graphics("./World_Mountain_Centerlines.png")
```