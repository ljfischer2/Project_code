library(sf)
library(maps)
library(sp)
library(tidyverse)
setwd("C:/Users/heref/Documents/Project stuff/LucasProject/Japan GIS Files/CGISJapan")


#full watershed data of Hokkaido
sf_path <- "流域界_watershed/北海道/流域界01北海道.shp"
Hoshed <- st_read(sf_path)
names(Hoshed)

plot(Hoshed)

sarshed <- Hoshed %>%    #selecting only Sarufutsu watershed
  filter(水系域名 == '猿払川水系')　%>%
  select(geometry, 河川CD)


sarshed_sp <- as(sarshed, "Spatial")
# Now use spplot
spplot(sarshed_sp) #Sarufutsu watershed?

# full coastline of Hokkaido
coast <- '海岸線coastline/北海道/海岸線01北海道.shp'
coast <- st_read(coast)
names(coast)


hocoast_sarshed <- st_intersects(coast, sarshed)
hocoast_sarshed_sp <- as(hocoast_sarshed, 'Spatial')
plot(hocoast_sarshed_sp)

#plot(st_geometry(coast), col = "black", axes = T,  #map that is not in use
#     xlim = c(140, 146))   # longitude range
#plot(st_geometry(sarshed), col = "darkgrey", border = NA, add = TRUE)





#rivers of Hokkaido
river_path <- "~/Project stuff/LucasProject/Japan GIS Files/CGISJapan/河川_rivers/北海道/河川01北海道.shp"
rivers <- st_read(river_path)
names(rivers)

sarriver <- rivers %>%
  filter(水系 == "猿払川")





#Sample points
point_path <- read.csv('C:/Users/heref/Documents/Project stuff/LucasProject/Repo_Backup/sampling_locations_singles.csv')
point_path <- point_path[-c(2),]
point_sf <- st_as_sf(point_path, coords = c("lon", "lat"), crs = 4326)

plot(st_geometry(coast),   # map in use (All of Hokkaido)
     col = "black",
     axes = TRUE,
     asp = 1)
plot(st_geometry(sarshed), col = "black", border = NA, add = TRUE)


#plot(st_geometry(sarshed), col = "grey", border = NA, axes = T)     #same as below but worse
#plot(st_geometry(sarriver), add = T)
#plot(st_geometry(coast), add = T)
#plot(st_geometry(point_sf), add = T, pch = 16, col = "red", cex = 1.5)


x_ticks <- seq(141.5, 143, by = 0.25)          #making axis labels
y_ticks <- seq(45.0, 45.3, by = 0.1)
x_labels <- paste0(abs(x_ticks), "°E")
y_labels <- paste0(y_ticks, "°N")


plot(st_geometry(sarshed), col = "grey", border = NA, axes = FALSE, asp = 1)  #final sarufutsu map?
axis(1, at = x_ticks,  labels = x_labels)
axis(2, at = y_ticks, labels = y_labels)
box()
plot(st_geometry(sarriver), add = TRUE)
plot(st_geometry(coast), add = TRUE)
plot(st_geometry(point_sf), add = TRUE, pch = 16, col = "red", cex = 1.25)












################# redundant code? #############
coast_sarshed <- coast %>%
  st_join(sarshed, by = "geometry")


coast_sp <- as(coast, 'Spatial')
plot(coast_sp)









sarcocoast_spsarcoast <- coast %>%
  filter(団体名 == '猿払村')

sarcoast_sp <- as(sarcoast, 'Spatial')

