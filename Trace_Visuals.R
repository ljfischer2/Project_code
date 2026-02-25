# Creating traces # Requires RMR to be run first #
setwd("~/Project stuff/LucasProject/Repo_Backup/Project_code/Fish_Traces")

### Data 1 #########

total_rows <- nrow(data1)
num_sets   <- total_rows %/% 450

#Masu1
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber1.1, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu1plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

# Ito1
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber1.2, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito1plots.pdf")
for (i in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

# Ito 2

for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber1.3, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito2plots.pdf")
for (i in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

# Ito3

for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber1.4, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito3plots.pdf")
for (i in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()



###### Trial 2 #########

total_rows <- nrow(data2)
num_sets   <- total_rows %/% 450

#Masu2
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber2.1, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu2plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Ito 4

for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber2.2, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito4plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()


#Masu3
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber2.3, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu3plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Masu4
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber2.4, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu4plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()
