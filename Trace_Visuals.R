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
for (f in png_files) {
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
for (f in png_files) {
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
for (f in png_files) {
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

########## FS2


#Ito5
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber2.5, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito5plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Ito 6

for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber2.6, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito6plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()


#Ito7
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber2.7, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito7plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Char1
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber2.8, starts = 450, wait = 390,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Char1plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()



###### Trial 3 #########

total_rows <- nrow(data3)
num_sets   <- total_rows %/% 450

#Ito8
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber3.1, starts = 450, wait = 375,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito8plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Ito9

for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber3.2, starts = 450, wait = 375,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito9plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()


#Masu5
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber3.3, starts = 450, wait = 375,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu5plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Masu6
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber3.4, starts = 450, wait = 375,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu6plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

########## FS2


#Masu9
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber3.5, starts = 450, wait = 375,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu9plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Masu7

for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber3.6, starts = 450, wait = 375,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu7plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()


#Masu8                          Masu 8 Died during trials
#for (i in 1:num_sets) {
#  
#  png(paste0("temp_", i, ".png"))
#  
#  calc_rate.int(
#    chamber3.7, starts = 450, wait = 390,
#    measure = 60, by = "row", pos = i
#  )
#  dev.off()
#}
#png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
#pdf("Masu8plots.pdf")
#for (f in png_files) {
#  grid::grid.newpage()
#  grid::grid.raster(png::readPNG(f))
#}
#dev.off()

#Ito10
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber3.8, starts = 450, wait = 375,
    measure = 60, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito10plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()



###### Trial 4 #########

total_rows <- nrow(data4)
num_sets   <- total_rows %/% 450

#Masu11
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber4.1, starts = 450, wait = 370,
    measure = 55, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu11plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Ito13

for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber4.2, starts = 450, wait = 370,
    measure = 55, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito13plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()


#Masu12
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber4.3, starts = 450, wait = 370,
    measure = 55, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu12plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Masu13
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber4.4, starts = 450, wait = 370,
    measure = 55, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu13plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

########## FS2


#Blank
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber4.5, starts = 450, wait = 370,
    measure = 55, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Bkgplots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Ito12

for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber4.6, starts = 450, wait = 370,
    measure = 55, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito12plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()


#Masu10
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber4.7, starts = 450, wait = 370,
    measure = 55, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Masu10plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()

#Ito11
for (i in 1:num_sets) {
  
  png(paste0("temp_", i, ".png"))
  
  calc_rate.int(
    chamber4.8, starts = 450, wait = 370,
    measure = 55, by = "row", pos = i
  )
  dev.off()
}
png_files <- list.files(pattern = "temp_.*\\.png$", full.names = TRUE)
pdf("Ito11plots.pdf")
for (f in png_files) {
  grid::grid.newpage()
  grid::grid.raster(png::readPNG(f))
}
dev.off()
