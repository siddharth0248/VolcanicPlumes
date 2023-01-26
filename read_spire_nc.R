## ---------------------------
##
## Script name: Reading spire dataset
##
## Purpose of script: Plotting bending angle at different pressure level to study volcanic plumes
## which took place in Hawaii last week of Nov 2022 till first week of Dec 2022
##
## Author: Dr. Siddharth Chaudhary
##
## Date Created: 2023-01-23
##
## Copyright (c) Siddharth Chaudhary, 2023
## Email: sc0248@uah.edu
##
## ---------------------------
##
## Notes:
##
##
## ---------------------------

## required packages
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sf) # optional
library(ggpubr) # used to arrange ggplot
library(lubridate)

#Open all files
files = list.files(
  "/Volumes/UAH/Impact/DIA/SPIRE/Data/Volcano/2022/",
  pattern = '*.nc',
  full.names = TRUE,
  recursive = T
)

#Location of volcanic eruption
volcano <- data.frame(Lon = -155.58938, Lat = 19.50054)

for (i in seq_along(files)) {
  nc_data <- nc_open(files[i])
  attributes(nc_data$var)
  
  #saving MMDDYYYY can use POSIX Date-Time function instead
  day <- ncatt_get(nc_data, 0, "day")
  day <- day[2]
  month <- ncatt_get(nc_data, 0, "month")
  month <- month[2]
  year <- ncatt_get(nc_data, 0, "year")
  year <- year[2]
  MMDDYYYY <<- paste(month, day, year, sep = "/")
  
  #Retrieving variables of interest
  bScore <- ncvar_get(nc_data, "bScore")
  bScore <- round(bScore, digits = 1)
  if (bScore > 35) {
    next
  }
  Bend_ang <- ncvar_get(nc_data, "Bend_ang")
  R_LEO <- ncvar_get(nc_data, "R_LEO")
  Impact_parm <- ncvar_get(nc_data, "Impact_parm")
  Pres <- ncvar_get(nc_data, "Pres")
  Comparision <- as.data.frame(cbind(Bend_ang, Pres))
  Temp <- ncvar_get(nc_data, "Temp")
  
  Lon <- ncvar_get(nc_data, "Lon")
  Lat <- ncvar_get(nc_data, "Lat")
  Loc <- as.data.frame(cbind(Lat, Lon))
  
  #Plot for Bending angle and pressure
  a <-  ggplot(data = Comparision, aes(x = Bend_ang, y = Pres)) +
    geom_line() + # make this a line plot
    scale_y_reverse(limits = c(1000, 0),
                    breaks = seq(0, 1000, 250)) +
    scale_x_continuous(limits = c(0, 0.03),
                       breaks = seq(0, 0.03, 0.005)) +
    xlab("Bending angle (rad)") + ylab("Pressure (mb)") +
    theme_bw()
  
  #Plot for locations
  b <- ggplot() +
    geom_point(data = Loc, aes(x = Lon, y = Lat), color = "black") +
    geom_point(data = volcano,
               aes(x = Lon, y = Lat),
               color = "red",
               size = 4) + #volcanic erruption
    scale_x_continuous(limits = c(-157, -153),
                       breaks = seq(-157, -153, 0.5)) +
    scale_y_continuous(limits = c(18, 21), breaks = seq(18, 21, 0.5)) +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ))
  
  #arranging both plots in one figure using ggrange
  figure <-
    ggarrange(
      a,
      b,
      ncol = 2,
      nrow = 1,
      widths = c(2, 1),
      heights = c(2, 1)
    )
  
  #adding common title to both the plots
  print(annotate_figure(
    figure,
    top = text_grob(
      paste(
        "Bending angle at different pressure levels on ",
        MMDDYYYY,
        " and score ",
        bScore,
        sep = ""
      )
    ),
    bottom = text_grob(
      "Data source: \n Spire Level 2 RO Atmospheric Profile",
      color = "blue",
      hjust = 1,
      x = 1,
      face = "italic",
      size = 10
    )
  )) # use the black and white theme
  
  d <- mdy(MMDDYYYY)
  ggsave(p, file=paste0("Pressure_",d,".png"), width = 14, height = 10, units = "cm",bg = "white")
}
