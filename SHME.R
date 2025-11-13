#########################################################################
### Analysis of snowpatch hollow metrics for the central Swabian Jura ###
#########################################################################

# 1. Load the required data #

data1 = read.csv("snowpatch_hollow_metrics_1.csv",
                 header = TRUE,
                 sep = ";")

data2 = read.csv("snowpatch_hollow_metrics_2.csv",
                 header = TRUE,
                 sep = ";")

# 2. Snowpatch hollow metrics analysis #

## 2.1 Snowpatch hollow area (A2D) ## 

cm_to_inch = 0.3937007874

pdf(file = "Snowpatch_hollow_metrics_analysis.pdf",
    width = 16 * cm_to_inch,
    height = 20 * cm_to_inch,
    pointsize = 6,
    useDingbats = TRUE,
    compress = FALSE)

# Set the outer margin #

par(omi = rep(cm_to_inch*0.5, times = 4))

# Set the inner margin # 

par(mai = rep(cm_to_inch*0.75, times = 4))

# Create multiple plots #

par(mfrow = c(5,2)) # Four rows and two columns 
par(cex = 1)

breaks = seq(from = 0,
             to = 120000,
             by = 5000)

par(xpd = NA)

hist = hist(data1$A2D,
            breaks = breaks,
            labels = TRUE, # Put labels on top of the bars
            main = NULL,
            xlab = "Snowpatch hollow area (m2)",
            ylab = "Frequency (n)")

## 2.2 Minimum elevation (elevation_min) #

breaks = seq(from = 650,
             to = 850,
             by = 10)

hist = hist(data1$elevation_min,
            breaks = breaks,
            labels = TRUE, # Put labels on top of the bars
            main = NULL,
            xlab = "Snowpatch hollow minimum elevation (m a.s.l.)",
            ylab = "Frequency (n)")

## 2.3 Mean elevation (elevation_mean) #

breaks = seq(from = 650,
             to = 850,
             by = 10)

hist = hist(data1$elevation_mean,
            breaks = breaks,
            main = NULL,
            labels = TRUE, # Put labels on top of the bars
            xlab = "Snowpatch hollow mean elevation (m a.s.l.)",
            ylab = "Frequency (n)")

## 2.4 Elevation range (elevation_range) #

breaks = seq(from = 0,
             to = 120,
             by = 10)

par(xpd = NA)

hist = hist(data1$elevation_range,
            breaks = breaks,
            main = NULL,
            labels = TRUE, # Put labels on top of the bars
            xlab = "Snowpatch hollow elevation range (m)",
            ylab = "Frequency (n)")

# 2.5 Mean aspect (aspect_mean) # 

breaks = seq(from = -22.5,
             to = 360,
             by = 45)

par(xpd = NA)

hist = hist(data1$aspect_mean,
            breaks = breaks,
            main = NULL,
            labels = TRUE, # Put labels on top of the bars
            xlab = "Snowpatch hollow aspect (cardinal direction)",
            ylab = "Frequency (n)",
            axes = FALSE)

axis(side = 1,
     at = hist$mids,
     labels = c("N","NE","E","SE","S","SW","W","NW"),
     lwd = 0,
     lwd.ticks = 0) 

axis(side = 2,
     at = seq(from = 0, to = 120, by = 30))

# 2.7 Length # 

breaks = seq(from = 0,
             to = 500,
             by = 20)

par(xpd = NA)

hist = hist(data2$length,
            breaks = breaks,
            main = NULL,
            labels = TRUE, # Put labels on top of the bars
            xlab = "Snowpatch hollow length (m)",
            ylab = "Frequency (n)")

# 2.8 Width # 

breaks = seq(from = 0,
             to = 400,
             by = 20)

par(xpd = NA)

hist = hist(data2$width,
            breaks = breaks,
            main = NULL,
            labels = TRUE, # Put labels on top of the bars
            xlab = "Snowpatch hollow width (m)",
            ylab = "Frequency (n)")

# 2.9 Length/width ratio (L/W) # 

breaks = seq(from = 0,
             to = 5,
             by = 0.25)

par(xpd = NA)

hist = hist(data2$L_W,
            breaks = breaks,
            main = NULL,
            labels = TRUE, # Put labels on top of the bars
            xlab = "Snowpatch hollow length/width ratio",
            ylab = "Frequency (n)")

# 2.10 Snowpatch hollow size (SHS) #

breaks = seq(from = 0,
             to = 400,
             by = 25)

par(xpd = NA)

hist = hist(data2$SHS,
            breaks = breaks,
            main = NULL,
            labels = TRUE, # Put labels on top of the bars
            xlab = "Snowpatch hollow size (m)",
            ylab = "Frequency (n)")

dev.off()

# Median aspect (aspect_median) # 

breaks = seq(from = -22.5,
             to = 360,
             by = 45)

par(xpd = NA)

hist = hist(data1$aspect_median,
            axes = FALSE,
            ann = FALSE,
            breaks = breaks,
            labels = TRUE, # Put labels on top of the bars
            xlab = "Snowpatch hollow median aspect (degrees from N)",
            ylab = "Frequency (n)")

axis(side = 1,
     at = hist$mids,
     labels = c("N","NE","E","SE","S","SW","W","NW"),
     lwd = 0,
     lwd.ticks = 0) 

axis(side = 2,
     at = seq(from = 0, to = 125, by = 25))

# Minimum elevation versus cirque aspect # 

plot(data1$aspect_mean,
     data1$elevation_min)

# Cirque aspect versus minimum elevation (in classes) # 

z = cut(data1$aspect_mean,
        breaks = seq(from = 22.5, to = 360, by = 45),
        c("NE","E","SE","S","SW","W","NW"))

#######################################
### Chi-Square Goodness-of-Fit Test ### 
#######################################

# 1. Load the required packages #

install.packages("readxl")

library(readxl)

# 2. Load the data and create a function for rounding #

terrain_elevation = readxl::read_xlsx("RASTERVALUES_elevation.xlsx")

terrain_slope = readxl::read_xlsx("RASTERVALUES_slope.xlsx")

terrain_aspect = readxl::read_xlsx("RASTERVALUES_aspect.xlsx")

round2 <- function(x, digits) {  #Function to always round 0.5 up
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}

# 3. Put the data in classes #

# 3.1 Elevation # 

# 3.1.1 Terrain #

terrain_elevation2 = as.numeric(terrain_elevation$ELEVATION[which(terrain_elevation$ELEVATION > 675 & terrain_elevation$ELEVATION < 825)])

breaks_elevation = seq(from = 675,
                       to = 825,
                       by = 25)

terrain_elevation_hist = hist(terrain_elevation2,
                              breaks = breaks_elevation,
                              plot = TRUE)

terrain_elevation_counts = terrain_elevation_hist$counts

terrain_elevation_counts_share = terrain_elevation_hist$counts/sum(terrain_elevation_counts)

terrain_elevation_counts_adjusted = terrain_elevation_counts_share*length(data1$elevation_mean)

# 3.1.2 Snowpatch hollows #

snowpatch_hollows_hist = hist(data1$elevation_mean,
                              breaks = breaks_elevation,
                              plot = TRUE)

snowpatch_hollows_elevation_counts = snowpatch_hollows_hist$counts

# 3.2 Slope #

# 3.2.1 Terrain #

terrain_slope = terrain_slope$SLOPE[which(terrain_slope$SLOPE < 35)]

breaks_slope = seq(from = 0,
                   to = 35,
                   by = 5)

terrain_slope_hist = hist(terrain_slope,
                          breaks = breaks_slope,
                          plot = TRUE)

terrain_slope_counts = terrain_slope_hist$counts

terrain_slope_counts_share = terrain_slope_counts/sum(terrain_slope_counts)

terrain_slope_counts_adjusted = terrain_slope_counts_share*length(data1$slope_mean)

# 3.2.2 Snowpatch hollows #

dem = terra::rast(file.choose()) # Import the DEM

axes = terra::vect(file.choose()) # Import the longest axes of the snowpatch hollows

axes_points = terra::as.points(axes)

axes_points_extract = terra::extract(dem,
                                     axes_points, # Create a point at the beginning and at the end of each polyline
                                     method = "bilinear") # Retrieve the elevation at these points from the DEM

axes_points_extract = axes_points_extract[,2] # Retrieve the elevation from axes_points_extract 

axes_length = terra::perim(axes) # Determine the length of the longest axes

index_1 = seq(from = 1, to = length(axes_points_extract), by = 2)
index_2 = seq(from = 2, to = length(axes_points_extract), by = 2)

axes_diff_elevation = abs(axes_points_extract[index_1] - axes_points_extract[index_2]) # Determine the difference in elevation
  
angle = (axes_diff_elevation/axes_length)*(180/pi)

breaks_slope = seq(from = 0,
                   to = 35,
                   by = 5)

snowpatch_hollows_hist = hist(angle,
                              breaks = breaks_slope,
                              plot = TRUE)

snowpatch_hollows_slope_counts = snowpatch_hollows_hist$counts

# 3.3 Aspect #

# 3.3.1 Terrain #

breaks_aspect = c(0,
                  seq(from = 22.5,
                      to = 337.5,
                      by = 45),
                  360) 

terrain_aspect_hist = hist(terrain_aspect$ASPECT,
                           breaks = breaks_aspect,
                           plot = TRUE)

terrain_aspect_counts = c(terrain_aspect_hist$counts[1] + terrain_aspect_hist$counts[9],
                          terrain_aspect_hist$counts[2:8])

terrain_aspect_counts_share = terrain_aspect_counts/sum(terrain_aspect_counts)
    
terrain_aspect_counts_adjusted = terrain_aspect_counts_share*length(data1$elevation_mean)

# 3.3.2 Snowpatch hollows #

snowpatch_hollows_aspect_hist = hist(data1$aspect_mean,
                                     breaks = breaks_aspect,
                                     plot = TRUE)

snowpatch_hollows_aspect_counts = c(snowpatch_hollows_aspect_hist$counts[1] + snowpatch_hollows_aspect_hist$counts[9],
                                    snowpatch_hollows_aspect_hist$counts[2:8])

# 4.1 Perform the chi-squared test of goodness of fit #

cm_to_inch = 0.3937007874

pdf(file = "Snowpatch_hollow_metrics_analysis_2.pdf",
    width = 16 * cm_to_inch,
    height = 6 * cm_to_inch,
    pointsize = 6,
    useDingbats = TRUE,
    compress = FALSE)

# Set the outer margin #

par(omi = rep(cm_to_inch*0, times = 4))

# Set the inner margin # 

par(mai = rep(cm_to_inch*0.5, times = 4))

# Create multiple plots #

par(mfrow = c(1,4)) # Four rows and two columns 
par(cex = 1)

# Set the line width #

par(lwd = 2/3) # 2/3 is equal to 0.5 pt

# Set ylim for the histograms #

ylim = c(-100,350)

# 4.1.1 Elevation # 

chisquared_test_elevation = chisq.test(snowpatch_hollows_elevation_counts,
                                       p = terrain_elevation_counts_share,
                                       simulate.p.value = TRUE)

frequency_anomaly_1 = ((snowpatch_hollows_elevation_counts - terrain_elevation_counts_adjusted)/terrain_elevation_counts_adjusted)*100

classes_elevation = c("675-700","700-725","725-750","750-775","775-800","800-825")

barplot(frequency_anomaly_1,
        xlab = "Elevation (m a.s.l.)",
        ylab = "Difference between observed and expected frequency (n)",
        main = "Elevation",
        names.arg = classes_elevation,
        space = 0,
        axes = FALSE, 
        ylim = ylim)

text(x = seq(from = 0.5,
             to = length(classes_elevation) - 0.5,
             by = 1),
     y = 150,
     labels = round2(frequency_anomaly_1, digits = 1)) 

axis(side = 2,
     at = seq(from = min(ylim),
              to = max(ylim),
              by = 50))

abline(a = 0,
       b = 0)

# 4.1.2 Slope #

chisquared_test_slope = chisq.test(snowpatch_hollows_slope_counts,
                                   p = terrain_slope_counts_share,
                                   simulate.p.value = TRUE)

frequency_anomaly_2 = ((snowpatch_hollows_slope_counts - terrain_slope_counts_adjusted)/terrain_slope_counts_adjusted)*100

classes_slope = c("0-5","5-10","10-15","15-20","20-25","25-30","30-35")

barplot(frequency_anomaly_2,
        xlab = "Slope angle (°)",
        ylab = "Anomaly in frequency (n)",
        main = "Slope",
        names.arg = classes_slope,
        axes = FALSE,
        space = 0,
        ylim = ylim)

text(x = seq(from = 0.5,
             to = length(classes_slope) - 0.5,
             by = 1),
     y = 150,
     labels = round2(frequency_anomaly_2, digits = 1)) 

abline(a = 0,
       b = 0)

# 4.1.3 Aspect #

chisquared_test_aspect = chisq.test(snowpatch_hollows_aspect_counts,
                                    p = terrain_aspect_counts_share,
                                    simulate.p.value = TRUE)

frequency_anomaly_3 = ((snowpatch_hollows_aspect_counts - terrain_aspect_counts_adjusted)/terrain_aspect_counts_adjusted)*100

classes_aspect = c("N","NE","E","SE","S","SW","W","NW")

barplot(frequency_anomaly_3,
        xlab = "Cardinal direction",
        names.arg = classes_aspect,
        main = "Aspect",
        ylim = ylim,
        space = 0,
        axes = FALSE)

text(x = seq(from = 0.5,
             to = length(classes_aspect) - 0.5,
             by = 1),
     y = 150,
     labels = round2(frequency_anomaly_3, digits = 1)) 

abline(a = 0,
       b = 0)

#############################################################################################
### Chi-Square Goodness-of-Fit Test for aspect at elevations between 725 and 750 m a.s.l. ###
#############################################################################################

# 1. Create the subset of the data #

terrain_elevation_subset = terrain_elevation$ELEVATION[which(terrain_elevation$ELEVATION >= 725 & terrain_elevation$ELEVATION < 750)]

terrain_aspect_subset = terrain_aspect$ASPECT[which(terrain_elevation$ELEVATION >= 725 & terrain_elevation$ELEVATION < 750)]

# 2. Determine the number of frequencies #

# 2.1 Determine the number of frequencies for aspect in the 725-750 m a.s.l. range #

breaks_aspect = c(0,
                  seq(from = 22.5,
                      to = 337.5,
                      by = 45),
                  360) 

terrain_aspect_subset_hist = hist(terrain_aspect_subset,
                                  breaks = breaks_aspect,
                                  plot = FALSE)

terrain_aspect_subset_counts = c(terrain_aspect_subset_hist$counts[1] + terrain_aspect_subset_hist$counts[9],
                                 terrain_aspect_subset_hist$counts[2:8])

# 2.1 Determine the number of frequencies for the whole study region #

terrain_aspect_hist = hist(terrain_aspect$ASPECT,
                           breaks = breaks_aspect,
                           plot = FALSE)

terrain_aspect_counts = c(terrain_aspect_hist$counts[1] + terrain_aspect_hist$counts[9],
                          terrain_aspect_hist$counts[2:8])

terrain_aspect_counts_share = terrain_aspect_counts/sum(terrain_aspect_counts)

terrain_aspect_counts_adjusted = terrain_aspect_counts_share*length(terrain_aspect_subset)

# 3. Perform the goodness-of-fit chi-squared test #

chisquared_test_aspect_subset = chisq.test(terrain_aspect_subset_counts,
                                           p = terrain_aspect_counts_share,
                                           simulate.p.value = TRUE)

frequency_anomaly = ((chisquared_test_aspect_subset$observed - chisquared_test_aspect_subset$expected)/chisquared_test_aspect_subset$expected)*100

classes_aspect = c("N","NE","E","SE","S","SW","W","NW")

barplot(frequency_anomaly,
        xlab = "Cardinal direction",
        names.arg = classes_aspect,
        main = "Aspect",
        ylim = ylim,
        space = 0,
        axes = FALSE)

text(x = seq(from = 0.5,
             to = length(classes_aspect) - 0.5,
             by = 1),
     y = 150,
     labels = round2(frequency_anomaly, digits = 1))

abline(a = 0,
       b = 0)

dev.off()