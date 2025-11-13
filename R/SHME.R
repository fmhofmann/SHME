#' Load geodata for snowpatch hollow metrics extraction
#'
#' This function allows for loading geodata (digital elevation model (DEM), shapefile (polygons) of the snowpatch hollows, and (optionally) user-defined snowpatch hollow focus points
#'
#' @param focus_point Logical. Should the function upload user-defined focus points? Default to FALSE.
#'
#' @details
#' Please note that this function only accepts geodata with a coordinate reference system whose unit is metres.
#' At the beginning, this function makes sure that the terra package is properly installed.
#' This function adds two objects of SpatVector (see [terra::SpatVector]) representing each the outlines of the snowpatch hollows (polygon) and the focus point (if applicable) and the DEM of the area/region of interest as object of type [terra::SpatRaster].
#' After interactive file selection in a separate window and the upload of the data with [terra::rast] and [terra::vect] for the shapefile and the raster, respectively, the function performs several quality checks.
#' The function first assesses with [terra::linearUnits] whether the linear unit of the coordinate reference system of the DEM is metres.
#' The function then evaluates with [terra::linearUnits] if the linear unit of the coordinate reference system of the DEM is also metres.
#' It subsequently checks with [terra::crs] if the coordinate reference systems of the input-DEM and the input-shapefile(s) are the same.
#' For the next step, the function assesses whether the DEM covers the whole area/region relevant for snowpatch hollow metrics calculation.
#' If "focus_points = TRUE", the function allows the user to interactively select the focus points for axes-related metrics calculation.
#' The abovementioned quality checks are then repeated for the focus point(s).
#' If loading geodata was successful, the function prompts the following message: "Geodata successfully imported."
#'
#' @returns The function adds two or three objects to the global environment depending on whether "focus_point" is set to FALSE or not: "dem", "snowpatch_hollows", and optionally "focus_point_vect".
#' These are of class "SpatRast", "SpatRast", and "SpatVect", respectively.
#' @examples
#' SHME_load_geodata(focus_point = FALSE)
#' @author Dr. Felix Martin Hofmann, University of Freiburg, Germany (\email{fmhofmann9892@@gmail.com})
#' @export
SHME_load_geodata = function(focus_point = FALSE){
  if("terra" %in% rownames(installed.packages()) == FALSE){
    message("Installing the terra package.")
    install.packages("terra")} # Check if the terra package is installed, otherwise install it.
  suppressWarnings(library(terra))
  message("Please select the DEM for metrics calculation")
  dem = terra::rast(file.choose())
  message("Please select shapefile of the hollows for snowpatch metrics calculation")
  snowpatch_hollows = terra::vect(file.choose())
  if(terra::linearUnits(dem) == 0){ # If the linear unit of the coordinate reference system of the DEM is not metres...
    stop("WARNING: the linear unit of the coordinate reference system of the DEM is not metres.")
  }
  if(terra::linearUnits(snowpatch_hollows) == 0){ # If the linear unit of the coordinate reference system of the input-shapefile is not metres...
    stop("WARNING: the linear unit of the coordinate reference system of the input-shapefile is not metres.")
  }
  if(terra::crs(dem) != terra::crs(snowpatch_hollows)){
    stop("WARNING: the coordinate reference systems of the input-DEM and the input-shapefile are not the same.")
  }
  if(terra::xmin(snowpatch_hollows) < (terra::xmin(dem) + 2*terra::res(dem)[1]) | terra::xmax(snowpatch_hollows) > (terra::xmax(dem) - 2*terra::res(dem)[1]) | terra::ymin(snowpatch_hollows) < (terra::ymin(dem) + 2*terra::res(dem)[1]) | terra::ymax(snowpatch_hollows) > (terra::ymax(dem) - 2*terra::res(dem)[1])){
    stop("The DEM does not overlap with the whole area/region relevant for metrics extraction. Please use a DEM which is slightly larger than your area/region of interest.")
  }
  if(focus_point == TRUE){ # If user-defined focus points should also be uploaded
    message("Please select shapefile of the user-defined focus points")
    focus_point_vect = terra::vect(file.choose())
    if(terra::linearUnits(focus_point_vect) == 0){ # If the linear unit of the coordinate reference system of the input-focus points is not metres...
      stop("WARNING: the linear unit of the coordinate reference system of the input-focus points is not metres.")
    }
    if(terra::crs(dem) != terra::crs(focus_point_vect) | terra::crs(snowpatch_hollows) != terra::crs(focus_point_vect)){
      stop("WARNING: the coordinate reference systems of the input-focus points differs from the coordinate reference system of the input-DEM and the input-shapefile of the snowpatch hollows.")
    }
    message("Geodata successfully imported.")
    list = list(snowpatch_hollows = snowpatch_hollows,
                dem = dem,
                focus_point_vect = focus_point_vect)
    return(invisible(list2env(list, envir = globalenv()))) # Return the vectors to the global environment
  } else {
    message("Geodata successfully imported.")
    list = list(snowpatch_hollows = snowpatch_hollows, dem = dem)
    return(invisible(list2env(list, envir = globalenv()))) # Return the vectors to the global environment
  }
}

#' Extract basin-related metrics of snowpatch hollows #
#'
#' @param dem object of class "SpatRast" (see [terra::SpatRaster]) which is nothing else than the digital elevation model (DEM) of the area/region of interest.
#' @param snowpatch_hollows object of class "SpatVect" (see [terra::SpatVector]) with the outline(s) of the snowpatch hollow(s)
#'
#' @details
#' This function allows for extracting 14 basin-related metrics of snowpatch hollows:
#'
#' - Easting
#' - Northing
#' - A2D (the hollows' map area)
#' - elevation_min (minimum elevation in the snowpatch hollow area (m a.s.l.)
#' - elevation_mean (maximum elevation in the snowpatch hollow area (m a.s.l.))
#' - elevation_max (mean elevation in the snowpatch hollow area (m a.s.l.))
#' - elevation_median (median elevation in the snowpatch hollow area (m a.s.l.))
#' - elevation_range (Elevation range in the snowpatch hollow area (m))
#' - aspect_mean (Mean aspect in the snowpatch hollow area (° from North))
#' - aspect_median (Median aspect in the snowpatch hollow area (° from North))
#' - slope_min (Minimum surface slope in the snowpatch hollow area (°))
#' - slope_mean (Mean surface slope in the snowpatch hollow area (°))
#' - slope_median (Median surface slope in the snowpatch hollow area (°))
#' - slope_max (Maximum surface slope in the snowpatch hollow area (°))
#'
#' This tool for the extraction of the 14 basin-related metrics requires a DEM (object of type "SpatRast", see) and a shapefile of the outline of the snowpatch hollows (Object of type "SpatVect", see [terra::SpatVector]).
#' This tool first performs some quality checks, i.e., it assesses whether the input-DEM and the shapefiles of the snowpatch hollow basins have the same coordinate reference system (CRS) and, if so, evaluates if the unit of the CRS is metres.
#' After these quality checks, the function determines the easting and the northing of the hollows’ midpoint.
#' For the third step, the function crops the dem to a slightly larger extent than the snowpatch hollows, derives raster files representing the aspect and the slope, and calculates the metrics associated with aspect and slope.
#' The function subsequently crops the DEM to the snowpatch hollows and computes the metrics related to elevation.
#' It finally exports the 14 basin-related snowpatch hollow metrics as a spreadsheet ("snowpatch_hollow_metrics_1.csv") to the current working directory.
#'
#' @returns
#' snowpatch_hollow_metrics_1
#' A dataframe with the exported, basin-related snowpatch hollow metrics
#' @examples
#' SHME_basin_related_metrics(dem = dem, snowpatch_hollows = snowpatch_hollows)
#'
#' @author Dr. Felix Martin Hofmann, University of Freiburg, Germany (\email{fmhofmann9892@@gmail.com})
#' @export
SHME_basin_related_metrics = function(dem, snowpatch_hollows){
  start_time = Sys.time()
  message(paste("Start time: ", Sys.time(), sep = ""))

  # 1. Check if the terra package is properly installed / quality checks #

  if("terra" %in% rownames(installed.packages()) == FALSE){
    message("Installing the terra package.")
    install.packages("terra")} # Check if the terra package is installed, otherwise install it.
  library(terra)
  if(terra::linearUnits(dem) == 0){ # If the linear unit of the coordinate reference system of the DEM is not metres...
    stop("Metrics calculation aborted: the linear unit of the coordinate reference system of the DEM is not metres")
  }
  if(terra::linearUnits(snowpatch_hollows) == 0){ # If the linear unit of the coordinate reference system of the input-shapefile is not metres...
    stop("Metrics calculation aborted: the linear unit of the coordinate reference system of the input-shapefile is not metres")
  }
  if(terra::crs(dem) != terra::crs(snowpatch_hollows)){
    stop("Metrics calculation aborted: the coordinate reference systems of the input-DEM and the input-shapefile are not the same")
  }

  # 2. Determine x- and y-coordinates of the snowpatch hollows' midpoints #

  easting = data.frame(terra::geom(terra::centroids(snowpatch_hollows)))[,3]
  northing = data.frame(terra::geom(terra::centroids(snowpatch_hollows)))[,4]

  message("Midpoints of the snowpatch hollows successfully determined.")

  # 3. Determine snowpatch hollow 2D area #

  A2D = terra::expanse(snowpatch_hollows, unit = "m")

  message("Snowpatch hollow 2D areas successfully calculated.")

  # 4. Determine the variables related to aspect, slope, and elevation #

  for(i in 1:length(snowpatch_hollows)){
    extent = terra::ext(terra::xmin(snowpatch_hollows[i]) - (2*terra::res(dem)[1]),
                        terra::xmax(snowpatch_hollows[i]) + (2*terra::res(dem)[1]),
                        terra::ymin(snowpatch_hollows[i]) - (2*terra::res(dem)[1]),
                        terra::ymax(snowpatch_hollows[i]) + (2*terra::res(dem)[1])) # Create a spatial extent slightly bigger than the area of interest
    aspect = terra::crop(terra::terrain(terra::crop(dem, extent),
                                        "aspect"),
                         snowpatch_hollows[i],
                         mask = TRUE) # Crop the dem to extent, derive a raster representing the aspect, and crop this raster to the snowpatch hollow
    slope = terra::crop(terra::terrain(terra::crop(dem, extent),
                                       "slope"),
                        snowpatch_hollows[i],
                        mask = TRUE) # Crop the dem to extent, derive a raster representing the slope, and crop this raster to the snowpatch hollow
    dem_cropped = terra::crop(dem,
                              snowpatch_hollows[i],
                              mask = TRUE)
    if(i == 1){
      aspect_mean = vector()
      aspect_median = vector()
      slope_min = vector()
      slope_mean = vector()
      slope_median = vector()
      slope_max = vector()
      elevation_min = vector()
      elevation_mean = vector()
      elevation_max = vector()
      elevation_median = vector()
      elevation_range = vector()
    } # Initialise vectors
    aspect_mean = c(aspect_mean,
                    mean(terra::as.data.frame(aspect)[,1]))
    aspect_median = c(aspect_median,
                      median(terra::as.data.frame(aspect)[,1]))
    slope_min = c(slope_min,
                  terra::global(slope, "min", na.rm = TRUE)[,1])
    slope_mean = c(slope_mean,
                   terra::global(slope, "mean", na.rm = TRUE)[,1])
    slope_median = c(slope_median,
                     median(terra::as.data.frame(slope)[,1]))
    slope_max = c(slope_max,
                  terra::global(slope, "max", na.rm = TRUE)[,1])
    elevation_min = c(elevation_min,
                      terra::global(dem_cropped, "min", na.rm = TRUE)[,1])
    elevation_mean = c(elevation_mean,
                       terra::global(dem_cropped, "mean", na.rm = TRUE)[,1])
    elevation_max = c(elevation_max,
                      terra::global(dem_cropped, "max", na.rm = TRUE)[,1])
    elevation_median = c(elevation_median,
                         median(terra::as.data.frame(dem_cropped)[,1]))
    elevation_range = c(elevation_range,
                        abs(terra::minmax(dem_cropped)[2] - terra::minmax(dem_cropped)[1]))
    message(paste("Snowpatch hollow #", i, " processed.", sep = ""))
    if (i == length(snowpatch_hollows)){
      message("Metrics related to aspect, slope, and elevation successfully calculated.")
    }
  }

  # 5. Put the extracted metrics in one dataframe #

  ID = 1:length(snowpatch_hollows)

  snowpatch_hollow_metrics = data.frame(cbind(ID,
                                              easting,
                                              northing,
                                              A2D,
                                              elevation_min,
                                              elevation_mean,
                                              elevation_median,
                                              elevation_max,
                                              elevation_range,
                                              aspect_mean,
                                              aspect_median,
                                              slope_min,
                                              slope_mean,
                                              slope_median,
                                              slope_max))
  write.table(snowpatch_hollow_metrics,
              file = "snowpatch_hollow_metrics_1.csv",
              sep = ";",
              row.names = FALSE,
              col.names = TRUE,
              na = "NA")
  message("Spreadsheet (.csv file) with the results successfully exported.")

  # 6. Calculate the processing time and remove unnecessary elements in the environment #

  end_time = Sys.time()
  processing_time = as.vector(difftime(end_time,
                                       start_time,
                                       units = "mins"))
  message(paste("End time: ", end_time, sep = ""))
  message(paste("Processing time: ", round(processing_time, digits = 2), " mins.", sep =""))

  # 7. Remove unnecessary objects #

  rm(list = setdiff(ls(), c("dem",
                            "snowpatch_hollows",
                            "snowpatch_hollow_metrics")))

  # 8. Add the dataframe with snowpatch hollow metrics to the environment #

  list = list(snowpatch_hollow_metrics_1 = snowpatch_hollow_metrics)
  return(invisible(list2env(list, envir = globalenv()))) # Return the vectors to the global environment
}

#' Extract axes-related metrics of snowpatch hollows #
#'
#' @param dem object of class "SpatRast" (see [terra::SpatRaster]) which is nothing else than the digital elevation model (DEM) of the area/region of interest.
#' @param snowpatch_hollows object of class "SpatVect" (see [terra::SpatVector]) with the outline(s) of the snowpatch hollow(s)
#' @param step Numeric. The distance between the interval nodes along the outline. Default to 5 m.
#' @details
#' This function allows for the calculation of the five axes-related metrics.
#' These are length (snowpatch hollow length (m)), width (snowpatch hollow width (m)), height (snowpatch hollow height (m)), L_W (length/width ratio (unitless)), SHS (Snowpatch hollow size, defined as the cube root of the product of length, width, and height (m)))
#' This function first performs the same quality checks as the function for basin-related metrics but additionally fixes topology issues, if applicable.
#' Obtaining the axes-related metrics requires the identification of the snowpatch hollow focus, i.e., the snowpatch hollows thresholds’ midpoints.
#' Note that the snowpatch hollow threshold is the relative flat part of a snowpatch hollow with slight topographical variations, corresponding to the “entrance” to a snowpatch hollow.
#' THis function first converts the polygon of the snowpatch hollow basin into a polyline, on which it sets interval nodes (spacing: 5 m).
#' To derive snowpatch hollow threshold points, the function first removes all interval nodes that have an elevation smaller than the average elevation at all interval nodes and then creates a histogram of the elevations at these points.
#' From this histogram, the function selects the threshold points, i.e., all interval nodes which have elevations in the most frequent elevation belt.
#' The next step is to select the focus point, which represents the centre of the selected threshold points.
#'
#' The main axis of the snowpatch hollow should meet the condition that it divides the hollow into two halves of equal size, starting from the focus point.
#' The function therefore searches for the point on its backwall (target elevation: ≥ mean elevation at all points) which best satisfies this condition and creates an axis from the focus to this point.
#' After the determination of the hollow’s length, the function determines the width axis which passes through the midpoint of the length axis.
#' The function copies and rotates the length axis 90° around its midpoint, extends it, determines the intersection points with the polygon of the snow patch hollows, and calculates the distance between these two intersection points.
#' The determination of the hollows’ length and width allows for calculating the size of snowpatch hollows (SHS), given in m (cube root of length plus width plus height).
#' The height corresponds to the difference in elevation between the snowpatch hollow focus and the endpoint of the main axis on the hollow’s outline.
#' Finally, the function exports the four axes-related metrics as a spreadsheet creates ESRI shapefiles of the focus points, threshold points, and the axes, respectively.
#'
#' @returns
#' snowpatch_hollow_metrics_2
#' A dataframe with the results of axes-related metrics extraction.
#' @examples
#' SHME_axes_related_metrics(dem = dem, snowpatch_hollows = snowpatch_hollows, step = 5)
#' @author Dr. Felix Martin Hofmann, University of Freiburg, Germany (\email{fmhofmann9892@@gmail.com})
#' @export
SHME_axes_related_metrics = function(dem,
                                     snowpatch_hollows,
                                     step = 5){
  for (i in 1:length(snowpatch_hollows)){
    if(i == 1){
      start_time = Sys.time()
      message(paste("Start time: ", Sys.time(), sep = ""))
      if("terra" %in% rownames(installed.packages()) == FALSE){
        message("Installing the terra package.")
        install.packages("terra")} # Check if the terra package is installed, otherwise install it.
      suppressWarnings(library(terra))
      snowpatch_hollows = terra::makeValid(snowpatch_hollows)
      message("Detect and fix topology problems (if applicable).")
      if(terra::linearUnits(dem) == 0){ # If the linear unit of the coordinate reference system of the DEM is not metres...
        stop("Metrics calculation aborted: the linear unit of the coordinate reference system of the DEM is not metres.")
      }
      if(terra::linearUnits(snowpatch_hollows) == 0){ # If the linear unit of the coordinate reference system of the input-shapefile is not metres...
        stop("Metrics calculation aborted: the linear unit of the coordinate reference system of the input-shapefile is not metres.")
      }
      if(terra::crs(dem) != terra::crs(snowpatch_hollows)){
        stop("Metrics calculation aborted: the coordinate reference systems of the input-DEM and the input-shapefile are not the same.")
      }
    }

    # 1. Create a subset of points for focus points calculation #

    sampled_points_xyz = terra::extract(dem,
                                        terra::spatSample(terra::as.lines(snowpatch_hollows[i]),
                                                          method = "regular",
                                                          size = floor(terra::perim(terra::as.lines(snowpatch_hollows[i]))/5)), # Create regularly spaced points along the outline of the snowpatch hollows (spacing: xy-resolution of the DEM)
                                        method = "bilinear",
                                        xy = TRUE) # Get the elevation at the points from dem
    points_subset = subset(sampled_points_xyz,
                           sampled_points_xyz[,2] < mean(sampled_points_xyz[,2])) # Exclude all points with an elevation higher than the mean elevation of all points
    breaks = seq(from = min(points_subset[,2]),
                 to = max(points_subset[,2]) + step*2,
                 by = step) # Create the breaks for the histogram of elevation

    # 2. Determine threshold points #

    if(length(breaks) < 3){
      threshold_points = points_subset
    } else {
      hist = hist(points_subset[,2], # Create a histogram of elevations in the bins
                  breaks = breaks,
                  plot = FALSE)
      threshold_points = points_subset[(points_subset[,2] >= breaks[which(max(hist$counts[1]) == max(hist$counts[1]))]) & (points_subset[,2] <= breaks[which(max(hist$counts[1]) == max(hist$counts[1])) + 1]),]
    }
    if (i == 1){ # During the first iteration
      threshold_vect = terra::vect(cbind(points_subset[,3], # Create a new object of class SpatVect with the snowpatch hollow thresholds
                                         points_subset[,4]),
                                   crs = terra::crs(snowpatch_hollows))
    } else { # During all subsequent iterations
      threshold_vect = rbind(threshold_vect,
                             terra::vect(cbind(threshold_points[,3],
                                               threshold_points[,4]),
                                         crs = terra::crs(snowpatch_hollows)))
    }

    # 3. Determine the snowpatch hollow focus #

    distance = as.vector(terra::distance(as.matrix(cbind(threshold_points[,3],
                                                         threshold_points[,4])),
                                         as.matrix(cbind(mean(threshold_points[,3]),
                                                         mean(threshold_points[,4]))),
                                         lonlat = FALSE,
                                         unit = "m"))
    # Determine the distance of all threshold points to the threshold points' midpoint
    if(i == 1){ # During the first iteration
      focus_point_vect = terra::vect(cbind(threshold_points[,3][which(distance == min(distance))],
                                           threshold_points[,4][which(distance == min(distance))]),
                                     crs = terra::crs(snowpatch_hollows))
    } else { # During all subsequent iterations
      focus_point_vect = rbind(focus_point_vect,
                               terra::vect(cbind(threshold_points[,3][which(distance == min(distance))],
                                                 threshold_points[,4][which(distance == min(distance))]),
                                           crs = terra::crs(snowpatch_hollows)))
    }

    # 4. Derive the snowpatch hollow length #

    points_subset2 = sampled_points_xyz[which(sampled_points_xyz[,2] > mean(sampled_points_xyz[,2])),] # Exclude all points with an elevation lower than the mean elevation of all points
    for (k in 1:length(points_subset2[,1])){
      if(k == 1){polygon_area_diff = vector()}
      possible_axis = terra::vect(cbind(c(points_subset2[k,3],terra::xmin(focus_point_vect[i])),
                                        c(points_subset2[k,4],terra::ymin(focus_point_vect[i]))),
                                  crs = terra::crs(snowpatch_hollows),
                                  type = "lines")
      possible_axis = terra::elongate(possible_axis,
                                      length = 1000)
      n_intersections = suppressWarnings(as.numeric(length(terra::as.points(terra::intersect(snowpatch_hollows[i],
                                                                                             possible_axis))))) # Determine the number of intersections
      if(n_intersections != 2){ # If the possible axis and the snowpatch hollow do not intersect at all or intersect more than two times...
        polygon_area_diff = c(polygon_area_diff,Inf)
        next}
      polygon_area = terra::expanse(terra::split(snowpatch_hollows[i],
                                                 possible_axis),
                                    unit = "m")
      polygon_area_diff = c(polygon_area_diff,
                            abs(polygon_area[1] - polygon_area[2]))
    }
    points_subset3 = points_subset2[which(polygon_area_diff == min(polygon_area_diff)),]
    if(i == 1){
      axis_length = terra::vect(cbind(id = 1,
                                      part = 1,
                                      c(points_subset3[,3], terra::xmin(focus_point_vect[i])),
                                      c(points_subset3[,4], terra::ymin(focus_point_vect[i]))),
                                crs = terra::crs(snowpatch_hollows),
                                type = "lines")
    } else {
      axis_length = rbind(axis_length,
                          terra::vect(cbind(id = 1,
                                            part = 1,
                                            c(points_subset3[,3], terra::xmin(focus_point_vect[i])),
                                            c(points_subset3[,4], terra::ymin(focus_point_vect[i]))),
                                      crs = terra::crs(snowpatch_hollows),
                                      type = "lines"))
    }

    # 5. Determine the snowpatch hollow width #

    axis = terra::crop(terra::elongate(terra::spin(axis_length[i],
                                                   angle = 90),
                                       length = 10000),
                       snowpatch_hollows[i])
    if(length(axis) > 1){ # If axis contains multiple lines...
      message("NOTE: multiple axes for the axis representing the width. Selecting the longest one.")
      axis = terra::subset(axis,
                           as.vector(terra::relate(axis, axis_length[i], relation = "crosses")) == TRUE)
    }
    if(i == 1){
      axis_width = axis
    } else {
      axis_width = rbind(axis_width, axis)
    }

    # 6. Derive the snowpatch hollow height #

    dem_cropped = terra::crop(dem,
                              snowpatch_hollows[i],
                              mask = TRUE)

    if(i == 1){
      axis_height = terra::minmax(dem_cropped)[2] - terra::minmax(dem_cropped)[1]
    } else {
      axis_height = c(axis_height,
                      terra::minmax(dem_cropped)[2] - terra::minmax(dem_cropped)[1])
    }
    message(paste("Snowpatch hollow #",i," processed.",
                  sep = ""))

    if(i == length(snowpatch_hollows)){

      # 7. Put the extracted metrics in a table #

      ID = 1:length(snowpatch_hollows)
      snowpatch_hollow_metrics = data.frame(cbind(ID,
                                                  terra::perim(axis_length),
                                                  terra::perim(axis_width),
                                                  axis_height,
                                                  terra::perim(axis_length)/terra::perim(axis_width),
                                                  (terra::perim(axis_length) + terra::perim(axis_length) + axis_height)^1/3)) # Determine the snowpatch hollow size
      colnames(snowpatch_hollow_metrics) = c("ID","length","width","height","L_W","SHS")
      write.table(snowpatch_hollow_metrics,
                  file = "snowpatch_hollow_metrics_2.csv",
                  row.names = FALSE,
                  sep = ";")
      message("Snowpatch hollow metrics successfully extracted.")

      # 8. Export geodata to the current working directory #

      terra::writeVector(focus_point_vect,
                         filename = "focus_points.shp",
                         filetype = "ESRI Shapefile",
                         overwrite = TRUE)
      terra::writeVector(threshold_vect,
                         filename = "threshold_points.shp",
                         filetype = "ESRI Shapefile",
                         overwrite = TRUE)
      terra::writeVector(axis_length,
                         filename = "snowpatch_hollow_length.shp",
                         filetype = "ESRI Shapefile",
                         overwrite = TRUE)
      terra::writeVector(axis_width,
                         filename = "snowpatch_hollow_width.shp",
                         filetype = "ESRI Shapefile",
                         overwrite = TRUE)

      # 9. Calculate the processing time and remove unnecessary elements in the environment #

      end_time = Sys.time()
      processing_time = as.vector(difftime(end_time,
                                           start_time,
                                           units = "mins"))
      message(paste("End time: ", end_time, sep = ""))
      message(paste("Processing time: ", round(processing_time, digits = 2), " mins.", sep =""))

      # 10. Remove all unnecessary variables from the environment

      rm(list = setdiff(ls(), c("dem","snowpatch_hollows","snowpatch_hollow_metrics","step")))
      list = list(snowpatch_hollow_metrics_2 = snowpatch_hollow_metrics)
      return(invisible(list2env(list, envir = globalenv()))) # Return the selected objects to the global environment
    }
  }
}
