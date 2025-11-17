`TopoShieldCalc` is an *R* package for snowpatch hollow metrics calculation in a geographic information system environment. It relies on various functions opf the *terra* package. The first metrics extraction function allows for the extraction of 14 basin-related metrics, i.e., metrics related to the whole area of the snowpatch hollow which encompasses both the floor of the hollow and the backwall. The second metrics extraction function is devoted to metrics extraction from the snowpatch hollows’ main axes. The approach of this function strongly resembles the methodology of the ACME (Spagnolo et al., 2017, Geomorphology) and ACME2 (Li et al., 2024, Geomorphology) toolboxes for the ArcGIS software. 

What you just need to run the two function is a digital elevation model (DEM) of the site or region of interest and a shapefile with the outlines of the snowpatch hollows.

If you have further questions on the toolbox, please reach out: felix.martin.hofmann@geologie.uni-freiburg.de

Note that there will be soon a paper on the performance of the toolbox presented here.

List of the retrieved metrics:

| Group         | Sub-group       | Metrics          | Description                                               |
|---------------|-----------------|------------------|-----------------------------------------------------------|
| Basin-related | Coordinates     | easting          | Easting of the snowpatch hollow’s midpoint (m)            |
|               |                 |                  |                                                           |
|               |                 | northing         | Northing of the snowpatch hollow’s midpoint (m)           |
|               |-----------------|------------------|-----------------------------------------------------------|
|               | 2D surface      | A2D              | The hollow’s map area (m²)                                |
|               |-----------------|------------------|-----------------------------------------------------------|
|               | Elevation       | elevation_min    | Minimum elevation in the snowpatch hollow area (m a.s.l.) |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | elevation_max    | Maximum elevation in the snowpatch hollow area (m a.s.l.) |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | elevation_mean   | Mean elevation in the snowpatch hollow area (m a.s.l.)    |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | elevation_median | Median elevation in the snowpatch hollow area (m a.s.l.)  |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | elevation_range  | Elevation range in the snowpatch hollow area (m)          |
|               |-----------------|------------------|-----------------------------------------------------------|
|               | Aspect          | aspect_mean      | Mean aspect in the snowpatch hollow area (° from North)   |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | aspect_median    | Median aspect in the snowpatch hollow area (° from North) |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | slope_min        | Minimum surface slope in the snowpatch hollow area (°)    |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | slope_mean       | Mean surface slope in the snowpatch hollow area (°)       |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | slope_median     | Median surface slope in the snowpatch hollow area (°)     |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | slope_max        | Maximum surface slope in the snowpatch hollow area (°)    |
|---------------|-----------------|------------------|-----------------------------------------------------------|
| Axes-related  | Axes            | length           | Snowpatch hollow length (m)                               |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | width            | Snowpatch hollow width (m)                                |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | height           | Snowpatch hollow height (m)                               |
|               |                 |------------------|-----------------------------------------------------------|
|               |                 | L_W              | Length/width ratio (unitless)                             |
|               |-----------------|------------------|-----------------------------------------------------------|
|               | Size            | SHS              | Snowpatch hollow size (m)                                 |

## Installation

`TopoShieldCalc` is available from GitHub, so you can use the following code to get the current *released version*:

1. Install `devtools` on your machine

```
install.packages("devtools")
```

2. Load the devtools package

```
library(devtools)
```

3. Install the package with the `install_github()`command
 
```
install_github("fmhofmann/SHME")
```
