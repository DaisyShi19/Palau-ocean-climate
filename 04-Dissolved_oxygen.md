## **Indicator: Dissolved Oxygen**

Dissolved oxygen (DO) refers to the concentration of oxygen present in
the ocean. Sufficient DO is essential to growth and reproduction of
aerobic aquatic life and provides suitable habitat for fish (e.g., Allan
1995, Giller and Malmqvist 1998, Murphy 2006). Climate change and
vairability (e.g., the El Niño-Southern Oscillation) leads to changes in
DO concentration that can affect the vertical distribution of important
species in the Pacific, such as tunas (e.g., Lehodey et al., 2010,
Mislan et al., 2017, Leung et al., 2019). This indicator is constructed
as DO concentration in the subsurface (~400m) water averaged over the
area of focus.

    ### Load libraries
    library(tidyverse)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

    library(lubridate)
    library(here)

    ## here() starts at /Users/daisyhuishi/github/Palau-ocean-climate

    library(stringr)
    library(nmfspalette)
    library(ncdf4) 

    ### Load libraries for mapping
    library(raster)

    ## Loading required package: sp

    ## The legacy packages maptools, rgdal, and rgeos, underpinning the sp package,
    ## which was just loaded, will retire in October 2023.
    ## Please refer to R-spatial evolution reports for details, especially
    ## https://r-spatial.org/r/2023/05/15/evolution4.html.
    ## It may be desirable to make the sf package available;
    ## package maintainers should consider adding sf to Suggests:.
    ## The sp package is now running under evolution status 2
    ##      (status 2 uses the sf package in place of rgdal)

    ## 
    ## Attaching package: 'raster'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

    library(rasterVis)

    ## Loading required package: lattice

    library(mapdata)

    ## Loading required package: maps

    ## 
    ## Attaching package: 'maps'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     map

    library(maptools)

    ## Please note that 'maptools' will be retired during October 2023,
    ## plan transition at your earliest convenience (see
    ## https://r-spatial.org/r/2023/05/15/evolution4.html and earlier blogs
    ## for guidance);some functionality will be moved to 'sp'.
    ##  Checking rgeos availability: FALSE

    library(cmocean)
    library(latticeExtra)

    ## 
    ## Attaching package: 'latticeExtra'

    ## The following object is masked from 'package:rasterVis':
    ## 
    ##     horizonplot

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     layer

    library(grid)
    library(rerddap)
    library(terra)

    ## terra 1.7.39

    ## 
    ## Attaching package: 'terra'

    ## The following object is masked from 'package:grid':
    ## 
    ##     depth

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

    library(viridis)

    ## Loading required package: viridisLite

    ## 
    ## Attaching package: 'viridis'

    ## The following object is masked from 'package:maps':
    ## 
    ##     unemp

    # Set report year (RptYr), to make things easier
    RptYr <- 2022

    # Set path to variable: Sea_Surface_Temperature
    # This is where the data are and where the plots will go
    Dir <- here("Dissolved_oxygen")

    ### Load data
    # Bounding box, from John Marra via email:
    lon_range <- c(129.4088, 137.0541)
    lat_range <- c(1.5214, 11.6587)

    #pre-downloaded netCDF file for pH at 5.14 m within the Palau domain (1,5-11.75N,129.5-137W)
    fn="dissolved_oxygen_palau.nc"
    o2_data <- nc_open(fn)
    o2 <- ncvar_get(o2_data, "o2")
    # convert unit of o2 from mmol m-3 to ml/l (divided by 44.66)
    o2 <- o2/44.66
    time <- ncvar_get(o2_data, "time")
    nc_close(o2_data) 

    # Change time from hours to year-month-date format
    time_df <- as.data.frame.table(as.POSIXct(time ,origin='1970-01-02 00:00'))[2]
    colnames(time_df) <- c('time')

    # Monthly spatial average
    o2_df=as.data.frame(apply(o2, c(3), mean, na.rm = TRUE))
    colnames(o2_df) <- c('o2')

    o2_ts <- data.frame(time_df, o2_df)

    ### Linear fit
    n_obs <- seq(1, length(o2_ts$o2), 1)
    o2_lm <- lm(o2_ts$o2~ n_obs)
    # summary(o2_lm) shows that there's a significant increasing
    # trend over time for this indicator.  There are some automated
    # checks that can be added to make sure this is still the case
    # in future years.

    # Change over time
    delta_o2 <- o2_lm$fitted.values[length(n_obs)] - o2_lm$fitted.value[1]

    ### Plot the time series
    # Create axes limits to make things simpler
    # These were determined through looking at quick rough plots and data limits
    o2_xlim <- c(min(ymd_hms(o2_ts$time)), max(ymd_hms(o2_ts$time)))
    o2_ylim <- c(1.5, 3.5) 

    # Access the NMFS color palette
    oceans <- nmfs_palette("oceans")(3)
    crustacean <- nmfs_palette("crustacean")(4)

    # Plot
    plot(ymd_hms(o2_ts$time), o2_ts$o2, type = "l", lwd = 2, col = oceans[2], 
         xlim = o2_xlim, ylim = o2_ylim, xlab = " ", ylab = "DO (ml/l)",
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
    par(new = TRUE)
    plot(ymd_hms(o2_ts$time), o2_lm$fitted.values, type = "l", lwd = 2, col = crustacean[1], 
         xlim = o2_xlim, ylim = o2_ylim, xlab = " ", ylab = " ",
         xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
    axis((1), at = ymd_hms(o2_ts$time[seq(1, length(n_obs), 12)]), tck = 0.025, labels = year(make_date(seq(1993, RptYr, 1))))
    axis((2), at = seq(1.5, 3.5, 0.25), tck = 0.025, las = 1)
    axis((3), at = ymd_hms(o2_ts$time[seq(1, length(n_obs), 12)]), tck = 0.025, labels = FALSE)
    axis((4), at = seq(1.5, 3.5, 0.25), tck = 0.025, labels = FALSE)

![](04-Dissolved_oxygen_files/figure-markdown_strict/unnamed-chunk-7-1.png)

    # _axt = "n" removes tick labels so that they can be customized later 
    # _axs = "i" removes whitespace beyond axes maxima

    # Read the file into R and make it to rasterstack
    stack_o2 = stack(fn)
    # Convert raster data to dataframe for calculating climatology
    df_temp = as.data.frame(rasterToPoints(stack_o2))
    df_temp$z = rowMeans(df_temp[,3:dim(df_temp)[2]], na.rm = T) /44.66
    # Convert dataframe to raster for mapping
    rst = rasterFromXYZ(df_temp[,c("x", "y", "z")])
    #create a rasterbrick with the rasterlayer
    o2_clim <- brick(rst)

    ### Mapping long-term climatology
    # Get land information and make it into a spatial object
    land <- maps::map('world', fill=TRUE, xlim=lon_range, ylim=lat_range, plot=FALSE)
    ids <- sapply(strsplit(land$names, ":"), function(x) x[1])
    bPols <- map2SpatialPolygons(land, IDs=ids, proj4string=CRS('+proj=longlat +datum=WGS84 +no_defs'))

    # Add EEZ
    llines.SpatVector <- function(x, ...) {
      xy <- crds(x, list=TRUE)
      names(xy) <- c("x", "y")
      lattice::llines(xy, ...)
    }
    f <- "eez/eez.shp"
    v <- vect(f)
    lns <- as.lines(v)

    # make map themes
    mapTheme <- rasterTheme(region=cmocean('oxy')(50))

    # Make plot
    #regular scale

    levelplot(o2_clim , pretty=T, margin=F,  par.setting=mapTheme,  colorkey=list( height = .5, width = 1) ) + layer(sp.polygons(bPols)) + layer(llines(lns))

    # add unit to colorbar
    grid.text(expression(ml/l) , y=unit(0.6, "npc"), 
                    x=unit(0.81, "npc"))    

![](04-Dissolved_oxygen_files/figure-markdown_strict/unnamed-chunk-9-1.png)

Dissolved oxygen (DO) is simulated by biogeochemical models using ocean
and atmosphere reanalyses as forcings. It spans from 1993 to 2022 and is
provided by Copernicus
(<https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_BGC_001_029/description>).
A highly significant (p&lt;0.001) increasing trend is found in the DO of
Palau subsurface waters (~400m) from 1993 to 2022, and DO increased by
0.51 ml/l over this period.
