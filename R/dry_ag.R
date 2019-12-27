## FieldSystemCovariates_Polygons_GraphsFinal.R
## Project: DrylandAg
## Author: A. Kagawa-Viviani
## Date: 23 January 2018
## Notes: 1) Extract values of gridded climate data for known locations
##        2) Visualize these
## Figure 2
## Figure 3
## Figure S2

## 1) gis_files for DEM and climate layers from rainfall and evapotranspiration
## pages at geography.hawaii.edu
## 2) spatial_data for spatial data tiffs

## Creates a directory structure under output_dir
## 3) geo_tiffs for geo-tiffs

dryland_ag <- function(root_dir,
                       shape_file,
                       layer_name,
                       name_column,
                       sort_column = NULL,
                       ahupuaa_shape_file,
                       ahupuaa_layer = "ahupuaa",
                       output_dir = root_dir,
                       raster_visualization_file = "raster_visualization_file.pdf",
                       plot_raster_visualization = TRUE,
                       area_comparison_file = "area_comparison_file.pdf",
                       plot_area_comparison = TRUE,
                       plot_geo_tiffs = TRUE,
                       results_file = "dryland_ag_result_list.R",
                       interest_area = c(-156.2, -155.4, 18.9, 20.3),
                       diverging_palette = "BuRd",
                       sequential_palette = "iridescent",
                       verbose = TRUE) {
    ## Load libraries
    library('sp')
    library('raster')
    library('rgdal')
    library('rgeos')
    ## library('sf')
    ## library('khroma')

#### Local function

    ## set scale for stacks
    fixstackcolors <- function(x, n)
        seq(min(minValue(x)),
            max(maxValue(x)),
            length.out = n)

#### Set up
    ## Set working directories
    setwd(root_dir)
    if(verbose == TRUE) message("Set root directory to ", root_dir)

    ## Set location of GIS files
    ## These should include DEM and climate layers
    ## from http://evapotranspiration.geography.hawaii.edu/,
    ## http://rainfall.geography.hawaii.edu
    gis_dir <- file.path(root_dir, "gis_files")
    if(!file.exists(gis_dir)) stop(sprintf("Folder %s does not exist",
                                           gis_dir))

    ## Set location of spatial_data tiffs
    tiff_dir <- file.path(root_dir, "spatial_data")
    if(!file.exists(tiff_dir)) stop(sprintf("Folder %s does not exist",
                                            tiff_dir))

    ## Output folder
    if(!file.exists(output_dir))
        if(askYesNo(msg = sprintf("Output directory %s does not exist.  Create?",
                                  output_dir)))
            dir.create(output_dir)
        else
            stop("Cannot continue without an output directory")

    ## Output subfolder for geo-tiffs
    geo_tiff_dir <- file.path(output_dir, "geo_tiffs")
    if((plot_geo_tiffs == TRUE) & !file.exists(geo_tiff_dir))
        if(askYesNo(msg = sprintf("Output sub-directory %s does not exist.  Create?",
                                  geo_tiff_dir)))
            dir.create(geo_tiff_dir)
        else
            stop("Cannot continue without an output sub-directory for the geo-tiffs")

    ## Variables for CRS strings
    crs_1 <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    crs_2 <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
    crs_3 <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs +datum=WGS84"

    ## Set up colors for plotting raster data
    ## maps use 15 colors
    sequential_palettes <- c("YlOrBr", "iridescent", "smooth rainbow")
    if(!is.element(sequential_palette, sequential_palettes))
        stop(sprintf("Sequential palette %s is not one of %s, %s, or %s",
                     sequential_palette,
                     sequential_palettes[1],
                     sequential_palettes[2],
                     sequential_palettes[3]))
    my_colors <- khroma::colour(sequential_palette)
    my.colors <- colorRampPalette(my_colors(6))

    diverging_palettes <- c("BuRd", "PRGn", "sunset")
    if(!is.element(diverging_palette, diverging_palettes))
        stop(sprintf("Diverging palette %s is not one of %s, %s, or %s",
                     diverging_palette,
                     diverging_palettes[1],
                     diverging_palettes[2],
                     diverging_palettes[3]))
    rf_colors <- khroma::colour(diverging_palette)
    rf.colors <- colorRampPalette(rev(rf_colors(5)))

#### Read in shapefiles ####
    ## Read in shapefile after testing whether or not it can be read
    if(verbose == TRUE) message("Attempting to read shape files")

    if (file.exists(shape_file)) {
        if(verbose == TRUE) message(sprintf("Reading %s", shape_file))
        in_shape <- readOGR(dsn = shape_file, layer = layer_name) }
    else
        stop(sprintf("Unable to read %s", shape_file))

    ## Get the polygon names and sort order from the shape file
    polygon_names <- sf::read_sf(shape_file, layer_name)[[name_column]]

    if(!is.null(sort_column))
        sort_order <- sf::read_sf(shape_file, layer_name)[[sort_column]]
    else
        sort_order <- NULL

    ## Set up different projections/datums since rasters are different CRS
    ## First CROP all rasters to the area of interest to speed up processing
    area_of_interest <- extent(interest_area)

    in_shape_crs_1 <- spTransform(in_shape, CRS = crs_1)

    in_shape_crs_2 <- spTransform(in_shape, CRS = crs_2)

    ## Read in ahupuaa shapefile for mapping later
    if (file.exists(ahupuaa_shape_file)) {
        if(verbose == TRUE) message(sprintf("Reading %s", ahupuaa_shape_file))
        ahupuaa <- readOGR(dsn = ahupuaa_shape_file, layer = ahupuaa_layer)
        ahupuaa <- spTransform(ahupuaa, CRS = crs_3) }
    else
        stop(sprintf("Unable to read %s", ahupuaa_shape_file))

    ## Crop the ahupuaa shapefile for plotting
    ahucrop <- crop(ahupuaa, area_of_interest)

### Read in raster data ####
### Get file names
    if (verbose) message("Reading raster data")
    ## Elevation (m) from USGS 10m DEM (source: National Map)
    dem <- dir(file.path(gis_dir, "HITemp_rastercovariates"),
               pattern = "clipdem_wgs84", full.names = T)

    ## Rainfall Atlas data, annual and monthly
    ## (source: Online Rainfall Atlas of HI)
    rfdirs <- list.dirs(file.path(gis_dir, "StateRFGrids_mm"),
                        full.names=T)
    staterf <- rfdirs[grep("staterf_mm", rfdirs)]

    ## ET-relevant data, annual and monthly
    ## (source: Evapotranspiration of HI)
    ## Solar Radiation (SW)
    swdirs <- list.dirs(file.path(gis_dir,
                                  "CloudySWGlobTerrain_month_raster"),
                        full.names=T)
    sw <- swdirs[grep("cl_sw", swdirs)]

    ## Diffuse Radiation (diffuse)
    diffdirs <- list.dirs(file.path(gis_dir,
                                    "DiffuseRadiation_month_raster"),
                          full.names=T)
    dif <- diffdirs[grep("dif_rd", diffdirs)]

    ## Penman Monteith PET
    petdirs <- list.dirs(file.path(gis_dir,
                                   "Penman_ET0_mm_month_raster"),
                         full.names=T)
    pet <- petdirs[grep("pen_mm", petdirs)]

    ## Actual ET
    aetdirs <- list.dirs(file.path(gis_dir,
                                   "AET_mm_month_raster"),
                         full.names=T)
    aet <- aetdirs[grep("aet_mm", aetdirs)]

    ## Soil Moisture
    smdirs <- list.dirs(file.path(gis_dir,
                                  "SoilMoisture_month_raster"),
                        full.names=T)
    sm <- smdirs[grep("sl_mst", smdirs)]

    ## Relative Humidity
    rhdirs <- list.dirs(file.path(gis_dir,
                                  "RH_month_raster"),
                        full.names=T)
    rh <- rhdirs[grep("rh_", rhdirs)]

    ## Vapor pressure deficit
    vpddirs <- list.dirs(file.path(gis_dir,
                                   "VPD_month_raster"),
                         full.names=T)
    vpd <- vpddirs[grep("vpd_", vpddirs)]

### Temperature
    ## Tair Max
    tmaxdirs <- list.dirs(file.path(gis_dir,
                                    "Tmax_month_raster"),
                          full.names=T)
    tmax <- tmaxdirs[grep("tmax", tmaxdirs)]

    ## Tair Min
    tmindirs <- list.dirs(file.path(gis_dir,
                                    "Tmin_month_raster"),
                          full.names=T)
    tmin <- tmindirs[grep("tmin", tmindirs)]

    ## Tair Mean
    tmeandirs <- list.dirs(file.path(gis_dir,
                                     "Tair_month_raster"),
                           full.names=T)
    tmean <- tmeandirs[grep("tair", tmeandirs)]

    ## Tsurf
    tsurfdirs <- list.dirs(file.path(gis_dir,
                                     "TSurf_month_raster"),
                           full.names=T)
    tsurf <- tsurfdirs[grep("tsurf", tsurfdirs)]

### Cloud and Wind
    ## Cloud cover
    clouddirs <- list.dirs(file.path(gis_dir,
                                     "HITemp_rastercovariates/CloudFreq_month_raster"),
                           full.names=T)
    clouds <- clouddirs[grep("cl_frq", clouddirs)]

    ## Wind speed
    winddirs <- list.dirs(file.path(gis_dir,
                                    "HITemp_rastercovariates/Wind_speed_ann_hr_raster"),
                          full.names=T)
    winds <- winddirs[grep("wind_sd", winddirs)]

#### Read in rasters ####
    ## DEM
    dem.r <- raster(dem) # single layer

    ## Rainfall
    staterf.mo <- stack(staterf[1:12])
    staterf.ann <- raster(staterf[13])

    ## Clouds
    clouds.mo <- stack(clouds[1:12])
    clouds.ann <- raster(clouds[13])

    ## Wind
    winds.24 <- stack(winds[1:24])

    ## RF Seasonality, calculated previously
    rfseason <- stack(file.path(tiff_dir,"rfseasonstack.tif"))
    names(rfseason) <- c("CV_MeanRF", "RelEntropy_D",
                         "Seasonality_S", "Centroid_C", "Duration_Z")

    ## Create a named list of directories for monthly series
    alldirs <-list(sw,   dif,   pet,   aet,   sm,   rh,   vpd,
                   tmax,   tmin,   tmean,   tsurf)
    names(alldirs) <- c("sw", "dif", "pet", "aet", "sm", "rh", "vpd",
                        "tmax", "tmin", "tmean", "tsurf")

    ## create index for plotting in order, Jan-Dec
    mo.order <- c(1, order(month.abb) + 1)
    mo.sort <- numeric(length = 13)
    mo.sort[mo.order] <- 1:13

    ## Organize directories for monthly rasters in order of month
    ## (rather than alphabetically)
    mofiles <- lapply(alldirs, FUN=function(x) x[mo.sort[2:13]])
    mostack <- lapply(mofiles, FUN=stack)  # read into raster stack

    ## Pull out directories of annual rasters
    annfiles <- lapply(alldirs, FUN=function(x) x[mo.sort[1]])
    annraster <- lapply(annfiles, FUN=raster)  # read into raster

#### Calculate other climate indices ####
    if(verbose == TRUE) message("Calculating climate indices")
    ## Calculate ARIDITY = PET/RF
    aridstack.mo <- mostack$pet/staterf.mo    # for each month
    names(aridstack.mo) <- paste0("PET.P_", month.abb)
    aridity.ann <- annraster$pet/staterf.ann  # for year
    names(aridity.ann) <- "PET.P_ann"

    ## Calculate AET/RF, Actual ET/Precip (>1 indicates irrigation)
    aetrfstack.mo <- mostack$aet/staterf.mo   # for each month
    names(aetrfstack.mo) <- paste0("AET.P_", month.abb)
    aetrf.ann <- annraster$aet/staterf.ann    # for year
    names(aetrf.ann) <- "AET.P_ann"

    ## Summarize winds:
    meanwinds <- mean(winds.24)
    maxwinds <- max(winds.24)
    minwinds <- min(winds.24)
    cvwinds <- cv(winds.24)
    windsp <- stack(meanwinds, maxwinds, minwinds, cvwinds)
    names(windsp)<-c("meanws", "maxws", "minws", "cvws")

#### Combine all of the above
    allstacks<-c(dem = dem.r,
                                        # annual
                 rf.ann = staterf.ann,
                 cl.ann = clouds.ann,
                 annraster,
                 aridity.ann = aridity.ann,
                 aetrf.ann = aetrf.ann,
                 windsp = windsp,
                                        # monthly
                 rf.mo =  staterf.mo,
                 cl.mo = clouds.mo,
                 mostack,
                 aridity.mo = aridstack.mo,
                 aetrf.mo = aetrfstack.mo,
                 rfseason = rfseason)

    ## Visualize the rasters as maps using plot default setting
    if (plot_raster_visualization) {
        out_file <- file.path(output_dir, raster_visualization_file)
        if(verbose == TRUE) message("Writing raster visualizations to ",
                                    out_file)
        pdf(out_file, onefile = T)
        for (i in 1:length(allstacks)) {
            plot(allstacks[[i]],
                 main = names(allstacks)[i],
                 col = my.colors(15))
            print(i)
        }
        dev.off()
    }


#### Zoom in to area of interest ####
    ## Clip - use extent(), crop(), and mask()
    if(verbose == TRUE) message("Zooming in to areas of interest")

    allcropped <- lapply(allstacks,
                         FUN= function(x) crop(x, area_of_interest))

    ## Create MASKS so we focus on field systems
    ## Note DEM, Temp layers, and 250m ET atlas layers have diff CRS, resolutions!
                                        # DEM mask, note 10m resolution
    in_shape_crs_1.crop10<-is.na(mask(allcropped$dem, in_shape_crs_1))
                                        # All others: 250m, +ellps=WGS84
    in_shape_crs_1.crop250<-is.na(mask(allcropped$rf.ann, in_shape_crs_1))
                                        # Temp layer mask, +ellps=GRS80
    in_shape_crs_1.crop250temp<-is.na(mask(allcropped$tmax, in_shape_crs_2))

    ## Create a list of masked rasters
    allmasked<-list()
    for (i in 1:length(allcropped))
        allmasked[[i]] <- switch(names(allcropped)[i],
                                 dem = mask(allcropped[[i]],
                                                    in_shape_crs_1.crop10,
                                                    maskvalue = T),
                                 tmax =, tmin = mask(allcropped[[i]],
                                                             in_shape_crs_1.crop250temp,
                                                             maskvalue = T),
                                 mask(allcropped[[i]],
                                              in_shape_crs_1.crop250,
                                              maskvalue=T))

    names(allmasked) <- names(allcropped)

#### Aids to data visualization ####


#### Plot rasters to compare areas ####
    if(plot_area_comparison == TRUE) {
        out_file <- file.path(output_dir, area_comparison_file)
        if(verbose == TRUE) message("Plotting areal comparisons to ",
                                    out_file)

        ## Make a series of pdfs
        pdf(out_file, onefile = T)
        for (i in 1:length(allmasked)) {
            rng <- range(cellStats(allmasked[[i]], stat = 'range'))
            labs <- seq(from = rng[1], to = rng[2], length = 5)
            arg <- list(at = labs, labels = round(labs, 2))
            fixbreaks <- fixstackcolors(allmasked[[i]], 15)
                                        # DON'T fix colors for these stacks
            if(names(allmasked)[i] %in% c("rfseason","windsp"))
                for (j in 1:nlayers(allmasked[[i]])) {
                    plot(ahucrop, main = names(allmasked[[i]][[j]]),
                         lwd = 0.5)
                    plot(allmasked[[i]][[j]], col = rev(my.colors(15)),
                         alpha = 0.8, add = T) }
            else
                                        # DO fix colors for these stacks
                if(names(allmasked)[i] %in% c("sw","pet","vpd", "tmax",
                                              "tmin", "tmean", "tsurf",
                                              "aridity.ann", "aridity.mo"))
                    for (j in 1:nlayers(allmasked[[i]])) {
                        plot(ahucrop, main = names(allmasked[[i]][[j]]),
                             lwd = 0.5)
                        plot(allmasked[[i]][[j]], breaks = fixbreaks,
                             col = rev(rf.colors(15)),
                             axis.args = arg, alpha = 0.9, add = T) }
            else
                                        # DO fix colors for these stacks
                for (j in 1:nlayers(allmasked[[i]])) {
                    plot(ahucrop, main = names(allmasked[[i]][[j]]),
                         lwd = 0.5)
                    plot(allmasked[[i]][[j]], breaks = fixbreaks,
                         col = rf.colors(15),
                         axis.args = arg, alpha = 0.9, add = T) }
        }
        dev.off()
    }

    ## Now save maps as geotiffs
    if(plot_geo_tiffs == TRUE) {
        if(verbose == TRUE)
            message("Saving spatial data as geo-tiffs in the folder ",
                    geo_tiff_dir)
        for (i in 1:length(allmasked)) {
            if(nlayers(allmasked[[i]]) == 1)
                writeRaster(allmasked[[i]],
                                    filename = file.path(geo_tiff_dir,
                                                         names(allmasked)[i]),
                                    format = "GTiff",
                                    overwrite = T)
            else
                writeRaster(allmasked[[i]],
                                    filename = file.path(geo_tiff_dir,
                                                         paste0(names(allmasked)[i],
                                                                "stack.tif")),
                                    format = "GTiff",
                                    bylayer = F,
                                    overwrite = T) }
    }

    ## ## CRS for most of the raster layers
    ## crs_1 <- proj4string(in_shape_crs_1)

    ## make a list of SpatialPolygons
    polygons <- lapply(X = in_shape_crs_1@polygons,
                       FUN = function(x, y)
                           SpatialPolygons(list(test = x),
                                           proj4string = CRS(y)),
                       y = crs_1)

    ## Crop each raster using each of the polygons
    rasters <- lapply(X = polygons,
                      FUN = function (x)
                          lapply(X = allmasked,
                                 FUN = function(x, y) crop(x, y),
                                 y = x))

    ## just refer to the first element of the list just made
    ann <- which(lapply(X = rasters[[1]], FUN = nlayers) == 1)
    mo <- which(lapply(X = rasters[[1]], FUN = nlayers) == 12)

    ## Which raster stacks need to be analyzed separately?
                                        # wind, rfseason
    sep <- which(lapply(X = rasters[[1]], FUN = nlayers) %in% c(4, 5))

    ## Return value
    res <- list(polygons = polygons,
                polygon_names = polygon_names,
                sort_order = sort_order,
                rasters = rasters,
                crs = crs_1,
                annual = ann,
                monthly = mo,
                separate = sep)

    if (!is.null(results_file)) {
        out_file <- file.path(output_dir, results_file)
        if(verbose == TRUE) {
            fmt <- "Writing results to %s\nUse the function readRDS(file = \"%s\") to read them"
            message(sprintf(fmt = fmt, out_file, out_file))}
        saveRDS(object = res, file = out_file)}

    if(verbose == TRUE) message("Pau")

    res
} # end of function
