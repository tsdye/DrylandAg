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

## Assumes a directory structure under output_dir
## 3) geo_tiffs for geo-tiffs

dryland_ag <- function(root_dir,
                       shape_file,
                       layer_name,
                       name_column,
                       ahupuaa_shape_file,
                       legend_order = NULL,
                       output_dir = root_dir,
                       raster_visualization_file = "raster_visualization_file.pdf",
                       plot_raster_visualization = TRUE,
                       area_comparison_file = "area_comparison_file.pdf",
                       plot_area_comparison = TRUE,
                       plot_geo_tiffs = TRUE,
                       annual_summary_file = "annual_summary_file.pdf",
                       plot_annual_summary = TRUE,
                       monthly_summary_file = "monthly_summary_file.pdf",
                       plot_monthly_summary = TRUE,
                       annual_plot_height = c(15, 4, 6),
                       interest_area = c(-156.2, -155.4, 18.9, 20.3),
                       diverging_palette = "BuRd",
                       sequential_palette = "iridescent",
                       qualitative_palette = "bright",
                       verbose = TRUE) {
    ## Load libraries
    library('sp')
    library('raster')
    library('rgdal')
    library('rgeos')
    library('khroma')
    library('sf')
    library('ggplot2')

#### Local functions
    ## set up some functions for plotting
    addHI3dryline <- function() plot(HI3dry, add=TRUE)  # show outline

    ## set scale for stacks
    fixstackcolors <- function(x,n)
        seq(min(minValue(x)),
            max(maxValue(x)),
            length.out = n)

    ## Utility function to generate file paths that can be used in series
    filename_splice <- function(pathname, filename, splice) {
        name_parts <- strsplit(basename(filename), ".", fixed = TRUE)
        file.path(pathname, paste0(name_parts[[1]][1], splice, ".",
                                   name_parts[[1]][2]))
    }

    ## function for plotting comparative graphs
    comparative_graph <- function(data, palette, out_file, height) {
        p <- ggplot(data = data, aes(x = value,
                                         fill = polygon_name,
                                         color = polygon_name))
        p <- p + geom_density(alpha = 0.4)
        p <- p + labs(y = "Density", x = NULL, fill = "Name")
        p <- p + theme_bw()
        p <- p + guides(color = FALSE)
        p <- p + switch(palette,
                        "light" = scale_colour_light(),
                        "bright" = scale_colour_bright(),
                        "vibrant" = scale_colour_vibrant(),
                        "muted" = scale_colour_muted())
        p <- p + switch(palette,
                        "light" = scale_fill_light(),
                        "bright" = scale_fill_bright(),
                        "vibrant" = scale_fill_vibrant(),
                        "muted" = scale_fill_muted())
        p <- p + facet_wrap(~plot_title, ncol = 2, scales = "free")
        ggsave(filename = out_file, plot = p, height = height)}

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
    if(!file.exists(geo_tiff_dir))
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
    my_colors <- colour(sequential_palette)
    my.colors <- colorRampPalette(my_colors(6))

    rf_colors <- colour(diverging_palette)
    rf.colors <- colorRampPalette(rev(rf_colors(5)))

#### Read in shapefiles ####
    ## Read in shapefile after testing whether or not it can be read
    if(verbose == TRUE) message("Reading shape files")
    if (file.exists(shape_file)) {
        if(verbose == TRUE) message(sprintf("Reading %s", shape_file))
        in_shape <- readOGR(dsn = shape_file, layer = layer_name) }
    else
        stop(sprintf("Unable to read %s", shape_file))

    ## Get the polygon names from the shape file
    polygon_names <- read_sf(shape_file, layer_name)[[name_column]]

    ## Set up different projections/datums since rasters are different CRS
    HI3dry <- spTransform(in_shape, CRS = crs_1)

    HI3dry.temp <- spTransform(in_shape, CRS = crs_2)

    ## Read in ahupuaa shapefile for mapping later
    if (file.exists(ahupuaa_shape_file)) {
        if(verbose == TRUE) message(sprintf("Reading %s", ahupuaa_shape_file))
        ahupuaa <- readOGR(dsn=ahupuaa_shape_file, layer="ahupuaa")
        ahupuaa <- spTransform(ahupuaa, CRS = crs_3) }
    else
        stop(sprintf("Unable to read %s", ahupuaa_shape_file))

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

    ## First CROP all rasters to the area of interest to speed up processing
    ext.HI3dry <- extent(interest_area)
    allcropped <- lapply(allstacks, FUN= function(x) crop(x, ext.HI3dry))

    ## Create MASKS so we focus on field systems
    ## Note DEM, Temp layers, and 250m ET atlas layers have diff CRS, resolutions!
                                        # DEM mask, note 10m resolution
    HI3dry.crop10<-is.na(mask(allcropped$dem, HI3dry))
                                        # All others: 250m, +ellps=WGS84
    HI3dry.crop250<-is.na(mask(allcropped$rf.ann, HI3dry))
                                        # Temp layer mask, +ellps=GRS80
    HI3dry.crop250temp<-is.na(mask(allcropped$tmax, HI3dry.temp))

    ## Create a list of masked rasters
    allmasked<-list()
    for (i in 1:length(allcropped))
        allmasked[[i]] <- switch(names(allcropped)[i],
                                 dem = mask(allcropped[[i]],
                                            HI3dry.crop10,
                                            maskvalue=T),
                                 tmax =, tmin = mask(allcropped[[i]],
                                                   HI3dry.crop250temp,
                                                   maskvalue=T),
                                 mask(allcropped[[i]],
                                      HI3dry.crop250,
                                      maskvalue=T))

    names(allmasked) <- names(allcropped)

#### Aids to data visualization ####


    ## Crop the ahupuaa shapefile for plotting
    ahucrop <- crop(ahupuaa, ext.HI3dry)

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
                    plot(ahucrop,main = names(allmasked[[i]][[j]]),
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

###### VISUALIZE STATISTICS

    if(verbose == TRUE) message("Compiling spatial statistics")

    ## CRS for most of the raster layers
    hi3_dry_crs <- proj4string(HI3dry)

    ## make a list of SpatialPolygons
    polygons <- lapply(X = HI3dry@polygons,
                       FUN = function(x, y)
                           SpatialPolygons(list(test = x),
                                           proj4string = CRS(y)),
                       y = hi3_dry_crs)

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
                rasters = rasters,
                crs = hi3_dry_crs,
                annual = ann,
                monthly = mo,
                separate = sep)

    ## Plot annual metrics across areas of interest
    if (plot_annual_summary == TRUE) {

        out_file <- filename_splice(output_dir, annual_summary_file, "_1")
        if(verbose == TRUE)
            message(sprintf("Writing annual metrics to %s", out_file))

        comb_dfs <- NULL
        for (i in 1:length(res$annual)) {

            ## Convert selected rasters, one for each area, into a list of data frames
            dfs <- lapply(X = res$rasters, FUN = function(x) as.data.frame(x[[i]]))

            ## add columns used for plotting and remove rows with NA data values
            ## polygon_name distinguishes areas in each of the plots
            ## plot_title distinguishes plots from one another
            for(j in 1:length(dfs)) {
                dfs[[j]]$polygon_name <- res$polygon_names[j]
                dfs[[j]]$plot_title <- names(res$rasters[[1]])[i]
                colnames(dfs[[j]])[1] <- "value"
                dfs[[j]] <- dfs[[j]][!is.na(dfs[[j]]$value), ]}

            comb_dfs <- dplyr::bind_rows(comb_dfs, dfs)}

        ## Possibly order the legend labels
        if(!is.null(legend_order))
            comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                            levels = legend_order)

        comparative_graph(data = comb_dfs, palette = qualitative_palette,
                          out_file = out_file, height = annual_plot_height[1])

        ## wind metrics
        out_file <- filename_splice(output_dir, annual_summary_file, "_2")
        if(verbose == TRUE)
            message(sprintf("Writing annual wind metrics to %s", out_file))

        comb_dfs <- NULL
        for(i in 1:nlayers(res$rasters[[1]][[res$separate[1]]])) {
            dfs <- lapply(X = res$rasters,
                          FUN = function(x)
                              as.data.frame(raster(x[[res$separate[[1]]]],
                                                   layer = i)))

            ## add columns used for plotting and remove rows with NA data values
            ## polygon_name distinguishes areas in each of the plots
            ## plot_title distinguishes plots from one another
            for(j in 1:length(dfs)) {
                dfs[[j]]$polygon_name <- res$polygon_names[j]
                dfs[[j]]$plot_title <- names(res$rasters[[1]][[res$separate[[1]]]])[i]
                colnames(dfs[[j]])[1] <- "value"
                dfs[[j]] <- dfs[[j]][!is.na(dfs[[j]]$value), ]}

            comb_dfs <- dplyr::bind_rows(comb_dfs, dfs)}

        ## Possibly order the legend labels
        if(!is.null(legend_order))
            comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                            levels = legend_order)

        comparative_graph(data = comb_dfs, palette = qualitative_palette,
                          out_file = out_file, height = annual_plot_height[2])

        ## intra-annual rainfall metrics

        comb_dfs <- NULL
        out_file <- filename_splice(output_dir, annual_summary_file, "_3")
        if(verbose == TRUE)
            message(sprintf("Writing intra-annual rainfall metrics to %s",
                            out_file))

        for (i in 1:length(res$rasters)) {
            dfs <- lapply(X = res$rasters,
                          FUN = function(x) as.data.frame(x$rf.ann))

            ## add columns used for plotting and remove rows with NA data values
            ## polygon_name distinguishes areas in each of the plots
            ## plot_title distinguishes plots from one another
            for(j in 1:length(dfs)) {
                dfs[[j]]$polygon_name <- res$polygon_names[j]
                dfs[[j]]$plot_title <- names(res$rasters[[1]]$rf.ann)
                colnames(dfs[[j]])[1] <- "value"
                dfs[[j]] <- dfs[[j]][!is.na(dfs[[j]]$value), ]}

            comb_dfs <- dplyr::bind_rows(comb_dfs, dfs)}

        for(i in 1:nlayers(res$rasters[[1]][[res$separate[2]]])) {
            dfs <- lapply(X = res$rasters,
                          FUN = function(x)
                              as.data.frame(raster(x[[res$separate[[2]]]],
                                                   layer = i)))

            ## add columns used for plotting and remove rows with NA data values
            ## polygon_name distinguishes areas in each of the plots
            ## plot_title distinguishes plots from one another
            for(j in 1:length(dfs)) {
                dfs[[j]]$polygon_name <- res$polygon_names[j]
                dfs[[j]]$plot_title <- names(res$rasters[[1]][[res$separate[[2]]]])[i]
                colnames(dfs[[j]])[1] <- "value"
                dfs[[j]] <- dfs[[j]][!is.na(dfs[[j]]$value), ]}

            comb_dfs <- dplyr::bind_rows(comb_dfs, dfs)}

        ## Possibly order the legend labels
        if(!is.null(legend_order))
            comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                            levels = legend_order)

        comparative_graph(data = comb_dfs, palette = qualitative_palette,
                          out_file = out_file, height = annual_plot_height[3])}

    ## Monthly metrics

    ## set up
    min_plot_size = 7/3
    monthly_plot_height = 15 * min_plot_size
    monthly_plot_width = length(res$rasters) * min_plot_size * 1.5

    out_file <- file.path(output_dir, monthly_summary_file)
    if(verbose == TRUE) message(sprintf("Writing monthly metrics to %s",
                                        out_file))

    comb_dfs <- NULL
    ## make data frame
    for (i in res$monthly) {

        ## Convert selected rasters, one for each area,
        ## into a list of data frames
        dfs <- lapply(X = res$rasters, FUN = function(x)
            as.data.frame(x[[i]]))

        ## add columns used for plotting and remove rows with NA data values
        ## polygon_name distinguishes areas in each of the plots
        ## facet distinguishes plots from one another
        for(j in 1:length(dfs)) {
            names(dfs[[j]]) <- month.abb
            dfs[[j]]$polygon_name <- res$polygon_names[j]
            dfs[[j]]$facet <- names(res$rasters[[1]])[i]
            dfs[[j]] <- suppressMessages(reshape2::melt(dfs[[j]],
                                                        na.rm = TRUE))
        }

        comb_dfs <- dplyr::bind_rows(comb_dfs, dfs)}

    ## Possibly order the legend labels
    if(!is.null(legend_order))
        comb_dfs$polygon_name <- factor(comb_dfs$polygon_name,
                                        levels = legend_order)

    ## boxplots
    p <- ggplot(data = comb_dfs, aes(x = variable, y = value,
                                     fill = polygon_name,
                                     color = polygon_name))
    p <- p + geom_boxplot(outlier.shape = 20, alpha = 0.4)
    p <- p + labs(x = "Month", y = NULL, fill = "Place")
    p <- p + theme_bw()
    p <- p + guides(color = FALSE)
    p <- p + switch(qualitative_palette,
                    "light" = scale_colour_light(),
                    "bright" = scale_colour_bright(),
                    "vibrant" = scale_colour_vibrant(),
                    "muted" = scale_colour_muted())
    p <- p + switch(qualitative_palette,
                    "light" = scale_fill_light(),
                    "bright" = scale_fill_bright(),
                    "vibrant" = scale_fill_vibrant(),
                    "muted" = scale_fill_muted())
    p <- p + facet_grid(facet ~ ., scales = "free_y")
    ggsave(filename = out_file, plot = p, height = monthly_plot_height,
           width = monthly_plot_width)

    res } # end of function

## #########################################################################################
## ################## Make pretty figures for publication ##################################
## ## FIGURE 2: Density plots of mean annual climate ---------
## pdf("Fig2_MeanAnnual_v3.pdf", width=9, height=5)
## ann.fin<-c(1, 13,
##            2, 9,
##            3, 4,
##            NA, 6)

## names.ann.fin<-c("Elevation (m)",
##                   expression("Mean Air Temperature " (degree*C)),
##                   "Annual Rainfall (mm)",
##                   "Relative Humidity (%)",
##                   "Cloud Frequency",
##                   expression("Shortwave Radiation " (Wm^-2)),
##                   expression("Mean Wind Speed " (ms^-1)),
##                   "Potential ET (mm)")
## figlabel<-letters[1:8]

## par(mfcol=c(2,4), mar=c(4.5, 4, 1, 0.6) + 0.1)
## for (i in 1:length(ann.fin)){
##   if(i==7){
##     d.koh<-density(raster(all.kohala[[sep[1]]], layer=1), plot=F)
##     d.kon<-density(raster(all.kona[[sep[1]]], layer=1), plot=F)
##     d.kau<-density(raster(all.kau[[sep[1]]], layer=1), plot=F)
##     ylim<-range(d.koh$y, d.kon$y, d.kau$y)
##     xlim<-range(d.koh$x, d.kon$x, d.kau$x)

##     plot(d.koh, xlim=xlim, ylim=ylim, col="#000000", lty=1,
##          xlab=names.ann.fin[i], main="", ylab="", cex.axis=0.9)
##     lines(d.kon, col="#0072B2", lty=2, lwd=1.5)
##     lines(d.kau, col="#E69F00", lty=3, lwd=3)
##     mtext(figlabel[i], side=3, line=-1.3, adj=0.04)
##   }else{
##   d.koh<-density(all.kohala[[ann.fin[i]]], plot=F)
##   d.kon<-density(all.kona[[ann.fin[i]]], plot=F)
##   d.kau<-density(all.kau[[ann.fin[i]]], plot=F)
##   ylim<-range(d.koh$y, d.kon$y, d.kau$y)
##   xlim<-range(d.koh$x, d.kon$x, d.kau$x)

##   plot(d.koh, xlim=xlim, ylim=ylim, col="#000000", lty=1,
##        xlab=names.ann.fin[i], main="", ylab="", cex.axis=0.9)
##   lines(d.kon, col="#0072B2", lty=2, lwd=1.5)
##   lines(d.kau, col="#E69F00", lty=3, lwd=3)
##   mtext(figlabel[i], side=3, line=-1.3, adj=0.04)
##   if(i==1){
##     legend("topright", legend=c("Kohala", "Kona", "Kau"),
##            lty=c(1,2,3), lwd=c(1,1.5,2), bty="n",
##            col=c("#000000","#0072B2","#E69F00"))
##     mtext("Density", side=2, line=2.5, cex=0.7)}
##   if(i==2){mtext("Density", side=2, line=2.5, cex=0.7)}
##   }
## }
## dev.off()

## ## FIGURE 3: Boxplots of seasonality: P, PET, and aridity (PET/P) ----------
## pdf("Fig3_AnnualCycle_P_PET_Aridity_fin2.pdf", width=6, height=5)
## fig3<-mo[names(mo) %in% c("rf.mo", "pet", "aridity.mo")]
## names(all.kohala[["aridity.mo"]])<-month.abb
## names(all.kona[["aridity.mo"]])<-month.abb
## names(all.kau[["aridity.mo"]])<-month.abb
## figlabel<-letters[1:9]
## par(mfrow=c(3,3), mar=c(0.2, 0, 0, 0), oma=c(3,5,3,2))
## for (i in fig3){
##   ylim<-range(minValue(all.kohala[[i]]),
##               minValue(all.kona[[i]]),
##               minValue(all.kau[[i]]),
##               maxValue(all.kohala[[i]]),
##               maxValue(all.kona[[i]]),
##               maxValue(all.kau[[i]]))
##   if (i==fig3[1]){
##     boxplot(all.kohala[[i]], ylim=ylim,
##             border="#000000", notch=T,
##             xaxt="n", las=1)
##     mtext("Kohala", side=3, line=1)
##     mtext("Rainfall (P, mm)", side=2, line=3.2, cex=0.8)
##     mtext("a", side=3, line=-1.3, adj=0.04)
##     boxplot(all.kona[[i]], ylim=ylim,
##             border="#0072B2", notch=T,
##             xaxt="n", yaxt="n")
##     mtext("Kona", side=3, line=1)
##     mtext("b", side=3, line=-1.3, adj=0.04)
##     boxplot(all.kau[[i]], ylim=ylim,
##             border="#E69F00", notch=T,
##             xaxt="n", yaxt="n")
##     mtext("Kau", side=3, line=1)
##     mtext("c", side=3, line=-1.3, adj=0.04)
##   }else if(i==fig3[2]){
##     boxplot(all.kohala[[i]], ylim=ylim,
##             border="#000000", notch=T,
##             xaxt="n", las=1)
##     mtext("PET (mm)", side=2, line=3.2, cex=0.8)
##     mtext("d", side=3, line=-1.3, adj=0.04)
##     boxplot(all.kona[[i]], ylim=ylim,
##             border="#0072B2", notch=T,
##             xaxt="n", yaxt="n")
##     mtext("e", side=3, line=-1.3, adj=0.04)
##     boxplot(all.kau[[i]], ylim=ylim,
##             border="#E69F00", notch=T,
##             xaxt="n", yaxt="n")
##     mtext("f", side=3, line=-1.3, adj=0.04)
##   }else{
##     boxplot(all.kohala[[i]], ylim=ylim,
##             border="#000000", notch=T,
##             cex.axis=0.9, las=1)
##     mtext("Aridity (PET/P)", side=2, line=3.2, cex=0.8)
##     mtext("g", side=3, line=-1.3, adj=0.04)
##     boxplot(all.kona[[i]], ylim=ylim,
##             border="#0072B2", notch=T,
##             cex.axis=0.9, yaxt="n", las=1)
##     mtext("h", side=3, line=-1.3, adj=0.04)
##     boxplot(all.kau[[i]], ylim=ylim,
##             border="#E69F00", notch=T,
##             cex.axis=0.9, yaxt="n", las=1)
##     mtext("i", side=3, line=-1.3, adj=0.04)
##   }
##   print(i)
## }
## dev.off()

## ## FIGURE S2: Boxplots of seasonality: Tmax, Tmin, RH, and VPD ----------------
## pdf("FigS2_AnnualCycle_T_RH_VPD_fin.pdf", width=6, height=5)
## figs2<-mo[names(mo) %in% c("tmax", "tmin", "rh", "vpd")]
## figs2<-figs2[c(3,1,2)]

## names(all.kohala[[figs2[3]]])<-month.abb
## names(all.kona[[figs2[3]]])<-month.abb
## names(all.kau[[figs2[3]]])<-month.abb

## figlabel<-letters[1:9]
## par(mfrow=c(3,3), mar=c(0.2, 0, 0, 0), oma=c(3,5,3,2))
## for (i in figs2){   # temperature
##   if (i==figs2[1]){
##     ylim<-range(minValue(all.kohala[[i+1]]),
##                 minValue(all.kona[[i+1]]),
##                 minValue(all.kau[[i+1]]),
##                 maxValue(all.kohala[[i]]),
##                 maxValue(all.kona[[i]]),
##                 maxValue(all.kau[[i]]))
##     boxplot(all.kohala[[i]], ylim=ylim,
##             border="#000000", notch=T,
##             xaxt="n", las=1)
##     boxplot(all.kohala[[i+1]], ylim=ylim,
##             border="#000000", notch=T,
##             xaxt="n", las=1, add=T)
##     mtext("Kohala", side=3, line=1)
##     mtext(expression(paste(T[min], " and ", T[max], " ", (degree*C)), sep=""), side=2, line=3.2, cex=0.8)
##     mtext("a", side=3, line=-1.3, adj=0.04)
##     boxplot(all.kona[[i]], ylim=ylim,
##             border="#0072B2", notch=T,
##             xaxt="n", yaxt="n")
##     boxplot(all.kona[[i+1]], ylim=ylim,
##             border="#0072B2", notch=T,
##             xaxt="n", yaxt="n", add=T)
##     mtext("Kona", side=3, line=1)
##     mtext("b", side=3, line=-1.3, adj=0.04)
##     boxplot(all.kau[[i]], ylim=ylim,
##             border="#E69F00", notch=T,
##             xaxt="n", yaxt="n")
##     boxplot(all.kau[[i+1]], ylim=ylim,
##             border="#E69F00", notch=T,
##             xaxt="n", yaxt="n", add=T)
##     mtext("Kau", side=3, line=1)
##     mtext("c", side=3, line=-1.3, adj=0.04)
##   }else{
##     ylim<-range(minValue(all.kohala[[i]]),
##                 minValue(all.kona[[i]]),
##                 minValue(all.kau[[i]]),
##                 maxValue(all.kohala[[i]]),
##                 maxValue(all.kona[[i]]),
##                 maxValue(all.kau[[i]]))
##     if(i==figs2[2]){
##       boxplot(all.kohala[[i]], ylim=ylim,
##               border="#000000", notch=T,
##               xaxt="n", las=1)
##       mtext("RH (%)", side=2, line=3.2, cex=0.8)
##       mtext("d", side=3, line=-1.3, adj=0.04)
##       boxplot(all.kona[[i]], ylim=ylim,
##               border="#0072B2", notch=T,
##               xaxt="n", yaxt="n")
##       mtext("e", side=3, line=-1.3, adj=0.04)
##       boxplot(all.kau[[i]], ylim=ylim,
##               border="#E69F00", notch=T,
##               xaxt="n", yaxt="n")
##       mtext("f", side=3, line=-1.3, adj=0.04)
##     }else{
##       boxplot(all.kohala[[i]], ylim=ylim,
##               border="#000000", notch=T,
##               cex.axis=0.9, las=1)
##       mtext("VPD (Pa)", side=2, line=3.2, cex=0.8)
##       mtext("g", side=3, line=-1.3, adj=0.04)
##       boxplot(all.kona[[i]], ylim=ylim,
##               border="#0072B2", notch=T,
##               cex.axis=0.9, yaxt="n", las=1)
##       mtext("h", side=3, line=-1.3, adj=0.04)
##       boxplot(all.kau[[i]], ylim=ylim,
##               border="#E69F00", notch=T,
##               cex.axis=0.9, yaxt="n", las=1)
##       mtext("i", side=3, line=-1.3, adj=0.04)
##     }
##   }
##   print(i)
## }
## dev.off()
