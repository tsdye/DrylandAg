## FieldSystemCovariates_CombinedAnalysesFin.R
## Project: DrylandAg
## Author: A. Kagawa-Viviani
## Date: 23 Jan 2017
## Notes: Script associated with FieldSystemCovariates_Polygons_GraphsFin.R
##        -Generate rasters for seasonality metrics of aridity
##        -Explore intra-annual shifting of optimal growing conditions
## Figure 4
## Figure 6

# References:
#    Feng X, Porporato A, Rodriguez-Iturbe I (2013) Changes in rainfall seasonality
#      in the tropics. Nature Clim Change 3:811-815. doi: 10.1038/nclimate1907
#    Valenzuela, H., S. Fukuda, and A. Arakaki (1994) Sweetpotato Production Guides for
#      Hawaii. Pages 1-10. Hawaii Institute of Tropical Agriculture and Human Resources.

## Set up workspace -----
combined_analyses <- function(working_dir,
                              geo_tiff_dir,
                              output_dir = "threshold",
                              results_file = NULL,
                              display_plot = TRUE,
                              verbose = TRUE)
{
    source("~/Projects/lkfs/data/DrylandAg/R/dry-ag-utilities.R",
           local = TRUE)

    if(!file.exists(working_dir))
        stop(sprintf("The directory '%s' does not exist", working_dir))
    else {
        setwd(working_dir)
        if(verbose == TRUE)
            message(sprintf("Set working directory to %s", working_dir))
    }

    if(!file.exists(geo_tiff_dir))
        stop(sprintf("The directory '%s' does not exist", geo_tiff_dir))

    ## Output folder
    if(!file.exists(output_dir))
        if(askYesNo(msg = sprintf("Output directory %s does not exist.  Create?",
                                  output_dir)))
            dir.create(output_dir)
        else
            stop("Cannot continue without an output directory")

    my.colors <- colorRampPalette(c("blue", "green", "light green", "yellow", "orange", "red"))


    library('raster')
    library('rgdal')
    library('rgeos')  # used to crop the ahupuaa shapefile for plotting
    library('sp')



########### DEFINE FUNCTIONS to calculate seasonality statistics ########################
    #' Pmonthly calculates the monthly probability distribution
    #' p_m = monthly rf/ annual rf
    #' @param rfmonths A 12-raster stack or brick of monthly rainfall
    #' @param rfann    A raster of annual rainfall
    #' @return A raster brick of fraction of annual rainfall in a given month (probability distribution)
    Pmonthly <- function(rfmonths, rfann) {return (rfmonths / rfann)}

    #' RelEntropy calculates the relative entropy of rainfall within a year
    #' D = sum across months m for a give year k: (p_k,m * log2(p_k,m / q_m)) where q_m=1/12, uniform distribution
    #' @param p_m A 12-raster brick of monthly probability distributions
    #' @return A raster layer of relative entropy, or deviation from a uniform distribution
    RelEntropy <- function(p_m) {return (sum(p_m * log2(p_m * 12)))}

    #' Seasonality calculates the seasonality S of rainfall within a year by scaling D
    #' S = D * rfann/Rmax, where Rmax is the maximum value in the raster
    #' @param rfmonths A 12-raster stack or brick of monthly rainfall
    #' @return A raster layer of seasonality, which scales relative entropy
    Seasonality <- function(D, rfann) {
        Rmax <- cellStats(rfann, stat="max")
        return (D * rfann / Rmax)}

    #' WaterYearMean rearranges mean monthly rainfall to start from water year
    #' @param rfmonths A 12-raster stack or brick of mean monthly rainfall
    #' @return wystart An integer indicating first month of water year (follows driest month)
    #' @return rfmonths.wy A 12-raster stack or brick of mean monthly rainfall
    WaterYearMean <- function(rfmonths) {
        rastermean <- cellStats(rfmonths, stat = "mean")
        wystart <- as.numeric(which.min(rastermean)) + 1

        wy <- if(wystart == 1)
                  1:12
              else
                  c(wystart:12, 1:(wystart - 1))

        rfmonths.wy <- rfmonths[[wy]]       # rearrange months for water year
        list(wystart = wystart, rfmonths.wy = rfmonths.wy)
    }

    #' Centroid (demodulated phase) indicates wet season timing, first moment of rfmonths
    #' Calculate centroid based on start to water year; find this with cellStats(stack(months), stat="mean")
    #' C=1/rfann*sum(rfmonths*(1:12))
    #' @param rfmonths.wy A 12-raster stack or brick of monthly rainfall arranged for water year
    #' @param wystart  An integer from 1:12 indicating the month following the lowest rainfall month;
    #' @return A raster layer of wet season centroid
    Centroid <- function (rfmonths.wy, wystart, calendar = TRUE){
        ann.wy <- sum(rfmonths.wy)
        centroid.wy <- 1 / ann.wy * sum(rfmonths.wy * (1:12))  # centroid based on wy
        centroid.cal <- wystart + centroid.wy - 1            # centroid based on cal yr
        if(calendar == TRUE) {
            return(centroid.cal)
            if(display_plot == TRUE)
                print("calendar year")
        }
        else {
            return(centroid.wy)
            if(display_plot == TRUE)
                print("water year")
        }
    }

    #' Entropic spread indicates duration of rainfall, second moment of rfmonths
    #' Calculate spread based on start to water year; find this with cellStats(stack(months), stat="mean")
    #' Z=1/rfann*sum for m from 1:12 [(m-C)^2*rfmonth(m))
    #' @param rfmonths A 12-raster stack or brick of monthly rainfall
    #' @param wystart  An integer from 1:12 indicating the month following the lowest rainfall month;
    #' @return A raster layer of wet season duration
    Duration <- function(rfmonths.wy, wystart){
        C <- Centroid(rfmonths.wy, wystart, calendar = FALSE)
        z1 <- list()
        for (m in 1:12) {
            z1[[m]] <- (m-C) ^ 2 * rfmonths.wy[[m]]
            if(display_plot == TRUE)
                print(m)
        }
        return (sqrt((1 / sum(rfmonths.wy)) * sum(brick(z1))))
    }


### Write some functions to help with thresholding #####
### Consider the locations of optimal growing conditions
### Consider the length of the growing season

    ## thresh1: a function to threshold based on a single condition
    thresh1 <- function(mogrids,    # mogrids is a rasterstack
                        threshold,
                        direction = c("min", "max"),
                        namegrid) {  # namegrid is character
        if(direction == "min") {
            thresh.grids <- mogrids > threshold
            if(display_plot == TRUE) {
                dev.new()
                plot(thresh.grids,
                     main = paste(namegrid, ">", threshold))
            }
            thresh.mos <- sum(thresh.grids)  # how many months at this condition?
            if(display_plot == TRUE) {
                dev.new()
                plot(thresh.mos,
                     main = paste("Number of months", namegrid, ">", threshold))
            }
        }
        else
            if(direction == "max") {
                thresh.grids <- mogrids < threshold
                if(display_plot == TRUE) {
                    dev.new()
                    plot(thresh.grids,
                         main = paste(namegrid, "<", threshold))
                }
                thresh.mos <- sum(thresh.grids)  # how many months at this condition?
                if(display_plot == TRUE) {
                    dev.new()
                    plot(thresh.mos,
                         main = paste("Number of months", namegrid, "<", threshold))
                }
            }

        return(list(thresh = thresh.grids,
                    months = thresh.mos,
                    descrip = paste("set", direction, namegrid)))
    }

    ## thresh2: a function to identify pixels that meet these conditions
    thresh2 <- function(thresh.grids1,   # thresh.grids is a raster
                       thresh.grids2,
                       conds){          # conds is a character vector of conditions
        thresh.grids_1.2 <- thresh.grids1 & thresh.grids2
        if(display_plot == TRUE) {
            dev.new()
            plot(thresh.grids_1.2,
                 main = paste(conds[1], "&", conds[2]))
        }
        thresh.grids_1.2mos <- sum(thresh.grids_1.2) # how many months at this condition?
        if(display_plot == TRUE) {
            dev.new()
            plot(thresh.grids_1.2mos,
                 main = paste("Number of months", conds[1], "&", conds[2]))
        }

        return(list(thresh = thresh.grids_1.2,
                    months = thresh.grids_1.2mos,
                    descrip = paste(conds[1], "&", conds[2])))
    }


    ## READ IN DATA: long term mean monthly and annual data from GeoTiff ##########
    ## Read in RF (P) geoTiff

    rf <- read_geo_tiff(file_name = "rf.mostack.tif",
                        tiff_dir = geo_tiff_dir,
                        names = month.abb)

    rf.ann <- read_geo_tiff(file_name ="rf.tif",
                            tiff_dir = geo_tiff_dir,
                            is_stack = FALSE)

    ## Read in PET geoTiff
    pet <- read_geo_tiff(file_name = "petstack.tif",
                         tiff_dir = geo_tiff_dir,
                         names = month.abb)

    pet.ann <- read_geo_tiff(file_name = "pet.tif",
                             tiff_dir = geo_tiff_dir,
                             is_stack = FALSE)

    ## Read in aridity (PET/P) geoTiff
    aridity <- read_geo_tiff(file_name = "aridity.mostack.tif",
                             tiff_dir = geo_tiff_dir,
                             names = month.abb)

    aridity.ann <- read_geo_tiff(file_name = "aridity.tif",
                                 tiff_dir = geo_tiff_dir,
                                 is_stack = FALSE)

    ## Read in AET
    aet <- read_geo_tiff(file_name = "aetstack.tif",
                         tiff_dir = geo_tiff_dir,
                         names = month.abb)

    aet.ann <- read_geo_tiff(file_name = "aet.tif",
                             tiff_dir = geo_tiff_dir,
                             is_stack = FALSE)

    ## Read in AET/P geoTiff
    aetrf <- read_geo_tiff(file_name = "aetrf.mostack.tif",
                           tiff_dir = geo_tiff_dir,
                           names = month.abb)

    aetrf.ann <- read_geo_tiff(file_name = "aetrf.tif",
                               tiff_dir = geo_tiff_dir,
                               is_stack = FALSE)

    ## Read in RF Seasonality geoTiff
    rfseason <- read_geo_tiff(file_name = "rfseasonstack.tif",
                              tiff_dir = geo_tiff_dir,
                              names = c("CV_MeanRF","RelEntropy_D",
                                        "Seasonality_S", "Centroid_C",
                                        "Duration_Z"))

############ CALCULATE SEASONALITY METRICS FOR ARIDITY ##################################
    if(verbose == TRUE)
        message("Calculating seasonality metrics for aridity")
                                        # Coefficient of Variation (sd/mean)
    aridity_cv <- cv(aridity)  # month-to-month variability

    ## Calculate the mean aridity for each month (use this for entropy later?)
    aridmean <- mean(aridity)  # compare this to 1/12 for uniform rainfall distrib
    Parid <- Pmonthly(aridity, aridmean)
    lims <- c(min(cellStats(Parid, stat = "min")),
             max(cellStats(Parid, stat = "max")))
    if(display_plot == TRUE) {
        dev.new()
        plot(Parid, zlim = lims,
             main = names(Parid))
    }

    ## calculate relative entropy (Feng et al 2013)
    D <- RelEntropy(Parid)

    ## calculate seasonality (Feng et al 2013)
    S <- Seasonality(D, aridity.ann)

###### Centroid and Duration: more complicated
    ## Identify start of water year (dryness/aridity year)
    ## for each pixel (Kona/Kohala/Kau all diff)
    min.mo <- which.min(aridity) # which LAYER has minimum value (return raster of layer indices)
    max.mo <- which.max(aridity) # which layer index has max value at given pixel

    wy.mo <- min.mo + 1     # water year begins the month after lowest value
    wy.mo[wy.mo == 13] <- 1 # convert month 13 to month 1

    ## repeat for all months: creating index rasters
    wy2 <- wy.mo + 1;
    wy2[wy2 == 13] <- 1  # each pixel indicates month of water year

    wy3 <- wy2 + 1
    wy3[wy3 == 13] <- 1

    wy4 <- wy3 + 1
    wy4[wy4 == 13] <- 1

    wy5 <- wy4 +1
    wy5[wy5 == 13] <- 1

    wy6 <- wy5 + 1
    wy6[wy6 == 13] <- 1

    wy7 <- wy6 + 1
    wy7[wy7 == 13] <- 1

    wy8 <- wy7 + 1
    wy8[wy8 == 13] <- 1

    wy9 <- wy8 + 1
    wy9[wy9 == 13] <- 1

    wy10 <- wy9 + 1
    wy10[wy10 == 13] <- 1

    wy11 <- wy10 + 1
    wy11[wy11 == 13] <- 1

    wy12 <- wy11 + 1
    wy12[wy12 == 13] <- 1

    h2o.mo <- aridity # raster stack

    for (i in 1:12){  # For each month of the calendar year
        ## where water year month pixel = Jan, Feb, Mar
        ## write monthly aridity for that month
        h2o.mo[[1]][wy.mo == i] <- aridity[[i]][wy.mo == i]

        h2o.mo[[2]][wy2 == i] <- aridity[[i]][wy2 == i]

        h2o.mo[[3]][wy3 == i] <- aridity[[i]][wy3 == i]

        h2o.mo[[4]][wy4 == i] <- aridity[[i]][wy4 == i]

        h2o.mo[[5]][wy5 == i] <- aridity[[i]][wy5 == i]

        h2o.mo[[6]][wy6 == i] <- aridity[[i]][wy6 == i]

        h2o.mo[[7]][wy7 == i] <- aridity[[i]][wy7 == i]

        h2o.mo[[8]][wy8 == i] <- aridity[[i]][wy8 == i]

        h2o.mo[[9]][wy9 == i] <- aridity[[i]][wy9 == i]

        h2o.mo[[10]][wy10 == i] <- aridity[[i]][wy10 == i]

        h2o.mo[[11]][wy11 == i] <- aridity[[i]][wy11 == i]

        h2o.mo[[12]][wy12 == i] <- aridity[[i]][wy12 == i]
    }

    wys <- stack(h2o.mo)  # raster stack

    ## Centroid of aridity year, pixel-by-pixel start to "water year"
    C <- Centroid(rfmonths.wy = wys, wystart = wy.mo, calendar = TRUE)

    C[C > 12] <- C[C > 12] - 12

    ## Duration of dryness/aridity year, pixel-by-pixel start to "water year"
    ## Z<-Duration(rfmonths.wy=wys, wystart= wy.mo)

################### THRESHOLDING with planting variables  ###############################
### Read in Tmin and Tmean and align projection #####
    if(verbose == TRUE)
        message("Calculating threshold with planting variables")

    tmin <- projectRaster(stack(file.path(geo_tiff_dir,
                                          "tminstack.tif")),
                          aridity)
    tmean <- projectRaster(stack(file.path(geo_tiff_dir,
                                           "tmeanstack.tif")),
                           aridity)
    names(tmin) <- month.abb
    names(tmean) <- month.abb

    plant <- list(rf, rf.ann, aridity, aetrf, pet, aet, rfseason, tmean,
                  tmin)
    names(plant) <- c("rf", "rf.ann", "aridity", "aetrf", "pet", "aet",
                      "rfseason", "tmean", "tmin")

### Apply the thresholding functions to the data #####
                                        # Set desired criteria
    rfthresh.min <- 90      # Set rainfall threshold in mm
    aridthresh.max <- 2.5   # Set aridity threshold as ratio
    tmeanthresh.min <- 18   # Set air temperature threshold in C

    if(verbose == TRUE)
        message("Applying threshold functions to the data")
                                        # Monthly rainfall > threshold
    rf90 <- thresh1(mogrids = rf,
                    threshold = rfthresh.min,
                    direction = "min",
                    namegrid = "rainfall (mm)")

                                        # Monthly temperature > threshold
    tmean18 <- thresh1(mogrids = tmean,
                       threshold = tmeanthresh.min,
                       direction = "min",
                       namegrid = "mean air temperature (C)")

                                        # Monthly aridity< threshold
    arid2.5 <- thresh1(mogrids = aridity,
                       threshold = aridthresh.max,
                       direction = "max",
                       namegrid = "PET/P")

### Intersection of 2 criteria
    if(verbose == TRUE)
        message("Intersecting two threshold criteria")
                                        # Monthly rainfall > 90 AND Tmean >20
    rf90.tmean18 <- thresh2(tmean18$thresh, rf90$thresh,
                            conds = c("Tmean > 18C", "P > 90 mm"))

                                        # Monthly aridity < 2.5 AND Tmean >18
    arid2.5.tmean18 <- thresh2(tmean18$thresh, arid2.5$thresh,
                              conds = c("Tmean > 18C", "Aridity < 2.5"))

### Difference when you consider aridity vs rainfall
    diff_arid2.5.rf90 <- arid2.5.tmean18$months - rf90.tmean18$months
    mask0 <- abs(diff_arid2.5.rf90) < 2
    if(display_plot == TRUE) {
        dev.new()
        plot(mask(diff_arid2.5.rf90, mask0, maskvalue=T),
             col = rev(my.colors(20)),
             main = "Difference of aridity vs. rainfall")
    }

####### Look closer: VISUALIZE STATISTICS by field system #########################
### Prepare thresholding grids for visualization -----
    if(verbose == TRUE)
        message("Preparing thresholding grids for visualization")
    plant2 <- c(plant = plant,
               rf90$months, rf90$thresh,
               tmean18$months, tmean18$thresh,
               arid2.5$months, arid2.5$thresh,
               rf90.tmean18$months, rf90.tmean18$thresh,
               arid2.5.tmean18$months, arid2.5.tmean18$thresh,
               diff_arid2.5.rf90)

    names(plant2) <- c(names(plant),
                       "rf90", "rf90_12",
                       "tmean18", "tmean18_12",
                       "arid2_5", "arid2_5_12",
                       "rf90_tmean18", "rf90_tmean18_12",
                       "arid2_5_tmean18", "arid2_5_tmean18_12",
                       "diff_arid2_5_rf90")


    ## save all this to GeoTiff
    threshold <- plant2[10:17]
    for (i in 1:length(threshold)) {
        writeRaster(threshold[[i]],
                    filename = file.path(working_dir,
                                         output_dir,
                                         names(threshold)[i]),
                    format = "GTiff",
                    overwrite = TRUE)
        if(verbose == TRUE)
            message(sprintf("Writing GeoTIFF file to '%s.tif'",
                            file.path(output_dir,
                                      names(threshold)[i])))
    }

    ret <- list(plant2 = plant2,
                wys = wys,
                wy.mo = wy.mo,
                aridity.ann = aridity.ann,
                C = C,
                plant_index = c(1:9),
                plant2_index = c(10:20))

    if (!is.null(results_file)) {
        out_file <- file.path(output_dir, results_file)
        if(verbose == TRUE) {
            fmt <- "Writing results to %s\nUse the function readRDS(file = \"%s\") to read them"
            message(sprintf(fmt = fmt, out_file, out_file))}
        saveRDS(object = ret, file = out_file)
    }

    if(verbose == TRUE) message("Pau")

    ret
}
