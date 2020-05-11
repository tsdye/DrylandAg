combined_analysis_graphics <- function(data = NULL,
                                       input_file_name = NULL,
                                       output_file_name = "Fig4_Seasonality_P_Aridity_fin.pdf",
                                       ahupuaa_shape_file,
                                       ahupuaa_layer = "ahupuaa",
                                       verbose = TRUE) {

    if(is.null(data) & is.null(input_file_name))
        stop("One of 'data' and 'input_file_name' required")

    if(!is.null(data) & !is.null(input_file_name))
        stop("Only one of 'data' and 'input_file_name' required")

    if(!is.null(input_file_name))
        if(!file.exists(input_file_name))
            stop(sprintf("The input file '%s' does not exist", input_file_name))

    if(!is.null(input_file_name))
        data <- readRDS(file = input_file_name)

    if(!file.exists(ahupuaa_shape_file))
        stop(sprintf("The file '%s' does not exist", ahupuaa_shape_file))

    library(khroma)
    library(raster)
    library(rgdal)
    library(sp)

    ## Variables for CRS strings
    ## crs_1 <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
    ## crs_2 <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
    crs_3 <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs +datum=WGS84"

    op <- par()

## Prepare for mapping: ahupuaa ---------------------------------------------------------

    if(verbose == TRUE)
        message(sprintf("Reading shape file %s", ahupuaa_shape_file))

    ahupuaa <- readOGR(dsn = ahupuaa_shape_file, layer = ahupuaa_layer)
    if(proj4string(ahupuaa) != crs_3) {
        if(verbose == TRUE)
            message(sprintf("Transforming shape file %s", ahupuaa_shape_file))
        ahupuaa <- spTransform(ahupuaa, CRS = crs_3)
}
    if(verbose == TRUE)
        message(sprintf("Cropping shape file %s", ahupuaa_shape_file))

    ahucrop <- crop(ahupuaa, extent(data$rf))

    aridity_cv <- cv(data$aridity)  # month-to-month variability

#########################################################################################
################## Make pretty figures for publication ##################################

    ## my.colors <- colorRampPalette(c("blue", "green", "light green", "yellow",
    ##                                 "orange", "red"))
    rf.colors<-colorRampPalette(c("red", "orange", "yellow", "light green",
                                  "blue"), bias = 3)
    ## centr.colors<-colorRampPalette(c("blue", "light green",  "yellow", "purple",
    ##                                  "blue"))

    my.colors <- khroma::colour("smooth rainbow")
    centr.colors <- khroma::colour("sunset")

##### FIGURE 4: Maps of seasonality: P and aridity (PET/P) ##############################
    pdf(output_file_name, width = 6, height = 5)

### Map RAINFALL (P) seasonality metrics ####
    ## Mean annual rainfall
    par(mfrow = c(2,3), oma = c(0,0,0,0), mar = c(1, 1, 3, 2) + 0.1)
    plot(ahucrop, lwd = 0.5, main = "Mean annual P (mm)")
    plot(data$rf.ann, alpha = 0.9, add = TRUE, col = rev(my.colors(24)), legend = FALSE)
    plot(data$rf.ann, legend.width = 1.5, legend.only = TRUE, add = TRUE,
         col = rev(my.colors(24)), smallplot = c(.75, .78, .3, .65))
    mtext("(a)", side = 3, line = -1.3, adj = 0.04)

    ## Coefficient of variation, seasonality proxy
    plot(ahucrop, lwd = 0.5, main = "CV of monthly P")
    plot(data$rfseason$CV_MeanRF, alpha = 0.9, add = TRUE, col = my.colors(24),
         legend = FALSE)
    plot(data$rfseason$CV_MeanRF, legend.width = 1.5, legend.only = TRUE, add = TRUE,
         col = my.colors(24), smallplot = c(.75, .78, .3, .65))
    scalebar(d = 40, type = "bar", divs = 4, below = "kilometers",
             xy = c(-156.315, y = 18.95))
    compassRose(x = -156.13, y = 19.3, cex = 0.5)
    mtext("(b)", side = 3, line = -1.3, adj = 0.04)

    ## Centroid of rainy season
    plot(ahucrop, lwd = 0.5, main = "Rainy season centroid (mo)")
    plot(data$rfseason$Centroid_C, alpha = 0.9, add = TRUE, col = centr.colors(24),
         legend = FALSE)
    plot(data$rfseason$Centroid_C, legend.width = 1.5, legend.only = TRUE,
         add = TRUE, col = centr.colors(24), smallplot = c(.75, .78, .3, .65))
    mtext("(c)", side = 3, line = -1.3, adj = 0.04)

                                        # Duration of rainy season
                                        # plot(ahucrop, lwd=0.5, main="Wet season spread (mos)")
                                        # plot(rfseason$Duration_Z, alpha=0.9, add=T, col=rf.colors(24))

### Map ARIDITY (PET/P) seasonality metrics ####
    ## Mean annual aridity
    plot(ahucrop, lwd = 0.5, main = "Mean annual PET/P")
    plot(data$aridity.ann, alpha = 0.9, add = TRUE, col = my.colors(24),
         legend = FALSE)
    plot(data$aridity.ann, legend.width = 1.5, legend.only = TRUE, add = TRUE,
         col = my.colors(24), smallplot = c(.75, .78, .3, .65))
    mtext("(d)", side = 3, line = -1.3, adj = 0.04)

    ## Coefficient of variation of aridity
    plot(ahucrop, lwd = 0.5, main = "CV of monthly PET/P")
    plot(aridity_cv, alpha = 0.9, add = TRUE,
         col = my.colors(24), legend = FALSE)
    plot(aridity_cv, legend.width = 1.5, legend.only = TRUE, add = TRUE,
         col = my.colors(24), smallplot = c(.75, .78, .3, .65))
    mtext("(e)", side = 3, line = -1.3, adj = 0.04)

    ## Centroid of dry season
    plot(ahucrop, lwd = 0.5, main = "Dry season centroid (mo)")
    plot(data$C, col = centr.colors(24), add = TRUE, alpha = 0.9, legend = FALSE)
    plot(data$C, legend.width = 1.5, legend.only = TRUE, add = TRUE,
         col = centr.colors(24), smallplot = c(.75, .78, .3, .65))
    mtext("(f)", side = 3, line = -1.3, adj = 0.04)

                                        # Duration of dry season
                                        # plot(ahucrop, lwd=0.5, main="Aridity spread (mos)")
                                        # plot(Z, col=rev(rf.colors(24)), add=T, alpha=0.8)
                                        # # this is a little confusing- longer dry season in Kona may be due to low seasonality?
                                        # # not sure centroid and duration work for aridity metric

    dev.off()


#########################################################################################
## ### Read in polygons and disaggregate field systems -----
##     HI3dry.wgs84coast<-readOGR(dsn="GIS FILES HERE/ContiguousPolyDryland_wgs84_coast_clipped.shp",
##                                layer="ContiguousPolyDryland_wgs84_coast_clipped")
##     proj4string(HI3dry.wgs84coast)
##     crs<-proj4string(aridity) # "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
##     HI3dry<-spTransform(HI3dry.wgs84coast, CRS=crs)

##     Kohala<-SpatialPolygons(list(test=HI3dry@polygons[[1]]), proj4string=CRS(crs))
##     Kona<-SpatialPolygns(list(test=HI3dry@polygons[[2]]), proj4string=CRS(crs))
##     Kau<-SpatialPolygons(list(test=HI3dry@polygons[[3]]), proj4string=CRS(crs))

##     plant.kohala<-lapply(plant2, FUN= function(x) crop(x, Kohala))
##     plant.kona<-lapply(plant2, FUN= function(x) crop(x, Kona))
##     plant.kau<-lapply(plant2, FUN= function(x) crop(x, Kau))

##     ahu.kohala<-crop(ahupuaa, extent(Kohala)+c(-0.02, 0.02, -0.02, 0.02))
##     ahu.kona<-crop(ahupuaa, extent(Kona)+c(-0.02, 0.02, -0.02, 0.02))
##     ahu.kau<-crop(ahupuaa, extent(Kau)+c(-0.02, 0.02, -0.02, 0.02))

##                                         # Rainfall and seasonality
##     par(mfrow=c(2,3))
##     plot(ahu.kohala, lwd=0.5, main="annual P, Kohala")
##     plot(plant.kohala$rf.ann, col=rf.colors(24), add=T, alpha=0.8, legend.width=1.5)
##     plot(ahu.kona, lwd=0.5, main="annual P, Kona")
##     plot(plant.kona$rf.ann, col=rf.colors(24), add=T, alpha=0.8, legend.width=1.5)
##     plot(ahu.kau, lwd=0.5, main="annual P, Kau")
##     plot(plant.kau$rf.ann, col=rf.colors(24), add=T, alpha=0.8, legend.width=1.5)

##     plot(ahu.kohala, lwd=0.5, main="CV of monthly P, Kohala")
##     plot(plant.kohala$rfseason$CV_MeanRF, col=my.colors(24), add=T, alpha=0.8, legend.width=1.5)
##     plot(ahu.kona, lwd=0.5, main="CV of monthly P, Kona")
##     plot(plant.kona$rfseason$CV_MeanRF, col=my.colors(24), add=T, alpha=0.8, legend.width=1.5)
##     plot(ahu.kau, lwd=0.5, main="CV of monthly P, Kau")
##     plot(plant.kau$rfseason$CV_MeanRF, col=my.colors(24), add=T, alpha=0.8, legend.width=1.5)

##                                         # Centroid
##     plot(ahu.kohala, lwd=0.5, main="Wet season centroid, Kohala")
##     plot(plant.kohala$rfseason$Centroid_C, col=centr.colors(24), add=T, alpha=0.8)
##     plot(ahu.kona, lwd=0.5, main="Wet season centroid, Kona")
##     plot(plant.kona$rfseason$Centroid_C, col=centr.colors(24), add=T, alpha=0.8)
##     plot(ahu.kau, lwd=0.5, main="Wet season centroid, Kau")
##     plot(plant.kau$rfseason$Centroid_C, col=centr.colors(24), add=T, alpha=0.8)

##                                         # Duration
##     plot(ahu.kohala, lwd=0.5, main="Wet season Duration, Kohala")
##     plot(plant.kohala$rfseason$Duration_Z, col=rf.colors(24), add=T, alpha=0.8)
##     plot(ahu.kona, lwd=0.5, main="Wet season Duration, Kona")
##     plot(plant.kona$rfseason$Duration_Z, col=rf.colors(24), add=T, alpha=0.8)
##     plot(ahu.kau, lwd=0.5, main="Wet season Duration, Kau")
##     plot(plant.kau$rfseason$Duration_Z, col=rf.colors(24), add=T, alpha=0.8)


## ##### Maps of optimal growing conditions by month #######################################
##                                         # gisdir<-"GIS FILES HERE"
##                                         # HI3dry.UTM<-readOGR(dsn=paste0(gisdir, "ContiguousPolyDryland_wgs84_coast_clipped.shp"),
##                                         #                     layer="ContiguousPolyDryland_wgs84_coast_clipped")
##                                         # proj4string(HI3dry.UTM)  # UTM Zone 5...
##                                         # HI3dry<-spTransform(HI3dry.UTM,
##                                         #                     CRS="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
##     poly.koh<-crop(HI3dry, extent(Kohala))
##     poly.kon<-crop(HI3dry, extent(Kona))
##     poly.kau<-crop(HI3dry, extent(Kau))

##                                         # Optimal uala growth, Tmean>18, rf>90
##     pdf("Tmean18_P90.pdf")
##     par(mfrow=c(3,4), mar=c(2,1,2,4))
##     for (i in 1:12){
##         plot(ahu.kohala, lwd=0.5, main=month.abb[i])
##         plot(plant.kohala$rf90.tmean18.12[[i]], col.regions=rev(terrain.colors(2)), alpha=0.9, add=T)
##         plot(poly.koh, add=T)
##     }

##     for (i in 1:12) {
##         plot(ahu.kona, lwd=0.5, main = month.abb[i])
##         plot(plant.kona$rf90.tmean18.12[[i]], col.regions = rev(terrain.colors(2)), alpha = 0.9, add = T)
##         plot(poly.kon, add = T)
##     }
##     for (i in 1:12){
##         plot(ahu.kau, lwd = 0.5, main = month.abb[i])
##         plot(plant.kau$rf90.tmean18.12[[i]], col.regions=rev(terrain.colors(2)), alpha=0.9, add=T)
##         plot(poly.kau, add=T)
##     }

##                                         # Optimal uala growth, Tmean>18, Aridity<2.5
##     pdf("Tmean18_Arid2_5.pdf")
##     par(mfrow = c(3,4), mar = c(2,1,2,4))
##     for (i in 1:12){
##         plot(ahu.kohala, lwd = 0.5, main = month.abb[i])
##         plot(plant.kohala$arid2.5.tmean18.12[[i]], col.regions = rev(terrain.colors(2)), alpha = 0.9, add = T)
##         plot(poly.koh, add = T)
##     }

##     for (i in 1:12){
##         plot(ahu.kona, lwd = 0.5, main = month.abb[i])
##         plot(plant.kona$arid2.5.tmean18.12[[i]], col.regions = rev(terrain.colors(2)), alpha = 0.9, add = T)
##         plot(poly.kon, add = T)
##     }
##     for (i in 1:12){
##         plot(ahu.kau, lwd = 0.5, main = month.abb[i])
##         plot(plant.kau$arid2.5.tmean18.12[[i]], col.regions = rev(terrain.colors(2)), alpha = 0.9, add = T)
##         plot(poly.kau, add = T)
##     }
##     dev.off()

## ##### FIGURE 6: Maps of months of optimal growing conditions ############################
##     pdf("Fig6_CultivableMonths2_fin.pdf", width=5.5, height=7)

##                                         # Optimal uala growth, Tmean>18, P>90
##     par(mfrow=c(3,3), mar=c(0,0,1,1), oma=c(1,2,1,1))

##     ahu.kohala@bbox[2]<-20.02
##     ahu.kohala@bbox[4]<-20.29
##     ahu.kona@bbox[2]<-19.29

##     brks<-0:12
##                                         # Kohala
##     plot(ahu.kohala, lwd=0.5, main="Kohala")
##     plot(plant.kohala$rf90.tmean18, breaks=brks,
##          col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
##     plot(poly.koh, add=T)
##     scalebar(d=16, type="bar", divs=4, below = "km")
##     mtext("(a)", side=3, line=-1.3, adj=0.04)
##     mtext(text=expression(paste(T[mean],">18", degree*C, ", P >90mm")),
##           side=2, line = 0, font=1.5)

##                                         # Kona
##     plot(ahu.kona, lwd=0.5, main="Kona")
##     plot(plant.kona$rf90.tmean18, breaks=brks,
##          col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
##     plot(poly.kon, add=T)
##     mtext("(b)", side=3, line=-1.3, adj=0.04)

##     plot(plant.kona$rf90.tmean18, breaks=brks, add=T,   # legend
##          legend.width = 1.5, legend.only=T,
##          col=rev(terrain.colors(12)), smallplot=c(.73, .77, .1, .8)) # x1,x2,y1,y2
##     scalebar(d=20, type="bar", divs=4, below = "km")

##                                         # Kau
##     plot(ahu.kau, lwd=0.5, main="Kau")
##     plot(plant.kau$rf90.tmean18, breaks=brks,
##          col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
##     plot(poly.kau, add=T)
##     scalebar(d=20, type="bar", divs=4, below = "km")
##     mtext("(c)", side=3, line=-1.3, adj=0.04)

## ############# Optimal uala growth, Tmean>18, aridity<2.5
##                                         # Kohala
##     plot(ahu.kohala, lwd=0.5, main="")
##     plot(plant.kohala$arid2.5.tmean18, breaks=brks,
##          col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
##     plot(poly.koh, add=T)
##     mtext("(d)", side=3, line=-1.3, adj=0.04)
##     mtext(text=expression(paste(T[mean],">18", degree*C, ", PET/P <2.5")),
##           side=2, line = 0, font=1.5)
##                                         # Kona
##     plot(ahu.kona, lwd=0.5, main="")
##     plot(plant.kona$arid2.5.tmean18, breaks=brks,
##          col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
##     plot(poly.kon, add=T)

##     plot(plant.kona$arid2.5.tmean18, breaks=brks, add=T,  # legend
##          col=rev(terrain.colors(12)),
##          legend.width = 1.5, legend.only=T,
##          smallplot=c(.73, .77, .1, .8)) #smallplot: x1,x2,y1,y2
##     mtext("(e)", side=3, line=-1.3, adj=0.04)

##                                         # Kau
##     plot(ahu.kau, lwd=0.5, main="")
##     plot(plant.kau$arid2.5.tmean18, breaks=brks,
##          col=rev(terrain.colors(12)), add=T, alpha=0.8, legend=F)
##     plot(poly.kau, add=T)
##     mtext("(f)", side=3, line=-1.3, adj=0.04)

## ### DIFFERENCE when you use Aridity threshold vs Rainfall
##     brk.diff<-seq(-10,12,2)
##     diff.cols<-rev(my.colors(length(brk.diff)))
##     diff.cols[brk.diff<1 & brk.diff>-1]<-"#FFFFFF"

##     mask(diff_arid2.5.rf90,mask0, maskvalue=T)
##                                         # Kohala
##     plot(ahu.kohala, lwd=0.5, main="")
##     plot(plant.kohala$diff_arid2.5.rf90,
##          breaks=brk.diff, col=diff.cols, add=T, alpha=0.9, legend=F)
##     plot(poly.koh, add=T)
##     mtext("(g)", side=3, line=-1.3, adj=0.04)
##     mtext(text=expression(Months[PET/P<2.5] - Months[P>90]), side=2, line = 0, font=1.5)

##                                         # Kona
##     plot(ahu.kona, lwd=0.5, main="")
##     plot(plant.kona$diff_arid2.5.rf90,
##          breaks=brk.diff, col=diff.cols, add=T, alpha=0.8, legend=F)
##     plot(poly.kon, add=T)
##     plot(plant.kona$diff_arid2.5.rf90,
##          breaks=brk.diff, add=T,
##          col=diff.cols, legend.width = 1.5,
##          legend.only=T, smallplot=c(.73, .77, .1, .8)) #smallplot: x1,x2,y1,y2
##     mtext("(h)", side=3, line=-1.3, adj=0.04)

##                                         # Kau
##     plot(ahu.kau, lwd=0.5, main="")
##     plot(plant.kau$diff_arid2.5.rf90,
##          breaks=brk.diff, col=diff.cols, add=T, alpha=0.8, legend=F)
##     plot(poly.kau, add=T)
##     mtext("(i)", side=3, line=-1.3, adj=0.04)

##     dev.off()

}
