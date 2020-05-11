
## #########################################################################################
## ################## Make pretty figures for publication ##################################
## ## FIGURE 2: Density plots of mean annual climate ---------
## pdf("Fig2_MeanAnnual_v3.pdf", width=9, height=5)
ann_fin <- c(1, 13, 2, 9, 3, 4, NA, 6)

names_ann_fin <- c("Elevation (m)",
                   expression("Mean Air Temperature " (degree*C)),
                   "Annual Rainfall (mm)",
                   "Relative Humidity (%)",
                   "Cloud Frequency",
                   expression("Shortwave Radiation " (Wm^-2)),
                   expression("Mean Wind Speed " (ms^-1)),
                   "Potential ET (mm)")

fig_label <- letters[1:length(ann_fin)]

## Note: sep is data$separate
## par(mfcol=c(2,4), mar=c(4.5, 4, 1, 0.6) + 0.1)
## for (i in 1:length(ann_fin)){
##   if(is.na(ann.fin[i])) {
##     d.koh<-density(raster(all.kohala[[sep[1]]], layer=1), plot=F)
##     d.kon<-density(raster(all.kona[[sep[1]]], layer=1), plot=F)
##     d.kau<-density(raster(all.kau[[sep[1]]], layer=1), plot=F)
##     ylim<-range(d.koh$y, d.kon$y, d.kau$y)
##     xlim<-range(d.koh$x, d.kon$x, d.kau$x)

##     plot(d.koh, xlim=xlim, ylim=ylim, col="#000000", lty=1,
##          xlab=names_ann_fin[i], main="", ylab="", cex.axis=0.9)
##     lines(d.kon, col="#0072B2", lty=2, lwd=1.5)
##     lines(d.kau, col="#E69F00", lty=3, lwd=3)
##     mtext(fig_label[i], side=3, line=-1.3, adj=0.04)
##   }
## else {
##   d.koh<-density(all.kohala[[ann_fin[i]]], plot=F)
##   d.kon<-density(all.kona[[ann_fin[i]]], plot=F)
##   d.kau<-density(all.kau[[ann_fin[i]]], plot=F)
##   ylim<-range(d.koh$y, d.kon$y, d.kau$y)
##   xlim<-range(d.koh$x, d.kon$x, d.kau$x)

##   plot(d.koh, xlim=xlim, ylim=ylim, col="#000000", lty=1,
##        xlab=names_ann_fin[i], main="", ylab="", cex.axis=0.9)
##   lines(d.kon, col="#0072B2", lty=2, lwd=1.5)
##   lines(d.kau, col="#E69F00", lty=3, lwd=3)
##   mtext(fig_label[i], side=3, line=-1.3, adj=0.04)
##   if(i==1){
##     legend("topright", legend=c("Kohala", "Kona", "Kau"),
##            lty=c(1,2,3), lwd=c(1,1.5,2), bty="n",
##            col=c("#000000","#0072B2","#E69F00"))
##     mtext("Density", side=2, line=2.5, cex=0.7)}
##   if(i==2){mtext("Density", side=2, line=2.5, cex=0.7)}
##   }
## }
## dev.off()
