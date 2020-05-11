
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
