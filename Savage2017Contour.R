#The following code was used for the analyses reported in:
#Savage, P. E., Tierney, A. T., & Patel, A. D. (2017). Global music recordings support the motor constraint hypothesis for human and avian song contour. Music Perception, 34(3), 327–334.


#The following packages must be installed/loaded
library(maps)

#set working drive
setwd("/Users/pesavage/Documents/Research/Papers/Published/Savage et al. (2017) Music Perception")

#Mapping Garland 35 solo songs (Fig. 1)

#import data
canto148<-read.csv("Garland35LonLat.csv",header=TRUE,row.names=1)
canto.x<-canto148$Lon
canto.y<-canto148$Lat

#USING MAPS, labeling according to song-style region 
map("world", fill=TRUE, col="white", bg="gray90", ylim=c(-60, 90), mar=c(0,0,0,0)) 
points(canto.x,canto.y, bg=c(rep("gray33",19),rep("gray66",16)), pch=21,cex=1.5)

#legend
legend(-180,-33, c("Western", "Non-Western"),pch=21, pt.bg=c("gray66","gray33"),cex=.9)

#Plotting example contours (Fig. 2)
#import data
fanbei<-read.csv("Fig2FanbeiContour.csv",header=TRUE)

#Plot data
par(pch=21, col="black") # plotting symbol and color 
par(mfrow=c(1,4),mar = c(2,2,2,1) + 0.1) # all plots on one page  
for(i in 1:4){ 
  plot(fanbei[1:50,1], fanbei[1:50,i+1], type="l", xlim=c(-1,1),ylim=c(-6.5,6.5),bg="white",ylab="") 
}

#Plotting average contours (Fig. 3)

#multiple graphs
avg.contour<-read.csv("Fig3BAverageContour.csv",header=TRUE)
par(pch=21, col="black") # plotting symbol and color 
par(mfrow=c(5,1),mar = c(0.5,2,0.5,1) + 0.1) # all plots on one page  
for(i in 1:5){ 
  plot(avg.contour[,1],avg.contour[,i+1],type="l",xlim=c(-1,1),ylim=c(-2,2),bg="black",xaxt="n")
polygon(c(avg.contour[,1], rev(avg.contour[,1])) , c(avg.contour[,i+11] , rev(avg.contour[,i+16])) , col = 'grey' , border = NA)
lines(avg.contour[,1],avg.contour[,i+1],type="l")
}

#multiple graphs (individually with different y-axes)
#Garland(all)
avg.contour<-read.csv("Fig3BAverageContour.csv",header=TRUE)

plot(avg.contour[,1],avg.contour[,1+1],type="l",xlim=c(-1,1),bg="black",xaxt="n")

polygon(c(avg.contour[,1], rev(avg.contour[,1])) , c(avg.contour[,1+11] , rev(avg.contour[,1+16])) , col = 'grey' , border = NA)

lines(avg.contour[,1],avg.contour[,1+1],type="l")

#Garland(Western)
plot(avg.contour[,1],avg.contour[,2+1],type="l",xlim=c(-1,1), bg="black",xaxt="n")
polygon(c(avg.contour[,1], rev(avg.contour[,1])) , c(avg.contour[,2+11] , rev(avg.contour[,2+16])) , col = 'grey' , border = NA)
lines(avg.contour[,1],avg.contour[,2+1],type="l")

#Garland(non-Western)
plot(avg.contour[,1],avg.contour[,3+1],type="l",xlim=c(-1,1), bg="black",xaxt="n")
polygon(c(avg.contour[,1], rev(avg.contour[,1])) , c(avg.contour[,3+11] , rev(avg.contour[,3+16])) , col = 'grey' , border = NA)
lines(avg.contour[,1],avg.contour[,3+1],type="l")

#Birdsong
plot(avg.contour[,1],avg.contour[,4+1],type="l",xlim=c(-1,1), bg="black",xaxt="n")
polygon(c(avg.contour[,1], rev(avg.contour[,1])) , c(avg.contour[,4+11] , rev(avg.contour[,4+16])) , col = 'grey' , border = NA)
lines(avg.contour[,1],avg.contour[,4+1],type="l")

#Essen
plot(avg.contour[,1],avg.contour[,5+1],type="l",xlim=c(-1,1), bg="black",xaxt="n")
polygon(c(avg.contour[,1], rev(avg.contour[,1])) , c(avg.contour[,5+11] , rev(avg.contour[,5+16])) , col = 'grey' , border = NA)
lines(avg.contour[,1],avg.contour[,5+1],type="l")

#multiple graphs overlain
gar.norm<-avg.contour[,2]/(max(avg.contour[,2])-min(avg.contour[,2]))
gar.norm<-gar.norm-min(gar.norm)

bird.norm<-avg.contour[,5]/(max(avg.contour[,5])-min(avg.contour[,5]))
bird.norm<-bird.norm-min(bird.norm)

essen.norm<-avg.contour[,6]/(max(avg.contour[,6])-min(avg.contour[,6]))
essen.norm<-essen.norm-min(essen.norm)

plot(avg.contour[,1],gar.norm,type="l",lty=1,xlim=c(-1,1),ylim=c(0,1),bg="black",xaxt="n",yaxt="n")
lines(avg.contour[,1],bird.norm,type="l",lty=2,xlim=c(-1,1),ylim=c(0,1),bg="black",xaxt="n",yaxt="n")

plot(avg.contour[,1],gar.norm,type="l",lty=1,xlim=c(-1,1),ylim=c(0,1),bg="black",xaxt="n",yaxt="n")
lines(avg.contour[,1],essen.norm,type="l",lty=2,xlim=c(-1,1),ylim=c(0,1),bg="black",xaxt="n",yaxt="n")
