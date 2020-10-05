library(tidyverse)

# 1
library(mvtnorm);
library(MASS);
set.seed(5)
sigma <-matrix(c(4,2,2,3), ncol=2)
x<-rmvnorm(n=500, mean=c(1,2), sigma=sigma,
           method="chol")
z<-kde2d(x[,1],x[,2],n=200);
par(mar=rep(0,4))
persp(z,theta=60,phi=5,col=heat.colors(199,alpha=1),
      shade=0.4,border=NA,box=FALSE)



# 2
library(marmap);
library(lattice);
data(nw.atlantic);
atl<-nw.atlantic;
atl<-as.bathy(atl);
wireframe(unclass(atl),shade=T,aspect=c(1/2,0.1),
          xlab="",ylab="",zlab="",scales=list(draw=F,arrows=FALSE));



atl<-readRDS("malopolskie.rds")

atl2 <- atl %>%
  filter(TERYT_gmn == "1210011") %>%
  dplyr::select(long, lat, height)

atl2$x <- group_indices(atl2, long)
atl2$y <- group_indices(atl2, lat)

atl2<-dplyr::select(atl2, height, x, y) %>%
  arrange(x, y)

atl<-as.bathy(as.data.frame(atl2))

wireframe(unclass(atl),shade=T,aspect=c(1/2,0.1),
          xlab="",ylab="",zlab="",scales=list(draw=F,arrows=FALSE));


# 3
library(rgl);
data(volcano);
z<-3*volcano;
x<-10*(1:nrow(z));
y<-10*(1:ncol(z));
zlim<-range(z);
zlen<-zlim[2]-zlim[1]+1;
colorlut<-terrain.colors(zlen,alpha=0);
col<-colorlut[z-zlim[1]+1];
open3d();
rgl.surface(x,y,z,color=col,alpha=1,back="lines");
#add the contour map in different color
colorlut <- heat.colors(zlen,alpha=1);
col<-colorlut[z-zlim[1]+1];
rgl.surface(x,y,matrix(1,nrow(z),ncol(z)),color=col,back="fill");



# 4
https://gis.stackexchange.com/questions/245724/create-a-3d-topographic-map-with-locations-marked-on-the-map-using-r


# 5
library(maptools)
library(raster)
srtm <- getData("SRTM", lon = -15.59972, lat = 27.965)


srtm <- readRDS("malopolskie.rds") %>%
  filter(TERYT_gmn == "1210011") %>%
  dplyr::select(long, lat, height)

# crop to Gran Canaria & Tenerife
e1 <- extent(min(data_combined$lon) - 1.2, # xmin
             max(data_combined$lon) + 0.1, # xmax
             min(data_combined$lat) - 0.1, # ymin
             max(data_combined$lat) + 0.5) # ymax

srtm_ct <- crop(srtm, e1)

# plot slope and aspect with hill shades
slope <- terrain(srtm_ct, opt = "slope")
aspect <- terrain(srtm_ct, opt = "aspect")
hill <- hillShade(slope, aspect, angle = 45, direction = 45, normalize = TRUE)
plot(hill, col = grey(0:100/100), legend = FALSE)
plot(srtm_ct, col = rainbow(25, alpha = 0.35), add = TRUE)
