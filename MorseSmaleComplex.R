MSC1 <- msc.nn(y=as.numeric(K2$Z),x=K2[,1:2],knn = 15)
plot(MSC1$y)
liMSC2<- msc.lm(MSC1, nfold = 10, modelSelect=FALSE, blend=FALSE, verbose=FALSE)
plot(MSC2$ms)
p

msc.graph(K2[,3],K2[,1:2],knn=15, knnd=15, nLevels=2)
  
p<-plot3d(K2)
plot(MSC1,plot=TRUE)

dxyz<-deldir::deldir(x=K2$X,y=K2$Y,z=K2$Z, rw=c(min(K2$X),max(K2$X),min(K2$Y),max(K2$Y)))
persp3d(dxyz)
plot.triang.list(triang.list(dxyz))
d<-divchain(dxyz)
plot(d)


%%-----------------------------------------------------
  
msc4<-msc.nn.kd(y=K2$Z, x=K2[,1:2], knn=15, nLevels=5, bw=0.3)
plot3d(x=msc4$x[,1], y=msc4$x[,2],z=msc4$y)
np <- length(msc4$persistence)
msc4$persistence[np] <- 1
par(mar = c(5, 4, 4, 2) + 3)
plot(msc4$persistence, np:1 + 1, xlab = "Persistence Percentage", ylab = "Extrema", cex = 2.5, cex.lab = 2, cex.axis = 2, type = "p", pch = 19)

msc4_lm<-msc.elnet(msc4, nfold = 10, blend=FALSE)
plot(msc4_lm$ms)
fp<-predict(msc4_lm, K2[,1:2])
plot3d(x=K2[,1], y=K2[,2],z=fp)
fp
plot3d(msc4)

%%---------------------------------------------------------
d<- deldir(K2$X,K2$Y)
d2<-deldir(x=K2$X,y=K2$Y,z=K2$Z, plotit=TRUE)
dlist<- triang.list(d)
plot(dlist)

dlist2<-triang.list(d2)
plot(dlist2)


library(akima)
library(plotly)
s = interp(K2$X, K2$Y, as.numeric(K2$Z))
p <- plot_ly(x = s$x, y = s$y, z = s$z) %>% add_surface()
p

persp3d(d2, add=TRUE)
open3d()
as.mesh3d(d2, col = "gray", coords = c("x", "y", "z"), smooth = TRUE, normals = NULL, texcoords = NULL, add=TRUE)
%%---------------------------------------------
  
  
library("TDA")
X <- circleUnif(400)
Xlim <- c(3.4,7.1); Ylim <- c(50.6, 53.5); Zlim<-c(0,67); by <- 0.1; by2<-5
Xseq <- seq(Xlim[1], Xlim[2], by = by)
Yseq <- seq(Ylim[1], Ylim[2], by = by)
Zseq <- seq(Zlim[1], Zlim[2], by=by2)
Grid <- expand.grid(Xseq, Yseq, Zseq)
Kdist <- kernelDist(K2, Grid = Grid, h = 0.3)
persp3d(Xseq, Yseq, matrix(Kdist, ncol = length(Yseq), nrow = length(Xseq)), xlab = "", ylab = "", zlab = "", main = "Kdist", shade = 0.9,col = 2)

Kde<- kde(K2,Grid=Grid,h=0.3)
persp3d(Xseq, Yseq, matrix(Kde, ncol = length(Yseq), nrow = length(Xseq)), xlab = "", ylab = "", zlab = "", col = 2, border = NA, main = "Kde",  shade = 0.9)

