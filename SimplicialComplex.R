alpha<-alphaComplexDiag(K2, maxdimension = 3)
plot(alpha$diagram, diagLim = c(0,5))
summary.diagram(alpha$diagram)



plot(K2[,1:2], pch=19,cex=0.1)
symbols(K2[,1:2], circles=r, inches=FALSE, add=TRUE)
n1<-nrow(K2)-2
n2<-nrow(K2)-1
n3<-nrow(K2)
for (p1 in 1:n1){
  q1<-p1+1
  for (p2 in q1:n2){
    q2<-p2+1
    for (p3 in q2:n3){
      dist12 <- sqrt((K2$X[p1]-K2$X[p2])^2+(K2$Y[p1]-K2$Y[p2])^2)
      dist13 <- sqrt((K2$X[p1]-K2$X[p3])^2+(K2$Y[p1]-K2$Y[p3])^2)
      dist23 <- sqrt((K2$X[p3]-K2$X[p2])^2+(K2$Y[p3]-K2$Y[p2])^2)
      r1<-r[p1]
      r2<-r[p2]
      r3<-r[p3]
        if(dist12<=r1+r2 && dist13<=r1+r3 && dist23<=r2+r3){
          polygon(x=c(K2$X[p1],K2$X[p2],K2$X[p3]),y=c(K2$Y[p1],K2$Y[p2],K2$Y[p3]), col='red', border='black')
        }
      
    }
  }
}

#above is not of much use

#base radius around coordinate on number of infections in that municipality
r=K2[,3]/200

#create delauney triangulation and safe all triangles in a dataframe, along with a value (initially 0)
deld <- deldir(K2$X,K2$Y)
trianList <- triang.list(deld)
plot(triangList)

df<-data.frame()
for (i in 1:length(trianList)){
  df<-rbind(df, c(trianList[[i]]$x[1],trianList[[i]]$y[1],trianList[[i]]$x[2],trianList[[i]]$y[2],trianList[[i]]$x[3],trianList[[i]]$y[3],0))
}
names(df)<-c("x1","y1","x2","y2","x3","y3","value")

#give each triangle a value based on the circles in which they fall
for(centerpoint in 1:nrow(K2)){
  radius = r[centerpoint]
  for(t in 1:nrow(df)){
    dist1 = sqrt((K2$X[centerpoint]-df[t,'x1'])^2+(K2$Y[centerpoint]-df[t,'y1'])^2)
    if(dist1<=r[centerpoint]){
      dist2 = sqrt((K2$X[centerpoint]-df[t,'x2'])^2+(K2$Y[centerpoint]-df[t,'y2'])^2)
      if(dist2<=r[centerpoint]){
        dist3 = sqrt((K2$X[centerpoint]-df[t,'x3'])^2+(K2$Y[centerpoint]-df[t,'y3'])^2)
        if(dist<=r[centerpoint]){
          df[t,'value']=df[t,'value']+r[centerpoint]
        }
      }
    }
  }
}

plot(K2[,1:2])
for(t in 1:nrow(df)){
  polygon(x=c(df[t,'x1'],df[t,'x2'],df[t,'x3']),y=c(df[t,'y1'],df[t,'y2'],df[t,'y3']), col=pal(df[t,'value']))
}

pal <- function(r2){
  if(r2==0){
    return('white')
  }else if(r2<=max(df[,'value'])/6){
    return('blue')
  }else if(r2<=2*max(df[,'value'])/6){
    return('green')
  }else if(r2<=3*max(df[,'value'])/6){
    return('yellow')
  }else if(r2<=4*max(df[,'value'])/6){
    return('orange')
  }else if(r2<=5*max(df[,'value'])/6){
    return('red')
  }else{
    return('black')
  }
}






