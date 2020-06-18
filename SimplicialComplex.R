alpha<-alphaComplexDiag(K2, maxdimension = 3)
plot(alpha$diagram, diagLim = c(0,5))
summary.diagram(alpha$diagram)


#create delauney triangulation and safe all triangles in a dataframe, along with a value (initially 0)
deld <- deldir(K2$X,K2$Y)
trianList <- triang.list(deld)
plot(trianList)

df<-data.frame()
for (i in 1:length(trianList)){
  df<-rbind(df, c(trianList[[i]]$x[1],trianList[[i]]$y[1],trianList[[i]]$x[2],trianList[[i]]$y[2],trianList[[i]]$x[3],trianList[[i]]$y[3],0,0))
}
names(df)<-c("x1","y1","x2","y2","x3","y3","value","colval")


plotTriangles<-function(k){
    df[,'value']<-0
    #base radius around coordinate on number of infections in that municipality
    r=K2[,3]*k
    
    #give each triangle a value based on the circles in which they fall
    for(centerpoint in 1:nrow(K2)){
      radius = r[centerpoint]
      for(t in 1:nrow(df)){
        dist1 = sqrt((K2$X[centerpoint]-df[t,'x1'])^2+(K2$Y[centerpoint]-df[t,'y1'])^2)
        if(dist1<=r[centerpoint]){
          dist2 = sqrt((K2$X[centerpoint]-df[t,'x2'])^2+(K2$Y[centerpoint]-df[t,'y2'])^2)
          if(dist2<=r[centerpoint]){
            dist3 = sqrt((K2$X[centerpoint]-df[t,'x3'])^2+(K2$Y[centerpoint]-df[t,'y3'])^2)
            if(dist3<=r[centerpoint]){
              df[t,'value']=df[t,'value']+r[centerpoint]
            }
          }
        }
      }
    }
    
  df[,'colval'] <- (df[,'value'] + abs(min(df[,'value'] )))/max(df[,'value']  + abs(min(df[,'value'] )))
  
  #plot the triangles and their colours
  plot(trianList)
  for(t in 1:nrow(df)){
    polygon(x=c(df[t,'x1'],df[t,'x2'],df[t,'x3']),y=c(df[t,'y1'],df[t,'y2'],df[t,'y3']),col=rgb(1,1-df[t,'colval'],1-df[t,'colval']))
  }
}

plotTriangles(1/100)
symbols(K2[,1:2], circles=r/100, inches=FALSE, add=TRUE)

