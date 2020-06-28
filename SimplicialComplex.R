#FIRST RUN "corona_datasets.R"
library("deldir")
library("lubridate")
library("stringr")
library("dplyr")


preprocess<-function(dataset){
  if(dataset[1,1]==Corona_NL_Hospitalizations_municipality[1,1] || dataset[1,1]==Corona_NL_Infections_municipality[1,1]){
    new_dataset<-dataset[1:(nrow(dataset)-1),]
    new_dataset<-cbind(new_dataset,lon=NL_municipality_coordinates[,"Longitude"], lat=NL_municipality_coordinates[,"Latitude"])
  } else if(dataset[1,1]==belgie[1,1]){
    new_dataset<-cbind(dataset[,1],dataset[,3],dataset[,11:ncol(dataset)],lon=dataset[,8], lat=dataset[,9])
  } else if(dataset[1,1]==germany[1,1]){
    new_dataset<-cbind(dataset[,1],dataset[,1],dataset[,4:ncol(dataset)],lon=dataset[,3], lat=dataset[,2])
    #new_dataset<-cbind(dataset[,1],dataset[,1],dataset[,4:32],lon=dataset[,3], lat=dataset[,2])
  }
  for(i in 1:nrow(new_dataset)){
    for(j in 1:ncol(new_dataset)){
      if(is.na(new_dataset[i,j])){
        #if NA then make it 0
        new_dataset[i,j]<-0
      }
      if(j>3){
        #make sure all values are doubles (and not characters)
        new_dataset[i,j]<-as.double(new_dataset[i,j])
      }
    }
  }
  new_dataset[,3:ncol(new_dataset)]<-sapply(new_dataset[,3:ncol(new_dataset)], as.numeric)

  return(new_dataset)
}

createTrianglesNL<-function(dataset){
  #create delauney triangulation and safe all triangles in a dataframe
  deld <- deldir(dataset$Longitude,dataset$Latitude)
  trianList <- triang.list(deld)
  return(trianList)
}

calculateTriangleValues<-function(k,ds,df_input,c,val_name,type){
  #base radius around coordinate on number of infections in that municipality
  r<-ds[,c]*k
  #give each triangle a value based on the circles in which they fall
  for(centerpoint in 1:nrow(ds)){
    radius <- r[centerpoint]
    if(type=='voronoi'){
      for(t in 1:nrow(ds)){
        dist<-sqrt((ds$Longitude[centerpoint]-ds$Longitude[t])^2+(ds$Latitude[centerpoint]-ds$Latitude[t])^2)
        if(dist<=radius){
          df_input[t,val_name]=df_input[t,val_name]+r[centerpoint]
        }
      }
    }else{
      for(t in 1:nrow(df_input)){
        dist1 <- sqrt((ds$Longitude[centerpoint]-df_input[t,'x1'])^2+(ds$Latitude[centerpoint]-df_input[t,'y1'])^2)
        if(dist1<=radius){
          dist2 <- sqrt((ds$Longitude[centerpoint]-df_input[t,'x2'])^2+(ds$Latitude[centerpoint]-df_input[t,'y2'])^2)
          if(dist2<=radius){
            dist3 <- sqrt((ds$Longitude[centerpoint]-df_input[t,'x3'])^2+(ds$Latitude[centerpoint]-df_input[t,'y3'])^2)
            if(dist3<=radius){
              df_input[t,val_name]=df_input[t,val_name]+r[centerpoint]
            }
          }
        }
      }
    }
  }
  return(df_input)
}

plotTriangles<-function(trianList_NL,ds,df_input,c,title,val_name,colval_name,maxVal,minVal,color,export,type){
  for(r in 1:nrow(df_input)){
      colval<-(df_input[r,val_name] + abs(minVal))/(maxVal  + abs(minVal))
      if(!is.na(colval)){
        df_input[r,colval_name] <- colval
      }
  }
  if(export){
    file_name <- paste(paste("C:/Users/s144740/Documents/Schooljaar 6/Topological data analysis/images/",gsub(" ","_",gsub(":","_",now())),type,title,str_sub(names(ds)[c],1,10),sep="_"),'jpg',sep='.')
    jpeg(file_name, width = 400, height = 400)
  }
  #plot the triangles and their colours
  plot(ds$Longitude,ds$Latitude, main=paste(title,str_sub(names(ds)[c],1,10),sep=" "), xlab = NA, ylab = NA)
  for(t in 1:nrow(df_input)){
    if(type=='voronoi'){
      pCount<-(length(which(!is.na(trianList_NL[t,])))/2)
      xlist<-c()
      ylist<-c()
      for(p in 1:pCount){
        xlist<-cbind(xlist,trianList_NL[t,2*p-1])
        ylist<-cbind(ylist,trianList_NL[t,2*p])
      }
    }
    else{
      xlist<-c(df_input[t,'x1'],df_input[t,'x2'],df_input[t,'x3'])
      ylist<-c(df_input[t,'y1'],df_input[t,'y2'],df_input[t,'y3'])
    }
    if(color=='red'){
      polygon(x=xlist,y=ylist,col=rgb(1,1-df_input[t,colval_name],1-df_input[t,colval_name]))
    }else if(color=='blue'){
      polygon(x=xlist,y=ylist,col=rgb(1-df_input[t,colval_name],1-df_input[t,colval_name],1))
    }else if(color=='green'){
      polygon(x=xlist,y=ylist,col=rgb(1-df_input[t,colval_name],1,1-df_input[t,colval_name]))
    }
  }
  
  #polygon(x = europeCoords$long, y=europeCoords$lat) 
  if(export){
  dev.off()}
  return(df_input)
}

computeDF<-function(trianList_NL,dataset,rad, title, type){
  #create empty df template
  
  endCol<-ncol(dataset)
  startCol<-3
  if(type=='voronoi'){
    df<-trianList_NL
  }else{
    df<-data.frame()
    for (i in 1:length(trianList_NL)){
      df<-rbind(df, c(trianList_NL[[i]]$x[1],trianList_NL[[i]]$y[1],trianList_NL[[i]]$x[2],trianList_NL[[i]]$y[2],trianList_NL[[i]]$x[3],trianList_NL[[i]]$y[3]))
    }
    names(df)<-c("x1","y1","x2","y2","x3","y3")
    endCol<-ncol(dataset)-2
    startCol<-3
  }
  #fill df with values
  
  for(c in startCol:endCol){
    val_name <- paste('value',names(dataset)[c],sep="_")
    print(paste('Calculating triangle values for ',names(dataset)[c]))
    df[, val_name]<-0
    df<-calculateTriangleValues(rad,dataset,df,c,val_name,type)
  }
  return(df)
}

plotAll<-function(trianList_NL,df,dataset,title,color, export,type){
  endCol<-ncol(dataset)-2
  #endCol<-4
  if(type=='voronoi'){
      df_input<-df_input[,3:ncol(df_input)]
      n<-ncol(trianList_NL)+1
      trianList_NL<-trianList_NL[3:ncol(trianList_NL)]
    
  }else{
    n<-7
  }
  maxVal = max(df[1:nrow(df),n:ncol(df)])

  minVal = 0
  for(c in 3:endCol){
    colval_name <- paste('colval',names(dataset)[c],sep="_")
    val_name <- paste('value',names(dataset)[c],sep="_")
    print(paste('Plotting ',names(dataset)[c]))
    df[, colval_name]<-0
    df<-plotTriangles(trianList_NL,dataset,df,c,title,val_name,colval_name,maxVal,minVal,color, export,type)
  }
  return(df)
}

doEverything<-function(dataset,r,title,color, export, type){
  dataset_preprocessed<-preprocess(dataset)
  print('Data preprocessing complete')
  trianList_NL<-createTrianglesNL(dataset_preprocessed)
  print('Triangulation of the Netherlands complete')
  df<-computeDF(trianList_NL,dataset_preprocessed,r,title, type)
  df<-plotAll(trianList_NL,df,dataset_preprocessed,title,color, export)
  return(df)
}

#this actually executes the code: you might want to run one at the time, as each will take ~10 minutes to complete.
#the input requirements are:
# - a dataset in the correct format (dates as column names, one row per city, values run from column 3 all the way till the end)
# - a constant with which the radius will be multiplied (to be chosen after experimentation), might differ per dataset
# - a title for the plots (main will be title + column name (=date))
# - a color, 'red', 'blue', or 'green', for the plot
NL_hospitalizations<-doEverything(Corona_NL_Hospitalizations_municipality,1/180,"NL hospitalizations ",'red', TRUE, 'delaunay')
NL_infections<-doEverything(Corona_NL_Infections_municipality,1/120,"NL infections ",'blue', TRUE,'delaunay')



BE_infections_preprocessed<-preprocess(belgie[,1:15])
#BE_infections_preprocessed<-cbind(BE_infections_preprocessed[,1:32],BE_infections_preprocessed[,(ncol(BE_infections_preprocessed)-1):ncol(BE_infections_preprocessed)])
trianList_BE<-createTrianglesNL(BE_infections_preprocessed)
BE_triangle_values<-computeDF(trianList_BE,BE_infections_preprocessed,1/300,"BE infections",'delaunay')
BE_infections_plots<-plotAll(trianList_BE,BE_triangle_values,BE_infections_preprocessed,"BE infections",'blue', FALSE,'delaunay')


DU_infections_preprocessed<-preprocess(germany)
trianList_DU<-createTrianglesNL(DU_infections_preprocessed)
DU_triangle_values<-computeDF(trianList_DU,DU_infections_preprocessed,1/2500,"DU infections",'delaunay')
DU_infections_plots<-plotAll(trianList_DU,DU_triangle_values,DU_infections_preprocessed,"DU infections",'blue', FALSE, 'delaunay')


#Below are just some notes, do not mind this.

#NL Datasets
#create delaunay triangulation of the Netherlands
#trianList_NL<-createTrianglesNL()

#NL INFECTIONS:
#mun_infections<-preprocess(Corona_NL_Infections_municipality)
#if you encounter the error 'Error in plot.new() : figure margins too large', just incerase the size of the plots pane
#df_nl_infections<-computeDF(trianList_NL,mun_infections,1/100,"NL infections")
#first computeDF above, before you plotAll below
#df_nl_infections<-plotAll(trianList_NL,df_nl_infections,mun_infections,"NL infections",'red')

#NL HOSPITALIZATIONS:
#mun_hospital<-preprocess(Corona_NL_Hospitalizations_municipality)
#df_nl_hospital<-computeDF(trianList_NL,mun_hospital,1/100,"NL hospitalizations")
#first computeDF above, before you plotAll below
#df_nl_hospital<-plotAll(trianList_NL,df_nl_hospital,mun_hospital,"NL hospitalizations ",'blue')

#NOTE TO SELF:
#symbols(K2[,1:2], circles=r/100, inches=FALSE, add=TRUE)

library('ggvoronoi')

createVoronoiDiagram<-function(dataset){
  #create voronoi diagram and safe all sections in a dataframe
  data_voronoi<-voronoi_polygon(dataset,x="Longitude",y="Latitude")
  test1 <- as(data_voronoi, "SpatialLinesDataFrame")  
  test2 <- as.data.frame(as(test1, "SpatialPointsDataFrame"))
  q<-data.frame()
  for(i in 1:max(test2$Lines.NR)){
    d<-filter(test2, Lines.NR==i)
    lonlat<-cbind(d$Longitude[1],d$Latitude[1])
    c<-cbind(lonlat)
    for(j in 1:length(d)){
      c<-cbind(c,d$coords.x1[j],d$coords.x2[j])
    }
    q<-rbind(q,c)
  }
  colnames<-cbind("Longitude", "Latitude")
  for(k in 1:(ncol(q)/2-1)){
    colnamex= paste("x",k,sep="")
    colnamey= paste("y",k,sep="")
    colnames<-cbind(colnames,colnamex,colnamey)
  }
  print(colnames)
  names(q)<-colnames
  return(q)
}

trianList_DU_voronoi<-createVoronoiDiagram(germany[,2:4])
DU_triangle_values_voronoi<-computeDF(trianList_DU_voronoi,DU_infections_preprocessed,1/10000,"DU infections",'voronoi')
DU_triangle_values_voronoi<-DU_triangle_values_voronoi[,1:(ncol(DU_triangle_values_voronoi)-2)]
DU_infections_plots<-plotAll(trianList_DU_voronoi,DU_triangle_values_voronoi,DU_infections_preprocessed,"DU Infections",'blue', TRUE, 'voronoi')

NL_hospitalizations_preprocessed<-preprocess(Corona_NL_Hospitalizations_municipality)
trianList_NL_voronoi<-createVoronoiDiagram(NL_hospitalizations_preprocessed[,(ncol(NL_hospitalizations_preprocessed)-1):ncol(NL_hospitalizations_preprocessed)])
NL_triangle_values_voronoi<-computeDF(trianList_NL_voronoi,NL_hospitalizations_preprocessed,1/250,"NL Hospitalizations",'voronoi')
NL_triangle_values_voronoi<-NL_triangle_values_voronoi[,1:(ncol(NL_triangle_values_voronoi)-2)]
NL_hospitalizations_voronoi_plots<-plotAll(trianList_NL_voronoi,NL_triangle_values_voronoi,NL_hospitalizations_preprocessed,"NL Hospitalizations",'red', TRUE, 'voronoi')

NL_infections_preprocessed<-preprocess(Corona_NL_Infections_municipality)
trianList_NL_voronoi_infections<-createVoronoiDiagram(NL_infections_preprocessed[,(ncol(NL_infections_preprocessed)-1):ncol(NL_infections_preprocessed)])
NL_triangle_values_voronoi_infections<-computeDF(trianList_NL_voronoi,NL_infections_preprocessed,1/250,"NL Infections",'voronoi')
NL_triangle_values_voronoi_infections<-NL_triangle_values_voronoi_infections[,1:(ncol(NL_triangle_values_voronoi_infections)-2)]
NL_hospitalizations_voronoi_plots_infections<-plotAll(trianList_NL_voronoi_infections,NL_triangle_values_voronoi_infections,NL_infections_preprocessed,"NL Infections",'blue', TRUE, 'voronoi')

BE_infections_preprocessed<-preprocess(belgie)
#BE_infections_preprocessed<-cbind(BE_infections_preprocessed[,1:32],BE_infections_preprocessed[,(ncol(BE_infections_preprocessed)-1):ncol(BE_infections_preprocessed)])
trianList_BE_voronoi_infections<-createVoronoiDiagram(cbind(BE_infections_preprocessed[,110:111],BE_infections_preprocessed[,15]))
BE_triangle_values_voronoi_infections<-computeDF(trianList_BE_voronoi_infections,BE_infections_preprocessed,1/1000,"BE Infections",'voronoi')
BE_infections_plots<-plotAll(trianList_BE_voronoi_infections,BE_triangle_values_voronoi_infections,BE_infections_preprocessed,"BE Infections",'blue', TRUE,'voronoi')




library(rworldmap)
worldMap <- getMap()
europeanUnion <- c("Netherlands")
indEU <- which(worldMap$NAME%in%europeanUnion)
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)


value <- sample(x = seq(0,3,by = 0.1), size = length(europeanUnion),
                replace = TRUE)
europeanUnionTable <- data.frame(country = europeanUnion, value = value)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]


minx<-100000
miny<-100000
maxx<-0
maxy<-0
for(i in 1:nrow(trianList_DU_voronoi)){
  for(j in 1:(ncol(trianList_DU_voronoi)/2-1)){
    if(!is.na(trianList_DU_voronoi[i,2*j+1])){
    if(trianList_DU_voronoi[i,2*j+1]<minx){
      minx<-trianList_DU_voronoi[i,2*j+1]
    }
      if(trianList_DU_voronoi[i,2*j+1]>maxx){
        minx<-trianList_DU_voronoi[i,2*j+1]
      }}
    if(!is.na(trianList_DU_voronoi[i,2*j+2])){
    if(trianList_DU_voronoi[i,2*j+2]<miny){
      miny<-trianList_DU_voronoi[i,2*j+2]
    }
    
    if(trianList_DU_voronoi[i,2*j+2]>maxy){
      maxy<-trianList_DU_voronoi[i,2*j+2]
    }}
  }
}

for(i in 1:nrow(trianList_DU_voronoi)){
  for(j in 1:(ncol(trianList_DU_voronoi)/2-1)){
    if(!is.na(trianList_DU_voronoi[i,2*j+1])){
      if(trianList_DU_voronoi[i,2*j+1]==minx){
        trianList_DU_voronoi[i,2*j+1]<-min(europeCoords$long, minx)
      }
      if(trianList_DU_voronoi[i,2*j+1]==maxx){
        trianList_DU_voronoi[i,2*j+1]<-max(europeCoords$long, maxx)
      }}
    if(!is.na(trianList_DU_voronoi[i,2*j+2])){
      if(trianList_DU_voronoi[i,2*j+2]==miny){
        trianList_DU_voronoi[i,2*j+2]<-min(europeCoords$lat, miny)
      }
      
      if(trianList_DU_voronoi[i,2*j+2]==maxy){
        trianList_DU_voronoi[i,2*j+2]<-max(europeCoords$lat, maxy)
      }}
  }
}



belgie2<-belgie
del<-c()
for(r in 1:nrow(belgie2)){
  for (r2 in (r+1):nrow(belgie2)){
    if(belgie2[r,"Longitude"]==belgie2[r2,"Longitude"]&& belgie2[r,"Latitude"]==belgie2[r2,"Latitude"]){
      del<-cbind(del,r2)
    }
  }
}
del<-rev(sort(del))
del<-unique(del)
belgie<-rbind(belgie[1:443,],belgie[450:478,])

