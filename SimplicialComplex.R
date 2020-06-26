#FIRST RUN "corona_datasets.R"
library("deldir")

preprocess<-function(dataset){
  new_dataset<-dataset[1:(nrow(dataset)-1),]
  new_dataset<-cbind(new_dataset,lon=NL_municipality_coordinates[,"Longitude"], lat=NL_municipality_coordinates[,"Latitude"])
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

calculateTriangleValues<-function(k,ds,df_input,c,val_name){
  #base radius around coordinate on number of infections in that municipality
  r<-ds[,c]*k
  #give each triangle a value based on the circles in which they fall
  for(centerpoint in 1:nrow(ds)){
    radius <- r[centerpoint]
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
  return(df_input)
}

plotTriangles<-function(trianList_NL,ds,df_input,c,title,val_name,colval_name,maxVal,minVal,color,export){
    for(r in 1:nrow(df_input)){
      colval<-(df_input[r,val_name] + abs(minVal))/(maxVal  + abs(minVal))
      if(!is.na(colval)){
        df_input[r,colval_name] <- colval
      }
    }
  if(export){
  file_name <- paste(paste("C:/Users/s144740/Documents/Schooljaar 6/Topological data analysis/images/",gsub(" ","_",gsub(":","_",now())),'delaunay',title,names(ds)[c],sep="_"),'jpg',sep='.')
  jpeg(file_name, width = 400, height = 400)
  #plot the triangles and their colours
  plot(trianList_NL, main=paste(title,names(ds)[c],sep=" "), xlab = NA, ylab = NA)
  for(t in 1:nrow(df_input)){
    if(color=='red'){
      polygon(x=c(df_input[t,'x1'],df_input[t,'x2'],df_input[t,'x3']),y=c(df_input[t,'y1'],df_input[t,'y2'],df_input[t,'y3']),col=rgb(1,1-df_input[t,colval_name],1-df_input[t,colval_name]))
    }else if(color=='blue'){
      polygon(x=c(df_input[t,'x1'],df_input[t,'x2'],df_input[t,'x3']),y=c(df_input[t,'y1'],df_input[t,'y2'],df_input[t,'y3']),col=rgb(1-df_input[t,colval_name],1-df_input[t,colval_name],1))
    }else if(color=='green'){
      polygon(x=c(df_input[t,'x1'],df_input[t,'x2'],df_input[t,'x3']),y=c(df_input[t,'y1'],df_input[t,'y2'],df_input[t,'y3']),col=rgb(1-df_input[t,colval_name],1,1-df_input[t,colval_name]))
    }
  }
  dev.off()}
  else{
    plot(trianList_NL, main=paste(title,names(ds)[c],sep=" "), xlab = NA, ylab = NA)
    for(t in 1:nrow(df_input)){
      if(color=='red'){
        polygon(x=c(df_input[t,'x1'],df_input[t,'x2'],df_input[t,'x3']),y=c(df_input[t,'y1'],df_input[t,'y2'],df_input[t,'y3']),col=rgb(1,1-df_input[t,colval_name],1-df_input[t,colval_name]))
      }else if(color=='blue'){
        polygon(x=c(df_input[t,'x1'],df_input[t,'x2'],df_input[t,'x3']),y=c(df_input[t,'y1'],df_input[t,'y2'],df_input[t,'y3']),col=rgb(1-df_input[t,colval_name],1-df_input[t,colval_name],1))
      }else if(color=='green'){
        polygon(x=c(df_input[t,'x1'],df_input[t,'x2'],df_input[t,'x3']),y=c(df_input[t,'y1'],df_input[t,'y2'],df_input[t,'y3']),col=rgb(1-df_input[t,colval_name],1,1-df_input[t,colval_name]))
      }
    }
    }
  return(df_input)
}

computeDF<-function(trianList_NL,dataset,rad, title){
  #create empty df template
  df<-data.frame()
  for (i in 1:length(trianList_NL)){
    df<-rbind(df, c(trianList_NL[[i]]$x[1],trianList_NL[[i]]$y[1],trianList_NL[[i]]$x[2],trianList_NL[[i]]$y[2],trianList_NL[[i]]$x[3],trianList_NL[[i]]$y[3]))
  }
  names(df)<-c("x1","y1","x2","y2","x3","y3")
  
  #fill df with values
  endCol<-ncol(dataset)-2
  #endCol<-4
  
  for(c in 3:endCol){
    val_name <- paste('value',names(dataset)[c],sep="_")
    print(paste('Calculating triangle values for ',names(dataset)[c]))
    df[, val_name]<-0
    df<-calculateTriangleValues(rad,dataset,df,c,val_name)
  }
  return(df)
}

plotAll<-function(trianList_NL,df,dataset,title,color, export){
  endCol<-ncol(dataset)-2
  #endCol<-4
  
  maxVal = max(df[1:nrow(df),7:ncol(df)])
  minVal = 0
  for(c in 3:endCol){
    colval_name <- paste('colval',names(dataset)[c],sep="_")
    val_name <- paste('value',names(dataset)[c],sep="_")
    print(paste('Plotting ',names(dataset)[c]))
    df[, colval_name]<-0
    df<-plotTriangles(trianList_NL,dataset,df,c,title,val_name,colval_name,maxVal,minVal,color, export)
  }
  return(df)
}

doEverything<-function(dataset,r,title,color, export){
  dataset_preprocessed<-preprocess(dataset)
  print('Data preprocessing complete')
  trianList_NL<-createTrianglesNL(dataset_preprocessed)
  print('Triangulation of the Netherlands complete')
  df<-computeDF(trianList_NL,dataset_preprocessed,r,title)
  df<-plotAll(trianList_NL,df,dataset_preprocessed,title,color, export)
  return(df)
}

#this actually executes the code: you might want to run one at the time, as each will take ~10 minutes to complete.
#the input requirements are:
# - a dataset in the correct format (dates as column names, one row per city, values run from column 3 all the way till the end)
# - a constant with which the radius will be multiplied (to be chosen after experimentation), might differ per dataset
# - a title for the plots (main will be title + column name (=date))
# - a color, 'red', 'blue', or 'green', for the plot
NL_hospitalizations<-doEverything(Corona_NL_Hospitalizations_municipality,1/200,"NL hospitalizations ",'red', TRUE)
NL_infections<-doEverything(Corona_NL_Infections_municipality,1/120,"NL infections ",'blue', TRUE)

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
