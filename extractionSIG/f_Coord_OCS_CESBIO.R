#Consideration of land use patterns (Small and large buffers)

##########INPUTS################
# points = the name of csv, with its path -> randomized (RandXXX) or non-randomized (SysSampleXXX) sampling points OR participation points (CoordWGS84)

Test=T
Coord_OCS_CESBIO=function(points,names_coord,bs,bm,layer)
{
  library(data.table)
  library(sp)
  library(raster)
  library(maptools)
  FOccSL=points
  OccSL=fread(paste0(FOccSL,".csv"))
  CoordH=names_coord
  BufferSmall=bs
  BufferMedium=bm
  #BufferLarge=5000 #too long
  #r�cup�ration de la couche habitats et de sa nomenclature
  Hab=raster(layer)
  #NomHab=fread("C:/Users/Yves Bas/Downloads/Nom_OSO.csv")
  
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
  
  OccSL$id=c(1:nrow(OccSL))
  
  
  #coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  
  #CRS.new <- CRS(proj4string(CarthageP))
  OccSL_L93=spTransform(OccSL,CRS("+init=epsg:2154"))
  OccSL_L93=crop(OccSL_L93,Hab)
  OccSL=subset(OccSL,OccSL$id %in% OccSL_L93$id)
  
  
  
  #habitats (OCS)
  Sys.time()
  #DataHp=extract(Hab,OccSL[,],buffer=BufferSmall) #2 sec / buffer
  Sys.time()
  
  #Hab1=rasterFromCells(Hab,11)
  Sys.time()
  HabufProp=list()
  for (i in 1:nrow(OccSL)) # 2 sec / points
    #for (i in 1:80)
  {
    radius<-BufferSmall # set the buffer radius
    buf <- buffer(OccSL[i,], width=radius) # buffer extraction per se
    Sys.time()
    Habuf=extract(Hab,buf)
    Sys.time()
    HabufProp[[i]]=as.data.table(t(as.matrix(table(Habuf))/sum(table(Habuf))))
    if(i%%100==1){print(paste(i,"/",nrow(OccSL),Sys.time()))}
  }
  
  HabufPropT=rbindlist(HabufProp,fill=T)
  HabufPropT[is.na(HabufPropT)]=0
  Sys.time()
  
  colnames(HabufPropT)=paste0("SpHO",colnames(HabufPropT),"S")
  
  #select three-levels habitats
  testNC=(nchar(colnames(HabufPropT))==8)
  N3=colnames(HabufPropT)[testNC]
  HabufPropT3l=HabufPropT[,..N3]
  Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,6)))
  
  if(nrow(Prop32)>0)
  {
    #sum three-levels habitats 
    for (i in 1:nrow(Prop32))
    {
      testi=substr(colnames(HabufPropT3l),1,6)==Prop32$V1[i]
      Ni=colnames(HabufPropT3l)[testi]
      HabufPropTi=HabufPropT3l[,..Ni]
      HabufPropTsum=apply(HabufPropTi,MARGIN=1,FUN=sum)
      HabufPropT=cbind(HabufPropT,HabufPropTsum)
      names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"S")
    }
  }
  
  
  #select 2-levels habitats
  testNC=(nchar(colnames(HabufPropT))==7)
  N3=colnames(HabufPropT)[testNC]
  HabufPropT3l=HabufPropT[,..N3]
  Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,5)))
  
  if(nrow(Prop32)>0)
  {
    #sum 2-levels habitats 
    for (i in 1:nrow(Prop32))
    {
      testi=substr(colnames(HabufPropT3l),1,5)==Prop32$V1[i]
      Ni=colnames(HabufPropT3l)[testi]
      HabufPropTi=HabufPropT3l[,..Ni]
      HabufPropTsum=apply(HabufPropTi,MARGIN=1,FUN=sum)
      HabufPropT=cbind(HabufPropT,HabufPropTsum)
      names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"S")
    }
  }
  
  OccSL2=spCbind(OccSL,HabufPropT)
  
  i=sample(c(1:ncol(HabufPropT)),1)
  print(names(HabufPropT)[i])
  print(spplot(OccSL2,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))
  
  
  
  #extract OCS habitats for medium buffers
  Sys.time()
  HabufProp=list()
  for (i in 1:nrow(OccSL)) # 2 sec / points
    #for (i in 1:80)
  {
    radius<-BufferMedium # set the buffer radius
    buf <- buffer(OccSL[i,], width=radius) # buffer extraction per se
    Sys.time()
    Habuf=extract(Hab,buf)
    Sys.time()
    HabufProp[[i]]=as.data.table(t(as.matrix(table(Habuf))/sum(table(Habuf))))
    if(i%%100==1){print(paste(i,"/",nrow(OccSL),Sys.time()))}
  }
  
  HabufPropT=rbindlist(HabufProp,fill=T)
  HabufPropT[is.na(HabufPropT)]=0
  Sys.time()
  
  colnames(HabufPropT)=paste0("SpHO",colnames(HabufPropT),"M")
  
  #select three-levels habitats
  testNC=(nchar(colnames(HabufPropT))==8)
  N3=colnames(HabufPropT)[testNC]
  HabufPropT3l=HabufPropT[,..N3]
  Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,6)))
  
  if(nrow(Prop32)>0)
  {
    #sum three-levels habitats 
    for (i in 1:nrow(Prop32))
    {
      testi=substr(colnames(HabufPropT3l),1,6)==Prop32$V1[i]
      Ni=colnames(HabufPropT3l)[testi]
      HabufPropTi=HabufPropT3l[,..Ni]
      HabufPropTsum=apply(HabufPropTi,MARGIN=1,FUN=sum)
      HabufPropT=cbind(HabufPropT,HabufPropTsum)
      names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"M")
    }
  }
  
  
  #select 2-levels habitats
  testNC=(nchar(colnames(HabufPropT))==7)
  N3=colnames(HabufPropT)[testNC]
  HabufPropT3l=HabufPropT[,..N3]
  Prop32=as.data.table(table(substr(colnames(HabufPropT3l),1,5)))
  
  if(nrow(Prop32)>0)
  {
    #sum 2-levels habitats 
    for (i in 1:nrow(Prop32))
    {
      testi=substr(colnames(HabufPropT3l),1,5)==Prop32$V1[i]
      Ni=colnames(HabufPropT3l)[testi]
      HabufPropTi=HabufPropT3l[,..Ni]
      HabufPropTsum=apply(HabufPropTi,MARGIN=1,FUN=sum)
      HabufPropT=cbind(HabufPropT,HabufPropTsum)
      names(HabufPropT)[ncol(HabufPropT)]=paste0(Prop32$V1[i],"M")
    }
  }
  
  OccSL3=spCbind(OccSL2,HabufPropT)
  
  i=sample(c(1:ncol(HabufPropT)),1)
  print(names(HabufPropT)[i])
  print(spplot(OccSL3,zcol=names(HabufPropT)[i],main=names(HabufPropT)[i]))
  
  
  #I don't do OCS habitats for large buffers because it's quite lengthy (40 sec / points)
  #At this scale, Corine Land Cover is sufficient anyway
  
  
  OccSL_ARajouter=subset(OccSL3,select=grepl("Sp",names(OccSL3)))
  
  OCS=data.frame(cbind(coordinates(OccSL),as.data.frame(OccSL_ARajouter)))
  fwrite(OCS,paste0(FOccSL,"_OCS.csv"))
  
  coordinates(OCS) <- CoordH
  
  SelCol=sample(names(OccSL_ARajouter),1)
  spplot(OCS,zcol=SelCol,main=SelCol)
  
}

if(Test)
{
  #for test
  Coord_OCS_CESBIO(
    #points="./vigiechiro/GIS/PA_Thymus nitens" #table giving coordinates in WGS84
    #points ="C:/Users/Cimc�/Desktop/chiro/Fichiers_obtenus/CoordWGS84_SpNuit2_Seuil90_DataLP_PF_exportTot" #table giving coordinates in WGS84 
    points ="C:/Users/Cimc�/Desktop/chiro/Fichiers_obtenus/SysGrid__75_100"
    ,names_coord=c("Group.1","Group.2") #vector of two values giving 
    ,bs=50
    ,bm=500
    ,layer="C:/Users/Cimc�/Desktop/chiro/Chiro/Fichiers SIG/OCS_2016_CESBIO.tif"
  )
  
}