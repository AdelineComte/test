#Consideration of altitude and slope

#####WARNINGS : pb de nombre de lignes ; comprendre !!)

##########INPUTS################
# points = the name of csv, with its path -> randomized (RandXXX) or non-randomized (SysSampleXXX) sampling points OR participation points (CoordWGS84)


Test=T
Coord_Alti=function(points,names_coord,bm,bl,layer)
{
  library(data.table)
  library(sp)
  library(raster)
  library(maptools)
  FOccSL=points
  OccSL=fread(paste0(FOccSL,".csv"))
  CoordH=names_coord 
  BufferMedium=bm
  BufferLarge=bl
  asc_files<- list.files(layer
                         ,pattern =".asc$",full.names=T)
  
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
  
  
  #coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  
  #CRS.new <- CRS(proj4string(CarthageP))
  OccSL_L93=spTransform(OccSL,CRS("+init=epsg:2154"))

  #extraction des données alti
  rast.list <- list()
  for(i in 1:length(asc_files)) { rast.list[i] <- raster(asc_files[i]) }
  rast.list$fun <- mean
  Sys.time()
  AltiTot <- do.call(mosaic,rast.list) # 8 min
  Sys.time()
  #plot(AltiTot)
  Sys.time()
  
  ####################################################
  ####################################################
  #############Extraction altitude####################
  ####################################################
  ####################################################
  #extraction des altitudes ponctuelles
  
  
  #######
  #Buffer S
  #######
  Sys.time()
  SpAltiS=extract(AltiTot,OccSL_L93) # < 0.001 sec / points
  OccSL=spCbind(OccSL,SpAltiS)
  spplot(OccSL,zcol="SpAltiS",main="SpAltiS")
  
  #######
  #Buffer M
  #######
  Sys.time()
  SpAltiM=extract(AltiTot,OccSL_L93,buffer=BufferMedium,fun=mean) # 0.01 sec / points
  OccSL=spCbind(OccSL,SpAltiM)
  spplot(OccSL,zcol="SpAltiM",main="SpAltiM")
  
  #######
  #Buffer L
  #######
  Sys.time()
  SpAltiL=extract(AltiTot,OccSL_L93,buffer=BufferLarge,fun=mean) # 0.02 sec / points
  Sys.time()
  
  OccSL=spCbind(OccSL,SpAltiL)
  spplot(OccSL,zcol="SpAltiL",main="SpAltiL")
  
  ####################################################
  ####################################################
  #############Calcul de la pente#####################
  ####################################################
  ####################################################
  
  #ajout de 8 points cardinaux à 75m de chaques points (N,S,E,O,NO,NE,SE,SO)
  
 
  Coord=as.data.frame(OccSL_L93) #extraire les colonnes x, Group1 et Group2
  ListePointCard=data.frame()
  for (k in 1:nrow(Coord)){
    
    x=c(0,0,0,0,0,0,0,0)
    Group.1=c(Coord$Group.1[k]+75,Coord$Group.1[k]-75,Coord$Group.1[k],Coord$Group.1[k],Coord$Group.1[k]+75*2^(1/2)/2,Coord$Group.1[k]+75*2^(1/2)/2,Coord$Group.1[k]-75*2^(1/2)/2,Coord$Group.1[k]-75*2^(1/2)/2)
    Group.2=c(Coord$Group.2[k]+75,Coord$Group.2[k]-75,Coord$Group.2[k],Coord$Group.2[k],Coord$Group.2[k]+75*2^(1/2)/2,Coord$Group.2[k]+75*2^(1/2)/2,Coord$Group.2[k]-75*2^(1/2)/2,Coord$Group.2[k]-75*2^(1/2)/2)
    
    PointsCard=data.frame(x,Group.1,Group.2)
    ListePointCard=rbind(ListePointCard,PointsCard)
  }
  
  coordinates(ListePointCard) <- CoordH
  proj4string(ListePointCard) <- CRS("+init=epsg:2154")
  
  #######
  #Buffer S
  #######
  AltiListePointCard=extract(AltiTot,ListePointCard) #liste altitudes des points Cardinaux
  
  #calcul de la pente maximale (en degré)
  PenteS=max(atan(abs(AltiListePointCard[1]-SpAltiS[1])/75),atan(abs(AltiListePointCard[2]-SpAltiS[1])/75),atan(abs(AltiListePointCard[3]-SpAltiS[1])/75),atan(abs(AltiListePointCard[4]-SpAltiS[1])/75),atan(abs(AltiListePointCard[5]-SpAltiS[1])/75),atan(abs(AltiListePointCard[6]-SpAltiS[1])/75),atan(abs(AltiListePointCard[7]-SpAltiS[1])/75),atan(abs(AltiListePointCard[8]-SpAltiS[1])/75))
  
  for(k in 1:(nrow(Coord)-1)){
    Pentek=max(atan(abs(AltiListePointCard[k*8+1]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+2]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+3]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+4]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+5]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+6]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+7]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+8]-SpAltiS[k])/75))
    PenteS=c(PenteS,Pentek)}
  OccSL=spCbind(OccSL,PenteS)
  
  #######
  #Buffer M
  #######
  AltiListePointCard=extract(AltiTot,ListePointCard,buffer=BufferMedium,fun=mean) #liste altitudes des points Cardinaux
  
  #calcul de la pente maximale (en degré)
  PenteM=max(atan(abs(AltiListePointCard[1]-SpAltiS[1])/75),atan(abs(AltiListePointCard[2]-SpAltiS[1])/75),atan(abs(AltiListePointCard[3]-SpAltiS[1])/75),atan(abs(AltiListePointCard[4]-SpAltiS[1])/75),atan(abs(AltiListePointCard[5]-SpAltiS[1])/75),atan(abs(AltiListePointCard[6]-SpAltiS[1])/75),atan(abs(AltiListePointCard[7]-SpAltiS[1])/75),atan(abs(AltiListePointCard[8]-SpAltiS[1])/75))
  
  for(k in 1:(nrow(Coord)-1)){
    Pentek=max(atan(abs(AltiListePointCard[k*8+1]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+2]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+3]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+4]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+5]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+6]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+7]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+8]-SpAltiS[k])/75))
    PenteM=c(PenteM,Pentek)}
  OccSL=spCbind(OccSL,PenteM)
  
  #######
  #Buffer L
  #######
  AltiListePointCard=extract(AltiTot,ListePointCard,buffer=BufferLarge,fun=mean) #liste altitudes des points Cardinaux
  
  #calcul de la pente maximale (en degré)
  PenteL=max(atan(abs(AltiListePointCard[1]-SpAltiS[1])/75),atan(abs(AltiListePointCard[2]-SpAltiS[1])/75),atan(abs(AltiListePointCard[3]-SpAltiS[1])/75),atan(abs(AltiListePointCard[4]-SpAltiS[1])/75),atan(abs(AltiListePointCard[5]-SpAltiS[1])/75),atan(abs(AltiListePointCard[6]-SpAltiS[1])/75),atan(abs(AltiListePointCard[7]-SpAltiS[1])/75),atan(abs(AltiListePointCard[8]-SpAltiS[1])/75))
  
  for(k in 1:(nrow(Coord)-1)){
    Pentek=max(atan(abs(AltiListePointCard[k*8+1]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+2]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+3]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+4]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+5]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+6]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+7]-SpAltiS[k])/75),atan(abs(AltiListePointCard[k*8+8]-SpAltiS[k])/75))
    PenteL=c(PenteL,Pentek)}
  OccSL=spCbind(OccSL,PenteL)
  
  ##################SUITE#######################
  
  
  Alti=data.frame(cbind(coordinates(OccSL),SpAltiS,SpAltiM,SpAltiL,PenteS,PenteM,PenteL))
  
  fwrite(Alti,paste0(FOccSL,"_Alti.csv"))
  
  coordinates(Alti) <- CoordH
  SelCol=sample(c("SpAltiS","SpAltiM","SpAltiL","PenteS","PenteM","PenteL"),1)
  spplot(Alti,zcol=SelCol,main=SelCol)
  
}

if(Test)
{
  #for test
  Coord_Alti(
    #points="./vigiechiro/GIS/PA_Thymus nitens" #table giving coordinates in WGS84
    #points ="C:/Users/Adeline/Desktop/chiro/Fichiers_obtenus/CoordWGS84_SpNuit2_Seuil90_DataLP_PF_exportTot" #table giving coordinates in WGS84 
    points ="C:/Users/Cimcä/Desktop/chiro/Fichiers_obtenus/SysGrid__75_100"
    ,names_coord=c("Group.1","Group.2") #vector of two values giving 
    ,bm=500 #range of first buffer in meters
    ,bl=5000 #range of second buffer in meters  
    ,layer="C:/Users/Cimcä/Desktop/chiro/Fichiers SIG/BDALTIV2_MNT_75M_ASC_LAMB93_IGN69_FRANCE"
  )
}