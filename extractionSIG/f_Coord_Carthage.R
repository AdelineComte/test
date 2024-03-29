#carthage P et C

##########INPUTS################
# points = the name of csv, with its path -> randomized (RandXXX) or non-randomized (SysSampleXXX) sampling points OR participation points (CoordWGS84)
# bs,bm,bl = buffers in meters
# layers (carthagec and carthagep) : warterCourse/cours d'eau and water body/ cours d'eau
# layers :download on : https://www.data.gouv.fr/fr/search/?q=Carthage+m%C3%A9tropole


Test=T
Coord_Carthage=function(points,names_coord,bs,bm,bl,carthagep,carthagec)
{
  
  library(data.table)
  library(sp)
  library(raster)
  library(maptools)
  library(rgeos)
  FOccSL=points
  OccSL=fread(paste0(FOccSL,".csv"))
  CoordH=names_coord
  #library(Rnightlights)
  BufferSmall=bs
  BufferMedium=bm
  BufferLarge=bl 
  #r�cup�ration des donn�es Carthage (eau)
  Sys.time()
  CarthageP <- shapefile(carthagep)
  CarthageC <- shapefile(carthagec)
  crs(CarthageP) #Lambert93 : EPSG:2154  +proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs 
  Split=F
  #Start=10001
  #End=20000
  Start=1
  End=5000
  

  
  if(Split)
  {
    OccSL=OccSL[Start:(min(End,nrow(OccSL))),]
  }
  
  
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
  OccSL$id=c(1:nrow(OccSL))
  
  #coordinates(OccSL) <- c("decimalLongitude", "decimalLatitude")
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  
  # CRL93<-CRS("+init=epsg:2154")
  #CRS.new <- CRS(proj4string(CarthageP))
  OccSL_L93=spTransform(OccSL,CRS(proj4string(CarthageP)))
  
  #subset des points d'eau douce
  CarthagePP=CarthageP[CarthageP$Nature=="Eau douce permanente",]
  #subset des cours d'eau permanent
  CarthageCP=CarthageC[CarthageC$Etat=="Permanent",]
  
  ###########################
  ###########################
  ######Plan d'eau###########
  ###########################
  ###########################
  

  
  #buffers of water surface 
  #loop to avoid exceeding memory
  
  BufferS=gBuffer(OccSL_L93,width=BufferSmall,byid=T)
  
  SpPPlistS=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpPPlistS[[k]]=intersect(BufferS[((k-1)*1000+1)
                                     :(min(k*1000,nrow(OccSL_L93))),],CarthagePP) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  SpCarthagePP=do.call(rbind,SpPPlistS) # 0.05 sec / pol
  #plot(SpCarthagePP)
  if(length(SpCarthagePP)>0)
  {
    AreaB=gArea(SpCarthagePP,byid=T)
    AreaAgg=aggregate(AreaB,by=list(SpCarthagePP$id)
                      ,FUN=function(x) sum(x)/BufferSmall^2/pi)
    names(AreaAgg)[ncol(AreaAgg)]="SpWS_S"
    Sys.time()
    OccSL_L93PP=merge(OccSL_L93,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
    OccSL_L93PP$SpWS_S[is.na(OccSL_L93PP$SpWS_S)]=0
    spplot(OccSL_L93PP,zcol="SpWS_S",col="transparent")
    
  }else{
    OccSL_L93PP=OccSL_L93
    OccSL_L93PP$SpWS_S=0
  }
  
  BufferM=gBuffer(OccSL_L93,width=BufferMedium,byid=T)
  
  SpPPlistS=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpPPlistS[[k]]=intersect(BufferM[((k-1)*1000+1)
                                     :(min(k*1000,nrow(OccSL_L93))),],CarthagePP) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  SpCarthagePP=do.call(rbind,SpPPlistS) # 0.05 sec / pol
  plot(SpCarthagePP)
  AreaB=gArea(SpCarthagePP,byid=T)
  AreaAgg=aggregate(AreaB,by=list(SpCarthagePP$id)
                    ,FUN=function(x) sum(x)/BufferMedium^2/pi)
  names(AreaAgg)[ncol(AreaAgg)]="SpWS_M"
  Sys.time()
  OccSL_L93PP=merge(OccSL_L93PP,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93PP$SpWS_M[is.na(OccSL_L93PP$SpWS_M)]=0
  spplot(OccSL_L93PP,zcol="SpWS_M",col="transparent")
  
  BufferL=gBuffer(OccSL_L93,width=BufferLarge,byid=T)
  
  SpPPlistS=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpPPlistS[[k]]=intersect(BufferL[((k-1)*1000+1)
                                     :(min(k*1000,nrow(OccSL_L93))),],CarthagePP) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  SpCarthagePP=do.call(rbind,SpPPlistS) # 0.05 sec / pol
  plot(SpCarthagePP)
  AreaB=gArea(SpCarthagePP,byid=T)
  AreaAgg=aggregate(AreaB,by=list(SpCarthagePP$id)
                    ,FUN=function(x) sum(x)/BufferLarge^2/pi)
  names(AreaAgg)[ncol(AreaAgg)]="SpWS_L"
  OccSL_L93PP=merge(OccSL_L93PP,AreaAgg,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93PP$SpWS_L[is.na(OccSL_L93PP$SpWS_L)]=0
  spplot(OccSL_L93PP,zcol="SpWS_L",col="transparent")
  
 
  
  ###########################
  ###########################
  ######Cours d'eau##########
  ###########################
  ###########################
  
  #for water courses
  SpPClistS=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpPClistS[[k]]=intersect(CarthageCP,BufferS[((k-1)*1000+1)
                                                :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  
  SpCarthagePC=do.call(rbind,SpPClistS) # 0.05 sec / pol
  Sys.time()
  #buftemp=intersect(CarthageCP,BufferS) # 0.05 sec / buffer
  Sys.time()
  LengthB=gLength(SpCarthagePC,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpCarthagePC$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpWC_S"
  OccSL_L93PP=merge(OccSL_L93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93PP$SpWC_S[is.na(OccSL_L93PP$SpWC_S)]=0
  spplot(OccSL_L93PP,zcol="SpWC_S",col="transparent")
  
  SpPClistM=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpPClistM[[k]]=intersect(CarthageCP,BufferM[((k-1)*1000+1)
                                                :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  SpCarthagePC=do.call(rbind,SpPClistM) # 0.05 sec / pol
  Sys.time()
  #buftemp=intersect(CarthageCP,BufferM) # 0.05 sec / buffer
  Sys.time()
  LengthB=gLength(SpCarthagePC,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpCarthagePC$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpWC_M"
  OccSL_L93PP=merge(OccSL_L93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93PP$SpWC_M[is.na(OccSL_L93PP$SpWC_M)]=0
  spplot(OccSL_L93PP,zcol="SpWC_M",col="transparent")
  
  SpPClistL=list()
  Sys.time()
  for (k in 1:ceiling(nrow(OccSL_L93)/1000))
  {
    SpPClistL[[k]]=intersect(CarthageCP,BufferL[((k-1)*1000+1)
                                                :(min(k*1000,nrow(OccSL_L93))),]) # 0.05 sec / pol
    print(paste(k,Sys.time()))
  }
  SpCarthagePC=do.call(rbind,SpPClistL) # 0.05 sec / pol
  Sys.time()
  #buftemp=intersect(CarthageCP,BufferL) # 0.05 sec / buffer
  Sys.time()
  LengthB=gLength(SpCarthagePC,byid=T)
  Sys.time()
  PC_50=aggregate(LengthB,by=list(SpCarthagePC$id),FUN=sum)
  names(PC_50)[ncol(PC_50)]="SpWC_L"
  OccSL_L93PP=merge(OccSL_L93PP,PC_50,by.x="id",by.y="Group.1",all.x=T)
  OccSL_L93PP$SpWC_L[is.na(OccSL_L93PP$SpWC_L)]=0
  spplot(OccSL_L93PP,zcol="SpWC_L",col="transparent")
  
  OccSL_ARajouter=subset(OccSL_L93PP,select=grepl("Sp",names(OccSL_L93PP)))
  
  Carthage=data.frame(cbind(coordinates(OccSL),as.data.frame(OccSL_ARajouter)))
  
  if(Split)
  {
    NewName=paste0(FOccSL,"_Carthage_",Start,"_",End,".csv")
  }else{
    NewName=paste0(FOccSL,"_Carthage.csv")
  }
  
  fwrite(Carthage,NewName)
  
  coordinates(Carthage) <- CoordH
  
  SelCol=sample(names(OccSL_ARajouter),1)
  spplot(Carthage,zcol=SelCol,main=SelCol)
  class(Carthage)
}

if(Test)
{
  #for testing
  Coord_Carthage(
    #points="./vigiechiro/GIS/PA_Thymus nitens" #table giving coordinates in WGS84
    points ="C:/Users/Cimc�/Desktop/chiro/Fichiers_obtenus/CoordWGS84_SpNuit2_Seuil90_DataLP_PF_exportTot" #table giving coordinates in WGS84 
    #points ="C:/Users/Cimc�/Desktop/chiro/Fichiers_obtenus/SysGrid__75_100"
    ,names_coord=c("Group.1","Group.2") #vector of two values giving 
    ,bs=50
    ,bm=500
    ,bl=5000
    #,carthagep="C:/wamp64/www/CARTHAGE_PLAN/HYDROGRAPHIE_SURFACIQUE.shp"
    #,carthagec="C:/wamp64/www/CARTHAGE_COURS/TRONCON_HYDROGRAPHIQUE.shp"
    ,carthagec="C:/Users/Cimc�/Desktop/Chiro/Fichiers SIG/Carthage/TronconHydrograElt_FXX.shp"
    , carthagep="C:/Users/Cimc�/Desktop/Chiro/Fichiers SIG/Carthage/EltHydroSurface_FXX.shp"
   
  )
}