#Taking into account night lighting

##########INPUTS################
#points = the name of csv, with its path -> randomized (RandXXX) or non-randomized (SysSampleXXX) sampling points OR participation points (CoordWGS84)
#layer download on https://ngdc.noaa.gov/eog/viirs/download_dnb_composites.html 

Test=T
Coord_ALAN=function(points,names_coord,bl,layer)
{
  
  library(data.table)
  library(sp)
  library(raster)
  library(maptools)
  FOccSL=points
  OccSL=fread(paste0(FOccSL,".csv"))
  CoordH=names_coord
  BufferLarge=bl
  ALAN=raster(layer)
  
  
  #library(Rnightlights)
  #OccSL=fread("./vigiechiro/Traits/GBIF/OccSL_bush-cricket.csv")
  
  
  testH=match(CoordH,names(OccSL))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[1]]))
  OccSL=subset(OccSL,!is.na(as.data.frame(OccSL)[,testH[2]]))
  
  #ALAN=getCtryNlData("FRA","FRA_adm0",nlTypes="OLS.Y",nlPeriods=2012,nlStats="mean")
  
  coordinates(OccSL) <- CoordH
  proj4string(OccSL) <- CRS("+init=epsg:4326") # WGS 84
  
  Sys.time()
  SpALAN_M=extract(ALAN,OccSL)
  Sys.time()
  OccSL=spCbind(OccSL,SpALAN_M)
  #OccSL$test=log(OccSL$SpALAN_M)
  spplot(OccSL,zcol="SpALAN_M",main="SpALAN_M",col="transparent")
  #spplot(OccSL,zcol="test",main="SpALAN_M",col="transparent")
  
  Sys.time()
  SpALAN_L=extract(ALAN,OccSL,buffer=BufferLarge,fun=mean) # 0.1 sec / donn�es
  Sys.time()
  OccSL=spCbind(OccSL,SpALAN_L)
  
  spplot(OccSL,zcol="SpALAN_L",main="SpALAN_L",col="transparent")
  
  ALAN=data.frame(cbind(coordinates(OccSL),SpALAN_M,SpALAN_L))
  
  fwrite(ALAN,paste0(FOccSL,"_ALAN.csv"))
  
  coordinates(ALAN) <- CoordH
  
  SelCol=sample(c("SpALAN_M","SpALAN_L"),1)
  spplot(ALAN,zcol=SelCol,main=SelCol)
  
}

if(Test){
  #for test
  Coord_ALAN(
    points="C:/Users/Cimc�/Desktop/chiro/Fichiers_obtenus/CoordWGS84_SpNuit2_Seuil90_DataLP_PF_exportTot" #table giving coordinates in WGS84
    #points ="C:/Users/Cimc�/Desktop/Chiro/Fichiers_obtenus/SysGrid__75_100"
    ,names_coord=c("Group.1","Group.2") #vector of two values giving 
    ,bl=5000
    #,layer="C:/Users/Yves Bas/Downloads/SVDNB_npp_20161201-20161231_75N060W_vcmslcfg_v10_c201701271138.avg_rade9.tif"
    ,layer="C:/Users/Cimc�/Desktop/Chiro/Fichiers SIG/SVDNB/SVDNB_npp_20160101-20161231_75N060W_vcm-orm-ntl_v10_c201807311200.avg_rade9.tif"

    )
}