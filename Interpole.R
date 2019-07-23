#vérifier crs Limite !! Et indices Title, taxon

library(data.table)
library(dismo)
library(raster)
library(gstat)

arg_CsvSpGI="C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/Pippyg_Act_06_GI_SysGrid__75_100"
arg_ShpFranceD="geoflar-departements-201575_75"
arg_PixelSize=2000 #PixelSize
#ModRF_file=paste0("./VigieChiro/ModPred/ModRFActLog_",args[1],"_Seuil",args[5],".learner")
ModRF="C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/ModRF/ModRFActLog_PippygxportTot.learner"
load(ModRF)
SpeciesList=fread("C:/Users/Cimcä/Desktop/Chiro/Fichiers_base/SpeciesList.csv")
Rasteriz=T

#Limite
Limite=shapefile(paste0("C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/",arg_ShpFranceD))
Sys.time()

LimiteL=as(Limite,'SpatialLines')

Title=substr(arg_CsvSpGI,47,52)   #indiquer positions nom espèce dans chemin

SubT=""
if(sum(grepl("rsq",names(ModRF)))>0)
{
  SubT=paste0(SubT,"PseudoR2 = ",round(ModRF$rsq[length(ModRF$rsq)],2))
}
Num=sum(ModRF$y>0)
SubT=paste0(SubT," / N = ",Num)

PredLoc=fread(paste0(arg_CsvSpGI,".csv"))

coordinates(PredLoc) <- c("Group.1", "Group.2")
proj4string(PredLoc) <- CRS("+init=epsg:4326") # WGS 84
#proj4string(PredLoc) <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")

#if Limite in L93


 # CRSL93 <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")
 # LimiteL<-spTransform(LimiteL,CRSL93)
 # PredL93=spTransform(PredLoc,CRSL93)
 # v <- voronoi(PredL93)


# crs(LimiteL)
# # crs(v)
# # crs(PredLoc)
# 
#if Limite in WGS84
CRSW84 <- CRS("+init=epsg:4326") # WGS 84
PredWGS84=spTransform(PredLoc,CRSW84)
v <- voronoi(PredWGS84)

#plot(v)
VL=crop(v,Limite)


#MaxScale=quantile(subset(PredL93$pred,PredL93$pred>0.1),0.95)
MaxScale=quantile(subset(PredWGS84$pred,PredWGS84$pred>0.1),0.95)
if(is.na(MaxScale)){MaxScale=0.1}
ScaleAt=c(-0.1,c(1:49)/49*MaxScale,Inf)


if(nrow(PredWGS84)<20000)
{
  Taxon=substr(arg_CsvSpGI,47,52)
  test=match(Taxon,SpeciesList$Esp)
  if(is.na(test))
  {
    Title=Taxon
  }else{
    Title=SpeciesList$NomFR[test]
  }
  # print(spplot(VL, 'pred',main=Title,col="transparent"
  #           ,par.settings =
  #            list(axis.line = list(col =  'transparent'))
  #         ,col.regions=get_col_regions(),at=ScaleAt))
  
  png(paste0(arg_CsvSpGI,".png"))
  
  
  
  p=spplot(VL, 'pred',main=Title,col="transparent"
           ,par.settings =
             list(axis.line = list(col =  'transparent'))
           ,col.regions=get_col_regions(),at=ScaleAt,sp.layout = LimiteL
           ,xlab=SubT)
  
  print(p)
  
  dev.off()
  
}

if(Rasteriz)
{
  r <- raster(Limite, res=as.numeric(arg_PixelSize))
  vpred <- rasterize(VL, r, 'pred')
  
  #gs <- gstat(formula=PredL93$pred~1, locations=PredL93, nmax=10, set=list(idp = 0))
  #nn <- interpolate(r, gs)
  ## [inverse distance weighted interpolation]
  #nnmsk <- mask(nn, vpred)
  #plot(nnmsk,main=substr(arg_CsvSpGI,22,27))
  
  #spplot(VL, 'err', col.regions=get_col_regions())
  
  
  spplot(vpred,main=substr(arg_CsvSpGI,22,27),at=ScaleAt)
  writeRaster(vpred,paste0(arg_CsvSpGI,"_pred.asc"),overwrite=T)
  
  png(paste0(arg_CsvSpGI,"_R.png"))
  print(spplot(vpred,main=substr(arg_CsvSpGI,22,27),at=ScaleAt,sp.layout = LimiteL
               ,xlab=SubT))
  dev.off()
  
  
  
  
  
  verr <- rasterize(VL, r, 'err')
  #plot(verr)
  writeRaster(verr,paste0(arg_arg_CsvSpGI,"_err.asc"),overwrite=T)
  
  
}