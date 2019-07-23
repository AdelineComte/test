######GOALS/OUTPUTS#############
#Obtaining a sampling points grid: 
##possibility to randomize or not, -> Rand=T/F : OUTPUT = RandXXX/SysSamplXXX
##to select the sampling of points within departments, -> SelDep=T/F , Dep=list(departments numbers)
##according to latitude and longitude data, -> SelLongLat=T/F, Latmax/min and Longmax/min in L93 (warnings : same CRS for the Zone ?)
##or according to a buffer.-> SelBuffer=T/F
#In the output (csv), we obtain the coordinates (longitude, latitude) of the grid points.
#We add a column x as in the output of SpNuit_Coord.R, which corresponds to the number of participations; here =1
##and a column id= row number

##########INPUTS################
#Sample : the number of sampling points
#Zone = map of France with its departments, download https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/

library(data.table)
library(raster)
library(rgdal)

#Zone="C:/Users/Yves Bas/Documents/natura/jasses/domaine.shp"
#Zone="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"
Zone="C:/Users/Cimcä/Desktop/Chiro/Fichiers SIG/Geofla_dept_fr/geoflar-departements-2015.shp" # en WGS84
#Zone="C:/Users/Cimcä/Desktop/chiro/Fichiers SIG/Départements/departements-20180101.shp"  # en WGS84

Sample=5500
SelDep=T  # F : on ne veut pas sélectionner de départements
#Dep=c("09","12","31","32","46","65","81","82") #midipy
#Dep=c("09","11","12","30","31","32","34","46","48","65","66","81","82") #occitanie
#Dep=c("12","30","34","48") #4
#Dep=c("07","11","12","13","30","34","48","81") #8
#Dep=c("07","11","12","13","26","30","34","48","81","84") #10
#Dep=c("04","05","07","09","11","12","13","15"
#     ,"26","30","31","34","38","42","43","46","48"
#    ,"63","66","81","82","83","84")
#Dep=c("75")
#Dep=c("30","34")
#Dep=c("75")
#Dep=c("75","77","78","91","92","93","94","95") #idf

#France métropolitaine (2 lignes suivantes) 
#not necessary if the layer Zone does not contain the overseas departments
Dep=as.character(seq(10,95))
Dep=c(Dep, "2A","2B","01","02","03","04","05","06","07","08","09")

Rand=F #randomise

SelLongLat=F # selectionner une plage par longitude et lattittude
LatMin=6297000
LatMax=6308000
LongMin=755000
LongMax=766000

SelBuffer=F # selectionner un buffer
LatOrigin=6303000
LongOrigin=762000
Radius=91000


#France_departement
Sys.time()
#FranceD=readShapeSpatial(Zone)
FranceD = shapefile(Zone)
#FranceD = Zone
crs(FranceD)
Sys.time()

Suffix=""
if(SelDep)  ## Pour sélectonner quelques départements.
{
  
  FranceD=subset(FranceD,FranceD$code_dept %in% Dep)
  for (i in 1:length(Dep))
  {
    Suffix=paste(Suffix,Dep[i],sep="_")
  }
}
FranceD$code_dept

#FranceD=subset(FranceD,select="OBJECT_ID")

if(SelLongLat)
{
  FranceD=crop(FranceD,extent(c(LongMin,LongMax,LatMin,LatMax)))
  Suffix=paste(Suffix,LongMin,LongMax,LatMin,LatMax,sep="_")
  
}

if(SelBuffer)
{
  p <- SpatialPoints(cbind(LongOrigin,LatOrigin))
  proj4string(p)=proj4string(FranceD)
  pBuffer=buffer(p,width=Radius)
  FranceD=crop(FranceD,pBuffer)
  Suffix=paste("Radius_",Radius)
}



if(Suffix!="")
{
  writeOGR(FranceD,dsn="C:/Users/Cimcä/Desktop/chiro/Fichiers_obtenus"
           ,layer=paste0(substr(basename(Zone),1,nchar(basename(Zone))-4),Suffix)
           ,driver="ESRI Shapefile",overwrite=T)
}


if(Rand)
{
  SysGrid=spsample(FranceD,Sample,type="random")
}else{
  SysGrid=spsample(FranceD,Sample,type="regular")
}

CRSW84 <- CRS("+init=epsg:4326") # WGS 84
SysG84=spTransform(SysGrid,CRSW84)
#SysG84<-SysGrid #déjà en WGS84


CoordSG=as.data.frame(SysG84)
names(CoordSG)=c("Group.1","Group.2")
CoordSG$x=1
CoordSG$id=c(1:nrow(CoordSG))

plot(SysG84,cex=1)
plot(SysGrid)


if(Rand)
{
  fwrite(CoordSG,paste0("C:/Users/Cimcä/Desktop/chiro/Fichiers_obtenus/RandPts_",substr(basename(Zone),1,nchar(basename(Zone))-4)
                        ,Suffix,"_",Sample,".csv"))
}else{
  fwrite(CoordSG,paste0("C:/Users/Cimcä/Desktop/chiro/Fichiers_obtenus/SysGrid_",Suffix,"_",Sample,".csv"))
}

