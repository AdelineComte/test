######GOALS/OUTPUTS#############
#Obtaining a map showing the sampling effort every 10m² 
##according to the type of protocol.

##########INPUTS################
#Sample=5,500 (or 100 times less than the area of France -> every 10m²)
##number of points on the grid
#args : CoordWGS84_Protocole : choice of protocole (see SpNuit_Coord.R)
##warning : if protocole="Pédestre", the Scale is different to do the map
#Zone = map of France with its departments, download https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/

library(data.table)
library(dismo)
library(raster)
library(gstat)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)

#Zone="C:/Users/Yves Bas/Documents/natura/jasses/domaine.shp"
#Zone="C:/Users/Yves Bas/Documents/SIG/Limite_administrative/France_dep_L93.shp"
Zone="C:/Users/Cimcä/Desktop/Chiro/Fichiers SIG/Geofla_dept_fr/geoflar-departements-2015.shp" #normalement en L93 mais en WGS84
#Zone="C:/Users/Cimcä/Desktop/chiro/Fichiers SIG/Départements/departements-20180101.shp"  # Pb en WGS84

arg="C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/CoordWGS84_Pédestre"
Protocole="Pédestre"
Eff_Ech=fread(paste0(arg,".csv"))

################################################
################################################
####Creation of a grid covering France##########
################################################
################################################

Sample=5500
SelDep=T  

#List of all metropolitan departments ( Warning the first 10 : 01,02,...)
Dep=as.character(seq(1,95))
Dep=c(Dep, "2A","2B","01","02","03","04","05","06","07","08","09")
Rand=F #randomise


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

Suffix="Tous" #the long list of departments numbers can't be part of the file name


if(Rand)
{
  SysGrid=spsample(FranceD,Sample,type="random")
}else{
  SysGrid=spsample(FranceD,Sample,type="regular")
}

CRSW84 <- CRS("+init=epsg:4326") # WGS 84
SysG84=spTransform(SysGrid,CRSW84)

CoordSG=as.data.frame(SysG84)
names(CoordSG)=c("Group.1","Group.2")

plot(SysG84,cex=1)
plot(SysGrid)

##################################################
##################################################
##creation of buffers with participations number##
##################################################
##################################################

coordinates(Eff_Ech) <- c("Group.1", "Group.2")
proj4string(Eff_Ech) <- CRS("+init=epsg:4326") # WGS 84r=raster(Eff_Ech)
plot(Eff_Ech)
#r<-raster(Eff_Ech)
#Eff_Echr=rasterize(Eff_Ech,r)

Lambert93 <- CRS("+init=epsg:2154")
Eff_Ech <- spTransform(Eff_Ech,Lambert93)

CoordH=c("Group.1","Group.2")
testH=match(CoordH,names(CoordSG))
CoordSG=subset(CoordSG,!is.na(as.data.frame(CoordSG)[,testH[1]]))
CoordSG=subset(CoordSG,!is.na(as.data.frame(CoordSG)[,testH[2]]))
CoordSG$Id=c(1:nrow(CoordSG))

#Eff_Ech=getCtryNlData("FRA","FRA_adm0",nlTypes="OLS.Y",nlPeriods=2012,nlStats="mean")

coordinates(CoordSG) <- CoordH
proj4string(CoordSG) <- CRS("+init=epsg:4326") # WGS 84

#CRS.new <- CRS(proj4string(CarthageP))
CoordSG_L93=spTransform(CoordSG,Lambert93)
test=over(CoordSG_L93,Eff_Ech)


BufferM=gBuffer(CoordSG_L93,width=10000,byid=T)
BufferL=gBuffer(CoordSG_L93,width=10000,byid=T)

plot(BufferM)

#if(exists("SpCLC_Mtot")){rm(SpCLC_Mtot)}

BufferIntersect=intersect(Eff_Ech,BufferM)
table(BufferIntersect$d) #nombre de points par buffer
BufferNbParticipations=aggregate(BufferIntersect$x,by=list(BufferIntersect$d),FUN=sum)

GrilleNbParticipations=merge(CoordSG_L93,BufferNbParticipations,by.x="Id",by.y="Group.1",all.x=T)

GrilleNbParticipations$x[is.na(GrilleNbParticipations$x)]=0

#GrilleNbParticipations$logx=log(log(GrilleNbParticipations$x+1)+1)
#spplot(GrilleNbParticipations,zcol="logx")

#################################################
#################################################
#####obtaining the map and registration##########
#################################################
#################################################

#polygonalisation
v <- voronoi(GrilleNbParticipations)
FranceD=spTransform(FranceD,Lambert93)
VL=crop(v,FranceD)

#the scale
#For Protocole = Routier or Point fixe
Scale=c(0,0.99,0.999,1,c(2:10),seq(11,19,2),seq(20,50,5),seq(51,100,10),101,max(GrilleNbParticipations$x)+1)
#For  Protocole=Pédestre
#Scale=c(0,0.99,0.999,1,c(2:10),seq(11,19,2),seq(20,max(GrilleNbParticipations$x)+5,5)) 

Nb=subset(GrilleNbParticipations$x,GrilleNbParticipations$x>0)
summary(Nb)

png(paste0("C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/CarteEffortEchantillonage",Protocole,".png"))
p=spplot(VL,"x",col="transparent", col.regions=get_col_regions(),at=Scale)
print(p)
dev.off()