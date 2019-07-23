library(data.table)
#library(rgdal)
#library(raster)
#library(sp)
#library(ggplot2)
#library(MASS)
#library(rgeos)
#pour afficher les milisecondes
op <- options(digits.secs=3)

#récupération des données participation
Particip=fread("C:/Users/Cimcä/Desktop/Chiro/Fichiers_base/p_export.csv")
#récupération des localités
SiteLoc=fread("C:/Users/Cimcä/Desktop/Chiro/Fichiers_base/sites_localites.txt")

path="C:/Users/Cimcädf/Desktop/Chiro/Fichiers_base/"
#args="SpNuit2_Seuil90_DataLP_PF_exportTot"
args="ALL"

if(args=="ALL")
{
  SelPar=Particip  
}else{
  #recupération des données chiros
  DataCPL3=fread(paste0(path,args,".csv"))
  
  #liste des coordonnées de participations existantes dans ce jeu de données
  ListPar=levels(as.factor(DataCPL3$participation))
  
  #Sélection des données de p_export selon les participations du jeu de données.
  SelPar=subset(Particip,Particip$participation %in% ListPar)
}

#aggrégation des données des sites_localites et des p_export selon les sites
SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
#Extraction des données Longitude et Latitude 
CoordCPL3=aggregate(SelParSL$participation
                    ,by=c(list(SelParSL$longitude),list(SelParSL$latitude))
                    ,FUN=length)


fwrite(CoordCPL3,paste0("C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/CoordWGS84_",args,".csv"))