library(data.table)
#library(rgdal)
#library(raster)
#library(sp)
#library(ggplot2)
#library(MASS)
#library(rgeos)
#pour afficher les milisecondes
op <- options(digits.secs=3)

#r�cup�ration des donn�es participation
Particip=fread("C:/Users/Cimc�/Desktop/Chiro/Fichiers_base/p_export.csv")
#r�cup�ration des localit�s
SiteLoc=fread("C:/Users/Cimc�/Desktop/Chiro/Fichiers_base/sites_localites.txt")

path="C:/Users/Cimc�df/Desktop/Chiro/Fichiers_base/"
#args="SpNuit2_Seuil90_DataLP_PF_exportTot"
args="ALL"

if(args=="ALL")
{
  SelPar=Particip  
}else{
  #recup�ration des donn�es chiros
  DataCPL3=fread(paste0(path,args,".csv"))
  
  #liste des coordonn�es de participations existantes dans ce jeu de donn�es
  ListPar=levels(as.factor(DataCPL3$participation))
  
  #S�lection des donn�es de p_export selon les participations du jeu de donn�es.
  SelPar=subset(Particip,Particip$participation %in% ListPar)
}

#aggr�gation des donn�es des sites_localites et des p_export selon les sites
SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
#Extraction des donn�es Longitude et Latitude 
CoordCPL3=aggregate(SelParSL$participation
                    ,by=c(list(SelParSL$longitude),list(SelParSL$latitude))
                    ,FUN=length)


fwrite(CoordCPL3,paste0("C:/Users/Cimc�/Desktop/Chiro/Fichiers_obtenus/CoordWGS84_",args,".csv"))