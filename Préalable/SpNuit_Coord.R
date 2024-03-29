######GOALS/OUTPUTS#############
#obtain the coordinates (longitude, latitude) of the previously sampled points -> Group.1,Group.2
#and the number of participations related to each set of coordinates.-> x
##########INPUTS################
#args="ALL" : possibility to work on all participations, taking the data from p_export: 
##in this case, Proto=T/F if you only want to select one type of protocol among ("P�destre","Routier","Point_Fixe") 
## Warning -> column "Protocole" added in p_export.
#args=!"SpNuit2_Seuil90_DataLP_PF_exportTot" : work on the fixed point data grouped in the file SpNuit2_Seuil90_DataLP_PF_exportTot

library(data.table)
#library(rgdal)
#library(raster)
#library(sp)
#library(ggplot2)
#library(MASS)
#library(rgeos)
#pour afficher les milisecondes
op <- options(digits.secs=3)
Proto=F
Protocol="P�destre"
#r�cup�ration des donn�es participation
Particip=fread("C:/Users/Cimc�/Desktop/Chiro/Fichiers_base/p_export.csv")
#r�cup�ration des localit�s
SiteLoc=fread("C:/Users/Cimc�/Desktop/Chiro/Fichiers_base/sites_localites.txt")

path="C:/Users/Cimc�/Desktop/Chiro/Fichiers_base/"
args="SpNuit2_Seuil90_DataLP_PF_exportTot"
#args="ALL"

if(args=="ALL")
{
  SelPar=Particip
  if (Proto) {SelPar=subset(SelPar,SelPar$Protocole==Protocol)}
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

args
if (Proto){fwrite(CoordCPL3,paste0("C:/Users/Cimc�/Desktop/Chiro/Fichiers_obtenus/CoordWGS84_",Protocole,".csv"))
  
}else {fwrite(CoordCPL3,paste0("C:/Users/Cimc�/Desktop/Chiro/Fichiers_obtenus/CoordWGS84_",args,".csv"))}
