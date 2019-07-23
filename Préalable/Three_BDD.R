##########GOALS/OUTPUTS################
#2 FUNCTIONS

##First of all: LINE
####extraction under a common mold of the data of interest from each participatory program.

###### STOC : Go to the LINE  27

###### SPIPOLL : Go to the LINE  40

###### vIGIE CHIRO : Go to the LINE 56

##In a second step: go to the LINE 90
###obtain the coordinates (longitude, latitude) of the previously sampled points -> Group.1,Group.2
###and the number of participations related to each set of coordinates.-> x

##########INPUTS################
#To make prediction maps, the following data are required: observation date, observation coordinates (longitude, latitude), 
#species/taxa observed and an id of the participations = observation at 1 place, at 1 time. 
#They will be extracted from the following data tables, 
#under a common mold in order to be able to use these data within the required R codes. 

library(data.table)

#################################FIRST STEP : extraction the data of interest############################################

####################
###extraction STOC##
####################

#il manque la date précise...

#coord in WGS84

path="C:/Users/Cimcä/Desktop/Chiro/Fichiers_base/data_FrenchBBS/"
args="data_FrenchBBS"
fread(paste0(path,args,".csv"))
Data_Stoc=data.frame(longitude=Data$longitude_grid_wgs84,latitude=Data$latitude_grid_wgs84,espece=Data$scientific_name)

#######################
###extraction Spipoll##
#######################

#I added the variable scientific_name_taxon_insect by taking the text between "< >" in the column insect_taxon. 
#The taxons_insect column sometimes provides more precise identification but is frequently unfilled or 
#contains information that is not relevant to predicting insect distribution (e. g. cat, snail, plane)
#I add NA in the empty cells on Excell

#issue in BDD : long : >70000 #CHAMP!, big lost of data :20%

#coord in WGS84
path="C:/Users/Cimcä/Desktop/Chiro/Fichiers_base/spipoll_insects_cache_view/"
args="spipoll_insects_cache_view"
Data=fread(paste0(path,args,".csv"))
Data_Spipoll=data.frame(longitude=Data$long,latitude=Data$lat,espece=Data$scientific_name_taxon_insect,date=Data$datedebut,participation=Data$collection_id)
Data_Spipoll$latitude=as.numeric(gsub(",",".",Data_Spipoll$latitude))
Data_Spipoll$longitude=as.numeric(gsub(",",".",Data_Spipoll$longitude))
Data_Spipoll=subset(Data_Spipoll,Data_Spipoll$longitude>0 & Data_Spipoll$latitude>0 & !is.na(Data_Spipoll$espece))

########################
#extraction VigieChiro##
########################

Particip=fread("C:/Users/Cimcä/Desktop/Chiro/Fichiers_base/p_export.csv")
#récupération des localités
SiteLoc=fread("C:/Users/Cimcä/Desktop/Chiro/Fichiers_base/sites_localites.txt")

path="C:/Users/Cimcä/Desktop/Chiro/Fichiers_base/"
args="SpNuit2_Seuil90_DataLP_PF_exportTot"

DataCPL3=fread(paste0(path,args,".csv"))

#liste des coordonnées de participations existantes dans ce jeu de données
#also contains cricket and others 
Data=data.frame(participation=DataCPL3$participation, date=DataCPL3$Nuit, espece=DataCPL3$espece)
ListPar=levels(as.factor(DataCPL3$participation))

#Sélection des données de p_export selon les participations du jeu de données.
SelPar=subset(Particip,Particip$participation %in% ListPar)

#aggrégation des données des sites_localites et des p_export selon les sites
SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
SP=merge(Data,SelParSL, by="participation")
Data_Chiro=data.frame(longitude=SP$longitude,latitude=SP$latitude,espece=SP$espece,date=SP$date.x, participation=SP$participation)





#################################SECOND STEP : aggregate ################################################################

Data=Data_Chiro
#Data=Data_Spipoll
#Data=Data_STOC

#grouping together identical participations
NbPar=aggregate(Data[,c("longitude","latitude")]
                ,by=list(Data_Chiro$participation)
                ,FUN=mean)

#the function "length" calculates the number of sampling forces at these coordinates.
Coord_Data=aggregate(NbPar$Group.1
                    ,by=c(list(NbPar$longitude),list(NbPar$latitude))
                    ,FUN=length)


fwrite(Coord_Data,paste0("C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/CoordWGS84_",args,".csv"))