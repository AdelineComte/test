library(dplyr)
library(RNCEP)
library(MASS)
library(car)
library(rgdal)
library(foreign)
library(maptools)
library(data.table)
library(stringdist)
library(raster)

Capture=fread("C:/Users/Cimcä/Desktop/Chiro/capture/fichier_capt_juillet2019.csv")
donnees_insee=Capture[!(is.na(Capture$INSEE)),]

#######################INSEE
Insee_mqt=Capture[is.na(Capture$INSEE),]
Insee_mqt=Insee_mqt[!(is.na(Insee_mqt$COMMUNE))]
Pourcentageperdu=1-nrow(Insee_mqt)/nrow(Capture) #>4000 données : ni nom COMMUNE ni numéro INSEE

communeshp <- shapefile("C:/Users/Cimcä/Desktop/Chiro/capture/CommunesCentr.shp")
#communeshp$ <- shapefile("C:/Users/Cimcä/Desktop/Chiro/Fichiers SIG/Commune-20190101/communes-20190101.shp")
proj4string(communeshp) <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
communesData <- as.data.frame(communeshp)


#jointure avec insee
donnees_insee1<-data.frame(INSEE_c=as.character(donnees_insee$INSEE))
donnees_insee<-cbind(donnees_insee1,donnees_insee)
Donnees_insee=merge(donnees_insee,communesData, by.x=c("INSEE_c"),by.y=c("INSEE_COM"))

######################COMMUNE

#comparaison de la mise en page des noms de départements
#table(Insee_mqt$COMMUNE)


#correction nom de commune
Insee_mqt$COMMUNE <- gsub("_", "-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" EN ", "-EN-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" ET ", "-ET-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" LE ", "-LE-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" LA ", "-LA-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" LES ", "-LES-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" L'", "-L'", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" AU ", "-AU-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" A ", "-A-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" SUR ", "-SUR-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" DE ", "-DE-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" DES ", "-DES-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" DU ", "-DU-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" D'", "-D'", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("ST ", "SAINT ", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("-ST ", "-SAINT ", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" ST-", " SAINT-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("-ST-", "-SAINT-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("SAINT ", "SAINT-", Insee_mqt$COMMUNE)

Insee_mqt$COMMUNE <- gsub("LES ", "LES8", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LE ", "LE8", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LA ", "LA8", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(" ", "-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LES8", "LES ", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LE8", "LE ", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LA8", "LA ", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("---TREMBLAY", "", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("--", "-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("É", "E", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("Ô", "O", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("È", "E", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("Â", "A", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("Ë", "E", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("Î", "I", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("Ê", "E", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("Ü", "U", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("Û", "U", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("O", "OE", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LE DI-", "LE-DI-", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("-D-", "-D'", Insee_mqt$COMMUNE)

#changements réalises sur le csv :
# Insee_mqt$COMMUNE <- gsub("FORGES-(LES)", "LES FORGES", Insee_mqt$COMMUNE)
# Insee_mqt$COMMUNE <- gsub("VAL-D'AJOL-(LE)", "LE VAL-D'AJOL", Insee_mqt$COMMUNE)
# Insee_mqt$COMMUNE <- gsub("THILLOT-(LE)", "LE THILLOT", Insee_mqt$COMMUNE)
# Insee_mqt$COMMUNE <- gsub("HAUTS-DE-CHEE-(LES)", "LES HAUTS-DE-CHEE", Insee_mqt$COMMUNE)
# Insee_mqt$COMMUNE <- gsub("CROIX-AUX-MINES-" , "LA CROIX-AUX-MINES", Insee_mqt$COMMUNE)
# Insee_mqt$COMMUNE <- gsub("VALTIN-(LE)", "LE VALTIN", Insee_mqt$COMMUNE)
# Insee_mqt$COMMUNE <- gsub("BRESSE-(", "LA BRESSE", Insee_mqt$COMMUNE)

Insee_mqt$COMMUNE <- gsub("LE-BRETHON", "LE BRETHON", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LA-BESSEYRE-SAINTE-MARY","LA BESSEYRE-SAINTE-MARY", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LA-CHAPELAUDE", "LA CHAPELAUDE", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LE-MONASTIER-SUR-GAZEILLE", "LE MONASTIER-SUR-GAZEILLE", Insee_mqt$COMMUNE)

Insee_mqt$COMMUNE <- gsub("ASNIERES-DU-POITOU", "ASNIERES-EN-POITOU", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("VILLAINE-LA-CARELLES" ,"VILLAINES-LA-CARELLE" , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "BAZOUGE-LA-PEROUSE",  "BAZOUGES-LA-PEROUSE", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "BAZOUGES-LA-PEROUZE",  "BAZOUGES-LA-PEROUSE", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("SAINT-ANASTASIE", "SAINTE-ANASTASIE", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "FRAYSSE-SUR-AGOUT",  "FRAISSE-SUR-AGOUT", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(  "AVESNE-SUR-HELPE",   "AVESNES-SUR-HELPE", Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("BRYAS"   , "BRIAS"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("ESCAUPONT"   , "ESCAUTPONT"   , Insee_mqt$COMMUNE)
#Insee_mqt$COMMUNE <- gsub("SAINT-DIE"   , "SAINT-DIE-DES-VOSGES"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("SANT-ANDREA-D'ORCINO"   , "SANT'ANDREA-D'ORCINO"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "PRUNELLI-DI-CASACONE"   , "PRUNELLI-DI-CASACCONI"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("PRUNELLI-DI-FIUMORBU"   , "PRUNELLI-DI-FIUMORBO"  , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "OLMI-CAPELLA"   ,  "OLMI-CAPPELLA"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("SAINT-LOUP-SUR-THOUET"  , "SAINT-LOUP-LAMAIRE"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(  "FAYE-L'ABESSE"   ,   "FAYE-L'ABBESSE"  , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("SAINT-GEORGE-DE-NOISNE"   , "SAINT-GEORGES-DE-NOISNE"  , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("BONHOMME"   , "LE BONHOMME"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("WINTZFELDEN"   , "SOULTZMATT"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("SAINTE-MAIRIE-AUX-MINES"    , "SAINTE-MARIE-AUX-MINES"    , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("BROQUE"    , "LA BROQUE"    , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LA LA BROQUE"    , "LA BROQUE"    , Insee_mqt$COMMUNE)
#Insee_mqt$COMMUNE <- gsub( "BAZOUGES-SOUS-HEDE"    ,  "HEDE-BAZOUGES"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(  "SAINT-GEORGE-D'AUNAY"  ,   "SAINT-GEORGES-D'AUNAY"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "CORRENÇON-EN-VERCORS"    ,  "CORRENCON-EN-VERCORS"    , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "SAINT-BONNET-DE-TRONCAIS"   ,  "SAINT-BONNET-TRONCAIS" , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "CHAMPS-SUR-TARENTAINE"    ,  "CHAMPS-SUR-TARENTAINE-MARCHAL"    , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "LA BESSEYRE-SAINTE-MARY"   ,  "LA BESSEYRE-SAINT-MARY"  , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("BOENAT"    , "LALIZOLLE"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub(  "FRIDEFOND"     ,   "FRIDEFONT"    , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("CHAPELLE-AUX-CHASSES"     , "LA CHAPELLE-AUX-CHASSES"    , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("LAVAULT-SAINT-ANNE"    , "LAVAULT-SAINTE-ANNE"  , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub("DENEUIL-LES-MINES"     , "DENEUILLE-LES-MINES"     , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "CHASTREIX--SANCY"    , "CHASTREIX"    , Insee_mqt$COMMUNE)

Insee_mqt$COMMUNE <- gsub( "MARSEILLE"    , "MARSEILLE--4E--ARRONDISSEMENT"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "-LYON"    , "-888"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "LYON"    , "LYON--2E--ARRONDISSEMENT"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE <- gsub( "-888"    , "-LYON"   , Insee_mqt$COMMUNE)
Insee_mqt$COMMUNE<- iconv(Insee_mqt$COMMUNE)
donnees<-Insee_mqt

#reperer les problemes restants
doublonstest1<-which(duplicated(Insee_mqt$COMMUNE))
List_comm<-Insee_mqt$COMMUNE[-doublonstest1]

L=("")

for (k in 1:length(List_comm)){
  if (!(List_comm[k]  %in% communesData$NOM_COMM)){
    L=rbind(L,List_comm[k])
  }
}
length(L) 

#pas nom de communes :   "SAINT-COLOMBIER"  "FORET-DU-PERTRE"  "TEISSAT" ...
# pb à corriger: nom ville (LE) -> fait sur excel ; changement nom communes, orthographe, accent, arrondissement des grandes villes,


#liste des communes non en doublons

#création d'une fonction qui retourne F ou R pour tous les éléments en plus de 1 exemplaire (et pas seulement à partir du second)
duplicated2 <- function(x){
  if (sum(dup <- duplicated(x))==0)
    return(dup)
  if (class(x) %in% c("data.frame","matrix"))
    duplicated(rbind(x[dup,],x))[-(1:sum(dup))]
  else duplicated(c(x[dup],x))[-(1:sum(dup))]
}

doublonstest<-which(duplicated2(communesData$NOM_COMM))
List_comm_sans_doublon<-communesData$NOM_COMM[-doublonstest]  
List_comm_doublon<-communesData$NOM_COMM[doublonstest]  
sort(List_comm_sans_doublon)
table(List_comm_doublon)

#coord.x1<-communesData$coords.x1[-doublonstest] 
#coord.x2<-communesData$coords.x2[-doublonstest]
#List_comm_sans_doublon<-data.frame(COMMUNE=as.character(List_comm_sans_doublon),coord.x1,coord.x2)

List_comm_sans_doublon<-data.frame(COMMUNE=as.character(List_comm_sans_doublon))
donnees_ss_doublon<-merge(donnees, List_comm_sans_doublon, by = "COMMUNE",ALL=F)
#Donnees_Comm<-donnees_ss_doublon
Donnees_Comm<-merge(donnees_ss_doublon,communesData,by.x="COMMUNE",by.y="NOM_COMM")

List_comm_doublon<-data.frame(COMMUNE=as.character(List_comm_doublon))
donnees_doublon<-subset(donnees, donnees$COMMUNE %in% List_comm_doublon$COMMUNE) 

Lignesperdues=nrow(donnees)-nrow(donnees_doublon)-nrow(donnees_ss_doublon) 
Pourcentageperdu_tot=1-(nrow(Insee_mqt)-Lignesperdues)/nrow(Capture)
Pourcentageperdu=(Lignesperdues)/(nrow(donnees_insee)+nrow(Insee_mqt)) #ss prendre en compte données ss insee et ss commune

#Donnees_Comm=merge(donnees_ss_doublon,communesData, by.x=c("COMMUNE"),by.y=c("NOM_COMM"))
#Donnees_Comm=subset(donnees_ss_doublon,donnees_ss_doublon$COMMUNE %in% communesData$NOM_COMM)

 

######################DEPARTEMENT
#vérification qu'il n'existe pas d'homonymes au sein du même département
communesData$deptcom <- paste(communeshp$NOM_DEPT, communeshp$NOM_COMM, sep = "")
doublonstest2<-which(duplicated(communesData$deptcom)) #n'existe pas

#enlever les données ss numéros et noms de départements
donnees_doublon_dep=subset(donnees_doublon, !(is.na(donnees_doublon$DEPARTEMENT)) | !(is.na(donnees_doublon$DEPARTEMENT2)))
Lignesperdues_dep=nrow(donnees_doublon)-nrow(donnees_doublon_dep) 
Pourcentage_inter=Lignesperdues_dep/nrow(donnees_doublon) #5 pourcent de perte si on éliminait les doublons ss départements (on analysera par région après)


#comparaison de la mise en page des noms de départements
table(Insee_mqt$DEPARTEMENT)
table(communesData$NOM_DEPT) #dans mon cas : aucune différences, pas besoin de modifier les noms
table(Insee_mqt$DEPARTEMENT2)

# donnees$Departement <- gsub(" ", "-", donnees$Departement)
# donnees$Departement <- gsub("_", "-", donnees$Departement)
# donnees$Departement <- gsub("D-ARMOR", "D'ARMOR", donnees$Departement)
# donnees$Departement  <- iconv(donnees$Departement)



donnees$Departement <- toupper(donnees$Departement)

 # coord.x1<-communesData$coords.x1[doublonstest]
 # coord.x2<-communesData$coords.x2[doublonstest]
 # CODE_DEPT<-communesData$CODE_DEPT[doublonstest]
 # NOM_DEPT<-communesData$NOM_DEPT[doublonstest]
 # NOM_REGION<-communesData$NOM_REGION[doublonstest]
 # X_CENTROID<-communesData$X_CENTROID[doublonstest]
 # Y_CENTROID<-communesData$Y_CENTROID[doublonstest]
 # 
 # List_comm_doublon<-data.frame(COMMUNE=as.character(List_comm_doublon),coord.x1,coord.x2,CODE_DEPT,NOM_DEPT,NOM_REGION,X_CENTROID,Y_CENTROID)

#jointure par num de département
donnees_doublon1<-data.frame(Num_dep=as.character(donnees_doublon$DEPARTEMENT2))
donnees_doublon<-cbind(donnees_doublon1,donnees_doublon)
donnees_doublon_depnum=subset(donnees_doublon, !(is.na(donnees_doublon$Num_dep)))
#Donnees_Dep_num=merge(donnees_doublon_depnum,List_comm_doublon, by.x=c("COMMUNE","DEPARTEMENT2"),by.y=c("COMMUNE","CODE_DEPT"))
#Donnees_Dep_num=subset(donnees_doublon_depnum,donnees_doublon_depnum$COMMUNE %in% communesData$NOM_COMM & donnees_doublon_depnum$Num_dep %in% communesData$CODE_DEPT) # un peu de pertes de données..
#Donnees_Dep_num=merge(donnees_doublon_depnum,communesData, by.x=c("COMMUNE","DEPARTEMENT2"),by.y=c("NOM_COMM","CODE_DEPT"))
#perte=nrow(donnees_doublon_depnum)-nrow(Donnees_Dep_num) #environ 500

#observations d'une perte de données : voir jointure par num de départements
#repérer les pb restants
Num_dep2=as.character(donnees_doublon_depnum$Num_dep[1])
for (k in 2:length(donnees_doublon_depnum$Num_dep)){
 
  if (as.integer(donnees_doublon_depnum$DEPARTEMENT2)[k]< 10) {
    Num_dep2=c(Num_dep2,as.character(paste("0",donnees_doublon_depnum$Num_dep[k], sep = "")))
  }
  else {
    Num_dep2=c(Num_dep2,as.character(donnees_doublon_depnum$Num_dep[k]))
  }
}
donnees_doublon_depnum$Num_dep<-Num_dep2

communesData$dept2com <- paste(communeshp$CODE_DEPT, communeshp$NOM_COMM, sep = "")
donnees_doublon_depnum$dept2com <- paste(donnees_doublon_depnum$Num_dep, donnees_doublon_depnum$COMMUNE, sep = "")
doublonstest3<-which(duplicated(donnees_doublon_depnum$dept2com))
List_deptcom<-donnees_doublon_depnum$dept2com[-doublonstest3]

# D=("")
# for (k in 1:length(List_deptcom)){
#   if (!(List_deptcom[k]  %in% communesData$dept2com)){
#     D=rbind(D,List_deptcom[k],k)
#   }
# }
# length(D) #D donne la liste des communes associées à un mauvais numéro de département
# #changement à faire :
# # "61LA POMMERAYE" ??? 14
# # "76CAUMONT" ??? 27

nrow=(1)
for (k in 1:length(donnees_doublon_depnum$Num_dep)){
  if (!(donnees_doublon_depnum$dept2com[k]  %in% communesData$dept2com)){
    nrow=c(nrow,as.integer(k))
  }
}
for (k in nrow){
  if (donnees_doublon_depnum$Num_dep[k]=="61" & donnees_doublon_depnum$COMMUNE[k]=="LA POMMERAYE" ){
    donnees_doublon_depnum$Num_dep[k]="14"
    donnees_doublon_depnum$DEPARTEMENT2[k]="14"}
  if (donnees_doublon_depnum$Num_dep[k]=="76"  & donnees_doublon_depnum$COMMUNE[k]=="CAUMONT" ){
    donnees_doublon_depnum$Num_dep[k]="27"
    donnees_doublon_depnum$DEPARTEMENT2[k]="27"}
}


###refaisons tourner la jointure par num
Donnees_Dep_num=merge(donnees_doublon_depnum,communesData, by.x=c("COMMUNE","Num_dep"),by.y=c("NOM_COMM","CODE_DEPT"))
perte=nrow(donnees_doublon_depnum)-nrow(Donnees_Dep_num) #0



#jointure par nom de département
donnees_doublon_depnom=subset(donnees_doublon,is.na(donnees_doublon$Num_dep) & !(is.na(donnees_doublon$DEPARTEMENT)))

Donnees_Dep_nom=merge(donnees_doublon_depnom,communesData, by.x=c("COMMUNE","DEPARTEMENT"),by.y=c("NOM_COMM","NOM_DEPT"))
#Donnees_Dep_nom=subset(donnees_doublon_depnom,donnees_doublon_depnom$COMMUNE %in% communesData$NOM_COMM & donnees_doublon_depnom$DEPARTEMENT %in% communesData$NOM_DEPT)
perte=nrow(donnees_doublon_depnom)-nrow(Donnees_Dep_nom) #0

######################REGION
#region

#comparaison de la mise en page des noms de régions : attention anciennes et nouvelles régions !!!
table(Insee_mqt$REGION)
table(communesData$NOM_REGION)

#récupération des données ss départements (nom et numéro) mais avec région
donnees_doublon_reg=subset(donnees_doublon, (is.na(donnees_doublon$DEPARTEMENT)) & (is.na(donnees_doublon$DEPARTEMENT2)) &!is.na(donnees_doublon$REGION))

donnees_doublon_reg$REGION #dans mon cas : 1 seule région : N pas de calais (ancienne région comme ds communesData) ??? pas besoin de modification

Lignesperdues_depreg=nrow(donnees_doublon)-nrow(donnees_doublon_dep) -nrow(donnees_doublon_reg)
Pourcentage_inter=Lignesperdues_depreg/nrow(donnees_doublon) #0 pourcent de perte quand on élimine les doublons ss départements ET ss regions

#Donnees_Reg_nom=subset(donnees_doublon_reg,donnees_doublon_reg$COMMUNE %in% communesData$NOM_COMM & donnees_doublon_reg$REGION %in% communesData$NOM_REGION)
Donnees_Reg_nom=merge(donnees_doublon_reg,communesData, by.x=c("COMMUNE","REGION"),by.y=c("NOM_COMM","NOM_REGION"))
perte=nrow(donnees_doublon_reg)-nrow(Donnees_Reg_nom) #0

###############################Récupération des données

TDonnees_Reg_nom=data.frame(date=Donnees_Reg_nom$DATE,heure=Donnees_Reg_nom$HEURE,taxon=Donnees_Reg_nom$TAXON,Group.1=Donnees_Reg_nom$coords.x1,Group.2=Donnees_Reg_nom$coords.x2,X_CENTROID=Donnees_Reg_nom$X_CENTROID,Y_CENTROID=Donnees_Reg_nom$Y_CENTROID)
TDonnees_Dep_nom=data.frame(date=Donnees_Dep_nom$DATE,heure=Donnees_Dep_nom$HEURE,taxon=Donnees_Dep_nom$TAXON,Group.1=Donnees_Dep_nom$coords.x1,Group.2=Donnees_Dep_nom$coords.x2,X_CENTROID=Donnees_Dep_nom$X_CENTROID,Y_CENTROID=Donnees_Dep_nom$Y_CENTROID)
TDonnees_Dep_num=data.frame(date=Donnees_Dep_num$DATE,heure=Donnees_Dep_num$HEURE,taxon=Donnees_Dep_num$TAXON,Group.1=Donnees_Dep_num$coords.x1,Group.2=Donnees_Dep_num$coords.x2,X_CENTROID=Donnees_Dep_num$X_CENTROID,Y_CENTROID=Donnees_Dep_num$Y_CENTROID)
TDonnees_Comm=data.frame(date=Donnees_Comm$DATE,heure=Donnees_Comm$HEURE,taxon=Donnees_Comm$TAXON,Group.1=Donnees_Comm$coords.x1,Group.2=Donnees_Comm$coords.x2,X_CENTROID=Donnees_Comm$X_CENTROID,Y_CENTROID=Donnees_Comm$Y_CENTROID)

Données=rbind(TDonnees_Reg_nom,TDonnees_Dep_nom,TDonnees_Dep_num,TDonnees_Comm)
Données=Données[!(is.na(Données$taxon)),]
Liste_espèces=rep(0,length(table(Données$taxon)))
 #pour avoir tous les taxons
for (k in 1:length(table(Données$taxon)))
 {
Liste_espèces[k]=levels(Données$taxon)[k]
  nom_var=paste0("Tax_",Données$taxon[k])
   var=rep(0,length(Données$taxon))
  for(l in 1:length(Données$taxon)){
  if (Données$taxon[l]==levels(Données$taxon)[k])  { var[l]=1 }}
 Données=data.frame(Données,tax=var)
 print(k)}

#selection of species with an issue of identification by acoustic knowledge.
id_Myotismyotis= which(Liste_espèces=="Myotis myotis")
id_Myotisblythii= which(Liste_espèces=="Myotis blythii")
id_Myotismystacinus= which(Liste_espèces=="Myotis mystacinus")
id_Myotisbrandtii= which(Liste_espèces=="Myotis brandtii")
id_Plecotusaustriacus= which(Liste_espèces=="Plecotus austriacus")
id_Plecotusauritus= which(Liste_espèces=="Plecotus auritus")
id_Plecotusmacrobullaris= which(Liste_espèces=="Plecotus macrobullaris")
table(Données$taxon)
Liste_espèces
#groupe MyoGT : Myotis myotis (Grand Murin) et Myotis blythii
#groupe Myomys : Myotis mystacinus (Murin à moustaches) et brandtii (Murin de Brandt)
#groupe oreillard : Plecotus austriacus (gris), auritus (roux) et macrobullaris (montagnard)


Capture=Données
Capture=data.frame(Capture)
if (length(Capture[1,])==63){
  Capture_sp=data.frame(taxon=Capture$taxon, Group.1=Capture$Group.1,Group.2=Capture$Group.2, Myotismyotis=Capture[,id_Myotismyotis+7],Myotisblythii=Capture[,id_Myotisblythii+7],Myotismystacinus=Capture[,id_Myotismystacinus+7],Myotisbrandtii=Capture[,id_Myotisbrandtii+7],Plecotusaustriacus=Capture[,id_Plecotusaustriacus+7],Plecotusauritus=Capture[,id_Plecotusauritus+7],Plecotusmacrobullaris=Capture[,id_Plecotusmacrobullaris+7])
  
}

if (length(Capture[1,])==64){
  Capture_sp=data.frame(taxon=Capture$taxon, Group.1=Capture$Group.1,Group.2=Capture$Group.2, Myotismyotis=Capture[,id_Myotismyotis+8],Myotisblythiis=Capture[,id_Myotisblythiis+8],Myotismystacinus=Capture[,id_Myotismystacinus+8],Myotisbrandtii=Capture[,id_Myotisbrandtii+8],Plecotusaustriacus=Capture[,id_Plecotusaustriacus+8],Plecotusauritus=Capture[,id_Plecotusauritus+8],Plecotusmacrobullaris=Capture[,id_Plecotusmacrobullaris+8])
}

Capture_sp=subset(Capture_sp,Capture_sp$taxon %in% Liste_espèces[c(id_Myotismyotis ,id_Myotisblythii ,id_Myotismystacinus ,id_Myotisbrandtii ,id_Plecotusaustriacus ,id_Plecotusauritus ,id_Plecotusmacrobullaris )])
table(Capture_sp$taxon) #vérification des espèces sélectionnées

Capture_MyoGT=Capture_sp[,c(1,2,3,4,5)]
Capture_MyoGT=subset(Capture_MyoGT,Capture_MyoGT$taxon %in% c("Myotis myotis","Myotis blythii"))

Capture_Myomys=Capture_sp[,c(1,2,3,6,7)]
Capture_Myomys=subset(Capture_Myomys,Capture_Myomys$taxon %in% c("Myotis mystacinus","Myotis brandtii"))

Capture_Plecotus=Capture_sp[,c(1,2,3,8,9,10)]
Capture_Plecotus=subset(Capture_Plecotus,Capture_Plecotus$taxon %in% c("Plecotus austriacus","Plecotus auritus","Plecotus macrobullaris"))


Capture_MyoGT=aggregate(Capture_MyoGT[,c("Myotismyotis","Myotisblythii")],by=c(list(Capture_MyoGT$Group.1),list(Capture_MyoGT$Group.2)),FUN=sum)
Capture_Myomys=aggregate(Capture_Myomys[,c("Myotismystacinus","Myotisbrandtii")],by=c(list(Capture_Myomys$Group.1),list(Capture_Myomys$Group.2)),FUN=sum)
Capture_Plecotus=aggregate(Capture_Plecotus[,c("Plecotusaustriacus", "Plecotusauritus", "Plecotusmacrobullaris")],by=c(list(Capture_Plecotus$Group.1),list(Capture_Plecotus$Group.2)),FUN=sum)

#Create a new colomn :percentage rate
Capture_MyoGT=data.frame(Capture_MyoGT,Pr_myotis=rep(0,length(Capture_MyoGT$Group.1)))
for (k in 1:length(Capture_MyoGT$Group.1)){
  
    Capture_MyoGT$Pr_myotis[k]= Capture_MyoGT$Myotismyotis[k]/( Capture_MyoGT$Myotismyotis[k]+ Capture_MyoGT$Myotisblythii[k])
}

Capture_Myomys=data.frame(Capture_Myomys,Pr_mystacinus=rep(0,length(Capture_Myomys$Group.1)))
for (k in 1:length(Capture_Myomys$Group.1)){
  Capture_Myomys$Pr_mystacinus[k]= Capture_Myomys$Myotismystacinus[k]/(Capture_Myomys$Myotismystacinus[k]+Capture_Myomys$Myotisbrandtii[k])
}
  
Capture_Plecotus=data.frame(Capture_Plecotus,Pr_macrobullaris=rep(0,length(Capture_Plecotus$Group.1)),Pr_auritus=rep(0,length(Capture_Plecotus$Group.1)),Pr_austriacus=rep(0,length(Capture_Plecotus$Group.1)))
for (k in 1:length(Capture_Plecotus$Group.1)){
  Capture_Plecotus$Pr_macrobullaris[k]= Capture_Plecotus$Plecotusmacrobullaris[k]/(Capture_Plecotus$Plecotusmacrobullaris[k]+Capture_Plecotus$Plecotusauritus[k]+Capture_Plecotus$Plecotusaustriacus[k])
  Capture_Plecotus$Pr_auritus[k]= Capture_Plecotus$Plecotusauritus[k]/(Capture_Plecotus$Plecotusmacrobullaris[k]+Capture_Plecotus$Plecotusauritus[k]+Capture_Plecotus$Plecotusaustriacus[k])
  Capture_Plecotus$Pr_austriacus[k]= Capture_Plecotus$Plecotusaustriacus[k]/(Capture_Plecotus$Plecotusmacrobullaris[k]+Capture_Plecotus$Plecotusauritus[k]+Capture_Plecotus$Plecotusaustriacus[k])
}


#backup

fwrite(Capture_Plecotus,"C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/Capture_Plecotus.csv")
fwrite(Capture_Myomys,"C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/Capture_Myomys.csv")
fwrite(Capture_MyoGT,"C:/Users/Cimcä/Desktop/Chiro/Fichiers_obtenus/Capture_MyoGT.csv")
