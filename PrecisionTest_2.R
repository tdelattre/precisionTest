# ______________________________________________________________________________________ 
# ______________________________________________________________________________________ 
# ______________________________________________________________________________________ 
# |                                                                                    |
# |          SCRIPT WRITTEN BY Raphaël Minguet raphael.minguet@inrae.fr                | 
# |                              ----------------                                      | 
# |                              LICENCE CC-BY-SA                                      | 
# |                              ----------------                                      |
# | This license lets others remix, adapt, and build upon your work even for           |
# | commercial purposes, as long as they credit you and license their new creations    |
# | under the identical terms.                                                         |
# |                                                                                    |
# | The proposed code has a purely academic purpose, is valid under the conditions     |
# | of use of the scientific project for which it was funded and at the date of        |
# | acceptance of the article presenting the code. As with any research work, the      |
# | code is not free of possible errors, approximations, sub-optimisations or          |
# | defects in monitoring dependencies between libraries of the program.               |
# |                                                                                    |
# ______________________________________________________________________________________ 
# |                                                                                    |
# | Cette licence permet à d'autres personnes de remixer, d'adapter et de              |
# | développer ce travail, même à des fins commerciales, à condition qu'elles          |
# | créditent l'auteur et accordent une licence pour leurs nouvelles créations aux     |
# | mêmes conditions.                                                                  |
# |                                                                                    |
# | Le code proposé a une visée purement académique, est valable dans les conditions   |
# | d'utilisation du projet scientifique pour lequel il a été financé et à la date de  |
# | d'acceptation de l'article de présentation du code.                                |
# | Comme tout travail de recherche, le code n'est pas exempt d'éventuelles erreurs,   |
# | approximations, sous-optimisations ou défauts de suivi des dépendances entre       |
# | sous-éléments du programme.                                                        |
# ______________________________________________________________________________________ 
# Objectif du script : 
# revue systématique d'un échantillon d'enregistrements acoustiques dans lesquels birdnet a détecté une espèce donnée
# l'observateur note chaque enregistrement, et le script génère un tableau synthétique
# ces données permettent ensuite d'évaluer la précision de Birdnet pour l'espèce, en fonction de l'indice de confiance
# ______________________________________________________________________________________ 
# --------------------------------------------
# Changelog : 
# V1 = déploiement public initial
# --------------------------------------------

library(plyr)
library(readr)
library(dplyr)
#library(sound)
library(seewave)
library(tuneR)
library(lubridate)
library(stringr)


#source("//nas-avi.paca.inrae.fr/paca-psh-users$/psh/rminguet/Documents/Thèse/Analyse/Script_generaux/Fonction/spectrogramCustomFunction.R")
source("/home/tdelattre/Dropbox/TOM/1-boulot/ecoacoustique/scripts/spectrogramCustomFunction.R")
source("/home/tdelattre/Dropbox/TOM/1-boulot/ecoacoustique/scripts/export_video_2.R")

#définition du lecteur audio 
tuneR::setWavPlayer('vlc.exe --play-and-exit') #windows
tuneR::setWavPlayer('/usr/bin/aplay') #linux
getWavPlayer()

#définition de la localisation pour les conversions de dates
Sys.setlocale("LC_TIME", "C")
#---------------------------------------------------------------------------------------------------------------------------




                                    ################################################################
                                    # 1er étape : charger le fichier des données birdnet compilées #
                                    ################################################################




#Ici fichier de compilation des détections de 2023

#FullDS2024<-read.csv("N:/homes/tdelattre/data/TERRAIN_2024_NAS/PRINTEMPS/compilation_birdnet_S1S8.csv", header=TRUE, sep=";")
FullDS2024<-read.csv("/home/tdelattre/pshnas2/data/TERRAIN_2024_NAS/PRINTEMPS/compilation_birdnet_S1S8.csv", header=TRUE, sep=";")

#mise en forme correcte du fichier
FullDS2024$Confidence <- gsub(",", ".", FullDS2024$Confidence)
FullDS2024$Confidence<-as.numeric(FullDS2024$Confidence)



#créer la colonne date, heure et capteur
FullDS2024$date<-substring(FullDS2024$fileName,10,17)


FullDS2024$recorder=substring(FullDS2024$fileName,1,8)

FullDS2024$time=substring(FullDS2024$fileName,19,24)


FullDS2024$date<-as_date(FullDS2024$date)
                                    ##########################################################
                                    # 2ème étape : définir chemin d'accès pour fichier audio #
                                    ##########################################################



# vérifier le dossier rootdir pour qu'il colle aux chemins d'accès ci-dessous 
# (doit correspondre à l'endroit où sont les sons à écouter, correspondant au fichier Birdnet ci-dessus)
rootdir="N:/tdelattre/data/TERRAIN_2024_NAS/PRINTEMPS/"
rootdir="/home/tdelattre/pshnas2/data/TERRAIN_2024_NAS/PRINTEMPS/"

# ajout du chemin d'accès complet dans le jeu de données en mémoire
FullDS2024$mydir=paste(rootdir,FullDS2024$session,"/",FullDS2024$parcelle,"/Data2/",sep="")
head(FullDS2024$mydir)

# ajout du nom de fichier audio dans le jeu de données en mémoire
FullDS2024$Begin.File=paste(substr(FullDS2024$fileName,start = 0,stop=25),"wav",sep="")
head(FullDS2024$Begin.File)


                                    ######################
                                    # Filtre à effectuer #

# filtrer sur le nom d'espèce, le niveau de confiance

Conf = 0.2

Listen <- FullDS2024 %>% 
  filter(Common.Name == "Petit-duc scops", Confidence > Conf, Confidence <= Conf + 0.1) %>%
  sample_n(30, replace = FALSE)


summary(Listen$Confidence)


                                    #############################################################################################
                                    # Création tableau de stockage + boucle pour annoter les 100 détections tirés aléatoirement #
                                    #############################################################################################

fenetre<-1   #permet d'avoir les abords du chant et pas que les 3 secondes


# Création d'un dataframe pour stocker les réponses
testrougegorge <- data.frame(Enregistrement = numeric(length(Listen$Selection)),
                             Chemin_acces = numeric(length(Listen$Selection)),
                             Begin.Time..s. = numeric(length(Listen$Selection)),
                             End.Time..s. = numeric(length(Listen$Selection)),
                             Ind_confiance = numeric(length(Listen$Selection)),
                             Vrai_Positif = numeric(length(Listen$Selection)))
                             

for (i in 1:length(Listen$Selection)) {
  # Afficher le numéro de l'enregistrement
  print(i)
  # Afficher le chemin d'accès
  print(paste(Listen$mydir[i], Listen$Begin.File[i], sep=""))
  # Afficher l'indice de confiance de chaque enregistrement
  print(paste("Indice de confiance = ", Listen$Confidence[i], sep=""))
  
  # Créer la sélection avec le chemin d'accès pour chaque enregistrement
  selection <- readWave(paste(Listen$mydir[i], Listen$Begin.File[i], sep=""),
                        from=Listen$Begin.Time..s.[i]-fenetre,
                        to=Listen$End.Time..s.[i]+fenetre,
                        units="seconds")
  
  # Création du spectrogramme
  spectroCustom(selection)
  
  # Pause pour laisser le spectrogramme être affiché
  Sys.sleep(4)
  
  # Ecoute de l'enregistrement
  play(selection, getWavPlayer())
  
  # Demander à l'utilisateur si la détection est vraie
  reponse <- as.numeric(readline("La détection est-elle vraie ? (1 pour oui, 0 pour non, 2 si doute, 3 pour réécouter) : "))
  
  # Stocker les informations dans le dataframe des réponses
  testrougegorge[i, "Enregistrement"] <- Listen$Begin.File[i]
  testrougegorge[i, "Chemin_acces"] <- paste(Listen$mydir[i], Listen$Begin.File[i], sep="")
  testrougegorge[i, "Begin.Time..s."] <- Listen$Begin.Time..s.[i]
  testrougegorge[i, "End.Time..s."] <- Listen$End.Time..s.[i]
  testrougegorge[i, "Ind_confiance"] <- Listen$Confidence[i]
  testrougegorge[i, "Vrai_Positif"] <- ifelse(reponse == 1, "oui", ifelse(reponse == 0, "non", ifelse(reponse == 2, "doute", reponse)))

  
  # Réécouter l'enregistrement si la réponse est 3
  while(reponse == 3) {
    play(selection, getWavPlayer())
    reponse <- as.numeric(readline("La détection est-elle vraie ? (1 pour oui, 0 pour non, 2 si doute, 3 pour réécouter) : "))
    testrougegorge[i, "Vrai_Positif"] <- reponse
  }
}; testrougegorge

                        #############################################################################################
                        # Création tableau de stockage + boucle pour annoter les 100 détections tirés aléatoirement #
                        #############################################################################################

write.csv(testrougegorge, 
          paste("/home/tdelattre/pshnas2/data/TERRAIN_2024_NAS/PRINTEMPS/test_validation/Tableau_validation_",
                str_replace_all(unique(Listen$Common.Name), " ", ""),
                round(min(Listen$Confidence),digits = 1),
                "_TD",sep="_"), row.names = FALSE)

#export des meilleurs enregistrements sous forme de sonagrammes vidéo
exportVideo(outputDir = "/home/tdelattre/Dropbox/TOM/1-boulot/ecoacoustique/BestRecordings/",
            timeWindow = 0, #temps avant-après à inclure
            wavTable = Listen, #needs to be a data.frame with columns Begin.File, Begin.Time..s., End.Time..s. as generated by Birdnet
            wavList = c(23), #liste des enregistrements
            suffix = "test",
            beeep = FALSE)