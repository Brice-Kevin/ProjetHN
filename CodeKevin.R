install.packages(c("tidyverse","xml2","rvest","jsonlite"))

library(tidyverse)
library(xml2)
library(rvest)
library(jsonlite)

#Les fonctions
##Supprimer les caracteres accentues et espace superflus
delete.accent <- function(x) {
  x <- chartr("éèêëÉÈÊËàÀçÇÎÏîï", "eeeeEEEEaAcCIIii", x)
  x <- gsub("(^\\s+|\\s+$|(?<=\\s)\\s)","",x, perl=T)
  return(x)
}

##Collecter les donnees sur le site (webscapping)
collecte.Data <- function(n){
  url.fonction <- paste('https://www.populationdata.net/palmares/',n,'/',sep="")
  pays.fonction <- str_to_upper(delete.accent(read_html(url.fonction) %>% html_nodes('td:nth-child(2)') %>% html_text()))
  info.fonction <- as.double(sub(",",".",gsub("[^0-9{,}]","",delete.accent(read_html(url.fonction) %>% html_nodes('td:nth-child(4)') %>% html_text()))))
  return(data.frame("Pays"=pays.fonction,"Info"=info.fonction))
}

##Remplacer le nom des pays pour faire concorder les donnees
remplacer.nom.pays <- function(v1,v2,data){
  j <- 1
  for (i in c(1:length(v1))) {
    data[data==v1[i]] <- v2[j]
    j <- j+1
  }
  return (data)
}
#Fin les fonctions

#Creation de la premiere df pour la jointure
url <- "https://www.populationdata.net/palmares/esperance-de-vie/"
pays <- str_to_upper(delete.accent(read_html(url) %>% html_nodes('td:nth-child(2)') %>% html_text()))
continent <- str_to_upper(delete.accent(read_html(url) %>% html_nodes('td:nth-child(3)') %>% html_text()))
information.pays <- data.frame("Continent"=continent)
information.pays$Pays <- pays
#pays <- remplacer.nom.pays(c("SLOVAQUIE"),c("SLOVAQUIE "),pays)

#Nom des pages pour la fonction
table_name <- c("esperance-de-vie","mortalite-infantile","ipe","mortalite","tourisme",
                "pib-par-habitant","natalite","population")

#Application de la fonction de collecte de donnees depuis le site
for(i in table_name)
  information.pays <- merge(information.pays,collecte.Data(i),by.x = "Pays", by.y = "Pays", all = TRUE)

#Creation de la premiere df pour la jointure
colnames(information.pays) <- c("Pays","Continent","Esperance_vie","Mortalite_inf","Indice_perf_env","Mortalite",
                                "tourisme","pib-par-habitant","Natalite","Superficie")

#Creation de la deuxieme df pour la jointure
url <- "https://jeretiens.net/tous-les-pays-du-monde-et-leur-capitale/" 
capitals_2 <- str_to_upper(delete.accent(read_html(url) %>% html_nodes("tr+ tr td:nth-child(2)") %>% html_text()))
pays_2 <- str_to_upper(delete.accent(read_html(url) %>% html_nodes("tr+ tr td:nth-child(1)") %>% html_text()))
v1 <- c("BIELORUSSIE","BIRMANIE","BOSNIE-HERZEGOVINE","GRENADE (ILES DE LA)","ILE MAURICE","ILES COOK","MACEDOINE","MARSHALL (ILES)","REPUBLIQUE TCHEQUE","SAINT-KITTS-ET-NEVIS","SAO TOME ET PRINCIPE",
        "SEYCHELLES","SWAZILAND","TIMOR-ORIENTAL")
v2 <- c("BELARUS (BIELORUSSIE)","MYANMAR (BIRMANIE)","BOSNIE-ET-HERZEGOVINE","GRENADE","MAURICE","COOK","MACEDOINE DU NORD","MARSHALL","TCHEQUIE","SAINT-CHRISTOPHE-ET-NIEVES","SAO TOME-ET-PRINCIPE",
        "SEYCHELLES","ESWATINI (SWAZILAND)","TIMOR ORIENTAL")
pays_2 <- remplacer.nom.pays(v1,v2,pays_2)
collecte.pays.capitals <- data.frame("Pays"=pays_2)
v1 <- c("SAINT JOHN'S","BUENOS-AIRES","SUCRE (OU LA PAZ)","LA HAVANE","ATHENES","KOWEIT","JERUSALEM-EST","SAINT-DOMINGUE","SRI JAYAWARDENAPURA","DOUCHANBE","FANAFUTI")
v2 <- c("SAINT JOHN","BUENOS AIRES","LA PAZ","HAVANA","ATHÈNES","KOWEÏT","JERUSALEM EST","SANTO DOMINGO","KOTTE","DOUCHANBÉ","FUNAFUTI")
capitals_2 <- remplacer.nom.pays(v1,v2,capitals_2)
collecte.pays.capitals$Capitals <- capitals_2

# Jointure entre la dataframe collecte.pays.capitals et information.pays
collecte <- merge(collecte.pays.capitals,information.pays,by.x = "Pays", by.y = "Pays", all = TRUE)
# Nettoyage des pays qu'on va pas utiliser car il ont pas d'information significative 
collecte <- collecte[-c(160,163,166,193,199:256),]
rownames(collecte) <- collecte$Pays
collecte$Pays <- NULL

# utilisation api openweather
api.Data <- function(n){
  #longitude <- latitude <- temperature <- temp_max <- temp_min <- humidity <- temps <- vitesse_vent <- direction_vent <-
  url_api  <- paste("http://api.openweathermap.org/data/2.5/weather?q=",n,"&units=metric&appid=9ada210033e2363be58a9fac5b682c4f&lang=fr",sep="")
  api_data <- fromJSON(url_api)
  
  if(length(api_data$coord$lon) == 0)
    longitude <- NA
  else
    longitude <- api_data$coord$lon
  
  if(length(api_data$coord$lat) == 0)
    latitude <- NA
  else
    latitude <- api_data$coord$lat
  
  if(length(api_data$main$temp) == 0)
    temperature <- NA
  else
    temperature <- api_data$main$temp
  
  if(length(api_data$main$temp_max) == 0)
    temp_max <- NA
  else
    temp_max <- api_data$main$temp_max
  
  if(length(api_data$main$temp_min) == 0)
    temp_min <- NA
  else
    temp_min <- api_data$main$temp_min
  
  if(length(api_data$main$humidity) == 0)
    humidity <- NA
  else
    humidity <- api_data$main$humidity
  
  if(length(api_data$weather$description) == 0)
    temps <- NA
  else
    temps <- api_data$weather$description
  
  Sys.sleep(runif(1,0.75,1.5))
  return(c("Capitals"=n,"longitude"=longitude,"latitude"=latitude,"temp_actu"=temperature,"temp_max"=temp_max,"temp_min"=temp_min,
                    "humidity"=humidity,"type_temps"=temps))
}

#api.Data("ABU DHABI")
collecte.api <- data.frame(matrix(1,1,8))
#Application de la fonction de collecte de donnees depuis le site
for(i in collecte$Capitals[1:194])
  collecte.api <- rbind(collecte.api,api.Data(i))

collecte.api <- collecte.api[-1,]
colnames(collecte.api) <- c("Capitals","Longitude","Latitude","Temp_actu","Temp_max","Temp_min","Humidity","Type_temps")

collecte <- merge(collecte,collecte.api, by.x="Capitals", by.y ="Capitals", all = TRUE)


  
  
