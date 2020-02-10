#Web scrapping
library(tidyverse)
library(xml2)
library(rvest)
library(jsonlite)

collecte.Data <- function(n){
  url.fonction <- paste('https://www.populationdata.net/palmares/',n,'/',sep="")
  pays.fonction <- str_to_upper(delete.accent(read_html(url.fonction) %>% html_nodes('td:nth-child(2)') %>% html_text()))
  info.fonction <- as.double(sub(",",".",gsub("[^0-9{,}]","",delete.accent(read_html(url.fonction) %>% html_nodes('td:nth-child(4)') %>% html_text()))))
  return(data.frame("Pays"=pays.fonction,"Info"=info.fonction))
}

delete.accent <- function(x) {
  x <- gsub("[`^~\"]", " ", x)
  x <- iconv(x, to="ASCII//TRANSLIT//IGNORE")
  x <- gsub("[`^~\"]", "", x)
  return(x)
}

delete.accent("é ç ï è ê")

url <- "https://www.populationdata.net/palmares/esperance-de-vie/"
pays <- str_to_upper(delete.accent(read_html(url) %>% html_nodes('td:nth-child(2)') %>% html_text()))
continent <- str_to_upper(delete.accent(read_html(url) %>% html_nodes('td:nth-child(3)') %>% html_text()))
information.pays <- data.frame("Pays"=pays,"Continent"=continent)

table_name <- c("esperance-de-vie","mortalite-infantile","ipe","mortalite","tourisme",
                "pib-par-habitant","natalite","population")

for(i in table_name)
  information.pays <- merge(information.pays,collecte.Data(i),by.x = "Pays", by.y = "Pays", all = TRUE)

# récuperation pays et capitals 
url <- "https://jeretiens.net/tous-les-pays-du-monde-et-leur-capitale/" 
pays_2 <- str_to_upper(delete.accent(read_html(url) %>% html_nodes("tr+ tr td:nth-child(1)") %>% html_text()))
capitals_2 <- str_to_upper(delete.accent(read_html(url) %>% html_nodes("tr+ tr td:nth-child(2)") %>% html_text()))
collecte.pays.capitals <- data.frame("Pays"=pays_2,"Capitals"=capitals_2)

# jointure avec collecte data
collecte <- merge(collecte.pays.capitals,information.pays,by.x = "Pays", by.y = "Pays", all = TRUE)

collecte <- collecte[-c(261:271),]

rownames(information.pays) <- information.pays$Pays
information.pays$Pays <- NULL
#"Esperance_vie","Mortalite infantile","Indice perf env","Mortalit?"
colnames(information.pays) <- c("Continent","Esperance_vie","Mortalite_inf","Indice_perf_env","Mortalite",
                                "tourisme","pib-par-habitant","Natalite","Superficie")


# utilisation api openweather
api.Data <- function(n){
  url_api  <- paste("http://api.openweathermap.org/data/2.5/weather?q=",n,"&units=metric&appid=9ada210033e2363be58a9fac5b682c4f&lang=fr",sep="")
  api_data <- fromJSON(url_api)
  longitude <- api_data$coord$lon
  latitude <- api_data$coord$lat
  temps <- api_data$weather$description
  temperature <- api_data$main$temp
  temp_max <- api_data$main$temp_max
  temp_min <- api_data$main$temp_min
  humid <- api_data$main$humidity
  vitesse_vent <- api_data$wind$speed
  direction_vent <- api_data$wind$deg
  rafale_vent <- api_data$wind$gust
  return(data.frame("Ville"=n,"longitude"=longitude,"latitude"=latitude,
                    "type_temps"=temps,"temp_actu"=temperature,
                    "temp_max"=temp_min,"temp_max"=temp_max,humid,
                    "vitesse_vent"=vitesse_vent,"direct_vent"=direction_vent,
                    "raf_vent"=rafale_vent))
}






#names(api_data)

  
  
  
