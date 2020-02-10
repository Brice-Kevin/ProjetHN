#Web scrapping
library(xml2)
library(rvest)
library(jsonlite)

collecte.Data <- function(n){
  url.fonction <- paste('https://www.populationdata.net/palmares/',n,'/',sep="")
  page <- read_html(url.fonction)
  pays.fonction_html <- html_nodes(page,'td:nth-child(2)')
  pays.fonction <- html_text(pays.fonction_html)
  info.fonction_html <- html_nodes(page,'td:nth-child(4)')
  info.fonction <- as.double(sub(",",".",gsub("[^0-9{,}]","",html_text(info.fonction_html))))
  return(data.frame("Pays"=pays.fonction,"Info"=info.fonction))
}

url <- "https://www.populationdata.net/palmares/esperance-de-vie/"
webpage <- read_html(url)
pays_html <- html_nodes(webpage,'td:nth-child(2)')
pays <- html_text(pays_html)
continent_html <- html_nodes(webpage,'td:nth-child(3)')
continent <- html_text(continent_html)
information.pays <- data.frame("Pays"=pays,"Continent"=continent)

table_name <- c("esperance-de-vie","mortalite-infantile","ipe","mortalite","tourisme",
                "pib-par-habitant","natalite","population")

for(i in table_name)
  information.pays <- merge(information.pays,collecte.Data(i),by.x = "Pays", by.y = "Pays", all = TRUE)
information.pays
rownames(information.pays) <- information.pays$Pays
information.pays$Pays <- NULL
#"Esperance_vie","Mortalite infantile","Indice perf env","Mortalit?"
colnames(information.pays) <- c("Continent","Esperance_vie","Mortalite_inf","Indice_perf_env","Mortalite",
                                "tourisme","pib-par-habitant","Natalite","Superficie")


# utilisation api openweather
url_api  <- serieData("http://api.openweathermap.org/data/2.5/weather?q=Kingston&appid=9ada210033e2363be58a9fac5b682c4f")
api_data <- fromJSON(url_api)
names(site_data)

  
  
  
