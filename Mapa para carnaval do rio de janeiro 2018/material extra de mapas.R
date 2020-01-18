library(readxl)
base <- read_excel("Agenda_BL_Rua_Carnaval_Rio-2018_Imprensa.xlsx")  #baixado em : http://www.radiosaara.com.br/noticias/carnaval-2018-lista-completa-blocos/

source("rm_accent.R")

ajustar_nomes=function(x){
  x%>%
    stringr::str_trim() %>%                        #Remove espaços em branco sobrando
    stringr::str_to_lower() %>%                    #Converte todas as strings para minusculo
    rm_accent() %>%                                #Remove os acentos com a funcao criada acima
    stringr::str_replace_all("[/' '.()]", "_") %>% #Substitui os caracteres especiais por "_"
    stringr::str_replace_all("_+", "_") %>%        #Substitui os caracteres especiais por "_"   
    stringr::str_replace("_$", "")                 #Substitui o caracter especiais por "_"
}

names(base)=ajustar_nomes(names(base))

base$local_da_concentracao=base$local_da_concentracao%>%
  stringr::str_trim() %>%                        #Remove espaços em branco sobrando
  stringr::str_to_lower() %>%                    #Converte todas as strings para minusculo
  rm_accent() %>%                                #Remove os acentos com a funcao criada acima
  stringr::str_replace_all("[/' '.()]", " ") %>% #Substitui os caracteres especiais por "_"
  stringr::str_replace_all("_+", " ") %>%        #Substitui os caracteres especiais por "_"   
  stringr::str_replace("_$", " ")%>%                 #Substitui o caracter especiais por "_"
  stringr::str_replace(", esquina.*","")%>%
  stringr::str_replace("n[º°].*","")%>%
  stringr::str_replace("em frente a.*","")%>%
  stringr::str_replace_all("[0-9]","")%>%
  stringr::str_replace_all("[:punct:]","")%>%
  stringr::str_c(" rio de janeiro")
  

  
for(i in 1:nrow(base)){
base$label[i]=str_c("Bloco: ",base$bloco[i]," - ","Bairro: ",base$bairro[i]," - ","Regiao: ", base$regiao, "Data: ", base$data[i], " - ", "Concentração", str_sub(base$concentracao[i], start = 11)  )
}
  
  
library(ggmap)

# Loop through the addresses to get the latitude and longitude of each address and add it to the
# base data frame in new columns lat and lon
for(i in 1:nrow(base)){
  # Print("Working...")
  result <- geocode(base$local_da_concentracao[i], output = "latlona", source = "google")
  base$lon[i] <- as.numeric(result[1])
  base$lat[i] <- as.numeric(result[2])
  #base$geoAddress[i] <- as.character(result[3])
}

library(dplyr)
library(leaflet)
# base %>%

base%>%
  filter(lat<(-7)&lat<(-22)&lon>(-57)&lon<(-43.1)& bloco!="Clubinho do Samba")%>%
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = ~lon, lat = ~lat,popup=~label,
             clusterOptions = markerClusterOptions())


#  ------------------------------------------------------------------------




coordinates(ll)<- ~lon+lat # convert to SPDF



proj4string(ll) <- CRS('+proj=longlat +datum=WGS84')


library(plotGoogleMaps) 
#http://www2.uaem.mx/r-mirror/web/packages/plotGoogleMaps/vignettes/plotGoogleMaps-intro.pdf
m<-bubbleGoogleMaps(ll,zcol="x", max.radius = 80,
                    filename='myMap3.htm') 



library(dplyr)
## 
## Attaching package: 'dplyr'
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
dados <- read_excel("~/@Mapas/Mapas municipio/latlong.xls")
library(leaflet)
dados %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = ~LONGITUDE, lat = ~LATITUDE,popup=~MUNICIPIO,
             clusterOptions = markerClusterOptions())




#  ------------------------------------------------------------------------




#Fazendo a leitura das bases de dados:
dadjov <- as.data.frame(readxl::read_excel("Prrenchimento_sexo_idade_ign_por_mun.xlsx"))
#Dados de latitude e longitude
latlong <- read_excel("latlong.xls")
#Removendo os tracos "-" e "'" :
dadjov$NOME=str_replace_all(dadjov$NOME, "[-]", " ") 

base1=dadjov[,c("NOME","FREQ BASE PFA", "FREQ_IGN")]
base2=latlong[,c("LATITUDE", "LONGITUDE", "MUNICIPIO", "UF")]
names(base2)[3]="NOME"

#Criando a base de dados que sera utilizada:
base=left_join(base1, base2, by="NOME")

#Por fim, a base de dados utilizada sera:
head(base)

library(dplyr)
library(leaflet)
base %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = ~lon, lat = ~lat,popup=~bloco,
             clusterOptions = markerClusterOptions())

base$local_da_concentracao[1]
library(stringr)

#  ------------------------------------------------------------------------

base2=base%>%
  na.omit()%>%
  select(LATITUDE,LONGITUDE,FREQ_IGN)
base2=as.data.frame(base2)
str(base2)
base2$FREQ_IGN=as.numeric(base2$FREQ_IGN)

coordinates(base2)<- ~LONGITUDE+LATITUDE # convert to SPDF
proj4string(base2) <- CRS('+proj=longlat +datum=WGS84')
library(plotGoogleMaps) 
#http://www2.uaem.mx/r-mirror/web/packages/plotGoogleMaps/vignettes/plotGoogleMaps-intro.pdf


m<-bubbleGoogleMaps(base2,zcol="FREQ_IGN",key.entries = quantile(base2@data$FREQ_IGN, (1:10)/10),, max.radius = 80,
                    filename='myMap3.htm') 



plotGoogleMaps(base2,
               zcol="FREQ_IGN",
               filename='MyMap6.htm',
               mapTypeId='TERRAIN',
               colPalette= brewer.pal(7,"Reds"),
               strokeColor="white")
