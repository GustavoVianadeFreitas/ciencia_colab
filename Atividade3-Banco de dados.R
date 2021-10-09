############## Atividade 3
require(tidyverse)

### Extrair dados do GBIF
install.packages("rgbif")
require(rgbif)

# checar funcoes
?occ_data

# baixar ocorrencias
alcantarea_gbif <- occ_data(scientificName = "Alcantarea imperialis", 
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

alcantarea_gbif 

# dimensoes
dim(alcantarea_gbif )
dim(alcantarea_gbif$data)

# checar campos
alcantarea_gbif$data %>% names


### Problemas reportados
gbif_issues()

issues_gbif() %>% alcantarea_gbif$data$issues %>%
  unique() %>% 
  strsplit(.,"[,]") %>%
  unlist()

issues_gbif<- alcantarea_gbif$data$issues %>%
  #unique() %>% 
  strsplit(.,"[,]") %>%
  unlist() %>%
  unique()
### Gerou e separou v?rios issues

## Aqui ? para checar que issues s?o esses para tirar essas coisas
## Ver quais problemas foram baixados no dataset da esp?cie em quest?o.
gbif_issues() %>% head %>%
  data.frame() %>% 
  filter(code %in% issues_gbif)

gbif_issues() %>%
  data.frame() %>% 
  filter(code %in% issues_gbif)

## Campos de Interesse
View(alcantarea_gbif)
alcantarea_gbif1 <- alcantarea_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, country) 

alcantarea_gbif1 <- alcantarea_gbif1 %>% 
  distinct() 


# checar niveis dos fatores
lapply(alcantarea_gbif1, unique)


### Problemas n?o reportados
# investigar niveis suspeitos
## Talvez aqui n?o se aplicaria porque a informa??o WaterBody n?o tem para Alcantarea
alcantarea_gbif1 %>% 
  distinct(country) %>% 
  pull()

# S? tem no Brasil mesmo... aqui no caso da Dory, tinha pa?s de ocorrencia que n?o tem a spp
alcantarea_gbif1 %>%
  group_by(country) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=country)) +
  geom_bar(stat = 'identity') 

### OBS: deu uma barra grande pq só tem o Brazil mesmo.


# fonte das regioes erradas
# Nao se aplicaria aqui
alcantarea_gbif1 %>% 
  filter(country %in% c("Brazil")) %>% 
  distinct(datasetName)


# 25 ocorrencias
alcantarea_gbif1 %>% 
  filter(datasetName %in% c("NA"))
# filtrar todas do dataset suspeito
alcantarea_gbif_ok <- alcantarea_gbif1 %>% 
  filter(!datasetName %in% c("NA"))

install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
library(ggmap)
library(maps)
library(mapdata)


World<-map_data('world')
Brazil <- map_data('world', region='Brazil')
# checar pontos

ggplot() +
  geom_polygon(data = Brazil, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = alcantarea_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "Longitude", y = "Latitude", title = expression(italic("Alcantarea imperialis")))


# checar profundidade - Não se aplicou no meu pq não tenho uma variável contínua.
# A maioria está com NAs como elevação
alcantarea_gbif_ok %>% 
  ggplot(aes(x = , fill = country)) +
  geom_histogram() 


### OBIS
## OBIS
### Aqui n?o se aplicaria muito porque o OBIS ? mais para a parte marinha
install.packages("robis")
remotes::install_github("iobis/robis")
library(robis)


alcantarea_obis <- robis::occurrence("Alcantarea imperialis")

# checar dados
names(alcantarea_obis)

alcantarea_obis1 <- alcantarea_obis %>% 
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, country) %>% 
  distinct()

# check problemas reportados (flags)
alcantarea_obis %>% 
  distinct(flags)


# check NA em datasetName
alcantarea_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         is.na(datasetName)) %>% 
  distinct(waterBody)

# depth ok
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  ggplot(aes(x = depth, fill = waterBody)) +
  geom_histogram() 


# checar niveis
dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique")) %>% 
  lapply(., unique)

# ok
dori_obis_ok <- dori_obis1 %>% 
  filter(!flags %in% c("no_depth,on_land", "on_land", "on_land,depth_exceeds_bath", "depth_exceeds_bath,on_land"),
         !is.na(datasetName),
         !waterBody %in% c("North America", "North America Atlantic", "atlantique", NA)) 


# check
ggplot() +
  geom_polygon(data = Brazil, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = alcantarea_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "Longitude", y = "Latitude", title = expression(italic("Alcantarea imperialis")))


# unir GBIF e OBIS

# ver diferencas
setdiff(names(dori_gbif_ok), names(dori_obis_ok))
setdiff(names(dori_obis_ok), names(dori_gbif_ok))

#all_data <- bind_rows(dori_gbif_ok %>% 
 #                       mutate(repo = paste0("gbif", row.names(.))), 
  #                    dori_obis_ok %>% 
   #                     mutate(repo = paste0("obis", row.names(.)))) %>%
  #column_to_rownames("repo") %>% 
  #dplyr::select(decimalLongitude, decimalLatitude, depth) %>% 
  #distinct() %>% 
  #rownames_to_column("occ") %>% 
  #separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  #mutate(scientificName = "Paracanthurus hepatus") %>% 
  #dplyr::select(-rn)

#### N?o juntei o gbif com o obis porque obis ? mais para a parte marinha
#### mas deu para fazer o script somente com os dados do gbif
all_data <- bind_rows(alcantarea_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                occurrenceStatus, country) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Alcantarea imperialis") %>% 
  dplyr::select(-rn)

# mapear ocorrencias
ggplot() +
  geom_polygon(data = Brazil, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = alcantarea_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "Longitude", y = "Latitude", title = expression(italic("Alcantarea imperialis")))


write.csv(all_data, "Alcantarea_imperialis_info_gbif2.csv", row.names = FALSE)

### Classifica??o autom?tica de pontos
## Script da fun??o caseira que est? abaixo
source('functions/aux_functions.R')

# funcao para classificar ocorrencias suspeitas
flag_outlier <- function(df, species){
  
  # funcao para classificar ocorrencias suspeitas
  # baseada no calculo do centroide de todas ocorrencias
  # indica como 'check' as ocorrencias que tem distancias at? o centroide
  # acima do 90th quantil (default) das distancias calculadas
  
  dados <- df %>% 
    dplyr::filter(scientificName == species); 
  
  dados2 <- geosphere::distVincentyEllipsoid(
    dados %>%
      summarise(centr_lon = median(decimalLongitude),
                centr_lat = median(decimalLatitude)),
    dados %>% 
      dplyr::select(decimalLongitude, decimalLatitude)
  ) %>% 
    bind_cols(dados) %>% 
    rename(dist_centroid = '...1') %>% 
    mutate(flag = ifelse(dist_centroid < quantile(dist_centroid, probs = 0.9), "OK",
                         ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.90) & dist_centroid < quantile(dist_centroid, probs = 0.95), "check > Q90",
                                ifelse(dist_centroid >= quantile(dist_centroid, probs = 0.95), "check > Q95", "OK"))))
  
  # mutate(flag = ifelse(dist_centroid > quantile(dist_centroid, probs = prob), "check", "OK"))
  
  print(dados2)
  
}


#Distâncias acima do 90° percentil, será considerado como dados suspeitos


# classificar ocorr?ncias
marcados <- alcantarea_gbif$data %>% 
  data.frame() %>% 
  dplyr::select(scientificName, decimalLongitude, decimalLatitude, datasetName) %>% 
  distinct() %>% 
  flag_outlier(., "Alcantarea imperialis (Carri?re) Harms")


# mapa
ggplot() +
  geom_polygon(data = Brazil, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados, 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = flag)) +
  theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", 
       title = expression(italic("Alcantarea imperialis")))


### Duas ocorrências estão suspeitas e de fato, pelo conhecimento da spp, a que está mais
### suspeita é o ponto marcado na região centro-oeste/norte.
### O outro ponto é o rosa, que se encontra no sul. 
### A espécie tem distribuição restrita a RJ/ES/SP


install.packages("scrubr")
remotes::install_github("ropensci/scrubr")
library(scrubr)

# usando os dados com flag
data_scrubr <- marcados %>% 
  dframe() %>% 
  coord_impossible() %>% 
  coord_incomplete() %>% 
  coord_unlikely() %>% 
  dedup()


# mapa
ggplot() +
  geom_polygon(data = Brazil, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = data_scrubr, 
             aes(x = decimalLongitude, y = decimalLatitude), 
             color = "red") +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude), 
             color = "blue", shape = 3) +
  theme(legend.title = element_blank()) +
  labs(x = "Longitude", y = "Latitude", 
       title = expression(italic("Alcantarea imperialis")))

### Duas ocorr?ncias suspeitas marcadas com uma cruz e retirou as duplicadas


install.packages("obistools")
devtools::install_github("iobis/obistools")
library(obistools)

# dori_obis %>% 
#   dplyr::select(decimalLongitude, decimalLatitude, scientificNameID) %>% 
#   distinct() %>% 
#   check_outliers_species(., report=TRUE)


# usando essa configura??o chegamos a valores pr?ximos aos da limpeza manual
alcantarea_gbif %>% 
  dplyr::select(decimalLongitude, decimalLatitude, scientificNameID) %>% 
  distinct() %>% 
  check_outliers_dataset(., report = FALSE, iqr_coef = 1, mad_coef = 5) %>% 
  dim()

#### Aqui nesse ggplot embaixo plotou-se apenas as duas ocorr?ncias suspeitas
ggplot() +
  geom_polygon(data = Brazil, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = marcados %>% 
               filter(flag != "OK"), 
             aes(x = decimalLongitude, y = decimalLatitude, 
                 color = datasetName)) +
  theme(legend.title = element_blank()) +
  labs(x = "Longitude", y = "Latitude", 
       title = expression(italic("Alcantarea imperialis")))


### Pacote CoordinateCleaner
### Vai checar algumas coordenadas e comparar com as coordenadas do gbif

install.packages("CoordinateCleaner")
library(CoordinateCleaner)

flags <-
  clean_coordinates(
    x = marcados,
    lon = "decimalLongitude",
    lat = "decimalLatitude",
    species = "scientificName",
    tests = c("equal", "gbif",
              "zeros", "seas")
  )

### Acabou não achando problemas também como o professor explicou.


