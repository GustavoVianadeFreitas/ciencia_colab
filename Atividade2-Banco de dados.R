##### Atividade 2 - Ferramentas em Ci?ncia Colaborativa e Banco de Dados

### Gerar tr?s planilhas finais pelo sistema Darwin Core
require(tidyverse)
install.packages("taxize")
require(taxize)
### Esse pacote taxize acessa um banco de dados taxonomicos

#### Importaçãoo dos Conjuntos de Dados feitos na atividade 1 da disciplina
#### A ideia era criar a planilha iris_mod a partir de cada conjunto de dados
iris_gustavo<-read.csv("atividade1_GUSTAVO_VIANA.csv", header=T)
iris_gabriel<-read.csv("atividade1_GABRIEL-DEPIANTTI.csv", header=T)
iris_marcos<-read.csv("atividade1_marcosdelucena.csv", header=T)
iris_carolina<-read.csv("atividade1_CAROLINA-OLIVEIRA-VALE.csv", header=T)
iris_vanessa<-read.csv("atividade1_Vanessa_Xavier.csv", header=T)
iris_nilson<-read.csv("atividade1_NILSON-BERRIEL.csv", header=T)
iris_marina<-read.csv("atividade1_marinasissini.csv", header=T)
iris_isabella<-read.csv("atividade1_ISABELLA-FERREIRA.csv", header=T)
iris_carlos<-read.csv("atividade1_carlos_filgueira.csv", header=T)

## com dados prontos
iris<-read.csv("iris_mod.csv", header=T)
View(iris)
lapply(iris, unique)

### Dados dos participantes
setwd("output/atividade1/")
getwd()
mydir<-"~/3. DOUTORADO/3 - Disciplinas 2021/Ferramentas em Ci?ncia Colaborativa e Banco de Dados"

files21<-list.files(pattern="*.csv", path=mydir, recursive = T) #include all subfolders
temp<-list.files(pattern="*.csv", path=mydir) 

dados21<- plyr::adply(files21, 1, read.csv, header=T)

read_csv(paste0(temp[2]))
## S? leu quando eu tirei o diret?rio
### A? vai trocando os n?meros conforme os nomes dados pela fun??o anterior 

##loop
for(i in c(1, 2, 5:8)){
  assign(paste0("iris", i),
         read.csv(paste0(temp[i]), sep=";", dec="."))
}


### A ideia aqui era ver todos aqui no R e ir modificando por aqui, da forma que cada
### pessoa fez na atividade 1




# tags21 <-cbind(headers1[c(11,)],
#                headers1[c(10), 3])

# names(tags21) <- c("X1, "x, "sensor", "serial)


# check taxa
species <- iris %>% 
  distinct(Species) %>% 
  pull() %>% 
  get_tsn() %>% 
  data.frame() %>% 
  bind_cols(iris %>% 
              distinct(Species))


### Manipulando os dados
iris_1 <- iris %>% 
  dplyr::mutate(eventID = paste(site, date, sep = "_"), # create indexing fields 
                occurrenceID = paste(site, date, amostra, sep = "_")) %>% 
  left_join(species %>% 
              select(Species, uri)) %>% # add species unique identifier
  dplyr::rename(decimalLongitude = lon, # rename fields according to DwC 
                decimalLatitude = lat,
                eventDate = date,
                scientificName = Species,
                scientificNameID = uri) %>% 
  mutate(geodeticDatum = "WGS84", # and add complementary fields
         verbatimCoordinateSystem = "decimal degrees",
         georeferenceProtocol = "Random coordinates obtained from Google Earth",
         locality = "Gaspe Peninsula",
         recordedBy = "Edgar Anderson",
         taxonRank = "Species",
         organismQuantityType = "individuals",
         basisOfRecord = "Human observation")
iris_1

####
### Fazer as tr?s planilhas, precisa fazer os script necess?rios

## create eventCore
eventCore <- iris_1 %>% 
  select(eventID, eventDate, decimalLongitude, decimalLatitude, locality, site,
         geodeticDatum, verbatimCoordinateSystem, georeferenceProtocol) %>% 
  distinct()
### Essa sele??o ? baseada no que o eventCore tem que ter no sistema DarwinCore


## create occurrence
occurrences <- iris_1 %>% 
  select(eventID, occurrenceID, scientificName, scientificNameID,
         recordedBy, taxonRank, organismQuantityType, basisOfRecord) %>%
  distinct()

## A sele??o ? com o eventID e ocurrencieID e outros dados das esp?cies


## create measurementsOrFacts - dados estendidos
eMOF <- iris_1 %>% 
  select(eventID, occurrenceID, recordedBy, Sepal.Length:Petal.Width) %>%  
  pivot_longer(cols = Sepal.Length:Petal.Width,
               names_to = "measurementType",
               values_to = "measurementValue") %>% 
  mutate(measurementUnit = "cm",
         measurementType = plyr::mapvalues(measurementType,
                                           from = c("Sepal.Length", "Sepal.Width", "Petal.Width", "Petal.Length"), 
                                           to = c("sepal length", "sepal width", "petal width", "petal length")))

##pivot_longer ? para colocar no formato longo 


## Controle de Qualidade

# check if all eventID matches
setdiff(eventCore$eventID, occurrences$eventID)
setdiff(eventCore$eventID, eMOF$eventID)
setdiff(occurrences$eventID, eMOF$eventID)

## Checando se tem NA
eMOF %>%
  filter(is.na(eventID))

occurrences %>%
  filter(is.na(eventID))

### Salvar os dados

##### Escrevendo matrizes como arquivo de texto
rm(list = setdiff(ls(), c("eventCore", "occurrences", "eMOF")))

files <- list(eventCore, occurrences, eMOF) 
data_names <- c("DF_eventCore","DF_occ","DF_eMOF")
dir.create("Dwc_Files")


for(i in 1:length(files)) {
  path <- paste0(getwd(), "/", "DwC_Files")
  write.csv(files[[i]], paste0(path, "/", data_names[i], ".csv"))
}

### Cria??o das tr?s planilhas fora do R
write.csv(eMOF, "eMOF.csv", row.names=FALSE)




