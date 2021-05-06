### Advanced Regression Project

library(caret)
library(doParallel)
library(tidyverse)
library(MASS)
library(VGAM)
library(e1071) 
library(gridExtra)
library(tictoc)
library(mice)

#####################################################################################3
###################### Preprocessing ########################################


data = read.csv("C:/Users/arpri/OneDrive/Escritorio/libros/master/4- Cuarto Semicuatrimestre/Regresión Avanzada y Predicción/owid-covid-data.csv")

discard_locations=c("Anguilla","Northern Cyprus","Vatican","World","Turks and Caicos Islands"
,"Trinidad and Tobago","Timor","Togo","Saint Kitts and Nevis","Saint Vincent and the Grenadines"
,"San Marino","Saint Helena","Saint Lucia","Vanuatu","Solomon Islands"
,"Seychelles","Samoa","Micronesia","Monaco","Montserrat","Marshall Islands"
,"Liechtenstein","Isle of Man","International","Guernsey","Grenada"
,"European Union","Falkland Islands","Faeroe Islands","Europe","Samoa"
,"Cayman Islands","Antigua and Barbuda","Jersey","Asia","North America"
,"South America","Africa","South Sudan","Syria","Oceania","Somalia"
,"Bermuda","Dominica","Kosovo","Gibraltar","Andorra","Sao Tome and Principe")

data=dplyr::filter(data, !(data$location %in% discard_locations))
#anguilla es una isla de 15000 personas, no interesa
#borrar todos los micro estados, islas aisladas del mundo, continentes enteros y mundo




#completar datos de paises que puedan tener na en gdp, population density,
#median age, aged 65 or older, life expectancy, HDI
data$location[(is.na(data$population_density))]
data$population_density[data$location=="Taiwan"]=656

unique(data$location[(is.na(data$gdp_per_capita))])# cuba, Taiwan, Greenland, 
data$gdp_per_capita[data$location=="Cuba"]=22237
data$gdp_per_capita[data$location=="Greenland"]=37000
data$gdp_per_capita[data$location=="Taiwan"]=54020

data$location[(is.na(data$median_age))]# greenland
data$median_age[data$location=="Greenland"]=34.3

data$location[(is.na(data$aged_65_older))]#greenland, taiwan
data$aged_65_older[data$location=="Greenland"]=10.36
data$aged_65_older[data$location=="Taiwan"]=16

data$location[(is.na(data$life_expectancy))]# no hay

data$location[(is.na(data$human_development_index))]# greenland, taiwan, macao
data$human_development_index[data$location=="Greenland"]=0.786
data$human_development_index[data$location=="Taiwan"]=0.916
data$human_development_index[data$location=="Macao"]=0.914


#nueva variable is_island? las islas tienen mucho mejor control

data$is_island="no"
islands=c("Taiwan", "Macao", "Greenland", "United Kingdom", "Sri Lanka",
          "Singapore", "Philippines", "Papua New Guinea", "New Zealand",
          "Jamaica", "Japan", "Iceland", "Dominican Republic", "Cuba", "Cyprus",
          "Barbados", "Bahamas", "Australia")
data$is_island[data$location %in% islands]="yes"
data$is_island=as.factor(data$is_island)

#Erase Country, ISO code, location but leave continet because its a factor with not too
#many levels that can account for geographical features and different seasons of the year in the same date, important.
#Predict weekly icu admissions per million
#smoothed son las medias a 7 dias, usarlas para captar mejor el efecto temporal
#como estamos comparando paises, usar solo magnitudes normalizadas.
#hospital beds per thousand esta logicamente relacionada con admisiones al hospital, quitarla
#aged 70 older contenido en 65 older así como median age
#population es mejor population density
#como quiero la evolucion, quitar los cumulativos


#Diferencia entre total vaccinated, people vaccinated etc???? Dejar solo una de las 
#tres vacunas totales (o la parcial y el total)
#total vaccinations, numero total de vacunas--> No tan interesante como las otras dos
#people vaccinated: al menos una dosis-->reduce riesgo uci?? Interesante
#people fully vaccinated: inmunes se supone, interesante
#quitar pacientes de uci pues queremos prededir pacientes a las siete semanas
# todos los weekly uci son NA... quitarlas y predecir otra cosa
#volver a poner entonces icu_patients_per million, no porque hay 70000 NAs
#tests per casees lo mismo que positive rate mas o menos
#extreme poverty es similar a gdp per capita y tiene 30000 NAs, quitarla
#predecir muertes
#quitar new test per thousand
#quitar todos los que tengan muchos NA
#weekly hosp admissions per million y hosp_patients_per_million tiene 80000 NAs
# las vacunas tambien pero esta claro que son 0 cuando hay NA, ponerlas a 0
#quitar new vaccinations smoothed porque eso ya va en el total de vacunas

data = subset(data, select = -c(iso_code, location, total_cases, total_cases_per_million, new_cases,
      new_cases_smoothed, total_deaths, total_deaths_per_million, new_deaths, new_deaths_smoothed,
      new_cases_per_million, new_deaths_per_million, icu_patients, 
      hosp_patients,weekly_icu_admissions, weekly_icu_admissions_per_million, weekly_hosp_admissions, new_tests,
      total_tests, total_tests_per_thousand, new_tests_smoothed, tests_units,
      total_vaccinations, people_vaccinated, people_fully_vaccinated,
      new_vaccinations, new_vaccinations_smoothed, hospital_beds_per_thousand,
      aged_70_older, population, total_vaccinations_per_hundred, new_tests_per_thousand,
      icu_patients_per_million, tests_per_case, extreme_poverty,
      weekly_hosp_admissions_per_million, hosp_patients_per_million,
      new_vaccinations_smoothed_per_million, positive_rate, female_smokers,
      male_smokers, new_tests_smoothed_per_thousand, reproduction_rate,
      handwashing_facilities, median_age, people_vaccinated_per_hundred,
      gdp_per_capita))

#data from July 1st is more reliable
data=data[data$date>"2020-07-01",]
#Leave +65, median age, and GDP as continuous (O AGRUPARLO??).
#escalar variables para que el algoritmo no se descontrole con numeros grandes.

#revisar que esto no este metiendo NA como 0 donde no debería
#data$people_vaccinated_per_hundred[is.na(data$people_vaccinated_per_hundred)]=0
data$people_fully_vaccinated_per_hundred[is.na(data$people_fully_vaccinated_per_hundred)]=0

#Cómo codificar el tiempo??? meses y factores

month_fun=function(x){
   return(strsplit(x=x, split="-")[[1]][2])
}
data$month=lapply(X=data$date, FUN = month_fun)
data$month=as.numeric(data$month)
data$month=as.factor(data$month)

data$continent=as.factor(data$continent)

dates=as.factor(data$date)
data=subset(data, select=-c(date))




# Create the training and test datasets
set.seed(100)
# Step 1: Get row numbers for the training data
# put the name of a column with no missing values
trainRowNumbers = createDataPartition(data$people_fully_vaccinated_per_hundred, p = 0.9, list = FALSE)
# Step 2: Create the training dataset
trainData = data[trainRowNumbers, ]
# Step 3: Create the test dataset
testData = data[-trainRowNumbers, ]

rm(trainRowNumbers)


#una vez esta todo codificado, replace missing values with random forest
# para train y test por separado para evitar data leakage
#casos, muertes en fechas sin datos. Stringency index solo en algunas fechas
#diabetes, riesgo cardiovascular 

#filtrar fechas iniciales sobre todo para new cases smoothed y new deaths??

set.seed(10)
c3 <- makePSOCKcluster(7)
registerDoParallel(c3)
tic()
train_def <- mice(trainData,m=1,method="rf", printFlag =TRUE)
train_def <- complete(train_def)
toc()
stopCluster(c3)

#take care of negative values (very few ones)

train_def=train_def[train_def$new_cases_smoothed_per_million>=0,]
train_def=train_def[train_def$new_deaths_smoothed_per_million>=0,]
#save it into a file
write.csv(train_def, file="C:/Users/arpri/OneDrive/Escritorio/libros/master/4- Cuarto Semicuatrimestre/Regresión Avanzada y Predicción/train_def_def.csv")

set.seed(10)
c3 <- makePSOCKcluster(7)
registerDoParallel(c3)
tic()
test_def <- mice(testData,m=1,method="rf", printFlag =TRUE)
test_def <- complete(test_def)
toc()
stopCluster(c3)

#take care of negative values (very few ones)

test_def=test_def[test_def$new_cases_smoothed_per_million>=0,]
test_def=test_def[test_def$new_deaths_smoothed_per_million>=0,]

#save it into a file
write.csv(test_def, file="C:/Users/arpri/OneDrive/Escritorio/libros/master/4- Cuarto Semicuatrimestre/Regresión Avanzada y Predicción/test_def_def.csv")
