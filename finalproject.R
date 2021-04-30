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
write.csv(train_def, file="C:/Users/arpri/OneDrive/Escritorio/libros/master/4- Cuarto Semicuatrimestre/Regresión Avanzada y Predicción/train_def.csv")

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
write.csv(test_def, file="C:/Users/arpri/OneDrive/Escritorio/libros/master/4- Cuarto Semicuatrimestre/Regresión Avanzada y Predicción/test_def.csv")

#############################################################################################################################################3
####################################################################################################3
#####################################################################################################

# Analysis


trainData = read.csv("C:/Users/arpri/OneDrive/Escritorio/libros/master/4- Cuarto Semicuatrimestre/Regresión Avanzada y Predicción/train_def.csv")
trainData=trainData[,-1]

testData = read.csv("C:/Users/arpri/OneDrive/Escritorio/libros/master/4- Cuarto Semicuatrimestre/Regresión Avanzada y Predicción/test_def.csv")
testData=testData[,-1]

#put categorical variables as factor
trainData$is_island=as.factor(trainData$is_island)
trainData$continent=as.factor(trainData$continent)
trainData$month=as.factor(trainData$month)

testData$is_island=as.factor(testData$is_island)
testData$continent=as.factor(testData$continent)
testData$month=as.factor(testData$month)

# rewrite continent into dummy variables in order to make correlation easier
# it doesnt change anything else because the models work with dummies inside(?)
# crear otro dataset solo para correlaciones y dejar el otro como base(?)
# en el stepAIC importa porque al meterlo en una unica variable categorica no elimina
# uno a uno niveles no significativos, pero si los separamos sí
# leave Europe as base value

#same for months??? Another way to perform correlations??
#better to use ANOVA for cateogorical variables with many levels
# en todo caso esto son correlaciones y no tienen en cuenta efectos no lineales

data_correlations=trainData

data_correlations$continent_Asia="no"
data_correlations$continent_Asia[data_correlations$continent=="Asia"]="yes"
data_correlations$continent_Asia=as.factor(data_correlations$continent_Asia)

data_correlations$continent_Africa="no"
data_correlations$continent_Africa[data_correlations$continent=="Africa"]="yes"
data_correlations$continent_Africa=as.factor(data_correlations$continent_Africa)


data_correlations$continent_Oceania="no"
data_correlations$continent_Oceania[data_correlations$continent=="Oceania"]="yes"
data_correlations$continent_Oceania=as.factor(data_correlations$continent_Oceania)


data_correlations$continent_N_America="no"
data_correlations$continent_N_America[data_correlations$continent=="North America"]="yes"
data_correlations$continent_N_America=as.factor(data_correlations$continent_N_America)


data_correlations$continent_S_America="no"
data_correlations$continent_S_America[data_correlations$continent=="South America"]="yes"
data_correlations$continent_S_America=as.factor(data_correlations$continent_S_America)


data_correlations=subset(data_correlations,select=-c(continent))


## First visualization and simple model/what to expect

#plot each variable versus new deaths

for(i in 1:length(colnames(trainData))){
   if(i!=3){
      plot(x=trainData[,i], y=trainData$new_deaths_smoothed_per_million,
           xlab=colnames(trainData)[i])
   }
}
#plot(dates, y=trainData$new_deaths_smoothed_per_million)








# which are the most correlated variables with new deaths


# Which are the most correlated variables with label?
library(ltm)
library(rcompanion)#cramer v for jet num
y = data_correlations$new_deaths_smoothed_per_million
categorical_variables=c("continent_Asia", "continent_Oceania", "continent_Africa",
                        "continent_S_America", "continent_N_America", 
                        "month", "is_island")
corr_label=matrix(nrow=length(data_correlations),ncol=length(data_correlations))
rownames(corr_label)=colnames(data_correlations)
colnames(corr_label)=colnames(data_correlations)
for(i in(1:(length(data_correlations)))){
   print(colnames(data_correlations)[i])
   if(colnames(data_correlations)[i] %in% categorical_variables){
      corr_label["new_deaths_smoothed_per_million", colnames(data_correlations)[i]]=abs(biserial.cor(y, data_correlations[,i]))#take absolute value
   }else{
      corr_label["new_deaths_smoothed_per_million", colnames(data_correlations)[i]]=abs(cor(y, data_correlations[,i]))#take absolute value
   }
}
corr_label["new_deaths_smoothed_per_million", "new_deaths_smoothed_per_million"]=1#add manually the correlation with its own

corr_label <- sort(corr_label["new_deaths_smoothed_per_million",], decreasing = T)
corr=data.frame(corr_label)
ggplot(corr,aes(x = row.names(corr), y = corr_label)) + 
   geom_bar(stat = "identity", fill = "lightblue") + 
   scale_x_discrete(limits= row.names(corr)) +
   labs(x = "", y = "new_deaths_smoothed_per_million", title = "Correlations") + 
   theme(plot.title = element_text(hjust = 0, size = rel(1.5)),
         axis.text.x = element_text(angle = 45, hjust = 1))








## Model building

ctrl <- trainControl(method = "repeatedcv", 
                     number = 5, repeats = 4)

# Statistical models

#solo con new cases smoothed per million R^2 de 0.55 y con todas las variables, 0.6

## Linear Regression
simple_model=lm(new_deaths_smoothed_per_million~., trainData)
summary(simple_model)

#evaluate its performance
predictions <- predict(simple_model, newdata=testData)
# r squared on the test
cor(testData$new_deaths_smoothed_per_million, predictions)^2
#RMSE
RMSE_simple_model <- sqrt(mean((predictions - testData$new_deaths_smoothed_per_million)^2))
RMSE_simple_model

# AIC (assumes "Normality")
AIC_model=stepAIC(simple_model, trainData)
summary(AIC_model)

#evaluate its performance
predictions <- predict(AIC_model, newdata=testData)
# r squared on the test
cor(testData$new_deaths_smoothed_per_million, predictions)^2
#RMSE
RMSE_AIC_model <- sqrt(mean((predictions - testData$new_deaths_smoothed_per_million)^2))
RMSE_AIC_model


## Backwards Regression


# Forward regression
# number of predictors nvmax as hyperparameter
#in total we have 16-1 variables + 10 levels for month (only need 11 dummies and
#1 is already in the 16-1) + 4 levels for continents + intercept=32
for_tune <- train(new_deaths_smoothed_per_million~., data = trainData, 
                  method = "leapForward", 
                  preProc=c('scale', 'center'),
                  tuneGrid = expand.grid(nvmax = 1:30),
                  trControl = ctrl)
for_tune
plot(for_tune)

## Lasso


## KNN tarda más de una hora con tuneLength=10
# 
# modelLookup('kknn')
# set.seed(10)
# c3 <- makePSOCKcluster(7)
# registerDoParallel(c3)
# knn_model <- train(new_deaths_smoothed_per_million~., data = trainData, 
#                   method = "kknn", 
#                   preProc=c('scale', 'center'),
#                   tuneLength=5,
#                   trControl = ctrl)
# stopCluster(c3)
# knn_model



# Machine Learning models with caret

## RF

# c3 <- makePSOCKcluster(7)
# registerDoParallel(c3)
# rf=train(new_deaths_smoothed_per_million~., method="rf",
#          data = trainData,
#          preProcess = c("center", "scale"),
#          tuneGrid=data.frame(.mtry=c(6,7,8)))
# stopCluster(c3)
# print(rf)

## SVR

## Gradient Boosting


