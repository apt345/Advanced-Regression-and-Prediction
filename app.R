#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(magrittr)
library(shinythemes)
library(shinyjs)


trainData = read.csv(url("https://raw.githubusercontent.com/apt345/Advanced-Regression-and-Prediction/main/train_def_def.csv"))
trainData=trainData[,-1]

#put categorical variables as factor
trainData$is_island=as.factor(trainData$is_island)
trainData$continent=as.factor(trainData$continent)
trainData$month=as.factor(trainData$month)

variables=colnames(trainData)

myHeader = div(id="advanced",
               useShinyjs(),
               selectInput(
                   inputId = "selected_date",
                   label = "Select one variable",
                   multiple =FALSE,
                   choices = variables,
                   selected = c(variables[1])
               )
)

introduction =tabPanel("Introduction", h1("Introduction"), p("The COVID-19 pandemic was first noticed in December 2019 and since then has propagated all around the world, leading all countries into trouble in both sanitary and economic ways. The evolution and propagation of the virus has been different all around the world since the countries themselves are different and so are the measures taken. The vaccination against the virus has already started but the pandemic has not finished yet and new variants of the virus might arise. Being able to predict when the peaks of the wave will happen accurately could be very useful knowledge against the virus. Furthermore, statistical models that could explain the drivers and quantify the effects of the different factors and countries' characteristics contributing to the expansion of the virus and deaths would be crucial to analyze whether the strategies taken by the different governments in the world have been useful and scientifically identify good choices to take in future waves of this virus or future pandemics that humanity might face. The present work aims to develop statistical models for explaining the main causes leading COVID-19 deaths and analyze its predictive accuracy, although an analysis based on Machine Learning models will later be done to focus on maximum predictive accuracy.
"), h1("Dataset Description"), HTML("
The dataset was taken from the OurWordlInData repository: https://github.com/owid/covid-19-data/tree/master/public/data on the 14th of April 2021. Data were selected only from July 1st 2020 in advance since data from the first wave is not so reliable and the goal of this project is not to focus only on the first wave. In total, the data set contained 47263 observations.

First thing is that in order to account for the seasonality of these events, one needs to work with lag variables. This is why the smoothed (7 day average) variables have been used instead of the daily ones. Furthermore, since this analysis includes different countries, it is important to normalize the data taking into account the total population of the country, either by taking parts per hundred or per million inhabitants. Also, the dataset contained many variables that were very related with each other (like people above 65 and people above 70, clearly all the ones in the second group are inside the first) and only one of those was kept. Finally, features like admissions in ICU were discarded since they had an enormous amount of missing values (well above 50%).

On the other hand, since accounting for individual countries would not generalize well and generate noise, it has been chosen to work only with continents, that together with the month, can also serve as an indication of the season of the year (in case that has an impact on virus propagation). However, first, many countries have been erased to avoid micro-countries as well as full continents. The full list of entities present in the original dataset that have been discarded for this project (even if country variable is not used) is: Africa, Andorra, Anguilla, Antigua and Barbuda, Asia, Bermuda, Cayman Islands, Dominica, Europe, European Union, Faeroe Islands, Falkland Islands, Gibraltar, Grenada, Guernsey, International, Isle of Man, Jersey, Kosovo, Liechtenstein, Marshall Islands, Micronesia, Monaco, Montserrat, North America, Northern Cyprus, Oceania, Saint Helena, Saint Kitts and Nevis, Saint Lucia, Saint Vincent and the Grenadines, Samoa, San Marino, Sao Tome and Principe, Seychelles, Solomon Islands, Somalia, South America, South Sudan, Syria, Timor, Togo, Trinidad and Tobago, Turks and Caicos Islands, Vanuatu, Vatican, World.

Finally, another variable, is island, was added to account for the fact that islands have it easier to close borders and protect themselves from the virus.

The selected variables thus are 

- New deaths smoothed per million: The target variable. Continuous variable representing the average of new deaths over 7 days per million inhabitants of the country.

- New cases smoothed per million: Continuous variable representing the average of new cases over 7 days per million inhabitants of the country.

- People fully vaccinated per hundred: Continuous variable representing the number of people with all the require doses of a given vaccine in that country per hundred inhabitants of the country, so the % of people fully vaccinated.

- Continent: Categorical variable representing the continent with six factors: Africa,
Asia, Europe, North America, Oceania and South America.

- Stringency Index: Continuous variable representing a measure of the level of restrictions a country had endured at that time. It is composite measure based on 9 response indicators including school closures, workplace closures, and travel bans, rescaled to a value from 0 to 100 (100 = strictest response)

- Population Density: Continuous variable representing the population density of the country measured in people per square kilometers.

- Aged 65 or older: Continuous variable representing the percentage of the population aged 65 or older.

- Cardiovascular Death Rate: Continuous variable representing the death rate from cardiovascular disease in 2017 (annual number of deaths per 100,000 people).

- Diabetes Prevalence: Continuous variable representing the diabetes prevalence (% of population aged 20 to 79) in 2017.

- Life Expectancy: Continuous variable representing the life expectancy of the country at birth in 2019.

- Human Development Index: Continuous variable representing the Human Development Index (HDI) of the country which is a composite index measuring average achievement in three basic dimensions of human development-a long and healthy life, knowledge and a decent standard of living. Values for 2019.

- Is island: Categorical variable of two levels accounting for the fact of whether the country is an island or not.

- Month: Categorical variable of 10 levels accounting for the month of the year. Data were selected from July 1st 2020, meaning that May and June are not present.


It is important to note that some of these variables contained missing values. The way this has been fixed is by noting that NA in vaccines and cases appeared mostly when those were 0: very beginning of the pandemic in the world so many countries didn't have any and, in the case of vaccines, not until their development. This two cases were set to 0. Also, missing values on population density, HDI and other country features can be replaced just by a quick search on Google. Finally, the rest of the missing values were imputed using Random Forest imputer of the R package Mice that fits a model of a certain feature containing missing values in terms of the other features in the dataset in order to give an estimation of the missing value.")
             
                       
                       )
    
    dataPanel = tabPanel("Variable description", h1("Variable description"),
                     p("Choose a variable to see its distribution"),
                     plotOutput("plot")
)

correlationpanel = tabPanel("Variable relation to deaths",
                     p("The following plot shows correlations"),
                     plotOutput("correlations")
)


plotlyPanel = tabPanel("Model Results",
                       tableOutput("resultstable")
)

conclusionsPanel=tabPanel("Discussion and Conclusions")
# Define UI for application that draws a histogram
ui = navbarPage("Statistical and Machine Learning Models for COVID-19 deaths analysis",
                    introduction,
                      dataPanel,
                        correlationpanel,
                      plotlyPanel,
                        conclusionsPanel,
                      header = myHeader,
                      theme = shinytheme("superhero"),
                      id = "navBar"
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #function to obtain the given variable in dataframe form
    vaccines_date = reactive({
        a=as.data.frame(trainData %>% pull(input$selected_date))
        colnames(a)=input$selected_date
        return(a)
        })
    #plot the selected variable density
    output$plot = renderPlot(
        ggplot(vaccines_date(), aes_string(x = input$selected_date)) + geom_density(alpha = 0.2))
    #refresh page when moving through sections
    observe({
        shinyjs::show("advanced")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
