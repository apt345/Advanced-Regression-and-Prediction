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
library(markdown)

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

introduction =tabPanel("Introduction", h1("Introduction"), p(align="justify", "The COVID-19 pandemic was first noticed in December 2019 and since then has propagated all around the world, leading all countries into trouble in both sanitary and economic ways. The evolution and propagation of the virus has been different all around the world since the countries themselves are different and so are the measures taken. The vaccination against the virus has already started but the pandemic has not finished yet and new variants of the virus might arise. Being able to predict when the peaks of the wave will happen accurately could be very useful knowledge against the virus. Furthermore, statistical models that could explain the drivers and quantify the effects of the different factors and countries' characteristics contributing to the expansion of the virus and deaths would be crucial to analyze whether the strategies taken by the different governments in the world have been useful and scientifically identify good choices to take in future waves of this virus or future pandemics that humanity might face. The present work aims to develop statistical models for explaining the main causes leading COVID-19 deaths and analyze its predictive accuracy, although an analysis based on Machine Learning models will later be done to focus on maximum predictive accuracy.
"), h1("Dataset Description"), p(align="justify","
The dataset was taken from the OurWordlInData repository: https://github.com/owid/covid-19-data/tree/master/public/data on the 14th of April 2021. Data were selected only from July 1st 2020 in advance since data from the first wave is not so reliable and the goal of this project is not to focus only on the first wave. In total, the data set contained 47263 observations.

First thing is that in order to account for the seasonality of these events, one needs to work with lag variables. This is why the smoothed (7 day average) variables have been used instead of the daily ones. Furthermore, since this analysis includes different countries, it is important to normalize the data taking into account the total population of the country, either by taking parts per hundred or per million inhabitants. Also, the dataset contained many variables that were very related with each other (like people above 65 and people above 70, clearly all the ones in the second group are inside the first) and only one of those was kept. Finally, features like admissions in ICU were discarded since they had an enormous amount of missing values (well above 50%).

On the other hand, since accounting for individual countries would not generalize well and generate noise, it has been chosen to work only with continents, that together with the month, can also serve as an indication of the season of the year (in case that has an impact on virus propagation). However, first, many countries have been erased to avoid micro-countries as well as full continents. The full list of entities present in the original dataset that have been discarded for this project (even if country variable is not used) is: Africa, Andorra, Anguilla, Antigua and Barbuda, Asia, Bermuda, Cayman Islands, Dominica, Europe, European Union, Faeroe Islands, Falkland Islands, Gibraltar, Grenada, Guernsey, International, Isle of Man, Jersey, Kosovo, Liechtenstein, Marshall Islands, Micronesia, Monaco, Montserrat, North America, Northern Cyprus, Oceania, Saint Helena, Saint Kitts and Nevis, Saint Lucia, Saint Vincent and the Grenadines, Samoa, San Marino, Sao Tome and Principe, Seychelles, Solomon Islands, Somalia, South America, South Sudan, Syria, Timor, Togo, Trinidad and Tobago, Turks and Caicos Islands, Vanuatu, Vatican, World.

Finally, another variable, is island, was added to account for the fact that islands have it easier to close borders and protect themselves from the virus.

The selected variables thus are"), HTML(" 
<ul>
<li>New deaths smoothed per million: The target variable. Continuous variable representing the average of new deaths over 7 days per million inhabitants of the country.
</li>
<li>New cases smoothed per million: Continuous variable representing the average of new cases over 7 days per million inhabitants of the country.
</li>
<li>People fully vaccinated per hundred: Continuous variable representing the number of people with all the require doses of a given vaccine in that country per hundred inhabitants of the country, so the % of people fully vaccinated.
</li>
<li>Continent: Categorical variable representing the continent with six factors: Africa,
Asia, Europe, North America, Oceania and South America.</li>

<li>Stringency Index: Continuous variable representing a measure of the level of restrictions a country had endured at that time. It is composite measure based on 9 response indicators including school closures, workplace closures, and travel bans, rescaled to a value from 0 to 100 (100 = strictest response)
</li>
<li>Population Density: Continuous variable representing the population density of the country measured in people per square kilometers.
</li>
<li>Aged 65 or older: Continuous variable representing the percentage of the population aged 65 or older.
</li>
<li>Cardiovascular Death Rate: Continuous variable representing the death rate from cardiovascular disease in 2017 (annual number of deaths per 100,000 people).
</li>
<li>Diabetes Prevalence: Continuous variable representing the diabetes prevalence (% of population aged 20 to 79) in 2017.
</li>
<li>Life Expectancy: Continuous variable representing the life expectancy of the country at birth in 2019.
</li>
<li>Human Development Index: Continuous variable representing the Human Development Index (HDI) of the country which is a composite index measuring average achievement in three basic dimensions of human development-a long and healthy life, knowledge and a decent standard of living. Values for 2019.
</li>
<li>Is island: Categorical variable of two levels accounting for the fact of whether the country is an island or not.
</li>
<li>Month: Categorical variable of 10 levels accounting for the month of the year. Data were selected from July 1st 2020, meaning that May and June are not present.
</li>
</ul>"), p(align="justify"," It is important to note that some of these variables contained missing values. The way this has been fixed is by noting that NA in vaccines and cases appeared mostly when those were 0: very beginning of the pandemic in the world so many countries didn't have any and, in the case of vaccines, not until their development. This two cases were set to 0. Also, missing values on population density, HDI and other country features can be replaced just by a quick search on Google. Finally, the rest of the missing values were imputed using Random Forest imputer of the R package Mice that fits a model of a certain feature containing missing values in terms of the other features in the dataset in order to give an estimation of the missing value.")
              
                       )
    
    dataPanel = tabPanel("Variable description", h1("Variable description"),
                     p("Choose a variable to see its distribution"),
                     plotOutput("plot")
)

correlationpanel = tabPanel("Variable relation to deaths",
                     p("Since it has been difficult to find non linear relationships on the predictors describing deaths, the following plot shows linear correlations. Machine Learning models will be used to identify these non linear trends. It can be seen that the highest linear relationship is that with new cases, percentage of population aged 65 or older and Human Development Index."),
                     uiOutput("img")
)


plotlyPanel = tabPanel("Model Results", p(align="justify", "Let's first remember the results obtained with statistical models, summarized in the following table.
                                          A considerable improvement was made by adding interaction terms, achieving up to 77% R squared."),
                       withMathJax(includeMarkdown(url("https://raw.githubusercontent.com/apt345/Advanced-Regression-and-Prediction/main/tabla.md"))),
                       p(align="justify", "And now we can see the improvement made with Machine Learning models. Random forest and K-nearest neighbours are able to explain an incredible 97% of the variability of the testing set. An ensemble with these two models was built in order to generalize better to unknown data, also with very good performance. The other models like SVM or XGBoost also obtain very good results. The presence of non-linearities and the improvement is obvious compared to statistical models. Neural Networks had difficult convergence, probably needing more data in order to achieve good performance."),
                       withMathJax(includeMarkdown(url("https://raw.githubusercontent.com/apt345/Advanced-Regression-and-Prediction/main/tablaML.md")))
                       )

conclusionsPanel=tabPanel("Discussion and Conclusions",
                          p(align="justify", "In conclusion, the methodology used has been able to obtain statistical models that achieve an explanation of 77% of the variance of the COVID-19 7-day average deaths per million as well as obtaining the main factors that explain it: the number of cases and the fact that the country is an island, together with corrections of the number of old people, diabetes prevalence, restrictive measures and the month of the year.

On the other hand, statistical models left some part of the problem unexplained, so Machine Learning tools were used in order to find these underlying patterns, with very good results with over 90% of the variance explained. In the following plots we can see the variable importance of Random Forest and Radial Suppor Vector Machines, were it can be seen that the main drivers are the same as found with statistical models, just that now non-linear trends are included."),
                          uiOutput("rfimportance"),
                          uiOutput("svmimportance"),
                          p(align="justify","Finally, we can use this knowledge not only to explain the main features that drove the pandemic's death but also to make predictions.
                            The data used stop at mid April 2021, so it could be used, for example, to predict the situation this
                            summer (July). Since most of the models have similar performance, I will use the ensemble because it
                            had the best performance and it also generalizes better. See the results plugging in Spain's characteristics, which are 50%
                            of vaccinated people by July (following government estimation); 1000 cases on average (20 per million
                            inhabitants of Spain) knowing that there are now around 10000 and last July it descended to around
                            1000 cases; Stringency Index with low measures, 30. HDI of 0.904 ; population density 93.105; 19.436%
                            of people older than 65; cardiovascular death rate of 99.403; diabetes prevalence of 7.17; life
                            expectancy of 83.56 years and on month 7 (July), in Europe and not an island. Furthermore, the same robust
                            confidence interval that was estimated for the out of sample testing set can be used yielding a low number of 7 day average of deaths in the interval [0.83, 1.16] and thus meaning that the pandemic might hopefully start to come to an end this summer.
                            "))
# Define UI for application that draws a histogram
ui = navbarPage("Statistical and Machine Learning Models for COVID-19 deaths by Arturo Prieto Tirado",
                    introduction,
                      dataPanel,
                        correlationpanel,
                      plotlyPanel,
                        conclusionsPanel,
                      header = myHeader,
                      theme = shinytheme("united"),
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
    name=reactive(input$selected_date)
    #create a, a reactive object that is either a regular plot for categorical variables or density for numerical
    a=reactive(if(name()%in%c("month", "continent", "is_island")){
        plot(vaccines_date())}
    else{ggplot(vaccines_date(), aes_string(x = input$selected_date)) + geom_density(alpha = 0.2)
    })
    #plot a
    output$plot=renderPlot(a())
    
    ##
    
    output$img <- renderUI({
        tags$img(src = "https://github.com/apt345/Advanced-Regression-and-Prediction/blob/main/correlaciones.PNG?raw=true")
    })
    
    output$rfimportance <- renderUI({
        tags$img(src = "https://github.com/apt345/Advanced-Regression-and-Prediction/blob/main/rfimportance.PNG?raw=true")
    })
    
    output$svmimportance <- renderUI({
        tags$img(src = "https://github.com/apt345/Advanced-Regression-and-Prediction/blob/main/svmimportance.PNG?raw=true")
    })
    
    #refresh page when moving through sections
    observe({
        shinyjs::show("advanced")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
