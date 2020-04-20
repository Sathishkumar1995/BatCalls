library(shiny)
library(shinythemes)
library(tidyverse)
library(MASS)
library(readxl)
library(nnet)
library(Boruta)
library(rhandsontable)
library(shinydashboard)
library(class)
#Read the data
#Species_Data <- read_excel("https://github.com/Sathishkumar1995/Batcall/blob/master/Species-Data.xlsx")
Species_Data<-read_excel('Species-Data.xlsx')
Species_Data<-Species_Data%>%mutate(Country=as.factor(Country),Species=as.factor(Species))
#Changing the datatypes
a<-suppressWarnings(data.frame(sapply(Species_Data[,-(1:8)],function(x) as.numeric(x))))
a<-cbind(a,Species_Data[,1:8])
a$Folds<-as.factor(a$Folds)
a$Guild<-as.factor(a$Guild)
a$Genus<-as.factor(a$Genus)
a$Family<-as.factor(a$Family)
#Omit the NAs
a<-na.omit(a)  
fact<-a[,74:81]#factor variables
nfact<-a[,-c(74:81)]#other parameters
tmp <- cor(a[,-c(74:81)])
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0  
#remove highly correlated variables
data.new <- nfact[,!apply(tmp,2,function(x) any(x > 0.9))]
a<-data.frame(fact,data.new)
rm(list=c('fact','nfact','data.new','tmp')) 
Species_Data<-a[,c(6,9:length(a))]#Final
load("Parameter.Rdata")
#species_boruta<-Boruta(Species ~ ., data=Species_Data, doTrace=0)
#regsubsets(Species~.,Species_Data[,-48],really.big = T)->regsub
#Ignoring the 48th column as it had more zeros and was coming as an important predictor
ui <- fluidPage(

theme = shinytheme('united'),
navbarPage('Bat Calls',
tabPanel('Introduction'),
tabPanel('Techniques'),
tabPanel('Predict Species',
                fluidRow(selectInput("Function",label = "Select the prediction method",choices = c('LDA','QDA')),
                selectInput('parametermethod',label = 'Parameter selection method: ',choices = c('Boruta','Regsubsets')),
                sliderInput('par','Number of parameters: ', min=1,max=6,5),
                box(actionButton("runButton","Predict the Species"))
                ),
                textOutput('specieserror'),
                tabsetPanel(
                    tabPanel("Input", rHandsontableOutput('speciesdata')),
                    tabPanel("Predicted", DT::dataTableOutput("speciespredicted"))
                )),
navbarMenu('More',tabPanel('Conclusion'))
))

server <- function(input, output,session) {
    imps <- attStats(species_boruta)
    imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
    
    observe({x<-input$parametermethod
    if(x=='Regsubsets')
        updateSliderInput(session, "par",min=1,max=7,value=5)
    if(x=='Boruta')
        updateSliderInput(session, "par",min=1,max=11,value=5)
    })
    values <- reactiveValues()
    output$speciesdata <- renderRHandsontable({
#        Species<-Species_Data$Species
        if(input$parametermethod=='Boruta'){
        rownames(head(imps2[order(-imps2$meanImp), ],input$par))->importantpar
        #Data with selected important parameters
        Species_Data1<<-Species_Data[,c("Species",importantpar)]}
        else if(input$parametermethod=='Regsubsets'){
#            Species<-Species_Data$Species
            Species_Data1<<-Species_Data[,c("Species",names(which(summary(regsub)$which[input$par,-1])))]
#            Species_Data1<<-data.frame(Species,Species_Data1)
        }
#        rownames(head(imps2[order(-imps2$meanImp), ],input$par))->importantpar
#        Species_Data1<<-Species_Data[,c("Species",importantpar)]
        lda(Species~., data = Species_Data1)->>ldafunctionspecies
        qda(Species~., data = Species_Data1)->>qdafunctionspecies
        head(Species_Data1[,-1],4)->displayspecies
        rownames(displayspecies)<-NULL #was getting errors if we added a new record
        rhandsontable(displayspecies)
    })
    observeEvent(input$runButton, {
        values$data <-  hot_to_r(input$speciesdata)
        if(input$Function == 'LDA'){
            values$data<-cbind(predict(ldafunctionspecies,values$data)$class,values$data)
        }
        else if(input$Function == 'QDA'){
            values$data<-cbind(predict(qdafunctionspecies,values$data)$class,values$data)
        }
        names(values$data)[1]<-'Species'
        })
    
    output$speciespredicted <- DT::renderDataTable({
        values$data
    })  
    output$specieserror<-renderText({
        if(input$Function == 'LDA')
            mean(predict(ldafunctionspecies)$class==Species_Data1$Species)->errorspecies
        else if(input$Function == 'QDA')
            mean(predict(qdafunctionspecies)$class==Species_Data1$Species)->errorspecies
            c('The model is ',round(errorspecies*100,2),'% accurate')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
