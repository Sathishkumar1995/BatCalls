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
library(leaps)
library(randomForest)
library(e1071)
library(corrplot)

#Read the data
Species_Data<-read_excel('Species-Data.xlsx',na='NA')
corrdata<-na.omit(Species_Data)[,-(1:8)]
#Changing the datatypes
a<-suppressWarnings(data.frame(sapply(Species_Data[,1:8],function(x) as.factor(x))))
a<-cbind(a,Species_Data[,-c(1:8)])
original<-a
#Omit the NAs
a<-na.omit(a)  

fact<-a[,1:8]#factor variables
nfact<-a[,-c(1:8)]#other variables

#correlation
tmp <- cor(a[,-c(1:8)])
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0  

#remove highly correlated variables
data.new <- nfact[,!apply(tmp,2,function(x) any(x > 0.9))]
a<-data.frame(fact,data.new)
rm(list=c('fact','nfact','data.new','tmp')) 
Species_Data<-a[,c(6,9:length(a))]#Final
original[names(Species_Data)]->original

#Dataset with a list of unique genus, species combinations
a%>%dplyr::select(Family,Genus,Species)%>%unique->genspe
genspe$Family<-as.character(genspe$Family)
genspe$Genus<-as.character(genspe$Genus)
genspe$Species<-as.character(genspe$Species)
paste(genspe$Genus,genspe$Species)->genspe$FullName

load("Parameter.Rdata")
#species_boruta<-Boruta(Species ~ ., data=Species_Data, doTrace=0)
#regsubsets(Species~.,Species_Data[,-48],really.big = T,nvmax=11)->regsubexh
#Ignoring the 48th column as it had more zeros and was coming as an important predictor

ui <- dashboardPage(
    dashboardHeader(title = 'BatCalls',dropdownMenuOutput('headernoti'),dropdownMenuOutput('headermess')),
    dashboardSidebar(
        sidebarMenu(
            menuItem('Introduction',tabName = 'dashintro'),
            menuItem('Data',tabName = 'dashdata'),
            menuItem('Predict Species',tabName = 'dashpred'),
            menuItem('Conclusion',tabName = 'dashconc')
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'dashintro',htmlOutput('intro')),
            tabItem(tabName = 'dashdata',plotOutput('corrgraph'),htmlOutput('corrdetails')),
            tabItem(tabName = 'dashpred',
                    inputPanel(
                        radioButtons('filter',label = 'Do you have additional information?',choices = c('Family','No'),selected = 'No',inline = T,width = '250px'),
                        uiOutput('hidefilter')
                    ),
                    fluidRow(
                             box(selectInput("Function",label = "Select the classification method",choices = c('LDA','QDA','KNN','Random Forest','SVM')),
                             uiOutput('hideknn')),
                             box(
                             selectInput('parametermethod',label = 'Variable selection method: ',choices = c('Boruta','Regsubsets')),
                             uiOutput('hideregsub'),
                             sliderInput('par','Number of variables to be used: ', min=2,max=11,5))
                    ),
                    column(12,
                    actionButton('instruct',"Instructions"),
                    actionButton("runButton","Predict the Species")),
                    textOutput('specieserror'),
                    tabsetPanel(
                        tabPanel("Input", rHandsontableOutput('speciesdata')),
                        tabPanel("Predicted", DT::dataTableOutput("speciespredicted"))
                    )),
            tabItem(tabName = 'dashconc',htmlOutput('conc'))
        )
    )
)

server <- function(input, output,session) {
    observeEvent(input$instruct,{
        showModal(modalDialog(paste('Select the classification method and the variable selection method along with the number of variables to be used from the dropdown boxes. You can edit the table displayed below or add new records and predict the species of the observations by clicking on the PREDICT button. The results can be viewd in the PREDICTED tab below.'),footer = modalButton('Okay')))
    })
    
    observe(
        if((input$filter=='No')&(input$Function == 'Random Forest')){
            showModal(modalDialog(paste('It may take upto a minute to run the RandomForest algorithm. Please edit the data once the table and the line showing the accuracy becomes opaque. Click outside the box to continue'),size = 'm',easyClose = T,footer = NULL))   
        }
    )
    
    observe(
        if(input$filter == 'Family')
            showModal(modalDialog(paste('There are a few families which have only one species and so they are not displayed in the dropdown menu. Click on the notifications pane for more details on the families and species. Click outside the box to continue'),size = 'm',easyClose = T,footer = NULL))
            )
    
    observe(
        if((input$filter == 'Family')&(input$parametermethod=='Boruta'))
            showModal(modalDialog(paste('It may take upto a minute to run the Boruta function. Please start editing the data once the table and the line showing the accuracy becomes opaque. Click outside the box to continue'),size = 'm',easyClose = T,footer = NULL))   
        )
    
    observe(
        if((input$filter == 'Family')&(input$parametermethod=='Boruta'))
            showNotification('Boruta takes time to run',duration = 10,type = 'warning')
        )
    
    output$corrgraph<-renderPlot({
        corrplot(cor(corrdata),order = 'hclust',hclust.method = 'ward.D2',tl.cex=0.5,cl.cex = 0.6,method = 'square')
    })
    
    output$corrdetails<-renderUI({
        HTML('From the correlation plot, it is evident that many variables are highly correlated with other variables.')
    })
    
    basedata<-reactive({
        if(input$filter=='No')
            a[,c(6,9:58)]
        else if(input$filter=='Family'){
            subset(a,Family==input$fam)[,c(6,9:58)]->pl
            pl$Species<-factor(pl$Species) #To get rid of factor levels which don't belong in the selected family
            pl
        }
    })
    
    output$headernoti<-renderMenu({
        if((input$filter=='No')&(input$Function == 'Random Forest')){
        dropdownMenu(type = 'notifications',notificationItem(
            text = 'Random Forest takes time',
            icon = icon("exclamation-triangle"),status = 'warning'
        ))}
    })
    
    output$headermess<-renderMenu({
        if(input$filter == 'Family'){
            dropdownMenu(type = 'messages',messageItem(from = 'Thyropteridae',message = 'Thyroptera Thytri'),
                         messageItem(from = 'Natalidae',message = 'Natalus Natstr'),
                         messageItem(from = 'Noctilionidae',message = 'Noctilio Noclep'))}
    })
    
    output$intro<-renderUI({HTML('Classifying the species of bats based on the information recorded on the sounds generated by bats.<br><br>
                                 Known issues in the shiny app:<br>A warning will be displayed when you change the variable selection method from Boruta to Regsubsets for the first time.<br><br><br>
                                 The code and the data used can be viewed <a href="https://github.com/Sathishkumar1995/BatCalls">here</a>')})

    output$hidefilter<-renderUI({
        if(input$filter=='Family')
            selectInput('fam',label = 'Select the Family',choices = unique(a$Family)[!unique(a$Family)%in%c("Natalidae",'Thyropteridae','Noctilionidae')])
    })    
    
    output$hideknn<-renderUI({
        if(input$Function=='KNN')
            sliderInput('knnk','Number of nearest neighbours to compare for KNN ', min=1,max=30,3)
    })
    
    output$hideregsub<-renderUI({
        if(input$parametermethod=='Regsubsets')
            selectInput('regsubmethod',label = 'Regsubsets variable searching method ',choices = c('Exhaustive','Forward Selection','Backward Selection','Sequential Replacement'),selected = 'Forward Selection')
    })
    
    values <- reactiveValues()
    
    regsubf<-reactive({
        if(input$filter=='No'){
            if(input$regsubmethod=="Exhaustive")
                regsubexh
            else if(input$regsubmethod=='Forward Selection')
                regsubsets(Species~.,basedata()[,-48],nvmax=11,method = 'forward')
            else if(input$regsubmethod=='Backward Selection')
                regsubsets(Species~.,basedata()[,-48],nvmax=11,method = 'backward')
            else if(input$regsubmethod=='Sequential Replacement')
                regsubsets(Species~.,basedata()[,-48],nvmax=11,method = 'seq')}
        else if(input$filter=='Family'){
            if(input$regsubmethod=="Exhaustive"){
                    if(input$fam=='Phyllostomidae')
                        regsubexhphy
                    else if(input$fam=='Vespertilionidae')
                        regsubexhves
                    else if(input$fam=='Emballonuridae')
                        regsubexhemb
                    else if(input$fam=='Molossidae')
                        regsubexhmol
                    else if(input$fam=='Mormoopidae')
                        regsubexhmor
                }
            else if(input$regsubmethod=='Forward Selection')
                regsubsets(Species~.,basedata()[,-48],nvmax=11,method = 'forward')
            else if(input$regsubmethod=='Backward Selection')
                regsubsets(Species~.,basedata()[,-48],nvmax=11,method = 'backward')
            else if(input$regsubmethod=='Sequential Replacement')
                regsubsets(Species~.,basedata()[,-48],nvmax=11,method = 'seq')}
    })
    
    bor<-reactive({
        if(input$filter=='No')
            species_boruta
        else if(input$filter=='Family')
            Boruta(Species~.,basedata())
    })
    
    importantpar<-reactive({
        if(input$parametermethod=='Boruta'){
            imps <- attStats(bor())
            imps2 <- imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
            rownames(head(imps2[order(-imps2$meanImp), ],input$par))->importantpar}
        else if(input$parametermethod=='Regsubsets'){
            importantpar<-names(coef(regsubf(),input$par))[-1]  }
    })
    
    Species_Data1<-reactive({
       # Species_Data[,c("Species",importantpar)]
        if(input$filter=='No')
            na.omit(original[,c("Species",importantpar())])
        else if(input$filter=='Family')
            basedata()[,c("Species",importantpar())]
    })
    
    output$speciesdata <- renderRHandsontable({
        head(Species_Data1()[,-1],4)->displayspecies
        rownames(displayspecies)<-NULL #was getting errors if we added a new record
        rhandsontable(displayspecies)
    })
    
    ldafunctionspecies<-reactive({
        lda(Species~., data = Species_Data1())
    })
    
    qdafunctionspecies<-reactive({
        qda(Species~., data = Species_Data1())
    })
    
    rfspecies<-reactive({
        randomForest(Species~.,data = Species_Data1())
    })
    
    svmspecies<-reactive({
        svm(Species~., data = Species_Data1())
    })
    
    observeEvent(input$runButton, {
        values$data <-  hot_to_r(input$speciesdata)
        if(input$Function == 'LDA'){
            values$data<-cbind(genspe%>%filter(Species %in% as.character(predict(ldafunctionspecies(),values$data)$class))%>%dplyr::select(FullName),values$data)
        }else if(input$Function == 'QDA'){
            values$data<-cbind(genspe%>%filter(Species %in% as.character(predict(qdafunctionspecies(),values$data)$class))%>%dplyr::select(FullName),values$data)
        }else if(input$Function == 'Random Forest'){
            values$data<-cbind(genspe%>%filter(Species %in% as.character(predict(rfspecies(),newdata=values$data)))%>%dplyr::select(FullName),values$data)
        }else if(input$Function == 'SVM'){
            values$data<-cbind(genspe%>%filter(Species %in% as.character(predict(svmspecies(),values$data)))%>%dplyr::select(FullName),values$data)
        }else if(input$Function == 'KNN'){
            knntrain<-scale(Species_Data1()[,-1])
            means<-attr(knntrain,'scaled:center')
            sd<-attr(knntrain,'scaled:scale')
            knntest<-scale(values$data,center=means,scale = sd)
            knnresult<-knn(knntrain,knntest,Species_Data1()[,1],k = input$knnk)
            values$data<-cbind(genspe%>%filter(Species %in% as.character(knnresult))%>%dplyr::select(FullName),values$data)
        }
        names(values$data)[1]<-'Species'
    })
    
    output$speciespredicted <- DT::renderDataTable({
        values$data
    })  
    
    output$specieserror<-renderText({
        if(input$Function == 'LDA'){
            mean(predict(ldafunctionspecies())$class==Species_Data1()$Species)->errorspecies
            c('The model is ',round(errorspecies*100,2),'% accurate on the training data')}
        else if(input$Function == 'QDA'){
            mean(predict(qdafunctionspecies())$class==Species_Data1()$Species)->errorspecies
            c('The model is ',round(errorspecies*100,2),'% accurate on the training data')}
        else if(input$Function == 'Random Forest'){
            mean(predict(rfspecies())==Species_Data1()$Species)->errorspecies
            c('The model is ',round(errorspecies*100,2),'% accurate on the training data')}
        else if(input$Function == 'SVM'){
            mean(predict(svmspecies())==Species_Data1()$Species)->errorspecies
            c('The model is ',round(errorspecies*100,2),'% accurate on the training data')}
        else if(input$Function == 'KNN')
            'You can edit the parameter k which has to be used in KNN'
    })
    
    output$conc <- renderUI({HTML(paste('Random Forest performs better in most cases',sep = '<br>'))})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
