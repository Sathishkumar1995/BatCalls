library(shiny)
library(tidyverse)
library(MASS)
library(readxl)
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
Species_info<-read_excel('Species-Data.xlsx',na='NA',sheet = 2)
corrdata<-na.omit(Species_Data)[,-(1:8)]

#Changing the datatypes
names(Species_info)<-c('FullName','Species')
a<-suppressWarnings(data.frame(sapply(Species_Data[,1:8],function(x) as.factor(x))))
a<-cbind(a,Species_Data[,-c(1:8)])
original<-a
fullbackup<-a

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
arrange(genspe,Family,Genus,Species)->genspe
left_join(genspe,Species_info,by='Species')->genspe

#save(regsubexh,regsubexhemb,regsubexhmol,regsubexhphy,regsubexhmorm,regsubexhves,species_boruta,borphy,borves,boremb,bormol,bormorm,file = 'Parameter.Rdata')
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
            menuItem('Predict Species',tabName = 'dashpred')
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'dashintro',htmlOutput('intro')),
            tabItem(tabName = 'dashdata',htmlOutput('corrdetails'),plotOutput('corrgraph'),
                    selectInput('datafam','Select the family',choices = unique(genspe$Family)),DT::dataTableOutput('datatab')),
            tabItem(tabName = 'dashpred',
                    inputPanel(
                        radioButtons('filter',label = 'Do you have additional information?',choices = c('Family','No'),selected = 'No',inline = T,width = '250px'),
                        HTML('    '),
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
                    radioButtons('file','Do you want to upload a csv file with the test data?',choices = c('Yes','No'),selected = 'No',inline = T),
                    
                    tabsetPanel(
                        tabPanel("Input", uiOutput('hidefile'),uiOutput('filedetails'),uiOutput('samplehide'),rHandsontableOutput('speciesdata')),
                        tabPanel("Results",downloadButton('download','Download the result') ,DT::dataTableOutput("speciespredicted"))
                    ))
        )
    )
)

server <- function(input, output,session) {
    observeEvent(input$instruct,{
        showModal(modalDialog(paste('Select the classification method and the variable selection method along with the number of variables to be used from the dropdown boxes. You can edit the table displayed below or add new records and predict the species of the observations by clicking on the PREDICT THE SPECIES button. The results can be viewd in the RESULTS tab below.'),footer = modalButton('Okay')))
    })
    
    observe(
        if(input$filter == 'Family')
            showModal(modalDialog(paste('There are a few families which have only one species and so they are not displayed in the dropdown menu. Click on the notifications pane for more details on the families and species. Click outside the box to continue'),size = 'm',easyClose = T,footer = NULL))
            )

    output$corrgraph<-renderPlot({
        corrplot(cor(corrdata),order = 'hclust',hclust.method = 'ward.D2',tl.cex=0.5,cl.cex = 0.7,method = 'square')
    })
    
    output$corrdetails<-renderUI({
        HTML('Some additional information can be viewed <a href=https://public.tableau.com/profile/sathishkumar.ravichandran#!/vizhome/Thesis_15960196333830/Observationpercountry>here</a>.<br><br>The data has a total of 4685 observations with 81 variables. 145 observations has atleast one of the variables missing in the data.<br>From the correlation plot below, it is evident that many variables are highly correlated with other variables.')
    })
    
    output$datatab<-DT::renderDataTable({
        genspe%>%filter(Family %in% input$datafam)%>%dplyr::select(-1)->p
        DT::datatable(p,options = list(pageLength=5))
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
    
    fildata<-reactive({
        subset(fullbackup[,names(a)],Family==input$fam)[,c(6,9:58)]->pl
        pl$Species<-factor(pl$Species) #To get rid of factor levels which don't belong in the selected family
        pl
    })
    
    output$headernoti<-renderMenu({
        if((input$filter=='No')&(input$Function == 'Random Forest')){
        dropdownMenu(type = 'notifications',notificationItem(
            text = 'Random Forest takes time',
            icon = icon("exclamation-triangle"),status = 'warning'
        ))}
    })
    
    output$samplehide<-renderUI({
        if(input$file=='Yes')
        downloadButton('sample','Download a sample')
    })
    
    output$headermess<-renderMenu({
        if(input$filter == 'Family'){
            dropdownMenu(type = 'messages',messageItem(from = 'Thyropteridae',message = 'Thyroptera tricolor'),
                         messageItem(from = 'Natalidae',message = 'Natalus stramineus'),
                         messageItem(from = 'Noctilionidae',message = 'Noctilio leporinus'))}
    })
    
    output$intro<-renderUI({HTML('Classifying the species of bats based on the information recorded on the sounds generated by bats.<br><br>
                                 Known issues in the shiny app:<br>Based on the number of variables selected, QDA may have issues with rank deficiency in some cases. Select a different variable selection method or change the number of variables to overcome this issue.<br><br><br>
                                 The code and the data used can be viewed <a href="https://github.com/Sathishkumar1995/BatCalls">here</a>')})

    output$filedetails<-renderUI({
        if(input$file=='Yes')
        HTML("Select the feature selection method, the number of variables needed and download a sample dataset using the button below.<br>
        A sample file with all the variables can be downloaded <a href=https://docs.google.com/spreadsheets/d/1jZFbaVDSbcdzQ5hZ7BLRDxHiea5R31etMLyQRCzDckQ/edit?usp=sharing>here</a>. The red highlighted cells in the sample data are the ones which are not highly correlated and they may be used in the classification
             based on the number of variables selected. Please do not upload data with NAs")
    })
    
    output$hidefilter<-renderUI({
        if(input$filter=='Family')
            selectInput('fam',label = 'Select the Family',choices = unique(a$Family)[!unique(a$Family)%in%c("Natalidae",'Thyropteridae','Noctilionidae')])
    })    
    
    output$hideknn<-renderUI({
        if(input$Function=='KNN')
            sliderInput('knnk','Number of nearest neighbours to compare for KNN ', min=1,max=30,3)
    })
    
    output$hidefile<-renderUI({
        if(input$file=='Yes')
            fileInput('infile','Upload a csv file',accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"))
    })
    
    output$hideregsub<-renderUI({
        if(input$parametermethod=='Regsubsets')
            selectInput('regsubmethod',label = 'Regsubsets variable searching method ',choices = c('Exhaustive','Forward Selection','Backward Selection','Sequential Replacement'),selected = 'Forward Selection')
    })
    
    values <- reactiveValues()
    
    inputdata<-reactive({
        inputdatafrom<-input$infile
        if (is.null(inputdatafrom))
            return(NULL)
        read_csv(inputdatafrom$datapath)
    })
    
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
                        regsubexhmorm
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
        else if(input$filter=='Family'){
            if(input$fam=='Phyllostomidae')
                borphy
            else if(input$fam=='Vespertilionidae')
                borves
            else if(input$fam=='Emballonuridae')
                boremb
            else if(input$fam=='Molossidae')
                bormol
            else if(input$fam=='Mormoopidae')
                bormorm}
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
            na.omit(fildata()[,c("Species",importantpar())])
    })
    
    output$speciesdata <- renderRHandsontable({
        if(input$file=='No'){
        head(Species_Data1()[,-1],4)->displayspecies
        rownames(displayspecies)<-NULL #was getting errors if we added a new record
        rhandsontable(displayspecies)%>%hot_validate_numeric(cols=c(1:ncol(displayspecies)))
        }
        # else{
        #     head(inputdata()[,importantpar()],4)->displayspecies
        #     rhandsontable(displayspecies)
        # }
    })
    
    samplefile<-reactive({
        head(Species_Data1()[,-1],4)->displayspecies
        rownames(displayspecies)<-NULL
        displayspecies
    })
    
    output$sample<-downloadHandler(
        filename = function(){"Sample.csv"}, 
        content = function(fname){
            write.csv(samplefile(), fname)}
    )
    
    ldafunctionspecies<-reactive({
        lda(Species~., data = Species_Data1())
    })
    
    qdafunctionspecies<-reactive({
        qda(Species~., data = Species_Data1())
    })
    
    rfspecies<-reactive({
        withProgress(randomForest(Species~.,data = Species_Data1()),value=1,message = 'Please wait while the algorithm runs in the background')
    })
    
    svmspecies<-reactive({
        withProgress(svm(Species~., data = Species_Data1()),value = 1,message = 'Please wait while the algorithm runs in the background')
    })
    
    observeEvent(input$runButton, {
        if(input$file=='No')
            values$data <-  hot_to_r(input$speciesdata)
        else
            values$data <- inputdata()
        
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
        DT::datatable(values$data,options = list(pageLength=5))
    })  
    
    output$download<-downloadHandler(
        filename = function(){"Result.csv"}, 
        content = function(fname){
            write.csv(values$data, fname)}
    )
    
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
        else if(input$Function == 'KNN'){
            knntrain<-scale(Species_Data1()[,-1])
            knnresult<-knn(knntrain,knntrain,Species_Data1()[,1],k = input$knnk)
            mean(knnresult == Species_Data1()$Species)->errorspecies
            c('The model is ',round(errorspecies*100,2),'% accurate on the training data')}
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
