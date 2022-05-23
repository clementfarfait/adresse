##### Chargement de la version et des parametres necessaires

version = read.table("version.txt")
version = version$V1[1]
options(shiny.maxRequestSize = 30*1024^2)
options(warn = -1)

##### Chargement des packages necessaires

library(stringr)
library(readxl)
library(writexl)
library(crayon)
library(stringi)
library(shinyalert)

##### Liste des remplacements récupérée depuis un git

replace <- read.table("https://raw.githubusercontent.com/clementfarfait/transformers/main/modifications", sep=";", header=TRUE)
replace$adresse <- lapply(replace$adresse, stri_replace_all_regex, pattern="apostrophe",replacement="'",vectorize_all=FALSE)
replace$correction <- lapply(replace$correction, stri_replace_all_regex, pattern="vide",replacement=" ",vectorize_all=FALSE)

##### Code client

ui <- navbarPage(paste0("Transformers - ",version), id="Adresse",
    tabPanel("Adresse",
             sidebarLayout(
                 sidebarPanel(
                     fileInput("file1", "Fichier excel", accept=".xlsx", buttonLabel = "Parcourir", placeholder = "Aucun fichier sélectionné"),
                     selectInput("s_input1", label="Colonnes", choices=c("Aucun fichier sélectionné"),selected="Aucun fichier sélectionné",multiple=TRUE),
                     actionButton("lancer","Lancer le traitement"),
                 ),
                 mainPanel(
                     tableOutput("contents"),
                 )
             )
    ),
    tabPanel("Prochainement",
             sidebarLayout(
                 sidebarPanel(
                 ),
                 mainPanel(
                     tableOutput("contents2"),
                 )
             )
    ),

)

##### Code serveur

server <- function(input, output) {
    
    observeEvent(input$lancer, {
        
        shinyalert("Traitement en cours","Veuillez patienter quelques secondes", type = "info", showConfirmButton = FALSE)
        time <- system.time({
            inFile <- input$file1
            if (is.null(inFile))
                return(NULL)
            new_text = input$s_input1
            for(i in 1:length(input$s_input1)){
                sep <- as.data.frame(str_split(new_text[i], " : "))
                data <- read_excel(inFile$datapath, sheet = sep[1,1])
                indice = which(colnames(data) == sep[2,1])
                n = nrow(data[,indice])
                d <- as.list(data[,indice])
                d <- lapply(d, stri_replace_all_regex, pattern=as.character(replace$adresse),replacement=as.character(replace$correction),vectorize_all=FALSE)
                d <- as.data.frame(d)
                new_data <- write_xlsx(d, paste0("../modifications/",sep[1,1],"-",sep[2,1],".xlsx",sep=""))
            }
        })
        
        if(time[3] >= 1){
            time <- as.integer(time[3])
        } else {
            time <- round(time[3],digits = 1)
        }
        if(time > 60){
            time <- round(time / 60)
            if(time > 1){
                t = " minutes"
            } else {
                t = " minute"
            }
            time <- paste0(time,t)
        } else {
            if(time > 1){
                t = " secondes"
            } else {
                t = " seconde"
            }
            time <- paste0(time,t)
        }
        
        shinyalert("Modifications terminées",paste0("Nombre de lignes traitées : ",n,"\nTemps : ",time,"\nDestination du fichier : modifications/",sep[1,1],"-",sep[2,1],".xlsx"), type = "success", immediate = TRUE)
    })
    
    output$contents <- renderTable({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        choices <- excel_sheets(inFile$datapath)
        v = c()
        for(i in 1:length(choices)){
            data <- read_excel(inFile$datapath, sheet = choices[i])
            for(y in 1:length(colnames(data))){
                v = c(v,paste0(choices[i]," : ",colnames(data)[y]))
            }
        }
        updateSelectInput(getDefaultReactiveDomain(),"s_input1",label = "Colonne", choices=as.vector(v))
        shinyalert("Fichier importé avec succès","Veuillez sélectionner les colonnes à traiter", type = "success", immediate = TRUE)
        cat("")
    })
}

shinyApp(ui = ui, server = server)
