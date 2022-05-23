version = read.table("version.txt")
version = version$V1[1]

options(shiny.maxRequestSize = 30*1024^2)
options(warn = -1)
library(stringr)
library(readxl)
library(writexl)
library(crayon)
library(stringi)
library(shinyalert)

##### Code client

ui <- navbarPage(paste0("Transformers - ",version), id="Adresse",
    tabPanel("Adresse"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Fichier excel", accept=".xlsx", multiple=TRUE, buttonLabel = "Parcourir", placeholder = "Aucun fichier sélectionné"),
            selectInput("s_input1", label="Colonne", choices=c("Aucun fichier sélectionné")),
            actionButton("lancer","Lancer le traitement")
        ),
        mainPanel(
            tableOutput("contents"),
        )
    )
)

##### Code serveur

server <- function(input, output) {
    
    observeEvent(input$lancer, {
        
        shinyalert("Démarrage en cours","Veuillez patienter quelques secondes", type = "info", timer = 2000)
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        new_text = input$s_input1
        replace <- read.table("https://raw.githubusercontent.com/clementfarfait/transformers/main/modifications", sep=";", header=TRUE)
        replace$adresse <- lapply(replace$adresse, stri_replace_all_regex, pattern="apostrophe",replacement="'",vectorize_all=FALSE)
        replace$correction <- lapply(replace$correction, stri_replace_all_regex, pattern="vide",replacement=" ",vectorize_all=FALSE)
        sep <- as.data.frame(str_split(new_text, " : "))
        data <- read_excel(inFile$datapath, sheet = sep[1,1])
        for(z in 1:length(colnames(data))){
            if(colnames(data[z]) == sep[2,1]){
                indice = z
                break
            }
        }
        n = nrow(data[,indice])
        estimation = as.integer(n / 137)
        convert = as.integer(round(estimation/1000))
        time <- system.time({
            shinyalert("Traitement en cours",paste0("Estimation : ",convert," secondes"), type = "info", timer = estimation)
            d <- as.list(data[,indice])
            d <- lapply(d, stri_replace_all_regex, pattern=as.character(replace$adresse),replacement=as.character(replace$correction),vectorize_all=FALSE)
            d <- as.data.frame(d)
            new_data <- write_xlsx(d, paste0("../modifications/",sep[1,1],"-",sep[2,1],".xlsx",sep=""))
        })
        
        time <- as.integer(time[3])
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
        
        shinyalert(paste0("Modifications terminées\nTemps : ",time),paste0("modifications/",sep[1,1],"-",sep[2,1],".xlsx"), type = "success")
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
        
        shinyalert("Fichier importé avec succès","Veuillez sélectionner la colonne à traiter", type = "success")
        cat("")
    })
}

shinyApp(ui = ui, server = server)
