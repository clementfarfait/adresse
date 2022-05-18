
##### Vérification de mises à jour

version = "1.0.4"
check_version = read.table("https://raw.githubusercontent.com/clementfarfait/adresse/main/version")
print("*******************")
print("*   Mise à jour   *")
print("*******************")
print(" ")
if(check_version$V1[1] != version){
    print("Une mise à jour est nécessaire.")
    print("Téléchargement..")
    download.file("https://raw.githubusercontent.com/clementfarfait/adresse/main/module.R","module.R")
    print("Mise à jour terminé")
} else {
    print("Aucune nécessaire")
}

##### Vérification des fichiers d'installation

if(file.exists("install.R")){
    file.remove("install.R")
}
if(file.exists("install.cmd")){
    file.remove("install.cmd")
}

##### Chargement des packages et paramètres

options(shiny.maxRequestSize = 30*1024^2)
library(stringr)
library(readxl)
library(writexl)
library(shiny)

##### Code client

ui <- fluidPage(
    titlePanel("Correction d'adresse"),

    sidebarLayout(
        sidebarPanel(
            textInput("input1", "feuille:col;feuille2:col", value = "", width = NULL, placeholder = NULL),
            fileInput("file1", "Fichier excel", accept=".xlsx", multiple=TRUE),
        ),
        mainPanel(
            tableOutput("contents")
        )
    )
)

##### Code serveur

server <- function(input, output) {
    output$contents <- renderTable({
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        text <- input$input1
        new_text <- as.data.frame(str_split(text, ";"))
        
        replace <- read.table("https://raw.githubusercontent.com/clementfarfait/clementfarfait.github.io/main/adresse", sep=";", header=TRUE)
        for(i in 1:length(replace$adresse)){
            replace[i,1] <- str_replace(replace[i,1], pattern = "apostrophe", replacement = "'")
        }
        for(i in 1:length(replace$correction)){
            replace[i,2] <- str_replace(replace[i,2], pattern = "vide", replacement = " ")
        }
        print(replace)
        
        for(i in 1:length(new_text[,1])){
            sep <- as.data.frame(str_split(new_text[i,1], ":"))
            data <- read_excel(inFile$datapath, sheet = sep[1,1])
            
            for(z in 1:length(colnames(data))){
                if(colnames(data[z]) == sep[2,1]){
                    indice = z
                }
            }
            
            for(p in 1:nrow(data[,indice])){
                data[p,indice] <- toupper(data[p,indice])
                for(y in 1:length(replace$adresse)){
                    data[p,indice] <- str_replace(data[p,indice], pattern = replace$adresse[y], replacement = replace$correction[y])
                }
            }
            
            new_data <- write_xlsx(data[,indice], paste0("modifications/",sep[1,1],"-",sep[2,1],".xlsx",sep=""))
            print(paste0("OK: modifications/",sep[1,1],"-",sep[2,1],".xlsx"))
        }
        print("OK!")
    })
    
    output$value <- renderText({ input$caption })
}

shinyApp(ui = ui, server = server)
