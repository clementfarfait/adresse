version = read.table("version.txt")
version = version$V1[1]

options(shiny.maxRequestSize = 30*1024^2)
library(stringr)
library(readxl)
library(writexl)
library(crayon)

##### Code client

ui <- fluidPage(
    titlePanel(paste0("Correction d'adresse - ",version)),

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
        
        po = 0
        
        withProgress(message = 'Modification en cours..', detail = paste0("[",po,"%]"), value = 0, {
            text <- input$input1
            new_text <- as.data.frame(str_split(text, ";"))
            
            replace <- read.table("https://raw.githubusercontent.com/clementfarfait/adresse/main/modifications", sep=";", header=TRUE)
            for(i in 1:length(replace$adresse)){
                replace[i,1] <- str_replace(replace[i,1], pattern = "apostrophe", replacement = "'")
            }
            for(i in 1:length(replace$correction)){
                replace[i,2] <- str_replace(replace[i,2], pattern = "vide", replacement = " ")
            }
            
            for(i in 1:length(new_text[,1])){
                sep <- as.data.frame(str_split(new_text[i,1], ":"))
                data <- read_excel(inFile$datapath, sheet = sep[1,1])
                
                for(z in 1:length(colnames(data))){
                    if(colnames(data[z]) == sep[2,1]){
                        indice = z
                    }
                }
                
                n = nrow(data[,indice])
                
                for(p in 1:nrow(data[,indice])){
                    data[p,indice] <- toupper(data[p,indice])
                    for(y in 1:length(replace$adresse)){
                        data[p,indice] <- str_replace(data[p,indice], pattern = replace$adresse[y], replacement = replace$correction[y])
                    }
                    setProgress(value = p/n, detail = paste0("[",round(p/n*100,1),"%]"))
                    #cat(paste0("\n[",green("OK"),"] Ligne ",p))
                }
                
                new_data <- write_xlsx(data[,indice], paste0("../modifications/",sep[1,1],"-",sep[2,1],".xlsx",sep=""))
                cat(paste0("\nOK: modifications/",sep[1,1],"-",sep[2,1],".xlsx\n"))
            }
        })
        print("Modifications terminées.")
    })
    
    output$value <- renderText({ input$caption })
}

shinyApp(ui = ui, server = server)
