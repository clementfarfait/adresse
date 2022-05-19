version = read.table("version.txt")
version = version$V1[1]

options(shiny.maxRequestSize = 30*1024^2)
library(stringr)
library(readxl)
library(writexl)
library(crayon)

##### Code client

ui <- navbarPage(paste0("Transformers - ",version),
    tabPanel("Adresse"),
    
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
        
        replace <- read.table("https://raw.githubusercontent.com/clementfarfait/adresse/main/modifications", sep=";", header=TRUE)
        for(i in 1:length(replace$adresse)){
            replace[i,1] <- str_replace(replace[i,1], pattern = "apostrophe", replacement = "'")
        }
        for(i in 1:length(replace$correction)){
            replace[i,2] <- str_replace(replace[i,2], pattern = "vide", replacement = " ")
        }
        
        time <- system.time(
            withProgress(message = 'Traitement..', detail = paste0("[0%]"), value = 0, {
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
                        data[p,indice] <- stri_replace_all_regex(data[p,indice], pattern = as.character(replace$adresse), replacement = as.character(replace$correction), vectorize = FALSE)
                        setProgress(value = p/n, detail = paste0("[",round(p/n*100,1),"% - ",p,"]"))
                    }
                    
                    new_data <- write_xlsx(data[,indice], paste0("../modifications/",sep[1,1],"-",sep[2,1],".xlsx",sep=""))
                    cat(paste0("\nOK: modifications/",sep[1,1],"-",sep[2,1],".xlsx\n"))
                }
            })
        )
        
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
        
        print(paste0("Modifications terminées en ",time))
    })
    
    output$value <- renderText({ input$caption })
}

shinyApp(ui = ui, server = server)
