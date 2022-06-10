
##### Verification des packages necessaires

cat("\n************************************")
cat("\n[>>] Verification des packages..")
check_packages = read.table("https://raw.githubusercontent.com/clementfarfait/transformers/main/packages")
packages = as.vector(check_packages[,1])
if(length(setdiff(packages, rownames(installed.packages()))) > 0) {
    cat("\n ")
    install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org", lib="~/R/win-library/4.0") 
} else {
    library(crayon)
    cat(paste0("\n[",green("OK"),"] Tous correctement installes"))
}
cat("\n ")

##### Verification de mises a jour

version = read.table("modules/version.txt")
version = version$V1[1]
check_version = read.table("https://raw.githubusercontent.com/clementfarfait/transformers/main/version")
cat("\n[>>] Verification des mises a jour..")
if(check_version$V1[1] != version){
    cat(paste0("\n[",red("!!"),"] Une mise a jour est necessaire."))
    cat("\n[>>] Telechargement..\n \n")
    name = paste0("modules/module-",check_version,".R",sep="")
    download.file("https://raw.githubusercontent.com/clementfarfait/transformers/main/module.R",name)
    file.remove(paste0("modules/module-",version,".R"))
    cat(paste0("[",green("OK"),"] Mise a jour terminee."))
} else {
    cat(paste0("\n[",green("OK"),"] Aucune necessaire"))
}
cat("\n************************************\n \n")
