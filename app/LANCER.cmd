"C:/Program Files/R/R-4.0.0/bin/R.exe" -e "source('modules/check.R')"
powershell wget https://raw.githubusercontent.com/clementfarfait/transformers/main/version -OutFile modules/version.txt
for /f "tokens=*" %%A in (modules/version.txt) do ("C:/Program Files/R/R-4.0.0/bin/R.exe" -e "shiny::runApp('modules/module-%%A.R', launch.browser = TRUE)")
PAUSE