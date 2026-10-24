paquetes <- c("shiny","bslib","ggplot2","DT","readxl","forecast","urca","lmtest","tseries")
faltantes <- setdiff(paquetes,rownames(installed.packages()))
if(length(faltantes)) install.packages(faltantes,repos="https://cloud.r-project.org")
cat("Paquetes listos. Ejecuta shiny::runApp() en la carpeta del laboratorio.\n")
