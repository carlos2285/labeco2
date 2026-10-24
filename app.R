# Laboratorio de Econometría — interfaz docente (R Shiny)
# Instalación: ejecutar instalar_paquetes.R una vez; después shiny::runApp()
suppressPackageStartupMessages({library(shiny); library(bslib); library(ggplot2); library(DT); library(readxl); library(forecast); library(urca); library(lmtest); library(tseries)})
options(shiny.maxRequestSize = 50*1024^2)
`%||%` <- function(x,y) if (is.null(x)||length(x)==0) y else x
numericas <- function(d) names(d)[vapply(d,is.numeric,logical(1))]
formato <- function(x) ifelse(is.finite(x),format(round(x,4),nsmall=4),"—")
serie <- function(d,v) { x <- suppressWarnings(as.numeric(d[[v]])); if(any(!is.finite(x))) stop("La variable debe ser numérica y no contener valores ausentes. Filtra o corrige los datos antes de estimar."); if(length(x)<25) stop("Se requieren al menos 25 observaciones."); x }
resultado_ur <- function(x,variable,transformacion,especificacion) {
  a <- urca::ur.df(x,type=especificacion,lags=min(12,max(1,floor(length(x)/5))),selectlags="AIC")
  p <- urca::ur.pp(x,type="Z-tau",model=if(especificacion=="trend") "trend" else "constant",lags="short")
  k <- urca::ur.kpss(x,type=if(especificacion=="trend") "tau" else "mu",lags="short")
  out <- data.frame(Variable=variable,Transformacion=transformacion,Prueba=c("ADF","Phillips-Perron","KPSS"),H0=c("Raíz unitaria","Raíz unitaria","Estacionariedad"),Estadistico=c(as.numeric(a@teststat[1]),as.numeric(p@teststat[1]),as.numeric(k@teststat[1])),Critico_5=c(as.numeric(a@cval[1,"5pct"]),as.numeric(p@cval[1,"5pct"]),as.numeric(k@cval[1,"5pct"])))
  out$Decision <- ifelse(c(out$Estadistico[1]<out$Critico_5[1],out$Estadistico[2]<out$Critico_5[2],out$Estadistico[3]>out$Critico_5[3]),"Rechazar H0","No rechazar H0")
  out$Interpretacion <- c(if(out$Decision[1]=="Rechazar H0") "Evidencia de estacionariedad" else "Sin evidencia suficiente de estacionariedad",if(out$Decision[2]=="Rechazar H0") "Evidencia de estacionariedad" else "Sin evidencia suficiente de estacionariedad",if(out$Decision[3]=="Rechazar H0") "Evidencia contra estacionariedad" else "No se rechaza estacionariedad")
  out
}
ui <- page_sidebar(
  title="Laboratorio de Econometría | Series de tiempo", theme=bs_theme(version=5,bootswatch="flatly",primary="#263C80"),
  sidebar=sidebar(width=320,
    fileInput("archivo","1. Cargar base de datos",accept=c(".csv",".xlsx",".xls")),
    helpText("CSV y Excel. Si no cargas un archivo puedes utilizar una base de demostración simulada."),
    conditionalPanel("input.archivo != null",uiOutput("hoja_ui")),
    selectInput("variable","2. Variable de análisis",choices=NULL),
    selectInput("fecha","Columna de fecha (opcional)",choices=c("Sin fecha"="")),
    selectInput("frecuencia","Frecuencia",choices=c("Mensual"=12,"Trimestral"=4,"Anual"=1),selected=12),
    selectInput("transformacion","Transformación",choices=c("Nivel"="nivel","Logaritmo"="log","Primera diferencia"="diff","Diferencia logarítmica × 100"="dlog")),
    hr(),helpText("Los cálculos se ejecutan localmente en R; los datos no se envían a un servidor por la aplicación.")),
  navset_card_tab(
    nav_panel("Explorar",br(),value_box(title="Observaciones",value=textOutput("nobs")),DTOutput("vista"),plotOutput("grafico",height=280),verbatimTextOutput("descriptivos")),
    nav_panel("Raíz unitaria",br(),selectInput("esp","Especificación determinística",choices=c("Constante"="drift","Constante y tendencia"="trend")),checkboxInput("ambas","Comparar nivel y primera diferencia",TRUE),actionButton("ejecutar_ur","Ejecutar ADF, PP y KPSS",class="btn-primary"),br(),br(),uiOutput("nota_ur"),DTOutput("tabla_ur"),downloadButton("descargar_ur","Descargar resultados CSV")),
    nav_panel("Regresión",br(),selectInput("xreg","Variables explicativas (X)",choices=NULL,multiple=TRUE),checkboxInput("constante","Incluir constante",TRUE),actionButton("ejecutar_lm","Estimar MCO",class="btn-primary"),br(),br(),verbatimTextOutput("salida_lm"),h5("Diagnóstico de residuos"),DTOutput("diag_lm"),plotOutput("res_lm",height=230)),
    nav_panel("ARIMA / ARIMAX",br(),checkboxInput("automatico","Seleccionar orden automáticamente mediante AICc",TRUE),fluidRow(column(4,numericInput("p","p",1,0,8)),column(4,numericInput("d","d",1,0,2)),column(4,numericInput("q","q",1,0,8))),checkboxInput("estacional","Permitir componente estacional en búsqueda automática",FALSE),selectInput("arimax_x","Regresores exógenos (ARIMAX)",choices=NULL,multiple=TRUE),numericInput("h","Horizonte de pronóstico (períodos)",12,min=1,max=120),actionButton("ejecutar_arima","Estimar modelo",class="btn-primary"),br(),br(),verbatimTextOutput("salida_arima"),DTOutput("diag_arima"),plotOutput("graf_arima",height=300),helpText("Para pronosticar un ARIMAX con regresores, se necesitarían sus valores futuros. Esta versión muestra ajuste y diagnóstico, sin pronóstico ARIMAX automático."),downloadButton("descargar_ajuste","Descargar valores ajustados")),
    nav_panel("Guía",br(),h3("Cómo usar este laboratorio"),p("1. Carga un Excel/CSV. Cada fila debe representar una observación y las series deben estar en columnas numéricas."),p("2. Selecciona tu variable y transformación. Revisa los gráficos y la estadística descriptiva."),p("3. Ejecuta las pruebas de raíz unitaria; ADF y PP tienen H0 de raíz unitaria, mientras KPSS tiene H0 de estacionariedad."),p("4. Estima regresiones o modelos ARIMA/ARIMAX; examina los diagnósticos antes de sacar conclusiones."),p("5. Descarga tus resultados y justifica las decisiones metodológicas."),hr(),h4("Alcance"),p("Esta primera versión implementa análisis descriptivo, ADF, PP, KPSS, MCO y diagnósticos, ARIMA y ARIMAX. No ejecuta todavía la cadena completa de ARDL, NARDL, VAR, VECM, cointegración, impulso-respuesta y robustez del proyecto de investigación original."),p("En modelos de series de tiempo debes verificar la frecuencia, la continuidad temporal, el orden de integración y la validez de los supuestos. La interfaz no reemplaza esas decisiones."))
  )
)
server <- function(input,output,session) {
  demo <- reactive({ set.seed(20261024); n<-192; w<-exp(cumsum(rnorm(n,0.004,0.045))+3.5); ipc<-exp(cumsum(0.002+0.08*c(0,diff(log(w)))+rnorm(n,0,0.004))+4.7); data.frame(fecha=format(seq(as.Date("2010-01-01"),by="month",length.out=n),"%Y-%m"),wti=round(w,2),ipc_general=round(ipc,3),ipc_transporte=round(ipc*exp(rnorm(n,0,0.015)),3)) })
  output$hoja_ui <- renderUI({req(input$archivo); if(grepl("\\.xlsx?$",input$archivo$name,ignore.case=TRUE)) selectInput("hoja","Hoja de Excel",choices=readxl::excel_sheets(input$archivo$datapath)) })
  datos <- reactive({
    if(is.null(input$archivo)) return(demo())
    ext<-tolower(tools::file_ext(input$archivo$name))
    if(ext%in%c("xls","xlsx")) {req(input$hoja); d<-as.data.frame(readxl::read_excel(input$archivo$datapath,sheet=input$hoja))}
    else if(ext=="csv") { d<-tryCatch(read.csv(input$archivo$datapath,check.names=FALSE),error=function(e) NULL); if(is.null(d)||ncol(d)<2) d<-read.csv2(input$archivo$datapath,check.names=FALSE); if(ncol(d)==1) { d2<-tryCatch(read.csv2(input$archivo$datapath,check.names=FALSE),error=function(e) NULL); if(!is.null(d2)&&ncol(d2)>1)d<-d2 } }
    else stop("Formato no compatible")
    validate(need(nrow(d)>0,"La base está vacía"),need(length(numericas(d))>0,"No se encontraron columnas numéricas")); d
  })
  observeEvent(datos(), {d<-datos(); vars<-numericas(d); updateSelectInput(session,"variable",choices=vars,selected=vars[1]); updateSelectInput(session,"fecha",choices=c("Sin fecha"="",names(d)),selected=if("fecha"%in%names(d)) "fecha" else ""); updateSelectInput(session,"xreg",choices=vars);updateSelectInput(session,"arimax_x",choices=vars)})
  preparado<-reactive({req(input$variable);d<-datos();validate(need(input$variable%in%names(d),"Selecciona una variable")); x<-serie(d,input$variable); if(input$transformacion=="log"||input$transformacion=="dlog") validate(need(all(x>0),"El logaritmo requiere valores estrictamente positivos")); if(input$transformacion=="log")x<-log(x); if(input$transformacion=="diff")x<-diff(x); if(input$transformacion=="dlog")x<-100*diff(log(x)); list(x=x,d=d,offset=length(d[[input$variable]])-length(x)) })
  output$nobs<-renderText({nrow(datos())});output$vista<-renderDT({datatable(head(datos(),100),options=list(scrollX=TRUE,pageLength=8))})
  output$grafico<-renderPlot({r<-preparado(); ggplot(data.frame(t=seq_along(r$x),y=r$x),aes(t,y))+geom_line(color="#263C80",linewidth=.8)+labs(x="Observación",y=input$variable,title=paste("Serie:",input$variable))+theme_minimal()})
  output$descriptivos<-renderPrint({x<-preparado()$x; print(summary(x));cat("Desviación estándar:",sd(x),"\nObservaciones:",length(x))})
  ur <- eventReactive(input$ejecutar_ur,{x<-preparado()$x; validate(need(length(x)>30,"Se requieren al menos 31 datos para realizar las pruebas")); tab<-resultado_ur(x,input$variable,input$transformacion,input$esp); if(isTRUE(input$ambas)){validate(need(length(x)>31,"Datos insuficientes"));tab<-rbind(tab,resultado_ur(diff(x),input$variable,paste0("Diferencia de ",input$transformacion),input$esp))};tab},ignoreInit=TRUE)
  output$nota_ur<-renderUI({req(ur()); tags$p(tags$b("Cómo leer:"),"ADF y PP rechazan H0 cuando el estadístico es menor que el crítico; KPSS rechaza H0 cuando es mayor. No rechazar H0 no demuestra que H0 sea verdadera.")})
  output$tabla_ur<-renderDT({datatable(ur(),options=list(scrollX=TRUE,pageLength=10))})
  output$descargar_ur<-downloadHandler(filename=function() "pruebas_raiz_unitaria.csv",content=function(file)write.csv(ur(),file,row.names=FALSE,fileEncoding="UTF-8"))
  lmfit<-eventReactive(input$ejecutar_lm,{req(input$variable); d<-datos(); x<-setdiff(input$xreg,input$variable); validate(need(length(x)>0,"Selecciona una o más variables explicativas")); vars<-c(input$variable,x); dd<-d[,vars,drop=FALSE]; dd<-dd[complete.cases(dd),,drop=FALSE];validate(need(nrow(dd)>length(x)+5,"No hay suficientes filas completas")); f<-reformulate(x,response=input$variable,intercept=input$constante); lm(f,data=dd)},ignoreInit=TRUE)
  output$salida_lm<-renderPrint({summary(lmfit())})
  output$diag_lm<-renderDT({m<-lmfit(); test<-list("Breusch-Pagan"=tryCatch(lmtest::bptest(m),error=function(e)NULL),"Breusch-Godfrey"=tryCatch(lmtest::bgtest(m,order=min(4,floor(length(residuals(m))/5))),error=function(e)NULL),"Jarque-Bera"=tryCatch(tseries::jarque.bera.test(residuals(m)),error=function(e)NULL));tab<-do.call(rbind,lapply(names(test),function(z)if(!is.null(test[[z]]))data.frame(Prueba=z,H0=switch(z,"Breusch-Pagan"="Homocedasticidad","Breusch-Godfrey"="Sin autocorrelación","Jarque-Bera"="Normalidad"),Estadistico=unname(test[[z]]$statistic),p_valor=test[[z]]$p.value,Decision=if(test[[z]]$p.value<.05)"Rechazar H0" else "No rechazar H0")));datatable(tab,options=list(dom="t",scrollX=TRUE))})
  output$res_lm<-renderPlot({plot(residuals(lmfit()),type="l",main="Residuos MCO",ylab="Residuo",xlab="Observación");abline(h=0,lty=2)})
  arma<-eventReactive(input$ejecutar_arima,{r<-preparado();d<-r$d;x<-r$x;xs<-setdiff(input$arimax_x,input$variable); validate(need(all(xs%in%numericas(d)),"Los regresores deben ser numéricos"));xx<-NULL;if(length(xs)){dd<-d[,xs,drop=FALSE];dd<-dd[(r$offset+1):nrow(dd),,drop=FALSE];validate(need(all(complete.cases(dd)),"Los regresores tienen valores faltantes"));xx<-as.matrix(dd)}; f<-as.integer(input$frecuencia); y<-ts(x,frequency=f);m<-if(isTRUE(input$automatico)) forecast::auto.arima(y,xreg=xx,seasonal=isTRUE(input$estacional),stepwise=TRUE,approximation=FALSE) else forecast::Arima(y,order=c(input$p,input$d,input$q),xreg=xx); list(modelo=m,x=x,xs=xs)},ignoreInit=TRUE)
  output$salida_arima<-renderPrint({print(summary(arma()$modelo))})
  output$diag_arima<-renderDT({a<-arma();m<-a$modelo;e<-as.numeric(residuals(m));lag<-min(24,max(1,floor(length(e)/5)));t<-Box.test(e,lag=lag,type="Ljung-Box",fitdf=sum(forecast::arimaorder(m)[c(1,3,4,6)]));datatable(data.frame(Prueba="Ljung-Box",H0="No autocorrelación residual",Rezago=lag,Estadistico=unname(t$statistic),p_valor=t$p.value,Decision=if(t$p.value<.05)"Rechazar H0" else "No rechazar H0"),options=list(dom="t"))})
  output$graf_arima<-renderPlot({a<-arma();m<-a$modelo;xx<-seq_along(a$x);plot(xx,a$x,type="l",col="#263C80",xlab="Observación",ylab="Valor",main="Observado vs. ajustado");lines(xx,as.numeric(fitted(m)),col="#D89C13",lwd=2);legend("topleft",c("Observado","Ajustado"),col=c("#263C80","#D89C13"),lty=1,bty="n");if(length(a$xs)==0){ fc<-forecast::forecast(m,h=input$h); n<-length(xx); lines((n+1):(n+length(fc$mean)),as.numeric(fc$mean),col="#1E806D",lwd=2,lty=2); legend("topright","Pronóstico",col="#1E806D",lty=2,bty="n") }})
  output$descargar_ajuste<-downloadHandler(filename=function()"arima_valores_ajustados.csv",content=function(file){a<-arma();write.csv(data.frame(observado=a$x,ajustado=as.numeric(fitted(a$modelo)),residuo=as.numeric(residuals(a$modelo))),file,row.names=FALSE)})
}
shinyApp(ui,server)
