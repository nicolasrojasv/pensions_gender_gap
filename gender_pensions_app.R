# Shiny App Explorador de Datos Previsionales con foco de Género
#
# Esta shiny app muestra distintos indicadores para visualizar las diferencias de género en Previsión Social

# Limpiar ambiente
rm(list = ls())
options(scipen = 999)

#Librerías
sapply(c("shiny", "data.table", "tidyverse", "lubridate", "shinyWidgets", "shinyBS", "XLConnect", "gt",
         "shinythemes", "plotly"),
       require, character.only = T, quietly = T)

#Directorio
#setwd("")

#Leer los datos
poblacion <- fread("indicadores/input_dashboard/poblacion_celade.csv", dec= ",", na.strings = "NA")
afiliados <- fread("indicadores/input_dashboard/afiliados.csv", dec= ",", na.strings = "NA")
cotizantes <- fread("indicadores/input_dashboard/cotizantes.csv", dec= ",", na.strings = "NA")
saldo_prom <- fread("indicadores/input_dashboard/saldo_prom.csv", dec = ",", na.strings = "NA")
saldo_prom_edad <- fread("indicadores/input_dashboard/saldo_prom_edad.csv", dec = ",", na.strings = "NA")
pensiones_pagadas <- fread("indicadores/input_dashboard/pensiones_pagadas.csv", dec = ",", na.strings = "NA")
nuevos_pensionados <- fread("indicadores/input_dashboard/nuevos_pensionados.csv", dec = ",", na.strings = "NA")
serie_uf <- fread("indicadores/input_dashboard/serie_uf.csv", dec = ",", na.strings = "NA")
datos_sps <- fread("indicadores/input_dashboard/datos_sps.csv", dec = ",", na.strings = "NA", integer64 = "numeric")

###############
## FUNCIONES ##
###############

#transformar de uf a pesos
monto_a_pesos <- function(dt, columna_pesos, input_columna) {
    dt[, (columna_pesos) := get(input_columna)*uf]
}

#Transformar de pesos  uf
monto_a_uf <- function(dt, columna_uf, input_columna) {
  dt[, (columna_uf) := get(input_columna)/uf]
}

########
## UI ##
########

# Definir la interfaz para el Usuario
ui <- fluidPage(
    
    #Tema(color/disenio) de la APP
    theme=shinytheme("cerulean"),

    #Para que la APP se vea bien en telefonos moviles
    HTML('<meta name="viewport" content="width=1024">'),
    
    #Determinar el tamaño relativo de la barra de inputs y del menú principal
    tags$head(tags$style(HTML(".col-sm-4 { width: 25%;}
                               .col-sm-6 { width: 75%;}
                               #sidebar {background-color: #ECF7F7;}
                               body, label, input, button, select {font-family: Arial}"))),
    
    # Título de la APP
    #headerPanel(HTML("<b><center>Brechas de Género en el Sistema Previsional Chileno</b></center></br>")),
    titlePanel(
        fixedRow(
            column(3,img(src="pension.jfif",height = 115, width = 200, align="left"),
                   style={"vertical-align: middle; margin-left:40px"}),
            column(5,HTML("<b><center>Brechas de Género en el Sistema de Pensiones Chileno</b></br>
                   <i><h4>Sistema de Capitalización Individual y Sistema de Pensiones Solidarias</i></h4></center>"), 
                   style={"vertical-align: middle; margin-top:30px"}),
            column(4, img(height = 115, width = 115, src = "logo_subprev.png", align="right"),
                   style={"margin-right:0px"})
            
            #style = {"background-color:lightseagreen; color:white"}
        ),
        
    ),
    
    
    # Nota al pie para poner los responsables de la página
    tags$footer(
      
      HTML("Elaborado por la Unidad de Análisis de Políticas Previsionales de la Dirección de Estudios </br>
    Para dudas o comentarios de la visualización escribir a nicolas.rojas@previsionsocial.gob.cl o vania.martinez@previsionsocial.gob.cl."), align = "left", 
      style = "
              text-align: right;
              position:fixed;
              bottom:0;
              width:90%;
              font-size:13px;
              height:43px;   /* Height of the footer */
              color: gray;
              padding: 5px;
              background-color: white;
              z-index: 1000;"),

    # App
    #Dividir los indicadores de la APP en tabs
    tabsetPanel(type = "pills", id="indicador",
                tabPanel("Afiliados", fluid=TRUE),
                tabPanel("Cotizantes", fluid=TRUE),
                tabPanel("Pensionados", fluid=TRUE),
                tabPanel("Nuevos Pensionados", fluid = TRUE),
                tabPanel("Beneficiarios SPS", fluid=TRUE)),
    
    #Barra de inputs
    sidebarLayout(
        sidebarPanel(width = 3,
            
#####################
## PANEL AFILIADOS ##
#####################
            
            # Panel condicional para Afiliados
            conditionalPanel(condition = "input.indicador == 'Afiliados'",
                             
                             #Número, tasa, saldo 
                             selectInput(
                                 inputId = "ind_af",
                                 label = "Seleccione el indicador:", 
                                 choices = c("Número de afiliados", "Tasa de afiliación", 
                                             "Saldo promedio de la cuenta individual"),
                                 selected = "Número de afiliados"),
                             
                             #Panel condicional para número de afiliados, tasa de afiliación y saldo promedio
                             conditionalPanel(condition = paste("input.ind_af == 'Número de afiliados'","|",
                                                                "input.ind_af == 'Tasa de afiliación'"),
                                              
                                              #Tipo de afiliado
                                              pickerInput(
                                                  inputId = "tipo_af",
                                                  label = "Seleccione el tipo de afiliación:",
                                                  choices = str_to_title(unique(afiliados$tipo_afiliado)),
                                                  multiple = T),
                                              
                                              #Rango de años
                                              sliderInput(
                                                  inputId = "fecha_af", 
                                                  label = "Seleccione el rango de fechas:",
                                                  min = min(ymd(gsub("-", "", afiliados$fecha))),
                                                  max = max(ymd(gsub("-", "", afiliados$fecha))),
                                                  value = c(min(ymd(gsub("-", "", afiliados$fecha))), 
                                                            max(ymd(gsub("-", "", afiliados$fecha)))),
                                                  timeFormat = "%m-%Y"),
                                              
                             ),
                             
                             conditionalPanel(condition = "input.ind_af == 'Saldo promedio de la cuenta individual'",
                                              
                                              #Moneda
                                              radioGroupButtons(
                                                  inputId = "moneda_saldo", 
                                                  label = "Seleccione la moneda:",
                                                  choices = c("Pesos", "UF")),
                                              
                                              #Tramo edad
                                              pickerInput(
                                                  inputId = "edad_saldo", 
                                                  label = "Seleccione los tramos de edad:",
                                                  choices = rev(saldo_prom_edad[order(tramo_edad, decreasing = T),
                                                                                (unique(tramo_edad2))]),
                                                  multiple = T),
                                              
                                              #Rango de años
                                              sliderInput(
                                                  inputId = "fecha_saldo", 
                                                  label = "Seleccione el rango de fechas:",
                                                  min = min(ymd(gsub("-", "", saldo_prom_edad$fecha))),
                                                  max = max(ymd(gsub("-", "", saldo_prom_edad$fecha))),
                                                  value = c(min(ymd(gsub("-", "", saldo_prom_edad$fecha))), 
                                                            max(ymd(gsub("-", "", saldo_prom_edad$fecha)))),
                                                  timeFormat = "%m-%Y"),

                             ),
                             
            ),

######################
## PANEL COTIZANTES ##
######################

           #Panel condicional para cotizantes
           conditionalPanel(condition = "input.indicador == 'Cotizantes'",
                            
                            #número, tasa o ingreso imponible
                            selectInput(
                                inputId = "ind_cot",
                                label = "Seleccione el indicador:",
                                choices = c("Número de cotizantes", "Tasa de cotización",
                                            "Ingreso imponible promedio mensual"),
                                selected = "Número de cotizantes"),
                            
                            #Panel condicional para ingreso promedio
                            conditionalPanel(condition = "input.ind_cot == 'Ingreso imponible promedio mensual'",
                                             
                                             #Moneda
                                             radioGroupButtons(
                                                 inputId = "moneda_cot", 
                                                 label = "Seleccione la moneda:",
                                                 choices = c("Pesos", "UF")),
                            ),
                            
                            #Tipo de afiliacion
                            pickerInput(
                                inputId = "tipo_cot",
                                label = "Seleccione el tipo de cotización:",
                                choices = str_to_title(unique(cotizantes$tipo_cotizante)),
                                multiple = T),
                            
                            #Rango de años
                            sliderInput(
                                inputId = "fecha_cot", 
                                label = "Seleccione el rango de fechas:",
                                min = min(ymd(gsub("-", "", cotizantes$fecha))),
                                max = max(ymd(gsub("-", "", cotizantes$fecha))),
                                value = c(min(ymd(gsub("-", "", cotizantes$fecha))),
                                          max(ymd(gsub("-", "", cotizantes$fecha)))),
                                timeFormat = "%m-%Y"),
                            
            ),

#######################
## PANEL PENSIONADOS ##
#######################
            
            #Panel condicional para Pensiones
            conditionalPanel(condition = "input.indicador == 'Pensionados'",
                             
                             #indicadores
                             selectInput(
                                 inputId = "ind_pens",
                                 label = "Seleccione el indicador:",
                                 choices = c("Número de pensionados", "Tasa de pensionados", "Monto de pensión"),
                                 selected = "Número de pensionados"),
                             
                             #Panel condicional para pensión
                             conditionalPanel("input.ind_pens == 'Monto de pensión'",
                                              
                                              #Pension total o autofinanciada
                                              radioGroupButtons(
                                                  inputId = "total_o_auto",
                                                  label = h5(strong("¿Cual pensión quiere analizar?"),
                                                             tags$style(type = "text/css", 
                                                                        "#q1 {vertical-align: top;}"),
                                                             bsButton("q1", label = "", 
                                                                      icon = icon("question-circle"),
                                                                      size = "extra-small", style = "primary")), 
                                                  choices = c("Pensión total", "Pensión autofinanciada"),
                                                  status = "primary"),
                                              
                                              #Explicacion Pension autofinanciada y total
                                              bsPopover(id = "q1", title = "",
                                                        content = paste("La pensión autofinanaciada es aquella",
                                                                        "financiada exclusivamente con el saldo",
                                                                        "ahorrado del afiliado(a). La pensión total",
                                                                        "incorpora los beneficios del Estado",
                                                                        "como aquellos que entrega el Pilar",
                                                                        "Solidario.")
                                           ),
                                           
                                           #Promedio o mediana
                                           radioGroupButtons(
                                               inputId = "medida",
                                               label = "¿Analizar en base al promedio o la mediana?", 
                                               choices = c("Promedio", "Mediana"),
                                               status = "success"),
                                           
                                           #Moneda
                                           radioGroupButtons(
                                               inputId = "moneda", 
                                               label = "Seleccione la moneda:",
                                               choices = c("Pesos", "UF")),
                             
                             ),
                             
                             #Tipo de pensión
                             pickerInput(
                                 inputId = "tipo_pension",
                                 label = "Seleccione el tipo de pensión:",
                                 choices = unique(pensiones_pagadas$tipo_pension),
                                 multiple = T),
                             
                             #Años cotizados
                             pickerInput(
                                 inputId = "anios_cot", 
                                 label = "Seleccione los tramos de años cotizados:",
                                 choices = unique(pensiones_pagadas[order(aux_cot), 
                                                                    anios_cotizados]),
                                 multiple = T),
                             
                             #Rango de años
                             sliderInput(
                                 inputId = "fecha_pens",
                                 label = "Seleccione el rango de fechas:",
                                 min = min(ymd(gsub("-", "", pensiones_pagadas$fecha))),
                                 max = max(ymd(gsub("-", "", pensiones_pagadas$fecha))),
                                 value = c(min(ymd(gsub("-", "", pensiones_pagadas$fecha))),
                                           max(ymd(gsub("-", "", pensiones_pagadas$fecha)))),
                                 timeFormat = "%m-%Y"),
                             
            ),

##############################
## PANEL NUEVOS PENSIONADOS ##
##############################

            #Panel condicional para Pensiones
            conditionalPanel(condition = "input.indicador == 'Nuevos Pensionados'",
                             
                             #indicadores
                             selectInput(
                                 inputId = "ind_nuevos_pens",
                                 label = "Seleccione el indicador:",
                                 choices = c("Número de nuevos pensionados", "Monto de pensión"),
                                 selected = "Número de nuevos pensionados"),
                             
                             #Panel condicional para pensión
                             conditionalPanel("input.ind_nuevos_pens == 'Monto de pensión'",
                                              
                                              #Moneda
                                              radioGroupButtons(
                                                  inputId = "moneda_nuevos_pens", 
                                                  label = "Seleccione la moneda:",
                                                  choices = c("Pesos", "UF")),
                                              
                                              
                             ),
                             
                             #Años cotizados nuevos pensionados
                             pickerInput(
                                 inputId = "anios_cot_nuevos_pens", 
                                 label = "Seleccione los tramos de años cotizados:",
                                 choices = unique(nuevos_pensionados[order(aux_cot),
                                                                     anios_cotizados]),
                                 multiple = T),
                             
                             #Densidad de cotización nuevos pensionados
                             sliderInput(
                                 inputId = "densidad_nuevos_pens",
                                 label = "Seleccione el rango de densidad de cotización:",
                                 min = min(nuevos_pensionados$densidad_cot, na.rm = T),
                                 max = max(nuevos_pensionados$densidad_cot, na.rm = T),
                                 value = c(min(nuevos_pensionados$densidad_cot, na.rm = T),
                                           max(nuevos_pensionados$densidad_cot, na.rm = T))
                             ),
                             
                             #Rango de años
                             sliderInput(
                                 inputId = "fecha_nuevos_pens",
                                 label = "Seleccione el rango de fechas:",
                                 min = min(ymd(gsub("-", "", nuevos_pensionados$fecha))),
                                 max = max(ymd(gsub("-", "", nuevos_pensionados$fecha))),
                                 value = c(min(ymd(gsub("-", "", nuevos_pensionados$fecha))),
                                           max(ymd(gsub("-", "", nuevos_pensionados$fecha)))),
                                 timeFormat = "%m-%Y"),
                             
        ),

#############################
## PANEL BENEFICIARIOS SPS ##
#############################

#Panel condicional para cotizantes
conditionalPanel(condition = "input.indicador == 'Beneficiarios SPS'",
                 
                 #número o monto beneficio
                 selectInput(
                   inputId = "ind_sps",
                   label = "Seleccione el indicador:",
                   choices = c("Número de beneficiarios",
                               "Monto promedio de beneficio"),
                   selected = "Número de beneficiarios"),
                 
                 #Panel condicional para ingreso promedio
                 conditionalPanel(condition = "input.ind_sps == 'Monto promedio de beneficio'",
                                  
                                  #Moneda
                                  radioGroupButtons(
                                    inputId = "moneda_sps", 
                                    label = "Seleccione la moneda:",
                                    choices = c("Pesos", "UF")),
                 ),
                 
                 #Tipo de beneficio
                 pickerInput(
                   inputId = "tipo_benef",
                   label = "Seleccione el tipo de beneficio:",
                   choices = unique(datos_sps$tipo_beneficio),
                   multiple = T),
                 
                 #Región en que se concedió el beneficio
                 pickerInput(
                   inputId = "region_benef",
                   label = "Seleccione la región en la que se concedió el beneficio:",
                   choices = str_to_title(unique(datos_sps$region)),
                   multiple = T),
                 
                 #Rango de años
                 sliderInput(
                   inputId = "fecha_benef",
                   label = "Seleccione el rango de fechas:",
                   min = min(ymd(gsub("-", "", datos_sps$fecha))),
                   max = max(ymd(gsub("-", "", datos_sps$fecha))),
                   value = c(min(ymd(gsub("-", "", datos_sps$fecha))),
                             max(ymd(gsub("-", "", datos_sps$fecha)))),
                   timeFormat = "%m-%Y"),
            
                 ),

#######################
## BOTÓN DE DESCARGA ##
#######################

    #Descarga de datos
    h5(strong(tags$div(
      tags$span(#style = "color:#000000", 
        "¿Necesita hacer su propio análisis?")))),
    
    downloadButton(
      outputId = "descargar_base", 
      label = "Descargar la base de datos"),

    ),

#####################
## PANEL PRINCIPAL ##
#####################
        
        mainPanel(
          
          tabsetPanel(
            
          
##############                        
## GRÁFICOS ##
##############
            
            #Graficar Indicador
            tabPanel("Gráficos",
                     
                     br(), #espacio 
                     splitLayout(
                       cellWidths = c("50%", "50%"), 
                       plotlyOutput("serie_genero"),
                       plotlyOutput("serie_brecha")
                     ),
                     
                     #Descarga de datos usados en el gráfico
                     downloadButton(
                       outputId = "descargar_datos_graficos", label = "Descargar datos de los gráficos"),
                     
                     #Notas y fuentes
                     hr(), #linea horizontal
                     
                     HTML("<b><i><h5>Fuentes:</br></i></b>
                             1) Datos del sistema de pensiones provenientes de la Superintendencia de Pensiones.</br>
                             2) Datos poblacionales según estimaciones de CELADE.</br>
                             3) Datos de precios en UF obtenidos de la base de datos del Banco Central.</br></h5>"),
                     HTML("<i><b><h5>Nota:</br></i></b>
                             La construcción de la brecha de género depende de si el indicador corresponde a una tasa o un número o monto:</br>
                             -Si es una tasa: la brecha será igual a la diferencia simple de hombres menos mujeres, indicando cuánto están los hombres por sobre las mujeres.</br>
                             -Si es un número o monto: la brecha será igual a la diferencia porcentual de hombres sobre mujeres, indicando cuánto están los hombres por sobre las mujeres.</h5>"),
                     
                     
                     ),

####################
## TABLA DE DATOS ##
####################

#            tabPanel("Tabla Estadística",
#                     br(), #espacio
#                     div(style = "display:inline-block", 
#                         uiOutput("anio_tabla")),
                     
#                     div(style = "display:inline-block", 
#                         uiOutput("mes_tabla")),
                     
                     #Tabla de datos
#                     gt_output("tabla_estadistica")
                     
#                     ),

           ),
            
        )
    ),

)

############
## SERVER ##
############

# Definir el server
server <- function(input, output) {
    
###############################
## DESCARGA DE BASE DE DATOS ##
###############################
    
    #Descargar todos los datos
    all_data <- reactive({
        
        if(input$indicador == "Afiliados") {
            
            if (input$ind_af != "Saldo promedio de la cuenta individual") {
                
                #datos
                afiliados_poblacion <- merge(afiliados, poblacion[,.(fecha, sexo, poblacion_adultos)],
                                             by = c("fecha", "sexo"), all.x = T)
                
              
            } else {
                
                #datos
                saldo_uf <- merge(saldo_prom_edad[,.(fecha, edad, tramo_edad = tramo_edad2, sexo, num_afiliados,
                                                     saldo_prom_pesos = saldo_prom_miles_pesos*1000)],
                                  serie_uf, by = "fecha", all.x = T)
                
                #nombre de la columna en pesos
                nombre_columna <- names(saldo_uf)[6]
                
                #Crear columna en uf
                monto_a_uf(saldo_uf, gsub("pesos", "uf", nombre_columna), nombre_columna)
                
            }
          
        } else if (input$indicador == "Cotizantes") {
          
          #datos
          cotizantes_poblacion <- merge(merge(cotizantes[,.(fecha, sexo, afp, tipo_cotizante, 
                                                           ing_prom_pesos = ing_prom, num_cotizantes)], 
                                              poblacion[,.(fecha, sexo, poblacion_edad_trabajar)],
                                        by = c("fecha", "sexo"), all.x = T),
                                        serie_uf, by = "fecha", all.x = T)
          
          #nombre de la columna en pesos
          nombre_columna <- names(cotizantes_poblacion)[5]
          
          #Crear las variables de ingreso promedio en pesos
          monto_a_uf(cotizantes_poblacion, gsub("pesos", "uf", nombre_columna), nombre_columna)
            
        } else if (input$indicador == "Pensionados") {
          
          #datos
          pensiones_poblacion <- merge(merge(pensiones_pagadas[,.(fecha, anio = year(fecha), sexo, anios_cot,
                                                                  anios_cotizados, numero_pensionados, tipo_pension, 
                                                                  pension_autofinanciada_promedio_uf,
                                                                  pension_total_promedio_uf,
                                                                  pension_autofinanciada_mediana_uf,
                                                                  pension_total_mediana_uf)], 
                                             unique(poblacion[,.(anio = year(fecha), sexo,
                                                                 poblacion_edad_jubilacion)]),
                                             by = c("anio", "sexo"), all.x = T),
                                       serie_uf, by = "fecha", all.x = T)
          pensiones_poblacion[, anio := NULL]
          
          #Vector con los nombres de las columnas de pensión en uf
          nombres <- names(pensiones_poblacion)[7:10]
          
          #Crear las variables de montos de pensión en pesos
          monto_a_pesos(monto_a_pesos(monto_a_pesos(monto_a_pesos(
            pensiones_poblacion, gsub("uf", "pesos", nombres[1]), nombres[1]),
            gsub("uf", "pesos", nombres[2]), nombres[2]),
            gsub("uf", "pesos", nombres[3]), nombres[3]),
            gsub("uf", "pesos", nombres[4]), nombres[4])
            
        } else if (input$indicador == "Nuevos Pensionados") {
          
          #datos
          nuevos_pensionados_uf <- merge(nuevos_pensionados[,.(fecha, sexo, anios_cotizados, densidad_cot, numero,
                                                               pension_autofinanciada_uf = pension_autofinanciada)],
                                         serie_uf, by = "fecha", all.x = T)
          
          #nombre columna pensión
          nombre <- names(nuevos_pensionados_uf)[6]
          
          #Crear las varibles montos de pensión en pesos
          monto_a_pesos(nuevos_pensionados_uf, gsub("uf", "pesos", nombre), nombre)
          
        } else if (input$indicador == "Beneficiarios SPS") {
          
          #datos
          sps_uf <- merge(datos_sps[,.(fecha, region, tipo_pension, tipo_beneficio, sexo, num_beneficiarios,
                                       monto_beneficio_pesos = monto_beneficio)], 
                          serie_uf, by = "fecha", all.x = T)
          
          #nombre columna pesos
          nombre <- names(sps_uf)[7]
          
          #Crear monto del beneficio en uf
          monto_a_uf(sps_uf, gsub("pesos", "uf", nombre), nombre)
          
        }
    })
    
    #Nombre datos descargados
    nombre_datos <- reactive({
        
        if(input$indicador == "Afiliados") {
            
            if (input$ind_af != "Saldo promedio de la cuenta individual") {
                
                #nombre
                nombre <- "Afiliados"
                
            } else {
                
                #nombre
                nombre <- "Saldo Promedio"
                
            }
          
        } else {
          
          nombre <- input$indicador
          
        }
    })
    
    #Descargar datos
    output$descargar_base <- downloadHandler(
      
      filename = function() {paste0(nombre_datos(),".xlsx")},
      content = function(file) {
        
        #Crear nombre ficticio del excel
        fname <- paste(file, "xlsx", sep = ".")
        
        #Crear excel vacio
        wb <- loadWorkbook(fname, create = T)
        
        #Se crea la hoja en donde se guardan los datos y se guarda el excel
        createSheet(wb, name = nombre_datos())
        writeWorksheet(wb, 
                       all_data()[, fecha := gsub("-01${1}", "", as.character(fecha))],
                       sheet = nombre_datos())
        saveWorkbook(wb)
        file.rename(fname, file)
      } 
    )
    
######################
## FILTROS DE DATOS ##
######################
    
    #Filtrar los datos
    datos <- reactive({
        
        if (input$indicador == "Afiliados") {
            
            if (input$ind_af == "Número de afiliados") {
                
                #Filtrar datos
                af_filtrado <- afiliados[ymd(gsub("-", "", fecha)) >= input$fecha_af[1] 
                                         & ymd(gsub("-", "", fecha)) <= input$fecha_af[2]
                                         & grepl(paste(input$tipo_af, collapse = "|"), str_to_title(tipo_afiliado)),
                                         .(num_afiliados = sum(num_afiliados)), 
                                         by = .(fecha, sexo)][!grepl("s/i", sexo)]
                
                #Pasar la tabla de ancho a largo
                af_filtrado <- dcast(af_filtrado, formula = fecha ~ sexo, value.var = "num_afiliados")
                af_filtrado[, brecha := (hombres/mujeres - 1)*100]
                
            } else if (input$ind_af == "Tasa de afiliación") {
                
                #Filtrar datos
                tasa_af_filtrado <- afiliados[ymd(gsub("-", "", fecha)) >= input$fecha_af[1] 
                                              & ymd(gsub("-", "", fecha)) <= input$fecha_af[2]
                                              & grepl(paste(input$tipo_af, collapse = "|"),
                                                      str_to_title(tipo_afiliado)),
                                              .(num_afiliados = sum(num_afiliados)), 
                                              by = .(fecha, sexo)][!grepl("s/i", sexo)]
                
                #Pegar datos poblacionales
                tasa_af_filtrado <- merge(tasa_af_filtrado, poblacion, by = c("fecha","sexo"), all.x = T)
                
                #Crear tasa de afiliado
                tasa_af_filtrado[, tasa_afil := num_afiliados/poblacion_adultos*100]
                
                #Pasar la tasas de largo a ancho para obtener brecha
                tasa_af_filtrado <- dcast(tasa_af_filtrado[,.(fecha, sexo, tasa_afil)],
                                          formula = fecha ~ sexo, value.var = "tasa_afil")
                
                #Crear la brecha de genero
                tasa_af_filtrado[, brecha := hombres - mujeres]
                
            } else if (input$ind_af == "Saldo promedio de la cuenta individual") {
                
                #Filtrar datos
                saldo <- saldo_prom_edad[ymd(gsub("-", "", fecha)) >= input$fecha_saldo[1] 
                                         & ymd(gsub("-", "", fecha)) <= input$fecha_saldo[2]
                                         & grepl(paste(input$edad_saldo, collapse = "|"), tramo_edad2),
                               .(saldo_pesos = sum(saldo_prom_miles_pesos*(num_afiliados/sum(num_afiliados)))), 
                               by = .(fecha, sexo)][!grepl("s/i", sexo)]
                saldo[, saldo_pesos := saldo_pesos*1000]
                
                #Pegar datos de UF
                datos_uf <- copy(serie_uf)
                saldo <- merge(saldo, datos_uf[, fecha := as.Date(paste(fecha, "01", sep = "-"))],
                               by = "fecha", all.x = T)
                
                #Variable en UF
                saldo[, saldo_uf := saldo_pesos/uf]
                
                #Crear una variable UF o Pesos
                saldo <- melt(saldo, id.vars = c("fecha", "sexo", "uf"), variable.name = "moneda", 
                              value.name = "saldo")
                saldo[, moneda := ifelse(gsub("saldo_", "", moneda) == "pesos", "Pesos", "UF")]
                
                #Filtrar la moneda
                saldo <- saldo[moneda == input$moneda_saldo,.(fecha, sexo, saldo)]
                
                #Transformar la tabla de largo a ancho para crear la brecha
                saldo <- dcast(saldo, formula = fecha ~ sexo, value.var = "saldo")
                
                #Crear brecha
                saldo[, brecha := (hombres/mujeres - 1)*100]
            }
            
        } else if (input$indicador == "Cotizantes") {
            
            if (input$ind_cot == "Número de cotizantes") {
                
                #Filtrar datos
                cot_filtrado <- copy(cotizantes)
                cot_filtrado <- cot_filtrado[ymd(gsub("-", "", fecha)) >= input$fecha_cot[1] 
                                           & ymd(gsub("-", "", fecha)) <= input$fecha_cot[2]
                                           & grepl(paste(input$tipo_cot, collapse = "|"),
                                                   str_to_title(tipo_cotizante)),
                                           .(num_cotizantes = sum(num_cotizantes)), 
                                           by = .(fecha, sexo)][!grepl("s/i", sexo)]
                
                #Pasar la tabla de ancho a lo largo
                cot_filtrado <- dcast(cot_filtrado, formula = fecha ~ sexo, value.var = "num_cotizantes")
                cot_filtrado[, brecha := (hombres/mujeres - 1)*100]
                
            } else if (input$ind_cot == "Tasa de cotización") {
                
                #Filtrar datos
                tasa_cot_filtrado <- copy(cotizantes)
                tasa_cot_filtrado <- tasa_cot_filtrado[ymd(gsub("-", "", fecha)) >= input$fecha_cot[1] 
                                                       & ymd(gsub("-", "", fecha)) <= input$fecha_cot[2]
                                                       & grepl(paste(input$tipo_cot, collapse = "|"), 
                                                               str_to_title(tipo_cotizante)),
                                                       .(num_cotizantes = sum(num_cotizantes)), 
                                                       by = .(fecha, sexo)][!grepl("s/i", sexo)]
                
                #Pegar datos poblacionales
                tasa_cot_filtrado <- merge(tasa_cot_filtrado, poblacion, by = c("fecha","sexo"), all = F)
                
                #Crear tasa de afiliado
                tasa_cot_filtrado[, tasa_cot := num_cotizantes/poblacion_edad_trabajar*100]
                
                #Pasar la tasas de largo a ancho para obtener brecha
                tasa_cot_filtrado <- dcast(tasa_cot_filtrado[,.(fecha, sexo, tasa_cot)],
                                           formula = fecha ~ sexo, value.var = "tasa_cot")
                
                #Crear la brecha de género
                tasa_cot_filtrado[, brecha := hombres - mujeres]
                
            } else if (input$ind_cot == "Ingreso imponible promedio mensual") {
                
                #Filtrar datos
                ing_prom <- copy(cotizantes)
                ing_prom <- ing_prom[ymd(gsub("-", "", fecha)) >= input$fecha_cot[1] 
                               & ymd(gsub("-", "", fecha)) <= input$fecha_cot[2]
                               & grepl(paste(input$tipo_cot, collapse = "|"), str_to_title(tipo_cotizante)),
                               .(ing_prom_pesos = sum(ing_prom*(num_cotizantes/sum(num_cotizantes)))), 
                               by = .(fecha, sexo)][!grepl("s/i", sexo)]
                
                #Pegar datos de UF
                datos_uf <- copy(serie_uf)
                ing_prom <- merge(ing_prom, datos_uf[, fecha := as.Date(paste(fecha, "01", sep = "-"))],
                                  by = "fecha", all.x = T)
                
                #Variable en UF
                ing_prom[, ing_prom_uf := ing_prom_pesos/uf]
                
                #Crear una variable UF o Pesos
                ing_prom <- melt(ing_prom, id.vars = c("fecha", "sexo", "uf"), variable.name = "moneda",
                                 value.name = "ing_prom")
                ing_prom[, moneda := ifelse(gsub("ing_prom_", "", moneda) == "pesos", "Pesos", "UF")]
                
                #Filtrar la moneda
                ing_prom <- ing_prom[moneda == input$moneda_cot,.(fecha, sexo, ing_prom)]
                
                #Transformar la tabla de largo a ancho para crear la brecha
                ing_prom <- dcast(ing_prom, formula = fecha ~ sexo, value.var = "ing_prom")
                
                #Crear brecha
                ing_prom[, brecha := (hombres/mujeres - 1)*100]
                
            }
            
        } else if (input$indicador == "Pensionados") {
          
          if (input$ind_pens == "Número de pensionados") {
            
            #Filtrar datos
            n_pens <- pensiones_pagadas[ymd(gsub("-", "", fecha)) >= input$fecha_pens[1]
                                        & ymd(gsub("-", "", fecha)) <= input$fecha_pens[2]
                                        & grepl(paste(input$anios_cot, collapse = "|"), anios_cotizados)
                                        & grepl(paste(input$tipo_pension, collapse = "|"), tipo_pension),
                                        .(numero_pensionados = sum(numero_pensionados)),
                                        by = .(fecha, sexo)][!grepl("s/i", sexo)]
            
            #Pasar la tabla de ancho a largo
            n_pens <- dcast(n_pens, formula = fecha ~ sexo, value.var = "numero_pensionados")
            
            #Crear brecha
            n_pens[, brecha := (hombres/mujeres - 1)*100]
            
          } else if (input$ind_pens == "Tasa de pensionados") {
            
            #Filtrar datos
            tasa_pens <- pensiones_pagadas[ymd(gsub("-", "", fecha)) >= input$fecha_pens[1]
                                   & ymd(gsub("-", "", fecha)) <= input$fecha_pens[2]
                                   & grepl(paste(input$anios_cot, collapse = "|"), anios_cotizados)
                                   & grepl(paste(input$tipo_pension, collapse = "|"), tipo_pension),
                                   .(numero_pensionados = sum(numero_pensionados)),
                                   by = .(fecha, sexo)][!grepl("s/i", sexo)]
            
            #Pegar datos poblacionales
            tasa_pens <- merge(tasa_pens[,.(anio = year(fecha), fecha, sexo, numero_pensionados)], 
                               unique(poblacion[,.(anio = year(fecha), sexo, poblacion_edad_jubilacion)]),
                               by = c("anio","sexo"), all.x = T)
            
            #Crear tasa de afiliado
            tasa_pens[, tasa_pensionados := numero_pensionados/poblacion_edad_jubilacion*100]
            
            #Pasar la tasas de largo a ancho para obtener brecha
            tasa_pens <- dcast(tasa_pens[,.(fecha, sexo, tasa_pensionados)],
                               formula = fecha ~ sexo, value.var = "tasa_pensionados")
            
            #Crear la brecha de género
            tasa_pens[, brecha := hombres - mujeres]
            
          } else if (input$ind_pens == "Monto de pensión") {
            
            #nombres de las columnas de montos de pension en uf
            nombres <- names(pensiones_pagadas)[c(5,6,8,9)]
            
            #Unir pensiones pagadas con la uf
            monto_pension <- merge(pensiones_pagadas, serie_uf, by = "fecha", all.x = T)
            
            #Crear montos de pensión en pesos
            monto_pension <- monto_a_pesos(monto_a_pesos(monto_a_pesos(monto_a_pesos(
              monto_pension, gsub("uf", "Pesos", nombres[1]), nombres[1]),
              gsub("uf", "Pesos", nombres[2]), nombres[2]),
              gsub("uf", "Pesos", nombres[3]), nombres[3]),
              gsub("uf", "Pesos", nombres[4]), nombres[4])
            
            #Pasar la tabla de ancho a lo largo para dejar los montos de pensión en una columna
            monto_pension <- melt(monto_pension, measure.vars = patterns("pension_"), variable.name = "datos",
                                  value.name = "monto_pension")
            
            #Separar columna que tiene los atributos de los montos de pensión
            monto_pension <- separate(monto_pension, col = "datos",
                                      into = c("pension","financiamiento", "medida", "moneda"), sep = "_")
            
            #Modificar los atributos para calzarlos con los filtros del UI
            monto_pension[, ":="(pension = str_to_title(gsub("o", "ó", pension)),
                                 medida = str_to_title(medida),
                                 moneda = fcase(moneda == "uf", "UF",  rep_len(TRUE, length(moneda)), moneda))]
            
            #Unir columnas para asimilarla al filtro en el UI
            monto_pension <- unite(monto_pension, col = "financiamiento", c("pension", "financiamiento"), sep = " ", 
                                   remove = T)
            
            #Filtrar datos
            monto_pension <- monto_pension[ymd(gsub("-", "", fecha)) >= input$fecha_pens[1]
                                           & ymd(gsub("-", "", fecha)) <= input$fecha_pens[2]
                                           & grepl(paste(input$anios_cot, collapse = "|"), anios_cotizados)
                                           & grepl(paste(input$tipo_pension, collapse = "|"), tipo_pension)
                                           & financiamiento == input$total_o_auto
                                           & medida == input$medida & moneda == input$moneda,
                                           .(pension = sum(monto_pension*numero_pensionados/sum(numero_pensionados))),
                                           by = .(fecha, sexo)][!grepl("s/i", sexo)]
            
            #Pasar la tabla del largo a lo ancho para graficar y obtener la brecha
            monto_pension <- dcast(monto_pension, formula = fecha ~ sexo, value.var = "pension")
            
            #Brecha
            monto_pension[, brecha := (hombres/mujeres - 1)*100]
            
          }
          
        } else if (input$indicador == "Nuevos Pensionados") {
          
          if (input$ind_nuevos_pens == "Número de nuevos pensionados") {
            
            #Filtrar datos
            nuevos_pens <- nuevos_pensionados[ymd(gsub("-", "", fecha)) >= input$fecha_nuevos_pens[1] 
                                              & ymd(gsub("-", "", fecha)) <= input$fecha_nuevos_pens[2]
                                              & grepl(paste(input$anios_cot_nuevos_pens, collapse = "|"),
                                                      anios_cotizados)
                                              & densidad_cot >= input$densidad_nuevos_pens[1] 
                                              & densidad_cot <= input$densidad_nuevos_pens[2],
                                              .(num_pensionados = sum(numero)), 
                                              by = .(fecha, sexo)][!grepl("s/i", sexo)]
            
            #Pasar la tabla de ancho a lo largo
            nuevos_pens <- dcast(nuevos_pens, formula = fecha ~ sexo, value.var = "num_pensionados")
            nuevos_pens[, brecha := (hombres/mujeres - 1)*100]
            
          } else if (input$ind_nuevos_pens == "Monto de pensión") {
            
            #Filtrar datos
            nuevos_pens <- nuevos_pensionados[ymd(gsub("-", "", fecha)) >= input$fecha_nuevos_pens[1] 
                                              & ymd(gsub("-", "", fecha)) <= input$fecha_nuevos_pens[2]
                                              & grepl(paste(input$anios_cot_nuevos_pens, collapse = "|"),
                                                      anios_cotizados)
                                              & densidad_cot >= input$densidad_nuevos_pens[1] 
                                              & densidad_cot <= input$densidad_nuevos_pens[2],
                                              .(pension_uf = sum(pension_autofinanciada*(numero/sum(numero)))), 
                                              by = .(fecha, sexo)][!grepl("s/i", sexo)]
            
            #Pegar datos de UF
            nuevos_pens <- merge(nuevos_pens, serie_uf, by = "fecha", all.x = T)
            
            #Variable en pesos
            nuevos_pens[, pension_pesos := pension_uf*uf]
            
            #Crear una variable UF o Pesos
            nuevos_pens <- melt(nuevos_pens, id.vars = c("fecha", "sexo", "uf"), variable.name = "moneda",
                                value.name = "pension")
            nuevos_pens[, moneda := ifelse(gsub("pension_", "", moneda) == "pesos", "Pesos", "UF")]
            
            #Filtrar la moneda
            nuevos_pens <- nuevos_pens[moneda == input$moneda_nuevos_pens,.(fecha, sexo, pension)]
            
            #Transformar la tabla de largo a ancho para crear la brecha
            nuevos_pens <- dcast(nuevos_pens, formula = fecha ~ sexo, value.var = "pension")
            
            #Crear brecha
            nuevos_pens[, brecha := (hombres/mujeres - 1)*100]
            
          }
          
        } else if (input$indicador == "Beneficiarios SPS") {
          
          if (input$ind_sps== "Número de beneficiarios") {
            
            #Filtrar datos
            sps_filtrado <- copy(datos_sps)
            sps_filtrado <- sps_filtrado[ymd(gsub("-", "", fecha)) >= input$fecha_benef[1] 
                                         & ymd(gsub("-", "", fecha)) <= input$fecha_benef[2]
                                         & grepl(paste(input$tipo_benef, collapse = "|"),
                                                 tipo_beneficio) 
                                         & grepl(paste(input$region_benef, collapse = "|"),
                                                 str_to_title(region)),
                                         .(num_beneficiarios = sum(num_beneficiarios)), 
                                         by = .(fecha, sexo)][!grepl("s/i", sexo)]
            
            #Pasar la tabla de ancho a lo largo
            sps_filtrado <- dcast(sps_filtrado, formula = fecha ~ sexo, value.var = "num_beneficiarios")
            sps_filtrado[, brecha := (hombres/mujeres - 1)*100]
            
          } else if (input$ind_sps == "Monto promedio de beneficio") {
            
            #Filtrar datos
            benef_prom <- copy(datos_sps)
            benef_prom <- benef_prom[ymd(gsub("-", "", fecha)) >= input$fecha_benef[1] 
                                     & ymd(gsub("-", "", fecha)) <= input$fecha_benef[2]
                                     & grepl(paste(input$tipo_benef, collapse = "|"), 
                                             tipo_beneficio)
                                     & grepl(paste(input$region_benef, collapse = "|"),
                                             str_to_title(region)),
                                     .(monto_prom_pesos = sum(monto_beneficio)/sum(num_beneficiarios)), 
                                     by = .(fecha, sexo)][!grepl("s/i", sexo)]
            
            #Pegar datos de UF
            datos_uf <- copy(serie_uf)
            benef_prom <- merge(benef_prom, datos_uf[, fecha := as.Date(paste(fecha, "01", sep = "-"))],
                                by = "fecha", all.x = T)
            
            #Variable en UF
            benef_prom[, monto_prom_uf := monto_prom_pesos/uf]
            
            #Crear una variable UF o Pesos
            benef_prom <- melt(benef_prom, id.vars = c("fecha", "sexo", "uf"), variable.name = "moneda",
                               value.name = "monto_prom")
            benef_prom[, moneda := ifelse(gsub("monto_prom_", "", moneda) == "pesos", "Pesos", "UF")]
            
            #Filtrar la moneda
            benef_prom <- benef_prom[moneda == input$moneda_sps,.(fecha, sexo, monto_prom)]
            
            #Transformar la tabla de largo a ancho para crear la brecha
            benef_prom <- dcast(benef_prom, formula = fecha ~ sexo, value.var = "monto_prom")
            
            #Crear brecha
            benef_prom[, brecha := (hombres/mujeres - 1)*100]
            
          }
          
        }

    })
    
####################
## GRÁFICO GÉNERO ##
####################
    
    #Variable auxiliar para el nombre del gráfico
    nombre_grafico <- reactive({
        
        if (input$indicador == "Afiliados") {
            
            nombre_grafico <- input$ind_af
            
        } else if (input$indicador == "Cotizantes") {
            
            nombre_grafico <- input$ind_cot
            
        } else if (input$indicador == "Pensionados") {
          
          nombre_grafico <- input$ind_pens
          
        } else if (input$indicador == "Nuevos Pensionados") {
          
          nombre_grafico <- input$ind_nuevos_pens
          
        } else if (input$indicador == "Beneficiarios SPS") {
          
          nombre_grafico <- input$ind_sps
        }
    })
    
    #Seleccionar mínimo para el eje y de la brecha
    min_y <- reactive(
        
        if (min(datos()$brecha, na.rm = T) >= 0) {
            
            0
                
        } else {
            
            1.2
        }
    )
    
    #Seleccionar maximo para el eje y de la brecha
    max_y <- reactive(
      
      if (max(datos()$brecha, na.rm = T) >= 0) {
        
        1.2
        
      } else {
        
        0
        
      }
    )
    
    
    #gráfico de número, tasa o ingreso 
    output$serie_genero <- renderPlotly({
        
        h1 <- plot_ly(datos(),type = 'scatter', mode = 'lines') %>%
            add_lines(x=~fecha, y=~mujeres, name="Mujeres", line=list(color='#4B82D4'),
                      hovertemplate = ~paste0(month(ymd(fecha), label = T, abbr = F), " de ", year(ymd(fecha)),
                                              "<br>", nombre_grafico(),": %{y:,}")) %>% 
            add_lines(x=~fecha, y=~hombres, name="Hombres", line=list(color='#4BD4B9'),
                      hovertemplate = ~paste0(month(ymd(fecha), label = T, abbr = F), " de ", year(ymd(fecha)),
                                              "<br>", nombre_grafico(),": %{y:,}")) %>% 
            layout(title = paste(nombre_grafico(),"según sexo"),
                   xaxis = list(title = ""),
                   yaxis = list(tickfont= list(color = '#1f77b4', size=11), color='#1f77b4', 
                                range=c(0,1.2*max(datos()$hombres,datos()$mujeres)),
                                title=nombre_grafico()),
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, y =-0.3))%>%
            config(locale = "es", modeBarButtonsToRemove = list('sendDataToCloud', 'zoom3d','zoom2d','autoScale2d',
                                                                'hoverClosestCartesian',
                                                                'hoverCompareCartesian'), displaylogo=FALSE)
        
        ggplotly(h1,dynamicTicks = TRUE)
    })
    
    #gráfico para la brecha
    output$serie_brecha <- renderPlotly({
        
        h2 <- plot_ly(datos(),type = 'scatter', mode = 'lines') %>%
            add_lines(x=~fecha, y=~brecha, name="Brecha (%)", line=list(color="F55E5E"),
                      hovertemplate = ~paste0(month(ymd(fecha), label = T, abbr = F), " de ", year(ymd(fecha)),
                                              "<br>", "Brecha: %","{y:.2f}%<extra></extra>")) %>%
            layout(title = paste("Brecha de género"),
                   xaxis = list(title = ""),
                   yaxis = list(tickfont= list(color = '#ff7f0e', size=11), color='#ff7f0e', 
                                range=c(min_y()*min(datos()$brecha),max_y()*max(datos()$brecha)),
                                title = "Brecha (%)"),
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, y =-0.3))%>%
            config(locale = "es", modeBarButtonsToRemove = list('sendDataToCloud', 'zoom3d','zoom2d','autoScale2d',
                                                                'hoverClosestCartesian',
                                                                'hoverCompareCartesian'), displaylogo=FALSE)
        
        ggplotly(h2,dynamicTicks = TRUE)
    })

##############################
## DESCARGAR DATOS GRÁFICOS ##
##############################
    
    #Descargar los datos usados para los gráficos
    output$descargar_datos_graficos <- downloadHandler(
      
      filename = function() {paste0(nombre_datos()," filtrados",".xlsx")},
      content = function(file) {
        
        #Crear nombre ficticio del excel
        fname <- paste(file, "xlsx", sep = ".")
        
        #Crear excel vacío
        wb <- loadWorkbook(fname, create = T)
        
        #Se crea la hoja en donde se guardan los datos y se guarda el excel
        createSheet(wb, name = nombre_datos())
        writeWorksheet(wb, 
                       datos()[, fecha := gsub("-01${1}", "", as.character(fecha))],
                       sheet = nombre_datos())
        saveWorkbook(wb)
        file.rename(fname, file)
      } 
    )
    
###########################
## TABLA DE ESTADÍSTICAS ##
###########################
    
    #Slider año para la tabla
    output$anio_tabla <- renderUI({
      
      #Seleccionar año
      pickerInput(inputId = "anio_tabla", label = "Selecciona el año de la tabla:",
                  choices = unique(year(datos()$fecha)),
                  selected = max(year(datos()$fecha)), multiple = F)
    })
    
    #Slider mes para la tabla
    output$mes_tabla <- renderUI({
      
      #Filtro para que aparezcan los meses que sí tienen datos
      max_anio <- reactive({
        datos()[year(fecha) == max(year(fecha))]
      })
      
      #Seleccionar mes
      pickerInput(inputId = "mes_tabla", label = "Selecciona el mes de la tabla:", 
                  choices = levels(droplevels(sort(unique(month(max_anio()$fecha, label = T,
                                                     abbr = F))))), 
                  selected = month(max(max_anio()$fecha), 
                                   label = T, abbr = F), multiple = F)
    })
    
    #Columnas de pensiones en pesos
    monto_pensiones <- reactive({
      merge(pensiones_pagadas, serie_uf, by = "fecha", all.x = T)
    })
    
    #Filtros fáciles
    pensiones_filtrada <- reactive({
      monto_pensiones()[ymd(gsub("-", "", fecha)) >= input$fecha_pens[1] 
                        & ymd(gsub("-", "", fecha)) <= input$fecha_pens[2]
                        & grepl(paste(input$anios_cot, collapse = "|"), anios_cotizados)
                        & grepl(paste(input$tipo_pension, collapse = "|"), tipo_pension)]
    })
    
    #Filtros raros  ##no se me ocurrio una manera mas eficiente de hacer estos filtros (menos lineas de código)
    filtros_pensiones <- reactive({
      
      if (input$total_o_auto == "Pensión total" & input$medida == "Promedio" & input$moneda == "UF") {
        
        #Seleccionar columnas
        pensiones_filtrada()[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, numero_pensionados, uf,
                                 pension_total_promedio_uf)]
        
      } else if (input$total_o_auto == "Pensión autofinanciada" & input$medida == "Promedio" 
                 & input$moneda == "UF") {
        
        #Seleccionar columnas
        pensiones_filtrada()[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, numero_pensionados, uf,
                                 pension_autofinanciada_promedio_uf)]
        
      } else if (input$total_o_auto == "Pensión total" & input$medida == "Mediana" 
                 & input$moneda == "UF") {
        
        #Seleccionar columnas
        pensiones_filtrada()[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, numero_pensionados, uf,
                                 pension_total_mediana_uf)]
        
      } else if (input$total_o_auto == "Pensión autofinanciada" & input$medida == "Mediana" 
                 & input$moneda == "UF") {
        
        #Seleccionar columnas
        pensiones_filtrada()[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, numero_pensionados, uf,
                                 pension_autofinanciada_mediana_uf)]
        
      } else if (input$total_o_auto == "Pensión total" & input$medida == "Promedio" 
                 & input$moneda == "Pesos") {
        
        #Seleccionar columnas
        dt <- pensiones_filtrada()[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, 
                                       numero_pensionados, pension_total_promedio_uf, uf)]
        
        #Crear nombres de variables
        columna_interes <- names(dt)[7]
        nueva_columna <- gsub("uf", "pesos", names(dt)[7])
        
        #Asignar la columna en pesos y borrar la columna en uf
        monto_a_pesos(dt, nueva_columna, columna_interes)
        
        #Seleccionar columnas de interes
        dt <- dt[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, 
                     numero_pensionados, uf, pension_total_promedio_pesos)]
        
        
      } else if (input$total_o_auto == "Pensión total" & input$medida == "Mediana" 
                 & input$moneda == "Pesos") {
        
        #Seleccionar columnas
        dt <- pensiones_filtrada()[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, 
                                       numero_pensionados, pension_total_mediana_uf, uf)]
        
        #Crear nombres de variables
        columna_interes <- names(dt)[7]
        nueva_columna <- gsub("uf", "pesos", names(dt)[7])
        
        #Asignar la columna en pesos y borrar la columna en uf
        monto_a_pesos(dt, nueva_columna, columna_interes)
        
        #Seleccionar columnas de interés
        dt <- dt[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, 
                     numero_pensionados, uf, pension_total_mediana_pesos)]
        
      } else if (input$total_o_auto == "Pensión autofinanciada" & input$medida == "Promedio" 
                 & input$moneda == "Pesos") {
        
        #Seleccionar columnas
        dt <- pensiones_filtrada()[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, 
                                       numero_pensionados, pension_autofinanciada_promedio_uf, uf)]
        
        #Crear nombres de variables
        columna_interes <- names(dt)[7]
        nueva_columna <- gsub("uf", "pesos", names(dt)[7])
        
        #Asignar la columna en pesos y borrar la columna en uf
        monto_a_pesos(dt, nueva_columna, columna_interes)
        
        #Seleccionar columnas de interés
        dt <- dt[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, 
                     numero_pensionados, uf, pension_autofinanciada_promedio_pesos)]
        
      } else if (input$total_o_auto == "Pensión autofinanciada" & input$medida == "Mediana" 
                 & input$moneda == "Pesos") {
        
        #Seleccionar columnas
        dt <- pensiones_filtrada()[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo, 
                                       numero_pensionados, pension_autofinanciada_mediana_uf, uf)]
        
        #Crear nombres de variables
        columna_interes <- names(dt)[7]
        nueva_columna <- gsub("uf", "pesos", names(dt)[7])
        
        #Asignar la columna en pesos y borrar la columna en uf
        monto_a_pesos(dt, nueva_columna, columna_interes)
        
        #Seleccionar columnas de interés
        dt <- dt[, .(fecha, tipo_pension, anios_cot, anios_cotizados, sexo,
                     numero_pensionados, uf, pension_autofinanciada_mediana_pesos)]
        
      }
      
    })
    
    #Función para armar la tabla estadística
    armar_tabla <- reactive({
      
      if (input$indicador == "Afiliados") {
        
        if (input$ind_af != "Saldo promedio de la cuenta individual") {
          
          #Colapsar los datos según Variable Disponible y sexo
          dt <- afiliados[grepl(paste(input$tipo_af, collapse = "|"),
                                str_to_sentence(tipo_afiliado)),]
          
          dt <- dt[year(fecha) == input$anio_tabla & month(fecha, label = T, abbr = F) == input$mes_tabla,
                   .(num = sum(num_afiliados)),
                   by =.(tipo_afiliado, sexo, fecha)]
          
          dt$tipo_afiliado = str_to_title(dt$tipo_afiliado)
          
          #Pegar datos poblacionales
          dt <- merge(dt, poblacion, by = c("fecha","sexo"), all.x = T)
          
          #Crear tasa de afiliado
          dt[, tasa_afil := num/poblacion_edad_trabajar]
          
          #Eliminar columna "sin información"
          dt= dt[dt$sexo != "s/i", ]
          
          #Armar fecha para el subtítulo
          anio <- unique(year(dt$fecha))
          mes <- unique(month(dt$fecha, label = T, abbr = F))
          
          #Crear variable brecha
          if (input$ind_af == "Número de afiliados") {
            
            #Armar la tabla a lo ancho según sexo
            dt <- dcast(dt, formula = tipo_afiliado ~ sexo, value.var = c("num"))
            
            dt[, brecha := (hombres - mujeres)/mujeres]
            
            #Formato tabla
            tabla <- gt(dt, rowname_col = "tipo_afiliado") %>%
              tab_header(title = paste(str_to_sentence(input$ind_af)," y brecha de género, según ", "tipo de afiliados" ),
                         subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
              tab_stubhead(label = "Tipo de afiliados") %>%
              cols_label(hombres = "Hombres",
                         mujeres = "Mujeres",
                         brecha = "Brecha (%)") %>%
              fmt_number(columns = c("hombres", "mujeres"),
                         decimals = 0, sep_mark = ".", dec_mark = ",") %>%
              fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
              tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
            
          } else {
            
            #Armar la tabla a lo ancho según sexo
            dt <- dcast(dt, formula = tipo_afiliado ~ sexo, value.var = c("num", "tasa_afil"))
            
            dt[, brecha := (tasa_afil_hombres - tasa_afil_mujeres)]
            
            #Formato tabla
            tabla <- gt(dt, rowname_col = "tipo_afiliado") %>%
              tab_header(title = paste(str_to_sentence(input$ind_af), "número de afiliados","y brecha de género,",
                                       "según tipo de afiliados"),
                         subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
              tab_stubhead(label = "Tipo de afiliados") %>%
              tab_spanner(label = "Hombres",
                          columns = c("tasa_afil_hombres", "num_hombres")) %>%
              tab_spanner(label = "Mujeres",
                          columns = c("tasa_afil_mujeres", "num_mujeres")) %>%
              cols_label(tasa_afil_hombres = "Tasa de afiliación",
                         num_hombres = "Número de afiliados",
                         tasa_afil_mujeres = "Tasa de afiliación",
                         num_mujeres = "Número de afiliadas",
                         brecha = "Brecha (%)") %>%
              fmt_percent(columns = c("tasa_afil_hombres", "tasa_afil_mujeres"), sep_mark = ".", dec_mark = ",") %>%
              fmt_number(columns = c("num_hombres", "num_mujeres"), 
                         decimals=0, dec_mark = ",", sep_mark = ".") %>%
              fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
              tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
          }
        } else {
          
          #Colapsar los datos según Variable Disponible y sexo
          dt <- saldo_prom_edad[grepl(paste(input$edad_saldo, collapse = "|"),
                                      tramo_edad2),]
          
          dt <- dt[year(fecha) == input$anio_tabla & month(fecha, label = T, abbr = F) == input$mes_tabla,
                   .(num = sum(num_afiliados), saldo_pesos = sum(saldo_prom_miles_pesos*(num_afiliados/sum(num_afiliados)))),
                   by =.(tramo_edad2, sexo, fecha)]
          
          #Eliminar columna "sin información"
          dt= dt[dt$sexo != "s/i", ]
          
          #Armar fecha para el subtítulo
          anio <- unique(year(dt$fecha))
          mes <- unique(month(dt$fecha, label = T, abbr = F))
          
          #Pegar datos de UF
          dt <- merge(dt, serie_uf[, fecha := as.Date(paste(fecha, "01", sep = "-"))],
                      by = "fecha", all.x = T)
          
          #Variable en UF
          dt[, saldo_uf := saldo_pesos/uf]
          
          if (input$moneda_saldo == "Pesos"){
            #Armar la tabla a lo ancho según sexo
            dt <- dcast(dt, formula = tramo_edad2 ~ sexo, value.var = c("num", "saldo_pesos"))
            
            #Crear variable brecha
            dt[, brecha := (saldo_pesos_hombres - saldo_pesos_mujeres)/saldo_pesos_mujeres]
            
            #Formato tabla
            tabla <- gt(dt, rowname_col = "tramo_edad2") %>%
              tab_header(title = paste(str_to_sentence(input$ind_af), " ($), número de afiliados y brecha de género,",
                                       "según tramo de edad"),
                         subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
              tab_stubhead(label = "Tramo de edad") %>%
              tab_spanner(label = "Hombres",
                          columns = c("saldo_pesos_hombres", "num_hombres")) %>%
              tab_spanner(label = "Mujeres",
                          columns = c("saldo_pesos_mujeres", "num_mujeres")) %>%
              cols_label(saldo_pesos_hombres = "Saldo promedio en la cuenta individual ($)",
                         num_hombres = "Número de afiliados",
                         saldo_pesos_mujeres = "Saldo promedio en la cuenta individual ($)",
                         num_mujeres = "Número de afiliadas",
                         brecha = "Brecha (%)") %>%
              fmt_number(columns = c("saldo_pesos_hombres", "saldo_pesos_mujeres"), 
                         decimals=0, sep_mark = ".", dec_mark = ",") %>%
              fmt_number(columns = c("num_hombres", "num_mujeres"),
                         decimals=0, dec_mark = ",", sep_mark = ".") %>%
              fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
              tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
            
          } else {
            
            #Armar la tabla a lo ancho según sexo
            dt <- dcast(dt, formula = tramo_edad2 ~ sexo, value.var = c("num", "saldo_uf"))
            
            #Crear variable brecha
            dt[, brecha := (saldo_uf_hombres - saldo_uf_mujeres)/saldo_uf_mujeres]
            
            #Formato tabla
            tabla <- gt(dt, rowname_col = "tramo_edad2") %>%
              tab_header(title = paste(str_to_sentence(input$ind_af), " (UF), número de afiliados y brecha de género,",
                                       "según tramo de edad"),
                         subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
              tab_stubhead(label = "Tramo de edad") %>%
              tab_spanner(label = "Hombres",
                          columns = c("saldo_uf_hombres", "num_hombres")) %>%
              tab_spanner(label = "Mujeres",
                          columns = c("saldo_uf_mujeres", "num_mujeres")) %>%
              cols_label(saldo_uf_hombres = "Saldo promedio en la cuenta individual (UF)",
                         num_hombres = "Número de afiliados",
                         saldo_uf_mujeres = "Saldo promedio en la cuenta individual (UF)",
                         num_mujeres = "Número de afiliadas",
                         brecha = "Brecha (%)") %>%
              fmt_number(columns = c("saldo_uf_hombres", "saldo_uf_mujeres"), 
                         decimals=2, sep_mark = ".", dec_mark = ",") %>%
              fmt_number(columns = c("num_hombres", "num_mujeres"),
                         decimals=0, dec_mark = ",", sep_mark = ".") %>%
              fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
              tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
          }
        }
        
      } else if (input$indicador == "Cotizantes") {
        
        #Colapsar los datos según Variable Disponible y sexo
        dt <- cotizantes[grepl(paste(input$tipo_cot, collapse = "|"),
                               str_to_title(tipo_cotizante)),]
        
        dt <- dt[year(fecha) == input$anio_tabla & month(fecha, label = T, abbr = F) == input$mes_tabla,
                 .(num = sum(num_cotizantes), ing_pesos = sum(ing_prom*(num_cotizantes/sum(num_cotizantes)))),
                 by =.(tipo_cotizante, sexo, fecha)]
        
        dt$tipo_cotizante = str_to_title(dt$tipo_cotizante)
        
        #Pegar datos poblacionales
        dt <- merge(dt, poblacion, by = c("fecha","sexo"), all.x = T)
        
        #Crear tasa de afiliado
        dt[, tasa_cot := num/poblacion_edad_trabajar]
        
        #Eliminar columna "sin información"
        dt= dt[dt$sexo != "s/i", ]
        
        #Armar fecha para el subtítulo
        anio <- unique(year(dt$fecha))
        mes <- unique(month(dt$fecha, label = T, abbr = F))
        
        #Crear variable brecha
        if (input$ind_cot == "Número de cotizantes") {
          
          #Armar la tabla a lo ancho según sexo
          dt <- dcast(dt, formula = tipo_cotizante ~ sexo, value.var = c("num"))
          
          dt[, brecha := (hombres - mujeres)/mujeres]
          
          #Formato tabla
          tabla <- gt(dt, rowname_col = "tipo_cotizante") %>%
            tab_header(title = paste(str_to_sentence(input$ind_cot)," y brecha de género, según ", "tipo de cotizantes" ),
                       subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
            tab_stubhead(label = "Tipo de cotizantes") %>%
            cols_label(hombres = "Hombres",
                       mujeres = "Mujeres",
                       brecha = "Brecha (%)") %>%
            fmt_number(columns = c("hombres", "mujeres"),
                       decimals = 0, sep_mark = ".", dec_mark = ",") %>%
            fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
            tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
          
        } else if (input$ind_cot == "Tasa de cotización") {
          
          #Armar la tabla a lo ancho según sexo
          dt <- dcast(dt, formula = tipo_cotizante ~ sexo, value.var = c("num", "tasa_cot"))
          
          dt[, brecha := (tasa_cot_hombres - tasa_cot_mujeres)]
          
          #Formato tabla
          tabla <- gt(dt, rowname_col = "tipo_cotizante") %>%
            tab_header(title = paste(str_to_sentence(input$ind_cot), "número de cotizantes y brecha de género,",
                                     "según tipo de cotizantes"),
                       subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
            tab_stubhead(label = "Tipo de cotizantes") %>%
            tab_spanner(label = "Hombres",
                        columns = c("tasa_cot_hombres", "num_hombres")) %>%
            tab_spanner(label = "Mujeres",
                        columns = c("tasa_cot_mujeres", "num_mujeres")) %>%
            cols_label(tasa_cot_hombres = "Tasa de cotización",
                       num_hombres = "Número de cotizantes",
                       tasa_cot_mujeres = "Tasa de cotización",
                       num_mujeres = "Número de cotizantes",
                       brecha = "Brecha (%)") %>%
            fmt_percent(columns = c("tasa_cot_hombres", "tasa_cot_mujeres"), sep_mark = ".", dec_mark = ",") %>%
            fmt_number(columns = c("num_hombres", "num_mujeres"), 
                       decimals=0, dec_mark = ",", sep_mark = ".") %>%
            fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
            tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
        } else {
          
          #Pegar datos de UF
          dt <- merge(dt, serie_uf[, fecha := as.Date(paste(fecha, "01", sep = "-"))],
                      by = "fecha", all.x = T)
          
          #Variable en UF
          dt[, ing_uf := ing_pesos/uf]
          
          if (input$moneda_cot == "Pesos"){
            #Armar la tabla a lo ancho según sexo
            dt <- dcast(dt, formula = tipo_cotizante ~ sexo, value.var = c("num", "ing_pesos"))
            
            #Crear variable brecha
            dt[, brecha := (ing_pesos_hombres - ing_pesos_mujeres)/ing_pesos_mujeres]
            
            #Formato tabla
            tabla <- gt(dt, rowname_col = "tipo_cotizante") %>%
              tab_header(title = paste(str_to_sentence(input$ind_cot), " ($), número de afiliados y brecha de género,",
                                       "según tipo de cotizantes"),
                         subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
              tab_stubhead(label = "Tipo de cotizantes") %>%
              tab_spanner(label = "Hombres",
                          columns = c("ing_pesos_hombres", "num_hombres")) %>%
              tab_spanner(label = "Mujeres",
                          columns = c("ing_pesos_mujeres", "num_mujeres")) %>%
              cols_label(ing_pesos_hombres = "Ingreso imponible promedio mensual ($)",
                         num_hombres = "Número de afiliados",
                         ing_pesos_mujeres = "Ingreso imponible promedio mensual ($)",
                         num_mujeres = "Número de afiliadas",
                         brecha = "Brecha (%)") %>%
              fmt_number(columns = c("ing_pesos_hombres", "ing_pesos_mujeres"), 
                         decimals=0, sep_mark = ".", dec_mark = ",") %>%
              fmt_number(columns = c("num_hombres", "num_mujeres"),
                         decimals=0, dec_mark = ",", sep_mark = ".") %>%
              fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
              tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
            
          } else {
            
            #Armar la tabla a lo ancho según sexo
            dt <- dcast(dt, formula = tipo_cotizante ~ sexo, value.var = c("num", "ing_uf"))
            
            #Crear variable brecha
            dt[, brecha := (ing_uf_hombres - ing_uf_mujeres)/ing_uf_mujeres]
            
            #Formato tabla
            tabla <- gt(dt, rowname_col = "tipo_cotizante") %>%
              tab_header(title = paste(str_to_sentence(input$ind_cot), " (UF), número de afiliados y brecha de género,",
                                       "según tipo de cotizantes"),
                         subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
              tab_stubhead(label = "Tipo de cotizantes") %>%
              tab_spanner(label = "Hombres",
                          columns = c("ing_uf_hombres", "ing_hombres")) %>%
              tab_spanner(label = "Mujeres",
                          columns = c("ing_uf_mujeres", "num_mujeres")) %>%
              cols_label(ing_uf_hombres = "Ingreso imponible promedio mensual (UF)",
                         num_hombres = "Número de afiliados",
                         ing_uf_mujeres = "Ingreso imponible promedio mensual (UF)",
                         num_mujeres = "Número de afiliadas",
                         brecha = "Brecha (%)") %>%
              fmt_number(columns = c("ing_uf_hombres", "ing_uf_mujeres"), 
                         decimals=2, sep_mark = ".", dec_mark = ",") %>%
              fmt_number(columns = c("num_hombres", "num_mujeres"),
                         decimals=0, dec_mark = ",", sep_mark = ".") %>%
              fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
              tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
          }
          
        }
        
      } else if (input$indicador == "Beneficiarios SPS") {
        
        #Colapsar los datos según Variable Disponible y sexo
        dt <- datos_sps[grepl(paste(input$tipo_benef, collapse = "|"),
                              tipo_beneficio) & grepl(paste(input$region_benef, collapse = "|"), str_to_title(region)),]
        
        dt <- dt[year(fecha) == input$anio_tabla & month(fecha, label = T, abbr = F) == input$mes_tabla,
                 .(num = sum(num_beneficiarios), monto_prom_pesos = sum(monto_beneficio)/sum(num_beneficiarios)),
                 by =.(tipo_beneficio, sexo, fecha)]
        
        #Eliminar columna "sin información"
        dt= dt[dt$sexo != "s/i", ]
        
        #Armar fecha para el subtítulo
        anio <- unique(year(dt$fecha))
        mes <- unique(month(dt$fecha, label = T, abbr = F))
        
        #Crear variable brecha
        if (input$ind_sps == "Número de beneficiarios") {
          
          #Armar la tabla a lo ancho según sexo
          dt <- dcast(dt, formula = tipo_beneficio ~ sexo, value.var = c("num"))
          
          dt[, brecha := (hombres - mujeres)/mujeres]
          
          #Formato tabla
          tabla <- gt(dt, rowname_col = "tipo_beneficio") %>%
            tab_header(title = paste(str_to_sentence(input$ind_sps)," y brecha de género, según ", "tipo de beneficio" ),
                       subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
            tab_stubhead(label = "Tipo de beneficio") %>%
            cols_label(hombres = "Hombres",
                       mujeres = "Mujeres",
                       brecha = "Brecha (%)") %>%
            fmt_number(columns = c("hombres", "mujeres"),
                       decimals = 0, sep_mark = ".", dec_mark = ",") %>%
            fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
            tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
          
        } else {
          
          if (input$moneda_sps == "Pesos"){
            
            #Armar la tabla a lo ancho según sexo
            dt <- dcast(dt, formula = tipo_beneficio ~ sexo, value.var = c("num", "monto_prom_pesos"))
            
            dt[, brecha := (monto_prom_pesos_hombres - monto_prom_pesos_mujeres)/monto_prom_pesos_mujeres]
            
            #Formato tabla
            tabla <- gt(dt, rowname_col = "tipo_beneficio") %>%
              tab_header(title = paste(str_to_sentence(input$ind_sps), " ($), ", "número de beneficiarios","y brecha de género,",
                                       "según tipo de beneficio"),
                         subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
              tab_stubhead(label = "Tipo de beneficio") %>%
              tab_spanner(label = "Hombres",
                          columns = c("monto_prom_pesos_hombres", "num_hombres")) %>%
              tab_spanner(label = "Mujeres",
                          columns = c("monto_prom_pesos_mujeres", "num_mujeres")) %>%
              cols_label(monto_prom_pesos_hombres = "Monto promedio del beneficio ($)",
                         num_hombres = "Número de afiliados",
                         monto_prom_pesos_mujeres = "Monto promedio del beneficio ($)",
                         num_mujeres = "Número de afiliadas",
                         brecha = "Brecha (%)") %>%
              fmt_number(columns = c("monto_prom_pesos_hombres", "monto_prom_pesos_mujeres"), 
                         decimals=0, sep_mark = ".", dec_mark = ",") %>%
              fmt_number(columns = c("num_hombres", "num_mujeres"),
                         decimals=0, dec_mark = ",", sep_mark = ".") %>%
              fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
              tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
            
          } else{
            
            #Pegar datos de UF
            dt <- merge(dt, serie_uf[, fecha := as.Date(paste(fecha, "01", sep = "-"))],
                        by = "fecha", all.x = T)
            
            #Variable en UF
            dt[, monto_prom_uf := monto_prom_pesos/uf]
            
            #Armar la tabla a lo ancho según sexo
            dt <- dcast(dt, formula = tipo_beneficio ~ sexo, value.var = c("num", "monto_prom_uf"))
            
            dt[, brecha := (monto_prom_uf_hombres - monto_prom_uf_mujeres)/monto_prom_uf_mujeres]
            
            #Formato tabla
            tabla <- gt(dt, rowname_col = "tipo_beneficio") %>%
              tab_header(title = paste(str_to_sentence(input$ind_sps), " (UF), ", "número de beneficiarios","y brecha de género,",
                                       "según tipo de beneficio"),
                         subtitle = str_to_sentence(paste(mes, "de", anio))) %>%
              tab_stubhead(label = "Tipo de beneficio") %>%
              tab_spanner(label = "Hombres",
                          columns = c("monto_prom_uf_hombres", "num_hombres")) %>%
              tab_spanner(label = "Mujeres",
                          columns = c("monto_prom_uf_mujeres", "num_mujeres")) %>%
              cols_label(monto_prom_uf_hombres = "Monto promedio del beneficio (UF)",
                         num_hombres = "Número de afiliados",
                         monto_prom_uf_mujeres = "Monto promedio del beneficio (UF)",
                         num_mujeres = "Número de afiliadas",
                         brecha = "Brecha (%)") %>%
              fmt_number(columns = c("monto_prom_uf_hombres", "monto_prom_uf_mujeres"), 
                         decimals=2, sep_mark = ".", dec_mark = ",") %>%
              fmt_number(columns = c("num_hombres", "num_mujeres"),
                         decimals=0, dec_mark = ",", sep_mark = ".") %>%
              fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
              tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
          }
        }
        
      } else if (input$indicador == "Pensionados") {
        
        if (input$ind_pens == "Monto de pensión") {
          
          #Obtener el nombre de la columna en donde está el valor de la pensión
          nombre_pension <- names(filtros_pensiones())[8]
          
          #Estos tres objetos sirven para que el nombre del gráfico sea dinámico
          tipo_pension <- gsub("pension|_|mediana|promedio|pesos|uf", "", nombre_pension)
          medida <- gsub("pension|autofinanciada|total|uf|pesos|_", "", nombre_pension)
          moneda <- gsub("pension|autofinanciada|total|promedio|mediana|_", "", nombre_pension)
          moneda <- ifelse(moneda == "pesos", "$", "UF")
          
          #Cambiar el nombre de la pensión a simplemente pensión
          dt <- copy(filtros_pensiones())
          setnames(dt, 8, "pension")
          
          #colapsar los datos segun años cotizados y sexo
          dt <- dt[year(fecha) == input$anio_tabla & month(fecha, label = T, abbr = F) == input$mes_tabla,
                   .(pension = sum(pension*(numero_pensionados/sum(numero_pensionados))),
                     n_pensionados = sum(numero_pensionados)),
                   by =.(anios_cotizados, sexo, fecha)]
          
          #Armar fecha para el subtítulo
          anio <- unique(year(dt$fecha))
          mes <- unique(month(dt$fecha, label = T, abbr = F))
          
          #Eliminar columna fecha
          dt[, fecha := NULL]
          
          #armar la tabla a lo ancho según sexo
          dt <- dcast(dt, formula = anios_cotizados ~ sexo, value.var = c("n_pensionados","pension"))
          
          #Crear variable brecha
          dt[, brecha := (pension_hombres - pension_mujeres)/pension_mujeres]
          
          #Formato tabla
          tabla <- gt(dt, rowname_col = "anios_cotizados") %>%
            tab_header(title = paste("Monto", medida,"de la pensión", tipo_pension,"y brecha de género,",
                                     "según años cotizados"),
                       subtitle = paste(mes, "de", anio)) %>%
            tab_stubhead(label = "Años Cotizados") %>%
            tab_spanner(label = "Hombres",
                        columns = c("n_pensionados_hombres", "pension_hombres")) %>%
            tab_spanner(label = "Mujeres",
                        columns = c("n_pensionados_mujeres", "pension_mujeres")) %>%
            cols_label(n_pensionados_hombres = "Número de pensionados",
                       pension_hombres = paste0("Monto de pensión ", medida," (", moneda, ")"),
                       n_pensionados_mujeres = "Número de Pensionadas",
                       pension_mujeres = paste0("Monto de pensión ", medida," (", moneda, ")"),
                       brecha = "Brecha (%)") %>%
            fmt_number(columns = c("n_pensionados_hombres", "n_pensionados_mujeres"), 
                       decimals = 0, sep_mark = ".", dec_mark = ",") %>%
            fmt_number(columns = c("pension_hombres", "pension_mujeres"), dec_mark = ",", sep_mark = ".") %>%
            fmt_percent(columns = "brecha", sep_mark = ".", dec_mark = ",") %>%
            tab_source_note(source_note = "Fuente: Superintendencia de Pensiones.")
          
        }
        
      }
      
      
    })
    
    #Tabla
    output$tabla_estadistica <- render_gt({
      
      armar_tabla()
      
    })

#################
## PENSIONADOS ##
#################
    
    #Gráfico pensiones pagadas
    #plot_pension_pagada <- reactive({
        
        #Obtener el nombre de la columna en donde esta el valor de la pensión
    #    nombre_pension <- names(filtros_pensiones())[8]
        
        #Estos tres objetos sirven para que el nombre del grafico sea dinamico
     #   tipo_pension <- gsub("pension|_|mediana|promedio|pesos|uf", "", nombre_pension)
    #    medida <- gsub("pension|autofinanciada|total|uf|pesos|_", "", nombre_pension)
    #    moneda <- gsub("pension|autofinanciada|total|promedio|mediana|_", "", nombre_pension)
    #    moneda <- ifelse(moneda == "pesos", "$", "UF")
        
        #Cambiar el nombre de la pensión a simplemente pension
    #    dt <- copy(filtros_pensiones())
    #    setnames(dt, 8, "pension")
        
        #Crear la tabla para el gráfico
    #    dt <- dt[,.(pension = sum(pension*numero_pensionados/sum(numero_pensionados))), by =.(fecha, sexo)]
        
        #Esta variable sirve para identificar el límite superior del eje y en el gráfico
    #    ultimo_monto <- tail(sort(dt$pension), 1)*1.1
        
        #Otra opción para el monto del límite superior
    #    monto_v2 <- ifelse(moneda == "pesos", 600000, 16)
        
        #Pasar los datos de largo a ancho para el gráfico
    #    dt <- dcast(dt, formula(fecha ~ sexo), value.var = "pension")
        
        #Gráfico
    #    graf_pension <- plot_ly(dt,type = 'scatter', mode = 'lines') %>%
    #        add_lines(x=~fecha, y=~mujeres, name="Mujeres", line=list(color='#4B82D4')) %>% 
    #        add_lines(x=~fecha, y=~hombres, name="Hombres", line=list(color='#4BD4B9')) %>% 
    #        layout(title = paste("Monto de pensión", tipo_pension, medida,"según sexo"),
    #               xaxis = list(title = ""),
    #               yaxis = list(tickfont= list(color = '#1f77b4', size=11), color='#1f77b4', 
    #                            range=c(0,1.2*max(dt$hombres,dt$mujeres)),
    #                            title= paste0("Monto de pensión ", tipo_pension, " ", medida," (", moneda, ")")),
    #               legend = list(orientation = "h", xanchor = "center", x = 0.5, y =-0.3))%>%
    #        config(locale = "es", modeBarButtonsToRemove = list('sendDataToCloud', 'zoom3d','zoom2d','autoScale2d',
    #                                                            'hoverClosestCartesian',
    #                                                            'hoverCompareCartesian'), displaylogo=FALSE)
        
    #    ggplotly(graf_pension, dynamicTicks = TRUE)
    #})
    #output$graf_pension_pagada <- renderPlotly(plot_pension_pagada())
    
    #Gráfico Brecha en pensión
    #plot_brecha_pension <- reactive({
        
        #Obtener el nombre de la columna en donde está el valor de la pensión
    #    nombre_pension <- names(filtros_pensiones())[8]
        
        #Esta variable sirve para el nombre dinámico del gráfico (total o autofinanciada)
    #    tipo_pension <- gsub("pension|_|mediana|promedio|pesos|uf", "", nombre_pension)
        
        #Eliminar variable que no se necesita
    #    dt <- copy(filtros_pensiones())
    #    dt <- dt[, uf := NULL]
        
        #Armar la tabla a lo ancho segun el sexo. Así es fácil obtener la brecha de pensión
     #   pensiones_wide <- dcast(dt, 
      #                          formula = fecha + tipo_pension + anios_cot + anios_cotizados ~ sexo,
      #                          value.var = c("numero_pensionados", nombre_pension))
        
        #Cambiar el nombre de las columnas para identificarlas al modificar la tabla
      #  setnames(pensiones_wide, 7:8, c("pension_hombre", "pension_mujer"))
        
        #Calcular los montos de pensión
      #  pensiones_wide <- pensiones_wide[,.(pension_hombre = sum(pension_hombre*numero_pensionados_hombres/sum(numero_pensionados_hombres)),
    #                                        pension_mujer = sum(pension_mujer*numero_pensionados_mujeres/sum(numero_pensionados_mujeres))),
    #                                     by =.(fecha)]
        
        #Crear columna brecha
    #    pensiones_wide[, brecha := (pension_hombre/pension_mujer - 1)*100]
        
        #Esta variable sirve para identificar el límite inferio del eje y en el gráfico
     #   valor_minimo <- if (min(pensiones_wide$brecha >= 0, na.rm = T)) {
            
    #        valor <- 0.4
            
    #    } else {
            
    #        valor <- 1.2
            
    #    }
        
        #Esta variable sirve para identificar el límite superior del eje y en el gráfico
     #   valor_maximo <- if(max(pensiones_wide$brecha <= 0, na.rm = T)) {
            
      #      valor <- 0.4
            
      #  }  else {
            
      #      valor <- 1.2
            
       # }
        
        #Gráfico
      #  graf_brecha <- plot_ly(pensiones_wide,type = 'scatter', mode = 'lines') %>%
      #      add_lines(x=~fecha, y=~brecha, name="Brecha (%)", line=list(color="F55E5E")) %>%
      #      layout(title = paste("Brecha de género"),
      #             xaxis = list(title = ""),
      #             yaxis = list(tickfont= list(color = '#ff7f0e', size=11), color='#ff7f0e', 
      #                          range=c(valor_minimo*min(pensiones_wide$brecha, na.rm = T), 
      #                                  1.2*max(pensiones_wide$brecha, na.rm = T)),
      #                          title = "Brecha (%)"),
      #             legend = list(orientation = "h", xanchor = "center", x = 0.5, y =-0.3))%>%
      #      config(locale = "es", modeBarButtonsToRemove = list('sendDataToCloud', 'zoom3d','zoom2d','autoScale2d',
      #                                                          'hoverClosestCartesian',
      #                                                          'hoverCompareCartesian'), displaylogo=FALSE)
        
      #  ggplotly(graf_brecha,dynamicTicks = TRUE)
            
    #})
    #output$graf_brecha_pension <- renderPlotly(plot_brecha_pension())
    
########################
## NUEVOS PENSIONADOS ##
########################
        
    #Nombre gráfico
    nombre_grafico_nuevos_pens <- reactive({
        
        if (input$ind_nuevos_pens == "Número de nuevos pensionados") {
            
            nombre <- "Nuevos Pensionados"
            
        } else if (input$ind_nuevos_pens == "Monto de pensión") {
            
            nombre <- "Pensión Autofinanciada"
            
        }
    })
    
    
    #identificar unidad
    identificar_unidad <- reactive({
        
        if (input$ind_nuevos_pens == "Número de nuevos pensionados") {
            
            unidad <- "Número"
            
        } else if (input$ind_nuevos_pens == "Monto de pensión") {
            
            if (input$moneda_nuevos_pens == "Pesos") {
                
                unidad <- "$"
                
            } else {
                
                unidad <- "UF"
            }
            
        }
        
        
    })
    
    #gráfico de número, tasa o ingreso 
    output$serie_3 <- renderPlotly({
        
        graf_nuevos_pens <- plot_ly(datos_nuevos_pens(),type = 'scatter', mode = 'lines') %>%
            add_lines(x=~fecha, y=~mujeres, name="Mujeres", line=list(color='#4B82D4')) %>% 
            add_lines(x=~fecha, y=~hombres, name="Hombres", line=list(color='#4BD4B9')) %>% 
            layout(title = paste(nombre_grafico_nuevos_pens(),"según sexo"),
                   xaxis = list(title = ""),
                   yaxis = list(tickfont= list(color = '#1f77b4', size=11), color='#1f77b4', 
                                range=c(0,1.2*max(datos_nuevos_pens()$hombres,datos_nuevos_pens()$mujeres)),
                                title= paste0(nombre_grafico_nuevos_pens(), " (", identificar_unidad() ,")")),
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, y =-0.3))%>%
            config(locale = "es", modeBarButtonsToRemove = list('sendDataToCloud', 'zoom3d','zoom2d','autoScale2d',
                                                                'hoverClosestCartesian',
                                                                'hoverCompareCartesian'), displaylogo=FALSE)
        
        ggplotly(graf_nuevos_pens,dynamicTicks = TRUE)
    })
    
    #factor para el minimo del eje y
    minimo_valor <- reactive(
        
        if (min(datos_nuevos_pens()$brecha, na.rm = T) >= 0) {
        
        factor <- 0.4
        
        } else {
        
        factor <- 1.2
            
        }
        
    )
    
    #gráfico para la brecha
    output$serie_4 <- renderPlotly({
        
        brecha_nuevos_pens <- plot_ly(datos_nuevos_pens(),type = 'scatter', mode = 'lines') %>%
            add_lines(x=~fecha, y=~brecha, name="Brecha (%)", line=list(color="F55E5E")) %>%
            layout(title = paste("Brecha de género"),
                   xaxis = list(title = ""),
                   yaxis = list(tickfont= list(color = '#ff7f0e', size=11), color='#ff7f0e', 
                                range=c(minimo_valor()*min(datos_nuevos_pens()$brecha),
                                        1.2*max(datos_nuevos_pens()$brecha)),
                                title = "Brecha (%)"),
                   legend = list(orientation = "h", xanchor = "center", x = 0.5, y =-0.3))%>%
            config(locale = "es", modeBarButtonsToRemove = list('sendDataToCloud', 'zoom3d','zoom2d','autoScale2d',
                                                                'hoverClosestCartesian',
                                                                'hoverCompareCartesian'), displaylogo=FALSE)
        
        ggplotly(brecha_nuevos_pens,dynamicTicks = TRUE)
    })
    
    #Descargar los datos filtrados de nuevos pensionados
    output$descargar_nuevos_pens_filtrados <- downloadHandler(
        
        filename = function() {paste0("nuevos pensionados"," filtrados",".xlsx")},
        content = function(file) {
            
            #Crear nombre ficticio del excel
            fname <- paste(file, "xlsx", sep = ".")
            
            #Crear excel vacío
            wb <- loadWorkbook(fname, create = T)
            
            #Se crea la hoja en donde se guardan los datos y se guarda el excel
            createSheet(wb, name = "Nuevos Pensionados")
            writeWorksheet(wb, 
                           datos_nuevos_pens()[, fecha := gsub("-01${1}", "", as.character(fecha))],
                           sheet = "Nuevos Pensionados")
            saveWorkbook(wb)
            file.rename(fname, file)
        } 
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
