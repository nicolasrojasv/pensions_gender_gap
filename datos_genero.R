# Web Scraping Género

#Limpiar ambiente
rm(list = ls())
options(scipen = 999)

#Librerías
sapply(c("data.table","compiler","lubridate","readxl","readr","tidyr","RSelenium","rvest", "purrr","stringr",
         "zoo", "stringi"),
       require, character.only = T, quietly = T)

#Escritorio
setwd("//192.168.100.101/g/nicolas_rojas/viz_webpage/genero/brechas_pensiones/dashboard/indicadores")

#Cargar datos
poblacion <- fread("input_dashboard/poblacion_celade.csv", dec= ",", na.strings = "NA")
afiliados <- fread("input_dashboard/afiliados.csv", dec= ",", na.strings = "NA")
cotizantes <- fread("input_dashboard/cotizantes.csv", dec= ",", na.strings = "NA")
saldo_prom <- fread("input_dashboard/saldo_prom.csv", dec = ",", na.strings = "NA")
saldo_prom_edad <- fread("input_dashboard/saldo_prom_edad.csv", dec = ",", na.strings = "NA")
pensiones_pagadas <- fread("input_dashboard/pensiones_pagadas.csv", dec = ",", na.strings = "NA")
nuevos_pensionados <- fread("input_dashboard/nuevos_pensionados.csv", dec = ",", na.strings = "NA")
serie_uf <- fread("input_dashboard/serie_uf.csv", dec = ",", na.strings = "NA")
datos_sps <- fread("input_dashboard/datos_sps.csv", dec = ",", na.strings = "NA", integer64 = "numeric")

#Fechas Información disponibles
fechas_consulta <- gsub(pattern = "-|[0-9]{2}$", replacement = "", as.character(seq(from = as.Date("1990-01-01"),
                                                                                    to = today(),
                                                                                    by = "month")))

########################
### DATOS POBLACION ####
########################

#Función para descargar los datos de poblacion desde CELADE
descargar_datos_poblacion <- function() {
  
  #Crear archivo temporal, descargar los datos y guardarlos en el archivo
  archivo_temporal <- tempfile(fileext = ".xlsx")
  url <- paste0("https://celade.cepal.org/bdcelade/proyecciones/resultados/04_CHL.xlsx")
  download.file(url, destfile = archivo_temporal, mode = "wb", quiet = T)
  
  #Leer el archivo y ponerle los nombres adecuados a las columnas
  head <- names(data.table(suppressMessages(read_excel(path = archivo_temporal, sheet = 2, skip = 7))))
  head[1] <- "edad"
  poblacion <- data.table(suppressMessages(read_excel(path = archivo_temporal, sheet = 2, skip = 8,
                                                      col_names = head)))
  
  #Eliminar missing values y filas que no tienen datos relevantes
  poblacion <- poblacion[!is.na(edad)][!grepl("Fuente|Source",edad)]
  poblacion <- poblacion[c(23:nrow(poblacion))]
  
  #Crear variable sexo y rellenar datos
  poblacion[, sexo := ifelse(grepl("Hombres", edad), "hombres",
                             ifelse(grepl("Mujeres", edad), "mujeres", NA_character_))]
  poblacion <- fill(poblacion, sexo)[!grepl("Hombres|Mujeres",edad)]
  
  #Pasar la tabla de ancho a largo creando la variable fecha y la poblacion
  poblacion <- melt(poblacion, id.vars = c(1,153), variable.name = "fecha", value.name = "poblacion_total",
                    variable.factor = F)
  return(poblacion)
}
descargar_datos_poblacion <- cmpfun(descargar_datos_poblacion)

#Extraer datos poblacionales
poblacion <- descargar_datos_poblacion()

#Función para ordenar datos de población
ordenar_dt <- function(bd) {
  
  #Crear variabla auxiliar para las edades
  bd[, aux := rep(c(1:length(unique(bd$edad))),
                         times = nrow(bd)/length(unique(bd$edad)))]
  
  #Base con el número de hombres y mujeres en edad de trabajar
  edad_trabajar <- bd[(sexo == "hombres" & !(aux %between% c(1,3) | aux %between% c(14,21)))
                      | (sexo == "mujeres" & !(aux %between% c(1,3) | aux %between% c(13,21))),
                      .(poblacion_edad_trabajar = sum(poblacion_total)),
                      by =.(fecha, sexo)]
  
  #Base con el número de hombres y mujeres mayores de 18 años
  adultos <- bd[(sexo == "hombres" & !aux %between% c(1,3)) | (sexo == "mujeres" & !aux %between% c(1,3)),
                .(poblacion_adultos = sum(poblacion_total)),
                by =.(fecha, sexo)]
  
  #Base con el número de hombres y mujeres en edad de jubilación
  edad_jubilacion <- bd[(sexo == "hombres" & aux %between% c(14,21)) | (sexo == "mujeres" & aux %between% c(13,21)),
                        .(poblacion_edad_jubilacion = sum(poblacion_total)),
                        by =.(fecha, sexo)]
  
  #Unir las bases
  bd <- merge(merge(edad_trabajar, adultos, by = c("fecha", "sexo")), edad_jubilacion, by = c("fecha", "sexo"))
  
  #Repetir cada dato cuatro veces para emparejarlos por trimestre con las demás bases
  bd <- bd[rep(seq_len(nrow(bd)), each = 4),]
  bd[, mes := rep(c("03","06","09","12"), times = nrow(bd)/4)][, fecha := paste(fecha, mes, "01", sep = "-")
                                                               ][,mes := NULL]
  return(bd)
}
ordenar_dt <- cmpfun(ordenar_dt)

#Ordenar datos poblacionales
poblacion <- ordenar_dt(poblacion)

#Guardar datos
write.csv2(poblacion, file = "input_dashboard/poblacion_celade.csv", row.names = F, na = "")

###################
### AFILIADOS  ####
###################

#Función para descargar datos de afiliados al sistema de pensiones
descargar_afiliados <- function(fechas) {
  
  #Primera fuente de información para extraer los datos (archivo excel)
  archivo_temporal <- tempfile(fileext = ".xls")
  url <- paste0("https://www.spensiones.cl/inf_estadistica/series_afp/afiliados/afiliados_tipo_sexo_afp.xls")
  download.file(url, destfile = archivo_temporal, mode = "wb", quiet = T)
  dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 1)))[c(1:961),]
  
  #Pasar a valor numérico las variables que corresponden
  cols <- grep("^VOL|^Vol",names(dt), value = T)
  dt[, (cols) := lapply(.SD, function(x) as.numeric(gsub("-", 0, x))), .SDcols = cols]
  
  #Eliminar los totales y cambiar el nombre de las columnas
  dt <- dt[!grepl("Sistema", AFP)]
  dt <- dt[,.(fecha = as.character(PERIODO), afp = AFP, dependientes_hombres = `Dep Masculino`,
              dependientes_mujeres = `Dep Femenino`, dependientes_sininfo = `Dep S/I`,
              independientes_hombres = `Indep Masculino`, independientes_mujeres = `Indep  Femenino`,
              independientes_sininfo = `Indep  S/I`, voluntarios_hombres = `Voluntarios Masculino`,
              voluntarios_mujeres = `Voluntarios  Femenino`, voluntarios_sininfo = `Voluntarios  S/I`)]
  
  #Pasar la tabla de lo ancho a lo largo
  dt <- melt(dt, id.vars = c(1:2), variable.name = "factor", value.name = "num_afiliados", variable.factor = F)
  
  #Crear variables tipo de afiliado, sexo y fecha
  dt <- separate(dt, "factor", into = c("tipo_afiliado", "sexo"), sep = "_")
  dt[, sexo := gsub("sininfo", "s/i", sexo)]
  
  #Segunda fuente de información si es que faltan datos en la primera fuente
  #Crear lista vacía para guardar las tablas de datos
  consolidado <- list()
  
  #Conexión remota a Chrome
  remDr <- remoteDriver(remoteServerAddr = "192.168.100.34", port = 4445L, browserName = "chrome")
  remDr$open()
  
  #Descargar la información para cada fecha
  for (fecha in fechas) {
    
    #Crear un índice para guardar las tablas de datos en la lista
    i <- match(fecha, fechas)
    
    #No descargar datos para fechas que han sido descargadas en la primera fuente
    if (fecha %in% unique(gsub("-|01$","",x = dt$fecha))) next
    
    #Navegar a la web de la SP y descargar la información
    remDr$navigate("https://www.spensiones.cl/apps/centroEstadisticas/paginaCuadrosCCEE.php?menu=sci&menuN1=afil&menuN2=afp")
    tryCatch({
      
      #Encontrar los datos para la fecha
      webelem <- remDr$findElement(using = "css", paste0("[value = 'inf_estadistica/aficot/trimestral/",
                                                         year(ym(fecha)),
                                                         "/",substr(fecha,5,6),"/02F']"))
      webelem$clickElement()
      webelem <- remDr$findElements(using = "css", paste0("[value = 'Ver']"))
      
      #Entrar a la página donde están los datos
      webelem[[3]]$clickElement()
      
      #Extraer el nombre de las columnas
      head1 <- as.character(as.matrix(html_table(read_html(remDr$getPageSource()[[1]]),
                                                 dec = ",", fill = T)[[1]])[1,])
      head1[1] <- NA_character_ ; head1 <- tolower(head1)
      head2 <- as.character(as.matrix(html_table(read_html(remDr$getPageSource()[[1]]),
                                                 dec = ",", fill = T)[[1]])[2,])
      head2 <- gsub("\\.", "", head2) ; head2 <- tolower(head2)
      head2 <- gsub("masculino", "hombres", head2) ; head2 <- gsub("femenino", "mujeres", head2)
      
      headers <- map_chr(1:length(head1), ~ {
        ifelse(!is.na(head1[.x]) & !is.na(head2[.x]),
               paste(head1[.x], head2[.x], sep = "_"),
               head2[.x])
      })
      
      #Leer los datos y asignarle los nombres a las columnas
      dt2 <- data.table(html_table(read_html(remDr$getPageSource()[[1]]), dec = ",", fill = T, header = F)[[1]])
      dt2 <- dt2[c(4:nrow(dt2))][!grepl("^Total",X1)]
      names(dt2) <- headers
      
      #Transformar las columnas correspondientes a número y mayúscula a las AFP
      cols <- names(dt2)[c(2:ncol(dt2))]
      dt2[, (cols) := lapply(.SD, function(x) as.numeric(gsub("\\.", "", x))), .SDcols = cols][, afp := str_to_title(afp)]
      
      #Pasar la tabla de ancho a largo y crear las variables tipo de afiliado, sexo y fecha. Eliminar los totales
      dt2 <- melt(dt2, id.vars = 1, variable.name = "factor", value.name = "num_afiliados")
      dt2 <- separate(dt2, col = "factor", into = c("tipo_afiliado", "sexo"), sep = "_")
      dt2 <- dt2[!grepl("^total", tipo_afiliado)][!grepl("^total", sexo)]
      dt2[, fecha := paste(year(ym(fecha)), substr(fecha, 5, 6), "01", sep = "-")]
      
      consolidado[[i]] <- dt2
      Sys.sleep(1)
      
    }, error = print)
  }
  remDr$close()
  return(rbindlist(list(dt,rbindlist(consolidado, use.names = T)), use.names = T))
}
descargar_afiliados <- cmpfun(descargar_afiliados) 

#Descargar serie de número de afiliados
afiliados <- descargar_afiliados(grep("03$|06$|09$|12$", fechas_consulta, value = T))

#Guardar datos
write.csv2(afiliados, file = "input_dashboard/afiliados.csv", row.names = F, na = "")

#######################################
### COTIZANTES E INGRESO IMPONIBLE ####
#######################################

#Función para descargar datos de cotizantes del sistema de pensiones
descargar_cotizantes <- function() {
  
  #Crear archivo temporal y guardar ahí el excel
  archivo_temporal <- tempfile(fileext = ".xls")
  url <- paste0("https://www.spensiones.cl/inf_estadistica/series_afp/cotizantes/cotizantes_ingreso_imponible_promedio.xls")
  download.file(url, destfile = archivo_temporal, mode = "wb", quiet = T)
  
  #Leer los datos
  dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 1, sheet = 1,
                                               col_types = c("date","text","numeric","numeric","numeric","numeric",
                                                             "numeric","numeric","numeric","numeric","numeric","numeric",
                                                             "numeric","numeric","numeric","numeric","numeric","numeric",
                                                             "numeric","numeric","numeric","numeric","numeric","numeric",
                                                             "numeric","numeric","numeric","numeric","numeric","numeric",
                                                             "numeric","numeric","numeric","numeric"))))
  
  #Cambiar el nombre de las columnas
  dt <- dt[,.(fecha = Fecha, afp = AFP, n_dependientes_total = `N° de Cotizantes Dependientes`,
              n_dependientes_hombres = `N° de Cotizantes Dependientes Masculino`, 
              n_dependientes_mujeres = `N° de Cotizantes Dependientes Femenino`, 
              n_dependientes_sininfo = `N° de Cotizantes Dependientes sin información de sexo`, 
              n_independientes_total = `N° de Cotizantes Independientes`, 
              n_independientes_hombres = `N° de Cotizantes Independientes Masculino`,
              n_independientes_mujeres = `N° de Cotizantes Independientes Femenino`,
              n_independientes_sininfo = `N° de Cotizantes Independientes sin información de sexo`,
              n_voluntarios_total = `N° de Cotizantes Afil. voluntarios`, 
              n_voluntarios_hombres = `N° de Cotizantes Afil. voluntarios Masculino`,
              n_voluntarios_mujeres = `N° de Cotizantes Afil. voluntarios Femenino`,
              n_voluntarios_sininfo = `N° de Cotizantes Afil. voluntarios sin información de sexo`,
              n_total_ambossexo = `N° de Cotizantes`,
              n_total_hombres = `N° de Cotizantes  Masculino`,
              n_total_mujeres = `N° de Cotizantes  Femenino`, 
              n_total_sininfo = `N° de Cotizantes  sin información de sexo`,
              ing_dependientes_total = `Ing. Imp. Prom. Cotizantes Dependientes`,
              ing_dependientes_hombres = `Ing. Imp. Prom. Cotizantes Dependientes Masculino`,
              ing_dependientes_mujeres = `Ing. Imp. Prom. Cotizantes Dependientes Femenino`,
              ing_dependientes_sininfo = `Ing. Imp. Prom. Cotizantes Dependientes sin información de sexo`,
              ing_independientes_total = `Ing. Imp. Prom. Cotizantes Independientes`,
              ing_independientes_hombres = `Ing. Imp. Prom. Cotizantes Independientes Masculino`,
              ing_independientes_mujeres = `Ing. Imp. Prom. Cotizantes Independientes Femenino`,
              ing_independientes_sininfo = `Ing. Imp. Prom. Cotizantes Independientes sin información de sexo`,
              ing_voluntarios_total = `Ing. Imp. Prom. Cotizantes Afil. voluntarios`,
              ing_voluntarios_hombres = `Ing. Imp. Prom. Cotizantes Afil. voluntarios Masculino`,
              ing_voluntarios_mujeres = `Ing. Imp. Prom. Cotizantes Afil. voluntarios Femenino`,
              ing_voluntarios_sininfo = `Ing. Imp. Prom. Cotizantes Afil. voluntarios sin información de sexo`,
              ing_total_ambossexo = `Ing. Imp. Prom. Cotizantes`,
              ing_total_hombres =  `Ing. Imp. Prom. Cotizantes  Masculino`,
              ing_total_mujeres = `Ing. Imp. Prom. Cotizantes  Femenino`,
              ing_total_sininfo = `Ing. Imp. Prom. Cotizantes  sin información de sexo`)]
  
  #Pasar la tabla de ancho a largo y crear variables numero, monto, tipo de cotizantes y sexo
  dt <- melt(dt, id.vars = c(1,2), variable.name = "factor", value.name = "num_monto")
  dt <- separate(dt, col = "factor", into = c("valor", "tipo_cotizante", "sexo"), sep = "_")
  dt <- dt[!grepl("^total",tipo_cotizante)][!(grepl("^total|^ambossexo", sexo))][!grepl("^TOTAL",afp)]
  dt[, ":="(valor = fcase(valor == "n", "num_cotizantes", rep_len(TRUE, length(valor)), "ing_prom"),
            afp = str_to_title(afp),
            sexo = ifelse(sexo == "sininfo", "s/i", sexo))]
  
  #Pasar la tabla de lo largo a lo ancho
  dt <- dcast(dt, formula = ... ~ valor, value.var = "num_monto")
  
  #Converitir missing values a cero
  dt[, ":="(ing_prom = ifelse(is.na(ing_prom), 0, ing_prom),
            num_cotizantes = ifelse(is.na(num_cotizantes), 0, num_cotizantes))]
  return(dt)
}
descargar_cotizantes <- cmpfun(descargar_cotizantes)

#Descargar serie de cotizantes
cotizantes <- descargar_cotizantes()

#Guardar datos
write.csv2(cotizantes, file = "input_dashboard/cotizantes.csv", row.names = F, na = "")

######################
### SALDO PROMEDIO ###
######################

#Descargar información del saldo promedio por sexo
descargar_saldo_prom <- function(fechas, bd) {
  
  #Control para que corra igual la función por si la base de datos no se encuentra en el ambiente de trabajo
  if (missing(bd)) {
    bd <- data.table(fecha = character(), sexo = character(), num_afiliados = integer(), 
                     saldo_prom_miles_pesos = numeric())
  } else {
    bd <- bd
  }
  
  #Lista vacía para ir guardando los datos
  consolidado <- list()
  
  #Conexión remota a Chrome
  remDr <- remoteDriver(remoteServerAddr = "192.168.100.34", port = 4445L, browserName = "chrome")
  remDr$open()
  
  for (fecha in fechas) {
    
    #Si es que los datos ya están en la base, saltar esa fecha de descarga
    if (fecha %in% unique(gsub(pattern = "-|01$","",x = bd$fecha))) next
    
    #indice para guardar la tabla de datos en el consolidado
    i <- match(fecha,fechas)
    
    #Ir a la página donde se descarga la info y hacer click en los datos de saldo promedio y afiliados por sexo
    remDr$navigate(paste0("https://www.spensiones.cl/apps/centroEstadisticas/paginaCuadrosCCEE.php?menu=sci&menuN1",
                          "=afil&menuN2=sdomovcci"))
    
    
    tryCatch({
      
      #Indicar la fecha en la página web y entrar al cuadro estadístico
      webelem <- remDr$findElement(using = "css", paste0("[value = 'inf_estadistica/aficot/trimestral/",
                                                         year(ym(fecha)),
                                                         "/",substr(fecha,5,6),"/04C']"))
      webelem$clickElement()
      webelem <- remDr$findElements(using = "css", paste0("[value = 'Ver']"))
      webelem[[2]]$clickElement()
      
      if (as.numeric(fecha) >= 201609) {
        
        #Ordenar el encabezado de las tablas
        head1 <- as.character(as.matrix(html_table(read_html(remDr$getPageSource()[[1]]), dec = ",", fill = T,
                                                   header = F)[[1]])[1,])
        head1[1] <- NA_character_ ; head1 <- tolower(head1)
        head1 <- gsub("numero de afiliados", "num_afiliados", head1)
        head1 <- gsub("saldo prom\\. cta\\. cap\\. individual\\(miles de pesos\\)", "saldo_prom_miles_pesos", head1) 
        head2 <- as.character(as.matrix(html_table(read_html(remDr$getPageSource()[[1]]), 
                                                   dec = ",", fill = T)[[1]])[1,])
        head2 <- tolower(head2) ; head2 <- gsub("\\.", "", head2)
        headers <- map_chr(1:length(head1), ~ {
          ifelse(!is.na(head1[.x]) & !is.na(head2[.x]), 
                 paste(head1[.x], head2[.x], sep = "_"),
                 head2[.x])
        })
        
        #Descargar la tabla e insertar el encabezado
        dt2 <- data.table(html_table(read_html(remDr$getPageSource()[[1]]), fill = T, dec = ",", header = F)[[1]])
        names(dt2) <- headers
        dt2 <- dt2[3:nrow(dt2)][!grepl("total", tolower(afp))]
        
        #Modificar el formato de las columnas
        cols <- names(dt2)[2:length(dt2)]
        dt2[, (cols) := lapply(.SD, function(x) as.numeric(gsub("\\.", "", x))), .SDcols = cols]
        
        #Ordenar la tabla. Crear las variables fecha, número de afiliados y sexo
        dt3 <- dt2[,.(num_afiliados_hombres = sum(num_afiliados_hombres),
                      num_afiliados_mujeres = sum(num_afiliados_mujeres),
                      `num_afiliados_s/i` = sum(`num_afiliados_s/i`))]
        dt3[, fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01",sep = "-")]
        dt3 <- melt(dt3, id.vars = "fecha", value.name = "num_afiliados", variable.factor = F, 
                    variable.name = "factor")
        dt3 <- separate(dt3, col = "factor", into = c("dato", "sexo"), sep = "_(?=[^_]+$)")
        dt3[, dato := NULL]
        
        #Crear la variable saldo promedio
        dt4 <- dt2[,.(saldo_prom_miles_pesos_hombres = sum(saldo_prom_miles_pesos_hombres*(num_afiliados_hombres/sum(num_afiliados_hombres))),
                      saldo_prom_miles_pesos_mujeres = sum(saldo_prom_miles_pesos_mujeres*(num_afiliados_mujeres/sum(num_afiliados_mujeres))),
                      `saldo_prom_miles_pesos_s/i` = sum(`saldo_prom_miles_pesos_s/i`*(`num_afiliados_s/i`/sum(`num_afiliados_s/i`))))]
        dt4[,fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        dt4 <- melt(dt4, id.vars = "fecha",value.name = "saldo_prom_miles_pesos", variable.factor = F, 
                    variable.name = "factor")
        dt4 <- separate(dt4, col = "factor", into = c("dato", "sexo"), sep = "_(?=[^_]+$)")
        dt4[, dato := NULL]
        
        dt5 <- merge.data.table(dt3, dt4, by = c("fecha", "sexo"), all = T)
        
        #Guardar la tabla en el consolidado
        consolidado[[i]] <- dt5
        
      } else {
        
        #Ordenar el encabezado de las tablas
        head1 <- as.character(as.matrix(html_table(read_html(remDr$getPageSource()[[1]]), dec = ",", fill = T,
                                                   header = T)[[1]])[2,])
        head1 <- head1[!is.na(head1)]
        head1[1] <- NA_character_ ; head1 <- tolower(head1)
        head1 <- head1[!grepl("a.f.p.", head1)]
        head1 <- gsub("numero de afiliados", "num_afiliados", head1)
        head1 <- gsub("saldo prom\\. cta\\. cap\\. individual\\(miles de pesos\\)", "saldo_prom_miles_pesos", head1) 
        head2 <- as.character(as.matrix(html_table(read_html(remDr$getPageSource()[[1]]), 
                                                   dec = ",", fill = T, header = T)[[1]])[3,])
        head2 <- head2[!is.na(head2)] ; head2 <- tolower(head2) ; head2 <- gsub("\\.", "", head2)
        headers <- map_chr(1:length(head1), ~ {
          ifelse(!is.na(head1[.x]) & !is.na(head2[.x]), 
                 paste(head1[.x], head2[.x], sep = "_"),
                 head2[.x])
        })
        
        #Descargar la tabla e insertar el encabezado
        dt2 <- data.table(html_table(read_html(remDr$getPageSource()[[1]]), fill = T, dec = ",", header = T)[[1]])[,1:9]
        names(dt2) <- headers
        dt2 <- dt2[4:nrow(dt2)][!grepl("total|fuente|\\(1\\)|s/i|afiliado|''", tolower(afp))][!is.na(num_afiliados_total)]
        
        #Modificar el formato de las columnas
        cols <- names(dt2)[2:length(dt2)]
        dt2[, (cols) := lapply(.SD, function(x) as.numeric(gsub("\\.", "", x))), .SDcols = cols]
        
        #Ordenar la tabla y crear las variables fecha, numero de afiliados y sexo
        dt3 <- dt2[,.(num_afiliados_hombres = sum(num_afiliados_hombres),
                      num_afiliados_mujeres = sum(num_afiliados_mujeres),
                      `num_afiliados_s/i` = sum(`num_afiliados_s/i`))]
        dt3[,fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        dt3 <- melt(dt3, id.vars = "fecha",value.name = "num_afiliados", variable.factor = F, 
                    variable.name = "factor")
        dt3 <- separate(dt3, col = "factor", into = c("dato", "sexo"), sep = "_(?=[^_]+$)")
        dt3[, dato := NULL]
        
        #Crear la variable saldo promedio
        dt4 <- dt2[,.(saldo_prom_miles_pesos_hombres = sum(saldo_prom_miles_pesos_hombres*(num_afiliados_hombres/sum(num_afiliados_hombres))),
                      saldo_prom_miles_pesos_mujeres = sum(saldo_prom_miles_pesos_mujeres*(num_afiliados_mujeres/sum(num_afiliados_mujeres))),
                      `saldo_prom_miles_pesos_s/i` = sum(`saldo_prom_miles_pesos_s/i`*(`num_afiliados_s/i`/sum(`num_afiliados_s/i`))))]
        dt4[, fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        dt4 <- melt(dt4, id.vars = "fecha",value.name = "saldo_prom_miles_pesos", variable.factor = F, 
                    variable.name = "factor")
        dt4 <- separate(dt4, col = "factor", into = c("dato", "sexo"), sep = "_(?=[^_]+$)")
        dt4[, dato := NULL]
        
        dt5 <- merge.data.table(dt3, dt4, by = c("fecha", "sexo"), all = T)
        
        #Guardar la tabla en el consolidado
        consolidado[[i]] <- dt5
      }
    }, error = print)
  }
  remDr$close()
  return(rbindlist(consolidado, use.names = T))
}
descargar_saldo_prom <- cmpfun(descargar_saldo_prom)

#Descargar datos de saldo promedio
if (exists("saldo_prom")) {
  
  saldo_prom_nuevo <- descargar_saldo_prom(fechas = grep("03$|06$|09$|12$", fechas_consulta, value = T),
                                           bd = saldo_prom)

} else {
  
  saldo_prom_nuevo <- descargar_saldo_prom(fechas = grep("03$|06$|09$|12$", fechas_consulta, value = T))
  
}

#Unir base nueva con antigua
if (exists("saldo_prom")) {
  
  #Unir las bases
  saldo_prom <- rbindlist(list(saldo_prom, saldo_prom_nuevo), use.names = T)
  
} else {
  
  #Renombrar la base nueva como la antigua
  saldo_prom <- copy(saldo_prom_nuevo)
  
}
rm(saldo_prom_nuevo)

#Guardar datos
write.csv2(saldo_prom, file = "input_dashboard/saldo_prom.csv", row.names = F, na = "")

###############################
### SALDO PROMEDIO POR EDAD ###
###############################

#Descargar información del saldo promedio por edad y sexo
descargar_saldo_prom_edad <- function(fechas, bd) {
  
  #Control por si la base de datos no se encuentra en el ambiente de trabajo y que corra igual la función
  if (missing(bd)) {
    bd <- data.table(fecha = character(), edad = character(), sexo = character(), num_afiliados = integer(), 
                     saldo_prom_miles_pesos = numeric())
  } else {
    bd <- bd
  }
  
  #Lista vacía para ir guardando las tablas de datos
  consolidado <- list()
  
  #Conexión remota a Chrome
  remDr <- remoteDriver(remoteServerAddr = "192.168.100.34", port = 4445L, browserName = "chrome")
  remDr$open()
  
  for (fecha in fechas) {
    
    #Si es que los datos ya están en la base, saltar esa fecha de descarga
    if (fecha %in% unique(gsub(pattern = "-|01$","",x = bd$fecha))) next
    
    #indice para guardar la tabla en el consolidado
    i <- match(fecha,fechas)
    
    #Ir a la página donde se descarga la info y hacer click en los datos de saldo promedio y afiliados por sexo
    remDr$navigate(paste0("https://www.spensiones.cl/apps/centroEstadisticas/paginaCuadrosCCEE.php?menu=sci&menuN1",
                          "=afil&menuN2=sdomovcci"))
    
    
    tryCatch({
      
      #Entar a la página según la fecha
      webelem <- remDr$findElement(using = "css", paste0("[value = 'inf_estadistica/aficot/trimestral/",
                                                         year(ym(fecha)),
                                                         "/",substr(fecha,5,6),"/04A']"))
      webelem$clickElement()
      webelem <- remDr$findElements(using = "css", paste0("[value = 'Ver']"))
      webelem[[1]]$clickElement()
      
      if (as.numeric(fecha) >= 201609) {
        
        #Ordenar el encabezado de las tablas
        head1 <- as.character(as.matrix(html_table(read_html(remDr$getPageSource()[[1]]), dec = ",", fill = T,
                                                   header = F)[[1]])[1,])
        
        head1[1] <- NA_character_ ; head1 <- tolower(head1)
        head1 <- gsub("numero de afiliados", "num_afiliados", head1)
        head1 <- gsub("saldo prom\\. cta\\. cap\\. individual\\(miles de pesos\\)", "saldo_prom_miles_pesos", head1) 
        head2 <- as.character(as.matrix(html_table(read_html(remDr$getPageSource()[[1]]), 
                                                   dec = ",", fill = T, header = F)[[1]])[2,])
        head2 <- head2 <- tolower(head2) ; head2 <- gsub(" \\(años\\)", "", head2)
        headers <- map_chr(1:length(head1), ~ {
          ifelse(!is.na(head1[.x]) & !is.na(head2[.x]), 
                 paste(head1[.x], head2[.x], sep = "_"),
                 head2[.x])
        })
        
        #Descargar la tabla e insertar el encabezado
        dt2 <- data.table(html_table(read_html(remDr$getPageSource()[[1]]), fill = T, dec = ",", header = T)[[1]])
        names(dt2) <- headers
        dt2 <- dt2[2:nrow(dt2)][!grepl("total|s/i", tolower(edad))]
        
        #Modificar el formato de las columnas
        cols <- names(dt2)[2:length(dt2)]
        dt2[, (cols) := lapply(.SD, function(x) as.numeric(gsub("\\.", "", x))), .SDcols = cols]
        
        #Ordenar la tabla y crear las columnas sexo, edad y número de afiliados
        dt3 <- dt2[,.(edad, num_afiliados_hombres, num_afiliados_mujeres, `num_afiliados_s/i`)]
        dt3[, fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        dt3 <- melt(dt3, id.vars = c("fecha", "edad") ,value.name = "num_afiliados", variable.factor = F, 
                    variable.name = "factor")
        dt3 <- separate(dt3, col = "factor", into = c("dato", "sexo"), sep = "_(?=[^_]+$)")
        dt3[, dato := NULL]
        
        #Crear la columna saldo promedio
        dt4 <- dt2[,.(edad, saldo_prom_miles_pesos_hombres, saldo_prom_miles_pesos_mujeres,
                      `saldo_prom_miles_pesos_s/i`)]
        dt4[, fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        dt4 <- melt(dt4, id.vars = c("fecha", "edad"), value.name = "saldo_prom_miles_pesos", variable.factor = F,
                    variable.name = "factor")
        dt4 <- separate(dt4, col = "factor", into = c("dato", "sexo"), sep = "_(?=[^_]+$)")
        dt4[, dato := NULL]
        
        #Unir las tablas de datos
        dt5 <- merge.data.table(dt3, dt4, by = c("fecha", "edad", "sexo"), all = T)
        
        #Guardar la tabla en el consolidado
        consolidado[[i]] <- dt5
        
      } else {
        
        #Ordenar el encabezado de las tablas
        head1 <- as.character(as.matrix(html_table(read_html(remDr$getPageSource()[[1]]), dec = ",", fill = T,
                                                   header = T)[[1]])[2,])
        head1 <- head1[!is.na(head1)]
        head1[1] <- NA_character_ ; head1 <- tolower(head1)
        head1 <- head1[!grepl("edad \\(años\\)", head1)]
        head1 <- gsub("numero de afiliados", "num_afiliados", head1)
        head1 <- gsub("saldo prom\\. cta\\. cap\\. individual\\(miles de pesos\\)", "saldo_prom_miles_pesos", head1) 
        head2 <- as.character(as.matrix(html_table(read_html(remDr$getPageSource()[[1]]), 
                                                   dec = ",", fill = T, header = T)[[1]])[3,])
        head2 <- head2[!is.na(head2)] ; head2 <- tolower(head2) ; head2 <- gsub(" \\(años\\)", "", head2)
        headers <- map_chr(1:length(head1), ~ {
          ifelse(!is.na(head1[.x]) & !is.na(head2[.x]), 
                 paste(head1[.x], head2[.x], sep = "_"),
                 head2[.x])
        })
        
        #Descargar la tabla e insertar el encabezado
        dt2 <- data.table(html_table(read_html(remDr$getPageSource()[[1]]), fill = T, dec = ",", header = T)[[1]])[,1:9]
        names(dt2) <- headers
        dt2 <- dt2[4:nrow(dt2)][!grepl("total|fuente|\\(1\\)|s/i|afiliado|''", tolower(edad))][!is.na(num_afiliados_total)]
        
        #Modificar el formato de las columnas
        cols <- names(dt2)[2:length(dt2)]
        dt2[, (cols) := lapply(.SD, function(x) as.numeric(gsub("\\.", "", x))), .SDcols = cols]
        
        #Ordenar la tabla. Crear las columnas sexo, edad y número de afiliados
        dt3 <- dt2[,.(edad, num_afiliados_hombres, num_afiliados_mujeres, `num_afiliados_s/i`)]
        dt3[,fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        dt3 <- melt(dt3, id.vars = c("fecha", "edad") ,value.name = "num_afiliados", variable.factor = F, 
                    variable.name = "factor")
        dt3 <- separate(dt3, col = "factor", into = c("dato", "sexo"), sep = "_(?=[^_]+$)")
        dt3[, dato := NULL]
        
        #Crear la variable saldo promedio
        dt4 <- dt2[,.(edad, saldo_prom_miles_pesos_hombres, saldo_prom_miles_pesos_mujeres,
                      `saldo_prom_miles_pesos_s/i`)]
        dt4[, fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        dt4 <- melt(dt4, id.vars = c("fecha", "edad"), value.name = "saldo_prom_miles_pesos", variable.factor = F,
                    variable.name = "factor")
        dt4 <- separate(dt4, col = "factor", into = c("dato", "sexo"), sep = "_(?=[^_]+$)")
        dt4[, dato := NULL]
        
        #Unir las tablas
        dt5 <- merge.data.table(dt3, dt4, by = c("fecha", "edad", "sexo"), all = T)
        
        #Guardar la tabla en el consolidado
        consolidado[[i]] <- dt5
      }
    }, error = print)
  }
  remDr$close()
  return(rbindlist(consolidado, use.names = T))
}
descargar_saldo_prom_edad <- cmpfun(descargar_saldo_prom_edad)

#Descargar datos de saldo promedio
if (exists("saldo_prom_edad")) {
  
  saldo_prom_edad_nuevo <- descargar_saldo_prom_edad(fechas = grep("03$|06$|09$|12$", fechas_consulta, value = T), 
                                                     bd = saldo_prom_edad)

} else {
  
  saldo_prom_edad_nuevo <- descargar_saldo_prom_edad(fechas = grep("03$|06$|09$|12$", fechas_consulta, value = T))
  
}

try({
  
  #Crear variable tramos de edad
  saldo_prom_edad_nuevo[, tramo_edad := as.numeric(ifelse(edad == "Hasta 45", 45,
                                                   ifelse(edad == "Mas de 70", 71, edad)))]
  saldo_prom_edad_nuevo[, tramo_edad := cut(tramo_edad, breaks = c(0, 45, 50, 55, 60, 65, 70, 71), 
                                            ordered_result = T)]
  saldo_prom_edad_nuevo[, tramo_edad2 := ifelse(tramo_edad == "(0,45]", "Hasta 45 años",
                                         ifelse(tramo_edad == "(45,50]", "Entre 46 y 50 años",
                                         ifelse(tramo_edad == "(50,55]", "Entre 51 y 55 años",
                                         ifelse(tramo_edad == "(55,60]", "Entre 56 y 60 años",
                                         ifelse(tramo_edad == "(60,65]", "Entre 61 y 65 años",
                                         ifelse(tramo_edad == "(65,70]", "Entre 66 y 70 años",
                                         "Más de 70 años"))))))]
  
}, silent = T)

#Unir bases
if (exists("saldo_prom_edad")) {
  
  saldo_prom_edad <- rbindlist(list(saldo_prom_edad, saldo_prom_edad_nuevo), use.names = T)
  
} else {
  
  saldo_prom_edad <- copy(saldo_prom_edad_nuevo)
    
}
rm(saldo_prom_edad_nuevo)

#Guardar datos
write.csv2(saldo_prom_edad, file = "input_dashboard/saldo_prom_edad.csv", row.names = F, na = "")

#########################
### PENSIONES PAGADAS ###
#########################

#Función para descargar pensiones pagadas de la página de la SP
descargar_pensiones_pagadas <- function(fechas, bd) {
  
  #Control para que corra igual la función por si la base de datos no se encuentra en el ambiente de trabajo
  if (missing(bd)) {
    bd <- data.table(fecha = character(), tipo_pension = character(), anios_cot = character(), sexo = character(), 
                     numero_pensionados = integer(), pension_autofinanciada_promedio_uf = numeric(),
                     pension_total_promedio_uf = numeric(), pension_autofinanciada_mediana_uf = numeric(),
                     pension_total_mediana_uf = numeric())
  } else {
    bd <- bd
  }
  
  #Lista vacía para guardar los datos
  consolidado <- list()
  
  #Descargar los archivos excel por fecha
  for (fecha in fechas) {
    
    #Si es que los datos ya están en la base, saltar esa fecha de descarga
    if (fecha %in% unique(gsub(pattern = "-|01$","",x = bd$fecha))) next
    
    #indice para guardar la tabla de datos en el consolidado
    i <- match(fecha, fechas)
    
    tryCatch({
      
      # Archivo temporal para guardar excel descargado
      archivo_temporal <- tempfile(fileext = ".xls")
      url <- paste0("https://www.spensiones.cl/inf_estadistica/afipen/mensual/",year(ym(fecha)),"/",substr(fecha,5,6),"/",
                    "PensionesAutofinanciadasAnoCotSexo.xls")
      download.file(url, destfile = archivo_temporal, mode = "wb", quiet = T)
      
      if (as.numeric(fecha) >= 201702) {
        
        # Encabezado de los datos
        head1 <- as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 2)))[1,])
        head1 <- gsub("Tipo de pensión", "tipo_pension", head1)
        head1 <- gsub("Tramos de años cotizados", "anios_cot", head1) ; head1 <- tolower(na.locf(head1))
        
        head2 <- as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 3)))[1,])
        head2 <- gsub("Pensión autofinanciada", "pension_autofinanciada", head2)
        head2 <- gsub("Pensión total \\(.*\\)", "pension_total", head2) ; head2 <- gsub("Número", "numero", head2)
        head2[1] <- head1[1] ; head2[2] <- head1[2]
        head2 <- tolower(na.locf(head2))
        
        head3 <- as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 4)))[1,])
        head3 <- tolower(head3) ; head3[is.na(head3)] <- "numero" ; head3[1] <- head1[1] ; head3[2] <- head1[2]
        
        head1[1] <- NA ; head1[2] <- NA ; head2[1] <- NA ; head2[2] <- NA ; head2 <- gsub("numero", NA, head2)
        
        # Armar el encabezado
        headers <- map_chr(1:length(head1), ~ {
          ifelse(!is.na(head1[.x]) & !is.na(head2[.x]) & !is.na(head3[.x]), 
                 paste(head1[.x], head2[.x], head3[.x],sep = "_"),
                 ifelse(!is.na(head1[.x]) & !is.na(head3[.x]),
                        paste(head1[.x], head3[.x],sep = "_"),
                        ifelse(!is.na(head2[.x]) & !is.na(head3[.x]),
                               paste(head2[.x], head3[.x],sep = "_"),
                               head3[.x])))
        })
        
        #Descargar los datos
        dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 5)))
        names(dt) <- headers
        
        dt <- fill(dt, tipo_pension)
        dt <- dt[!is.na(mujeres_numero)][!grepl("Total", anios_cot)]
        
        dt[, anios_cot := gsub(">", "(", anios_cot)][, anios_cot := gsub(" y <=", ", ", anios_cot)]
        dt[, anios_cot := gsub(" año", "", anios_cot)][, anios_cot := gsub("S/I \\(3\\)", "sin_info", anios_cot)]
        dt[!grepl("sin_info", anios_cot), anios_cot := paste0(anios_cot, "]")]
        
        #Arreglar columnas de las tablas
        dt2 <- dt[,.(tipo_pension, anios_cot, hombres_numero, mujeres_numero)]
        dt2 <- melt(dt2, id.vars = c(1,2), variable.name = "factor", value.name = "numero_pensionados")
        dt2 <- separate(dt2, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        dt2 <- dt2[!grepl("Total", tipo_pension)]
        
        dt3 <- dt[,.(tipo_pension, anios_cot, hombres_pension_autofinanciada_promedio,
                     mujeres_pension_autofinanciada_promedio)]
        dt3 <- melt(dt3, id.vars = c(1,2), variable.name = "factor", value.name = "pension_autofinanciada_promedio_uf")
        dt3 <- separate(dt3, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        dt3 <- dt3[!grepl("Total", tipo_pension)]
        
        dt4 <- dt[,.(tipo_pension, anios_cot, hombres_pension_autofinanciada_mediana,
                     mujeres_pension_autofinanciada_mediana)]
        dt4 <- melt(dt4, id.vars = c(1,2), variable.name = "factor", value.name = "pension_autofinanciada_mediana_uf")
        dt4 <- separate(dt4, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        dt4 <- dt4[!grepl("Total", tipo_pension)]
        
        dt5 <- dt[,.(tipo_pension, anios_cot, hombres_pension_total_promedio, mujeres_pension_total_promedio)]
        dt5 <- melt(dt5, id.vars = c(1,2), variable.name = "factor", value.name = "pension_total_promedio_uf")
        dt5 <- separate(dt5, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        dt5 <- dt5[!grepl("Total", tipo_pension)]
        
        dt6 <- dt[,.(tipo_pension, anios_cot, hombres_pension_total_mediana, mujeres_pension_total_mediana)]
        dt6 <- melt(dt6, id.vars = c(1,2), variable.name = "factor", value.name = "pension_total_mediana_uf")
        dt6 <- separate(dt6, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        dt6 <- dt6[!grepl("Total", tipo_pension)]
        
        #Juntar las tablas
        dt7 <- merge(merge(merge(merge(dt2, dt3, by = c("tipo_pension", "anios_cot", "sexo"),all = T),
                                 dt4, by =c("tipo_pension", "anios_cot", "sexo"), all = T),
                           dt5, by =c("tipo_pension", "anios_cot", "sexo"), all = T),
                     dt6, by =c("tipo_pension", "anios_cot", "sexo"), all = T)
        dt7[, fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        
        #Guardar la tabla en el consolidado
        consolidado[[i]] <- dt7
      
      } else {
        
        # Encabezado de los datos
        head1 <- as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 2)))[1,])
        head1 <- gsub("Tipo de pensión", "tipo_pension", head1)
        head1 <- gsub("Tramos de años cotizados", "anios_cot", head1) ; head1 <- tolower(na.locf(head1))
        
        head2 <- as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 3)))[1,])
        head2 <- gsub("Número", "numero", head2) 
        head2 <- gsub("Pensión promedio autofinanciada", "pension_autofinanciada", head2)
        head2 <- gsub("Pensión promedio  total \\(.*\\)", "pension_total", head2)
        head2[1] <- head1[1] ; head2[2] <- head1[2] ; head1[1] <- NA ; head1[2] <- NA ; head2 <- tolower(head2)
        
        # Armar el encabezado
        headers <- map_chr(1:length(head1), ~ {
          ifelse(!is.na(head1[.x]) & !is.na(head2[.x]), 
                 paste(head1[.x], head2[.x], sep = "_"),
                 head2[.x])
        })
        
        #Descargar los datos
        dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 4)))
        names(dt) <- headers
        
        dt <- fill(dt, tipo_pension)
        dt <- dt[!is.na(mujeres_numero)][!grepl("Total", anios_cot)]
        
        dt[, anios_cot := gsub(">", "(", anios_cot)][, anios_cot := gsub(" y <=", ", ", anios_cot)]
        dt[, anios_cot := gsub(" año", "", anios_cot)][, anios_cot := gsub("S/I \\(3\\)", "sin_info", anios_cot)]
        dt[!grepl("sin_info", anios_cot), anios_cot := paste0(anios_cot, "]")]
        
        #Arreglar columnas de las tablas
        dt2 <- dt[,.(tipo_pension, anios_cot, hombres_numero, mujeres_numero)]
        dt2 <- melt(dt2, id.vars = c(1,2), variable.name = "factor", value.name = "numero_pensionados")
        dt2 <- separate(dt2, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        dt2 <- dt2[!grepl("Total", tipo_pension)]
        
        dt3 <- dt[,.(tipo_pension, anios_cot, hombres_pension_autofinanciada, mujeres_pension_autofinanciada)]
        dt3 <- melt(dt3, id.vars = c(1,2), variable.name = "factor", value.name = "pension_autofinanciada_promedio_uf")
        dt3 <- separate(dt3, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        dt3 <- dt3[!grepl("Total", tipo_pension)]
        
        dt4 <- dt[,.(tipo_pension, anios_cot, hombres_pension_total, mujeres_pension_total)]
        dt4 <- melt(dt4, id.vars = c(1,2), variable.name = "factor", value.name = "pension_total_promedio_uf")
        dt4 <- separate(dt4, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        dt4 <- dt4[!grepl("Total", tipo_pension)]
        
        #Juntar las tablas
        dt5 <- merge.data.table(merge.data.table(dt2, dt3, by = c("tipo_pension", "anios_cot", "sexo"), all = T),
                                dt4, by =c("tipo_pension", "anios_cot", "sexo"), all = T)
        dt5[, ":="(fecha = paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-"),
                   pension_autofinanciada_mediana_uf = NA_integer_, pension_total_mediana_uf = NA_integer_)]
        
        #Guardar la tabla en el consolidado
        consolidado[[i]] <- dt5
        
      }
      
    }, error = print)
  }
  return(rbindlist(consolidado, use.names = T))
}
descargar_pensiones_pagadas <- cmpfun(descargar_pensiones_pagadas)

#Descargar datos de pensiones pagadas
if (exists("pensiones_pagadas")) {
  
  pensiones_pagadas_nuevo <- suppressWarnings(descargar_pensiones_pagadas(
    fechas = fechas_consulta[fechas_consulta >= 201611], bd = pensiones_pagadas))
  
} else {
  
  pensiones_pagadas_nuevo <- suppressWarnings(descargar_pensiones_pagadas(
    fechas = fechas_consulta[fechas_consulta >= 201611]))
  
}

try({
  
  #Crear columna de años cotizados
  pensiones_pagadas_nuevo[, ":="(anios_cotizados = 
                                   gsub("\\s\\s", " ", 
                                        gsub("sin_info", "Sin información",
                                             gsub("]", "",
                                                  gsub(",", " y menor o igual a ", 
                                                       gsub("\\(", "Mayor a ", anios_cot))))),
                                 aux_cot = as.numeric(gsub("[^[0-9]]*", "", anios_cot)))]
  pensiones_pagadas_nuevo[, aux_cot := ifelse(is.na(aux_cot), 4000, aux_cot)]
  
}, silent = T)

#Unir datos
if (exists("pensiones_pagadas")) {
  
  pensiones_pagadas <- rbindlist(list(pensiones_pagadas, pensiones_pagadas_nuevo), use.names = T)
  
} else {
  
  pensiones_pagadas <- copy(pensiones_pagadas_nuevo)
  
}
rm(pensiones_pagadas_nuevo)

#Guardar datos
write.csv2(pensiones_pagadas, file = "input_dashboard/pensiones_pagadas.csv", row.names = F, na = "")

##########################
### NUEVOS PENSIONADOS ###
##########################

#Función para descargar datos de nuevos pensionados
descargar_nuevos_pensionados <- function(fechas, bd) {
  
  #Control para que corra igual la función por si la base de datos no se encuentra en el ambiente de trabajo
  if (missing(bd)) {
    bd <- data.table(anios_cot = character(), sexo = character(), numero = integer(), 
                     pension_autofinanciada = numeric(), densidad_cot = numeric(), fecha = character())
  } else {
    bd <- bd
  }
  
  #Lista vacía para guardar los datos
  consolidado <- list()
  
  #Descargar los archivos excel por fecha
  for (fecha in fechas) {
    
    #Si es que los datos ya están en la base, saltar esa fecha de descarga
    if (fecha %in% unique(gsub(pattern = "-|01$","",x = bd$fecha))) next
    
    #indice para guardar la tabla de datos en el consolidado
    i <- match(fecha, fechas)
    
    tryCatch({
      
      # Archivo temporal para guardar excel descargado
      archivo_temporal <- tempfile(fileext = ".xls")
      url <- paste0("https://www.spensiones.cl/inf_estadistica/afipen/mensual/",year(ym(fecha)),"/",substr(fecha,5,6),"/",
                    "PensionesAutofinanciadasDensidadAnoCotSexo.xls")
      download.file(url, destfile = archivo_temporal, mode = "wb", quiet = T)
      
      if (as.numeric(fecha) >= 201804) {
        
        # Encabezado de los datos
        head1 <- as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 2)))[1,])
        head1 <- gsub("Pensión autofinanciada", "pension_autofinanciada", head1)
        head1 <- gsub("Número", "numero", head1)
        head1 <- gsub("Densidad de cotizaciones", "densidad_cot", head1)
        head1 <- gsub("Tramos de años cotizados", "anios_cot", head1) ; head1 <- tolower(na.locf(head1))
        
        head2 <- tolower(as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 3)))[1,]))
        
        # Armar el encabezado
        headers <- map_chr(1:length(head1), ~ {
          ifelse(!is.na(head1[.x]) & !is.na(head2[.x]), 
                 paste(head1[.x], head2[.x] ,sep = "_"),
                 head1[.x])
        })
        
        #Descargar los datos
        dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 4)))
        names(dt) <- headers
        
        #Arreglar la tabla
        dt <- fill(dt, sexo)
        dt <- dt[!is.na(anios_cot)][!grepl("Total", anios_cot)][!grepl("TOTAL", sexo)]
        dt[, sexo := tolower(sexo)]
        dt[, anios_cot := gsub(">", "(", anios_cot)][, anios_cot := gsub(" y <=", ", ", anios_cot)]
        dt[, anios_cot := gsub(" año", "", anios_cot)][, anios_cot := gsub("S/I \\(4\\)", "sin_info", anios_cot)]
        dt[!grepl("sin_info", anios_cot), anios_cot := paste0(anios_cot, "]")]
        
        #Arreglar columnas numericas
        cols <- tail(names(dt), 2)
        dt[, (cols) := lapply(.SD, function(x) as.numeric(gsub("-", NA, x))), .SDcols = cols]
        dt[, fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01",sep = "-")]
        dt <- dt[,.(fecha, sexo, anios_cot, numero, pension_autofinanciada = pension_autofinanciada_promedio,
                    densidad_cot = densidad_cot_promedio)]
        
        #Guardar la tabla en el consolidado
        consolidado[[i]] <- dt
 
      } else if (as.numeric(fecha) %between% c(201708,201803)) {
        
        head1 <- as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 2)))[1,])
        head1 <- gsub("Pensión autofinanciada promedio", "pension_autofinanciada", head1)
        head1 <- gsub("Número", "numero", head1)
        head1 <- gsub("Densidad de cotizaciones", "densidad_cot", head1)
        head1 <- gsub("Tramos de años cotizados", "anios_cot", head1) ; head1 <- tolower(na.locf(head1))
        
        head2 <- tolower(as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 3)))[1,]))
        
        # Armar el encabezado
        headers <- map_chr(1:length(head1), ~ {
          ifelse(!is.na(head1[.x]) & !is.na(head2[.x]), 
                 paste(head1[.x], head2[.x] ,sep = "_"),
                 head1[.x])
        })
        
        #Descargar los datos
        dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 4)))
        names(dt) <- headers
        
        #Arreglar la tabla
        dt <- fill(dt, sexo)
        dt <- dt[!is.na(anios_cot)][!grepl("Total", anios_cot)][!grepl("TOTAL|Total", sexo)]
        dt[, sexo := tolower(sexo)]
        dt[, anios_cot := gsub(">", "(", anios_cot)][, anios_cot := gsub(" y <=", ", ", anios_cot)]
        dt[, anios_cot := gsub(" año", "", anios_cot)][, anios_cot := gsub("S/I \\(4\\)", "sin_info", anios_cot)]
        dt[!grepl("sin_info", anios_cot), anios_cot := paste0(anios_cot, "]")]
        
        #Arreglar columnas numericas
        cols <- tail(names(dt), 2)
        dt[, (cols) := lapply(.SD, function(x) as.numeric(gsub("-", NA, x))), .SDcols = cols]
        dt[, fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        dt <- dt[,.(fecha, sexo, anios_cot, numero, pension_autofinanciada = pension_autofinanciada_promedio,
                    densidad_cot = densidad_cot_promedio)]
        
        #Guardar la tabla en el consolidado
        consolidado[[i]] <- dt
        
      } else if (as.numeric(fecha) %between% c(201702, 201707)) {
        
        archivo_temporal <- tempfile(fileext = ".xls")
        url <- paste0("https://www.spensiones.cl/inf_estadistica/afipen/mensual/",year(ym(fecha)),"/",substr(fecha,5,6),"/",
                      "PensionesAutofinanciadasDensidadAnoCotSexo.xls")
        download.file(url, destfile = archivo_temporal, mode = "wb", quiet = T)
        
        # Encabezado de los datos
        head1 <- as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 2)))[1,])
        head1 <- gsub("Pensión promedio", "pension_autofinanciada", head1)
        head1 <- gsub("Número", "numero", head1)
        head1 <- gsub("Densidad de cotizaciones", "densidad_cot", head1)
        head1 <- gsub("Tramos de años cotizados", "anios_cot", head1) ; head1 <- tolower(na.locf(head1))
        
        head2 <- tolower(as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 3)))[1,]))
        
        # Armar el encabezado
        headers <- map_chr(1:length(head1), ~ {
          ifelse(!is.na(head1[.x]) & !is.na(head2[.x]), 
                 paste(head1[.x], head2[.x] ,sep = "_"),
                 head1[.x])
        })
        
        #Descargar los datos
        dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 4)))
        names(dt) <- headers
        
        #Arreglar la tabla
        dt <- fill(dt, sexo)
        dt <- dt[!is.na(anios_cot)][!grepl("Total", anios_cot)][!grepl("TOTAL|Total", sexo)]
        dt[, sexo := tolower(sexo)][, sexo := gsub("femenino", "mujeres", sexo)][, sexo := gsub("masculino", "hombres", sexo)]
        dt[, anios_cot := gsub(">", "(", anios_cot)][, anios_cot := gsub(" y <=", ", ", anios_cot)]
        dt[, anios_cot := gsub(" año", "", anios_cot)][, anios_cot := gsub("S/I \\(4\\)", "sin_info", anios_cot)]
        dt[!grepl("sin_info", anios_cot), anios_cot := paste0(anios_cot, "]")]
        
        #Arreglar columnas numericas
        cols <- tail(names(dt), 2)
        dt[, (cols) := lapply(.SD, function(x) as.numeric(gsub("-", NA, x))), .SDcols = cols]
        dt[, fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        dt <- dt[,.(fecha, sexo, anios_cot, numero, pension_autofinanciada = pension_autofinanciada_promedio,
                    densidad_cot = densidad_cot_promedio)]
        
        #Guardar la tabla en el consolidado
        consolidado[[i]] <- dt
        
      }
      
        else {
        
        # Encabezado de los datos
        head1 <- as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 2)))[1,])
        head1 <- gsub("Tramos de años cotizados", "anios_cot", head1) ; head1 <- tolower(na.locf(head1))
        
        head2 <- tolower(as.character(as.matrix(suppressMessages(read_excel(path = archivo_temporal, skip = 3)))[1,]))
        head2 <- gsub("número", "numero", head2)
        head2 <- gsub("pensión promedio", "pension_autofinanciada", head2)
        head2 <- gsub("densidad promedio", "densidad_cot", head2)
        
        # Armar el encabezado
        headers <- map_chr(1:length(head1), ~ {
          ifelse(!is.na(head1[.x]) & !is.na(head2[.x]), 
                 paste(head1[.x], head2[.x] ,sep = "_"),
                 head1[.x])
        })
        
        #Descargar los datos
        dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 4)))
        names(dt) <- headers
        
        #Arreglar la tabla
        dt <- dt[!grepl("Total", anios_cot)][!is.na(mujeres_numero)]
        dt[, anios_cot := gsub(">", "(", anios_cot)][, anios_cot := gsub(" y <=", ", ", anios_cot)]
        dt[, anios_cot := gsub(" año", "", anios_cot)][, anios_cot := gsub("S/I \\(4\\)", "sin_info", anios_cot)]
        dt[!grepl("sin_info", anios_cot), anios_cot := paste0(anios_cot, "]")]
        
        #Ordenar la tabla
        dt2 <- dt[,.(anios_cot, mujeres_numero, hombres_numero)]
        dt2 <- melt(dt2, id.vars = 1, variable.name = "factor", value.name = "numero")
        dt2 <- separate(dt2, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        
        dt3 <- dt[,.(anios_cot, mujeres_pension_autofinanciada, hombres_pension_autofinanciada)]
        dt3 <- melt(dt3, id.vars = 1, variable.name = "factor", value.name = "pension_autofinanciada")
        dt3 <- separate(dt3, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        
        dt4 <- dt[,.(anios_cot, mujeres_densidad_cot, hombres_densidad_cot)]
        dt4 <- melt(dt4, id.vars = 1, variable.name = "factor", value.name = "densidad_cot")
        dt4 <- separate(dt4, col = "factor", into = c("sexo", "dato"), sep = "_")[, dato := NULL]
        
        dt5 <- merge.data.table(merge.data.table(dt2, dt3, by = c("anios_cot", "sexo"), all = T),
                                dt4, by = c("anios_cot", "sexo"), all = T)
        
        #Arreglar columnas numericas
        cols <- tail(names(dt5), 2)
        dt5[, (cols) := lapply(.SD, function(x) as.numeric(gsub("-", NA, x))), .SDcols = cols]
        dt5[, fecha := paste(year(ym(fecha)), substr(fecha, 5,6), "01", sep = "-")]
        
        #Guardar la tabla en el consolidado
        consolidado[[i]] <- dt5
      }
    }, error = print)
  }
  return(rbindlist(consolidado, use.names = T))
  
}
descargar_nuevos_pensionados <- cmpfun(descargar_nuevos_pensionados)

#Descargar datos nuevos pensionados
if (exists("nuevos_pensionados")) {
  
  nuevos_pensionados_new <- suppressWarnings(descargar_nuevos_pensionados(
    fechas = fechas_consulta[fechas_consulta >= 201611], bd = nuevos_pensionados))
  
} else {
  
  nuevos_pensionados_new <- suppressWarnings(descargar_nuevos_pensionados(
    fechas = fechas_consulta[fechas_consulta >= 201611]))
  
}

try({
  
  #Crear columnas de años cotizados y fecha
  nuevos_pensionados_new[, ":="(anios_cotizados = 
                                  gsub("\\s\\s", " ", 
                                       gsub("sin_info", "Sin información",
                                            gsub("]", "",
                                                 gsub(",", " y menor o igual a ", 
                                                      gsub("\\(", "Mayor a ", anios_cot))))),
                                aux_cot = as.numeric(gsub("[^[0-9]]*", "", anios_cot)))]
  nuevos_pensionados_new[, aux_cot := ifelse(is.na(aux_cot), 4000, aux_cot)]
}, silent = T)

#Unir datos
if (exists("nuevos_pensionados")) {
  
  nuevos_pensionados <- rbindlist(list(nuevos_pensionados, nuevos_pensionados_new), use.names = T)

} else {
  
  nuevos_pensionados <- copy(nuevos_pensionados_new)
  
}
rm(nuevos_pensionados_new)

#Guardar datos
write.csv2(nuevos_pensionados, file = "input_dashboard/nuevos_pensionados.csv", row.names = F, na = "")

############################################################
### DATOS DEL SISTEMA DE PENSIONES SOLIDARIAS POR REGIÓN ###
############################################################

#Función para obtener los beneficiarios sps por region
descargar_sps <- function(fechas, bd) {
  
  #Control para que corra igual la función por si la base de datos no se encuentra en el ambiente de trabajo
  if (missing(bd)) {
    bd <- data.table(fecha = character(), region = character(), tipo_pension = character(),
                     tipo_beneficio = character(), sexo = character(), num_beneficiarios = integer(),
                     monto_beneficio = integer())
  } else {
    bd <- bd
  }
  
  #Lista vacía para guardar las tablas de datos
  consolidado <- list()
  
  #Descargar la información para cada fecha
  for (fecha in fechas) {
    
    #Saltar fechas que ya están en la base
    if (fecha %in% unique(gsub(pattern = "-|01$","",x = bd$fecha))) next
    
    #Crear un indice para ir guardando las tablas en un consolidado
    i <- match(fecha,fechas)
    
    tryCatch({
      
      #Crear un archivo temporal y descargar los datos
      archivo_temporal <- tempfile(fileext = ".xls")
      url <- paste0("https://www.spensiones.cl/inf_estadistica/sps/nbmpm/",year(ym(fecha)),"/c02nbmpm",fecha,".xls")
      download.file(url, destfile = archivo_temporal, mode = "wb", quiet = T)
      
      if (as.numeric(fecha) > 202001) {
        
        #Leer los datos y renombrar las columnas
        dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 4)))[,c(8:13, 16:19), with = F]
        names(dt) <- c("region","tipo_beneficio","num_mujeres_fin_estatal","monto_mujeres_fin_estatal",
                       "num_hombres_fin_estatal","monto_hombres_fin_estatal","num_mujeres_fin_cuenta_ind",
                       "monto_mujeres_fin_cuenta_ind","num_hombres_fin_cuenta_ind","monto_hombres_fin_cuenta_ind")
        
        #Rellenar datos y eliminar los totales y partes vacías
        dt <- fill(dt, region)
        dt <- dt[!grepl("^Total",region)]
        dt <- dt[-c(120:125),]
        
        #Crear tabla financiamiento estatal para las mujeres
        dt_muj_est <- dt[,.(region,tipo_beneficio,num_mujeres_fin_estatal,monto_mujeres_fin_estatal)]
        dt_muj_est[,":="(sexo = "mujeres", financiamiento = "Estatal")]
        dt_muj_est <- dt_muj_est[,.(region,tipo_beneficio,sexo,financiamiento,
                                    num_beneficiarios = num_mujeres_fin_estatal,
                                    mon_beneficio = monto_mujeres_fin_estatal)]
        
        #Crear tabla financiamiento estatal para los hombres
        dt_hom_est <- dt[,.(region,tipo_beneficio,num_hombres_fin_estatal,monto_hombres_fin_estatal)]
        dt_hom_est[,":="(sexo = "hombres", financiamiento = "Estatal")]
        dt_hom_est <- dt_hom_est[,.(region,tipo_beneficio,sexo,financiamiento,
                                    num_beneficiarios = num_hombres_fin_estatal,
                                    mon_beneficio = monto_hombres_fin_estatal)]
        
        #Crear tabla financiamiento cuenta individual para las mujeres
        dt_muj_ind <- dt[,.(region,tipo_beneficio,num_mujeres_fin_cuenta_ind,monto_mujeres_fin_cuenta_ind)]
        dt_muj_ind[,":="(sexo = "mujeres", financiamiento = "Cuenta Individual")]
        dt_muj_ind <- dt_muj_ind[,.(region,tipo_beneficio,sexo,financiamiento,
                                    num_beneficiarios = num_mujeres_fin_cuenta_ind,
                                    mon_beneficio = monto_mujeres_fin_cuenta_ind)]
        
        #Crear tabla financiamiento cuenta individual para los hombres
        dt_hom_ind <- dt[,.(region,tipo_beneficio,num_hombres_fin_cuenta_ind,monto_hombres_fin_cuenta_ind)]
        dt_hom_ind[,":="(sexo = "hombres", financiamiento = "Cuenta Individual")]
        dt_hom_ind <- dt_hom_ind[,.(region,tipo_beneficio,sexo,financiamiento,
                                    num_beneficiarios = num_hombres_fin_cuenta_ind,
                                    mon_beneficio = monto_hombres_fin_cuenta_ind)]
        
        #Unir las tablas
        dt <- rbindlist(list(dt_muj_est, dt_hom_est, dt_muj_ind, dt_hom_ind))
        
        #Crear columnas fecha y numero de región
        dt[, fecha := paste(year(ym(fecha)), substr(fecha,5,6), "01", sep = "-")]
        dt[, num_region := fcase(region == "Arica y Parinacota", 1,
                                 region == "Tarapacá", 2,
                                 region == "Antofagasta", 3,
                                 region == "Atacama", 4,
                                 region == "Coquimbo", 5,
                                 region == "Valparaíso", 6,
                                 region == "Metropolitana de Santiago", 7,
                                 region == "Libertador Gral. Bernardo O'Higgins", 8,
                                 region == "Maule", 9,
                                 region == "Ñuble", 10,
                                 region == "Biobío", 11,
                                 region == "La Araucanía", 12,
                                 region == "Los Ríos", 13,
                                 region == "Los Lagos", 14,
                                 region == "Aysén del Gral. Carlos Ibáñez del Campo", 15,
                                 region == "Magallanes y de la Antártica Chilena", 16,
                                 default = 0)]
        
        #Guardar los datos en el consolidado
        consolidado[[i]] <- dt
        
        #Tabla auxiliar para equiparar el nombre de las regiones (diferencias entre fechas de descarga)
        region <- unique(dt[,.(region,num_region)])
        region[, aux := toupper(stri_trans_general(str = region, id = "Latin-ASCII"))]
        region[, aux := fcase(grepl("LIBERTADOR", aux), "O'HIGGINS", 
                              grepl("BIOBIO", aux), "BIO BIO", 
                              grepl("^LA A", aux), "ARAUCANIA", 
                              grepl("AYSEN", aux), "AYSEN", 
                              grepl("MAGA", aux), "MAGALLANES", 
                              grepl("METRO", aux), "METROPOLITANA", 
                              grepl("NUBLE", aux), "CHILLÁN",
                              grepl("SIN INFORMACION", aux), "Sin Información",
                              rep_len(TRUE, length(aux)), aux)]
        region_nuble <- data.table(region = "Ñuble", num_region = 10, aux = "ÑUBLE")
        region <- rbindlist(list(region, region_nuble), use.names = T)
        
      } else if (as.numeric(fecha) %between% c(201905,202001)) {
        
        #Leer los datos y renombrar las columnas
        dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 4)))[,c(1:6),with = F]
        names(dt) <- c("region","tipo_beneficio","num_mujeres_fin_estatal","monto_mujeres_fin_estatal",
                       "num_hombres_fin_estatal","monto_hombres_fin_estatal")
        
        #Rellenar las columnas y eliminar datos vacíos
        dt <- fill(dt, region)
        dt <- dt[!grepl("^Total|^Fuente|^Notas|^\\(",region)]
        
        #Crear tabla de financiamiento estatal para las mujeres
        dt_muj_est <- dt[,.(region,tipo_beneficio,num_mujeres_fin_estatal,monto_mujeres_fin_estatal)]
        dt_muj_est[,":="(sexo = "mujeres", financiamiento = "Estatal")]
        dt_muj_est <- dt_muj_est[,.(region,tipo_beneficio,sexo,financiamiento,
                                    num_beneficiarios = num_mujeres_fin_estatal,
                                    mon_beneficio = monto_mujeres_fin_estatal)]
        
        #Crear tabla de financiamiento estatal para los hombres
        dt_hom_est <- dt[,.(region,tipo_beneficio,num_hombres_fin_estatal,monto_hombres_fin_estatal)]
        dt_hom_est[,":="(sexo = "hombres", financiamiento = "Estatal")]
        dt_hom_est <- dt_hom_est[,.(region,tipo_beneficio,sexo,financiamiento,
                                    num_beneficiarios = num_hombres_fin_estatal,
                                    mon_beneficio = monto_hombres_fin_estatal)]
        
        #Unir las tablas
        dt <- rbindlist(list(dt_muj_est, dt_hom_est))
        
        #Crear columna fecha y número de región
        dt[, fecha := paste(year(ym(fecha)), substr(fecha,5,6), "01", sep = "-")]
        dt[, num_region := fcase(region == "Arica y Parinacota", 1,
                                 region == "Tarapacá", 2,
                                 region == "Antofagasta", 3,
                                 region == "Atacama", 4,
                                 region == "Coquimbo", 5,
                                 region == "Valparaíso", 6,
                                 region == "Metropolitana de Santiago", 7,
                                 region == "Libertador Gral. Bernardo O'Higgins", 8,
                                 region == "Maule", 9,
                                 region == "Ñuble", 10,
                                 region == "Biobío", 11,
                                 region == "La Araucanía", 12,
                                 region == "Los Ríos", 13,
                                 region == "Los Lagos", 14,
                                 region == "Aysén del Gral. Carlos Ibáñez del Campo", 15,
                                 region == "Magallanes y de la Antártica Chilena", 16,
                                 default = 0)]
        
        #Guardar los datos en el consolidado
        consolidado[[i]] <- dt
        
      } else {
        
        #Leer los datos y renombrar las columnas
        dt <- data.table(suppressMessages(read_excel(path = archivo_temporal, skip = 6)))[,c(1:6), with = F]
        names(dt) <- c("region","tipo_beneficio","num_mujeres_fin_estatal","monto_mujeres_fin_estatal",
                       "num_hombres_fin_estatal","monto_hombres_fin_estatal")
        
        #Rellenar los datos de región y eliminar los totales
        dt <- fill(dt,region)
        dt <- dt[!grepl("^TOTAL|^Total|^\\(",region)]
        dt[, aux := trimws(gsub("[0-9]{1,}", "", region))][,region := NULL]
        dt[, aux := ifelse(is.na(aux), "SIN INFORMACION DE REGION", aux)]
        dt <- merge.data.table(dt,region, by = "aux", all.x = T)
        
        #Crear tabla de financiamiento estatal para mujeres
        dt_muj_est <- dt[,.(region,tipo_beneficio,num_mujeres_fin_estatal,monto_mujeres_fin_estatal,num_region)]
        dt_muj_est[,":="(sexo = "mujeres", financiamiento = "Estatal")]
        dt_muj_est <- dt_muj_est[,.(region,tipo_beneficio,sexo,financiamiento,
                                    num_beneficiarios = num_mujeres_fin_estatal,
                                    mon_beneficio = monto_mujeres_fin_estatal, num_region)]
        
        #Crear tabla de financiamiento estatal para hombres
        dt_hom_est <- dt[,.(region,tipo_beneficio,num_hombres_fin_estatal,monto_hombres_fin_estatal,
                            num_region)]
        dt_hom_est[,":="(sexo = "hombres", financiamiento = "Estatal")]
        dt_hom_est <- dt_hom_est[,.(region,tipo_beneficio,sexo,financiamiento,
                                    num_beneficiarios = num_hombres_fin_estatal,
                                    mon_beneficio = monto_hombres_fin_estatal, num_region)]
        
        #Unir las tablas
        dt <- rbindlist(list(dt_muj_est, dt_hom_est), use.names = T)
        
        #Crear columna fecha
        dt[, fecha := paste(year(ym(fecha)), substr(fecha,5,6), "01", sep = "-")]
        
        #Guardar los datos en el consolidado
        consolidado[[i]] <- dt
      }
    }, error = print
    )
  }
  return(rbindlist(consolidado, use.names = T))
}
descargar_sps <- cmpfun(descargar_sps)

#Descargar datos del sistema de pensiones solidarias (desde el 2010, antes de ese año los datos están raros)
if (exists("datos_sps")) {
  
  sps_nuevo <- descargar_sps(rev(fechas_consulta[fechas_consulta > 200912]), datos_sps)
  
} else {
  
  sps_nuevo <- descargar_sps(rev(fechas_consulta[fechas_consulta > 200912]))
  
}

try({
  
  #Limpiar variables categóricas y missing values pasarlos a cero
  sps_nuevo[, ":="(tipo_beneficio = trimws(gsub("\\([0-9]\\)", "", tipo_beneficio)),
                   num_beneficiarios = ifelse(is.na(num_beneficiarios), 0, num_beneficiarios),
                   mon_beneficio = ifelse(is.na(mon_beneficio), 0, mon_beneficio))]
  
  #Complemento de trabajo pesado tiene solo valores iguales a cero. Se elimina.
  sps_nuevo <- sps_nuevo[!grepl("Complemento", tipo_beneficio)]
  
  #Colapsar los APS de Vejez, crear tipo de pensión
  sps_nuevo[, tipo_beneficio := fcase(tipo_beneficio == "PBS Vejez", "PBS Vejez",
                                      tipo_beneficio == "PBS Invalidez", "PBS Invalidez",
                                      tipo_beneficio == "APS Invalidez", "APS Invalidez",
                                      default = "APS Vejez")]
  sps_nuevo[, tipo_pension := fcase(grepl("APS", tipo_beneficio), "APS",
                                    grepl("PBS", tipo_beneficio), "PBS")]
  
  #Sumar el número de beneficiarios y el monto de beneficio por fecha, region, tipo de pensión, sexo y beneficio
  sps_nuevo <- sps_nuevo[,.(num_beneficiarios = sum(num_beneficiarios), monto_beneficio = sum(mon_beneficio)),
                         by =.(fecha, region, tipo_pension, tipo_beneficio, sexo)]
  
  #Quedarse solo con los datos de vejez
  sps_nuevo <- sps_nuevo[!grepl("Invalidez", tipo_beneficio)]
  
}, silent = T)

#Unir los nuevos datos con el consolidado
if (exists("datos_sps")) {
  
  datos_sps <- rbindlist(list(sps_nuevo, datos_sps), use.names = T)
  
} else {
  
  datos_sps <- copy(sps_nuevo)
  
}
rm(sps_nuevo)

#Guardar datos
write.csv2(datos_sps, "input_dashboard/datos_sps.csv", row.names = F)

#Función para extraer la UF del Banco Central
extraer_uf <- function(fechas, bd) {
  
  #Control para que corra igual la función por si la base de datos no se encuentra en el ambiente de trabajo
  if (missing(bd)) {
    
    bd <- data.table(fecha = character(), uf = numeric())
    
  } else {
    
    bd <- bd
  }
  
  #Crear lista vacía para ir guardando las series anuales
  series_uf <- vector("list", length(fechas))
  
  #Abrir sesión remota de Chrome
  remDr <- remoteDriver(remoteServerAddr = "192.168.100.34", port = 4445L, browserName = "chrome")
  remDr$open()
  
  #Navegar por la Base de Datos Estadísticos del Banco Central
  remDr$navigate("https://si3.bcentral.cl/Siete/ES/Siete/Cuadro/CAP_PRECIOS/MN_CAP_PRECIOS/UF_IVP_DIARIO")
  
  for (fecha in fechas) {
    
    #Saltar año de descarga si ya está en la base descargada
    if (fecha %in% bd[, year(ymd(fecha))] & 
        bd[year(ymd(fecha)) == fecha, .N] == 12) next
    
    tryCatch({
      
      #Crear un índice para ir guardando los data table
      i <- match(fecha, fechas)
      
      #Buscar el año para extraer la serie
      webelem <- remDr$findElement(using = "css", paste0("[value = '",fecha,"']"))
      webelem$clickElement()
      
      #Extraer la serie de UF
      serie_uf <- data.table(html_table(html_nodes(read_html(remDr$getPageSource()[[1]]), xpath = "//table")[[2]]))
      
      #Ordenar la serie
      tidy_serie <- melt(serie_uf, id.vars = c(1,2), variable.name = "fecha", 
                         value.name = "valor")[,.(Serie, fecha, valor)]
      tidy_serie <- dcast(tidy_serie, formula = fecha ~ Serie, 
                          value.var = "valor")[,.(fecha, uf = `Unidad de fomento (UF)`)]
      tidy_serie <- tidy_serie[,":="(fecha = parse_date(gsub("sep","sept",tolower(sub("\\.", " ", fecha))), 
                                                        "%d %b%Y", locale = locale("es")),
                                     uf = as.numeric(gsub(",",".", gsub("\\.","",uf))))]
      
      #Se filtra por la UF del último día del mes. En la tabla se indica que la fecha es del primero del mes para 
      #poder hacer un merge directo con las demás tablas
      tidy_serie <- tidy_serie[,.(uf = tail(uf, n = 1)), by =.(fecha = paste(year(fecha), substr(fecha,6,7), "01",
                                                                             sep = "-"))]
      
      #Guardar data table en la lista
      series_uf[[i]] <- tidy_serie
      
    }, error = print)
  }
  remDr$close()
  return(rbindlist(series_uf, use.names = T))
}
extraer_uf <- cmpfun(extraer_uf)

#Extraer las series de UF
if (exists("serie_uf")) {
  
  serie_uf_nuevo <- extraer_uf(unique(year(ym(fechas_consulta))), serie_uf)

} else {
  
  serie_uf_nuevo <- extraer_uf(unique(year(ym(fechas_consulta))))
  
}

#Unir datos
if (exists("serie_uf")) {
  
  serie_uf <- rbindlist(list(serie_uf, serie_uf_nuevo), use.names = T)
  
} else {
  
  serie_uf <- copy(serie_uf_nuevo)
  
}
rm(serie_uf_nuevo)

#Eliminar duplicados
serie_uf <- unique(serie_uf)

#Guardar serie
write.csv2(serie_uf, "input_dashboard/serie_uf.csv", row.names = F)
