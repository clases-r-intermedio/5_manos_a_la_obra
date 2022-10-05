################################
# CARGAR LIBRERÍAS Y FUNCIONES #
################################

# No es tan buena idea cargar todo tidyverse
library(RSelenium)
library(glue)
library(purrr)
library(stringr)
library(data.table)
library(readr)
library(janitor)
library(readxl)
library(dplyr)

# Funciones
source(file = "code/functions.R", encoding = "utf-8")


##################
# CONSEGUIR URLS #
##################

#### Opción 1: Recolectar manualmente todas las url de descarga

urls <- c("https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial1992.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial1993.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial1994.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial1995.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial1996.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial1997.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial1998.zip",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial1999.zip",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial2000.zip",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial2001.zip",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial2002.zip",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/DirectorioOficial2003.zip",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio_oficial_EE_2004.csv",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio_oficial_EE_2005.csv",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio_oficial_EE_2006.csv",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio_oficial_EE_2007.csv",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio_oficial_EE_2008.csv",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio_oficial_EE_2009.csv",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio_oficial_EE_2010.csv",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio_oficial_EE_2011.csv",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio_oficial_EE_2012.csv",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio-oficial-EE-2013.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio-oficial-EE-2014.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio-Oficial-EE-2015.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio-oficial-EE-2016.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio-oficial-EE-2017.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio-oficial-EE-2018.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio-oficial-EE-2019.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio-oficial-EE-2020.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2021/12/Directorio-oficial-EE-2021.rar",
"https://datosabiertos.mineduc.cl/wp-content/uploads/2022/09/Directorio-oficial-EE-2022.rar")



#### Opción 2: identificar cuáles son las url mediante la exploración automática del html de la página
# ¡¡Esta opción requiere que tengan instalado Java en sus computadores!!

# Conexión con el servidor selenium
rdriver <- rsDriver(browser = "chrome",
                    port = 2122L,
                    chromever  = "105.0.5195.52"
)
remDr <- rdriver[["client"]]


# Navegar en la página
remDr$navigate("https://datosabiertos.mineduc.cl/directorio-de-establecimientos-educacionales/")

# Ejemplo: Encontrar los elementos que nos interesan
element <- remDr$findElement(using = "xpath", value = '/html/body/div[3]/div/main/article/div/div/div/div/div/div[2]/div[2]/div/div/div/div/a')

# Encontrar el atributo que nos interesa. En este caso, usamos href 
element$getElementAttribute(attrName = "href")

# Almacenar todas las url
urls <- map_chr(1:31, get_url)

# No olviden desconectar el servidor selenium
remDr$closeServer()
remDr$close()
rdriver[["server"]]$stop()
rm(rdriver)
gc()


###################
# DESCARGAR DATOS #
###################

# Crear el directorio para almacenar los datos. Esto es opcional, pero ayuda a reproducir el trabajo. 
# No sé si funciona en windows Hay un equivalente en R:  if ( !dir.exists("data/bruto") ) dir.create("data/bruto")
system("mkdir data -p && mkdir data/bruto -p")

# Crear el nombre de los archivos que descargaremos
file_names <- map_chr(urls, ~str_extract(.x, "Directorio.*")) 

# Descargar datos con función download_data 
walk2(urls, file_names, ~download_data(url = .x, file_name = .y, relative_path = "data/bruto"))

#########################
# DESCOMPRIMIR ARCHIVOS #
#########################

# Encontrar los archivos que están comprimidos 
zip <- list.files("data/bruto/", pattern = "zip", full.names = T)
rar <- list.files("data/bruto/", pattern = "rar", full.names = T)

# Descomprimir archivos zip
walk(zip, ~unzip(.x, exdir=paste0("data/bruto/", str_extract(.x, pattern = "[[:digit:]]+"))))

# Descomprimir archivos rar
# La función unrar es mía y solo corre en Linux. 
walk(rar, ~unrar(.x))

################################
# LLEVAR TODO AL MISMO FORMATO #
################################

# Crear carpeta para recibir archivos csv 
if ( !dir.exists("data/csv") ) dir.create("data/csv")

# Encontrar todos los archivos csv y excel en los datos brutos
csv_files <- list.files("data", pattern = "csv", recursive = T, full.names = T)
excel_files <- list.files("data", pattern = " [[:digit:]]{4}.xls", recursive = T, full.names = T)

# Leer todos los archivos csv con fread para asegurarnos de que tengan el mismo separador (fread reconoce el separador)
csv <- map(csv_files, fread)
csv_names <-map_chr(csv_files, edit_names)
walk2(csv, csv_names, ~fwrite(.x, file = paste0("data/csv/", .y), sep = ";"  ))


# Leer todos los archivos excel 
excel <- map(excel_files, read_excel )

# Editar nombre de archivos
excel_names <- map_chr(excel_files, edit_names)

# Guardar en carpeta nueva
walk2(excel, excel_names, ~fwrite(.x, file = paste0("data/csv/", .y), sep = ";"  ))

# Limpieza básica de nombre de columnas (función anónima)
clean_csv <- map(list.files("data/csv", full.names = T), function(x) {clean_names(fread(x))  }  )

# Archivos csv con una limpieza básica de columnas. 
# Requiere homologar columnas y otras cosas
walk2(clean_csv, list.files("data/csv", full.names = T), ~fwrite(x = .x, file = .y, sep = ";"))



#########################################
# DESCARGAR DATOS DE RENDIMIENTO ALUMNOS 
#########################################

# Se descarga directamente con una url
rendimiento <- "https://datosabiertos.mineduc.cl/wp-content/uploads/2022/04/Rendimiento-2021.rar"
download_data(url = rendimiento , file_name = "rendimiento.rar", relative_path = "data/bruto")

# Descomprimir con la función creada 
unrar("data/bruto/rendimiento.rar", folder_name = "rendimiento")

# Crear nueva carpeta
dir.create("data/csv_rendimiento")

# Copiar a una carpeta para mantener estándar de los otros datos
data_rendimiento <- fread("data/bruto/rendimiento/20220302_Rendimiento_2021_20220131_WEB.csv")
fwrite(data_rendimiento, file = "data/csv_rendimiento/rendimiento_2021.csv", sep = ";")

