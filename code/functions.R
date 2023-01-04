get_url <- function(number) {
  #number <- 1
  xpath = "/html/body/div[3]/div/main/article/div/div/div/div/div/div[{number}]/div[2]/div/div/div/div/a"
  element <- remDr$findElement(using = "xpath", value = glue(xpath))  
  url <-  element$getElementAttribute(attrName = "href")
  return(url[[1]])
}

# Descargar datos
download_data <- function(url, file_name, relative_path) {

  file_directory <- paste0(relative_path, "/", file_name)
  curl_download(url,  file_directory)
}


# Descomprimir archivos .rar
unrar <- function(file, folder_name = NULL) {
  #apt-get install rar
  
  if (is.null(folder_name)) {
    new_folder <-  paste0("data/bruto/", str_extract(file, pattern = "[[:digit:]]+"))  
  } else {
    new_folder <-  paste0("data/bruto/", folder_name) 
  }
  
  
  
  # Crear nueva carpeta y descomprimir si el archivo no existe
  if ( !dir.exists(new_folder) ) {
    dir.create(new_folder)
    unrar_command = glue("unrar x {file} {new_folder}")
    system(unrar_command)  
  } 
}



unrar_windows <- function(file ) {
  
  folder_name <- paste0("data/bruto/", str_extract(file, pattern = "[[:digit:]]+"))
  
  
  # Crear nueva carpeta y descomprimir si el archivo no existe
  if ( !dir.exists(folder_name) ) {
    dir.create(folder_name)
}
    
    
    unrar_command <- glue('"C:/Program Files/7-Zip/7z" e {file} -o{folder_name}')
    
    system(unrar_command)  
  
  
} 




# Función para editar nombre de archivos
edit_names <- function(string) {
  string <- str_extract(tolower(string), "directorio.*")
  
  string <- str_replace_all(string, pattern = " ", replacement = "_") %>% 
    str_replace_all(pattern = ".xlsx|.xls", replacement = ".csv") %>% 
    str_remove_all(pattern = "_oficial_ee|_20220430_web") %>% 
    str_remove(pattern = "directoriooficial2003/")
  return(string)
}

# Homologar nombres de los identificadores
create_new_names <- function(data) {
  names(data)[names(data) == "rbd"] <- "rbd_n"
  names(data)[names(data) == "dgv_rbd"] <- "rbd_d"
  names(data)[names(data) == "dv_rbd"] <- "rbd_d"
  names(data)[names(data) == "rbd_dv"] <- "rbd_d"
  
  names(data)[names(data) == "iagno"] <- "anio"
  names(data)[names(data) == "agno"] <- "anio"
  names(data)[names(data) == "ano"] <- "anio"
  
  return(data)
}


no_match <- function(df1, df2, id) {
  anti <- df1 %>% 
    anti_join(df2, by = id)
  return(anti)
}
