
library(RSelenium)
library(glue)
library(purrr)
library(stringr)
library(data.table)
library(readr)
library(janitor)
library(readxl)
library(dplyr)
library(chilemapas)
library(ggplot2)

# Funciones
source(file = "code/functions.R", encoding = "utf-8")

#########################
# OBTENER ALGUNOS DATOS #
#########################

# Cargar todos los datos
clean_csv <- map(list.files("data/csv", full.names = T), fread, dec = ","     )

# Seleccionar columnas relevantes
rbd_data <- map(clean_csv, ~.x %>% 
                  select(1:3))  

# Homologar nombres del identificador
rbd_data2 <- rbd_data %>% 
  map(create_new_names) %>% 
  map(~.x %>%
        select(rbd_n, rbd_d) %>%
        mutate(id = paste(rbd_n, rbd_d, sep = "-")) %>% 
        distinct()
        )


#¡¡¡!!!!! Identificar los colegios que están presentes en todos los años
all_years <- rbd_data2 %>%
  map(~.x %>% select(id) ) %>% 
  reduce(inner_join, by = "id")

# ¡¡¡!!!! Identificar colegios perdidos de un año a otro
# No hacen match de un año a otro

for (i in 1:length(rbd_data2)) {
  if (i < length(rbd_data2)) {
     no_match(rbd_data2[[i]] , rbd_data2[[i+1]], "id") %>% 
      nrow() %>%
      print()
    
  }
  
}


############################
# VISUALIZACIÓN GEOGRÁFICA #
############################
options(scipen = "999")
anio_2021 <- clean_csv[[31]]

# shape files de la región metropolitana
mapa_rm <- mapa_comunas %>% 
  filter(codigo_region == 13) 

# Puntos geografícos de los colegios de la rm
colegios_rm <- anio_2021 %>% 
  filter(cod_reg_rbd == 13) %>% 
  filter(!is.na(latitud) & !is.na(longitud)) %>% 
  filter(longitud < -30) # sacar un dato extremo

# Ubicación de los colegios en la región metropolitana
ggplot(mapa_rm) +
  geom_sf(aes(geometry = geometry)) +
  geom_point(data = colegios_rm, aes(x = longitud, y = latitud), size = 0.8)

# Pago mensual 
colegios_comuna <- anio_2021 %>% 
  filter(cod_reg_rbd == 13) %>% 
  filter( pago_mensual != "SIN INFORMACION" ) %>% 
  group_by(cod_com_rbd, nom_com_rbd) %>% 
  summarise(n_colegios = n()) %>% 
  ungroup()

pago_mensual <- anio_2021 %>% 
  filter(cod_reg_rbd == 13) %>% 
  filter( pago_mensual != "SIN INFORMACION" ) %>% 
  group_by(cod_com_rbd, pago_mensual) %>% 
  summarise(contar = n()) %>% 
  ungroup() %>% 
  left_join(colegios_comuna, by = "cod_com_rbd") %>% 
  mutate(porcentaje = contar / n_colegios * 100)

# Crear una tabla que contiene todos los tipos de pago para todas las comunas
comunas <- pago_mensual %>% 
  group_by(cod_com_rbd, nom_com_rbd) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(cod_com_rbd = as.character(cod_com_rbd)) %>% 
  select(cod_com_rbd, nom_com_rbd)

#  Armar mapa con porcentaje de colegios que valen más de 100.000
mapa_rm2 <- mapa_rm %>% 
  left_join(pago_mensual %>% 
              mutate(cod_com_rbd = as.character(cod_com_rbd)) %>% 
              filter(pago_mensual == "MAS DE $100.000"),
            by = c("codigo_comuna" = "cod_com_rbd")) %>% 
  left_join(comunas, 
            by = c("codigo_comuna" = "cod_com_rbd")) %>% # recuperar nombre de comunas que no tienen más de 100.000
  mutate(porcentaje = if_else(is.na(porcentaje), 0, porcentaje), # pasar a 0 las comunas que no tienen más de 100.000
         pago_mensual = if_else(is.na(pago_mensual), "MAS DE $100.000", pago_mensual),
         comuna = nom_com_rbd.y
         )   

ggplot(mapa_rm2) + 
  geom_sf(aes(fill = porcentaje, geometry = geometry)) #+
  #geom_sf_label(aes(label = comuna, geometry = geometry)) 

#####################
# DATOS RENDIMIENTO # 
#####################
rendimiento <- fread(file = "data/csv_rendimiento/rendimiento_2021.csv", dec = ",")

# LImpiar nombres
rendimiento <- rendimiento %>% 
  clean_names() %>% 
  mutate(id = paste(rbd, dgv_rbd, sep = "-"))

# Promedio para alumnos no retirados
promedio_notas <- rendimiento %>%
  filter(prom_gral > 0) %>% 
  mutate(reprobado = if_else(sit_fin == "R", 1, 0),
         retirado = if_else(sit_fin == "Y", 1, 0)) %>% 
  group_by(id) %>% 
  summarise(media = mean(prom_gral), reprobado = mean(reprobado) * 100, retirado = mean(retirado) * 100)

# Agregar info geográfica
promedio_notas <- promedio_notas %>% 
  left_join(anio_2021 %>%
              mutate(id = paste(rbd, dgv_rbd, sep = "-")) %>% 
              select(id, cod_reg_rbd, cod_com_rbd ),
            by = "id") %>% 
  group_by(cod_com_rbd) %>% 
  summarise(media = mean(media), reprobado = mean(reprobado) , retirado = mean(retirado) ) %>% 
  ungroup()

# Llevar la información de rendimiento a la tabla inicial que contiene pagos
mapa_final <- mapa_rm2 %>% 
  left_join(promedio_notas %>% 
              mutate(cod_com_rbd = as.character(cod_com_rbd)),
            by = c("codigo_comuna" = "cod_com_rbd") 
            )

# Mapa media notas
ggplot(mapa_final) + 
  geom_sf(aes(fill = media, geometry = geometry))

# Porcentaje reprobado
ggplot(mapa_final) + 
  geom_sf(aes(fill = reprobado, geometry = geometry))

