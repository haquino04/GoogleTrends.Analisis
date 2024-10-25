

#Busquedas en google en tiempo real - Google trends!!
#-------------------------------------------------------------

#Limpiamos el work space, Cambiamos el directorio de trabajo y fijamos UTF-8 para manipulación de caracteres
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


library(gtrendsR) # Para acceder a los datos de Google Trends
library(DT) # Para crear tablas dinámicas en el entorno de RStudio
library(tidyverse) 
library(ggplot2) 
library(scales) # Para formatear las escalas de los gráficos (e.g., porcentajes, fechas)


# Obtener datos de Google Trends para las palabras clave "Chatgpt" y "IA" en Perú, de los últimos 3 meses.
datos = gtrends(keyword = c("chatgpt", "IA"),
                geo = "PE", # Filtrar por Perú
                time = "today 3-m") # Últimos tres meses

# Mostrar los datos obtenidos en formato gráfico.
plot(datos)

# Mostrar la tabla de tendencias de búsqueda a lo largo del tiempo.
datatable(datos$interest_over_time)

# Mostrar la tabla de interés por ciudad.
datatable(datos$interest_by_city)

# Limpiar y procesar los datos.
# Convertimos las fechas a formato Date y normalizamos los valores de "hits" (que representan la intensidad de búsqueda).
interes = datos$interest_over_time %>%
  mutate(date = as.Date(date)) %>% # Convertir la fecha a formato Date
  group_by(keyword) %>% # Agrupar por palabra clave
  mutate(hits = as.numeric(hits), # Convertir hits a numérico
         hits = ifelse(is.na(hits), 0, hits), # Reemplazar valores NA por 0
         mhits = max(hits), # Encontrar el valor máximo de hits para cada palabra clave
         hits = hits / mhits) # Normalizar hits para que los valores estén entre 0 y 1
datatable(interes)

# Crear un gráfico de línea que muestre la evolución de las búsquedas de "Chatgpt" y "IA" en el tiempo.
plot1 = ggplot(interes, aes(x = date, y = hits, col = keyword)) + 
  geom_line(size = 0.8) # Dibujar líneas con diferentes colores para cada palabra clave
plot1

# Añadir puntos al gráfico en las fechas de inicio y fin del rango de datos.
extremos = interes %>% filter(date == max(date) | date == min(date))
maximos = interes %>% filter(date == max(date))
plot2 = plot1 +
  geom_point(data = extremos, size = 3) + # Añadir puntos en extremos
  geom_point(data = extremos, size = 1, col = "white") + # Puntos más pequeños en blanco dentro de los anteriores
  geom_text(data = maximos, aes(x = date + 1, label = keyword), hjust = "left", size = 4) # Etiquetas de las palabras clave
plot2

# Añadir títulos y subtítulos para mejorar la interpretación del gráfico.
plot3 = plot2 + 
  guides(col = F) + # Eliminar leyenda de color
  labs(x = "", y = "", 
       title = "Número de búsquedas del IA (Últimos tres meses - Perú)", # Título del gráfico
       subtitle = "Fuente de datos: Google Trends", # Subtítulo
       caption = "CADPeru") # Pie de página
plot3

# Ajustar el eje Y para mostrar porcentajes, con un rango de 0 a 1.
plot4 = plot3 + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), # Marcas en el eje Y cada 25%
                     labels = percent_format(accuracy = 1), # Formato de porcentaje
                     limits = c(0, 1)) # Limitar el eje Y entre 0 y 1
plot4

# Ajustar el eje X para que las fechas se muestren cada semana y en la parte superior.
plot5 = plot4 + 
  scale_x_date(breaks = seq.Date(min(interes$date), max(interes$date), by = 7), # Mostrar fechas cada semana
               labels = date_format(format = "%b-%d"), # Formato de fecha: mes-día
               limits = c(min(interes$date), max(interes$date) + 10), # Límite del rango de fechas
               position = "top") # Mover las etiquetas de fecha a la parte superior
plot5

# Personalizar los colores de las líneas para "Chatgpt" e "IA".
colores = c("Chatgpt" = "#c8446c", "IA" = "#f377fb")
plot6 = plot5 + 
  scale_color_manual(values = colores) # Aplicar los colores personalizados
plot6

# Aplicar un tema minimalista al gráfico y ajustar la visualización de la cuadrícula.
plot7 = plot6 + theme_minimal() + 
  theme(panel.grid.major.y = element_blank(), # Eliminar la cuadrícula mayor en Y
        panel.grid.minor.y = element_blank(), # Eliminar la cuadrícula menor en Y
        panel.grid.minor.x = element_blank(), # Eliminar la cuadrícula menor en X
        axis.text.x = element_text(angle = 45, hjust = 0)) # Rotar las etiquetas del eje X
plot7

# Añadir una línea vertical que marque un evento importante (e.g., inicio del confinamiento).
plot8 = plot7 + 
  geom_vline(xintercept = as.Date("2024-09-01"), alpha = 0.2, size = 4) + # Línea vertical en la fecha del evento
  ggplot2::annotate("text", x = as.Date("2024-09-01"), y = 1, 
           label = "· Ciencia de Datos", hjust = "left") # Añadir texto junto a la línea
plot8


# Obtenemos tendencias de búsqueda en Perú durante los últimos 3 años
#______________________________________________________________________________________

datos = gtrends(keyword = c("Chatgpt", "IA"), 
                geo = "PE", time = "today+3-y") # Buscar en España en los últimos 5 años
interes = datos$interest_over_time %>%
  group_by(keyword) %>%  
  mutate(hits = as.numeric(hits), # Convertir hits a numérico
         hits = ifelse(is.na(hits), 0, hits), # Reemplazar valores NA por 0
         mhits = max(hits), 
         hits = hits / mhits) # Normalizar

maximos = interes %>% filter(date == max(date))

# 17. Crear una animación para mostrar la evolución de las búsquedas en el tiempo.
library(gganimate)
plot = ggplot(interes, aes(x = date, y = hits, col = keyword)) + 
  geom_line() + # Dibujar línea
  transition_reveal(date) + # Añadir animación de revelación a lo largo del tiempo
  geom_point(size = 3) + # Puntos en cada fecha
  labs(x = "", y = "", 
       title = "Google Trends: Chatgpt vs. IA", # Título de la animación
       caption = "@cadperu", 
       col = "") + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), 
                     labels = percent_format(accuracy = 1), 
                     limits = c(0,1)) + 
  scale_color_manual(values = c("#0054cf", "#343334")) + 
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        legend.position="top")

# Renderizar la animación y guardarla en formato GIF.
library(gifski)
animacion = animate(plot, end_pause = 10, nframes = 100, 
                    width = 500, height = 250, renderer = gifski_renderer())


#Guardamos el archivo y lo podemos ver en nuestra carpeta
anim_save("animacion.gif", animation = animacion)

