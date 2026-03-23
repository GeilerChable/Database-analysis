#An√°lisis Descriptivo de mi base de datos "Anatomia_Parcial1"

# 1. Cargar librer√≠as necesarias
library(dplyr)

# 2. Configurar directorio y leer base
setwd("C:/Users/geile/Desktop/MME/Semestre Enero - Junio/RegresionL/Proyecto_Unidad1")
base = read.csv("Anatomia_Parcial1.csv")

# 3. Limpiar base (ahora el %>% funcionar√°)
base_limpia <- base %>% 
  select(1:4) %>% 
  filter(!is.na(Peso..kg.))

# 4. An√°lisis descriptivo
summary(base_limpia)

#---Construcciones de Caja y pivotes --- 
par(mfrow=c(2,2)) # Dividimos la pantalla en 4

boxplot(base_limpia$Peso..kg., main="Peso (kg)", col="lightblue")
boxplot(base_limpia$Altura..cm., main="Altura (cm)", col="orange")
boxplot(base_limpia$Longitud.pie, main="Pie (cm)", col="lightgreen")
boxplot(base_limpia$Circunferencia.cabeza, main="Cabeza (cm)", col="pink")

par(mfrow=c(1,1)) # Regresamos a la normalidad

#Correlaciones
# Calculamos la matriz de correlaci√≥n
matriz_cor <- cor(base_limpia)
print(matriz_cor)



# -------- Un gr√°fico de dispersi√≥n m√∫ltiple para ver todas las relaciones de un golpe
pairs(base_limpia, main="Matriz de Dispersi√≥n de Anatom√≠a", col="darkblue", pch=19)

# 1. Instalamos y cargamos la librer√≠a
install.packages("GGally")
library(GGally)

# 2. Creamos la matriz visual
ggpairs(base_limpia, 
        title = "Matriz de Correlaci√≥n de Datos de Anatom√≠a",
        lower = list(continuous = wrap("smooth", color = "blue", alpha = 0.3))) # A√±ade l√≠nea de tendencia

#ModificaciÛn fila 1, columna 2

library(GGally)

ggpairs(base, 
        upper = list(continuous = wrap("points", color = "blue", alpha = 0.5, size = 1)),
        diag = list(continuous = wrap("densityDiag", fill = "lightblue", alpha = 0.5)),
        lower = list(continuous = wrap("points", color = "blue", alpha = 0.5, size = 1))) +
  theme_bw()


#--------- Mapa de Calor
install.packages("corrplot")
library(corrplot)

# Primero calculamos la matriz num√©rica
M <- cor(base_limpia)

# Dibujamos el mapa de calor
corrplot(M, method = "color", 
         type = "upper",          # Solo muestra la parte de arriba (evita repeticiones)
         addCoef.col = "black",   # Muestra el n√∫mero de la correlaci√≥n
         tl.col = "black", tl.srt = 45) # Color y rotaci√≥n de etiquetas


#Modelo de RLS de Altura - Peso
library(ggplot2)

ggplot(base, aes(x = `Altura..cm.`, y = `Peso..kg.`)) +
  geom_point(color = "darkblue", alpha = 0.5, size = 2) + # Puntos azules transl˙cidos
  geom_smooth(method = "lm", color = "red", fill = "gray", se = TRUE) + # Recta de regresiÛn lineal roja
  labs(
    title = "Diagrama de DispersiÛn: Peso vs. Altura",
    subtitle = "El Peso es la variable de respuesta (Eje Y)",
    x = "Altura (cm)",
    y = "Peso (kg)"
  ) +
  theme_bw() + # Fondo blanco con cuadrÌcula, para mayor claridad
  theme(plot.title = element_text(face = "bold", hjust = 0.5))




#---- Gr·fico externo 
# 1. Abrir una ventana de gr·ficos externa
windows() 

# 2. Cargar librerÌas y datos (aseg˙rate de que existan)
library(ggplot2)

# 3. Ejecutar el gr·fico (se abrir· en una ventana nueva flotante)
ggplot(base, aes(x = `Altura..cm.`, y = `Peso..kg.`)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm") +
  labs(title = "Peso vs Altura", y = "Peso (kg)", x = "Altura (cm)")