setwd("C:/Users/geile/Desktop/MME/Semestre Enero - Junio/RegresionL/Proyecto_Unidad1")
base = read.csv("Anatomia_Parcial1.csv") #Leyendo la base de datos 

base_limpia <- base %>% 
  # Esto selecciona solo las columnas que NO se llamen '...5', si es que existen
  select(1:4) %>% 
  # Esto limpia las filas donde el Peso sea NA
  filter(!is.na(`Peso..kg.`))


# 1. Modelo: Peso vs Altura
modelo_altura <- lm(Peso..kg. ~ Altura..cm., data = base_limpia)

# 2. Modelo: Peso vs Longitud del pie
modelo_pie <- lm(Peso..kg. ~ Longitud.pie, data = base_limpia)

# 3. Modelo: Peso vs Circunferencia de la cabeza
modelo_cabeza <- lm(Peso..kg. ~ Circunferencia.cabeza, data = base_limpia)

# --- Ver resultados ---
summary(modelo_altura)
summary(modelo_pie)
summary(modelo_cabeza)

par(mfrow=c(1,2)) # Divide la pantalla en 3 espacios

plot(base_limpia$Altura..cm., base_limpia$Peso..kg., main="Peso vs Altura")
abline(modelo_altura, col="red")

plot(base_limpia$Longitud.pie, base_limpia$Peso..kg., main="Peso vs Pie")
abline(modelo_pie, col="blue")

plot(base_limpia$Circunferencia.cabeza, base_limpia$Peso..kg., main="Peso vs Cabeza")
abline(modelo_cabeza, col="green")


#---------------------
par(mfrow=c(1,1)) # Regresa la pantalla a la normalidad

# Función para extraer lo más importante de cada modelo
resumen_modelos <- data.frame(
  Variable = c("Altura", "Pie", "Cabeza"),
  R_Cuadrado = c(summary(modelo_altura)$r.squared, 
                 summary(modelo_pie)$r.squared, 
                 summary(modelo_cabeza)$r.squared),
  Error_Estandar = c(summary(modelo_altura)$sigma, 
                     summary(modelo_pie)$sigma, 
                     summary(modelo_cabeza)$sigma)
)

print(resumen_modelos)
