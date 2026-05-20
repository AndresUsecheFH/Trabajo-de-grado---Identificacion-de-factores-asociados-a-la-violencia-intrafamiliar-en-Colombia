#Andres Felipe Useche Hernandez
#Codigo sección de Fisher, ACM, Fuzzy C-Means


#Librerias
install.packages("readxl")
library(readxl)
install.packages('FactoMineR')
install.packages('factoextra')
install.packages("ggrepel")
library(FactoMineR)
install.packages("rlang")
library(rlang)
library(factoextra)
library("ggrepel")
library(dplyr)
library(ggplot2)
library(writexl)
library(ppclust)
library(tidyr)

if(!require(plotly)) install.packages("plotly")
library(plotly)

if(!require(ggalluvial)) install.packages("ggalluvial")
library(ggalluvial)
library(dplyr)

#Carga de datos

datos_1 <- read_excel("C:/Users/usech/Downloads/ver1_2.xlsx")


head(datos_1)
colnames(datos_1)

# Algunas asociaciones importantes no pueden evaluarse mediante la prueba Chi-cuadrado de independencia,
# debido al incumplimiento de sus supuestos (por ejemplo, bajas frecuencias esperadas).
# En estos casos, se utiliza la prueba exacta de Fisher como alternativa.
pares_fuertes <- data.frame(
  v1 = c(
  
    "Circunstancia del Hecho Detallada",
    "Mecanismo Causal de la Lesión no Fatal",
    "Diagnostico Topográfico de la Lesión no Fatal",
    "Sexo del Agresor",
    "Escenario del Hecho",
    "Mecanismo Causal de la Lesión no Fatal",
    "Actividad Durante el Hecho",
    "Escenario del Hecho",
    "Zona del Hecho",
    "Escenario del Hecho",
    "Grupo Mayor Menor de Edad",
    "Sexo del Agresor",
    "Circunstancia del Hecho Detallada",
    "Presunto Agresor Detallado",
    "Circunstancia del Hecho Detallada"
  ),
  
  v2 = c(
    
    "Factor Desencadenante de la Agresión",
    "Días de Incapacidad Medicolegal",
    "Días de Incapacidad Medicolegal",
    "Presunto Agresor Detallado",
    "Actividad Durante el Hecho",
    "Diagnostico Topográfico de la Lesión no Fatal",
    "Sexo del Agresor",
    "Presunto Agresor Detallado",
    "Escenario del Hecho",
    "Sexo del Agresor",
    "Días de Incapacidad Medicolegal",
    "Sexo de la victima",
    "Presunto Agresor Detallado",
    "Factor Desencadenante de la Agresión",
    "Ciclo Vital"
  )
)


# Prueba exacta de Fisher
# Dado el tamaño del conjunto de datos y la dimensionalidad de las tablas de contingencia,
# el cálculo exacto de la prueba de Fisher no es computacionalmente viable.
# En su lugar, se emplea una aproximación basada en simulación de Monte Carlo
# con 100000 replicaciones para estimar el valor p.


resultados_fisher <- data.frame()

for(i in 1:nrow(pares_fuertes)){
  
  var1 <- pares_fuertes$v1[i]
  var2 <- pares_fuertes$v2[i]
  
  tabla <- table(datos_1[[var1]], datos_1[[var2]])
  
 
  test <- fisher.test(
    tabla,
    simulate.p.value = TRUE,
    B= 1000000
    
  )
  
  resultados_fisher <- rbind(
    resultados_fisher,
    data.frame(
      Variable_1 = var1,
      Variable_2 = var2,
      p_value = test$p.value,
      filas = nrow(tabla),
      columnas = ncol(tabla)
    )
  )
  
}

resultados_fisher

View(resultados_fisher)



# Analisis de correspondencia Multiple (ACM)

#ACM I

#Se selecionan las variables categorias que entran al ACM I

cat_vars <- c( "Sexo de la victima", 
              
               "Ciclo Vital",
               "Escolaridad",
               "Estado Civil",
               "Pertenencia Grupal",
               "Zona del Hecho", 
               "Escenario del Hecho",
               "Actividad Durante el Hecho",
               "Circunstancia del Hecho Detallada", 
               "Contexto del Hecho", 
               "Sexo del Agresor",
               "Presunto Agresor Detallado",
               "Mecanismo Causal de la Lesión no Fatal",
               "Diagnostico Topográfico de la Lesión no Fatal",
               "Factor Desencadenante de la Agresión",
               "Días de Incapacidad Medicolegal",
               "Tipo de Discapacidad",
               "Orientación Sexual"
               
               
)


# Se genera una copia del conjunto de datos, se recodifica una de las variables categóricas
# para mejorar su interpretación y se distinguen las variables e individuos suplementarios,
# los cuales no participan en la construcción del espacio factorial del análisis
# pero si se pueden representar en los planos factoriales.

df_clean <- datos_1[, cat_vars]

names(df_clean) <- gsub("Circunstancia.del.Hecho.Detallada",
                        "Circunstancia.del.Hecho",
                        names(df_clean))



df_clean$Contexto.del.Hecho <- recode(df_clean$"Contexto del Hecho",
                                      "6 Lesiones no Fatales por Violencia de Pareja" = "Violencia de pareja",
                                      "4 Lesiones no Fatales por Violencia entre otros Familiares" = "Violencia entre familiares",
                                      "3 Lesiones no Fatales contra Niños, Niñas y Adolescentes por Violencia Intrafamiliar" = "Violencia  contra NNA",
                                      "5 Lesiones no Fatales contra el Adulto Mayor por Violencia Intrafamiliar" = "Violencia contra el adulto mayor"
)
df_clean <- df_clean[, -10]

valores_invalidos <- c(
  "Sin información", "sin información", "SIN INFORMACIÓN",
  "Sin informacion", "sin informacion",
  "Por determinar", "por determinar",
  "Otros", "otros",
  "Sin información/Otros", "sin informacion/otros"
)

invalid_matrix <- as.data.frame(
  lapply(df_clean, function(x) x %in% valores_invalidos)
)
invalid_ratio <- rowMeans(invalid_matrix, na.rm = TRUE)

# Los registros que presentan más del 15% de valores faltantes o categorías no válidas
# son tratados como individuos suplementarios, por lo que no participan en la construcción
# del espacio factorial, pero se proyectan posteriormente para su interpretación.

ind_sup <- which(invalid_ratio > 0.15)

length(ind_sup)
df_clean[] <- lapply(df_clean, as.factor)

# Categorías tratadas como suplementarias:
# se incluyen aquellas con baja asociación estadística dentro del conjunto de variables,
# así como aquellas que presentan menos del 80% de información válida.
# Estas no participan en la construcción del espacio factorial, pero se proyectan para su interpretación.

quali_sup_vars <- c(
  "Orientación.Sexual",
  "Pertenencia.Grupal",
  "Zona.del.Hecho", 
   "Escenario.del.Hecho",
  "Actividad.Durante.el.Hecho",
  "Mecanismo.Causal.de.la.Lesión.no.Fatal",
  "Diagnostico.Topográfico.de.la.Lesión.no.Fatal",
  "Días.de.Incapacidad Medicolegal",
  "Tipo.de.Discapacidad"
 
)

names(df_clean) <- make.names(names(df_clean))
names(df_clean)

quali_sup <- which(names(df_clean) %in% quali_sup_vars)



niveles_excluir <- valores_invalidos

# Construcción del ACM I

ACM_datos<- MCA(
  df_clean,
  quali.sup = quali_sup,
  ind.sup = ind_sup,
  graph = FALSE
)



summary(ACM_datos)



# Valores propios

vp <- get_eigenvalue(ACM_datos)

vp




var_acm <- get_mca_var(ACM_datos)

# Contribuciones de las categorias a las dimensiones

var_acm$contrib

# Ver las 10 categorías que más contribuyen a la primeras 5 dimensiones
res_var <- get_mca_var(ACM_datos)
top10_contrib <- head(res_var$contrib[order(-res_var$contrib[,1]), ], 10)
print(top10_contrib)


# Ver las 5 categorías que más contribuyen a las primeras 5 dimensiones

lapply(1:ncol(var_acm$contrib), function(i){
  
  x <- var_acm$contrib[, i]
  
  data.frame(
    contribucion = sort(x, decreasing = TRUE)[1:5],
    dimension = colnames(var_acm$contrib)[i]
  )
})




# Que tan bien se representa una categoria en una dimensión

var_acm$cos2


lapply(1:ncol(var_acm$cos2), function(i){
  
  x <- var_acm$cos2[, i]
  
  data.frame(
    cos2 = sort(x, decreasing = TRUE)[1:5],
    dimension = colnames(var_acm$cos2)[i]
  )
})


#Varianza explicada


fviz_screeplot(
  ACM_datos,
  title = "Varianza explicada por dimensión",
  xlab = "Dimensiones",
  ylab = "Porcentaje de varianza (%)"
)

# grafica de barras con las categorias que mejor se representan en la dimensión 1

fviz_cos2(ACM_datos, choice = "var", axes = 1, top = 20) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#Representación de los individuos y las 12 categorias que mas contribuyen en las
#construciones de los ejes
# En el codigo se pueden cambiar los ejes para visualizarlos tambien.

p <-fviz_mca_biplot(
  ACM_datos,
  # 1. Selección de Dimensiones
  axes = c(2,3),                 
  
  # 2. Selección y etiquetas
  select.var = list(contrib = 12), 
  label = "var",                  
  repel = TRUE,                   
  max.overlaps = Inf,
  
  # 3. Estética de Individuos (Activos y Suplementarios)
  geom.ind = "point",             
  col.ind = "grey80",             
  alpha.ind = 0.3,                
  
  col.ind.sup = "grey85",         
  alpha.ind.sup = 0.2,            
  
  # 4. Estética de Variables
  col.var = "firebrick2",         
  pointsize = 3,                  
  
  # 5. Formato y Títulos
  ggtheme = theme_minimal() + 
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 14),
      axis.title = element_text(face = "italic")
    )
) +
  labs(
    title = "Biplot del ACM: Dimensiones 2 y 3",
    subtitle = "Nube gris = Reportes, Categorias principales de color rojo",
    x = paste0("Dimensión 2 (", round(ACM_datos$eig[2,2], 1), "%)"),
    y = paste0("Dimensión 3 (", round(ACM_datos$eig[3,2], 1), "%)")
  )


p


# Análisis de la proyección de los individuos en las dos primeras dimensiones factoriales
# según el sexo de la víctima


# 1. Extraer individuos ACTIVOS
ind_act <- as.data.frame(ACM_datos$ind$coord[, 1:2])
nombres_act <- rownames(ind_act)


ind_act$Sexo <- as.factor(df_clean[nombres_act, "Sexo.de.la.victima", drop = TRUE])

# 2. Extraer individuos SUPLEMENTARIOS
ind_sup <- as.data.frame(ACM_datos$ind.sup$coord[, 1:2])
nombres_sup <- rownames(ind_sup)


ind_sup$Sexo <- as.factor(df_clean[nombres_sup, "Sexo.de.la.victima", drop = TRUE])

# 3. Unir ambos grupos
todos_ind <- rbind(ind_act, ind_sup)
colnames(todos_ind) <- c("Dim1", "Dim2", "Sexo")

# 4. Preparar las 12 categorías que más contribuyen
res_var <- get_mca_var(ACM_datos)
df_vars <- data.frame(
  Dim1 = res_var$coord[,1],
  Dim2 = res_var$coord[,2],
  Categoria = rownames(res_var$coord),
  Contrib = res_var$contrib[,1]
)
df_top12 <- df_vars[order(-df_vars$Contrib), ][1:12, ]

# 5. GRAFICAR
ggplot() +
 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  
  # Nube de puntos de TODOS los individuos
  geom_point(data = todos_ind, 
             aes(x = Dim1, y = Dim2, color = Sexo), 
             alpha = 0.55, size = 0.5) +
  stat_ellipse(data = todos_ind, 
               aes(x = Dim1, y = Dim2, color = Sexo, group = Sexo), 
               level = 0.95, linewidth = 1) +
  geom_point(data = df_top12, aes(x = Dim1, y = Dim2), 
             color = "black", shape = 17, size = 3) +
  geom_text_repel(data = df_top12, aes(x = Dim1, y = Dim2, label = Categoria),
                  color = "black", fontface = "bold", size = 3.5, max.overlaps = 20) +
  
 
  scale_color_manual(values = c("Hombre" = "#3498DB", "Mujer" = "#E74C3C")) + 
  theme_minimal() +
  labs(title = "Biplot ACM: Perfilamiento por sexo",
       x = "Dimensión 1", y = "Dimensión 2",
       color = "Sexo de la víctima") +
  
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14, face = "bold"), # Tamaño del título "Sexo de la víctima"
    legend.text = element_text(size = 12),               # Tamaño de "Hombre" / "Mujer"
    legend.key.size = unit(1.5, "cm")                    # Aumenta el espacio del icono
  ) +
  

  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))


# Análisis de la proyección de los individuos en las dos primeras dimensiones factoriales
# según el Contexto del Hecho

 #Extraer coordenadas de individuos ACTIVOS (Dims 1 y 2)
ind_act <- as.data.frame(ACM_datos$ind$coord[, 1:2])
colnames(ind_act) <- c("Dim1", "Dim2")

# Extraer individuos SUPLEMENTARIOS (Dims 1 y 2)
ind_sup <- as.data.frame(ACM_datos$ind.sup$coord[, 1:2])
colnames(ind_sup) <- c("Dim1", "Dim2")


ind_act$Contexto <- as.factor(df_clean[rownames(ind_act), "Contexto.del.Hecho", drop = TRUE])
ind_sup$Contexto <- as.factor(df_clean[rownames(ind_sup), "Contexto.del.Hecho", drop = TRUE])


todos_ind <- rbind(ind_act, ind_sup)


res_var <- get_mca_var(ACM_datos)
df_vars <- data.frame(
  Dim1 = res_var$coord[, 1],
  Dim2 = res_var$coord[, 2],
  Categoria = rownames(res_var$coord),
  Contrib = res_var$contrib[, 1] + res_var$contrib[, 2]
)
df_top12 <- df_vars[order(-df_vars$Contrib), ][1:12, ]

#  GRAFICAR
ggplot() +
 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  # Nube de puntos de individuos coloreados por Contexto
  geom_point(data = todos_ind, 
             aes(x = Dim1, y = Dim2, color = Contexto), 
             alpha = 0.15, size = 0.5) +
  
 
  stat_ellipse(data = todos_ind, 
               aes(x = Dim1, y = Dim2, color = Contexto, group = Contexto), 
               level = 0.95, linewidth = 0.8) +
  
  geom_point(data = df_top12, aes(x = Dim1, y = Dim2), 
             color = "black", shape = 18, size = 3) +
  
  geom_text_repel(data = df_top12, aes(x = Dim1, y = Dim2, label = Categoria),
                  color = "black", fontface = "bold", size = 3.2, max.overlaps = 20) +
  
  
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(title = "Biplot ACM: Perfilamiento por contexto del hecho",
  
       x = paste0("Dimensión 1 (", round(ACM_datos$eig[1,2], 1), "%)"),
       y = paste0("Dimensión 2 (", round(ACM_datos$eig[2,2], 1), "%)"),
       color = "Contexto del hecho") +

  theme(
    legend.position = "right",            
    legend.direction = "vertical",        
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9), 
    legend.key.height = unit(0.8, "cm")   
  ) +
  
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))



# Análisis de la proyección de los individuos en las dos primeras dimensiones factoriales
# según el ciclo vital

#  Extraer coordenadas de individuos ACTIVOS
ind_act <- as.data.frame(ACM_datos$ind$coord[, 1:2])
colnames(ind_act) <- c("Dim1", "Dim2")

#  Extraer coordenadas de individuos SUPLEMENTARIOS
ind_sup <- as.data.frame(ACM_datos$ind.sup$coord[, 1:2])
colnames(ind_sup) <- c("Dim1", "Dim2")


ind_act$CicloVital <- as.factor(df_clean[rownames(ind_act), "Ciclo.Vital", drop = TRUE])
ind_sup$CicloVital <- as.factor(df_clean[rownames(ind_sup), "Ciclo.Vital", drop = TRUE])

#  Unir ambos grupos
todos_ind <- rbind(ind_act, ind_sup)


res_var <- get_mca_var(ACM_datos)
df_vars <- data.frame(
  Dim1 = res_var$coord[, 1],
  Dim2 = res_var$coord[, 2],
  Categoria = rownames(res_var$coord),
  Contrib = res_var$contrib[, 1] + res_var$contrib[, 2]
)
df_top12 <- df_vars[order(-df_vars$Contrib), ][1:12, ]

# GRAFICAR
ggplot() +
 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
  
  
  # Nube de puntos de individuos coloreados por Ciclo Vital
  geom_point(data = todos_ind, 
             aes(x = Dim1, y = Dim2, color = CicloVital), 
             alpha = 0.1, size = 0.4) + # Bajamos alpha a 0.1 por la densidad
  
  
  stat_ellipse(data = todos_ind, 
               aes(x = Dim1, y = Dim2, color = CicloVital, group = CicloVital), 
               level = 0.95, linewidth = 0.8) +
  
  
  geom_point(data = df_top12, aes(x = Dim1, y = Dim2), 
             color = "black", shape = 18, size = 3) +
  
  geom_text_repel(data = df_top12, aes(x = Dim1, y = Dim2, label = Categoria),
                  color = "black", fontface = "bold", size = 3.2, max.overlaps = 25) +
  
  
  scale_color_brewer(palette = "Set2") + 
  theme_minimal() +
  labs(title = "Biplot ACM: Perfilamiento por ciclo vital",
   
       x = paste0("Dimensión 1 (", round(ACM_datos$eig[1,2], 1), "%)"),
       y = paste0("Dimensión 2 (", round(ACM_datos$eig[2,2], 1), "%)"),
       color = "Ciclo vital") +
  theme(
    legend.position = "right",           
    legend.direction = "vertical",        
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9), 
    legend.key.height = unit(0.8, "cm")  
  ) +
  
 
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))



# Si se quiere descargar los valores propios y demas información del ACM I
#Ejecutar las siguientes instrucciones


df_eig <- as.data.frame(get_eigenvalue(ACM_datos))
df_eig$Dimension <- rownames(df_eig)
df_eig <- df_eig[order(-df_eig$variance.percent), ] 
df_eig <- df_eig[, c("Dimension", "eigenvalue", "variance.percent", "cumulative.variance.percent")]

write_xlsx(df_eig, "1_Autovalores_Ordenado.xlsx")


df_eta2 <- as.data.frame(ACM_datos$var$eta2)
df_eta2$Variable <- rownames(df_eta2)
df_eta2 <- df_eta2[order(-df_eta2[, 1]), ] 
df_eta2 <- df_eta2[, c(ncol(df_eta2), 1:(ncol(df_eta2)-1))]

write_xlsx(df_eta2, "2_Variables_Eta2_Ordenado.xlsx")





res_var <- get_mca_var(ACM_datos)


tabla_maestra <- data.frame(
  Categoria = rownames(res_var$contrib),
  Contrib_D1 = res_var$contrib[,1],
  Cos2_D1    = res_var$cos2[,1],
  Contrib_D2 = res_var$contrib[,2],
  Cos2_D2    = res_var$cos2[,2],
  Contrib_D3 = res_var$contrib[,3],
  Cos2_D3    = res_var$cos2[,3],
  Contrib_D4 = res_var$contrib[,4],
  Cos2_D4    = res_var$cos2[,4]
)

tabla_maestra_ordenada <- tabla_maestra[order(-tabla_maestra$Contrib_D1), ]

write_xlsx(tabla_maestra_ordenada, "3_Tabla_Maestra_Categorias_Ordenada.xlsx")





#ACM II
 
#Se mantienen los mismos pasos que se realizaron con el ACM I
# Con la diferencia que la variable Contexto del Hecho entra como suplementaria
 
 cat_vars <- c( "Sexo de la victima", 
               
                "Ciclo Vital",
                "Escolaridad",
                "Estado Civil",
                "Pertenencia Grupal",
                "Zona del Hecho", 
                "Escenario del Hecho",
                "Actividad Durante el Hecho",
                "Circunstancia del Hecho Detallada", 
                "Contexto del Hecho", 
                "Sexo del Agresor",
                "Presunto Agresor Detallado",
                "Mecanismo Causal de la Lesión no Fatal",
                "Diagnostico Topográfico de la Lesión no Fatal",
                "Factor Desencadenante de la Agresión",
                "Días de Incapacidad Medicolegal",
                "Tipo de Discapacidad",
                "Orientación Sexual"
                
                
 )
 
 
 
 df_clean <- datos_1[, cat_vars]
 
 names(df_clean) <- gsub("Circunstancia.del.Hecho.Detallada",
                         "Circunstancia.del.Hecho",
                         names(df_clean))
 
 
 
 
 df_clean$Contexto.del.Hecho <- recode(df_clean$"Contexto del Hecho",
                                       "6 Lesiones no Fatales por Violencia de Pareja" = "Violencia de pareja",
                                       "4 Lesiones no Fatales por Violencia entre otros Familiares" = "Violencia entre familiares",
                                       "3 Lesiones no Fatales contra Niños, Niñas y Adolescentes por Violencia Intrafamiliar" = "Violencia  contra NNA",
                                       "5 Lesiones no Fatales contra el Adulto Mayor por Violencia Intrafamiliar" = "Violencia contra el adulto mayor"
 )
 df_clean <- df_clean[, -10]
 
 valores_invalidos <- c(
   "Sin información", "sin información", "SIN INFORMACIÓN",
   "Sin informacion", "sin informacion",
   "Por determinar", "por determinar",
   "Otros", "otros",
   "Sin información/Otros", "sin informacion/otros"
 )
 
 invalid_matrix <- as.data.frame(
   lapply(df_clean, function(x) x %in% valores_invalidos)
 )
 invalid_ratio <- rowMeans(invalid_matrix, na.rm = TRUE)
 
 ind_sup <- which(invalid_ratio > 0.15)
 length(ind_sup)
 df_clean[] <- lapply(df_clean, as.factor)
 
 quali_sup_vars <- c(
   "Orientación.Sexual",
   "Pertenencia.Grupal",
   "Zona.del.Hecho", 
   "Escenario.del.Hecho",
   "Actividad.Durante.el.Hecho",
   "Mecanismo.Causal.de.la.Lesión.no.Fatal",
   "Diagnostico.Topográfico.de.la.Lesión.no.Fatal",
   "Días.de.Incapacidad Medicolegal",
   "Tipo.de.Discapacidad",
   "Contexto.del.Hecho"
   
 )
 
 names(df_clean) <- make.names(names(df_clean))
 names(df_clean)
 
 quali_sup <- which(names(df_clean) %in% quali_sup_vars)
 
 
 
 niveles_excluir <- valores_invalidos
 
 ACM_datos<- MCA(
   df_clean,
   quali.sup = quali_sup,
   ind.sup = ind_sup,
  
   graph = FALSE
 )
 
 
 
 summary(ACM_datos)
 
 
 

 
 vp <- get_eigenvalue(ACM_datos)
 
 vp
 
 
 
 
 var_acm <- get_mca_var(ACM_datos)
 
 # CONTRIBUCIONES DE CATEGORIAS A LOS EJES
 
 
 var_acm$contrib
 
 
 lapply(1:ncol(var_acm$contrib), function(i){
   
   x <- var_acm$contrib[, i]
   
   data.frame(
     contribucion = sort(x, decreasing = TRUE)[1:10],
     dimension = colnames(var_acm$contrib)[i]
   )
 })
 
 
 
 
 # QUE TANTO SE REPRESENTA UNA CATEGORIA EN EL EJE
 
 var_acm$cos2
 
 
 lapply(1:ncol(var_acm$cos2), function(i){
   
   x <- var_acm$cos2[, i]
   
   data.frame(
     cos2 = sort(x, decreasing = TRUE)[1:10],
     dimension = colnames(var_acm$cos2)[i]
   )
 })
 
 #Varianza explicada
 
 
 fviz_screeplot(
   ACM_datos,
   title = "Varianza explicada por dimensión",
   xlab = "Dimensiones",
   ylab = "Porcentaje de varianza (%)"
 )
 
 
 #Representación de categoria en la dimensión 1
 
 
 fviz_cos2(ACM_datos, choice = "var", axes = 1, top = 20) +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
 
 
 
 
 fviz_mca_biplot(
   ACM_datos,
   # 1. Selección de Dimensiones
   axes = c(1,2),               
   
   # 2. Selección y etiquetas
   select.var = list(contrib = 16), 
   label = "var",                  
   repel = TRUE,                   
   max.overlaps = Inf,
   
   # 3. Estética de Individuos (Activos y Suplementarios)
   geom.ind = "point",             
   col.ind = "grey80",             
   alpha.ind = 0.3,                
   
   col.ind.sup = "grey85",         
   alpha.ind.sup = 0.2,            
   
   # 4. Estética de Variables
   col.var = "firebrick2",         
   pointsize = 3,                  
   
   # 5. Formato y Títulos
   ggtheme = theme_minimal() + 
     theme(
       panel.grid.minor = element_blank(),
       plot.title = element_text(face = "bold", size = 14),
       axis.title = element_text(face = "italic")
     )
 ) +
   labs(
     title = "Biplot del ACM: Dimensiones 1 y 2",
     subtitle = "Nube gris = reportes, categorias principales de color rojo",
     x = paste0("Dimensión 1 (", round(ACM_datos$eig[1,2], 1), "%)"),
     y = paste0("Dimensión 2 (", round(ACM_datos$eig[2,2], 1), "%)")
   )
 
 
 # Visualizacion atraves de categorias especificas
 
 # 1. Extraer individuos ACTIVOS
 ind_act <- as.data.frame(ACM_datos$ind$coord[, 2:3])
 nombres_act <- rownames(ind_act)
 

 ind_act$Sexo <- as.factor(df_clean[nombres_act, "Sexo.de.la.victima", drop = TRUE])
 
 # 2. Extraer individuos SUPLEMENTARIOS
 ind_sup <- as.data.frame(ACM_datos$ind.sup$coord[, 2:3])
 nombres_sup <- rownames(ind_sup)
 

 ind_sup$Sexo <- as.factor(df_clean[nombres_sup, "Sexo.de.la.victima", drop = TRUE])
 
 # 3. Unir ambos grupos
 todos_ind <- rbind(ind_act, ind_sup)
 colnames(todos_ind) <- c("Dim2", "Dim3", "Sexo")
 
 # 4. Preparar las 12 categorías que más contribuyen 
 res_var <- get_mca_var(ACM_datos)
 df_vars <- data.frame(
   Dim2 = res_var$coord[,2],
   Dim3 = res_var$coord[,3],
   Categoria = rownames(res_var$coord),
   Contrib = res_var$contrib[, 2] + res_var$contrib[, 3]
 )
 df_top12 <- df_vars[order(-df_vars$Contrib), ][1:12, ]
 
 # 5. GRAFICAR
 ggplot() +
 
   geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
   geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
   
   # Nube de puntos de TODOS los individuos
   geom_point(data = todos_ind, 
              aes(x = Dim2, y = Dim3, color = Sexo), 
              alpha = 0.55, size = 0.5) +
   stat_ellipse(data = todos_ind, 
                aes(x = Dim2, y = Dim3, color = Sexo, group = Sexo), 
                level = 0.95, linewidth = 1) +
   geom_point(data = df_top12, aes(x = Dim2, y = Dim3), 
              color = "black", shape = 17, size = 3) +
   geom_text_repel(data = df_top12, aes(x = Dim2, y = Dim3, label = Categoria),
                   color = "black", fontface = "bold", size = 3.5, max.overlaps = 20) +
   

   scale_color_manual(values = c("Hombre" = "#3498DB", "Mujer" = "#E74C3C")) + 
   theme_minimal() +
   labs(title = "Biplot ACM: Perfilamiento por sexo",
        x = "Dimensión 2", y = "Dimensión 3",
        color = "Sexo de la víctima") +
   

   theme(
     legend.position = "bottom",
     legend.title = element_text(size = 14, face = "bold"), # Tamaño del título "Sexo de la víctima"
     legend.text = element_text(size = 12),               # Tamaño de "Hombre" / "Mujer"
     legend.key.size = unit(1.5, "cm")                    # Aumenta el espacio del icono
   ) +
   

   guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))
 
 
 
 
 #  Extraer coordenadas de individuos ACTIVOS (Dims 1 y 2)
 ind_act <- as.data.frame(ACM_datos$ind$coord[, 2:3])
 colnames(ind_act) <- c("Dim2", "Dim3")
 
 #  Extraer individuos SUPLEMENTARIOS (Dims 1 y 2)
 ind_sup <- as.data.frame(ACM_datos$ind.sup$coord[, 2:3])
 colnames(ind_sup) <- c("Dim2", "Dim3")
 

 ind_act$Contexto <- as.factor(df_clean[rownames(ind_act), "Contexto.del.Hecho", drop = TRUE])
 ind_sup$Contexto <- as.factor(df_clean[rownames(ind_sup), "Contexto.del.Hecho", drop = TRUE])
 
 #  Unir ambos grupos
 todos_ind <- rbind(ind_act, ind_sup)
 
 #  Preparar las 12 categorías que más contribuyen al plano 1-2
 res_var <- get_mca_var(ACM_datos)
 df_vars <- data.frame(
   Dim2 = res_var$coord[, 2],
   Dim3 = res_var$coord[, 3],
   Categoria = rownames(res_var$coord),
   Contrib = res_var$contrib[, 2] + res_var$contrib[, 3]
 )
 df_top12 <- df_vars[order(-df_vars$Contrib), ][1:12, ]
 
 #  GRAFICAR
 ggplot() +
 
   geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
   geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
   # Nube de puntos de individuos coloreados por Contexto
   geom_point(data = todos_ind, 
              aes(x = Dim2, y = Dim3, color = Contexto), 
              alpha = 0.15, size = 0.5) +
   
   
   stat_ellipse(data = todos_ind, 
                aes(x = Dim2, y = Dim3, color = Contexto, group = Contexto), 
                level = 0.95, linewidth = 0.8) +
   
  
   geom_point(data = df_top12, aes(x = Dim2, y = Dim3), 
              color = "black", shape = 18, size = 3) +
   
   geom_text_repel(data = df_top12, aes(x = Dim2, y = Dim3, label = Categoria),
                   color = "black", fontface = "bold", size = 3.2, max.overlaps = 20) +
   
  
   scale_color_brewer(palette = "Set1") +
   theme_minimal() +
   labs(title = "Biplot ACM: Perfilamiento por contexto del hecho",
        
        x = paste0("Dimensión 2 (", round(ACM_datos$eig[2,2], 1), "%)"),
        y = paste0("Dimensión 3 (", round(ACM_datos$eig[3,2], 1), "%)"),
        color = "Contexto del hecho") +
  
   theme(
     legend.position = "right",            
     legend.direction = "vertical",      
     legend.title = element_text(size = 10, face = "bold"),
     legend.text = element_text(size = 9),
     legend.key.height = unit(0.8, "cm")  
   ) +
   
  
   guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))
 
 
 
 
 # Extraer coordenadas de individuos ACTIVOS
 ind_act <- as.data.frame(ACM_datos$ind$coord[, 2:3])
 colnames(ind_act) <- c("Dim2", "Dim3")
 
 #  Extraer coordenadas de individuos SUPLEMENTARIOS
 ind_sup <- as.data.frame(ACM_datos$ind.sup$coord[, 2:3])
 colnames(ind_sup) <- c("Dim2", "Dim3")
 

 ind_act$CicloVital <- as.factor(df_clean[rownames(ind_act), "Ciclo.Vital", drop = TRUE])
 ind_sup$CicloVital <- as.factor(df_clean[rownames(ind_sup), "Ciclo.Vital", drop = TRUE])
 
 #  Unir ambos grupos
 todos_ind <- rbind(ind_act, ind_sup)
 
 #  Preparar las 12 categorías que más contribuyen (Variables)
 res_var <- get_mca_var(ACM_datos)
 df_vars <- data.frame(
   Dim2 = res_var$coord[, 2],
   Dim3 = res_var$coord[, 3],
   Categoria = rownames(res_var$coord),
   Contrib = res_var$contrib[, 2] + res_var$contrib[, 3]
 )
 df_top12 <- df_vars[order(-df_vars$Contrib), ][1:12, ]
 
 #  GRAFICAR
 ggplot() +
  
   geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
   geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5) +
   
   
   # Nube de puntos de individuos coloreados por Ciclo Vital
   geom_point(data = todos_ind, 
              aes(x = Dim2, y = Dim3, color = CicloVital), 
              alpha = 0.1, size = 0.4) + 
   
  
   stat_ellipse(data = todos_ind, 
                aes(x = Dim2, y = Dim3, color = CicloVital, group = CicloVital), 
                level = 0.95, linewidth = 0.8) +
   
 
   geom_point(data = df_top12, aes(x = Dim2, y = Dim3), 
              color = "black", shape = 18, size = 3) +
   
   geom_text_repel(data = df_top12, aes(x = Dim2, y = Dim3, label = Categoria),
                   color = "black", fontface = "bold", size = 3.2, max.overlaps = 25) +
   
   
   scale_color_brewer(palette = "Set2") + 
   theme_minimal() +
   labs(title = "Biplot ACM: Perfilamiento por ciclo vital",
        
        x = paste0("Dimensión 2 (", round(ACM_datos$eig[2,2], 1), "%)"),
        y = paste0("Dimensión 3 (", round(ACM_datos$eig[3,2], 1), "%)"),
        color = "Ciclo vital") +
   theme(
     legend.position = "right",           
     legend.direction = "vertical",       
     legend.title = element_text(size = 10, face = "bold"),
     legend.text = element_text(size = 9),
     legend.key.height = unit(0.8, "cm")  
   ) +
   
 
   guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))
 
 
 

 
 #  Extraer coordenadas de individuos ACTIVOS
 ind_act <- as.data.frame(ACM_datos$ind$coord[, 2:3])
 colnames(ind_act) <- c("Dim2", "Dim3")
 
 #  Extraer coordenadas de individuos SUPLEMENTARIOS
 ind_sup <- as.data.frame(ACM_datos$ind.sup$coord[, 2:3])
 colnames(ind_sup) <- c("Dim2", "Dim3")
 

 ind_act$Circunstancia <- as.factor(df_clean[rownames(ind_act), "Circunstancia.del.Hecho", drop = TRUE])
 ind_sup$Circunstancia <- as.factor(df_clean[rownames(ind_sup), "Circunstancia.del.Hecho", drop = TRUE])
 
 #  Unir ambos grupos
 todos_ind <- rbind(ind_act, ind_sup)
 
 #  Preparar las 12 categorías de variables que más contribuyen
 res_var <- get_mca_var(ACM_datos)
 df_vars <- data.frame(
   Dim2 = res_var$coord[, 2],
   Dim3 = res_var$coord[, 3],
   Categoria = rownames(res_var$coord),
   Contrib = res_var$contrib[, 2] + res_var$contrib[, 3]
 )
 df_top12 <- df_vars[order(-df_vars$Contrib), ][1:12, ]
 
 # 6. GRAFICAR
 ggplot() +
   # Líneas de referencia de los ejes
   geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
   geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
   
   # Nube de puntos coloreada por Circunstancia
   geom_point(data = todos_ind, 
              aes(x = Dim2, y = Dim3, color = Circunstancia), 
              alpha = 0.12, size = 0.4) + 
   
   
   stat_ellipse(data = todos_ind, 
                aes(x = Dim2, y = Dim3, color = Circunstancia, group = Circunstancia), 
                level = 0.95, linewidth = 0.7) +
   

   geom_point(data = df_top12, aes(x = Dim2, y = Dim3), 
              color = "black", shape = 17, size = 3) +
   
   geom_text_repel(data = df_top12, aes(x = Dim2, y = Dim3, label = Categoria),
                   color = "black", fontface = "bold", size = 3, max.overlaps = 35) +
   

   scale_color_brewer(palette = "Paired") + 
   theme_minimal() +
   labs(title = "Biplot ACM: Circunstancias del Hecho",
      
        x = paste0("Dimensión 2 (", round(ACM_datos$eig[2,2], 1), "%)"),
        y = paste0("Dimensión 3 (", round(ACM_datos$eig[3,2], 1), "%)"),
        color = "Circunstancia") +
   
   theme(
     legend.position = "right",
     legend.direction = "vertical",
     legend.title = element_text(size = 9, face = "bold"),
     legend.text = element_text(size = 7.5), 
     legend.key.height = unit(0.5, "cm"),
     panel.grid.minor = element_blank(),
     axis.title = element_text(size = 10)
   ) +
   

   guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))
 
 
 
 
 
 
 df_eig <- as.data.frame(get_eigenvalue(ACM_datos))
 df_eig$Dimension <- rownames(df_eig)
 df_eig <- df_eig[order(-df_eig$variance.percent), ] 
 df_eig <- df_eig[, c("Dimension", "eigenvalue", "variance.percent", "cumulative.variance.percent")]
 
 write_xlsx(df_eig, "sin_1_Autovalores_Ordenado.xlsx")
 
 
 df_eta2 <- as.data.frame(ACM_datos$var$eta2)
 df_eta2$Variable <- rownames(df_eta2)
 
 df_eta2 <- df_eta2[order(-df_eta2[, 1]), ] 
 df_eta2 <- df_eta2[, c(ncol(df_eta2), 1:(ncol(df_eta2)-1))]
 
 write_xlsx(df_eta2, "sin_2_Variables_Eta2_Ordenado.xlsx")
 
 
 
 
 
 res_var <- get_mca_var(ACM_datos)
 
 
 tabla_maestra <- data.frame(
   Categoria = rownames(res_var$contrib),
   Contrib_D1 = res_var$contrib[,1],
   Cos2_D1    = res_var$cos2[,1],
   Contrib_D2 = res_var$contrib[,2],
   Cos2_D2    = res_var$cos2[,2],
   Contrib_D3 = res_var$contrib[,3],
   Cos2_D3    = res_var$cos2[,3],
   Contrib_D4 = res_var$contrib[,4],
   Cos2_D4    = res_var$cos2[,4]
 )
 

 tabla_maestra_ordenada <- tabla_maestra[order(-tabla_maestra$Contrib_D1), ]
 
 write_xlsx(tabla_maestra_ordenada, "sin_3_Tabla_Maestra_Categorias_Ordenada.xlsx")
 
 
 
 
 
 # Algoritmo Fuzzy C-Means
 
 
 
 coords <- ACM_datos$ind$coord
 
 

 
  datos_fuzzy <- ACM_datos$ind$coord[, 1:4] 
 
  set.seed(12345)
  # La determinación del número de clústeres se realiza mediante métodos tradicionales como el criterio
  # del codo (WSS) y el índice de silueta, utilizando k-means como aproximación inicial.
  # Posteriormente, se evalúa la calidad de la partición en el contexto de clustering difuso mediante
  # métricas como el Partition Coefficient (PC) y la Partition Entropy (PE).
  # Debido al alto costo computacional, estos procedimientos se aplican sobre una muestra del conjunto de datos.
  datos_muestra <- datos_fuzzy[sample(seq_len(nrow(datos_fuzzy)), 20000), ]
  
  fviz_nbclust(
    datos_muestra[,1:4],
    kmeans,
    method = "wss",
    nstart = 50
  )
  
  fviz_nbclust(
    datos_muestra[,1:4],
    kmeans,
    method = "silhouette",
    nstart = 50
  )

  # Se evalúan distintas configuraciones del número de clústeres (k = 3, 4 y 5)
  # utilizando métricas  Partition Coefficient (PC)
  # y la Partition Entropy (PE), con el fin de analizar la calidad de la partición.

 evaluar_custom_k <- function(data, ks, m_val = 2) {
   resultados <- data.frame(k = integer(), PC = numeric(), PE = numeric())
   
   for (k in ks) {
     cat("Procesando k =", k, "...\n")
     
     
     res_fcm <- fcm(data, centers = k, m = m_val, nstart = 5)
     
     # Extraer matriz de membresía
     u <- res_fcm$u
     n <- nrow(u)
     
     # --- Cálculo Manual de Métricas ---
     
     # Partition Coefficient (PC) - Meta: Cercano a 1
     pc_val <- sum(u^2) / n
     
     # Partition Entropy (PE) - Meta: Cercano a 0
     u_log <- u * log(u)
     u_log[is.nan(u_log)] <- 0 
     pe_val <- -sum(u_log) / n
     
     # Guardar en el dataframe
     resultados <- rbind(resultados, data.frame(k = k, PC = pc_val, PE = pe_val))
   }
   
   cat("¡Finalizado!\n")
   return(resultados)
 }
 
 # --- EJEMPLO DE USO ---
 
 
 mis_k_interes <- c(3,4,5)
 
 
 metricas_finales <- evaluar_custom_k(datos_muestra, ks = mis_k_interes)
 
 
 print(metricas_finales)
 
 
 
 
 
 
 #  Extraer y unir coordenadas de todos (Activos + Suplementarios)
 coord_activos <- ACM_datos$ind$coord[, 1:4]
 coord_suplem  <- ACM_datos$ind.sup$coord[, 1:4]
 coord_totales <- rbind(coord_activos, coord_suplem)
 
 # 2. Ejecutar Fuzzy C-Means 
 # Usando  k=4
 set.seed(4444) 
 res_fuzzy <- fcm(coord_totales, centers = 4, m = 2)
 
 
 
 #  Extraer la pertenencia "Hard" (El grupo con mayor probabilidad)
 cluster_asignado <- apply(res_fuzzy$u, 1, which.max)
 

 df_clean$Cluster_Fuzzy <- NA
 df_clean[names(cluster_asignado), "Cluster_Fuzzy"] <- cluster_asignado
 df_clean <- df_clean %>%
   mutate(
     Cluster_Fuzzy2 = case_when(
       Cluster_Fuzzy == 1 ~ "Cluster 1: violencia de pareja: actual y expareja",
       Cluster_Fuzzy == 2 ~ "Cluster 2: violencia relacional asociada a sustancias",
       Cluster_Fuzzy == 3 ~ "Cluster 3: violencia contra NNA",
       Cluster_Fuzzy == 4 ~ "Cluster 4: violencia mixta o transicional",
       TRUE ~ NA_character_
     )
   )
 
 
 df_clean$Cluster_Fuzzy <- as.factor(df_clean$Cluster_Fuzzy)
 
 
 
 
 
 
 # Cantidad de reportes asignados con la etiqueta en cada cluster
 table(df_clean$Cluster_Fuzzy2, useNA = "always")

 #Verificación
 table(df_clean$Cluster_Fuzzy, useNA = "always")
 

 
 # Comparar porcentajes de Contexto por Cluster
 prop.table(table(df_clean$Cluster_Fuzzy, df_clean$Contexto.del.Hecho), 1) * 100
 
 # Comparar porcentajes de Sexo por Cluster
 prop.table(table(df_clean$Cluster_Fuzzy, df_clean$Sexo.de.la.victima), 1) * 100
 
 
 # Comparar porcentajes de ciclo vital por Cluster
 prop.table(table(df_clean$Cluster_Fuzzy, df_clean$Ciclo.Vital), 1) * 100
 # Comparar porcentajes de circunstancia del hecho por Cluster
 prop.table(table(df_clean$Cluster_Fuzzy, df_clean$Circunstancia.del.Hecho), 1) * 100
 # Comparar porcentajes de presunto agresor por Cluster
 prop.table(table(df_clean$Cluster_Fuzzy, df_clean$Presunto.Agresor.Detallado), 1) * 100
 

 
 
 # Se seleccionan las variables que quedaran para el perfilamiento final de los clusters
 columnas_perfil <- c("Sexo.de.la.victima", 
                    
                      "Ciclo.Vital",
                      "Escolaridad",
                      "Estado.Civil",
                      "Pertenencia.Grupal",
                      "Zona.del.Hecho", 
                      "Escenario.del.Hecho",
                      "Actividad.Durante.el.Hecho",
                      "Circunstancia.del.Hecho", 
                      "Contexto.del.Hecho", 
                      "Sexo.del.Agresor",
                      "Presunto.Agresor.Detallado",
                      "Mecanismo.Causal.de.la.Lesión.no.Fatal",
                      "Diagnostico.Topográfico.de.la.Lesión.no.Fatal",
                      "Factor.Desencadenante.de.la.Agresión",
                      "Días.de.Incapacidad.Medicolegal",
                      "Tipo.de.Discapacidad",
                      "Orientación.Sexual")
 
 
 
 # Se crea la función para extraer la moda (categoría más frecuente)
 get_mode <- function(x) {
   ux <- unique(na.omit(x))
   ux[which.max(tabulate(match(x, ux)))]
 }
 
 #  tabla resumen
 tabla_perfiles <- df_clean %>%
   group_by(Cluster_Fuzzy) %>%
   summarise(across(all_of(columnas_perfil), get_mode))
 

 print(tabla_perfiles)
 
 # Opcional: Exportar a Excel de los perfiles

  write_xlsx(tabla_perfiles, "-Perfil_Clusters_Fuzzy.xlsx")
 
  
 
  print(t(tabla_perfiles))
  
  
  # matriz de pertenencia (u)
  matriz_u <- as.data.frame(res_fuzzy$u)
  
  #  Se calcula la pertenencia máxima por individuo
  df_clean$pertenencia_max <- apply(matriz_u, 1, max)
  
  #estadísticas descriptivas de la certeza
  summary(df_clean$pertenencia_max)
  
  
  # Se crean las etiquetas de seguridad en la asignación
  df_clean$tipo_pertenencia <- cut(df_clean$pertenencia_max, 
                                   breaks = c(0, 0.4, 0.7, 1),
                                   labels = c("Muy Ambiguo [0 , 0.4]", "Intermedio [0.4 , 0.7]", "Perfil Claro [0.7 , 1]"))
  
  # Cantidad de reportes en cada intervalo
  tabla_ambiguedad <- table(df_clean$tipo_pertenencia)
  print(tabla_ambiguedad)

  
  # Porcentaje de solapamiento
  prop.table(tabla_ambiguedad) * 100
  

  
  
  # certeza promedio por cada cluster
  df_clean %>%
    group_by(Cluster_Fuzzy) %>%
    summarise(Certeza_Promedio = mean(pertenencia_max))
  
  
  # Creacción de matriz Gap
  p1 <- apply(matriz_u, 1, function(x) sort(x, decreasing = TRUE)[1])
  p2 <- apply(matriz_u, 1, function(x) sort(x, decreasing = TRUE)[2])
  
  
  df_clean$gap_pertenencia <- p1 - p2
  
  
  casos_puente_pro <- df_clean[df_clean$gap_pertenencia < 0.1, ]
  
  nrow(casos_puente_pro)
  
  
  # Calculo de entropia
  calcular_entropia <- function(probabilidades) {
    probabilidades <- probabilidades[probabilidades > 0] 
    -sum(probabilidades * log2(probabilidades))
  }
  
  df_clean$entropia <- apply(matriz_u, 1, calcular_entropia)
  
  # Normalizar la entropía (de 0 a 1, donde 1 es máxima confusión)
  # Para 4 clusters, el log2(4) es 2.
  df_clean$entropia_norm <- df_clean$entropia / log2(4)
  
  summary(df_clean$entropia_norm)
  
  
  
  # Identificar los dos clústeres principales para cada individuo
  df_clean$top_cluster1 <- apply(matriz_u, 1, function(x) names(matriz_u)[order(x, decreasing = TRUE)[1]])
  df_clean$top_cluster2 <- apply(matriz_u, 1, function(x) names(matriz_u)[order(x, decreasing = TRUE)[2]])
  
  # Crear una matriz de "Confusión Interna" para los casos ambiguos (Gap < 0.15)
  tabla_solapamiento <- df_clean %>%
    filter(gap_pertenencia < 0.15) %>%
    group_by(top_cluster1, top_cluster2) %>%
    tally() %>%
    spread(top_cluster2, n, fill = 0)
  
  print(tabla_solapamiento)

  
  # Grafico de trayectoria
  
 
  df_aluvial <- df_clean %>%
    group_by(Sexo.de.la.victima, Ciclo.Vital, Presunto.Agresor.Detallado, Cluster_Fuzzy) %>%
    tally() %>%
    ungroup()
  

  ggplot(data = df_aluvial,
         aes(axis1 = Sexo.de.la.victima, 
             axis2 = Ciclo.Vital, 
             axis3 = Presunto.Agresor.Detallado, 
             y = n)) +
    
    geom_alluvium(aes(,fill = as.factor(Cluster_Fuzzy)), width = 1/12, alpha = 0.7) +
    geom_stratum(width = 1/12, fill = "grey90", color = "grey40") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
    
    
    scale_fill_manual(
      values = c("#D55E00", "#0072B2", "#009E73", "#CC79A7"),
      labels = c(
        "C1: Violencia de Pareja (Actual y expareja)",
        "C2: Violencia relacional asociada a sustancias",
        "C3: Violencia en la Niñez",
        "C4: Violencia mixta o transicional"
      )
    ) +
    
    
    scale_x_discrete(
      limits = c("Sexo de la víctima", "Ciclo Vital", "Tipo de agresor"),
      expand = c(.05, .05)
    ) +
    
    theme_minimal() +
    labs(
      title = "Trayectorias de clasificación hacia los clústeres difusos",
      subtitle = "Relación entre características sociodemográficas, vínculo con el agresor y perfiles identificados",
      y = "Número de casos",
      fill = "Clúster"
    ) +
    
    theme(
      legend.position = "bottom",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  
 
  
  
  
  
  # Grafico de violin 
  ggplot(df_clean, aes(x = as.factor(Cluster_Fuzzy), y = entropia_norm, fill = as.factor(Cluster_Fuzzy))) +
    
   
    geom_violin(alpha = 0.3, color = NA) +
    
   
    geom_boxplot(width = 0.2, color = "grey20", outlier.alpha = 0.1) +
    
   
    scale_fill_manual(
      values = c("#D55E00", "#0072B2", "#009E73", "#CC79A7"),
      labels = c(
        "C1: Violencia de Pareja (Actual y Expareja)",
        "C2: Violencia relacional con consumo de sustancias",
        "C3: Violencia contra NNA",
        "C4: Violencia Mixta o transacional"
      )
    ) +
    
   
    scale_x_discrete(labels = c(
      "1" = "C1",
      "2" = "C2",
      "3" = "C3",
      "4" = "C4"
    )) +
  
    theme_minimal() +
    labs(
      title = "Distribución de la incertidumbre (entropía) por clúster",
      subtitle = "Valores cercanos a 1 indican mayor solapamiento entre perfiles",
      x = "Clúster identificado",
      y = "Entropía normalizada (0-1)",
      fill = "Clúster"
    ) +
    
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
  
  
  
  
  # Visualización de los clusters en el plano factorial del ACM II

  ind_act_3d <- as.data.frame(ACM_datos$ind$coord[, 1:3])
  ind_sup_3d <- as.data.frame(ACM_datos$ind.sup$coord[, 1:3])
  todos_3d <- rbind(ind_act_3d, ind_sup_3d)
  colnames(todos_3d) <- c("Dim1", "Dim2", "Dim3")
  
 
  cluster_datos <- df_clean[rownames(todos_3d), "Cluster_Fuzzy"][[1]]
  todos_3d$Cluster <- as.factor(cluster_datos)
  
  
  fig <- plot_ly(todos_3d, 
                 x = ~Dim1, y = ~Dim2, z = ~Dim3, 
                 color = ~Cluster, 
                 # Colores para tus 4 perfiles
                 colors = c("#D55E00", "#0072B2", "#009E73", "#CC79A7"),
                 type = 'scatter3d', 
                 mode = 'markers',
                 marker = list(size = 1.5, opacity = 0.4)) %>%
    layout(title = "Espacio Factorial 3D: Perfiles de Violencia",
           scene = list(xaxis = list(title = 'Dimensión 1'),
                        yaxis = list(title = 'Dimensión 2'),
                        zaxis = list(title = 'Dimensión 3')))
  
  fig
  
  
  

  
  
  var_coords <- as.data.frame(ACM_datos$var$coord[, 2:3])
  colnames(var_coords) <- c("Dim2", "Dim3")
  
 
  var_contrib <- rowSums(ACM_datos$var$contrib[, 2:3])
  top_12_cat <- names(sort(var_contrib, decreasing = TRUE)[1:12])
  var_coords_top <- var_coords[top_12_cat, ]
  var_coords_top$Categoria <- rownames(var_coords_top)
  

  ggplot() +
 
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
    
 
    geom_point(data = todos_3d, 
               aes(x = Dim2, y = Dim3, color = Cluster), 
               size = 1, alpha = 0.2) +
    

    geom_label_repel(data = var_coords_top,
                     aes(x = Dim2, y = Dim3, label = Categoria),
                     box.padding = 0.5, segment.color = 'black',
                     fontface = "bold", size = 3.5, fill = "white", alpha = 0.8) +
    

    scale_color_manual(
      values = c("1" = "#D55E00", "2" = "#0072B2", "3" = "#009E73", "4" = "#CC79A7"),
    
      labels = c(
        "1" = "C1: Pareja (Actual y expareja)",
        "2" = "C2: Violencia relacional con consumo de sustancias",
        "3" = "C3: Violencia contra NNA",
        "4" = "C4: Violencia mixta o transicional"
      )
    ) +
    
    theme_minimal() +
    labs(title = "Biplot Estático: Perfiles de violencia intrafamiliar (Dimensiones 2 y 3)",
         subtitle = "Superposición de las 12 categorías con mayor contribución",
         x = "Dimensión 2",
         y = "Dimensión 3",
         color = "Clúster") +
    
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 9)
    ) +
    

    guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))

 
  
  
  
  
  
  

  var_coords <- as.data.frame(ACM_datos$var$coord[, 1:2])
  colnames(var_coords) <- c("Dim1", "Dim2")
  

  var_contrib <- rowSums(ACM_datos$var$contrib[, 1:2])
  top_12_cat <- names(sort(var_contrib, decreasing = TRUE)[1:12])
  var_coords_top <- var_coords[top_12_cat, ]
  var_coords_top$Categoria <- rownames(var_coords_top)
  
 
  ggplot() +
   
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
    
 
    geom_point(data = todos_3d, 
               aes(x = Dim1, y = Dim2, color = Cluster), 
               size = 1, alpha = 0.2) +
    
   
    geom_label_repel(data = var_coords_top,
                     aes(x = Dim1, y = Dim2, label = Categoria),
                     box.padding = 0.5, segment.color = 'black',
                     fontface = "bold", size = 3.5, fill = "white", alpha = 0.8) +
    

    scale_color_manual(
      values = c("1" = "#D55E00", "2" = "#0072B2", "3" = "#009E73", "4" = "#CC79A7"),
      
      labels = c(
        "1" = "C1: Pareja (Actual y expareja)",
        "2" = "C2: Violencia relacional con consumo de sustancias",
        "3" = "C3: Violencia contra NNA",
        "4" = "C4: Violencia mixta o transicional"
      )
    ) +
    
    theme_minimal() +
    labs(title = "Biplot Estático: Perfiles de violencia intrafamiliar (Dimensiones 1 y 2)",
         subtitle = "Superposición de las 12 categorías con mayor contribución",
         x = "Dimensión 1",
         y = "Dimensión 2",
         color = "Clúster") +
    
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 9)
    ) +
    
   
    guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))

  
  
  
  
  