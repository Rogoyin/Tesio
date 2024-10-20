# Cargar librerías necesarias
library(MASS)
library(readxl)
library(dplyr)
library(car)  # Para ANOVA
library(ggplot2)  # Para visualización

# Cargar el archivo Excel
df <- read_excel("C:/Users/tomas/Documents/Programación/Github/Patricionog/Tesio/Databases/1. Generales - Original.xlsx")

# Renombrar columnas
df <- df %>% rename(autopercep_izq_der = `autopercep_izq-der`)

# Volver variable a ordinal
df$cercania_Massa <- factor(df$cercania_Massa, levels = 1:5, ordered = TRUE)

# Convertir variables categóricas a factores
categorical_vars <- c('genero', 'nacionalidad', 'provincia', 'niv_educativo', 
                      'voto_2019', 'voto_PASO_2023', 'candidato_PASO_2023')

df[categorical_vars] <- lapply(df[categorical_vars], as.factor)  # Convertir a factor

# Eliminar filas con NA
df <- na.omit(df)  

# Definir todas las variables independientes
Variables_Independientes <- c(
    #'genero',  # Descomentarlo si es necesario
    'edad',
    #'nacionalidad',  # Descomentarlo si es necesario
    #'provincia',  # Descomentarlo si es necesario
    #'e_social',  # Descomentarlo si es necesario
    #'niv_educativo',  # Descomentarlo si es necesario
    'voto_2019',
    'voto_PASO_2023',
    'candidato_PASO_2023',
    'autopercep_izq_der',
    'autopercep_conpro',
    'autopercep_perantiper',
    'indice_positividad',
    'massa_ip_izqder',
    'massa_ip_conpro',
    'bullrich_ip_izqder',
    'bullrich_ip_conpro',
    'schiaretti_ip_izqder',
    'schiaretti_ip_conpro',
    'milei_ip_izqder',
    'milei_ip_conpro',
    'bregman_ip_izqder',
    'bregman_ip_conpro',
    'indice_progresismo',
    'indice_conservadurismo'
)

# Comprobar los niveles de las variables categóricas
print(lapply(df[categorical_vars], levels))

# Convertir la lista de variables en un formato adecuado para la fórmula
formula_independientes <- paste(Variables_Independientes, collapse = " + ")

# Construir la fórmula final para el modelo
formula_final <- as.formula(paste("cercania_Massa ~", formula_independientes))

# Ajustar el modelo ordinal
modelo_ordinal <- polr(formula_final, data = df, Hess = TRUE)

# Resumen del modelo
print('------------------------------------')
print('MODELO')
print('------------------------------------')
summary(modelo_ordinal)

# Calculando errores estándar y ANOVA
print('------------------------------------')
print('ANOVA')
print('------------------------------------')
print(Anova(modelo_ordinal, type = "II"))

# Visualización del modelo
ggplot(data = df, aes(x = voto_2019, y = cercania_Massa)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Relación entre Voto 2019 y Cercanía a Massa", 
       x = "Voto 2019", 
       y = "Cercanía a Massa")
