# Cargar librerías.
library(nnet)
library(car)
library(ggplot2)
library(MASS)
library(readxl)
#install.packages("dplyr")
library(dplyr)

# Cargar el archivo Excel.
df <- read_excel("C:/Users/tomas/Documents/Programación/Github/Patricionog/Tesio/Databases/1. Generales - Original.xlsx")

# Renombrar columnas.
df <- df %>% rename(autopercep_izq_der = `autopercep_izq-der`)  

# Convertir variables a factores.
df$cercania_Massa <- as.factor(df$cercania_Massa)
df$voto_2019 <- as.numeric(as.factor(df$voto_2019))

# Definir variables independientes y dependientes.
Variables_Independientes <- c("voto_2019", "autopercep_perantiper", "autopercep_conpro")
y <- df$cercania_Massa  # Variable dependiente
X <- df[ , Variables_Independientes]  # Variables independientes

# Modelo.
modelo_multinomial <- multinom(cercania_Massa ~ ., data = df[, c("cercania_Massa", Variables_Independientes)])
print('------------------------------------')
print('MODELO')
print('------------------------------------')
print(summary(modelo_multinomial))

# Supuestos.

# 1. Multicolinealidad.
vif_results <- vif(modelo_multinomial)
print('------------------------------------')
print('MULTICOLINEALIDAD')
print('------------------------------------')
print(vif_results)

# 2. Análisis de residuos.
residuals <- residuals(modelo_multinomial)
print('------------------------------------')
print('RESIDUOS')
print('------------------------------------')
plot(residuals)  # Gráfico de residuos

# ANOVA para comparar con un modelo nulo.
modelo_nulo <- multinom(cercania_Massa ~ 1, data = df)
anova_results <- anova(modelo_nulo, modelo_multinomial, test = "Chisq")
print('------------------------------------')
print('ANOVA')
print('------------------------------------')
print(anova_results)

# Interpretar resultados.
coef_summary <- summary(modelo_multinomial)
print('------------------------------------')
print('INTERPRETACIÓN DE RESULTADOS')
print('------------------------------------')
print(coef_summary$coefficients)

# Visualizar coeficientes.
coef_df <- as.data.frame(coef_summary$coefficients)
ggplot(coef_df, aes(x = rownames(coef_df), y = Estimate)) + 
  geom_point() + 
  coord_flip() + 
  labs(title = "Coeficientes del Modelo Multinomial", 
       x = "Variables", 
       y = "Estimación") +
  theme_minimal()

print('------------------------------------')
print('MATRIZ DE CORRELACIÓN')
print('------------------------------------')
correlation_matrix <- cor(df[, Variables_Independientes], use = "pairwise.complete.obs")
print(correlation_matrix)

# Definir coeficientes y errores estándar
coeficientes <- c(-0.1342467, -0.2722211, 0.17214117)
errores_estandar <- c(0.02888014, 0.02871499, 0.03097877)

# Calcular valores Z
valores_z <- coeficientes / errores_estandar

# Calcular valores p
valores_p <- 2 * (1 - pnorm(abs(valores_z)))

# Crear un data frame para ver los resultados
resultados <- data.frame(Variable = c("voto_2019", "autopercep_perantiper", "autopercep_conpro"),
                          Coeficiente = coeficientes,
                          Error_Estandar = errores_estandar,
                          Valor_Z = valores_z,
                          Valor_P = valores_p)

print('------------------------------------')
print('RESULTADOS')
print('------------------------------------')

print(resultados)