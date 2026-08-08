# ==============================================================================
#                 GUÍA DE RESOLUCIÓN DE PROBLEMAS: UNIDAD 1
#           Distribuciones de Probabilidad Discretas (Trabajo Grupal)
# ==============================================================================
#
# GRUPO DE TRABAJO: Grupo #1

# ==============================================================================
# BLOQUE A: Distribución binomial ----
# Responsable: [Integrante 1]
# ==============================================================================

# Problema 1. Disponibilidad de un servidor ----
# Escriba su código aquí:


# Problema 2. Prueba psicométrica y afirmación del reclutador ----
# Escriba su código aquí:


# Problema 3. Planificación de certificados ----
# Escriba su código aquí:


# ==============================================================================
# BLOQUE B: Distribución de Poisson ----
# Responsable: [Integrante 2]
# ==============================================================================

# Problema 4. Diseño del tamaño de una inspección ----
p <- 0.05

# Inciso (a): Tamaño mínimo de muestra
# Se requiere que P(X >= 1) sea al menos 0.95.

n <- ceiling(log(0.05) / log(1 - p))
n


# Inciso (b): Verificación con n y n - 1

prob_n <- pbinom(0, size = n, prob = p, lower.tail = FALSE)
prob_n_anterior <- pbinom(0, size = n - 1, prob = p, lower.tail = FALSE) 
#Con n = 58 la probabilidad es de 94.9% menor a lo que indica de al menos el 95%

round(prob_n, 4)
round(prob_n_anterior, 4) 


# Inciso (c): Probabilidad de detectar al menos un defectuoso
# si la proporción real de defectuosos es del 2 %

p_real <- 0.02

prob_p_real <- pbinom(
  0,
  size = n,
  prob = p_real,
  lower.tail = FALSE
)

round(prob_p_real, 4)


# Problema 5. Siniestralidad en carretera urbana ----

lambda_semana <- 2.3

# Inciso (a): Probabilidad de exactamente cinco accidentes
# durante un período de dos semanas

lambda_dos_semanas <- lambda_semana * 2

prob_cinco_accidentes <- dpois(5, lambda = lambda_dos_semanas)

round(prob_cinco_accidentes, 4)


# Inciso (b): Probabilidad de al menos un accidente
# durante un día cualquiera

lambda_dia <- lambda_semana / 7

prob_al_menos_uno <- ppois(
  0,
  lambda = lambda_dia,
  lower.tail = FALSE
)

round(prob_al_menos_uno, 4)


# Inciso (c): Probabilidad de menos de ocho accidentes
# durante un período de 30 días

lambda_30_dias <- lambda_semana / 7 * 30

prob_menos_ocho <- ppois(7, lambda = lambda_30_dias)

round(prob_menos_ocho, 4)


# Problema 6. Dimensionamiento de un centro de llamadas ----

lambda <- 15
capacidad_por_agente <- 4

# Inciso (a): Probabilidad de saturación con tres agentes

agentes <- 3
capacidad_total <- agentes * capacidad_por_agente

prob_saturacion <- ppois(
  capacidad_total,
  lambda = lambda,
  lower.tail = FALSE
)

round(prob_saturacion, 4)


# Inciso (b): Número mínimo de agentes para que
# la probabilidad de saturación sea menor al 5 %

agentes <- 1:10
capacidad_total <- agentes * capacidad_por_agente

prob_saturacion <- ppois(
  capacidad_total,
  lambda = lambda,
  lower.tail = FALSE
)

resultados <- data.frame(
  Agentes = agentes,
  Capacidad = capacidad_total,
  Probabilidad = round(prob_saturacion, 4)
)

resultados

minimo_agentes <- agentes[which(prob_saturacion < 0.05)[1]]
minimo_agentes

# ==============================================================================
# BLOQUE C: Distribución geométrica ----
# Responsable: [Integrante 3]
# ==============================================================================


#Problema 7. Procesos de Poisson simultáneos e independientes ----
#Escriba su código aquí:
#Primer Inciso (a)
#Tasas independientes por minuto
lambda_compra <- 8
lambda_consulta <- 12

#Por propiedad aditiva: Y = X1 + X2 ~ Poisson(lambda_total)
lambda_total <- lambda_compra + lambda_consulta

#Probabilidad de recibir exactamente 25 solicitudes en total
prob_7a <- dpois(
  x = 25,
  lambda = lambda_total
)

#Resultado con cuatro decimales
round(prob_7a, 4)

#INCISO B
#Evento (i): Exactamente 10 de compra Y 15 de consulta
prob_evento_i <- dpois(x = 10, lambda = lambda_compra) * 
                  dpois(x = 15, lambda = lambda_consulta)

#Evento (ii): 25 en total sin distinción de tipo
prob_evento_ii <- dpois(x = 25, lambda = lambda_total)

#Diferencia entre probabilidades
diferencia_7b <- prob_evento_ii - prob_evento_i

#Resultados con cuatro decimales
round(prob_evento_i, 4)
round(prob_evento_ii, 4)
round(diferencia_7b, 4)

#Tabla comparativa
tabla_7b <- data.frame(
  Evento = c(
    "10 compra y 15 consulta",
    "25 en total"
  ),
  Probabilidad = round(
    c(
      prob_evento_i,
      prob_evento_ii
    ),
    4
  )
)

print(tabla_7b)

#Problema 8. Cajero con demanda variable ----
#Escriba su código aquí:
#INCISO A

#Tasa por hora en alta demanda (11:00 a 13:00)
lambda_hora_alta <- 8
horas_alta <- 2

#Lambda reescalado para 2 horas completos
lambda_2h_alta <- lambda_hora_alta * horas_alta

#Probabilidad de que lleguen exactamente 20 clientes
prob_8a <- dpois(
  x = 20,
  lambda = lambda_2h_alta
)

#Resultado con cuatro decimales
round(prob_8a, 4)

#INCISO B Eventos independientes en subintervalos
#Tasa para los primeros 30 min (0.5 horas) de alta demanda
lambda_30min_alta <- 8 * 0.5

#Tasa para la primera hora de demanda regular (13:00 a 16:00)
lambda_1h_regular <- 3 * 1

#Probabilidad de al menos 5 clientes en alta demanda (30 min)
#P(A >= 5) = 1 - P(A <= 4)
prob_A_8b <- ppois(
  q = 4,
  lambda = lambda_30min_alta,
  lower.tail = FALSE
)

#Probabilidad de al menos 3 clientes en demanda regular (1 hora)
#P(B >= 3) = 1 - P(B <= 2)
prob_B_8b <- ppois(
  q = 2,
  lambda = lambda_1h_regular,
  lower.tail = FALSE
)

#Probabilidad conjunta por independencia
prob_8b <- prob_A_8b * prob_B_8b

#Resultado con cuatro decimales
round(prob_8b, 4)

#INCISO C Jornada Completa de 5 horas
#Lambda acumulado: 2 horas de alta (16) + 3 horas de regular (9)
lambda_jornada <- (8 * 2) + (3 * 3)

#Probabilidad de que el total supere los 25 clientes
#P(X > 25) = 1 - P(X <= 25)
prob_8c <- ppois(
  q = 25,
  lambda = lambda_jornada,
  lower.tail = FALSE
)

#Resultado con cuatro decimales
round(prob_8c, 4)




#Problema 9. Validación de códigos de descuento ----
#Escriba su código aquí:

#INCISO A EXITO EN EL QUINTO INTENTO
#Probabilidad de éxito
p_exito <- 0.08

#En R, dgeom() cuenta fracasos antes del primer éxito (k - 1)
intentos <- 5
fracasos <- intentos - 1

prob_9a <- dgeom(
  x = fracasos,
  prob = p_exito
)

#Resultado con cuatro decimales
round(prob_9a, 4)

#INCISO B MAS DE DIEZ INTENTOS REQUERIDOS
#P(X > 10) equivale a tener más de 10 intentos (más de 9 fracasos)
prob_9b <- pgeom(
  q = 10 - 1,
  prob = p_exito,
  lower.tail = FALSE
)

#Resultado con cuatro decimales
round(prob_9b, 4)

#Inciso c Numero esperado de intentos

#Esperanza matemática para la distribución geométrica
esperanza_9c <- 1 / p_exito

#Resultado con cuatro decimales
round(esperanza_9c, 4)

#inciso D falta de memoria tras 20 intentos 
#Por la propiedad de falta de memoria: P(X > 20 + k | X > 20) = P(X > k)
#El número esperado de intentos adicionales es el mismo valor esperado inicial:
esperanza_adicional_9d <- 1 / p_exito

#Resultado con cuatro decimales
round(esperanza_adicional_9d, 4)




# ==============================================================================
# BLOQUE D: Distribución binomial negativa ----
# Responsable: [Integrante 4]
# ==============================================================================

# Problema 10. Rechazo de transacciones por sospecha de fraude ----
p_rechazo <- 0.12
p_aceptacion <- 1 - p_rechazo

# a: probabilidad de primer intento rechazado y segundo aceptado
prob_10a <- p_rechazo * p_aceptacion
round(prob_10a, 4)

# b: probabilidad de cuarta aceptada dado 3 rechazos previos
prob_10b <- p_aceptacion
round(prob_10b, 4)

# c: probabilidad de más de 3 intentos
prob_10c <- 1 - pgeom(q = 3, prob = p_aceptacion)
round(prob_10c, 4)


# Problema 11. Costo esperado de depuración ----
p <- 0.15
costo <- 50

# a: sacamos el promedio dividiendo 1 entre p
esperanza <- 1 / p
round(esperanza, 4)

# b: multiplicamos el promedio por los 50 dólares de cada intento
costo_total <- esperanza * costo
round(costo_total, 4)

# c: probabilidad de pasar de los 500 dólares (osea más de 10 intentos)
prob_excede <- 1 - pgeom(q = 10, prob = p)
round(prob_excede, 4)

# d: comparamos de p = 0.30 y 85 
p_nueva <- 0.30
costo_nuevo <- 85
esperanza_nueva <- 1 / p_nueva
costo_total_nuevo <- esperanza_nueva * costo_nuevo

tabla_11d <- data.frame(
  Plataforma = c("Actual", "Nueva"),
  Costo_Esperado = round(c(costo_total, costo_total_nuevo), 2)
)
print(tabla_11d)


# Problema 12. Muestreo hasta detectar defectos ----
r <- 3
p_defectuosa <- 0.04

# a: probabilidad de que sean necesarias exactamente 50 piezas
intentos <- 50
fracasos_a <- intentos - r
prob_12a <- dnbinom(x = fracasos_a, size = r, prob = p_defectuosa)
round(prob_12a, 4)

# b: número esperado de piezas a muestrear
esperanza_b <- r / p_defectuosa
round(esperanza_b, 4)

# c: probabilidad de terminar con 40 piezas o menos
intentos_c <- 40
fracasos_c <- intentos_c - r
prob_12c <- pnbinom(q = fracasos_c, size = r, prob = p_defectuosa)
round(prob_12c, 4)

# ==============================================================================
# BLOQUE E: Distribución hipergeométrica ----
# Responsable: [Integrante 5]
# ==============================================================================


# Problema 13. Contactación hasta lograr entrevistas ----
# Escriba su código aquí:


# Problema 14. Detección de sobredispersión y elección de modelo ----
# Escriba su código aquí:


# Problema 15. Auditoría de un lote de servidores ----
# Escriba su código aquí:


# Problema 16. Aproximación de la hipergeométrica por la binomial ----
# Escriba su código aquí:


# ==============================================================================
# BLOQUE F: Problemas integradores ----
# Responsable: [Integrante 6]
# ==============================================================================



# ============================================================
# PROBLEMA 17
# Identificación del modelo probabilístico apropiado
# ============================================================


# ------------------------------------------------------------
# PROBLEMA 17(a)
# Distribución hipergeométrica
# ------------------------------------------------------------

# Datos
N <- 300       # Total de usuarios
K <- 6         # Número de bots: 2% de 300
n <- 40        # Tamaño de la muestra
x <- 3         # Bots que se desean encontrar

# Probabilidad de encontrar exactamente 3 bots
prob_17a <- dhyper(
  x = x,
  m = K,
  n = N - K,
  k = n
)

# Resultado con cuatro decimales
round(prob_17a, 4)


# ------------------------------------------------------------
# PROBLEMA 17(b)
# Distribución de Poisson
# ------------------------------------------------------------

# Promedio mensual de devoluciones
lambda_mensual <- 2.5

# Un trimestre tiene 3 meses
lambda_trimestre <- lambda_mensual * 3

# Probabilidad de recibir exactamente 6 devoluciones
prob_17b <- dpois(
  x = 6,
  lambda = lambda_trimestre
)

# Resultado con cuatro decimales
round(prob_17b, 4)


# ------------------------------------------------------------
# PROBLEMA 17(c)
# Distribución binomial negativa
# ------------------------------------------------------------

# Probabilidad de éxito por intento
p <- 0.30

# Número de éxitos requeridos
r <- 2

# Número total de intentos
intentos <- 6

# R utiliza el número de fracasos antes de alcanzar los éxitos
fracasos <- intentos - r

# Probabilidad de necesitar exactamente 6 intentos
prob_17c <- dnbinom(
  x = fracasos,
  size = r,
  prob = p
)

# Resultado con cuatro decimales
round(prob_17c, 4)


# ------------------------------------------------------------
# PROBLEMA 17(d)
# Distribución binomial
# ------------------------------------------------------------

# Datos
n <- 12
p <- 0.25

# Probabilidad de que al menos 4 estudiantes
# obtengan calificación de honor
# P(X >= 4) = 1 - P(X <= 3)

prob_17d <- 1 - pbinom(
  q = 3,
  size = n,
  prob = p
)

# Resultado con cuatro decimales
round(prob_17d, 4)



# ============================================================
# PROBLEMA 18
# Contrastación de una tasa de fallas contractual
# ============================================================


# ------------------------------------------------------------
# PROBLEMA 18(a)
# Distribución binomial exacta
# ------------------------------------------------------------

# Datos
n <- 400
p <- 0.005

# Probabilidad de observar más de 5 fallas
# P(X > 5) = P(X >= 6)
# P(X >= 6) = 1 - P(X <= 5)

prob_18a <- 1 - pbinom(
  q = 5,
  size = n,
  prob = p
)

# Resultado con cuatro decimales
round(prob_18a, 4)


# ------------------------------------------------------------
# PROBLEMA 18(b)
# Aproximación mediante distribución de Poisson
# ------------------------------------------------------------

# Parámetros de la distribución binomial
n <- 400
p <- 0.005

# Parámetro lambda para Poisson
lambda <- n * p

# Probabilidad binomial exacta
prob_binomial_18b <- 1 - pbinom(
  q = 5,
  size = n,
  prob = p
)

# Probabilidad usando aproximación de Poisson
prob_poisson_18b <- 1 - ppois(
  q = 5,
  lambda = lambda
)

# Diferencia absoluta entre ambos modelos
diferencia_18b <- abs(
  prob_binomial_18b - prob_poisson_18b
)

# Resultados
round(prob_binomial_18b, 4)
round(prob_poisson_18b, 4)
round(diferencia_18b, 4)


# Tabla comparativa
tabla_18b <- data.frame(
  Metodo = c(
    "Binomial exacta",
    "Aproximación Poisson"
  ),
  Probabilidad = round(
    c(
      prob_binomial_18b,
      prob_poisson_18b
    ),
    4
  )
)

print(tabla_18b)


# ------------------------------------------------------------
# PROBLEMA 18(c)
# Comparación entre dos tasas de falla
# ------------------------------------------------------------

# Número de unidades
n <- 400

# Tasa contractual
p_contractual <- 0.005

# Tasa alternativa
p_alternativa <- 0.01

# Probabilidad de observar 6 o más fallas
# bajo la tasa contractual
prob_contractual <- 1 - pbinom(
  q = 5,
  size = n,
  prob = p_contractual
)

# Probabilidad de observar 6 o más fallas
# bajo la tasa alternativa
prob_alternativa <- 1 - pbinom(
  q = 5,
  size = n,
  prob = p_alternativa
)

# Resultados
round(prob_contractual, 4)
round(prob_alternativa, 4)


# Tabla comparativa
tabla_18c <- data.frame(
  Tasa = c(
    "Contractual: 0.5%",
    "Alternativa: 1%"
  ),
  Probabilidad_X_mayor_igual_6 = round(
    c(
      prob_contractual,
      prob_alternativa
    ),
    4
  )
)

print(tabla_18c)



# ============================================================
# PROBLEMA 19
# Priorización de tickets en un sistema de soporte
# ============================================================


# ------------------------------------------------------------
# PROBLEMA 19(a)
# Dos procesos de Poisson independientes
# ------------------------------------------------------------

# Tasa de tickets de prioridad alta A
lambda_A <- 4

# Tasa de tickets de prioridad baja B
lambda_B <- 6

# Probabilidad de recibir exactamente 3 tickets A
prob_A <- dpois(
  x = 3,
  lambda = lambda_A
)

# Probabilidad de recibir exactamente 8 tickets B
prob_B <- dpois(
  x = 8,
  lambda = lambda_B
)

# Como los procesos son independientes:
# P(A = 3 y B = 8) = P(A = 3) * P(B = 8)

prob_19a <- prob_A * prob_B

# Resultado con cuatro decimales
round(prob_19a, 4)


# ------------------------------------------------------------
# PROBLEMA 19(b)
# Tickets de prioridad alta durante media hora
# ------------------------------------------------------------

# Tasa de tickets A por hora
lambda_hora <- 4

# Intervalo de tiempo: 30 minutos = 0.5 horas
tiempo <- 0.5

# Lambda correspondiente a media hora
lambda_media_hora <- lambda_hora * tiempo

# Probabilidad de que lleguen al menos 2 tickets
# P(X >= 2) = 1 - P(X <= 1)

prob_19b <- 1 - ppois(
  q = 1,
  lambda = lambda_media_hora
)

# Resultado con cuatro decimales
round(prob_19b, 4)


# ------------------------------------------------------------
# PROBLEMA 19(c)
# Simulación con 100,000 réplicas
# ------------------------------------------------------------

# Semilla indicada en el ejercicio
set.seed(2026)

# Número de simulaciones
n_sim <- 100000

# Lambda de tickets A durante media hora
lambda <- 2

# Generar 100,000 observaciones Poisson
simulacion_19 <- rpois(
  n = n_sim,
  lambda = lambda
)

# Probabilidad simulada de recibir al menos 2 tickets
prob_simulada_19 <- mean(
  simulacion_19 >= 2
)

# Probabilidad teórica
prob_teorica_19 <- 1 - ppois(
  q = 1,
  lambda = lambda
)

# Resultados
round(prob_teorica_19, 4)
round(prob_simulada_19, 4)


# Tabla comparativa
tabla_19c <- data.frame(
  Metodo = c(
    "Probabilidad teórica",
    "Simulación"
  ),
  Probabilidad = round(
    c(
      prob_teorica_19,
      prob_simulada_19
    ),
    4
  )
)

print(tabla_19c)



# ============================================================
# PROBLEMA 20
# Análisis integrado de rechazos en un servicio web
# ============================================================


# ------------------------------------------------------------
# PROBLEMA 20(a)
# Distribución binomial exacta
# ------------------------------------------------------------

# Número de solicitudes
n <- 500

# Probabilidad de rechazo
p <- 0.02

# Probabilidad de obtener 5 o menos rechazos
prob_20a <- pbinom(
  q = 5,
  size = n,
  prob = p
)

# Resultado con cuatro decimales
round(prob_20a, 4)


# ------------------------------------------------------------
# PROBLEMA 20(b)
# Aproximación mediante distribución de Poisson
# ------------------------------------------------------------

# Parámetros
n <- 500
p <- 0.02

# Lambda = n * p
lambda <- n * p

# Probabilidad mediante Poisson
prob_20b_poisson <- ppois(
  q = 5,
  lambda = lambda
)

# Probabilidad binomial exacta para comparar
prob_20b_binomial <- pbinom(
  q = 5,
  size = n,
  prob = p
)

# Diferencia absoluta
diferencia_20b <- abs(
  prob_20b_binomial - prob_20b_poisson
)

# Resultados
round(prob_20b_binomial, 4)
round(prob_20b_poisson, 4)
round(diferencia_20b, 4)


# Tabla comparativa
tabla_20b <- data.frame(
  Metodo = c(
    "Binomial exacta",
    "Aproximación Poisson"
  ),
  Probabilidad = round(
    c(
      prob_20b_binomial,
      prob_20b_poisson
    ),
    4
  )
)

print(tabla_20b)


# ------------------------------------------------------------
# PROBLEMA 20(c)
# Simulación Binomial y Poisson
# ------------------------------------------------------------

# Semilla indicada por el ejercicio
set.seed(2026)

# Número de réplicas
n_sim <- 100000

# Simulación de la distribución binomial exacta
sim_binomial <- rbinom(
  n = n_sim,
  size = 500,
  prob = 0.02
)

# Simulación de la aproximación Poisson
sim_poisson <- rpois(
  n = n_sim,
  lambda = 10
)

# Probabilidad simulada Binomial:
# proporción de casos con 5 o menos rechazos
p_sim_binomial <- mean(
  sim_binomial <= 5
)

# Probabilidad simulada Poisson:
# proporción de casos con 5 o menos rechazos
p_sim_poisson <- mean(
  sim_poisson <= 5
)

# Probabilidad teórica Binomial
p_exacta <- pbinom(
  q = 5,
  size = 500,
  prob = 0.02
)

# Probabilidad teórica Poisson
p_poisson <- ppois(
  q = 5,
  lambda = 10
)

# Tabla comparativa
tabla_20c <- data.frame(
  Metodo = c(
    "Binomial exacta",
    "Poisson teórica",
    "Simulación binomial",
    "Simulación Poisson"
  ),
  Probabilidad = round(
    c(
      p_exacta,
      p_poisson,
      p_sim_binomial,
      p_sim_poisson
    ),
    4
  )
)

print(tabla_20c)


# ------------------------------------------------------------
# PROBLEMA 20(d)
# Comparación entre tasa nominal y tasa alternativa
# ------------------------------------------------------------

# Número de solicitudes
n <- 500

# Tasa nominal
p_nominal <- 0.02

# Tasa alternativa
p_alternativa <- 0.03

# Probabilidad de observar 15 o más rechazos
# bajo la tasa nominal
prob_nominal <- 1 - pbinom(
  q = 14,
  size = n,
  prob = p_nominal
)

# Probabilidad de observar 15 o más rechazos
# bajo la tasa alternativa
prob_alternativa <- 1 - pbinom(
  q = 14,
  size = n,
  prob = p_alternativa
)

# Resultados
round(prob_nominal, 4)
round(prob_alternativa, 4)


# Tabla comparativa
tabla_20d <- data.frame(
  Tasa = c(
    "Nominal: 2%",
    "Alternativa: 3%"
  ),
  Probabilidad_X_mayor_igual_15 = round(
    c(
      prob_nominal,
      prob_alternativa
    ),
    4
  )
)

print(tabla_20d)
