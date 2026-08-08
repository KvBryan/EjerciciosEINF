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


# Problema 7. Procesos de Poisson simultáneos e independientes ----
# Escriba su código aquí:


# Problema 8. Cajero con demanda variable ----
# Escriba su código aquí:


# Problema 9. Validación de códigos de descuento ----
# Escriba su código aquí:

# ==============================================================================
# BLOQUE D: Distribución binomial negativa ----
# Responsable: [Integrante 4]
# ==============================================================================

# Problema 10. Rechazo de transacciones por sospecha de fraude ----
# Escriba su código aquí:


# Problema 11. Costo esperado de depuración ----
# Escriba su código aquí:



# Problema 12. Muestreo hasta detectar defectos ----
# Escriba su código aquí:

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



# Problema 17. Identificación del modelo probabilístico apropiado ----
# Escriba su código aquí:


# Problema 18. Contrastación de una tasa de fallas contractual ----
# Escriba su código aquí:


# Problema 19. Priorización de tickets en un sistema de soporte ----
# Escriba su código aquí:


# Problema 20. Análisis integrado de rechazos en un servicio web ----
# Escriba su código aquí:

