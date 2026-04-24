
# Configuración inicial ----------------------------------------------------

library(readxl)
library(ggplot2)
library(gridExtra)

RUTA_EXCEL <- "datos/datos_prestaciones_vehiculo.xlsx"

# Importar y ordenar datos ------------------------------------------------

leer_vehiculo_excel <- function(ruta, nombre = "Vehículo base") {
  
  p <- read_excel(ruta,
                  sheet = "Parámetros",
                  col_names = FALSE,
                  col_types = "text")
  
  v <- function(fila, col = 3) { as.numeric(p[[col]][[fila]]) }
  
  V_des <- as.numeric(p[13, 3:7])
  
  list(
    nombre  = nombre,
    T_max   = v(4),
    w_tmax  = v(5),
    w_min   = v(6),
    w_max   = v(7),
    rend_t  = v(10),
    R_neum  = v(11),
    gamma_m = v(12),
    V_des   = V_des,
    m       = v(17),
    m_carga = v(18),
    Af      = v(19),
    Cx      = v(20),
    rho_r   = v(21),
    rho_a   = v(22),
    g       = v(23),
    H       = v(24) * 1e6,
    rend_m  = v(25),
    phi     = v(26)
  )
  
}

veh_base <- leer_vehiculo_excel(RUTA_EXCEL, nombre = "Citroen xSara")

enriquecer_vehiculo <- function(veh) {
  
  n_marchas <- length(veh$V_des)
  marchas <- seq_along(veh$V_des)
  v_des_ms <- veh$V_des / 3.6
  wr <- v_des_ms / veh$R_neum
  Wr <- wr * 60 / (2*pi)
  
  veh$r_marchas <- 1000 / Wr
  veh$n_marchas <- n_marchas
  veh$marchas <- marchas
  veh$phi_rad <- veh$phi * pi / 180
  veh$m_total <- veh$m + veh$m_carga
  
  return(veh)
  
}

veh_base <- enriquecer_vehiculo(veh_base)


# Motor -------------------------------------------------------------------

curva_par <- function(w_rpm, veh) {
  k <- veh$T_max / (veh$w_max - veh$w_tmax)^2
  T <- veh$T_max - k * (w_rpm -veh$w_tmax)^2
  pmax(T, 0)
}

curva_potencia <- function(w_rpm, veh) {
  T <- curva_par(w_rpm, veh)
  T * w_rpm * 2 * pi /60
}

w_rng <- seq(veh_base$w_min, veh_base$w_max, length.out = 300)

T_vec <- curva_par(w_rng, veh_base)
P_vec <- curva_potencia(w_rng, veh_base)

df_motor <- data.frame(rpm = w_rng, par = T_vec, potencia = P_vec / 1000)
df_motor$potencia_CV <- df_motor$potencia / 735.5 * 1000
  
ggplot(df_motor, aes(x = rpm, y = par)) +
  geom_line() +
  geom_vline(xintercept = w_rng[which.max(T_vec)],
             linetype = "dashed", colour = "steelblue", alpha = 0.6) +
  annotate("label", x = w_rng[which.max(T_vec)], y = max(T_vec) * 0.6,
           label = sprintf("%.0f Nm\n@ %.0f rpm", max(T_vec), w_rng[which.max(T_vec)]),
           fill = "white", colour = "steelblue", size = 3) +
  labs(title = "Curva de Par",
       x = "Régimen del motor (rpm)", y = "Par (Nm)") +
  theme_minimal()


# Relaciones de cambio ----------------------------------------------------

velocidad_marcha <- function(w_rpm, i_marcha, veh) {
  (w_rpm / veh$r_marchas[i_marcha]) * (2 * pi  / 60) * veh$R_neum * 3.6
}

w_m <- seq(veh_base$w_min, veh_base$w_max, lenght.out = 100)

v_mat <- sapply(veh_base$marchas, velocidad_marcha, w_rpm = w_m, veh = veh_base)
colnames(v_mat) <- paste0(1:veh_base$n_marchas, "ª")

df_marchas <- do.call(rbind, lapply(veh_base$marchas, function(i) {
  data.frame(rpm = w_m, velocidad = v_mat[, i], marcha = paste0(i, "ª"))
}))

ggplot(df_marchas, aes(x = velocidad, y = rpm, colour = marcha)) +
  geom_line(linewidth = 1.1)


# Tracción ----------------------------------------------------------------

fuerza_traccion <- function(w_rpm, i_marcha, veh) {
  curva_par(w_rpm, veh) * veh$r_marchas[i_marcha] * veh$rend_t / veh$R_neum
}

F_mat <- sapply(veh_base$marchas, fuerza_traccion,
                w_rpm = w_m, veh = veh_base)
colnames(F_mat) <- paste0(veh_base$marchas, "ª")

fuerza_resistiva <- function(v_kmh, veh, masa = NULL) {
  if(is.null(masa)) masa <- veh$m
  v_ms <- v_kmh / 3.6
  phi <- veh$phi_rad
  f_rod <- veh$rho_r * masa * veh$g
  f_gra <- masa * veh$g * sin(phi)
  f_aer <- 0.5 * veh$rho_a * veh$Cx * veh$Af * v_ms^2
  f_rod + f_gra + f_aer
}

vr <- 0:400
Fres <- fuerza_resistiva(vr, veh_base)

v_um <- v_mat[, veh_base$n_marchas]
F_um <- F_mat[, veh_base$n_marchas]
Fres_um <- fuerza_resistiva(v_um, veh_base)

diff_F <- F_um - Fres_um
idx_vmax <- which(diff(sign(diff_F)) < 0)

v_max <- if(!is.na(idx_vmax)) {
  approx(diff_F[idx_vmax:(idx_vmax + 1)],
         v_um[idx_vmax:(idx_vmax+1)], xout = 0)$y
}
  
df_trac <- do.call(rbind, lapply(veh_base$marchas, function(i) {
  data.frame(velocidad = v_mat[, i], fuerza = F_mat[, i],
             serie = paste0(i, "ª"))
}))

df_fres <- data.frame(velocidad = vr, fuerza = Fres, serie = "Resistencia")

ggplot() +
  geom_line(data = df_trac, aes(x = velocidad, y = fuerza, colour = serie),
            linewidth = 1.1) +
  geom_line(data = df_fres, aes(x = vr, y = fuerza), colour = "black")
  

# Potencia ----------------------------------------------------------------

P_mat <- F_mat * v_mat / 3.6 / 1000
Pres <- fuerza_resistiva(vr, veh_base) * (vr / 3.6) / 1000

Pres_marcha <- sapply(veh_base$marchas, function(i) {
  fuerza_resistiva(v_mat[, i], veh_base) * v_mat[, i] / 3.6 / 1000
})

Pexc <- P_mat - Pres_marcha

df_pot <- do.call(rbind, lapply(veh_base$marchas, function(i){
  data.frame(velocidad = v_mat[, i], potencia = P_mat[, i],
             serie = paste0(i, "ª"))
}))
df_pres <- data.frame(velocidad = vr, potencia = Pres, serie = "Resistencia")

df_pot_diff <- rbind(df_pot, df_pres)

ggplot() +
  geom_line(data = df_pot, aes(x = velocidad, y = potencia, colour = serie)) +
  geom_line(data = df_pres, aes(x = velocidad, y = potencia), colour = "black")

df_exc <- do.call(rbind, lapply(veh_base$marchas, function(i){
  data.frame(velocidad = v_mat[, i], exceso = Pexc[, i],
             marcha = paste0(i, "ª"))
}))

ggplot(df_exc, aes(x = velocidad, y = exceso, colour = marcha)) +
  geom_line()


# Aceleraciones -----------------------------------------------------------

aceleracion_marcha <- function(i, veh) {
  F_t <- fuerza_traccion(w_m, i, veh)
  v_km <- velocidad_marcha(w_m, i, veh)
  F_r <- fuerza_resistiva(v_km, veh)
  a <- (F_t - F_r) / (veh$m * veh$gamma_m) # F = m * a -> a = F / m
  data.frame(velocidad = v_km, aceleracion = a, marcha = paste0(i, "ª"))
}

df_acel <- do.call(rbind, lapply(veh_base$marchas,
                                 aceleracion_marcha,
                                 veh = veh_base))

ggplot(df_acel, aes(x = velocidad, y = aceleracion, color = marcha)) +
  geom_line()

evolucion_temporal <- function(i, veh) {
  F_t <- fuerza_traccion(w_m, i, veh)
  v_km <- velocidad_marcha(w_m, i, veh)
  F_r <- fuerza_resistiva(v_km, veh)
  a <- (F_t - F_r) / (veh$m * veh$gamma_m) # F = m * a -> a = F / m
  a <- pmax(a, 1e-6)
  
  v_ms <- v_km / 3.6
  delta_v <- diff(v_ms)
  delta_t <- delta_v / a[-length(a)]
  t_acum <- cumsum(delta_t)
  
  data.frame(tiempo = c(0, t_acum), velocidad = v_km, marcha = paste0(i, "ª"))
}

df_tevo <- do.call(rbind, lapply(veh_base$marchas,
                                 evolucion_temporal,
                                 veh = veh_base))

ggplot(df_tevo, aes(x = tiempo, y = velocidad, colour = marcha)) +
  geom_line() +
  coord_cartesian(xlim = c(0, 60), y = c(0, ceiling(v_max))) +
  theme_minimal()

# Ejemplo: tiempo que tardaría de 80 a 120 en 3ª 
round(
  min(df_tevo[df_tevo$marcha == "3ª" & df_tevo$velocidad >= 120, "tiempo"]) -
    min(df_tevo[df_tevo$marcha == "3ª" & df_tevo$velocidad >= 80, "tiempo"]),
  3)


# Pendientes máximas ------------------------------------------------------

pendiente_max_marcha <- function(i, veh) {
  v_km <- velocidad_marcha(w_m, i, veh)
  F_t <- fuerza_traccion(w_m, i, veh)
  v_ms <- v_km / 3.6
  
  f_aer <- 0.5 * veh$rho_a * veh$Cx * veh$Af * v_ms^2
  
  sin_phi <- (F_t - veh$rho_r * veh$m * veh$g - f_aer) / (veh$m * veh$g)
  # Alternativa por pasos:
  # f_gra <- (F_t - veh$rho_r * veh$m * veh$g - f_aer)
  # sin_phi <- f_gra / (veh$m * veh$g)
  sin_phi <- pmin(pmax(sin_phi, 0), 1)
  phi_deg <- asin(sin_phi) * 180 / pi
  
  data.frame(velocidad = v_km, pendiente_deg = phi_deg, marcha = paste0(i, "ª"))
}

df_pend <- do.call(rbind, lapply(veh_base$marchas,
                                 pendiente_max_marcha,
                                 veh = veh_base))

ggplot(data = df_pend, aes(x = velocidad, y = pendiente_deg, colour = marcha)) +
  geom_line() + 
  theme_minimal()


# Consumo -----------------------------------------------------------------

# IDEALMENTE: dataframe (simplificado) de rpm vs velocidad vs consumo específico
# data.frame(marcha = "1ª", rpm = c(1000, 1200, 1400), rend_m = c(.2, .4, .3))
# data.frame(marcha = "1ª", velocidad = c(1000, 1200, 1400
# marcha + velocidad -> rpm -> f_resistiva y rend_m -> consumo = F_resistiva / (rend_m * H) 

consumo_um <- function(v_kmh, veh, masa = NULL) {
  if (is.null(masa)) masa <- veh$m
  d <- 1e5 # Para pasar 100 km a metros
  F <- fuerza_resistiva(v_kmh, veh, masa = masa)
  F * d / (veh$H * veh$rend_m)
}

v_cons <- seq(80, 140, by = 5)
Q_veh <- consumo_um(v_cons, veh_base)

df_cons <- data.frame(velocidad = v_cons, consumo = Q_veh)
# v_opt <- v_cons[which.min(Q_veh)]
# Q_opt <- min(Q_veh)

ggplot(df_cons, aes(x = velocidad, y = consumo)) +
  geom_line()


# Modificaciones ----------------------------------------------------------

variantes <- lapply(
  list(
    list(nombre = "Base", T_max = veh_base$T_max, m = veh_base$m, Cx = veh_base$Cx),
    list(nombre = "Motor +15%", T_max = veh_base$T_max * 1.25, m = veh_base$m, Cx = veh_base$Cx),
    list(nombre = "Peso -120kg", T_max = veh_base$T_max, m = veh_base$m - 120, Cx = veh_base$Cx),
    list(nombre = "Cx 0.26", T_max = veh_base$T_max, m = veh_base$m, Cx = 0.26)
  ),
  function(mod) enriquecer_vehiculo(modifyList(veh_base, mod))
)
names(variantes) <- sapply(variantes, `[[`, "nombre")

t_evo_v <- function(veh) {
  evo <- evolucion_temporal(2, veh)
  idx <- which(evo$velocidad >= 100)[1]
  if(!is.na(idx)){
    approx(evo$velocidad[(idx-1):idx], evo$tiempo[(idx-1):idx], xout = 100)$y
  }
  else NA
}

vmax_v <- function(veh) {
  n <- veh$n_marchas
  v_ <- velocidad_marcha(w_m, n, veh)
  F_ <- fuerza_traccion(w_m, n, veh)
  d <- F_ - fuerza_resistiva(v_, veh)
  idx <- which(diff(sign(d)) < 0)[1]
  if(!is.na(idx)){
    approx(d[idx:(idx+1)], v_[idx:(idx+1)], xout = 0)$y
  }
  else max(v_)
}

resumen <- data.frame(
  Variantes = names(variantes),
  vmax_kmh = round(sapply(variantes, vmax_v), 2),
  t100_s = round(sapply(variantes, t_evo_v), 2),
  Q120_l100 = round(sapply(variantes, consumo_um, v_kmh = 120), 2)
)


# Sensibilidad de modificaciones ------------------------------------------

metrica_pendiente <- function(veh) {
  F_t <- veh$T_max * veh$r_marchas[1] * veh$rend_t / veh$R_neum
  sin_phi <- (F_t - veh$rho_r * veh$m * veh$g - veh$m * 0.5) / (veh$m * veh$g)
  asin(max(sin_phi, 0)) * 180 / pi
}

sensibilidad <- function(param, rango, fn_metrica, veh = veh_base) {
  data.frame(
    valor = rango,
    metrica = sapply(rango, function(val) {
      fn_metrica(enriquecer_vehiculo(modifyList(veh, setNames(list(val), param))))
    }),
    param = param
  )
}

p_sens <- function(df, x_base, titulo, eje_x, eje_y) {
  ggplot(df, aes(x = valor, y = metrica)) +
    geom_line() +
    geom_vline(xintercept = x_base, linetype  = "dashed", color = "red") +
    labs(title = titulo, x = eje_x, y = eje_y) +
    theme_minimal()
}

aux <- function(v) consumo_um(a, v)

grid.arrange(
  p_sens(sensibilidad("m", seq(900, 1600, 50), vmax_v), veh_base$m, "v_max vs masa", "masa (kg)", "v max (km/h)"),
  p_sens(sensibilidad("m", seq(900, 1600, 50), t_evo_v), veh_base$m, "t100 vs masa", "masa (kg)", "s"),
  p_sens(sensibilidad("Cx", seq(0.2, 0.5, 0.02), metrica_pendiente), veh_base$m, "pendiente vs Cx", "Cx", "pendiente"),
  p_sens(sensibilidad("Cx", seq(0.2, 0.5, 0.02), function(v) consumo_um(120, v)), veh_base$m, "Consumo vs Cx", "Cx", "Consumo"),
  ncol = 2, 
  top = "Sensibilidad de las modificaciones"
)











