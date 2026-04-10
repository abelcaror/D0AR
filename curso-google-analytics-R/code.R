
# Configuración general ---------------------------------------------------

library(googleAnalyticsR)
library(ggplot2)
library(memoise)

# property_id <- Sys.getenv("GA_PROPERTY_ID")

ga_auth(email = "carorubio.abel@gmail.com")

ga_accounts <- ga_account_list("ga4")
property_id <- 488984443
property_id_la <- 259059580


# Inputs ------------------------------------------------------------------

date_range <- c("2025-10-1", "2025-12-31")


# Métricas básicas --------------------------------------------------------

basic_metrics <-
  ga_data(
    propertyId = property_id,
    metrics = c("activeUsers", "sessions", "screenPageViews"),
    dimensions = "date", 
    date_range = date_range
  )

basic_metrics <- basic_metrics[order(basic_metrics$date), ]

basic_metrics$sessionsByUsers <- round(basic_metrics$sessions / basic_metrics$activeUsers, 2)


# Métricas avanzadas ------------------------------------------------------

meta <- ga_meta(version = "data", propertyId = property_id)
meta_metrics <- meta[meta$class == "metric", ]
meta_dimensions <- meta[meta$class == "dimension", ]
rm(meta); gc()

advance_metrics <- 
  ga_data(
    propertyId = property_id,
    metrics = c("activeUsers", "sessions", "engagementRate"),
    dimensions = c("date", "browser"),
    date_range = date_range
  )


# Datos filtrados ---------------------------------------------------------

filters <-
  ga_data_filter(
    country == "Spain" &
      pageTitle %begins% "D0AR - Curso de" &
      pageTitle %contains% "#"
    )

filtered_metrics <- 
  ga_data(
    propertyId = property_id,
    metrics = c("activeUsers", "sessions", "engagementRate"),
    dimensions = c("date", "pageTitle", "country"),
    date_range = date_range,
    dim_filters = filters
  )


# Orden y límites ---------------------------------------------------------

limited_metrics <- 
  ga_data(
    propertyId = property_id,
    metrics = c("activeUsers", "sessions", "engagementRate"),
    dimensions = c("date", "pageTitle", "country"),
    date_range = date_range,
    dim_filters = filters,
    limit = 500,
    page_size = 500L,
    orderBys = ga_data_order(-activeUsers)
  )


# Análisis temporal -------------------------------------------------------

datos_diarios <- ga_data(
  propertyId = property_id,
  metrics = "activeUsers",
  dimensions = "date",
  date_range = date_range
)

ggplot(datos_diarios, aes(x = date, y = activeUsers)) +
  geom_line(color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(title = "Tendencia de usuarios activos") +
  theme_minimal()


# Análisis por dispositivo ------------------------------------------------

dispositivos <- ga_data(
  propertyId = property_id,
  metrics = c("activeUsers", "sessions"),
  dimensions = "deviceCategory",
  date_range = date_range
)

dispositivos$sessionsByActiveUsers <- dispositivos$sessions / dispositivos$activeUsers

dispositivos_long <- rbind(
  data.frame(deviceCategory = dispositivos$deviceCategory,
             metrica = "activeUsers",
             valor = dispositivos$activeUsers),
  data.frame(deviceCategory = dispositivos$deviceCategory,
             metrica = "sessions",
             valor = dispositivos$sessions)
)


ggplot(dispositivos_long, aes(x = deviceCategory, y = valor, fill = metrica)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("activeUsers" = "steelblue", "sessions" = "coral"),
    labels = c("activeUsers" = "Usuarios activos", "sessions" = "Sesiones")
  ) +
  theme_minimal()


# Análisis de comportamiento ----------------------------------------------

eventos <- ga_data(
  propertyId = property_id_la,
  metrics = "eventCount",
  dimensions = "eventName",
  orderBys = ga_data_order(-eventCount),
  date_range = date_range
)

eventos <- eventos[1:5, ]
eventos$eventName <- factor(eventos$eventName, eventos$eventName)

plot_funnel <- 
  ggplot(eventos, aes(x = eventName, y = eventCount, fill = eventName)) +
    geom_col() +
    coord_flip()


# Automatizaciones --------------------------------------------------------

extraer_metricas <- function(input_metrics, input_dimensions) {
  ga_data(
    propertyId = property_id,
    date_range = date_range,
    metrics = input_metrics,
    dimensions = input_dimensions
  )
}

dispositivos <- extraer_metricas(c("activeUsers", "sessions"), "deviceCategory")
metricas_generales <- extraer_metricas(c("activeUsers", "sessions", "engagementRate"),
                                       c("date", "pageTitle", "country"))

ggsave(filename = "plot_funnel.png", plot = plot_funnel)

rmarkdown::render(input = "Untitled.Rmd")


# Optimización ------------------------------------------------------------

dispositivos <- ga_data(
  propertyId = property_id,
  metrics = c("activeUsers", "sessions"),
  dimensions = "deviceCategory",
  date_range = date_range,
  realtime = FALSE
)

ga_data_cache <- memoise(ga_data)

tryCatch({
  dispositivos <- ga_data(
    propertyId = 4879137,
    metrics = c("activeUsers", "sessions"),
    dimensions = "deviceCategory",
    date_range = date_range,
    realtime = FALSE
  )
}, error = function(e) {
  message("Error en la consulta: ", e$message)
  Sys.sleep(3)
})


