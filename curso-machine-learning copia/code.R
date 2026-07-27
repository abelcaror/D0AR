
# 01 - Ajustes iniciales --------------------------------------------------

library(ggplot2)
library(corrplot)
library(mice)
library(caret)
library(pROC)
library(class)
library(rpart)
library(rpart.plot)
library(randomForest)
library(xgboost)
library(factoextra)


setwd("~/Library/Mobile Documents/com~apple~CloudDocs/D0AR/Cursos/curso-machine-learning")
autos <- read.csv("datos/coches.csv", stringsAsFactors = F)
saveRDS(object = autos, file = "autos_raw.RDS")


# 02 - Estructura de los datos --------------------------------------------

str(autos)
summary(autos)

colSums(is.na(autos))

ggplot(autos, aes(x = potencia_cv)) +
  geom_histogram(bins = 20, fill = "red", color = "white") +
  labs(title = "Distribución de potencia (cv)",
       x = "Potencia", y = "Nº de coches") +
  theme_minimal()

ggplot(autos, aes(x = combustible, y = precio_eur, fill = combustible)) +
  geom_boxplot(show.legend = F) +
  scale_y_log10() +
  theme_minimal()
  
orden_cat <- names(sort(table(autos$categoria), decreasing = T))
autos$categoria_ord <- factor(autos$categoria, levels = orden_cat)

ggplot(autos, aes(x = categoria_ord, fill = categoria_ord)) +
  geom_bar(show.legend = F) +
  coord_flip() +
  labs(title = "Coches por categoría") +
  theme_minimal()

autos$categoria_ord <- NULL

ggplot(autos, aes(x = potencia_cv, y = tiempo_0_100, color = categoria)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = F, color = "gray", linetype = "dashed") +
  labs(title = "Potencia vs tiempo 0-100 km/h") +
  theme_minimal()

cols_num <- sapply(autos, is.numeric)
nums <- na.omit(autos[, cols_num])

corrplot(cor(nums),
         method = "color",
         type = "upper", 
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "Correlación entre variables",
         mar = c(0, 0, 1, 0))


# 03 - Limpieza inicial de datos ------------------------------------------

pct_na <- sapply(autos, function(x) mean(is.na(x)) * 100)
pct_na <- sort(round(pct_na[pct_na > 0], 1), decreasing = T)
print(pct_na)

tabla_na_cil <- table(autos$combustible[is.na(autos$cilindrada_cc)])
print(tabla_na_cil)
tabla_na_con <- table(autos$combustible[is.na(autos$consumo_l100km)])
print(tabla_na_con)

autos$cilindrada_cc[autos$combustible == "Electrico"] <- 0
autos$consumo_l100km[autos$combustible == "Electrico"] <- 0

for(cat in unique(autos$categoria)) {
  idx_na <- is.na(autos$precio_eur) & autos$categoria == cat
  med <- median(autos$precio_eur[autos$categoria == cat], na.rm = T)
  autos$precio_eur[idx_na] <- med
}

autos_mice_in <- autos[, c("tiempo_0_100", "potencia_cv", "peso_kg")]

imputed <-
  mice(
    autos_mice_in,
    m = 5,
    method = "pmm",
    seed = 40,
    printFlag = F
  )

autos$tiempo_0_100 <- complete(imputed, 1)$tiempo_0_100

colSums(is.na(autos))

# autos$marca <- as.factor(autos$marca)
# autos$traccion <- as.factor(autos$traccion)

saveRDS(object = autos, file = "autos_sin_na.RDS")


# 04 - Transformación final -----------------------------------------------

# IQR - Potencia
q1 <- quantile(autos$potencia_cv, 0.25, na.rm = T)
q3 <- quantile(autos$potencia_cv, 0.75, na.rm = T)
iqr <- q3 - q1

lim_inf <- q1 - 1.5 * iqr
lim_sup <- q3 + 1.5 * iqr

cat("Límite inferior IQR: ", lim_inf, "CV\n")
cat("Límite superior IQR: ", lim_sup, "CV\n")

idx_out <- autos$potencia_cv < lim_inf | autos$potencia_cv > lim_sup
print(autos[idx_out, c("marca", "modelo", "potencia_cv", "categoria")])

autos <- autos[!idx_out, ]

# Z-Score
autos$z_peso <- as.numeric(scale(autos$peso_kg))
print(autos[abs(autos$z_peso) > 3, c("marca", "modelo", "peso_kg", "z_peso")])

autos <- autos[abs(autos$z_peso) <= 3, ]

# Directo
autos <- autos[autos$potencia_cv < 2000 & autos$peso_kg > 1000, ]

# Transformación pre-algoritmos
autos$cv_por_kg <- autos$potencia_cv / autos$peso_kg
autos$es_superdeportivo <- as.integer(autos$categoria == "Superdeportivo")
date <- unlist(strsplit(date(), " "))
autos$antiguedad <- as.numeric(date[length(date)]) - autos$anio

autos$potencia_std <- as.numeric(scale(autos$potencia_cv))
autos$peso_std <- as.numeric(scale(autos$peso_kg))
autos$precio_std <- as.numeric(scale(autos$precio_eur))

minmax <- function(x) {
  (x - min(x, na.rm = T)) /
    (max(x, na.rm = T) - min(x, na.rm = T))
}

autos$potencia_norm <- minmax(autos$potencia_cv)
autos$peso_norm <- minmax(autos$peso_kg)
autos$precio_norm <- minmax(autos$precio_eur)

dummies <- dummyVars(~ traccion + combustible, data = autos, fullRank = T)
autos_enc <- as.data.frame(predict(dummies, newdata = autos))
autos <- cbind(autos, autos_enc)

set.seed(123)
idx_train <- createDataPartition(autos$precio_eur, p = 0.8, list = F)

train <- autos[idx_train, ]
test <- autos[-idx_train, ]

saveRDS(autos, "autos_transformado.RDS")
saveRDS(train, "train.RDS")
saveRDS(test, "test.RDS")


# 05 - Regresión lineal ---------------------------------------------------

cols_reg <- c("precio_eur", "potencia_cv", "peso_kg", "tiempo_0_100",
              "cilindrada_cc", "cv_por_kg", "consumo_l100km")

autos_reg <- na.omit(autos[, cols_reg])

set.seed(42)
idx_reg <- createDataPartition(autos_reg$precio_eur, p = .75, list = F)

tr_reg <- autos_reg[idx_reg, ]
te_reg <- autos_reg[-idx_reg, ]

m_multi <- lm(log(precio_eur) ~ potencia_cv + peso_kg + tiempo_0_100 +
                cilindrada_cc + cv_por_kg + consumo_l100km,
              data = tr_reg)
summary(m_multi)

par(mfrow = c(2, 2))
plot(m_multi, col = "steelblue", pch = 16)
par(mfrow = c(1, 1))


rmse <- function(real, pred) sqrt(mean((real-pred)^2))
mae <- function(real, pred) mean(abs(real-pred))
r2 <- function(real, pred) 1 - sum((real-pred)^2) /
                                sum((real-mean(real))^2)

fmt <- function(x) format(round(x), big.mark. = ".", scientific = F)

pred_tr <- predict(m_multi, newdata = tr_reg)
pred_te <- predict(m_multi, newdata = te_reg)

cat("TRAIN: \n")
cat("RMSE: ", fmt(rmse(tr_reg$precio_eur, pred_tr)), "€\n")
cat("MAE: ", fmt(mae(tr_reg$precio_eur, pred_tr)), "€\n")
cat("R2: ", fmt(r2(tr_reg$precio_eur, pred_tr)), "€\n")

cat("TEST: \n")
cat("RMSE: ", fmt(rmse(te_reg$precio_eur, pred_te)), "€\n")
cat("MAE: ", fmt(mae(te_reg$precio_eur, pred_te)), "€\n")
cat("R2: ", fmt(r2(te_reg$precio_eur, pred_te)), "€\n")
  
  
comp_df <- data.frame(real = te_reg$precio_eur, predicho = pred_te)

ggplot(comp_df, aes(x = real, y = predicho)) +
  geom_point(size = 2.5) +
  labs(title = "Precio real vs predicho") +
  theme_minimal()


# 06 - Algoritmos clasificatorios -----------------------------------------

# Logit
cols_cls <- c("es_superdeportivo", "potencia_cv", "peso_kg", "tiempo_0_100",
              "cilindrada_cc", "consumo_l100km", "cv_por_kg")

autos_cls <- na.omit(autos[, cols_cls])
autos_cls$es_superdeportivo <-
  factor(
    autos_cls$es_superdeportivo,
    levels = c(0, 1),
    labels = c("No", "Si")
  )

set.seed(42)
idx_cls <- createDataPartition(autos_cls$es_superdeportivo, p = 0.75, list = F)
tr_cls <- autos_cls[idx_cls, ]
te_cls <- autos_cls[-idx_cls, ]

logit <-
  glm(es_superdeportivo ~ potencia_cv + peso_kg + tiempo_0_100 +
        cilindrada_cc + consumo_l100km + cv_por_kg,
      data = tr_cls,
      family = binomial(link = "logit"))

summary(logit)

curve(1 / (1 + exp(-x)), from = -6, to = 6,
      col = "#e63946", lwd = 2,
      main = "Función sigmoide",
      xlab = "Combinación lineal de predictores", ylab = "P(superdeportivo)")
abline(h = 0.5, lty = 2, col = "gray50")

prob_test <- predict(logit, newdata = te_cls, type = "response")
pred_cls <- factor(ifelse(prob_test >= .5, "Si", "No"), levels = c("No", "Si"))

cm <- confusionMatrix(pred_cls, te_cls$es_superdeportivo, positive = "Si")
print(cm)

roc_obj <- roc(te_cls$es_superdeportivo, prob_test, levels = c("No", "Si"))
plot(roc_obj,
     col = "steelblue",
     lwd = 2,
     print.auc = T,
     main = paste0("Curva ROC - Reg logística (AUC = ",
                   round(auc(roc_obj), 3), ")"))

# KNN
feats <- c("potencia_cv", "peso_kg", "tiempo_0_100", "cv_por_kg")

tr_knn <- scale(tr_cls[, feats])
te_knn <- scale(te_cls[, feats],
                center = attr(tr_knn, "scaled:center"),
                scale = attr(tr_knn, "scaled:scale"))

res_k <- do.call(rbind, lapply(1:15, function(k) {
  p <- knn(train = tr_knn, test = te_knn,
           cl = tr_cls$es_superdeportivo, k = k)
  acc <- mean(p == te_cls$es_superdeportivo)
  data.frame(k = k, accuracy = acc)
}))

res_k$acc_pct <- round(res_k$accuracy * 100, 2)
print(res_k)

best_k <- res_k$k[which.max(res_k$accuracy)]
pred_knn <- knn(train = tr_knn, test = te_knn,
                cl = tr_cls$es_superdeportivo, k = best_k)
cat("\nKNN (K = ", best_k, ") - Accuracy: ",
    round(mean(pred_knn == te_cls$es_superdeportivo), 3), "\n")


# 07 - Árboles y bosques de decisión --------------------------------------

autos_tree <- autos_cls

set.seed(42)
idx_t <- createDataPartition(autos_tree$es_superdeportivo, p = .75, list = F)
tr_t <- autos_tree[idx_t, ]
te_t <- autos_tree[-idx_t, ]

arbol <-
  rpart(
    es_superdeportivo ~ .,
    data = tr_t,
    method = "class",
    control = rpart.control(cp = 0.01, minsplit = 5)
  )

rpart.plot(
  arbol,
  type = 4,
  extra = 104,
  main = "Árbol para saber si es deportivo",
  col = "black"
)

printcp(arbol)
plotcp(arbol)

best_cp <- arbol$cptable[which.min(arbol$cptable[, "xerror"]), "CP"]
arbol_poda <- prune(arbol, cp = best_cp)

pred_arbol <- predict(arbol_poda, newdata = te_t, type = "class")
cat("Árbol podado - Accuracy: ",
    round(mean(pred_arbol == te_t$es_superdeportivo), 3), "\n")

# Random forest
set.seed(42)
rf <-
  randomForest(
    es_superdeportivo ~ .,
    data = tr_t,
    ntree = 500,
    mtry = 3,
    importance = T
  )

print(rf)

plot(rf, main = "Error 00B según número de árboles")
legend("topright", legend = colnames(rf$err.rate), col = 1:3, lty = 1)

pred_rf <- predict(rf, newdata = te_t)
cat("Random forest - Accuracy: ",
    round(mean(pred_rf == te_t$es_superdeportivo), 3), "\n")

imp_df <- as.data.frame(importance(rf))
imp_df$variable <- rownames(imp_df)
rownames(imp_df) <- NULL
imp_df <- imp_df[order(-imp_df$MeanDecreaseGini), ]

ggplot(imp_df, aes(x = reorder(variable, MeanDecreaseGini),
                   y = MeanDecreaseGini)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de variables en RF", x = NULL) +
  theme_minimal()


# 08 - XGBoost ------------------------------------------------------------

cols_xgb  <- c("es_superdeportivo", "potencia_cv", "peso_kg", "tiempo_0_100",
               "cilindrada_cc", "consumo_l100km", "cv_por_kg")
autos_xgb <- na.omit(autos[, cols_xgb])

set.seed(42)
idx_x <- createDataPartition(autos_xgb$es_superdeportivo, p = 0.75, list = FALSE)
tr_x  <- autos_xgb[ idx_x, ]
te_x  <- autos_xgb[-idx_x, ]

features <- c("potencia_cv", "peso_kg", "tiempo_0_100",
              "cilindrada_cc", "consumo_l100km", "cv_por_kg")

mat_tr <- as.matrix(tr_x[, features])
mat_te <- as.matrix(te_x[, features])
label_tr <- tr_x$es_superdeportivo
label_te <- te_x$es_superdeportivo

dtrain <- xgb.DMatrix(data = mat_tr, label = label_tr)
dtest <- xgb.DMatrix(data = mat_te, label = label_te)

params <- 
  list(
    objetive = "binary:logistic",
    eval_metric = "logloss",
    eta = .1,
    max_depth = 4
  )

set.seed(42)
xgb_base <- 
  xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    watchlist = list(train = dtrain, test = dtest),
    verbose = 0
  )

log_df <- as.data.frame(xgb_base$evaluation_log)

ggplot(log_df, aes(x = iter)) +
  geom_line(aes(y = train_logloss, color = "Train")) +
  geom_line(aes(y = test_logloss, color = "Test")) +
  theme_minimal()

pred_xgb <- predict(xgb_base, newdata = dtest, type = "class")
pred_xgb <- ifelse(pred_xgb >= .5, 1, 0)
cat("XGBoost base - Accuracy: ",
    round(mean(pred_xgb == label_te), 3), "\n")

grid_xgb <-
  expand.grid(
    nrounds = c(50, 100, 200),
    max_depth = c(3, 4, 6),
    eta = c(.05, .1, .3),
    gamma = 0,
    colsample_bytree = .8,
    min_child_weight = 1,
    subsample = .8
  )

ctrl_xgb <- trainControl(method = "cv", number = 5, verboseIter = F)

set.seed(42)
xgb_cv <- 
  train(
    x = mat_tr,
    y = factor(label_tr, labels = c("No", "Si")),
    method = "xgbTree",
    trControl = ctrl_xgb,
    tuneGrid = grid_xgb
  )

xgb_cv$bestTune

pred_xgb_cv <- predict(xgb_cv, newdata = mat_te)
cat("XGBoost (CV) - Accuracy: ", 
    round(mean(pred_xgb_cv == factor(label_te, labels = c("No", "Si"))), 3), "\n")


imp_xgb <- xgb.importance(feature_names = features, model = xgb_base)
xgb.plot.importance(
  imp_xgb,
  main = "Importancia de variables - XGBoost base",
  col = "steelblue"
)


# 09 - Clustering ---------------------------------------------------------

telemetria <- read.csv("datos/telemetria.csv", stringsAsFactors = F)
str(telemetria)

cols_tel <- c("velocidad_max_kmh", "velocidad_media_kmh", "frenadas_fuertes",
              "g_lateral_max", "tiempo_vuelta_s", "desgaste_neumatico_pct",
              "combustible_consumido_l")

tel_num <- scale(telemetria[, cols_tel])

set.seed(42)
fviz_nbclust(tel_num, kmeans, method = "wss") +
  labs(title = "Método del codo - ¿Cuántos clusters", 
       x = "Número de clusters K",
       y = "Suma de cuadrados intra-cluster (WSS)")

fviz_nbclust(tel_num, kmeans, method = "silhouette") +
  labs(title = "Método de Silhouette - ¿Cuántos clusters", 
       x = "Número de clusters K")

# K-means k = 3
set.seed(42)
km <- kmeans(tel_num, centers = 3, nstart = 25)

fviz_cluster(km, data = tel_num,
             geom = "point",
             ellipse = T,
             palette = c("#e63946", "#457b9d", "#2a9d8f"),
             main = "Clusters de estilos de pilotaje según K-Means") +
  theme_minimal()


telemetria$cluster <- factor(km$cluster,
                             labels = c("Estilo A", "Estilo B", "Estilo C"))

perfil <- aggregate(telemetria[, c("velocidad_max_kmh", "tiempo_vuelta_s",
                                   "frenadas_fuertes", "desgaste_neumatico_pct")],
                    by = list(cluster = telemetria$cluster),
                    FUN = function(x) round(mean(x), 1))
print(perfil)

# Clustering jerárquico
dist_mat <- dist(tel_num, method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")

fviz_dend(hc,
          k = 3,
          palette = c("#e63946", "#457b9d", "#2a9d8f"),
          main = "Dendograma - Distancia euclídea") +
  theme_minimal()


pca <- prcomp(tel_num, center = T, scale. = T)
summary(pca)

fviz_eig(pca,
         col.var = "contrib",
         gradient.cols = c("#2a9d8f", "#457b9d", "#e63946"),
         repel = T,
         title = "Variables en el espacio PCA")


# 10 - Validación de modelos ----------------------------------------------

cols_eval  <- c("precio_eur", "potencia_cv", "peso_kg", "tiempo_0_100",
                "cilindrada_cc", "cv_por_kg", "consumo_l100km")

autos_eval <- na.omit(autos[, cols_eval])

ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = F,
  savePredictions = "final"
)

set.seed(42)
m_lm_cv <- train(precio_eur ~., data = autos_eval,
                 method = "lm", trControl = ctrl)

grid_rf <- data.frame(mtry = c(2, 3, 4, 5, 6))

set.seed(42)
m_rf_cv <- 
  train(
    precio_eur ~.,
    data = autos_eval,
    method = "rf",
    trControl = ctrl,
    tuneGrid = grid_rf,
    ntree = 300
  )

resultados <- resamples(list(Lineal = m_lm_cv, RandomForest = m_rf_cv))

bwplot(resultados, metric = "RMSE",
       main = "RMSE por fold - Regresión lineal vs Random Forest")

dotplot(resultados, metric = "Rsquared",
        main = "R2 for fold - Regresión lineal vs Random Forest")

saveRDS(m_rf_cv, "model_rf_cv_precio.RDS")
