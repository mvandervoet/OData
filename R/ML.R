# 📦 Packages laden
utils::install.packages(c("randomForest", "caret", "dplyr", "here"))  # alleen als nog niet geïnstalleerd
library(randomForest)
library(caret)
library(dplyr)
library(here)

# 📂 1. Voorbeelddata inlezen (vervang dit door je eigen data-inleesmethode)
# Stel: data staat in 'Baarn - train.csv' met kolommen: Gemeente, Vraagprijs, Wonen_m2, Perceel_m2, Slaapkamers, Energielabel
data <- utils::read.csv("~/Downloads/Baarn - train.csv")

# 🧹 2. Preprocessing
# Kolomnamen standaardiseren
colnames(data) <- c("Gemeente", "Vraagprijs", "Wonen_m2", "Perceel_m2", "Slaapkamers", "Energielabel")
if (ncol(data) > 6) {
  data <- data[, -ncol(data)]
}

# Verwijder eventuele rijen met missende waarden
data <- tidyr::drop_na(data)

# Categoriën omzetten naar factor
data$Gemeente <- as.factor(data$Gemeente)
data$Energielabel <- as.factor(data$Energielabel)

# 🧪 3. Train/Test split
set.seed(123)  # voor reproduceerbaarheid
train_index <- caret::createDataPartition(data$Vraagprijs, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# 🌳 4. Random Forest model trainen
model <- randomForest::randomForest(
  Vraagprijs ~ Gemeente + Wonen_m2 + Perceel_m2 + Slaapkamers + Energielabel,
  data = train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

# 📈 5. Voorspellingen en evaluatie
predictions <- stats::predict(model, newdata = test_data)
rmse <- sqrt(mean((predictions - test_data$Vraagprijs)^2))
cat("RMSE op testset:", rmse, "\n")

# 🔍 6. Belangrijkste variabelen tonen
graphics::barplot(
  randomForest::importance(model)[, 1],
  main = "Belangrijkheid van variabelen",
  las = 2
)

# 📊 7. Nieuwe voorspelling maken
# Stel: je hebt een nieuwe woning
nieuwe_woning <- data.frame(
  Gemeente = factor("Baarn", levels = levels(data$Gemeente)),
  Wonen_m2 = 155,
  Perceel_m2 = 455,
  Slaapkamers = 6,
  Energielabel = factor("D", levels = levels(data$Energielabel))
)
voorspelde_prijs <- stats::predict(model, newdata = nieuwe_woning)
cat("Voorspelde vraagprijs voor nieuwe woning:", round(voorspelde_prijs), "\n")
