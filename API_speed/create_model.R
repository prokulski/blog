library(randomForest)

# budujemy model
model <- randomForest(Species ~ ., data = iris)

# zapisujemy go w postaci pliku
saveRDS(model, "model_rf.RDS")
