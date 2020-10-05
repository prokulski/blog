# plumber.R

library(tidyverse)
library(randomForest)


# wczytanie modelu
model <- readRDS("model_rf.RDS")


#* Predict Class
#* @post /iris
function(req, sepal_length, sepal_width, petal_length, petal_width) {

  new_df <- tibble(
    Sepal.Length = sepal_length,
    Sepal.Width = sepal_width,
    Petal.Length = petal_length,
    Petal.Width = petal_width
  )

  pred <- as.character(predict(model, new_df))

  return(list(Sepal.Length = sepal_length,
              Sepal.Width = sepal_width,
              Petal.Length = petal_length,
              Petal.Width = petal_width,
              Spices = pred))

}


#* @get /alive
function(req) {
  return("I'm alive!")
}



#* @get /add/<a>/<b>
function(req, a, b) {
  return(as.numeric(a) + as.numeric(b))
}
