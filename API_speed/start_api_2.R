library(RestRserve)
library(tidyverse)
library(randomForest)
library(jsonlite)

# wczytanie modelu
model <- readRDS("model_rf.RDS")


# API - metody
predict_iris <- function(sepal_length, sepal_width,
                         petal_length, petal_width) {

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


alive <- function() {
  return("I'm alive!")
}

add_floats <- function(a, b) {
  return(as.numeric(a) + as.numeric(b))
}


predict_iris_handler <- function(request, response) {

  iris <- predict_iris(request$body$sepal_length,
                       request$body$sepal_width,
                       request$body$petal_length,
                       request$body$petal_width)

  response$set_body(list("Sepal.Length" = request$body$sepal_length,
         "Sepal.Width" = request$body$sepal_width,
         "Petal.Length" = request$body$petal_length,
         "Petal.Width" = request$body$petal_width,
         "Spices" = iris)
  )
  response$set_content_type("application/json")
}

# API handlers
alive_handler <- function(request, response) {
  response$set_body(as.character(alive()))
  response$set_content_type("text/plain")
}

add_floats_handler <- function(request, response) {

  a <- as.numeric(request$get_param_query("a"))
  b <- as.numeric(request$get_param_query("b"))

  response$set_body(as.character(a+b))
  response$set_content_type("text/plain")
}


app = Application$new()

app$add_get(path = "/alive", FUN = alive_handler)
app$add_post(path = "/iris", FUN = predict_iris_handler)
app$add_get(path = "/add", FUN = add_floats_handler)


backend = BackendRserve$new()

backend$start(app, http_port = 8092)
