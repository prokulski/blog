library(plumber)

r <- plumb("plumber.R")

r$run(host = "0.0.0.0", port = 8091, swagger = FALSE) # domyślnie host = "127.0.0.1"
