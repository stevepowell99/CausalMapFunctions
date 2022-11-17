## code to prepare `DATASET` dataset goes here

example_file <- load_mapfile("example-file") %>% pipe_coerce_mapfile()
usethis::use_data(example_file,overwrite = T)

example2 <- load_mapfile("example2") %>% pipe_coerce_mapfile()

usethis::use_data(example2,overwrite = T)
