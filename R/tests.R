ll <- load_premap("moz")
ll %>% create_mapfile %>% update_mapfile(statements=empty_tibble)
ll %>% create_mapfile %>% update_mapfile(list(statements=empty_tibble))
