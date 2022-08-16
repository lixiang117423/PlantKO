# add meta table
meta.table = data.table::fread("./data-raw/信息整合.csv", encoding = "UTF-8") %>%
  dplyr::mutate(Version = as.character(Version))
usethis::use_data(meta.table, overwrite = TRUE)

