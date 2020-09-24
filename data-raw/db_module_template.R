db_module_template <- readLines('data-raw/templates/template_database_module')
usethis::use_data(db_module_template, overwrite = T)
