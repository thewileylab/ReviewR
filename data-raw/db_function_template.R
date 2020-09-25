# All Patients Table
db_function_all_patients_table_template <- readLines('data-raw/templates/template_all_patients_table')
usethis::use_data(db_function_all_patients_table_template, overwrite = T)

# Subject Table
db_function_subject_table_template <- readLines('data-raw/templates/template_subject_table')
usethis::use_data(db_function_subject_table_template, overwrite = T)
