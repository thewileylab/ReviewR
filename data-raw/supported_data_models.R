## code to prepare `DATASET` dataset goes here

library(tidyverse)
supported_data_models <- list.files(path = file.path('data-raw/data_models'),full.names = T,recursive = T) %>%
  tibble::enframe(name = NULL, value = 'file_path') %>% 
  mutate(data_model = basename(file_path),
         data_model = str_remove_all(data_model, '.csv')
         ) %>% 
  separate(col = data_model, into = c('data_model','model_version'), sep = '_', extra = 'drop', fill = 'right') %>% 
  mutate(model_version = tidyr::replace_na(model_version, ''),
         cdm = map(file_path,
                   ~read_csv(.x)
                   )
         ) %>% 
  unnest(cols = cdm) %>% 
  mutate(joinable_table = tolower(table),
         joinable_field = tolower(field)
         ) %>% 
  group_by(file_path,data_model,model_version) %>% 
  nest() %>% 
  relocate(data_model, model_version, data, file_path)

usethis::use_data(supported_data_models, overwrite = T)
