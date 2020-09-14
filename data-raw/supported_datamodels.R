## code to prepare `DATASET` dataset goes here

library(tidyverse)
supported_datamodels <- list.files(path = file.path('data-raw/datamodels'),full.names = T,recursive = T) %>%
  tibble::enframe(name = NULL, value = 'file_path') %>% 
  mutate(datamodel = basename(file_path),
         datamodel = str_remove_all(datamodel, '.csv')
         ) %>% 
  separate(col = datamodel, into = c('datamodel','model_version'), sep = '_', extra = 'drop', fill = 'right') %>% 
  mutate(model_version = tidyr::replace_na(model_version, ''),
         cdm = map(file_path,
                   ~read_csv(.x)
                   )
         ) %>% 
  unnest(cols = cdm) %>% 
  mutate(joinable_table = tolower(table),
         joinable_field = tolower(field)
         ) %>% 
  group_by(file_path,datamodel,model_version) %>% 
  nest()

usethis::use_data(supported_datamodels, overwrite = T)
