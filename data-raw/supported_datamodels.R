## code to prepare `DATASET` dataset goes here

supported_datamodels <- list.files(path = file.path('data-raw/data_models'),full.names = T,recursive = T) %>% 
  tibble(file_path = .) %>% 
  mutate(data_model = str_extract(string = file_path, pattern = regex('(mimic3)|(omop)',ignore_case = T)),
         data_model = tolower(data_model),
         model_version = basename(file_path),
         model_version = str_replace(string = model_version, pattern = regex(pattern = '(mimic3)(_)?|(omop_cdm_)',ignore_case = T),replacement = ''),
         model_version = str_replace(string = model_version, pattern = regex(pattern = '.csv',ignore_case = T),replacement = ''),
         model_version = tolower(x = model_version),
         cdm = map(.x = file_path,.f = read_csv)
  ) %>% 
  ## Process the supported data models slightly, turning table names and fields to lowercase. Re-group.
  unnest(cols = c(cdm)) %>% 
  mutate(table = tolower(table),
         field = tolower(field)) %>% 
  group_by(file_path,data_model,model_version) %>% 
  nest()

usethis::use_data(supported_datamodels)
