loadProject <- function(project_id) {
  tryCatch({
    config <- read.csv(file=paste0("./projects/",project_id,"/config.csv"), stringsAsFactors = FALSE)
    config
  }, error=function(c) {NULL})
}

active_cohort_data <- function(project_id, projects) {
  tryCatch({
    cohort <- read.csv(file=paste0("./projects/",project_id,"/",projects[projects$id==project_id,"cohort_file"]), stringsAsFactors = FALSE)
    data <- read.csv(file=paste0("./projects/",project_id,"/",projects[projects$id==project_id,"data_file"]), stringsAsFactors = FALSE)
    cohort %>% left_join(data)
  }, error=function(c) {NULL})
}