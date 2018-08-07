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
    cohort %>%
      left_join(data, by="subject_id") %>%
      mutate(subject_id = paste0("<a class='row_subject_id' href='#'>", subject_id, "</a>"))
  }, error=function(c) {NULL})
}

active_abstraction_data <- function(project_id, projects) {
  tryCatch({
    cohort <- read.csv(file=paste0("./projects/",project_id,"/",projects[projects$id==project_id,"cohort_file"]), stringsAsFactors = FALSE)
    data <- read.csv(file=paste0("./projects/",project_id,"/",projects[projects$id==project_id,"data_file"]), stringsAsFactors = FALSE)
    cohort %>%
      left_join(data, by="subject_id")
  }, error=function(c) {NULL})
}