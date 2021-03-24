## code to prepare `synPUF` dataset goes here
library(tidyverse)
library(devtools)

# Install WileyLab synPUF package, containing 10 person subset of the CMS 2008-2010 Data Entrepreneurs’ Synthetic Public Use File (DE-SynPUF) from OHDSI in the OMOP v5.2.2 CDM
## https://github.com/thewileylab/synPUF/releases/tag/0.0.1.10
devtools::install_github("thewileylab/synPUF", ref = '0.0.1.10')

# Table Names
table_name <- c('care_site', 'concept', 'concept_class', 'concept_synonym', 'condition_era', 'condition_occurrence', 'death', 'device_exposure', 'domain', 'drug_era', 'drug_exposure', 'drug_strength', 'measurement', 'note', 'observation', 'observation_period', 'payer_plan_period', 'person', 'procedure_occurrence', 'provider', 'relationship', 'visit_occurrence', 'vocabulary')

# Tables
table_data <- list(synPUF::care_site, synPUF::concept, synPUF::concept_class, synPUF::concept_synonym, synPUF::condition_era, synPUF::condition_occurrence, synPUF::death, synPUF::device_exposure, synPUF::domain, synPUF::drug_era, synPUF::drug_exposure, synPUF::drug_strength, synPUF::measurement, synPUF::note, synPUF::observation, synPUF::observation_period, synPUF::payer_plan_period, synPUF::person, synPUF::procedure_occurrence, synPUF::provider, synPUF::relationship, synPUF::visit_occurrence, synPUF::vocabulary)

# Create dataset
synPUF <- tibble(table_name, table_data)
usethis::use_data(synPUF, overwrite = TRUE)
