# Datasets ----
# Demo data from: https://github.com/thewileylab/synPUF/releases/tag/0.0.1.10
#' care_site
#'
#' @description
#' ## Table Description
#'
#' The CARE_SITE table contains a list of uniquely identified institutional (physical or organizational) units where healthcare delivery is practiced (offices, wards, hospitals, clinics, etc.).
#'
#' ## ETL Conventions
#'
#' Care site is a unique combination of location_id and place_of_service_source_value. Care site does not take into account the provider (human) information such a specialty. Many source data do not make a distinction between individual and institutional providers. The CARE_SITE table contains the institutional providers. If the source, instead of uniquely identifying individual Care Sites, only provides limited information such as Place of Service, generic or “pooled” Care Site records are listed in the CARE_SITE table. There can be hierarchical and business relationships between Care Sites. For example, wards can belong to clinics or departments, which can in turn belong to hospitals, which in turn can belong to hospital systems, which in turn can belong to HMOs.The relationships between Care Sites are defined in the FACT_RELATIONSHIP table.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 0 rows and 6 variables:
#' \describe{
#'   \item{care_site_id}{\emph{character}}
#'   \item{care_site_name}{\emph{character}}
#'   \item{place_of_service_concept_id}{\emph{character}}
#'   \item{location_id}{\emph{character}}
#'   \item{care_site_source_value}{\emph{character}}
#'   \item{place_of_service_source_value}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#care_site }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}

"care_site"
#' concept
#'
#' @description
#' ## Table Description
#'
#' The Standardized Vocabularies contains records, or Concepts, that uniquely identify each fundamental unit of meaning used to express clinical information in all domain tables of the CDM. Concepts are derived from vocabularies, which represent clinical information across a domain (e.g. conditions, drugs, procedures) through the use of codes and associated descriptions. Some Concepts are designated Standard Concepts, meaning these Concepts can be used as normative expressions of a clinical entity within the OMOP Common Data Model and within standardized analytics. Each Standard Concept belongs to one domain, which defines the location where the Concept would be expected to occur within data tables of the CDM.
#'
#' Concepts can represent broad categories (like ‘Cardiovascular disease’), detailed clinical elements (‘Myocardial infarction of the anterolateral wall’) or modifying characteristics and attributes that define Concepts at various levels of detail (severity of a disease, associated morphology, etc.).
#'
#' Records in the Standardized Vocabularies tables are derived from national or international vocabularies such as SNOMED-CT, RxNorm, and LOINC, or custom Concepts defined to cover various aspects of observational data analysis.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 2355 rows and 10 variables:
#' \describe{
#'   \item{concept_name}{\emph{character}}
#'   \item{domain_id}{\emph{character}}
#'   \item{vocabulary_id}{\emph{character}}
#'   \item{concept_class_id}{\emph{character}}
#'   \item{standard_concept}{\emph{character}}
#'   \item{concept_code}{\emph{character}}
#'   \item{valid_start_date}{\emph{Date}}
#'   \item{valid_end_date}{\emph{Date}}
#'   \item{invalid_reason}{\emph{character}}
#'   \item{concept_id}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#concept }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"concept"
#' concept_class
#'
#' @description
#' ## Table Description
#'
#' The CONCEPT_CLASS table is a reference table, which includes a list of the classifications used to differentiate Concepts within a given Vocabulary. This reference table is populated with a single record for each Concept Class.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 0 rows and 3 variables:
#' \describe{
#'   \item{concept_class_id}{\emph{character}}
#'   \item{concept_class_name}{\emph{character}}
#'   \item{concept_class_concept_id}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#concept_class }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"concept_class"
#' concept_synonym
#'
#' @description
#' ## Table Description
#'
#' The CONCEPT_SYNONYM table is used to store alternate names and descriptions for Concepts.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 5835 rows and 2 variables:
#' \describe{
#'   \item{concept_synonym_name}{\emph{character}}
#'   \item{concept_id}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#concept_synonym }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"concept_synonym"
#' condition_era
#'
#' @description
#' ## Table Description
#'
#' A Condition Era is defined as a span of time when the Person is assumed to have a given condition. Similar to Drug Eras, Condition Eras are chronological periods of Condition Occurrence. Combining individual Condition Occurrences into a single Condition Era serves two purposes:
#' \itemize{
#'   \item{It allows aggregation of chronic conditions that require frequent ongoing care, instead of treating each Condition Occurrence as an independent event.}
#'   \item{It allows aggregation of multiple, closely timed doctor visits for the same Condition to avoid double-counting the Condition Occurrences. For example, consider a Person who visits her Primary Care Physician (PCP) and who is referred to a specialist. At a later time, the Person visits the specialist, who confirms the PCP’s original diagnosis and provides the appropriate treatment to resolve the condition. These two independent doctor visits should be aggregated into one Condition Era.}
#' }
#'
#' ## ETL Conventions
#'
#' Each Condition Era corresponds to one or many Condition Occurrence records that form a continuous interval. The condition_concept_id field contains Concepts that are identical to those of the CONDITION_OCCURRENCE table records that make up the Condition Era. In contrast to Drug Eras, Condition Eras are not aggregated to contain Conditions of different hierarchical layers. The SQl Script for generating CONDITION_ERA records can be found here The Condition Era Start Date is the start date of the first Condition Occurrence. The Condition Era End Date is the end date of the last Condition Occurrence. Condition Eras are built with a Persistence Window of 30 days, meaning, if no occurrence of the same condition_concept_id happens within 30 days of any one occurrence, it will be considered the condition_era_end_date.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 688 rows and 6 variables:
#' \describe{
#'   \item{condition_era_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{condition_concept_id}{\emph{integer}}
#'   \item{condition_era_start_date}{\emph{Date}}
#'   \item{condition_era_end_date}{\emph{Date}}
#'   \item{condition_occurrence_count}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#condition_era }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"condition_era"
#' condition_occurrence
#'
#' @description
#' ## Table Description
#'
#' This table contains records of Events of a Person suggesting the presence of a disease or medical condition stated as a diagnosis, a sign, or a symptom, which is either observed by a Provider or reported by the patient.
#'
#' ## User Guide
#'
#' Conditions are defined by Concepts from the Condition domain, which form a complex hierarchy. As a result, the same Person with the same disease may have multiple Condition records, which belong to the same hierarchical family. Most Condition records are mapped from diagnostic codes, but recorded signs, symptoms and summary descriptions also contribute to this table. Rule out diagnoses should not be recorded in this table, but in reality their negating nature is not always captured in the source data, and other precautions must be taken when when identifying Persons who should suffer from the recorded Condition. Record all conditions as they exist in the source data. Any decisions about diagnosis/phenotype definitions would be done through cohort specifications. These cohorts can be housed in the COHORT table. Conditions span a time interval from start to end, but are typically recorded as single snapshot records with no end date. The reason is twofold: (i) At the time of the recording the duration is not known and later not recorded, and (ii) the Persons typically cease interacting with the healthcare system when they feel better, which leads to incomplete capture of resolved Conditions. The CONDITION_ERA table addresses this issue. Family history and past diagnoses (‘history of’) are not recorded in this table. Instead, they are listed in the OBSERVATION table. Codes written in the process of establishing the diagnosis, such as ‘question of’ of and ‘rule out’, should not represented here. Instead, they should be recorded in the OBSERVATION table, if they are used for analyses. However, this information is not always available.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 992 rows and 15 variables:
#' \describe{
#'   \item{condition_occurrence_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{condition_concept_id}{\emph{integer}}
#'   \item{condition_start_date}{\emph{Date}}
#'   \item{condition_start_datetime}{\emph{POSIXct}}
#'   \item{condition_end_date}{\emph{Date}}
#'   \item{condition_end_datetime}{\emph{POSIXct}}
#'   \item{condition_type_concept_id}{\emph{integer}}
#'   \item{stop_reason}{\emph{character}}
#'   \item{provider_id}{\emph{integer}}
#'   \item{visit_occurrence_id}{\emph{integer}}
#'   \item{condition_source_value}{\emph{character}}
#'   \item{condition_source_concept_id}{\emph{integer}}
#'   \item{condition_status_source_value}{\emph{character}}
#'   \item{condition_status_concept_id}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#condition_occurrence }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"condition_occurrence"
#' death
#'
#' @description
#' ## Table Description
#'
#' The death domain contains the clinical event for how and when a Person dies. A person can have up to one record if the source system contains evidence about the Death, such as: Condition in an administrative claim, status of enrollment into a health plan, or explicit record in EHR data.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 2 rows and 7 variables:
#' \describe{
#'   \item{person_id}{\emph{integer}}
#'   \item{death_date}{\emph{Date}}
#'   \item{death_datetime}{\emph{POSIXct}}
#'   \item{death_type_concept_id}{\emph{integer}}
#'   \item{cause_concept_id}{\emph{integer}}
#'   \item{cause_source_value}{\emph{character}}
#'   \item{cause_source_concept_id}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#death }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"death"
#' device_exposure
#'
#' @description
#' ## Table Description
#'
#' The Device domain captures information about a person’s exposure to a foreign physical object or instrument which is used for diagnostic or therapeutic purposes through a mechanism beyond chemical action. Devices include implantable objects (e.g. pacemakers, stents, artificial joints), medical equipment and supplies (e.g. bandages, crutches, syringes), other instruments used in medical procedures (e.g. sutures, defibrillators) and material used in clinical care (e.g. adhesives, body material, dental material, surgical material).
#'
#' ## User Guide
#'
#' The distinction between Devices or supplies and Procedures are sometimes blurry, but the former are physical objects while the latter are actions, often to apply a Device or supply.
#'
#' ## ETL Conventions
#'
#' Source codes and source text fields mapped to Standard Concepts of the Device Domain have to be recorded here.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 4 rows and 14 variables:
#' \describe{
#'   \item{device_exposure_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{device_concept_id}{\emph{integer}}
#'   \item{device_exposure_start_date}{\emph{Date}}
#'   \item{device_exposure_start_datetime}{\emph{POSIXct}}
#'   \item{device_exposure_end_date}{\emph{Date}}
#'   \item{device_exposure_end_datetime}{\emph{POSIXct}}
#'   \item{device_type_concept_id}{\emph{integer}}
#'   \item{unique_device_id}{\emph{character}}
#'   \item{quantity}{\emph{integer}}
#'   \item{provider_id}{\emph{integer}}
#'   \item{visit_occurrence_id}{\emph{integer}}
#'   \item{device_source_value}{\emph{character}}
#'   \item{device_source_concept_id}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#device_exposure }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"device_exposure"
#' domain
#'
#' @description
#' ## Table Description
#'
#' The DOMAIN table includes a list of OMOP-defined Domains the Concepts of the Standardized Vocabularies can belong to. A Domain defines the set of allowable Concepts for the standardized fields in the CDM tables. For example, the “Condition” Domain contains Concepts that describe a condition of a patient, and these Concepts can only be stored in the condition_concept_id field of the CONDITION_OCCURRENCE and CONDITION_ERA tables. This reference table is populated with a single record for each Domain and includes a descriptive name for the Domain.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 0 rows and 3 variables:
#' \describe{
#'   \item{domain_id}{\emph{character}}
#'   \item{domain_name}{\emph{character}}
#'   \item{domain_concept_id}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#domain }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"domain"
#' drug_era
#'
#' @description
#' ## Table Description
#'
#' A Drug Era is defined as a span of time when the Person is assumed to be exposed to a particular active ingredient. A Drug Era is not the same as a Drug Exposure: Exposures are individual records corresponding to the source when Drug was delivered to the Person, while successive periods of Drug Exposures are combined under certain rules to produce continuous Drug Eras.
#'
#' ## ETL Conventions
#'
#'The SQL script for generating DRUG_ERA records can be found here.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 344 rows and 7 variables:
#' \describe{
#'   \item{drug_era_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{drug_concept_id}{\emph{integer}}
#'   \item{drug_era_start_date}{\emph{Date}}
#'   \item{drug_era_end_date}{\emph{Date}}
#'   \item{drug_exposure_count}{\emph{integer}}
#'   \item{gap_days}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#drug_era }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"drug_era"
#' drug_exposure
#'
#' @description
#' ## Table Description
#'
#' The Device domain captures information about a person’s exposure to a foreign physical object or instrument which is used for diagnostic or therapeutic purposes through a mechanism beyond chemical action. Devices include implantable objects (e.g. pacemakers, stents, artificial joints), medical equipment and supplies (e.g. bandages, crutches, syringes), other instruments used in medical procedures (e.g. sutures, defibrillators) and material used in clinical care (e.g. adhesives, body material, dental material, surgical material).
#'
#' ## User Guide
#'
#' The distinction between Devices or supplies and Procedures are sometimes blurry, but the former are physical objects while the latter are actions, often to apply a Device or supply.
#'
#' ## ETL Conventions
#'
#' Source codes and source text fields mapped to Standard Concepts of the Device Domain have to be recorded here.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 363 rows and 22 variables:
#' \describe{
#'   \item{drug_exposure_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{drug_concept_id}{\emph{integer}}
#'   \item{drug_exposure_start_date}{\emph{Date}}
#'   \item{drug_exposure_start_datetime}{\emph{POSIXct}}
#'   \item{drug_exposure_end_date}{\emph{Date}}
#'   \item{drug_exposure_end_datetime}{\emph{POSIXct}}
#'   \item{verbatim_end_date}{\emph{Date}}
#'   \item{drug_type_concept_id}{\emph{integer}}
#'   \item{stop_reason}{\emph{character}}
#'   \item{refills}{\emph{integer}}
#'   \item{quantity}{\emph{numeric}}
#'   \item{days_supply}{\emph{integer}}
#'   \item{sig}{\emph{character}}
#'   \item{route_concept_id}{\emph{integer}}
#'   \item{lot_number}{\emph{character}}
#'   \item{provider_id}{\emph{integer}}
#'   \item{visit_occurrence_id}{\emph{integer}}
#'   \item{drug_source_value}{\emph{character}}
#'   \item{drug_source_concept_id}{\emph{integer}}
#'   \item{route_source_value}{\emph{character}}
#'   \item{dose_unit_source_value}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#drug_exposure }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"drug_exposure"
#' drug_strength
#'
#' @description
#' ## Table Description
#'
#' The DRUG_STRENGTH table contains structured content about the amount or concentration and associated units of a specific ingredient contained within a particular drug product. This table is supplemental information to support standardized analysis of drug utilization.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 475035 rows and 9 variables:
#' \describe{
#'   \item{amount_value}{\emph{numeric}}
#'   \item{numerator_value}{\emph{numeric}}
#'   \item{denominator_value}{\emph{numeric}}
#'   \item{box_size}{\emph{integer}}
#'   \item{valid_start_date}{\emph{Date}}
#'   \item{valid_end_date}{\emph{Date}}
#'   \item{invalid_reason}{\emph{character}}
#'   \item{ingredient_concept_id}{\emph{integer}}
#'   \item{drug_concept_id}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#drug_strength }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"drug_strength"
#' measurement
#'
#' @description
#' ## Table Description
#'
#' The MEASUREMENT table contains records of Measurements, i.e. structured values (numerical or categorical) obtained through systematic and standardized examination or testing of a Person or Person’s sample. The MEASUREMENT table contains both orders and results of such Measurements as laboratory tests, vital signs, quantitative findings from pathology reports, etc. Measurements are stored as attribute value pairs, with the attribute as the Measurement Concept and the value representing the result. The value can be a Concept (stored in VALUE_AS_CONCEPT), or a numerical value (VALUE_AS_NUMBER) with a Unit (UNIT_CONCEPT_ID). The Procedure for obtaining the sample is housed in the PROCEDURE_OCCURRENCE table, though it is unnecessary to create a PROCEDURE_OCCURRENCE record for each measurement if one does not exist in the source data. Measurements differ from Observations in that they require a standardized test or some other activity to generate a quantitative or qualitative result. If there is no result, it is assumed that the lab test was conducted but the result was not captured.
#'
#' ## User Guide
#'
#' Measurements are predominately lab tests with a few exceptions, like blood pressure or function tests. Results are given in the form of a value and unit combination. When investigating measurements, look for operator_concept_ids (<, >, etc.).
#'
#' ## ETL Conventions
#'
#' Only records where the source value maps to a Concept in the measurement domain should be included in this table. Even though each Measurement always has a result, the fields VALUE_AS_NUMBER and VALUE_AS_CONCEPT_ID are not mandatory as often the result is not given in the source data. When the result is not known, the Measurement record represents just the fact that the corresponding Measurement was carried out, which in itself is already useful information for some use cases. For some Measurement Concepts, the result is included in the test. For example, ICD10 CONCEPT_ID 45548980 ‘Abnormal level of unspecified serum enzyme’ indicates a Measurement and the result (abnormal). In those situations, the CONCEPT_RELATIONSHIP table in addition to the ‘Maps to’ record contains a second record with the relationship_id set to ‘Maps to value’. In this example, the ‘Maps to’ relationship directs to 4046263 ‘Enzyme measurement’ as well as a ‘Maps to value’ record to 4135493 ‘Abnormal’.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 194 rows and 18 variables:
#' \describe{
#'   \item{measurement_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{measurement_concept_id}{\emph{integer}}
#'   \item{measurement_date}{\emph{Date}}
#'   \item{measurement_datetime}{\emph{POSIXct}}
#'   \item{measurement_type_concept_id}{\emph{integer}}
#'   \item{operator_concept_id}{\emph{integer}}
#'   \item{value_as_number}{\emph{numeric}}
#'   \item{value_as_concept_id}{\emph{integer}}
#'   \item{unit_concept_id}{\emph{integer}}
#'   \item{range_low}{\emph{numeric}}
#'   \item{range_high}{\emph{numeric}}
#'   \item{provider_id}{\emph{integer}}
#'   \item{visit_occurrence_id}{\emph{integer}}
#'   \item{measurement_source_value}{\emph{character}}
#'   \item{measurement_source_concept_id}{\emph{integer}}
#'   \item{unit_source_value}{\emph{character}}
#'   \item{value_source_value}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#measurement }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"measurement"
#' note
#'
#' @description
#' ## Table Description
#'
#' The NOTE table captures unstructured information that was recorded by a provider about a patient in free text (in ASCII, or preferably in UTF8 format) notes on a given date. The type of note_text is CLOB or varchar(MAX) depending on RDBMS.
#'
#' ## ETL Conventions
#'
#' HL7/LOINC CDO is a standard for consistent naming of documents to support a range of use cases: retrieval, organization, display, and exchange. It guides the creation of LOINC codes for clinical notes. CDO annotates each document with 5 dimensions:
#' \itemize{
#'   \item{Kind of Document: Characterizes the general structure of the document at a macro level (e.g. Anesthesia Consent)}
#'   \item{Type of Service: Characterizes the kind of service or activity (e.g. evaluations, consultations, and summaries). The notion of time sequence, e.g., at the beginning (admission) at the end (discharge) is subsumed in this axis. Example: Discharge Teaching.}
#'   \item{Setting: Setting is an extension of CMS’s definitions (e.g. Inpatient, Outpatient)}
#'   \item{Subject Matter Domain (SMD): Characterizes the subject matter domain of a note (e.g. Anesthesiology)}
#'   \item{Role: Characterizes the training or professional level of the author of the document, but does not break down to specialty or subspecialty (e.g. Physician) Each combination of these 5 dimensions rolls up to a unique LOINC code.}
#' }
#'
#' According to CDO requirements, only 2 of the 5 dimensions are required to properly annotate a document; Kind of Document and any one of the other 4 dimensions. However, not all the permutations of the CDO dimensions will necessarily yield an existing LOINC code. Each of these dimensions are contained in the OMOP Vocabulary under the domain of ‘Meas Value’ with each dimension represented as a Concept Class.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 851 rows and 13 variables:
#' \describe{
#'   \item{note_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{note_date}{\emph{Date}}
#'   \item{note_datetime}{\emph{POSIXct}}
#'   \item{note_type_concept_id}{\emph{integer}}
#'   \item{note_class_concept_id}{\emph{integer}}
#'   \item{note_title}{\emph{character}}
#'   \item{note_text}{\emph{character}}
#'   \item{encoding_concept_id}{\emph{integer}}
#'   \item{language_concept_id}{\emph{integer}}
#'   \item{provider_id}{\emph{integer}}
#'   \item{visit_occurrence_id}{\emph{integer}}
#'   \item{note_source_value}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#note }
#' @source \url{https://www.mtsamples.com/ }
"note"
#' observation
#'
#' @description
#' ## Table Description
#'
#' The OBSERVATION table captures clinical facts about a Person obtained in the context of examination, questioning or a procedure. Any data that cannot be represented by any other domains, such as social and lifestyle facts, medical history, family history, etc. are recorded here.
#'
#' ## User Guide
#'
#' Observations differ from Measurements in that they do not require a standardized test or some other activity to generate clinical fact. Typical observations are medical history, family history, the stated need for certain treatment, social circumstances, lifestyle choices, healthcare utilization patterns, etc. If the generation clinical facts requires a standardized testing such as lab testing or imaging and leads to a standardized result, the data item is recorded in the MEASUREMENT table. If the clinical fact observed determines a sign, symptom, diagnosis of a disease or other medical condition, it is recorded in the CONDITION_OCCURRENCE table. Valid Observation Concepts are not enforced to be from any domain though they still should be Standard Concepts.
#'
#' ## ETL Conventions
#'
#' Records whose Source Values map to any domain besides Condition, Procedure, Drug, Measurement or Device should be stored in the Observation table. Observations can be stored as attribute value pairs, with the attribute as the Observation Concept and the value representing the clinical fact. This fact can be a Concept (stored in VALUE_AS_CONCEPT), a numerical value (VALUE_AS_NUMBER), a verbatim string (VALUE_AS_STRING), or a datetime (VALUE_AS_DATETIME). Even though Observations do not have an explicit result, the clinical fact can be stated separately from the type of Observation in the VALUE_AS_* fields. It is recommended for Observations that are suggestive statements of positive assertion should have a value of ‘Yes’ (concept_id=4188539), recorded, even though the null value is the equivalent.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 184 rows and 17 variables:
#' \describe{
#'   \item{observation_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{observation_concept_id}{\emph{integer}}
#'   \item{observation_date}{\emph{Date}}
#'   \item{observation_datetime}{\emph{POSIXct}}
#'   \item{observation_type_concept_id}{\emph{integer}}
#'   \item{value_as_number}{\emph{numeric}}
#'   \item{value_as_string}{\emph{character}}
#'   \item{value_as_concept_id}{\emph{integer}}
#'   \item{qualifier_concept_id}{\emph{integer}}
#'   \item{unit_concept_id}{\emph{integer}}
#'   \item{provider_id}{\emph{integer}}
#'   \item{visit_occurrence_id}{\emph{integer}}
#'   \item{observation_source_value}{\emph{character}}
#'   \item{observation_source_concept_id}{\emph{integer}}
#'   \item{unit_source_value}{\emph{character}}
#'   \item{qualifier_source_value}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#observation }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"observation"
#' observation_period
#'
#' @description
#' ## Table Description
#'
#' This table contains records which define spans of time during which two conditions are expected to hold: (i) Clinical Events that happened to the Person are recorded in the Event tables, and (ii) absence of records indicate such Events did not occur during this span of time.
#'
#' ## User Guide
#'
#' For each Person, one or more OBSERVATION_PERIOD records may be present, but they will not overlap or be back to back to each other. Events may exist outside all of the time spans of the OBSERVATION_PERIOD records for a patient, however, absence of an Event outside these time spans cannot be construed as evidence of absence of an Event. Incidence or prevalence rates should only be calculated for the time of active OBSERVATION_PERIOD records. When constructing cohorts, outside Events can be used for inclusion criteria definition, but without any guarantee for the performance of these criteria. Also, OBSERVATION_PERIOD records can be as short as a single day, greatly disturbing the denominator of any rate calculation as part of cohort characterizations. To avoid that, apply minimal observation time as a requirement for any cohort definition.
#'
#' ## ETL Conventions
#'
#' Each Person needs to have at least one OBSERVATION_PERIOD record, which should represent time intervals with a high capture rate of Clinical Events. Some source data have very similar concepts, such as enrollment periods in insurance claims data. In other source data such as most EHR systems these time spans need to be inferred under a set of assumptions. It is the discretion of the ETL developer to define these assumptions. In many ETL solutions the start date of the first occurrence or the first high quality occurrence of a Clinical Event (Condition, Drug, Procedure, Device, Measurement, Visit) is defined as the start of the OBSERVATION_PERIOD record, and the end date of the last occurrence of last high quality occurrence of a Clinical Event, or the end of the database period becomes the end of the OBSERVATION_PERIOD for each Person. If a Person only has a single Clinical Event the OBSERVATION_PERIOD record can be as short as one day. Depending on these definitions it is possible that Clinical Events fall outside the time spans defined by OBSERVATION_PERIOD records. Family history or history of Clinical Events generally are not used to generate OBSERVATION_PERIOD records around the time they are referring to. Any two overlapping or adjacent OBSERVATION_PERIOD records have to be merged into one.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 8 rows and 5 variables:
#' \describe{
#'   \item{observation_period_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{observation_period_start_date}{\emph{Date}}
#'   \item{observation_period_end_date}{\emph{Date}}
#'   \item{period_type_concept_id}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#observation_period }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"observation_period"
#' payer_plan_period
#'
#' @description
#' ## Table Description
#'
#' The PAYER_PLAN_PERIOD table captures details of the period of time that a Person is continuously enrolled under a specific health Plan benefit structure from a given Payer. Each Person receiving healthcare is typically covered by a health benefit plan, which pays for (fully or partially), or directly provides, the care. These benefit plans are provided by payers, such as health insurances or state or government agencies. In each plan the details of the health benefits are defined for the Person or her family, and the health benefit Plan might change over time typically with increasing utilization (reaching certain cost thresholds such as deductibles), plan availability and purchasing choices of the Person. The unique combinations of Payer organizations, health benefit Plans and time periods in which they are valid for a Person are recorded in this table.
#'
#' ## User Guide
#'
#' A Person can have multiple, overlapping, Payer_Plan_Periods in this table. For example, medical and drug coverage in the US can be represented by two Payer_Plan_Periods. The details of the benefit structure of the Plan is rarely known, the idea is just to identify that the Plans are different.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 33 rows and 7 variables:
#' \describe{
#'   \item{payer_plan_period_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{payer_plan_period_start_date}{\emph{Date}}
#'   \item{payer_plan_period_end_date}{\emph{Date}}
#'   \item{payer_source_value}{\emph{character}}
#'   \item{plan_source_value}{\emph{character}}
#'   \item{family_source_value}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#payer_plan_period }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"payer_plan_period"
#' person
#'
#' @description
#' ## Table Description
#'
#' This table serves as the central identity management for all Persons in the database. It contains records that uniquely identify each person or patient, and some demographic information.
#'
#' ## User Guide
#'
#' All records in this table are independent Persons.
#'
#' ## ETL Conventions
#'
#' All Persons in a database needs one record in this table, unless they fail data quality requirements specified in the ETL. Persons with no Events should have a record nonetheless. If more than one data source contributes Events to the database, Persons must be reconciled, if possible, across the sources to create one single record per Person. The content of the BIRTH_DATETIME must be equivalent to the content of BIRTH_DAY, BIRTH_MONTH and BIRTH_YEAR.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 10 rows and 18 variables:
#' \describe{
#'   \item{person_id}{\emph{integer}}
#'   \item{gender_concept_id}{\emph{integer}}
#'   \item{year_of_birth}{\emph{integer}}
#'   \item{month_of_birth}{\emph{integer}}
#'   \item{day_of_birth}{\emph{integer}}
#'   \item{birth_datetime}{\emph{POSIXct}}
#'   \item{race_concept_id}{\emph{integer}}
#'   \item{ethnicity_concept_id}{\emph{integer}}
#'   \item{location_id}{\emph{integer}}
#'   \item{provider_id}{\emph{integer}}
#'   \item{care_site_id}{\emph{integer}}
#'   \item{person_source_value}{\emph{character}}
#'   \item{gender_source_value}{\emph{character}}
#'   \item{gender_source_concept_id}{\emph{integer}}
#'   \item{race_source_value}{\emph{character}}
#'   \item{race_source_concept_id}{\emph{integer}}
#'   \item{ethnicity_source_value}{\emph{character}}
#'   \item{ethnicity_source_concept_id}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#person }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"person"
#' procedure_occurrence
#'
#' @description
#' ## Table Description
#'
#' This table contains records of activities or processes ordered by, or carried out by, a healthcare provider on the patient with a diagnostic or therapeutic purpose.
#'
#' ## User Guide
#'
#' Lab tests are not a procedure, if something is observed with an expected resulting amount and unit then it should be a measurement. Phlebotomy is a procedure but so trivial that it tends to be rarely captured. It can be assumed that there is a phlebotomy procedure associated with many lab tests, therefore it is unnecessary to add them as separate procedures. If the user finds the same procedure over concurrent days, it is assumed those records are part of a procedure lasting more than a day. This logic is in lieu of the procedure_end_date, which will be added in a future version of the CDM.
#'
#' ## ETL Conventions
#'
#' If a procedure lasts more than a day, then it should be recorded as a separate record for each day the procedure occurred, this logic is in lieu of the PROCEDURE_END_DATE, which will be added in a future version of the CDM. When dealing with duplicate records, the ETL must determine whether to sum them up into one record or keep them separate. Things to consider are: - Same Procedure - Same PROCEDURE_DATETIME - Same Visit Occurrence or Visit Detail - Same Provider - Same Modifier for Procedures. Source codes and source text fields mapped to Standard Concepts of the Procedure Domain have to be recorded here.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 987 rows and 13 variables:
#' \describe{
#'   \item{procedure_occurrence_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{procedure_concept_id}{\emph{integer}}
#'   \item{procedure_date}{\emph{Date}}
#'   \item{procedure_datetime}{\emph{POSIXct}}
#'   \item{procedure_type_concept_id}{\emph{integer}}
#'   \item{modifier_concept_id}{\emph{integer}}
#'   \item{quantity}{\emph{integer}}
#'   \item{provider_id}{\emph{integer}}
#'   \item{visit_occurrence_id}{\emph{integer}}
#'   \item{procedure_source_value}{\emph{character}}
#'   \item{procedure_source_concept_id}{\emph{integer}}
#'   \item{qualifier_source_value}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#procedure_occurrence }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"procedure_occurrence"
#' provider
#'
#' @description
#' ## Table Description
#'
#' The PROVIDER table contains a list of uniquely identified healthcare providers. These are individuals providing hands-on healthcare to patients, such as physicians, nurses, midwives, physical therapists etc.
#'
#' ## User Guide
#'
#' Many sources do not make a distinction between individual and institutional providers. The PROVIDER table contains the individual providers. If the source, instead of uniquely identifying individual providers, only provides limited information such as specialty, generic or ‘pooled’ Provider records are listed in the PROVIDER table.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 0 rows and 13 variables:
#' \describe{
#'   \item{provider_id}{\emph{character}}
#'   \item{provider_name}{\emph{character}}
#'   \item{NPI}{\emph{character}}
#'   \item{DEA}{\emph{character}}
#'   \item{specialty_concept_id}{\emph{character}}
#'   \item{care_site_id}{\emph{character}}
#'   \item{year_of_birth}{\emph{character}}
#'   \item{gender_concept_id}{\emph{character}}
#'   \item{provider_source_value}{\emph{character}}
#'   \item{specialty_source_value}{\emph{character}}
#'   \item{specialty_source_concept_id}{\emph{character}}
#'   \item{gender_source_value}{\emph{character}}
#'   \item{gender_source_concept_id}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#provider }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"provider"
#' relationship
#'
#' @description
#' ## Table Description
#'
#' The RELATIONSHIP table provides a reference list of all types of relationships that can be used to associate any two concepts in the CONCEPT_RELATIONSHIP table.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 0 rows and 6 variables:
#' \describe{
#'   \item{relationship_id}{\emph{character}}
#'   \item{relationship_name}{\emph{character}}
#'   \item{is_hierarchical}{\emph{character}}
#'   \item{defines_ancestry}{\emph{character}}
#'   \item{reverse_relationship_id}{\emph{character}}
#'   \item{relationship_concept_id}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#relationship }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"relationship"
#' visit_occurrence
#'
#' @description
#' ## Table Description
#'
#' This table contains Events where Persons engage with the healthcare system for a duration of time. They are often also called “Encounters”. Visits are defined by a configuration of circumstances under which they occur, such as (i) whether the patient comes to a healthcare institution, the other way around, or the interaction is remote, (ii) whether and what kind of trained medical staff is delivering the service during the Visit, and (iii) whether the Visit is transient or for a longer period involving a stay in bed.
#'
#' ## User Guide
#'
#' The configuration defining the Visit are described by Concepts in the Visit Domain, which form a hierarchical structure, but rolling up to generally familiar Visits adopted in most healthcare systems worldwide:
#'
#' \itemize{
#'   \item{Inpatient Visit: Person visiting hospital, at a Care Site, in bed, for duration of more than one day, with physicians and other Providers permanently available to deliver service around the clock}
#'   \item{Emergency Room Visit: Person visiting dedicated healthcare institution for treating emergencies, at a Care Site, within one day, with physicians and Providers permanently available to deliver service around the clock}
#'   \item{Emergency Room and Inpatient Visit: Person visiting ER followed by a subsequent Inpatient Visit, where Emergency department is part of hospital, and transition from the ER to other hospital departments is undefined}
#'   \item{Non-hospital institution Visit: Person visiting dedicated institution for reasons of poor health, at a Care Site, long-term or permanently, with no physician but possibly other Providers permanently available to deliver service around the clock}
#'   \item{Outpatient Visit: Person visiting dedicated ambulatory healthcare institution, at a Care Site, within one day, without bed, with physicians or medical Providers delivering service during Visit}
#'   \item{Home Visit: Provider visiting Person, without a Care Site, within one day, delivering service}
#'   \item{Telehealth Visit: Patient engages with Provider through communication media}
#'   \item{Pharmacy Visit: Person visiting pharmacy for dispensing of Drug, at a Care Site, within one day}
#'   \item{Laboratory Visit: Patient visiting dedicated institution, at a Care Site, within one day, for the purpose of a Measurement.}
#'   \item{Ambulance Visit: Person using transportation service for the purpose of initiating one of the other Visits, without a Care Site, within one day, potentially with Providers accompanying the Visit and delivering service}
#'   \item{Case Management Visit: Person interacting with healthcare system, without a Care Site, within a day, with no Providers involved, for administrative purposes}
#' }
#'
#' The Visit duration, or ‘length of stay’, is defined as VISIT_END_DATE - VISIT_START_DATE. For all Visits this is <1 day, except Inpatient Visits and Non-hospital institution Visits. The CDM also contains the VISIT_DETAIL table where additional information about the Visit is stored, for example, transfers between units during an inpatient Visit.
#'
#' ## ETL Conventions
#'
#' Visits can be derived easily if the source data contain coding systems for Place of Service or Procedures, like CPT codes for well visits. In those cases, the codes can be looked up and mapped to a Standard Visit Concept. Otherwise, Visit Concepts have to be identified in the ETL process. This table will contain concepts in the Visit domain. These concepts are arranged in a hierarchical structure to facilitate cohort definitions by rolling up to generally familiar Visits adopted in most healthcare systems worldwide. Visits can be adjacent to each other, i.e. the end date of one can be identical with the start date of the other. As a consequence, more than one-day Visits or their descendants can be recorded for the same day. Multi-day visits must not overlap, i.e. share days other than start and end days. It is often the case that some logic should be written for how to define visits and how to assign Visit_Concept_Id. For example, in US claims outpatient visits that appear to occur within the time period of an inpatient visit can be rolled into one with the same Visit_Occurrence_Id. In EHR data inpatient visits that are within one day of each other may be strung together to create one visit. It will all depend on the source data and how encounter records should be translated to visit occurrences. Providers can be associated with a Visit through the PROVIDER_ID field, or indirectly through PROCEDURE_OCCURRENCE records linked both to the VISIT and PROVIDER tables.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 409 rows and 17 variables:
#' \describe{
#'   \item{visit_occurrence_id}{\emph{integer}}
#'   \item{person_id}{\emph{integer}}
#'   \item{visit_concept_id}{\emph{integer}}
#'   \item{visit_start_date}{\emph{Date}}
#'   \item{visit_start_datetime}{\emph{POSIXct}}
#'   \item{visit_end_date}{\emph{Date}}
#'   \item{visit_end_datetime}{\emph{POSIXct}}
#'   \item{visit_type_concept_id}{\emph{integer}}
#'   \item{provider_id}{\emph{integer}}
#'   \item{care_site_id}{\emph{integer}}
#'   \item{visit_source_value}{\emph{character}}
#'   \item{visit_source_concept_id}{\emph{integer}}
#'   \item{admitting_source_concept_id}{\emph{integer}}
#'   \item{admitting_source_value}{\emph{character}}
#'   \item{discharge_to_concept_id}{\emph{integer}}
#'   \item{discharge_to_source_value}{\emph{character}}
#'   \item{preceding_visit_occurrence_id}{\emph{integer}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#visit_occurrence }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"visit_occurrence"
#' vocabulary
#'
#' @description
#' ## Table Description
#'
#' The VOCABULARY table includes a list of the Vocabularies collected from various sources or created de novo by the OMOP community. This reference table is populated with a single record for each Vocabulary source and includes a descriptive name and other associated attributes for the Vocabulary.
#'
#' @docType data
#' @keywords internal
#' 
#'
#' @format A data frame with 0 rows and 5 variables:
#' \describe{
#'   \item{vocabulary_id}{\emph{character}}
#'   \item{vocabulary_name}{\emph{character}}
#'   \item{vocabulary_reference}{\emph{character}}
#'   \item{vocabulary_version}{\emph{character}}
#'   \item{vocabulary_concept_id}{\emph{character}}
#'   ...
#' }
#' @source \url{https://ohdsi.github.io/CommonDataModel/cdm531.html#vocabulary }
#' @source \url{https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF}
"vocabulary"

# UI ----
#' Demo SQLite Setup UI
#'
#' This module is designed to guide a user through the process of authenticating your database
#' 
#' @param id The module namespace
#' 
#' @return The Demo SQLite Setup UI
#' @keywords internal
#' @export
#' 
#' @importFrom shiny NS
#' @importFrom shinyjs hidden
#'
demo_sqlite_setup_ui <- function(id) { 
  ns <- NS(id)
  tagList(
    div(id = ns('demodb_setup'),
      HTML('Connect to a demonstration SQLite database containing synPUF data.'),
      br(),
      br(),
      actionButton(inputId = ns('demodb_connect'), label = 'Connect', icon = icon(name = 'database')),
      ),
    hidden(
      div(id = ns('demodb_connected'),
          h3('Success!'),
          HTML("This demonstration database module contains a 10 person subset of the CMS 2008-2010 Data Entrepreneurs' Synthetic Public Use File (DE-SynPUF) from OHDSI."),
          h4('Details'),
          HTML('<ul>
                <li><a href="https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/DE_Syn_PUF.html" target="_blank" rel="noopener noreferrer">SynPUF Dataset Information</a></li>
                <li><a href="https://github.com/OHDSI/CommonDataModel/blob/v5.2.2/PostgreSQL/OMOP%20CDM%20ddl%20-%20PostgreSQL.sql" target="_blank" rel="noopener noreferrer">OMOP CDM 5.2.2 DDL for OHDSI supported DBMSs</a></li>
                <li><a href="https://www.mtsamples.com/" target="_blank" rel="noopener noreferrer">Notes Obtained from MTSamples.com</a></li>
                <li><a href="https://github.com/thewileylab/synPUF" target="_blank" rel="noopener noreferrer">R Dataset ETL Process</a></li>
                </ul>'
               ),
          br(),
          actionButton(inputId = ns('demodb_disconnect'), label = 'Disconnect')
          )
      )
    )
  }

# Server ----
#' Demo SQLite Setup Server
#'
#' @param id The Module namespace
#'
#' @return Demo SQLite connection variables
#' @keywords internal
#' @export
#'
#' @importFrom DBI dbConnect
#' @importFrom RSQLite dbDisconnect dbWriteTable SQLite
#' @importFrom shinyjs hide show
#' 
demo_sqlite_setup_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ## demo_sqlite Export Values ----
      demo_sqlite_export <- reactiveValues(
        ### Module Info
        moduleName = 'Demo SQLite',
        moduleType = 'database',
        setup_ui = ReviewR::demo_sqlite_setup_ui,
        is_connected = 'no',       
        db_con = NULL
        )
      # Server Code Here ----
      observeEvent(input$demodb_connect, {
        message('creating sqlite db')
        shinyjs::hide('demodb_setup')
        ## Create a SQLite DB in memory
        demo_sqlite_export$db_con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
        ## Populate with data from synPUF package
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'care_site', value = ReviewR::care_site)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'concept', value = ReviewR::concept)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'concept_class', value = ReviewR::concept_class)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'concept_synonym', value = ReviewR::concept_synonym)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'condition_era', value = ReviewR::condition_era)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'condition_occurrence', value = ReviewR::condition_occurrence)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'death', value = ReviewR::death)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'device_exposure', value = ReviewR::device_exposure)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'domain', value = ReviewR::domain)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'drug_era', value = ReviewR::drug_era)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'drug_exposure', value = ReviewR::drug_exposure)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'drug_strength', value = ReviewR::drug_strength)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'measurement', value = ReviewR::measurement)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'note', value = ReviewR::note)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'observation', value = ReviewR::observation)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'observation_period', value = ReviewR::observation_period)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'payer_plan_period', value = ReviewR::payer_plan_period)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'person', value = ReviewR::person)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'procedure_occurrence', value = ReviewR::procedure_occurrence)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'provider', value = ReviewR::provider)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'relationship', value = ReviewR::relationship)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'visit_occurrence', value = ReviewR::visit_occurrence)
        dbWriteTable(conn = demo_sqlite_export$db_con, name = 'vocabulary', value = ReviewR::vocabulary)
        ## Report connected
        demo_sqlite_export$is_connected = 'yes'
        shinyjs::show('demodb_connected')
      })
      
      observeEvent(input$demodb_disconnect, {
        req(class(demo_sqlite_export$db_con)[1] == 'SQLiteConnection')
        shinyjs::hide('demodb_connected')
        RSQLite::dbDisconnect(demo_sqlite_export$db_con)
        demo_sqlite_export$db_con <- NULL
        demo_sqlite_export$is_connected = 'no'
        shinyjs::show('demodb_setup')
      })
      
      # Return ----
      return(demo_sqlite_export)
    }
  )
}