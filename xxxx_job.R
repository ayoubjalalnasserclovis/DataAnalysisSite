# xxxx_job.R
# Group Members: John Doe, Jane Smith
source("xxxx_src_utilities.R")

library(data.table)
library(dplyr)

file_path <- "DATA/emp_offers_fmt.tsv"
job_dt <- fread(file_path, sep=",", quote="\"", encoding="UTF-8")

# Clean firm names
job_dt[, firm_name := toupper(trimws(entreprise))]
job_dt <- job_dt[!is.na(firm_name) & firm_name != ""]

job_dt[, experience_requise := suppressWarnings(as.numeric(experience_requise))]

extract_salary <- function(sal) {
  if (is.na(sal) || sal == "") return(NA_real_)
  if (grepl("Non spécifié", sal, ignore.case=TRUE)) return(NA_real_)
  
  monthly <- grepl("par mois", sal, ignore.case=TRUE)
  
  # Clean the string to extract a number
  clean <- tolower(sal)
  clean <- gsub("salaire.*?:", "", clean)
  clean <- gsub("eur.*par\\s+an", "", clean)
  clean <- gsub("eur.*par\\s+mois", "", clean)
  clean <- gsub("€.*par\\s+an", "", clean)
  clean <- gsub("€.*par\\s+mois", "", clean)
  clean <- gsub("par\\s+an", "", clean)
  clean <- gsub("par\\s+mois", "", clean)
  clean <- gsub("de\\s+", "", clean)
  clean <- gsub("à", "-", clean)
  clean <- gsub("[^0-9.,-k]", "", clean)
  clean <- trimws(clean)
  
  if (clean == "") return(NA_real_)
  
  parts <- unlist(strsplit(clean, "-"))
  val <- trimws(parts[1])
  val <- gsub(",", ".", val)
  
  if (grepl("k", val, ignore.case=TRUE)) {
    val <- gsub("k", "", val, ignore.case=TRUE)
    numeric_val <- suppressWarnings(as.numeric(val))
    if (is.na(numeric_val)) return(NA_real_)
    numeric_val <- numeric_val * 1000
  } else {
    numeric_val <- suppressWarnings(as.numeric(val))
  }
  
  if (is.na(numeric_val)) return(NA_real_)
  
  if (monthly) {
    numeric_val <- numeric_val * 12
  }
  
  return(numeric_val)
}

job_dt[, salaire_clean := sapply(salaire, extract_salary)]

# Handle competences
job_dt[is.na(competences_requises), competences_requises := ""]
long_skills <- job_dt[, .(skill = unlist(strsplit(competences_requises, ",\\s*"))), by=firm_name]
long_skills <- long_skills[skill != ""]
skill_counts <- long_skills[, .N, by=.(firm_name, skill)]
top_skills <- skill_counts[, .SD[N==max(N)], by=firm_name]
top_skills_combined <- top_skills[, .(top_skill_req = paste(skill, collapse=", ")), by=firm_name]

# Department
dept_counts <- job_dt[, .N, by=.(firm_name, departement)]
main_dept <- dept_counts[, .SD[which.max(N)], by=firm_name]
setnames(main_dept, "departement", "addr_dept_main")

# Sector
sector_counts <- job_dt[, .N, by=.(firm_name, secteur)]
main_sector <- sector_counts[, .SD[which.max(N)], by=firm_name]
setnames(main_sector, "secteur", "sector_main")

base_emp_dt <- job_dt[, .(
  n_offres = .N,
  avg_req_exp = mean(experience_requise, na.rm=TRUE),
  avg_wage = mean(salaire_clean, na.rm=TRUE)
), by=firm_name]

base_emp_dt <- merge(base_emp_dt, top_skills_combined, by="firm_name", all.x=TRUE)
base_emp_dt <- merge(base_emp_dt, main_dept[, .(firm_name, addr_dept_main)], by="firm_name", all.x=TRUE)
base_emp_dt <- merge(base_emp_dt, main_sector[, .(firm_name, sector_main)], by="firm_name", all.x=TRUE)

base_emp_dt[!(firm_name %in% main_sector$firm_name), sector_main := NA_character_]

setcolorder(base_emp_dt, c("firm_name", "n_offres", "sector_main", "avg_req_exp", "top_skill_req", "avg_wage", "addr_dept_main"))

base_emp <- distinct(as.data.frame(base_emp_dt), firm_name, .keep_all=TRUE)
saveRDS(base_emp, "DATA/base_emp.rds")
