
source("najc_src_utilities.R")

library(dplyr)

base_brevets <- readRDS("DATA/base_brevets.rds")
base_emp <- readRDS("DATA/base_emp.rds")

# Full join on firm_name to not lose info from either side
base_emp_inno <- full_join(base_brevets, base_emp, by="firm_name")

saveRDS(base_emp_inno, "DATA/base_emp_inno.rds")
