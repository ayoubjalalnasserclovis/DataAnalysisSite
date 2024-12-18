# xxxx_patent.R
# Group Members: John Doe, Jane Smith
source("xxxx_src_utilities.R")

app_df <- read.table("DATA/202202_EPO_App_reg_small.txt", 
                     header=TRUE, sep=",", quote="\"",
                     stringsAsFactors=FALSE, fill=TRUE)

# Filter to French companies only
app_df <- subset(app_df, ctry_code == "FR")

# Clean firm names
app_df$app_name <- sapply(app_df$app_name, clean_firm_name, USE.NAMES=FALSE)

ipc_df <- read.table("DATA/202202_EPO_IPC_small.txt", 
                     header=TRUE, sep=",", quote="\"",
                     stringsAsFactors=FALSE, fill=TRUE)

# Filter by app_year 2010–2020
ipc_df <- subset(ipc_df, app_year >= 2010 & app_year <= 2020)

# Merge applicant and IPC data on appln_id
patent_df <- merge(app_df, ipc_df, by="appln_id")

# Extract IPC-4 code
patent_df$ipc4 <- sapply(patent_df$IPC, get_ipc4)

# Create IPC description lookup
files_ipc_desc <- list.files("DATA", pattern="EN_ipc_section_.*_title_list_20120101.txt", full.names=TRUE)
ipc_desc_list <- lapply(files_ipc_desc, function(f){
  df <- read.table(f, header=TRUE, sep="\t", quote="\"", stringsAsFactors=FALSE, fill=TRUE)
  colnames(df) <- c("ipc_code_raw","ipc_desc_raw")
  df$ipc_code <- substring(df$ipc_code_raw, 1,4)
  df$ipc_desc <- df$ipc_desc_raw
  df <- df[!duplicated(df$ipc_code), c("ipc_code","ipc_desc")]
  df
})

ipc_desc_df <- do.call(rbind, ipc_desc_list)
ipc_desc_df <- ipc_desc_df[!duplicated(ipc_desc_df$ipc_code), ]

library(dplyr)

base_brevets <- patent_df %>%
  group_by(app_name) %>%
  summarise(
    n_patents = n(),
    {
      codes_info <- get_top_n_codes(patent_df[patent_df$app_name == first(app_name),],
                                    "ipc4", ipc_desc_df, n=2)
      ipc_main_code = codes_info$main_code
      ipc_main_desc = codes_info$main_desc
      ipc_second_code = codes_info$second_code
      ipc_second_desc = codes_info$second_desc
      
      addr_city_main = names(sort(table(city[app_name==first(app_name)]), decreasing=TRUE))[1]
      addr_dept_main = names(sort(table(reg_code[app_name==first(app_name)]), decreasing=TRUE))[1]
      
      data.frame(ipc_main_code, ipc_main_desc, ipc_second_code, ipc_second_desc, addr_city_main, addr_dept_main)
    }
  ) %>% ungroup() %>%
  rename(firm_name = app_name)

# Ensure one row per firm_name
base_brevets <- distinct(base_brevets, firm_name, .keep_all = TRUE)

saveRDS(base_brevets, "DATA/base_brevets.rds")
