# xxxx_src_utilities.R 
# Group Members: John Doe, Jane Smith

# Extract the IPC-4 code from a full IPC string
get_ipc4 <- function(ipc) {
  substring(ipc, 1, 4)
}

# Function to get top N frequent IPC codes and their descriptions
get_top_n_codes <- function(df, code_col, desc_lookup, n=2) {
  freq_table <- sort(table(df[[code_col]]), decreasing = TRUE)
  top_codes <- names(freq_table)[1:n]
  top_desc <- desc_lookup[match(top_codes, desc_lookup$ipc_code), "ipc_desc"]
  
  # Handle fewer than n unique codes
  if (length(top_codes) < n) {
    top_codes <- c(top_codes, rep(NA, n - length(top_codes)))
    top_desc <- c(top_desc, rep(NA, n - length(top_desc)))
  }
  
  list(
    main_code = top_codes[1],
    main_desc = top_desc[1],
    second_code = top_codes[2],
    second_desc = top_desc[2]
  )
}

###############################################################################
#                           ALGO DE RESSEMBLANCE                              #
###############################################################################
# Prépare le nom pour comparaison, similarité, matching
clean_firm_name_for_similarity <- function(name) {
  # Passage en minuscules
  out <- tolower(name)
  
  # On supprime les espaces multiples
  out <- gsub("\\s+", " ", out)
  
  # Trim (début/fin)
  out <- trimws(out)
  
  # Retrait des caractères spéciaux sauf l’espace
  out <- gsub("[^a-z0-9\\s]", "", out)
  
  # Mapping des variations connues
  name_mapping <- c(
    "total energie" = "totalenergies",
    "edf"           = "electricite de france",
    "saint gobain"  = "saint-gobain"
  )
  
  if (out %in% names(name_mapping)) {
    out <- name_mapping[out]
  }
  
  return(out)
}

###############################################################################
#                          RENDU FINAL PROPRE                                 #
###############################################################################
# Nettoyage léger sans trop altérer l'écriture originale
clean_firm_name <- function(name) {
  # On retire juste les espaces de début/fin
  new_name <- trimws(name)
  
  # On ne garde qu’un seul espace entre les mots
  new_name <- gsub("\\s+", " ", new_name)
  
  # Ici, pas de passage en minuscules, ni de mapping “violent”
  # On veut rester le plus proche de l’original, juste "propre"
  return(new_name)
}

# Clean and extract a numeric salary from a salary string
clean_salary <- function(sal) {
  if (is.na(sal) || sal == "") {
    return(NA_real_)
  }
  sal <- gsub(",", "", sal)
  sal <- gsub("[^0-9. -]", "", sal)
  
  parts <- unlist(strsplit(sal, "-"))
  
  # If no parts found, return NA
  if (length(parts) == 0) return(NA_real_)
  
  val <- trimws(parts[1])
  
  if (is.na(val) || val == "") return(NA_real_)
  
  val <- gsub("\\s+", "", val)
  
  if (val == "") return(NA_real_)
  
  out <- suppressWarnings(as.numeric(val))
  if (is.na(out)) return(NA_real_)
  out
}

# Extract top skills from a vector of skill strings
extract_top_skills <- function(skill_vec) {
  if (all(is.na(skill_vec)) || all(skill_vec == "")) {
    return(NA_character_)
  }
  
  all_skills <- unlist(strsplit(na.omit(skill_vec), ",\\s*"))
  if (length(all_skills) == 0) return(NA_character_)
  
  freq_table <- sort(table(all_skills), decreasing = TRUE)
  top_freq <- freq_table[freq_table == freq_table[1]]
  top_skill_names <- names(top_freq)
  
  paste(top_skill_names, collapse=", ")
}
