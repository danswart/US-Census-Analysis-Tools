###For your PUMS analysis, use these PUMA codes:
###
###  # Define the PUMAs that cover SCUC ISD area
###  scuc_pumas <- c("05700", "05916")  # Remove the "48" state prefix for PUMS data
###
###  # Filter your education data for these PUMAs
###  scuc_education_data <- education_data %>%
###    dplyr::filter(PUMA %in% scuc_pumas)
###
###  # Or if you want to include surrounding areas for comparison:
###  extended_area_pumas <- c(
###    "05700",  # Guadalupe County (Schertz, Cibolo)
###    "05916",  # Bexar County Northeast (Universal City)
###    "05800",  # Comal County (adjacent northern area)
###    "05915"   # Bexar County North (adjacent area)
###  )
###
###  cat("SCUC ISD area PUMAs: 05700 (Guadalupe County), 05916 (Bexar County Northeast)\n")



# Load libraries
library(tidyverse)
library(tidycensus)

# STEP 1: Access the 69,400 built-in PUMS variables dataset
data("pums_variables", package = "tidycensus")

# STEP 2: Explore the structure
cat("PUMS variable structure:\n")
print(names(pums_variables))
cat("Total PUMS variables:", nrow(pums_variables), "\n\n")

# STEP 3: Search for education-related variables
education_terms <- c("SCHOOL", "EDUCATION", "GRADE", "DEGREE", "COLLEGE",
                     "UNIVERSITY", "DIPLOMA", "SCHL", "SCHG", "EDU")

education_vars <- pums_variables %>%
  filter(
    str_detect(toupper(var_code), paste(education_terms, collapse = "|")) |
      str_detect(toupper(var_label), paste(education_terms, collapse = "|"))
  ) %>%
  arrange(var_code)

# STEP 4: Display the 5,300 x 12 education variables tibble found
cat("=== EDUCATION VARIABLES IN PUMS ===\n")
print(education_vars)

# STEP 5: Create the 325 x 12 tibble of key education variables like SCHL
schl_vars <- pums_variables %>% filter(var_code == "SCHL")
cat("\n=== SCHL (Educational Attainment) Variable ===\n")
print(schl_vars)

