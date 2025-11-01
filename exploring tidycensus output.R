####################################
##########  STEP-BY-STEP  ##########
####################################


# load libraries
library(tidyverse)
library(tidycensus)

# This object goes into the global environment
dv_acs = c(
  hus = "B25002_001",
  husocc = "B25002_002",
  husvac = "B25002_003"
)

# Create an empty list to hold results
# yearly_dfs <- list()

# Load a list of all 28,152 variables in the acs5 database.  Then, build a
# list of tibbles, each containing the content of the 'label' and 'concept'
# columns for each year specified for each variable defined above

# Loop over these years
# for (yr in 2011:2022) {

# Create new column names by appending the year to "label" and "concept"
# e.g., "label_2009", "concept_2009", "label_2010", "concept_2010", etc.
nm <- stringr::str_c(c("label", "concept"), "_", "2022")

# Load all 28,152 variables as a 28,152 x 4 tibble which includes the
# 'geography' column

STEP1 <- tidycensus::load_variables("2022", "acs5")


# Keep only rows where the 'name' column matches the 3 variable codes
# stored in 'dv_acs'.  Reduces tibble to 3 x 4
STEP2 <- STEP1 %>%
  dplyr::filter(name %in% dv_acs)

# Arrange rows to match the exact order of variables as they appear in 'dv_acs'
# In this case there is no change to the tibble
STEP3 <- STEP2 %>%
  dplyr::arrange(match(name, dv_acs))


# Add a plain English 'id' column at the beginning using the names of the 'dv_acs' vector
STEP4 <- STEP3 %>%
  dplyr::mutate(id = names(dv_acs), .before = 1)

# Rename 'label' and 'concept' columns to include the year suffix
# e.g., 'label' becomes 'label_2009', 'concept' becomes 'concept_2009'
STEP5 <- STEP4 %>%
  dplyr::rename_with(~ nm, c("label", "concept"))


# Display the results for each year
cat("\n=== Results for year", "2022", "===\n")
print(STEP5)
cat("\n")




#####################################################################
##########  FOR LONGITUDINAL DATA, COMBINE ALL YEARS INTO  ##########
##########         ONE WIDE TIBBLE BY USING A LOOP         ##########
#####################################################################


# load libraries
library(tidyverse)
library(tidycensus)

# Give each variable a plain English coding.  In this case, the object goes
# into the global environment
vars_acs5 = c(
  hus = "B25002_001",
  husocc = "B25002_002",
  husvac = "B25002_003"
)

# Create an empty list to hold results
yearly_tibbles <- list()


# Load a list of all 28,152 variables in the acs5 database.  Then, build a
# list of tibbles, each containing the content of the 'label' and 'concept'
# columns for each year specified for each variable defined above

# Loop over these years
for (yr in 2011:2022) {

  # Create new column names by appending the year name to "label" and "concept"
  # e.g., "label_2009", "concept_2009", "label_2010", "concept_2010", etc.
  col_names <- stringr::str_c(c("label", "concept"), "_", yr)

  # For each year, load all variables and create a tibble and store it in
  # the yearly_dfs list object

  yearly_tibbles[[as.character(yr)]] <- tidycensus::load_variables(yr, "acs5") %>%

    # Keep only rows where the 'name' column matches variable codes stored in 'vars_acs5'
    dplyr::filter(name %in% vars_acs5) %>%

    # Arrange rows to match the exact order of variables as they appear in 'vars_acs5'
    dplyr::arrange(match(name, vars_acs5)) %>%

    # Add an 'id' column at the beginning using the names of the 'dv_acs' vector
    dplyr::mutate(id = names(vars_acs5), .before = 1) %>%

    # Rename 'label' and 'concept' columns to include the year suffix
    # e.g., 'label' becomes 'label_2009', 'concept' becomes 'concept_2009'
    dplyr::rename_with(~ col_names, c("label", "concept"))

  # Display the results for each year
  cat("\n=== Results for year", yr, "===\n")
  print(yearly_tibbles[[as.character(yr)]])
  cat("\n")

}


# Combine all yearly tibbles into one WIDE format dataset
# Each year's label/concept data becomes separate columns in the final result
# Reduce applies full_join by iteration across all list elements
# Result: id, name, label_2009, concept_2009, label_2010, concept_2010, label_2011, concept_2011
output_of_forloop <- Reduce(dplyr::full_join, yearly_tibbles)



##########       NOTE ANY CHANGES IN VARIABLES SHOWN IN NEW TIBBLE       ##########
##########  CONSIDER LIMITING TIBBLE TO YEARS WITH CONSISTENT VARIABLES  ##########



##########  FOR SERIOUS RESEARCH, CONSIDER THE FOLLOWING STEPS  ##########


# Check column consistency across years first (this takes a while)
column_check <- purrr::map_dfr(2011:2022, ~{
  vars <- tidycensus::load_variables(.x, "acs5")
  dplyr::tibble(year = .x, columns = list(names(vars)))
})


# See what columns appear when
column_check %>%
  tidyr::unnest(columns) %>%
  dplyr::count(columns, sort = TRUE)


# Find variables that don't exist in all years (this will take awhile)
availability_check <- purrr::map_dfr(2011:2022, ~{
  vars <- tidycensus::load_variables(.x, "acs5")
  dplyr::tibble(
    year = .x,
    available_vars = list(vars$name),
    missing_dv_acs = list(dplyr::setdiff(vars_acs5, vars$name))
  )
})


