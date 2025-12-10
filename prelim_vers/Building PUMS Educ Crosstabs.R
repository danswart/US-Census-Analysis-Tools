library(tidyverse)
library(tidycensus)

# STEP 1: Specify years and variables
working_years <- 2017:2022  # 6 years of data
working_vars <- c("SCHL", "FOD1P", "FOD2P", "RAC1P", "HISP", "AGEP", "SEX",
                  "HINCP", "POVPIP", "NP", "HHT", "MAR", "WAGP", "WKHP", "ESR")

cat("Downloading PUMS data for", length(working_years), "years with", length(working_vars), "variables...\n")




# STEP 2: Download and standardize data types for each year
education_data_list <- purrr::map(working_years, ~{
  cat("Downloading year", .x, "...\n")

  year_data <- tidycensus::get_pums(
    variables = working_vars,
    state = "TX",
    year = .x,
    survey = "acs5"
  )

  # Add year and standardize only existing columns
  year_data <- year_data %>%
    dplyr::mutate(
      year = .x,

      # Standardize numeric columns
      AGEP = as.numeric(AGEP),
      HINCP = as.numeric(HINCP),
      WAGP = as.numeric(WAGP),
      WKHP = as.numeric(WKHP),
      POVPIP = as.numeric(POVPIP),
      NP = as.numeric(NP),

      # Standardize character variables
      RAC1P = as.character(RAC1P),
      HISP = as.character(HISP),
      SEX = as.character(SEX),
      MAR = as.character(MAR),
      ESR = as.character(ESR),
      HHT = as.character(HHT),
      SCHL = as.character(SCHL),
      FOD1P = as.character(FOD1P),
      FOD2P = as.character(FOD2P)
    )

  # Standardize automatic columns if they exist
  if("PWGTP" %in% names(year_data)) {
    year_data$PWGTP <- as.numeric(year_data$PWGTP)
  }
  if("SPORDER" %in% names(year_data)) {
    year_data$SPORDER <- as.character(year_data$SPORDER)
  }
  if("PUMA" %in% names(year_data)) {
    year_data$PUMA <- as.character(year_data$PUMA)
  }
  if("SERIALNO" %in% names(year_data)) {
    year_data$SERIALNO <- as.character(year_data$SERIALNO)
  }
  if("ST" %in% names(year_data)) {
    year_data$ST <- as.character(year_data$ST)
  }

  return(year_data)
})

# Combine the datasets
education_data <- dplyr::bind_rows(education_data_list)
cat("Download complete! Data dimensions:", dim(education_data), "\n")



# STEP 3: Create analysis-ready dataset
analysis_ready <- education_data %>%
  dplyr::mutate(
    # Define racial/ethnic groups using CORRECT character comparisons
    race_ethnicity = dplyr::case_when(
      RAC1P == "1" & HISP == "01" ~ "White_NH",      # White, not Hispanic
      RAC1P == "2" & HISP == "01" ~ "Black_NH",      # Black, not Hispanic
      RAC1P == "6" & HISP == "01" ~ "Asian_NH",      # Asian, not Hispanic
      HISP == "02" ~ "Hispanic",                     # Hispanic (any race)
      TRUE ~ "Other"
    ),

    # Convert SCHL to numeric
    SCHL_num = as.numeric(SCHL),
    college_plus = ifelse(SCHL_num >= 21, 1, 0),
    graduate_degree = ifelse(SCHL_num >= 22, 1, 0),

    # Demographics and controls - FIXED CODES
    age_group = cut(AGEP, breaks = c(0, 25, 35, 45, 55, 65, 100)),
    married = ifelse(MAR == "1", 1, 0),              # FIXED: "1" not "01"
    never_married = ifelse(MAR == "5", 1, 0),        # Never married
    income_quintile = dplyr::ntile(HINCP, 5),
    full_time = ifelse(WKHP >= 35, 1, 0),

    # Proxy for having children - FIXED HHT codes
    likely_has_children = dplyr::case_when(
      HHT %in% c("1", "2") & NP >= 3 ~ 1,           # FIXED: "1", "2" not "01", "02"
      HHT %in% c("3", "4") & NP >= 2 ~ 1,
      TRUE ~ 0
    )
  ) %>%
  dplyr::filter(
    AGEP >= 25 & AGEP <= 65,  # Working age adults
    race_ethnicity != "Other",
    !is.na(SCHL_num)  # Remove records with invalid education codes
  )

cat("Analysis dataset ready! Dimensions:", dim(analysis_ready), "\n")




# STEP 4: Validation checks
cat("Race/ethnicity distribution:\n")
print(table(analysis_ready$race_ethnicity, useNA = "always"))

cat("\nMarried distribution:\n")
print(table(analysis_ready$married, useNA = "always"))

cat("\nNever married distribution:\n")
print(table(analysis_ready$never_married, useNA = "always"))

cat("\nHas children distribution:\n")
print(table(analysis_ready$likely_has_children, useNA = "always"))

cat("\nCollege completion rate:\n")
print(table(analysis_ready$college_plus, useNA = "always"))






# # STEP 5: Filter for SCUC ISD area (optional)
# scuc_data <- analysis_ready %>%
#   dplyr::filter(PUMA %in% c("05700", "05916"))  # Guadalupe County & Bexar County Northeast
#
# cat("\nSCUC ISD area data dimensions:", dim(scuc_data), "\n")




# STEP 6: Hypothesis 1 - Racial achievement gaps controlling for socioeconomic factors
hypothesis_1 <- analysis_ready %>%
  dplyr::filter(AGEP >= 30 & AGEP <= 40) %>%  # Peak career-building years
  dplyr::group_by(year, race_ethnicity, income_quintile, married, likely_has_children) %>%
  dplyr::summarise(
    college_plus_rate = stats::weighted.mean(college_plus, PWGTP, na.rm = TRUE),
    graduate_degree_rate = stats::weighted.mean(graduate_degree, PWGTP, na.rm = TRUE),
    n = sum(PWGTP),
    .groups = "drop"
  ) %>%
  dplyr::filter(n >= 50)  # Minimum sample size for reliable estimates

cat("\nHypothesis 1 results preview:\n")
print(head(hypothesis_1))




# STEP 7: Create the line graph test
college_plot <- ggplot2::ggplot(hypothesis_1, ggplot2::aes(x = year, y = college_plus_rate, color = race_ethnicity)) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::facet_wrap(~paste("Income Q", income_quintile, "| Married:", married, "| Children:", likely_has_children)) +
  ggplot2::labs(
    title = "College Completion Rates: Apples-to-Apples Comparison",
    subtitle = "Controlling for income, marriage, and family structure",
    y = "College Completion Rate",
    x = "Year",
    color = "Race/Ethnicity",
    caption = "If gaps persist within same socioeconomic groups, they're not explained by those factors"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_y_continuous(labels = scales::percent)

print(college_plot)




# STEP 8: Summary statistics by race/ethnicity
summary_stats <- analysis_ready %>%
  dplyr::group_by(race_ethnicity) %>%
  dplyr::summarise(
    n = n(),
    college_rate = mean(college_plus, na.rm = TRUE),
    married_rate = mean(married, na.rm = TRUE),
    has_children_rate = mean(likely_has_children, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nSummary by race/ethnicity:\n")
print(summary_stats)



# ANALYSIS 1: Focus on just never-married people (your wage analysis approach)
never_married_analysis <- analysis_ready %>%
  dplyr::filter(
    AGEP >= 30 & AGEP <= 40,
    never_married == 1  # Never married only
  ) %>%
  dplyr::group_by(year, race_ethnicity, income_quintile) %>%
  dplyr::summarise(
    college_rate = stats::weighted.mean(college_plus, PWGTP, na.rm = TRUE),
    n = sum(PWGTP),
    .groups = "drop"
  ) %>%
  dplyr::filter(n >= 50)


# Simple plot: Never-married people by income
plot1 <- ggplot2::ggplot(never_married_analysis, ggplot2::aes(x = year, y = college_rate, color = race_ethnicity)) +
  ggplot2::geom_line(linewidth = 1.2) +
  ggplot2::facet_wrap(~paste("Income Quintile", income_quintile), nrow = 1) +
  ggplot2::labs(
    title = "College Rates: Never-Married Adults Age 30-40",
    subtitle = "Same approach as your wage analysis - comparing apples to apples",
    y = "College Completion Rate",
    x = "Year"
  ) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::theme_minimal()

print(plot1)




# ANALYSIS 2: Simple summary - do gaps persist when controlling for income?
income_controlled <- never_married_analysis %>%
  dplyr::group_by(race_ethnicity, income_quintile) %>%
  dplyr::summarise(
    avg_college_rate = mean(college_rate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = race_ethnicity, values_from = avg_college_rate) %>%
  dplyr::mutate(
    White_Black_gap = White_NH - Black_NH,
    White_Hispanic_gap = White_NH - Hispanic
  )

cat("College completion gaps by income quintile (never-married 30-40 year olds):\n")
print(income_controlled)




# ANALYSIS 3: Overall summary without overwhelming facets
simple_summary <- analysis_ready %>%
  dplyr::filter(AGEP >= 30 & AGEP <= 40) %>%
  dplyr::group_by(race_ethnicity) %>%
  dplyr::summarise(
    n = n(),
    college_rate = mean(college_plus, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    married_rate = mean(married, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nSimple summary by race (ages 30-40):\n")
print(simple_summary)





# TEST: Do high-performing people have more in common across races
# than with lower-performing people of their own race?

# STEP 1: Define "high performing"
high_performers <- analysis_ready %>%
  dplyr::filter(
    AGEP >= 30 & AGEP <= 50,  # Prime career years
    college_plus == 1,        # College educated
    income_quintile >= 4      # Top 40% income
  )

# STEP 2: Compare high-performing groups across races
high_performer_comparison <- high_performers %>%
  dplyr::group_by(race_ethnicity) %>%
  dplyr::summarise(
    n = n(),
    grad_degree_rate = mean(graduate_degree, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    married_rate = mean(married, na.rm = TRUE),
    full_time_rate = mean(full_time, na.rm = TRUE),
    has_children_rate = mean(likely_has_children, na.rm = TRUE),
    .groups = "drop"
  )

cat("HIGH-PERFORMING people by race (college+ & top 40% income):\n")
print(high_performer_comparison)

# STEP 3: Compare high vs low performers WITHIN racial groups
within_race_comparison <- analysis_ready %>%
  dplyr::filter(AGEP >= 30 & AGEP <= 50) %>%
  dplyr::mutate(
    performance_group = dplyr::case_when(
      college_plus == 1 & income_quintile >= 4 ~ "High_Performer",
      college_plus == 0 & income_quintile <= 2 ~ "Low_Performer",
      TRUE ~ "Middle"
    )
  ) %>%
  dplyr::filter(performance_group != "Middle") %>%
  dplyr::group_by(race_ethnicity, performance_group) %>%
  dplyr::summarise(
    n = n(),
    median_income = median(HINCP, na.rm = TRUE),
    married_rate = mean(married, na.rm = TRUE),
    grad_degree_rate = mean(graduate_degree, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nHIGH vs LOW performers WITHIN each racial group:\n")
print(within_race_comparison)

# STEP 4: Calculate the gaps
# Gap between races among high performers
high_perf_racial_gaps <- high_performer_comparison %>%
  dplyr::summarise(
    White_Black_income_gap = median_income[race_ethnicity == "White_NH"] -
      median_income[race_ethnicity == "Black_NH"],
    White_Hispanic_income_gap = median_income[race_ethnicity == "White_NH"] -
      median_income[race_ethnicity == "Hispanic"],
    Black_White_marriage_gap = married_rate[race_ethnicity == "White_NH"] -
      married_rate[race_ethnicity == "Black_NH"]
  )

cat("\nGaps BETWEEN races among high performers:\n")
print(high_perf_racial_gaps)

# STEP 5: Test your hypothesis directly
hypothesis_test <- within_race_comparison %>%
  tidyr::pivot_wider(names_from = performance_group, values_from = c(median_income, married_rate)) %>%
  dplyr::mutate(
    income_gap_within_race = median_income_High_Performer - median_income_Low_Performer,
    marriage_gap_within_race = married_rate_High_Performer - married_rate_Low_Performer
  )

cat("\nGaps WITHIN racial groups (high vs low performers):\n")
print(hypothesis_test)



##  How to interpret the results:
##    If your hypothesis is TRUE:
##
##    High-performing Black people will have similar incomes/marriage rates to high-performing White/Asian/Hispanic people
##  The gaps within racial groups (high vs low performers) will be larger than gaps between racial groups among high performers
##
##  If your hypothesis is FALSE:
##
##    Large racial gaps will persist even among high performers
##  High-performing Black people will be more similar to low-performing Black people than to high-performing people of other races
##
##  The key comparison: Are the "within race" gaps (high vs low Black performers) bigger than the "between race" gaps (high-performing Black vs high-performing White people)?
##    This analysis directly tests whether class/performance matters more than race - exactly what you're asking.





##  The results strongly support your hypothesis. Here's what the data shows:
##  Your hypothesis is supported:
##  Income gaps:
##
##  Between races among high performers: $24,000-$24,500
##  Within races (high vs low performers): $104,000-$132,000
##
##  Marriage rate gaps:
##
##  Between races among high performers: 14 percentage points (Black-White)
##  Within races (high vs low performers): 47 percentage points (Black), 45 percentage points (White)
##
##  Key finding: The gaps within racial groups (high vs low performers) are 4-5 times larger than gaps between racial groups among high performers.
##  What this means:
##
##  A high-performing Black person ($134k income, 70% married) has much more in common with a high-performing White person ($158k income, 84% married) than with a low-performing Black person ($20k income, 23% married)
##  The $24k income difference between high-performing Black and White people is dwarfed by the $114k difference between high and low-performing Black people
##
##  Your poverty hypothesis also appears correct:
##  When you control for education and income (defining "high performers"), racial gaps shrink dramatically. Meanwhile, class/performance gaps within racial groups remain enormous.
##  However, some caveats:
##
##  Racial gaps don't completely disappear among high performers
##  Marriage rate differences persist (14 percentage points)
##  This is observational data - causation requires more analysis
##
##  Bottom line: Your intuition was right. Among high achievers, class and performance appear to matter much more than race for predicting life outcomes. The "racial gaps" narrative may be masking underlying socioeconomic differences.



## Available "lifestyle/cultural" proxies in your data:

# Cultural/lifestyle indicators you can analyze
lifestyle_analysis <- analysis_ready %>%
  dplyr::filter(AGEP >= 25 & AGEP <= 65) %>%
  dplyr::group_by(race_ethnicity) %>%
  dplyr::summarise(
    # Family structure indicators
    married_rate = mean(married, na.rm = TRUE),
    never_married_rate = mean(never_married, na.rm = TRUE),
    likely_single_parent = mean(likely_has_children == 1 & married == 0, na.rm = TRUE),

    # Work/education patterns
    full_time_work_rate = mean(full_time, na.rm = TRUE),
    college_completion = mean(college_plus, na.rm = TRUE),
    graduate_degree_rate = mean(graduate_degree, na.rm = TRUE),

    # Economic indicators
    median_income = median(HINCP, na.rm = TRUE),
    poverty_rate = mean(POVPIP < 100, na.rm = TRUE),

    n = n(),
    .groups = "drop"
  )

print(lifestyle_analysis)

# Test: Do family structure differences explain educational gaps?
family_education_analysis <- analysis_ready %>%
  dplyr::filter(AGEP >= 30 & AGEP <= 50) %>%
  dplyr::group_by(race_ethnicity, married, likely_has_children) %>%
  dplyr::summarise(
    college_rate = mean(college_plus, na.rm = TRUE),
    median_income = median(HINCP, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  dplyr::filter(n >= 100)

print(family_education_analysis)




##  The results reveal several striking patterns that partially align with cultural explanations, but also raise important questions about causation:
##    Family structure correlations are strong:
##
##    Marriage rates vary dramatically: Asian (76%) and White (66%) vs Black (40%) and Hispanic (60%)
##  Single parenthood follows inverse pattern: Black (28%) and Hispanic (25%) vs Asian (11%) and White (13%)
##
##  Marriage appears protective across all groups:
##
##    Married people consistently outperform unmarried in education and income within every racial group
##  Black married adults (37% college) substantially outperform Black unmarried (21% college)
##  Pattern holds across races: Marriage correlates with better outcomes universally
##
##  However, family structure doesn't eliminate racial gaps:
##
##  Married Black adults (37% college, $90k income) still trail married White adults (51% college, $119k income)
##  Even unmarried Asian adults (73% college) exceed married White adults (51% college)
##
##  Key observations:
##
##  Family structure matters enormously - the marriage effect is consistent across all groups
##  But it's not the whole story - substantial racial gaps persist even within family structure categories
##  Group differences in family formation rates are themselves phenomena that need explanation
##
##  Critical questions the data raises:
##
##    Why do family formation patterns differ so dramatically between groups?
##    Are these "cultural choices" or responses to different economic opportunities and constraints?
##    What explains the persistent gaps even among married individuals?
##
##    The data supports the idea that family structure is a crucial factor, but suggests the story is more complex than purely cultural explanations would predict. Both structural and cultural factors likely interact in ways that simple correlational analysis cannot disentangle.
##





##  Here are the major data sources that could provide the "missing elements" for your analysis, along with their linkage possibilities:
##    Criminal Justice Data:
##
##    Bureau of Justice Statistics (BJS) - Survey of Prison Inmates, probation data
##  FBI Uniform Crime Reporting (UCR) - arrest data by geography
##  National Crime Victimization Survey (NCVS) - individual-level data
##  Linkage: Usually aggregated by geographic area (county/metro) rather than individual-level
##
##  Neighborhood/Community Data:
##
##    American Community Survey - neighborhood characteristics by census tract
##  Opportunity Atlas (Raj Chetty et al.) - intergenerational mobility by census tract
##  Policy Map - comprehensive neighborhood data platform
##  Linkage: Can join to PUMS via PUMA codes (though geographic precision is limited)
##
##  Wealth/Assets:
##
##    Survey of Consumer Finances (Federal Reserve) - detailed wealth data
##  Panel Study of Income Dynamics (PSID) - intergenerational wealth tracking
##  Survey of Income and Program Participation (SIPP) - assets and program participation
##  Linkage: Usually requires statistical matching, not direct linkage
##
##  Social/Cultural Measures:
##
##    General Social Survey (GSS) - values, attitudes, social networks
##  Current Population Survey (CPS) - some social indicators
##  Add Health - social networks, health behaviors (limited age range)
##  Linkage: Statistical matching by demographics/geography
##
##  Practical linkage challenges:
