#########################################

# BRFSS BASE CODE - R TRANSLATION FROM SAS
# CREATED 12/12/2025

########### INSTALLING PACKAGES #########

##INSTALL PACKAGES
install.packages("haven")
install.packages("dplyr")
install.packages("survey")
install.packages("grid")
install.packages("Matrix")
install.packages("survival")

# ENSURE YOU HAVE INSTALLED PACKAGES IN R CONSOLE BEFORE LOADING THE LIBRARIES
# LOADING LIBRARIES
library(haven)      # READ SAS FILES
library(dplyr)      # DATA MANIPULATION
library(srvyr)      # DPLYR-FRIENDLY SURVEY ANALYSIS
library(survey)     # FOR SURVEYFREQ-LIKE ANALYSIS
library(labelled)   # WORK WITH AND MANAGE LABELLE DATA FROM SAS
library(stringr)    # STRING HELPERS
library(tidyr)      # HELPERS
library(grid)
library(Matrix)
library(survival)


# NOTES: LOCALITY CODES - REGION = HLTH_REGION_ID , DISTRICT = HLTH_DIST_NAME , INTERVIEW YEAR = IYEAR, COUNTY = CTYCODE2
# NOTES2: I AM TRANSLATING THIS CODE FROM A SAS CODE; I WILL BE REFERENCING SOME SAS COMMANDS TO THE EQUIVALENT IN R


############ FILE PATHS ############

# IMPORTING SAS DATASETS (SAS -> R)
brfss21 <- read_sas("C:/Users/mhg62973/Desktop/R Projects/BRFSS Data Practice/brpub21.sas7bdat")
brfss22 <- read_sas("C:/Users/mhg62973/Desktop/R Projects/BRFSS Data Practice/brpub22.sas7bdat")
brfss23 <- read_sas("C:/Users/mhg62973/Desktop/R Projects/BRFSS Data Practice/brpub23.sas7bdat")


############ CHECKING DATA; CLEANING AND FORMATTING DATA ############
# IN SAS, THE PROC FORMAT STEP WOULD BE NEXT. IN R, YOU WILL USE FACTORS TO ATTACH CODES TO LABELS
# DO THIS STEP AFTER YOU HAVE MERGED AND CLEANED DATA


# RENAMING THE DATASET
  # BRFSS SOMETIMES CHANGES VARIABLE NAMES ACROSS YEARS, SO I AM CREATING VARIABLE LISTS FIRST, 
  # IF VARIABLE NAMES DIFFER IN A YEAR,
  # THIS CODE IS TAKING YOUR FULL DATASET AND RENAMING ONE COLMN SO
  # THE NAMES MATCH THE OTHER YEARS (BELOW) AND THEN KEETING ONLY A SPECIFIED
  # SUBSET OF COLUMNS.
  # THIS IS ALSO THE SAME AS THE SQL STEP IN THE SAS CODE
BRFSS_21 <- brfss21 %>%
  select(SEQNO, IYEAR, `_STSTR`, `_PSU`, `_LLCPWT`,
         GENHLTH, HLTH_DIST_NAME, sex, race, `_AGE_G`,
         EMPLOY1, `_HLTHPLN`, CHECKUP1, INCOME3, `_EDUCAG`, `_IMPRACE`,
         DIABETE4, HAVARTH5, CHCCOPD3, ADDEPEV3)

# RENAMING THE DATA SET AND ONLY KEEP VARIABLES THAT YOU WANT TO ANALYZE FOR THE YEAR 2022
BRFSS_22 <- brfss22 %>%
  select(SEQNO, IYEAR, `_STSTR`, `_PSU`, `_LLCPWT`,
         GENHLTH, HLTH_DIST_NAME, sex, race, `_AGE_G`,
         EMPLOY1, `_HLTHPLN`, CHECKUP1, INCOME3, `_EDUCAG`, `_IMPRACE`,
         DIABETE4, HAVARTH4, CHCCOPD3, ADDEPEV3)

# RENAMING THE DATA SET AND ONLY KEEP VARIABLES THAT YOU WANT TO ANALYZE FOR THE YEAR 2023
  # YEAR 2023 USES _HLTHPL1; RENAME TO MATCH _HLTHPLN BEFORE SELECT
  # THE dplyr:: PREFIX TELLS THE CODE TO USE THE rename() AND select() FUNCTIONS FROM THE dplyr PACKAGE
  # rename() syntax harmonizes the 2023 health-plan variable name with the 2021 
  # and 2022 data so that you can stack years later without having different variable names
  # select() restricts the dataset to only the columns you list, in that order
  # This mirrors the SAS PROC SQL SELECT list
BRFSS_23 <- brfss23 %>%
  dplyr::rename(`_HLTHPLN` = `_HLTHPL1`) %>%      #rename(new_name = old_name)  #creates column called _HLTHPLN in the data coming from brfss23
  dplyr::select(
    SEQNO, IYEAR, `_STSTR`, `_PSU`, `_LLCPWT`,
    GENHLTH, HLTH_DIST_NAME, sex, race,`_AGE_G`,
    EMPLOY1, `_HLTHPLN`, CHECKUP1, INCOME3,`_EDUCAG`, `_IMPRACE`,
    DIABETE4, HAVARTH4, CHCCOPD3, ADDEPEV3
  )

# COMBINE DATASETS BY ROWS
# bind_rows() takes the data frames created and binds them by rows, 
  # returning a single data frame. This syntax matches columns by name.
  # If a column doesn't exist in one year but in another, bind_rows() keeps
  # the columns and fills missing values with NA where needed.
#In SAS, this is comparable to taking the three year specific tables and 
  # stacking them into one combined multi-year data set before analysis
All_BRFSS <- bind_rows(BRFSS_21, BRFSS_22, BRFSS_23)


# CREATE A NEW DATA FRAME
# RECODING VARIABLES INTO CLEANED BRFSS_clean
  # mutate() ADDS NEW COLUMNS OR MODIFIES EXISTING ONES IN A DATA FRAME
  # WITHOUT DROPPING OTHER VARIABLES
BRFSS_clean <- All_BRFSS %>%
  # ENSURES COLUMN NAMES EXIST (AVOID ERRORS)
  mutate(
    # age2: map _AGE_G TO AGE CATEGORIES (6 CATEGORIES)
    age2 = case_when(
      is.na(`_AGE_G`) ~ NA_real_,
      `_AGE_G` == 1 ~ 1, # 18-24
      `_AGE_G` == 2 ~ 2, # 25-34
      `_AGE_G` == 3 ~ 3, # 35-44
      `_AGE_G` == 4 ~ 4, # 45-54
      `_AGE_G` == 5 ~ 5, # 55-64
      `_AGE_G` == 6 ~ 6  # 65+
    ),
    # EMPLOYMENT: 3-CATEGORY VARIABLE FROM EMPLOY1
    employ = case_when(
      is.na(EMPLOY1) ~ NA_real_,
      EMPLOY1 %in% c(1,2) ~ 1,                    # EMPLOYED OR SELF-EMPLOYED (REF)
      EMPLOY1 %in% c(3,4,5,6,8) ~ 2,              # OUT OF WORK, HOMEMAKER, STUDENT
      EMPLOY1 == 7 ~ 3                            # RETIRED
    ),
    # INCOME CATEGORIES (INCOME 3 MAPPING) 4 categories
    income = case_when(
      INCOME3 %in% c(99,77) | is.na(INCOME3) ~ NA_real_, # MISSING
      INCOME3 %in% c(1,2,3,4) ~ 1,   # <25
      INCOME3 %in% c(5,6) ~ 2,       # 25-50
      INCOME3 == 7 ~ 3,              # 50-75
      INCOME3 %in% c(8,9,10,11) ~ 4  # 75+ (REF)
      TRUE ~ NA_integer_
    ),
    # EDUCATION: _EDUCAG -> education
    education = case_when(
      is.na(`_EDUCAG`)     ~ NA_real_,
      `_EDUCAG` == 1       ~ 1,  # <HS
      `_EDUCAG` == 2       ~ 2,  # HS
      `_EDUCAG` == 3       ~ 3,  # some college
      `_EDUCAG` == 4       ~ 4   # college grad
    ),
    # HEALTH INSURANCE: _HLTHPLN -> insurance
    insurance = case_when(
      `_HLTHPLN` %in% c(7, 9) | is.na(`_HLTHPLN`) ~ NA_real_,
      `_HLTHPLN` == 1                            ~ 1,  # has insurance
      `_HLTHPLN` == 2                            ~ 2   # no insurance
    ),
    # DEPRESSION: ADDEPEV3 -> depression (1 = no, 2 = yes)
    depression = case_when(
      ADDEPEV3 %in% c(7, 9) | is.na(ADDEPEV3) ~ NA_real_,
      ADDEPEV3 == 1                           ~ 2,  # has depression
      ADDEPEV3 == 2                           ~ 1   # no depression
    ),
    # ROUTINE CHECKUP: SAS SETS CHECKYEAR=1 IF CHECKUP==1 ELSE 2
    checkyear = if_else(CHECKUP1 == 1, 1, 2, missing = NA_real_),
    
    # GOODHLTH: SAS COMBINED GENHLTH 1/2/3 => 1 ELSE 2
    # General health: GENHLTH -> GOODHLTH (1 = good/very good/excellent)
    GOODHLTH = case_when(
      GENHLTH %in% c(1, 2, 3) ~ 1,
      GENHLTH %in% c(4, 5)   ~ 2,
      TRUE                   ~ NA_real_
    ),
    # DISEASE INDICATORS: SAS USED 1 -> DISEASE=2, ELSE DISEASE=1
    diabetes = if_else(DIABETE4 == 1, 2, 1, missing = NA_real_),
    
    # ARTHTRITIS
    arthritis = case_when(
      is.na(HAVARTH4) ~ NA_real_,
      HAVARTH4 == 1   ~ 2,
      TRUE            ~ 1
    ),
    # COPD
    COPD = if_else(CHCCOPD3 == 1, 2, 1, missing = NA_real_),
    
    # CHRONIC DISEASE COMPOSITE: IF ANY OF THE THREE INDICATOR VARS ==1 THEN CHRODIS=2 ELSE 1
    Chrodis = case_when(
      DIABETE4 == 1 | HAVARTH4 == 1 | CHCCOPD3 == 1 ~ 2,
      !is.na(DIABETE4) | !is.na(HAVARTH4) | !is.na(CHCCOPD3) ~ 1,
      TRUE ~ NA_real_
    )
)


# FACTORS: SAS PROC FORMAT CODE
# levels = takes the numeric codes
# labels = takes the strings you want to see
BRFSS_clean <- BRFSS_clean %>%
  mutate(
    # GENHLTH format
    GOODHLTH_f = factor(
      GOODHLTH,
      levels = c(1, 2, 3, 4, 5, 7, 9),
      labels = c("Excellent", "Very good", "Good",
                 "Fair", "Poor",
                 "Dont Know/ Not sure", "Refused")
    ),
    
    # age format (your 6-category age variable)
    age_f = factor(
      age2,   # or whatever variable uses codes 1–6
      levels = c(1, 2, 3, 4, 5, 6),
      labels = c("18 to 24", "25 to 34", "35 to 44",
                 "45 to 54", "55 to 64", "65+")
    ),
    
    # race format
    race_f = factor(
      race,
      levels = c(1, 2, 3, 4),
      labels = c("White", "Black", "Hispanic", "Other")
    ),
    
    # income format (your 4‑category income variable)
    income_f = factor(
      income,    # 1–4 from your recode
      levels = c(1, 2, 3, 4),
      labels = c("<25", "25-50", "50-75", "75+")
    ),
    
    # education format (4-category)
    education_f = factor(
      education,
      levels = c(1, 2, 3, 4),
      labels = c("Didnt Graduate HS",
                 "Graduated HS",
                 "Some College",
                 "Graduated College")
    ),
    
    # employ format (3-category)
    employ_f = factor(
      employ,
      levels = c(1, 2, 3),
      labels = c("Employed", "Unemployed", "Retired")
    ),
    
    # sex format
    sex_f = factor(
      sex,
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),
    
    # _HLTHPLN categorical insurance type
    insurance_f = factor(
      insurance,
      levels = 1:10,
      labels = c("Employer or Union",
                 "Private",
                 "Medicare",
                 "Medigap",
                 "Medicaid",
                 "CHIP",
                 "CHAMPUS, CHAMP-VA, VA health Care",
                 "Indian Health Services",
                 "Other Government Program",
                 "No Coverage")
    ),
    
    # CHECKUP1
    checkyear_f = factor(
      checkyear,
      levels = c(1, 2, 3, 4, 7),
      labels = c(
        "Within past year (anytime less than 12 months ago)",
        "Within past 2 years (1 year but less than 2 years ago)",
        "Within past 5 years (2 years but less than 5 years ago)",
        "5 or more years ago",
        "Not sure"
      )
    )
  )


options(survey.lonely.psu = "adjust")  #tells package how to handle strata that have only one 
# PSU (primary sampling unit) when computing variances

# SET UP SURVEY DESIGN
brfss_design <- svydesign(
  ids    = ~`_PSU`,            # BRFSS CLUSTER ID 
  strata = ~`_STSTR`,          # BRFSS DESIGN STRATA - SIMILAR TO SAS SURVEY PROC
  weights= ~`_LLCPWT`,         # BRFSS SAMPLING WEIGHT
  nest   = TRUE,               # TREATS PSUs AS NESTED WITHIN STRATA
  data   = BRFSS_clean         # USES CLEANED MULTI-YEAR BRFSS DATASET
)

# Create and then print a survey‑weighted contingency table of health district by sex
# Replicates PROC SURVEYFREQ crosstabs
tbl_dist_sex <- svytable(~ HLTH_DIST_NAME + sex_f, design = brfss_design)     #build two-way table
tbl_dist_sex                                                                #prints table to the console

# Convert weighted count table into ROW PERCENTAGES
prop_dist_sex <- prop.table(tbl_dist_sex, margin = 1)         # tbl_dist_sex is a two way weighted counts
prop_dist_sex                                                 # prints the row-percentage table

# Chi‑square test of association (Rao–Scott)
svychisq(~ HLTH_DIST_NAME + sex, design = brfss_design)       # using ~ in the svychisq() syntax: uses that formula to compute a design‑based chi‑square test for the association between HLTH_DIST_NAME and sex              

# Create 3-way survey weighted contingency table
tbl_dist_chrodis_dep <- svytable(~ HLTH_DIST_NAME + Chrodis + depression,
                                 design = brfss_design)
ftable(tbl_dist_chrodis_dep)  # FLATTENED DISPLAY

# Create 2‑way chi‑square: Chrodis vs depression
svychisq(~ Chrodis + depression, design = brfss_design)

# You can compute row‑percent + CIs for a binary outcome using svymean():
# Percent with depression=2 (has depression) by HLTH_DIST_NAME
# Create indicator in R if you prefer:
BRFSS_clean <- BRFSS_clean %>%
  dplyr::mutate(dep_yes = ifelse(depression == 2, 1, 0))

brfss_design <- svydesign(
  ids    = ~`_PSU`,
  strata = ~`_STSTR`,
  weights= ~`_LLCPWT`,
  nest   = TRUE,
  data   = BRFSS_clean
)

brfss_design <- update(
  brfss_design,
  dep_yes = ifelse(depression == 2, 1, 0)
)

# restrict the respondents with non-missing depression before running svyby
brfss_design_dep <- subset(brfss_design, !is.na(depression))

svyby(~dep_yes,
      ~HLTH_DIST_NAME,
      brfss_design_dep,
      svymean,
      vartype = c("se", "ci"))

# svyby() computes survey statistics by groups (or subsets) of the survey design object
svyby(~dep_yes,
      ~HLTH_DIST_NAME,
      brfss_design,
      svymean, vartype = c("se", "ci"),  # gives % (mean), SE, CI by region
      na.rm = TRUE)      #Be explicit about missing and domain variance

# Replicate PROC SURVEYLOGISTIC (binary)
BRFSS_clean <- BRFSS_clean %>%
  mutate(
    Depression_f = factor(depression, levels = c(1, 2)), # 1 = ref, 2 = has depression
    Chrodis_f    = factor(Chrodis,    levels = c(1, 2))  # 1 = ref, 2 = chronic disease
  )

# ADD variables inside survey design object
# Create Depression_f and Chrodis_f
brfss_design <- update(brfss_design,            #evaluates the right-hand side expression in the data that is inside brfss_design
                       Depression_f = factor(depression, levels = c(1, 2)),
                       Chrodis_f    = factor(Chrodis,    levels = c(1, 2)))

# Survey-weighted binary logistic regression model of depression on chronic disease
fit_biv <- svyglm(
  Depression_f ~ Chrodis_f,
  design = brfss_design,
  family = quasibinomial()
)

# Print the regression results
summary(fit_biv)             # coefficients on log‑odds scale

# Odds ratios and 95% CIs
OR  <- exp(coef(fit_biv))
CI  <- exp(confint(fit_biv, ddf = degf(fit_biv$survey.design)))
cbind(OR, CI)

# Multivariable survey logistic regression
BRFSS_clean <- BRFSS_clean %>%
  mutate(
    HLTH_DIST_NAME_f = relevel(factor(HLTH_DIST_NAME), ref = "Richmond City"),
    race_f           = relevel(factor(race),           ref = 1),
    sex_f            = relevel(factor(sex),            ref = 1),
    income_f         = relevel(factor(income),         ref = 4),
    education_f      = relevel(factor(education),      ref = 4),
    insurance_f      = relevel(factor(insurance),      ref = 1)
  )

# Reattach variables to your survey design object
brfss_design <- update(       #evaluates each right-hand side expression in the data sorted in brfss_design and saves results as a variable with the name on the left-hand side
  brfss_design,
  HLTH_DIST_NAME_f = relevel(factor(HLTH_DIST_NAME), ref = "Richmond City"),
  race_f           = relevel(factor(race),           ref = 1),
  sex_f            = relevel(factor(sex),            ref = 1),
  income_f         = relevel(factor(income),         ref = 4),
  education_f      = relevel(factor(education),      ref = 4),
  insurance_f      = relevel(factor(insurance),      ref = 1)
)


fit_multiv <- svyglm(
  Depression_f ~ Chrodis_f + HLTH_DIST_NAME_f + race_f + sex_f +
    income_f + education_f + insurance_f,
  design = brfss_design,
  family = quasibinomial()
)

summary(fit_multiv)

# Odds ratios + CIs
OR_mult <- exp(coef(fit_multiv))
CI_mult <- exp(confint(fit_multiv, ddf = degf(fit_multiv$survey.design)))
cbind(OR_mult, CI_mult)


# THE PURPOSE OF THIS CODE THAT WAS ORIGINALLY RUN IN SAS WAS TO INVESTIGATE THE CORRELATION/ASSOCIATION BETWEEN CHRONIC DISEASE AND DEPRESSION.
# THE BRFSS DATA USED WERE FROM YEARS 2021-2023. 



















