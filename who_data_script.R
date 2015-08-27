# Global health observatory data repository
# http://apps.who.int/gho/data/?theme=home
# Accessed July 30. 2015

# Use setwd() to go to the directory where the data has been stored. Alternatively
# data can be handled online though I have chosen to download the data for
# later documentation.

# The source of the data (URL) is given for each variable.

library(dplyr)

# Funktion: Risikofaktorer - mean --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A867?lang=en

RiskFactor <- function(path) {
        t <- read.csv(path, stringsAsFactor = FALSE)
        t <- filter(t, COUNTRY..DISPLAY. %in% c("Denmark",
                                             "Algeria",
                                             "Egypt",
                                             "Libya",
                                             "Morocco",
                                             "Sudan",
                                             "Tunisia",
                                             "Bahrain",
                                             "United Arab Emirates",
                                             "Iraq",
                                             "Iran",
                                             "Jordan",
                                             "Kuwait",
                                             "Lebanon",
                                             "Oman",
                                             "Saudi Arabia",
                                             "Syrian Arab Republic",
                                             "Turkey",
                                             "Yemen",
                                             "Qatar")) %>%
                filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
                select(GHO..DISPLAY.,
                       YEAR..CODE.,
                       REGION..CODE.,
                       COUNTRY..CODE.,
                       COUNTRY..DISPLAY.,
                       SEX..CODE.,
                       Numeric,
                       Low,
                       High,
                       Display.Value) %>%
                arrange(Numeric)
        names(t) <- tolower(names(t))
        t
}

mean.bmi <- RiskFactor("./who_mean_bmi.csv") %>%
        filter(sex..code. == "BTSX")

mean.fasting.glucose <- RiskFactor("./who_mean_fasting_glucose.csv")

mean.sys.bp <- RiskFactor("./who_mean_sys_bp.csv") %>%
        filter(sex..code. == "BTSX")

mean.colestrl <- RiskFactor("./who_mean_tot_cholest.csv")

# Funktion: Risikofaktorer - procent af beflokning --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A867?lang=en

RiskFactor <- function(path) {
        t <- read.csv(path, stringsAsFactor = FALSE)
        t <- filter(t, COUNTRY..DISPLAY. %in% c("Denmark",
                                                "Algeria",
                                                "Egypt",
                                                "Libya",
                                                "Morocco",
                                                "Sudan",
                                                "Tunisia",
                                                "Bahrain",
                                                "United Arab Emirates",
                                                "Iraq",
                                                "Iran",
                                                "Jordan",
                                                "Kuwait",
                                                "Lebanon",
                                                "Oman",
                                                "Saudi Arabia",
                                                "Syrian Arab Republic",
                                                "Turkey",
                                                "Yemen",
                                                "Qatar")) %>%
                filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
                select(GHO..DISPLAY.,
                       YEAR..CODE.,
                       REGION..CODE.,
                       COUNTRY..CODE.,
                       COUNTRY..DISPLAY.,
                       SEX..CODE.,
                       Numeric,
                       Low,
                       High,
                       Display.Value) %>%
                arrange(Numeric)
        names(t) <- tolower(names(t))
        t
}

# Overweight: BMI >= 25
overweight <- RiskFactor("./who_overweight_bmi25or_above.csv") %>%
        filter(sex..code. == "BTSX")

# Obesity: BMI >= 30
obesity <- RiskFactor("./who_obesity_bmi30or_above.csv") %>%
        filter(sex..code. == "BTSX")

# Raised blood pressure: systolic >= 140 and/or diastolic >= 90
rsd.pres <- RiskFactor("./who_raised_bp_sys_dia_140or90or_above.csv") %>%
        filter(sex..code. == "BTSX") %>%
        filter(gho..display. == "Raised blood pressure (SBP>=140 OR DBP>=90) (age-standardized estimate)")

# Raised fasting blood glucose: bg >= 7.0 mmol/L or on medication
rsd.gluc <- RiskFactor("./who_raised_fasting_glucose_7or_abode_or_on_meds.csv") %>%
        filter(sex..code. == "BTSX") %>%
        filter(gho..display. == "Raised fasting blood glucose (>=7.0 mmol/L or on medication)(age-standardized estimate)")

t <- filter(rsd.gluc, !country..code. == "DNK") %>%
        group_by(gho..display.) %>%
        summarise(median(numeric), IQR(numeric))

# Raised total cholesterol: cholesterol >= 190 mg/dl (5.0 mmol/L)
rsd.clstrl <- RiskFactor("./who_raised_tot_choles_5or_above.csv") %>%
        filter(sex..code. == "BTSX") %>%
        filter(gho..display. == "Raised total cholesterol (>= 5.0 mmol/L) (age-standardized estimate)")

# Probability of dying from any NCD ---------------------------------------
# http://apps.who.int/gho/data/node.main.A857?lang=en

dta <- read.csv("./who_risk_of_death_ncd.csv",
                           stringsAsFactors = FALSE)

prob.dying.ncd <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                     "Algeria",
                                     "Egypt",
                                     "Libya",
                                     "Morocco",
                                     "Sudan",
                                     "Tunisia",
                                     "Bahrain",
                                     "United Arab Emirates",
                                     "Iraq",
                                     "Iran",
                                     "Jordan",
                                     "Kuwait",
                                     "Lebanon",
                                     "Oman",
                                     "Saudi Arabia",
                                     "Syrian Arab Republic",
                                     "Turkey",
                                     "Yemen",
                                     "Qatar")) %>%
        filter(YEAR..CODE. == 2012) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               Numeric) %>%
        arrange(Numeric)

names(prob.dying.ncd) <- tolower(names(prob.dying.ncd))

rm(dta)

# qplot(country..code., numeric, data = deaths.all.ncd)

# Prevalence: tobacco use -------------------------------------------------
# http://apps.who.int/gho/data/node.main.1250?lang=en

dta <- read.csv("./who_preval_tobacco.csv",
                stringsAsFactors = FALSE)

tobac <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                                       "Algeria",
                                                       "Egypt",
                                                       "Libya",
                                                       "Morocco",
                                                       "Sudan",
                                                       "Tunisia",
                                                       "Bahrain",
                                                       "United Arab Emirates",
                                                       "Iraq",
                                                       "Iran",
                                                       "Jordan",
                                                       "Kuwait",
                                                       "Lebanon",
                                                       "Oman",
                                                       "Saudi Arabia",
                                                       "Syrian Arab Republic",
                                                       "Turkey",
                                                       "Yemen",
                                                       "Qatar")) %>%
        filter(GHO..DISPLAY. == "Current smoking of any tobacco product (age-standardized rate)") %>%
        filter(SEX..CODE. == "BTSX") %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               Numeric) %>%
        arrange(Numeric)

names(tobac) <- tolower(names(tobac))

rm(dta)

# Distribution of years of life lost (%) ----------------------------------
# http://apps.who.int/gho/data/node.main.21?lang=en

dta <- read.csv("./who_distribution_of_years_of_life_lost.csv",
                stringsAsFactors = FALSE)

yll <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                              "Algeria",
                                              "Egypt",
                                              "Libya",
                                              "Morocco",
                                              "Sudan",
                                              "Tunisia",
                                              "Bahrain",
                                              "United Arab Emirates",
                                              "Iraq",
                                              "Iran",
                                              "Jordan",
                                              "Kuwait",
                                              "Lebanon",
                                              "Oman",
                                              "Saudi Arabia",
                                              "Syrian Arab Republic",
                                              "Turkey",
                                              "Yemen",
                                              "Qatar")) %>%
        filter(YEAR..CODE. == 2012) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               GHECAUSES..DISPLAY.,
               Numeric) %>%
        arrange(GHECAUSES..DISPLAY., Numeric)

names(yll) <- tolower(names(yll))

rm(dta)

# Mortality rate pr. 100,000, age-standardised, 3 grp. --------------------
# http://apps.who.int/gho/data/node.main.18?lang=en

dta <- read.csv("./who_death_rate_major_groups.csv",
                stringsAsFactors = FALSE)

mort.rate <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                            "Algeria",
                                            "Egypt",
                                            "Libya",
                                            "Morocco",
                                            "Sudan",
                                            "Tunisia",
                                            "Bahrain",
                                            "United Arab Emirates",
                                            "Iraq",
                                            "Iran",
                                            "Jordan",
                                            "Kuwait",
                                            "Lebanon",
                                            "Oman",
                                            "Saudi Arabia",
                                            "Syrian Arab Republic",
                                            "Turkey",
                                            "Yemen",
                                            "Qatar")) %>%
        filter(YEAR..CODE. == 2012) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               GHECAUSES..DISPLAY.,
               Numeric) %>%
        arrange(GHECAUSES..DISPLAY., Numeric)

names(mort.rate) <- tolower(names(mort.rate))

rm(dta)

# DALYs -------------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.DALYCTRY?lang=en

dta <- read.csv("./who_daly.csv",
                stringsAsFactors = FALSE)

daly <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                                  "Algeria",
                                                  "Egypt",
                                                  "Libya",
                                                  "Morocco",
                                                  "Sudan",
                                                  "Tunisia",
                                                  "Bahrain",
                                                  "United Arab Emirates",
                                                  "Iraq",
                                                  "Iran",
                                                  "Jordan",
                                                  "Kuwait",
                                                  "Lebanon",
                                                  "Oman",
                                                  "Saudi Arabia",
                                                  "Syrian Arab Republic",
                                                  "Turkey",
                                                  "Yemen",
                                                  "Qatar")) %>%
        filter(YEAR..CODE. == 2012) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               GHECAUSES..DISPLAY.,
               Numeric) %>%
        arrange(GHECAUSES..DISPLAY., Numeric)

names(daly) <- tolower(names(daly))

rm(dta)

# Number of reported cases of selected CDs --------------------------------
# http://apps.who.int/gho/data/node.main.31?lang=en

dta <- read.csv("./who_CD_number_of_cases.csv",
                stringsAsFactors = FALSE)

nbr.cd <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                             "Algeria",
                                             "Egypt",
                                             "Libya",
                                             "Morocco",
                                             "Sudan",
                                             "Tunisia",
                                             "Bahrain",
                                             "United Arab Emirates",
                                             "Iraq",
                                             "Iran",
                                             "Jordan",
                                             "Kuwait",
                                             "Lebanon",
                                             "Oman",
                                             "Saudi Arabia",
                                             "Syrian Arab Republic",
                                             "Turkey",
                                             "Yemen",
                                             "Qatar")) %>%
        filter(YEAR..CODE. == 2012) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               Numeric) %>%
        arrange(GHO..DISPLAY., Numeric)

names(nbr.cd) <- tolower(names(nbr.cd))

rm(dta)

d <- group_by(nbr.cd, gho..display.) %>%
        filter(! country..code. == "DNK") %>%
        summarise(nbr = sum(numeric, na.rm = TRUE)) %>%
        arrange(- nbr)

# Mortality + prevalence: Tuberculosis ------------------------------------
# http://apps.who.int/gho/data/node.main.1317?lang=en

dta <- read.csv("./who_tb.csv",
                stringsAsFactors = FALSE)

tb <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                               "Algeria",
                                               "Egypt",
                                               "Libya",
                                               "Morocco",
                                               "Sudan",
                                               "Tunisia",
                                               "Bahrain",
                                               "United Arab Emirates",
                                               "Iraq",
                                               "Iran",
                                               "Jordan",
                                               "Kuwait",
                                               "Lebanon",
                                               "Oman",
                                               "Saudi Arabia",
                                               "Syrian Arab Republic",
                                               "Turkey",
                                               "Yemen",
                                               "Qatar")) %>%
        filter(YEAR..CODE. == 2013) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               Numeric,
               Low,
               High,
               Display.Value) %>%
        arrange(GHO..DISPLAY., Numeric)

names(tb) <- tolower(names(tb))

rm(dta)

t <- filter(tb, !country..code. == "DNK") %>%
        group_by(gho..display.) %>%
        summarise(median(numeric), IQR(numeric))

# Deaths pr. 100,000 all NCDs --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A863?lang=en

dta <- read.csv("./who_all_ncd_deaths.csv",
                stringsAsFactors = FALSE)

deaths.all.ncd <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                                       "Algeria",
                                                       "Egypt",
                                                       "Libya",
                                                       "Morocco",
                                                       "Sudan",
                                                       "Tunisia",
                                                       "Bahrain",
                                                       "United Arab Emirates",
                                                       "Iraq",
                                                       "Iran",
                                                       "Jordan",
                                                       "Kuwait",
                                                       "Lebanon",
                                                       "Oman",
                                                       "Saudi Arabia",
                                                       "Syrian Arab Republic",
                                                       "Turkey",
                                                       "Yemen",
                                                       "Qatar")) %>%
        filter(YEAR..CODE. == 2012) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               SEX..CODE.,
               Numeric) %>%
        filter(SEX..CODE. == "BTSX") %>%
        arrange(Numeric)

names(deaths.all.ncd) <- tolower(names(deaths.all.ncd))

rm(dta)

# Deaths pr. 100,000: Cancer --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A864?lang=en

dta <- read.csv("./who_cancer_deaths.csv",
                stringsAsFactors = FALSE)

deaths.cancer <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                                       "Algeria",
                                                       "Egypt",
                                                       "Libya",
                                                       "Morocco",
                                                       "Sudan",
                                                       "Tunisia",
                                                       "Bahrain",
                                                       "United Arab Emirates",
                                                       "Iraq",
                                                       "Iran",
                                                       "Jordan",
                                                       "Kuwait",
                                                       "Lebanon",
                                                       "Oman",
                                                       "Saudi Arabia",
                                                       "Syrian Arab Republic",
                                                       "Turkey",
                                                       "Yemen",
                                                       "Qatar")) %>%
        filter(YEAR..CODE. == 2012) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               Numeric) %>%
        arrange(Numeric)

names(deaths.cancer) <- tolower(names(deaths.cancer))

rm(dta)

# Deaths pr. 100,000: Cardiovascular --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A865CARDIOVASCULAR?lang=en

dta <- read.csv("./who_cardiovas_deaths.csv",
                stringsAsFactors = FALSE)

deaths.cvd <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                                       "Algeria",
                                                       "Egypt",
                                                       "Libya",
                                                       "Morocco",
                                                       "Sudan",
                                                       "Tunisia",
                                                       "Bahrain",
                                                       "United Arab Emirates",
                                                       "Iraq",
                                                       "Iran",
                                                       "Jordan",
                                                       "Kuwait",
                                                       "Lebanon",
                                                       "Oman",
                                                       "Saudi Arabia",
                                                       "Syrian Arab Republic",
                                                       "Turkey",
                                                       "Yemen",
                                                       "Qatar")) %>%
        filter(YEAR..CODE. == 2012) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               Numeric) %>%
        arrange(Numeric)

names(deaths.cvd) <- tolower(names(deaths.cvd))

rm(dta)

# Deaths pr. 100,000: Cronic respiratory disease --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A866?lang=en

dta <- read.csv("./who_crd_deaths.csv",
                stringsAsFactors = FALSE)

deaths.crd <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                                       "Algeria",
                                                       "Egypt",
                                                       "Libya",
                                                       "Morocco",
                                                       "Sudan",
                                                       "Tunisia",
                                                       "Bahrain",
                                                       "United Arab Emirates",
                                                       "Iraq",
                                                       "Iran",
                                                       "Jordan",
                                                       "Kuwait",
                                                       "Lebanon",
                                                       "Oman",
                                                       "Saudi Arabia",
                                                       "Syrian Arab Republic",
                                                       "Turkey",
                                                       "Yemen",
                                                       "Qatar")) %>%
        filter(YEAR..CODE. == 2012) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               Numeric) %>%
        arrange(Numeric)

names(deaths.crd) <- tolower(names(deaths.crd))

rm(dta)

# Deaths pr. 100,000: Diabetes --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A865DIABETES?lang=en

dta <- read.csv("./who_diabetes_deaths.csv",
                stringsAsFactors = FALSE)

deaths.diabe <- filter(dta, COUNTRY..DISPLAY. %in% c("Denmark",
                                                       "Algeria",
                                                       "Egypt",
                                                       "Libya",
                                                       "Morocco",
                                                       "Sudan",
                                                       "Tunisia",
                                                       "Bahrain",
                                                       "United Arab Emirates",
                                                       "Iraq",
                                                       "Iran",
                                                       "Jordan",
                                                       "Kuwait",
                                                       "Lebanon",
                                                       "Oman",
                                                       "Saudi Arabia",
                                                       "Syrian Arab Republic",
                                                       "Turkey",
                                                       "Yemen",
                                                       "Qatar")) %>%
        filter(YEAR..CODE. == 2012) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               Numeric) %>%
        arrange(Numeric)

names(deaths.diabe) <- tolower(names(deaths.diabe))

rm(dta)
