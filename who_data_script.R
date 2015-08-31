# Global health observatory data repository & World DataBank
# Accessed July 30. 2015

# Use setwd() to go to the directory where the data has been stored. Alternatively
# data can be handled online though I have chosen to download the data for
# later documentation.

# The source of the data (URL) is given for each variable. They were all accessed and
# data downloaded on the date given above.

library(dplyr)
library(tidyr)


# Selecting low and middle income Middle Eastern countries ----------------
# http://data.worldbank.org/about/country-and-lending-groups
# Accessed 26. August 2015

# To limit variation within in the study inclusion criteria are adults from 
# low and middle income countries in the region defined by The World Bank as 
# "Middle East & North Africa"

countries <- read.csv2("./country_class.csv") %>%
        select(- c(1, 2, 5, 8:11)) %>%
        slice(-1) %>%
        filter(Region == "Middle East & North Africa") %>%
        filter(!grepl("^[Hh]igh", .$Income.group)) %>%
        sapply(., gsub, pattern = ",.*", replacement = "")
        
# Thus, adults from the following countries are included
#[1] "Algeria"              "Djibouti"             "Egypt"               
#[4] "Iran"                 "Iraq"                 "Jordan"              
#[7] "Lebanon"              "Libya"                "Morocco"             
#[10] "Syrian Arab Republic" "Tunisia"              "West Bank and Gaza"  
#[13] "Yemen"

# Population census of selected countries in 2014 --------------------------------
# http://databank.worldbank.org/data/reports.aspx?Code=SP.POP.TOTL&id=af3ce82b&report_name=Popular_indicators&populartype=series&ispopular=y#
pop <- read.csv("./pop.tot.csv") %>%
        gather(year, freq, -c(1:4)) %>%
        setNames(tolower(names(.))) %>%
        mutate(country.name = gsub(", Islamic Rep.", "", fixed = TRUE,
                                   .$country.name)) %>%
        mutate(country.name = gsub(", Arab Rep.", "", fixed = TRUE,
                                   .$country.name)) %>%
        mutate(country.name = gsub(", Rep.", "", fixed = TRUE,
                                   .$country.name)) %>%
        filter(country.name %in% countries[, 1])
pop <- data.frame(sapply(pop, gsub, pattern = "^X", replacement = "")) %>%
        mutate(freq = as.numeric(levels(freq)[freq])) %>%
        group_by(country.name) %>%
        summarise(pop14 = max(freq, na.rm = TRUE))

# Funktion: Risikofaktorer - mean --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A867?lang=en

RiskFactor <- function(path) {
        t <- read.csv(path, stringsAsFactor = FALSE) %>%
                mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                                "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
                filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
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

# Funktion: Risikofaktorer - prevalence --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A867?lang=en
# The prevalence is defined as the percent of defined population with the risk factor

RiskFactor <- function(path) {
        t <- read.csv(path, stringsAsFactor = FALSE) %>%
                mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                                "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
                filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
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
rsd.gluc[, c(5, 10)]

#      country..display.    display.value
#1               Denmark    5.2 [2.5-7.8]
#2              Djibouti   8.7 [4.2-13.5]
#3                  Iran  12.2 [7.3-16.8]
#4               Lebanon  12.6 [7.1-17.8]
#5               Tunisia  13.3 [8.0-18.4]
#6               Morocco  13.5 [7.6-19.0]
#7  Syrian Arab Republic  13.9 [8.3-19.2]
#8               Algeria  14.2 [8.7-19.6]
#9                Jordan  14.9 [8.8-21.0]
#10                Yemen  15.5 [9.4-22.5]
#11                 Iraq 16.8 [11.0-22.7]
#12                Libya 17.0 [10.7-23.1]
#13                Egypt 18.9 [12.5-25.3]


# An estimate of the combined prevalence in the selected countries

rsd.gluc.prev.tot <- left_join(pop, rsd.gluc, by = c("country.name" = "country..display.")) %>%
        mutate(prev = numeric / 100) %>%
        group_by(country.name) %>%
        summarise(cases = pop14 * prev, pop = pop14) %>%
        ungroup() %>%
        na.omit() %>%
        summarise(tot.cases = sum(cases), tot.pop = sum(pop))
summarise(rsd.gluc.prev.tot, pre.tot = tot.cases / tot.pop)
# prev.tot
# 1 0.1514005

# Raised total cholesterol: cholesterol >= 190 mg/dl (5.0 mmol/L)
rsd.clstrl <- RiskFactor("./who_raised_tot_choles_5or_above.csv") %>%
        filter(sex..code. == "BTSX") %>%
        filter(gho..display. == "Raised total cholesterol (>= 5.0 mmol/L) (age-standardized estimate)")

# Probability of dying from any NCD ---------------------------------------
# http://apps.who.int/gho/data/node.main.A857?lang=en

prob.dying.ncd <- read.csv("./who_risk_of_death_ncd.csv",
                           stringsAsFactor = FALSE) %>%
        mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
        filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
        filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               Numeric) %>%
        arrange(Numeric)

names(prob.dying.ncd) <- tolower(names(prob.dying.ncd))

# qplot(country..code., numeric, data = deaths.all.ncd)

# Prevalence: tobacco use -------------------------------------------------
# http://apps.who.int/gho/data/node.main.1250?lang=en

tobac <- read.csv("./who_preval_tobacco.csv",
                  stringsAsFactor = FALSE) %>%
        mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
        filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark"))  %>%
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

# Distribution of years of life lost (%) ----------------------------------
# http://apps.who.int/gho/data/node.main.21?lang=en

yll <- read.csv("./who_distribution_of_years_of_life_lost.csv",
                stringsAsFactor = FALSE) %>%
        mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
        filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
        filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               GHECAUSES..DISPLAY.,
               Numeric) %>%
        arrange(GHECAUSES..DISPLAY., Numeric)

names(yll) <- tolower(names(yll))

# Mortality rate pr. 100,000, age-standardised, 3 grp. --------------------
# http://apps.who.int/gho/data/node.main.18?lang=en

mort.rate <- read.csv("./who_death_rate_major_groups.csv",
                      stringsAsFactor = FALSE) %>%
        mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
        filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
        filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               GHECAUSES..DISPLAY.,
               Numeric) %>%
        arrange(GHECAUSES..DISPLAY., Numeric)

names(mort.rate) <- tolower(names(mort.rate))

# DALYs -------------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.DALYCTRY?lang=en

dta <- read.csv("./who_daly.csv",
                stringsAsFactors = FALSE)

daly <- read.csv("./who_daly.csv",
                 stringsAsFactor = FALSE) %>%
        mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
        filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
        filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               GHECAUSES..DISPLAY.,
               Numeric) %>%
        arrange(GHECAUSES..DISPLAY., Numeric)

names(daly) <- tolower(names(daly))

# Number of reported cases of selected CDs --------------------------------
# http://apps.who.int/gho/data/node.main.31?lang=en

nbr.cd <- read.csv("./who_CD_number_of_cases.csv",
                   stringsAsFactor = FALSE) %>%
        mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
        filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
        filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
        select(GHO..DISPLAY.,
               YEAR..CODE.,
               REGION..CODE.,
               COUNTRY..CODE.,
               COUNTRY..DISPLAY.,
               Numeric) %>%
        arrange(GHO..DISPLAY., Numeric)

names(nbr.cd) <- tolower(names(nbr.cd))

d <- group_by(nbr.cd, gho..display.) %>%
        filter(! country..code. == "DNK") %>%
        summarise(nbr = sum(numeric, na.rm = TRUE)) %>%
        arrange(- nbr)

# Mortality + prevalence: Tuberculosis ------------------------------------
# http://apps.who.int/gho/data/node.main.1317?lang=en
# The prevalence is defined as cases per 100,000 populations

tb <- read.csv("./who_tb.csv",
               stringsAsFactor = FALSE) %>%
        mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
        filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
        filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
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

# An estimate of the combined prevalence in the selected countries

tb.prev.tot <- filter(tb, !country..code. == "DNK") %>%
        group_by(gho..display.) %>%
        summarise(median(numeric), IQR(numeric))

# Deaths pr. 100,000 all NCDs --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A863?lang=en

deaths.all.ncd <- read.csv("./who_all_ncd_deaths.csv",
                stringsAsFactors = FALSE) %>%
                mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
                filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
                filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
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

# Deaths pr. 100,000: Cancer --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A864?lang=en

deaths.cancer <- read.csv("./who_all_ncd_deaths.csv",
                stringsAsFactors = FALSE) %>%
                mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
                filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
                filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
                select(GHO..DISPLAY.,
                        YEAR..CODE.,
                        REGION..CODE.,
                        COUNTRY..CODE.,
                        COUNTRY..DISPLAY.,
                        Numeric) %>%
                arrange(Numeric)

names(deaths.cancer) <- tolower(names(deaths.cancer))

# Deaths pr. 100,000: Cardiovascular --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A865CARDIOVASCULAR?lang=en

deaths.cvd <- read.csv("./who_all_ncd_deaths.csv",
                stringsAsFactors = FALSE) %>%
                mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
                filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
                filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
                select(GHO..DISPLAY.,
                        YEAR..CODE.,
                        REGION..CODE.,
                        COUNTRY..CODE.,
                        COUNTRY..DISPLAY.,
                        Numeric) %>%
                arrange(Numeric)

names(deaths.cvd) <- tolower(names(deaths.cvd))

# Deaths pr. 100,000: Cronic respiratory disease --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A866?lang=en

deaths.crd <- read.csv("./who_all_ncd_deaths.csv",
                stringsAsFactors = FALSE) %>%
                mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
                filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
                filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
                select(GHO..DISPLAY.,
                        YEAR..CODE.,
                        REGION..CODE.,
                        COUNTRY..CODE.,
                        COUNTRY..DISPLAY.,
                        Numeric) %>%
                arrange(Numeric)

names(deaths.crd) <- tolower(names(deaths.crd))

# Deaths pr. 100,000: Diabetes --------------------------------------------------------------
# http://apps.who.int/gho/data/node.main.A865DIABETES?lang=en

deaths.diabe <- read.csv("./who_all_ncd_deaths.csv",
                stringsAsFactors = FALSE) %>%
                mutate(COUNTRY..DISPLAY. = gsub(" (Islamic Republic of)",
                                        "", fixed = TRUE, .$COUNTRY..DISPLAY.)) %>%
                filter(COUNTRY..DISPLAY. %in% c(countries[, 1], "Denmark")) %>%
                filter(YEAR..CODE. == max(YEAR..CODE.)) %>%
                select(GHO..DISPLAY.,
                        YEAR..CODE.,
                        REGION..CODE.,
                        COUNTRY..CODE.,
                        COUNTRY..DISPLAY.,
                        Numeric) %>%
                arrange(Numeric)

names(deaths.diabe) <- tolower(names(deaths.diabe))
