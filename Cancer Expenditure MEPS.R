#################################################################################
# Title:          Cancer Expenditure Project - MEMPS 2017                       #
# Programmer:     Olivia Yip, adapted from Aryana Sepassi                       #
# Date:           31 July 2023.                                                 #
# Updated:        NA                                                            #
# Updated by:     NA                                                            #
# Notes:          Contains function and loops for drug categories               #
#################################################################################

#Part 1 - Pull main HC data of all respondents in 2016
#Part 2 - Prescription drug file: Pull data on study drug class prescriptions and number of fills/other data for each drug class. 

################################################################################
# Import libraries
################################################################################
setwd("/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Cancer Expenditure Project")

library(dplyr)            #For mutate function/data manipulation (required)
library(tidyr)            #For miscellaneous data manipulation functions 
library(maditr)           #For dcast function (to reshape data from long to wide)
library(haven)            #To import SAS file extension format into R 
library(foreign)
library(cobalt)           #For covariate balance matching tables and plots 

options(scipen =999)      #Removes scientific notation from R output (easier to interpret)

################################################################################
# Household - 2017 Full-Year Consolidated Data File | HC-201                   # 
################################################################################

#Starting in 2017, AHRQ has provided SAS V9 Format Files to Import 

hc201 <- read_sas("h201.sas7bdat")

#save(hc201, file="/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Cancer Expenditure Project/h201.Rdata")

#Load Rdata file (after first time executing code)---------------------------------------------------------------------------------------------------


#load(file="h201.Rdata")

#Convert all variable names to lowercase 
names(hc201) <- tolower(names(hc201))


#Cohort Flag Creation---------------------------------------------------------------------------------------------------------------------------------

#Flag: Age 18 or older: 
hc201 <- hc201 %>% 
  mutate(adult = ifelse(age17x >= 18, 1, 0))

table(hc201$adult)

#Flag: Cancer Diagnosis (CCRNDI31 = yes --> cancer diagnosis by health professional)
hc201 <- hc201 %>% 
  mutate(cancer = ifelse(ccnrdi31 == "1",1,
                         ifelse(hc201$ccnrdi31==-1,0,NA)))

table(hc201$cancer, useNA='always') 
table(hc201$cancer,hc201$adult)

#Subset if Necessary (this should be performed AFTER propensity score matching): 
#hc201 <- subset(hc201, age17x >= 18) 
#hc201 <- subset(hc201, cancer==1)

#This resulting dataset should only include those age 18 or older AND were told by a healthcare provider that they had cancer (existing or previous)


###### Demographics -------------------------------------------------------------------------------

#Gender: 
table(hc201$sex, useNA="always")  #1: Male, 2:Female
table(hc201$sex,hc201$cancer)
chisq.test(table(hc201$sex,hc201$cancer), correct=FALSE)

# Age
summary(hc201$age17x) #AGE AS OF 12/31/17 
sd(hc201$age17x) 


####Age should also be coded as a categorical variable: 
hc201 <- hc201 %>% 
  mutate(age_cat = ifelse(age17x>17 & age17x<25,1,
                          ifelse(age17x>24 & age17x<45,2,
                                 ifelse(age17x>44 & age17x<65,3,
                                        ifelse(age17x>64 & age17x<100,4,NA)))))
table(hc201$age_cat)
#Categories: 1 = Age 18-24 years 
            #2 = Age 25-44 years 
            #3 = Age 45-64 years 
            #4 = Age 65+ years 


###### Cancer History
table(hc201$ctrtmt31, useNA="always") # Currently Treated for Cancer

table(hc201$clstrt31, useNA="always") # How long ago received last cancer treatment

table(hc201$cbck31, useNA="always") # Doctor or health professional ever told you cancer had come back 


##### Race/Ethnicity:
table(hc201$racev1x, useNA="always")

chisq.test(table(hc201$racev1x,hc201$ccnrdi31), correct=FALSE)

#ASIAN AMONG RACES 
table(hc201$raceax, useNA="always")
table(hc201cancer$raceax, useNA="always")

#BLACK AMONG RACES
table(hc201$racebx, useNA="always")
table(hc201cancer$racebx, useNA="always")

#WHITE AMONG RACES
table(hc201$racewx, useNA='always')
table(hc201cancer$racewx, useNA='always')

#HISPANIC ETHNICITY
table(hc201$hispanx, useNA="always")   #Hispanic yes/no 
table(hc201cancer$hispanx, useNA="always")


##### Martial Status 
table(hc201$marry17x, useNA="always")
table(hc201cancer$marry17x, useNA="always")

#-8 dont know, -7 refused, 
#1 married 
#2 widowed, divorced, separated, never married 
#3 under 16 - inapplicable 

#Recode marital status into a variable that condenses categories: 
hc201 <- hc201 %>% 
  mutate(marritalstatus = ifelse(marry17x==1,1,
                            ifelse((marry17x==2|marry17x==3|marry17x==4|marry17x==5),2,
                              ifelse((marry17x==-8|marry17x==-7|marry17x==3),3,NA))))

hc201$marritalstatus <- factor(hc201$marritalstatus,
                               levels=c(1,2,3),
                               labels=c("1 MARRIED",
                                        "2 NOT MARRIED",
                                        "3 NOT APPLICABLE"))

table(hc201$marritalstatus) 

chisq.test(table(hc201$marry17x,hc201$ccnrdi31), correct=FALSE)

##### Education 

#Recode education variables to generate fewer categories: 
table(hc201$educyr, useNA="always")         #Year of education when first entered MEPS 
table(hc201$hideg, useNA="always")         #Starting in 2016: Highest Degree 

hc201 <- hc201 %>%
  mutate(neweducode=ifelse((hideg==1 | hideg==2 | hideg==3),1,
                                  ifelse(hideg==4,2,
                                         ifelse((hideg==5|hideg==6|hideg==7),3,
                                                ifelse((hideg==-9|hideg==-8|hideg==-7|hideg==8),4,NA)))))
table(hc201$neweducode, useNA='always')

hc201$neweducode <- factor(hc201$neweducode,
                           levels=c(1,2,3,4),
                           labels=c("1 HS OR LESS",
                                    "2 BS",
                                    "3 GRADUATE LEVEL DEGREE",
                                    "4 NOT APPLICABLE"))

table(hc201$neweducode, useNA='always')

chisq.test(table(hc201$neweducode,hc201$ccnrdi31), correct=FALSE)

#### Geographic Location at end of year

hc201$region17 <- factor(hc201$region17,
                         levels=c(-1, 1, 2, 3, 4),
                         labels=c("-1 Inapplicable",
                                  "1 Northeast",
                                  "2 Midwest",
                                  "3 South",
                                  "4 West"))

table(hc201$region17, useNA="always")

table(hc201cancer$region17, useNA="always")

chisq.test(table(hc201$region17,hc201$ccnrdi31), correct=FALSE)


##### Poverty status, end of year 

hc201$povcat17 <- factor(hc201$povcat17,
                         levels=c(1, 2, 3, 4, 5),
                         labels=c("1 Poor/negative",
                                  "2 Near poor",
                                  "3 Low income",
                                  "4 middle income",
                                  "5 high income"))

table(hc201$povcat17, useNA="always")
table(hc201cancer$povcat17, useNA="always")

chisq.test(table(hc201$povcat17,hc201$ccnrdi31), correct=FALSE)


##### Any insurance coverage 
hc201$inscov17 <- factor(hc201$inscov17,
                         levels=c(1,2,3),
                         labels=c("1 Any private",
                                  "2 Public only",
                                  "3 Uninsured"))

table(hc201$inscov17, useNA="always")
table(hc201cancer$inscov17, useNA="always")

chisq.test(table(hc201$inscov17,hc201$ccnrdi31), correct=FALSE)


##### insurance coverage - CSAQ
hc201$cincov31 <- factor(hc201$cincov31,
                         levels=c(-9,-1,1,2,3),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No",
                                  "3 Dont know"))

table(hc201$cincov31, useNA="always")

##### private insurance - CSAQ

hc201$cinprv31 <- factor(hc201$cinprv31,
                         levels=c(-9,-1,1,2),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cinprv31, useNA="always")


##### health insurance is Medicare - CSAQ

hc201$cinmdc31 <- factor(hc201$cinmdc31,
                         levels=c(-9,-1,1,2),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cinmdc31, useNA="always")


##### health insurance is Medicaid - CSAQ

hc201$cinmda31 <- factor(hc201$cinmda31,
                         levels=c(-9,-1,1,2),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cinmda31, useNA="always")

##### health insurance is Military - CSAQ


hc201$cinmlt31 <- factor(hc201$cinmlt31,
                         levels=c(-9,-1,1,2),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cinmlt31, useNA="always")


##### health insurance is State-sponsored health plan - CSAQ


hc201$cinshp31 <- factor(hc201$cinshp31,
                         levels=c(-9,-1,1,2),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cinshp31, useNA="always")

##### health insurance is other government program - CSAQ

hc201$cinogp31 <- factor(hc201$cinogp31,
                         levels=c(-9,-1,1,2),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cinogp31, useNA="always")

##### Cancer Diagnosed - Bladder (>17) 

hc201$cabladdr <- factor(hc201$cabladdr,
                         levels=c(-8,-1,1,2),
                         labels=c("-8 Not ascertained",
                                  "-1 Dont know",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cabladdr, useNA="always")

##### Cancer Diagnosed - Breast (>17) 

hc201$cabreast <- factor(hc201$cabreast,
                         levels=c(-8,-1,1,2),
                         labels=c("-8 Not ascertained",
                                  "-1 Dont know",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cabreast, useNA="always")

##### Cancer Diagnosed - Cervix (>17) 

hc201$cacervix <- factor(hc201$cacervix,
                         levels=c(-8,-1,1,2),
                         labels=c("-8 Not ascertained",
                                  "-1 Dont know",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cacervix, useNA="always")

##### Cancer Diagnosed - Colon (>17) 

hc201$cacolon <- factor(hc201$cacolon,
                         levels=c(-8,-1,1,2),
                         labels=c("-8 Not ascertained",
                                  "-1 Dont know",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cacolon, useNA="always")

##### Cancer Diagnosed - Lung (>17) 


hc201$calung <- factor(hc201$calung,
                        levels=c(-8,-1,1,2),
                        labels=c("-8 Not ascertained",
                                 "-1 Dont know",
                                 "1 Yes",
                                 "2 No"))

table(hc201$calung, useNA="always")

##### Cancer Diagnosed - Lymphoma (>17) 
hc201$calymph <- factor(hc201$calymph,
                       levels=c(-8,-1,1,2),
                       labels=c("-8 Not ascertained",
                                "-1 Dont know",
                                "1 Yes",
                                "2 No"))

table(hc201$calymph, useNA="always")


##### Cancer Diagnosed - Melanoma (>17) 

hc201$camelano <- factor(hc201$camelano,
                        levels=c(-8,-1,1,2),
                        labels=c("-8 Not ascertained",
                                 "-1 Dont know",
                                 "1 Yes",
                                 "2 No"))

table(hc201$camelano, useNA="always")

##### Cancer Diagnosed - Prostate (>17) 

hc201$caprosta <- factor(hc201$caprosta,
                         levels=c(-8,-1,1,2),
                         labels=c("-8 Not ascertained",
                                  "-1 Dont know",
                                  "1 Yes",
                                  "2 No"))

table(hc201$caprosta, useNA="always")

##### The Effects of Cancer and Its Treatment on Finances 
# Medical expenses paid out of pocket 
hc201$cncmed31 <- factor(hc201$cncmed31,
                         levels=c(-9,-1,1,2),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cncmed31, useNA="always")

# Had no expenses paid out of pocket

hc201$cncnon31 <- factor(hc201$cncnon31,
                         levels=c(-9,-1,1,2),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cncnon31, useNA="always")

# Unable to cover cost of medical care visits

hc201$cfnunb31 <- factor(hc201$cfnunb31,
                         levels=c(-9,-1,1,2),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cfnunb31, useNA="always")

# You or family member ever filed for bankruptcy

hc201$cfnbnk31 <- factor(hc201$cfnbnk31,
                         levels=c(-9,-1,1,2),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes",
                                  "2 No"))

table(hc201$cfnbnk31, useNA="always")


# Doctor ever discussed cost for cancer paid out of your own pocket 
hc201$cmcost31 <- factor(hc201$cmcost31,
                         levels=c(-9,-1,1,2,3,4),
                         labels=c("-9 Not ascertained",
                                  "-1 N/A",
                                  "1 Yes, Discussed in detail",
                                  "2 briefly discussed",
                                  "3 did not discuss",
                                  "4 do not remember"))


table(hc201$cmcost31, useNA="always")


##### Total expenditures 
summary(hc201$totexp17)
sd(hc201$totexp17)

# calymph (lymphoma)
summary(hc201$totexp17[hc201$calymph==1]) 
sd(hc201$totexp17[hc201$calymph==1]) 

# camelano (melanoma)
summary(hc201$totexp17[hc201$camelano==1]) 
sd(hc201$totexp17[hc201$camelano==1]) 

# caprosta (prostate)
summary(hc201$totexp17[hc201$caprosta==1]) 
sd(hc201$totexp17[hc201$caprosta==1]) 

# cabladdr (bladder)
summary(hc201$totexp17[hc201$cabladdr==1]) 
sd(hc201$totexp17[hc201$cabladdr==1]) 

# cabreast (Breast)
summary(hc201$totexp17[hc201$cabreast==1]) 
sd(hc201$totexp17[hc201$cabreast==1]) 

# cacervix (CERVIX)
summary(hc201$totexp17[hc201$cacervix==1]) 
sd(hc201$totexp17[hc201$cacervix==1]) 

# cacolon (COLON)
summary(hc201$totexp17[hc201$cacolon==1]) 
sd(hc201$totexp17[hc201$cacolon==1]) 

# calung (LUNG)
summary(hc201$totexp17[hc201$calung==1]) 
sd(hc201$totexp17[hc201$calung==1]) 

# cauterus (uterus)
summary(hc201$totexp17[hc201$cauterus==1]) 
sd(hc201$totexp17[hc201$cauterus==1])

##### Rx total expenditures 
summary(hc201$rxtot17)      #Number of prescribed medications, including refills 
summary(hc201$rxexp17)      #Total Rx expenditure
sd(hc201$rxexp17)

# calymph (lymphoma)
summary(hc201$rxexp17[hc201$calymph==1]) 
sd(hc201$rxexp17[hc201$calymph==1]) 

# camelano (melanoma)
summary(hc201$rxexp17[hc201$camelano==1]) 
sd(hc201$rxexp17[hc201$camelano==1]) 

# caprosta (prostate)
summary(hc201$rxexp17[hc201$caprosta==1]) 
sd(hc201$rxexp17[hc201$caprosta==1]) 

# cabladdr (bladder)
summary(hc201$rxexp17[hc201$cabladdr==1]) 
sd(hc201$rxexp17[hc201$cabladdr==1]) 

# cabreast (Breast)
summary(hc201$rxexp17[hc201$cabreast==1]) 
sd(hc201$rxexp17[hc201$cabreast==1]) 

# cacervix (CERVIX)
summary(hc201$rxexp17[hc201$cacervix==1]) 
sd(hc201$rxexp17[hc201$cacervix==1]) 

# cacolon (COLON)
summary(hc201$rxexp17[hc201$cacolon==1]) 
sd(hc201$rxexp17[hc201$cacolon==1]) 

# calung (LUNG)
summary(hc201$rxexp17[hc201$calung==1]) 
sd(hc201$rxexp17[hc201$calung==1]) 

# cauterus (uterus)
summary(hc201$rxexp17[hc201$cauterus==1]) 
sd(hc201$rxexp17[hc201$cauterus==1])


#### Office-Based Visits 
summary(hc201$obtotv17)    #Total n, office-based visits reported 
sd(hc201$obtotv17)

# calymph (lymphoma)
summary(hc201$obtotv17[hc201$calymph==1]) 
sd(hc201$obtotv17[hc201$calymph==1]) 

# camelano (melanoma)
summary(hc201$obtotv17[hc201$camelano==1]) 
sd(hc201$obtotv17[hc201$camelano==1]) 

# caprosta (prostate)
summary(hc201$obtotv17[hc201$caprosta==1]) 
sd(hc201$obtotv17[hc201$caprosta==1]) 

# cabladdr (bladder)
summary(hc201$obtotv17[hc201$cabladdr==1]) 
sd(hc201$obtotv17[hc201$cabladdr==1]) 

# cabreast (Breast)
summary(hc201$obtotv17[hc201$cabreast==1]) 
sd(hc201$obtotv17[hc201$cabreast==1]) 

# cacervix (CERVIX)
summary(hc201$obtotv17[hc201$cacervix==1]) 
sd(hc201$obtotv17[hc201$cacervix==1]) 

# cacolon (COLON)
summary(hc201$obtotv17[hc201$cacolon==1]) 
sd(hc201$obtotv17[hc201$cacolon==1]) 

# calung (LUNG)
summary(hc201$obtotv17[hc201$calung==1]) 
sd(hc201$obtotv17[hc201$calung==1]) 

# cauterus (uterus)
summary(hc201$obtotv17[hc201$cauterus==1]) 
sd(hc201$obtotv17[hc201$cauterus==1])

summary(hc201$obdrv17)     #Total n of visits to physicians
sd(hc201$obdrv17)

# calymph (lymphoma)
summary(hc201$obdrv17[hc201$calymph==1]) 
sd(hc201$obdrv17[hc201$calymph==1]) 

# camelano (melanoma)
summary(hc201$obdrv17[hc201$camelano==1]) 
sd(hc201$obdrv17[hc201$camelano==1]) 

# caprosta (prostate)
summary(hc201$obdrv17[hc201$caprosta==1]) 
sd(hc201$obdrv17[hc201$caprosta==1]) 

# cabladdr (bladder)
summary(hc201$obdrv17[hc201$cabladdr==1]) 
sd(hc201$obdrv17[hc201$cabladdr==1]) 

# cabreast (Breast)
summary(hc201$obdrv17[hc201$cabreast==1]) 
sd(hc201$obdrv17[hc201$cabreast==1]) 

# cacervix (CERVIX)
summary(hc201$obdrv17[hc201$cacervix==1]) 
sd(hc201$obdrv17[hc201$cacervix==1]) 

# cacolon (COLON)
summary(hc201$obdrv17[hc201$cacolon==1]) 
sd(hc201$obdrv17[hc201$cacolon==1]) 

# calung (LUNG)
summary(hc201$obdrv17[hc201$calung==1]) 
sd(hc201$obdrv17[hc201$calung==1]) 

# cauterus (uterus)
summary(hc201$obdrv17[hc201$cauterus==1]) 
sd(hc201$obdrv17[hc201$cauterus==1])


summary(hc201$optexp17) #Total office-based visit expenditure
sd(hc201$optexp17)

# calymph (lymphoma)
summary(hc201$optexp17[hc201$calymph==1]) 
sd(hc201$optexp17[hc201$calymph==1]) 

# camelano (melanoma)
summary(hc201$optexp17[hc201$camelano==1]) 
sd(hc201$optexp17[hc201$camelano==1]) 

# caprosta (prostate)
summary(hc201$optexp17[hc201$caprosta==1]) 
sd(hc201$optexp17[hc201$caprosta==1]) 

# cabladdr (bladder)
summary(hc201$optexp17[hc201$cabladdr==1]) 
sd(hc201$optexp17[hc201$cabladdr==1]) 

# cabreast (Breast)
summary(hc201$optexp17[hc201$cabreast==1]) 
sd(hc201$optexp17[hc201$cabreast==1]) 

# cacervix (CERVIX)
summary(hc201$optexp17[hc201$cacervix==1]) 
sd(hc201$optexp17[hc201$cacervix==1]) 

# cacolon (COLON)
summary(hc201$optexp17[hc201$cacolon==1]) 
sd(hc201$optexp17[hc201$cacolon==1]) 

# calung (LUNG)
summary(hc201$optexp17[hc201$calung==1]) 
sd(hc201$optexp17[hc201$calung==1]) 

# cauterus (uterus)
summary(hc201$optexp17[hc201$cauterus==1]) 
sd(hc201$optexp17[hc201$cauterus==1])


#### Hospital Outpatient Visits 
summary(hc201$optotv17)    #Total n, visits to hospital outpatient departments 
summary(hc201$opdrv17)     #Number of these visits to physicians 


#### Hospital ED Visits 
summary(hc201$ertot17)     #Total count of all ED visits reported  
summary(hc201$ertexp17)    #Total ED visit expenditures  
sd(hc201$ertexp17) 

# calymph (lymphoma)
summary(hc201$ertexp17[hc201$calymph==1]) 
sd(hc201$ertexp17[hc201$calymph==1]) 

# camelano (melanoma)
summary(hc201$ertexp17[hc201$camelano==1]) 
sd(hc201$ertexp17[hc201$camelano==1]) 

# caprosta (prostate)
summary(hc201$ertexp17[hc201$caprosta==1]) 
sd(hc201$ertexp17[hc201$caprosta==1]) 

# cabladdr (bladder)
summary(hc201$ertexp17[hc201$cabladdr==1]) 
sd(hc201$ertexp17[hc201$cabladdr==1]) 

# cabreast (Breast)
summary(hc201$ertexp17[hc201$cabreast==1]) 
sd(hc201$ertexp17[hc201$cabreast==1]) 

# cacervix (CERVIX)
summary(hc201$ertexp17[hc201$cacervix==1]) 
sd(hc201$ertexp17[hc201$cacervix==1]) 

# cacolon (COLON)
summary(hc201$ertexp17[hc201$cacolon==1]) 
sd(hc201$ertexp17[hc201$cacolon==1]) 

# calung (LUNG)
summary(hc201$ertexp17[hc201$calung==1]) 
sd(hc201$ertexp17[hc201$calung==1]) 

# cauterus (uterus)
summary(hc201$ertexp17[hc201$cauterus==1]) 
sd(hc201$ertexp17[hc201$cauterus==1])

### Hospital Inpatient Stays 
summary(hc201$ipdis17)     #Total number of hospital discharges 
summary(hc201$ipngtd17)    #Total number of nights associated with these discharges 
summary(hc201$iptexp17)    #Total inpatient stays, expenditures 
sd(hc201$iptexp17) 

# calymph (lymphoma)
summary(hc201$iptexp17[hc201$calymph==1]) 
sd(hc201$iptexp17[hc201$calymph==1]) 

# camelano (melanoma)
summary(hc201$iptexp17[hc201$camelano==1]) 
sd(hc201$iptexp17[hc201$camelano==1]) 

# caprosta (prostate)
summary(hc201$iptexp17[hc201$caprosta==1]) 
sd(hc201$iptexp17[hc201$caprosta==1]) 

# cabladdr (bladder)
summary(hc201$iptexp17[hc201$cabladdr==1]) 
sd(hc201$iptexp17[hc201$cabladdr==1]) 

# cabreast (Breast)
summary(hc201$iptexp17[hc201$cabreast==1]) 
sd(hc201$iptexp17[hc201$cabreast==1]) 

# cacervix (CERVIX)
summary(hc201$iptexp17[hc201$cacervix==1]) 
sd(hc201$iptexp17[hc201$cacervix==1]) 

# cacolon (COLON)
summary(hc201$iptexp17[hc201$cacolon==1]) 
sd(hc201$iptexp17[hc201$cacolon==1]) 

# calung (LUNG)
summary(hc201$iptexp17[hc201$calung==1]) 
sd(hc201$iptexp17[hc201$calung==1]) 

# cauterus (uterus)
summary(hc201$iptexp17[hc201$cauterus==1]) 
sd(hc201$iptexp17[hc201$cauterus==1])


##### Create a new, final dataset 

hc201c <- subset(hc201, select=c(age_cat, age17x, sex, ccnrdi31, ctrtmt31, clstrt31, cbck31, cbckyr31, cftrt31, racev1x, hispanx,
                                 raceax, racebx, racewx, educyr, region17, povcat17, marritalstatus, inscov17, cincov31,
                                 cinprv31, cinmdc31, cinmda31, cinmlt31, cinshp31, cinogp31, cabladdr, cabreast, cacervix, cacolon,
                                 calung, calymph, camelano, caprosta, cncmed31, cncnon31, cfnunb31, cfnbnk31, cmcost31, neweducode,totexp17,rxexp17,
                                 obtotv17, obdrv17, optexp17, optotv17, ipdis17, ipngtd17,iptexp17,cancer, dupersid))

#### Rename hc201c variables to common names 
names(hc201c)[which(names(hc201c)=="age17x")] <- "age" 
names(hc201c)[which(names(hc201c)=="region17")] <- "region" 
names(hc201c)[which(names(hc201c)=="povcat17")] <- "povcat" 
names(hc201c)[which(names(hc201c)=="marry17x")] <- "marry"
names(hc201c)[which(names(hc201c)=="totexp17")] <- "totexp" 
names(hc201c)[which(names(hc201c)=="rxtot17")] <- "rxtot" 
names(hc201c)[which(names(hc201c)=="rxexp17")] <- "rxexp" 
names(hc201c)[which(names(hc201c)=="obtotv17")] <- "obtotv"
names(hc201c)[which(names(hc201c)=="obdrv17")] <- "obdrv"
names(hc201c)[which(names(hc201c)=="optexp17")] <- "optexp"
names(hc201c)[which(names(hc201c)=="optotv17")] <- "optotv"
names(hc201c)[which(names(hc201c)=="opdrv17")] <- "opdrv"
names(hc201c)[which(names(hc201c)=="ertot17")] <- "ertot" 
names(hc201c)[which(names(hc201c)=="ertexp17")] <- "ertexp" 
names(hc201c)[which(names(hc201c)=="ipdis17")] <- "ipdis" 
names(hc201c)[which(names(hc201c)=="ipngtd17")] <- "ipngtd" 
names(hc201c)[which(names(hc201c)=="iptexp17")] <- "iptexp" 
names(hc201c)[which(names(hc201c)=="rxslf17")] <- "rxslf" 
names(hc201c)[which(names(hc201c)=="inscov17")] <- "inscov"
names(hc201c)[which(names(hc201c)=="perwt17f")] <- "poolwt" 

#Save new dataframe 
save(hc201c, file="h201c.Rdata")

#hc201c contains the same number of rows as the raw dataset (31,880)

################################################################################
# Household - 2019 Medical Conditions File | HC-214
################################################################################

#Import, load, and save files (one time only - load Rdata file for future loads)
#If you dont have the haven package/library installed, install it first 

#Website for 2017 conditions file: https://urldefense.com/v3/__https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_detail.jsp?cboPufNumber=HC-199__;!!OLgoXmg!QiQiCemLWqdS44ZnTBoRyjS9VKgK-cO29ky7cc8UyvjooCJMCDq59jRNgD-YP6M1N_j3h2_k6V9FzNeQTGxvxQ$  
hc199 <- read_sas("/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Cancer Expenditure Project/h199.sas7bdat")

save(hc199, file="/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Cancer Expenditure Project/hc199.Rdata")

#Load Rdata file (after first time executing code)-------------------------------------------------

load(file="hc199.Rdata")


#Convert all variable names to lowercase 
names(hc199) <- tolower(names(hc199))

#Flag for cancer: 

hc199 <- hc199 %>% 
  mutate(cancerpatients = ifelse((icd10cdx=="C85"|icd10cdx=="C43"|icd10cdx=="C61"|icd10cdx=="C67"|icd10cdx=="C50"|icd10cdx=="C53"|icd10cdx=="C18"|icd10cdx=="C34"|icd10cdx=="C55"),1,0))

table(hc199$cancerpatients)

#Coding for the charlson co-morbidity index variables:


### CCI - Myocardial Infarction ### 

hc199 <- hc199 %>% 
  mutate(vcom1 = ifelse(icd10cdx=="I21",1,
                        ifelse(icd10cdx=="I22",1,
                               ifelse(icd10cdx=="I25.2",1,
                                      ifelse(icd10cdx=="-9",NA,0)))))
table(hc199$vcom1)

### CCI - Congestive Heart Failure (CHF) ### 

hc199 <- hc199 %>% 
  mutate(vcom2 = ifelse(icd10cdx=="I90",1,
                        ifelse(icd10cdx=="I11",1,
                               ifelse(icd10cdx=="I13",1,
                                      ifelse(icd10cdx=="I25",1,
                                             ifelse(icd10cdx=="I42",1,
                                                    ifelse(icd10cdx=="I43",1,
                                                           ifelse(icd10cdx=="I50",1,
                                                                  ifelse(icd10cdx=="P29",1,
                                                                         ifelse(icd10cdx=="-9",NA,0))))))))
                        
                        
                        
  ))
table(hc199$vcom2)

### CCI - Peripheral Vascular Disease (PVD) ###

hc199 <- hc199 %>%
  mutate(vcom3 = ifelse(icd10cdx=="I71",1,
                        ifelse(icd10cdx=="I73",1,
                               ifelse(icd10cdx=="I77",1,
                                      ifelse(icd10cdx=="I79",1,
                                             ifelse(icd10cdx=="K55",1,
                                                    ifelse(icd10cdx=="Z95",1,
                                                           ifelse(icd10cdx=="-9",NA,0))))))))


### CCI - Cerebrovascular Disease (CVD) ### 

hc199 <- hc199 %>% 
  mutate(vcom4=ifelse(icd10cdx=="G45",1,
                      ifelse(icd10cdx=="G46",1,
                             ifelse(icd10cdx=="H34",1,
                                    ifelse(icd10cdx=="I60",1,
                                           ifelse(icd10cdx=="I61",1,
                                                  ifelse(icd10cdx=="I62",1,
                                                         ifelse(icd10cdx=="I63",1,
                                                                ifelse(icd10cdx=="I64",1,
                                                                       ifelse(icd10cdx=="I65",1,
                                                                              ifelse(icd10cdx=="I66",1,
                                                                                     ifelse(icd10cdx=="I67",1,
                                                                                            ifelse(icd10cdx=="I68",1,
                                                                                                   ifelse(icd10cdx=="I69",1,
                                                                                                          ifelse(icd10cdx=="-9",NA,0)))))))))))))))
table(hc199$vcom4)

### CCI - Dementia ### 

hc199 <- hc199 %>% 
  mutate(vcom5 = ifelse(icd10cdx=="F00",1,
                        ifelse(icd10cdx=="F01",1,
                               ifelse(icd10cdx=="F02",1,
                                      ifelse(icd10cdx=="F03",1,
                                             ifelse(icd10cdx=="F05",1,
                                                    ifelse(icd10cdx=="G30",1,
                                                           ifelse(icd10cdx=="G31",1,
                                                                  ifelse(icd10cdx=="-9",NA,0)))))))))
table(hc199$vcom5)

### CCI - Chronic Pulmonary Disease ### 

hc199 <- hc199 %>% 
  mutate(vcom6 = ifelse(icd10cdx=="I27",1,
                        ifelse(icd10cdx=="J40",1,
                               ifelse(icd10cdx=="J41",1,
                                      ifelse(icd10cdx=="J42",1,
                                             ifelse(icd10cdx=="J43",1,
                                                    ifelse(icd10cdx=="J44",1,
                                                           ifelse(icd10cdx=="J45",1,
                                                                  ifelse(icd10cdx=="J46",1,
                                                                         ifelse(icd10cdx=="J47",1,
                                                                                ifelse(icd10cdx=="J60",1,
                                                                                       ifelse(icd10cdx=="J61",1,
                                                                                              ifelse(icd10cdx=="J62",1,
                                                                                                     ifelse(icd10cdx=="J63",1,
                                                                                                            ifelse(icd10cdx=="J64",1,
                                                                                                                   ifelse(icd10cdx=="J65",1,
                                                                                                                          ifelse(icd10cdx=="J66",1,
                                                                                                                                 ifelse(icd10cdx=="J67",1,
                                                                                                                                        ifelse(icd10cdx=="J68",1,
                                                                                                                                               ifelse(icd10cdx=="J70",1,
                                                                                                                                                      ifelse(icd10cdx=="-9",NA,0)))))))))))))))))))))
table(hc199$vcom6)  

### CCI - Rheumatic Disease ### 

hc199 <- hc199 %>% 
  mutate(vcom7 = ifelse(icd10cdx=="M05",1,
                        ifelse(icd10cdx=="M06",1,
                               ifelse(icd10cdx=="M31",1,
                                      ifelse(icd10cdx=="M32",1,
                                             ifelse(icd10cdx=="M33",1,
                                                    ifelse(icd10cdx=="M34",1,
                                                           ifelse(icd10cdx=="M35",1,
                                                                  ifelse(icd10cdx=="M36",1,
                                                                         ifelse(icd10cdx=="-9",NA,0))))))))))
table(hc199$vcom7)  

### CCI - Peptic Ulcer Disease (PUD) ### 

hc199 <- hc199 %>% 
  mutate(vcom8 = ifelse(icd10cdx=="K25",1,
                        ifelse(icd10cdx=="K26",1,
                               ifelse(icd10cdx=="K27",1,
                                      ifelse(icd10cdx=="K28",1,
                                             ifelse(icd10cdx=="-9",NA,0))))))
table(hc199$vcom8)  


### CCI - Mild Liver Disease (MLD) ### 

hc199 <- hc199 %>% 
  mutate(vcom9 = ifelse(icd10cdx=="B18",1,
                        ifelse(icd10cdx=="K70",1,
                               ifelse(icd10cdx=="K71",1,
                                      ifelse(icd10cdx=="K73",1,
                                             ifelse(icd10cdx=="K74",1,
                                                    ifelse(icd10cdx=="K76",1,
                                                           ifelse(icd10cdx=="Z94",1,
                                                                  ifelse(icd10cdx=="-9",NA,0)))))))))
table(hc199$vcom9)  

### CCI - Diabetes with and without complications both Type I (E10) and Type II ### 

hc199 <- hc199 %>% 
  mutate(vcom10 = ifelse(icd10cdx=="E10",1,
                         ifelse(icd10cdx=="E11",1,
                                ifelse(icd10cdx=="E12",1,
                                       ifelse(icd10cdx=="E13",1,
                                              ifelse(icd10cdx=="E14",1,
                                                     ifelse(icd10cdx=="-9",NA,0)))))))
table(hc199$vcom10)  

### CCI - Hemiplegia or Paraplegia ###

hc199 <- hc199 %>% 
  mutate(vcom11 = ifelse(icd10cdx=="G04",1,
                         ifelse(icd10cdx=="G11",1,
                                ifelse(icd10cdx=="G80",1,
                                       ifelse(icd10cdx=="G81",1,
                                              ifelse(icd10cdx=="G82",1,
                                                     ifelse(icd10cdx=="G83",1,
                                                            ifelse(icd10cdx=="-9",NA,0))))))))
table(hc199$vcom11)

### CCI - Renal Disease ### 

hc199 <- hc199 %>% 
  mutate(vcom12 = ifelse(icd10cdx=="I12",1,
                         ifelse(icd10cdx=="I13",1,
                                ifelse(icd10cdx=="N03",1,
                                       ifelse(icd10cdx=="N05",1,
                                              ifelse(icd10cdx=="N18",1,
                                                     ifelse(icd10cdx=="N19",1,
                                                            ifelse(icd10cdx=="N25",1,
                                                                   ifelse(icd10cdx=="Z49",1,
                                                                          ifelse(icd10cdx=="Z94",1,
                                                                                 ifelse(icd10cdx=="Z99",1,
                                                                                        ifelse(icd10cdx=="-9",NA,0))))))))))))
table(hc199$vcom12)

### CCI - Any Malignancy ###

#hc199 <- hc199 %>% 
#  mutate(vcom13 = ifelse(icd10cdx=="C00" | icd10cdx=="C01" | icd10cdx=="C02" | icd10cdx=="C03" | icd10cdx=="C04" | icd10cdx=="C05" | icd10cdx=="C06" | icd10cdx=="C07" | icd10cdx=="C08" | 
#                           icd10cdx=="C09" | icd10cdx=="C10" | icd10cdx=="C11" | icd10cdx=="C12" | icd10cdx=="C13" | icd10cdx=="C14" | icd10cdx=="C15" | icd10cdx=="C16" | icd10cdx=="C17" |icd10cdx=="C18"
#                         | icd10cdx=="C19" | icd10cdx=="C20" | icd10cdx=="C21" | icd10cdx=="C22" | icd10cdx=="C23" | icd10cdx=="C24" | icd10cdx=="C25" | icd10cdx=="C26" | icd10cdx=="C30" |icd10cdx=="C31"
#                         | icd10cdx=="C32" | icd10cdx=="C33" | icd10cdx=="C34" | icd10cdx=="C37" | icd10cdx=="C38" | icd10cdx=="C39" | icd10cdx=="C40" | icd10cdx=="C41" | icd10cdx=="C43" 
#                         | icd10cdx=="C45" | icd10cdx=="C47" | icd10cdx=="C49" | icd10cdx=="C49" | icd10cdx=="C50" | icd10cdx=="C51" | icd10cdx=="C52" | icd10cdx=="C53" | icd10cdx=="C54" | icd10cdx=="C55" 
#                         | icd10cdx=="C56" | icd10cdx=="C57" | icd10cdx=="C58" | icd10cdx=="C60" | icd10cdx=="C61" | icd10cdx=="C62" | 
#                           icd10cdx=="C63" | icd10cdx=="C64" | icd10cdx=="C65" | icd10cdx=="C66" | icd10cdx=="C67" | icd10cdx=="C68" | icd10cdx=="C69" | icd10cdx=="C70" | icd10cdx=="C71" | icd10cdx=="C72"|
#                           icd10cdx=="C73" | icd10cdx=="C73" | icd10cdx=="C74" | icd10cdx=="C75"|icd10cdx=="C76"|icd10cdx=="C81"|icd10cdx=="C82"|icd10cdx=="C83"|icd10cdx=="C84"|icd10cdx=="C85"|icd10cdx=="C88"|
#                           icd10cdx=="C90"|icd10cdx=="C91"|icd10cdx=="C92"|icd10cdx=="C93"|icd10cdx=="C94"|icd10cdx=="C95"|icd10cdx=="C96"|icd10cdx=="C97",1,
#                         ifelse(icd10cdx=="-9",NA,0)))
#table(hc199$vcom13) 

### CCI - Moderate or Severe Liver Disease ### 

hc199 <- hc199 %>% 
  mutate(vcom14 = ifelse(icd10cdx=="I85",1,
                         ifelse(icd10cdx=="I86",1,
                                ifelse(icd10cdx=="I98",1,
                                       ifelse(icd10cdx=="K70",1,
                                              ifelse(icd10cdx=="K71",1,
                                                     ifelse(icd10cdx=="K72",1,
                                                            ifelse(icd10cdx=="K76",1,
                                                                   ifelse(icd10cdx=="-9",NA,0)))))))))
table(hc199$vcom14)

### CCI - Metastatic Solid Tumor ### 

#hc199 <- hc199 %>% 
#  mutate(vcom15 = ifelse(icd10cdx=="C77",1,
#                         ifelse(icd10cdx=="C78",1,
#                                ifelse(icd10cdx=="C79",1,
#                                       ifelse(icd10cdx=="C80",1,
#                                              ifelse(icd10cdx=="-9",NA,0))))))
#table(hc199$vcom15)

### CCI - AIDS/HIV ### 

hc199 <- hc199 %>% 
  mutate(vcom16 = ifelse(icd10cdx=="B20",1,
                         ifelse(icd10cdx=="B21",1,
                                ifelse(icd10cdx=="B22",1,
                                       ifelse(icd10cdx=="B24",1,
                                              ifelse(icd10cdx=="-9",NA,0))))))
table(hc199$vcom16)

save(hc199, file="/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Cancer Expenditure Project/hc199.Rdata")

######################################
# Generate Indicator Variables       #
######################################

#Filter data to leave dupersid and vcom variables 
load(file="/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Cancer Expenditure Project/hc199.Rdata")

test <- subset(hc199, select=c(dupersid, cancerpatients, vcom1, vcom2, vcom3, vcom4, vcom5, vcom6, vcom7, vcom8, vcom9, vcom10, vcom11, vcom12, vcom14, vcom16))

#Test dataframe will contain duplicate rows of DUPERSID, regardless of vcom yes/no. Rows must be collapsed into one and summarised 

test <- test %>% 
  group_by(dupersid) %>%
  summarise_if(
    is.numeric,
    sum,
    na.rm=TRUE
  )

#Create a new variable that sums the number of chronic diseases per dupersid, called mvcom (maxvcom)
test <- test %>% 
  group_by(dupersid) %>% 
  mutate(mvcom = `vcom1` + `vcom2` + `vcom3` + `vcom4` + `vcom5` + `vcom6` + `vcom7` + `vcom8` + `vcom9` + `vcom10` + `vcom11` + `vcom12` + `vcom14` + `vcom16`)
#mvcom is the calculated charlson comorbidity index score 
# Generate labels for variables: 

library(Hmisc)

label(test$vcom1) <- 'mi' #(Myocardial Infarction)
label(test$vcom2) <- 'chf' #(Congestive Heart Failure)
label(test$vcom3) <- 'pvd' #(Peripheral Vascular Disease)
label(test$vcom4) <- 'cvd' #( Cerebrovascular Disease )
label(test$vcom5) <- 'dementia' #(Dementia)
label(test$vcom6) <- 'cpd' #(Chronic Pulmonary Disease)
label(test$vcom7) <- 'rheum' #(Rheumatic Disease )
label(test$vcom8) <- 'pud' #( Peptic Ulcer Disease)
label(test$vcom9) <- 'mld' #(Mild Liver Disease )
label(test$vcom10) <- 'dmwoc' #(Diabetes)
label(test$vcom11) <- 'hemi' #(Hemiplegia or Paraplegi)
label(test$vcom12) <- 'renal' #(Renal Disease)
# label(test$vcom13) <- 'malignancy' ( Any Malignancy)
label(test$vcom14) <- 'sld' #(Moderate or Severe Liver Disease)
#label(test$vcom15) <- 'mst' (Metastatic Solid Tumor)
label(test$vcom16) <- 'aids' #( AIDS/HIV)

#Self-Check: 
#test --> 23,936 rows 
#hc201c --> 31,880 rows 
#Final data set should have an equal number of rows as the original full year consolidated data file 

save(hc199, file="hc199.Rdata")
#load(file="/Users/oliviayip/Library/Mobile Documents/com~apple~CloudDocs/UCSD/Research Projects/Cancer Expenditure Project/hc199.Rdata")

#load(file='hc199.Rdata')
#load(file='h201c.Rdata') #The original raw 2017 dataset had 23,529 observations 
colnames(hc199)

hc199 <- subset(hc199, select=c(dupersid, vcom1, vcom2, vcom3, vcom4, vcom5, vcom6, vcom7, vcom8, vcom9, vcom10, vcom11, vcom12, vcom14, vcom16))

test <- hc199 %>% 
  group_by(dupersid) %>% 
  summarise(across(starts_with('vcom'),~sum(.,na.rm=TRUE)))

test <- test %>% 
  mutate_if(is.numeric, ~1*(. >0))

#Change all instances of a value >1 to "1"; these variable should be binary (yes/no) 
hc199 <- test
  
#Merge the condition information into the final dataset: 
#The final dataset should have 23,529 observations: 

hc216_final <- merge(hc201c, hc199, by="dupersid",all.x=TRUE)
#Merge was successful - number of rows still corresponds to the raw dataset (n=31,880)

#Calculate the CCI Score: 
#https://urldefense.com/v3/__https://www.mdcalc.com/calc/3917/charlson-comorbidity-index-cci__;!!OLgoXmg!QiQiCemLWqdS44ZnTBoRyjS9VKgK-cO29ky7cc8UyvjooCJMCDq59jRNgD-YP6M1N_j3h2_k6V9FzNdLsw0fyw$ 

#Age: <50 = 0 points 
#     50-59 years = +1 point 
#     60-69 years = +2 points 
#     70-79 years = +3 points 
#     80 or more years = +4 points 

hc216_final <- hc216_final %>% 
  mutate(cci_age = ifelse(age>17 & age<50,0,
                          ifelse(age>49 & age<60,1,
                                 ifelse(age>59 & age<70,2,
                                        ifelse(age>69 & age<80,3,
                                            ifelse(age>79 & age<100,4,NA))))))

table(hc216_final$cci_age) 

#Myocardial infarction +1 point 
table(hc216_final$vcom1)

hc216_final <- hc216_final %>% 
  mutate(cci_mi = ifelse(vcom1==1,1,
                      ifelse(vcom1==0,0,NA)))
                                  
table(hc216_final$cci_mi)

#Congestive heart failure +1 point 
table(hc216_final$vcom2)

hc216_final <- hc216_final %>% 
  mutate(cci_chf = ifelse(vcom2==1,1,
                          ifelse(vcom2==0,0,NA)))

table(hc216_final$cci_chf)

#Peripheral vascular disease +1 point 
table(hc216_final$vcom3)

hc216_final <- hc216_final %>% 
  mutate(cci_pvd = ifelse(vcom3==1,1,
                           ifelse(vcom3==0,0,NA)))

table(hc216_final$cci_pvd)


#CVA or TIA +1 point 
table(hc216_final$vcom4)

hc216_final <- hc216_final %>% 
  mutate(cci_tia = ifelse(vcom4==1,1,
                           ifelse(vcom4==0,0,NA)))

table(hc216_final$cci_tia)


#Chronic cognitive deficit +1 point 
table(hc216_final$vcom5)

hc216_final <- hc216_final %>% 
  mutate(cci_ccd = ifelse(vcom5==1,1,
                           ifelse(vcom5==0,0,NA)))

table(hc216_final$cci_ccd)

#COPD +1 point 
table(hc216_final$vcom6)

hc216_final <- hc216_final %>% 
  mutate(cci_copd = ifelse(vcom6==1,1,
                          ifelse(vcom6==0,0,NA)))

table(hc216_final$cci_copd)

#Connective tissue disease +1 point 
table(hc216_final$vcom7)

hc216_final <- hc216_final %>% 
  mutate(cci_rheum = ifelse(vcom7==1,1,
                            ifelse(vcom7==0,0,NA)))

table(hc216_final$cci_rheum)

#Peptic ulcer disease +1 point 
table(hc216_final$vcom8)

hc216_final <- hc216_final %>% 
  mutate(cci_pud = ifelse(vcom8==1,1,
                             ifelse(vcom8==0,0,NA)))

table(hc216_final$cci_pud)

#Liver disease +1 point for mild, +3 points for moderate/severe 

table(hc216_final$vcom9) #mild

hc216_final <- hc216_final %>% 
  mutate(cci_mld = ifelse(vcom9==1,1,
                            ifelse(vcom9==0,0,NA)))
table(hc216_final$cci_mld)

table(hc216_final$vcom14) #moderate/severe 

hc216_final <- hc216_final %>% 
  mutate(cci_sld = ifelse(vcom14==1,3,
                          ifelse(vcom14==0,0,NA)))
table(hc216_final$cci_sld)

#Diabetes mellitus +1 point for uncomplicated, +2 points for end-organ damage
table(hc216_final$vcom10)

hc216_final <- hc216_final %>% 
  mutate(cci_dmwoc = ifelse(vcom10==1,1,
                            ifelse(vcom10==0,0,NA)))

table(hc216_final$cci_dmwoc)


#Hemiplegia +2 point 
table(hc216_final$vcom11)

hc216_final <- hc216_final %>% 
  mutate(cci_hemi = ifelse(vcom11==1,2,
                           ifelse(vcom11==0,0,NA)))

table(hc216_final$cci_hemi)


#Moderate to severe CKD +2 point 
table(hc216_final$vcom12)

hc216_final <- hc216_final %>% 
  mutate(cci_ckd = ifelse(vcom12==1,2,
                            ifelse(vcom12==0,0,NA)))

table(hc216_final$cci_ckd)


#AIDS +6 point 
table(hc216_final$vcom16)

hc216_final <- hc216_final %>% 
  mutate(cci_aids = ifelse(vcom16==1,6,
                            ifelse(vcom16==0,0,NA)))

table(hc216_final$cci_aids)

# Calculate final cci  score 

hc216_final<-hc216_final%>%
  mutate(cci_score  = cci_age + cci_mi + cci_chf + cci_tia + cci_ccd  + cci_copd +  cci_rheum +  cci_pud +  cci_mld +  cci_sld +  cci_dmwoc + cci_hemi + cci_ckd +  cci_aids)

table(hc216_final$cci_score)


save(hc216_final, file="hc216_final.Rdata")

load(file="hc216_final.Rdata")

################################################################################
# Propensity Score Matching                                                    #
################################################################################

#Propensity Score Matching-matched data = df.match-----------------------------------------------------------------------
#You cannot match data with missingness, so we need to remove rows with "NAs" for variables that are RELEVANT to the study: 
hc216_final_match <- subset(hc216_final, select=c(dupersid, age_cat, age, sex, racev1x, hispanx, region, povcat, marritalstatus, inscov, neweducode, 
                                                  cancer, cci_score, perwt17f, varstr, varpsu))

hc216_final_match <-  filter(hc216_final_match, age>=18)

hc216_final_match$sex <- as.factor(hc216_final_match$sex)
hc216_final_match$racev1x <- as.factor(hc216_final_match$racev1x)

table(hc216_final_match$sex)
table(hc216_final_match$racev1x)

is.factor(hc216_final_match$sex)
is.factor(hc216_final_match$racev1x)

ecls_nomiss <- na.omit(hc216_final_match)  

#You cannot perform propensity score matching (PSM) if you have missing data ("NA").                                #This generates a new dataframe "ecls_nomiss" that is a copy of your existing dataset removing missing variables 

#Execute using matching algorithms 
library(optmatch)
library(MatchIt)
library(ggplot2)
library(Hmisc)
library(cobalt)

#For future analysis (manuscript): add the survey weight to the matching method 
match.it <- matchit(ecls_nomiss$cancer ~age + sex + marritalstatus + neweducode + povcat      
                    + inscov + racev1x + cci_score + perwt17f+varpsu+varstr, data=ecls_nomiss,
                    method="nearest", discard='both', caliper=0.1)            #Method='nearest" ensures we use the nearest neighbor approach with a caliper of 0.1 (only people in cancer and non-cancer groups with propensity scores within 0.1 points of each other will be a match)


a<-summary(match.it)
a                    #694 cases matched to 694 controls 

s.out <- summary(match.it, standardize=TRUE)
plot(s.out)

match.data=match.data(match.it)

histbackback(split(match.data$distance,
                   match.data$cancer),           #Replace "mdistress" with you cancer flag variable name 
             main="Propensity score after matching",
             xlab=c("control",
                    "treatment"))


df.match <- match.data(match.it)[1:ncol(ecls_nomiss)]

v <- data.frame(old=c("age", "sex", "marritalstatus", "neweducode", "povcat",      #Replace "mdistress" with the cancer flag variable 
                      "inscov","racev1x", "cci", "distance","LONGWT"), 
                new=c("Age", "Gender", "Marital Startus", "Education", "Poverty",      #Replace "mdistress" with the cancer flag variable 
                      "Insurance Coverage","Race", "CCI", "Propensity Score","Survey Weight"))
head(v) #"Head" is executed to check that the renaming was correctly performed 

love.plot(match.it, stats=c("mean.diffs", "ks.statistics"),  #Generates a love plot of the PSM procedure; we use this to see if it was successful or not 
          threshold = c(m=.1, ks=.05),
          binary= "std", abs=TRUE,
          var.order = "unadjusted", var.names = v,
          limits = c(0,1), grid=FALSE, wrap=20,
          sample.names=c("Unmatched","Matched"),
          position="top", shapes=c("circle", "triangle"),
          colors=c("black", "gray"))

bal.plot(match.it, var.name="distance", which="both", type="histogram", mirror=TRUE)

save(df.match, file="df.match.Rdata") 

##### Applying survey weights --------------------------------------------------------------------------
#First, create a flag in the "match.data" set to identify which persons are matched: 
df.match <- df.match %>%
  mutate(match=1)

#Next, subset matched data to ONLY include dupersid and match flag: 
df.match <- subset(df.match, select=c(dupersid, match))

#Third, merge this back into the cleaned original datast (hc216_final)
load(file="hc216_final.Rdata")
hc216_final <- subset(hc216_final, select=-c(match.x, match.y))


test <- merge(hc216_final, df.match, by="dupersid", all.x=TRUE)
hc216_final <- test
remove(test)
?merge
table(hc216_final$match) #1388 matched observations 

#SAVE THIS DATASET FOR FURTHER SURVEY WEIGHTED ANALYSIS: 
save(hc216_final, file="hc216_final.Rdata")

load(file="hc216_final.Rdata")

library(survey)


## Apply the survey weights to the dataframe using the svydesign function
options(survey.lonely.psu = 'adjust')

mepsdsn = svydesign(
  id=~varpsu,
  strata=~varstr,
  weights=~perwt17f, #2017: PERWT17F
  data=hc216_final, #Replace pool with your dataset name --> use your matched data 
  nest=TRUE
)

study_dsn <- subset(mepsdsn, match==1) 


#Count the number of people in each group (with cancer and without cancer) 
table(hc216_final$match, useNA='always')
svytable(~cancer, design=study_dsn)

svymean(~age, design=study_dsn)
svytable(~age_cat, design=study_dsn) 
svytable(~age_cat+cancer, design=study_dsn)

svychisq(~cancer+region, design=study_dsn)

svytable(~sex, design=study_dsn)
svytable(~sex+cancer, design=study_dsn)
svytable(~racev1x, design=study_dsn)
svytable(~hispanx+cancer, design=study_dsn)
svytable(~hispanx, design=study_dsn)
svytable(~marritalstatus+cancer, design=study_dsn)
svytable(~marritalstatus, design=study_dsn)
svytable(~neweducode+cancer, design=study_dsn)
svytable(~neweducode, design=study_dsn)
svytable(~region+cancer, design=study_dsn)
svytable(~region, design=study_dsn)
svytable(~povcat+cancer, design=study_dsn)
svytable(~povcat, design=study_dsn)
svytable(~inscov+cancer, design=study_dsn)
svytable(~inscov, design=study_dsn)
svymean(~cci_score, design=study_dsn)
svyby(~cci_score, ~cancer, design=study_dsn,svymean)

svyranktest(~totexp+cancer, design=study_dsn, test='KruskalWallis')
svyranktest(~rxexp+cancer, design=study_dsn, test='KruskalWallis')
svyranktest(~optexp+cancer, design=study_dsn, test='KruskalWallis')
svyranktest(~iptexp+cancer, design=study_dsn, test='KruskalWallis')
svyranktest(~obtotv+cancer, design=study_dsn, test='KruskalWallis')
svyranktest(~optotv+cancer, design=study_dsn, test='KruskalWallis')
svyranktest(~ipngtd+cancer, design=study_dsn, test='KruskalWallis')
svyranktest(~cncmed31+cancer, design=study_dsn, test='KruskalWallis')

#unweighted, 1,390 people (cancer AND no cancer)
#weighted, 7,633,556 with cancer, 7,590,607 without cancer 

#Continuous Variable subgroup means and SE 

# Expenditures - Cancer
svyby(~totexp, ~cancer, design=study_dsn, svymean) #total exp
svyby(~rxexp, ~cancer, design=study_dsn, svymean) #RX exp
svyby(~optexp, ~cancer, design=study_dsn, svymean) #outpatient fac + dr exp
svyby(~iptexp, ~cancer, design=study_dsn, svymean)  #inpatient exp
?svyby
svymean(~totexp, design=study_dsn) #total exp
svymean(~rxexp, design=study_dsn) #RX exp
svymean(~optexp, design=study_dsn) #outpatient fac + dr exp
svymean(~iptexp, design=study_dsn) #inpatient exp


# Visits
svyby(~obtotv, ~cancer, design=study_dsn, svymean)  #office based visits 
svyby(~obdrv, ~cancer, design=study_dsn, svymean)  #physician visits
svyby(~ipngtd, ~cancer, design=study_dsn, svymean) #nights in hospital

svymean(~obtotv, design=study_dsn)  #office based visits 
svymean(~obdrv, design=study_dsn)  #physician visits
svymean(~ipngtd, design=study_dsn)

#Categorical Frequency Counts 
svyby(~age_cat, ~cancer, design=study_dsn, svytotal) #age category
svyby(~age, ~cancer, design=study_dsn, svymean)
svyby(~racev1x, ~cancer, design=study_dsn, svytotal) #race
svyby(~neweducode, ~cancer, design=study_dsn, svytotal) #edu
svyby(~region, ~cancer, design=study_dsn, svytotal) #region
svyby(~povcat, ~cancer, design=study_dsn, svytotal) #poverty
svyby(~marritalstatus, ~cancer, design=study_dsn, svytotal) #maritalstatus
svyby(~inscov, ~cancer, design=study_dsn, svytotal) #inscoverage


svytable(~cabreast, design=study_dsn) #breast cancer
svytable(~cacervix, design=study_dsn) #cervix cancer
svytable(~cacolon, design=study_dsn) #colon cancer
svytable(~calung, design=study_dsn) #lung cancer
svytable(~calymph, design=study_dsn) #lymph cancer
svytable(~camelano, design=study_dsn) #melanoma cancer
svytable(~caprosta, design=study_dsn) #prostate cancer

study_dsn %>%
  tbl_svysummary(by = cacolon, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(obtotv,obdrv,optotv,ipngtd)) 
svytable(~cabreast+cacervix, design=study_dsn) #breast cancer

7641260+8348576
7641260/15989836

library("tidyverse")
library("gtsummary")

#demographics / table 1
study_dsn %>%
  tbl_svysummary(by = cancer, 
                 statistic = list(
                   all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{n} ({p}%)"),
                 missing= "no",
                 percent = "column", 
                 include = c(sex,age_cat, hispanx, racev1x,neweducode,region,povcat,marritalstatus,inscov,cci_score))%>%
  add_overall() %>%
  add_p() %>%
  bold_p(t = 0.05, q = FALSE)

# table 2
study_dsn %>%
  tbl_svysummary(by = cancer, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(totexp,iptexp,optexp, rxexp)) %>%
  add_overall() %>%
  add_p() %>%
  bold_p(t = 0.05, q = FALSE)

study_dsn %>%
  tbl_svysummary(by = cancer, 
                 statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"), 
                 percent = "column", 
                 include = c(totexp,iptexp,optexp, rxexp)) %>%
  add_overall() %>%
  add_p() %>%
  bold_p(t = 0.05, q = FALSE)


study_dsn %>%
  tbl_svysummary(by = cancer, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(obtotv,obdrv,optotv,ipngtd)) %>%
  add_overall() %>%
  add_p() %>%
  bold_p(t = 0.05, q = FALSE)

study_dsn %>%
  tbl_svysummary(by = cancer, 
                 statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"), 
                 percent = "column", 
                 include = c(obtotv,obdrv,optotv,ipngtd)) %>%
  add_overall() %>%
  add_p() %>%
  bold_p(t = 0.05, q = FALSE)

##Expenditures by Types of Cancer
study_dsn %>%
  tbl_svysummary(by = cabreast, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(totexp,rxexp,optexp,iptexp,iptexp)) %>%
  add_overall()

study_dsn %>%
  tbl_svysummary(by = cacervix, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(totexp,rxexp,optexp,iptexp,iptexp)) %>%
  add_overall()

study_dsn %>%
  tbl_svysummary(by = cacolon, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(totexp,rxexp,optexp,iptexp,iptexp)) %>%
  add_overall()


study_dsn %>%
  tbl_svysummary(by = calung, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(totexp,rxexp,optexp,iptexp,iptexp)) %>%
  add_overall()


study_dsn %>%
  tbl_svysummary(by = calymph, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(totexp,rxexp,optexp,iptexp,iptexp)) %>%
  add_overall()

study_dsn %>%
  tbl_svysummary(by = camelano, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(totexp,rxexp,optexp,iptexp,iptexp)) %>%
  add_overall()

study_dsn %>%
  tbl_svysummary(by = caprosta, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(totexp,rxexp,optexp,iptexp,iptexp)) %>%
  add_overall()

study_dsn %>%
  tbl_svysummary(by = cabladdr, 
                 statistic = list(all_continuous() ~ "{mean} ({sd})"), 
                 percent = "column", 
                 include = c(totexp,rxexp,optexp,iptexp,iptexp)) %>%
  add_overall()



##using median and IQRs
study_dsn %>%
  tbl_svysummary(by = calung, 
                 statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"), 
                 percent = "column", 
                 include = c(totexp,rxexp,optexp,iptexp)) %>%
                  remove_row_type(variables = caprosta,
                      type="level",
                      level_value = "-8")

study_dsn %>%
  tbl_svysummary(by = cacervix, 
                 statistic = list(all_continuous() ~ "{median} ({p25}, {p75})"), 
                 percent = "column", 
                 include = c(totexp,rxexp,optexp,iptexp))  %>%
  remove_row_type(variables = cacervix,
                  type="level",
                  level_value = "-8")

class(study_dsn)
