# |----------------------------------------------------------------------------------|
# | Project: Readmission or Death After First HF Discharge                           |
# | Script: Make the data set for the analysis                                       |
# | Authors: Michail Giakoumis; Davit Sargsyan                                       |   
# | Created: 04/05/2019                                                              |
# | Modified: 07/12/2019: removed duplicated 'Patient_ID' column                     |
# |----------------------------------------------------------------------------------|
# Header----
# Save consol output to a log file
sink(file = "tmp/log_midas15_hf_data_v1.txt")
date()

# Header----
options(scipen = 999)

# Load packages
require(data.table)
require(knitr)
require(icd)

# Part I----
# Load MIDAS----
# NOTE: data was preprocessed. See project 
# 'midas' R script 'export_midas_from_csv_to_rdata_v4.R'
system.time(load("E:/MIDAS/midas15_clean.RData"))
midas15

# Remove unused variables----
midas15[, ZIP := NULL]
midas15[, TOTBIL := NULL]
midas15[, DIV := NULL]
midas15[, STATUS := NULL]
gc()

# Number of patients
length(unique(midas15$Patient_ID))
# 18,057,028 records of 4,446,438 patients

summary(midas15)

# Exclude Emergency/Other Outpatient---- 
# 1 = inpatient
# 2 = ER outpatient
# 3 = same day surgery (SDS) outpatient
# 4 = other outpatient (non-ER and non-SDS)
# 5 = non-ER outpatient (3 or 4)
t1 <- table(midas15$YEAR,
            midas15$ADM_TYPE)
kable(format(t1, big.mark = ","))
  # |     |1       |2       |3       |4      |5       |
  # |:----|:-------|:-------|:-------|:------|:-------|
  # |1995 |445,427 |0       |0       |0      |51,019  |
  # |1996 |462,937 |0       |0       |0      |60,475  |
  # |1997 |469,139 |0       |0       |0      |73,844  |
  # |1998 |480,809 |0       |0       |0      |83,075  |
  # |1999 |487,342 |0       |0       |0      |86,042  |
  # |2000 |506,849 |0       |0       |0      |90,692  |
  # |2001 |512,121 |0       |0       |0      |94,236  |
  # |2002 |538,553 |0       |0       |0      |99,614  |
  # |2003 |567,841 |0       |0       |0      |104,101 |
  # |2004 |565,937 |0       |0       |0      |124,838 |
  # |2005 |577,161 |0       |0       |0      |127,869 |
  # |2006 |588,474 |0       |0       |0      |126,024 |
  # |2007 |588,841 |71      |0       |7      |129,072 |
  # |2008 |605,433 |367,436 |78,367  |79,698 |0       |
  # |2009 |605,830 |418,646 |95,288  |78,484 |0       |
  # |2010 |599,233 |456,183 |102,948 |83,270 |0       |
  # |2011 |574,068 |478,511 |105,419 |78,619 |0       |
  # |2012 |568,398 |532,918 |98,152  |84,347 |0       |
  # |2013 |550,449 |540,387 |97,425  |87,278 |0       |
  # |2014 |541,051 |562,339 |102,041 |92,963 |0       |
  # |2015 |530,759 |614,445 |106,091 |98,142 |0       |

# Exclude emergency records (missing before 2008)----
midas15 <- droplevels(subset(midas15,
                             ADM_TYPE != 3))
table(midas15$ADM_TYPE)
midas15[, ADM_TYPE := NULL]
gc()
# 17,271,297 records left

length(unique(midas15$Patient_ID))
# 4,310,486 patients

unique(midas15$HOSP)
length(unique(midas15$HOSP))
# 115 hospitals

range(midas15$ADMDAT)
# "1995-01-01" "2015-12-31"

# Exclusions----
# Separate diagnostic codes (DX1:DX9)----
dx <- midas15[, DX1:DX9]

dx.1.3 <- data.table(apply(dx,
                           2,
                           substr,
                           start = 1,
                           stop = 3))

# Cancers----
# See "C:\Users\ds752\Documents\svn_local\trunk\Cardiovascular\Jen Wellings HF After MI\ICD-9-CM Cancer Codes Exclude.txt" for details)
cancer <- rowSums(apply(X = dx.1.3,
                        MARGIN = 2, 
                        FUN = function(a) {
                          a %in% as.character(c(140:165,
                                                170:176, 
                                                180:239))
                        })) > 0
table(cancer)
# FALSE     TRUE 
# 15462575  1808722 
length(unique(midas15$Patient_ID[cancer]))
# 946,835 patients

# HIV----
hiv <- rowSums(apply(X = dx,
                     MARGIN = 2, 
                     FUN = function(a) {
                       a == "042"
                     })) > 0
table(hiv)
# FALSE     TRUE 
# 17198453    72844
length(unique(midas15$Patient_ID[hiv]))
# 23,718 patients

tmp <- data.table(midas15[, c("Patient_ID", 
                              "RACE",
                              "SEX", 
                              "HISPAN")],
                  hiv = hiv)
tmp <- tmp[hiv, ]
tmp[, N := 1:.N,
    by = Patient_ID]
tmp <- tmp[N == 1, ]
tmp <- data.table(table(hiv = tmp$hiv,
                        race = tmp$RACE,
                        sex = tmp$SEX,
                        hisp = tmp$HISPAN))[, -1]
tmp$pct <- round(100*tmp$N/sum(tmp$N), 1)
kable(tmp)
  # |race  |sex |hisp         |    N|  pct|
  # |:-----|:---|:------------|----:|----:|
  # |White |F   |Hispanic     |  715|  3.0|
  # |Black |F   |Hispanic     |  142|  0.6|
  # |Other |F   |Hispanic     |  353|  1.5|
  # |White |M   |Hispanic     | 1386|  5.8|
  # |Black |M   |Hispanic     |  235|  1.0|
  # |Other |M   |Hispanic     |  727|  3.1|
  # |White |F   |Non-Hispanic | 1301|  5.5|
  # |Black |F   |Non-Hispanic | 5319| 22.4|
  # |Other |F   |Non-Hispanic |  287|  1.2|
  # |White |M   |Non-Hispanic | 3209| 13.5|
  # |Black |M   |Non-Hispanic | 7479| 31.5|
  # |Other |M   |Non-Hispanic |  555|  2.3|
  # |White |F   |Unknown      |  137|  0.6|
  # |Black |F   |Unknown      |  417|  1.8|
  # |Other |F   |Unknown      |  107|  0.5|
  # |White |M   |Unknown      |  429|  1.8|
  # |Black |M   |Unknown      |  664|  2.8|
  # |Other |M   |Unknown      |  256|  1.1|

id.rm <- unique(midas15$Patient_ID[cancer | hiv])
rec.keep <- which(!(midas15$Patient_ID %in% id.rm))

midas15 <- midas15[rec.keep, ]

rm(dx.1.3,
   cancer,
   hiv,
   id.rm,
   rec.keep)
gc()

# Load ICD-9 codes----
# NOTE: this file is copied form the ankylosing spondylitis project
l1 <- fread("data/midas.hf_icd9_codes_2019-05-10.csv",
            colClasses = c("character"))
unique(l1$comorb)

# Heart failure----
kable(l1[comorb == "hf",
         c("code",
           "long_desc")])
  # |code  |long_desc                                                      |
  # |:-----|:--------------------------------------------------------------|
  # |4280  |Congestive heart failure, unspecified                          |
  # |4281  |Left heart failure                                             |
  # |42820 |Systolic heart failure, unspecified                            |
  # |42821 |Acute systolic heart failure                                   |
  # |42822 |Chronic systolic heart failure                                 |
  # |42823 |Acute on chronic systolic heart failure                        |
  # |42830 |Diastolic heart failure, unspecified                           |
  # |42831 |Acute diastolic heart failure                                  |
  # |42832 |Chronic diastolic heart failure                                |
  # |42833 |Acute on chronic diastolic heart failure                       |
  # |42840 |Combined systolic and diastolic heart failure, unspecified     |
  # |42841 |Acute combined systolic and diastolic heart failure            |
  # |42842 |Chronic combined systolic and diastolic heart failure          |
  # |42843 |Acute on chronic combined systolic and diastolic heart failure |
  # |4289  |Heart failure, unspecified                                     |

l2 <- as.comorbidity_map(split(x = l1$code,
                               f = l1$comorb))

# Separate diagnostic codes (DX1:DX9)
dx <- data.table(Record_ID = 1:nrow(midas15),
                 Patient_ID = midas15$Patient_ID,
                 midas15[, DX1:DX9])
dx

# Number of patients with each condition----
dtt <- list()
for(i in 1:9){
  dtt[[i]] <- icd9_comorbid(x = dx,
                            map = l2[c("hf")],
                            visit_name = "Patient_ID",
                            icd_name = names(dx)[i + 2])
}

# Patients with HF admissons (DX1)----
hf.dx1 <- data.table(Patient_ID = rownames(dtt[[1]]),
                     dtt[[1]])
hf.dx1
kable(format(data.frame(N_Patients = sum(hf.dx1$hf)),
             big.mark = ","))
  # |   |N_Patients |
  # |:--|:----------|
  # |hf |240,851    |

# Patients with HF diagnoses (DX1-9)----
dt2 <- data.table(apply(Reduce("+", dtt),
                        MARGIN = 2,
                        function(a){
                          a > 0
                        }))
dt2$Patient_ID <- rownames(dtt[[1]])
kable(format(data.frame(N_Patients = colSums(dt2[, 1])),
             big.mark = ","))
  # |   |N_Patients |
  # |:--|:----------|
  # |hf |593,581    |

# Keep only the patients with HF admissions----
id.keep <- unique(hf.dx1$Patient_ID[hf.dx1$hf])
dt1 <- midas15[Patient_ID %in% id.keep, ]
setkey(dt1,
       Patient_ID,
       ADMDAT)
dt1
dt1$Record_ID <- 1:nrow(dt1)

# Save datasets----
attr(dt1, "created") <- date()
attr(dt1, c("description")) <- "Source: 'midas15_hf_data_v1.R'."
attributes(dt1)
save(dt1,
     file = "data/dt1.RData")

attr(l2, "created") <- date()
attr(l2, c("description")) <- "Source: 'midas15_hf_data_v1.R'."
attributes(l2)
save(l2,
     file = "data/l2.RData")

# Clear memory
rm(list = setdiff(ls(), 
                  c("dt1",
                    "l2")))
gc()

# Part II----
# # Reload HF data----
# load("data/dt1.RData")
# load("data/l2.RData")

# Separate diagnostic codes (DX1:DX9)
dx <- data.table(Record_ID = dt1$Record_ID,
                 Patient_ID = dt1$Patient_ID,
                 dt1[, DX1:DX9])
dx

# Records with each condition----
dtt <- list()
for(i in 1:9){
  dtt[[i]] <- icd9_comorbid(x = dx,
                            map = l2,
                            visit_name = "Record_ID",
                            icd_name = names(dx)[i + 2])
}

# Patients with HF admissons (DX1)----
dt.dx1 <- data.table(Record_ID = as.numeric(rownames(dtt[[1]])),
                     dtt[[1]])
dt.dx1
summary(dt.dx1)

# Patients with HF diagnoses (DX1-9)----
dt2 <- data.table(apply(Reduce("+", dtt),
                        MARGIN = 2,
                        function(a){
                          a > 0
                        }))
kable(format(data.frame(N_Records = colSums(dt2)),
             big.mark = ","))
  # |       |N_Records |
  # |:------|:---------|
  # |af     |567,337   |
  # |ami    |118,370   |
  # |anemia |265,617   |
  # |ckd    |270,465   |
  # |copd   |532,747   |
  # |diab   |815,966   |
  # |hf     |1,083,733 |
  # |hyper  |1,385,752 |
  # |lipid  |392,947   |
  # |osa    |30,907    |
  # |park   |22,822    |
  # |stroke |35,718    |
  # |tia    |33,176    |

dt2 <- data.table(Record_ID = as.numeric(rownames(dtt[[1]])),
                  dt2)

dt2 <- merge(dt1[, c("Record_ID",
                     "Patient_ID",
                     "patbdte",
                     "NEWDTD",
                     "CAUSE",
                     "AGE",
                     "SEX",
                     "RACE",
                     "HISPAN",
                     "ADMDAT",
                     "DSCHDAT",
                     "HOSP",
                     "PRIME",
                     "YEAR",
                     "DSCYR",
                     "DX1")],
             dt2,
             by = "Record_ID")

dt.dx1$hf.dx1 <- dt.dx1$hf
dt2 <- merge(dt2,
             dt.dx1[, c("Record_ID",
                        "hf.dx1")])
dt2

# First HF discharge----
table(hf.dx1 = dt2$hf.dx1,
      hf = dt2$hf)
#                 hf
# hf.dx1    FALSE    TRUE
# FALSE   1063166  610742
# TRUE          0  472991

dt2[, first := min(DSCHDAT[hf.dx1],
                   na.rm = TRUE), 
    by = Patient_ID]
dt2

# Admissions prior to first MI (5 years look-back)
dt2[, prior := (DSCHDAT < first)]

# First AF admissions
dt2[, current := (DSCHDAT == first)]

# Summary
summary(dt2)
attr(dt2, "created") <- date()
attr(dt2, c("description")) <- "Source: 'midas15_hf_data_v1.R'."
attributes(dt2)

save(dt2, 
     file = "data/dt2.RData")

# Remove everything except dt2----
rm(list = setdiff(ls(), 
                  "dt2"))
gc()

# Part III----
# load("data/dt2.RData")
dt2

# Outcomes and histories (prior to 1st HF discharge)----
system.time(
  hh <- dt2[, list(Record_ID,
                   patbdte,
                   NEWDTD,
                   CAUSE,
                   AGE,
                   SEX,
                   RACE,
                   HISPAN,
                   ADMDAT,
                   DSCHDAT,
                   HOSP,
                   PRIME,
                   YEAR,
                   DSCYR,
                   DX1,
                   first,
                   prior,
                   current,
                   hf.dx1,
                   hf,
                   readm = sum(!(prior | current) &
                                 (difftime(ADMDAT,
                                           first,
                                           units = "days") > 0) &
                                 (is.na(NEWDTD) | (difftime(NEWDTD,
                                                            DSCHDAT,
                                                            units = "days")) > 0)) > 0,
                   readm.dat = min(ADMDAT[!(prior | current) &
                                            (difftime(ADMDAT,
                                                      first,
                                                      units = "days") > 0) &
                                            (is.na(NEWDTD) | (difftime(NEWDTD,
                                                                       DSCHDAT,
                                                                       units = "days")) > 0)],
                                   na.rm = TRUE),
                   days2readm = -1,
                   readm.dx1 = "",
                   post.hf.dx1 = sum(hf.dx1 & 
                                       !(prior | current) &
                                       (difftime(ADMDAT,
                                                 first,
                                                 units = "days") >= 0) &
                                       (is.na(NEWDTD) | (difftime(NEWDTD,
                                                                  DSCHDAT,
                                                                  units = "days")) > 0)) > 0,
                   post.hf.dat = min(ADMDAT[hf.dx1 & 
                                              !(prior | current) &
                                              (difftime(ADMDAT,
                                                        first,
                                                        units = "days") >= 0) &
                                              (is.na(NEWDTD) | (difftime(NEWDTD,
                                                                         DSCHDAT,
                                                                         units = "days")) > 0)],
                                     na.rm = TRUE),
                   days2post.hf.dx1 = -1,
                   dead = sum(!(prior | current) &
                                (difftime(NEWDTD,
                                          first,
                                          units = "days") > 0),
                              na.rm = TRUE) > 0,
                   days2death = -1,
                   cvdeath = FALSE,
                   days2cvdeath = -1,
                   haf = (sum(af & prior) > 0),
                   hami = (sum(ami & prior) > 0),
                   hanemia = (sum(anemia & prior) > 0),
                   hckd = (sum(ckd & prior) > 0),
                   hcopd = (sum(copd & prior) > 0),
                   hdiab = (sum(diab & prior) > 0),
                   hhf = (sum(hf & prior) > 0),
                   hhyper = (sum(hyper & prior) > 0),
                   hlipid = (sum(lipid & prior) > 0),
                   hosa = (sum(osa & prior) > 0),
                   hpark = (sum(park & prior) > 0),
                   hstroke = (sum(stroke & prior) > 0),
                   htia = (sum(tia & prior) > 0)),
            by = Patient_ID]
)
# 07/12/2019
# user  system elapsed 
# 224.20    0.58  227.00 

# Reason for readmission----
hh[, readm.dx1 := DX1[ADMDAT == readm.dat][1],
   by = Patient_ID]

summary(hh)
gc()

# Separate first HF admissions
# Remove all cases with no HF records
case <- unique(subset(hh, current & hf.dx1))

# If the are are more than 1 records of 1st MI admissions per person,
nrow(case) - length(unique(case$Patient_ID))
# Remove 231 patient with duplicate records
case <- case[!(Patient_ID %in% Patient_ID[duplicated(Patient_ID)]), ]
summary(case)

# Remove patients that died at 1st HF discharge
case <- droplevels(subset(case, (is.na(NEWDTD) | NEWDTD != first)))

# Remove anyone with history of HF
case <- droplevels(subset(case, !hhf))

summary(case)
case[, hf.dx1 := NULL] # All patients have it
case[, hf := NULL] # All patients have it
case[, hhf := NULL] # No patient has it
case[, prior := NULL] # False for all patients
case[, current := NULL] # True for all patients
gc()

# Days to events----
# a. Days to readmission for any reason----
case$days2readm <- as.numeric(as.character(difftime(case$readm.dat,
                                                    case$first,
                                                    units = "days")))
case$days2readm[is.infinite(case$days2readm)] <- NA
summary(case$days2readm)
hist(case$days2readm, 100)

# b. Days to readmission for HF----
case$days2post.hf.dx1 <- as.numeric(as.character(difftime(case$post.hf.dat,
                                                                 case$first,
                                                                 units = "days")))
case$days2post.hf.dx1[is.infinite(case$days2post.hf.dx1)] <- NA
summary(case$days2post.hf.dx1)
hist(case$days2post.hf.dx1, 100)

# c. Days to all-cause death----
case$days2death <- as.numeric(as.character(difftime(case$NEWDTD,
                                                    case$first,
                                                    units = "days")))
case$days2death[is.infinite(case$days2death)] <- NA
summary(case$days2death)
hist(case$days2death, 100)

# d. Days to cardiovascular death----
# Source: http://www.health.state.ok.us/stats/Vital_Statistics/Death/039_causes.shtml
# |-------------------------------------------------------------------|
# | Major cardiovascular diseases        | I00-I78                    |
# | Diseases of heart	                   | I00-I09, I11, I13, I20-I51 |
# | Hypertensive heart disease with      |                            | 
# |    or without renal disease          | I11,I13                    |
# | Ischemic heart diseases	             | I20-I25                    |
# | Other diseases of heart	             | I00-I09,I26-I51            |
# | Essential (primary) hypertension     |                            |
# |    and hypertensive renal disease	   | I10,I12                    |
# | Cerebrovascular diseases	           | I60-I69                    |
# | Atherosclerosis	                     | I70                        |
# | Other diseases of circulatory system | I71-I78                    |
# |-------------------------------------------------------------------|

case$cvdeath[case$dead & substr(case$CAUSE, 1, 1) == "I"] <- TRUE

kable(addmargins(table(all_cause_death = case$dead,
                       cv_death = case$cvdeath)))
# Row: all-cause death
# Column: cv death
  # |      |  FALSE|  TRUE|    Sum|
  # |:-----|------:|-----:|------:|
  # |FALSE |  73243|     0|  73243|
  # |TRUE  |  38816| 37368|  76184|
  # |Sum   | 112059| 37368| 149427|

case$days2cvdeath <- case$days2death
case$days2cvdeath[!case$cvdeath] <- NA
summary(case$days2cvdeath)
hist(case$days2cvdeath, 100)

# Summary of the subset----
summary(case)
case
gc()

# Save----
attr(case, "created") <- date()
attr(case, c("description")) <- "Source: 'midas15_hf_data_v1.R'."
attributes(case)

save(case, 
     file = file.path("data/case.RData"),
     compress = FALSE)

# Clean memory----
gc()

sessionInfo()
sink()