## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Overview Download & Initial Setup, include = FALSE--------------------------------------------------------------------------------------------
##########################################################
# Download & Setup
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

if(!require(gmodels)) install.packages('gmodels')
if(!require(rpart)) install.packages('rpart')
if(!require(randomForest)) install.packages('randomForest')
if(!require(e1071)) install.packages('e1071')
if(!require(rpart.plot)) install.packages("rpart.plot")

library(tidyverse)
library(caret)
library(data.table)

library(gmodels)
library(rpart)
library(randomForest)
library(e1071) # SVM
library(xml2) #Kable
library(rpart.plot)

# I create a CrossTableNarrow.R in base of CrossTable.
source(file="CrossTableNarrow.R") # Minor change in Column and Row Total Headings of gmodels::CrossTable
if(!file.exists("CrossTableNarrow.R")) {
  CrossTableNarrow <- function(arg1, arg2, arg3, arg4, arg5 ) {
        CrossTable(arg1, arg2, arg3, arg4, arg5 )  # use the standard  CrossTable()
}}
# in other words, just used CrossTable() instead of CrossTableNarrow()


# For Mac and windows uncomment the following 2 lines and comment the Linux lines below:
#library(kableExtra) # does not works in Linux
#if(!require(kableExtra)) install.packages("kableExtra")  # does not works in Linux

# To use KableExtra works in linux first download the code from
# https://github.com/haozhu233/kableExtra
#and leaved at the same level than the project
source(file="../kableExtra/R/kable_styling.R") # to center table
source(file="../kableExtra/R/scroll_box.R")
source(file="../kableExtra/R/util.R")
source(file="../kableExtra/R/magic_mirror.R") # kable latex
# end of KableExtra works in linux





# Cardiac Arrhythmia dataset:
# https://archive.ics.uci.edu/ml/datasets/Arrhythmia
# https://archive.ics.uci.edu/ml/machine-learning-databases/arrhythmia/arrhythmia.data



arrhythmia.uci <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/arrhythmia/arrhythmia.data"),
                     header=FALSE,
                     stringsAsFactors = FALSE)


colnames(arrhythmia.uci)[280] <- "Class code"


# Set NA to the ? value (missing value)
arrhythmia.uci[arrhythmia.uci == "?"] <- NA


## ----Overview Code chunk font size in Rmarkdown with knitr and latex, echo=FALSE-------------------------------------------------------------------
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})

# The size in order are:
#Huge > huge > LARGE > Large > large > normalsize > small > footnotesize > scriptsize > tiny


## ----Overview Characteristics of dataset dim-------------------------------------------------------------------------------------------------------
# dimension
dim(arrhythmia.uci)


## ----Overview Characteristics of dataset str, size='scriptsize'------------------------------------------------------------------------------------
# Display predictors (columns)
str(arrhythmia.uci, list.len = 25)


## ----Overview Class definition, include = FALSE----------------------------------------------------------------------------------------------------

Class <- data.frame(`Class code`= 01, `Class name` = "Normal", check.names = FALSE)
Class <- bind_rows( Class,
                    data.frame(`Class code`= 02, `Class name` = "Ischemic changes (Coronary Artery Disease)", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 03, `Class name` = "Old Anterior Myocardial Infarction", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 04, `Class name` = "Old Inferior Myocardial Infarction", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 05, `Class name` = "Sinus tachycardy", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 06, `Class name` = "Sinus bradycardy", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 07, `Class name` = "Ventricular Premature Contraction (PVC)", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 08, `Class name` = "Supraventricular Premature Contraction", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 09, `Class name` = "Left bundle branch block", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 10, `Class name` = "Right bundle branch block", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 11, `Class name` = "1. degree AtrioVentricular block", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 12, `Class name` = "2. degree AV block", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 13, `Class name` = "3. degree AV block", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 14, `Class name` = "Left ventricule hypertrophy", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 15, `Class name` = "Atrial Fibrillation or Flutter", check.names = FALSE))
Class <- bind_rows( Class,
                    data.frame(`Class code`= 16, `Class name` = "Others", check.names = FALSE))
Class$`Class code` <- as.factor(Class$`Class code`)


## ----Overview class as factor, include = FALSE-----------------------------------------------------------------------------------------------------
# Initial Redefine some type of the predictor from initial read from files. if using R 3.6 or earlier:
arrhythmia <- arrhythmia.uci %>% mutate(`Class code` = as.factor(`Class code`)
                                        )


## ----Overview Class code table, echo=FALSE, message=FALSE------------------------------------------------------------------------------------------
left_join(arrhythmia, Class, by = "Class code") %>% 
  group_by(`Class code`, `Class name`) %>% summarise( "N Ocurrences" = n(), Percentage = round(100*n()/nrow(arrhythmia),digits = 1)) %>%
  knitr::kable(caption = "Presence of codes in our dataset")
  


## ----Overview Distribution of Cardiac Arrhythmia Class, echo=FALSE, warning=FALSE------------------------------------------------------------------
left_join(arrhythmia, Class, by = "Class code") %>% ggplot(aes(`Class name`)) + geom_histogram(stat="count") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))  + # Rotate axis labels
  xlab("Cardiac Arrhythmia Classification") +
  ylab("Number of Patiences with the Disease") +
  ggtitle("Class Distribution of Cardiac Arrhythmia Dataset")


## ----Analysis setting colnames, include = FALSE----------------------------------------------------------------------------------------------------


# Because we have too many columns prefer make one by one to decrease error setting instead to set all of then in one sentence like 
# colnames(arrhythmia.uci) <- c("Age", "Sex", "Height", "Weight", .....)

colnames(arrhythmia.uci)[001] <- "Age"
colnames(arrhythmia.uci)[002] <- "Sex"
colnames(arrhythmia.uci)[003] <- "Height"
colnames(arrhythmia.uci)[004] <- "Weight"
colnames(arrhythmia.uci)[005] <- "QRS duration"
colnames(arrhythmia.uci)[006] <- "P-R interval"
colnames(arrhythmia.uci)[007] <- "Q-T interval"
colnames(arrhythmia.uci)[008] <- "T interval"
colnames(arrhythmia.uci)[009] <- "P interval"
colnames(arrhythmia.uci)[010] <- "QRS Vector angles"
colnames(arrhythmia.uci)[011] <- "T Vector angles"
colnames(arrhythmia.uci)[012] <- "P Vector angles"
colnames(arrhythmia.uci)[013] <- "QRST Vector angles"
colnames(arrhythmia.uci)[014] <- "J Vector angles"
colnames(arrhythmia.uci)[015] <- "Heart rate"
colnames(arrhythmia.uci)[016] <- "Channel DI Q wave Average"
colnames(arrhythmia.uci)[017] <- "Channel DI R wave Average"
colnames(arrhythmia.uci)[018] <- "Channel DI S wave Average"
colnames(arrhythmia.uci)[019] <- "Channel DI R' wave Average"
colnames(arrhythmia.uci)[020] <- "Channel DI S' Average"
colnames(arrhythmia.uci)[021] <- "Channel DI Number of intrinsic deflections"
colnames(arrhythmia.uci)[022] <- "Channel DI Existence of ragged R wave"
colnames(arrhythmia.uci)[023] <- "Channel DI Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[024] <- "Channel DI Existence of ragged P wave"
colnames(arrhythmia.uci)[025] <- "Channel DI Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[026] <- "Channel DI Existence of ragged T wave"
colnames(arrhythmia.uci)[027] <- "Channel DI Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[028] <- "Channel DII Q wave Average"
colnames(arrhythmia.uci)[029] <- "Channel DII R wave Average"
colnames(arrhythmia.uci)[030] <- "Channel DII S wave Average"
colnames(arrhythmia.uci)[031] <- "Channel DII R' wave Average"
colnames(arrhythmia.uci)[032] <- "Channel DII S' wave Average"
colnames(arrhythmia.uci)[033] <- "Channel DII Number of intrinsic deflections"
colnames(arrhythmia.uci)[034] <- "Channel DII Existence of ragged R wave"
colnames(arrhythmia.uci)[035] <- "Channel DII Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[036] <- "Channel DII Existence of ragged P wave"
colnames(arrhythmia.uci)[037] <- "Channel DII Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[038] <- "Channel DII Existence of ragged T wave"
colnames(arrhythmia.uci)[039] <- "Channel DII Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[040] <- "Channel DIII Q wave Average"
colnames(arrhythmia.uci)[041] <- "Channel DIII R wave Average"
colnames(arrhythmia.uci)[042] <- "Channel DIII S wave Average"
colnames(arrhythmia.uci)[043] <- "Channel DIII R' wave Average"
colnames(arrhythmia.uci)[044] <- "Channel DIII S' wave Average"
colnames(arrhythmia.uci)[045] <- "Channel DIII Number of intrinsic deflections"
colnames(arrhythmia.uci)[046] <- "Channel DIII Existence of ragged R wave"
colnames(arrhythmia.uci)[047] <- "Channel DIII Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[048] <- "Channel DIII Existence of ragged P wave"
colnames(arrhythmia.uci)[049] <- "Channel DIII Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[050] <- "Channel DIII Existence of ragged T wave"
colnames(arrhythmia.uci)[051] <- "Channel DIII Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[052] <- "Channel AVR Q wave Average"
colnames(arrhythmia.uci)[053] <- "Channel AVR R wave Average"
colnames(arrhythmia.uci)[054] <- "Channel AVR S wave Average"
colnames(arrhythmia.uci)[055] <- "Channel AVR R' wave Average"
colnames(arrhythmia.uci)[056] <- "Channel AVR S' wave Average"
colnames(arrhythmia.uci)[057] <- "Channel AVR Number of intrinsic deflections"
colnames(arrhythmia.uci)[058] <- "Channel AVR Existence of ragged R wave"
colnames(arrhythmia.uci)[059] <- "Channel AVR Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[060] <- "Channel AVR Existence of ragged P wave"
colnames(arrhythmia.uci)[061] <- "Channel AVR Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[062] <- "Channel AVR Existence of ragged T wave"
colnames(arrhythmia.uci)[063] <- "Channel AVR Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[064] <- "Channel AVL Q wave Average"
colnames(arrhythmia.uci)[065] <- "Channel AVL R wave Average"
colnames(arrhythmia.uci)[066] <- "Channel AVL S wave Average"
colnames(arrhythmia.uci)[067] <- "Channel AVL R' wave Average"
colnames(arrhythmia.uci)[068] <- "Channel AVL S' wave Average"
colnames(arrhythmia.uci)[069] <- "Channel AVL Number of intrinsic deflections"
colnames(arrhythmia.uci)[070] <- "Channel AVL Existence of ragged R wave"
colnames(arrhythmia.uci)[071] <- "Channel AVL Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[072] <- "Channel AVL Existence of ragged P wave"
colnames(arrhythmia.uci)[073] <- "Channel AVL Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[074] <- "Channel AVL Existence of ragged T wave"
colnames(arrhythmia.uci)[075] <- "Channel AVL Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[076] <- "Channel AVF Q wave Average"
colnames(arrhythmia.uci)[077] <- "Channel AVF R wave Average"
colnames(arrhythmia.uci)[078] <- "Channel AVF S wave Average"
colnames(arrhythmia.uci)[079] <- "Channel AVF R' wave Average"
colnames(arrhythmia.uci)[080] <- "Channel AVF S' wave Average"
colnames(arrhythmia.uci)[081] <- "Channel AVF Number of intrinsic deflections"
colnames(arrhythmia.uci)[082] <- "Channel AVF Existence of ragged R wave"
colnames(arrhythmia.uci)[083] <- "Channel AVF Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[084] <- "Channel AVF Existence of ragged P wave"
colnames(arrhythmia.uci)[085] <- "Channel AVF Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[086] <- "Channel AVF Existence of ragged T wave"
colnames(arrhythmia.uci)[087] <- "Channel AVF Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[088] <- "Channel V1 Q wave Average"
colnames(arrhythmia.uci)[089] <- "Channel V1 R wave Average"
colnames(arrhythmia.uci)[090] <- "Channel V1 S wave Average"
colnames(arrhythmia.uci)[091] <- "Channel V1 R' wave Average"
colnames(arrhythmia.uci)[092] <- "Channel V1 S' wave Average"
colnames(arrhythmia.uci)[093] <- "Channel V1 Number of intrinsic deflections"
colnames(arrhythmia.uci)[094] <- "Channel V1 Existence of ragged R wave"
colnames(arrhythmia.uci)[095] <- "Channel V1 Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[096] <- "Channel V1 Existence of ragged P wave"
colnames(arrhythmia.uci)[097] <- "Channel V1 Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[098] <- "Channel V1 Existence of ragged T wave"
colnames(arrhythmia.uci)[099] <- "Channel V1 Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[100] <- "Channel V2 Q wave Average"
colnames(arrhythmia.uci)[101] <- "Channel V2 R wave Average"
colnames(arrhythmia.uci)[102] <- "Channel V2 S wave Average"
colnames(arrhythmia.uci)[103] <- "Channel V2 R' wave Average"
colnames(arrhythmia.uci)[104] <- "Channel V2 S' wave Average"
colnames(arrhythmia.uci)[105] <- "Channel V2 Number of intrinsic deflections"
colnames(arrhythmia.uci)[106] <- "Channel V2 Existence of ragged R wave"
colnames(arrhythmia.uci)[107] <- "Channel V2 Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[108] <- "Channel V2 Existence of ragged P wave"
colnames(arrhythmia.uci)[109] <- "Channel V2 Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[110] <- "Channel V2 Existence of ragged T wave"
colnames(arrhythmia.uci)[111] <- "Channel V2 Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[112] <- "Channel V3 Q wave Average"
colnames(arrhythmia.uci)[113] <- "Channel V3 R wave Average"
colnames(arrhythmia.uci)[114] <- "Channel V3 S wave Average"
colnames(arrhythmia.uci)[115] <- "Channel V3 R' wave Average"
colnames(arrhythmia.uci)[116] <- "Channel V3 S' wave Average"
colnames(arrhythmia.uci)[117] <- "Channel V3 Number of intrinsic deflections"
colnames(arrhythmia.uci)[118] <- "Channel V3 Existence of ragged R wave"
colnames(arrhythmia.uci)[119] <- "Channel V3 Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[120] <- "Channel V3 Existence of ragged P wave"
colnames(arrhythmia.uci)[121] <- "Channel V3 Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[122] <- "Channel V3 Existence of ragged T wave"
colnames(arrhythmia.uci)[123] <- "Channel V3 Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[124] <- "Channel V4 Q wave Average"
colnames(arrhythmia.uci)[125] <- "Channel V4 R wave Average"
colnames(arrhythmia.uci)[126] <- "Channel V4 S wave Average"
colnames(arrhythmia.uci)[127] <- "Channel V4 R' wave Average"
colnames(arrhythmia.uci)[128] <- "Channel V4 S' wave Average"
colnames(arrhythmia.uci)[129] <- "Channel V4 Number of intrinsic deflections"
colnames(arrhythmia.uci)[130] <- "Channel V4 Existence of ragged R wave"
colnames(arrhythmia.uci)[131] <- "Channel V4 Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[132] <- "Channel V4 Existence of ragged P wave"
colnames(arrhythmia.uci)[133] <- "Channel V4 Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[134] <- "Channel V4 Existence of ragged T wave"
colnames(arrhythmia.uci)[135] <- "Channel V4 Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[136] <- "Channel V5 Q wave Average"
colnames(arrhythmia.uci)[137] <- "Channel V5 R wave Average"
colnames(arrhythmia.uci)[138] <- "Channel V5 S wave Average"
colnames(arrhythmia.uci)[139] <- "Channel V5 R' wave Average"
colnames(arrhythmia.uci)[140] <- "Channel V5 S' wave Average"
colnames(arrhythmia.uci)[141] <- "Channel V5 Number of intrinsic deflections"
colnames(arrhythmia.uci)[142] <- "Channel V5 Existence of ragged R wave"
colnames(arrhythmia.uci)[143] <- "Channel V5 Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[144] <- "Channel V5 Existence of ragged P wave"
colnames(arrhythmia.uci)[145] <- "Channel V5 Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[146] <- "Channel V5 Existence of ragged T wave"
colnames(arrhythmia.uci)[147] <- "Channel V5 Existence of diphasic derivation of T wave"
colnames(arrhythmia.uci)[148] <- "Channel V6 Q wave Average"
colnames(arrhythmia.uci)[149] <- "Channel V6 R wave Average"
colnames(arrhythmia.uci)[150] <- "Channel V6 S wave Average"
colnames(arrhythmia.uci)[151] <- "Channel V6 R' wave Average"
colnames(arrhythmia.uci)[152] <- "Channel V6 S' wave Average"
colnames(arrhythmia.uci)[153] <- "Channel V6 Number of intrinsic deflections"
colnames(arrhythmia.uci)[154] <- "Channel V6 Existence of ragged R wave"
colnames(arrhythmia.uci)[155] <- "Channel V6 Existence of diphasic derivation of R wave"
colnames(arrhythmia.uci)[156] <- "Channel V6 Existence of ragged P wave"
colnames(arrhythmia.uci)[157] <- "Channel V6 Existence of diphasic derivation of P wave"
colnames(arrhythmia.uci)[158] <- "Channel V6 Existence of ragged T wave"
colnames(arrhythmia.uci)[159] <- "Channel V6 Existence of diphasic derivation of T wave"

colnames(arrhythmia.uci)[160] <- "Channel DI JJ wave Amplitude"
colnames(arrhythmia.uci)[161] <- "Channel DI Q wave Amplitude"
colnames(arrhythmia.uci)[162] <- "Channel DI R wave Amplitude"
colnames(arrhythmia.uci)[163] <- "Channel DI S wave Amplitude"
colnames(arrhythmia.uci)[164] <- "Channel DI R' wave Amplitude"
colnames(arrhythmia.uci)[165] <- "Channel DI S' wave Amplitude"
colnames(arrhythmia.uci)[166] <- "Channel DI P wave Amplitude"
colnames(arrhythmia.uci)[167] <- "Channel DI T wave Amplitude"
colnames(arrhythmia.uci)[168] <- "Channel DI QRSA"
colnames(arrhythmia.uci)[169] <- "Channel DI QRSTA"
colnames(arrhythmia.uci)[170] <- "Channel DII JJ wave Amplitude"
colnames(arrhythmia.uci)[171] <- "Channel DII Q wave Amplitude"
colnames(arrhythmia.uci)[172] <- "Channel DII R wave Amplitude"
colnames(arrhythmia.uci)[173] <- "Channel DII S wave Amplitude"
colnames(arrhythmia.uci)[174] <- "Channel DII R' wave Amplitude"
colnames(arrhythmia.uci)[175] <- "Channel DII S' wave Amplitude"
colnames(arrhythmia.uci)[176] <- "Channel DII P wave Amplitude"
colnames(arrhythmia.uci)[177] <- "Channel DII T wave Amplitude"
colnames(arrhythmia.uci)[178] <- "Channel DII QRSA"
colnames(arrhythmia.uci)[179] <- "Channel DII QRSTA"
colnames(arrhythmia.uci)[180] <- "Channel DIII JJ wave Amplitude"
colnames(arrhythmia.uci)[181] <- "Channel DIII Q wave Amplitude"
colnames(arrhythmia.uci)[182] <- "Channel DIII R wave Amplitude"
colnames(arrhythmia.uci)[183] <- "Channel DIII S wave Amplitude"
colnames(arrhythmia.uci)[184] <- "Channel DIII R' wave Amplitude"
colnames(arrhythmia.uci)[185] <- "Channel DIII S' wave Amplitude"
colnames(arrhythmia.uci)[186] <- "Channel DIII P wave Amplitude"
colnames(arrhythmia.uci)[187] <- "Channel DIII T wave Amplitude"
colnames(arrhythmia.uci)[188] <- "Channel DIII QRSA"
colnames(arrhythmia.uci)[189] <- "Channel DIII QRSTA"
colnames(arrhythmia.uci)[190] <- "Channel AVR JJ wave Amplitude"
colnames(arrhythmia.uci)[191] <- "Channel AVR Q wave Amplitude"
colnames(arrhythmia.uci)[192] <- "Channel AVR R wave Amplitude"
colnames(arrhythmia.uci)[193] <- "Channel AVR S wave Amplitude"
colnames(arrhythmia.uci)[194] <- "Channel AVR R' wave Amplitude"
colnames(arrhythmia.uci)[195] <- "Channel AVR S' wave Amplitude"
colnames(arrhythmia.uci)[196] <- "Channel AVR P wave Amplitude"
colnames(arrhythmia.uci)[197] <- "Channel AVR T wave Amplitude"
colnames(arrhythmia.uci)[198] <- "Channel AVR QRSA"
colnames(arrhythmia.uci)[199] <- "Channel AVR QRSTA"
colnames(arrhythmia.uci)[200] <- "Channel AVL JJ wave Amplitude"
colnames(arrhythmia.uci)[201] <- "Channel AVL Q wave Amplitude"
colnames(arrhythmia.uci)[202] <- "Channel AVL R wave Amplitude"
colnames(arrhythmia.uci)[203] <- "Channel AVL S wave Amplitude"
colnames(arrhythmia.uci)[204] <- "Channel AVL R' wave Amplitude"
colnames(arrhythmia.uci)[205] <- "Channel AVL S' wave Amplitude"
colnames(arrhythmia.uci)[206] <- "Channel AVL P wave Amplitude"
colnames(arrhythmia.uci)[207] <- "Channel AVL T wave Amplitude"
colnames(arrhythmia.uci)[208] <- "Channel AVL QRSA"
colnames(arrhythmia.uci)[209] <- "Channel AVL QRSTA"
colnames(arrhythmia.uci)[210] <- "Channel AVF JJ wave Amplitude"
colnames(arrhythmia.uci)[211] <- "Channel AVF Q wave Amplitude"
colnames(arrhythmia.uci)[212] <- "Channel AVF R wave Amplitude"
colnames(arrhythmia.uci)[213] <- "Channel AVF S wave Amplitude"
colnames(arrhythmia.uci)[214] <- "Channel AVF R' wave Amplitude"
colnames(arrhythmia.uci)[215] <- "Channel AVF S' wave Amplitude"
colnames(arrhythmia.uci)[216] <- "Channel AVF P wave Amplitude"
colnames(arrhythmia.uci)[217] <- "Channel AVF T wave Amplitude"
colnames(arrhythmia.uci)[218] <- "Channel AVF QRSA"
colnames(arrhythmia.uci)[219] <- "Channel AVF QRSTA"
colnames(arrhythmia.uci)[220] <- "Channel V1 JJ wave Amplitude"
colnames(arrhythmia.uci)[221] <- "Channel V1 Q wave Amplitude"
colnames(arrhythmia.uci)[222] <- "Channel V1 R wave Amplitude"
colnames(arrhythmia.uci)[223] <- "Channel V1 S wave Amplitude"
colnames(arrhythmia.uci)[224] <- "Channel V1 R' wave Amplitude"
colnames(arrhythmia.uci)[225] <- "Channel V1 S' wave Amplitude"
colnames(arrhythmia.uci)[226] <- "Channel V1 P wave Amplitude"
colnames(arrhythmia.uci)[227] <- "Channel V1 T wave Amplitude"
colnames(arrhythmia.uci)[228] <- "Channel V1 QRSA"
colnames(arrhythmia.uci)[229] <- "Channel V1 QRSTA"
colnames(arrhythmia.uci)[230] <- "Channel V2 JJ wave Amplitude"
colnames(arrhythmia.uci)[231] <- "Channel V2 Q wave Amplitude"
colnames(arrhythmia.uci)[232] <- "Channel V2 R wave Amplitude"
colnames(arrhythmia.uci)[233] <- "Channel V2 S wave Amplitude"
colnames(arrhythmia.uci)[234] <- "Channel V2 R' wave Amplitude"
colnames(arrhythmia.uci)[235] <- "Channel V2 S' wave Amplitude"
colnames(arrhythmia.uci)[236] <- "Channel V2 P wave Amplitude"
colnames(arrhythmia.uci)[237] <- "Channel V2 T wave Amplitude"
colnames(arrhythmia.uci)[238] <- "Channel V2 QRSA"
colnames(arrhythmia.uci)[239] <- "Channel V2 QRSTA"
colnames(arrhythmia.uci)[240] <- "Channel V3 JJ wave Amplitude"
colnames(arrhythmia.uci)[241] <- "Channel V3 Q wave Amplitude"
colnames(arrhythmia.uci)[242] <- "Channel V3 R wave Amplitude"
colnames(arrhythmia.uci)[243] <- "Channel V3 S wave Amplitude"
colnames(arrhythmia.uci)[244] <- "Channel V3 R' wave Amplitude"
colnames(arrhythmia.uci)[245] <- "Channel V3 S' wave Amplitude"
colnames(arrhythmia.uci)[246] <- "Channel V3 P wave Amplitude"
colnames(arrhythmia.uci)[247] <- "Channel V3 T wave Amplitude"
colnames(arrhythmia.uci)[248] <- "Channel V3 QRSA"
colnames(arrhythmia.uci)[249] <- "Channel V3 QRSTA"
colnames(arrhythmia.uci)[250] <- "Channel V4 JJ wave Amplitude"
colnames(arrhythmia.uci)[251] <- "Channel V4 Q wave Amplitude"
colnames(arrhythmia.uci)[252] <- "Channel V4 R wave Amplitude"
colnames(arrhythmia.uci)[253] <- "Channel V4 S wave Amplitude"
colnames(arrhythmia.uci)[254] <- "Channel V4 R' wave Amplitude"
colnames(arrhythmia.uci)[255] <- "Channel V4 S' wave Amplitude"
colnames(arrhythmia.uci)[256] <- "Channel V4 P wave Amplitude"
colnames(arrhythmia.uci)[257] <- "Channel V4 T wave Amplitude"
colnames(arrhythmia.uci)[258] <- "Channel V4 QRSA"
colnames(arrhythmia.uci)[259] <- "Channel V4 QRSTA"
colnames(arrhythmia.uci)[260] <- "Channel V5 JJ wave Amplitude"
colnames(arrhythmia.uci)[261] <- "Channel V5 Q wave Amplitude"
colnames(arrhythmia.uci)[262] <- "Channel V5 R wave Amplitude"
colnames(arrhythmia.uci)[263] <- "Channel V5 S wave Amplitude"
colnames(arrhythmia.uci)[264] <- "Channel V5 R' wave Amplitude"
colnames(arrhythmia.uci)[265] <- "Channel V5 S' wave Amplitude"
colnames(arrhythmia.uci)[266] <- "Channel V5 P wave Amplitude"
colnames(arrhythmia.uci)[267] <- "Channel V5 T wave Amplitude"
colnames(arrhythmia.uci)[268] <- "Channel V5 QRSA"
colnames(arrhythmia.uci)[269] <- "Channel V5 QRSTA"
colnames(arrhythmia.uci)[270] <- "Channel V6 JJ wave Amplitude"
colnames(arrhythmia.uci)[271] <- "Channel V6 Q wave Amplitude"
colnames(arrhythmia.uci)[272] <- "Channel V6 R wave Amplitude"
colnames(arrhythmia.uci)[273] <- "Channel V6 S wave Amplitude"
colnames(arrhythmia.uci)[274] <- "Channel V6 R' wave Amplitude"
colnames(arrhythmia.uci)[275] <- "Channel V6 S' wave Amplitude"
colnames(arrhythmia.uci)[276] <- "Channel V6 P wave Amplitude"
colnames(arrhythmia.uci)[277] <- "Channel V6 T wave Amplitude"
colnames(arrhythmia.uci)[278] <- "Channel V6 QRSA"
colnames(arrhythmia.uci)[279] <- "Channel V6 QRSTA"

colnames(arrhythmia.uci)[280] <- "Class code"



## ----Analysis Redefine types, include = FALSE------------------------------------------------------------------------------------------------------
# Redefine some type of the predictor from initial read from files. if using R 3.6 or earlier:
arrhythmia <- arrhythmia.uci %>% mutate(Sex = as.factor( ifelse(Sex == 0, 'M', 'F')),
                                        `T interval`= as.integer(`T interval`),
                                        `P interval` = as.integer(`P interval`),
                                        `QRST Vector angles` = as.integer(`QRST Vector angles`),
                                        `T Vector angles` = as.integer(`T Vector angles`),
                                        `P Vector angles` = as.integer(`P Vector angles`),
                                        `J Vector angles` = as.integer(`J Vector angles`),
                                        `Heart rate` = as.integer(`Heart rate`),
                                        `Channel DI Existence of ragged R wave` = as.factor(`Channel DI Existence of ragged R wave`),
                                        `Channel DI Existence of diphasic derivation of R wave` = as.factor(`Channel DI Existence of diphasic derivation of R wave`),
                                        `Channel DI Existence of ragged P wave` = as.factor(`Channel DI Existence of ragged P wave`),
                                        `Channel DI Existence of diphasic derivation of P wave` = as.factor(`Channel DI Existence of diphasic derivation of P wave`),
                                        `Channel DI Existence of ragged T wave` = as.factor(`Channel DI Existence of ragged T wave`),
                                        `Channel DI Existence of diphasic derivation of T wave` = as.factor(`Channel DI Existence of diphasic derivation of T wave`),
                                        `Channel DII Existence of ragged R wave` = as.factor(`Channel DII Existence of ragged R wave`),
                                        `Channel DII Existence of diphasic derivation of R wave` = as.factor(`Channel DII Existence of diphasic derivation of R wave`),
                                        `Channel DII Existence of ragged P wave` = as.factor(`Channel DII Existence of ragged P wave`),
                                        `Channel DII Existence of diphasic derivation of P wave` = as.factor(`Channel DII Existence of diphasic derivation of P wave`),
                                        `Channel DII Existence of ragged T wave` = as.factor(`Channel DII Existence of ragged T wave`),
                                        `Channel DII Existence of diphasic derivation of T wave` = as.factor(`Channel DII Existence of diphasic derivation of T wave`),
                                        `Channel DIII Existence of ragged R wave` = as.factor(`Channel DIII Existence of ragged R wave`),
                                        `Channel DIII Existence of diphasic derivation of R wave` = as.factor(`Channel DIII Existence of diphasic derivation of R wave`),
                                        `Channel DIII Existence of ragged P wave` = as.factor(`Channel DIII Existence of ragged P wave`),
                                        `Channel DIII Existence of diphasic derivation of P wave` = as.factor(`Channel DIII Existence of diphasic derivation of P wave`),
                                        `Channel DIII Existence of ragged T wave` = as.factor(`Channel DIII Existence of ragged T wave`),
                                        `Channel DIII Existence of diphasic derivation of T wave` = as.factor(`Channel DIII Existence of diphasic derivation of T wave`),
                                        `Channel AVR Existence of ragged R wave` = as.factor(`Channel AVR Existence of ragged R wave`),
                                        `Channel AVR Existence of diphasic derivation of R wave` = as.factor(`Channel AVR Existence of diphasic derivation of R wave`),
                                        `Channel AVR Existence of ragged P wave` = as.factor(`Channel AVR Existence of ragged P wave`),
                                        `Channel AVR Existence of diphasic derivation of P wave` = as.factor(`Channel AVR Existence of diphasic derivation of P wave`),
                                        `Channel AVR Existence of ragged T wave` = as.factor(`Channel AVR Existence of ragged T wave`),
                                        `Channel AVR Existence of diphasic derivation of T wave` = as.factor(`Channel AVR Existence of diphasic derivation of T wave`),
                                        `Channel AVL Existence of ragged R wave` = as.factor(`Channel AVL Existence of ragged R wave`),
                                        `Channel AVL Existence of diphasic derivation of R wave` = as.factor(`Channel AVL Existence of diphasic derivation of R wave`),
                                        `Channel AVL Existence of ragged P wave` = as.factor(`Channel AVL Existence of ragged P wave`),
                                        `Channel AVL Existence of diphasic derivation of P wave` = as.factor(`Channel AVL Existence of diphasic derivation of P wave`),
                                        `Channel AVL Existence of ragged T wave` = as.factor(`Channel AVL Existence of ragged T wave`),
                                        `Channel AVL Existence of diphasic derivation of T wave` = as.factor(`Channel AVL Existence of diphasic derivation of T wave`),
                                        `Channel AVF Existence of ragged R wave` = as.factor(`Channel AVF Existence of ragged R wave`),
                                        `Channel AVF Existence of diphasic derivation of R wave` = as.factor(`Channel AVF Existence of diphasic derivation of R wave`),
                                        `Channel AVF Existence of ragged P wave` = as.factor(`Channel AVF Existence of ragged P wave`),
                                        `Channel AVF Existence of diphasic derivation of P wave` = as.factor(`Channel AVF Existence of diphasic derivation of P wave`),
                                        `Channel AVF Existence of ragged T wave` = as.factor(`Channel AVF Existence of ragged T wave`),
                                        `Channel AVF Existence of diphasic derivation of T wave` = as.factor(`Channel AVF Existence of diphasic derivation of T wave`),
                                        `Channel V1 Existence of ragged R wave` = as.factor(`Channel V1 Existence of ragged R wave`),
                                        `Channel V1 Existence of diphasic derivation of R wave` = as.factor(`Channel V1 Existence of diphasic derivation of R wave`),
                                        `Channel V1 Existence of ragged P wave` = as.factor(`Channel V1 Existence of ragged P wave`),
                                        `Channel V1 Existence of diphasic derivation of P wave` = as.factor(`Channel V1 Existence of diphasic derivation of P wave`),
                                        `Channel V1 Existence of ragged T wave` = as.factor(`Channel V1 Existence of ragged T wave`),
                                        `Channel V1 Existence of diphasic derivation of T wave` = as.factor(`Channel V1 Existence of diphasic derivation of T wave`),
                                        `Channel V2 Existence of ragged R wave` = as.factor(`Channel V2 Existence of ragged R wave`),
                                        `Channel V2 Existence of diphasic derivation of R wave` = as.factor(`Channel V2 Existence of diphasic derivation of R wave`),
                                        `Channel V2 Existence of ragged P wave` = as.factor(`Channel V2 Existence of ragged P wave`),
                                        `Channel V2 Existence of diphasic derivation of P wave` = as.factor(`Channel V2 Existence of diphasic derivation of P wave`),
                                        `Channel V2 Existence of ragged T wave` = as.factor(`Channel V2 Existence of ragged T wave`),
                                        `Channel V2 Existence of diphasic derivation of T wave` = as.factor(`Channel V2 Existence of diphasic derivation of T wave`),
                                        `Channel V3 Existence of ragged R wave` = as.factor(`Channel V3 Existence of ragged R wave`),
                                        `Channel V3 Existence of diphasic derivation of R wave` = as.factor(`Channel V3 Existence of diphasic derivation of R wave`),
                                        `Channel V3 Existence of ragged P wave` = as.factor(`Channel V3 Existence of ragged P wave`),
                                        `Channel V3 Existence of diphasic derivation of P wave` = as.factor(`Channel V3 Existence of diphasic derivation of P wave`),
                                        `Channel V3 Existence of ragged T wave` = as.factor(`Channel V3 Existence of ragged T wave`),
                                        `Channel V3 Existence of diphasic derivation of T wave` = as.factor(`Channel V3 Existence of diphasic derivation of T wave`),
                                        `Channel V4 Existence of ragged R wave` = as.factor(`Channel V4 Existence of ragged R wave`),
                                        `Channel V4 Existence of diphasic derivation of R wave` = as.factor(`Channel V4 Existence of diphasic derivation of R wave`),
                                        `Channel V4 Existence of ragged P wave` = as.factor(`Channel V4 Existence of ragged P wave`),
                                        `Channel V4 Existence of diphasic derivation of P wave` = as.factor(`Channel V4 Existence of diphasic derivation of P wave`),
                                        `Channel V4 Existence of ragged T wave` = as.factor(`Channel V4 Existence of ragged T wave`),
                                        `Channel V4 Existence of diphasic derivation of T wave` = as.factor(`Channel V4 Existence of diphasic derivation of T wave`),
                                        `Channel V5 Existence of ragged R wave` = as.factor(`Channel V5 Existence of ragged R wave`),
                                        `Channel V5 Existence of diphasic derivation of R wave` = as.factor(`Channel V5 Existence of diphasic derivation of R wave`),
                                        `Channel V5 Existence of ragged P wave` = as.factor(`Channel V5 Existence of ragged P wave`),
                                        `Channel V5 Existence of diphasic derivation of P wave` = as.factor(`Channel V5 Existence of diphasic derivation of P wave`),
                                        `Channel V5 Existence of ragged T wave` = as.factor(`Channel V5 Existence of ragged T wave`),
                                        `Channel V5 Existence of diphasic derivation of T wave` = as.factor(`Channel V5 Existence of diphasic derivation of T wave`),
                                        `Channel V6 Existence of ragged R wave` = as.factor(`Channel V6 Existence of ragged R wave`),
                                        `Channel V6 Existence of diphasic derivation of R wave` = as.factor(`Channel V6 Existence of diphasic derivation of R wave`),
                                        `Channel V6 Existence of ragged P wave` = as.factor(`Channel V6 Existence of ragged P wave`),
                                        `Channel V6 Existence of diphasic derivation of P wave` = as.factor(`Channel V6 Existence of diphasic derivation of P wave`),
                                        `Channel V6 Existence of ragged T wave` = as.factor(`Channel V6 Existence of ragged T wave`),
                                        `Channel V6 Existence of diphasic derivation of T wave` = as.factor(`Channel V6 Existence of diphasic derivation of T wave`),
                                        `Class code` = as.factor(`Class code`)
                                        )



## ----Analysis new structure, echo=FALSE, size='scriptsize'-----------------------------------------------------------------------------------------
# Display predictors (columns) of the new dataset
str(arrhythmia, list.len = 25)


## ----Analysis Cleaning,warning=FALSE---------------------------------------------------------------------------------------------------------------
sd.arrhythmia <- apply(arrhythmia,2,sd, na.rm=TRUE)
sd0.list.arrhythmia <- 
  names(sd.arrhythmia[sd.arrhythmia == 0 & !is.na(sd.arrhythmia)]) # check is.na

# Identify the column number to be deleted
ix <- which(colnames(arrhythmia) %in% sd0.list.arrhythmia)
variable.names(arrhythmia[ix])



## ----Analysis Cleaning delete columns--------------------------------------------------------------------------------------------------------------

arrhythmia <- arrhythmia[,-ix]


## ----Analysis Cleaning working with NA, echo = FALSE, warning=FALSE--------------------------------------------------------------------------------
# counts the number of NAs per column, resulting in:
colna <- colSums(is.na(arrhythmia))
colna <- colna[colna > 0]
colna %>% knitr::kable(caption = 'Table of Ocurrence of NA', col.names = c( 'Ocurrences')) %>%
  kable_styling(full_width = F, position = "left")


## ----Delete J Vector angles Predictor--------------------------------------------------------------------------------------------------------------

arrhythmia <- arrhythmia %>% select(-`J Vector angles`)



## ----Missing P Vector angles Summary, echo = FALSE-------------------------------------------------------------------------------------------------
summary(arrhythmia$`P Vector angles`)


## ----Missing P Vector angles Histogram, echo = FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------
arrhythmia %>% ggplot(aes(`P Vector angles`)) + geom_histogram() + 
  geom_vline( xintercept = mean(arrhythmia$`P Vector angles`,na.rm = TRUE), colour = "blue") +
  geom_vline( xintercept = median(arrhythmia$`P Vector angles`,na.rm = TRUE), colour = "green")


## ----Missing P Vector angles replacement, size='small'---------------------------------------------------------------------------------------------
arrhythmia$`P Vector angles`[is.na(arrhythmia$`P Vector angles`)] <- 
  median(arrhythmia$`P Vector angles`,na.rm = TRUE)


## ----Missing T Vector angles Summary, echo = FALSE-------------------------------------------------------------------------------------------------
summary(arrhythmia$`T Vector angles`)


## ----Missing T Vector angles Histogram, echo = FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------
arrhythmia %>% ggplot(aes(`T Vector angles`)) + geom_histogram() + 
  geom_vline( xintercept = mean(arrhythmia$`T Vector angles`,na.rm = TRUE), colour = "blue") +
  geom_vline( xintercept = median(arrhythmia$`T Vector angles`,na.rm = TRUE), colour = "green") + 
  ggtitle("Histogram of T Vector angles")


## ----Missing T Vector angles replacement, size='small'---------------------------------------------------------------------------------------------
arrhythmia$`T Vector angles`[is.na(arrhythmia$`T Vector angles`)] <- 
  median(arrhythmia$`T Vector angles`,na.rm = TRUE)


## ----Missing QRST Vector angles analysis, echo=FALSE, size='small'---------------------------------------------------------------------------------
arrhythmia %>% filter(is.na(`QRST Vector angles`)) %>% select(Age, Sex, Height, Weight, `QRS duration`,`P-R interval`, `Q-T interval`, `T interval`, `P interval` ,`QRS Vector angles`)


## ----Missing QRST Vector angles Summary, echo = FALSE----------------------------------------------------------------------------------------------
summary(arrhythmia$`QRST Vector angles`)


## ----Missing QRST Vector angles Histogram, echo = FALSE, message=FALSE, warning=FALSE--------------------------------------------------------------
arrhythmia %>% ggplot(aes(`QRST Vector angles`)) + geom_histogram() + 
  geom_vline( xintercept = mean(arrhythmia$`QRST Vector angles`,na.rm = TRUE), colour = "blue") +
  geom_vline( xintercept = median(arrhythmia$`QRST Vector angles`,na.rm = TRUE), colour = "green") + 
  ggtitle("Histogram of QRST Vector angles")


## ----Missing QRST Vector angles replacement, size='small'------------------------------------------------------------------------------------------
arrhythmia$`QRST Vector angles`[is.na(arrhythmia$`QRST Vector angles`)] <- 
  median(arrhythmia$`QRST Vector angles`,na.rm = TRUE)


## ----Missing Heart rate Analysis, echo=FALSE, size='small'-----------------------------------------------------------------------------------------
arrhythmia %>% filter(is.na(`Heart rate`)) %>% select(Age, Sex, Height, Weight, `QRS duration`,`P-R interval`, `Q-T interval`, `T interval`, `P interval`)


## ----Missing Heart rate Summary, echo = FALSE------------------------------------------------------------------------------------------------------
summary(arrhythmia$`Heart rate`)


## ----Missing Heart rate Histogram, echo = FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------
arrhythmia %>% ggplot(aes(`Heart rate`)) + geom_histogram() + 
  geom_vline( xintercept = mean(arrhythmia$`Heart rate`,na.rm = TRUE), colour = "blue") +
  geom_vline( xintercept = median(arrhythmia$`Heart rate`,na.rm = TRUE), colour = "green") + 
  ggtitle("Histogram of Heart rate")


## ----Missing Heart rate replacement, size='small'--------------------------------------------------------------------------------------------------
arrhythmia$`Heart rate`[is.na(arrhythmia$`Heart rate`)] <- median(arrhythmia$`Heart rate`,na.rm = TRUE)


## ----Correlation, echo=FALSE-----------------------------------------------------------------------------------------------------------------------
data_numeric <- select_if(arrhythmia, is.numeric)             # Subset numeric columns only
cor_data_numeric <- cor(data_numeric)

#given matrix with entries TRUE in the lower  triangle. With this I use half of the matrix, because it is symmetric to de diagonal and only need the data one time (cor(a,b = cor(b,a)))
# The diagonal it is set to NA because is 1 but I do not needed for computing (cor(a,a) = 1)
cor_data_numeric[lower.tri(cor_data_numeric, diag = TRUE)] <- NA # Should the diagonal be included?


## ----Correlation creating the tidy table, echo=FALSE,warning=FALSE---------------------------------------------------------------------------------
data2 <- data.frame(cor_data_numeric, check.names = FALSE)   
data2 <- tibble::rownames_to_column(data2, "correlated.1") # Apply rownames_to_column
cor_arrhythmia_numeric <- data2 %>%  gather(correlated.2,value,2:ncol(data2))
cor_arrhythmia_numeric %>% slice_max(order_by = value, n = 10) %>% knitr::kable(caption = "Top 10 Positive Correlation between predictors")
cor_arrhythmia_numeric %>% slice_min(order_by = value, n = 10) %>% knitr::kable(caption = "Top 10 Negative Correlation between predictors")


## ----Correlation with limit, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------
limit <- 0.9
cor_arrhythmia_numeric %>% filter(value < -limit | value > limit) %>% count() %>% knitr::kable( caption = "Correlation over 0.9 & under -0.9") %>%
  kable_styling(full_width = F, position = "left")


## ----Correlation histogram, echo=FALSE-------------------------------------------------------------------------------------------------------------
cor_arrhythmia_numeric %>% ggplot(aes(value)) + geom_histogram(bins = 20,na.rm = TRUE) + ggtitle("Distribution of Correlation between Predictors") + xlab("Correlation")


## ----Correlation of previous missing values P Vector angles, echo=FALSE, warning=FALSE-------------------------------------------------------------

cor_arrhythmia_numeric %>%  filter( correlated.1 %in% "P Vector angles" | correlated.2 %in% "P Vector angles") %>% slice_max(order_by = value, n = 5) %>% knitr::kable(caption = "Correlation for P Vector angles") %>%
  kable_styling(full_width = F, position = "left")
cor_arrhythmia_numeric %>%  filter( correlated.1 %in% "P Vector angles" | correlated.2 %in% "P Vector angles") %>% slice_min(order_by = value, n = 5) %>% knitr::kable(caption = "Correlation for P Vector angles") %>%
  kable_styling(full_width = F, position = "left")


## ----Correlation of T Vector angles, echo=FALSE, warning=FALSE-------------------------------------------------------------------------------------

cor_arrhythmia_numeric %>%  filter( correlated.1 %in% "T Vector angles" | correlated.2 %in% "T Vector angles") %>% slice_max(order_by = value, n = 5) %>% knitr::kable(caption = "Correlation for T Vector angles") %>%
  kable_styling(full_width = F, position = "left")
cor_arrhythmia_numeric %>%  filter( correlated.1 %in% "T Vector angles" | correlated.2 %in% "T Vector angles") %>% slice_min(order_by = value, n = 5) %>% knitr::kable(caption = "Correlation for T Vector angles") %>%
  kable_styling(full_width = F, position = "left")


## ----Correlation of QRST Vector angles, echo=FALSE, warning=FALSE----------------------------------------------------------------------------------

cor_arrhythmia_numeric %>%  filter( correlated.1 %in% "QRST Vector angles" | correlated.2 %in% "QRST Vector angles") %>% slice_max(order_by = value, n = 5) %>% knitr::kable(caption = "Correlation for QRST Vector angles") %>%
  kable_styling(full_width = F, position = "left")
cor_arrhythmia_numeric %>%  filter( correlated.1 %in% "QRST Vector angles" | correlated.2 %in% "QRST Vector angles") %>% slice_min(order_by = value, n = 5) %>% knitr::kable(caption = "Correlation for QRST Vector angles") %>%
  kable_styling(full_width = F, position = "left")


## ----Correlation Heart rate, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------

cor_arrhythmia_numeric %>%  filter( correlated.1 %in% "Heart rate" | correlated.2 %in% "Heart rate") %>% slice_max(order_by = value, n = 5) %>% knitr::kable(caption = "Correlation for Heart rate") %>%
  kable_styling(full_width = F, position = "left")
cor_arrhythmia_numeric %>%  filter( correlated.1 %in% "Heart rate" | correlated.2 %in% "Heart rate") %>% slice_min(order_by = value, n = 5) %>% knitr::kable(caption = "Correlation for Heart rate") %>%
  kable_styling(full_width = F, position = "left")


## ----Correlation Less Correlated, echo=FALSE, warning=FALSE----------------------------------------------------------------------------------------
limit <- 0.0001
cor_arrhythmia_numeric %>% filter(value > -limit & value < limit)  %>% knitr::kable(caption = "Less Correlated Preditors") %>%
  kable_styling(full_width = F, position = "left")


## ----Outliers Age Boxplot, echo=FALSE, fig.height = 3----------------------------------------------------------------------------------------------
arrhythmia %>% ggplot(aes(Age)) + geom_boxplot(fill="steelblue") + ggtitle("Age Outliers")


## ----Outliers Height Boxplot, echo=FALSE, fig.height = 3-------------------------------------------------------------------------------------------
arrhythmia %>% ggplot(aes(Height)) + geom_boxplot(fill="steelblue") + ggtitle("Height Outliers")


## ----Outliers Height Max, echo=FALSE---------------------------------------------------------------------------------------------------------------
slice_max(arrhythmia, order_by = Height, n = 2) %>% select(Height)


## ----Outliers Height in detail, echo=FALSE---------------------------------------------------------------------------------------------------------
slice_max(arrhythmia, order_by = Height, n = 2) %>% select(Age,Weight, Height)


## ----Outliers Height fixing------------------------------------------------------------------------------------------------------------------------
arrhythmia[arrhythmia$Height == 780,3] <- 78   # Height is column 3
arrhythmia[arrhythmia$Height == 608,3] <- 61   # Height is column 3



## ----Outliers Weight Boxplot, echo=FALSE, fig.height = 3-------------------------------------------------------------------------------------------
arrhythmia %>% ggplot(aes(Weight)) + geom_boxplot(fill="steelblue") + ggtitle("Weight Outliers")


## ----Outliers Weight Table with Age and Height, echo=FALSE-----------------------------------------------------------------------------------------
slice_max(arrhythmia, order_by = Weight) %>% select(Age,Weight, Height)


## ----Outliers Heart rate Boxplot, echo=FALSE, fig.height = 3---------------------------------------------------------------------------------------
arrhythmia %>% ggplot(aes(`Heart rate`)) + geom_boxplot(fill="steelblue") + ggtitle("Heart rate Outliers")


## ----Outliers Heart rate in detail, echo=FALSE-----------------------------------------------------------------------------------------------------
slice_max(arrhythmia, order_by = `Heart rate`, n = 2) %>% select(Age,Weight, Height,`Heart rate`)


## ----Create train set, validation set, include = FALSE---------------------------------------------------------------------------------------------
##########################################################
# Create train set, validation set (final hold-out test set)
##########################################################

# Validation set will be 20% of data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = arrhythmia$`Class code`, times = 1, p = 0.2, list = FALSE)
train_set <- arrhythmia[-test_index,]
validation_set <- arrhythmia[test_index,]


dim(train_set)
dim(validation_set)



## ----Create train set check Class code-------------------------------------------------------------------------------------------------------------
unique(train_set$`Class code`) %>% sort()
unique(validation_set$`Class code`) %>% sort()


## ----Create train set check train_set, echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------------------------------
left_join(train_set, Class, by = "Class code") %>% 
  group_by(`Class code`, `Class name`) %>% summarise( "N Ocurrences" = n(), Percentage = round(100*n()/nrow(train_set),digits = 1)) %>%
  knitr::kable(caption = "Presence of codes in train_set dataset")
  


## ----Create train set check validation_set, echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------
left_join(validation_set, Class, by = "Class code") %>% 
  group_by(`Class code`, `Class name`) %>% summarise( "N Ocurrences" = n(), Percentage = round(100*n()/nrow(validation_set),digits = 1)) %>%
  knitr::kable(caption = "Presence of codes in validation_set dataset")
  


## ----Create train set Unique value in train_set, echo=FALSE----------------------------------------------------------------------------------------
values_count <- sapply(lapply(train_set, unique), length)  # Identify variables with 1 value
variable.names(train_set[values_count == 1])


## ----Create train set delete predictors with not varianza in the train_set-------------------------------------------------------------------------
train_set <- train_set[,values_count > 1]
validation_set <- validation_set[,values_count > 1]


## ----Analysis delete unwanted objects, echo=FALSE--------------------------------------------------------------------------------------------------
rm(data_numeric, data2, cor_data_numeric, colna, ix, sd0.list.arrhythmia, sd.arrhythmia, values_count, limit)


## ----Evaluation metrics Binary Table, echo=FALSE, warning=FALSE------------------------------------------------------------------------------------
M <- matrix(c("True Positives (TP)","False Positives (FP)","False Negatives (FN)","True Negatives (TN)"),2,byrow = TRUE) #%>%
rownames(M) <- c("Predicted Positive","Predicted Negative")
colnames(M) <- c("Actually Positive", "Actually Negative")

M %>% knitr::kable() %>%
  kable_styling(full_width = F)


## ----Evaluation metrics Multiple Table, echo=FALSE, warning=FALSE----------------------------------------------------------------------------------
M3 <- matrix(c("True A (TA)","False A (FA.B)","False A (FA.C)","False B (FB.A)","True B (TB)","False B (FB.C)","False C (FC.A)","False C (FC.B)","True C (TC)"),3,byrow = TRUE) #%>%
rownames(M3) <- c("Predicted A", "Predicted B", "Predicted C")
colnames(M3) <- c("Actually A", "Actually B", "Actually C")
M3 %>% knitr::kable() %>%
  kable_styling(full_width = F)


## ----Define new dataset for first Prediction, include=FALSE----------------------------------------------------------------------------------------

# The first "Class code" is "Normal" and all the rest are some kind of "Arrhythmia"
# Train dataset prediction1
train_set_p1 <- train_set
train_set_p1$`Class code` <- ifelse(train_set_p1$`Class code` == 1,"Normal","Arrhythmia") %>% factor()

# validation for prediction1
validation_set_p1 <- validation_set
validation_set_p1$`Class code` <- ifelse(validation_set_p1$`Class code` == 1,"Normal","Arrhythmia") %>% factor()



## ----Model just the most common first Prediction, echo=FALSE, warning=FALSE------------------------------------------------------------------------
predicted_value <- rep("Normal", times = nrow(validation_set)) %>% factor() # Using Normal class (1) as the only answer
cm <- confusionMatrix(predicted_value, validation_set_p1$`Class code`)  # or positive = "Arrhythmia"

cm$overall["Accuracy"]
cm$byClass["Sensitivity"]
cm$byClass["Specificity"]


## ----Model just the most common first Prediction Table, echo=FALSE, size='scriptsize'--------------------------------------------------------------

# Table names
Predicted <- predicted_value
Actually <- validation_set_p1$`Class code`
#Computes the crosstable calculations
CrossTable(Predicted, Actually)


## ----Model just the most common first Prediction Result,echo=FALSE, warning=FALSE------------------------------------------------------------------
Pred1_results <- data.frame(Model = "Just the most common",
                                     Accuracy = cm$overall["Accuracy"],
                                     Sensitivity = cm$byClass["Sensitivity"],
                                     Specificity = cm$byClass["Specificity"] , row.names = NULL)
Pred1_results %>% knitr::kable(caption = "Prediction Normal/Arrhytmia Summary") %>%
  kable_styling(full_width = F)


## ----Model just the most common Second Prediction, echo=FALSE, warning=FALSE-----------------------------------------------------------------------
y_hat <- rep(1, times = nrow(validation_set)) %>% factor() # Using Normal class (1) as the only answer
cm <- confusionMatrix(y_hat, validation_set$`Class code`)


cm$overall["Accuracy"]
cm$byClass[,1:2]


## ----Model just the most common Second Prediction Table, echo=FALSE, size='scriptsize'-------------------------------------------------------------
# Table names
Predicted <- y_hat
Actually <- validation_set$`Class code`
#Computes the crosstable calculations
CrossTableNarrow(Predicted, Actually, digits = 2)


## ----Model just the most common Second Prediction Result, echo=FALSE, warning=FALSE----------------------------------------------------------------
Pred2_results <- data.frame(Model = "Just the most common",
                            Accuracy = cm$overall["Accuracy"],
                            Code.1 = row.names(cm$byClass)[1],
                            Sensitivity.1 = cm$byClass[1,1],
                            Specificity.1 = cm$byClass[1,2],
                            Code.2 = row.names(cm$byClass)[2],
                            Sensitivity.2 = cm$byClass[2,1],
                            Specificity.2 = cm$byClass[2,2],
                            Code.3 = row.names(cm$byClass)[3],
                            Sensitivity.3 = cm$byClass[3,1],
                            Specificity.3 = cm$byClass[3,2],
                            Code.4 = row.names(cm$byClass)[4],
                            Sensitivity.4 = cm$byClass[4,1],
                            Specificity.4 = cm$byClass[4,2],
                            Code.5 = row.names(cm$byClass)[5],
                            Sensitivity.5 = cm$byClass[5,1],
                            Specificity.5 = cm$byClass[5,2],
                            Code.6 = row.names(cm$byClass)[6],
                            Sensitivity.6 = cm$byClass[6,1],
                            Specificity.6 = cm$byClass[6,2],
                            Code.7 = row.names(cm$byClass)[7],
                            Sensitivity.7 = cm$byClass[7,1],
                            Specificity.7 = cm$byClass[7,2],
                            Code.8 = row.names(cm$byClass)[8],
                            Sensitivity.8 = cm$byClass[8,1],
                            Specificity.8 = cm$byClass[8,2],
                            Code.9 = row.names(cm$byClass)[9],
                            Sensitivity.9 = cm$byClass[9,1],
                            Specificity.9 = cm$byClass[9,2],
                            Code.10 = row.names(cm$byClass)[10],
                            Sensitivity.10 = cm$byClass[10,1],
                            Specificity.10 = cm$byClass[10,2],
                            Code.11 = row.names(cm$byClass)[11],
                            Sensitivity.11 = cm$byClass[11,1],
                            Specificity.11 = cm$byClass[11,2],
                            Code.12 = row.names(cm$byClass)[12],
                            Sensitivity.12 = cm$byClass[12,1],
                            Specificity.12 = cm$byClass[12,2],
                            Code.13 = row.names(cm$byClass)[13],
                            Sensitivity.13 = cm$byClass[13,1],
                            Specificity.13 = cm$byClass[13,2],
                            row.names = NULL)
Pred2_results %>% t() %>% knitr::kable(caption = "Prediction All Arrhytmia Classification Summary")


## ----K-Nearest Neighbors Model first Prediction train, echo=FALSE, warning=FALSE, fig.height = 4---------------------------------------------------
knn.control <- trainControl(method = "repeatedcv", number = 20, repeats = 3)   


set.seed(1, sample.kind="Rounding")
fit_knn <- train(`Class code` ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(2, 20, 2)),
             data = train_set_p1, na.action=na.pass)

ggplot(fit_knn) + ggtitle("K-Nearest Neighbors")


## ----K-Nearest Neighbors Model first Prediction train fit, echo=FALSE, warning=FALSE, size='scriptsize'--------------------------------------------
fit_knn


## ----K-Nearest Neighbors Model first Prediction Variable Importance, echo=FALSE, warning=FALSE, size='scriptsize'----------------------------------
varImp(fit_knn)
varimp_knn_p1 <- varImp(fit_knn)$importance
ggplot(varImp(fit_knn), top = 20) + ggtitle("K-Nearest Neighbors Model Top 20 feature")


## ----K-Nearest Neighbors Model first Prediction Prediction, echo=FALSE, warning=FALSE, size='scriptsize'-------------------------------------------
y_hat_knn <- predict(fit_knn, validation_set_p1, type = "raw")
cm <- confusionMatrix(data = y_hat_knn, reference = validation_set_p1$`Class code`) 
cm


## ----K-Nearest Neighbors Model first Prediction Table, echo=FALSE, size='scriptsize'---------------------------------------------------------------
# Table names
Predicted <- y_hat_knn
Actually <- validation_set_p1$`Class code`
#Computes the crosstable calculations
CrossTable(Predicted, Actually, digits = 3, prop.chisq = FALSE, prop.t=FALSE)


## ----K-Nearest Neighbors Model first Prediction Result, echo=FALSE, warning=FALSE------------------------------------------------------------------
Pred1_results <- bind_rows(Pred1_results,
                           data.frame(Model = "K-Nearest Neighbors",
                                     Accuracy = cm$overall["Accuracy"],
                                     Sensitivity = cm$byClass["Sensitivity"],
                                     Specificity = cm$byClass["Specificity"], row.names = NULL ))
Pred1_results %>% knitr::kable(caption = "Prediction Normal/Arrhytmia Summary") %>%
  kable_styling(full_width = F)



## ----K-Nearest Neighbors Model Second Prediction train, echo=FALSE, warning=FALSE, fig.height = 4--------------------------------------------------
# Predict region using KNN


knn.control <- trainControl(method = "repeatedcv", number = 20, repeats = 3)

set.seed(1, sample.kind="Rounding")
fit_knn <- train(`Class code` ~ .,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 17, 1)),
             trControl = knn.control,
             data = train_set, na.action=na.pass)

ggplot(fit_knn) + ggtitle("K-Nearest Neighbors")


## ----K-Nearest Neighbors Model Second Prediction train fit, echo=FALSE, warning=FALSE, size='scriptsize'-------------------------------------------
fit_knn


## ----K-Nearest Neighbors Model Second Prediction Variable Importance, echo=FALSE, warning=FALSE, size='scriptsize'---------------------------------
options(digits =2)
varImp(fit_knn)
varimp_knn_p2 <- varImp(fit_knn)$importance
options(digits =7) # default
ggplot(varImp(fit_knn), top = 8) + ggtitle("K-Nearest Neighbors Model Top 8 feature")


## ----K-Nearest Neighbors Model Second Prediction Prediction, echo=FALSE, warning=FALSE, size='scriptsize'------------------------------------------

options(digits =2)

y_hat_knn <- predict(fit_knn, validation_set, type = "raw")
cm <- confusionMatrix(data = y_hat_knn, reference = validation_set$`Class code`)
cm
options(digits =7) # default


## ----K-Nearest Neighbors Model Second Prediction Table, echo=FALSE, size='scriptsize'--------------------------------------------------------------
# Table names
Predicted <- y_hat_knn
Actually <- validation_set$`Class code`
#Computes the crosstable calculations
CrossTableNarrow(Predicted, Actually, digits = 2, prop.chisq = FALSE, prop.t=FALSE)


## ----K-Nearest Neighbors Model Second Prediction Result, echo=FALSE, warning=FALSE-----------------------------------------------------------------
Pred2_results <- bind_rows(Pred2_results,
                           data.frame(Model = "K-Nearest Neighbors",
                            Accuracy = cm$overall["Accuracy"],
                            Code.1 = row.names(cm$byClass)[1],
                            Sensitivity.1 = cm$byClass[1,1],
                            Specificity.1 = cm$byClass[1,2],
                            Code.2 = row.names(cm$byClass)[2],
                            Sensitivity.2 = cm$byClass[2,1],
                            Specificity.2 = cm$byClass[2,2],
                            Code.3 = row.names(cm$byClass)[3],
                            Sensitivity.3 = cm$byClass[3,1],
                            Specificity.3 = cm$byClass[3,2],
                            Code.4 = row.names(cm$byClass)[4],
                            Sensitivity.4 = cm$byClass[4,1],
                            Specificity.4 = cm$byClass[4,2],
                            Code.5 = row.names(cm$byClass)[5],
                            Sensitivity.5 = cm$byClass[5,1],
                            Specificity.5 = cm$byClass[5,2],
                            Code.6 = row.names(cm$byClass)[6],
                            Sensitivity.6 = cm$byClass[6,1],
                            Specificity.6 = cm$byClass[6,2],
                            Code.7 = row.names(cm$byClass)[7],
                            Sensitivity.7 = cm$byClass[7,1],
                            Specificity.7 = cm$byClass[7,2],
                            Code.8 = row.names(cm$byClass)[8],
                            Sensitivity.8 = cm$byClass[8,1],
                            Specificity.8 = cm$byClass[8,2],
                            Code.9 = row.names(cm$byClass)[9],
                            Sensitivity.9 = cm$byClass[9,1],
                            Specificity.9 = cm$byClass[9,2],
                            Code.10 = row.names(cm$byClass)[10],
                            Sensitivity.10 = cm$byClass[10,1],
                            Specificity.10 = cm$byClass[10,2],
                            Code.11 = row.names(cm$byClass)[11],
                            Sensitivity.11 = cm$byClass[11,1],
                            Specificity.11 = cm$byClass[11,2],
                            Code.12 = row.names(cm$byClass)[12],
                            Sensitivity.12 = cm$byClass[12,1],
                            Specificity.12 = cm$byClass[12,2],
                            Code.13 = row.names(cm$byClass)[13],
                            Sensitivity.13 = cm$byClass[13,1],
                            Specificity.13 = cm$byClass[13,2],
                            row.names = NULL))
Pred2_results %>% t() %>% knitr::kable(caption = "Prediction All Arrhytmia Classification Summary")


## ----Decision Tree Model First Prediction train, echo=FALSE, warning=FALSE, fig.height = 4---------------------------------------------------------
# use cross validation to choose cp

# rpart need Valid Column Names 
train_set2_p1 <- data.frame(train_set_p1)
set.seed(1, sample.kind="Rounding")
fit_rpart <- train(Class.code ~ ., method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = train_set2_p1, na.action=na.pass)


ggplot(fit_rpart) + ggtitle("Decision Tree")


## ----Decision Tree Model First Prediction train fit, echo=FALSE, warning=FALSE, size='scriptsize'--------------------------------------------------
fit_rpart


## ----Decision Tree Model First Prediction plot, echo=FALSE, warning=FALSE--------------------------------------------------------------------------
# access the final model and plot it


rpart.plot(fit_rpart$finalModel, extra = 2, roundint=FALSE,
  box.palette = list("Gy", "Gn", "Bu", "Bn", "Or", "Rd", "Pu", "RdYlGn", "GnYlRd", "BlGnYl","YlGnBl",
    "GyGy", "GyGn")) # specify 13 colors)


## ----Decision Tree Model First Prediction Variable Importance, echo=FALSE, warning=FALSE, size='scriptsize'----------------------------------------
options(digits =2)
varImp(fit_rpart)
varimp_rpart_p1 <- varImp(fit_rpart)$importance
options(digits =7) # default


## ----Decision Tree Model First Prediction Predict, echo=FALSE, warning=FALSE, size='scriptsize'----------------------------------------------------
# compute accuracy
options(digits =2)

y_hat_rpart <- predict(fit_rpart, data.frame(validation_set_p1), type = "raw")
cm <- confusionMatrix(y_hat_rpart, reference = validation_set_p1$`Class code`)
cm
options(digits =7) # default


## ----Decision Tree Model First Prediction Prune, echo=FALSE, warning=FALSE-------------------------------------------------------------------------
# prune the tree 
fit_rpart <- rpart(Class.code ~ ., data = train_set2_p1, control = rpart.control(cp = 0.027, minsplit = 2))
pruned_fit <- prune(fit_rpart, cp = 0.01)

plot(pruned_fit, margin = 0.1)
text(pruned_fit, cex = 0.75)


## ----Decision Tree Model First Prediction Prune confusionMatrix, echo=FALSE, warning=FALSE, size='scriptsize'--------------------------------------
# compute accuracy
options(digits =2)

y_hat_rpart <- predict(pruned_fit, data.frame(validation_set_p1), type = "class")

cm <- confusionMatrix(y_hat_rpart, reference = validation_set_p1$`Class code`)
cm
options(digits =7) # default


## ----Decision Tree Model First Prediction Table, echo=FALSE, size='scriptsize'---------------------------------------------------------------------
# Table names
Predicted <- y_hat_rpart
Actually <- validation_set$`Class code`
#Computes the crosstable calculations
CrossTableNarrow(Predicted, Actually, digits = 2, prop.chisq = FALSE, prop.t=FALSE)


## ----Decision Tree Model First Prediction Result, echo=FALSE, warning=FALSE------------------------------------------------------------------------
Pred1_results <- bind_rows(Pred1_results,
                           data.frame(Model = "Decision Tree Classifier",
                                     Accuracy = cm$overall["Accuracy"],
                                     Sensitivity = cm$byClass["Sensitivity"],
                                     Specificity = cm$byClass["Specificity"], row.names = NULL ))
Pred1_results %>% knitr::kable(caption = "Prediction Normal/Arrhytmia Summary") %>%
  kable_styling(full_width = F)


## ----Decision Tree Model Second Prediction train, echo=FALSE, warning=FALSE, fig.height = 4--------------------------------------------------------
# use cross validation to choose cp

# rpart need Valid Column Names 
train_set2 <- data.frame(train_set)
set.seed(1, sample.kind="Rounding")
fit_rpart <- train(Class.code ~ ., method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = train_set2, na.action=na.pass)


ggplot(fit_rpart) + ggtitle("Decision Tree")


## ----Decision Tree Model Second Prediction train fit, echo=FALSE, warning=FALSE, size='scriptsize'-------------------------------------------------
fit_rpart


## ----Decision Tree Model Second Prediction plot, echo=FALSE, warning=FALSE-------------------------------------------------------------------------
# access the final model and plot it


rpart.plot(fit_rpart$finalModel, extra = 2, roundint=FALSE,
  box.palette = list("Gy", "Gn", "Bu", "Bn", "Or", "Rd", "Pu", "RdYlGn", "GnYlRd", "BlGnYl","YlGnBl",
    "GyGy", "GyGn")) # specify 13 colors)


## ----Decision Tree Model Second Prediction Variable Importance, echo=FALSE, warning=FALSE, size='scriptsize'---------------------------------------
options(digits =2)
varImp(fit_rpart)
varimp_rpart_p2 <- varImp(fit_rpart)$importance
options(digits =7) # default


## ----Decision Tree Model Second Prediction Predict, echo=FALSE, warning=FALSE, size='scriptsize'---------------------------------------------------
# compute accuracy
options(digits =2)

y_hat_rpart <- predict(fit_rpart, data.frame(validation_set), type = "raw")
cm <- confusionMatrix(y_hat_rpart, reference = validation_set$`Class code`)
cm
options(digits =7) # default


## ----Decision Tree Model Second Prediction Prune, echo=FALSE, warning=FALSE------------------------------------------------------------------------
# prune the tree 

fit_rpart <- rpart(Class.code ~ ., data = train_set2, control = rpart.control(cp = 0.033, minsplit = 2))
pruned_fit <- prune(fit_rpart, cp = 0.01)

plot(pruned_fit, margin = 0.1)
text(pruned_fit, cex = 0.75)


## ----Decision Tree Model Second Prediction Prune ConfusionMatrix, echo=FALSE, warning=FALSE, size='scriptsize'-------------------------------------
# compute accuracy
options(digits =2)

y_hat_rpart <- predict(pruned_fit, data.frame(validation_set), type = "class")

cm <- confusionMatrix(y_hat_rpart, reference = validation_set$`Class code`)
cm
options(digits =7) # default


## ----Decision Tree Model Second Prediction Table, echo=FALSE, size='scriptsize'--------------------------------------------------------------------
# Table names
Predicted <- y_hat_rpart
Actually <- validation_set$`Class code`
#Computes the crosstable calculations
CrossTableNarrow(Predicted, Actually, digits = 2, prop.chisq = FALSE, prop.t=FALSE)


## ----Decision Tree Model Second Prediction Result, echo=FALSE, warning=FALSE-----------------------------------------------------------------------
Pred2_results <- bind_rows(Pred2_results,
                           data.frame(Model = "Decision Tree Classifier",
                            Accuracy = cm$overall["Accuracy"],
                            Code.1 = row.names(cm$byClass)[1],
                            Sensitivity.1 = cm$byClass[1,1],
                            Specificity.1 = cm$byClass[1,2],
                            Code.2 = row.names(cm$byClass)[2],
                            Sensitivity.2 = cm$byClass[2,1],
                            Specificity.2 = cm$byClass[2,2],
                            Code.3 = row.names(cm$byClass)[3],
                            Sensitivity.3 = cm$byClass[3,1],
                            Specificity.3 = cm$byClass[3,2],
                            Code.4 = row.names(cm$byClass)[4],
                            Sensitivity.4 = cm$byClass[4,1],
                            Specificity.4 = cm$byClass[4,2],
                            Code.5 = row.names(cm$byClass)[5],
                            Sensitivity.5 = cm$byClass[5,1],
                            Specificity.5 = cm$byClass[5,2],
                            Code.6 = row.names(cm$byClass)[6],
                            Sensitivity.6 = cm$byClass[6,1],
                            Specificity.6 = cm$byClass[6,2],
                            Code.7 = row.names(cm$byClass)[7],
                            Sensitivity.7 = cm$byClass[7,1],
                            Specificity.7 = cm$byClass[7,2],
                            Code.8 = row.names(cm$byClass)[8],
                            Sensitivity.8 = cm$byClass[8,1],
                            Specificity.8 = cm$byClass[8,2],
                            Code.9 = row.names(cm$byClass)[9],
                            Sensitivity.9 = cm$byClass[9,1],
                            Specificity.9 = cm$byClass[9,2],
                            Code.10 = row.names(cm$byClass)[10],
                            Sensitivity.10 = cm$byClass[10,1],
                            Specificity.10 = cm$byClass[10,2],
                            Code.11 = row.names(cm$byClass)[11],
                            Sensitivity.11 = cm$byClass[11,1],
                            Specificity.11 = cm$byClass[11,2],
                            Code.12 = row.names(cm$byClass)[12],
                            Sensitivity.12 = cm$byClass[12,1],
                            Specificity.12 = cm$byClass[12,2],
                            Code.13 = row.names(cm$byClass)[13],
                            Sensitivity.13 = cm$byClass[13,1],
                            Specificity.13 = cm$byClass[13,2],
                            row.names = NULL))
Pred2_results %>% t() %>% knitr::kable(caption = "Prediction All Arrhytmia Classification Summary")


## ----Random Forests Model First Predictions train, echo=FALSE, warning=FALSE, fig.height = 4-------------------------------------------------------
#Random Forests

set.seed(1, sample.kind="Rounding")
fit_rf <- train(`Class code` ~ ., method = "rf",
                     tuneGrid = data.frame(mtry = seq(10, 100, len = 25)),
                     data = train_set_p1)



ggplot(fit_rf) + ggtitle("Random Forests")


## ----Random Forests Model First Predictions train fit, echo=FALSE, warning=FALSE, size='scriptsize'------------------------------------------------
fit_rf


## ----Random Forests Model First Prediction Variable Importance, echo=FALSE, warning=FALSE, size='scriptsize'---------------------------------------
options(digits =2)
varImp(fit_rf)
varimp_rf_p1 <- varImp(fit_rf)$importance
options(digits =7) # default


## ----Random Forests Model First Prediction Prediction, echo=FALSE, warning=FALSE, size='scriptsize'------------------------------------------------
options(digits =2)
y_hat_rf <- predict(fit_rf, validation_set_p1, type = "raw")
cm <- confusionMatrix(data = y_hat_rf, reference = validation_set_p1$`Class code`)
cm
options(digits =7) # default


## ----Random Forests Model First Prediction Table, echo=FALSE, size='scriptsize'--------------------------------------------------------------------
# Table names
Predicted <- y_hat_rf
Actually <- validation_set_p1$`Class code`
#Computes the crosstable calculations
CrossTable(Predicted, Actually, digits = 2, prop.chisq = FALSE, prop.t=FALSE)


## ----Random Forests Model First Prediction Result, echo=FALSE, warning=FALSE-----------------------------------------------------------------------
Pred1_results <- bind_rows(Pred1_results,
                           data.frame(Model = "Random Forest Classifier",
                                     Accuracy = cm$overall["Accuracy"],
                                     Sensitivity = cm$byClass["Sensitivity"],
                                     Specificity = cm$byClass["Specificity"], row.names = NULL ))
Pred1_results %>% knitr::kable(caption = "Prediction Normal/Arrhytmia Summary") %>%
  kable_styling(full_width = F)


## ----Random Forests Model Second Predictions train, echo=FALSE, warning=FALSE, fig.height = 4------------------------------------------------------
#Random Forests

set.seed(1, sample.kind="Rounding")
fit_rf <- train(`Class code` ~ ., method = "rf",
                     tuneGrid = data.frame(mtry = seq(10, 100, len = 25)),
                     data = train_set)



ggplot(fit_rf) + ggtitle("Random Forests")


## ----Random Forests Model Second Predictions train fit, echo=FALSE, warning=FALSE, size='scriptsize'-----------------------------------------------
fit_rf


## ----Random Forests Model Second Prediction Variable Importance, echo=FALSE, warning=FALSE, size='scriptsize'--------------------------------------
options(digits =2)
varImp(fit_rf)
varimp_rf_p2 <- varImp(fit_rf)$importance
#ggplot(varImp(fit_knn))
options(digits =7) # default


## ----Random Forests Model Second Prediction Prediction, echo=FALSE, warning=FALSE, size='scriptsize'-----------------------------------------------
options(digits =2)
y_hat_rf <- predict(fit_rf, validation_set, type = "raw")
cm <- confusionMatrix(data = y_hat_rf, reference = validation_set$`Class code`) 
cm
options(digits =7) # default


## ----Random Forests Model Second Prediction Table, echo=FALSE, size='scriptsize'-------------------------------------------------------------------
# Table names
Predicted <- y_hat_rf
Actually <- validation_set$`Class code`
#Computes the crosstable calculations
CrossTableNarrow(Predicted, Actually, digits = 2, prop.chisq = FALSE, prop.t=FALSE)


## ----Random Forests Model Second Prediction Result, echo=FALSE, warning=FALSE----------------------------------------------------------------------
Pred2_results <- bind_rows(Pred2_results,
                           data.frame(Model = "Random Forest Classifier",
                            Accuracy = cm$overall["Accuracy"],
                            Code.1 = row.names(cm$byClass)[1],
                            Sensitivity.1 = cm$byClass[1,1],
                            Specificity.1 = cm$byClass[1,2],
                            Code.2 = row.names(cm$byClass)[2],
                            Sensitivity.2 = cm$byClass[2,1],
                            Specificity.2 = cm$byClass[2,2],
                            Code.3 = row.names(cm$byClass)[3],
                            Sensitivity.3 = cm$byClass[3,1],
                            Specificity.3 = cm$byClass[3,2],
                            Code.4 = row.names(cm$byClass)[4],
                            Sensitivity.4 = cm$byClass[4,1],
                            Specificity.4 = cm$byClass[4,2],
                            Code.5 = row.names(cm$byClass)[5],
                            Sensitivity.5 = cm$byClass[5,1],
                            Specificity.5 = cm$byClass[5,2],
                            Code.6 = row.names(cm$byClass)[6],
                            Sensitivity.6 = cm$byClass[6,1],
                            Specificity.6 = cm$byClass[6,2],
                            Code.7 = row.names(cm$byClass)[7],
                            Sensitivity.7 = cm$byClass[7,1],
                            Specificity.7 = cm$byClass[7,2],
                            Code.8 = row.names(cm$byClass)[8],
                            Sensitivity.8 = cm$byClass[8,1],
                            Specificity.8 = cm$byClass[8,2],
                            Code.9 = row.names(cm$byClass)[9],
                            Sensitivity.9 = cm$byClass[9,1],
                            Specificity.9 = cm$byClass[9,2],
                            Code.10 = row.names(cm$byClass)[10],
                            Sensitivity.10 = cm$byClass[10,1],
                            Specificity.10 = cm$byClass[10,2],
                            Code.11 = row.names(cm$byClass)[11],
                            Sensitivity.11 = cm$byClass[11,1],
                            Specificity.11 = cm$byClass[11,2],
                            Code.12 = row.names(cm$byClass)[12],
                            Sensitivity.12 = cm$byClass[12,1],
                            Specificity.12 = cm$byClass[12,2],
                            Code.13 = row.names(cm$byClass)[13],
                            Sensitivity.13 = cm$byClass[13,1],
                            Specificity.13 = cm$byClass[13,2],
                            row.names = NULL))
Pred2_results %>% t() %>% knitr::kable(caption = "Prediction All Arrhytmia Classification Summary")


## ----Support Vector Machines Model First Prediction pre-train, include=FALSE-----------------------------------------------------------------------
train_svm <- svm(`Class code` ~ ., data = train_set_p1) # initial approach
train_svm


## ----Support Vector Machines Model First Prediction train, echo=FALSE, warning=FALSE, size='scriptsize'--------------------------------------------
#Support Vector Classifier:
set.seed(1)
fit_svm <- train(`Class code` ~ ., data = train_set_p1, method="svmRadial")  # caret train method 
fit_svm

y_hat_svm <- predict(fit_svm,validation_set_p1)

options(digits =2)
cm <- confusionMatrix(data = y_hat_svm, reference = validation_set_p1$`Class code`)
cm
options(digits =7) # default



## ----Support Vector Machines Model First Prediction Variable Importance, echo=FALSE, warning=FALSE, size='scriptsize'------------------------------
options(digits =2)
varImp(fit_svm)
varimp_svm_p1 <- varImp(fit_svm)$importance
options(digits =7) # default


## ----Support Vector Machines Model first Prediction Table, echo=FALSE, size='scriptsize'-----------------------------------------------------------
# Table names
Predicted <- y_hat_svm
Actually <- validation_set_p1$`Class code`
#Computes the crosstable calculations
CrossTable(Predicted, Actually, digits = 3, prop.chisq = FALSE, prop.t=FALSE)


## ----Support Vector Machines Model First Prediction Result, echo=FALSE, warning=FALSE--------------------------------------------------------------

Pred1_results <- bind_rows(Pred1_results,
                           data.frame(Model = "SVM Classifier",
                                     Accuracy = cm$overall["Accuracy"],
                                     Sensitivity = cm$byClass["Sensitivity"],
                                     Specificity = cm$byClass["Specificity"], row.names = NULL ))
Pred1_results %>% knitr::kable(caption = "Prediction Normal/Arrhytmia Summary") %>%
  kable_styling(full_width = F)


## ----Support Vector Machines Model Second Prediction train, echo=FALSE, warning=FALSE, size='scriptsize'-------------------------------------------
#Support Vector Classifier

ctrl_svm <- trainControl(method = "cv", savePred=T)
set.seed(1)
#fit_svm <- svm(`Class code` ~ ., data = train_set) # See note below in .Rmd file
fit_svm <- train(`Class code` ~ ., data = train_set, method="svmRadial", trControl = ctrl_svm)  # caret train method 
fit_svm

y_hat_svm <- predict(fit_svm,validation_set)

options(digits =2)
cm <- confusionMatrix(data = y_hat_svm, reference = validation_set$`Class code`)

cm
options(digits =7) # default



## ----Support Vector Machines Model Second Prediction Variable Importance, echo=FALSE, warning=FALSE, size='scriptsize'-----------------------------
options(digits =2)
varImp(fit_svm)
varimp_svm_p2 <- varImp(fit_svm)$importance
options(digits =7) # default


## ----Support Vector Machines Model Second Prediction Table, echo=FALSE, size='scriptsize'----------------------------------------------------------
# Table names
Predicted <- y_hat_svm
Actually <- validation_set_p1$`Class code`
#Computes the crosstable calculations
CrossTable(Predicted, Actually, digits = 3, prop.chisq = FALSE, prop.t=FALSE)


## ----Support Vector Machines Model Second Prediction Result, echo=FALSE, warning=FALSE-------------------------------------------------------------
Pred2_results <- bind_rows(Pred2_results,
                           data.frame(Model = "SVM Classifier",
                            Accuracy = cm$overall["Accuracy"],
                            Code.1 = row.names(cm$byClass)[1],
                            Sensitivity.1 = cm$byClass[1,1],
                            Specificity.1 = cm$byClass[1,2],
                            Code.2 = row.names(cm$byClass)[2],
                            Sensitivity.2 = cm$byClass[2,1],
                            Specificity.2 = cm$byClass[2,2],
                            Code.3 = row.names(cm$byClass)[3],
                            Sensitivity.3 = cm$byClass[3,1],
                            Specificity.3 = cm$byClass[3,2],
                            Code.4 = row.names(cm$byClass)[4],
                            Sensitivity.4 = cm$byClass[4,1],
                            Specificity.4 = cm$byClass[4,2],
                            Code.5 = row.names(cm$byClass)[5],
                            Sensitivity.5 = cm$byClass[5,1],
                            Specificity.5 = cm$byClass[5,2],
                            Code.6 = row.names(cm$byClass)[6],
                            Sensitivity.6 = cm$byClass[6,1],
                            Specificity.6 = cm$byClass[6,2],
                            Code.7 = row.names(cm$byClass)[7],
                            Sensitivity.7 = cm$byClass[7,1],
                            Specificity.7 = cm$byClass[7,2],
                            Code.8 = row.names(cm$byClass)[8],
                            Sensitivity.8 = cm$byClass[8,1],
                            Specificity.8 = cm$byClass[8,2],
                            Code.9 = row.names(cm$byClass)[9],
                            Sensitivity.9 = cm$byClass[9,1],
                            Specificity.9 = cm$byClass[9,2],
                            Code.10 = row.names(cm$byClass)[10],
                            Sensitivity.10 = cm$byClass[10,1],
                            Specificity.10 = cm$byClass[10,2],
                            Code.11 = row.names(cm$byClass)[11],
                            Sensitivity.11 = cm$byClass[11,1],
                            Specificity.11 = cm$byClass[11,2],
                            Code.12 = row.names(cm$byClass)[12],
                            Sensitivity.12 = cm$byClass[12,1],
                            Specificity.12 = cm$byClass[12,2],
                            Code.13 = row.names(cm$byClass)[13],
                            Sensitivity.13 = cm$byClass[13,1],
                            Specificity.13 = cm$byClass[13,2],
                            row.names = NULL))
Pred2_results %>% t() %>% knitr::kable(caption = "Prediction All Arrhytmia Classification Summary")


## ----Results No data in our dataset about these arrhythmias, echo=FALSE----------------------------------------------------------------------------
results <- data.frame(`Class code` = 11,
                            Prediction = "No data", check.names = FALSE)
results <- bind_rows(results,
                     data.frame(`Class code` = 12,
                            Prediction = "No data", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 13,
                            Prediction = "No data", check.names = FALSE))


## ----Results First Prediction Result, echo=FALSE, warning=FALSE------------------------------------------------------------------------------------

Pred1_results[-1,] %>% data.frame(row.names = NULL) %>% knitr::kable(caption = "Prediction Normal/Arrhytmia Summary") %>%
  kable_styling(full_width = F, position = "left")


## ----Results random forest varimp in first prediction, echo=FALSE, size='scriptsize'---------------------------------------------------------------
slice_max(varimp_rf_p1, order_by = Overall, n=20)


## ----Results means of varimp in first prediction, echo=FALSE, size='scriptsize'--------------------------------------------------------------------
varimp_knn_p1_mean <- rowMeans(varimp_knn_p1)
varimp_p1_mean <- rowMeans(cbind(varimp_knn_p1_mean, varimp_svm_p1[,1], varimp_rf_p1))
varimp_p1_mean <- data.frame(varimp_p1_mean)
slice_max(varimp_p1_mean, order_by =  varimp_p1_mean, n=20)


## ----Results of varimp of Heart rate, echo=FALSE---------------------------------------------------------------------------------------------------
varimp_knn_p1_mean %>% data.frame()  %>% rownames_to_column() %>% filter(rowname == "Heart rate")
varimp_svm_p1 %>% data.frame()  %>% rownames_to_column() %>% filter(rowname == "Heart rate")


## ----Results of varimp of Channel V1 QRSA, echo=FALSE----------------------------------------------------------------------------------------------
varimp_knn_p1_mean %>% data.frame()  %>% rownames_to_column() %>% filter(rowname == "Channel V1 QRSA")
varimp_svm_p1 %>% data.frame()  %>% rownames_to_column() %>% filter(rowname == "Channel V1 QRSA")


## ----Correlation of Channel V1 QRSA, echo=FALSE, warning=FALSE-------------------------------------------------------------------------------------
predictor <- "Channel V1 QRSA"
cor_arrhythmia_numeric %>%  filter( correlated.1 %in% predictor | correlated.2 %in% predictor) %>% slice_max(order_by = value, n = 5) %>% knitr::kable(caption = "Correlation for Channel V1 QRSA") %>%
  kable_styling(full_width = F, position = "left")
cor_arrhythmia_numeric %>%  filter( correlated.1 %in% predictor | correlated.2 %in% predictor) %>% slice_min(order_by = value, n = 5) %>% knitr::kable(caption = "Correlation for Channel V1 QRSA") %>%
  kable_styling(full_width = F, position = "left")


## ----Results Second Prediction Result, echo=FALSE, warning=FALSE-----------------------------------------------------------------------------------
Pred2_results[-1,] %>% data.frame(row.names = NULL) %>% t() %>% knitr::kable(caption = "Prediction All Arrhytmia Classification Summary")


## ----Results Class code table, echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------------------------------
left_join(arrhythmia, Class, by = "Class code") %>% 
  group_by(`Class code`, `Class name`) %>% summarise( "N Ocurrences" = n(), Percentage = round(100*n()/nrow(arrhythmia),digits = 1)) %>%
   filter(`Class code` %in% c(5, 7, 8, 14, 15, 16)) %>%
  knitr::kable(caption = "Presence of codes in our dataset") %>%
  kable_styling(full_width = F, position = "left")


## ----Results No Predictable in our dataset about these arrhythmias, echo=FALSE---------------------------------------------------------------------
results <- bind_rows(results,
                     data.frame(`Class code` = 5,
                            Prediction = "No Predictable", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 7,
                            Prediction = "No Predictable", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 8,
                            Prediction = "No Predictable", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 14,
                            Prediction = "No Predictable", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 15,
                            Prediction = "No Predictable", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 16,
                            Prediction = "No Predictable", check.names = FALSE))


## ----Results Predictable in our dataset about these arrhythmias, echo=FALSE------------------------------------------------------------------------
results <- bind_rows(results,
                     data.frame(`Class code` = 9,
                            Prediction = "Predictable", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 3,
                            Prediction = "Predictable", check.names = FALSE))


## ----Results Analyzed 5 coded in our dataset about these arrhythmias, echo=FALSE, warning=FALSE----------------------------------------------------
Pred2_results %>% select(Accuracy, Code.1, Sensitivity.1, Specificity.1,
                         Code.2, Sensitivity.2, Specificity.2,
                         #Code.3, Sensitivity.3, Specificity.3,
                         Code.4, Sensitivity.4, Specificity.4,
                         #Code.5, Sensitivity.5, Specificity.5,
                         Code.6, Sensitivity.6, Specificity.6,
                         Code.10, Sensitivity.10, Specificity.10)  %>% 
  t() %>% knitr::kable(caption = "Prediction in remaining Arrhytmia Classification") %>%
  kable_styling(full_width = F, position = "left")


## ----Results Predictable in our dataset about these arrhythmias Part 2, echo=FALSE-----------------------------------------------------------------
results <- bind_rows(results,
                     data.frame(`Class code` = 1,
                            Prediction = "Predictable", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 2,
                            Prediction = "Predictable", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 4,
                            Prediction = "Predictable", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 6,
                            Prediction = "Predictable", check.names = FALSE))
results <- bind_rows(results,
                     data.frame(`Class code` = 10,
                            Prediction = "Predictable", check.names = FALSE))


## ----Results by class codes in our validation dataset, echo=FALSE, message=FALSE, warning=FALSE----------------------------------------------------
tmp <- left_join(arrhythmia, Class, by = "Class code") %>% 
  group_by(`Class code`, `Class name`) %>% summarise( "N Ocurrences" = n(), Percentage = round(100*n()/nrow(arrhythmia),digits = 1))
results <- results %>% mutate(`Class code` = as.factor(`Class code`))
left_join(tmp,results, by = "Class code") %>% 
  knitr::kable(caption = "Result by class codes in our validation dataset") %>%
  kable_styling(full_width = F, position = "left")


## ----Results random forest varimp in second prediction, echo=FALSE, size='scriptsize'--------------------------------------------------------------
slice_max(varimp_rf_p2, order_by = Overall, n=20)


## ----Results means of varimp in second prediction, echo=FALSE, size='scriptsize'-------------------------------------------------------------------
varimp_knn_p2_mean <- rowMeans(varimp_knn_p2)
varimp_p2_mean <- rowMeans(cbind(varimp_knn_p2_mean, varimp_svm_p2[,1], varimp_rf_p2))
varimp_p2_mean <- data.frame(varimp_p2_mean)
slice_max(varimp_p2_mean, order_by =  varimp_p2_mean, n=20)


## ----Results Heart rate Predictor, echo=FALSE------------------------------------------------------------------------------------------------------
varimp_knn_p2_mean %>% data.frame()  %>% rownames_to_column() %>% filter(rowname == "Heart rate")
varimp_svm_p2 %>% data.frame()  %>% rownames_to_column() %>% filter(rowname == "Heart rate")


## ----Results Channel V1 R prima wave Amplitude, echo=FALSE-----------------------------------------------------------------------------------------
varimp_knn_p2_mean %>% data.frame()  %>% rownames_to_column() %>% filter(rowname == "Channel V1 R' wave Amplitude")
varimp_svm_p2 %>% data.frame()  %>% rownames_to_column() %>% filter(rowname == "Channel V1 R' wave Amplitude")


## ----Conclusion Prediction Normal/Arrhytmia Summary, echo=FALSE, warning=FALSE---------------------------------------------------------------------
Pred1_results[4,] %>% data.frame(row.names = NULL) %>% knitr::kable(caption = "Prediction Normal/Arrhytmia Summary") %>%
  kable_styling(full_width = F, position = "left")


## ----Conclusion Prediction All Arrhytmia Classification Summary, echo=FALSE, warning=FALSE---------------------------------------------------------
Pred2_results[4,] %>% data.frame(row.names = NULL) %>% t() %>%
  knitr::kable(caption = "Prediction All Arrhytmia Classification Summary") %>%
  kable_styling(full_width = F, position = "left")


## ----Conclusion Prediction by Classes,echo=FALSE---------------------------------------------------------------------------------------------------
cols <- c("red", "yellow", "green")
pie_data_4graph <-results %>% group_by(Prediction) %>% summarise( n=n())
pie(pie_data_4graph$n, labels = pie_data_4graph$Prediction, col = cols,  main = "Prediction by Classes")

