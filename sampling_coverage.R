#### Coverage certification MT2 IPD samples
#### Script to compare the number of trips sampled by IPD, samples saved in
#### SIRENO and samples required in the prescriptions.
#### 
#### The script return a formatted table in xls.
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
####
#### files required: prescripciones_2018.csv, file with the IPD trips, file with
#### the samples saved in SIRENO

# INSTRUCTIONS -----------------------------------------------------------------
# To use this scritp:
# - Change variables in "YOU HAVE ONLY TO CHANGE THIS VARIABLES" section of this
# script.
# - Make sure that this required files are in PATH_FILES path:
#     * .xls file with the IPD trips send by IPD. If the format of the file
# change, this script probably should be fixed.
#     * prescripciones_2018_anual.csv (it is a csv separated by ";" without quotes)
#     * report files tallas_x_up from SIRENO
# - Run all the script
# - A formatted xlsx file is obtained in PATH_DATA_FILES path


# YOU HAVE ONLY TO CHANGE THIS VARIABLES ---------------------------------------
# All the files must be located in this PATH_FILES (the path is relative):
PATH_DATA_FILES <- "/data/2019/2019_05/"

# FILES FROM SIRENO
FILENAME_SIRENO_DES_TOT <- "IEOUPMUEDESTOTMARCO.TXT"
FILENAME_SIRENO_DES_TAL <- "IEOUPMUEDESTALMARCO.TXT"
FILENAME_SIRENO_TAL <- "IEOUPMUETALMARCO.TXT"

# FILE FROM IPD
FILENAME_IPD <- "inf_cobertura_mayo.xls"

# FILE WITH ANNUAL PRESCRIPTIONS
FILENAME_PRESCRIPTIONS <- "prescripciones_2019_anual.csv"

MONTH <- 5 #a month in number

YEAR <- "2019"

# PACKAGES ---------------------------------------------------------------------
library(dplyr) #arrange_()
library(tools) #file_path_sans_ext()
library(devtools)
#install.packages("openxlsx", dependencies=TRUE)  
library(openxlsx)
library(readxl)

# ---- install sapmuebase from local
#install("F:/misdoc/sap/sapmuebase")

# ---- install sapmuebase from github
#install_github("Eucrow/sapmuebase") # Make sure this is the last version

library(sapmuebase) # and load the library


# FUNCTIONS --------------------------------------------------------------------
source('sampling_coverage_functions.R')


# CONSTANTS AND GLOBAL VARIABLES -----------------------------------------------
BASE_FIELDS <- c("FECHA_MUE", "PUERTO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE", "MES")  ###list with the common fields used in tables
month_as_character <- sprintf("%02d", MONTH)


# IMPORT DATA ------------------------------------------------------------------
# import tallas_x_up and isolate catches dataframe
catches <- importRIMCatches(FILENAME_SIRENO_DES_TOT, path = paste0(getwd(),PATH_DATA_FILES))

# import IPD's trip data
ipd_trips <- get_ipd_trips()
  
# import prescriptions file
prescriptions <- importCsvSAPMUE(paste0(getwd(),PATH_DATA_FILES, FILENAME_PRESCRIPTIONS))
  

# CLEAN AND PREPARE DATA -------------------------------------------------------
# Clean and prepare IPD's trip dataframe
ipd_trips <- subset(ipd_trips, NUM_MAREAS > 0)
names(ipd_trips)[names(ipd_trips)== "NUM_MAREAS"] <- "NUM_MAREAS_IPD"
ipd_trips$PUERTO <- toupper(ipd_trips$PUERTO)
# Convert to factors:
ipd_trips$PUERTO <- as.factor(ipd_trips$PUERTO)
ipd_trips$ESTRATO_RIM <- as.factor(ipd_trips$ESTRATO_RIM)
  
# Sum JURELERA_CN with BACA_CN
  # change levels
  levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="BACA_CN"] <- "BACA_CN / JURELERA_CN"
  levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="JURELERA_CN"] <- "BACA_CN / JURELERA_CN"
  
  #sum
  by <- list(ipd_trips$MES, ipd_trips$PUERTO, ipd_trips$ESTRATO_RIM)
  ipd_trips <- aggregate(x = ipd_trips$NUM_MAREAS_IPD, by = by, FUN = sum)
  colnames(ipd_trips) <- c("MES", "PUERTO", "ESTRATO_RIM", "NUM_MAREAS_IPD")
  
# Sum Avilés with Gijón
  # change names
  levels(ipd_trips$PUERTO)[levels(ipd_trips$PUERTO)=="AVILÉS"] <- "AVILÉS / GIJÓN"
  levels(ipd_trips$PUERTO)[levels(ipd_trips$PUERTO)=="GIJÓN"] <- "AVILÉS / GIJÓN"

  #sum
  by <- list(ipd_trips$MES, ipd_trips$PUERTO, ipd_trips$ESTRATO_RIM)
  ipd_trips <- aggregate(x = ipd_trips$NUM_MAREAS_IPD, by = by, FUN = sum)
  colnames(ipd_trips) <- c("MES", "PUERTO", "ESTRATO_RIM", "NUM_MAREAS_IPD")

  
# Clean and prepare sireno trip dataframe
catches_to_clean <- catches
catches_to_clean$MES <- as.POSIXlt(catches_to_clean$FECHA_MUE, format="%d-%m-%Y")$mon
catches_to_clean$MES <- as.integer(catches_to_clean$MES)+1
catches_to_clean <- catches_to_clean[catches_to_clean$MES==MONTH,]
catches_to_clean <- catches_to_clean[, c(BASE_FIELDS)]
catches_to_clean <- unique(catches_to_clean)
catches_to_clean$PUERTO <- toupper(catches_to_clean$PUERTO)
catches_clean <- catches_to_clean[, c("PUERTO", "ESTRATO_RIM", "COD_TIPO_MUE", "MES")]

  # select only mt2 samples
  catches_clean <- catches_clean[catches_clean["COD_TIPO_MUE"]==2,]
  catches_clean <- catches_clean[,!(names(catches_clean))==c("COD_TIPO_MUE")]
  
  # obtain number of trips in sireno dataframe
  by <- list(catches_clean$PUERTO, catches_clean$ESTRATO_RIM, catches_clean$MES)
  aggregated_catches <- aggregate(x=catches_clean$PUERTO, by=by, FUN=length)
  colnames(aggregated_catches) <- c("PUERTO", "ESTRATO_RIM", "MES", "NUM_MAREAS_SIRENO")
  sireno_trips <- arrange(aggregated_catches, MES, PUERTO, ESTRATO_RIM)
  
  # Sum JURELERA_CN with BACA_CN
    # change levels
    levels(sireno_trips$ESTRATO_RIM)[levels(sireno_trips$ESTRATO_RIM)=="BACA_CN"] <- "BACA_CN / JURELERA_CN"
    levels(sireno_trips$ESTRATO_RIM)[levels(sireno_trips$ESTRATO_RIM)=="JURELERA_CN"] <- "BACA_CN / JURELERA_CN"
    
    #sum
    by <- list(sireno_trips$MES, sireno_trips$PUERTO, sireno_trips$ESTRATO_RIM)
    sireno_trips <- aggregate(x = sireno_trips$NUM_MAREAS_SIRENO, by = by, FUN = sum)
    colnames(sireno_trips) <- c("MES", "PUERTO", "ESTRATO_RIM", "NUM_MAREAS_SIRENO")
  
  # Sum Avilés with Gijón
    # change names
    sireno_trips[sireno_trips$PUERTO=="AVILÉS","PUERTO"] <-  "AVILÉS / GIJÓN"
    sireno_trips[sireno_trips$PUERTO=="GIJÓN","PUERTO"] <-  "AVILÉS / GIJÓN"
    
    #sum
    by <- list(sireno_trips$MES, sireno_trips$PUERTO, sireno_trips$ESTRATO_RIM)
    sireno_trips <- aggregate(x = sireno_trips$NUM_MAREAS_SIRENO, by = by, FUN = sum)
    colnames(sireno_trips) <- c("MES", "PUERTO", "ESTRATO_RIM", "NUM_MAREAS_SIRENO")  
    
  # Cambio de BACA_AP A BACA_APN --> ESTO NO LO TENDR?AMOS QUE HACER EN EL FUTURO  
    levels(sireno_trips$ESTRATO_RIM)[levels(sireno_trips$ESTRATO_RIM)=="BACA_AP"] <- "BACA_APN"

# Clean and prepare prescriptions dataframe
prescriptions_trips <- prescriptions[, c("PUERTO", "Pesquería", "Nº.mareas")]
colnames(prescriptions_trips) <- c("PUERTO", "PESQUERIA", "NUM_MAREAS")
prescriptions_trips$PUERTO <- toupper(prescriptions_trips$PUERTO)

  # obtain number of trips in prescriptions dataframe by month
    prescriptions_trips$NUM_MAREAS <- round(prescriptions_trips$NUM_MAREAS/12, 1)
    #change name columns
    colnames(prescriptions_trips) <- c("PUERTO", "ESTRATO_RIM", "NUM_MAREAS_PRESCRIPCIONES_by_month")

  # add months    
    # HAY QUE HABILITAR LA OPCI?N DE ESCOGER MES
    # months_to_select <- ""
    # if(MONTH == "all" || MONTH == ""){
    #   prescriptions_trips$frec_months <- 12
    #   prescriptions_trips <- prescriptions_trips[rep(row.names(prescriptions_trips), prescriptions_trips$frec_months),]
    #   prescriptions_trips$MES <- 1:12
    #   prescriptions_trips<-prescriptions_trips[,!(names(prescriptions_trips))==c("frec_months")] #remove column
    # } else if (is.numeric(MONTH) && MONTH <= 12) {
    #   prescriptions_trips$MES <- MONTH
    # }
  
  #LAS LINEAS SIGUIENTES SON PARA EL CASO DE QUE SEA UN TRIMESTRE:
  ##ESTO ES UNA CHAPUZA DE MUCHO CUIDADO:
  #prescriptions_trips$frec_months <- 3#ES IMPORTANTE ATUOMATIZAR ESTO
  #prescriptions_trips <- prescriptions_trips[rep(row.names(prescriptions_trips), prescriptions_trips$frec_months),]
  #prescriptions_trips$MES <- 1:3#Y ESTO
  #prescriptions_trips<-prescriptions_trips[,!(names(prescriptions_trips))==c("frec_months")]
    
  # change name column
  names(prescriptions_trips)[names(prescriptions_trips)=="NUM_MAREAS_PRESCRIPCIONES_by_month"] <- "NUM_MAREAS_PRES"
  # ESTO TAMBIÉN HABRÍA QUE CAMBIARLO:
  # add column with the month
  prescriptions_trips$MES <- MONTH
  
  #change port's name
  prescriptions_trips[prescriptions_trips$PUERTO=="S. VICENTE","PUERTO"] <-  "SAN VICENTE DE LA BARQUERA"
  prescriptions_trips[prescriptions_trips$PUERTO=="FISTERRA","PUERTO"] <-  "FINISTERRE"
  prescriptions_trips[prescriptions_trips$PUERTO=="RIBEIRA","PUERTO"] <-  "SANTA EUGENIA DE RIBEIRA"
  prescriptions_trips[prescriptions_trips$PUERTO=="CELEIRO","PUERTO"] <-  "CILLERO"
  
  #change levels estrato_rim
  levels(prescriptions_trips$ESTRATO_RIM)[levels(prescriptions_trips$ESTRATO_RIM)=="RAPANTEROS_AC"] <- "RAPANTER_AC"
  levels(prescriptions_trips$ESTRATO_RIM)[levels(prescriptions_trips$ESTRATO_RIM)=="MERLUCEROS_AC"] <- "MERLUCER_AC"
  levels(prescriptions_trips$ESTRATO_RIM)[levels(prescriptions_trips$ESTRATO_RIM)=="NASAPULPO_CN"] <- "NASAPULP_CN"
  levels(prescriptions_trips$ESTRATO_RIM)[levels(prescriptions_trips$ESTRATO_RIM)=="LINEA_CABALLA"] <- "LIN_CABALLA"

# COMPARE NUM_MAREAS -----------------------------------------------------------
# Compare SIRENO vs IPD vs Prescriptions
    
  # TODO: usar Reduce!!!!!!!!
  sireno_ipd <- merge(x = sireno_trips, y = ipd_trips, by = c("PUERTO", "ESTRATO_RIM", "MES"), all.x = TRUE, all.y = TRUE)
  sireno_ipd <- arrange(sireno_ipd, MES, PUERTO, ESTRATO_RIM)
  
  sireno_ipd_presc <- merge(x = sireno_ipd, y = prescriptions_trips, by = c("PUERTO", "ESTRATO_RIM", "MES"), all.x = TRUE, all.y = TRUE)
  sireno_ipd_presc <- arrange(sireno_ipd_presc, MES, PUERTO, ESTRATO_RIM)
  
  sireno_ipd_presc[is.na(sireno_ipd_presc)] <- 0

  
# Export to csv
  # filename <- paste("cobertura_muestreos_IPD_", MONTH, ".csv", sep="")
  # write.csv(sireno_ipd_presc, file = filename, quote = FALSE, row.names = FALSE, na="0")

# Export to excel
  exportCoverageToExcel(sireno_ipd_presc)
