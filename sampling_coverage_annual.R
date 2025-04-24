# Coverage certification MT2 IPD samples
# Script to compare the number of samples saved in SIRENO and required in the
# prescriptions.
# 
# The script create an xls file as result.
#
# Author: Marco A. Ámez Fernandez
# Email: ieo.marco.a.amez@gmail.com
# 
# Files required: prescripciones_2021_anual_ansi.csv and file with the samples saved
# in SIRENO from 


# INSTRUCTIONS -----------------------------------------------------------------
# To use this scritp:
# - Change variables in "YOU HAVE ONLY TO CHANGE THIS VARIABLES" section of this
# script.
# - Make sure that this required files are in PATH_FILENAME path:
#     * prescripciones_2018.csv (with this format: csv separated by ";", without quotes)
#     * report files tallas_x_up from SIRENO
# - Run all the script
# - A formatted xlsx file is obtained in PATH_FILENAME path


# YOU HAVE ONLY TO CHANGE THIS VARIABLES ---------------------------------------
# All the files must be located in this PATH_FILENAME:
PATH_DATA_FILES <- "/data/2023/2023_annual/"

# FILES FROM SIRENO
FILENAME_SIRENO_DES_TOT <- "IEOUPMUEDESTOTSIRENO_RIM_ICES_2023.TXT"

# FILE WITH ANNUAL PRESCRIPTIONS
FILENAME_PRESCRIPTIONS <- "prescripciones_2021_anual_ansi.csv"

MONTH <- all #empty, a month in number, or "all"

SUFFIX_FILENAME <- "annual_01_11"

YEAR <- "2023"


# PACKAGES ---------------------------------------------------------------------
library(dplyr) #arrange()
library(devtools)
library(openxlsx)

# install sapmuebase from github
# remove.packages("sapmuebase")
# .rs.restartR()
#install_github("Eucrow/sapmuebase") # Make sure this is the last version

library(sapmuebase) # and load the library


# CONSTANTS AND GLOBAL VARIABLES -----------------------------------------------
BASE_FIELDS <- c("FECHA_MUE", "PUERTO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE", "MES")


# FUNCTIONS --------------------------------------------------------------------
source('sampling_coverage_functions.R')


# IMPORT DATA ------------------------------------------------------------------
# import catches from SIRENO files
  catches <- importRIMCatches(FILENAME_SIRENO_DES_TOT, path = paste0(getwd(),PATH_DATA_FILES))

# import prescriptions file
  prescriptions <- importCsvSAPMUE(paste0(getwd(),PATH_DATA_FILES, FILENAME_PRESCRIPTIONS))


# CLEAN AND PREPARE DATA -------------------------------------------------------
  catches_to_clean <- catches
  catches_to_clean <- catches_to_clean[, c(BASE_FIELDS)]
  catches_to_clean <- unique(catches_to_clean)
  catches_to_clean$PUERTO <- toupper(catches_to_clean$PUERTO)
  catches_clean <- catches_to_clean[, c("FECHA_MUE", "PUERTO", "ESTRATO_RIM", "COD_TIPO_MUE")]
  
  # select only mt2 samples
  catches_clean <- catches_clean[catches_clean["COD_TIPO_MUE"]==2,]
  catches_clean <- catches_clean[,!(names(catches_clean))==c("COD_TIPO_MUE")]
  
  # Sum JURELERA_CN with BACA_CN
  # change levels
  levels(catches_clean$ESTRATO_RIM)[levels(catches_clean$ESTRATO_RIM)=="BACA_CN"] <- "BACA_CN / JURELERA_CN"
  levels(catches_clean$ESTRATO_RIM)[levels(catches_clean$ESTRATO_RIM)=="JURELERA_CN"] <- "BACA_CN / JURELERA_CN"
  # Sum BETA_CN with TRASMALL_CN
  # change levels
  levels(catches_clean$ESTRATO_RIM)[levels(catches_clean$ESTRATO_RIM)=="BETA_CN"] <- "BETA_CN / TRASMALL_CN"
  levels(catches_clean$ESTRATO_RIM)[levels(catches_clean$ESTRATO_RIM)=="TRASMALL_CN"] <- "BETA_CN / TRASMALL_CN"
  # Sum ENMALLE_GC with TRASMALL_GC
  # change levels
  levels(catches_clean$ESTRATO_RIM)[levels(catches_clean$ESTRATO_RIM)=="ENMALLE_GC"] <- "ENMALLE_GC / TRASMALL_GC"
  levels(catches_clean$ESTRATO_RIM)[levels(catches_clean$ESTRATO_RIM)=="TRASMALL_GC"] <- "ENMALLE_GC / TRASMALL_GC"
  # Sum Avilés with Gijón, except PALANGRE_CN and PALANGRE_AC
  # change names
  levels(catches_clean$PUERTO) <- c(levels(catches_clean$PUERTO), "AVILÉS / GIJÓN")
  catches_clean[catches_clean$PUERTO=="AVILÉS" & !(catches_clean$ESTRATO_RIM %in% c("PALANGRE_CN", "PALANGRE_AC")), "PUERTO"] <- "AVILÉS / GIJÓN"
  catches_clean[catches_clean$PUERTO=="GIJÓN" & catches_clean$ESTRATO_RIM != "BACA_AC", "PUERTO"] <- "AVILÉS / GIJÓN"
  
  # obtain number of trips in sireno dataframe
  by <- list(catches_clean$PUERTO, catches_clean$ESTRATO_RIM)
  sireno_trips <- aggregate(x=catches_clean$PUERTO, by=by, FUN=length)
  colnames(sireno_trips) <- c("PUERTO", "ESTRATO_RIM", "NUM_MAREAS_SIRENO")
  sireno_trips <- arrange(sireno_trips,PUERTO, ESTRATO_RIM)
  
  # obtain number of port/day in sireno dataframe
  catches_port_day <- unique(catches_clean)
  by <- list(catches_port_day$PUERTO, catches_port_day$ESTRATO_RIM)
  sireno_port_days <- aggregate(x=catches_port_day$FECHA_MUE, by=by, FUN=length)
  colnames(sireno_port_days) <- c("PUERTO", "ESTRATO_RIM", "NUM_PUERTO_DIA_SIRENO")
  
  # merge number of trips with port/day
  sireno_trips_port_days <- merge(x=sireno_trips, y=sireno_port_days,
                                  by=c("PUERTO", "ESTRATO_RIM"),
                                  all.x = TRUE,
                                  all.y = TRUE)

  # I think this is not neccesary:
  # sireno_trips_port_days <- sireno_trips_port_days %>%
  #   group_by(PUERTO, ESTRATO_RIM) %>%
  #   mutate(across(,sum))


  # Clean and prepare prescriptions dataframe
  prescriptions_trips <- prescriptions[, c("PUERTO", "PESQUERIA", "MUESTREOS.ANUALES")]
  colnames(prescriptions_trips) <- c("PUERTO", "PESQUERIA", "NUM_PUERTO_DIA")
  prescriptions_trips$PUERTO <- toupper(prescriptions_trips$PUERTO)
  
  #change name columns
  colnames(prescriptions_trips) <- c("PUERTO", "ESTRATO_RIM", "NUM_PUERTO_DIA_PRESCR_by_month")

  # change name column
  names(prescriptions_trips)[names(prescriptions_trips)=="NUM_PUERTO_DIA_PRESCR_by_month"] <- "NUM_PUERTO_DIA_PRESCR"
  
  #change port's name
  prescriptions_trips[prescriptions_trips$PUERTO=="S. VICENTE","PUERTO"] <-  "SAN VICENTE DE LA BARQUERA"
  prescriptions_trips[prescriptions_trips$PUERTO=="FISTERRA","PUERTO"] <-  "FINISTERRE"
  prescriptions_trips[prescriptions_trips$PUERTO=="RIBEIRA","PUERTO"] <-  "SANTA EUGENIA DE RIBEIRA"
  prescriptions_trips[prescriptions_trips$PUERTO=="CELEIRO","PUERTO"] <-  "CILLERO"
  
  #change estrato_rim
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="RAPANTEROS_AC", "ESTRATO_RIM"] <- "RAPANTER_AC"
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="MERLUCEROS_AC", "ESTRATO_RIM"] <- "MERLUCER_AC"
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="NASAPULPO_CN", "ESTRATO_RIM"] <- "NASAPULP_CN"
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="LINEA_CABALLA", "ESTRATO_RIM"] <- "LIN_CABALLA"
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="BETA_CN / TRASMALLO_CN", "ESTRATO_RIM"] <- "BETA_CN / TRASMALL_CN"
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="ENMALLE_GC / TRASMALLO_GC", "ESTRATO_RIM"] <- "ENMALLE_GC / TRASMALL_GC"


# COMPARE NUM_MAREAS -----------------------------------------------------------
# Compare SIRENO vs IPD vs Prescriptions
  
  # sireno_ipd_presc <- merge(x = sireno_trips, y = prescriptions_trips, by.x = c("PUERTO", "ESTRATO_RIM"), all.x = TRUE, all.y = TRUE)
  sireno_ipd_presc <- merge(x = sireno_trips_port_days, y = prescriptions_trips, by.x = c("PUERTO", "ESTRATO_RIM"), all.x = TRUE, all.y = TRUE)
  sireno_ipd_presc <- arrange(sireno_ipd_presc, PUERTO, ESTRATO_RIM)
  
  sireno_ipd_presc[is.na(sireno_ipd_presc)] <- 0

#export to csv
  # filename <- paste("cobertura_muestreos_IPD_", MONTH, ".csv", sep="")
  # write.csv(sireno_ipd_presc, file = filename, quote = FALSE, row.names = FALSE, na="0")

#TODO: the file is saved in c:\...\sampling_coverage\ folder, change it to save
  # in its property folder
#export to excel
  exportAnnualCoverageToExcel(sireno_ipd_presc)

  