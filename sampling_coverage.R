# Coverage certification MT2 IPD samples
# Script to compare the number of trips sampled by IPD, samples saved in
# SIRENO and samples required in the prescriptions.
#
# The script create an xls file as result.
#
# Author: Marco A. Amez Fernandez
# Email: ieo.marco.a.amez@gmail.com
#
# files required: prescripciones_2021_anual.csv, file with the IPD trips, file with
# the samples saved in SIRENO

# INSTRUCTIONS ----
# To use this scritp:
# - Change variables in "YOU HAVE ONLY TO CHANGE THIS VARIABLES" section of this
# script.
# - Make sure that this required files are in PATH_FILES path:
#     * .xls file with the IPD trips send by IPD. If the format of the file
# change, this script probably should be fixed.
#     * prescripciones_2021_anual.csv (it is a csv separated by ";" without quotes)
#     * report files tallas_x_up from SIRENO
# - Run all the script
# - A formatted xlsx file is obtained in PATH_DATA_FILES path

Sys.getlocale()
Sys.setlocale(locale="es-es")

# PACKAGES ---------------------------------------------------------------------
library(dplyr) #arrange()
library(devtools)
library(openxlsx)
library(readxl)

# install sapmuebase from github
# remove.packages("sapmuebase")
# .rs.restartR()
# install_github("eucrow/sapmuebase")

library(sapmuebase) # and load the library


# YOU HAVE ONLY TO CHANGE THIS VARIABLES ---------------------------------------
# All the files must be located in this PATH_FILES (relative to the path where
# this file is stored):
PATH_DATA_FILES <- "/data/2022/2022_10/"

# FILES FROM SIRENO
FILENAME_DES_TOT <- "IEOUPMUEDESTOTMARCO.TXT"

# FILE FROM IPD (excel)
FILENAME_IPD <- "Informes_cobertura_10_2022_lote 1.xlsx"

# FILE WITH ANNUAL PRESCRIPTIONS
FILENAME_PRESCRIPTIONS <- "prescripciones_2021_anual.csv"

# MONTH in numeric format
MONTH <- 10

# YEAR with four digits
YEAR <- "2022"


# FUNCTIONS --------------------------------------------------------------------
source('sampling_coverage_functions.R')


# CONSTANTS AND GLOBAL VARIABLES -----------------------------------------------
BASE_FIELDS <- c("FECHA_MUE", "PUERTO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE",
                 "MES")  ###list with the common fields used in tables
month_as_character <- sprintf("%02d", MONTH)


# IMPORT DATA ------------------------------------------------------------------
# import tallas_x_up and isolate catches dataframe
catches <- importRIMCatches(FILENAME_DES_TOT, path = paste0(getwd(),PATH_DATA_FILES))

# import IPD's trip data
ipd_trips <- get_ipd_trips()

# import prescriptions file
prescriptions <- importCsvSAPMUE(paste0(getwd(),PATH_DATA_FILES, FILENAME_PRESCRIPTIONS))


# CLEAN AND PREPARE DATA ----
# IPD data ----
ipd_trips <- subset(ipd_trips, NUM_PUERTO_DIA > 0)
names(ipd_trips)[names(ipd_trips)== "NUM_PUERTO_DIA"] <- "NUM_PUERTO_DIA_IPD"
ipd_trips$PUERTO <- toupper(ipd_trips$PUERTO)
ipd_trips$MES <- MONTH
# Convert to factors:
ipd_trips$PUERTO <- as.factor(ipd_trips$PUERTO)
ipd_trips$ESTRATO_RIM <- as.factor(ipd_trips$ESTRATO_RIM)

#change levels estrato_rim
levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="RAPANTEROS_AC"] <- "RAPANTER_AC"
levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="MERLUCEROS_AC"] <- "MERLUCER_AC"
levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="NASAPULPO_CN"] <- "NASAPULP_CN"
levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="LINEA_CABALLA"] <- "LIN_CABALLA"

  # Sum JURELERA_CN with BACA_CN
  # change levels
  levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM) == "BACA_CN"] <- "BACA_CN / JURELERA_CN"
  levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM) == "JURELERA_CN"] <- "BACA_CN / JURELERA_CN"

  # Sum BETA_CN with TRASMALLO_CN
  # change levels
  levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM) == "BETA_CN"] <- "BETA_CN / TRASMALL_CN"
  levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM) == "TRASMALL_CN"] <- "BETA_CN / TRASMALL_CN"

  # Sum ENMALLE_GC with TRASMALL_GC
  # change levels
  levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="ENMALLE_GC"] <- "ENMALLE_GC / TRASMALL_GC"
  levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="TRASMALL_GC"] <- "ENMALLE_GC / TRASMALL_GC"

  # Sum Avilés with Gijón, except PALANGRE_CN and PALANGRE_AC
  # change names
  levels(ipd_trips$PUERTO) <- c(levels(ipd_trips$PUERTO), "AVILÉS / GIJÓN")
  ipd_trips[ipd_trips$PUERTO=="AVILÉS" & !(ipd_trips$ESTRATO_RIM %in% c("PALANGRE_CN", "PALANGRE_AC")), "PUERTO"] <- "AVILÉS / GIJÓN"
  ipd_trips[ipd_trips$PUERTO=="GIJÓN" & ipd_trips$ESTRATO_RIM != "BACA_AC", "PUERTO"] <- "AVILÉS / GIJÓN"

  # Sum
  by <- list(ipd_trips$MES, ipd_trips$PUERTO, ipd_trips$ESTRATO_RIM)
  ipd_trips <- aggregate(x = ipd_trips$NUM_PUERTO_DIA_IPD, by = by, FUN = sum)
  colnames(ipd_trips) <- c("MES", "PUERTO", "ESTRATO_RIM", "NUM_PUERTO_DIA_IPD")


# SIRENO data ----
catches_to_clean <- catches
# catches_to_clean$MES <- as.POSIXlt(catches_to_clean$FECHA_MUE, format="%d-%m-%Y")$mon
# catches_to_clean$MES <- as.integer(catches_to_clean$MES)+1
catches_to_clean <- catches_to_clean[as.character(catches_to_clean$YEAR)==YEAR,]
catches_to_clean <- catches_to_clean[catches_to_clean$MES==MONTH,]
catches_to_clean <- catches_to_clean[, c(BASE_FIELDS)]
catches_to_clean <- unique(catches_to_clean)
catches_to_clean$PUERTO <- toupper(catches_to_clean$PUERTO)
catches_clean <- catches_to_clean[, c("FECHA_MUE", "PUERTO", "ESTRATO_RIM", "COD_TIPO_MUE", "MES")]

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
  by <- list(catches_clean$PUERTO, catches_clean$ESTRATO_RIM, catches_clean$MES)
  sireno_trips <- aggregate(x=catches_clean$PUERTO, by=by, FUN=length)
  colnames(sireno_trips) <- c("PUERTO", "ESTRATO_RIM", "MES", "NUM_MAREAS_SIRENO")
  sireno_trips <- arrange(sireno_trips, MES, PUERTO, ESTRATO_RIM)

  # obtain number of port/day in sireno dataframe
  catches_port_day <- unique(catches_clean)
  by <- list(catches_port_day$PUERTO, catches_port_day$ESTRATO_RIM, catches_port_day$MES)
  sireno_port_days <- aggregate(x=catches_port_day$FECHA_MUE, by=by, FUN=length)
  colnames(sireno_port_days) <- c("PUERTO", "ESTRATO_RIM", "MES", "NUM_PUERTO_DIA_SIRENO")

  # merge number of trips with port/day
  sireno_trips_port_days <- merge(x=sireno_trips, y=sireno_port_days,
                                  by=c("PUERTO", "ESTRATO_RIM", "MES"),
                                  all.x = TRUE,
                                  all.y = TRUE)

  #I think this is not needed:
    # sireno_trips_port_days <- sireno_trips_port_days %>%
    #   group_by(MES, PUERTO, ESTRATO_RIM) %>%
    #   summarise_each(sum)

# Prescriptions data ----
prescriptions_trips <- prescriptions[, c("PUERTO", "PESQUERIA", "MUESTREOS.ANUALES")]
colnames(prescriptions_trips) <- c("PUERTO", "PESQUERIA", "NUM_PUERTO_DIA")
prescriptions_trips$PUERTO <- toupper(prescriptions_trips$PUERTO)

  # obtain number of trips in prescriptions dataframe by month
    prescriptions_trips$NUM_PUERTO_DIA <- round(prescriptions_trips$NUM_PUERTO_DIA/12, 1)
    #change name columns
    colnames(prescriptions_trips) <- c("PUERTO", "ESTRATO_RIM", "NUM_PUERTO_DIA_PRESCR_by_month")

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
  names(prescriptions_trips)[names(prescriptions_trips)=="NUM_PUERTO_DIA_PRESCR_by_month"] <- "NUM_PUERTO_DIA_PRESCR"
  # ESTO TAMBIÉN HABRÍA QUE CAMBIARLO:
  # add column with the month
  prescriptions_trips$MES <- MONTH

  #change port's name
  prescriptions_trips[prescriptions_trips$PUERTO=="S. VICENTE","PUERTO"] <-  "SAN VICENTE DE LA BARQUERA"
  prescriptions_trips[prescriptions_trips$PUERTO=="FISTERRA","PUERTO"] <-  "FINISTERRE"
  prescriptions_trips[prescriptions_trips$PUERTO=="RIBEIRA","PUERTO"] <-  "SANTA EUGENIA DE RIBEIRA"
  prescriptions_trips[prescriptions_trips$PUERTO=="CELEIRO","PUERTO"] <-  "CILLERO"

  #change levels estrato_rim
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="RAPANTEROS_AC", "ESTRATO_RIM"] <- "RAPANTER_AC"
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="MERLUCEROS_AC", "ESTRATO_RIM"] <- "MERLUCER_AC"
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="NASAPULPO_CN", "ESTRATO_RIM"] <- "NASAPULP_CN"
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="LINEA_CABALLA", "ESTRATO_RIM"] <- "LIN_CABALLA"
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="BETA_CN / TRASMALLO_CN", "ESTRATO_RIM"] <- "BETA_CN / TRASMALL_CN"
  prescriptions_trips[prescriptions_trips$ESTRATO_RIM=="ENMALLE_GC / TRASMALLO_GC", "ESTRATO_RIM"] <- "ENMALLE_GC / TRASMALL_GC"

# COMPARE NUM_MAREAS -----------------------------------------------------------
# Compare SIRENO vs IPD vs Prescriptions
    
  # TODO: usar Reduce!!!!!!!!
  sireno_ipd <- merge(x = sireno_trips_port_days, y = ipd_trips, by = c("PUERTO", "ESTRATO_RIM", "MES"), all.x = TRUE, all.y = TRUE)
  sireno_ipd <- arrange(sireno_ipd, MES, PUERTO, ESTRATO_RIM)
  
  sireno_ipd_presc <- merge(x = sireno_ipd, y = prescriptions_trips, by = c("PUERTO", "ESTRATO_RIM", "MES"), all.x = TRUE, all.y = TRUE)
  sireno_ipd_presc <- arrange(sireno_ipd_presc, MES, PUERTO, ESTRATO_RIM)
  
  sireno_ipd_presc[is.na(sireno_ipd_presc)] <- 0

  
# Export to csv
  # filename <- paste("cobertura_muestreos_IPD_", MONTH, ".csv", sep="")
  # write.csv(sireno_ipd_presc, file = filename, quote = FALSE, row.names = FALSE, na="0")

# Export to excel
  exportMonthlyCoverageToExcel(sireno_ipd_presc)

  