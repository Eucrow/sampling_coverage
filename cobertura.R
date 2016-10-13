#### Coverage certification MT2 IPD samples
#### Script to compare the number of trips sampled by IPD and compare to the
#### samples keyed in SIRENO and required in the prescriptions.
#### 
#### 
#### Return a csv file
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
#### date of last modification: 3/8/2016
#### version: 1.00
####
#### files required: prescripciones_2016.csv, file with the IPD trips, file with
#### the samples keyed in SIRENO


# #### CONFIG ##################################################################

# ---- PACKAGES ----------------------------------------------------------------

library(dplyr) #arrange_()
library(tools) #file_path_sans_ext()

# ---- install sapmuebase from local
#install("F:/misdoc/sap/sapmuebase")
# ---- install sapmuebase from github
#install_github("Eucrow/sapmuebase") # Make sure this is the last version

library(sapmuebase) # and load the library


# ---- SET WORKING DIRECTORY ---------------------------------------------------

setwd("F:/misdoc/sap/cobertura muestreos/abril")


# ---- CONSTANTS ---------------------------------------------------------------

PATH <- getwd()

# ---- GLOBAL VARIABLES --------------------------------------------------------

BASE_FIELDS <- c("FECHA", "PUERTO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE", "MES")  ###list with the common fields used in tables

################################################################################
# YOU HAVE ONLY TO CHANGE THIS VARIABLES:
PATH_FILENAME <- "F:/misdoc/sap/cobertura muestreos/abril/"

FILENAME_SIRENO_DES_TOT <- "IEOUPMUEDESTOTMARCO.TXT"
FILENAME_SIRENO_DES_TAL <- "IEOUPMUEDESTALMARCO.TXT"
FILENAME_SIRENO_TAL <- "IEOUPMUETALMARCO.TXT"

FILENAME_IPD <- "IPD_4.csv"

FILENAME_PRESCRIPTIONS <- "prescripciones_2016.csv"

MONTH <- 4 #empty, a month in number, or "all" #SIN IMPLEMENTAR!!!!!!!!

YEAR <- "2016"

# #### FUNCTIONS ###############################################################

#function to import the ipd trips data file
##filename: name to import
import_ipd_trips <- function(){
  fullpath<-paste(PATH_FILENAME, FILENAME_IPD, sep="")
  ipd_trips<-read.table(file=fullpath, head=TRUE, sep=";", fill=TRUE)
  clean_ipd_trips <- ipd_trips[, c("MES", "PUERTO", "ESTRATO_RIM" ,"NUM_MAREAS")]
  return (clean_ipd_trips)
}

# function to import our usual csv or txt files
# file must be in the same directory than this script
import_csv <- function(filename){
  fullpath<-paste(PATH_FILENAME, filename, sep="")
  file <- read.table(file=fullpath, head=TRUE, sep=";", fill=TRUE)
}

export_csv <- function(data, filename){
  fullpath <- paste(PATH_FILENAME, filename, sep="")
  fullpath <- paste(fullpath, ".csv", sep="")
  write.csv(data, file=fullpath, quote = FALSE, row.names = FALSE)
}

# #### IMPORT DATA #############################################################

# import tallas_x_up and isolate catches dataframe
  #tallas_x_up<-split_tallas_x_up(path_filename=PATH_FILENAME, filename=FILENAME_SIRENO, export=FALSE, month_selected = MONTH)
  #catches <- tallas_x_up$catches
  
  muestreos_up <- import_muestreos_up(FILENAME_SIRENO_DES_TOT, FILENAME_SIRENO_DES_TAL,FILENAME_SIRENO_TAL, by_month = 4)
  catches <- muestreos_up$catches  

# import IPD's trip data
  ipd_trips <- import_ipd_trips()
  
# import prescriptions file
  prescriptions <- import_csv(FILENAME_PRESCRIPTIONS)


# #### CLEAN AND PREPARE DATA ##################################################

  # Clean and prepare IPD's trip dataframe
  ipd_trips <- subset(ipd_trips, NUM_MAREAS > 0)
  names(ipd_trips)[names(ipd_trips)== "NUM_MAREAS"] <- "NUM_MAREAS_IPD"
  ipd_trips$PUERTO <- toupper(ipd_trips$PUERTO)
  
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
      ipd_trips[ipd_trips$PUERTO=="AVILÉS","PUERTO"] <-  "AVILÉS / GIJÓN"
      ipd_trips[ipd_trips$PUERTO=="GIJÓN","PUERTO"] <-  "AVILÉS / GIJÓN"
      
      #sum
      by <- list(ipd_trips$MES, ipd_trips$PUERTO, ipd_trips$ESTRATO_RIM)
      ipd_trips <- aggregate(x = ipd_trips$NUM_MAREAS_IPD, by = by, FUN = sum)
      colnames(ipd_trips) <- c("MES", "PUERTO", "ESTRATO_RIM", "NUM_MAREAS_IPD")
    
    # Cambio de BACA_AP A BACA_APN --> ESTO NO LO TENDR?AMOS QUE HACER EN EL FUTURO  
    levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="BACA_AP"] <- "BACA_APN"
  
  # Clean and prepare sireno trip dataframe
  catches_to_clean <- catches
  catches_to_clean$MES <- strptime(catches_to_clean$FECHA, "%d-%m-%y")
  catches_to_clean$MES <- format(catches_to_clean$MES, "%m")
  catches_to_clean$MES <- as.integer(catches_to_clean$MES)
  catches_to_clean <- catches_to_clean[catches_to_clean$MES==MONTH,]
  catches_to_clean <- catches_to_clean[, c(BASE_FIELDS)]
  catches_to_clean <- unique(catches_to_clean)
  catches_to_clean$PUERTO <- toupper(catches_to_clean$PUERTO)
  catches_clean <- catches_to_clean[, c("PUERTO", "ESTRATO_RIM", "COD_TIPO_MUE", "MES")]
    # select only mt2 samples
    catches_clean <- catches_clean[catches_clean["COD_TIPO_MUE"]==2,]
    
    catches_clean<-catches_clean[,!(names(catches_clean))==c("COD_TIPO_MUE")]
    
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
    #ES NECESARIO VOLVER A AGRUPAR:¿¿¿??? yo creo que no
    

  
  
# #### REMOVE USELESS VARIABLES ################################################
  rm(catches_to_clean, catches_clean, aggregated_catches)
  
# #### COMPARE NUM_MAREAS ######################################################
  
# Compare SIRENO vs IPD vs Prescriptions
  sireno_ipd <- merge(x = sireno_trips, y = ipd_trips, by = c("PUERTO", "ESTRATO_RIM", "MES"), all.x = TRUE, all.y = TRUE)
  sireno_ipd <- arrange(sireno_ipd, MES, PUERTO, ESTRATO_RIM)
  
  sireno_ipd_presc <- merge(x = sireno_ipd, y = prescriptions_trips, by = c("PUERTO", "ESTRATO_RIM", "MES"), all.x = TRUE, all.y = TRUE)
  sireno_ipd_presc <- arrange(sireno_ipd_presc, MES, PUERTO, ESTRATO_RIM)

  
#export to csv
  filename <- paste("cobertura_muestreos_IPD_", MONTH, ".csv", sep="")
  write.csv(sireno_ipd_presc, file = filename, quote = FALSE, row.names = FALSE, na="0")
  
  
  

# #### check if MT2 are really MT2 or MT1 ######################################
  catches_MT2 <- catches[catches["COD_TIPO_MUE"]==2,]
  
  
  
  # select trips with lengths
  catches_in_lenghts <- tallas_x_up$catches_in_lengths
  trips_catches_in_lengths <- catches_in_lenghts[, c("FECHA", "UNIPESCOD", "PUERTO", "BARCO")]
  trips_catches_in_lengths <- unique(trips_catches_in_lengths)  
  colnames(trips_catches_in_lengths) <- c("FECHA", "ESTRATO_RIM", "PUERTO", "BARCO")
  trips_catches_in_lengths$TIENE_TALLAS <- "con tallas"
  
  # select sireno trips keyed as MT2
  trips_sireno_w_vessel <- catches[, c(BASE_FIELDS)]
  trips_sireno_w_vessel <- unique(trips_sireno_w_vessel)
  trips_sireno_w_vessel <- trips_sireno_w_vessel[trips_sireno_w_vessel["COD_TIPO_MUE"]==2,]
  
  # MT1 keyed in sireno as MT2
  false_MT2_in_sireno <- merge(x = trips_sireno_w_vessel, y = trips_catches_in_lengths, all = TRUE)
  false_MT2_in_sireno <- false_MT2_in_sireno[is.na(false_MT2_in_sireno$TIENE_TALLAS),]  
  
  export_csv(false_MT2_in_sireno, "false_MT2_in_sireno")

  
  # MT1 samples with lengths --> usually keyed initial and final lenght but without lenght meassures
  # this is not useful
  # MT1_with_lengths_in_sireno <- merge(x = trips_sireno_w_vessel, y = trips_catches_in_lengths, all = TRUE)  
  # MT1_with_lengths_in_sireno <- false_MT1_in_sireno[is.na(false_MT1_in_sireno$MES),]
  
  # false MT1 in sireno
  lengths <- tallas_x_up$lengths
  lengths_MT1 <- lengths[lengths["COD_TIPO_MUE"]==1,]
  
  to_lengths_MT1_by_trip <- select(lengths_MT1, FECHA, TIPO.MUESTREO, UNIPESCOD, PUERTO, BARCO, ESPECIE.TAX., CATEGORIA, ESPECIE, P.MUE.DES, TALLA, EJEMPLARES.MEDIDOS)
  
  lengths_MT1_by_trip <- group_by(to_lengths_MT1_by_trip, FECHA, TIPO.MUESTREO, UNIPESCOD, PUERTO, BARCO)
  lengths_MT1_by_trip <- summarise(lengths_MT1_by_trip, EJEMMPLARES_MEDIDOS = sum(EJEMPLARES.MEDIDOS))
  false_MT1_in_sireno <- filter(lengths_MT1_by_trip, !is.na(EJEMMPLARES_MEDIDOS))
  
  export_csv(false_MT1_in_sireno, "false_MT1_in_sireno")

  PRUEBA <- lengths[lengths$FECHA=="03-MAR-16" & lengths$BARCO == "MOROPA",]
  