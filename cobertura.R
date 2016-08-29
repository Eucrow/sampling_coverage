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


# ---- SET WORKING DIRECTORY ---------------------------------------------------

setwd("F:/misdoc/sap/conbertura muestreos")


# ---- CONSTANTS ---------------------------------------------------------------

PATH <- getwd()

# ---- GLOBAL VARIABLES --------------------------------------------------------

BASE_FIELDS <- c("FECHA", "PUERTO", "BARCO", "ESTRATO_RIM", "TIP_MUESTREO", "MES")  ###list with the common fields used in tables

################################################################################
# YOU HAVE ONLY TO CHANGE THIS VARIABLES:
PATH_FILENAME <- "F:/misdoc/sap/conbertura muestreos/"
FILENAME_SIRENO <- "muestreos_2016_new0108.TXT"
FILENAME_IPD <- "IPD_1_trim.csv"
FILENAME_PRESCRIPTIONS <- "prescripciones_2016.csv"
MONTH <- "all" #empty, a month in number, or "all" #SIN IMPLEMENTAR!!!!!!!!
YEAR <- "2016"

# #### FUNCTIONS ###############################################################

#function to split the file tallas_x_up
##filename: name to import
##export = "TRUE" the file is export to csc
split_tallas_x_up <- function(path_filename, filename, export="FALSE", month_selected=""){
  
  fullpath<-paste(path_filename, filename, sep="/")
  
  #read the file to find where are the "----"
  temp_tallas_x_up<-readLines(c(fullpath))
  cut_rows<-grep("^---", temp_tallas_x_up, value="FALSE")#return two values
  
  #if '---' not exist, this is not a valid file
  if(length(cut_rows)==0){
    stop ("This file doesn't like a valid a tallas_x_up file.")
  } 
  
  #split the file:
  full_lengths<-read.table(file=fullpath, nrows=(cut_rows[1]-2), head=TRUE, sep=";", fill=TRUE)
  
  ##catches_in_lengths: catches of the sampled species
  catches_in_lengths<-subset(full_lengths, full_lengths$TALLA=="T.T. :")
  ##lengths
  lengths<-subset(full_lengths, full_lengths$TALLA!="T.T. :")
  ##catchs
  catches<-read.table(file=fullpath, skip=(cut_rows[2]), sep=";", head=TRUE)
  sapply(catches, class)
  
  
  #correct category "0901 Chicharros, jureles"
  catches$CATEGORIA <- as.character(catches$CATEGORIA)
  catches$CATEGORIA[catches$CATEGORIA == "0901 Chicharros, jureles"] <- "0901 Chicharros jureles"
  catches$CATEGORIA <- as.factor(catches$CATEGORIA)
  
  catches_in_lengths$CATEGORIA <- as.character(catches_in_lengths$CATEGORIA)
  catches_in_lengths$CATEGORIA[catches_in_lengths$CATEGORIA == "0901 Chicharros, jureles"] <- "0901 Chicharros jureles"
  catches_in_lengths$CATEGORIA <- as.factor(catches_in_lengths$CATEGORIA)
  
  lengths$CATEGORIA <- as.character(lengths$CATEGORIA)
  lengths$CATEGORIA[lengths$CATEGORIA == "0901 Chicharros, jureles"] <- "0901 Chicharros jureles"
  lengths$CATEGORIA <- as.factor(lengths$CATEGORIA)
  
  
  #select only the month
  if (month_selected != "" || month_selected == "all"){
    # to avoid some problems with Spanish_Spain.1252 (or if you are using another locale), change locale to Spanish_United States.1252:
    lct <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME","Spanish_United States.1252")
    
    catches$fecha_formateada <- (as.character(catches$FECHA))
    catches$fecha_formateada <- as.Date(catches$fecha_formateada, "%d-%b-%y")
    catches$fecha_formateada <- as.POSIXlt(catches$fecha_formateada)
    catches$MES <- catches$fecha_formateada$mon+1 #+1 because POSIXlt$mon is 0 to 11
    if(month_selected != "all"){
      catches <- catches[catches$month==month_selected,]
    }
    
    catches_in_lengths$fecha_formateada <- (as.character(catches_in_lengths$FECHA))
    catches_in_lengths$fecha_formateada <- as.Date(catches_in_lengths$fecha_formateada, "%d-%b-%y")
    catches_in_lengths$fecha_formateada <- as.POSIXlt(catches_in_lengths$fecha_formateada)
    catches_in_lengths$MES <- catches_in_lengths$fecha_formateada$mon+1 #+1 because POSIXlt$mon is 0 to 11
    if(month_selected != "all"){
      catches_in_lengths <- catches_in_lengths[catches_in_lengths$month==month_selected,]
    }
    
    lengths$fecha_formateada <- (as.character(lengths$FECHA))
    lengths$fecha_formateada <- as.Date(lengths$fecha_formateada, "%d-%b-%y")
    lengths$fecha_formateada <- as.POSIXlt(lengths$fecha_formateada)
    lengths$MES <- lengths$fecha_formateada$mon+1 #+1 because POSIXlt$mon is 0 to 11
    if(month_selected != "all"){
      lengths <- lengths[lengths$month==month_selected,]
    }
    
    # and now the return the initial configuration of locale:
    Sys.setlocale("LC_TIME", lct)
  }
  #group in list
  tallas_x_up<-list(catches_in_lengths=catches_in_lengths, lengths=lengths, catches=catches)
  
  #remove the extension
  filename_without_extension <- file_path_sans_ext(filename)
  
  #export in csv
  # if(export=="TRUE"){
  #   write.csv(tallas_x_up$catches, paste(PATH_FILENAME, filename_without_extension, "_catches.csv", sep=""), quote=FALSE, row.names=FALSE)
  #   write.csv(tallas_x_up$catches_in_lengths, paste(PATH_FILENAME, filename_without_extension, "_catches_in_lengths.cvs", sep=""), quote=FALSE, row.names=FALSE)
  #   write.csv(tallas_x_up$lengths, paste(PATH_FILENAME, filename_without_extension, "_lengths.csv", sep=""), quote=FALSE, row.names=FALSE)
  # }
  #return data
  return(tallas_x_up)
}

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
  tallas_x_up<-split_tallas_x_up(path_filename=PATH_FILENAME, filename=FILENAME_SIRENO, export=FALSE, month_selected = MONTH)
  catches <- tallas_x_up$catches
  
  #change column names
  names(catches)[names(catches) == "UNIPESCOD"]<-"ESTRATO_RIM"
  
  #export to csv
  #write.csv(catches, file="catches.csv", row.names = FALSE, quote = FALSE)

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
      
    # Sum Avil?s with Gij?n
      # change names
      ipd_trips[ipd_trips$PUERTO=="AVIL?S","PUERTO"] <-  "AVIL?S / GIJ?N"
      ipd_trips[ipd_trips$PUERTO=="GIJ?N","PUERTO"] <-  "AVIL?S / GIJ?N"
      
      #sum
      by <- list(ipd_trips$MES, ipd_trips$PUERTO, ipd_trips$ESTRATO_RIM)
      ipd_trips <- aggregate(x = ipd_trips$NUM_MAREAS_IPD, by = by, FUN = sum)
      colnames(ipd_trips) <- c("MES", "PUERTO", "ESTRATO_RIM", "NUM_MAREAS_IPD")
    
    # Cambio de BACA_AP A BACA_APN --> ESTO NO LO TENDR?AMOS QUE HACER EN EL FUTURO  
    levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="BACA_AP"] <- "BACA_APN"
  
  # Clean and prepare sireno trip dataframe
  catches_to_clean <- catches[, c(BASE_FIELDS)]
  catches_to_clean <- unique(catches_to_clean)
  catches_to_clean$PUERTO <- toupper(catches_to_clean$PUERTO)
  catches_clean <- catches_to_clean[, c("PUERTO", "ESTRATO_RIM", "TIP_MUESTREO", "MES")]
    # select only mt2 samples
    catches_clean <- catches_clean[catches_clean["TIP_MUESTREO"]==2,]
    
    catches_clean<-catches_clean[,!(names(catches_clean))==c("TIP_MUESTREO")]
    
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
    
    # Sum Avil?s with Gij?n
      # change names
      sireno_trips[sireno_trips$PUERTO=="AVIL?S","PUERTO"] <-  "AVIL?S / GIJ?N"
      sireno_trips[sireno_trips$PUERTO=="GIJ?N","PUERTO"] <-  "AVIL?S / GIJ?N"
      
      #sum
      by <- list(sireno_trips$MES, sireno_trips$PUERTO, sireno_trips$ESTRATO_RIM)
      sireno_trips <- aggregate(x = sireno_trips$NUM_MAREAS_SIRENO, by = by, FUN = sum)
      colnames(sireno_trips) <- c("MES", "PUERTO", "ESTRATO_RIM", "NUM_MAREAS_SIRENO")  
      
    # Cambio de BACA_AP A BACA_APN --> ESTO NO LO TENDR?AMOS QUE HACER EN EL FUTURO  
      levels(sireno_trips$ESTRATO_RIM)[levels(sireno_trips$ESTRATO_RIM)=="BACA_AP"] <- "BACA_APN"
  
  # Clean and prepare prescriptions dataframe
  prescriptions_trips <- prescriptions[, c("PUERTO", "Pesquer?a", "N?.mareas")]
  prescriptions_trips$PUERTO <- toupper(prescriptions_trips$PUERTO)

    # obtain number of trips in prescriptions dataframe by month
      prescriptions_trips$"N?.mareas" <- round(prescriptions$N?.mareas/12, 1)
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
    prescriptions_trips$frec_months <- 3#ES IMPORTANTE ATUOMATIZAR ESTO
    prescriptions_trips <- prescriptions_trips[rep(row.names(prescriptions_trips), prescriptions_trips$frec_months),]
    prescriptions_trips$MES <- 1:3#Y ESTO
    prescriptions_trips<-prescriptions_trips[,!(names(prescriptions_trips))==c("frec_months")]
      
    # change name column
    names(prescriptions_trips)[names(prescriptions_trips)=="NUM_MAREAS_PRESCRIPCIONES_by_month"] <- "NUM_MAREAS_PRES"
    
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
    #ES NECESARIO VOLVER A AGRUPAR:
    

  
  
# #### REMOVE USELESS VARIABLES ################################################
  rm(catches_to_clean, catches_clean, aggregated_catches)
  
# #### COMPARE NUM_MAREAS ######################################################
  
# Compare SIRENO vs IPD vs Prescriptions
  sireno_ipd <- merge(x = sireno_trips, y = ipd_trips, by = c("PUERTO", "ESTRATO_RIM", "MES"), all.x = TRUE, all.y = TRUE)
  sireno_ipd <- arrange(sireno_ipd, MES, PUERTO, ESTRATO_RIM)
  
  sireno_ipd_presc <- merge(x = sireno_ipd, y = prescriptions_trips, by = c("PUERTO", "ESTRATO_RIM", "MES"), all.x = TRUE, all.y = TRUE)
  sireno_ipd_presc <- arrange(sireno_ipd_presc, MES, PUERTO, ESTRATO_RIM)

  
#export to csv
  write.csv(sireno_ipd_presc, file = "cobertura muestreos IPD.csv", quote = FALSE, row.names = FALSE, na="0")
  
  
  

# #### check if MT2 are really MT2 or MT1 ######################################
  catches_MT2 <- catches[catches["TIP_MUESTREO"]==2,]
  
  
  
  # select trips with lengths
  catches_in_lenghts <- tallas_x_up$catches_in_lengths
  trips_catches_in_lengths <- catches_in_lenghts[, c("FECHA", "UNIPESCOD", "PUERTO", "BARCO")]
  trips_catches_in_lengths <- unique(trips_catches_in_lengths)  
  colnames(trips_catches_in_lengths) <- c("FECHA", "ESTRATO_RIM", "PUERTO", "BARCO")
  trips_catches_in_lengths$TIENE_TALLAS <- "con tallas"
  
  # select sireno trips keyed as MT2
  trips_sireno_w_vessel <- catches[, c(BASE_FIELDS)]
  trips_sireno_w_vessel <- unique(trips_sireno_w_vessel)
  trips_sireno_w_vessel <- trips_sireno_w_vessel[trips_sireno_w_vessel["TIP_MUESTREO"]==2,]
  
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
  lengths_MT1 <- lengths[lengths["TIP_MUESTREO"]==1,]
  
  to_lengths_MT1_by_trip <- select(lengths_MT1, FECHA, TIPO.MUESTREO, UNIPESCOD, PUERTO, BARCO, ESPECIE.TAX., CATEGORIA, ESPECIE, P.MUE.DES, TALLA, EJEMPLARES.MEDIDOS)
  
  lengths_MT1_by_trip <- group_by(to_lengths_MT1_by_trip, FECHA, TIPO.MUESTREO, UNIPESCOD, PUERTO, BARCO)
  lengths_MT1_by_trip <- summarise(lengths_MT1_by_trip, EJEMMPLARES_MEDIDOS = sum(EJEMPLARES.MEDIDOS))
  false_MT1_in_sireno <- filter(lengths_MT1_by_trip, !is.na(EJEMMPLARES_MEDIDOS))
  
  export_csv(false_MT1_in_sireno, "false_MT1_in_sireno")

  PRUEBA <- lengths[lengths$FECHA=="03-MAR-16" & lengths$BARCO == "MOROPA",]
  