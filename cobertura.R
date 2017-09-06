#### Coverage certification MT2 IPD samples
#### Script to compare the number of trips sampled by IPD and compare to the
#### samples keyed in SIRENO and required in the prescriptions.
#### 
#### The script return a formatted table in xls.
####
#### author: Marco A. Amez Fernandez
#### email: ieo.marco.a.amez@gmail.com
####
#### files required: prescripciones_2017.csv, file with the IPD trips, file with
#### the samples saved in SIRENO
#### ---------------------------------------------------------------------------

# PRUEBA FEATURE 
# añadimos version

# ------------------------------------------------------------------------------
# #### INSTRUCTIONS ############################################################
# ------------------------------------------------------------------------------

# To use this scritp:
# - Change variables in "YOU HAVE ONLY TO CHANGE THIS VARIABLES" section of this
# script.
# - Make sure that this required files are in PATH_FILENAME path:
#     * file with the IPD trips
#     * prescripciones_2017.csv (with this format: csv separated by ";", without quotes)
#     * report files tallas_x_up from SIRENO
# - Run all the script
# - A formatted xlsx file is obtained in PATH_FILENAME path


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# YOU HAVE ONLY TO CHANGE THIS VARIABLES 
# All the files must be located in this PATH_FILENAME:
PATH_FILENAME <- "F:/misdoc/sap/cobertura muestreos/2017/2017-02/"

# FILES FROM SIRENO
FILENAME_SIRENO_DES_TOT <- "IEOUPMUEDESTOTMARCO.TXT"
FILENAME_SIRENO_DES_TAL <- "IEOUPMUEDESTALMARCO.TXT"
FILENAME_SIRENO_TAL <- "IEOUPMUETALMARCO.TXT"

# FILE FROM IPD: attention with the format: csv separated by ";", without quotes
FILENAME_IPD <- "IPD-2017-02.csv"

# FILE WITH ANNUAL PRESCRIPTIONS
FILENAME_PRESCRIPTIONS <- "prescripciones_2017.csv"

MONTH <- 2 #a month in number

YEAR <- "2017"

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# #### CONFIG ##################################################################
# ------------------------------------------------------------------------------

# ---- PACKAGES ----------------------------------------------------------------

library(dplyr) #arrange_()
library(tools) #file_path_sans_ext()
library(devtools)
#install.packages("openxlsx", dependencies=TRUE)  
library(openxlsx)

# ---- install sapmuebase from local
#install("F:/misdoc/sap/sapmuebase")

# ---- install sapmuebase from github
#install_github("Eucrow/sapmuebase") # Make sure this is the last version

library(sapmuebase) # and load the library


# ---- CONSTANTS AND GLOBAL VARIABLES ------------------------------------------

# set working directory:
setwd(PATH_FILENAME)


BASE_FIELDS <- c("FECHA", "PUERTO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE", "MES")  ###list with the common fields used in tables


month_as_character <- sprintf("%02d", MONTH)


# ------------------------------------------------------------------------------
# #### FUNCTIONS ###############################################################
# ------------------------------------------------------------------------------

# OLD function to import the ipd trips data file
##filename: name to import
OLD_import_ipd_trips <- function(){
  fullpath<-paste(PATH_FILENAME, FILENAME_IPD, sep="")
  ipd_trips<-read.table(file=fullpath, head=TRUE, sep=";", fill=TRUE)
  clean_ipd_trips <- ipd_trips[, c("MES", "PUERTO", "ESTRATO_RIM" ,"NUM_MAREAS")]
  return (clean_ipd_trips)
}

#function to import the ipd trips data file
##filename: name to import
import_ipd_trips <- function(){
  fullpath<-paste(PATH_FILENAME, FILENAME_IPD, sep="")
  ipd_trips<-read.table(file=fullpath, head=TRUE, sep=";", fill=TRUE)
  colnames(ipd_trips) <- c("MES", "AÑO", "PROYECTO", "PUERTO", "ESTRATO_RIM", "TIPO_MUE", "NUM_MAREAS", "OBSERVACIONES")
  return (ipd_trips)
}

#function to obtain the filtered and formatted ipd trips dataframe
get_ipd_trips <- function(){
  ipd_trips <- import_ipd_trips()
  ipd_trips <- ipd_trips %>%
                filter(PROYECTO == "AREA ICES" & TIPO_MUE == "MT2") %>%
                select(MES, PUERTO, ESTRATO_RIM, NUM_MAREAS, OBSERVACIONES)
  ipd_trips[["NUM_MAREAS"]] <- as.integer(as.character(ipd_trips[["NUM_MAREAS"]])) # NUM_MAREAS must be numeric
  return(ipd_trips)
}

# function to export the coverage dataframe obtained in this script to excel
# df: dataframe to export. In this case is contained in sireno_ipd_pres variable
exportCoverageToExcel <- function(df){
  
  # ---- Create a Workbook
  wb <- createWorkbook()
  
  
  # ---- Add worksheets
  name_worksheet <- paste("0",MONTH,sep="")
  addWorksheet(wb, name_worksheet)
  
  
  # ---- Add calculated columns
  num_rows_df <- nrow(df)
  num_cols_df <- length(df)
  first_row <- 2 #the first row with data (avoid the header row) in excel
  last_row <- num_rows_df+1 
  
  # ATTENTION: the formula must have the english format:
  # name of function in english, parameters separation with comma
  SIRENOvsIPD <- paste0("IF(D", first_row:last_row, "=E", first_row:last_row, ",\"CORRECTO\",\"FALSO\")")
  
  SIRENOvsPRESCRIPCIONES <- paste0("IF(D", first_row:last_row ,"=F", first_row:last_row, ",\"OK\",", "IF(D", first_row:last_row ,"<F", first_row:last_row,",\"DEFICIT\", IF(D", first_row:last_row, ">F", first_row:last_row," ,\"SUPERAVIT\",)))" )
  
  # And add it to the workbook
  df["SIRENOvsIPD"] <- SIRENOvsIPD
  df["SIRENOvsPRESCRIPCIONES"] <- SIRENOvsPRESCRIPCIONES
  
  # the variable must to have the formula class
  # in this way, the variable has two clases!! :O ???????????:
  class(df[["SIRENOvsIPD"]]) <- c(class(df[["SIRENOvsIPD"]]), "formula")
  class(df[["SIRENOvsPRESCRIPCIONES"]]) <- c(class(df[["SIRENOvsPRESCRIPCIONES"]]), "formula")
  
  
  # ---- Add data to the workbook
  writeData(wb, name_worksheet, df)    
  
  
  # ---- Stylize data
  # ---- Create styles
  head_style <- createStyle(fgFill = "#8DB4E3", 
                            fontName="Calibri", 
                            fontSize = "11",
                            halign = "center",
                            valign = "center")
  head_style_remarked <- createStyle(textDecoration = "bold",
                                     fgFill = "#C2D69A",
                                     halign = "center",
                                     valign = "center")
  borders_style <- createStyle(border =  "TopBottomLeftRight",
                               borderColour = "#000000",
                               borderStyle = "thin")
  total_style <- createStyle(fgFill = "#538ED5",
                             textDecoration = "bold",
                             halign = "center")
  number_style <- createStyle(halign = "center")
  
  # ---- Apply styles
  addStyle(wb, sheet = name_worksheet, head_style, rows = 1, cols = 1:num_cols_df)
  addStyle(wb, sheet = name_worksheet, head_style_remarked, rows = 1, cols = (num_cols_df+1):(num_cols_df+2))
  addStyle(wb, sheet = name_worksheet, total_style, rows = num_rows_df+2, cols = 1:8)
  addStyle(wb, sheet = name_worksheet, number_style, rows = 2:(num_rows_df+1), cols = 3:6, stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = name_worksheet, borders_style, rows = 1:(num_rows_df+1), cols = 1:8, stack = TRUE, gridExpand = TRUE)
  
  # ---- Cells width
  setColWidths(wb, name_worksheet, cols = c(1:(num_cols_df+2)), widths = c(28, 28, 11, 20, 20, 20, 20, 25) )
  
  # ---- Row heights
  setRowHeights(wb, name_worksheet, rows = 1, heights = "24")
  
  
  # ---- Add text cells
  with_ALL_cells <- "ALL"
  comment_cell <- "Las prescripciones establecen un numero de mareas anuales que deben repartirse a lo largo de los 12 meses. Para determinados estratos (LINEA_CABALLA, CERCO_CN en Santoña) el muestreo se aumenta o concentra, por necesidades de seguimiento de la actividad "
  
  # ---- And add it to the workbook
  writeData(wb, name_worksheet, with_ALL_cells, startCol = 1, startRow = num_rows_df+2)
  writeData(wb, name_worksheet, with_ALL_cells, startCol = 2, startRow = num_rows_df+2)
  writeData(wb, name_worksheet, comment_cell, startCol = 1, startRow = num_rows_df+7)
  
  
  # ---- Add calculated rows
  total_cells <- paste0("SUM(", letters[4:6], "2:", letters[4:6], num_rows_df+1 ,")")
  
  # ---- And add it to the workbook
  # TODO: improve this:
  writeFormula(wb, name_worksheet, total_cells[1], startCol = 4, startRow = num_rows_df+2)
  writeFormula(wb, name_worksheet, total_cells[2], startCol = 5, startRow = num_rows_df+2)
  writeFormula(wb, name_worksheet, total_cells[3], startCol = 6, startRow = num_rows_df+2)
  
  
  # ---- Add conditional formatting
  content_false_style <- createStyle(bgFill = "#E46D0A")
  content_deficit_style <- createStyle(bgFill = "#E46D0A")
  content_superavit_style <- createStyle(bgFill = "#FFC000")
  
  conditionalFormatting(wb, name_worksheet, cols = 7, rows = 2:num_rows_df, rule = "==\"FALSO\"", style = content_false_style, type = "expression")
  conditionalFormatting(wb, name_worksheet, cols = 8, rows = 2:num_rows_df, rule = "==\"DEFICIT\"", style = content_deficit_style, type = "expression")
  conditionalFormatting(wb, name_worksheet, cols = 8, rows = 2:num_rows_df, rule = "==\"SUPERAVIT\"", style = content_superavit_style, type = "expression")
  
  # ---- Export to excel
  final_filename <- paste0("cobertura_muestreos_IPD_", YEAR, "_" , month_as_character, ".xlsx")
  # source: https://github.com/awalker89/openxlsx/issues/111
  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") ## path to zip.exe
  saveWorkbook(wb, final_filename, overwrite = TRUE)
}


# ------------------------------------------------------------------------------
# #### IMPORT DATA #############################################################
# ------------------------------------------------------------------------------

# import tallas_x_up and isolate catches dataframe
  muestreos_up <- importMuestreosUP(FILENAME_SIRENO_DES_TOT, FILENAME_SIRENO_DES_TAL,FILENAME_SIRENO_TAL, by_month = MONTH)
  catches <- muestreos_up$catches  
  catches_in_lengths <- muestreos_up$catches_in_lengths
  lengths <- muestreos_up$lengths
  
  muestreos_up1 <- muestreos_up
  muestreos_up1$catches[1:10,"newcolumn"] <- "jeolu1"
  muestreos_up1$catches_in_lengths[1:10,"newcolumn"] <- "jeolu1"
  muestreos_up1$lengths[1:10,"newcolumn"] <- "jeolu1"
  
  muestreos_up2 <- muestreos_up
  muestreos_up2$catches[11:20,"newcolumn"] <- "jeolu2"
  muestreos_up2$catches_in_lengths[11:20,"newcolumn"] <- "jeolu2"
  muestreos_up2$lengths[11:20,"newcolumn"] <- "jeolu2"
  
  variables_to_merge <- colnames(muestreos_up$catches)

  total_m <- mapply(function(x,y){
    merge(x, y, by = variables_to_merge)
  },
  muestreos_up1,
  muestreos_up2)
  
  catches_total_m <- total_m$catches
  catches_in_lenghts_total_m <- total_m$catches_in_lengths
  lengths_total_m <- total_m$lengths
  
# import IPD's trip data
  ipd_trips <- get_ipd_trips()
  
# import prescriptions file
  prescriptions <- importCsvSAPMUE(FILENAME_PRESCRIPTIONS)

  
# ------------------------------------------------------------------------------
# #### CLEAN AND PREPARE DATA ##################################################
# ------------------------------------------------------------------------------
  
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
    # levels(ipd_trips$ESTRATO_RIM)[levels(ipd_trips$ESTRATO_RIM)=="BACA_AP"] <- "BACA_APN"
  
  # Clean and prepare sireno trip dataframe
  catches_to_clean <- catches
  catches_to_clean$MES <- as.POSIXlt(catches_to_clean$FECHA, format="%d-%m-%Y")$mon
  catches_to_clean$MES <- as.integer(catches_to_clean$MES)+1
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

    
# ------------------------------------------------------------------------------
# #### COMPARE NUM_MAREAS ######################################################
# ------------------------------------------------------------------------------


# Compare SIRENO vs IPD vs Prescriptions
    
  # TODO: usar Reduce!!!!!!!!
  sireno_ipd <- merge(x = sireno_trips, y = ipd_trips, by = c("PUERTO", "ESTRATO_RIM", "MES"), all.x = TRUE, all.y = TRUE)
  sireno_ipd <- arrange(sireno_ipd, MES, PUERTO, ESTRATO_RIM)
  
  sireno_ipd_presc <- merge(x = sireno_ipd, y = prescriptions_trips, by = c("PUERTO", "ESTRATO_RIM", "MES"), all.x = TRUE, all.y = TRUE)
  sireno_ipd_presc <- arrange(sireno_ipd_presc, MES, PUERTO, ESTRATO_RIM)
  
  sireno_ipd_presc[is.na(sireno_ipd_presc)] <- 0

  
#export to csv
  # filename <- paste("cobertura_muestreos_IPD_", MONTH, ".csv", sep="")
  # write.csv(sireno_ipd_presc, file = filename, quote = FALSE, row.names = FALSE, na="0")

#export to excel
  exportCoverageToExcel(sireno_ipd_presc)


  
  