# Coverage certification MT2 IPD samples
# Script to compare the number of trips sampled by IPD with the samples saved in
# SIRENO and required in the prescriptions.
# 
# The script return a formatted table in xls.
#
# author: Marco A. Amez Fernandez
# email: ieo.marco.a.amez@gmail.com
#
# files required: prescripciones_2018.csv and file with the samples saved in
# SIRENO


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
PATH_FILENAME <- "F:/misdoc/sap/sampling_coverage/data/2019/2019_fist_semester"

# FILES FROM SIRENO
FILENAME_SIRENO_DES_TOT <- "IEOUPMUEDESTOTSIRENO_2018_first_semester.TXT"

# FILE WITH ANNUAL PRESCRIPTIONS
FILENAME_PRESCRIPTIONS <- "prescripciones_2019_anual.csv"

MONTH <- all #empty, a month in number, or "all"

YEAR <- "2018"


# PACKAGES ---------------------------------------------------------------------
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


# CONSTANTS AND GLOBAL VARIABLES -----------------------------------------------
# set working directory:
setwd(PATH_FILENAME)


BASE_FIELDS <- c("FECHA_MUE", "PUERTO", "BARCO", "ESTRATO_RIM", "COD_TIPO_MUE", "MES")  ###list with the common fields used in tables


# FUNCTIONS --------------------------------------------------------------------
# Export the coverage dataframe obtained in this script to excel
# df: dataframe to export. In this case is contained in sireno_ipd_pres variable
exportCoverageToExcel <- function(df){
  
  # ---- Create a Workbook
  wb <- createWorkbook()
  
  # ---- Add worksheets
  name_worksheet <- YEAR
  addWorksheet(wb, name_worksheet)
  
  # ---- Add calculated columns
  num_rows_df <- nrow(df)
  num_cols_df <- length(df)
  first_row <- 2 #the first row with data (avoid the header row) in excel
  last_row <- num_rows_df+1 
  
  # ATTENTION: the formula must have the english format:
  # name of function in english, parameters separation with comma
  SIRENOvsPRESCRIPCIONES <- paste0("IF(C", first_row:last_row ,"=D", first_row:last_row, ",\"OK\",", "IF(C", first_row:last_row ,"<D", first_row:last_row,",\"DEFICIT\", IF(C", first_row:last_row, ">D", first_row:last_row," ,\"SUPERAVIT\",)))" )
  
  # And add it to the workbook
  df["SIRENOvsPRESCRIPCIONES"] <- SIRENOvsPRESCRIPCIONES
  
  # the variable must to have the formula class
  # in this way, the variable has two clases!! :O ???????????:
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
  addStyle(wb, sheet = name_worksheet, head_style_remarked, rows = 1, cols = (num_cols_df+1))
  addStyle(wb, sheet = name_worksheet, total_style, rows = num_rows_df+2, cols = 1:5)
  addStyle(wb, sheet = name_worksheet, number_style, rows = 2:(num_rows_df+1), cols = 3:4, stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = name_worksheet, borders_style, rows = 1:(num_rows_df+1), cols = 1:5, stack = TRUE, gridExpand = TRUE)
  
  # ---- Cells width
  #setColWidths(wb, name_worksheet, cols = c(1:(num_cols_df+2)), widths = c(28, 28, 11, 20, 20, 20, 20, 25) )
  
  # ---- Row heights
  setRowHeights(wb, name_worksheet, rows = 1, heights = "24")
  
  # ---- Add calculated rows
  total_cells <- paste0("SUM(", letters[3:5], "2:", letters[3:5], num_rows_df+1 ,")")
  
  # ---- And add it to the workbook
  # TODO: improve this:
  writeFormula(wb, name_worksheet, total_cells[1], startCol = 3, startRow = num_rows_df+2)
  writeFormula(wb, name_worksheet, total_cells[2], startCol = 4, startRow = num_rows_df+2)
  writeFormula(wb, name_worksheet, total_cells[3], startCol = 5, startRow = num_rows_df+2)
  
  
  # ---- Add conditional formatting
  content_false_style <- createStyle(bgFill = "#E46D0A")
  content_deficit_style <- createStyle(bgFill = "#E46D0A")
  content_superavit_style <- createStyle(bgFill = "#FFC000")
  
  conditionalFormatting(wb, name_worksheet, cols = 7, rows = 2:num_rows_df, rule = "==\"FALSO\"", style = content_false_style, type = "expression")
  conditionalFormatting(wb, name_worksheet, cols = 8, rows = 2:num_rows_df, rule = "==\"DEFICIT\"", style = content_deficit_style, type = "expression")
  conditionalFormatting(wb, name_worksheet, cols = 8, rows = 2:num_rows_df, rule = "==\"SUPERAVIT\"", style = content_superavit_style, type = "expression")
  
  # ---- Export to excel
  final_filename <- paste0("cobertura_muestreos_IPD_", YEAR, ".xlsx")
  # source: https://github.com/awalker89/openxlsx/issues/111
  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") ## path to zip.exe
  saveWorkbook(wb, final_filename, overwrite = TRUE)
}


# IMPORT DATA ------------------------------------------------------------------
# import catches from SIRENO files
  catches <- importRIMCatches(FILENAME_SIRENO_DES_TOT)

# import prescriptions file
  prescriptions <- importCsvSAPMUE(FILENAME_PRESCRIPTIONS)


# CLEAN AND PREPARE DATA -------------------------------------------------------
  # Clean and prepare sireno trip dataframe
  catches_to_clean <- catches
  # select only IEO samples
  catches_to_clean <- catches_to_clean[catches_to_clean$PROCEDENCIA == "IEO",]
  # select only mt2 samples
  catches_to_clean <- catches_to_clean[catches_to_clean["COD_TIPO_MUE"]==2,]
  
  catches_to_clean <- catches_to_clean[, c(BASE_FIELDS)]
  catches_to_clean <- unique(catches_to_clean)
  catches_to_clean$PUERTO <- toupper(catches_to_clean$PUERTO)
  catches_clean <- catches_to_clean[, c("PUERTO", "ESTRATO_RIM", "COD_TIPO_MUE")]
    
    catches_clean<-catches_clean[,!(names(catches_clean))==c("COD_TIPO_MUE")]
    
    # obtain number of trips in sireno dataframe
    by <- list(catches_clean$PUERTO, catches_clean$ESTRATO_RIM)
    aggregated_catches <- aggregate(x=catches_clean$PUERTO, by=by, FUN=length)
    colnames(aggregated_catches) <- c("PUERTO", "ESTRATO_RIM", "NUM_MAREAS_SIRENO")
    sireno_trips <- arrange(aggregated_catches, PUERTO, ESTRATO_RIM)
    
    # Sum JURELERA_CN with BACA_CN
      # change levels
      levels(sireno_trips$ESTRATO_RIM)[levels(sireno_trips$ESTRATO_RIM)=="BACA_CN"] <- "BACA_CN / JURELERA_CN"
      levels(sireno_trips$ESTRATO_RIM)[levels(sireno_trips$ESTRATO_RIM)=="JURELERA_CN"] <- "BACA_CN / JURELERA_CN"
      
      #sum
      by <- list(sireno_trips$PUERTO, sireno_trips$ESTRATO_RIM)
      sireno_trips <- aggregate(x = sireno_trips$NUM_MAREAS_SIRENO, by = by, FUN = sum)
      colnames(sireno_trips) <- c("PUERTO", "ESTRATO_RIM", "NUM_MAREAS_SIRENO")
    
    # Sum Avilés with Gijón
      # change names
      sireno_trips[sireno_trips$PUERTO=="AVILÉS","PUERTO"] <-  "AVILÉS / GIJÓN"
      sireno_trips[sireno_trips$PUERTO=="GIJÓN","PUERTO"] <-  "AVILÉS / GIJÓN"
      
      #sum
      by <- list(sireno_trips$PUERTO, sireno_trips$ESTRATO_RIM)
      sireno_trips <- aggregate(x = sireno_trips$NUM_MAREAS_SIRENO, by = by, FUN = sum)
      colnames(sireno_trips) <- c("PUERTO", "ESTRATO_RIM", "NUM_MAREAS_SIRENO")  
      

  # Clean and prepare prescriptions dataframe
  prescriptions_trips <- prescriptions[, c("PUERTO", "Pesquería", "Nº.mareas")]
  colnames(prescriptions_trips) <- c("PUERTO", "ESTRATO_RIM", "NUM_MAREAS_PRES")
  prescriptions_trips$PUERTO <- toupper(prescriptions_trips$PUERTO)

    
    #LAS LINEAS SIGUIENTES SON PARA EL CASO DE QUE SEA UN TRIMESTRE:
    ##ESTO ES UNA CHAPUZA DE MUCHO CUIDADO:
    #prescriptions_trips$frec_months <- 3#ES IMPORTANTE ATUOMATIZAR ESTO
    #prescriptions_trips <- prescriptions_trips[rep(row.names(prescriptions_trips), prescriptions_trips$frec_months),]
    #prescriptions_trips$MES <- 1:3#Y ESTO
    #prescriptions_trips<-prescriptions_trips[,!(names(prescriptions_trips))==c("frec_months")]

    
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
  
  sireno_ipd_presc <- merge(x = sireno_trips, y = prescriptions_trips, by.x = c("PUERTO", "ESTRATO_RIM"), all.x = TRUE, all.y = TRUE)
  sireno_ipd_presc <- arrange(sireno_ipd_presc, PUERTO, ESTRATO_RIM)
  
  sireno_ipd_presc[is.na(sireno_ipd_presc)] <- 0

#export to csv
  # filename <- paste("cobertura_muestreos_IPD_", MONTH, ".csv", sep="")
  # write.csv(sireno_ipd_presc, file = filename, quote = FALSE, row.names = FALSE, na="0")

#export to excel
  exportCoverageToExcel(sireno_ipd_presc)


  
  