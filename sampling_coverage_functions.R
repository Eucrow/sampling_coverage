#' Import the trips file from ipd (in excel format). 
#' The parameters are taken from the constants in the script.
#' @return dataframe with the data of file properly imported.
import_ipd_trips <- function(){
  fullpath<-paste(getwd(), PATH_DATA_FILES, FILENAME_IPD, sep="")
  ipd_trips<-read_excel(fullpath, skip = 8)
  colnames(ipd_trips) <- c("TIPO_MUE", "PUERTO", "ESTRATO_RIM", "NUM_PUERTO_DIA", "NUM_MUESTREOS", "OBSERVACIONES")
  return (ipd_trips)
}

#' Obtain the filtered and formatted ipd trips dataframe.
#' @details Filter data by TIPO_MUE MT2 and MT3 type samples.
#' Requires the import_ipd_trips() function.
#' @return dataframe whit the data of the file properly formatted and filtered.
get_ipd_trips <- function(){
  ipd_trips <- import_ipd_trips()
  # use subset instead of [ because [ return the NAs too.
  ipd_trips <- subset(ipd_trips, ipd_trips[["TIPO_MUE"]] == "MT2")
  ipd_trips <- ipd_trips[, c("PUERTO", "ESTRATO_RIM", "NUM_PUERTO_DIA", "OBSERVACIONES") ]
  ipd_trips[["NUM_PUERTO_DIA"]] <- as.integer(as.character(ipd_trips[["NUM_PUERTO_DIA"]]))
  return(ipd_trips)
}

#' Export the monthly coverage dataframe obtained to excel.
#' df: dataframe to export.
exportMonthlyCoverageToExcel <- function(df){
  
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
  index_num_port_day_sireno <- toupper(letters[which(colnames(df)=="NUM_PUERTO_DIA_SIRENO")])
  index_num_port_day_ipd <- toupper(letters[which(colnames(df)=="NUM_PUERTO_DIA_IPD")])
  index_num_port_day_presc <- toupper(letters[which(colnames(df)=="NUM_PUERTO_DIA_PRESCR")])
  
  # ATTENTION: the formula must have the english format:
  # name of function in english, parameters separation with comma
  SIRENOvsIPD <- paste0("IF(", index_num_port_day_sireno, first_row:last_row, "=", index_num_port_day_ipd, first_row:last_row, ",\"CORRECTO\",\"FALSO\")")
  
  SIRENOvsPRESCRIPCIONES <- paste0("IF(", index_num_port_day_sireno, first_row:last_row, "=", index_num_port_day_presc, first_row:last_row, ",\"OK\",", "IF(",index_num_port_day_sireno, first_row:last_row ,"<",index_num_port_day_presc, first_row:last_row,",\"DEFICIT\", IF(",index_num_port_day_sireno, first_row:last_row, ">",index_num_port_day_presc, first_row:last_row," ,\"SUPERAVIT\",)))" )
  
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
  addStyle(wb, sheet = name_worksheet, number_style, rows = 2:(num_rows_df+1), cols = 3:num_cols_df, stack = TRUE, gridExpand = TRUE)
  addStyle(wb, sheet = name_worksheet, borders_style, rows = 1:(num_rows_df+1), cols = 1:(num_cols_df+2), stack = TRUE, gridExpand = TRUE)
  
  # ---- Cells width
  setColWidths(wb, name_worksheet, cols = c(1:(num_cols_df+2)), widths = c(28, 28, 11, 25, 25, 25, 25, 25, 25) )
  
  # ---- Row heights
  setRowHeights(wb, name_worksheet, rows = 1, heights = "24")
  
  
  # ---- Add text cells
  # with_ALL_cells <- "ALL"
  comment_cell <- "Las prescripciones establecen un numero de DÍAS anuales por
  PUERTO y ESTRATO TÉCNICO en los que realizar los muestreos de tallas, y que
  deben repartirse a lo largo de los 12 meses. Para determinados estratos
  (LINEA_CABALLA, CERCO_CN en Santoña) el muestreo se aumenta o concentra,
  por necesidades de seguimiento de la actividad."
  
  # ---- And add it to the workbook
  # writeData(wb, name_worksheet, with_ALL_cells, startCol = 1, startRow = num_rows_df+2)
  # writeData(wb, name_worksheet, with_ALL_cells, startCol = 2, startRow = num_rows_df+2)
  writeData(wb, name_worksheet, comment_cell, startCol = 1, startRow = num_rows_df+7)
  
  
  # ---- Add calculated rows
  total_cells <- paste0("SUM(", letters[4:7], "2:", letters[4:7], num_rows_df+1 ,")")
  
  # ---- And add it to the workbook
  # TODO: improve this:
  writeFormula(wb, name_worksheet, total_cells[1], startCol = 4, startRow = num_rows_df+2)
  writeFormula(wb, name_worksheet, total_cells[2], startCol = 5, startRow = num_rows_df+2)
  writeFormula(wb, name_worksheet, total_cells[3], startCol = 6, startRow = num_rows_df+2)
  writeFormula(wb, name_worksheet, total_cells[4], startCol = 7, startRow = num_rows_df+2)
  
  # ---- Add "TOTALES:" text:
  writeData(wb, name_worksheet, "TOTALES:", startCol = 3, startRow = num_rows_df+2)
  # ---- And add styles:
  addStyle(wb, sheet = name_worksheet, total_style, rows = num_rows_df+2, cols = 3:num_cols_df)
  addStyle(wb, sheet = name_worksheet, borders_style, rows = num_rows_df+2, cols = 3:num_cols_df, stack = TRUE, gridExpand = TRUE)
  
  # ---- Add conditional formatting
  content_false_style <- createStyle(bgFill = "#E46D0A")
  content_deficit_style <- createStyle(bgFill = "#E46D0A")
  content_superavit_style <- createStyle(bgFill = "#FFC000")
  
  conditionalFormatting(wb, name_worksheet, cols = 8, rows = 2:num_rows_df+1, rule = "==\"FALSO\"", style = content_false_style, type = "expression")
  conditionalFormatting(wb, name_worksheet, cols = 9, rows = 2:num_rows_df+1, rule = "==\"DEFICIT\"", style = content_deficit_style, type = "expression")
  conditionalFormatting(wb, name_worksheet, cols = 9, rows = 2:num_rows_df+1, rule = "==\"SUPERAVIT\"", style = content_superavit_style, type = "expression")
  
  # ---- Export to excel
  final_filename <- paste0(getwd(), PATH_DATA_FILES, "cobertura_muestreos_IPD_", YEAR, "_" , month_as_character, ".xlsx")
  # source: https://github.com/awalker89/openxlsx/issues/111
  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") ## path to zip.exe
  saveWorkbook(wb, final_filename, overwrite = TRUE)
}

# Export the annual coverage dataframe obtained to excel.
# df: dataframe to export.
exportAnnualCoverageToExcel <- function(df){
  
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
  SIRENOvsPRESCRIPCIONES <- paste0("IF(D", first_row:last_row ,"=E", first_row:last_row, ",\"OK\",", "IF(D", first_row:last_row ,"<E", first_row:last_row,",\"DEFICIT\", IF(D", first_row:last_row, ">E", first_row:last_row," ,\"SUPERAVIT\",)))" )
  
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
  total_cells <- paste0("SUM(", c("C", "D", "E"), "2:", c("C", "D", "E"), num_rows_df+1 ,")")
  
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
  if(SUFFIX_FILENAME != ""){
    final_filename <- paste0("cobertura_muestreos_IPD_", YEAR, "_", SUFFIX_FILENAME, ".xlsx")
  } else {
    final_filename <- paste0("cobertura_muestreos_IPD_", YEAR, ".xlsx")
  }
  # source: https://github.com/awalker89/openxlsx/issues/111
  Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") ## path to zip.exe
  saveWorkbook(wb, final_filename, overwrite = TRUE)
}