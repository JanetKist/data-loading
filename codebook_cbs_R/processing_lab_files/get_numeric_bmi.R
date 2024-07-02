#' Function that presents a standardised way to get BMIS, both coded as
#' BMI and BMI calculated from height and weight
#'  
#' For the calculation, the user can give a margin for weights to be combined with height to get a BMI. Measurements of weights
#' will be matched with measurements of height if the dBepalingdatum of the
#' weight is within MARGIN days of the dBepalingdatum of height.
#' In other words: [dBepalingdatum - margin, dBepalingdatum + margin]
#' 
#' This does mean that it may combine the same weight measurement with multiple
#' height measurements.
#' 
#' Date: 28-6-2024
#' @author Jonne ter Braake
#' @author Ammar Faiq 
#' @author Janet Kist 
#' @author Sukainah Alfaraj
#' @author Lisette de Schipper
#' @param data data table that you want to use
#' @param margin of days to scour for matching weight and height measurements
#' If this is 10, it would look for weights up until 10 days before and
#' 10 days after the height measurement.
#' @returns a data table (see package data.table https://rdatatable.gitlab.io/data.table/)
#' @example
#' data <- get_numeric_bmi(data, 10) (opmerking Janet, ik begrijp niet goed wat dit voorbeeld hier doet, geeft dit de 1e 10 resultaten van bmi?

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("get_numeric_lab_measurements.R")
library(data.table)

# line 31-86 is 1 funtion, after you run the function, the result is a new file with only bmi
get_numeric_bmi <- function(data, margin){

  # height is 560 #these are dWCIA codes, codes which identify lab or measurements in the Netherlands, more information can be found on NHG lab code viewer: https://bepalingen.nhg.org/labcodes/determinations
  # bmi is c(1234, 1272)
  # weight is c(357, 2408)
  
  data <- get_numeric_lab_measurements(data, "values", c(560, 1234, 1272, 357, 2408)) #this selects only the rows with the dWCIAcodes concerning bmi, height and weight
  
  # bmi #NB: hier is een check nodig, BMI staat af en toe in onlogische waarden of negatieve waarden, zou net als bij height aangepast moeten worden.
  bmi <- data[dWCIANummer %in% c(1234, 1272)]  # %in% is een call uit base R? optioneel wellicht overwegen (dWCIANummer == “1234”| dWCIANummer == “1272”) als dat sneller is, nog niet getest. 
  bmi[, bmi := values] #renames column name to bmi

  # height
  heightdata <- data[dWCIANummer == 560]
  heightdata[, height := values] #renames column name to height
  # some clean up
  heightdata[Eenheid == "m",':=' (height = height * 100) (Eenheid = "cm")]
  heightdata[height < 3, height := height * 100]
  heightdata[height > 10000, height := height / 100]
  
  heightdata <- heightdata[, c("RINPERSOONS", "RINPERSOON", "dBepalingdatum", "height"), with = FALSE]
  
  # weight
  weightdata <- data[dWCIANummer %in% c(357, 2408)]
  weightdata[, weight := values]
  weightdata <- weightdata[, c("RINPERSOONS", "RINPERSOON", "dBepalingdatum", "weight"), with = FALSE]
  setnames(weightdata, "dBepalingdatum", "dBepalingdatumw")

  
  # the bmi we calculate (including the margin)
  bmi_calc <- weightdata[
    heightdata[, ':=' (minhdate = dBepalingdatum - margin, 
                       maxhdate = dBepalingdatum + margin)],
    on = .(RINPERSOONS == RINPERSOONS, RINPERSOON == RINPERSOON,
           dBepalingdatumw >= minhdate, dBepalingdatumw <= maxhdate)]
  
  # to generate a fitting dBepalingdatum
  # we pick the date in between the dBepalingdatum of the height
  # and de weight measurements as the dBepalingdatum of the calculated BMI
  bmi_calc[, dBepalingdatum := pmin(dBepalingdatum, dBepalingdatumw) + 
             abs(dBepalingdatum - dBepalingdatumw) ]
  
  bmi_calc <- bmi_calc[!(is.na(weight) | is.na(height))]
  bmi_calc[, bmi := weight / ((height/100)^2) ]
  bmi_calc[, bmi := mean(bmi, na.rm = TRUE), by = c("RINPERSOONS", "RINPERSOON", 
                                                "dBepalingdatum")]
  bmi_calc[, bmi_gen := TRUE]
  
  # now we combine the bmi measurements we got from the lab files with 
  # the bmis we calculated
  bmi <- rbindlist(list(bmi, bmi_calc), fill = TRUE)
  
  bmi <- bmi[!is.na(bmi)]
  bmi <- bmi[, c("RINPERSOONS", "RINPERSOON", "dBepalingdatum", "bmi", "bmi_gen"), with = FALSE]
  return(unique(bmi))
}

#syntax might need to be added
dt_heigth[ , heigth_gp := str_replace_all(heigth_gp, “,”,”.”] # replace Komma’s by dot’s, Dutch system uses a komma in numericals, R needs dots
dt_heigth[ , heigth_gp := ifelse(grepl(“\\/+”, heigth_gp), word(gp_heigth$heigth_gp, 1, sep = fixed”/”)), weight] # replaces leading // 
dt_heigth[ , heigth_gp := gsub(“[^0-9.]”, “”, heigth_gp)] # removes leading empties
Unique(gp_heigth$heigth_gp) # check what is left in unique results, fix when needed before next step
dt_heigth[ , heigth_gp := as.numeric(heigth)] # change string to numerical
dt_heigth = dt_heigth[!is.na(heigth_gp), . (Ids, ID, dBepalingdatum, heigth_gp, Eenheid, dWCIANummer)] # select columns needed for analyses and checks
dt_heigth = dt_heigth[!duplicated(dt_heigth, by = colnames(dt_heigth), fromLast = TRUE] #remove duplicates
uniqueN(dt_heigth$ID)
saveRDS(dt_heigth, file = “filepath/23_gp_heigth.rds”) # save as separate file .rds and .csv are small saving formats
