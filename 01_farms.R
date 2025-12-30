# ------------------------------------------------------------------------------

# this .r script compiles data on farms across municipalities in Italy


# ------------------------------------------------------------------------------

# set working directory
setwd("C:/Users/mitch/Desktop/Istat/data/")



# information on farms
# https://esploradati.istat.it/databrowser/#/en/dw/categories/IT1,Z1000AGR,1.0/AGR_ECON

# references to remove accents from names
# https://answers.microsoft.com/en-us/msoffice/forum/all/excel-how-to-substitute-language-specific/d2a6d451-ddb2-4440-928b-ca16cbd6965c
# https://support.microsoft.com/en-gb/office/run-a-macro-5e855fd2-02d1-45f5-90a3-50e645fe3155

# Istat official codes for municipalities or comunes
# https://www.istat.it/classificazione/codici-dei-comuni-delle-province-e-delle-regioni/
# https://situas.istat.it/web/#/home/piu-consultati?id=61&dateFrom=2024-10-25

# import Istat codes for municipalities ----------------------------------------
readr::guess_encoding("istatcodes.csv", n_max = 100, threshold = 0.2) # https://stackoverflow.com/questions/65073636/cant-correctly-import-a-csv-with-italian-accented-characters-into-r
istat <- read.csv(
  "istatcodes.csv", 
  comment.char = "#", 
  fileEncoding = "ISO-8859-1"
  )
istat <- istat[, 5:11] # drop other columns 
colnames(istat) <- c(
  "code",       # istat code for municipality
  "comune",     # municipality
  "italiano",   # Italian name
  "lingua",     # common name
  "region_agg", # region indicator groups into regions in the north, central, south, etc.
  "region",     # nominal level that indicates the 20 regions in Italy 
  "province"    # nominal level that incidates the 107 provinces in Italy, nested inside the 20 regions
  )
istat <- istat[, -3:-5] # drop columns

# the first column 'Italiano e strainiera' is the matching column, and is a combination of the columns...
# ... 'italiano' and 'lingua'

  # don't run
  # show that the first column is a combination of the second and third
  # istat$match <- "N"
  # istat$match[istat$comune == istat$italiano] <- "Y"
  # table(istat$match)
  
  # filter and view names
  # nomatch <- dplyr::filter(istat, match == "N"); istat$match <- NULL # drop
  # rm(nomatch)

# remove diacritical marks i.e., accents
istat$comune <- stringi::stri_trans_general(istat$comune, "Latin-ASCII")

# trim the istat codes
istat$comune <- stringi::stri_replace_all_regex( # replace invalid UTF-8 characters with a placeholder (or simply remove them)
  istat$comune, 
  "[\x80-\xFF]", 
  "", 
  vectorize_all = FALSE
  )
# convert to UTF-8 after removing or replacing invalid characters
istat$comune <- stringi::stri_enc_toutf8(istat$comune)

# trim leading and trailing white spaces
istat$comune <- stringi::stri_trim_both(istat$comune)

# drop dash or hyphens
istat$comune <- stringi::stri_replace_all_regex(istat$comune, "-", " ")

# lower case
istat$comune <- stringr::str_to_lower(istat$comune, locale = "en")

# drop duplicates
istat <- unique(istat)

# label columns
Hmisc::label(istat$code) <- "Istat code for comune"
Hmisc::label(istat$comune) <- "name of comune"
Hmisc::label(istat$region) <- "region of Italy"
Hmisc::label(istat$province) <- "province of Italy"



# agricultural census for Italy, 2020 ------------------------------------------
# https://esploradati.istat.it/databrowser/#/en/censimentoagricoltura
# https://esploradati.istat.it/databrowser/#/en/censimentoagricoltura/categories/IT1,Z1100AGR,1.0/CENSAGR/IT1,DF_DCAT_CENSAGRIC2020_SURF_ALL,1.0
# also see: https://www.arc2020.eu/agricultural-census-in-italy/
readr::guess_encoding("farms1.csv", n_max = 100, threshold = 0.2) # https://stackoverflow.com/questions/65073636/cant-correctly-import-a-csv-with-italian-accented-characters-into-r
farms1 <- read.csv(
  "farms1.csv", 
  comment.char = "#", 
  fileEncoding = "ISO-8859-1"
  )
farms1[is.na(farms1)] <- 0 # recode empty cells = 0

# for reference
# 2005 micro-level census data:
# https://www.istat.it/en/microdata/farm-structure-survey-year-2005/

# for reference
# 2010 agricultural census data:
# http://dati-censimentoagricoltura.istat.it/Index.aspx?lang=en&SubSessionId=742bbcd6-afcf-4169-aa84-8b0be19ccdac&themetreeid=-200

# name columns
colnames(farms1) <- c(
  "code",
  "comune",
  "htotal",
  "hutilized",
  "hunutilized",
  "ftotal",
  "futilized",
  "funutilized"
  )

# label columns
Hmisc::label(farms1$comune) <- "name of comune"
Hmisc::label(farms1$code) <- "Istat code for comune"
Hmisc::label(farms1$htotal) <- "total hectares of farmland"
Hmisc::label(farms1$hutilized) <- "hectares of utilized farmland"
Hmisc::label(farms1$hunutilized) <- "hectares of unutilized farmland"
Hmisc::label(farms1$ftotal) <- "total number of farms"
Hmisc::label(farms1$futilized) <- "number of farms with utilized farmland"
Hmisc::label(farms1$funutilized) <- "number of farms with unutilized farmland"

# trim the istat codes
farms1$comune <- stringi::stri_replace_all_regex( # replace invalid UTF-8 characters with a placeholder (or simply remove them)
  farms1$comune, 
  "[\x80-\xFF]", 
  "", 
  vectorize_all = FALSE
  )

# remove diacritical marks i.e., accents
farms1$comune <- stringi::stri_trans_general(farms1$comune, "Latin-ASCII")

# convert to UTF-8 after removing or replacing invalid characters
farms1$comune <- stringi::stri_enc_toutf8(farms1$comune)

# trim leading and trailing white spaces
farms1$comune <- stringi::stri_trim_both(farms1$comune)

# drop dash or hyphens
farms1$comune <- stringi::stri_replace_all_regex(farms1$comune, "-", " ")

# lower case
farms1$comune <- stringr::str_to_lower(farms1$comune, locale = "en")

# recode to numeric
farms1$code <- as.numeric(unlist(stringi::stri_extract_all_regex(farms1$code, "\\d+")))

# drop duplicates
farms1 <- unique(farms1)

# join the istat codes to the farm census data ---------------------------------
farms1 <- merge(istat, farms1, by = c("comune", "code"), all.x = TRUE, all.y = FALSE)
rm(istat)

# sort by numeric code
farms1 <- dplyr::arrange(farms1, code)

# calculate the average size of a farm in each municipality --------------------
geomean <- function(data){
  `%>%` <- magrittr::`%>%`
  data <- data %>%
    # total farmland
    dplyr::mutate(havgtotal = log(htotal/ftotal)) %>% # log total hectares : total number of farms
    dplyr::mutate(havgtotal = exp(havgtotal)) %>%  # exponentiate
    dplyr::mutate(havgtotal = ifelse(is.nan(havgtotal), 0, havgtotal)) %>% # = 0
    dplyr::mutate(havgtotal = ifelse(is.infinite(havgtotal), 0, havgtotal)) %>% # = 0
    dplyr::mutate(havgtotal = ifelse(is.na(havgtotal), 0, havgtotal)) %>% # = 0
    # utilized farmland
    dplyr::mutate(havgutilized = log(hutilized/futilized)) %>% # log total hectares : total number of farms
    dplyr::mutate(havgutilized = exp(havgutilized)) %>%  # exponentiate
    dplyr::mutate(havgutilized = ifelse(is.nan(havgutilized), 0, havgutilized)) %>% # = 0
    dplyr::mutate(havgutilized = ifelse(is.infinite(havgutilized), 0, havgutilized)) %>% # = 0
    dplyr::mutate(havgutilized = ifelse(is.na(havgutilized), 0, havgutilized)) %>% # = 0
    # unutilized farmland
    dplyr::mutate(havgunutilized = log(hunutilized/funutilized)) %>% # log total hectares : total number of farms
    dplyr::mutate(havgunutilized = exp(havgunutilized)) %>%  # exponentiate
    dplyr::mutate(havgunutilized = ifelse(is.nan(havgunutilized), 0, havgunutilized)) %>% # = 0
    dplyr::mutate(havgunutilized = ifelse(is.infinite(havgunutilized), 0, havgunutilized)) %>% # = 0
    dplyr::mutate(havgunutilized = ifelse(is.na(havgunutilized), 0, havgunutilized)) # = 0
  return(data)
}
farms1 <- geomean(farms1)


# label
Hmisc::label(farms1$havgtotal) <- "number of total hectares of the (geometric) average parcel of farmland"
Hmisc::label(farms1$havgutilized) <- "number of utilized hectares of the (geometric) average parcel of farmland"
Hmisc::label(farms1$havgunutilized) <- "number of untilized hectares of the (geometric) average parcel of farmland"



# other types of data for municipalities ---------------------------------------

# source:
# https://situas.istat.it/web/#/home

# total land area
# https://esploradati.istat.it/databrowser/#/en/dw/categories/IT1,Z0930TER,1.0/DCCV_CARGEOMOR_ST_COM
# https://esploradati.istat.it/databrowser/#/en/dw/categories/IT1,Z0930TER,1.0/DCCV_CARGEOMOR_ST_COM/IT1,DCCV_CARGEOMOR_ST_COM,1.0



# geographic characteristics of municipalities ---------------------------------
# https://situas.istat.it/web/#/home/piu-consultati?id=73&dateFrom=2024-10-25
readr::guess_encoding("geo.csv", n_max = 100, threshold = 0.2) # https://stackoverflow.com/questions/65073636/cant-correctly-import-a-csv-with-italian-accented-characters-into-r
geo <- read.csv(
  "geo.csv", 
  comment.char = "#", 
  fileEncoding = "ISO-8859-1"
  )
geo <- geo[, 6:23]; geo <- geo[, -3:-6]; geo <- geo[, 1:6] # drop columns
colnames(geo) <- c( # name columns
  "code",        # Istat code
  "comune",      # municipality
  "altimetric",  # altimetric area
  "altitude",    # altitude
  "coastal",     # coastal areas
  "urbanization" # DEGURBA = degree of urbanization
  )

# label columns
Hmisc::label(geo$code) <- "Istat code for comune"
Hmisc::label(geo$comune) <- "name of comune"
Hmisc::label(geo$altimetric) <- "altimetric area"
Hmisc::label(geo$altitude) <- "altitude"
Hmisc::label(geo$coastal) <- "coastal area"
Hmisc::label(geo$urbanization) <- "degree of urbanization (DEGURBA)"

# trim the istat codes
geo$comune <- stringi::stri_replace_all_regex( # replace invalid UTF-8 characters with a placeholder (or simply remove them)
  geo$comune, 
  "[\x80-\xFF]", 
  "", 
  vectorize_all = FALSE
  )

# remove diacritical marks i.e., accents
geo$comune <- stringi::stri_trans_general(geo$comune, "Latin-ASCII")

# convert to UTF-8 after removing or replacing invalid characters
geo$comune <- stringi::stri_enc_toutf8(geo$comune)

# trim leading and trailing white spaces
geo$comune <- stringi::stri_trim_both(geo$comune)

# drop dash or hyphens
geo$comune <- stringi::stri_replace_all_regex(geo$comune, "-", " ")

# lower case
geo$comune <- stringr::str_to_lower(geo$comune, locale = "en")

# recode to numeric
geo$code <- as.numeric(unlist(stringi::stri_extract_all_regex(geo$code, "\\d+")))

# drop duplicates
geo <- unique(geo)

# join to census data on farmlands
farms1 <- merge(farms1, geo, by = c("code", "comune"), all.x = TRUE, all.y = TRUE)
rm(geo) # drop



# geographic characteristics of municipalities ---------------------------------
# https://situas.istat.it/web/#/home/piu-consultati?id=73&dateFrom=2024-10-25
readr::guess_encoding("geo2.csv", n_max = 100, threshold = 0.2) # https://stackoverflow.com/questions/65073636/cant-correctly-import-a-csv-with-italian-accented-characters-into-r
geo2 <- read.csv(
  "geo2.csv", 
  comment.char = "#", 
  fileEncoding = "ISO-8859-1"
  )
geo2 <- geo2[, 6:14]; geo2 <- geo2[, -3:-6]; geo2 <- geo2[, 1:3] # drop other columns
colnames(geo2) <- c( # name columns
  "code",     # Istat codes
  "comune",   # municipality
  "mountain"  # mountain region
  )

# label columns
Hmisc::label(geo2$code) <- "Istat code for comune"
Hmisc::label(geo2$comune) <- "name of comune"
Hmisc::label(geo2$mountain) <- "moutain region"

# trim the istat codes
geo2$comune <- stringi::stri_replace_all_regex( # replace invalid UTF-8 characters with a placeholder (or simply remove them)
  geo2$comune, 
  "[\x80-\xFF]", 
  "", 
  vectorize_all = FALSE
  )

# remove diacritical marks i.e., accents
geo2$comune <- stringi::stri_trans_general(geo2$comune, "Latin-ASCII")

# convert to UTF-8 after removing or replacing invalid characters
geo2$comune <- stringi::stri_enc_toutf8(geo2$comune)

# trim leading and trailing white spaces
geo2$comune <- stringi::stri_trim_both(geo2$comune)

# drop dash or hyphens
geo2$comune <- stringi::stri_replace_all_regex(geo2$comune, "-", " ")

# lower case
geo2$comune <- stringr::str_to_lower(geo2$comune, locale = "en")

# recode to numeric
geo2$code <- as.numeric(unlist(stringi::stri_extract_all_regex(geo2$code, "\\d+")))

# drop duplicates
geo2 <- unique(geo2)

# join to census data on farmlands
farms1 <- merge(farms1, geo2, by = c("code", "comune"), all.x = TRUE, all.y = FALSE)
rm(geo2) # drop

# 23 municipalities that form part of the Nebrodi mountains
# https://www.hitsicily.com/en/natural-parc-nebrodi-mountains-sicily-rent-houses-villas-holidays/
farms1$nebrodi <- "N"
farms1$nebrodi[farms1$comune == "acquedolci" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "alcara li fusi" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "capizzi" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "caronia" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "cesaro" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "floresta" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "galati mamertino" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "longi" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "militello rosmarino" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "mistretta" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "san fratello" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "san marco d'alunzio" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "san teodoro" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "sant'agata di militello" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "santa domenica vittoria" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "santo stefano di camastra" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "tortorici" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "ucria" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "bronte" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "maniace" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "randazzo" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "cerami" & farms1$region == "Sicilia"] <- "Y"
farms1$nebrodi[farms1$comune == "troina" & farms1$region == "Sicilia"] <- "Y"

# label
Hmisc::label(farms1$nebrodi) <- "municipality is part of Nebrodi National Park"



# population of municipalities -------------------------------------------------
readr::guess_encoding("pop.csv", n_max = 100, threshold = 0.2) # https://stackoverflow.com/questions/65073636/cant-correctly-import-a-csv-with-italian-accented-characters-into-r
pop <- read.csv(
  "pop.csv", 
  comment.char = "#", 
  fileEncoding = "ISO-8859-1"
  )
pop <- pop[, 6:15]; pop <- pop[, -3:-4]; pop <- pop[, -4]; pop <- pop[, -5]; pop <- pop[, -6] # drop columns
colnames(pop) <- c( # rename columns
  "code",   # Istat codes
  "comune", # municipality
  "popleg", # population of legal residents, 2021 
  "sqkilo", # land area - squared kilometers, 2024
  "popres"  # population of all residents, 2022
  )
pop <- dplyr::select( # reorder columns
  pop,
  code,
  comune,
  popleg,
  popres,
  sqkilo
  )
# label columns
Hmisc::label(pop$code) <- "Istat code for comune"
Hmisc::label(pop$comune) <- "name of comune"
Hmisc::label(pop$popleg) <- "population of legal residents, 2021"
Hmisc::label(pop$popres) <- "population of all legal and non-legal residents, 2022"
Hmisc::label(pop$sqkilo) <- "land area of the municipality - squared kilometers, 2024"

# trim the istat codes
pop$comune <- stringi::stri_replace_all_regex( # replace invalid UTF-8 characters with a placeholder (or simply remove them)
  pop$comune, 
  "[\x80-\xFF]", 
  "", 
  vectorize_all = FALSE
  )

# remove diacritical marks i.e., accents
pop$comune <- stringi::stri_trans_general(pop$comune, "Latin-ASCII")

# convert to UTF-8 after removing or replacing invalid characters
pop$comune <- stringi::stri_enc_toutf8(pop$comune)

# trim leading and trailing white spaces
pop$comune <- stringi::stri_trim_both(pop$comune)

# drop dash or hyphens
pop$comune <- stringi::stri_replace_all_regex(pop$comune, "-", " ")

# lower case
pop$comune <- stringr::str_to_lower(pop$comune, locale = "en")

# recode to numeric
pop$code <- as.numeric(unlist(stringi::stri_extract_all_regex(pop$code, "\\d+")))

# drop duplicates
pop <- unique(pop)

# drop commas and change to numeric
pop$sqkilo <- as.numeric(gsub(",", "", pop$sqkilo))

# calculate hectares of the municipality
pop$hectares <- pop$sqkilo * 100 # hectares = square kilometers x 100
Hmisc::label(pop$hectares) <- "land area of the municipality - hectares, 2024" # label column

# drop duplicates
pop <- unique(pop)

# join to census data on farmlands
farms1 <- merge(farms1, pop, by = c("code", "comune"), all.x = TRUE, all.y = FALSE)
rm(pop) # drop\

# calculate the population density of farms with utilized farmland in each municipality ------------------------------
farms1 <- dplyr::mutate(farms1, pd.futilized = futilized/sqkilo) # number of farms per squared kilometer
farms1$pd.futilized[is.infinite(farms1$pd.futilized)] <- 0
Hmisc::label(farms1$pd.futilized) <- "number of farms with utilized farmland per squared kilometer"

# calculate the population density of farms with unutilized farmland in each municipality ------------------------------
farms1 <- dplyr::mutate(farms1, pd.funutilized = funutilized/sqkilo) # number of farms per squared kilometer
farms1$pd.funutilized[is.infinite(farms1$pd.funutilized)] <- 0
Hmisc::label(farms1$pd.funutilized) <- "number of farms with unutilized farmland per squared kilometer"




# heads of livestock counts ----------------------------------------------------
# https://esploradati.istat.it/databrowser/#/en/censimentoagricoltura/categories/IT1,Z1100AGR,1.0/AU/IT1,DF_DCAT_CENSAGRIC2020_AU_CATTLE_1,1.0
readr::guess_encoding("livestock.csv", n_max = 100, threshold = 0.2) # https://stackoverflow.com/questions/65073636/cant-correctly-import-a-csv-with-italian-accented-characters-into-r
livestock <- read.csv(
  "livestock.csv", 
  comment.char = "#", 
  fileEncoding = "UTF-8"
  )
livestock <- livestock[, 1:10] # drop columns
livestock[is.na(livestock)] <- 0 # recode empty cells = 0
colnames(livestock) <- c( # rename columns
  "code",   # Istat codes
  "comune", # municipality
  "dairycows", # dairy cattle
  "cattle", # total head of cattle
  "buffalo",  # water buffalo
  "horses",  # horses
  "sheep", # sheep
  "goats", # goats
  "pigs", # pigs
  "poultry" # chickens
  )

# subtract the number of dairy cows from the total head of cattle
livestock$cattle <- livestock$cattle - livestock$dairycows 

# label columns
Hmisc::label(livestock$code) <- "Istat code for comune"
Hmisc::label(livestock$comune) <- "name of comune"
Hmisc::label(livestock$dairycows) <- "total head of dairy cows"
Hmisc::label(livestock$cattle) <- "total head of non-dairy cattle"
Hmisc::label(livestock$buffalo) <- "total head of water buffalo"
Hmisc::label(livestock$horses) <- "total number of horses on farmland" # not in the wild
Hmisc::label(livestock$sheep) <- "total head of sheep"
Hmisc::label(livestock$goats) <- "total head of goats"
Hmisc::label(livestock$pigs) <- "total number of pigs on farmland" # not in the wild
Hmisc::label(livestock$poultry) <- "total number of poultry" # chickens, turkey, ducks, etc.

# trim the istat codes
livestock$comune <- stringi::stri_replace_all_regex( # replace invalid UTF-8 characters with a placeholder (or simply remove them)
  livestock$comune, 
  "[\x80-\xFF]", 
  "", 
  vectorize_all = FALSE
  )

# remove diacritical marks i.e., accents
livestock$comune <- stringi::stri_trans_general(livestock$comune, "Latin-ASCII")

# convert to UTF-8 after removing or replacing invalid characters
livestock$comune <- stringi::stri_enc_toutf8(livestock$comune)

# trim leading and trailing white spaces
livestock$comune <- stringi::stri_trim_both(livestock$comune)

# drop dash or hyphens
livestock$comune <- stringi::stri_replace_all_regex(livestock$comune, "-", " ")

# lower case
livestock$comune <- stringr::str_to_lower(livestock$comune, locale = "en")

# recode to numeric
livestock$code <- as.numeric(unlist(stringi::stri_extract_all_regex(livestock$code, "\\d+")))

# drop duplicates
livestock <- unique(livestock)

# join to census data on farmlands
farms1 <- merge(farms1, livestock, by = c("code", "comune"), all.x = TRUE, all.y = FALSE)
rm(livestock) # drop






# calculate different measures of the total heads of different types of livestock
`%>%` <- magrittr::`%>%`
farms1 <- farms1 %>%
  # the total heads of different types of livestock
  dplyr::mutate(livestock = dairycows + cattle + sheep + goats + pigs) %>%
  dplyr::mutate(livestock = ifelse(is.nan(livestock), 0, livestock)) %>%
  dplyr::mutate(livestock = ifelse(is.infinite(livestock), 0, livestock)) %>%
  dplyr::mutate(livestock = ifelse(is.na(livestock), 0, livestock)) %>%
  # the average heads of different types of livestock on each farm with utilized farmland
  dplyr::mutate(avg.livestock = log(livestock/futilized)) %>% # log total hectares : total number of farms
  dplyr::mutate(avg.livestock = exp(avg.livestock)) %>%  # exponentiate
  dplyr::mutate(avg.livestock = ifelse(is.nan(avg.livestock), 0, avg.livestock)) %>% # = 0
  dplyr::mutate(avg.livestock = ifelse(is.infinite(avg.livestock), 0, avg.livestock)) %>% # = 0
  dplyr::mutate(avg.livestock = ifelse(is.na(avg.livestock), 0, avg.livestock)) %>% # = 0
  # the total heads of different types of livestock per square kilometer of utilized farmland
  dplyr::mutate(pd.livestock = livestock/(hutilized/100)) %>% # divide the utilized farmland by 100 to compute squared kilometers
  dplyr::mutate(pd.livestock = ifelse(is.nan(pd.livestock), 0, pd.livestock)) %>%
  dplyr::mutate(pd.livestock = ifelse(is.infinite(pd.livestock), 0, pd.livestock)) %>%
  dplyr::mutate(pd.livestock = ifelse(is.na(pd.livestock), 0, pd.livestock))

# labels
Hmisc::label(farms1$livestock) <- "total head of livestock on utilized farmland"
Hmisc::label(farms1$avg.livestock) <- "average head of livestock on farms with utilized farmland"
Hmisc::label(farms1$pd.livestock) <- "total head of livestock per squared kilometer of utilized farmland"



# investments ------------------------------------------------------------------
# https://esploradati.istat.it/databrowser/#/en/censimentoagricoltura/categories/IT1,Z1100AGR,1.0/CENSAGR/IT1,DF_DCAT_CENSAGRIC2020_INV_ALL,1.0
readr::guess_encoding("investments.csv", n_max = 100, threshold = 0.2) # https://stackoverflow.com/questions/65073636/cant-correctly-import-a-csv-with-italian-accented-characters-into-r
investments <- read.csv(
  "investments.csv", 
  comment.char = "#", 
  fileEncoding = "ASCII"
  )
investments <- investments[, 3:17]; investments <- investments[, -2]; investments <- investments[, -6]; investments <- investments[, -7:-13]; investments <- investments[, -3]; investments <- investments[, -4]
colnames(investments) <- c( # rename columns
  "code",   # Istat codes
  "sector", # sector of investment
  "value", # number of farms that had investments
  "code2"
  )

# to numeric
investments$code <- as.numeric(investments$code); investments$code2 <- as.numeric(investments$code2)

# fill any missing values
investments$code[is.na(investments$code)] <- investments$code2[is.na(investments$code)]

# drop column
investments <- investments[, -4]

# recode empty cells = 0
investments$value[is.na(investments$value)] <- 0

# reshape
investments <- stats::reshape(investments, idvar = "code", timevar = "sector", direction = "wide")

# recode empty cells = 0
investments[is.na(investments)] <- 0

colnames(investments) <- c(
  "code",
  "itotal", # total number of farms with investments across all sectors
  "inutrition", # total number of farms with investments for animal nutrition
  "ibuilding", # total number of farms with investments for building structure
  "ilivestock", # total number of farms with investments for housing livestock
  "imechanization", # total number of farms with investments for mechanization of farms i.e., equipment
  "imilking", # milking of dairy cattle
  "iother", # other investments
  "iphytosanitary", # phytosanitary fight is the protection of plants against pests and disease
  "irelated", # related activities
  "isales", # sales and marketing of products
  "ifertilization", # fertilization
  "iirrigation", # irrigation
  "iplanting", # planting and sowing
  "isoiltillage", # soil tillage is the mechanical process of disturbing soil to prepare it for planting and growing crops
  "ibreeding", # varieties, breeds, and clones ... * does this measure include animal husbandry?
  "icarting", # waste management
  "imanagement", # business organization and management
  "iscaffolding" # scaffolding and pruning of arboretums
  )

# investments that increase soil productivity - soil tillage, fertilization, and irrigation
investments <- dplyr::mutate(investments, iproductivity = iirrigation + isoiltillage + ifertilization)

# investments for livestock - housing and nutrition
investments <- dplyr::mutate(investments, ilivestock = ilivestock + inutrition)


# drop columns
investments <- dplyr::select(
  investments,
  code,
  itotal,
  ibuilding,
  ilivestock,
  imechanization,
  iphytosanitary,
  iplanting,
  iproductivity,
  icarting
  )

# label columns
Hmisc::label(investments$code) <- "Istat code for comune"
Hmisc::label(investments$itotal) <- "total number of farms with investments across all sectors"
Hmisc::label(investments$ibuilding) <- "total number of farms with investments for building structure"
Hmisc::label(investments$ilivestock) <- "total number of farms with investments for housing livestock and animal nutrition"
Hmisc::label(investments$imechanization) <- "total number of farms with investments for mechanization of farms"
Hmisc::label(investments$iphytosanitary) <- "total number of farms with investments in phytosanitary"
Hmisc::label(investments$iplanting) <- "total number of farms with investments in planing and sowing"
Hmisc::label(investments$iproductivity) <- "total number of farms with investments in soil tillage, fertilization, and irrigation"
Hmisc::label(investments$icarting) <- "total number of farms with investments in waste management"

# drop duplicates
investments <- unique(investments)

# join to census data on farmlands
farms1 <- merge(farms1, investments, by = c("code"), all.x = TRUE, all.y = FALSE)
rm(investments) # drop

# recode empty cells = 0
farms1[is.na(farms1)] <- 0




# farms with utilized agricultural area by type of crops
# https://esploradati.istat.it/databrowser/#/en/censimentoagricoltura/categories/IT1,Z1100AGR,1.0/CENSAGR/IT1,DF_DCAT_CENSAGRIC2020_CROPS_ALL,1.0
crops <- 
  
  # utilized agricultural area - hectares	
  # farms with utilized agricultural area




  
  
# agricultural units by type of crop
# https://esploradati.istat.it/databrowser/#/en/censimentoagricoltura/categories/IT1,Z1100AGR,1.0/AU/IT1,DF_DCAT_CENSAGRIC2020_UA_CROPS_2,1.0



  
# number of farms by land area
# https://esploradati.istat.it/databrowser/#/en/censimentoagricoltura/categories/IT1,Z1100AGR,1.0/CENSAGR/IT1,DF_DCAT_CENSAGRIC2020_SAU_AZONE_ALL,1.0





  
  
  


# economic size and farm typology
# https://esploradati.istat.it/databrowser/#/en/censimentoagricoltura/categories/IT1,Z1100AGR,1.0/CENSAGR/IT1,DF_DCAT_CENSAGRIC2020_ECSIZE_ALL,1.0
readr::guess_encoding("profits.csv", n_max = 100, threshold = 0.2) # https://stackoverflow.com/questions/65073636/cant-correctly-import-a-csv-with-italian-accented-characters-into-r
profits <- read.csv(
  "profits.csv", 
  comment.char = "#", 
  fileEncoding = "ASCII"
  )
profits <- profits[, 3:19]; profits <- profits[, -2]; profits <- profits[, -6:-16];  profits <- profits[, -4] # drop columns
colnames(profits) <- c(
  "code",   # Istat codes
  "profits", # sector of investment
  "farming", # number of farms that had investments
  "value"
  )
  
# recode type of farm  

  # specialist field crops
  profits$farming[profits$farming == "1"] <- "specialist field crops farms"
  profits$farming[profits$farming == "15"] <- "specialist cereals, oilseeds, and protein crops farms"  
  profits$farming[profits$farming == "151"] <- "specialist cereals (other than rice), oilseeds, and protein crops farms"  
  profits$farming[profits$farming == "152"] <- "specialist rice farms"  
  profits$farming[profits$farming == "153"] <- "cereals, oilseeds, protein crops farms, and rice combined farms"  
  
  # general field crops
  profits$farming[profits$farming == "16"] <- "general field cropping farms"
  profits$farming[profits$farming == "161"] <- "specialist root crop farms"
  profits$farming[profits$farming == "162"] <- "specialist cereals, oilseeds, and protein crops and root crops farms"
  profits$farming[profits$farming == "163"] <- "specialist field vegetables farms"
  profits$farming[profits$farming == "164"] <- "specialist tobacco farms"
  profits$farming[profits$farming == "166"] <- "various field crops combined farms"
  
  # specialist horticultural farms - indoor
  profits$farming[profits$farming == "2"] <- "specialist horticulture farms"
  profits$farming[profits$farming == "21"] <- "specialist horticulture indoor farms"
  profits$farming[profits$farming == "211"] <- "specialist vegetable indoor farms"
  profits$farming[profits$farming == "212"] <- "specialist flowers and ornamentals indoor farms"
  profits$farming[profits$farming == "213"] <- "mixed horticulture indoor specialist farms"
  
  # specialist horticultural farms - outdoor
  profits$farming[profits$farming == "22"] <- "specialist horticulture outdoor farms"
  profits$farming[profits$farming == "221"] <- "specialist vegetables outdoor farms"
  profits$farming[profits$farming == "222"] <- "specialist flowers and ornamentals outdoor farms"
  profits$farming[profits$farming == "223"] <- "mixed horticulture outdoor specialist farms"
  
  # general horticultural farms
  profits$farming[profits$farming == "23"] <- "other horticulture specialist farms"
  profits$farming[profits$farming == "231"] <- "specialist mushrooms farms"
  profits$farming[profits$farming == "232"] <- "specialist nurseries farms"
  profits$farming[profits$farming == "233"] <- "various horticulture specialist farms"
  
  # permant crops
  profits$farming[profits$farming == "3"] <- "specialist permanent crops farms"
  profits$farming[profits$farming == "35"] <- "specialist vineyards farms"
  profits$farming[profits$farming == "351"] <- "specialist quality wine farms"
  profits$farming[profits$farming == "352"] <- "specialist wine other than quality wine farms"
  profits$farming[profits$farming == "353"] <- "specialist table grapes farms"
  profits$farming[profits$farming == "354"] <- "other vineyards farms"
  
  # citrus farms and tropical fruits 
  profits$farming[profits$farming == "36"] <- "specialist fruit and citrus fruit farms"
  profits$farming[profits$farming == "361"] <- "specialist fruit (other than citrus, tropical fruits and nuts) farms"
  profits$farming[profits$farming == "362"] <- "specialist citrus fruit farms"
  profits$farming[profits$farming == "363"] <- "specialist nuts farms"
  profits$farming[profits$farming == "364"] <- "specialist tropical fruits farms"
  profits$farming[profits$farming == "365"] <- "specialist fruit, citrus, tropical fruits and nuts: mixed production farms"
  
  # olive oil plantations
  profits$farming[profits$farming == "37"] <- "specialist olives farms"
  
  # permanent crops
  profits$farming[profits$farming == "38"] <- "various permanent crops farms"
  
  # grazing livestock farms - dairy cows, cattle, sheep, goats, etc.
  profits$farming[profits$farming == "4"] <- "specialist grazing livestock farms"
  profits$farming[profits$farming == "45"] <- "specialist dairying farms"
  profits$farming[profits$farming == "46"] <- "specialist cattle - rearing and fattening farms"
  profits$farming[profits$farming == "47"] <- "cattle - dairying, rearing, and fattening combined farms" 
  profits$farming[profits$farming == "48"] <- "sheep, goats, and other grazing livestock farms" 
  profits$farming[profits$farming == "481"] <- "specialist sheep farms" 
  profits$farming[profits$farming == "482"] <- "sheep and cattle combined farms" 
  profits$farming[profits$farming == "483"] <- "specialist goats farms" 
  profits$farming[profits$farming == "484"] <- "various grazing livestock farms" 
  
  # pig and poultry farms
  profits$farming[profits$farming == "5"] <- "specialist granivores farms"
  profits$farming[profits$farming == "51"] <- "specialist pigs farms"
  profits$farming[profits$farming == "511"] <- "specialist pig rearing farms"
  profits$farming[profits$farming == "512"] <- "specialist pig fattening farms"
  profits$farming[profits$farming == "513"] <- "pig rearing and fattening farms"
  profits$farming[profits$farming == "52"] <- "specialist poultry farms"
  profits$farming[profits$farming == "522"] <- "specialist poultry-meat farms"
  profits$farming[profits$farming == "523"] <- "layers and poultry-meat combined farms"
  profits$farming[profits$farming == "53"] <- "various granivores combined farms" 
  
  # mixed cropping farms
  profits$farming[profits$farming == "6"] <- "mixed cropping farms"
  profits$farming[profits$farming == "61"] <- "mixed cropping farms"
  profits$farming[profits$farming == "611"] <- "horticulture and permanent crops combined farms"
  profits$farming[profits$farming == "612"] <- "field crops and horticulture combined farms"
  profits$farming[profits$farming == "613"] <- "field crops and vineyard combined farms"
  profits$farming[profits$farming == "614"] <- "field crops and permanent crops combined farms"
  profits$farming[profits$farming == "615"] <- "mixed cropping, mainly field crops farms"
  profits$farming[profits$farming == "616"] <- "other mixed cropping farms"
  
  # mixed livestock
  profits$farming[profits$farming == "7"] <- "mixed livestock holding farms"
  profits$farming[profits$farming == "73"] <- "mixed livestock, mainly grazing livestock farms"
  profits$farming[profits$farming == "731"] <- "mixed livestock, mainly grazing dairying farms"
  profits$farming[profits$farming == "732"] <- "mixed livestock, mainly non-dairying grazing livestock farms"
  profits$farming[profits$farming == "74"] <- "mixed livestock, mainly granivores farms"
  profits$farming[profits$farming == "741"] <- "mixe livestock: granivores and dairying combined farms"
  profits$farming[profits$farming == "742"] <- "mixed livestock: granivores and non-dairying grazing livestock"
  
  # mixed crops and livestock
  profits$farming[profits$farming == "8"] <- "mixed crops - livestock farms"
  profits$farming[profits$farming == "83"] <- "field crops - grazing livestock combined farms"
  profits$farming[profits$farming == "831"] <- "field crops combined with dairying farms"
  profits$farming[profits$farming == "832"] <- "dairying combined with field crops farms"
  profits$farming[profits$farming == "833"] <- "field crops combined with non-dairying grazing livestock farms"
  profits$farming[profits$farming == "834"] <- "non-dairying grazing livestock combined with field crops farms"
  profits$farming[profits$farming == "84"] <- "various crops and livestock combined farms"
  profits$farming[profits$farming == "841"] <- "field crops and granivores combined farms"
  profits$farming[profits$farming == "842"] <- "permanent crops and grazing livestock combined farms"
  profits$farming[profits$farming == "843"] <- "apiculture farms"
  profits$farming[profits$farming == "844"] <- "various mixed crops and livestock farms"
  
  # non-classified
  profits$farming[profits$farming == "9"] <- "non-classified farms"
  
  # total number of farms
  profits$farming[profits$farming == "ALL"] <- "total number of farms"  
  
# recode the profit categories of the farms
# profits$profits[profits$profit  == "TOTAL"
profits$profits[profits$profits == "E0"] <- "E0"
profits$profits[profits$profits == "E0_01-1999_99"] <- "E0.01-1999" #  0.01 - 1,999.99 euro
profits$profits[profits$profits == "E2000-3999_99"] <- "E2000-3999" # 2,000.00 - 3,999.99 euro
profits$profits[profits$profit  == "E4000-7999_99"] <- "E4000-7999" # 4,000.00 - 7,999.99 euro
profits$profits[profits$profit  == "E8000-14999_99"]  <- "E8000-14999" # 8,000.00 - 14,999.99 euro
profits$profits[profits$profit  == "E15000-24999_99"] <- "E15000-24999" # 15,000.00 - 24,999.99 euro
profits$profits[profits$profit  == "E25000-49999_99"] <- "E25000-49999" # 25,000.00 - 49,999.99 euro
profits$profits[profits$profit  == "E50000-99999_99"] <- "E50000-99999" # 50,000.00 - 99,999.99 euro
profits$profits[profits$profit  == "E100000-249999_99"] <- "E100000-249999" # 100,000.00 - 249,999.99 euro

# drop
profits <- profits %>% dplyr::filter(profits != "TOTAL") %>%
  # don't run
  # dplyr::filter(farming == "ALL") # 
  dplyr::filter( # retain farms that for citrus fruit (lemons, limes, etc.) and olive farms
  farming == "specialist citrus fruit farms" | 
    farming ==  "specialist tropical fruits farms" | 
    farming == "specialist olives farms") %>%
  dplyr::select(-farming) # drop column

# sum the number of citrus farms and olive oil plantations
profits <- profits %>% dplyr::group_by(code, profits) %>%
  dplyr::summarise(value = sum(value))

# to numeric
profits$code <- as.numeric(profits$code)

# recode empty cells = 0
profits$value[is.na(profits$value)] <- 0

# reshape
profits <- stats::reshape(profits, idvar = "code", timevar = "profits", direction = "wide")

# recode empty cells = 0
profits$value[is.na(profits$value)] <- 0


