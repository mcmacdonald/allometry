
# NUTS correspondence tables that provide codes for Istat regions
# https://ec.europa.eu/eurostat/web/nuts/correspondence-tables
# https://gisco-services.ec.europa.eu/tercet/flat-files
readr::guess_encoding("pc2025_IT_NUTS-2024_v1.0.csv", n_max = 100, threshold = 0.2)
nuts <- utils::read.csv(
  "pc2025_IT_NUTS-2024_v1.0.csv", 
  encoding = "ASCII", 
  quote = "'",
  fileEncoding = "ASCII"
  )

# rename columns
colnames(nuts) <- c("code", "nuts")



# NUTS correspondence tables that provide codes for Istat regions
# https://ec.europa.eu/eurostat/web/nuts/correspondence-tables
# https://gisco-services.ec.europa.eu/tercet/flat-files
# https://ec.europa.eu/eurostat/web/nuts/local-administrative-units

# codes for comunes contained in the LAU data file
nuts <- read_excel("EU-27-LAU-2020-NUTS-2021-NUTS-2016.xlsx", sheet = "IT NUTS 2021")
nuts <- nuts[, 1:7]; nuts <- nuts[, -4:-5]

# rename columns
colnames(nuts) <- c("nuts", "code", "comune", "pop", "sqkilo")

# remove diacritical marks i.e., accents
nuts$comune <- stringi::stri_trans_general(nuts$comune, "Latin-ASCII")

# trim the istat codes
nuts$comune <- stringi::stri_replace_all_regex( # replace invalid UTF-8 characters with a placeholder (or simply remove them)
  nuts$comune, 
  "[\x80-\xFF]", 
  "", 
  vectorize_all = FALSE
  )
# convert to UTF-8 after removing or replacing invalid characters
nuts$comune <- stringi::stri_enc_toutf8(nuts$comune)

# trim leading and trailing white spaces
nuts$comune <- stringi::stri_trim_both(nuts$comune)

# drop dash or hyphens
nuts$comune <- stringi::stri_replace_all_regex(nuts$comune, "-", " ")

# lower case
nuts$comune <- stringr::str_to_lower(nuts$comune, locale = "en")


# measure is squared metres, transform into square kilometres
nuts$sqkilo <- nuts$sqkilo/1e-6 



# Istat dashboard
# https://esploradati.istat.it/databrowser/#/en

# crimes reported by the police forces to the judicial authority
readr::guess_encoding("crimes.csv", n_max = 100, threshold = 0.2) # https://stackoverflow.com/questions/65073636/cant-correctly-import-a-csv-with-italian-accented-characters-into-r
crime <- utils::read.csv(
  "crimes.csv", 
  comment.char = "#", 
  quote = "",
  fileEncoding = "UTF-8"
  )
crime[is.na(crime)] <- 0 # recode empty cells = 0

# retain other columns
crime <- dplyr::select(crime, REF_AREA, TIME_PERIOD, Type.of.crime, Observation)

# name columns
colnames(crime) <- c(
  "nuts", # 'REF_AREA' is the NUTS code
  "year", 
  "type",
  "count"
  )

# codes
unique(crime$nuts)
crime <- dplyr::filter(crime, nuts != "IT"); crime <- dplyr::filter(crime, nuts != "")



# join
crime <- merge(nuts, crime, by = "nuts")
# crime <- crime[, -1] # drop NUTS code
rm(nuts)

# retain only mafia-related crime types
crime <- dplyr::filter(
  crime,
  type == "Arson" | 
  type == "Criminal association" | 
  type == "Damage followed by arson" |
  type == "Extortions" |
  type == "Forest arson" |
  type == "Homicides of mafia" |
  type == "Kidnappings" |
  type == "Mafia criminal association" |
  type == "Terrorist homicides" |
  type == "Usury"
  )

# recode strings
crime$type[crime$type == "Criminal association"] <- "mafia association"
crime$type[crime$type == "Mafia criminal association"] <- "mafia association"
crime$type[crime$type == "Homicides of mafia"] <- "mafia homicides"
crime$type[crime$type == "Terrorist homicides"] <- "mafia homicides"
crime$type[crime$type == "Arson"] <- "arson, building"
crime$type[crime$type == "Forest arson"] <- "arson, forest"
crime$type[crime$type == "Damage followed by arson"] <- "arson, vandalism of property"
crime$type[crime$type == "Extortions"] <- "extortion"
crime$type[crime$type == "Kidnappings"] <- "kidnapping"
crime$type[crime$type == "Usury"] <- "usury"

# drop earlier time points
crime <- dplyr::filter(crime, year >= 2017)

# average the number of incidents
crime <- crime %>%
  dplyr::group_by(code, type) %>%
  dplyr::summarise(count = mean(count)) %>%
  dplyr::ungroup()

#
crime <- crime %>%
  tidyr::pivot_wider(
    id_cols = code,
    names_from = type,   # These unique values become the new column headers
    values_from = count  # These values fill the new crime columns
    )

# join
crime$code <- as.numeric(crime$code)

# join
farms1 <- merge(farms1, crime, by = "code", all.x = TRUE)

