# Project       : Clean vehicle ownership in Spain
# Creation date : 08/04/2024
# Last update   : 08/11/2024
# Author        : Mercè Amich (merce.amich@ehu.eus)
# Institution   : Euskal Herriko Unibertsitatea / Universidad del País Vasco
# Last run time : ~86h

# [1] Objective

# Download ECEPOV-21 data & process it
# Descriptive statistics and frequency tables
# Error-Component Mixed Logit model (Apollo)
# Post-estimation: benchmark individual, plots

# [2] Preliminaries ----

# Clear workspace
rm(list = ls(all = TRUE))

# Set language
Sys.setenv(LANG = "en")

# Set start time
start.time = Sys.time()

# Install and load any required R-package
packages.loaded <- installed.packages()
packages.needed <- c ("dplyr"       ,
                      "openxlsx"    ,
                      "httr"        ,
                      "downloader"  ,
                      "stringr"     ,
                      "here"        ,
                      "tidyr"       ,
                      "DataExplorer",
                      "descstat"    ,
                      "summarytools",
                      "tibble"      ,
                      "weights"     ,
                      "TAM"         ,
                      "apollo"      , 
                      "rsample"     , 
                      "ggplot2"     , 
                      "dplyr"       , 
                      "MASS"        , 
                      "forplo"      , 
                      "patchwork"   ,
                      "forcats"
)

for (p in packages.needed) {
  if (!p %in% row.names(packages.loaded)) install.packages(p)
  eval(bquote(library(.(p))))
}

# Define main path
path <- here()

# Set working directory
setwd(paste0(path))





################################################################################
####### DOWNLOAD DATA & PROCESS IT #############################################
################################################################################

# [3] Download survey from the Spanish Statistical Office ----

# Define structural part of the url
base.url = 'https://www.ine.es/ftp/microdatos/ecepov/'

# Delete the folder generated in previous runs (if it exists) 
if(dir.exists(paste0(path, "/DATA/"))){unlink(paste0(path, "/DATA/"), recursive = TRUE)}

# Create raw data folder 
dir.create(paste0(path, "/DATA/"))

# Define zip file
eval(parse(text = paste0("file = 'datos_2021.zip'")))

# Define url
url <- paste0(base.url, file)

# Set the destination file
destination <- paste0(file)

# Set working directory
setwd(paste0(path, "/DATA/"))

# Download microdata
download(url, destination,  mode = 'wb')

# Unzip microdata
unzip(destination)

# Delete unzipped folder
unlink(destination, recursive = TRUE)

# Create list of folders in the directory
list.folders = list.files(pattern     = '.zip', 
                          ignore.case = TRUE)

# By folder
for (f in 1:length(list.folders)) {
  
  # Define parameters
  folder = list.folders[f]
  
  # Unzip microdata
  unzip(folder)
  
  # Delete useless folder(s)
  unlink(folder, recursive = TRUE)
}

# Function to load and rename datasets
load_and_rename <- function(filename, new_name) {
  load(paste0(path, "/DATA/R/", filename))
  assign(new_name, Microdatos, 
         envir = .GlobalEnv)
  remove(list  = c("Metadatos", "Microdatos"))
}

# Load and rename datasets
load_and_rename("ECEPOVvivienda_2021.RData", "dataV")
load_and_rename("ECEPOVhogar_2021.RData"   , "dataH")
load_and_rename("ECEPOVadultos_2021.RData" , "dataA")

# [4] Create intermediate files ----
## [4.1] From "dataH" ("hogar") to "mergeFileH" ####

# Create new dummy variables and select final variables of interest

# "NACIM" · 1: "Individual born outside from Spain" 
dataH$NACIM         <- ifelse(dataH$NACIM == 2, 1, 0)

# HOUSEHOLD TYPE (uniperson, monoparen, dosadsinhij, dosadconhij, otrostiphogar)
dataH$UNIPERSON     <- ifelse(dataH$TIPOHOGAR == 1, 1, 0)
dataH$MONOPAREN     <- ifelse(dataH$TIPOHOGAR == 2, 1, 0)
dataH$DOSADSINHIJ   <- ifelse(dataH$TIPOHOGAR == 3, 1, 0)
dataH$DOSADCONHIJ   <- ifelse(dataH$TIPOHOGAR == 4, 1, 0)
dataH$OTROSTIPOSHOG <- ifelse(dataH$TIPOHOGAR %in% c(5, 6, 7), 1, 0)

# HH EMPLOYMENT STATUS
dataH$TODOSTRABAJAN  <- ifelse(dataH$SITLABHOGAR == 4, 1, 0)
dataH$NINGUNOTRABAJA <- ifelse(dataH$SITLABHOGAR == 1, 1, 0)
dataH$OTRASSITLAB    <- ifelse(dataH$SITLABHOGAR %in% c(2, 3, 5, 6, 7), 1, 0)

# HH HIGH EDUCATION
dataH$NINGUNOHOGEESS <- ifelse(dataH$NIVEDUCAHOGAR == 1, 1, 0)
dataH$ALGUNOHOGEESS  <- ifelse(dataH$NIVEDUCAHOGAR == 2, 1, 0)
dataH$TODOSHOGEESS   <- ifelse(dataH$NIVEDUCAHOGAR == 3, 1, 0)

# MALE PROPORTION >18y / HH
dataH$MALE           <- ifelse(dataH$SEXO == 1, 1, 0)


### Create intermediate "mergeFileH"
### Select only variables of interest

mergeFileH <- dataH %>%
  dplyr::select(
    IDEN, FACTOR, 
    # select numerical variables
    HIJOS_NUCLEO_MENORES, EDAD,
    # select newly created dummy variables
    NACIM:MALE
  )

### Collapse / Group observations by "IDEN"
### "mergeFileH" will contain the 36 vars of interest + has the size of dataV

mergeFileH <- mergeFileH %>%
  
  # Transform NA's to 0
  dplyr::mutate(HIJOS_NUCLEO_MENORES = replace_na(HIJOS_NUCLEO_MENORES, 0)) %>%
  
  # Group by "IDEN"
  group_by(IDEN) %>%
  
  # Indications on how to collapse variables
  dplyr::summarize(
    
    # Deliver value of the first observation (repeated in all IDEN)
    FACTOR               = first(FACTOR),
    HIJOS_NUCLEO_MENORES = first(HIJOS_NUCLEO_MENORES),
    
    # Deliver 1 if any of the dummies is 1
    NACIM             = ifelse(any(NACIM             == 1), 1, 0),
    
    UNIPERSON         = ifelse(any(UNIPERSON         == 1), 1, 0),
    DOSADSINHIJ       = ifelse(any(DOSADSINHIJ       == 1), 1, 0),
    DOSADCONHIJ       = ifelse(any(DOSADCONHIJ       == 1), 1, 0),
    MONOPAREN         = ifelse(any(MONOPAREN         == 1), 1, 0),
    OTROSTIPOSHOG     = ifelse(any(OTROSTIPOSHOG     == 1), 1, 0),
    
    TODOSTRABAJAN     = ifelse(any(TODOSTRABAJAN     == 1), 1, 0),
    NINGUNOTRABAJA    = ifelse(any(NINGUNOTRABAJA    == 1), 1, 0),
    OTRASSITLAB       = ifelse(any(OTRASSITLAB       == 1), 1, 0),
    
    NINGUNOHOGEESS    = ifelse(any(NINGUNOHOGEESS    == 1), 1, 0),
    ALGUNOHOGEESS     = ifelse(any(ALGUNOHOGEESS     == 1), 1, 0),
    TODOSHOGEESS      = ifelse(any(TODOSHOGEESS      == 1), 1, 0),
    
    #Build new variable PROPFEM18 (%female per HH =>18y)
    # "ifelse" used to avoid NA's for those HH with no male: 0/0 = NA
    PROPMALE18        = ifelse(sum(EDAD >= 18) == 0, 0, 
                               sum(MALE == 1 & EDAD >= 18) / sum(EDAD >= 18)),
    
    #Build new variable MEANAGE18 (mean age >=18y)
    # IDEN(=008233, 058919, 117255, 119309) were unipersonal HH <18y
    # Not to have NA's introduced, "ifelse" to put 0 instead
    MEANAGE18         = ifelse(sum(EDAD >= 18) == 0, 0, mean(EDAD[EDAD >= 18]))
    
  )


## [4.2] From "dataA" ("adultos") to "mergeFileA" ####

# Recode EC (Estado Civil)
dataA$SOLTERO  <- ifelse(dataA$EC == 1, 1, 0)
dataA$CASADO   <- ifelse(dataA$EC %in% c(2, 3), 1, 0)
dataA$VIDUO    <- ifelse(dataA$EC == 4, 1, 0)
dataA$SEPARADO <- ifelse(dataA$EC == 5, 1, 0)
dataA$DIVORCI  <- ifelse(dataA$EC == 6, 1, 0)

# Recode LUGTRAB
dataA$MIDOMICILIO <- ifelse(dataA$LUGTRAB == 1, 1, 0)
dataA$MIMUNICIPIO <- ifelse(dataA$LUGTRAB == 3, 1, 0)
dataA$MIPROVINCIA <- ifelse(dataA$LUGTRAB == 4, 1, 0)
dataA$OTROSSITIOS <- ifelse(dataA$LUGTRAB %in% c(2, 5, 6), 1, 0)

# Recode TIEMDESPLA
dataA$MENOSDE20MIN  <- ifelse(dataA$TIEMDESPLA == 1, 1, 0)
dataA$ENTRE20Y39MIN <- ifelse(dataA$TIEMDESPLA == 2, 1, 0)
dataA$ENTRE40Y59MIN <- ifelse(dataA$TIEMDESPLA == 3, 1, 0)
dataA$MASDE60       <- ifelse(dataA$TIEMDESPLA %in% c(4, 5, 6, 7), 1, 0)
dataA$ENTRE60Y89MIN <- ifelse(dataA$TIEMDESPLA == 4, 1, 0)
dataA$MASDE90       <- ifelse(dataA$TIEMDESPLA %in% c(5, 6, 7), 1, 0)

# Transform NDESPLA (handling NAs)
dataA$NDESPLA <- as.numeric(dataA$NDESPLA)
dataA$NDESPLA[is.na(dataA$NDESPLA)] <- 0

# Recode MEDIO TRANSPORTE_1
dataA$MTRANSPOR_1 <- as.numeric(dataA$MTRANSPOR_1)
dataA$MTRANSPOR_1[is.na(dataA$MTRANSPOR_1)] <- 0

dataA$PMT_COCHEPART <- ifelse(dataA$MTRANSPOR_1 == 1, 1, 0)
dataA$PMT_TPUBLICO  <- ifelse(dataA$MTRANSPOR_1 %in% c(11, 13, 14, 16), 1, 0)
dataA$PMT_MOTO      <- ifelse(dataA$MTRANSPOR_1 == 7, 1, 0)
dataA$PMT_OTROS     <- ifelse(dataA$MTRANSPOR_1 %in% c(0, 2:6, 8:10, 12, 15, 17), 1, 0)

# Recode "personas dependientes"
dataA$TIPODEPDH <- as.numeric(dataA$TIPODEPDH)

dataA$ENFECRONIC <- ifelse(dataA$TIPODEPDH == 1, 1, 0)
dataA$DISCAP     <- ifelse(dataA$TIPODEPDH == 2, 1, 0)
dataA$MAYOR70DEP <- ifelse(dataA$TIPODEPDH == 4, 1, 0)


# Create intermediate "mergeFileA"
# Select only variables of interest

mergeFileA <- dataA %>%
  dplyr::select(
    IDEN, FACTOR,
    # select the numerical variables
    NDESPLA,
    # select the newly created dummy variables
    SOLTERO:MAYOR70DEP,
    # remove the transformed variables
    -SITLAB, -ESTUDIOS, -EC, -LUGTRAB, -MTRANSPOR_1, -MTRANSPOR_2, -TIEMDESPLA  
  )

## Collapse / Group observations by "IDEN"
## Now "mergeFileA" contains the 30 vars of interest + has the size of dataV

mergeFileA <- mergeFileA %>%
  
  # group by "IDEN"
  dplyr::group_by(IDEN) %>%
  
  # indicate how to collapse each variable
  dplyr::summarize(
    
    # Deliver value of the first observation (repeated in all IDEN)
    FACTOR               = first(FACTOR),
    
    # Sum numerical variables
    NDESPLA = ifelse(is.na(max(NDESPLA)) || max(NDESPLA) == 0, 0, 
                     max(NDESPLA)),
    
    # Recode EC (Estado Civil)
    SOLTERO  = ifelse(any(SOLTERO          == 1), 1, 0),
    CASADO   = ifelse(any(CASADO           == 1), 1, 0),
    VIDUO    = ifelse(any(VIDUO            == 1), 1, 0),
    SEPARADO = ifelse(any(SEPARADO         == 1), 1, 0),
    DIVORCI  = ifelse(any(DIVORCI          == 1), 1, 0),
    
    # Recode LUGTRAB
    MIDOMICILIO      = ifelse(any(MIDOMICILIO      == 1), 1, 0),
    MIMUNICIPIO      = ifelse(any(MIMUNICIPIO      == 1), 1, 0),
    MIPROVINCIA      = ifelse(any(MIPROVINCIA      == 1), 1, 0),
    OTROSSITIOS      = ifelse(any(OTROSSITIOS      == 1), 1, 0),
    
    # Recode TIEMPDESPLA
    MENOSDE20MIN     = ifelse(any(MENOSDE20MIN     == 1), 1, 0),
    ENTRE20Y39MIN    = ifelse(any(ENTRE20Y39MIN    == 1), 1, 0),
    ENTRE40Y59MIN    = ifelse(any(ENTRE40Y59MIN    == 1), 1, 0),
    MASDE60          = ifelse(any(MASDE60          == 1), 1, 0),
    ENTRE60Y89MIN    = ifelse(any(ENTRE60Y89MIN    == 1), 1, 0),
    MASDE90          = ifelse(any(MASDE90          == 1), 1, 0),
    
    # Recode MTRANSPOR_1
    PMT_COCHEPART    = ifelse(any(PMT_COCHEPART    == 1), 1, 0),
    PMT_TPUBLICO     = ifelse(any(PMT_TPUBLICO     == 1), 1, 0),
    PMT_MOTO         = ifelse(any(PMT_MOTO         == 1), 1, 0),
    PMT_OTROS        = ifelse(any(PMT_OTROS        == 1), 1, 0),
    
    # Recode TIPODEPH
    ENFECRONIC       = ifelse(any(ENFECRONIC       == 1), 1, 0), 
    DISCAP           = ifelse(any(DISCAP           == 1), 1, 0),
    MAYOR70DEP       = ifelse(any(MAYOR70DEP       == 1), 1, 0)
  )


## [4.3] Merge "mergeFileH" and "mergeFileA" with "dataV" ----

data <- left_join(dataV, mergeFileH, by = c("IDEN", "FACTOR"))
data <- left_join(data,  mergeFileA, by = c("IDEN", "FACTOR"))

# The resulting dataframe has 172444 observations and 121 variables

# Create final "data.Rdata" object
save(data, file = "data.Rdata")

remove("dataA", "dataH", "dataV", "mergeFileA", "mergeFileH")

# [5] Create new variables ----

# Clean-Car Ownership 
# 1: No car / 2: Fuel car  / 3: Clean car

# 1. Optimize CARTYPE assignment
data$CARTYPE <- NA  # Initialize with NA
data$CARTYPE[data$VEHICULO == 6] <- 1
data$CARTYPE[data$VEHICULO == 1 & (data$VEHIELECTR != 1 | data$VEHIHIBRI != 1)] <- 2
data$CARTYPE[data$VEHIELECTR == 1 | data$VEHIHIBRI == 1] <- 3

# 2. Optimize TYPEOF assignment
data$TYPEOF <- NA  # Initialize with NA
data$TYPEOF[data$VEHICULO == 6] <- 1
data$TYPEOF[data$VEHICULO == 1 & (data$VEHIELECTR != 1 | data$VEHIHIBRI != 1)] <- 2
data$TYPEOF[data$VEHIELECTR == 1] <- 3
data$TYPEOF[data$VEHIHIBRI == 1] <- 4

# 3. Set NVEHICULOS missing values and create OTHERCARS
data$NVEHICULOS[is.na(data$NVEHICULOS)] <- 0
data$OTHERCARS <- ifelse(data$NVEHICULOS > 1, 1, 0)

# 4. Housing Tenure variables
data$PROPIEDAD <- ifelse(data$REGVI %in% 1:3, 1, 0)
data$ALQUILER <- ifelse(data$REGVI == 4, 1, 0)
data$OTROSREGVI <- ifelse(data$REGVI %in% 5:6, 1, 0)

# 5. Simplified Regional code assignment using matrix assignment (for multiple regions)
region_codes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", 
                  "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
                  "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", 
                  "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", 
                  "49", "50", "51", "52")
region_names <- c("ALAVA", "ALBACETE", "ALACANT", "ALMERIA", "AVILA", "BADAJOZ", "BALEARS", 
                  "BARCELONA", "BURGOS", "CACERES", "CADIZ", "CASTELLO", "CIUDADREAL", 
                  "CORDOBA", "ACORUNA", "CUENCA", "GIRONA", "GRANADA", "GUADALAJARA", 
                  "GIPUZKOA", "HUELVA", "HUESCA", "JAEN", "LEON", "LLEIDA", "LARIOJA", 
                  "LUGO", "MADRID", "MALAGA", "MURCIA", "NAVARRA", "OURENSE", "ASTURIAS", 
                  "PALENCIA", "LASPALMAS", "PONTEVEDRA", "SALAMANCA", "SCTENERIFE", 
                  "CANTABRIA", "SEGOVIA", "SEVILLA", "SORIA", "TARRAGONA", "TERUEL", 
                  "TOLEDO", "VALENCIA", "VALLADOLID", "BIZKAIA", "ZAMORA", "ZARAGOZA", 
                  "CEUTA", "MELILLA")

for (i in seq_along(region_codes)) {
  data[[region_names[i]]] <- ifelse(data$IDQ_PV == region_codes[i], 1, 0)
}

# 6. Assign OTRASPROV based on exclusion
data$OTRASPROV <- ifelse(data$IDQ_PV != "08" & data$IDQ_PV != "28", 1, 0)

# 7. Municipality-based calculations
data$MENOS50MILHAB <- ifelse(data$IDQ_MUN == "00000", 1, 0)

# 8. Capital cities (5 largest)
capital_codes <- c("08019", "28079", "46250", "41091", "50297")
capital_names <- c("c_barcelona", "c_madrid", "c_valencia", "c_sevilla", "c_zaragoza")

for (i in seq_along(capital_codes)) {
  data[[capital_names[i]]] <- ifelse(data$IDQ_MUN == capital_codes[i], 1, 0)
}

# 9. Municipality size
data$MENOS50MILHAB <- ifelse(data$TAM_MUNI == 1, 1, 0)
data$DE50A100MIL <- ifelse(data$TAM_MUNI == 2, 1, 0)
data$DE100A500MIL <- ifelse(data$TAM_MUNI == 3, 1, 0)
data$MASDE500MIL <- ifelse(data$TAM_MUNI == 4, 1, 0)

# 10. Income brackets
data$INGREHOG <- as.numeric(data$INGREHOG)
data$MENOSDEMIL <- ifelse(data$INGREHOG %in% 1:2, 1, 0)
data$DEMILADOSMIL <- ifelse(data$INGREHOG %in% 3:4, 1, 0)
data$DEDOSATRESMIL <- ifelse(data$INGREHOG %in% 5:6, 1, 0)
data$DETRESACINCOMIL <- ifelse(data$INGREHOG == 7, 1, 0)
data$MASDECINCOMIL <- ifelse(data$INGREHOG %in% 8:9, 1, 0)

# 11. Recycling behavior
recycling_items <- c("PAPEL", "VIDRIO", "ENVASES", "ORGANICO")
for (item in recycling_items) {
  data[[item]] <- ifelse(data[[item]] == 1, 1, 0)
}

data$RECYCLE <- rowSums(data[, recycling_items], na.rm = TRUE)

# 12. Services in area
services_items <- c("COLEGIO", "CSALUD", "SUPER", "FARMACIA", "BARES")
for (item in services_items) {
  data[[item]] <- ifelse(data[[item]] == 1, 1, 0)
}

data$SERVICES <- rowSums(data[, services_items], na.rm = TRUE)

# 13. Perception of bad communication
data$MALCOMUNIC <- ifelse(data$MALCOMUNIC == 1, 1, 0)

# 14. Vacation home ownership, location, and days used
data$SEGUNRESI <- ifelse(data$SEGUNRESI == 1, 1, 0)
segun_resi_vars <- c("LUGSEGUNRESI", "DIASUSA")
segun_resi_labels <- c("SEGUNRESIMISMOMUNI", "SEGUNRESIOTROMUNI", "SEGUNROTRAPROVINMISMACCAA", "SEGUNRESIOTRACCAA", "SEGUNRESIEXTRANJERO", "SEGUNRESIMENOS15", "SEGUNRESI15A29", "SEGUNRESIENTRE30Y59", "SEGUNRESIMAS60D")

# Handle the vacation home conditions
for (i in seq_along(segun_resi_labels)) {
  data[[segun_resi_labels[i]]] <- ifelse(data[[segun_resi_vars[[i %/% 5 + 1]]]] == (i %% 5 + 1), 1, 0)
}

# 15. Garage ownership
data$GARAJE <- ifelse(data$GARAJE == 1, 1, 0)

# 16. Renewable energy use
data$ENERENOV <- ifelse(data$ENERENOV == 1, 1, 0)

# 17. Building type
data$UNIFAMILIAR <- ifelse(data$TIPOEDIF == 1, 1, 0)
data$MULTIFAMILIAR <- ifelse(data$TIPOEDIF == 2, 1, 0)

save(data, file = "data.Rdata")

# [6] Rename vars in english ----

data <- data %>%
  rename(
    
    # Family type
    hh_foreign           = NACIM,
    hh_oneperson         = UNIPERSON,
    hh_singleparent      = MONOPAREN,
    hh_twoadultsandchild = DOSADCONHIJ,
    hh_twoadultsalone    = DOSADSINHIJ,
    hh_num_members       = NRESI,
    hh_num_minors        = HIJOS_NUCLEO_MENORES,
    
    # Socio-dems household
    hh_propmale18        = PROPMALE18,
    hh_meanage18         = MEANAGE18,
    hh_some_higheduc     = ALGUNOHOGEESS,
    hh_all_higheduc      = TODOSHOGEESS,
    
    # Income
    inc_1_to_2_thous     = DEMILADOSMIL,
    inc_2_to_3_thous     = DEDOSATRESMIL,
    inc_3_to_5_thous     = DETRESACINCOMIL,
    inc_more_5_thous     = MASDECINCOMIL,
    
    # Working status
    ws_all_work  = TODOSTRABAJAN,
    ws_none_work = NINGUNOTRABAJA,
    
    # Working place
    wp_ownhome    = MIDOMICILIO,
    wp_myprovince = MIPROVINCIA,
    wp_otherplace = OTROSSITIOS,
    
    # Commutement
    com_20_to_39_min = ENTRE20Y39MIN,
    com_40_to_59_min = ENTRE40Y59MIN,
    com_more_1_hour  = MASDE60,
    com_num_trips    = NDESPLA,
    
    # Housing location
    geo_50_100_thous   = DE50A100MIL,
    geo_100_500_thous  = DE100A500MIL,
    geo_more_500_thous = MASDE500MIL,
    
    geo_madrid         = MADRID,
    geo_barcelona      = BARCELONA,
    
    geo_c_madrid       = c_madrid,
    geo_c_barcelona    = c_barcelona,
    geo_c_valencia     = c_valencia,
    geo_c_zaragoza     = c_zaragoza,
    geo_c_sevilla      = c_sevilla,
    
    geo_services       = SERVICES,
    
    # Environmental attitude
    ea_recycle      = RECYCLE,
    ea_renew_energy = ENERENOV,
    
    # Housing
    h_secondhome = SEGUNRESI,
    h_ownership  = PROPIEDAD,
    h_rental     = ALQUILER,
    h_park_slot  = GARAJE,
    h_detached   = UNIFAMILIAR,
    
    # Mobility
    mob_other_vehi    = OTHERCARS,
    mob_pmt_priv_car  = PMT_COCHEPART,
    mob_pmt_pub_trans = PMT_TPUBLICO,
    mob_pmt_moto      = PMT_MOTO,
    
    # Dependent people at hh
    dep_enfecronic    = ENFECRONIC,
    dep_disability    = DISCAP,
    dep_mayor70y      = MAYOR70DEP,
    
    # Malcomunicado subjetivo
    mob_malcomunicado = MALCOMUNIC,
    
    # dependent:
    vehitype          = CARTYPE
    
  )

save(data, file = "data.Rdata")



################################################################################
####### TABLES #################################################################
################################################################################

## [7.1] "Table 2: Vehicle ownership" ----
### a) "No vehicle", "Yes vehicle": Type of vehicles (4 types) ----

table2_a <- data %>%
  dplyr::mutate(
    VEHICLETYPE  =  case_when(
      VEHIELECTR == 1  ~ "Electric",
      VEHIHIBRI  == 1  ~ "Hybrid",
      VEHICULO   == 1 & (VEHIELECTR != 1 & VEHIHIBRI != 1) ~ "Fuel",
      TRUE ~ "No car"  
    )
  ) %>%
  dplyr::group_by(VEHICLETYPE) %>%
  dplyr::summarize(
    count  = n(),            
    weight = sum(FACTOR)
  ) %>%
  mutate(
    prevalence         = (count / sum(count))   * 100,
    prevalence_percent = (weight / sum(weight)) * 100  
  ) %>%
  filter(VEHICLETYPE != "No car") %>%
  print()

### b) "No vehicle", "Yes vehicle": Type of vehicles (3 types) ----
table2_b <- data %>%
  dplyr::mutate(
    CARTYPE      =  case_when(
      VEHICULO   == 6 ~ "No car",  
      VEHICULO   == 1 & (VEHIELECTR != 1 & VEHIHIBRI != 1) ~ "Fuel car", 
      VEHIELECTR == 1 | VEHIHIBRI == 1 ~ "Clean car"  
    )
  ) %>%
  dplyr::group_by(CARTYPE) %>%
  dplyr::summarize(
    count  = n(),            
    weight = sum(FACTOR)
  ) %>%
  mutate(
    prevalence         = (count / sum(count))   * 100,
    prevalence_percent = (weight / sum(weight)) * 100  
  ) %>%
  arrange(match(CARTYPE, c("No car", "Fuel car", "Clean car"))) %>%
  print()



## [7.2] "Table B.1. Numerical variables: descriptive statistics"
## [7.2] "Table B.1. Numerical variables: descriptive statistics" ----
### a) Socio-demographic ----
tableb1_socdem <- data %>%
  summarise(
    variable = c("hh_num_members", "hh_num_minors", "hh_propmale18", "hh_meanage18"),
    weighted_mean = c(
      weighted_mean(hh_num_members, w = FACTOR),
      weighted_mean(hh_num_minors,  w = FACTOR),
      weighted_mean(hh_propmale18,  w = FACTOR),
      weighted_mean(hh_meanage18,   w = FACTOR)
    ),
    weighted_median = c(
      weighted_quantile(hh_num_members, w = FACTOR, probs = 0.5),
      weighted_quantile(hh_num_minors,  w = FACTOR, probs = 0.5),
      weighted_quantile(hh_propmale18,  w = FACTOR, probs = 0.5),
      weighted_quantile(hh_meanage18,   w = FACTOR, probs = 0.5)
    ),
    weighted_sd = c(
      weighted_sd(hh_num_members, w = FACTOR),
      weighted_sd(hh_num_minors,  w = FACTOR),
      weighted_sd(hh_propmale18,  w = FACTOR),
      weighted_sd(hh_meanage18,   w = FACTOR)
    ),
    weighted_min = c(
      weighted_quantile(hh_num_members, w = FACTOR, probs = 0),
      weighted_quantile(hh_num_minors,  w = FACTOR, probs = 0),
      weighted_quantile(hh_propmale18,  w = FACTOR, probs = 0),
      weighted_quantile(hh_meanage18,   w = FACTOR, probs = 0)
    ),
    weighted_max = c(
      weighted_quantile(hh_num_members, w = FACTOR, probs = 1),
      weighted_quantile(hh_num_minors,  w = FACTOR, probs = 1),
      weighted_quantile(hh_propmale18,  w = FACTOR, probs = 1),
      weighted_quantile(hh_meanage18,   w = FACTOR, probs = 1)
    )
  ) %>%
  tibble() %>%
  print()


### b) Mobility-related ----
tableb1_mob <- data %>%
  dplyr::filter(!is.na(com_num_trips)) %>%
  dplyr::summarise(
    variable        = "com_num_trips",
    weighted_mean   = weighted_mean(com_num_trips,     w = FACTOR),
    weighted_median = weighted_quantile(com_num_trips, w = FACTOR, probs = 0.5),
    weighted_sd     = weighted_sd(com_num_trips,       w = FACTOR),
    weighted_min    = weighted_quantile(com_num_trips, w = FACTOR, probs = 0),
    weighted_max    = weighted_quantile(com_num_trips, w = FACTOR, probs = 1)
  ) %>%
  tibble() %>%
  print()

## [7.3] "Table C.1. Categorical variables: frequency distributions" ----
### a) Socio-demographic ----
tablec1_socdem <- data %>%
  summarise(
    across(
      c(hh_foreign, 
        hh_oneperson, 
        hh_singleparent, 
        hh_twoadultsalone, 
        hh_twoadultsandchild, 
        hh_some_higheduc, 
        hh_all_higheduc, 
        inc_1_to_2_thous, 
        inc_2_to_3_thous, 
        inc_3_to_5_thous, 
        inc_more_5_thous, 
        ws_all_work, 
        ws_none_work
      ),
      ~ weighted.mean(.x, 
                      w = FACTOR, 
                      na.rm = TRUE),
      .names = "proportion_of_1_{.col}"
    )
  ) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "variable",
    values_to = "proportion_of_1"
  ) %>%
  mutate(variable = sub("proportion_of_1_", "", variable)) %>%
  print()





### b) Mobility-related ----
tablec1_mob <- data %>%
  summarise(
    across(
      c(wp_ownhome, 
        wp_myprovince, 
        wp_otherplace, 
        com_20_to_39_min, 
        com_40_to_59_min, 
        com_more_1_hour, 
        geo_50_100_thous, 
        geo_100_500_thous, 
        geo_more_500_thous, 
        geo_barcelona, 
        geo_madrid, 
        mob_other_vehi, 
        mob_pmt_priv_car, 
        mob_pmt_pub_trans, 
        mob_pmt_moto
      ),
      ~ weighted.mean(.x, 
                      w     = FACTOR, 
                      na.rm = TRUE),
      .names = "proportion_of_1_{.col}"
    )
  ) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "variable",
    values_to = "proportion_of_1"
  ) %>%
  mutate(variable = sub("proportion_of_1_", "", variable)) %>%
  print()


### c) Services ----
tablec1_services <- data %>%
  dplyr::group_by(geo_services) %>%
  dplyr::summarise(weighted_count   = sum(FACTOR)) %>%
  dplyr::mutate(weighted_percentage = (weighted_count / sum(weighted_count)) * 100)
  
### d) Dwelling-related ----
tablec1_dwell <- data %>%
  summarise(
    across(
      c(h_secondhome, 
        h_ownership, 
        h_rental, 
        h_park_slot, 
        h_detached),
      ~ weighted.mean(.x, 
                      w     = FACTOR, 
                      na.rm = TRUE),
      .names = "proportion_of_1_{.col}"
    )
  ) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "variable",
    values_to = "proportion_of_1"
  ) %>%
  mutate(variable = sub("proportion_of_1_", "", variable)) %>%
  print()



################################################################################
####### ERROR-COMPONENT MIXED LOGIT MODEL (APOLLO) #############################
################################################################################

# [8] Set Apollo controls ----
apollo_control = list(
  modelName  = "EC",
  modelDescr = "Weighted_EC_vehitype",
  indivID    = "IDEN",
  weights    = "FACTOR",
  nCores     = 10
)

# [9] Load data ----

# Read data
load("data.Rdata")
set.seed(123)
database  <- data
remove(list = "data")

# Divide FACTOR/100 in order for the Log-Likelihood algorithm to converge
database$FACTOR <- database$FACTOR/100

# [10] Define model parameters ----

#ASC_2 + Betas of Alt:2 [baseline: Fuel Car] are fixed to zero

# [11] Define model parameters (initial values) ----
apollo_beta = c(
  asc_1                    =  -0.067885,
  asc_2                    =  0.0,
  asc_3                    =  -4.144465,
  
  b1_hh_foreign            =  1.007025,
  b2_hh_foreign            =  0.0,
  b3_hh_foreign            =  0.144441,
  
  b1_hh_oneperson           =  0.518905,
  b2_hh_oneperson           =  0.0,
  b3_hh_oneperson           =  0.074264,
  
  b1_hh_singleparent        =  -0.039404,
  b2_hh_singleparent        =  0.0,
  b3_hh_singleparent        =  0.022402,
  
  b1_hh_twoadultsalone      =  -0.790167,
  b2_hh_twoadultsalone      =  0.0,
  b3_hh_twoadultsalone      =  0.152213,
  
  b1_hh_twoadultsandchild   =  -1.241126,
  b2_hh_twoadultsandchild   =  0.0,
  b3_hh_twoadultsandchild   =  -0.086202,
  
  b1_hh_num_members         = -0.192716,
  b2_hh_num_members         = 0.0,
  b3_hh_num_members         = -0.038092,
  
  b1_hh_num_minors          = 0.066684,
  b2_hh_num_minors          = 0.0,
  b3_hh_num_minors          = 0.186956,
  
  b1_hh_propmale18          = 0.0,
  b2_hh_propmale18          = 0.0,
  b3_hh_propmale18          = 0.0,
  
  b1_hh_meanage18           = 0.0,
  b2_hh_meanage18           = 0.0,
  b3_hh_meanage18           = 0.0,
  
  b1_hh_some_higheduc       =  -0.468874,
  b2_hh_some_higheduc       =  0.0,
  b3_hh_some_higheduc       =  0.526702,
  
  b1_hh_all_higheduc        =  -0.562453,
  b2_hh_all_higheduc        =  0.0,
  b3_hh_all_higheduc        =  0.713541,
  
  b1_inc_1_to_2_thous       =  -0.360102,
  b2_inc_1_to_2_thous       =  0.0,
  b3_inc_1_to_2_thous       =  -0.052324,
  
  b1_inc_2_to_3_thous       =  -0.980756,
  b2_inc_2_to_3_thous       =  0.0,
  b3_inc_2_to_3_thous       =  0.303296,
  
  b1_inc_3_to_5_thous       =  -1.104291,
  b2_inc_3_to_5_thous       =  0.0,
  b3_inc_3_to_5_thous       =  0.748509,
  
  b1_inc_more_5_thous       =  -0.656055,
  b2_inc_more_5_thous       =  0.0,
  b3_inc_more_5_thous       =  1.158671,
  
  b1_ws_all_work            =  -0.511006,
  b2_ws_all_work            =  0.0,
  b3_ws_all_work            =  0.072424,
  
  b1_ws_none_work           =  0.275080,
  b2_ws_none_work           =  0.0,
  b3_ws_none_work           =  -0.712666,
  
  b1_wp_ownhome             =  -0.083514,
  b2_wp_ownhome             =  0.0,
  b3_wp_ownhome             =  0.362673,
  
  b1_wp_myprovince          =  -0.467923,
  b2_wp_myprovince          =  0.0,
  b3_wp_myprovince          =  0.235941,
  
  b1_wp_otherplace          =  -0.590603,
  b2_wp_otherplace          =  0.0,
  b3_wp_otherplace          =  -0.083879,
  
  b1_com_20_to_39_min       =  -0.189068,
  b2_com_20_to_39_min       =  0.0,
  b3_com_20_to_39_min       =  -0.083177,
  
  b1_com_40_to_59_min       =  0.077159,
  b2_com_40_to_59_min       =  0.0,
  b3_com_40_to_59_min       =  0.003872,
  
  b1_com_more_1_hour        =  0.077159,
  b2_com_more_1_hour        =  0.0,
  b3_com_more_1_hour        =  0.003872,
  
  b1_com_num_trips          = -0.155225,
  b2_com_num_trips          = 0.0,
  b3_com_num_trips          = -0.003803,
  
  b1_geo_50_100_thous       = 0.176347,
  b2_geo_50_100_thous       = 0.0,
  b3_geo_50_100_thous       = 0.227539,
  
  b1_geo_100_500_thous      = 0.516286,
  b2_geo_100_500_thous      = 0.0,
  b3_geo_100_500_thous      = 0.179566,
  
  b1_geo_more_500_thous     = 1.138612,
  b2_geo_more_500_thous     = 0.0,
  b3_geo_more_500_thous     = 0.168840,
  
  b1_geo_barcelona          = 0.450697,
  b2_geo_barcelona          = 0.0,
  b3_geo_barcelona          = 0.133745,
  
  b1_geo_madrid             = 0.0,
  b2_geo_madrid             = 0.0,
  b3_geo_madrid             = 0.0,
  
  b1_geo_services           = 0.0,
  b2_geo_services           = 0.0,
  b3_geo_services           = 0.0,
  
  b1_h_secondhome           = 0.0,
  b2_h_secondhome           = 0.0,
  b3_h_secondhome           = 0.0,
  
  b1_h_ownership           = 0.0,
  b2_h_ownership           = 0.0,
  b3_h_ownership           = 0.0,
  
  b1_h_rental              = 0.0,
  b2_h_rental              = 0.0,
  b3_h_rental              = 0.0,
  
  b1_h_park_slot             = 0.0,
  b2_h_park_slot             = 0.0,
  b3_h_park_slot             = 0.0,
  
  b1_h_detached            = 0.0, 
  b2_h_detached            = 0.0, 
  b3_h_detached            = 0.0,
  
  #b1: No
  b2_mob_other_vehi         = 0.0,
  b3_mob_other_vehi         = 0.0,
  
  #b1: No
  b2_mob_pmt_priv_car      = 0.0,
  b3_mob_pmt_priv_car      = 0.0,
  
  b1_mob_pmt_pub_trans     = 0.0,
  b2_mob_pmt_pub_trans     = 0.0,
  b3_mob_pmt_pub_trans     = 0.0,
  
  #b1: No
  b2_mob_pmt_moto       = 0.0,
  b3_mob_pmt_moto       = 0.0,
  
  # ERROR TERMS
  sd_2                = 0.1,
  sd_3                = 0.2
  
)

## Apollo fixed (alt2) ####
### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta
### Use apollo_beta_fixed = c() if none

apollo_fixed = c("asc_2",
                 
                 "b2_hh_foreign",
                 
                 "b2_hh_oneperson",
                 "b2_hh_singleparent",
                 "b2_hh_twoadultsalone",
                 "b2_hh_twoadultsandchild",
                 "b2_hh_num_members",
                 "b2_hh_num_minors",
                 
                 "b2_hh_propmale18",
                 "b2_hh_meanage18",
                 "b2_hh_some_higheduc",
                 "b2_hh_all_higheduc",
                 
                 "b2_inc_1_to_2_thous",
                 "b2_inc_2_to_3_thous",
                 "b2_inc_3_to_5_thous",
                 "b2_inc_more_5_thous",
                 
                 "b2_ws_all_work",
                 "b2_ws_none_work",
                 
                 "b2_wp_ownhome",
                 "b2_wp_myprovince",
                 "b2_wp_otherplace",
                 
                 "b2_com_20_to_39_min",
                 "b2_com_40_to_59_min",
                 "b2_com_more_1_hour",
                 "b2_com_num_trips",
                 
                 "b2_geo_50_100_thous",
                 "b2_geo_100_500_thous",
                 "b2_geo_more_500_thous",
                 
                 "b2_geo_barcelona",
                 "b2_geo_madrid",
                 
                 "b2_geo_services",
                 
                 "b2_h_secondhome",
                 "b2_h_ownership",
                 "b2_h_rental",
                 "b2_h_park_slot",
                 "b2_h_detached",
                 
                 "b2_mob_other_vehi",
                 "b2_mob_pmt_priv_car",
                 "b2_mob_pmt_pub_trans",
                 "b2_mob_pmt_moto"
                 
) 

## Define random components ----

### Set parameters for generating draws (5000 sobol draws)
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 5000,
  interUnifDraws = c(),
  interNormDraws = c("draws_2",
                     "draws_3")
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["r_2"]] =     ( sd_2 * draws_2)
  randcoeff[["r_3"]] =     ( sd_3 * draws_3 )
  return(randcoeff)
}


# [12] Validate data ----

apollo_inputs = apollo_validateInputs()

# [13] Define Apollo probabilities ####

apollo_probabilities=function(apollo_beta,
                              apollo_inputs,
                              functionality = "estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### UTILITY EQUATIONS *************************************
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  #### V[["alt1"]] ####
  V[["alt1"]]  = asc_1                             +
    b1_hh_foreign           * hh_foreign           +
    b1_hh_oneperson         * hh_oneperson         +
    b1_hh_singleparent      * hh_singleparent      +
    b1_hh_twoadultsalone    * hh_twoadultsalone    +
    b1_hh_twoadultsandchild * hh_twoadultsandchild +
    b1_hh_num_members       * hh_num_members       +
    b1_hh_num_minors        * hh_num_minors        +
    
    b1_hh_propmale18        * hh_propmale18        +
    b1_hh_meanage18         * hh_meanage18         +
    b1_hh_some_higheduc     * hh_some_higheduc     +
    b1_hh_all_higheduc      * hh_all_higheduc      +
    
    b1_inc_1_to_2_thous     * inc_1_to_2_thous     +
    b1_inc_2_to_3_thous     * inc_2_to_3_thous     +
    b1_inc_3_to_5_thous     * inc_3_to_5_thous     +
    b1_inc_more_5_thous     * inc_more_5_thous     +
    
    b1_ws_all_work          * ws_all_work          +
    b1_ws_none_work         * ws_none_work         +
    
    b1_wp_ownhome           * wp_ownhome           +
    b1_wp_myprovince        * wp_myprovince        +
    b1_wp_otherplace        * wp_otherplace        +
    
    b1_com_20_to_39_min     * com_20_to_39_min     +
    b1_com_40_to_59_min     * com_40_to_59_min     +
    b1_com_more_1_hour      * com_more_1_hour      +
    b1_com_num_trips        * com_num_trips        +
    
    b1_geo_50_100_thous     * geo_50_100_thous     +
    b1_geo_100_500_thous    * geo_100_500_thous    +
    b1_geo_more_500_thous   * geo_more_500_thous   +
    
    b1_geo_barcelona        * geo_barcelona        +
    b1_geo_madrid           * geo_madrid           + 
    
    b1_geo_services         * geo_services         +
    
    b1_h_secondhome         * h_secondhome         +
    b1_h_ownership          * h_ownership          +
    b1_h_rental             * h_rental             +
    b1_h_park_slot          * h_park_slot          +
    b1_h_detached           * h_detached           +
    
    # b1_mob_other_vehi     * mob_other_vehi       + # Not in this alternative
    # b1_mob_pmt_priv_car   * mob_pmt_priv_car     + # Not in this alternative
    b1_mob_pmt_pub_trans    * mob_pmt_pub_trans    
    # b1_mob_pmt_moto       * mob_pmt_moto           # Not in this alternative
  
  
  #### V[["alt2"]] ####
  V[["alt2"]]  = asc_2                             +
    b2_hh_foreign           * hh_foreign           +
    b2_hh_oneperson         * hh_oneperson         +
    b2_hh_singleparent      * hh_singleparent      +
    b2_hh_twoadultsalone    * hh_twoadultsalone    +
    b2_hh_twoadultsandchild * hh_twoadultsandchild +
    b2_hh_num_members       * hh_num_members       +
    b2_hh_num_minors        * hh_num_minors        +
    
    b2_hh_propmale18        * hh_propmale18        +
    b2_hh_meanage18         * hh_meanage18         +
    b2_hh_some_higheduc     * hh_some_higheduc     +
    b2_hh_all_higheduc      * hh_all_higheduc      +
    
    b2_inc_1_to_2_thous     * inc_1_to_2_thous     +
    b2_inc_2_to_3_thous     * inc_2_to_3_thous     +
    b2_inc_3_to_5_thous     * inc_3_to_5_thous     +
    b2_inc_more_5_thous     * inc_more_5_thous     +
    
    b2_ws_all_work          * ws_all_work          +
    b2_ws_none_work         * ws_none_work         +
    
    b2_wp_ownhome           * wp_ownhome           +
    b2_wp_myprovince        * wp_myprovince        +
    b2_wp_otherplace        * wp_otherplace        +
    
    b2_com_20_to_39_min     * com_20_to_39_min     +
    b2_com_40_to_59_min     * com_40_to_59_min     +
    b2_com_more_1_hour      * com_more_1_hour      +
    b2_com_num_trips        * com_num_trips        +
    
    b2_geo_50_100_thous     * geo_50_100_thous     +
    b2_geo_100_500_thous    * geo_100_500_thous    +
    b2_geo_more_500_thous   * geo_more_500_thous   +
    
    b2_geo_barcelona        * geo_barcelona        +
    b2_geo_madrid           * geo_madrid           + 
    
    b2_geo_services         * geo_services         +
    
    b2_h_secondhome         * h_secondhome         +
    b2_h_ownership          * h_ownership          +
    b2_h_rental             * h_rental             +
    b2_h_park_slot          * h_park_slot          +
    b2_h_detached           * h_detached           +
    
    b2_mob_other_vehi       * mob_other_vehi       +
    b2_mob_pmt_priv_car     * mob_pmt_priv_car     +
    b2_mob_pmt_pub_trans    * mob_pmt_pub_trans    +
    b2_mob_pmt_moto         * mob_pmt_moto         +
    
    r_2 
  
  #### V[["alt3"]] ####
  V[["alt3"]]  = asc_3                             +
    b3_hh_foreign           * hh_foreign           +
    b3_hh_oneperson         * hh_oneperson         +
    b3_hh_singleparent      * hh_singleparent      +
    b3_hh_twoadultsalone    * hh_twoadultsalone    +
    b3_hh_twoadultsandchild * hh_twoadultsandchild +
    b3_hh_num_members       * hh_num_members       +
    b3_hh_num_minors        * hh_num_minors        +
    
    b3_hh_propmale18        * hh_propmale18        +
    b3_hh_meanage18         * hh_meanage18         +
    b3_hh_some_higheduc     * hh_some_higheduc     +
    b3_hh_all_higheduc      * hh_all_higheduc      +
    
    b3_inc_1_to_2_thous     * inc_1_to_2_thous     +
    b3_inc_2_to_3_thous     * inc_2_to_3_thous     +
    b3_inc_3_to_5_thous     * inc_3_to_5_thous     +
    b3_inc_more_5_thous     * inc_more_5_thous     +
    
    b3_ws_all_work          * ws_all_work          +
    b3_ws_none_work         * ws_none_work         +
    
    b3_wp_ownhome           * wp_ownhome           +
    b3_wp_myprovince        * wp_myprovince        +
    b3_wp_otherplace        * wp_otherplace        +
    
    b3_com_20_to_39_min     * com_20_to_39_min     +
    b3_com_40_to_59_min     * com_40_to_59_min     +
    b3_com_more_1_hour      * com_more_1_hour      +
    b3_com_num_trips        * com_num_trips        +
    
    b3_geo_50_100_thous     * geo_50_100_thous     +
    b3_geo_100_500_thous    * geo_100_500_thous    +
    b3_geo_more_500_thous   * geo_more_500_thous   +
    
    b3_geo_barcelona        * geo_barcelona        +
    b3_geo_madrid           * geo_madrid           + 
    
    b3_geo_services         * geo_services         +
    
    b3_h_secondhome         * h_secondhome         +
    b3_h_ownership          * h_ownership          +
    b3_h_rental             * h_rental             +
    b3_h_park_slot          * h_park_slot          +
    b3_h_detached           * h_detached           +
    
    b3_mob_other_vehi       * mob_other_vehi         +
    b3_mob_pmt_priv_car     * mob_pmt_priv_car       +
    b3_mob_pmt_pub_trans    * mob_pmt_pub_trans      +
    b3_mob_pmt_moto         * mob_pmt_moto           +
    
    r_2 + r_3
  
  
  ### Define settings for MNL model component ####
  mnl_settings = list(
    alternatives  = c(alt1 = 1, 
                      alt2 = 2,
                      alt3 = 3), 
    avail         = 1, 
    choiceVar     = vehitype,
    V             = V)
  
  # *************************************************************************************
  
  
  ### Create list of probabilities P
  P = list()
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Account for the weights
  P = apollo_weighting(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# [14] Estimate the model ----

model = apollo_estimate(apollo_beta, 
                        apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs, 
                        estimate_settings=list(
                          maxIterations=500))     
# [15] Model output (obtain & save for Table A.1) ----

apollo_modelOutput(model,
                   modelOutput_settings = list(
                     printClassical  = TRUE, # std. errors & t-ratios + robust
                     printDataReport = TRUE # summary choices
                   ))


## P-values
summary(model, pTwoSided = FALSE) # p-values 1 sided
summary(model, pTwoSided = TRUE)  # p-values 2 sided

## Display eigenvalues (discard saddle points)
print(model$hessianEigenValue)

## Display rho2_0 & rho2_C & adjRho2_0 & adjRho2_C
print(model$rho2_0)
print(model$rho2_C)
print(model$adjRho2_0)
print(model$adjRho2_C)

## Save output
apollo_saveOutput(model,
                  saveOutput_settings = list(
                    printClassical  = TRUE, # std. errors
                    printCorr       = TRUE, # param corr matrix
                    printDataReport = TRUE, # summary choices
                    printPVal       = 2   , # p-values 2 sided
                    printpVal       = 1   , # p-values 1 sided
                    saveEst         = TRUE  # save estimation
                  ))




################################################################################
####### POST-ESTIMATION   ######################################################
################################################################################

# [16] Load model and data ----
# Clear workspace
rm(list = ls(all = TRUE))

# Define main path
path <- here()

# Set working directory
setwd(paste0(path))

# Load Apollo model output
model <- readRDS(file.path(path, "DATA", "EC_model.rds"))

# Load data and assign it to `database` directly
load(file.path(path, "DATA", "data.Rdata"))
database <- data; rm(data)


# [17] Extract "betas" from the model ----
betas <- as.matrix(t(model$estimate[!grepl("^b2", 
                                           names(model$estimate)) 
                                    & names(model$estimate) != "asc_2"]))

# [18] Create "benchmark household" data.frame ----
individual <- data.frame(
  
  hh_foreign              = 1,     
  
  hh_oneperson            = 0,     
  hh_singleparent         = 0,       
  hh_twoadultsalone       = 1,
  hh_twoadultsandchild    = 0,       
  
  hh_num_members          = 2, 
  hh_num_minors           = 0,                 
  hh_propmale18           = median(database$hh_propmale18),      
  hh_meanage18            = median(database$hh_meanage18),
  
  hh_some_higheduc         = 0,
  hh_all_higheduc          = 1,
  
  inc_1_to_2_thous        = 0,
  inc_2_to_3_thous        = 0,
  inc_3_to_5_thous        = 0,
  inc_more_5_thous        = 1,
  
  ws_all_work             = 0,
  ws_none_work            = 0,
  
  wp_ownhome              = 0,
  wp_myprovince           = 0,
  wp_otherplace           = 1,
  
  com_20_to_39_min        = 1,
  com_40_to_59_min        = 0,
  com_more_1_hour         = 0,
  
  com_num_trips           = median(database$com_num_trips),   
  
  geo_50_100_thous        = 1,
  geo_100_500_thous       = 0,
  geo_more_500_thous      = 0,
  
  geo_barcelona           = 0,
  geo_madrid              = 0,
  
  geo_services            = median(database$geo_services),
  
  h_secondhome            = 1,
  
  h_ownership             = 1,
  h_rental                = 0,
  
  h_park_slot             = 1,
  h_detached              = 1,
  
  mob_other_vehi         = 1,
  
  mob_pmt_priv_car        = 1,
  mob_pmt_pub_trans       = 0,
  mob_pmt_moto            = 0
  
)

save(individual, file = "individual.RData")
# [19] Compute probabilities alt1/alt2/alt3 for "benchmark" individual ----

# Function to calculate probabilities
calculate_probabilities <- function(betas, individual) {
  
  # Initialize matrix to store probabilities
  benchmark <- matrix(NA,
                      nrow = nrow(betas), 
                      ncol = 3)
  
  # Define the attributes used in the model
  attributes <- names(individual)
  
  # Loop through each set of simulated betas
  for (i in 1:nrow(betas)) {
    
    # Calculate utility for alt1 ####
    V_alt1 <- betas[i, "asc_1"         ] +
      betas[i,"b1_hh_foreign"          ] * individual$hh_foreign           +
      betas[i,"b1_hh_oneperson"        ] * individual$hh_oneperson         +
      betas[i,"b1_hh_singleparent"     ] * individual$hh_singleparent      +
      betas[i,"b1_hh_twoadultsalone"   ] * individual$hh_twoadultsalone    +
      betas[i,"b1_hh_twoadultsandchild"] * individual$hh_twoadultsandchild +
      betas[i,"b1_hh_num_members"      ] * individual$hh_num_members       +
      betas[i,"b1_hh_num_minors"       ] * individual$hh_num_minors        +
      betas[i,"b1_hh_propmale18"       ] * individual$hh_propmale18        +
      betas[i,"b1_hh_meanage18"        ] * individual$hh_meanage18         +
      betas[i,"b1_hh_some_higheduc"    ] * individual$hh_some_higheduc     +
      betas[i,"b1_hh_all_higheduc"     ] * individual$hh_all_higheduc      +
      betas[i,"b1_inc_1_to_2_thous"    ] * individual$inc_1_to_2_thous     +
      betas[i,"b1_inc_2_to_3_thous"    ] * individual$inc_2_to_3_thous     +
      betas[i,"b1_inc_3_to_5_thous"    ] * individual$inc_3_to_5_thous     +
      betas[i,"b1_inc_more_5_thous"    ] * individual$inc_more_5_thous     +
      betas[i,"b1_ws_all_work"         ] * individual$ws_all_work          +
      betas[i,"b1_ws_none_work"        ] * individual$ws_none_work         +
      betas[i,"b1_wp_ownhome"          ] * individual$wp_ownhome           +
      betas[i,"b1_wp_myprovince"       ] * individual$wp_myprovince        +
      betas[i,"b1_wp_otherplace"       ] * individual$wp_otherplace        +
      betas[i,"b1_com_20_to_39_min"    ] * individual$com_20_to_39_min     +
      betas[i,"b1_com_40_to_59_min"    ] * individual$com_40_to_59_min     +
      betas[i,"b1_com_more_1_hour"     ] * individual$com_more_1_hour      +
      betas[i,"b1_com_num_trips"       ] * individual$com_num_trips        +
      betas[i,"b1_geo_50_100_thous"    ] * individual$geo_50_100_thous     +
      betas[i,"b1_geo_100_500_thous"   ] * individual$geo_100_500_thous    +
      betas[i,"b1_geo_more_500_thous"  ] * individual$geo_more_500_thous   +
      betas[i,"b1_geo_barcelona"       ] * individual$geo_barcelona        +
      betas[i,"b1_geo_madrid"          ] * individual$geo_madrid           +
      betas[i,"b1_geo_services"        ] * individual$geo_services         +
      betas[i,"b1_h_secondhome"        ] * individual$h_secondhome         +
      betas[i,"b1_h_ownership"         ] * individual$h_ownership          +
      betas[i,"b1_h_rental"            ] * individual$h_rental             +
      betas[i,"b1_h_park_slot"         ] * individual$h_park_slot          +
      betas[i,"b1_h_detached"          ] * individual$h_detached           +
      betas[i,"b1_mob_pmt_pub_trans"   ] * individual$mob_pmt_pub_trans    
    
    # Calculate utility for alt2 ####
    V_alt2 <- rep(0, i)
    
    # Calculate utility for alt3 ####
    V_alt3 <- betas[i, "asc_3"] +
      betas[i, "b3_hh_foreign"          ] * individual$hh_foreign           +
      betas[i, "b3_hh_oneperson"        ] * individual$hh_oneperson         +
      betas[i, "b3_hh_singleparent"     ] * individual$hh_singleparent      +
      betas[i, "b3_hh_twoadultsalone"   ] * individual$hh_twoadultsalone    +
      betas[i, "b3_hh_twoadultsandchild"] * individual$hh_twoadultsandchild +
      betas[i, "b3_hh_num_members"      ] * individual$hh_num_members       +
      betas[i, "b3_hh_num_minors"       ] * individual$hh_num_minors        +
      betas[i, "b3_hh_propmale18"       ] * individual$hh_propmale18        +
      betas[i, "b3_hh_meanage18"        ] * individual$hh_meanage18         +
      betas[i, "b3_hh_some_higheduc"    ] * individual$hh_some_higheduc      +
      betas[i, "b3_hh_all_higheduc"     ] * individual$hh_all_higheduc      +
      betas[i, "b3_inc_1_to_2_thous"    ] * individual$inc_1_to_2_thous     +
      betas[i, "b3_inc_2_to_3_thous"    ] * individual$inc_2_to_3_thous     +
      betas[i, "b3_inc_3_to_5_thous"    ] * individual$inc_3_to_5_thous     +
      betas[i, "b3_inc_more_5_thous"    ] * individual$inc_more_5_thous     +
      betas[i, "b3_ws_all_work"         ] * individual$ws_all_work          +
      betas[i, "b3_ws_none_work"        ] * individual$ws_none_work         +
      betas[i, "b3_wp_ownhome"          ] * individual$wp_ownhome           +
      betas[i, "b3_wp_myprovince"       ] * individual$wp_myprovince        +
      betas[i, "b3_wp_otherplace"       ] * individual$wp_otherplace        +
      betas[i, "b3_com_20_to_39_min"    ] * individual$com_20_to_39_min     +
      betas[i, "b3_com_40_to_59_min"    ] * individual$com_40_to_59_min     +
      betas[i, "b3_com_more_1_hour"     ] * individual$com_more_1_hour      +
      betas[i, "b3_com_num_trips"       ] * individual$com_num_trips        +
      betas[i, "b3_geo_50_100_thous"    ] * individual$geo_50_100_thous     +
      betas[i, "b3_geo_100_500_thous"   ] * individual$geo_100_500_thous    +
      betas[i, "b3_geo_more_500_thous"  ] * individual$geo_more_500_thous   +
      betas[i, "b3_geo_barcelona"       ] * individual$geo_barcelona        +
      betas[i, "b3_geo_madrid"          ] * individual$geo_madrid           +
      betas[i, "b3_geo_services"        ] * individual$geo_services         +
      betas[i, "b3_h_secondhome"        ] * individual$h_secondhome         +
      betas[i, "b3_h_ownership"         ] * individual$h_ownership          +
      betas[i, "b3_h_rental"            ] * individual$h_rental             +
      betas[i, "b3_h_park_slot"         ] * individual$h_park_slot          +
      betas[i, "b3_h_detached"          ] * individual$h_detached           +
      betas[i, "b3_mob_other_vehi"      ] * individual$mob_other_vehi       +
      betas[i, "b3_mob_pmt_priv_car"    ] * individual$mob_pmt_priv_car     +
      betas[i, "b3_mob_pmt_pub_trans"   ] * individual$mob_pmt_pub_trans    +
      betas[i, "b3_mob_pmt_moto"        ] * individual$mob_pmt_moto    
    
    # Compute exponentiation of utilities
    exp_V_alt1 <- exp(V_alt1)
    exp_V_alt3 <- exp(V_alt3)
    
    # Compute denominator of the choice probability formula
    denominator <- exp_V_alt1 + 1 + exp_V_alt3
    
    # Compute choice probabilities
    prob_alt1 <- exp_V_alt1 / denominator
    prob_alt2 <- 1          / denominator
    prob_alt3 <- exp_V_alt3 / denominator
    
    # Store probabilities in the matrix
    benchmark[i, ] <- c(prob_alt1, prob_alt2, prob_alt3)
  }
  
  # Return the matrix of probabilities benchmark
  return(benchmark)
}

# Compute probabilities & mean values for each alternative
benchmark <- as.data.frame(calculate_probabilities(betas, 
                                                   individual))
means     <- colMeans(benchmark, 
                      na.rm = TRUE)

# Replace the original columns and transpose them
benchmark <- data.frame(t(means))

# Rename row and column names
rownames(benchmark) <- "benchmark"
colnames(benchmark) <- c("alt1", "alt2", "alt3")


# [20] Make changes in attributes and recompute probabilities ----

## [1 to 0] hh_foreign ####

# Reset individual
load("individual.RData")

# Manually set changes
individual$hh_foreign <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
changes <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] hh_foreign", "[1 to 0] hh_foreign", "[1 to 0] hh_foreign") 
)

## Subgroup: Type of family [benchmark set to hh_twoadultsalone] ####

### [0 to 1] hh_oneperson ####

# Reset individual
load("individual.RData")

# Manually set changes
individual$hh_oneperson <- 1

# Adjust intra-group vars
individual$hh_twoadultsalone <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] hh_oneperson", "[0 to 1] hh_oneperson", "[0 to 1] hh_oneperson") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [0 to 1] hh_singleparent ####   

# Reset individual
load("individual.RData")

# Manually set changes
individual$hh_singleparent <- 1

# Adjust intra-group variables
individual$hh_twoadultsalone <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] hh_singleparent", "[0 to 1] hh_singleparent", "[0 to 1] hh_singleparent") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [1 to 0] hh_twoadultsalone ####  

# Reset individual
load("individual.RData")

# Manually set changes
individual$hh_twoadultsalone <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] hh_twoadultsalone", "[1 to 0] hh_twoadultsalone", "[1 to 0] hh_twoadultsalone") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [1 to 0] hh_twoadultsandchild ####            

# Reset individual
load("individual.RData")

# Manually set changes
individual$hh_twoadultsandchild <- 1

# Adjust intra-group variables
individual$hh_twoadultsalone <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] hh_twoadultsandchild", "[0 to 1] hh_twoadultsandchild", "[0 to 1] hh_twoadultsandchild") 
)

# Append to changes dataframe
changes <- rbind(changes, df)



## [+1sd] hh_num_members ####  

# Reset individual
load("individual.RData")

# Manually set changes: 2 + 1
individual$hh_num_members <-  round(median(database$hh_num_members)) + round(sd(database$hh_num_members))

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[+1sd] hh_num_members", "[+1sd] hh_num_members", "[+1sd] hh_num_members") 
)

# Append to changes dataframe
changes <- rbind(changes, df)



## [+1sd] hh_num_minors ####              

# Reset individual
load("individual.RData")

# Manually set changes: 0+1
individual$hh_num_minors <-  0 + round(sd(database$hh_num_minors))

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[+1sd] hh_num_minors", "[+1sd] hh_num_minors", "[+1sd] hh_num_minors") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


## [+1sd] hh_propmale18 ####    

# Reset individual
load("individual.RData")

# Manually set changes
individual$hh_propmale18 <-  median(database$hh_propmale18) + sd(database$hh_propmale18)

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[+1sd] hh_propmale18", "[+1sd] hh_propmale18", "[+1sd] hh_propmale18") 
)

# Append to changes dataframe
changes <- rbind(changes, df)




## [+1sd] hh_meanage18 ####     

# Reset individual
load("individual.RData")

# Manually set changes
individual$hh_meanage18 <-  median(database$hh_meanage18) + sd(database$hh_meanage18)

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[+1sd] hh_meanage18", "[+1sd] hh_meanage18", "[+1sd] hh_meanage18") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


## Subgroup: High education attainment [benchmark set to hh_all_higheduc] ####
### [0 to 1] hh_somehigheduc ####   

# Reset individual
load("individual.RData")


# Manually set changes
individual$hh_some_higheduc <- 1

# Adjust intra-group variables
individual$hh_all_higheduc <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] hh_some_higheduc", "[0 to 1] hh_some_higheduc", "[0 to 1] hh_some_higheduc") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [1 to 0] hh_all_higheduc ####     

# Reset individual
load("individual.RData")

# Manually set changes
individual$hh_all_higheduc <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] hh_all_higheduc", "[1 to 0] hh_all_higheduc", "[1 to 0] hh_all_higheduc") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


## Subgroup: Income [benchmark set to inc_more_5_thous] ####
### [0 to 1] inc_1_to_2_thous ####   

# Reset individual
load("individual.RData")

# Manually set changes
individual$inc_1_to_2_thous <- 1

# Adjust intra-group variables
individual$inc_more_5_thous <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] inc_1_to_2_thous", "[0 to 1] inc_1_to_2_thous", "[0 to 1] inc_1_to_2_thous") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [0 to 1] inc_2_to_3_thous ####    

# Reset individual
load("individual.RData")

# Manually set changes
individual$inc_2_to_3_thous <- 1

# Adjust intra-group variables
individual$inc_more_5_thous <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] inc_2_to_3_thous", "[0 to 1] inc_2_to_3_thous", "[0 to 1] inc_2_to_3_thous") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [0 to 1] inc_3_to_5_thous ####  

# Reset individual
load("individual.RData")

# Manually set changes
individual$inc_3_to_5_thous <- 1

# Adjust intra-group variables
individual$inc_more_5_thous <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] inc_3_to_5_thous", "[0 to 1] inc_3_to_5_thous", "[0 to 1] inc_3_to_5_thous") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [1 to 0] inc_more_5_thous ####    

# Reset individual
load("individual.RData")

# Manually set changes
individual$inc_more_5_thous <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] inc_more_5_thous", "[1 to 0] inc_more_5_thous", "[1 to 0] inc_more_5_thous") 
)

# Append to changes dataframe
changes <- rbind(changes, df)



## Subgroup: Working status[benchmark set to "some work"] ####
### [0 to 1] ws_all_work ####  

# Reset individual
load("individual.RData")

# Manually set changes
individual$ws_all_work <- 1

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] ws_all_work", "[0 to 1] ws_all_work", "[0 to 1] ws_all_work") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [0 to 1] ws_none_work ####     

# Reset individual
load("individual.RData")

# Manually set changes
individual$ws_none_work <- 1

# Adjust intra-group variables
#individual$ws_all_work <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] ws_none_work", "[0 to 1] ws_none_work", "[0 to 1] ws_none_work") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


## Subgroup: Working place [benchmark set to wp_otherplace] ####
### [0 to 1] wp_ownhome ####   

# Reset individual
load("individual.RData")

# Manually set changes
individual$wp_ownhome <- 1

# Adjust intra-group variables
individual$wp_otherplace <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] wp_ownhome", "[0 to 1] wp_ownhome", "[0 to 1] wp_ownhome") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [0 to 1] wp_myprovince ####   

# Reset individual
load("individual.RData")

# Manually set changes
individual$wp_myprovince <- 1

# Adjust intra-group variables
individual$wp_otherplace <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] wp_myprovince", "[0 to 1] wp_myprovince", "[0 to 1] wp_myprovince") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [0 to 1] wp_otherplace #### 

# Reset individual
load("individual.RData")

# Manually set wp_otherplace to 0
individual$wp_otherplace <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] wp_otherplace", "[1 to 0] wp_otherplace", "[1 to 0] wp_otherplace") 
)

# Append to changes dataframe
changes <- rbind(changes, df)



## Subgroup: Commutement time [benchmark set to com_20_to_39_min] ####
### [1 to 0] com_20_to_39_min ####         

# Reset individual
load("individual.RData")

# Manually set changes
individual$com_20_to_39_min <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] com_20_to_39_min", "[1 to 0] com_20_to_39_min", "[1 to 0] com_20_to_39_min") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


### [0 to 1] com_40_to_59_min ####  

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$com_40_to_59_min <- 1

# Adjust intra-group variables
individual$com_20_to_39_min <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] com_40_to_59_min", "[0 to 1] com_40_to_59_min", "[0 to 1] com_40_to_59_min") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [0 to 1] com_more_1_hour #### 

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$com_more_1_hour <- 1

# Adjust intra-group variables
individual$com_20_to_39_min <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] com_more_1_hour", "[0 to 1] com_more_1_hour", "[0 to 1] com_more_1_hour") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


## [+1sd] com_num_trips ####      

# Reset individual
load("individual.RData")

# Manually set changes
individual$com_num_trips <-  median(database$com_num_trips) + round(sd(database$com_num_trips))

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[+1sd] com_num_trips", "[+1sd] com_num_trips", "[+1sd] com_num_trips") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


## Subgroup: Municipalty size[benchmark set to geo_50_100_thous]  ####
### [1 to 0] geo_50_100_thous ####

# Reset individual
load("individual.RData")

# Manually set changes
individual$geo_50_100_thous <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] geo_50_100_thous", "[1 to 0] geo_50_100_thous", "[1 to 0] geo_50_100_thous") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [0 to 1] geo_100_500_thous ####

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$geo_100_500_thous <- 1

# Adjust intra-group variables
individual$geo_50_100_thous <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] geo_100_500_thous", "[0 to 1] geo_100_500_thous", "[0 to 1] geo_100_500_thous") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [0 to 1]geo_more_500_thous ####  

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$geo_more_500_thous <- 1

# Adjust intra-group variables
individual$geo_50_100_thous <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] geo_more_500_thous", "[0 to 1] geo_more_500_thous", "[0 to 1] geo_more_500_thous") 
)

# Append to changes dataframe
changes <- rbind(changes, df)



## [0 to 1] geo_barcelona ####

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$geo_barcelona <- 1

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] geo_barcelona", "[0 to 1] geo_barcelona", "[0 to 1] geo_barcelona") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


## [0 to 1] geo_madrid ####    

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$geo_madrid <- 1

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] geo_madrid", "[0 to 1] geo_madrid", "[0 to 1] geo_madrid") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


## [5 to 0] geo_services #### 

# Reset individual
load("individual.RData")

# Manually set to zero services available
individual$geo_services <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[5 to 0] geo_services", "[5 to 0] geo_services", "[5 to 0] geo_services") 
)

# Append to changes dataframe
changes <- rbind(changes, df)



## [1 to 0] h_secondhome ####  

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$h_secondhome <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] h_secondhome", "[1 to 0] h_secondhome", "[1 to 0] h_secondhome") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


# Subgroup: Housing tenure status [benchmark: h_ownership]
## [1 to 0] h_ownership ####              

# Reset individual
load("individual.RData")

# Manually set changes
individual$h_ownership <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] h_ownership", "[1 to 0] h_ownership", "[1 to 0] h_ownership") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

## [0 to 1] h_rental ####   

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$h_rental <- 1

# Adjust intra-group variables
individual$h_ownership <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] h_rental", "[0 to 1] h_rental", "[0 to 1] h_rental") 
)

# Append to changes dataframe
changes <- rbind(changes, df)



## [1 to 0] h_park_slot ####  

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$h_park_slot <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] h_park_slot", "[1 to 0] h_park_slot", "[1 to 0] h_park_slot") 
)

# Append to changes dataframe
changes <- rbind(changes, df)



## [1 to 0] h_detached ####    

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$h_detached <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] h_detached", "[1 to 0] h_detached", "[1 to 0] h_detached") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


## [1 to 0] mob_other_vehi #### 

# Reset individual
load("individual.RData")

# Manually set wp_otherplace
individual$mob_other_vehi <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] mob_other_vehi", "[1 to 0] mob_other_vehi", "[1 to 0] mob_other_vehi") 
)

# Append to changes dataframe
changes <- rbind(changes, df)


## Subgroup: Principal mode transportation [benchmark: mob_pmt_priv_car] ####
### [1 to 0] mob_pmt_priv_car ####

# Reset individual
load("individual.RData")

# Manually set changes
individual$mob_pmt_priv_car <- 0

# Adjust intra-group variables
# Not needed

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[1 to 0] mob_pmt_priv_car", "[1 to 0] mob_pmt_priv_car", "[1 to 0] mob_pmt_priv_car") 
)

# Append to changes dataframe
changes <- rbind(changes, df)

### [0 to 1] mob_pmt_pub_trans ####   

# Reset individual
load("individual.RData")


# Manually set wp_otherplace
individual$mob_pmt_pub_trans <- 1

# Adjust intra-group variables
individual$mob_pmt_priv_car <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] mob_pmt_pub_trans", "[0 to 1] mob_pmt_pub_trans", "[0 to 1] mob_pmt_pub_trans") 
)

# Append to changes dataframe
changes <- rbind(changes, df) 

### [0 to 1] mob_pmt_moto ####  

# Reset individual
load("individual.RData")


# Manually set wp_otherplace
individual$mob_pmt_moto <- 1

# Adjust intra-group variables
individual$mob_pmt_priv_car <- 0

# Compute probabilities
recomputed           <- as.data.frame(calculate_probabilities(betas, individual))
colnames(recomputed) <- c("alt1", "alt2", "alt3")

# Calculate mean values and transpose
recomputed <- as.data.frame(colMeans(recomputed, na.rm = TRUE))
recomputed <- t(recomputed)

# Compute differences
diff1 <- recomputed[, 1] - benchmark[, 1]
diff2 <- recomputed[, 2] - benchmark[, 2]
diff3 <- recomputed[, 3] - benchmark[, 3]

# Create dataframe
df <- data.frame(
  OR  = c(diff1, diff2, diff3),
  var = c("[0 to 1] mob_pmt_moto", "[0 to 1] mob_pmt_moto", "[0 to 1] mob_pmt_moto") 
)

# Append to changes dataframe
changes <- rbind(changes, df) 

# [21] Prepare data to plot ----

rownames(changes) <- NULL

changes$ALT <- rep(1:3)

# Create ALT labels 
changes$ALT_label <- factor(changes$ALT, 
                            levels = c(1, 2, 3), 
                            labels = c("No vehicle", 
                                       "Fuel vehicle", 
                                       "Clean vehicle"))

# Determine the unique ALT levels and the number of levels
alt_levels <- unique(changes$ALT)

# Create subsets of data for each ALT
alt1 <- subset(changes, ALT == 1)
alt2 <- subset(changes, ALT == 2)
alt3 <- subset(changes, ALT == 3)


# Clean workspace & free local memory usage

rm(list = c("apollo_control",
            "apollo_beta",
            "apollo_fixed",
            "diff1", "diff2", "diff3",
            "means",
            "apollo_probabilities",
            "apollo_randCoeff",
            "calculate_probabilities",
            "apollo_draws",
            "apollo_inputs",
            "database",
            "df",
            "model",
            "recomputed",
            "betas",
            "alt_levels"))

gc()


# [22] Plot average changes in probabilities
## [22.1] "Figure 1. Effects of attribute changes on Clean Vehicle choice probability" ----

figure1 <- ggplot(data     = alt3, 
                     aes(x = OR, 
                         y = reorder(var, -OR))) +
  
  geom_vline(xintercept = 0, 
             linetype   = "dashed", 
             color      = "grey50") +
  
  geom_point(size      = 4, 
             aes(color = ifelse(grepl("^\\[1 to 0\\]", var) | var == "[5 to 0] geo_services", "grey65",
                                "black"))) +
  
  labs(title    = "",
       subtitle = "",
       x = "Change in probability",
       y = "") +
  
  theme_light(base_size   = 14, 
              base_family = "serif") +
  
  theme(plot.title = element_text(size   = 0, 
                                  face   = "bold", 
                                  hjust  = 0.5, 
                                  family = "sans", 
                                  margin = margin(b = 10)),
        plot.subtitle = element_text(size   = 0, 
                                     face   = "italic", 
                                     hjust  = 0.5, 
                                     family = "serif", 
                                     margin = margin(b = 10)),
        axis.title.x = element_text(size    = 20, 
                                    family  = "serif", 
                                    margin  = margin(t = 20)),
        axis.text.x = element_text(size   = 20,
                                   family = "serif"),
        axis.text.y = element_text(size   = 20, 
                                   family = "serif"),
        legend.text = element_text(size   = 20, 
                                   family = "serif"),
        legend.spacing.y = unit(2, "cm"),
        legend.key.height = unit(1, "cm"),
        legend.title = element_text(size   = 19, 
                                    face   = "bold", 
                                    family = "sans"), 
        
        panel.grid.major = element_line(color = "grey80"),  
        panel.grid.minor = element_line(color = "grey82"),  
        panel.background = element_rect(fill = "white"),    
        panel.border = element_blank()) + 
  
  scale_color_manual(values = c("grey65" = "grey65", 
                                "black" = "black"), 
                     name = "Change type",
                     labels = c("[0-1], [+1sd]", 
                                "[1-0], [5-0]")) +
  
  coord_cartesian(xlim = c(-0.045, 0.1)) +
  
  scale_y_discrete(labels = c("[1 to 0] inc_more_5_thous"     = "Income: More than 5000€ [1-0]",
                              "[0 to 1] inc_1_to_2_thous"     = "Income: 1000€ to 2000€ [0-1]",
                              "[0 to 1] inc_2_to_3_thous"     = "Income: 2000€ to 3000€ [0-1]",
                              "[1 to 0] mob_other_vehi"       = "Other vehicles [1-0]",
                              "[0 to 1] geo_madrid"           = "Madrid [0-1]",
                              "[1 to 0] hh_all_higheduc"      = "High education: all [1-0]",
                              "[1 to 0] geo_50_100_thous"     = "Inhabitants: 50k-100k  [1-0]",
                              "[0 to 1] mob_pmt_moto"         = "Primary transport: Motorbike [0-1]",
                              "[0 to 1] inc_3_to_5_thous"     = "Income: 3000€ to 5000€ [0-1]",
                              "[0 to 1] wp_ownhome"           = "Working place: own home [0-1]",
                              "[1 to 0] h_park_slot"          = "Parking slot [1-0]",
                              "[0 to 1] geo_barcelona"        = "Barcelona [0-1]",
                              "[0 to 1] geo_more_500_thous"   = "Inhabitants: >500k [0-1]",
                              "[+1sd] hh_num_members"         = "Household members [+1sd]",
                              "[1 to 0] hh_foreign"           = "Foreign members [1-0]",
                              "[0 to 1] hh_some_higheduc"     = "High education: some [0-1]",
                              "[1 to 0] h_detached"           = "Detached house [1-0]",
                              "[0 to 1] ws_none_work"         = "Working status: none work [0-1]",
                              "[0 to 1] wp_myprovince"        = "Working place: own province [0-1]",
                              "[1 to 0] h_secondhome"         = "Second home ownership [1-0]",
                              "[0 to 1] hh_oneperson"         = "Family type: one person [0-1]",
                              "[0 to 1] mob_pmt_pub_trans"    = "Primary transport: public transp. [0-1]",
                              "[0 to 1] h_rental"             = "Housing tenure: rental [0-1]",
                              "[1 to 0] hh_twoadultsalone"    = "Family type: two adults [1-0]",
                              "[1 to 0] mob_pmt_priv_car"     = "Primary transport: private car [1-0]",
                              "[0 to 1] hh_twoadultsandchild" = "Family type: two adults and child [0-1]",
                              "[0 to 1] com_40_to_59_min"     = "Max. commutement: 40-59 min. [0-1]",
                              "[1 to 0] wp_otherplace"        = "Working place: other places [1-0]",
                              "[1 to 0] com_20_to_39_min"     = "Max. commutement: 20-39 min. [1-0]",
                              "[0 to 1] geo_100_500_thous"    = "Inhabitants: 100k-500k [0-1]",
                              "[5 to 0] geo_services"         = "Services available [5-0]",
                              "[+1sd] hh_propmale18"          = "Male >18 proportion [+1sd]",
                              "[0 to 1] com_more_1_hour"      = "Max. commutement: >1h. [0-1]",
                              "[0 to 1] ws_all_work"          = "Working status: all work [0-1]",
                              "[+1sd] com_num_trips"          = "Number of trips [+1sd]",
                              "[+1sd] hh_num_minors"          = "Number of minors [+1sd]",
                              "[1 to 0] h_ownership"          = "Housing tenure: ownership [1-0]",
                              "[0 to 1] hh_singleparent"      = "Family type: single parent [0-1]",
                              "[+1sd] hh_meanage18"           = "Mean age members >18 [+1sd]")) +
  
  scale_x_continuous(breaks = c(-0.05, -0.025, 0, 0.025, 0.05, 0.075, 0.1)) 


# Save the plot as a SVG
ggsave("Figure_1.svg", 
       plot   = figure1, 
       width  = 31, 
       #       height = 15, 
       height = 12,
       units  = "in", 
       dpi    = 300)


## [22.2] "Figure 2. Effects of income changes on Clean Vehicle choice probability" ----

figure2 <- ggplot(data = alt3 %>% filter(var %in% c("[0 to 1] inc_1_to_2_thous", 
                                                    "[0 to 1] inc_2_to_3_thous", 
                                                    "[0 to 1] inc_3_to_5_thous", 
                                                    "[1 to 0] inc_more_5_thous")),
                  aes(x = OR, 
                      y = reorder(var, case_when(
                        var == "[1 to 0] inc_more_5_thous" ~ 1,
                        var == "[0 to 1] inc_1_to_2_thous" ~ 2,
                        var == "[0 to 1] inc_2_to_3_thous" ~ 3,
                        var == "[0 to 1] inc_3_to_5_thous" ~ 4
                      )))) +  
  
  geom_vline(xintercept = 0, 
             linetype   = "dashed", 
             color      = "grey50") +
  
  geom_point(size = 2, color = "black") +
  
  geom_point(aes(x = 0.00, 
                 y = 5), 
             shape = 8, 
             size  = 3, 
             color = "black") +
  
  labs(title    = "",
       subtitle = " ",
       x = "Change in probability",
       y = "") +
  
  theme_light(base_size   = 14, 
              base_family = "serif") +
  
  theme(plot.title = element_text(size      = 15, 
                                  face      = "bold", 
                                  hjust     = 0.5, 
                                  family    = "sans", 
                                  margin    = margin(b = 10)),
        plot.subtitle = element_text(size   = 11, 
                                     face   = "italic", 
                                     hjust  = 0.5, 
                                     family = "serif", 
                                     margin = margin(b = 10)),
        axis.title.x = element_text(size    = 13, 
                                    family  = "serif",
                                    margin  = margin(t = 10)),
        axis.text.x = element_text(size     = 12, 
                                   family   = "serif", 
                                   angle    = 0, 
                                   hjust    = 0.5), 
        axis.text.y = element_text(size     = 11, 
                                   family   = "serif"),
        axis.title.y = element_text(size    = 11,
                                    family  = "serif",
                                    margin  = margin(t = 10)),
        legend.position = "none") + 
  
  coord_flip() +  
  
  scale_x_continuous(limits = c(-0.045, 0.01)) +  
  
  scale_y_discrete(limits = c("[1 to 0] inc_more_5_thous",
                              "[0 to 1] inc_1_to_2_thous",
                              "[0 to 1] inc_2_to_3_thous",
                              "[0 to 1] inc_3_to_5_thous",
                              "new_obs"),
                   labels = c("[1 to 0] inc_more_5_thous" = "<1,000€",
                              "[0 to 1] inc_1_to_2_thous" = "1,000€ to 2,000€",
                              "[0 to 1] inc_2_to_3_thous" = "2,000€ to 3,000€",
                              "[0 to 1] inc_3_to_5_thous" = "3,000€ to 5,000€",
                              "new_obs" = ">5,000€"))

# Save the plot as a SVG
ggsave("PLOT_02_income.svg", 
       plot   = p_income, 
       width  = 11, 
       height = 3, 
       units  = "in", 
       dpi    = 300)


## [22.3] "Figure 3. Effects of high education attainment changes on Clean Vehicle Choice Probability" ----

figure3 <- ggplot(data = alt3 %>% 
                    filter(var %in% c("[1 to 0] hh_all_higheduc",
                                      "[0 to 1] hh_some_higheduc")) %>%
                    mutate(facet_label = case_when(
                      var == "[1 to 0] hh_all_higheduc"   ~ "High education: none\n [Model baseline]",
                      var == "[0 to 1] hh_some_high_educ" ~ "High education: some",
                      TRUE ~ NA_character_
                    )),
                  aes(x = OR, 
                      y = reorder(var, case_when(
                        var == "[1 to 0] hh_all_higheduc"  ~ 1,
                        var == "[0 to 1] hh_some_higheduc" ~ 2,
                        var == "new_obs" ~ 3 
                      )))) +  
  
  geom_vline(xintercept = 0, 
             linetype   = "dashed", 
             color      = "grey50") +
  
  geom_point(size  = 2.5, 
             color = "black") +
  
  geom_point(aes(x = 0.00, 
                 y = 3), 
             shape = 8, 
             size  = 3, 
             color = "black") +  
  
  labs(title    = "",
       subtitle = "",
       x = "Change in probability",
       y = "") +
  
  theme_light(base_size   = 14, 
              base_family = "serif") +  
  
  theme(plot.title = element_text(size      = 15, 
                                  face      = "bold", 
                                  hjust     = 0.5, 
                                  family    = "sans", 
                                  margin    = margin(b = 10)),
        plot.subtitle = element_text(size   = 11, 
                                     face   = "italic", 
                                     hjust  = 0.5, 
                                     family = "serif", 
                                     margin = margin(b = 10)),
        axis.title.x = element_text(size    = 13, 
                                    family  = "serif",
                                    margin  = margin(t = 10)),
        axis.text.x = element_text(size     = 12, 
                                   family   = "serif", 
                                   angle    = 0, 
                                   hjust    = 0.5), 
        axis.text.y = element_text(size     = 11, 
                                   family   = "serif"),
        axis.title.y = element_text(size    = 11,
                                    family  = "serif",
                                    margin  = margin(t = 10)),
        legend.spacing.y = unit(0.5, "cm"),
        legend.position = "none") +  
  
  coord_flip() +  
  
  scale_x_continuous(limits = c(-0.03, 0.005)) +  
  
  scale_y_discrete(limits = c("[1 to 0] hh_all_higheduc",
                              "[0 to 1] hh_some_higheduc",
                              "new_obs"),
                   labels = c("[1 to 0] hh_all_higheduc"  = "High education: non",
                              "[0 to 1] hh_some_higheduc" = "High education: some",
                              "new_obs"                   = "High education: All"))

# Save the plot as a SVG
ggsave("PLOT_03_higheduc.svg", 
       plot   = p_higheduc, 
       width  = 11, 
       height = 3, 
       units  = "in", 
       dpi    = 300)

# [23] End ----

# Track errors
traceback()

# Set end time
end_time <- Sys.time()

# Execution time
time = end_time - start.time
print(time) 

# Last run time: ~86h
