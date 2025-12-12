# Project       : Clean vehicle ownership in Spain
# Creation date : 08/04/2024
# Last update   : 31/11/2024
# Author        : Mercè Amich (merce.amich@ehu.eus)
# Institution   : Euskal Herriko Unibertsitatea / Universidad del País Vasco
# Last run time : ~100h

# 1. OBJECTIVE #################################################################

# Download ECEPOV-21 data & process it
# Descriptive statistics and frequency tables
# Error-Component Mixed Logit model (Apollo)
# Post-estimation: AME computation, plot

# 2. PRELIMINARY STEPS #########################################################

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
                      "forcats"     , 
                      "ggh4x"       ,
                      "openxlsx"
                      
)

for (p in packages.needed) {
  if (!p %in% row.names(packages.loaded)) install.packages(p)
  eval(bquote(library(.(p))))
}

# Define main path
path <- here()

# Set working directory
setwd(paste0(path))

# 3. DOWNLOAD DATA & PROCESS IT ################################################

# == [A] Download survey from the Spanish Statistical Office ==================

base.url = 'https://www.ine.es/ftp/microdatos/ecepov/' # Define structural url
setwd(paste0(path, "/DATA/")) # Set working directory

# Delete the folder generated in previous runs (if it exists) 
if(dir.exists(paste0(path, "/DATA/"))){unlink(paste0(path, "/DATA/"), recursive = TRUE)}

dir.create(paste0(path, "/DATA/")) # Create raw data folder 
eval(parse(text = paste0("file = 'datos_2021.zip'"))) # Define zip file
url <- paste0(base.url, file) # Define url
destination <- paste0(file) # Set the destination file
setwd(paste0(path, "/DATA/")) # Set working directory
download(url, destination,  mode = 'wb') # Download microdata
unzip(destination)  # Unzip microdata
unlink(destination, recursive = TRUE) # Delete unzipped folder

# Create list of folders in the directory
list.folders = list.files(pattern     = '.zip', 
                          ignore.case = TRUE)

# By folder
for (f in 1:length(list.folders)) {
  
  folder = list.folders[f]   # Define parameters
  unzip(folder)   # Unzip microdata
  unlink(folder, recursive = TRUE) # Delete useless folder(s)
  
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

# == [B] Create intermediate files =============================================

## [B.1] From "dataH" ("hogar") to "mergeFileH" ####

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


## [B.2] From "dataA" ("adultos") to "mergeFileA" ####

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


## [B.3] Merge "mergeFileH" and "mergeFileA" with "dataV" ----

data <- left_join(dataV, mergeFileH, by = c("IDEN", "FACTOR"))
data <- left_join(data,  mergeFileA, by = c("IDEN", "FACTOR"))

# The resulting dataframe has 172444 observations and 121 variables

# Create final "data.Rdata" object
save(data, file = "data.Rdata")

remove("dataA", "dataH", "dataV", "mergeFileA", "mergeFileH")

# == [C] Create new variables ==================================================

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

# == [D] Rename vars in english ================================================

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


# 4. CREATE DESCRIPTIVE TABLES #################################################

## == [A] "Table 2: Vehicle ownership" =======================================

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

## == [B] "Table B.1. Numerical variables: descriptive statistics" =============
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

## == [C] "Table C.1. Categorical variables: frequency distributions" ==========
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


# 5. ERROR-COMPONENT MIXED LOGIT MODEL (APOLLO) ################################

gc() 

# == [A] Set Apollo controls ===================================================
apollo_control = list(
  modelName  = "EC_24092025",
  modelDescr = "Weighted_EC_vehitype",
  indivID    = "IDEN",
  weights    = "FACTOR",
  nCores     = 10
)

# == [B] Load data =============================================================

load("data.Rdata")
set.seed(123)
database  <- data
remove(list = "data")

gc()

# Divide FACTOR/100 in order for the Log-Likelihood algorithm to converge
database$FACTOR <- database$FACTOR/100

# == [C] Define model parameters ===============================================

#ASC_2 + Betas of Alt:2 [baseline: Fuel Car] are fixed to zero

# [C.1] "apollo_beta": define model parameters (initial values) ----
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
  b2_mob_pmt_moto          = 0.0,
  b3_mob_pmt_moto          = 0.0,
  
  # ERROR TERMS
  sd_2                     = 0.1,
  sd_3                     = 0.2
  
)

# [C.2] "apollo_fixed": alt2
# Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta

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

# [C.3] "apollo_draws" ----

### Set parameters for generating draws (5000 sobol draws)
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_2",
                     "draws_3")
)

# [C.3] "apollo_randCoeff" ----
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["r_2"]] =     ( sd_2 * draws_2)
  randcoeff[["r_3"]] =     ( sd_3 * draws_3 )
  return(randcoeff)
}

# == [D] Validate data =========================================================

apollo_inputs = apollo_validateInputs()

# == [E] Apollo probabilities ==================================================

# [E.1] "apollo_probabilities" ----

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
  
  # !! KEY STEP for the AME computation to well-behave:
  # "raw" returns "P": raw pred. probabilities for each individual/draw
  # if not, apollo averages the random components of the draws and CIs cannot
  # be computed:
  if (functionality == "raw") return(P)  # !! 
  
  # The rest, as in apollo estimation:
  ### Account for the weights
  P = apollo_weighting(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  
  ### Prepare and return outputs of function
  # if (functionality == "raw") {
  #   return(P)  # skip apollo_prepareProb() and averaging
  # }
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# == [F] Estimate the model ====================================================

# [F.1] "apollo_estimate" ----

model = apollo_estimate(apollo_beta, 
                        apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs, 
                        estimate_settings=list(
                          maxIterations=500))     

# == [G] Model output (obtain & save for Table A.1) ============================

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

# 6. POST-ESTIMATION ###########################################################

# == [A] Load model and data ===================================================

rm(list = ls(all = TRUE)) # clear workspace
database <- load(file.path(path, "data.Rdata")) # load data
database <- data # rename & transform to data.frame
database$FACTOR <- database$FACTOR / 100 # adjust weights as in the model
remove(data) # keep environment clean & memory usage low
model    <- readRDS(file.path(path, "EC_model.rds")) # load model

# == [B] Create apollo objects in environment again ============================

set.seed(123) # for reproducibility

apollo_control = list(
  modelName  = "EC",
  modelDescr = "Weighted_EC_vehitype",
  indivID    = "IDEN",
  weights    = "FACTOR",
  nCores     = 10 
)

apollo_beta = c(asc_1 = -0.067885, asc_2 = 0.0, asc_3 = -4.144465, 
                b1_hh_foreign = 1.007025, b2_hh_foreign = 0.0, 
                b3_hh_foreign = 0.144441, b1_hh_oneperson = 0.518905, 
                b2_hh_oneperson = 0.0, b3_hh_oneperson = 0.074264, 
                b1_hh_singleparent = -0.039404, b2_hh_singleparent = 0.0, 
                b3_hh_singleparent = 0.022402, b1_hh_twoadultsalone = -0.790167,
                b2_hh_twoadultsalone = 0.0, b3_hh_twoadultsalone = 0.152213, 
                b1_hh_twoadultsandchild = -1.241126, 
                b2_hh_twoadultsandchild = 0.0, b3_hh_twoadultsandchild = -0.086202, 
                b1_hh_num_members = -0.192716, b2_hh_num_members = 0.0, 
                b3_hh_num_members = -0.038092, b1_hh_num_minors = 0.066684, 
                b2_hh_num_minors = 0.0, b3_hh_num_minors = 0.186956, 
                b1_hh_propmale18 = 0.0, b2_hh_propmale18 = 0.0, 
                b3_hh_propmale18 = 0.0, b1_hh_meanage18 = 0.0, 
                b2_hh_meanage18 = 0.0, b3_hh_meanage18 = 0.0, 
                b1_hh_some_higheduc = -0.468874, b2_hh_some_higheduc = 0.0, 
                b3_hh_some_higheduc = 0.526702, b1_hh_all_higheduc = -0.562453, 
                b2_hh_all_higheduc = 0.0, b3_hh_all_higheduc = 0.713541, 
                b1_inc_1_to_2_thous = -0.360102, b2_inc_1_to_2_thous = 0.0, 
                b3_inc_1_to_2_thous = -0.052324, b1_inc_2_to_3_thous = -0.980756,
                b2_inc_2_to_3_thous = 0.0, b3_inc_2_to_3_thous = 0.303296, 
                b1_inc_3_to_5_thous = -1.104291, b2_inc_3_to_5_thous = 0.0, 
                b3_inc_3_to_5_thous = 0.748509, b1_inc_more_5_thous = -0.656055, 
                b2_inc_more_5_thous = 0.0, b3_inc_more_5_thous = 1.158671, 
                b1_ws_all_work = -0.511006, b2_ws_all_work = 0.0, 
                b3_ws_all_work = 0.072424, b1_ws_none_work = 0.275080, 
                b2_ws_none_work = 0.0, b3_ws_none_work = -0.712666, 
                b1_wp_ownhome = -0.083514, b2_wp_ownhome = 0.0, 
                b3_wp_ownhome = 0.362673, b1_wp_myprovince = -0.467923, 
                b2_wp_myprovince = 0.0, b3_wp_myprovince = 0.235941, 
                b1_wp_otherplace = -0.590603, b2_wp_otherplace = 0.0, 
                b3_wp_otherplace = -0.083879, b1_com_20_to_39_min = -0.189068, 
                b2_com_20_to_39_min = 0.0, b3_com_20_to_39_min = -0.083177, 
                b1_com_40_to_59_min = 0.077159, b2_com_40_to_59_min = 0.0, 
                b3_com_40_to_59_min = 0.003872, b1_com_more_1_hour = 0.077159, 
                b2_com_more_1_hour = 0.0, b3_com_more_1_hour = 0.003872, 
                b1_com_num_trips = -0.155225, b2_com_num_trips = 0.0, 
                b3_com_num_trips = -0.003803, b1_geo_50_100_thous = 0.176347, 
                b2_geo_50_100_thous = 0.0, b3_geo_50_100_thous = 0.227539, 
                b1_geo_100_500_thous = 0.516286, b2_geo_100_500_thous = 0.0, 
                b3_geo_100_500_thous = 0.179566, b1_geo_more_500_thous = 1.138612, 
                b2_geo_more_500_thous = 0.0, b3_geo_more_500_thous = 0.168840, 
                b1_geo_barcelona = 0.450697, b2_geo_barcelona = 0.0, 
                b3_geo_barcelona = 0.133745, b1_geo_madrid = 0.0, 
                b2_geo_madrid = 0.0, b3_geo_madrid = 0.0, b1_geo_services = 0.0,
                b2_geo_services = 0.0, b3_geo_services = 0.0, 
                b1_h_secondhome = 0.0, b2_h_secondhome = 0.0, 
                b3_h_secondhome = 0.0, b1_h_ownership = 0.0, 
                b2_h_ownership = 0.0, b3_h_ownership = 0.0, b1_h_rental = 0.0, 
                b2_h_rental = 0.0, b3_h_rental = 0.0, b1_h_park_slot = 0.0, 
                b2_h_park_slot = 0.0, b3_h_park_slot = 0.0, b1_h_detached = 0.0, 
                b2_h_detached = 0.0, b3_h_detached = 0.0, b2_mob_other_vehi = 0.0, 
                b3_mob_other_vehi = 0.0, b2_mob_pmt_priv_car = 0.0, 
                b3_mob_pmt_priv_car = 0.0, b1_mob_pmt_pub_trans = 0.0, 
                b2_mob_pmt_pub_trans = 0.0, b3_mob_pmt_pub_trans = 0.0, 
                b2_mob_pmt_moto = 0.0, b3_mob_pmt_moto = 0.0, 
                sd_2 = 0.1, sd_3 = 0.2)

apollo_fixed = c("asc_2", "b2_hh_foreign", "b2_hh_oneperson", "b2_hh_singleparent", "b2_hh_twoadultsalone", 
                 "b2_hh_twoadultsandchild", "b2_hh_num_members", "b2_hh_num_minors", "b2_hh_propmale18", 
                 "b2_hh_meanage18", "b2_hh_some_higheduc", "b2_hh_all_higheduc", "b2_inc_1_to_2_thous",
                 "b2_inc_2_to_3_thous", "b2_inc_3_to_5_thous", "b2_inc_more_5_thous", "b2_ws_all_work", 
                 "b2_ws_none_work", "b2_wp_ownhome", "b2_wp_myprovince", "b2_wp_otherplace", "b2_com_20_to_39_min", 
                 "b2_com_40_to_59_min", "b2_com_more_1_hour", "b2_com_num_trips", "b2_geo_50_100_thous", 
                 "b2_geo_100_500_thous", "b2_geo_more_500_thous", "b2_geo_barcelona", "b2_geo_madrid", 
                 "b2_geo_services", "b2_h_secondhome", "b2_h_ownership", "b2_h_rental", "b2_h_park_slot", 
                 "b2_h_detached", "b2_mob_other_vehi", "b2_mob_pmt_priv_car", "b2_mob_pmt_pub_trans", "b2_mob_pmt_moto") 

apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 1000, 
  interUnifDraws = c(),
  interNormDraws = c("draws_2", "draws_3")
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["r_2"]] = sd_2 * apollo_inputs$draws$draws_2 # notation needed for AME computation
  randcoeff[["r_3"]] = sd_3 * apollo_inputs$draws$draws_3 # same
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta,
                              apollo_inputs,
                              functionality = "estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### UTILITY EQUATIONS *************************************
  
  ### List of utilities: these must use the same names as in mnl_settings
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
    alternatives  = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail         = 1,
    choiceVar     = vehitype,
    V             = V
  )
  
  # ****************************************************************************
  
  ### Create list of probabilities P
  P = list()
  
  ### Compute probabilities using MNL model
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  # !! KEY STEP for the AME computation to well-behave:
  # "raw" returns "P": raw pred. probabilities for each individual/draw
  # if not, apollo averages the random components of the draws and CIs cannot
  # be computed:
  if (functionality == "raw") return(P)  # !! 
  
  # The rest, as in apollo estimation:
  ### Account for the weights
  P = apollo_weighting(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  
  ### Prepare and return outputs of function
  # if (functionality == "raw") {
  #   return(P)  # skip apollo_prepareProb() and averaging
  # }
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

gc() # keep environment clean & memory usage low

# == [C] Define and differentiate types of variables in the model ==============

numeric_vars <- c("hh_num_members","hh_num_minors","hh_propmale18",  
                  "hh_meanage18", "com_num_trips", "geo_services") 

est_names  <- names(model$estimate) # extract names from estimated parameters
model_vars <- unique(gsub("^(b[1-3]_)?", "", est_names)) # remove prefixes

ignore_vars <- c("asc_1", "asc_2", "asc_3", "sd_2", "sd_3") # not variables
model_vars <- setdiff(model_vars, ignore_vars) # ignore them

choice_var <- "vehitype" # remove "vehitype" (choice variable) also
model_vars <- setdiff(model_vars, choice_var) # ignore it

dummy_vars <- setdiff(model_vars, numeric_vars) # dummy variables

dummy_groups <- list( # identify dummies that are part of a group of variables
  hh_type  = c("hh_oneperson", "hh_singleparent", "hh_twoadultsalone", "hh_twoadultsandchild"), 
  hh_edu   = c("hh_some_higheduc", "hh_all_higheduc"), 
  income   = c("inc_1_to_2_thous", "inc_2_to_3_thous", "inc_3_to_5_thous", "inc_more_5_thous"), 
  workstat = c("ws_all_work", "ws_none_work"), 
  workloc  = c("wp_ownhome", "wp_myprovince", "wp_otherplace"), 
  commute  = c("com_20_to_39_min", "com_40_to_59_min", "com_more_1_hour"), 
  geo_size = c("geo_50_100_thous", "geo_100_500_thous", "geo_more_500_thous"), 
  city     = c("geo_barcelona", "geo_madrid"), 
  housing  = c("h_ownership", "h_rental"), 
  modetra  = c("mob_pmt_priv_car", "mob_pmt_pub_trans", "mob_pmt_moto") ) 

grouped_dummy_vars <- unlist(dummy_groups) # dummies part of a group
ungrouped_dummy_vars <- setdiff(dummy_vars, grouped_dummy_vars) # standalone ones

# == [D] Compute AMEs at individual level (with debugging) =====================

## a) Create "V_theta" =========================================================

# Intermediate step needed because "model$estimate" includes alt2 parameters (0)
# which are not present in robvarcov
# To propagate uncertainty using a multivariate normal distribution using ::MASS,
# no parameters = 0 can be present in "mu"

theta_hat <- model$estimate        # extract vector of estimated parameters
est_names <- names(theta_hat)      # extract names of parameters 
vcov_raw  <- model$robvarcov       # extract robust variance-covariance

# Start a zero matrix to store all parameters
V_theta <- matrix(0, 
                  nrow     = length(apollo_beta), 
                  ncol     = length(apollo_beta),
                  dimnames = list(names(apollo_beta), names(apollo_beta)))

# Identify estimated parameters with estimated rob-varcov in "vcov_raw"
common <- intersect(rownames(vcov_raw), names(apollo_beta))

# Insert them in the zero matrix created before
V_theta[common, common] <- vcov_raw[common, common]

## b) Generate theta draws =====================================================

nSim = 1000 # Using 500 parametric MC simulations

theta_draws <- MASS::mvrnorm( # from a multivariate normal distribution
  n     = nSim, # nSim times
  mu    = theta_hat[common],# around the model estimates present in robvarcov 
  Sigma = V_theta[common, common] # using estimated robvarcov
)

# == [E] Helper function 1: "getFullTheta" =====================================

# Function to reconstruct full parameter vector

# To revert the previous step and include again parameters fixed to 0 (alt2) and
# estimate the random coefficients (sd_2 and sd_3) for the simulation

getFullTheta <- function(draw_i, db) {
  
  # Start from base theta vector names (all of them, also alt2 ones)
  theta_full <- apollo_beta
  
  # Replace estimated parameters with each simulated draw
  theta_full[common] <- as.numeric(draw_i)  # ensure it's a numeric vector
  
  # Number of individuals in the dataset
  nInd <- nrow(db)
  
  # Simulate individual-level random coefficients 
  r_2 <- rnorm(nInd, mean = 0, sd = theta_full["sd_2"]) # zero mean and sd model
  r_3 <- rnorm(nInd, mean = 0, sd = theta_full["sd_3"]) # idem
  
  # Return list containing full parameter vector and individual random draws
  list(theta_full = theta_full, r_2 = r_2, r_3 = r_3)
}

# == [F] Helper function 2: "computeAMEsForTheta" ==============================

# Computes AMEs for each draw as the change in choice probabilities when a 
# covariate changes, holding all else constant.

# Inputs:
#   - theta_i: a list containing
#       * theta_full: full vector of simulated parameters for the model
#       * r_2, r_3: previously simulated random coefficients 
#
# Output:
#   - A list where each element corresponds to a variable, containing a matrix 
#     of marginal effects for each alternative (alt1-alt3) and each observation.

# The function identifies three types of AMEs to compute depending on the type
# of variable. Those are:
# [a] Numeric variables: increment by 0.01
# [b] Standalone dummy variables: from 0 to 1
# [c] Dummy part of a group: sets all group to 0, puts each dummy to 1 one by one


computeAMEsForTheta <- function(theta_i, numeric_method = c("discrete", "analytic")) {
  
  # Match the numeric method argument and allow multiple options
  numeric_method <- match.arg(numeric_method, several.ok = TRUE)  # flag
  
  # Copies of database and theta inputs
  db_orig <- apollo_inputs$database                           # original database
  db_mod  <- db_orig                                          # working copy
  theta_vec <- theta_i$theta_full                             # full theta vector
  r_2 <- theta_i$r_2                                          # random draw (2)
  r_3 <- theta_i$r_3                                          # random draw (3)
  
  
  # Helper: compute individual-level choice probabilities [nInd x nAlts]
  getProbsIndiv <- function(db_mod) {
    apollo_inputs_tmp <- apollo_inputs                        # temporary copy of inputs
    apollo_inputs_tmp$database <- db_mod                      # replace database with modified one
    
    # Ensure same random draws for all probability calls for this theta_i
    assign("r_2", r_2, envir = environment(apollo_probabilities))
    assign("r_3", r_3, envir = environment(apollo_probabilities))
    
    # Compute model probabilities
    P_list <- apollo_probabilities(theta_vec, apollo_inputs_tmp, "raw")$model
    do.call(cbind, P_list[1:3])                               # combine alternatives [nInd x nAlts]
  }
  
  
  # Baseline individual probabilities (for both discrete & analytic AMEs)
  P_baseline_indiv <- getProbsIndiv(db_orig)                  # [nInd x nAlts]
  n_alts <- ncol(P_baseline_indiv)                            # number of alternatives
  
  
  # Create empty result matrices for both methods (vars x alts)
  all_vars <- c(numeric_vars, ungrouped_dummy_vars, unlist(dummy_groups))
  
  mat_discrete <- matrix(NA_real_, 
                         nrow = length(all_vars), 
                         ncol = n_alts,
                         dimnames = list(all_vars, paste0("alt", 1:n_alts)))  # discrete AMEs
  
  mat_analytic <- matrix(NA_real_, 
                         nrow = length(all_vars), 
                         ncol = n_alts,
                         dimnames = list(all_vars, paste0("alt", 1:n_alts)))  # analytic AMEs
  
  
  ## [A] NUMERIC VARIABLES
  for (var in numeric_vars) {
    
    # --- DISCRETE METHOD: simulate +1 change in variable ---
    if ("discrete" %in% numeric_method) {
      db_mod[[var]] <- db_mod[[var]] + 1                      # increase var by 1
      P_mod_indiv <- getProbsIndiv(db_mod)                    # recompute probabilities
      mat_discrete[var, ] <- colMeans(P_mod_indiv - P_baseline_indiv)  # mean change
      db_mod[[var]] <- db_mod[[var]] - 1                      # reset variable
    }
    
    
    # --- ANALYTIC METHOD: use derivative formula ---
    if ("analytic" %in% numeric_method) {
      
      # Identify coefficient(s) linked to variable (alt-specific or shared)
      beta_alt_names <- paste0("b", 1:n_alts, "_", var)
      
      if (all(beta_alt_names %in% names(theta_vec))) {
        beta_vec <- as.numeric(theta_vec[beta_alt_names])     # alt-specific coefficients
      } else if (var %in% names(theta_vec)) {
        beta_vec <- rep(as.numeric(theta_vec[var]), n_alts)   # shared coefficient
      } else {
        beta_vec <- NULL                                      # not found
      }
      
      # If no coefficient, skip
      if (is.null(beta_vec)) {
        mat_analytic[var, ] <- NA_real_
      } else {
        # Build beta matrix per individual [nInd x nAlts]
        beta_row <- matrix(rep(beta_vec, each = nrow(P_baseline_indiv)), 
                           nrow = nrow(P_baseline_indiv))
        
        # Compute weighted mean beta per individual
        weighted_mean_beta <- rowSums(P_baseline_indiv * beta_row)       # length nInd
        
        # Derivative: P_ik * (beta_ik - Σ_l P_il * beta_il)
        deriv_mat <- P_baseline_indiv * 
          (beta_row - matrix(rep(weighted_mean_beta, n_alts), ncol = n_alts))
        
        # Average over individuals
        mat_analytic[var, ] <- colMeans(deriv_mat)
      }
    }
  }  # end numeric loop
  
  
  ## [B] STANDALONE DUMMY VARIABLES
  for (var in ungrouped_dummy_vars) {
    original_value <- db_mod[[var]]                           # store original values
    
    db_mod[[var]] <- 0                                        # set dummy = 0
    P_orig_indiv <- getProbsIndiv(db_mod)                     # probabilities at 0
    
    db_mod[[var]] <- 1                                        # set dummy = 1
    P_mod_indiv <- getProbsIndiv(db_mod)                      # probabilities at 1
    
    # Discrete AME: mean difference (1 - 0)
    mat_discrete[var, ] <- colMeans(P_mod_indiv - P_orig_indiv)
    
    db_mod[[var]] <- original_value                           # restore variable
  }
  
  ## [C] GROUPED DUMMY VARIABLES
  for (group_name in names(dummy_groups)) {
    vars_in_group <- dummy_groups[[group_name]]               # list of dummies in group
    original_values <- db_mod[, vars_in_group, drop = FALSE]  # store originals
    
    db_mod[, vars_in_group] <- 0                              # set all group dummies = 0
    P_orig_indiv <- getProbsIndiv(db_mod)                     # base probabilities
    
    for (var in vars_in_group) {
      db_mod[[var]] <- 1                                      # activate one dummy
      P_mod_indiv <- getProbsIndiv(db_mod)                    # new probabilities
      
      # Discrete AME: mean difference (1 - 0)
      mat_discrete[var, ] <- colMeans(P_mod_indiv - P_orig_indiv)
      
      db_mod[[var]] <- 0                                      # reset dummy
    }
    
    db_mod[, vars_in_group] <- original_values                # restore group dummies
  }
  
  # Return both discrete and analytic AME matrices
  list(discrete = mat_discrete, 
       analytic = mat_analytic)
}

# == [G] Compute AMEs, activate functions ======================================

# Purpose:
# Runs nSim simulation draws. For each draw:
#   1. Builds a full theta vector via getFullTheta().
#   2. Computes AMEs (discrete & analytic) using computeAMEsForTheta().
#   3. Tracks runtime, memory usage, and estimated time remaining.

res_list <- vector("list", nSim)        # store list(discrete=mat, analytic=mat) per draw
time_vec <- numeric(nSim)               # track elapsed time per simulation

for (i in seq_len(nSim)) {
  start_time <- Sys.time()              # start timer for current simulation
  
  # 1. Generate full parameter vector (theta) for this draw
  theta_i <- getFullTheta(theta_draws[i, ], apollo_inputs$database)
  
  # 2. Compute AMEs for current theta (both methods)
  res_list[[i]] <- computeAMEsForTheta(theta_i, 
                                       numeric_method = c("discrete", "analytic"))
  
  # 3. Cleanup random draws (avoid polluting next iteration)
  env <- environment(apollo_probabilities)
  rm(list = c("r_2", "r_3"), envir = env)
  
  rm(theta_i)                          # free memory from local theta
  gc(FALSE, TRUE)                      # garbage collection (light)
  
  # 4. Track and report progress
  elapsed     <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))  # sec
  time_vec[i] <- elapsed
  avg_time    <- mean(time_vec[1:i])                                          # average time per sim
  remaining   <- avg_time * (nSim - i)                                        # sec remaining
  remaining_h <- remaining / 3600                                             # convert to hours
  
  # Progress message with elapsed time, memory usage, and ETA
  cat(sprintf("Simulation %d/%d done in %.1f sec (mem: %.2f GB) | ETA: ~%.2f h\n",
              i, nSim, elapsed, pryr::mem_used()/1e9, remaining_h))
  flush.console() 
  
  # Every 10 iterations, run a full garbage collection
  if (i %% 10 == 0) gc(verbose = FALSE, full = TRUE)
}

# == [H] Stack simulation results into array [alts x vars x draws] ==============

nSim     <- length(res_list) # nSim = 500
all_vars <- rownames(res_list[[1]]) # extract var names
n_vars   <- length(all_vars) # extract number of vars
n_alts   <- ncol(res_list[[1]]) # extract number of alts

# Stack results: [alts × vars × draws]
res_array <- array(NA_real_, 
                   dim = c(n_alts, n_vars, nSim),
                   dimnames = list(paste0("alt", 1:n_alts), all_vars, NULL))

for(i in seq_len(nSim)) {
  res_array[,,i] <- t(res_list[[i]]) # traspose [vars x alts] to [alts x vars]
}

# == [I] Stack results =========================================================

# Purpose:
# Combine all simulation outputs into 3D arrays with dimensions: 
# [alt x var x draw]
# for both discrete and analytic AMEs.

# Extract variable and dimension information from the first simulation result
first_draw <- res_list[[1]]
vars   <- rownames(first_draw$discrete)      # variable names
n_vars <- length(vars)                       # number of variables
n_alts <- ncol(first_draw$discrete)          # number of alternatives
nSim_r <- length(res_list)                   # number of completed draws

# Allocate empty 3D arrays for results
res_discrete_array <- array(NA_real_, 
                            dim = c(n_alts, n_vars, nSim_r),
                            dimnames = list(paste0("alt", 1:n_alts), vars, NULL))

res_analytic_array <- array(NA_real_, 
                            dim = c(n_alts, n_vars, nSim_r),
                            dimnames = list(paste0("alt", 1:n_alts), vars, NULL))

# Fill arrays with results from each draw
for (i in seq_len(nSim_r)) {
  disc_i <- res_list[[i]]$discrete      # [vars x alts]
  anal_i <- res_list[[i]]$analytic
  
  # Transpose so dimensions match [alts x vars]
  res_discrete_array[,,i] <- t(disc_i)
  res_analytic_array[,,i] <- t(anal_i)
}

# == [J] Compute means & 95% Confidence Intervals ==============================

# Purpose:
# For each variable–alternative combination:
#   - Compute mean AME across draws
#   - Compute 95% percentile-based confidence intervals
# Also compute difference (discrete - analytic) statistics.

compute_summary <- function(array) {
  list(
    mean  = apply(array, c(1,2), mean, na.rm = TRUE),
    lower = apply(array, c(1,2), quantile, probs = 0.025, na.rm = TRUE),
    upper = apply(array, c(1,2), quantile, probs = 0.975, na.rm = TRUE)
  )
}

# --- Summaries ---
discrete_summary   <- compute_summary(res_discrete_array)
analytic_summary   <- compute_summary(res_analytic_array)
diff_array         <- res_discrete_array - res_analytic_array
diff_summary_stats <- compute_summary(diff_array)

# Convert to percentages
to_pct <- function(x) x * 100
discrete_summary   <- lapply(discrete_summary, to_pct)
analytic_summary   <- lapply(analytic_summary, to_pct)
diff_summary_stats <- lapply(diff_summary_stats, to_pct)

# == [K] Summary table of differences (numeric AMEs: discrete - analytic) ======

numeric_vars_present <- vars[vars %in% numeric_vars]

diff_summary <- lapply(1:length(numeric_vars_present), function(i) {
  var        <- numeric_vars_present[i]
  diff_vals  <- sapply(1:dim(res_discrete_array)[1], 
                       function(k) discrete_summary$mean[k, var] - analytic_summary$mean[k, var])
  c(mean  = mean(diff_vals, na.rm = TRUE),
    sd    = sd(diff_vals, na.rm = TRUE),
    lower = quantile(diff_vals, probs = 0.025, na.rm = TRUE),
    upper = quantile(diff_vals, probs = 0.975, na.rm = TRUE))
})

diff_summary <- data.frame(variable = numeric_vars_present, 
                           do.call(rbind, diff_summary)) %>%
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 4)))

# == [L] Build final AME table (all alternatives, numeric vs dummy logic) ======

# Initialize empty data.frame
AME_table <- data.frame(variable = vars, stringsAsFactors = FALSE)

for (alt_idx in 1:n_alts) {
  
  # Choose MEANS based on numeric vs dummy
  mean_vals  <- ifelse(vars %in% numeric_vars, 
                       analytic_summary$mean[alt_idx, ], 
                       discrete_summary$mean[alt_idx, ])
  
  # Standard deviations across draws
  sd_vals <- sapply(vars, function(v) {
    if (v %in% numeric_vars) {
      sd(res_analytic_array[alt_idx, v, ], na.rm = TRUE) * 100
    } else {
      sd(res_discrete_array[alt_idx, v, ], na.rm = TRUE) * 100
    }
  })
  
  # Confidence intervals
  lower_vals <- ifelse(vars %in% numeric_vars, 
                       analytic_summary$lower[alt_idx, ], 
                       discrete_summary$lower[alt_idx, ])
  
  upper_vals <- ifelse(vars %in% numeric_vars, 
                       analytic_summary$upper[alt_idx, ], 
                       discrete_summary$upper[alt_idx, ])
  
  # Add columns
  AME_table[[paste0("alt", alt_idx, "_mean")]]  <- round(mean_vals, 4)
  AME_table[[paste0("alt", alt_idx, "_sd")]]    <- round(sd_vals, 4)
  AME_table[[paste0("alt", alt_idx, "_lower")]] <- round(lower_vals, 4)
  AME_table[[paste0("alt", alt_idx, "_upper")]] <- round(upper_vals, 4)
}

print(AME_table)

# == [M] Subset AME_table by alternative: AME1, AME2, AME3 =====================

# Function to subset AME_table by alternative
make_AME_subset <- function(alt_idx) {
  alt_cols <- grep(paste0("^alt", alt_idx, "_"), names(AME_table), value = TRUE)
  AME_table[, c("variable", alt_cols)]
}

# Subsets
AME1 <- make_AME_subset(1)
AME2 <- make_AME_subset(2)
AME3 <- make_AME_subset(3)

# Salve results
saveRDS(diff_summary,  file = here::here("AME_numeric_diff_summary.rds"))
saveRDS(AME_table,     file = here::here("AME_table_full.rds"))
saveRDS(AME1,          file = here::here("AME1.rds"))
saveRDS(AME2,          file = here::here("AME2.rds"))
saveRDS(AME3,          file = here::here("AME3.rds"))

# Save tables en .xls:

# Mapping of variables to sections
category_map <- data.frame(
  variable = c(
    "hh_foreign","hh_oneperson","hh_singleparent","hh_twoadultsalone","hh_twoadultsandchild",
    "hh_num_members","hh_num_minors","hh_propmale18","hh_meanage18",
    "hh_some_higheduc","hh_all_higheduc",
    "inc_1_to_2_thous","inc_2_to_3_thous","inc_3_to_5_thous","inc_more_5_thous",
    "ws_all_work","ws_none_work",
    "wp_myprovince","wp_otherplace","wp_ownhome",
    "com_20_to_39_min","com_40_to_59_min","com_more_1_hour",
    "com_num_trips",
    "geo_50_100_thous","geo_100_500_thous","geo_more_500_thous",
    "geo_barcelona","geo_madrid","geo_services",
    "h_secondhome","h_ownership","h_rental","h_park_slot","h_detached",
    "mob_other_vehi","mob_pmt_priv_car","mob_pmt_pub_trans","mob_pmt_moto"
  ),
  category = c(
    "Members born outside of Spain","Household structure","Household structure","Household structure","Household structure",
    "Number of household members","Number of minor household members","Proportion of males >18 years","Mean age >18 years",
    "High education","High education",
    "Income","Income","Income","Income",
    "Working status","Working status",
    "Working place","Working place","Working place",
    "Commutement time","Commutement time","Commutement time",
    "Number of trips",
    "Municipality size","Municipality size","Municipality size",
    "Metropolitan areas","Metropolitan areas","Services available",
    "Second home","Housing regime","Housing regime","Parking slot","Type of building",
    "Other vehicles","Principal mode of transportation","Principal mode of transportation","Principal mode of transportation"
  ),
  stringsAsFactors = FALSE
)

# Function to create mean + CI in a single cell (rounded to 2 decimals)
format_rows <- function(df){
  df %>%
    mutate(
      `No vehicle (alt1)`    = paste0(round(alt1_mean,2), "\n[", round(alt1_lower,2), ", ", round(alt1_upper,2), "]"),
      `Fuel vehicle (alt2)`  = paste0(round(alt2_mean,2), "\n[", round(alt2_lower,2), ", ", round(alt2_upper,2), "]"),
      `Clean vehicle (alt3)` = paste0(round(alt3_mean,2), "\n[", round(alt3_lower,2), ", ", round(alt3_upper,2), "]")
    ) %>%
    select(variable, 
           `No vehicle (alt1)`, 
           `Fuel vehicle (alt2)`, 
           `Clean vehicle (alt3)`)
}

# Build table with section headers
AME_with_sections <- category_map %>%
  group_by(category) %>%
  group_modify(~{
    section_header <- data.frame(
      variable               = .y$category,
      `No vehicle (alt1)`    = "",
      `Fuel vehicle (alt2)`  = "",
      `Clean vehicle (alt3)` = "",
      stringsAsFactors = FALSE
    )
    vars_rows <- AME_table %>%
      filter(variable %in% .x$variable) %>%
      format_rows()
    bind_rows(section_header, vars_rows)
  }) %>%
  ungroup() %>%
  rename(Variable = variable)

# Save to Excel with formatting
save_path <- "D:/Educacio/05_PHD/1_Papers/1_EV_Ownership/03_Review/AME_OR_Scripts_Petr/AME_table_final.xlsx"
wb <- createWorkbook()
addWorksheet(wb, "AME Table")
writeData(wb, "AME Table", AME_with_sections, rowNames = FALSE)

# Apply wrap text for all cells
wrapStyle <- createStyle(wrapText = TRUE, valign = "top")
addStyle(wb, "AME Table", style = wrapStyle, rows = 1:(nrow(AME_with_sections)+1), cols = 1:4, gridExpand = TRUE)

# Bold section headers (rows where Variable equals a category)
section_rows <- which(AME_with_sections$Variable %in% unique(category_map$category))
headerStyle <- createStyle(textDecoration = "bold")
addStyle(wb, "AME Table", 
         style = headerStyle, 
         rows  = section_rows + 1, 
         cols  = 1:4, gridExpand = TRUE) # +1 because Excel starts at row 1

# Adjust column widths
setColWidths(wb, "AME Table", cols = 1:4, widths = "auto")

# Save workbook
saveWorkbook(wb, save_path, overwrite = TRUE)
file.exists(save_path)

# == [N] AME3 plot =============================================================

# Prepare Data for Plotting ----
label_map <- c(
  "inc_more_5_thous"     = ">5000€",
  "inc_1_to_2_thous"     = "1000€ to 2000€",
  "inc_2_to_3_thous"     = "2000€ to 3000€",
  "mob_other_vehi"       = ">1 vehicle",
  "geo_madrid"           = "Madrid",
  "hh_all_higheduc"      = "High educ (all)",
  "geo_50_100_thous"     = "50k–100k",
  "mob_pmt_moto"         = "Motorbike",
  "inc_3_to_5_thous"     = "3000€ to 5000€",
  "wp_ownhome"           = "Own home",
  "h_park_slot"          = "Parking slot",
  "geo_barcelona"        = "Barcelona",
  "geo_more_500_thous"   = ">500k",
  "hh_num_members"       = "No. of members",
  "hh_foreign"           = "Foreign members",
  "hh_some_higheduc"     = "High educ (some)",
  "h_detached"           = "Detached house",
  "ws_none_work"         = "None work",
  "wp_myprovince"        = "Own province",
  "h_secondhome"         = "Second home",
  "hh_oneperson"         = "One person",
  "mob_pmt_pub_trans"    = "Public transp.",
  "h_rental"             = "Rented",
  "hh_twoadultsalone"    = "Two adults",
  "mob_pmt_priv_car"     = "Private car",
  "hh_twoadultsandchild" = "Couple w/ 1 child",
  "com_40_to_59_min"     = "40–59 min.",
  "wp_otherplace"        = "Other places",
  "com_20_to_39_min"     = "20–39 min.",
  "geo_100_500_thous"    = "100k–500k",
  "geo_services"         = "Basic services",
  "hh_propmale18"        = "Male % >18",
  "com_more_1_hour"      = ">1h",
  "ws_all_work"          = "All work",
  "com_num_trips"        = "No. of trips",
  "hh_num_minors"        = "No. of minors",
  "h_ownership"          = "Owned",
  "hh_singleparent"      = "Single parent",
  "hh_meanage18"         = "Mean age (≥18)"
)

# Filter out NAs
AME3_plot <- AME3 %>%
  filter(!is.na(alt3_mean))

# Prepare plot_data
plot_data <- AME3_plot %>%
  mutate(
    group = case_when(
      variable %in% c("inc_more_5_thous","inc_1_to_2_thous","inc_2_to_3_thous","inc_3_to_5_thous") ~ "Income",
      variable %in% c("mob_other_vehi","mob_pmt_moto","mob_pmt_pub_trans","mob_pmt_priv_car") ~ "Principal mode \n of transportation",
      variable %in% c("hh_all_higheduc","hh_num_members","hh_foreign","hh_some_higheduc","hh_propmale18","hh_num_minors","hh_meanage18") ~ "Sociodemographic \n characteristics",
      variable %in% c("geo_50_100_thous","geo_more_500_thous","geo_100_500_thous","geo_services","geo_madrid","geo_barcelona") ~ "Location",
      variable %in% c("wp_ownhome","wp_myprovince","wp_otherplace","ws_none_work","ws_all_work") ~ "Working place \n and status",
      variable %in% c("h_park_slot","h_detached","h_secondhome","h_rental","h_ownership") ~ "Dwelling \n characteristics",
      variable %in% c("hh_oneperson","hh_twoadultsalone","hh_twoadultsandchild","hh_singleparent") ~ "Family structure",
      TRUE ~ "Commutement \n habits"
    ),
    significant    = alt3_lower > 0 | alt3_upper < 0,
    variable_label = recode(variable, !!!label_map)
  ) %>%
  group_by(group) %>%
  mutate(variable_label = fct_reorder(variable_label, alt3_mean)) %>%
  ungroup()

# Plot
plot <- ggplot(plot_data, aes(x = alt3_mean, y = variable_label)) +
  geom_vline(xintercept = 0, 
             color = "red", 
             linetype = "dashed", 
             linewidth = 0.4) +
  geom_errorbarh(aes(xmin = alt3_lower, 
                     xmax = alt3_upper), 
                 height = 0.15, 
                 linewidth = 0.35, 
                 color = "black") +
  geom_point(aes(shape = significant, 
                 fill = significant), 
             size = 2.4, 
             color = "black") +
  scale_shape_manual(values = c("TRUE" = 21, "FALSE" = 21), guide = "none") +
  scale_fill_manual(values = c("TRUE" = "black", "FALSE" = "white"), guide = "none") +
  facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(
    title = "Average Marginal Effects on the probability of choosing a clean vehicle",
    subtitle = "95% CIs (1000 Parametric Monte Carlo Simulations)*",
    x = "AME (percentage points)",
    y = NULL,
    caption = "*AMEs for binary variables report the discrete change in predicted probability from 0 to 1. 
    For continuous variables, AMEs report the average analytic derivative of the predicted probability with respect to each variable, evaluated at every observation*"
  ) +
  scale_x_continuous(
    breaks = seq(floor(min(plot_data$alt3_lower)), ceiling(max(plot_data$alt3_upper)), by = 0.25),
    labels = scales::number_format(accuracy = 0.01),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  theme_minimal(base_size = 13, base_family = "Times New Roman") +
  theme(
    plot.title         = element_text(face = "bold", size = 14, margin = margin(b = 4), hjust = 0.5),
    plot.subtitle      = element_text(size = 11, margin = margin(b = 12), hjust = 0.5),
    axis.text.y        = element_text(size = 9, color = "black"),
    axis.text.x        = element_text(size = 8.5, color = "black"),
    axis.title.x       = element_text(size = 11, margin = margin(t = 8)),
    panel.spacing.y    = unit(0.9, "lines"),
    panel.background   = element_rect(fill = "grey98", color = NA),
    panel.grid.major.y = element_line(color = "grey90", size = 0.25),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", size = 0.25),
    panel.grid.minor.x = element_blank(),
    strip.text.y.left = element_text(face = "bold", size = 9, angle = 0, 
                                     hjust = 0.5, vjust = 0.5, color = "black"),
    strip.background = element_rect(fill = "grey93", color = NA),
    axis.ticks.y = element_blank(),
    plot.caption = element_text(size = 8, color = "grey20",
                                hjust = 0, margin = margin(t = 12, b = 4)),
    plot.caption.position = "plot",
    plot.margin = margin(t = 20, r = 40, b = 25, l = 40)
  ) +
  ggh4x::force_panelsizes(
    rows = unit(c(0.5, 0.6, 0.5, 0.5, 0.6, 0.5, 0.75, 0.6), "in"),
    respect = TRUE
  )

if(file.exists("plot.pdf")) file.remove("plot.pdf")


ggsave(
  "plot.pdf",
  plot = plot,
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300,
  device = pdf
)

ggsave(
  filename = "plot.png",
  plot = plot,
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300
)

# 6. END #######################################################################

# Track errors
traceback()

# Set end time
end_time <- Sys.time()

# Execution time
time = end_time - start.time
print(time) 

# Last run time: ~100h
