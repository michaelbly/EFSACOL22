source("functions/pre_processing.R")

assessment_start_date <- as.Date("2022-09-21")

delete_time_limit <- 20
flag_time_limit <- 35

######################
#
# read data from excel file
#df <- read_excel("input/raw_data/raw_dataset_efsa22_240622.xlsx")
df <- read.csv("data/raw_data/efsacol_dataset_021222.csv", sep = ";"
               , comment.char = "", strip.white = TRUE,
               stringsAsFactors = TRUE, encoding="UTF-8-BOM")
names(df)[names(df) == 'ï..Apl'] <- "Apl"


#############################
# change column names
#change_names <- read_excel("Input/Codebook_Questionnaire_EFSA22.xlsx", sheet = "colnames")
change_names <- read.csv("data/change_names/Codebook_Questionnaire_EFSA22.csv", sep=";", 
                         fileEncoding="UTF-8-BOM")
names(df) <- plyr::mapvalues(names(df), from = change_names$old_name, to = change_names$new_name)
table(change_names$new_name %in% names(df))

`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}
if(any(!(change_names$new_name %in% names(df)))){
  warning("some names present in  not found in change_names dataset")
  warning(which(!(change_names$new_name %in% names(df))) %>% length)
}
change_names$new_name %find those not in% names(df)



############################
# replace accented letters with regular ones
accented_letters <- function (x){
  stri_replace_all_fixed(x,
                         c("á","é","ń","í","ó","ú","ñ","ü","Á","Ó","Í","Ñ"),
                         c("a","e","n","i","o","u","n","u","A","O","I","N"),
                         vectorize_all = FALSE)}

df <- rapply(df, f = accented_letters, classes = c("factor", "character"), how = "replace")
df <- rapply(df, f = accented_letters, classes = c("factor", "character"), how = "replace")



#############################
# set string to lower case and replace everything that is not alphanumeric or underscore by a dot "."
to_alphanumeric_lowercase <- function (x)
{
  tolower(gsub("[^a-zA-Z0-9_]", "\\_", x))
}

df <- rapply(df, f = to_alphanumeric_lowercase, classes = c("factor", "character"), how = "replace")
table(sapply(df, is.numeric))
check_numeric <- df[ , purrr::map_lgl(df, is.numeric)]


############################
# remove interviews that are market rechazado or en curso
df <- filter(df, 
                  estado == "finalizada__mobinet_")


#############################
# rename bogota__d_c_ to bogota_dc
df$departamento[df$departamento == "bogota__d_c_"] <- "bogota_dc"
df$departamento[df$departamento == "archipi_lago_de_san_andr_s__providencia_y_santa_catalina"] <- "archipielago_de_san_andres"
df$departamento[df$departamento == "vaup_s"] <- "vaupes"



####################################
# when survey does not continue to LCS section, survey is considered incomplete
df <- df %>% 
  mutate(not_eligible = case_when(lcs_sacar_ninos_escuela == "" ~ "not_eligible",
                                  TRUE ~ "eligible"))
table(df$not_eligible)


####################################
# change date format
df$fecha_in <- gsub("sep", "09", df$fecha_in)
df$fecha_in <- gsub("oct", "10", df$fecha_in)
df$fecha_in <- gsub("nov", "11", df$fecha_in)
df$fecha_in <- gsub("_22", "_2022", df$fecha_in)

df$date_assessment <- strptime(as.character(df$fecha_in), "%d_%m_%Y")
df$date_assessment <-  format(df$date, "%Y-%m-%d")



####################################
############################
# calculate mean interview duration by enumerator
df$duracion_min <- df$duracion / 60

df %>% 
  group_by(entrevistador) %>%
  summarise_at(vars(duracion_min), list(name = mean))
df$duracion_min <- round(df$duracion_min, 1)


# flag surveys that were below the time limit
df <- df %>% 
  mutate(time_validity = case_when(duracion_min < flag_time_limit ~ "Flagged", 
                                   duracion_min < delete_time_limit ~ "Delete",
                                   TRUE ~ "valid")) %>%
  mutate_if(is.numeric, ~na_if(.,99))




########################################################################################
###########################################################################################################
# calculate new variables
# number of NAs check
df$NAs <- apply(df,1,FUN=function(x){length(which(is.na(x)))})
df$NAs


#import samplingframe
samplingframe_sample <- read.csv("data/muestra_departamento.csv", sep = ";"
               , comment.char = "", strip.white = TRUE,
               stringsAsFactors = TRUE, encoding="UTF-8-BOM")

samplingframe_sample <- rapply(samplingframe_sample, f = accented_letters, classes = c("factor", "character"), how = "replace")
samplingframe_sample <- rapply(samplingframe_sample, f = to_alphanumeric_lowercase, classes = c("factor", "character"), how = "replace")
samplingframe_sample$departamento[samplingframe_sample$departamento == "bogota__d_c_"] <- "bogota_dc"


# merge with df to count nutrition surveys
df_n$count_nutrition <- 1

df <- df_n %>% select(registro, count_nutrition) %>%
                  right_join(df, by = "registro")




#import categories classification for analysis disaggregations
categories_disagregations <- read.csv("data/change_names/categories_disagregations.csv", sep=";", 
                         fileEncoding="UTF-8-BOM")



