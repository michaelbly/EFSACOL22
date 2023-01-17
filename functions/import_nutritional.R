######################
#
# read data from excel file
#df <- read_excel("input/raw_data/raw_dataset_efsa22_240622.xlsx")
df_n <- read.csv("data/raw_data/nutrition/efsacol_nutrition_clean_081222.csv", sep = ";"
               , comment.char = "", strip.white = TRUE,
               stringsAsFactors = TRUE, encoding="UTF-8-BOM")
names(df_n)[names(df_n) == 'ï..Apl'] <- "Apl"


#############################
# change column names
#change_names <- read_excel("Input/Codebook_Questionnaire_EFSA22.xlsx", sheet = "colnames")
change_names_n <- read.csv("data/change_names/Codebook_NUTRITION_EFSA22.csv", sep=";", 
                         fileEncoding="UTF-8-BOM")
names(df_n) <- plyr::mapvalues(names(df_n), from = change_names_n$old_name, to = change_names_n$new_name)
table(change_names_n$new_name %in% names(df_n))

`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}
if(any(!(change_names_n$new_name %in% names(df_n)))){
  warning("some names present in  not found in change_names dataset")
  warning(which(!(change_names_n$new_name %in% names(df_n))) %>% length)
}
change_names_n$new_name %find those not in% names(df_n)



############################
# replace accented letters with regular ones
accented_letters <- function (x){
  stri_replace_all_fixed(x,
                         c("á","é","ń","í","ó","ú","ñ","ü","Á","Ó","Í","Ñ"),
                         c("a","e","n","i","o","u","n","u","A","O","I","N"),
                         vectorize_all = FALSE)}

df_n <- rapply(df_n, f = accented_letters, classes = c("factor", "character"), how = "replace")
df_n <- rapply(df_n, f = accented_letters, classes = c("factor", "character"), how = "replace")



#############################
# set string to lower case and replace everything that is not alphanumeric or underscore by a dot "."
to_alphanumeric_lowercase <- function (x)
{
  tolower(gsub("[^a-zA-Z0-9_]", "\\_", x))
}

df_n <- rapply(df_n, f = to_alphanumeric_lowercase, classes = c("factor", "character"), how = "replace")
table(sapply(df_n, is.numeric))
check_numeric <- df_n[ , purrr::map_lgl(df_n, is.numeric)]


df_n <- df_n %>% setNames(make.names(names(.)))

df_n <- df_n %>% dplyr::select(-matches("X"))

df_n$departamento[df_n$departamento == "bogota__d_c_"] <- "bogota_dc"



##########################
#CONSUMO ALIMENTOS 
df_n$nr_veces_consumo_ninos023_leche_tarro <- sub("_", ",", df_n$nr_veces_consumo_ninos023_leche_tarro)
df_n$nr_veces_consumo_ninos023_leche_vaca <- sub("_", ",", df_n$nr_veces_consumo_ninos023_leche_vaca)
df_n$nr_veces_consumo_ninos023_yogurt <- sub("_", ",", df_n$nr_veces_consumo_ninos023_yogurt)

df_n$nr_veces_consumo_ninos2459_leche_tarro <- sub("_", ",", df_n$nr_veces_consumo_ninos2459_leche_tarro)
df_n$nr_veces_consumo_ninos2459_leche_vaca <- sub("_", ",", df_n$nr_veces_consumo_ninos2459_leche_vaca)
df_n$nr_veces_consumo_ninos2459_yogurt <- sub("_", ",", df_n$nr_veces_consumo_ninos2459_yogurt)


#write.xlsx2(df_n, "output/dataset/dataset_nutrition_EFSA_COL_201222.xlsx")

