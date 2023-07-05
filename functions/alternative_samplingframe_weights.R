samplingframe <- load_samplingframe("data/sampling_frame/strata_population_2.csv")

####################################################
#PREPARE SAMPLINGFRAMES
###################################################

##CLUSTER SAMPLING FRAME: MAKE VALUES THAT CAN BE MATCHED WITH DATA
samplingframe$stratum <- paste(samplingframe$departamento, samplingframe$urbano_rural, sep = "_")


#CREATE ATN STRATA
df$departamento <- ifelse(df$departamento %in% c("vichada", "vaupes", "amazonas"), "atn", 
                                                df$departamento)

## CREATE STRATA BASED ON CNC SAMPLING
df <- df %>% dplyr::mutate(strata1 = case_when(
  df$urbano_rural == "urbano" & df$municipio %in% c("arauca", "armenia", "barranquilla", 
                                                    "bucaramanga", "cali", "cartagena_de_indias", 
                                                    "florencia", "ibague", "manizales", "medellin", 
                                                    "monteria", "neiva", "pasto", "pereira", "popayan", "quibdo", 
                                                    "riohacha", "san_jose_de_cucuta", "santa_marta", "sincelejo", 
                                                    "tunja", "valledupar", "villavicencio", "yopal") ~ "capital",
  
  df$urbano_rural == "urbano" & !(df$municipio %in% c("arauca", "armenia", "barranquilla", 
                                                      "bucaramanga", "cali", "cartagena_de_indias", 
                                                    "florencia", "ibague", "manizales", "medellin", 
                                                    "monteria", "neiva", "pasto", "pereira", "popayan", "quibdo", 
                                                    "riohacha", "san_jose_de_cucuta", "santa_marta", "sincelejo",
                                                    "tunja", "valledupar", "villavicencio", "yopal")) ~ "urbano",
  
  df$urbano_rural == "rural" ~ "rural"))


df$departamento_region <- ifelse(df$urbano_rural == "rural", df$regiones_sampling, 
                                 df$departamento)


df <- df %>% 
  mutate(strata = paste(departamento_region,strata1, sep ="_"))



##CHECK IF ALL MATCH SAMPLINGFRAME:
`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}

if(any(!(df$strata %in% samplingframe$stratum))){
  warning("some strata not found in samplingframe")
  warning(which(!(df$strata %in% samplingframe$stratum)) %>% length)
}
df$strata %find those not in% samplingframe$stratum


if(any(is.na(df$strata))){
  warning("strata can not be NA")
}



####################################################
# WEIGHTS CALCULATION NATIONAL LEVEL AGGREGATION
###################################################
# CALCULATE STRATA WEIGHTS FOR NATIONAL AGGREGATION
strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "stratum",
                                      data.stratum.column = "strata",
                                      data = df)


weight_fun <-strata_weight_fun
df$weights_nacional<- weight_fun(df)



#CREATE NEW FUNCTION FOR WEIGHTING
weight_fun_national <-function(df){
  df$weights_nacional
}




##############################################
# DEPARTMENT LEVEL WEIGHTS
#############################################
samplingframe_department <- load_samplingframe("data/sampling_frame/strata_population_3.csv")

##CLUSTER SAMPLING FRAME: MAKE VALUES THAT CAN BE MATCHED WITH DATA
samplingframe_department$stratum <- paste(samplingframe_department$departamento, samplingframe_department$urbano_rural, sep = "_")




df <- df %>% 
  mutate(strata_departamento = paste(departamento,strata1, sep ="_"))




##CHECK IF ALL MATCH SAMPLINGFRAME:
`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}

if(any(!(samplingframe_department$stratum %in% df$strata_departamento))){
  warning("some strata not found in samplingframe")
  warning(which(!(samplingframe_department$stratum %in% df$strata_departamento)) %>% length)
}
samplingframe_department$stratum %find those not in% df$strata_departamento


if(any(is.na(df$strata_departamento))){
  warning("strata can not be NA")
}



####################################################
# WEIGHTS CALCULATION DEPARTAMENTAL LEVEL AGGREGATION
###################################################
# CALCULATE STRATA WEIGHTS
strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe_department,
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "stratum",
                                      data.stratum.column = "strata_departamento",
                                      data = df)


weight_fun <-strata_weight_fun
df$weights_departamento<- weight_fun(df)



#CREATE NEW FUNCTION FOR WEIGHTING
weight_fun_departamento <-function(df){
  df$weights_departamento
}



##############################################
# IMPORT LOOKUP TABLE
#############################################
lookup_table <- read.csv("data/lookup_tables/lookup_table_names.csv", sep = ";"
                         , comment.char = "", strip.white = TRUE,
                         stringsAsFactors = TRUE, encoding="UTF-8-BOM")


