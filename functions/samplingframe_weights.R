samplingframe <- load_samplingframe("data/sampling_frame/strata_population.csv")


#PREPARE SAMPLINGFRAMES

##CLUSTER SAMPLING FRAME: MAKE VALUES THAT CAN BE MATCHED WITH DATA
samplingframe$stratum <- paste(samplingframe$departamento, samplingframe$urbano_rural, sep = "_")


#ADD STRATA NAMES TO DATA 

## create strata for response dataset
df <- df %>% 
  mutate(strata = paste(departamento,urbano_rural, sep ="_"))




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
# WEIGHTS CALCULATION
###################################################
# CALCULATE STRATA WEIGHTS
strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "stratum",
                                      data.stratum.column = "strata",
                                      data = df)


weight_fun <-strata_weight_fun
df$weights<- weight_fun(df)



#CREATE NEW FUNCTION FOR WEIGHTING
weight_fun<-function(df){
  df$weights
}



##############################################
# IMPORT LOOKUP TABLE
#############################################
lookup_table <- read.csv("data/lookup_tables/lookup_table_names.csv", sep = ";"
               , comment.char = "", strip.white = TRUE,
               stringsAsFactors = TRUE, encoding="UTF-8-BOM")

