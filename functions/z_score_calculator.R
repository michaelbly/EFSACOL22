library(anthro)
r <- response_nutritional


r$final_peso_ninos023 <-  gsub("_", ".", r$final_peso_ninos023, fixed = TRUE)
r$final_peso_ninos2459 <-  gsub("_", ".", r$final_peso_ninos2459, fixed = TRUE)
r$final_peso_ninos059 <- ifelse(r$final_peso_ninos023 != "", r$final_peso_ninos023, 
                                   r$final_peso_ninos2459)
r$final_peso_ninos059 <- as.numeric(as.character(r$final_peso_ninos059))


r$final_talla_ninos023 <-  gsub("_", ".", r$final_talla_ninos023, fixed = TRUE)
r$final_talla_ninos2459 <-  gsub("_", ".", r$final_talla_ninos2459, fixed = TRUE)
r$final_talla_ninos059 <- ifelse(r$final_talla_ninos023 != "", r$final_talla_ninos023, 
                                   r$final_talla_ninos2459)
r$final_talla_ninos059 <- as.numeric(as.character(r$final_talla_ninos059))


r$edad_meses_ninos023 <-  gsub("_", ".", r$edad_meses_ninos023, fixed = TRUE)
r$edad_meses_ninos2459 <-  gsub("_", ".", r$edad_meses_ninos02459, fixed = TRUE)
r$edad_meses_ninos059 <- ifelse(r$edad_meses_ninos023 != "", r$edad_meses_ninos023, 
                                    r$edad_meses_ninos2459)
r$edad_meses_ninos059 <- as.numeric(as.character(r$edad_meses_ninos059))



#######################################
# calculate the z-scores for children 023
names(r)
r <- r %>% mutate(sexo = case_when(
  genero == 1 ~ "m",
  genero == 2 ~ "f",
))
r$measurement <- "h"

growth_zscores059 <-  anthro_zscores(
  sex = r$sexo,
  age = r$edad_meses_ninos059,
  is_age_in_month = TRUE,
  weight = r$final_peso_ninos059,
  lenhei = r$final_talla_ninos059
)

#select zlen (length for age), zwei (weight for age), zwfl (weight for length), zbmi (bmi for age)
growth_zscores059 <- growth_zscores059[,c("zlen", "zwei", "zwfl", "zbmi")]
r <- cbind(r, growth_zscores059)

r <- r
#WEIGHT FOR HEIGHT
r <- r %>% dplyr::mutate(weight_height = case_when(
   r$zwfl > 3 ~ "obesity",
   r$zwfl <= 3 & r$zwfl > 2 ~ "sobrepeso",
   r$zwfl <= 2 & r$zwfl > 1 ~ "riesgo_sobrepeso",
   r$zwfl <= 1 & r$zwfl >= -1 ~ "peso_adecuado",
   r$zwfl < -1 & r$zwfl >= -2 ~ "desnutricion_aguda",
   r$zwfl < -2 & r$zwfl >= -3 ~ "desnutricion_moderada",
   r$zwfl < -3 ~ "desnutricion_severa"))


#WEIGHT FOR HEIGHT
#Obesity> 3
r$wh1 <- ifelse(r$zwfl > 3,1,0)
#Sobrepeso <= 3 & > 2
loop_nutri_ninos059$wh2 <- ifelse(loop_nutri_ninos059$zwfl <= 3 & loop_nutri_ninos059$zwfl > 2,1,0)
#riesgo de sobrepeso <= 2 & >1
loop_nutri_ninos059$wh3 <- ifelse(loop_nutri_ninos059$zwfl <= 2 & loop_nutri_ninos059$zwfl > 1,1,0)
#peso adecuado para la talla <=1 & >=-1
loop_nutri_ninos059$wh4 <- ifelse(loop_nutri_ninos059$zwfl <= 1 & loop_nutri_ninos059$zwfl >= -1,1,0)
#riesgo de desnutricion aguda <-1 & >= -2
loop_nutri_ninos059$wh5 <- ifelse(loop_nutri_ninos059$zwfl < -1 & loop_nutri_ninos059$zwfl >= -2,1,0)
#desnutricion aguda moderada < -2 & >=-3
loop_nutri_ninos059$wh6 <- ifelse(loop_nutri_ninos059$zwfl < -2 & loop_nutri_ninos059$zwfl >= -3,1,0)
#desnutricion aguda severa < -3
loop_nutri_ninos059$wh7 <- ifelse(loop_nutri_ninos059$zwfl < -3,1,0)



gaggi <- r %>% select("weight_height", "clasificacion_peso_talla_ninos059")
