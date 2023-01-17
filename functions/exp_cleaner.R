# CLEAN ALL GASTOS VALUES ACCORDING TO TITO'S GUIDANCE
expenditure_cleaner_30d <- function(r) {
  
  
  r$gastos_cereales <- ifelse(is.na(r$gastos_cereales) | (r$gastos_cereales > 20*sd(r$gastos_cereales, na.rm = T)), 
                              round(mean(r$gastos_cereales, na.rm = T),0), r$gastos_cereales)
  
  r$gastos_tuberculos <- ifelse(is.na(r$gastos_tuberculos) | (r$gastos_tuberculos > 20*sd(r$gastos_tuberculos, na.rm = T)), 
                                round(mean(r$gastos_tuberculos, na.rm = T),0), r$gastos_tuberculos)
  
  r$gastos_legumbres <- ifelse(is.na(r$gastos_legumbres) | (r$gastos_legumbres > 20*sd(r$gastos_legumbres, na.rm = T)), 
                               round(mean(r$gastos_legumbres, na.rm = T),0), r$gastos_legumbres)
  
  r$gastos_vegetales <- ifelse(is.na(r$gastos_vegetales) | (r$gastos_vegetales > 20*sd(r$gastos_vegetales, na.rm = T)), 
                               round(mean(r$gastos_vegetales, na.rm = T),0), r$gastos_vegetales)
  
  r$gastos_frutas <- ifelse(is.na(r$gastos_frutas) | (r$gastos_frutas > 20*sd(r$gastos_frutas, na.rm = T)), 
                            round(mean(r$gastos_frutas, na.rm = T),0), r$gastos_frutas)
  
  r$gastos_carne <- ifelse(is.na(r$gastos_carne) | (r$gastos_carne > 20*sd(r$gastos_carne, na.rm = T)), 
                           round(mean(r$gastos_carne, na.rm = T),0), r$gastos_carne)
  
  r$gastos_pescado <- ifelse(is.na(r$gastos_pescado) | (r$gastos_pescado > 20*sd(r$gastos_pescado, na.rm = T)), 
                             round(mean(r$gastos_pescado, na.rm = T),0), r$gastos_pescado)
  
  r$gastos_huevos <- ifelse(is.na(r$gastos_huevos) | (r$gastos_huevos > 20*sd(r$gastos_huevos, na.rm = T)), 
                            round(mean(r$gastos_huevos, na.rm = T),0), r$gastos_huevos)
  
  r$gastos_aceite <- ifelse(is.na(r$gastos_aceite) | (r$gastos_aceite > 20*sd(r$gastos_aceite, na.rm = T)), 
                            round(mean(r$gastos_aceite, na.rm = T),0), r$gastos_aceite)
  
  r$gastos_leche <- ifelse(is.na(r$gastos_leche) | (r$gastos_leche > 20*sd(r$gastos_leche, na.rm = T)), 
                           round(mean(r$gastos_leche, na.rm = T),0), r$gastos_leche)
  
  r$gastos_azucar <- ifelse(is.na(r$gastos_azucar) | (r$gastos_azucar > 20*sd(r$gastos_azucar, na.rm = T)), 
                            round(mean(r$gastos_azucar, na.rm = T),0), r$gastos_azucar)
  
  r$gastos_condimentos <- ifelse(is.na(r$gastos_condimentos) | (r$gastos_condimentos > 20*sd(r$gastos_condimentos, na.rm = T)), 
                                 round(mean(r$gastos_condimentos, na.rm = T),0), r$gastos_condimentos)
  
  r$gastos_bebidas_non_alcoholicas <- ifelse(is.na(r$gastos_bebidas_non_alcoholicas) | (r$gastos_bebidas_non_alcoholicas > 20*sd(r$gastos_bebidas_non_alcoholicas, na.rm = T)), 
                                             round(mean(r$gastos_bebidas_non_alcoholicas, na.rm = T),0), r$gastos_bebidas_non_alcoholicas)
  
  r$gastos_comida_fuera_casa <- ifelse(is.na(r$gastos_comida_fuera_casa) | (r$gastos_comida_fuera_casa > 20*sd(r$gastos_comida_fuera_casa, na.rm = T)), 
                                       round(mean(r$gastos_comida_fuera_casa, na.rm = T),0), r$gastos_comida_fuera_casa)
  
  r$gastos_agua_beber <- ifelse(is.na(r$gastos_agua_beber) | (r$gastos_agua_beber > 20*sd(r$gastos_agua_beber, na.rm = T)), 
                                round(mean(r$gastos_agua_beber, na.rm = T),0), r$gastos_agua_beber)
  
  r$gastos_agua_domestico <- ifelse(is.na(r$gastos_agua_domestico) | (r$gastos_agua_domestico > 20*sd(r$gastos_agua_domestico, na.rm = T)), 
                                    round(mean(r$gastos_agua_domestico, na.rm = T),0), r$gastos_agua_domestico)
  
  r$gastos_renta <- ifelse(is.na(r$gastos_renta) | (r$gastos_renta > 20*sd(r$gastos_renta, na.rm = T)), 
                           round(mean(r$gastos_renta, na.rm = T),0), r$gastos_renta)
  
  r$gastos_electricidad <- ifelse(is.na(r$gastos_electricidad) | (r$gastos_electricidad > 20*sd(r$gastos_electricidad, na.rm = T)), 
                                  round(mean(r$gastos_electricidad, na.rm = T),0), r$gastos_electricidad)
  
  r$gastos_basura <- ifelse(is.na(r$gastos_basura) | (r$gastos_basura > 20*sd(r$gastos_basura, na.rm = T)), 
                            round(mean(r$gastos_basura, na.rm = T),0), r$gastos_basura)
  
  r$gastos_higiene <- ifelse(is.na(r$gastos_higiene) | (r$gastos_higiene > 20*sd(r$gastos_higiene, na.rm = T)), 
                             round(mean(r$gastos_higiene, na.rm = T),0), r$gastos_higiene)
  
  r$gastos_transporte <- ifelse(is.na(r$gastos_transporte) | (r$gastos_transporte > 20*sd(r$gastos_transporte, na.rm = T)), 
                                round(mean(r$gastos_transporte, na.rm = T),0), r$gastos_transporte)
  
  r$gastos_comunicacion <- ifelse(is.na(r$gastos_comunicacion) | (r$gastos_comunicacion > 20*sd(r$gastos_comunicacion, na.rm = T)), 
                                  round(mean(r$gastos_comunicacion, na.rm = T),0), r$gastos_comunicacion)
  
  r$gastos_lena <- ifelse(is.na(r$gastos_lena) | (r$gastos_lena > 20*sd(r$gastos_lena, na.rm = T)), 
                          round(mean(r$gastos_lena, na.rm = T),0), r$gastos_lena)
  
  r$gastos_gasolina <- ifelse(is.na(r$gastos_gasolina) | (r$gastos_gasolina > 20*sd(r$gastos_gasolina, na.rm = T)), 
                              round(mean(r$gastos_gasolina, na.rm = T),0), r$gastos_gasolina)
  
  r$gastos_otros <- ifelse(is.na(r$gastos_otros) | (r$gastos_otros > 20*sd(r$gastos_otros, na.rm = T)), 
                           round(mean(r$gastos_otros, na.rm = T),0), r$gastos_otros)
  

  return(r)
}



expenditure_cleaner_6m <- function(r) {
  
  ###########################
  # gastos de 6 meses
  r$gastos_medicos <- ifelse(is.na(r$gastos_medicos) | (r$gastos_medicos > 20*sd(r$gastos_medicos, na.rm = T)), 
                             round(mean(r$gastos_medicos, na.rm = T),0), r$gastos_medicos)
  
  r$gastos_vestimenta <- ifelse(is.na(r$gastos_vestimenta) | (r$gastos_vestimenta > 20*sd(r$gastos_vestimenta, na.rm = T)), 
                                round(mean(r$gastos_vestimenta, na.rm = T),0), r$gastos_vestimenta)
  
  r$gastos_educacion <- ifelse(is.na(r$gastos_educacion) | (r$gastos_educacion > 20*sd(r$gastos_educacion, na.rm = T)), 
                               round(mean(r$gastos_educacion, na.rm = T),0), r$gastos_educacion)
  
  r$gastos_deudas <- ifelse(is.na(r$gastos_deudas) | (r$gastos_deudas > 20*sd(r$gastos_deudas, na.rm = T)), 
                            round(mean(r$gastos_deudas, na.rm = T),0), r$gastos_deudas)
  
  r$gastos_insumos <- ifelse(is.na(r$gastos_insumos) | (r$gastos_insumos > 20*sd(r$gastos_insumos, na.rm = T)), 
                             round(mean(r$gastos_insumos, na.rm = T),0), r$gastos_insumos)
  
  r$gastos_construccion <- ifelse(is.na(r$gastos_construccion) | (r$gastos_construccion > 20*sd(r$gastos_construccion, na.rm = T)), 
                                  round(mean(r$gastos_construccion, na.rm = T),0), r$gastos_construccion)
  
  r$gastos_seguros <- ifelse(is.na(r$gastos_seguros) | (r$gastos_seguros > 20*sd(r$gastos_seguros, na.rm = T)), 
                             round(mean(r$gastos_seguros, na.rm = T),0), r$gastos_seguros)
  
  r$gastos_textiles <- ifelse(is.na(r$gastos_textiles) | (r$gastos_textiles > 20*sd(r$gastos_textiles, na.rm = T)), 
                              round(mean(r$gastos_textiles, na.rm = T),0), r$gastos_textiles)
  
  
  
  return(r)
}