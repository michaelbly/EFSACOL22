recoding_nutrition <- function(nr) {

  
  
###############################################################
# NUTRICION NINOS 0/59
###############################################################
# create new classification category for children < 5 years
nr$clasificacion_peso_talla_ninos059 <- ifelse(nr$clasificacion_peso_talla_ninos023 == "_", 
                                               nr$clasificacion_peso_talla_ninos2459, 
                                               nr$clasificacion_peso_talla_ninos023)
nr$clasificacion_peso_talla_ninos059 <- ifelse(nr$clasificacion_peso_talla_ninos059 == "_", NA, 
                                               nr$clasificacion_peso_talla_ninos059)


nr$clasificacion_peso_edad_ninos059 <- ifelse(nr$clasificacion_peso_edad_ninos023 == "_", 
                                              nr$clasificacion_peso_edad_ninos2459, 
                                              nr$clasificacion_peso_edad_ninos023)
nr$clasificacion_peso_edad_ninos059 <- ifelse(nr$clasificacion_peso_edad_ninos059 == "_", NA, 
                                               nr$clasificacion_peso_edad_ninos059)


nr$clasificacion_pb_ninos059 <- ifelse(nr$clasificacion_pb_ninos023 == "_", 
                                       nr$clasificacion_pb_ninos2459, 
                                       nr$clasificacion_pb_ninos023)
nr$clasificacion_pb_ninos059 <- ifelse(nr$clasificacion_pb_ninos059 == "_", NA, 
                                              nr$clasificacion_pb_ninos059)


nr$clasificacion_imc_ninos059 <- ifelse(nr$clasificacion_imc_ninos023 == "_", 
                                        nr$clasificacion_imc_ninos2459, 
                                        nr$clasificacion_imc_ninos023)
nr$clasificacion_imc_ninos059 <- ifelse(nr$clasificacion_imc_ninos059 == "_", NA, 
                                       nr$clasificacion_imc_ninos059)


nr$clasificacion_talla_edad_ninos059 <- ifelse(nr$clasificacion_talla_edad_ninos023 == "_", 
                                               nr$clasificacion_talla_edad_ninos2459, 
                                               nr$clasificacion_talla_edad_ninos023)
nr$clasificacion_talla_edad_ninos059 <- ifelse(nr$clasificacion_talla_edad_ninos059 == "_", NA, 
                                        nr$clasificacion_talla_edad_ninos059)



nr$clasificacion_anemia_ninos059 <- ifelse(nr$clasificacion_anemia_ninos023 == "_", 
                                           nr$clasificacion_anemia_ninos2459, 
                                           nr$clasificacion_anemia_ninos023)
nr$clasificacion_anemia_ninos059 <- ifelse(nr$clasificacion_anemia_ninos059 == "_", NA, 
                                               nr$clasificacion_anemia_ninos059)



######################################################
# WEIGHT FOR HEIGHT
# % de ninos y ninas menores de 5 anos con obesidad
nr$n5 <- ifelse(nr$clasificacion_peso_talla_ninos059 == "obesidad",1,0)

# % de ninos y ninas menores de 5 anos con sobrepeso
nr$n6 <- ifelse(nr$clasificacion_peso_talla_ninos059 == "sobrepeso",1,0)

# % de ninos y ninas menores de 5 anos con peso adecuado para la talla
nr$n7 <- ifelse(nr$clasificacion_peso_talla_ninos059 == "riesgo_de_sobrepeso",1,0)

# % de ninos y ninas menores de 5 anos con peso adecuado para la talla
nr$n8 <- ifelse(nr$clasificacion_peso_talla_ninos059 == "peso_adecuado_para_la_talla",1,0)

# % de ninos y ninas menores de 5 anos con riesgo de desnutricion aguda
nr$n9 <- ifelse(nr$clasificacion_peso_talla_ninos059 == "riesgo_de_desnutricion_aguda",1,0)

# % de ninos y ninas menores de 5 anos con desnutricion aguda moderada
nr$n10 <- ifelse(nr$clasificacion_peso_talla_ninos059 == "desnutricion_aguda_moderada",1,0)

# % de ninos y ninas menores de 5 anos con desnutricion aguda severa
nr$n11 <- ifelse(nr$clasificacion_peso_talla_ninos059 == "desnutricion_severa",1,0)

# % de ninos y ninas menores de 5 anos con desnutricion
nr$n12 <- ifelse(nr$clasificacion_peso_talla_ninos059 %in% c("desnutricion_severa","desnutricion_aguda_moderada"),1,0)



######################################################
# HEIGHT FOR AGE
# % de ninos y ninas menores de 5 anos con talla adecuada para la edad
nr$n13 <- ifelse(nr$clasificacion_talla_edad_ninos059 == "talla_adecuada_para_la_edad",1,0)

# % de ninos y ninas menores de 5 anos con riesgo de talla baja
nr$n14 <- ifelse(nr$clasificacion_talla_edad_ninos059 == "riesgo_de_talla_baja",1,0)

# % de ninos y ninas menores de 5 anos con talla baja para la edad
nr$n15 <- ifelse(nr$clasificacion_talla_edad_ninos059 == "talla_baja_para_la_edad_o_retraso_de_talla",1,0)



######################################################
# BMI FOR AGE
# % de ninos y ninas menores de 5 anos con obesidad
nr$n16 <- ifelse(nr$clasificacion_imc_ninos059 == "obesidad",1,0)

# % de ninos y ninas menores de 5 anos con sobrepeso
nr$n17 <- ifelse(nr$clasificacion_imc_ninos059 == "sobrepeso",1,0)

# % de ninos y ninas menores de 5 anos con riesgo de sobrepeso
nr$n18 <- ifelse(nr$clasificacion_imc_ninos059 == "riesgo_de_sobrepeso",1,0)



######################################################
# WEIGHT FOR AGE
# % de ninos y ninas menores de 5 anos con peso adecuado para la edad
nr$n20 <- ifelse(nr$clasificacion_peso_edad_ninos059 == "peso_adecuado_para_la_edad",1,0)

# % de ninos y ninas menores de 5 anos con riesgo de desnutricion global
nr$n21 <- ifelse(nr$clasificacion_peso_edad_ninos059 == "desnutricion_global",1,0)

# % de ninos y ninas menores de 5 anos con desnutricion global
nr$n22 <- ifelse(nr$clasificacion_peso_edad_ninos059 == "riesgo_de_desnutricion_global",1,0)



###############################################################
# NUTRICION GESTANTES
###############################################################
# % de mujeres gestantes con anemia leve, moderada, severa
nr$clasificacion_anemia_gestante <- ifelse(nr$clasificacion_anemia_gestante == "_", NA, 
                                           nr$clasificacion_anemia_gestante)
nr$n28_i <- ifelse(nr$clasificacion_anemia_gestante == "1__anemia_leve",1,0)
nr$n28_ii <- ifelse(nr$clasificacion_anemia_gestante == "1__anemia_moderada",1,0)
nr$n28_iii <- ifelse(nr$clasificacion_anemia_gestante == "sin_anemia",1,0)



###############################################################
# NUTRICION MAYORES
###############################################################
nr$clasificacion_talla_imc_mayor <- ifelse(nr$clasificacion_talla_imc_mayor == "_", NA, 
                                           nr$clasificacion_talla_imc_mayor)
# % de mayores con desnutricion severa
nr$n33 <- ifelse(nr$clasificacion_talla_imc_mayor == "1__desnutricion_grave" | nr$clasificacion_talla_imc_mayor == "desnutricion_severa",1,0)

# % de mayores con desnutricion moderada
nr$n34 <- ifelse(nr$clasificacion_talla_imc_mayor == "desnutricion_moderada",1,0)

# % de mayores con desnutricion leve
nr$n35 <- ifelse(nr$clasificacion_talla_imc_mayor == "3__desnutricion_leve" | nr$clasificacion_talla_imc_mayor == "desnutricion_leve",1,0)

# % de mayores con peso insuficiente
nr$n36 <- ifelse(nr$clasificacion_talla_imc_mayor == "4__peso_insuficiente" | nr$clasificacion_talla_imc_mayor == "peso_insuficiente",1,0)

# % de mayores con normpeso
nr$n37 <- ifelse(nr$clasificacion_talla_imc_mayor == "5__normopeso" | nr$clasificacion_talla_imc_mayor == "normopeso",1,0)

# % de mayores con sobrepeso
nr$n38 <- ifelse(nr$clasificacion_talla_imc_mayor == "6__sobrepeso"  | nr$clasificacion_talla_imc_mayor == "6_sobrepeso" | nr$clasificacion_talla_imc_mayor == "sobrepeso",1,0)

# % de mayores con obesidad grado I
nr$n39 <- ifelse(nr$clasificacion_talla_imc_mayor == "7__obesidad_grado_i" | nr$clasificacion_talla_imc_mayor == "obesidad_grado_i",1,0)

# % de mayores con obesidad grado II
nr$n40 <- ifelse(nr$clasificacion_talla_imc_mayor == "8__obesidad_grado_ii" | nr$clasificacion_talla_imc_mayor == "obesidad_grado_ii",1,0)

# % de mayores con obesidad grado III
nr$n41 <- ifelse(nr$clasificacion_talla_imc_mayor == "9__obesidad_grado_iii" | nr$clasificacion_talla_imc_mayor == "obesidad_grado_iii",1,0)

# % de mayores con obesidad grado IV
nr$n42 <- ifelse(nr$clasificacion_talla_imc_mayor == "obesidad_grado_iv",1,0)




###########################################
#INTERVENCIONES
###########################################
#INTERVENCIONES NINOS 0-23
nr$n43 <- ifelse(nr$intervenciones_ninos023_evaluacion_nutricional == 1,1,0)
nr$n44 <- ifelse(nr$intervenciones_ninos023_desparasitacion == 1,1,0)
nr$n45 <- ifelse(nr$intervenciones_ninos023_micronutrientes == 1,1,0)
nr$n46 <- ifelse(nr$intervenciones_ninos023_prevencion == 1,1,0)
nr$n47 <- ifelse(nr$intervenciones_ninos023_tratamiento_desnutricion == 1,1,0)
nr$n48 <- ifelse(nr$intervenciones_ninos023_orientacion == 1,1,0)


#INTERVENCIONES NINOS 2459
nr$n49 <- ifelse(nr$intervenciones_ninos2459_evaluacion_nutricional == 1,1,0)
nr$n50 <- ifelse(nr$intervenciones_ninos2459_desparasitacion == 1,1,0)
nr$n51 <- ifelse(nr$intervenciones_ninos2459_micronutrientes == 1,1,0)
nr$n52 <- ifelse(nr$intervenciones_ninos2459_prevencion == 1,1,0)
nr$n53 <- ifelse(nr$intervenciones_ninos2459_tratamiento_desnutricion == 1,1,0)
nr$n54 <- ifelse(nr$intervenciones_ninos2459_orientacion == 1,1,0)


#INTERVENCIONES GESTANTES
nr$n55 <- ifelse(nr$suplementos_gestante_acido_folico == 1,1,0)
nr$n56 <- ifelse(nr$suplementos_gestante_calcio == 1,1,0)
nr$n57 <- ifelse(nr$suplementos_gestante_hierro == 1,1,0)



#INTERVENCIONES GESTANTES
nr$n58 <- ifelse(nr$intervenciones_gestantes_evaluacion == 1,1,0)
nr$n59 <- ifelse(nr$intervenciones_gestantes_desparasitacion == 1,1,0)
nr$n60 <- ifelse(nr$intervenciones_gestantes_micronutrientes == 1,1,0)
nr$n61 <- ifelse(nr$intervenciones_gestantes_prevencion == 1,1,0)
nr$n62 <- ifelse(nr$intervenciones_gestantes_orientacion == 1,1,0)



# leche materna exclusiva
nr$n63 <- ifelse(nr$consumo_ayer_ninos023_leche_materna == 1 & nr$consumo_ayer_ninos023_agua == 0 & nr$consumo_ayer_ninos023_ahuyama == 0 &
                   nr$consumo_ayer_ninos023_arroz == 0 & nr$consumo_ayer_ninos023_carne == 0 & nr$consumo_ayer_ninos023_frijoles == 0 & 
                   nr$consumo_ayer_ninos023_huevos == 0 & nr$consumo_ayer_ninos023_leche_tarro == 0,1,0)
nr$n63 <- ifelse(nr$edad_ninos023 > 6,NA, 
                 nr$n63)



#response_nutritional$consumo_ayer_ninos023_leche_materna



return(nr)
}
