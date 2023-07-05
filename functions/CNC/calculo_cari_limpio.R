##############################################################
# COMPONENTE 1 - FOOD CONSUMPTION SCORE
###############################################################
# calcular la puntaje del FCS
r$fcs <- 
  (as.numeric(r$fcs_cereales)*2) +(as.numeric(r$fcs_leguminosas)*3) +(as.numeric(r$fcs_leche)*4) + (as.numeric(r$fcs_carne)*4)+ 
  as.numeric(r$fcs_vegetales) + as.numeric(r$fcs_frutas) + (as.numeric(r$fcs_grasas)*0.5) + (as.numeric(r$fcs_azucares)*0.5)


# clasificar la puntaje del FCS en las 3 categorias (pobre, limitrofe, aceptable)
r$sa1_poor <- ifelse(r$fcs <= 28, 1,0)
r$sa1_borderline <- ifelse(r$fcs > 28 & r$fcs <=42,1,0)
r$sa1_acceptable <- ifelse(r$fcs > 42,1,0)


# calcular la puntaje del CSI
r$csi_score <- 
  (as.numeric(r$csi_alimentos_menos_preferidos)*1) +(as.numeric(r$csi_pedir_prestados_alimentos)*2) +
  (as.numeric(r$csi_reducir_tamano_porciones)*1) + (as.numeric(r$csi_reducir_adultos)*3)+ 
  (as.numeric(r$csi_reducir_numero_comidas)*1) 


# clasificar la puntaje del CSI en las 3 fases 1, 2, 3
r$sa2_i <- ifelse(r$csi_score <= 3, 1,0)
r$sa2_ii <- ifelse(r$csi_score > 3 & r$csi_score <=18, 1,0)
r$sa2_iii <- ifelse(r$csi_score > 18, 1,0)



# crear las 4 categorias del primer sub-indicador (FCS ajustado por CSI)
r$sa3_i <- ifelse(r$sa1_acceptable == 1 & r$sa2_i == 1, 1,0)
r$sa3_ii <- ifelse(r$sa1_acceptable == 1 & (r$sa2_ii == 1 | r$sa2_iii == 1), 1,0)
r$sa3_iii <- ifelse(r$sa1_borderline == 1, 1,0)
r$sa3_iv <- ifelse(r$sa1_poor == 1, 1,0)



###############################################################
# COMPONENTE 2 - VULNERABILIDAD ECONOMICA
###############################################################

# calcular los gastos en alimentos
r$exp_food <- as.numeric(apply(r[,c("gastos_cereales", "gastos_tuberculos", "gastos_legumbres", "gastos_vegetales",
                                    "gastos_frutas", "gastos_carne", "gastos_pescado", "gastos_huevos", "gastos_aceite",
                                    "gastos_leche", "gastos_azucar", "gastos_condimentos", "gastos_bebidas_non_alcoholicas",
                                    "gastos_comida_fuera_casa", "gastos_agua_beber")], 
                               1, sum, na.rm = T))
r$exp_food <- ifelse(as.numeric(apply(r[,c("gastos_cereales", "gastos_tuberculos", "gastos_legumbres", "gastos_vegetales",
                                           "gastos_frutas", "gastos_carne", "gastos_pescado", "gastos_huevos", "gastos_aceite",
                                           "gastos_leche", "gastos_azucar", "gastos_condimentos", "gastos_bebidas_non_alcoholicas",
                                           "gastos_comida_fuera_casa", "gastos_agua_beber")], 
                                      1, function(x) sum(is.na(x)))) > 2,NA, r$exp_food)


# calcular los gastos en cosas no alimentarios en los ultimos 30 dias
r$exp_nonfood_30d <- as.numeric(apply(r[,c("gastos_renta", "gastos_electricidad", "gastos_higiene", 
                                           "gastos_transporte", "gastos_comunicacion", "gastos_gasolina", "gastos_otros", "gastos_lena")], 
                                      1, sum, na.rm = T))
r$exp_nonfood_30d <- ifelse(as.numeric(apply(r[,c("gastos_renta", "gastos_electricidad", "gastos_higiene", 
                                                  "gastos_transporte", "gastos_comunicacion", "gastos_gasolina", "gastos_otros", "gastos_lena")], 
                                             1, function(x) sum(is.na(x)))) > 2,NA, r$exp_nonfood_30d)


# calcular los gastos en cosas no alimentarios en los ultimos 6 meses
r$exp_nonfood_6m <- as.numeric(apply(r[,c("gastos_medicos", "gastos_vestimenta", "gastos_educacion", "gastos_deudas", 
                                          "gastos_insumos", "gastos_construccion", "gastos_seguros", "gastos_textiles")], 
                                     1, sum, na.rm = T))
r$exp_nonfood_6m <- ifelse(as.numeric(apply(r[,c("gastos_medicos", "gastos_vestimenta", "gastos_educacion", "gastos_deudas", 
                                                 "gastos_insumos", "gastos_construccion", "gastos_seguros", "gastos_textiles")], 
                                            1, function(x) sum(is.na(x)))) > 2,NA, r$exp_nonfood_6m)


# calcular los gastos totales en cosas no alimentarios en los ultimos 30 dias
r$exp_nonfood <- r$exp_nonfood_30d + (r$exp_nonfood_6m / 6)

# calcular los gastos totales en alimentos y cosas no alimentarios en los ultimos 30 dias
r$exp_total <- r$exp_nonfood + r$exp_food



# calcular los gastos totales por miembro del hogar
r$exp_pp <- r$exp_total / r$nr_personas_hogar
r$exp_pp <- round(as.numeric(r$exp_pp),0)

# calcular la puntaje de vulnerabilidad economica con la linea de pobreza y linea de pobreza extrema
r <- r %>% dplyr::mutate(sa6 = case_when(
  r$exp_pp > 396182 & r$urbano_rural == "urbano" ~ 1,
  r$exp_pp > 228725 & r$urbano_rural == "rural" ~ 1,
  
  r$exp_pp < 396182 & r$exp_pp > 178906 & r$urbano_rural == "urbano" ~ 3,
  r$exp_pp < 228725 & r$exp_pp > 125291 & r$urbano_rural == "rural" ~ 3,
  
  r$exp_pp < 178906 & r$urbano_rural == "urbano" ~ 4,
  r$exp_pp < 125291 & r$urbano_rural == "rural" ~ 4))


# crear columnas binarias con las 3 categorias de vulnerabilidad economica
r$sa6_i <- ifelse(r$sa6 == 1, 1,0)
r$sa6_iii <- ifelse(r$sa6 == 3, 1,0)
r$sa6_iv <- ifelse(r$sa6 == 4, 1,0)



###############################################################
# COMPONENTE 3 - ESTRATEGIAS DE AFRONTAMIENTO
###############################################################

# crear columnas binarias por cada estrategia
r$sa7_i <- ifelse(r$lcs_actividades_riesgo_no =="3__no__porque_ya_habia_vendido_esos_activos_en_los_ultimos_12_meses_o_ya_realice_esa_actividad_y_no_podia_continuar_haciendolo_" |
                    r$lcs_actividades_riesgo == "si_",1,0)
r$sa7_ii <- ifelse(r$lcs_vender_casa_no =="3__no__porque_ya_habia_vendido_esos_activos_en_los_ultimos_12_meses_o_ya_realice_esa_actividad_y_no_podia_continuar_haciendolo_" |
                     r$lcs_vender_casa == "si_",1,0)
r$sa7_iii <- ifelse(r$lcs_pedir_ayuda_no =="3__no__porque_ya_habia_vendido_esos_activos_en_los_ultimos_12_meses_o_ya_realice_esa_actividad_y_no_podia_continuar_haciendolo_" |
                      r$lcs_pedir_ayuda == "si_",1,0)
r$sa7_iv <- ifelse(r$lcs_sacar_ninos_escuela_no =="3__no__porque_ya_habia_vendido_esos_activos_en_los_ultimos_12_meses_o_ya_realice_esa_actividad_y_no_podia_continuar_haciendolo_" |
                     r$lcs_sacar_ninos_escuela == "si_",1,0)
r$sa7_v <- ifelse(r$lcs_reducir_gastos_salud_educacion_no =="3__no__porque_ya_habia_vendido_esos_activos_en_los_ultimos_12_meses_o_ya_realice_esa_actividad_y_no_podia_continuar_haciendolo_" |
                    r$lcs_reducir_gastos_salud_educacion == "si_",1,0)
r$sa7_vi <- ifelse(r$lcs_vender_activos_produccion_no =="3__no__porque_ya_habia_vendido_esos_activos_en_los_ultimos_12_meses_o_ya_realice_esa_actividad_y_no_podia_continuar_haciendolo_" |
                     r$lcs_vender_activos_produccion == "si_",1,0)
r$sa7_vii <- ifelse(r$lcs_enviar_miembros_comer_familia_no =="3__no__porque_ya_habia_vendido_esos_activos_en_los_ultimos_12_meses_o_ya_realice_esa_actividad_y_no_podia_continuar_haciendolo_" |
                      r$lcs_enviar_miembros_comer_familia == "si_",1,0)
r$sa7_viii <- ifelse(r$lcs_gastar_ahorros_no =="3__no__porque_ya_habia_vendido_esos_activos_en_los_ultimos_12_meses_o_ya_realice_esa_actividad_y_no_podia_continuar_haciendolo_" |
                       r$lcs_gastar_ahorros == "si_",1,0)
r$sa7_ix <- ifelse(r$lcs_comprar_credito_no =="3__no__porque_ya_habia_vendido_esos_activos_en_los_ultimos_12_meses_o_ya_realice_esa_actividad_y_no_podia_continuar_haciendolo_" |
                     r$lcs_comprar_credito == "si_",1,0)
r$sa7_x <- ifelse(r$lcs_vender_activos_no =="3__no__porque_ya_habia_vendido_esos_activos_en_los_ultimos_12_meses_o_ya_realice_esa_actividad_y_no_podia_continuar_haciendolo_" |
                    r$lcs_vender_activos == "si_",1,0)


r$ningun_estrategia <- ifelse(r$sa7_i == 0 & r$sa7_ii == 0 & r$sa7_iii == 0 & r$sa7_iv == 0 & r$sa7_v == 0 &
                                r$sa7_vi == 0 & r$sa7_vii == 0 & r$sa7_viii == 0 & r$sa7_ix == 0 &
                                r$sa7_x == 0, 1,0)


# crear columnas binarias por las 3 categorias de estrategias (estres, crisis, emergencia)
r$stress <-
  ifelse(
    r$sa7_ix == 1 |
      r$sa7_viii == 1 |
      r$sa7_vii == 1 |
      r$sa7_x == 1, 
    1,
    0  
  )


r$crisis <-
  ifelse(
    r$sa7_vi == 1 |
      r$sa7_v == 1 |
      r$sa7_iv == 1,
    1,
    0
  )

r$emergency <-
  ifelse(
    r$sa7_i == 1 |
      r$sa7_ii == 1 |
      r$sa7_iii == 1,
    1,
    0
  )

r$sa7_stress <- ifelse(r$stress == 1, 1,0)
r$sa7_crisis <- ifelse(r$crisis == 1, 1,0)
r$sa7_emergency <- ifelse(r$emergency == 1, 1,0)
r$sa7_ninguna <- ifelse(r$stress == 0 & r$crisis == 0 & r$emergency == 0,1,0)


# crear columnas binarias por cada de los 4 categorias de la tercera componente del CARI (estrategias de afrontamiento)
r$salcs_i <- ifelse(r$sa7_crisis == 0 & r$sa7_emergency == 0 & r$sa7_stress == 0, 1,0)
r$salcs_ii <- ifelse(r$sa7_crisis == 0 & r$sa7_emergency == 0 & r$sa7_stress == 1, 1,0)
r$salcs_iii <- ifelse(r$sa7_crisis == 1 & r$sa7_emergency == 0, 1,0)
r$salcs_iv <- ifelse(r$sa7_emergency == 1, 1,0)



###############################################################
# CALCULO CARI FINAL
###############################################################

# crear una columna con la puntaje del componente 1
r <- r %>% dplyr::mutate(fcs_ajuste_cari = case_when(
  r$sa3_i == 1 ~ 1,
  r$sa3_ii == 1 ~ 2,
  r$sa3_iii == 1 ~ 3,
  r$sa3_iv == 1 ~ 4))

# crear una columna con la puntaje del componente 2
r <- r %>% dplyr::mutate(lcs_cari = case_when(
  r$salcs_i == 1 ~ 1,
  r$salcs_ii == 1 ~ 2,
  r$salcs_iii == 1 ~ 3,
  r$salcs_iv == 1 ~ 4
))

# crear una columna con la puntaje del componente 3
r <- r %>% dplyr::mutate(sa6 = case_when(
  r$exp_pp > 396182 & r$urbano_rural == "urbano" ~ 1,
  r$exp_pp > 228725 & r$urbano_rural == "rural" ~ 1,
  
  r$exp_pp < 396182 & r$exp_pp > 178906 & r$urbano_rural == "urbano" ~ 3,
  r$exp_pp < 228725 & r$exp_pp > 125291 & r$urbano_rural == "rural" ~ 3,
  
  r$exp_pp < 178906 & r$urbano_rural == "urbano" ~ 4,
  r$exp_pp < 125291 & r$urbano_rural == "rural" ~ 4))




# calcular la puntaje del cari y crear columnas binarias por cada de las 4 categorias (seguridad alimentaria, seguridad alimentaria marginal, inseguridad alimentaria moderada, inseguridad alimentaria severa)
r$cari <- as.numeric(r$fcs_ajuste_cari * 0.5) + as.numeric(r$lcs_cari * 0.25) + as.numeric(r$fes_cari * 0.25)
r$sa8_sa <- ifelse(r$cari < 1.5,1,0)
r$sa8_sam <- ifelse(r$cari >= 1.5 & r$cari < 2.5,1,0)
r$sa8_iam <- ifelse(r$cari >= 2.5 & r$cari < 3.5,1,0)
r$sa8_ias <- ifelse(r$cari >= 3.5,1,0)


r <- r %>% dplyr::mutate(cari_insecurity = case_when(
  r$sa8_sa == 1 ~ "seguridad_alimentaria",
  r$sa8_sam == 1 ~ "seguridad_alimentaria",
  r$sa8_iam == 1 ~ "inseguridad_alimentaria",
  r$sa8_ias == 1 ~ "inseguridad_alimentaria"
))
