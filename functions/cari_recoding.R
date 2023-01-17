recoding_cari <- function(r) {
  
  r <- df
  # create variable with regions
  r <- r %>% dplyr::mutate(regiones_naturales = case_when(
    r$departamento %in% c("antioquia", "bogota_dc", "boyaca", "caldas", "cundinamarca", "huila", "quindio", "risaralda", "santander", "tolima", "norte_de_santander") ~ "andina",
    r$departamento %in% c("vichada", "vaupes", "amazonas", "caqueta", "putumayo") ~ "amazonia",
    r$departamento %in% c("atlantico", "bolivar", "cesar", "cordoba", "la_guajira", "magdalena", "sucre") ~ "caribe",
    r$departamento %in% c("archipielago_de_san_andres") ~ "insular",
    r$departamento %in% c("arauca", "casanare", "meta") ~ "orinoquia",
    r$departamento %in% c("cauca", "choco", "valle_del_cauca", "narino") ~ "pacifica"
  ))
  
  
  
  
  
  
  
  
  # % de hogares por nivel de estudios del jefe del hogar
  r <- r %>% dplyr::mutate(nivel_estudios_jh = case_when(
    r$ind1_parentesco == "jefe_del_hogar" ~ r$ind1_nivel_educacion,
    r$ind2_parentesco == "jefe_del_hogar" ~ r$ind2_nivel_educacion,
    r$ind3_parentesco == "jefe_del_hogar" ~ r$ind3_nivel_educacion,
    r$ind4_parentesco == "jefe_del_hogar" ~ r$ind4_nivel_educacion,
    r$ind5_parentesco == "jefe_del_hogar" ~ r$ind5_nivel_educacion,
    r$ind6_parentesco == "jefe_del_hogar" ~ r$ind6_nivel_educacion,
    r$ind7_parentesco == "jefe_del_hogar" ~ r$ind7_nivel_educacion,
    r$ind8_parentesco == "jefe_del_hogar" ~ r$ind8_nivel_educacion
  ))
  
  
  #############################################################
  #############################################################
  #############################################################
  #Add disaggregation variables
  #RANGOS DE INGRESO
  r$ingreso_pp <- r$ingreso / r$nr_personas_hogar
  r <- r %>% dplyr::mutate(rangos_ingreso = case_when(
    ingreso_pp < 50000 | is.na(ingreso_pp) ~ "menos_50", 
    ingreso_pp >= 50000 & ingreso_pp < 100000 ~ "50_100", 
    ingreso_pp >= 100000 & ingreso_pp < 200000 ~ "100_200",
    ingreso_pp >= 200000 & ingreso_pp < 300000 ~ "200_300",
    ingreso_pp >= 300000 & ingreso_pp < 400000 ~ "300_400",
    ingreso_pp >= 400000 & ingreso_pp < 500000 ~ "400_500", 
    ingreso_pp >= 500000 & ingreso_pp < 600000 ~ "500_600",
    ingreso_pp >= 600000 & ingreso_pp < 700000 ~ "600_700",
    ingreso_pp >= 700000 & ingreso_pp < 800000 ~ "700_800", 
    ingreso_pp >= 800000 & ingreso_pp < 1000000 ~ "800_1000", 
    ingreso_pp >= 1000000 ~ "mas_1000"))
  
  
  
  r$ingreso_pp_menos50 <- ifelse(r$rangos_ingreso == "menos_50",1,0)
  r$ingreso_pp_50_100 <- ifelse(r$rangos_ingreso == "50_100",1,0)
  r$ingreso_pp_100_200 <- ifelse(r$rangos_ingreso == "100_200",1,0)
  r$ingreso_pp_200_300 <- ifelse(r$rangos_ingreso == "200_300",1,0)
  r$ingreso_pp_300_400 <- ifelse(r$rangos_ingreso == "300_400",1,0)
  r$ingreso_pp_400_500 <- ifelse(r$rangos_ingreso == "400_500",1,0)
  r$ingreso_pp_500_600 <- ifelse(r$rangos_ingreso == "500_600",1,0)
  r$ingreso_pp_600_700 <- ifelse(r$rangos_ingreso == "600_700",1,0)
  r$ingreso_pp_700_800 <- ifelse(r$rangos_ingreso == "700_800",1,0)
  r$ingreso_pp_800_900 <- ifelse(r$rangos_ingreso == "800_900",1,0)
  r$ingreso_pp_900_1000 <- ifelse(r$rangos_ingreso == "900_1000",1,0)
  r$ingreso_pp_mas1000 <- ifelse(r$rangos_ingreso == "mas_1000",1,0)

  
  
  # rangos for the amount of debt
  r$valor_deuda <- as.numeric(as.character(r$valor_deuda))
  r$valor_deuda <- ifelse(r$valor_deuda %in% c(99, 999), NA, 
                          r$valor_deuda)
  r <- r %>% dplyr::mutate(rangos_deuda = case_when(
    is.na(valor_deuda)  ~ "ningun_deuda", 
    valor_deuda < 500000 ~ "menos_500", 
    valor_deuda >= 500000 & valor_deuda < 1000000 ~ "500_1000", 
    valor_deuda >= 1000000 & valor_deuda < 2000000 ~ "1000_2000", 
    valor_deuda >= 2000000 & valor_deuda < 3000000 ~ "2000_3000", 
    valor_deuda >= 3000000 & valor_deuda < 4000000 ~ "3000_4000", 
    valor_deuda >= 4000000 & valor_deuda < 7000000 ~ "4000_7000", 
    valor_deuda >= 7000000 & valor_deuda < 10000000 ~ "7000_10000", 
    valor_deuda >= 10000000 & valor_deuda < 20000000 ~ "10000_20000", 
    valor_deuda >= 20000000 ~ "mas_20000"))
  
  
  # households having received assistance from UN, government or PMA
  r <- r %>% dplyr::mutate(recibe_asistencia = case_when(
    r$asistencia_gobierno == "si" | r$asistencia_organizacion == "si" |
      r$asistencia_PMA == "si" ~ "recibe_asistencia", 
    TRUE ~ "no_recibe_asistencia"))
  
  
  
  
  #ESTADO CIVIL Y SEXO
  r <- r %>% dplyr::mutate(estado_civil_sexo_jh = case_when(
    r$sexo_jh == "mujer" & r$estado_civil_jh == "esta_casado_a_" ~ "mujer_casado",
    r$sexo_jh == "mujer" & r$estado_civil_jh == "esta_separado_a__o_divorciado_a_" ~ "mujer_divorciado",
    r$sexo_jh == "mujer" & r$estado_civil_jh == "esta_soltero_a_" ~ "mujer_soltera",
    r$sexo_jh == "mujer" & r$estado_civil_jh == "esta_viudo_a_" ~ "mujer_viudo",
    r$sexo_jh == "mujer" & r$estado_civil_jh == "vive_en_union_libre" ~ "mujer_union_libre",
    
    TRUE ~ "hombre"
  ))
  
  
  #TAMANO DEL HOGAR
  r <- r %>% dplyr::mutate(tamano_hogar = case_when(
    r$nr_personas_hogar == 1  ~ "1",
    r$nr_personas_hogar > 1 & r$nr_personas_hogar <= 2  ~ "1-2",
    r$nr_personas_hogar > 2 & r$nr_personas_hogar <= 3 ~ "2-3",
    r$nr_personas_hogar > 3 & r$nr_personas_hogar <= 4  ~ "3-4",
    r$nr_personas_hogar > 4 & r$nr_personas_hogar <= 5 ~ "4-5",
    r$nr_personas_hogar > 5 ~ ">5",
    TRUE ~ "0"
  ))
  
  
  #DISCAPACIDAD JEFE DEL HOGAR
  r <- r %>% dplyr::mutate(discapacidad_jh = case_when(
    r$discapacidad_jh == "mucha_dificultad" | 
      r$discapacidad_jh == "no_puede_hacer_nada" ~ "con_discapacidad", 
    TRUE ~ "sin_discapacidad"))
  
  
  #DISCAPACIDAD JEFE DEL HOGAR Y SEXO
  r <- r %>% dplyr::mutate(discapacidad_sexo_jh = case_when(
    (r$discapacidad_jh == "mucha_dificultad" | r$discapacidad_jh == "no_puede_hacer_nada") & r$sexo_jh == "hombre"  ~ "con_discapacidad_hombre",
    (r$discapacidad_jh == "mucha_dificultad" | r$discapacidad_jh == "no_puede_hacer_nada") & r$sexo_jh == "mujer"  ~ "con_discapacidad_mujer",
    (r$discapacidad_jh == "alguna_dificultad" | r$discapacidad_jh == "ninguna_dificultad") & r$sexo_jh == "mujer"  ~ "sin_discapacidad_mujer",
    (r$discapacidad_jh == "alguna_dificultad" | r$discapacidad_jh == "ninguna_dificultad") & r$sexo_jh == "hombre"  ~ "sin_discapacidad_hombre",
    
    TRUE ~ "0"
  ))
  
  
  r <- r %>% dplyr::mutate(etnia_respondiente = case_when(
    r$ind1_pertenencia_etnica == "afrodescendiente__negro__mulato_"  ~ "afrodescendiente",
    r$ind1_pertenencia_etnica == "indigena"  ~ "indigena",
    r$ind1_pertenencia_etnica == "mestizo"  ~ "mestizo",
    r$ind1_pertenencia_etnica == "ninguno"  ~ "ninguno",
    r$ind1_pertenencia_etnica == "raizal"  ~ "raizal",
    r$ind1_pertenencia_etnica == "palenquero"  ~ "palenquero",
    TRUE ~ "otro"
  ))
  
  
  
  r <- r %>% dplyr::mutate(enfermedad_cronica = case_when(
    r$enfermedad_cronica_jh == "si"  ~ "con_enfermedad_cronica",
    r$enfermedad_cronica_jh == "no"  ~ "sin_enfermedad_cronica",
    TRUE ~ "otro"
  ))
  
  
  r <- r %>% dplyr::mutate(enfermedad_mental = case_when(
    r$enfermedad_mental_jh == "si"  ~ "con_enfermedad_mental",
    r$enfermedad_mental_jh == "no"  ~ "sin_enfermedad_mental",
    TRUE ~ "otro"
  ))
  
  
  r <- r %>% dplyr::mutate(con_deuda = case_when(
    r$deuda == "si"  ~ "con_deuda",
    r$deuda == "no"  ~ "sin_deuda",
    TRUE ~ "otro"
  ))
  
  
  r <- r %>% dplyr::mutate(ahorrado_dinero_6m = case_when(
    r$ahorrado_dinero == "si"  ~ "ahorrado_dinero",
    r$ahorrado_dinero == "no"  ~ "no_ahorrado_dinero",
    TRUE ~ "otro"
  ))
  
  r <- r %>% dplyr::mutate(afectado_por_desastre_natural = case_when(
    r$afectado_desastre_natural == "si"  ~ "afectado",
    r$afectado_desastre_natural == "no"  ~ "no_afectado",
    TRUE ~ "otro"
  ))
  
  r <- r %>% dplyr::mutate(afectado_por_conflicto = case_when(
    r$afectado_conflicto == "si"  ~ "afectado",
    r$afectado_conflicto == "no"  ~ "no_afectado",
    TRUE ~ "otro"
  ))
  
  
  r <- r %>% dplyr::mutate(sexo_jefatura = case_when(
    r$sexo_jh %in% c("hombre", "la_jefatura_es_compartida_entre_hombres")  ~ "hombre",
    r$sexo_jh %in% c("mujer", "la_jefatura_es_compartida_entre_mujeres")  ~ "mujer",
    r$sexo_jh == "la_jefatura_es_compartida_entre_hombre_y_mujer" ~ "compartida_mujer_hombre",
    TRUE ~ "otro"
  ))
  
  
  r <- r %>% dplyr::mutate(tipo_de_vivienda = case_when(
    r$tipo_vivienda == "apartamento_apartaestudio"  ~ "apartamento",
    r$tipo_vivienda == "casa"  ~ "casa",
    r$tipo_vivienda == "habitacion_cuarto_pieza_en_otro_tipo_de_estructura__parqueaderos__depositos__bodegas__iglesias__colegios__fabricas__cuarto_para_portero_o_celador_en_un_edificio_de_apartamentos_"  ~ "otro_tipo_de_estructura",
    r$tipo_vivienda == "habitacion_cuarto_pieza_en_un_inquilinato"  ~ "habitacion_inquilinato",
    r$tipo_vivienda == "situacion_de_calle_con_espacio_para_alojarse__carpa__vagon__embarcacion__cueva__refugio_natural__etc__"  ~ "situacion_calle",
    r$tipo_vivienda == "vivienda_improvisada__construcciones_informales_con_materiales_menos_durables__cambuches__etc__"  ~ "vivienda_improvisada",
    TRUE ~ "otro"
  ))
  
  
  r$ninguna <- "ninguna"
  r$nacional <- "nacional"
  
  
  

###############################################################
# SEGURIDAD ALIMENTARIA
###############################################################
# % de hogares con {poor, borderline, acceptable} Food Consumption Score
r$fcs <- 
  (as.numeric(r$fcs_cereales)*2) +(as.numeric(r$fcs_leguminosas)*3) +(as.numeric(r$fcs_leche)*4) + (as.numeric(r$fcs_carne)*4)+ 
  as.numeric(r$fcs_vegetales) + as.numeric(r$fcs_frutas) + (as.numeric(r$fcs_grasas)*0.5) + (as.numeric(r$fcs_azucares)*0.5)

r$sa1_poor <- ifelse(r$fcs <= 28, 1,0)
r$sa1_borderline <- ifelse(r$fcs > 28 & r$fcs <=42,1,0)
r$sa1_acceptable <- ifelse(r$fcs > 42,1,0)


# % de hogares por Coping Strategies Index Score 
r$csi_score <- 
  (as.numeric(r$csi_alimentos_menos_preferidos)*1) +(as.numeric(r$csi_pedir_prestados_alimentos)*2) +
  (as.numeric(r$csi_reducir_tamano_porciones)*1) + (as.numeric(r$csi_reducir_adultos)*3)+ 
  (as.numeric(r$csi_reducir_numero_comidas)*1) 

r$sa2_i <- ifelse(r$csi_score <= 3, 1,0)
r$sa2_ii <- ifelse(r$csi_score > 3 & r$csi_score <=18, 1,0)
r$sa2_iii <- ifelse(r$csi_score > 18, 1,0)


# % de hogares por FCS-CSI Ajuste
r$sa3_i <- ifelse(r$sa1_acceptable == 1 & r$sa2_i == 1, 1,0)
r$sa3_ii <- ifelse(r$sa1_acceptable == 1 & (r$sa2_ii == 1 | r$sa2_iii == 1), 1,0)
r$sa3_iii <- ifelse(r$sa1_borderline == 1, 1,0)
r$sa3_iv <- ifelse(r$sa1_poor == 1, 1,0)


# cuota media del gasto en alimentacion (en % del gasto total)
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



r$exp_nonfood_30d <- as.numeric(apply(r[,c("gastos_renta", "gastos_electricidad", "gastos_higiene", 
                                           "gastos_transporte", "gastos_comunicacion", "gastos_gasolina", "gastos_otros", "gastos_lena")], 
                                      1, sum, na.rm = T))
r$exp_nonfood_30d <- ifelse(as.numeric(apply(r[,c("gastos_renta", "gastos_electricidad", "gastos_higiene", 
                                                  "gastos_transporte", "gastos_comunicacion", "gastos_gasolina", "gastos_otros", "gastos_lena")], 
                                             1, function(x) sum(is.na(x)))) > 2,NA, r$exp_nonfood_30d)


r$exp_nonfood_6m <- as.numeric(apply(r[,c("gastos_medicos", "gastos_vestimenta", "gastos_educacion", "gastos_deudas", 
                                          "gastos_insumos", "gastos_construccion", "gastos_seguros", "gastos_textiles")], 
                                     1, sum, na.rm = T))
r$exp_nonfood_6m <- ifelse(as.numeric(apply(r[,c("gastos_medicos", "gastos_vestimenta", "gastos_educacion", "gastos_deudas", 
                                                 "gastos_insumos", "gastos_construccion", "gastos_seguros", "gastos_textiles")], 
                                            1, function(x) sum(is.na(x)))) > 2,NA, r$exp_nonfood_6m)


r$exp_nonfood <- r$exp_nonfood_30d + (r$exp_nonfood_6m / 6)
r$exp_total <- r$exp_nonfood + r$exp_food


r$sa4 <- r$exp_food / r$exp_total



r <- r %>% dplyr::mutate(fes_cari_alternative = case_when(
  r$sa4 < 0.5 ~ 1,
  r$sa4 >= 0.5 & r$sa4 < 0.65 ~ 2,
  r$sa4 >= 0.65 & r$sa4 < 0.75 ~ 3,
  r$sa4 >= 0.75 ~ 4))



# promedio de gastos en alimentos por persona
r$sa4_i_cop <- round(r$exp_food / r$nr_personas_hogar,0)
r$sa4_i_usd <- round(as.numeric(r$sa4_i_cop / 4813),0)


# promedio de gastos totales por persona
r$sa4_ii_cop <- round(as.numeric(r$exp_total / r$nr_personas_hogar),0)
r$sa4_ii_usd <- round(as.numeric(r$sa4_ii_cop / 4813),0)



# % de hogares por vulnerabilidad economic gastos
r$exp_pp <- r$exp_total / r$nr_personas_hogar
r$exp_pp <- round(as.numeric(r$exp_pp),0)

r <- r %>% dplyr::mutate(sa6 = case_when(
  r$exp_pp > 396182 & r$urbano_rural == "urbano" ~ 1,
  r$exp_pp > 228725 & r$urbano_rural == "rural" ~ 1,
  
  r$exp_pp < 396182 & r$exp_pp > 178906 & r$urbano_rural == "urbano" ~ 3,
  r$exp_pp < 228725 & r$exp_pp > 125291 & r$urbano_rural == "rural" ~ 3,
  
  r$exp_pp < 178906 & r$urbano_rural == "urbano" ~ 4,
  r$exp_pp < 125291 & r$urbano_rural == "rural" ~ 4))


# % de hogares pobres (LP-DANE)
r <- r %>% dplyr::mutate(so4 = case_when(
  r$exp_pp < 396182 & r$urbano_rural == "urbano" ~ 1,
  r$exp_pp < 228725 & r$urbano_rural == "rural" ~ 1,
  TRUE ~ 0
))


# % de hogares en pobreza extrema (LPE-DANE)
r <- r %>% dplyr::mutate(so5 = case_when(
  r$exp_pp < 178906 & r$urbano_rural == "urbano" ~ 1,
  r$exp_pp < 125291 & r$urbano_rural == "rural" ~ 1,
  TRUE ~ 0
))

r$sa6_i <- ifelse(r$sa6 == 1, 1,0)
r$sa6_iii <- ifelse(r$sa6 == 3, 1,0)
r$sa6_iv <- ifelse(r$sa6 == 4, 1,0)




# % de hogares por estrategia
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


# % de hogares que recurren a estrategias de stress/crisis/emergency para hacer frente a la falta de alimentos o de dinero para comprarlos
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

# % de hogares por LCS para CARI
r$salcs_i <- ifelse(r$sa7_crisis == 0 & r$sa7_emergency == 0 & r$sa7_stress == 0, 1,0)
r$salcs_ii <- ifelse(r$sa7_crisis == 0 & r$sa7_emergency == 0 & r$sa7_stress == 1, 1,0)
r$salcs_iii <- ifelse(r$sa7_crisis == 1 & r$sa7_emergency == 0, 1,0)
r$salcs_iv <- ifelse(r$sa7_emergency == 1, 1,0)



# % de hogares por situacion de seguridad alimentaria segun la metodologia del CARI
r <- r %>% dplyr::mutate(fcs_ajuste_cari = case_when(
  r$sa3_i == 1 ~ 1,
  r$sa3_ii == 1 ~ 2,
  r$sa3_iii == 1 ~ 3,
  r$sa3_iv == 1 ~ 4))

r <- r %>% dplyr::mutate(lcs_cari = case_when(
  r$salcs_i == 1 ~ 1,
  r$salcs_ii == 1 ~ 2,
  r$salcs_iii == 1 ~ 3,
  r$salcs_iv == 1 ~ 4
))
r$fes_cari <- r$sa6
#r$fes_cari <- r$fes_cari_alternative





r$cari <- as.numeric(r$fcs_ajuste_cari * 0.5) + as.numeric(r$lcs_cari * 0.25) + as.numeric(r$fes_cari * 0.25)
r$sa8_sa <- ifelse(r$cari < 1.5,1,0)
r$sa8_sam <- ifelse(r$cari >= 1.5 & r$cari < 2.5,1,0)
r$sa8_iam <- ifelse(r$cari >= 2.5 & r$cari < 3.5,1,0)
r$sa8_ias <- ifelse(r$cari >= 3.5,1,0)

r <- r %>% dplyr::mutate(cari_category = case_when(
  r$sa8_sa == 1 ~ "Seguridad",
  r$sa8_sam == 1 ~ "Seguridad Marginal",
  r$sa8_iam == 1 ~ "Inseguridad Moderada",
  r$sa8_ias == 1 ~ "Inseguridad Severa"
))

r <- r %>% dplyr::mutate(cari_insecurity = case_when(
  r$sa8_sa == 1 ~ "seguridad_alimentaria",
  r$sa8_sam == 1 ~ "seguridad_alimentaria",
  r$sa8_iam == 1 ~ "inseguridad_alimentaria",
  r$sa8_ias == 1 ~ "inseguridad_alimentaria"
))




# % de hogares que han comido menos de 3 veces el dia anterior a la recogida de datos
r$sa9_i <- ifelse(r$nr_comidas_7d == "3_comidas_o_mas",1,0)
r$sa9_ii <- ifelse(r$nr_comidas_7d == "2_comidas",1,0)
r$sa9_iii <- ifelse(r$nr_comidas_7d == "1_comida",1,0)
r$sa9_iv <- ifelse(r$nr_comidas_7d == "ninguna",1,0)


# % de hogares que han comido menos de 3 veces el dia anterior de la recogida de datos
r$sa10_i <- ifelse(r$nr_comidas_ayer == "3_comidas_o_mas",1,0)
r$sa10_ii <- ifelse(r$nr_comidas_ayer == "2_comidas",1,0)
r$sa10_iii <- ifelse(r$nr_comidas_ayer == "1_comida",1,0)
r$sa10_iv <- ifelse(r$nr_comidas_ayer == "ninguna",1,0)


# % de hogares por FCS-N
r <- r %>% 
  dplyr::mutate_at(vars(starts_with("fcs_") & !ends_with("ayer")), funs(as.numeric)) %>%
  rowwise() %>% 
  dplyr::mutate(fg_vita = sum(fcs_leche, fcs_carne, fcs_huevos, 
                       fcs_vegetales_anaranjados, fcs_vegetales_verdes, fcs_frutas_naranja, na.rm = T)) %>%
  dplyr::mutate(sa_16_vita1 = ifelse(fg_vita == 0,1,0),
         sa_16_vita2 = ifelse(fg_vita > 0 & fg_vita <= 6,1,0),
         sa_16_vita3 = ifelse(fg_vita > 6,1,0)) %>%
  
  dplyr::mutate(fg_protein = sum(fcs_leguminosas, fcs_leche, fcs_visceras_rojo, 
                          fcs_carne_frescas, fcs_pescado, fcs_huevos, na.rm = T)) %>%
  dplyr::mutate(sa_17_protein1 = ifelse(fg_protein == 0,1,0),
         sa_17_protein2 = ifelse(fg_protein > 0 & fg_protein <= 6,1,0),
         sa_17_protein3 = ifelse(fg_protein > 6,1,0)) %>%
  
  dplyr::mutate(fg_iron = sum(fcs_visceras_rojo, 
                       fcs_carne_frescas, fcs_pescado, na.rm = T)) %>%
  dplyr::mutate(sa_19_iron1 = ifelse(fg_iron == 0,1,0),
         sa_19_iron2 = ifelse(fg_iron > 0 & fg_iron <= 6,1,0),
         sa_19_iron3 = ifelse(fg_iron > 6,1,0)) 


# % de hogares por categoria HDDS
bin_fcs <- function(x) (ifelse(x == "si",1,0))
r <- r %>%
  rowwise() %>%
  dplyr::mutate_at(vars(ends_with("_ayer") & starts_with("fcs_")), bin_fcs) %>%
  dplyr::mutate(sum_hdds = sum(c(fcs_cereales_ayer, fcs_raices_ayer, fcs_vegetales_ayer, 
                          fcs_frutas_ayer, fcs_carne_ayer,
                          fcs_leguminosas_ayer, fcs_leche_ayer, fcs_grasas_ayer, fcs_azucares_ayer,
                          fcs_condimentos_ayer), na.rm = T)) %>%
  dplyr::mutate(sa_15_cat1 = ifelse(sum_hdds <= 2,1,0),
         sa_15_cat2 = ifelse(sum_hdds > 2 & sum_hdds <= 4,1,0),
         sa_15_cat3 = ifelse(sum_hdds == 5,1,0),
         sa_15_cat4 = ifelse(sum_hdds > 5,1,0))


# % de hogares que han empleado al menos una estrategia CSI en los ultimos 7 dias
r$sa11 <- ifelse(r$csi_alimentos_menos_preferidos > 0 | r$csi_pedir_prestados_alimentos > 0 | 
                   r$csi_reducir_adultos > 0 | r$csi_reducir_numero_comidas > 0 | r$csi_reducir_tamano_porciones > 0,1,0)


# media numero de dias en que el hogar ha empleado una estrategia CSI por estrategia
r$sa13_i <- r$csi_alimentos_menos_preferidos
r$sa13_ii <- r$csi_pedir_prestados_alimentos
r$sa13_iii <- r$csi_reducir_adultos
r$sa13_iv <- r$csi_reducir_numero_comidas
r$sa13_v <- r$csi_reducir_tamano_porciones



# media numero de dias por tipo de alimentos
r$sa14_i <- r$fcs_carne
r$sa14_ii <- r$fcs_cereales
r$sa14_iii <- r$fcs_frutas
r$sa14_iv <- r$fcs_huevos
r$sa14_v <- r$fcs_leche
r$sa14_vi <- r$fcs_leguminosas
r$sa14_vii <- r$fcs_pescado
r$sa14_viii <- r$fcs_raices
r$sa14_ix <- r$fcs_vegetales
r$sa14_x <- r$fcs_condimentos


#% de hogares por CSI estrategia
r$sa15_i <- ifelse(r$csi_alimentos_menos_preferidos > 0,1,0)
r$sa15_ii <- ifelse(r$csi_pedir_prestados_alimentos > 0,1,0)
r$sa15_iii <- ifelse(r$csi_reducir_adultos > 0,1,0)
r$sa15_iv <- ifelse(r$csi_reducir_numero_comidas > 0,1,0)
r$sa15_v <- ifelse(r$csi_reducir_tamano_porciones > 0,1,0)



###############################################################
# SITUACION SOCIOECONOMICA
###############################################################
# % de hogares segun la fuente principal de los ingresos
r$so2_i <- ifelse(r$fuente_ingresos == "trabajo__cualquier_actividad_economica_que_genera_ingresos_",1,0)
r$so2_ii <- ifelse(r$fuente_ingresos == "pension__sistema_de_proteccion_social_",1,0)
r$so2_iii <- ifelse(r$fuente_ingresos == "asistencia_del_gobierno",1,0)
r$so2_iv <- ifelse(r$fuente_ingresos == "asistencia_de_naciones_unidas__ongs__organizaciones_caritativas",1,0)
r$so2_v <- ifelse(r$fuente_ingresos == "remesas_de_migrantes_o_ayuda_de_familiares_amigos",1,0)
r$so2_vi <- ifelse(r$fuente_ingresos == "el_hogar_no_recibe_ingresos_de_ningun_tipo",1,0)


# ingresos medios mensuales por miembro del hogar
r$ingreso_pp <- r$ingreso / r$nr_personas_hogar
r$so3 <- round(as.numeric(r$ingreso_pp),0)
r$so3_cop <- round(as.numeric(r$ingreso_pp),0)
r$so3_usd <- round(as.numeric(r$so3_cop / 4836),0)

# % de hogares que declaran tener una deuda en el momento de la recogida de datos
r$so6 <- ifelse(r$deuda == "si",1,0)


# importe medio de la dueda
r$so7 <- as.numeric(r$valor_deuda)

# % de hogares por motivo de su deuda
r <- r %>%
  mutate(razon_deuda = na_if(razon_deuda, "_"))
r$so8_i <- ifelse(r$razon_deuda == "comprar_comida",1,0)
r$so8_ii <- ifelse(r$razon_deuda == "comprar_insumos_productivos",1,0)
r$so8_iii <- ifelse(r$razon_deuda == "comprar_ropa__zapatos",1,0)
r$so8_iv <- ifelse(r$razon_deuda == "cubrir_gastos_de_salud",1,0)
r$so8_v <- ifelse(r$razon_deuda == "cubrir_servicios_basicos__agua__electricidad_",1,0)
r$so8_vi <- ifelse(r$razon_deuda == "pagar_la_escuela_o_gastos_de_educacion",1,0)
r$so8_vii <- ifelse(r$razon_deuda == "pagar_renta_o_alquiler_de_la_vivienda",1,0)
r$so8_viii <- ifelse(r$razon_deuda == "pagar_viajes",1,0)
r$so8_ix <- ifelse(r$razon_deuda == "compra_de_activos__casa__apartamento__carro__moto__electrodomesticos_etc__",1,0)



# % de hogares que declaran haber disminuido sus ingresos en los ultimos 12 meses
r <- r %>%
  mutate(cambio_ingresos = na_if(cambio_ingresos, "_"))
r$so9_i <- ifelse(r$cambio_ingresos == "aumentaron_los_ingresos",1,0)
r$so9_ii <- ifelse(r$cambio_ingresos == "disminuyeron_los_ingresos",1,0)
r$so9_iii <- ifelse(r$cambio_ingresos == "no_hubo_cambios",1,0)
r$so9_iv <- ifelse(r$cambio_ingresos == "se_perdieron_los_ingresos_por_completo",1,0)


# % de hogares por razon principal por la que disminuyeron o perdieron sus ingresos
r <- r %>%
  mutate(razon_cambio_ingresos = na_if(razon_cambio_ingresos, "_"))
r$so10_i <- ifelse(r$razon_cambio_ingresos == "algun_miembro_del_hogar_murio",1,0)
r$so10_ii <- ifelse(r$razon_cambio_ingresos == "algun_miembro_del_hogar_perdio_su_empleo_o_redujo_las_horas_de_trabajo",1,0)
r$so10_iii <- ifelse(r$razon_cambio_ingresos == "algun_miembro_del_hogar_se_enfermo_o_esta_incapacitado",1,0)
r$so10_iv <- ifelse(r$razon_cambio_ingresos == "dejo_de_recibir_ayuda_de_familia_o_amigos__incluye_remesas_",1,0)
r$so10_v <- ifelse(r$razon_cambio_ingresos == "dejo_de_recibir_la_asistencia_del_gobierno_o_de_una_organizacion",1,0)
r$so10_vi <- ifelse(r$razon_cambio_ingresos == "los_salarios_se_han_reducido_en_el_sector_en_que_trabaja_o_las_ventas_han_disminuido",1,0)



# cuota media del gasto en renta (en % del gasto total)
r$so11 <- as.numeric(r$gastos_renta) / as.numeric(r$exp_total)

# cuota media del gasto en gastos medicos o cuidado de la salud (en % del gasto total)
r$so12 <- as.numeric(r$gastos_medicos / 6) / as.numeric(r$exp_total)

# cuota media del gasto en educacion (en % del gasto total)
r$so13 <- as.numeric(r$gastos_educacion / 6) / as.numeric(r$exp_total)

# cuota media del gasto en gastos pago de deudas (en % del gasto total)
r$so14 <- as.numeric(r$gastos_deudas / 6) / as.numeric(r$exp_total)

# cuota media del gasto en productos higienicos (en % del gasto total)
r$so14_i <- as.numeric(r$gastos_higiene) / as.numeric(r$exp_total)

# cuota media del gasto en transporte (en % del gasto total)
r$so14_ii <- as.numeric(r$gastos_transporte) / as.numeric(r$exp_total)


# % de hogares que declaran haber ahorrado dinero en los ultimos 6 meses
r$so15 <- ifelse(r$ahorrado_dinero == "si",1,0)


# cuota media del ahorro
r$so16 <- as.numeric(r$monto_ahorrado)


# % de hogares por rangos de ingresos pp
r$so19_i <- ifelse(r$ingreso_pp < 100000,1,0)
r$so19_ii <- ifelse(r$ingreso_pp >= 100000 & r$ingreso_pp < 200000,1,0)
r$so19_iii <- ifelse(r$ingreso_pp >= 200000 & r$ingreso_pp < 300000,1,0)
r$so19_iv <- ifelse(r$ingreso_pp >= 300000,1,0)



# % de hogares por tipo de empleo de la persona que aporta la mayor parte de recursos en el hogar
r <- r %>%
  mutate(miembro_mayor_recursos_empleo = na_if(miembro_mayor_recursos_empleo, "_"))
r$so20_i <- ifelse(r$miembro_mayor_recursos_empleo == "empleado_a__domestico_a_",1,0)
r$so20_ii <- ifelse(r$miembro_mayor_recursos_empleo == "jornalero_o_peon",1,0)
r$so20_iii <- ifelse(r$miembro_mayor_recursos_empleo == "obrero_a__o_empleado_a__de_empresa_particular",1,0)
r$so20_iv <- ifelse(r$miembro_mayor_recursos_empleo == "obrero_a__o_empleado_a__del_gobierno",1,0)
r$so20_v <- ifelse(r$miembro_mayor_recursos_empleo == "patron_o_empleador",1,0)
r$so20_vi <- ifelse(r$miembro_mayor_recursos_empleo == "trabajador_familiar_sin_remuneracion",1,0)
r$so20_vii <- ifelse(r$miembro_mayor_recursos_empleo == "trabajador_por_cuenta_propia",1,0)
r$so20_viii <- ifelse(r$miembro_mayor_recursos_empleo == "trabajador_sin_remuneracion_en_empresas_o_negocios_de_otros_hogares",1,0)


# % de hogares por actividad a que se dedica principalmente la empresa o negocio en la que trabaja la persona que aporta la mayor parte de recursos
r <- r %>%
  mutate(miembro_mayor_recursos_actividad_empresa = na_if(miembro_mayor_recursos_actividad_empresa, "_"))
r$so21_i <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "actividades_financieras_y_de_seguros__actividades_inmobiliarias__empresariales_y_de_alquiler",1,0)
r$so21_ii <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "administracion_publica__gobierno___defensa__seguridad_social__educacion__servicios_sociales_y_de_salud_",1,0)
r$so21_iii <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "agricultura__ganaderia__caza__silvicultura_y_pesca_",1,0)
r$so21_iv <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "comercio__ventas__al_por_mayor_y_al_por_menor",1,0)
r$so21_v <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "construccion",1,0)
r$so21_vi <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "explotacion_de_minas_y_canteras",1,0)
r$so21_vii <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "industrias_manufactureras",1,0)
r$so21_viii <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "otra_actividad_economica",1,0)
r$so21_ix <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "restaurantes_y_hoteles",1,0)
r$so21_x <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "servicios_a_los_hogares_y_servicio_domestico",1,0)
r$so21_xi <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "servicios_comunitarios__sociales_y_personales",1,0)
r$so21_xii <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "suministro_de_electricidad__gas_y_agua",1,0)
r$so21_xiii <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "transporte__almacenamiento_y_comunicaciones",1,0)



# cuota media del gasto en alimentos
r$so22_i <- r$sa4


# cuota media del gasto en gastos del hogar (agua domestico, renta, electricidad, recoleccion basura, construccion o reparacion de casa, textiles y bienes para el mantenimiento del hogar)
r$so22_ii <- (as.numeric(r$gastos_renta) + as.numeric(r$gastos_agua_domestico) + as.numeric(r$gastos_electricidad) +
               as.numeric(r$gastos_basura) + as.numeric(r$gastos_construccion / 6) + as.numeric(r$gastos_textiles / 6)) / as.numeric(r$exp_total)


# cuota media del gasto en gastos essenciales (productos de higiene, gastos salud, vestimenta, educacion)
r$so22_iii <- (as.numeric(r$gastos_higiene) + as.numeric(r$gastos_medicos / 6) + as.numeric(r$gastos_vestimenta / 6) + 
                 as.numeric(r$gastos_educacion / 6)) / as.numeric(r$exp_total)


# cuota media del gasto total en gastos de transporte y combustibles (transporte, lena carbon gas, gasolina)
r$so22_iv <- (as.numeric(r$gastos_transporte) + as.numeric(r$gastos_lena) + as.numeric(r$gastos_gasolina)) / as.numeric(r$exp_total)


#cutoa media del gasto en gastos de comunicacion
r$so22_v <- as.numeric(r$gastos_comunicacion)/ as.numeric(r$exp_total)


# cuota media del gasto en gastos de deudas
r$so22_vi <- as.numeric(r$gastos_deudas / 6) / as.numeric(r$exp_total)


# cuota media del gasto en gastos de seguros
r$so22_vii <- as.numeric(r$gastos_seguros / 6) / as.numeric(r$exp_total)

# cuota media del gasto en gastos de insumos productivos de agricultura
r$so22_viii <- as.numeric(r$gastos_insumos / 6) / as.numeric(r$exp_total)

# cuota media del gasto en otros gastos 30d
r$so22_ix <- as.numeric(r$gastos_otros) / as.numeric(r$exp_total)




###############################################################
# ASISTENCIA HUMANITARIA
###############################################################
# % de hogares que declaran haber recibido ayuda de una organizacion no gubernamental en los ultimos 6 meses
r$ah1 <- ifelse(r$asistencia_organizacion == "si",1,0)


# % de hogares que declaran haber recibido ayuda del gobierno en los ultimos 6 meses
r$ah2 <- ifelse(r$asistencia_gobierno == "si",1,0)


# % de hogares que declaran haber recibido ayuda del Programa Mundial de Alimentos en los ultimos 6 meses
r$ah3 <- ifelse(r$asistencia_PMA == "si",1,0)


# % de hogares que declaran haber recibido ayuda de su comunidad, familia o amigos para cubrir el costo de alimentos u otras necesidades en los ultimos 6 meses
r$ah4 <- ifelse(r$asistencia_familia == "si",1,0)


# % de hogares que declaran haber recibido ayuda de su comunidad, familia o amigos para cubrir el costo de alimentos u otras necesidades en los ultimos 6 meses por tipo de familiares que han prestado la asistencia
r$ah4_i <- ifelse(r$asistencia_familia_quien == "amigos",1,0)
r$ah4_ii <- ifelse(r$asistencia_familia_quien == "familiares_que_viven_en_colombia",1,0)
r$ah4_iii <- ifelse(r$asistencia_familia_quien == "familiares_que_viven_fuera_de_colombia",1,0)
r$ah4_iv <- ifelse(r$asistencia_familia_quien == "iglesia",1,0)
r$ah4_v <- ifelse(r$asistencia_familia_quien == "miembros_de_la_comunidad",1,0)




###############################################################
# SITUACION DE LA VIVIENDA Y ACTIVOS DEL HOGAR
###############################################################
# % de hogares por tipo de vivienda
r <- r %>%
  mutate(tipo_vivienda = na_if(tipo_vivienda, "_"))
r$v1_i <- ifelse(r$tipo_vivienda == "apartamento_apartaestudio",1,0)
r$v1_ii <- ifelse(r$tipo_vivienda == "casa",1,0)
r$v1_iii <- ifelse(r$tipo_vivienda == "habitacion_cuarto_pieza_en_otro_tipo_de_estructura__parqueaderos__depositos__bodegas__iglesias__colegios__fabricas__cuarto_para_portero_o_celador_en_un_edificio_de_apartamentos_",1,0)
r$v1_iv <- ifelse(r$tipo_vivienda == "habitacion_cuarto_pieza_en_un_inquilinato",1,0)
r$v1_v <- ifelse(r$tipo_vivienda == "situacion_de_calle_con_espacio_para_alojarse__carpa__vagon__embarcacion__cueva__refugio_natural__etc__",1,0)
r$v1_vi <- ifelse(r$tipo_vivienda == "otro_",1,0)
r$v1_vii <- ifelse(r$tipo_vivienda == "vivienda_improvisada__construcciones_informales_con_materiales_menos_durables__cambuches__etc__",1,0)



# % de hogares que declaran tener los siguentes servicios en su vivienda
r$v2_i <- ifelse(r$servicios_acueducto == "si", 1,0)
r$v2_ii <- ifelse(r$servicios_energia_electrica == "si", 1,0)
r$v2_iii <- ifelse(r$servicios_gas == "si", 1,0)
r$v2_iv <- ifelse(r$servicios_alcantarillado == "si", 1,0)
r$v2_v <- ifelse(r$servicios_recoleccion_basura == "si", 1,0)


# % de hogares que reportan que el agua del acueducto no llega las 24 horas del dia durante los siete dias de la semana
r$v3 <- ifelse(r$acueducto_24h == "no", 1,0)


# % de hogares en los que todos los miembros del hogar duermen en la misma habitacion
r$v4 <- ifelse(r$nr_cuartos_duermen == 1 & r$nr_personas_hogar != 0, 1,0)


# % de hogares en los que hay mas de 2 personas por habitacion
r$personas_por_habitacion <- r$nr_personas_hogar / as.numeric(as.character(r$nr_cuartos_total))
r$v5_i <- ifelse(r$personas_por_habitacion <= 1,1,0)
r$v5_ii <- ifelse(r$personas_por_habitacion > 1 & r$personas_por_habitacion < 2,1,0)
r$v5_iii <- ifelse(r$personas_por_habitacion > 2,1,0)


# % de hogares que utilizan servicios de saneamiento mejorados
r$v6 <- ifelse(r$tipo_servicio_sanitario %in% c("inodoro_conectado_a_alcantarillado", "inodoro_conectado_a_pozo_septico"),1,0)


# % de hogares con fuentes de agua mejoradas
r$v7 <- ifelse(r$fuente_agua %in% c("aguas_lluvias", "de_pozo_sin_bomba__aljibe__jaguey_o_barreno", "rio__quebrada__nacimiento_o_manantial"),0,1)


# % de hogares que declaran que cocinan en una habitacion que solo se utiliza para cocinar
r$v8 <- ifelse(r$lugar_preparacion_alimentos == "en_un_cuarto_usado_solo_para_cocinar",1,0)


# % de hogares por tipo de energia o combustible con que concinan en su hogar
r$v9_i <- ifelse(r$tipo_energia_cocinar == "electricidad",1,0)
r$v9_ii <- ifelse(r$tipo_energia_cocinar == "gas_natural_conectado_a_red_publica",1,0)
r$v9_iii <- ifelse(r$tipo_energia_cocinar == "gas_propano_en_cilindro_o_pipeta",1,0)
r$v9_iv <- ifelse(r$tipo_energia_cocinar == "lena__madera_o_carbon_de_lena",1,0)
r$v9_v <- ifelse(r$tipo_energia_cocinar == "petroleo__gasolina__kerosene__alcohol",1,0)


# % de hogares por sexo de la persona que tiene el titulo de propriedad de la vivienda
r$v10_i <- ifelse(r$sexo_titulo_propiedad == "hombre",1,0)
r$v10_ii <- ifelse(r$sexo_titulo_propiedad == "mujer",1,0)
r$v10_iii <- ifelse(r$sexo_titulo_propiedad == "ambos__mujer_y_hombre_",1,0)


# % de hogares sin telefono movil
r$v11 <- ifelse(r$bienes_celular == "no",1,0)


# % de hogares que declaran que la tierra o arena es el material principal de los pisos de la vivienda
r$v12 <- ifelse(r$material_pisos == "tierra__arena",1,0)


# % de hogares que declaran que la lata u otros materiales improvisados son el material principal de sus paredes
r$v13 <- ifelse(r$material_paredes_exteriores %in% c("zinc__tela__carton__latas__desechos__plastico", "cana__esterilla__otro_tipo_de_material_vegetal"),1,0)


# % de hogares que declaran que la fuente principal de agua potable es el grifo publico o compartido, el pozo, el rio o el agua lluvia
r$v14 <- ifelse(r$fuente_agua %in% c("aguas_lluvias", "de_pozo_sin_bomba__aljibe__jaguey_o_barreno", "rio__quebrada__nacimiento_o_manantial", "de_pila_publica", "de_pozo_con_bomba"),0,1)


# % de hogares que declaran la deificacion al aire libre o los espacios publicos como su tipo de instalacion de saneamiento


# % de hogares que declaran compartir su instalacion de saneamiento con otros hogares
r$v16 <- ifelse(r$servicio_sanitario_compartido == "compartido_con_personas_de_otros_hogares_",1,0)



# % de hogares por acuerdo de ocupacion
r$v17_i <- ifelse(r$acuerdo_ocupacion == "en_arriendo_subarriendo",1,0)
r$v17_ii <- ifelse(r$acuerdo_ocupacion == "en_usufructo__aunque_no_es_propia__usa_la_vivienda_y_no_paga_ningun_valor___como_si_fuera_propia__aunque_este_a_nombre_de_otra_persona_",1,0)
r$v17_iii <- ifelse(r$acuerdo_ocupacion == "paga_diario",1,0)
r$v17_iv <- ifelse(r$acuerdo_ocupacion == "otra",1,0)
r$v17_v <- ifelse(r$acuerdo_ocupacion == "posesion_sin_titulo__ocupante_de_hecho__o_propiedad_colectiva",1,0)
r$v17_vi <- ifelse(r$acuerdo_ocupacion == "propia__la_estan_pagando",1,0)
r$v17_vii <- ifelse(r$acuerdo_ocupacion == "propia__totalmente_pagada",1,0)


###############################################################
# AFECTACIONES
###############################################################
# % de hogares que han sido afectado por conflicto armado en los ultimos 10 anos
r$af1 <- ifelse(r$afectado_conflicto == "si",1,0)

# % de hogares que han sido afectado por conflicto armado en los ultimos 10 anos
r$af2 <- ifelse(r$afectado_desastre_natural == "si",1,0)

# % de hogares que han sido afectado por conflicto armado por hecho de victimizantes
r$af3_i <- ifelse(r$afectado_conflicto_confinamiento == "si",1,0)
r$af3_ii <- ifelse(r$afectado_conflicto_desparicion == "si",1,0)
r$af3_iii <- ifelse(r$afectado_conflicto_desplazamiento == "si",1,0)
r$af3_iv <- ifelse(r$afectado_conflicto_homicidio == "si",1,0)
r$af3_v <- ifelse(r$afectado_conflicto_integridad_sexual == "si",1,0)
r$af3_vi <- ifelse(r$afectado_conflicto_lesiones_discapacidad == "si",1,0)
r$af3_vii <- ifelse(r$afectado_conflicto_masacre == "si",1,0)
r$af3_viii <- ifelse(r$afectado_conflicto_recrutamiento_menores == "si",1,0)
r$af3_ix <- ifelse(r$afectado_conflicto_secuestro == "si",1,0)
r$af3_x <- ifelse(r$afectado_conflicto_tortura == "si",1,0)


# % de hogares que han sido afectado por desastres naturales por hecho de victimizantes
r$af5_i <- ifelse(r$afectado_desastre_natural_creciente_subita == "si",1,0)
r$af5_ii <- ifelse(r$afectado_desastre_natural_inundaciones == "si",1,0)
r$af5_iii <- ifelse(r$afectado_desastre_natural_deslizamientos_tierra == "si",1,0)
r$af5_iv <- ifelse(r$afectado_desastre_natural_terremotos == "si",1,0)
r$af5_v <- ifelse(r$afectado_desastre_natural_sequias == "si",1,0)
r$af5_vi <- ifelse(r$afectado_desastre_natural_sismos == "si",1,0)
r$af5_vii <- ifelse(r$afectado_desastre_natural_creciente_subita == "si",1,0)
r$af5_viii <- ifelse(r$afectado_desastre_natural_vendaval == "si",1,0)
r$af5_ix <- ifelse(r$afectado_desastre_natural_huracan == "si",1,0)
r$af5_x <- ifelse(r$afectado_desastre_natural_tormenta_tropical == "si",1,0)


# de hogares que han sido afectado por conflicto o desastre naturales que han perdido sus fuentes de ingreso o algun bien por consequencia
r$af7 <- ifelse(r$perdido_fuentes_ingreso == "si",1,0)





###############################################################
# CARACTERISTICAS SOCIODEMOGRAFICAS
###############################################################
# % de hogares por sexo del jefe del hogar
r$d1_1 <- ifelse(r$sexo_jh == "hombre", 1,0)
r$d1_2 <- ifelse(r$sexo_jh == "mujer", 1,0)
r$d1_3 <- ifelse(r$sexo_jh == "la_jefatura_es_compartida_entre_hombre_y_mujer",1,0)
r$d1_4 <- ifelse(r$sexo_jh == "la_jefatura_es_compartida_entre_mujeres",1,0)
r$d1_5 <- ifelse(r$sexo_jh == "la_jefatura_es_compartida_entre_hombres",1,0)


# razón por la que una persona es considerada jefe de hogar (% de hogares) 
r$d2_i <- ifelse(r$razon_jefatura_hogar == "cultural__asi_es_la_tradicion",1,0)
r$d2_ii <- ifelse(r$razon_jefatura_hogar == "es_la_persona_que_aporta_la_mayor_parte_de_los_recursos",1,0)
r$d2_iii <- ifelse(r$razon_jefatura_hogar == "es_la_persona_que_toma_las_decisiones",1,0)
r$d2_iv <- ifelse(r$razon_jefatura_hogar == "no_hay_nadie_mas_con_quien_compartir_decisiones_o_gastos__jefatura_unica_por_parte_de_mujer_o_de_hombre_",1,0)
r$d2_v <- ifelse(r$razon_jefatura_hogar == "toma_decisiones_y_aporta_la_mayor_parte_de_recursos",1,0)


# Promedio miembros del hogar
r$d3 <- as.numeric(r$nr_personas_hogar)



# % de hogares con al menos un miembro en estado de embarazo
r$d4 <- ifelse(r$presencia_embarazo == "si", 1, 0)


# % de hogares con al menos un miembro mayor de 65 anos
r$d5 <- ifelse(r$presencia_65 == "si", 1,0)


# % de hogares con al menos un miembro menor de 5 anos
r$d6 <- ifelse(r$presencia_0_59_meses == "si",1,0)



# % de hogares por nivel educativo del jefe del hogar



# % de hogares en los que el jefe del hogar tiene una discapacidad
r$d8 <- ifelse(r$discapacidad_jh == "con_discapacidad", 1, 0)


# % de hogares en los que el jefe del hogar padece una enfermedad cronica
r$d9 <- ifelse(r$enfermedad_cronica_jh == "si", 1, 0)


# % de hogares en los que el jefe del hogar padece una enfermedad mental
r$d10 <- ifelse(r$enfermedad_mental_jh == "si", 1, 0)


# % de hogares por pertenencia étnica del encuestado
r$d11_i <- ifelse(r$ind1_pertenencia_etnica == "afrodescendiente__negro__mulato_", 1, 0)
r$d11_ii <- ifelse(r$ind1_pertenencia_etnica == "indigena", 1, 0)
r$d11_iii <- ifelse(r$ind1_pertenencia_etnica == "gitano_rrom", 1, 0)
r$d11_iv <- ifelse(r$ind1_pertenencia_etnica == "mestizo", 1, 0)
r$d11_v <- ifelse(r$ind1_pertenencia_etnica == "ninguno", 1, 0)
r$d11_vi <- ifelse(r$ind1_pertenencia_etnica == "palenquero", 1, 0)
r$d11_vii <- ifelse(r$ind1_pertenencia_etnica == "raizal", 1, 0)


# % de hogares en los que el encuestado se identifica como transgenero
r$d12 <- ifelse(r$ind1_genero == "hombre_trans" | 
                  r$ind1_genero == "mujer_trans", 1, 0)



# % de hogares por rango de edad del jefe


# % de hogares monoparentales
r$d14 <- ifelse(r$monoparental == "si",1,0)


# % de hogares por rango de la tasa de dependencia
# tasa de dependencia por rangos
loop$edad <- as.numeric(loop$edad)
r <- loop %>% 
  mutate(dependent = ifelse(edad < 15 | edad > 64,1,0),
         independent = ifelse(edad >=15 & edad <= 64,1,0)) %>%
  group_by(registro) %>%
  dplyr::summarise(nr_dependent = sum(dependent),
                   nr_independent = sum(independent),
                   hh_size = n()) %>%
  mutate(dependency_ratio = nr_dependent / nr_independent,
         dependency_ratio = ifelse(dependency_ratio == "Inf", "all_dependent", dependency_ratio),
         p4 = ifelse(dependency_ratio == "all_dependent",NA,dependency_ratio),
         p4_i = ifelse(dependency_ratio == 0,1,0),
         p4_i = ifelse(dependency_ratio == "all_dependent",0, p4_i),
         
         p4_ii = ifelse(dependency_ratio > 0 & dependency_ratio <= 1,1,0),
         p4_ii = ifelse(dependency_ratio == "all_dependent",0, p4_ii),
         
         p4_iv = ifelse(dependency_ratio > 1,1,0),
         p4_iv = ifelse(dependency_ratio == "all_dependent",0, p4_iv),
         
         p4_v = ifelse(dependency_ratio == "all_dependent",1,0)) %>%
  select(starts_with("p4"), registro) %>%
  right_join(r)



r <- r %>% mutate(dependency_ratio = case_when(
  p4_i == 1 ~ "0", 
  p4_ii == 1 ~ "0-0.5",
  p4_iv == 1 ~ ">1",
  p4_v == 1 ~ "all_dependent"))


# % de hogares por rangos del tamano del hogar
r$d16_i <- ifelse(r$nr_personas_hogar == 1, 1,0)
r$d16_ii <- ifelse(r$nr_personas_hogar > 1 & r$nr_personas_hogar <= 2, 1,0)
r$d16_iii <- ifelse(r$nr_personas_hogar > 2 & r$nr_personas_hogar <= 3, 1,0)
r$d16_iv <- ifelse(r$nr_personas_hogar > 3 & r$nr_personas_hogar <= 4, 1,0)
r$d16_v <- ifelse(r$nr_personas_hogar > 4 & r$nr_personas_hogar <= 5, 1,0)
r$d16_vi <- ifelse(r$nr_personas_hogar > 5, 1,0)


# % de hogares en los que el jefe del hogar NO tiene una discapacidad fisica, enfermedad cronica o enfermedad mental
r$d17 <- ifelse(r$d8 == 0 & r$d9 == 0 & r$d10 == 0,1,0)


# % de hogares por nivel educativo del jefe del hogar
r$d7_i <- ifelse(r$nivel_estudios_jh == "primaria_completa",1,0)
r$d7_ii <- ifelse(r$nivel_estudios_jh == "primaria_incompleta",1,0)
r$d7_iii <- ifelse(r$nivel_estudios_jh == "secundaria_completa",1,0)
r$d7_iv <- ifelse(r$nivel_estudios_jh == "secundaria_incompleta",1,0)
r$d7_v <- ifelse(r$nivel_estudios_jh == "sin_educacion",1,0)
r$d7_vi <- ifelse(r$nivel_estudios_jh == "tecnico_tecnologico_completo",1,0)
r$d7_vii <- ifelse(r$nivel_estudios_jh == "tecnico_tecnologico_incompleto",1,0)
r$d7_viii <- ifelse(r$nivel_estudios_jh == "universitario_completo_o_postgrado",1,0)
r$d7_ix <- ifelse(r$nivel_estudios_jh == "universitario_incompleto",1,0)


r <- r %>% mutate(nivel_estudios_grupo = case_when(
  r$nivel_estudios_jh == "sin_educacion" | r$nivel_estudios_jh == "primaria_incompleta"  ~ "sin_educacion",
  r$nivel_estudios_jh == "primaria_completa" | r$nivel_estudios_jh == "secundaria_incompleta"  ~ "primaria",
  r$nivel_estudios_jh == "secundaria_completa" | r$nivel_estudios_jh == "universitario_incompleto" |
    r$nivel_estudios_jh == "tecnico_tecnologico_incompleto" ~ "secundaria",
  r$nivel_estudios_jh == "universitario_completo_o_postgrado" | r$nivel_estudios_jh == "tecnico_tecnologico_completo"  ~ "universitario_tecnico",
  TRUE ~ "0"
))




###############################################################
# ASISTENCIA ESCOLAR
###############################################################
# % de hogares en los que al menos un nino que asiste a la escuela y no se beneficia del PAE
#r$ae2 <- ifelse(as.numeric(as.character(r$nr_escuela_colegio)) > as.numeric(as.character(r$nr_PAE)),1,0)
#r$ae2 <- ifelse(as.numeric(as.character(r$nr_escuela_colegio)) == 0,0,
#                r$ae2)









return(r)
}

