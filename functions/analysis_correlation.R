library(ggplot2)
library(ggpubr)

conflicto <- df %>% filter(afectado_conflicto == "si")
r <- conflicto



##########################################################################################################################
##########################################################################################################################
# RENAME THE EXPRESSIONS OF VARIABLES WITH LONG NAMES THAT ARE DIFFICULT TO BE DISPLAYED IN A SINGLE CHART
##########################################################################################################################
##########################################################################################################################

r <- r %>% 
  mutate(estado_civil_jh = dplyr::recode(estado_civil_jh, "esta_casado_a_" = "casado", 
                                  "esta_separado_a__o_divorciado_a_" = "divorciado", 
                                  "esta_soltero_a_" = "soltero", 
                                  "esta_viudo_a_" = "viudo", 
                                  "vive_en_union_libre" = "union_libre"))


r <- r %>% 
  dplyr::mutate(ind1_pertenencia_etnica = dplyr::recode(ind1_pertenencia_etnica, 
                                  "afrodescendiente__negro__mulato_" = "afro", 
                                  "gitano_rrom" = "gitano", 
                                  "otra_" = "otra_etnia", 
                                  "ninguna" = "ningun_etnia"))


r <- r %>% 
  mutate(fuente_ingresos = dplyr::recode(fuente_ingresos, 
                                                 "asistencia_de_naciones_unidas__ongs__organizaciones_caritativas" = "asist_hum", 
                                                 "asistencia_del_gobierno" = "asist_gob", 
                                                "el_hogar_no_recibe_ingresos_de_ningun_tipo" = "no_tiene",
                                                "pension__sistema_de_proteccion_social_" = "pension", 
                                                "otra__rentas__azar__herencias__" = "otra", 
                                                "remesas_de_migrantes_o_ayuda_de_familiares_amigos" = "remesas", 
                                                "trabajo__cualquier_actividad_economica_que_genera_ingresos_" = "trabajo"))



r <- r %>% 
  mutate(miembro_mayor_recursos_empleo = dplyr::recode(miembro_mayor_recursos_empleo, 
                                         "empleado_a__domestico_a_" = "empl_domestico", 
                                         "jornalero_o_peon" = "jornalero", 
                                         "obrero_a__o_empleado_a__de_empresa_particular" = "empleado_empresa",
                                         "obrero_a__o_empleado_a__del_gobierno" = "empleado_gob", 
                                         "patron_o_empleador" = "patron", 
                                         "trabajador_familiar_sin_remuneracion" = "trab_familiar", 
                                         "trabajador_por_cuenta_propia" = "trabajador_propia", 
                                         "trabajador_sin_remuneracion_en_empresas_o_negocios_de_otros_hogares" = "trabajador_sinrenum"))


r <- r %>% 
  mutate(razon_deuda = dplyr::recode(razon_deuda, 
                                                       "compra_de_activos__casa__apartamento__carro__moto__electrodomesticos_etc__" = "compra_activos", 
                                                       "comprar_comida" = "comida", 
                                                       "comprar_insumos_productivos" = "insumos_productivos",
                                                       "comprar_ropa__zapatos" = "ropa_zapatos", 
                                                       "cubrir_gastos_de_salud" = "salud", 
                                                       "cubrir_servicios_basicos__agua__electricidad_" = "servic_basicos", 
                                                       "ns_nr__strong_e__no_leer__strong_" = "ns_nr", 
                                                       "pagar_la_escuela_o_gastos_de_educacion" = "educacion", 
                                                       "pagar_renta_o_alquiler_de_la_vivienda" = "renta"))


r <- r %>% 
  mutate(material_paredes_exteriores = dplyr::recode(material_paredes_exteriores, 
                                                       "adobe_o_tapia_pisada" = "adobe", 
                                                       "cana__esterilla__otro_tipo_de_material_vegetal" = "cana", 
                                                       "ladrillo__bloque__material_prefabricado__piedra" = "ladrillo",
                                                       "madera_burda__tabla__tablon" = "madera_burda", 
                                                       "ns_nr_strong__e__no_leer__strong_" = "ns_nr", 
                                                       "zinc__tela__carton__latas__desechos__plastico" = "zinc"))



r <- r %>% 
  mutate(material_pisos = dplyr::recode(material_pisos, 
                                                     "alfombra_o_tapete_de_pared_a_pared" = "afombra", 
                                                     "baldosin__ladrillo__vinisol__otros_materiales_sinteticos" = "baldosin", 
                                                     "cemento__gravilla" = "cemento",
                                                     "madera_burda__tabla__tablon__otro_vegetal" = "madera_burda", 
                                                     "ns_nr_strong__e__no_leer__strong_" = "ns_nr", 
                                                     "tierra__arena" = "tierra"))


r <- r %>% 
  mutate(material_techo = dplyr::recode(material_techo, 
                                        "madera_burda__tabla__tablon__otro_vegetal" = "madera_burda", 
                                        "paja_bambu_techo_de_paja" = "paja", 
                                        "tejas__barro__zinc__eternit_" = "tejas", 
                                        "ns_nr__strong_e__no_leer__strong_" = "ns_nr"))


r <- r %>% 
  mutate(lugar_preparacion_alimentos = dplyr::recode(lugar_preparacion_alimentos, 
                                        "en_ninguna_parte__no_preparan_alimentos" = "ningun_parte", 
                                        "en_un_cuarto_usado_solo_para_cocinar" = "cuart_cocinar", 
                                        "en_un_cuarto_usado_tambien_para_dormir" = "cuart_dormir", 
                                        "en_un_patio__corredor__enramada__al_aire_libre" = "aire_libre", 
                                        "en_una_sala_comedor_con_lavaplatos" = "comedor_lavaplato", 
                                        "en_una_sala_comedor_sin_lavaplatos" = "comedor_sin_lavaplatos", 
                                        "ns_nr__strong_e__no_leer__strong_" = "ns_nr"))




r <- r %>% 
  mutate(lugar_preparacion_alimentos = dplyr::recode(lugar_preparacion_alimentos, 
                                                     "gas_natural_conectado_a_red_publica" = "gas_red", 
                                                     "gas_propano_en_cilindro_o_pipeta" = "gas_cilindro", 
                                                     "lena__madera_o_carbon_de_lena" = "lena", 
                                                     "ns_nr_e__no_leer" = "ns_nr", 
                                                     "petroleo__gasolina__kerosene__alcohol" = "petroleo", 
                                                     "carbon_mineral" = "carbon"))


r <- r %>% 
  mutate(servicio_sanitario_compartido = dplyr::recode(servicio_sanitario_compartido, 
                                                     "compartido_con_personas_de_otros_hogares_" = "compartido", 
                                                     "de_uso_exclusivo_de_las_personas_del_hogar" = "exclusivo", 
                                                     "ns_nr_strong__e__no_leer__strong_" = "ns_nr"))


r <- r %>% 
  mutate(eliminacion_basura = dplyr::recode(eliminacion_basura, 
                                                       "la_eliminan_de_otra_forma" = "otra_forma", 
                                                       "la_queman_o_entierran" = "queman", 
                                                       "la_tiran_a_un_patio__lote__zanja_o_baldio" = "tiran_patio", 
                                            "la_tiran_a_un_rio__quebrada__cano_o_laguna" = "tiran_rio", 
                                            "ns_nr_strong__e__no_leer__strong_" = "ns_nr", 
                                            "por_recoleccion_publica_o_privada" = "recoleccion"))


r <- r %>% 
  mutate(fuente_agua = dplyr::recode(fuente_agua, 
                                            "agua_embotellada_o_en_bolsa" = "botella", 
                                            "aguas_lluvias" = "lluvia", 
                                            "de_otra_fuente_por_tuberia" = "otra_tuberia", 
                                            "de_acueducto_por_tuberia" = "acue_tuberia", 
                                            "de_pila_publica" = "pila_publica", 
                                            "de_pozo_con_bomba" = "pozo_bomba", 
                                     "de_pozo_sin_bomba__aljibe__jaguey_o_barreno" = "pozo_sin_bomba", 
                                     "ns_nr_e__no_leer" = "ns_nr",
                                    "rio__quebrada__nacimiento_o_manantial" = "rio"
                                     ))


r <- r %>% 
  mutate(tipo_vivienda = dplyr::recode(tipo_vivienda, 
                                     "apartamento_apartaestudio" = "apartamento", 
                                     "habitacion_cuarto_pieza_en_otro_tipo_de_estructura__parqueaderos__depositos__bodegas__iglesias__colegios__fabricas__cuarto_para_portero_o_celador_en_un_edificio_de_apartamentos_" = "habitacion_apto", 
                                     "habitacion_cuarto_pieza_en_un_inquilinato" = "habitacion_inquilinato", 
                                     "ns_nr_strong__e__no_leer__strong_" = "ns_nr", 
                                     "situacion_de_calle_con_espacio_para_alojarse__carpa__vagon__embarcacion__cueva__refugio_natural__etc__" = "situacion_calle", 
                                     "vivienda_improvisada__construcciones_informales_con_materiales_menos_durables__cambuches__etc__" = "vivienda_improvisada"
  ))



r <- r %>% 
  mutate(cambio_ingresos = dplyr::recode(cambio_ingresos, 
                                     "aumentaron_los_ingresos" = "aumentaron", 
                                     "disminuyeron_los_ingresos" = "disminuyeron", 
                                     "no_hubo_cambios" = "no_cambios", 
                                     "ns_nr__strong_e__no_leer__strong_" = "ns_nr", 
                                     "se_perdieron_los_ingresos_por_completo" = "perdieron"
  ))


r <- r %>% 
  mutate(acueducto_24h = dplyr::recode(acueducto_24h, 
                                         "ns_nr_strong__e__no_leer__strong_" = "ns_nr"
  ))


##########################################################################################################################
##########################################################################################################################
# BINARY VARIABLES
##########################################################################################################################

# Sexo del jefe del hogar
plot_bin_1 <- ggplot(r, aes(x = sexo_jefatura, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por sexo jefatura")


# Discapacidad jefe del hogar
plot_bin_2 <- ggplot(r, aes(x = discapacidad_jh, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por discapacidad fisica del jh")


# Enfermedad cronica jefe del hogar
plot_bin_3 <- ggplot(r, aes(x = enfermedad_cronica_jh, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por enfermedad cronica del jh")


# Enfermedad mental jefe del hogar
plot_bin_4 <- ggplot(r, aes(x = enfermedad_mental_jh, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por enfermedad mental del jh")


# Urbano Rural
plot_bin_5 <- ggplot(r, aes(x = urbano_rural, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por urbano-rural")


# Presencia de deuda
plot_bin_6 <- ggplot(r, aes(x = deuda, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia de deuda")


# Afectado conflicto
plot_bin_7 <- ggplot(r, aes(x = afectado_conflicto, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por afectados por conflicto")


# Afectado desastres naturales
plot_bin_8 <- ggplot(r, aes(x = afectado_desastre_natural, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por afectados por desastres naturales")


# Servicios: energia electrica
plot_bin_9 <- ggplot(r, aes(x = servicios_energia_electrica, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicio energia electrica")


# Servicios: gas
plot_bin_10 <- ggplot(r, aes(x = servicios_gas, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicio gas")


# Servicios: alcantarillado
plot_bin_11 <- ggplot(r, aes(x = servicios_alcantarillado, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicio alcantarillado")


# Servicios: recoleccion basura
plot_bin_12 <- ggplot(r, aes(x = servicios_recoleccion_basura, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicio recoleccion basura")


# Agua del acueducto llega las 24h los 7 dias de la semana
plot_bin_13 <- ggplot(r, aes(x = acueducto_24h, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por agua llega 24h")



# Servicio sanitario compartido
plot_bin_14 <- ggplot(r, aes(x = servicio_sanitario_compartido, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicio sanitario compartido")



# Presencia bienes: Colchon
plot_bin_15 <- ggplot(r, aes(x = bienes_colchon, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia colchon")


# Presencia bienes: Computador
plot_bin_16 <- ggplot(r, aes(x = bienes_computador, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia computador")


# Presencia bienes: Horno
plot_bin_17 <- ggplot(r, aes(x = bienes_horno, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia horno")


# Presencia bienes: Horno
plot_bin_18 <- ggplot(r, aes(x = bienes_nevera, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia nevera")


# Presencia bienes: Televisor
plot_bin_19 <- ggplot(r, aes(x = bienes_televisor, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia televisor")


# Presencia bienes: Celular
plot_bin_20 <- ggplot(r, aes(x = bienes_celular, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia celular")


# Presencia servicios: Celular
plot_bin_21 <- ggplot(r, aes(x = servicios_telefono_fijo, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicios telefono fijo")


# Presencia servicios: Internet
plot_bin_22 <- ggplot(r, aes(x = servicios_internet, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicios internet")


# Presencia servicios: Televisor subscripcion
plot_bin_23 <- ggplot(r, aes(x = servicios_television_subscripcion, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicios televisor subscripcion")


# Presencia medio transporte: bicicleta
plot_bin_24 <- ggplot(r, aes(x = transporte_bicicleta, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por transporte bicicleta")


# Presencia medio transporte: motocicleta
plot_bin_25 <- ggplot(r, aes(x = transporte_motocicleta, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por transporte motocicleta")


# Presencia medio transporte: carro
plot_bin_26 <- ggplot(r, aes(x = transporte_carro_particular, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por transporte carro")



multi_plot_1 <- ggarrange(plot_bin_1, plot_bin_2, plot_bin_3, plot_bin_4,
                    labels = c("1.", "2.", "3.", "4."),
                    ncol = 2, nrow = 2)
multi_plot_2 <- ggarrange(plot_bin_5, plot_bin_6, plot_bin_7, plot_bin_8,
                          labels = c("5.", "6.", "7.", "8."),
                          ncol = 2, nrow = 2)
multi_plot_3 <- ggarrange(plot_bin_9, plot_bin_10, plot_bin_11, plot_bin_12,
                          labels = c("9.", "10.", "11.", "12."),
                          ncol = 2, nrow = 2)
multi_plot_4 <- ggarrange(plot_bin_13, plot_bin_14, plot_bin_15, plot_bin_16,
                          labels = c("13.", "14.", "15.", "16."),
                          ncol = 2, nrow = 2)
multi_plot_5 <- ggarrange(plot_bin_17, plot_bin_18, plot_bin_19, plot_bin_20,
                          labels = c("17.", "18.", "19.", "20."),
                          ncol = 2, nrow = 2)
multi_plot_6 <- ggarrange(plot_bin_21, plot_bin_22, plot_bin_23, plot_bin_24,
                          labels = c("21.", "22.", "23.", "24."),
                          ncol = 2, nrow = 2)
multi_plot_7 <- ggarrange(plot_bin_25, plot_bin_26,
                          labels = c("25.", "26."),
                          ncol = 2, nrow = 2)

multi_plot_1
multi_plot_2
multi_plot_3
multi_plot_4
multi_plot_5
multi_plot_6
multi_plot_7



##########################################################################################################################
##########################################################################################################################
# MULTIEXPRESSION VARIABLES
##########################################################################################################################


# Estado civil del jefe del hogar
plot_mul_1 <- ggplot(r, aes(x = estado_civil_jh, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por estado civil del jh")

# Nivel de estudios del jefe del hogar
plot_mul_2 <- ggplot(r, aes(x = nivel_estudios_grupo, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por nivel educativo del jh")


# Genero del respondiente
plot_mul_3 <- ggplot(r, aes(x = ind1_genero, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por genero del respondiente")


# Fuente de ingresos
plot_mul_4 <- ggplot(r, aes(x = fuente_ingresos, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por principal fuente de ingresos")


# Tipo de empleo
plot_mul_5 <- ggplot(r, aes(x = miembro_mayor_recursos_empleo, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por tipo de empleo")


# Pertenencia etnica del respondiente
plot_mul_6 <- ggplot(r, aes(x = ind1_pertenencia_etnica, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por pertenencia etnica")


# Cambio de ingresos
plot_mul_7 <- ggplot(r, aes(x = cambio_ingresos, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por cambio de ingresos")


# Tipo de vivienda
plot_mul_8 <- ggplot(r, aes(x = tipo_vivienda, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por tipo de vivienda")


# Material paredes exteriores
plot_mul_9 <- ggplot(r, aes(x = material_paredes_exteriores, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por material de los paredes exteriores")


# Material pisos
plot_mul_10 <- ggplot(r, aes(x = material_pisos, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por material de los pisos")


# Material techo
plot_mul_11 <- ggplot(r, aes(x = material_techo, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por material del techo")


# Lugar en que preparan los alimentos
plot_mul_12 <- ggplot(r, aes(x = lugar_preparacion_alimentos, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por lugar en que prepara alimentos")


# Eliminacion Basura
plot_mul_13 <- ggplot(r, aes(x = eliminacion_basura, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por eliminacion de basura")


# Fuente de agua
plot_mul_14 <- ggplot(r, aes(x = fuente_agua, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por fuente de agua")



bin_plot_1 <- ggarrange(plot_mul_1, plot_mul_2,
                          labels = c("1.", "2."),
                          ncol = 1, nrow = 2)
bin_plot_2 <- ggarrange(plot_mul_3, plot_mul_4,
                        labels = c("3.", "4."),
                        ncol = 1, nrow = 2)
bin_plot_3 <- ggarrange(plot_mul_5, plot_mul_6,
                        labels = c("5.", "6."),
                        ncol = 1, nrow = 2)
bin_plot_4 <- ggarrange(plot_mul_7, plot_mul_8,
                        labels = c("7.", "8."),
                        ncol = 1, nrow = 2)
bin_plot_5 <- ggarrange(plot_mul_9, plot_mul_10,
                        labels = c("9.", "10."),
                        ncol = 1, nrow = 2)
bin_plot_6 <- ggarrange(plot_mul_11, plot_mul_12,
                        labels = c("11.", "12."),
                        ncol = 1, nrow = 2)
bin_plot_7 <- ggarrange(plot_mul_13, plot_mul_14,
                        labels = c("13.", "14."),
                        ncol = 1, nrow = 2)


bin_plot_1
bin_plot_2
bin_plot_3
bin_plot_4
bin_plot_5
bin_plot_6
bin_plot_7



##########################################################################################################################
##########################################################################################################################
# NUMERIC CORRELATIONS
##########################################################################################################################
#edad respondiente
cor.test(r$edad_respondiente, r$cari, method = "pearson", conf.level = 0.95)
cor.test(r$nr_personas_hogar, r$cari, method = "pearson", conf.level = 0.95)
cor.test(r$ingreso_pp, r$cari, method = "pearson", conf.level = 0.95)
cor.test(r$valor_deuda, r$cari, method = "pearson", conf.level = 0.95)






######################################################################################################################################################################################
######################################################################################################################################################################################
# PROFILE ANALYSIS
######################################################################################################################################################################################
######################################################################################################################################################################################

r <- conflicto

r <- r %>% 
  mutate(tipo_vivienda = dplyr::recode(tipo_vivienda, 
                                       "apartamento_apartaestudio" = "apartamento", 
                                       "otro_" = "otro_tipo_vivienda",
                                       "habitacion_cuarto_pieza_en_otro_tipo_de_estructura__parqueaderos__depositos__bodegas__iglesias__colegios__fabricas__cuarto_para_portero_o_celador_en_un_edificio_de_apartamentos_" = "habitacion_apto", 
                                       "habitacion_cuarto_pieza_en_un_inquilinato" = "habitacion_inquilinato", 
                                       "ns_nr_strong__e__no_leer__strong_" = "ns_nr", 
                                       "situacion_de_calle_con_espacio_para_alojarse__carpa__vagon__embarcacion__cueva__refugio_natural__etc__" = "situacion_calle", 
                                       "vivienda_improvisada__construcciones_informales_con_materiales_menos_durables__cambuches__etc__" = "vivienda_improvisada"
  ))

r <- r %>% 
  mutate(tipo_vivienda = dplyr::recode(ind1_pertenencia_etnica, 
                                       "ninguno" = "ningun_etnia", 
                                       "otra_" = "otra_etnia"
  ))

# Create a sample data frame
profile_pair <- r %>% select("inseg_alimentaria_bin", "sa8_ias", "estado_civil_jh", "sexo_jefatura", "nivel_estudios_grupo", "ind1_pertenencia_etnica", 
                                               "urbano_rural", "discapacidad_jh", "enfermedad_cronica", "enfermedad_mental", "afectado_por_conflicto", "afectado_por_desastre_natural", 
                                               "tipo_vivienda", "tipo_empleo_grupos", "fuente_ingresos", "razon_deuda", "material_paredes_exteriores", "material_pisos", "material_techo", 
                             "servicio_sanitario_compartido", "eliminacion_basura", "fuente_agua", "cambio_ingresos", "ano_afectado_conflicto", "tipo_afectacion_conflicto")
colnames_to_pair <- colnames(profile)[5:length(colnames(profile))]


# Get all possible pairs of column names
cols <- colnames(profile_pair)

# create new columns for each possible pair of column names
for (i in 1:(length(cols) - 1)) {
  for (j in (i + 1):length(cols)) {
    colname <- paste(cols[i], cols[j], sep = "_")
    profile_pair[[colname]] <- paste(profile_pair[[i]], profile_pair[[j]], sep = " ")
  }
}
profile_pair <- profile_pair %>% select(-starts_with("inseg_alimentaria_bin_"), -starts_with("sa8_ias_"))



#############################################
# 3 Variables in a combination
#############################################

# Create a sample data frame
profile_triple <- r %>% select("inseg_alimentaria_bin", "sa8_ias", "estado_civil_jh", "sexo_jefatura", "nivel_estudios_grupo", "ind1_pertenencia_etnica", 
                                                    "urbano_rural", "discapacidad_jh", "enfermedad_cronica", "enfermedad_mental", "afectado_por_conflicto", "afectado_por_desastre_natural", 
                                                    "tipo_vivienda", "tipo_empleo_grupos", "fuente_ingresos", "razon_deuda", "material_paredes_exteriores", "material_pisos", "material_techo", 
                               "servicio_sanitario_compartido", "eliminacion_basura", "fuente_agua", "cambio_ingresos", "ano_afectado_conflicto", "tipo_afectacion_conflicto")


# Get all possible pairs of column names
cols <- colnames(profile_triple)


# create empty dataframe to store output
output <- data.frame()

# loop through all possible triples of column names
for (i in 1:length(cols)) {
  for (j in (i+1):length(cols)) {
    for (k in (j+1):length(cols)) {
      
      # create new column name
      new_col <- paste(cols[i], cols[j], cols[k], sep = "_")
      
      # check if column names are not NA before pasting
      if(!is.na(cols[i]) && !is.na(cols[j]) && !is.na(cols[k])) {
        # paste columns together with _
        profile_triple[[new_col]] <- paste(profile_triple[[cols[i]]], profile_triple[[cols[j]]], profile_triple[[cols[k]]], sep = " ")
      }
    }
  }
}



# remove original columns
profile_triple <- profile_triple %>% select(-starts_with("inseg_alimentaria_bin_"), -starts_with("sa8_ias_"))




####################################################################
# ANALYSIS FOR DOUBLE PAIRS
####################################################################
# Example code to calculate mean of "sa1_poor" for multiple separate disaggregations and merge the output data frames using a loop
library(dplyr)

# Define a vector of variables to disaggregate by
var_list <- colnames(profile_pair)[17:ncol(profile_pair)]


# Loop over the variables to disaggregate by
# Initialize an empty list to store the results
output_list <- list()

# loop through each combination of categorical variables
for (i in 1:length(var_list)) {
  
  # calculate the mean of "sa1_poor" for the current combination of categorical variables
  output <- profile_pair %>%
    group_by_at(vars(var_list[[i]])) %>%
    dplyr::summarize(mean_inseg = mean(inseg_alimentaria_bin), pr1_comb_count = n())
  
  # add the output dataframe to the output list
  output_list[[i]] <- output
}

# combine all output dataframes into a single dataframe
final_output_pairs <- do.call(bind_rows, output_list)

# remove all rows where the n is lower than 60
final_output_pairs <- final_output_pairs %>% subset(pr1_comb_count >= 40) %>%
                      arrange(desc(mean_inseg))

#replace all NA cells
final_output_pairs <- as.data.frame(lapply(final_output_pairs, function(x) {
  x[is.na(x)] <- "gaggi"
  x
}))


# create a new column which contains the combinations
final_output_pairs$var_combinations <- apply(final_output_pairs[, 4:ncol(final_output_pairs)], 1, function(x) {
  paste0(x[x != "gaggi"], collapse = "")
})

#delete all non-used columns
final_output_pairs <- final_output_pairs[, c(2, 3, ncol(final_output_pairs))]


# round and change to percentage
final_output_pairs$mean_inseg <- round(as.numeric(as.character(final_output_pairs$mean_inseg)),3) *100

final_output_pairs <- final_output_pairs %>% distinct(var_combinations, .keep_all = TRUE)


##############################################
#create wordcloud for double
##############################################

library(wordcloud)
library(tm)
library(slam)
library(tidytext)

# keep only the first 150 rows
df_subset_pair <- head(final_output_pairs, n = 100)

# convert var_combinations to a tidy format
wordcloud(df_subset_pair$var_combinations, max.words = 10, random.order = FALSE, colors=brewer.pal(8, "Greys"), rot.per = 0.15)

# count the most frequent characteristics
df_long <- separate_rows(df_subset_pair, var_combinations, sep = " ")

# Count frequency of individual strings
df_freq <- df_long %>% dplyr::count(var_combinations, sort = TRUE)




####################################################################
# ANALYSIS FOR TRIPLE PAIRS
####################################################################
# Example code to calculate mean of "sa1_poor" for multiple separate disaggregations and merge the output data frames using a loop
library(dplyr)

# Define a vector of variables to disaggregate by
var_list <- colnames(profile_triple)[17:ncol(profile_triple)]


# Loop over the variables to disaggregate by
# Initialize an empty list to store the results
output_list <- list()

# loop through each combination of categorical variables
for (i in 1:length(var_list)) {
  
  # calculate the mean of "sa1_poor" for the current combination of categorical variables
  output <- profile_triple %>%
    group_by_at(vars(var_list[[i]])) %>%
    dplyr::summarize(mean_inseg = mean(inseg_alimentaria_bin), pr1_comb_count = n())
  
  # add the output dataframe to the output list
  output_list[[i]] <- output
}

# combine all output dataframes into a single dataframe
final_output_triple <- do.call(bind_rows, output_list)

# remove all rows where the n is lower than 60
final_output_triple <- final_output_triple %>% subset(pr1_comb_count >= 40) %>%
  arrange(desc(mean_inseg))

#replace all NA cells
final_output_triple <- as.data.frame(lapply(final_output_triple, function(x) {
  x[is.na(x)] <- "gaggi"
  x
}))


# create a new column which contains the combinations
final_output_triple$var_combinations <- apply(final_output_triple[, 4:ncol(final_output_triple)], 1, function(x) {
  paste0(paste0(x[x != "gaggi"], collapse = ""))
})

#delete all non-used columns
final_output_triple <- final_output_triple[, c(2, 3, ncol(final_output_triple))]


# round and change to percentage
final_output_triple$mean_inseg <- round(as.numeric(as.character(final_output_triple$mean_inseg)),3) *100
final_output_triple <- final_output_triple %>% distinct(var_combinations, .keep_all = TRUE)


##############################################
#create wordcloud for triple
##############################################

library(wordcloud)
library(tm)
library(slam)
library(tidytext)

# keep only the first 150 rows
df_subset_triple <- head(final_output_triple, n = 200)

# convert var_combinations to a tidy format
wordcloud(df_subset_triple$var_combinations, max.words = 600, random.order = FALSE, colors=brewer.pal(8, "Greys"), rot.per = 0.05)

# count the most frequent characteristics
df_long <- separate_rows(df_subset_triple, var_combinations, sep = " ")

# Count frequency of individual strings
df_freq <- df_long %>% dplyr::count(var_combinations, sort = TRUE)



######################################################################################################################################################################################
######################################################################################################################################################################################
# group comparison analysis
######################################################################################################################################################################################
######################################################################################################################################################################################
r <- antioquia

r <- r %>% 
  mutate(tipo_vivienda = dplyr::recode(tipo_vivienda, 
                                       "apartamento_apartaestudio" = "apartamento", 
                                       "otro_" = "otro_tipo_vivienda",
                                       "habitacion_cuarto_pieza_en_otro_tipo_de_estructura__parqueaderos__depositos__bodegas__iglesias__colegios__fabricas__cuarto_para_portero_o_celador_en_un_edificio_de_apartamentos_" = "habitacion_apto", 
                                       "habitacion_cuarto_pieza_en_un_inquilinato" = "habitacion_inquilinato", 
                                       "ns_nr_strong__e__no_leer__strong_" = "ns_nr", 
                                       "situacion_de_calle_con_espacio_para_alojarse__carpa__vagon__embarcacion__cueva__refugio_natural__etc__" = "situacion_calle", 
                                       "vivienda_improvisada__construcciones_informales_con_materiales_menos_durables__cambuches__etc__" = "vivienda_improvisada"
  ))

r <- r %>% 
  mutate(tipo_vivienda = dplyr::recode(ind1_pertenencia_etnica, 
                                       "ninguno" = "ningun_etnia", 
                                       "otra_" = "otra_etnia"
  ))

# Create a sample data frame
subset_r <- r %>% select("inseg_alimentaria_bin", "sa8_ias", "estado_civil_jh", "sexo_jefatura", "nivel_estudios_grupo", "ind1_pertenencia_etnica", 
                             "urbano_rural", "discapacidad_jh", "enfermedad_cronica", "enfermedad_mental", "afectado_por_conflicto", "afectado_por_desastre_natural", 
                             "tipo_vivienda", "tipo_empleo_grupos", "fuente_ingresos", "razon_deuda", "material_paredes_exteriores", "material_pisos", "material_techo", 
                             "servicio_sanitario_compartido", "eliminacion_basura", "fuente_agua", "cambio_ingresos")

# Convert the categorical variables to factors
for (i in 3:length(subset_r)) {
  if (is.factor(subset_r[[i]]) == FALSE) {
    subset_r[[i]] <- factor(subset_r[[i]])
  }
}

# Calculate the grouped mean and count of "sa8_ias" for each categorical variable
means_list <- lapply(subset_r[, 3:length(subset_r)], function(x) {
  tapply(subset_r$inseg_alimentaria_bin, x, function(y) c(mean = mean(y), count = length(y)))
})

# Convert the list to a dataframe
means_df <- data.frame(variable = character(), group = character(),
                       mean_sa8_ias = double(), count = integer(),
                       stringsAsFactors = FALSE)
for (i in 1:length(means_list)) {
  for (j in 1:length(means_list[[i]])) {
    means_df[nrow(means_df) + 1,] <- c(names(means_list)[i],
                                       names(means_list[[i]])[j],
                                       means_list[[i]][[j]][1],
                                       means_list[[i]][[j]][2])
  }
}

# Filter out groups with count < 120
means_df$count <- as.numeric(as.character(means_df$count))
means_df_filtered <- means_df[means_df$count >= 60,]
means_df_filtered$mean_inseg_alimentaria_bin <- round(as.numeric(as.character(means_df_filtered$mean_sa8_ias)),3) *100







######################################################################################################################################################################################
######################################################################################################################################################################################
# logit analysis of household characteristics
######################################################################################################################################################################################
######################################################################################################################################################################################
conflicto <- df %>% filter(afectado_conflicto == "si")
df <- conflicto

Y <- df$cari


X <- df[, c("d17_i", "d17_ii", "d17_iii", "d17_iv", "d17_v", 
              "d1_1", "d1_2", "d1_3", "d1_4", "d1_5", 
              "d7_i", "d7_ii", "d7_iii", "d7_iv", "d7_v", "d7_vi", "d7_vii", "d7_viii", "d7_ix", 
              "d11_i", "d11_ii", "d11_iii", "d11_iv", "d11_v", "d11_vi", "d11_vii", 
              "d18_ii", 
              "d8",
              "d9", 
              "d10",
              "v1_i", "v1_ii", "v1_iii", "v1_iv", "v1_v", "v1_vi", "v1_vii", 
              "so20_i", "so20_ii", "so20_iii", "so20_iv", "so20_v", "so20_vi", "so20_vii", "so20_viii", 
              "so21_i", "so21_ii", "so21_iii", "so21_iv", "so21_v", "so21_vi", "so21_vii", "so21_viii", "so21_ix", "so21_x", "so21_xi", "so21_xii", "so21_xiii",
              "so8_i", "so8_ii", "so8_iii", "so8_iv", "so8_v", "so8_vi", "so8_vii", "so8_viii", "so8_ix", 
              "v13_i", "v13_ii", "v13_iii", "v13_iv", "v13_v", 
              "v12_i", "v12_ii", "v12_iii", "v12_iv", 
              "v20_i", "v20_ii", "v20_iii", "v20_iv", "v20_v", 
              "v16", 
              "v7_i", "v7_ii", "v7_iii", "v7_iv", "v7_v", "v7_vi", "v7_vii", 
              "so9_i", "so9_ii", "so9_iii", "so9_iv", 
              "afc_2012", "afc_2013", "afc_2014", "afc_2015", "afc_2016", "afc_2017", "afc_2018", "afc_2019", "afc_2020", "afc_2021", "afc_2022",
              "afc_i", "afc_ii", "afc_iii", "afc_iv", "afc_v", "afc_vi", "afc_vii", "afc_viii", "afc_ix", "afc_x", 
              "d16_i", "d16_ii", "d16_iii", "d16_iv", "d16_v", "d16_vi", 
              "v17_i", "v17_ii", "v17_iii", "v17_iv", "v17_v", "v17_vi", "v17_vii", 
              "p4_i", "p4_ii", "p4_iv", "p4_v", 
              "v5_iv", "v5_v", "v5_vi", 
              "ah2", 
              "ah4", 
              "v2_i", "v2_ii", "v2_iii", "v2_iv", "v2_v", 
              "d4", "d5", "d6", 
              "d14", 
              "d18_i", "d18_ii", 
              "v19_i", "v19_ii", "v19_iii", "v19_iv", "v19_v", "v19_vi", 
              "d14")]


X_1 <- df[, c("d17_i", "d17_ii", "d17_iii", "d17_iv", "d17_v")]

X_2 <- df[, c("d1_1", "d1_2", "d1_3", "d1_4", "d1_5")]

X_3 <- df[, c("d7_i", "d7_ii", "d7_iii", "d7_iv", "d7_v", "d7_vi", "d7_vii", "d7_viii", "d7_ix")]

X_4 <- df[, c("d11_i", "d11_ii", "d11_iii", "d11_iv", "d11_v", "d11_vi", "d11_vii")]

X_5 <- df[, c("d18_i", "d18_ii")]

X_6 <- df[, c("d8")]

X_7 <- df[, c("d9")]

X_8 <- df[, c("d10")]

X_9 <- df[, c("v1_i", "v1_ii", "v1_iii", "v1_iv", "v1_v", "v1_vi", "v1_vii")]

X_10 <- df[, c("so20_i", "so20_ii", "so20_iii", "so20_iv", "so20_v", "so20_vi", "so20_vii", "so20_viii")]

X_11 <- df[, c("so21_i", "so21_ii", "so21_iii", "so21_iv", "so21_v", "so21_vi", "so21_vii", "so21_viii", "so21_ix", "so21_x", "so21_xi", "so21_xii", "so21_xiii")]

X_12 <- df[, c("so8_i", "so8_ii", "so8_iii", "so8_iv", "so8_v", "so8_vi", "so8_vii", "so8_viii", "so8_ix")]

X_13 <- df[, c("v13_i", "v13_ii", "v13_iii", "v13_iv", "v13_v")]

X_14 <- df[, c("v12_i", "v12_ii", "v12_iii", "v12_iv")]

X_15 <- df[, c("v20_i", "v20_ii", "v20_iii", "v20_iv", "v20_v")]

X_16 <- df[, c("v16")]

X_17 <- df[, c("v7_i", "v7_ii", "v7_iii", "v7_iv", "v7_v", "v7_vi", "v7_vii")]

X_18 <- df[, c("so9_i", "so9_ii", "so9_iii", "so9_iv")]

X_19 <- df[, c("afc_2012", "afc_2013", "afc_2014", "afc_2015", "afc_2016", "afc_2017", "afc_2018", "afc_2019", "afc_2020", "afc_2021", "afc_2022")]

X_20 <- df[, c("afc_i", "afc_ii", "afc_iii", "afc_iv", "afc_v", "afc_vi", "afc_vii", "afc_viii", "afc_ix", "afc_x")]

X_21 <- df[, c("d16_i", "d16_ii", "d16_iii", "d16_iv", "d16_v", "d16_vi")]

X_22 <- df[, c("v17_i", "v17_ii", "v17_iii", "v17_iv", "v17_v", "v17_vi", "v17_vii")]

X_23 <- df[, c("p4_i", "p4_ii", "p4_iv", "p4_v")]

X_24 <- df[, c("v5_iv", "v5_v", "v5_vi")]


X <- as.matrix(X_24)

model <- glm(Y ~ X)
summary(model)

model <- glm(Y ~ X)



library(vcd)
cramers_v <- assocstats(table(df$transporte_carro_particular, df$cari))$cramer

# Print the result
print(paste0("Cramer's V: ", cramers_v))




######################################################################################################################################################################################
######################################################################################################################################################################################
# calculate vulnerability scores based on new tool
######################################################################################################################################################################################
######################################################################################################################################################################################
conflicto <- df %>% filter(afectado_conflicto == "si")
r <- conflicto

r <- r %>% dplyr::mutate(crowding_personas_dormir = case_when(
  r$personas_por_habitacion_dormir <= 1  ~ "_1",
  r$personas_por_habitacion_dormir > 1 & r$personas_por_habitacion_dormir <= 3  ~ "1_3",
  r$personas_por_habitacion_dormir > 3  ~ "3_"
))

r <- r %>% dplyr::mutate(vuln_1 = case_when(
  r$sexo_jh == "hombre" ~ -0.13 * 0.179,
  r$sexo_jh == "la_jefatura_es_compartida_entre_hombre_y_mujer" ~ -0.07 * 0.179,
  r$sexo_jh == "la_jefatura_es_compartida_entre_hombres" ~ 0.03 * 0.179,
  r$sexo_jh == "mujer" ~ 0.16 * 0.179, 
  r$sexo_jh == "la_jefatura_es_compartida_entre_mujeres" ~ 0.03 * 0.179))


r <- r %>% dplyr::mutate(vuln_2 = case_when(
  r$nivel_estudios_jh == "primaria_completa" ~ 0.01 * 0.193,
  r$nivel_estudios_jh == "primaria_incompleta" ~ 0.07 * 0.193,
  r$nivel_estudios_jh == "secundaria_completa" ~ 0.05 * 0.193,
  r$nivel_estudios_jh == "secundaria_incompleta" ~ 0.13 * 0.193, 
  r$nivel_estudios_jh == "sin_educacion" ~ 0.07 * 0.193,
  r$nivel_estudios_jh == "tecnico_tecnologico_completo" ~ -0.18 * 0.193,
  r$nivel_estudios_jh == "tecnico_tecnologico_incompleto" ~ -0.01 * 0.193,
  r$nivel_estudios_jh == "universitario_completo_o_postgrado" ~ -0.21 * 0.193,
  r$nivel_estudios_jh == "universitario_incompleto" ~ -0.07 * 0.193
  ))


r <- r %>% dplyr::mutate(vuln_3 = case_when(
  r$ind1_pertenencia_etnica == "afrodescendiente__negro__mulato_" ~ 0.07 * 0.165,
  r$ind1_pertenencia_etnica == "indigena" ~ 0.06 * 0.165,
  r$ind1_pertenencia_etnica == "mestizo" ~ -0.07 * 0.165,
  r$ind1_pertenencia_etnica == "ninguno" ~ 0.00 * 0.165, 
  r$ind1_pertenencia_etnica == "raizal" ~ -0.1 * 0.165,
  r$ind1_pertenencia_etnica == "ns_nr" ~ 0 * 0.165,
  r$ind1_pertenencia_etnica == "otra" ~ 0 * 0.165
  
))


r <- r %>% dplyr::mutate(vuln_4 = case_when(
  r$discapacidad_jh == "con_discapacidad" ~ 0.1 * 0.13,
  r$discapacidad_jh == "sin_discapacidad" ~ -0.1 * 0.13
))


r <- r %>% dplyr::mutate(vuln_5 = case_when(
  r$enfermedad_mental == "con_enfermedad_mental" ~ 0.1 * 0.184,
  r$enfermedad_mental == "sin_enfermedad_mental" ~ -0.1 * 0.184
))


r <- r %>% dplyr::mutate(vuln_6 = case_when(
  r$crowding_personas_dormir == "_1" ~ -0.18 * 0.256,
  r$crowding_personas_dormir == "1_3" ~ 0.03 * 0.256,
  r$crowding_personas_dormir == "3_" ~ 0.26 * 0.256
))



r <- r %>% dplyr::mutate(vuln_7 = case_when(
  r$dependency_ratio == "0" ~ -0.18 * 0.179,
  r$dependency_ratio == "0-0.5" ~ 0.1 * 0.179,
  r$dependency_ratio == ">1" ~ 0.13 * 0.179,
  r$dependency_ratio == "all_dependent" ~ -0.07 * 0.179
))


r <- r %>% dplyr::mutate(vuln_8 = case_when(
  r$presencia_ninos_menos5 == "1" ~ 0.12 * 0.161,
  r$presencia_ninos_menos5 == "0" ~ -0.12
))


r <- r %>% dplyr::mutate(vuln_9 = case_when(
  r$monoparental == "si" ~ 0.13 * 0.206,
  r$monoparental == "no" ~ -0.13 * 0.206
))


r <- r %>% dplyr::mutate(vuln_10 = case_when(
  r$miembro_mayor_recursos_empleo == "empleado_a__domestico_a_" ~ 0.1 * 0.175,
  r$miembro_mayor_recursos_empleo == "jornalero_o_peon" ~ 0.02 * 0.17,
  r$miembro_mayor_recursos_empleo == "obrero_a__o_empleado_a__de_empresa_particular" ~ -0.14 * 0.17,
  r$miembro_mayor_recursos_empleo == "obrero_a__o_empleado_a__del_gobierno" ~ -0.13 * 0.17,
  r$miembro_mayor_recursos_empleo == "otro_" ~ 0 * 0.17,
  r$miembro_mayor_recursos_empleo == "patron_o_empleador" ~ -0.1 * 0.17,
  r$miembro_mayor_recursos_empleo == "trabajador_familiar_sin_remuneracion" ~ +0.12 * 0.17,
  r$miembro_mayor_recursos_empleo == "trabajador_por_cuenta_propia" ~ 0.09 * 0.17,
  r$miembro_mayor_recursos_empleo == "trabajador_sin_remuneracion_en_empresas_o_negocios_de_otros_hogares" ~ -0.03 * 0.17
))


r <- r %>% dplyr::mutate(vuln_11 = case_when(
  r$tipo_vivienda == "apartamento_apartaestudio" ~ -0.11 * 0.164,
  r$tipo_vivienda == "casa" ~ -0.01 * 0.164,
  r$tipo_vivienda == "habitacion_cuarto_pieza_en_otro_tipo_de_estructura__parqueaderos__depositos__bodegas__iglesias__colegios__fabricas__cuarto_para_portero_o_celador_en_un_edificio_de_apartamentos_" ~ 0.01 * 0.164,
  r$tipo_vivienda == "habitacion_cuarto_pieza_en_un_inquilinato" ~ 0.08 * 0.164,
  r$tipo_vivienda == "otro_" ~ 0.08 * 0.164,
  r$tipo_vivienda == "situacion_de_calle_con_espacio_para_alojarse__carpa__vagon__embarcacion__cueva__refugio_natural__etc__" ~ 0.07 * 0.164,
  r$tipo_vivienda == "vivienda_improvisada__construcciones_informales_con_materiales_menos_durables__cambuches__etc__" ~ 0.08 * 0.164
))


r <- r %>% dplyr::mutate(vuln_12 = case_when(
  r$material_paredes_exteriores == "adobe_o_tapia_pisada" ~ -0.04 * 0.148,
  r$material_paredes_exteriores == "bahareque" ~ 0.00 * 0.148,
  r$material_paredes_exteriores == "cana__esterilla__otro_tipo_de_material_vegetal" ~ -0.14 * 0.148,
  r$material_paredes_exteriores == "ladrillo__bloque__material_prefabricado__piedra" ~ -0.14 * 0.148,
  r$material_paredes_exteriores == "madera_burda__tabla__tablon" ~ 0.13 * 0.148,
  r$material_paredes_exteriores == "madera_pulida" ~ 0.02 * 0.148,
  r$material_paredes_exteriores == "zinc__tela__carton__latas__desechos__plastico" ~ 0 * 0.148,
  r$material_paredes_exteriores == "_" ~ 0 * 0.148,
  r$material_paredes_exteriores == "ns_nr_strong__e__no_leer__strong_" ~ -0 * 0.148,
  r$material_paredes_exteriores == "otro" ~ 0  * 0.148,
  
  
))


r <- r %>% dplyr::mutate(vuln_13 = case_when(
  r$material_pisos == "baldosin__ladrillo__vinisol__otros_materiales_sinteticos" ~ -0.19 * 0.171,
  r$material_pisos == "cemento__gravilla" ~ 0.08 * 0.171,
  r$material_pisos == "madera_burda__tabla__tablon__otro_vegetal" ~ 0.04 * 0.171,
  r$material_pisos == "madera_pulida" ~ 0.04 * 0.171,
  r$material_pisos == "marmol" ~ 0 * 0.171,
  r$material_pisos == "otro_" ~ 0 * 0.171,
  r$material_pisos == "tierra__arena" ~ 0.10 * 0.171
))


r <- r %>% dplyr::mutate(vuln_14 = case_when(
  r$material_techo == "cemento_concreto" ~ -0.16 * 0.145,
  r$material_techo == "ladrillos" ~ -0.0 * 0.145,
  r$material_techo == "madera_burda__tabla__tablon__otro_vegetal" ~ -0.05 * 0.145,
  r$material_techo == "paja_bambu_techo_de_paja" ~ 0.04 * 0.145,
  r$material_techo == "plastico" ~ 0.1 * 0.145,
  r$material_techo == "otro_" ~ 0 * 0.145,
  r$material_techo == "tejas__barro__zinc__eternit_" ~ 0.1 * 0.145
))


r <- r %>% dplyr::mutate(vuln_15 = case_when(
  r$fuente_agua == "agua_embotellada_o_en_bolsa" ~ -0.09 * 0.139,
  r$fuente_agua == "aguas_lluvias" ~ 0.14 * 0.139,
  r$fuente_agua == "aguatero" ~ 0.03 * 0.139,
  r$fuente_agua == "carrotanque" ~ -0.04 * 0.139,
  r$fuente_agua == "de_acueducto_por_tuberia" ~ -0.15 * 0.139,
  r$fuente_agua == "otro" ~ 0 * 0.139,
  r$fuente_agua == "de_otra_fuente_por_tuberia" ~ -0.01 * 0.139,
  r$fuente_agua == "rio__quebrada__nacimiento_o_manantial" ~ 0.02 * 0.139,
  r$fuente_agua == "_" ~ -0 * 0.139,
  r$fuente_agua == "de_pila_publica" ~ -0 * 0.139,
  r$fuente_agua == "de_pozo_con_bomba" ~ 0 * 0.139,
  r$fuente_agua == "de_pozo_sin_bomba__aljibe__jaguey_o_barreno" ~ -0 * 0.139
))


r <- r %>% dplyr::mutate(vuln_16 = case_when(
  r$servicios_acueducto == "si" ~ -0.18 * 0.175,
  r$servicios_acueducto == "no" ~ 0.18 * 0.175))

r <- r %>% dplyr::mutate(vuln_17 = case_when(
  r$servicios_gas == "si" ~ -0.14 * 0.172,
  r$servicios_gas == "no" ~ 0.14 * 0.172))

r <- r %>% dplyr::mutate(vuln_18 = case_when(
  r$bienes_nevera == "si" ~ -0.18 * 0.177,
  r$bienes_nevera == "no" ~ 0.18 * 0.177))

r <- r %>% dplyr::mutate(vuln_19 = case_when(
  r$transporte_motocicleta == "si" ~ -0.23 * 0.201,
  r$transporte_motocicleta == "no" ~ 0.23 * 0.201))



r <- r %>% dplyr::mutate(vuln_20 = case_when(
  r$ano_afectado_conflicto == "2012" ~ -0.04 * 0.146,
  r$ano_afectado_conflicto == "2013" ~ 0.07 * 0.146,
  r$ano_afectado_conflicto == "2014" ~ -0.03 * 0.146,
  r$ano_afectado_conflicto == "2015" ~ 0.01 * 0.146,
  r$ano_afectado_conflicto == "2016" ~ -0.03 * 0.146,
  r$ano_afectado_conflicto == "2017" ~ 0.04 * 0.146,
  r$ano_afectado_conflicto == "2018" ~ 0.07 * 0.146,
  r$ano_afectado_conflicto == "2019" ~ 0.11 * 0.146,
  r$ano_afectado_conflicto == "2020" ~ 0.06 * 0.146,
  r$ano_afectado_conflicto == "2021" ~ 0.00 * 0.146,
  r$ano_afectado_conflicto == "2022" ~ 0.05 * 0.146
))


r <- r %>% dplyr::mutate(vuln_21 = case_when(
  r$tipo_afectacion_conflicto == "confinamiento" ~ 0.08 * 0.164,
  r$tipo_afectacion_conflicto == "desparacion" ~ -0.01 * 0.164,
  r$tipo_afectacion_conflicto == "desplazamiento" ~ 0.13 * 0.164,
  r$tipo_afectacion_conflicto == "homicidio" ~ -0.02 * 0.164,
  r$tipo_afectacion_conflicto == "integridad_sexual" ~ 0.01 * 0.164,
  r$tipo_afectacion_conflicto == "lesiones_discapacidad" ~ 0.04 * 0.164,
  r$tipo_afectacion_conflicto == "masacre" ~ 0.02 * 0.164,
  r$tipo_afectacion_conflicto == "secuestro" ~ -0.04 * 0.164
))


r$vuln_final <- (r$vuln_1*0.179)+(r$vuln_2*0.193)+(r$vuln_3*0.165)+(r$vuln_4_i*0.13)+(r$vuln_4_ii*0.184)+(r$vuln_5*0.256)+(r$vuln_6*0.179)+(r$vuln_7*0.161)+(r$vuln_8*0.206)+(r$vuln_9*0.175)+(r$vuln_10*0.164)+(r$vuln_11*0.148)+
              (r$vuln_12*0.171)+(r$vuln_13*0.145)+(r$vuln_14*0.139)+(r$vuln_15_i*0.175)+(r$vuln_15_ii*0.172)+(r$vuln_16_i*0.177)+(r$vuln_16_ii*0.201)+(r$vuln_17*0.146)+(r$vuln_18*0.164)

r$vuln_final <- (r$vuln_1)+(r$vuln_2)+(r$vuln_3)+(r$vuln_6)+(r$vuln_7)+(r$vuln_8)+(r$vuln_9)+(r$vuln_10)+(r$vuln_11)+
  (r$vuln_12)+(r$vuln_13)+(r$vuln_14)+(r$vuln_17)+(r$vuln_18)

table(r$vuln_final)
hist(r$vuln_final)
summary(r$vuln_final)


r <- r %>% dplyr::mutate(categoria_vulnerabilidad = case_when(
  r$vuln_final < -0.04715 ~ "categoria_1",
  r$vuln_final > -0.04715 & r$vuln_final < 0.0304 ~ "categoria_2",
  r$vuln_final > 0.0304 & r$vuln_final < 0.132 ~ "categoria_3",
  r$vuln_final > 0.132 ~ "categoria_4"
))
table(r$categoria_vulnerabilidad, r$cari_category)




######################################################################################################################################################################################
######################################################################################################################################################################################
# create different combinations of 10 targeting indicators
######################################################################################################################################################################################
######################################################################################################################################################################################
# Create a sample data frame
  profile_ten <- r %>% select("cari", "vuln_1", "vuln_2", "vuln_3", "vuln_4", 
                               "vuln_5", "vuln_6", "vuln_7", "vuln_8", "vuln_9", "vuln_10", 
                               "vuln_11", "vuln_12", "vuln_13", "vuln_14", "vuln_15", "vuln_17", "vuln_18", 
                               "vuln_19", "vuln_20", "vuln_21")

library(dplyr)

# Load your data
# Select the numerical columns
num_cols <- names(profile_ten)[2:21]

# Get all combinations of 10 numerical columns
combs <- combinat::combn(num_cols, 12, simplify = FALSE)

# Calculate row sums of all combinations and store results in a list
results <- lapply(combs, function(x) {
  rowSums(profile_ten[, x], na.rm=T)
})

# Combine results into a data frame with column names as variable names
results_df <- as.data.frame(do.call(cbind, results))
results_df <- results_df %>% set_names(sapply(combs, paste, collapse = "_"))

results_df <- profile_ten %>% select("cari") %>% 
                              cbind(results_df)


library(DescTools)

# Calculate Cramer's V for each column
correlations <- apply(results_df[, 2:ncol(results_df)], 2, function(x) cor.test(results_df[, 1], x)$estimate)
cor_df <- data.frame(column_name = names(results_df)[2:ncol(results_df)], correlation = correlations)



#########################################################################
############################
#test best
r$vuln_final_good <- (r$vuln_19)+(r$vuln_2)+(r$vuln_3)+(r$vuln_4)+(r$vuln_5)+(r$vuln_6)+(r$vuln_9)+(r$vuln_10)+(r$vuln_11)+(r$vuln_15)+(r$vuln_21)

table(r$vuln_final_good)
hist(r$vuln_final_good)
summary(r$vuln_final_good)


r <- r %>% dplyr::mutate(categoria_vulnerabilidad_good = case_when(
  r$vuln_final_good < -0.059 ~ "categoria_1",
  r$vuln_final_good > -0.059 & r$vuln_final_good < -0.0172 ~ "categoria_2",
  r$vuln_final_good > -0.0172 & r$vuln_final_good < 0.07 ~ "categoria_3",
  r$vuln_final_good > 0.07 ~ "categoria_4"
))
table(r$categoria_vulnerabilidad_good, r$cari_category)
cor.test(r$vuln_final_good, r$cari)


############################
#test worst
r$vuln_final_bad <- (r$vuln_1)+(r$vuln_3)+(r$vuln_4)+(r$vuln_5)+(r$vuln_8)+(r$vuln_12)+(r$vuln_14)+(r$vuln_17)+(r$vuln_18)+
  (r$vuln_19)+(r$vuln_20)+(r$vuln_21)

table(r$vuln_final_bad)
hist(r$vuln_final_bad)
summary(r$vuln_final_bad)


r <- r %>% dplyr::mutate(categoria_vulnerabilidad_bad = case_when(
  r$vuln_final_bad < 0.139 ~ "categoria_1",
  r$vuln_final_bad > 0.139 & r$vuln_final_bad < 0.201 ~ "categoria_2",
  r$vuln_final_bad > 0.201 & r$vuln_final_bad < 0.266 ~ "categoria_3",
  r$vuln_final_bad > 0.266 ~ "categoria_4"
))
table(r$categoria_vulnerabilidad_bad, r$cari_category)
table(r$categoria_vulnerabilidad_good, r$cari_category)



##################
#create wordcloud
# keep only the first 150 rows
cor_df <- cor_df[order(cor_df$correlation, decreasing = TRUE), ]
df_subset_pair <- head(cor_df, n = 200)

# convert var_combinations to a tidy format
#wordcloud(df_subset_pair$var_combinations, max.words = 10, random.order = FALSE, colors=brewer.pal(8, "Greys"), rot.per = 0.15)

# count the most frequent characteristics
df_long <- separate_rows(df_subset_pair, column_name, sep = "_")

# Count frequency of individual strings
df_freq <- df_long %>% dplyr::count(column_name, sort = TRUE)



##############################################
#visualization
library(ggplot2)

# Create the scatter plot
ggplot(r, aes(x = vuln_final_bad, y = cari)) +
  geom_point(color = "steelblue", size = 3) +  # Set point color and size
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Add the regression line with error margin
  labs(x = "Puntaje Vulnerabilidad", y = "CARI") +
  theme_minimal(base_family = "Arial") +  # Set theme to minimal with Arial font
  theme(plot.title = element_text(size = 16, face = "bold"),  # Customize title
        axis.title = element_text(size = 12, face = "bold"),  # Customize axis labels
        axis.text = element_text(size = 10),  # Customize axis tick labels
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10)) 


# Create the scatter plot
ggplot(r, aes(x = vuln_final_bad, y = cari)) +
  geom_point() +
  labs(x = "categoria_vulnerabilidad_bad", y = "cari") +
  geom_smooth(method = "lm", color = "blue") +  # Add the regression line
  ggtitle("Scatter Plot of cari vs categoria_vulnerabilidad_bad")



############################
#proposed model
r$vuln_final_proposed <- (r$vuln_1)+(r$vuln_2)+(r$vuln_3)+(r$vuln_4)+(r$vuln_5)+(r$vuln_6)+(r$vuln_7)+(r$vuln_12)+(r$vuln_15)+(r$vuln_20)

table(r$vuln_final_proposed)
hist(r$vuln_final_proposed)
summary(r$vuln_final_proposed)


r <- r %>% dplyr::mutate(categoria_vulnerabilidad_proposed = case_when(
  r$vuln_final_proposed < -0.169 ~ "categoria_1",
  r$vuln_final_proposed > -0.169 & r$vuln_final_proposed < -0.0924 ~ "categoria_2",
  r$vuln_final_proposed > -0.092 & r$vuln_final_proposed < -0.031 ~ "categoria_3",
  r$vuln_final_proposed > -0.031 ~ "categoria_4"
))
table(r$categoria_vulnerabilidad_proposed, r$cari_category)
table(r$categoria_vulnerabilidad_good, r$cari_category)
















########################################################################
# INDIVIDUAL CORRELATION ANALYSIS
#######################################################################
# List of variables for correlation tests (excluding "cari")
variables <- c("d17_i", "d17_ii", "d17_iii", "d17_iv", "d17_v", 
               "d1_1", "d1_2", "d1_3", "d1_4", "d1_5", 
               "d7_i", "d7_ii", "d7_iii", "d7_iv", "d7_v", "d7_vi", "d7_vii", "d7_viii", "d7_ix", 
               "d11_i", "d11_ii", "d11_iii", "d11_iv", "d11_v", "d11_vi", "d11_vii", 
               "d18_ii", 
               "d8",
               "d9", 
               "d10",
               "v1_i", "v1_ii", "v1_iii", "v1_iv", "v1_v", "v1_vi", "v1_vii", 
               "so20_i", "so20_ii", "so20_iii", "so20_iv", "so20_v", "so20_vi", "so20_vii", "so20_viii", 
               "so21_i", "so21_ii", "so21_iii", "so21_iv", "so21_v", "so21_vi", "so21_vii", "so21_viii", "so21_ix", "so21_x", "so21_xi", "so21_xii", "so21_xiii",
               "so8_i", "so8_ii", "so8_iii", "so8_iv", "so8_v", "so8_vi", "so8_vii", "so8_viii", "so8_ix", 
               "v13_i", "v13_ii", "v13_iii", "v13_iv", "v13_v", 
               "v12_i", "v12_ii", "v12_iii", "v12_iv", 
               "v20_i", "v20_ii", "v20_iii", "v20_iv", "v20_v", 
               "v16", 
               "v7_i", "v7_ii", "v7_iii", "v7_iv", "v7_v", "v7_vi", "v7_vii", 
               "so9_i", "so9_ii", "so9_iii", "so9_iv", 
               "afc_2012", "afc_2013", "afc_2014", "afc_2015", "afc_2016", "afc_2017", "afc_2018", "afc_2019", "afc_2020", "afc_2021", "afc_2022",
               "afc_i", "afc_ii", "afc_iii", "afc_iv", "afc_v", "afc_vi", "afc_vii", "afc_viii", "afc_ix", "afc_x", 
               "d16_i", "d16_ii", "d16_iii", "d16_iv", "d16_v", "d16_vi", 
               "v17_i", "v17_ii", "v17_iii", "v17_iv", "v17_v", "v17_vi", "v17_vii", 
               "p4_i", "p4_ii", "p4_iv", "p4_v", 
               "v5_iv", "v5_v", "v5_vi", 
               "ah2", 
               "ah4", 
               "v2_i", "v2_ii", "v2_iii", "v2_iv", "v2_v", 
               "d4", "d5", "d6", 
               "d14", 
               "d18_i", "d18_ii", 
               "v19_i", "v19_ii", "v19_iii", "v19_iv", "v19_v", "v19_vi", 
               "d14")

# Create an empty dataframe to store the correlation results
correlation_results <- data.frame(Variable = character(), Coefficient = numeric(), P_Value = numeric(), stringsAsFactors = FALSE)

# Perform correlation tests
for (i in 1:length(variables)) {
  correlation_result <- cor.test(df$cari, df[[variables[i]]])
  
  # Store the results in the correlation_results dataframe
  correlation_results <- rbind(correlation_results, data.frame(
    Variable = variables[i],
    Coefficient = correlation_result$estimate,
    P_Value = correlation_result$p.value
  ))
}

# View the correlation results dataframe
print(correlation_results)
