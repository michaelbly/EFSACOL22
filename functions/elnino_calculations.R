#r <- df

########################################################################################################################################################
# AGRICULTURE
########################################################################################################################################################
r$agricultura <- ifelse(r$miembro_mayor_recursos_actividad_empresa == "agricultura__ganaderia__caza__silvicultura_y_pesca_", "agricultura", "non_agricultura")


r <- r %>% dplyr::mutate(classification_department_elnino = case_when(
  r$departamento == "amazonas" ~ "bajo",
  r$departamento == "antioquia" ~ "bajo",
  r$departamento == "arauca" ~ "moderada",
  r$departamento == "archipielago_de_san_andres" ~ "moderada",
  r$departamento == "atlantico" ~ "severa",
  r$departamento == "bogota_dc" ~ "bajo",
  r$departamento == "bolivar" ~ "severa",
  r$departamento == "boyaca" ~ "bajo",
  r$departamento == "caldas" ~ "bajo",
  r$departamento == "caqueta" ~ "bajo",
  r$departamento == "casanare" ~ "bajo",
  r$departamento == "cauca" ~ "moderada",
  r$departamento == "cesar" ~ "severa",
  r$departamento == "choco" ~ "bajo",
  r$departamento == "cordoba" ~ "bajo",
  r$departamento == "cundinamarca" ~ "bajo",
  r$departamento == "huila" ~ "bajo",
  r$departamento == "la_guajira" ~ "severa",
  r$departamento == "magdalena" ~ "severa",
  r$departamento == "meta" ~ "bajo",
  r$departamento == "narino" ~ "severa",
  r$departamento == "norte_de_santander" ~ "moderada",
  r$departamento == "putumayo" ~ "bajo",
  r$departamento == "quindio" ~ "bajo",
  r$departamento == "risaralda" ~ "bajo",
  r$departamento == "santander" ~ "bajo",
  r$departamento == "sucre" ~ "severa",
  r$departamento == "tolima" ~ "severa",
  r$departamento == "valle_del_cauca" ~ "moderada",
  r$departamento == "vaupes" ~ "bajo",
  r$departamento == "vichada" ~ "bajo"
))


r <- r %>% dplyr::mutate(newly_insecure_elnino_agricultura = case_when(
  r$classification_department_elnino == "moderada" & r$agricultura == "agricultura" & r$cari_category == "2_Seguridad Marginal" ~ 1,
  r$classification_department_elnino == "severa" & r$agricultura == "agricultura" & r$cari_category == "2_Seguridad Marginal" ~ 1,
  r$classification_department_elnino == "severa" & r$agricultura == "agricultura" & r$cari_category == "1_Seguridad" ~ 1,
  TRUE ~ 0
))




##############################################################################################################################################################
# NON-AGRICULTURE EL NIÑO FUERTE
##############################################################################################################################################################
####################################
# FCS Adjustment
####################################
r$fcs_elnino_fuerte <- r$fcs - 2.14

r$sa1_poor_elnino_fuerte <- ifelse(r$fcs_elnino_fuerte <= 28, 1,0)
r$sa1_borderline_elnino_fuerte <- ifelse(r$fcs_elnino_fuerte > 28 & r$fcs_elnino_fuerte <=42,1,0)
r$sa1_acceptable_elnino_fuerte <- ifelse(r$fcs_elnino_fuerte > 42,1,0)



r <- r %>% dplyr::mutate(fcs_ajuste_cari_elnino_fuerte = case_when(
  r$fcs_elnino_fuerte > 42 & r$sa2_i == 1 ~ 1,
  r$fcs_elnino_fuerte > 42 & (r$sa2_ii == 1 | r$sa2_iii == 1) ~ 2,
  r$fcs_elnino_fuerte > 28 & r$fcs_elnino_fuerte <=42 ~ 3,
  r$fcs_elnino_fuerte <= 28 ~ 4))




####################################
# Economic Vulnerability
####################################
r$exp_pp_elnino_fuerte <- r$exp_pp - (r$exp_pp * 0.14)

r <- r %>% dplyr::mutate(ve_elnino_fuerte = case_when(
  r$exp_pp_elnino_fuerte > 396182 & r$urbano_rural == "urbano" ~ 1,
  r$exp_pp_elnino_fuerte > 228725 & r$urbano_rural == "rural" ~ 1,
  
  r$exp_pp_elnino_fuerte < 396182 & r$exp_pp_elnino_fuerte > 178906 & r$urbano_rural == "urbano" ~ 3,
  r$exp_pp_elnino_fuerte < 228725 & r$exp_pp_elnino_fuerte > 125291 & r$urbano_rural == "rural" ~ 3,
  
  r$exp_pp_elnino_fuerte < 178906 & r$urbano_rural == "urbano" ~ 4,
  r$exp_pp_elnino_fuerte < 125291 & r$urbano_rural == "rural" ~ 4))




####################################
# CARI Calculation
####################################
r$cari_elnino_fuerte <- as.numeric(r$fcs_ajuste_cari_elnino_fuerte * 0.5) + as.numeric(r$lcs_cari * 0.25) + as.numeric(r$ve_elnino_fuerte * 0.25)
r$sa8_sa_elnino_fuerte <- ifelse(r$cari_elnino_fuerte < 1.5,1,0)
r$sa8_sam_elnino_fuerte <- ifelse(r$cari_elnino_fuerte >= 1.5 & r$cari_elnino_fuerte < 2.5,1,0)
r$sa8_iam_elnino_fuerte <- ifelse(r$cari_elnino_fuerte >= 2.5 & r$cari_elnino_fuerte < 3.5,1,0)
r$sa8_ias_elnino_fuerte <- ifelse(r$cari_elnino_fuerte >= 3.5,1,0)


r <- r %>% dplyr::mutate(cari_category_elnino_fuerte = case_when(
  r$sa8_sa_elnino_fuerte == 1 & r$agricultura == "non_agricultura" ~ "1_Seguridad",
  r$sa8_sam_elnino_fuerte == 1 & r$agricultura == "non_agricultura" ~ "2_Seguridad Marginal",
  r$sa8_iam_elnino_fuerte == 1 & r$agricultura == "non_agricultura" ~ "3_Inseguridad Moderada",
  r$sa8_ias_elnino_fuerte == 1 & r$agricultura == "non_agricultura" ~ "4_Inseguridad Severa"
))


r <- r %>% dplyr::mutate(cari_insecurity_elnino_fuerte = case_when(
  r$sa8_sa_elnino_fuerte == 1 & r$agricultura == "non_agricultura" ~ "seguridad_alimentaria",
  r$sa8_sam_elnino_fuerte == 1 & r$agricultura == "non_agricultura" ~ "seguridad_alimentaria",
  r$sa8_iam_elnino_fuerte == 1 & r$agricultura == "non_agricultura" ~ "inseguridad_alimentaria",
  r$sa8_ias_elnino_fuerte == 1 & r$agricultura == "non_agricultura" ~ "inseguridad_alimentaria"
))

r$newly_insecure_elnino_fuerte <- ifelse(r$cari_insecurity_elnino_fuerte == "inseguridad_alimentaria" & r$cari_insecurity == "seguridad_alimentaria", 1,0)

table(r$agricultura, r$newly_insecure_elnino_fuerte)







########################################################################################################################################################
# NON-AGRICULTURE EL NIÑO MODERADO
########################################################################################################################################################
####################################
# FCS Adjustment
####################################
r$fcs_elnino_moderado <- r$fcs - 0.59

r$sa1_poor_elnino_moderado <- ifelse(r$fcs_elnino_moderado <= 28, 1,0)
r$sa1_borderline_elnino_moderado <- ifelse(r$fcs_elnino_moderado > 28 & r$fcs_elnino_moderado <=42,1,0)
r$sa1_acceptable_elnino_moderado <- ifelse(r$fcs_elnino_moderado > 42,1,0)



r <- r %>% dplyr::mutate(fcs_ajuste_cari_elnino_moderado = case_when(
  r$fcs_elnino_moderado > 42 & r$sa2_i == 1 ~ 1,
  r$fcs_elnino_moderado > 42 & (r$sa2_ii == 1 | r$sa2_iii == 1) ~ 2,
  r$fcs_elnino_moderado > 28 & r$fcs_elnino_moderado <=42 ~ 3,
  r$fcs_elnino_moderado <= 28 ~ 4))



####################################
# Economic Vulnerability
####################################
r$exp_pp_elnino_moderado <- r$exp_pp - (r$exp_pp * 0.0512)

r <- r %>% dplyr::mutate(ve_elnino_moderado = case_when(
  r$exp_pp_elnino_moderado > 396182 & r$urbano_rural == "urbano" ~ 1,
  r$exp_pp_elnino_moderado > 228725 & r$urbano_rural == "rural" ~ 1,
  
  r$exp_pp_elnino_moderado < 396182 & r$exp_pp_elnino_moderado > 178906 & r$urbano_rural == "urbano" ~ 3,
  r$exp_pp_elnino_moderado < 228725 & r$exp_pp_elnino_moderado > 125291 & r$urbano_rural == "rural" ~ 3,
  
  r$exp_pp_elnino_moderado < 178906 & r$urbano_rural == "urbano" ~ 4,
  r$exp_pp_elnino_moderado < 125291 & r$urbano_rural == "rural" ~ 4))



####################################
# CARI Calculation
####################################
r$cari_elnino_moderado <- as.numeric(r$fcs_ajuste_cari_elnino_moderado * 0.5) + as.numeric(r$lcs_cari * 0.25) + as.numeric(r$ve_elnino_moderado * 0.25)
r$sa8_sa_elnino_moderado <- ifelse(r$cari_elnino_moderado < 1.5,1,0)
r$sa8_sam_elnino_moderado <- ifelse(r$cari_elnino_moderado >= 1.5 & r$cari_elnino_moderado < 2.5,1,0)
r$sa8_iam_elnino_moderado <- ifelse(r$cari_elnino_moderado >= 2.5 & r$cari_elnino_moderado < 3.5,1,0)
r$sa8_ias_elnino_moderado <- ifelse(r$cari_elnino_moderado >= 3.5,1,0)


r <- r %>% dplyr::mutate(cari_category_elnino_moderado = case_when(
  r$sa8_sa_elnino_moderado == 1 & r$agricultura == "non_agricultura" ~ "1_Seguridad",
  r$sa8_sam_elnino_moderado == 1 & r$agricultura == "non_agricultura" ~ "2_Seguridad Marginal",
  r$sa8_iam_elnino_moderado == 1 & r$agricultura == "non_agricultura" ~ "3_Inseguridad Moderada",
  r$sa8_ias_elnino_moderado == 1 & r$agricultura == "non_agricultura" ~ "4_Inseguridad Severa"
))


r <- r %>% dplyr::mutate(cari_insecurity_elnino_moderado = case_when(
  r$sa8_sa_elnino_moderado == 1 & r$agricultura == "non_agricultura" ~ "seguridad_alimentaria",
  r$sa8_sam_elnino_moderado == 1 & r$agricultura == "non_agricultura" ~ "seguridad_alimentaria",
  r$sa8_iam_elnino_moderado == 1 & r$agricultura == "non_agricultura" ~ "inseguridad_alimentaria",
  r$sa8_ias_elnino_moderado == 1 & r$agricultura == "non_agricultura" ~ "inseguridad_alimentaria"
))

r$newly_insecure_elnino_moderado <- ifelse(r$cari_insecurity_elnino_moderado == "inseguridad_alimentaria" & r$cari_insecurity == "seguridad_alimentaria", 1,0)

table(r$agricultura, r$newly_insecure_elnino_moderado)



####################################
# BRING TOGETHER AGRICULTURE AND NON-AGRICULTURE
####################################
r$final_newly_insecure_elnino_fuerte <- ifelse(r$newly_insecure_elnino_fuerte == 1 | r$newly_insecure_elnino_agricultura == 1,1,0)
r$final_newly_insecure_elnino_moderado <- ifelse(r$newly_insecure_elnino_moderado == 1 | r$newly_insecure_elnino_agricultura == 1,1,0)

table(r$final_newly_insecure_elnino_fuerte)
table(r$final_newly_insecure_elnino_moderado)





r <- r %>% dplyr::mutate(cari_elnino_todos = case_when(
  r$classification_department_elnino == "moderada" & r$agricultura == "agricultura" & r$cari_category == "2_Seguridad Marginal" ~ "3_Inseguridad Moderada",
  r$classification_department_elnino == "severa" & r$agricultura == "agricultura" & r$cari_category == "2_Seguridad Marginal" ~ "3_Inseguridad Moderada",
  r$classification_department_elnino == "severa" & r$agricultura == "agricultura" & r$cari_category == "1_Seguridad" ~ "3_Inseguridad Moderada",
  r$classification_department_elnino == "severa" & r$agricultura == "agricultura" & r$cari_category == "1_Seguridad" ~ "3_Inseguridad Moderada",
  r$classification_department_elnino == "severa" & r$agricultura == "agricultura" & r$cari_category == "1_Seguridad" ~ "3_Inseguridad Moderada",
  r$classification_department_elnino == "severa" & r$agricultura == "agricultura" & r$cari_category == "1_Seguridad" ~ "3_Inseguridad Moderada",
  r$classification_department_elnino == "severa" & r$agricultura == "agricultura" & r$cari_category == "1_Seguridad" ~ "3_Inseguridad Moderada",
  r$classification_department_elnino == "severa" & r$agricultura == "agricultura" & r$cari_category == "1_Seguridad" ~ "3_Inseguridad Moderada",
  
  TRUE ~ 0
))























########################################################################################################################################################
# NON-AGRICULTURE EL NIÑO MODERADO
########################################################################################################################################################
dap_name <- "elnino"
analysisplan <- read.csv(sprintf("data/dap/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")

r$departamento <- ifelse(r$departamento %in% c("vichada", 
                                                                                             "vaupes", "amazonas"), "atn", 
                                                r$departamento)

#response_with_composites <- response_with_composites %>% filter(nivel_estudios_jh != "_")



analysisplan$repeat.for.variable <- "one"
analysisplan$independent.variable <- "one"
analysisplan$independent.variable.type <- "categorical"
#analysisplan$hypothesis.type <- "group_difference"
r$one <- "one"


weight_fun_departamento<-function(r){
  r$weights_departamento
}

weight_fun_national<-function(r){
  r$weights_nacional
}


result <- from_analysisplan_map_to_output(r, analysisplan = analysisplan,
                                          weighting = weight_fun_national, cluster_variable_name = NULL,
                                          questionnaire = NULL, confidence_level = 0.95)


name <- "V2_impacto_elnino_cari_nacional_cfsvacol22"
saveRDS(result,paste(sprintf("output/RDS/result_%s.RDS", name)))

summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
gaggi <- bind_rows(lapply(result[[1]], function(x){x$hypothesis.test}))
#gaggi <- cbind(summary, gaggi)


write.csv(summary, sprintf("output/raw_results/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/raw_results/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
summary$max <- ifelse(summary$numbers < 1 & summary$max > 1, 1, 
                      summary$max)
summary$min <- ifelse(summary$min < 0, 0, summary$min)


#summary$max <- ifelse(summary$max > 1, 1, summary$max)
summary$max <- NULL
#summary$min <- ifelse(summary$min < 0, 0, summary$min)
summary$min <- NULL
#summary$numbers <- as.character(as.numeric(round(summary$numbers,1)))


write.csv(summary, sprintf("output/raw_results/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
library(plyr)


for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, file=sprintf("Output/summary_sorted/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("Output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE, showNA = F)
  } else {
    write.xlsx(df, file=sprintf("Output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE, showNA = F)
  }
}
