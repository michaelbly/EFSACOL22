loop_with_composites <- response_with_composites %>% 
  select(registro, weights_departamento, departamento, weights_nacional, sa8_sa, sa8_sam, sa8_iam, sa8_ias) %>%
  right_join(loop, by="registro") %>%
  mutate(sex_age_group = case_when(
    (sexo == "mujer" & edad < 2 & edad_anos_meses == "anos") | (sexo == "mujer" & edad_anos_meses == "meses") ~ "mujer_023",
    (sexo == "hombre" & edad < 2 & edad_anos_meses == "anos") | (sexo == "hombre" & edad_anos_meses == "meses") ~ "hombre_023",
    sexo == "mujer" & edad >= 2 & edad < 5 & edad_anos_meses == "anos" ~ "mujer_2459",
    sexo == "hombre" & edad >= 2 & edad < 5 & edad_anos_meses == "anos" ~ "hombre_2459",
    sexo == "mujer" & edad >= 5 & edad <= 11 & edad_anos_meses == "anos" ~ "mujer_511",
    sexo == "hombre" & edad >= 5 & edad <= 11 & edad_anos_meses == "anos" ~ "hombre_511",
    sexo == "mujer" & edad == 6 & edad_anos_meses == "anos" ~ "mujer_511",
    sexo == "hombre" & edad == 6 & edad_anos_meses == "anos" ~ "hombre_511",
    
    sexo == "mujer" & edad >= 12 & edad <= 17 & edad_anos_meses == "anos" ~ "mujer_1217",
    sexo == "hombre" & edad >= 12 & edad <= 17 & edad_anos_meses == "anos" ~ "hombre_1217",
    sexo == "mujer" & edad >= 18 & edad <= 29 & edad_anos_meses == "anos" ~ "mujer_1829",
    sexo == "hombre" & edad >= 18 & edad <= 29 & edad_anos_meses == "anos" ~ "hombre_1829",
    sexo == "mujer" & edad >= 30 & edad <= 59 & edad_anos_meses == "anos" ~ "mujer_3059",
    sexo == "hombre" & edad >= 30 & edad <= 59 & edad_anos_meses == "anos" ~ "hombre_3059",
    sexo == "mujer" & edad >= 60 & edad_anos_meses == "anos" ~ "mujer_60",
    sexo == "hombre" & edad >= 60 & edad_anos_meses == "anos" ~ "hombre_60"
  )) %>%
  drop_na(weights_nacional)



loop_with_composites <- response_with_composites %>% 
  select(registro, weights, departamento, cari_category) %>%
  right_join(loop, by="registro") %>%
  mutate(
    adulto_mujer = ifelse(sexo == "mujer" & edad >= 18 & edad_anos_meses == "anos",1,0),
    adulto_hombre = ifelse(sexo == "hombre" & edad >= 18 & edad_anos_meses == "anos",1,0),
    nino_mujer = ifelse((sexo == "mujer" & edad < 18 & edad_anos_meses == "anos") | 
                          (sexo == "mujer" & edad_anos_meses == "meses"),1,0),
    nino_hombre = ifelse((sexo == "hombre" & edad < 18 & edad_anos_meses == "anos") | 
                           (sexo == "hombre" & edad_anos_meses == "meses"),1,0)
  ) %>% 
  mutate(cari_categories_agg = case_when(
    cari_category %in% c("iam", "ias") ~ "inseguridad",
    cari_category %in% c("sa", "sam") ~ "seguridad",
    is.na(cari_category) ~ "not_available"
  )) %>%
  drop_na(cari_categories_agg) %>%
  drop_na(weights)


weight_fun_loop<-function(df){
  loop_with_composites$weights_nacional
}


#LOAD ANALYSISPLAN
dap_name <- "cari_solo"
analysisplan <- read.csv(sprintf("data/dap/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")
#analysisplan$independent.variable <-  "tiempo_en_pais"
analysisplan$repeat.for.variable <- "departamento"
loop_with_composites$one <- "one"
analysisplan$independent.variable <- "sex_age_group"




result <- from_analysisplan_map_to_output(loop_with_composites, analysisplan = analysisplan,
                                          weighting = NULL, cluster_variable_name = NULL,
                                          questionnaire = NULL, confidence_level = 0.95)


name <- "department_percentage_ind_by CARI_popgroupagg_nacional"
saveRDS(result,paste(sprintf("output/RDS/result_%s.RDS", name)))

summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
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


write.csv(summary, sprintf("Output/raw_results/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
library(plyr)


for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, sprintf("Output/summary_sorted/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("Output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE, showNA = F)
  } else {
    write.xlsx(df, file=sprintf("Output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE, showNA = F)
  }
}

