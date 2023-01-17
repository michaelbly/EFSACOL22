#Investigate Group Profiles 
loop$edad <- as.numeric(as.character(loop$edad))

loop <- loop %>% mutate(edad_sexo_grupo = case_when(
  ((loop$edad < 2 & loop$edad_anos_meses == "anos") | (loop$edad_anos_meses == "meses")) & loop$sexo == "hombre" ~ "hombre0_23_m",
  ((loop$edad < 2 & loop$edad_anos_meses == "anos") | (loop$edad_anos_meses == "meses")) & loop$sexo == "mujer" ~ "mujer0_23_m",
  
  ((loop$edad >= 2 & loop$edad_anos_meses == "anos") & (loop$edad < 5 & loop$edad_anos_meses == "anos")) & loop$sexo == "hombre" ~ "hombre24_59_m",
  ((loop$edad >= 2 & loop$edad_anos_meses == "anos") & (loop$edad < 5 & loop$edad_anos_meses == "anos")) & loop$sexo == "mujer" ~ "mujer24_59_m",
  
  ((loop$edad >= 6 & loop$edad_anos_meses == "anos") & (loop$edad < 12 & loop$edad_anos_meses == "anos")) & loop$sexo == "hombre" ~ "hombre6_11",
  ((loop$edad >= 6 & loop$edad_anos_meses == "anos") & (loop$edad < 12 & loop$edad_anos_meses == "anos")) & loop$sexo == "mujer" ~ "mujer6_11",
  
  ((loop$edad >= 12 & loop$edad_anos_meses == "anos") & (loop$edad < 18 & loop$edad_anos_meses == "anos")) & loop$sexo == "hombre" ~ "hombre12_17",
  ((loop$edad >= 12 & loop$edad_anos_meses == "anos") & (loop$edad < 18 & loop$edad_anos_meses == "anos")) & loop$sexo == "mujer" ~ "mujer12_17",
  
  ((loop$edad >= 18 & loop$edad_anos_meses == "anos") & (loop$edad < 30 & loop$edad_anos_meses == "anos")) & loop$sexo == "hombre" ~ "hombre18_29",
  ((loop$edad >= 18 & loop$edad_anos_meses == "anos") & (loop$edad < 30 & loop$edad_anos_meses == "anos")) & loop$sexo == "mujer" ~ "mujer18_29",
  
  ((loop$edad >= 30 & loop$edad_anos_meses == "anos") & (loop$edad < 59 & loop$edad_anos_meses == "anos")) & loop$sexo == "hombre" ~ "hombre30_59",
  ((loop$edad >= 30 & loop$edad_anos_meses == "anos") & (loop$edad < 59 & loop$edad_anos_meses == "anos")) & loop$sexo == "mujer" ~ "mujer30_59",
  
  (loop$edad >= 60 & loop$edad_anos_meses == "anos") & loop$sexo == "hombre" ~ "hombre60",
  (loop$edad >= 60 & loop$edad_anos_meses == "anos") & loop$sexo == "mujer" ~ "mujer60"
)
)


loop$hombre0_23_m <- ifelse(loop$edad_sexo_grupo == "hombre0_23_m",1,0)
loop$mujer0_23_m <- ifelse(loop$edad_sexo_grupo == "mujer0_23_m",1,0)

loop$hombre24_59_m <- ifelse(loop$edad_sexo_grupo == "hombre24_59_m",1,0)
loop$mujer24_59_m <- ifelse(loop$edad_sexo_grupo == "mujer24_59_m",1,0)

loop$hombre6_11 <- ifelse(loop$edad_sexo_grupo == "hombre6_11",1,0)
loop$mujer6_11 <- ifelse(loop$edad_sexo_grupo == "mujer6_11",1,0)

loop$hombre12_17 <- ifelse(loop$edad_sexo_grupo == "hombre12_17",1,0)
loop$mujer12_17 <- ifelse(loop$edad_sexo_grupo == "mujer12_17",1,0)

loop$hombre18_29 <- ifelse(loop$edad_sexo_grupo == "hombre18_29",1,0)
loop$mujer18_29 <- ifelse(loop$edad_sexo_grupo == "mujer18_29",1,0)

loop$hombre30_59 <- ifelse(loop$edad_sexo_grupo == "hombre30_59",1,0)
loop$mujer30_59 <- ifelse(loop$edad_sexo_grupo == "mujer30_59",1,0)

loop$hombre60 <- ifelse(loop$edad_sexo_grupo == "hombre60",1,0)
loop$mujer60 <- ifelse(loop$edad_sexo_grupo == "mujer60",1,0)


loop <- df %>%
  select(registro, departamento, weights, sa8_sa, sa8_sam, sa8_iam, sa8_ias, cari_category) %>%
  left_join(loop, by = "registro")





######################################################
# RUN ANALYSIS
######################################################
loop_with_composites <- loop

dap_name <- "cari_individuo_V2"
analysisplan <- read.csv(sprintf("data/dap/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")

loop_with_composites$departamento <- ifelse(loop_with_composites$departamento %in% c("vichada", 
                                                                                             "vaupes", "amazonas"), "atn", 
                                                loop_with_composites$departamento)

loop_with_composites <- loop_with_composites %>% filter(nivel_estudios_jh != "_")


analysisplan$repeat.for.variable <- "cari_category"
analysisplan$independent.variable <- "one"
#analysisplan$hypothesis.type <- "group_difference"
loop_with_composites$one <- "one"
analysisplan <- analysisplan_nationwide(analysisplan)
analysisplan <- analysisplan_pop_group_aggregated(analysisplan)



weight_fun<-function(loop_with_composites){
  loop_with_composites$weights
}


result <- from_analysisplan_map_to_output(loop_with_composites, analysisplan = analysisplan,
                                          weighting = NULL, cluster_variable_name = NULL,
                                          questionnaire = NULL, confidence_level = 0.95)


name <- "2_reverso_sexo_grupo edad_cari_efsacol22_100222"
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
#summary$max <- NULL
#summary$min <- ifelse(summary$min < 0, 0, summary$min)
#summary$min <- NULL
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






