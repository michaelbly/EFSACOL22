response_with_composites <- df

dap_name <- "gastos_perc"
analysisplan <- read.csv(sprintf("data/dap/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")

response_with_composites$departamento <- ifelse(response_with_composites$departamento %in% c("vichada", 
                                                                                             "vaupes", "amazonas"), "atn", 
                                                response_with_composites$departamento)

response_with_composites <- response_with_composites %>% filter(nivel_estudios_jh != "_")



analysisplan$repeat.for.variable <- "urbano_rural"
analysisplan$independent.variable <- "one"
#analysisplan$hypothesis.type <- "group_difference"
response_with_composites$one <- "one"
analysisplan <- analysisplan_nationwide(analysisplan)
analysisplan <- analysisplan_pop_group_aggregated(analysisplan)



weight_fun<-function(response_with_composites){
  response_with_composites$weights
}


result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          weighting = weight_fun, cluster_variable_name = NULL,
                                          questionnaire = NULL, confidence_level = 0.95)


name <- "gastos_weighted_exp share_urbano rural_popgroupagg"
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




