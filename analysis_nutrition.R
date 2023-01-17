source("functions/nutrition_recoding.R")

response_nutritional <- recoding_nutrition(df_n)

#response_nutritional$weights <- round(response_nutritional$weights)
#write.csv(response_nutritional, "Output/dataset/nutritional_dataset_merged.csv")



#CREATE NEW FUNCTION FOR WEIGHTING
weight_fun_antro<-function(df){
  response_nutritional$weights
}

#LOAD ANALYSISPLAN
dap_name <- "nutrition"
analysisplan <- read.csv(sprintf("data/dap/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")
analysisplan$repeat.for.variable <- "one"
analysisplan$independent.variable <- "one"
response_nutritional$one <- "one"



result <- from_analysisplan_map_to_output(response_nutritional, analysisplan = analysisplan,
                                          weighting = NULL, cluster_variable_name = NULL,
                                          questionnaire = NULL, confidence_level = 0.95)


name <- "unweighted_full_nutricion"
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


write.csv(summary, sprintf("output/raw_results/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
library(plyr)


for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, sprintf("output/summary_sorted/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE, showNA = F)
  } else {
    write.xlsx(df, file=sprintf("output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE, showNA = F)
  }
}

