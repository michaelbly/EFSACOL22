############################################
# RESHAPE DATASET TO ONLY INCLUDE COUNT SURVEYS TOTAL AND COUNT FOR VOCACION DE PERMANENCIA AND RETORNADOS
df$count <- 1
df$date_assessment <- substring(df$date_assessment, 6)

home_popgroup <- df %>% select(date_assessment, pop_group, count) %>%
  group_by(date_assessment, pop_group) %>%
  summarise(count = sum(count)) %>%
  filter(pop_group %in% c("retornado", "vocaci_n_de_permanencia")) %>%
  arrange(date_assessment)

home_total <- df %>% select(date_assessment, pop_group, count) %>%
  group_by(date_assessment) %>%
  summarise(count = sum(count)) %>%
  mutate(pop_group = "total") 

home_merged <- home_total %>%
  rbind(home_popgroup)



############################################
# CREATE OVERVIEW TABLE FOR OVERVIEW PAGE
start_date <- "05.08.2022"
municipios_covered    <- n_distinct(df$municipio, na.rm = FALSE)
departamentos_covered <- n_distinct(df$departamento, na.rm = FALSE)
nr_enumerators <- n_distinct(df$entrevistador, na.rm = FALSE)
median_duration <- paste0(round(mean(df$duracion/60),0), "min")
median_surveys_pd <-  round((nrow(df) / n_distinct(df$date_assessment, na.rm = T)),0)
df$count <- 1
median_surveys_pd_pe <- df %>% 
  group_by(entrevistador, date_assessment) %>%
  dplyr::summarise(hallo = n())
median_surveys_pd_pe <- round(median(median_surveys_pd_pe$hallo),1)


overview_round       <- data.frame(figure = c("Fecha de Inicio", "Municipios Cubiertos", "Departamentos Cubiertos", "# de Encuestadores", "Duración Media Entrevista", "Mediana # encuestas pd", "Mediana # encuestas pd y encuestador"),
                                   value  = c(start_date, municipios_covered, departamentos_covered, nr_enumerators, median_duration, median_surveys_pd, median_surveys_pd_pe)
)

table_round <- overview_round %>%                                                                         # style overview table
  kbl(escape = F, format.args = list(big.mark = ","), align = "lr", col.names = NULL) %>%
  column_spec(1, width = "12em") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T, full_width = T) %>%
  row_spec(1,   extra_css = "font-size: 11.5px; border-top: 2px solid gainsboro") %>%
  row_spec(2:7, extra_css = "font-size: 11.5px;")

df$departamento <- as.character(as.factor(df$departamento))



############################################
# BAR CHART DATA FOR ENUMERATOR CHECKS
bar_chart_data <- df %>% 
  group_by(departamento) %>%
  dplyr::summarise(Cargado = n(),
                   Flagged = sum(time_validity == "Flagged", na.rm = T),
                   Median_Duration = round(median(duracion / 60),0),
                   Nr_surveys_pd = round((n() / n_distinct(date_assessment, na.rm = T)),1) %>%
                     sort(Flagged, decreasing = T), 
                   Nr_surveys_less_20 = sum(duracion < 1200, na.rm = T)) %>%
  pivot_longer(cols = 2:6, names_to = "variable", values_to = "values") %>%
  arrange(desc(values)) %>%
  mutate(variable = recode(variable, 
                           "Cargado"="# Submitted Surveys",
                           "Flagged" = "# surveys in < 30min",
                           "Median_Duration" = "Median Interview Duration",
                           "Nr_surveys_less_20" = "# surveys in < 20min",
                           "Nr_surveys_pd" = "# surveys per day")) 



############################################
# CREATE DATASET FOR CARI PIE CHART
cari <- recoding_cari(df) %>%
  select(departamento, cari_category) %>%
  group_by(departamento, cari_category) %>%
  summarise(Percentage=n()) %>%
  group_by(departamento) %>%
  mutate(Percentage=round(Percentage/sum(Percentage)*100),0)
cari$n <- 1
