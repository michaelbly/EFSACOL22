######################################
# GAUGE CHART FOR HOME PAGE
col_stops <- data.frame(
  q = c(0.15, 0.4, .8),
  c = c('#A93131', '#EB9534', '#32B748'),
  stringsAsFactors = FALSE
)

gauge_chart <- highchart() %>%
  hc_chart(type = "solidgauge") %>%
  hc_pane(
    startAngle = -90,
    endAngle = 90,
    background = list(
      outerRadius = '100%',
      innerRadius = '60%',
      shape = "arc"
    )
  ) %>%
  hc_tooltip(enabled = FALSE) %>% 
  hc_yAxis(
    stops = list_parse2(col_stops),
    lineWidth = 0,
    minorTickWidth = 0,
    tickAmount = 2,
    min = 0,
    max = 100,
    labels = list(y = 26, style = list(fontSize = "15px"))
  ) %>%
  hc_add_series(
    data = (round((sum(home_total$count)/7000)*100,0)),
    dataLabels = list(
      y = -50,
      borderWidth = 0,
      useHTML = TRUE,
      style = list(fontSize = "27px")
    )
  ) %>% 
  hc_size(height = 260)



######################################
######## Data Table
data_table_enumerator <- function(){
  df %>% 
    filter(departamento == input$table_department) %>% 
    dplyr::group_by(entrevistador) %>% 
    dplyr::summarise(Cargado = n(),
                     Flagged = sum(time_validity == "Flagged", na.rm = T),
                     Median_Duration = round(median(duracion / 60),0),
                     Nr_surveys_pd = round((n() / n_distinct(date_assessment, na.rm = T)),1) %>%
                       sort(Flagged, decreasing = T))
}
# MODIFY CODE BELOW: Render a DT output named "table_top_10_names"
output$data_table_enumerator <- DT::renderDT({
  DT::datatable(data_table_enumerator(), options = list(
    lengthMenu = list(c(10, 50, -1), c('10', '50', 'Todos')),
    pageLength = 50))
})



##############################
# BAR CHART FOR DEPARTMENT COMPARISON
bar_chart_maker <- function(){
  bar_chart_data %>% 
    filter(
      variable == input$indicator_checks
    ) %>% 
    slice_max(values,n = input$nr_departments) %>%
    hchart(type = "column", hcaes(x=departamento, y = values)) %>%
    hc_colors('rgb(201,232,246)') %>%
    hc_tooltip(valueSuffix = "") %>%
    hc_plotOptions(
      series = list(showInLegend = F)
    ) %>%
    hc_title(text = paste0(input$indicator_checks))
  
}



########################
# BOXPLOT FOR DATA EXPLORER
boxplot_maker <- function(){
  gaggi <- df %>% select(groups_numeric$Variable, "departamento") %>%
    pivot_longer(!departamento, names_to = "variables", values_to = "values") %>%
    filter(variables == input$variables_boxplot)
  
  dat <- data_to_boxplot(gaggi, values, departamento, 
                         name = "height in meters")
  
  
  highchart() %>%
    hc_xAxis(type = "category") %>%
    hc_add_series_list(dat)%>%
    
    hc_title(text = "Distribution of values by department") %>%
    hc_yAxis(title = list(text = "Value")) %>%
    hc_plotOptions(boxplot = list(fillColor = "#C9E8F6",
                                  lineWidth = 1,
                                  lineColor = "#075287",
                                  medianWidth = 2,
                                  medianColor = "#4BBDEB",
                                  stemColor = "#075287",
                                  stemWidth = 1,
                                  whiskerColor = "#075287",
                                  whiskerLength = "0%",
                                  whiskerWidth = 1),
                   series = list(dataSorting = list(enabled = TRUE, sortKey = "median"), 
                                 showInLegend = F)
    )  %>%
    hc_tooltip(pointFormat = "Max: {point.high}<br>
                                          Q3:\u00a0\u00a0 {point.q3}<br>
                                          Med: {point.median}<br>
                                          Q1:\u00a0\u00a0 {point.q1}<br>
                                          Min:\u00a0 {point.low}<br>")
  
  
}




############################################
# PIE CHART CARI
colors <- c('rgb(201,232,246)', 'rgb(118,210,242)', 'rgb(71,146,199)', 'rgb(7,82,135)')

pie_chart_maker <- function(){
  cari %>% filter(
    departamento == input$departments_CARI
  ) %>% 
    hchart(type = "pie", hcaes(x=cari_category, y = Percentage)) %>%
    hc_colors(colors) %>%
    hc_tooltip(valueSuffix = "% de hogares") %>%
    hc_plotOptions(
      series = list(showInLegend = TRUE)
    ) %>%
    hc_title(text = "% de hogares por puntaje CARI")
  
}