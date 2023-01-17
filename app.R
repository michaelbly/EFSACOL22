#SETUP
##############
##############
rm(list=ls(all=T))
R.version
#### 1 LOAD PACKAGES ###########################################################

library(dplyr)                                                                    # data wrangling work horse
library(tidyr)                                                                    # additional data wrangling
library(tidytidbits)                                                              # for conditional piping
library(stringr)                                                                  # to do some operations with strings
#library(shiny)                                                                    # for shiny app functions
library(shinyWidgets)                                                             # additional UI options for shiny
library(shinythemes)                                                              # to apply a theme to the shiny app
library(sf)                                                                       # to read/manipulate shapefiles
library(leaflet)                                                                  # to display maps
library(leaflet.extras)                                                           # additional options for leaflet
library(highcharter)                                                              # to build plots
library(DT)                                                                       # for datatable in data explorer
library(kableExtra)                                                               # to make tables
library(scales)                                                                   # to define percentages
library(shinydashboard)
library(leaflet.extras)
library(stringi)
library(ggplot2)
library(forcats)
library(plotly)
library(shinyauthr)
library(shinymanager)
library(xlsx)
library(rJava)
library("pryr")
library("rgdal")
library("rmapshaper")
library(raster)
library(conflicted)
library(hypegrammaR) # simple stats 4 complex samples
library(surveyweights) # calculate weights from samplingframes

options(java.parameters = "-Xss2560k")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("summarise", "dplyr")



#### 2 LOAD DATA ###############################################################
source("functions/cari_recoding.R")
source("functions/import_nutritional.R")
source("functions/import_prepare_dataset.R")
source("functions/function_handler.R")
source("functions/cleaning_checks.R")
source("functions/exp_cleaner.R")
source("functions/samplingframe_weights.R")
source("functions/postprocessing_functions.R")
source("functions/loop_generator.R")


adm <- st_read("gis/shapefiles_unzipped/col_admbnda_adm1_mgn_20200416.shp") %>% 
  st_transform(crs = 4326)
to_alphanumeric_lowercase <- function (x)
{
  tolower(gsub("[^a-zA-Z0-9_]", "\\_", x))
}
accented_letters <- function (x){
  stri_replace_all_fixed(x,
                         c("á","é","ń","í","ó","ú","ñ","ü","Á","Ó","Í","Ñ"),
                         c("a","e","n","i","o","u","n","u","A","O","I","N"),
                         vectorize_all = FALSE)}
adm$ADM1_ES <- accented_letters(adm$ADM1_ES)
adm$ADM1_ES <- to_alphanumeric_lowercase(adm$ADM1_ES)
adm$ADM1_ES
names(adm)[names(adm) == "ADM1_ES"] <- "departamento"
adm$departamento[adm$departamento == "bogota__d_c_"] <- "bogota_dc"



object_size(adm)
# ~13MB

adm <- ms_simplify(adm)
object_size(adm)
# < 1MB
df$registro <- as.factor(as.numeric(df$registro))

#merge inconsistencies dataset with df
df <- df %>% left_join(incoherences_registro, by = "registro")

# import analysisplan 
dap_name <- "preliminary_fs"
analysisplan <- read.csv(sprintf("data/dap/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")
sub_analysisplan <- analysisplan %>% select(research.question, sub.research.question, dependent.variable) %>%
                                     rename(dependent.var = dependent.variable,
                                            indicator = research.question,
                                            sub_indicator = sub.research.question)


overall_merged <- df %>% 
  group_by(departamento) %>%
  left_join(samplingframe_sample, by="departamento") %>%
  dplyr::summarise(Aceptado = n(),
                   Tamano_Muestra = mean(Tamano_Muestra),
                   Datos_Anthropometricos = sum(count_nutrition, na.rm = T),
                   Restante = Tamano_Muestra - n())




df.map <- left_join(adm, overall_merged, by="departamento") %>%
  mutate(pct.done=pmin(Aceptado/Tamano_Muestra*100, 100))

cols      <- c("rgb(238,88,89)",   "rgb(88,88,90)",    "rgb(165,201,161)",        # define color palette for plot lines
               "rgb(86,179,205)",  "rgb(246,158,97)",  "rgb(255,246,122)",
               "rgb(210,203,184)", "rgb(247,172,172)", "rgb(172,172,173)",
               "rgb(210,228,208)", "rgb(171,217,230)", "rgb(251,207,176)",
               "rgb(255,251,189)", "rgb(233,229,220)")


groups_numeric <- read.csv("data/categories_numeric_variables.csv", sep = ";"
                           , comment.char = "", strip.white = TRUE,
                           stringsAsFactors = F, encoding="UTF-8-BOM")


dates <- sort(unique(df$date_assessment))                                           # define list with date range in data
dates_min  <- as.Date("2022-09-14")                                               # set minimum date to be displayed
dates_max  <- as.Date(max(df$date_assessment))
#add day of data collection
df <- df %>% group_by(date_assessment) %>%
  transform(df,day_dc =as.numeric(factor(date_assessment)))




# reshape dataset to only include count surveys total, and count for vocacion de permanencia and retornados
df$count <- 1
df$date_assessment <- substring(df$date_assessment, 6)


home_total <- df %>% select(date_assessment, count) %>%
  group_by(date_assessment) %>%
  summarise(count = sum(count))

home_merged <- home_total


# generate map
pal.strata <- colorNumeric(c("red", "yellow", "green"), 0:100)
pal <- colorNumeric(palette = "RdYIGn", 0:100)


labels <- paste(
  "<strong>", df.map$departamento,
  "</strong><br>% Completado:", round(df.map$pct.done,  1),
  "</strong><br># Cargado:", df.map$Aceptado, 
  "</strong><br># Restante:", df.map$Restante, 
  "</strong><br>Tamano Muestra:", df.map$Tamano_Muestra,
  "</strong><br>Datos Anthro.:", df.map$Datos_Anthropometricos) %>%
  lapply(htmltools::HTML)




# create overview table
start_date <- "21.09.2022"
date_today <- format(Sys.Date(), "%d.%m.%Y")
municipios_covered    <- n_distinct(df$municipio, na.rm = FALSE)
departamentos_covered <- n_distinct(df$departamento, na.rm = FALSE)
nr_enumerators <- n_distinct(df$entrevistador, na.rm = FALSE)
median_duration <- paste0(round(mean(df$duracion/60),0), "min")
median_surveys_pd <-  round((nrow(df) / n_distinct(df$date_assessment, na.rm = T)),1)
df$count <- 1
median_surveys_pd_pe <- df %>% 
  group_by(entrevistador, date_assessment) %>%
  dplyr::summarise(hallo = n())
median_surveys_pd_pe <- round(median(median_surveys_pd_pe$hallo),1)


overview_round       <- data.frame(figure = c("Fecha Actualicazion", "Fecha de Inicio", "Municipios Cubiertos", "# de Encuestadores", "Duración Media Entrevista", "Mediana # encuestas pd", "Mediana # encuestas pd y encuestador"),
                                   value  = c(date_today, start_date, municipios_covered, nr_enumerators, median_duration, median_surveys_pd, median_surveys_pd_pe)
)

table_round <- overview_round %>%                                                                         # style overview table
  kbl(escape = F, format.args = list(big.mark = ","), align = "lr", col.names = NULL) %>%
  column_spec(1, width = "12em") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T, full_width = T) %>%
  row_spec(1,   extra_css = "font-size: 11.5px; border-top: 2px solid gainsboro") %>%
  row_spec(2:7, extra_css = "font-size: 11.5px;")

df$departamento <- as.character(as.factor(df$departamento))



# prepare dataset for department tab
bar_chart_table <- df %>%
  group_by(departamento) %>%
  dplyr::summarise(Cargado = n(),
                   menos35min = sum(time_validity == "Flagged", na.rm = T),
                   Median_Duration = round(median(duracion / 60),0),
                   Medidas_Anthropometricas = sum(count_nutrition, na.rm = T),
                   Nr_Incoherencias = sum(nr_inhoherencias, na.rm = T),
                   Nr_surveys_pd = round((n() / n_distinct(date_assessment, na.rm = T)),1) %>%
                     sort(menos35min, decreasing = T)) %>%
  pivot_longer(cols = 2:6, names_to = "variable", values_to = "values") %>%
  arrange(desc(values)) %>%
  mutate(variable = recode(variable, 
                           "Cargado"="Número de encuestas completadas",
                           "menos35min" = "Número de encuestas completadas en < 35min",
                           "Median_Duration" = "Duración media de la entrevista",
                           "Medidas_Anthropometricas" = "Número de encuestas anthropometricas cargadas",
                           "Nr_surveys_less_20" = "Número de encuestas completadas en < 20min",
                           "Nr_surveys_pd" = "Número de encuestas por dia",
                           "Nr_Incoherencias" = "Número de incoherencias en las encuestas"))



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
    data = (round((sum(home_total$count)/7900)*100,0)),
    dataLabels = list(
      y = -50,
      borderWidth = 0,
      useHTML = TRUE,
      style = list(fontSize = "27px")
    )
  ) %>% 
  hc_size(height = 260)



# clean expenditure data with Tito's method
df <- expenditure_cleaner_30d(df)
df <- expenditure_cleaner_6m(df)


cari <- recoding_cari(df) %>%
  select(departamento, cari_category) %>%
  group_by(departamento, cari_category) %>%
  drop_na(cari_category) %>%
  summarise(Percentage=n()) %>%
  group_by(departamento) %>%
  mutate(Percentage=round(Percentage/sum(Percentage)*100),0)
cari$n <- 1


loop <- loop_generator(df)


df <- recoding_cari(df)


#### 6 UI ######################################################################

ui <- bootstrapPage(
  
  navbarPage("EFSA22 | Data Collection",                                                # define dashboard title
             theme = shinytheme("flatly"),                                                             # set theme
             setSliderColor(rep("Grey",(2)), sliderId = c(1:10)),

             #### * 6.1 Home ######################################################################
             
             tabPanel("Overview",  
                      tags$head(
                        tags$style(HTML(".leaflet-container { background: #FFFFFF; }"))
                      ),            # define panel title
                      
                      leafletOutput("map_home_gaggi", width = "100%", height = 910), 
                      tags$style(type = "text/css", ".container-fluid {padding-left:0px;
                    padding-right:0px;}"),
                      tags$style(type = "text/css", ".navbar {margin-bottom: .5px;}"),
                      tags$style(type = "text/css", ".container-fluid .navbar-header 
                    .navbar-brand {margin-left: 0px;}"),# display background map
                      
                      absolutePanel(                                                                # define introduction box
                        id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                        top = 90, left = "20", right = "auto", bottom = "auto", width = "600", height = 270,
                        div(style = "margin-top: -8px;"),
                        
                        h4("Introducción:"),
                        p("Este dashboard proporciona una herramienta para supervisar y controlar la recoleccion de datos de la Evaluación de la Seguridad Alimentaria de Emergencia en Colombia (ESAE) de 2022, realizada por el Programa Mundial de Alimentos (PMA) y el Centro Nacional de Consultoría (CNC). 
                            El dashboard proporciona información sobre el progreso de la recoleccion de datos e identifica los problemas e inconsistencias por departamento y encuestador para que los problemas o discrepancias puedan ser identificados y corregidos durante la recoleccion de datos. 
                            El dashboard se actualiza diariamente y la última actualización se menciona en el cuadro resumen.
                            Toda la información sobre las encuestas recogidas con poca duración o con incoherencias aparece en las pestañas Departamentos y Encuestadores. En la última pestaña denominada Data Explorer se pueden investigar y visualizar diferentes variables. ",
                          style="text-align:justify"),
                        br()
                      ),
                      
                      
                      
                      absolutePanel(                                                                    # define chart box
                        id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                        top = "400", left = "20", right = "auto", bottom = "auto", width = "600", height = "500",
                        div(style = "margin-top: -8px;"),
                        
                        h4("Número de encuestas por día:"),
                        div(style = "margin-top: -12px;"),
                        
                        tags$br(),
                        hchart(home_merged, "column",                                           # define chart
                               hcaes(x = date_assessment, y = count)) %>%
                          hc_yAxis(min = 0, title = list(text = "")) %>%
                          hc_xAxis(title = "", labels = list(align = "center")) %>%
                          hc_size(height = "400") %>%
                          hc_title(
                            text = "",
                            margin = 10,
                            align = "left",
                            style = list(fontSize = 15)
                          ) %>%
                          hc_colors('rgb(201,232,246)') %>%
                          hc_legend(align = "left", 
                                    layout = "horizontal")
                      ),
                      
                      absolutePanel(
                        id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                        top = "90", left = "680", right = "auto", bottom = "auto", width = "290", height = "270",
                        div(style = "margin-top: -8px;"),
                        
                        h4("Resumen:"),
                        div(style = "margin-top: 3px;"),
                        
                        HTML(table_round), br()
                      ),
                      
                      
                      absolutePanel(
                        id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                        top = "90", left = "1000", right = "auto", bottom = "auto", width = "250", height = "270",
                        div(style = "margin-top: -8px;"),
                        
                        h4("Número de encuestas recogidas:"),
                        div(style = "margin-top: -15px;"),
                        h1(strong(format(sum(home_total$count), big.mark = ","), style = "color: #922121")), 
                        div(style = "margin-top: -5px;"),
                        
                        print(gauge_chart)
                      ),
                      
                      
                      
                      absolutePanel(                                                                    # define chart box
                        id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                        top = "400", left = "680", right = "auto", bottom = "auto", width = "570", height = "500",
                        div(style = "margin-top: -8px;"),
                        h4("Mapa de Progreso:"),
                        tags$a(leafletOutput("map_progress", width = "500", height = "420"), target = "_blank")
                      )
                      , 
                      
                      
                      absolutePanel(id = "logo", class = "card", bottom = 18, right = 10, fixed=TRUE, draggable = FALSE, height = "auto",
                                    tags$a(href='https://www.wfp.org', target = "_blank",
                                           tags$img(src='wfp_logo.png', height='50'))),
                      absolutePanel(id = "logo", class = "card", bottom = 16, right = 150, fixed=TRUE, draggable = FALSE, height = "auto",
                                    tags$a(href='https://www.centronacionaldeconsultoria.com/', target = "_blank",
                                           tags$img(src='cnc_logo.png', height='55')))
                      
                      
                      
                      
             ), 
             
             
             tabPanel("Departamentos", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          
                          pickerInput("indicator_checks",
                                      label = "Indicator:",   
                                      options = list(title = "Select"),
                                      choices = sort(unique(bar_chart_table$variable)),
                                      multiple = FALSE, 
                                      selected = "Número de encuestas completadas"
                          ),
                          
                          hr(),
                          
                          numericInput("nr_departments",
                                       label = "Número de departamentos a mostrar:",
                                       value = 15,
                                       min = 1,
                                       max = 40
                          ),
                          
                          hr(),
                          chooseSliderSkin(skin = "Flat", color = "Grey"),
                          sliderTextInput("departamentos_day_select",
                                          "Resultados por días de recoleccion:",
                                          choices = sort(unique(df$day_dc)),
                                          force_edges = TRUE,
                                          selected = c(1, 54)
                          )
                          
                        ),
                        
                        mainPanel(
                          br(),
                          highchartOutput("bar_graph", width = "100%", height = "500px"),                   # display large chart
                          width = 8,    
                        ),
                        
                        
                        
                      )
                      
                      
             ), 
             
             tabPanel("Encuestadores", 

                      sidebarLayout(
                        sidebarPanel(
                          
                          
                          pickerInput("table_department",
                                      label = "Departamento:",   
                                      options = list(title = "Select"),
                                      choices = sort(unique(df$departamento)),
                                      multiple = FALSE, 
                                      selected = "antioquia"
                                      
                          ),
                          
                          hr(),

                          sliderTextInput("enumerators_day_select",
                                          "Resultados por días de recoleccion:",
                                          choices = sort(unique(df$day_dc)),
                                          force_edges = TRUE,
                                          selected = c(1, 54)
                          ),
                          
                          
                          hr(),
                          
                          
                          downloadButton("downloadData", "Descargar como CSV"),
                          hr(),
                          
                          downloadButton("downloadData_incoherencias", "Lista Incoherencias"),
                          
                          width = 3
                        ),
                        
                        mainPanel(
                          DT::dataTableOutput("data_table_enumerator", width = "100%", height = "100%"),
                          width = 9
                        )
                      )
             ),
             
             
             
             
             tabPanel("Data Explorer", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioGroupButtons("plot_type",
                                            label = "Grafica:",
                                            choices = c("Boxplots", "CARI"),
                                            selected = "Boxplots",
                                            justified = TRUE
                          ),
                          hr(),
                          
                          conditionalPanel(condition = "input.plot_type == 'Boxplots'",
                                           pickerInput("variables_boxplot",
                                                       label = "Variable:",   
                                                       options = list(title = "Select"),
                                                       choices = lapply(base::split(groups_numeric$Variable, groups_numeric$Group_Variable), as.list),
                                                       multiple = FALSE, 
                                                       selected = "ingreso"
                                           )),
                          
                          conditionalPanel(condition = "input.plot_type == 'CARI'",
                                           pickerInput("departments_CARI",
                                                       label = "Departamento:",   
                                                       options = list(title = "Select"),
                                                       choices = sort(unique(df$departamento)),
                                                       multiple = FALSE, 
                                                       selected = "antioquia"
                                           ))
                          
                          
                        ),
                        
                        mainPanel(
                          br(),
                          highchartOutput("graph_explorer", width = "100%", height = "500px"),                   # display large chart
                          width = 8,    
                        ),
                        
                        
                        
                      )
                      
                      
             ), 
             
              
             
             tabPanel("Analisis", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          radioGroupButtons("geographic_desagregation",
                                            label = "Desagregacion Geografica:",
                                            choices = c("nacional", "departamento"),
                                            selected = "nacional",
                                            justified = TRUE
                          ),
                          hr(),
                          
                          radioGroupButtons("weighting_analysis",
                                            label = "Ponderacion:",
                                            choices = c("ponderado", "no_ponderado"),
                                            selected = "ponderado",
                                            justified = TRUE
                          ),
                          hr(),
                          
                          
                          pickerInput("popgroup_desagregacion",
                                      label = "Desagregacion Demografico:",   
                                      options = list(title = "Select"),
                                      choices = lapply(base::split(categories_disagregations$Indicator, categories_disagregations$Group), as.list),
                                      multiple = FALSE, 
                                      selected = "ninguna"
                          ), 
                          hr(),
                          
                          pickerInput("sectores_desagregacion",
                                      label = "Sectores:",   
                                      options = list(title = "Select"),
                                      choices = c("asistencia_humanitaria", "seguridad_alimentaria", "situacion_socioeconomica", "vivienda_y_activos"),
                                      multiple = TRUE, 
                                      selected = "seguridad_alimentaria"
                          ), 
                          hr(),
                          
                          
                          actionButton("go","Realizar Analisis", icon("paper-plane"), 
                                       style="color: #fff; background-color: #A9361E; border-color: #762616"),
                          
                          hr(),
                          
                          
                          downloadButton("download_analysis", "Descargar Resultados")
                          
             
                        ),                      #close sidebar panel
                        
                        mainPanel(
                          br(),
                          DT::dataTableOutput("data_table_analysisplan", width = "100%", height = "100%"),
                          width = 8,    
                        ),
                      )                        #close sidebar layout
             )                                 #close tabpanel
             #### * 6.3 Map ######################################################################
             
             
             
             
  )                                                                                                         # close navbarpage
)                                                                                                             # close bootstrappage

#### 7 SERVER ##################################################################

server <- function(input, output, session) {
  #  options(shiny.maxRequestSize=1000*1024^2)
  
  
  output$map_home_gaggi <- renderLeaflet({
    map_home <- leaflet(options = leafletOptions(attributionControl=FALSE, zoomControl = FALSE, dragging = FALSE, minZoom = 12, maxZoom = 12)) %>%
      setView(lng = -74.062356, lat = 4.678103, zoom = 12) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB",
                       options = providerTileOptions(opacity = 0.8))
  })
  
  
  pal <- colorNumeric(c("firebrick", "burlywood", "forestgreen"), 0:100)
  
  labels <- paste(
    "<strong>", df.map$departamento,
    "</strong><br>% Completado:", round(df.map$pct.done,  1),
    "</strong><br># Cargado:", df.map$Aceptado, 
    "</strong><br># Restante:", df.map$Restante, 
    "</strong><br>Tamano Muestra:", df.map$Tamano_Muestra,
    "</strong><br>Datos Anthro.:", df.map$Datos_Anthropometricos) %>%
    lapply(htmltools::HTML)
  
  output$map_progress <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,
                                     minZoom = 5.2, maxZoom = 5.2, dragging = FALSE, width = "100%")) %>%
      addPolygons(data=df.map, color = ~pal(df.map$pct.done),
                  weight = 1, opacity = 1, fill = T, fillOpacity = 0.7,
                  label=labels) %>%
      addMeasure(primaryLengthUnit = "kilometers") 
    
  })
  
  ######## Data Table
  data_table_enumerator <- function(){
    df %>% 
      filter(day_dc >= input$enumerators_day_select[1], day_dc <= input$enumerators_day_select[2]) %>%
      filter(departamento == input$table_department) %>% 
      dplyr::group_by(entrevistador) %>% 
      dplyr::summarise(Cargado = n(),
                       menos35min = sum(time_validity == "Flagged", na.rm = T),
                       Median_Duration = round(median(duracion / 60),0),
                       Nr_Incoherencias = sum(nr_inhoherencias, na.rm = T),
                       Nr_surveys_pd = round((n() / n_distinct(date_assessment, na.rm = T)),1) %>%
                         sort(menos35min, decreasing = T))
  }
  # MODIFY CODE BELOW: Render a DT output named "table_top_10_names
  output$data_table_enumerator <- DT::renderDT({
    DT::datatable(data_table_enumerator(), options = list(
      lengthMenu = list(c(10, 50, -1), c('10', '50', 'Todos')),
      pageLength = 50))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("EFSA recoleccion_cheques de encuestadores_", Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_table_enumerator(), file, row.names = FALSE, na = "")
    }
  )
  
  output$downloadData_incoherencias <- downloadHandler(
    filename = function() {
      paste("lista incoherencias_", Sys.Date(),".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx2(ordered_df, file, row.names = FALSE, na = "")
    }
  )
  
  
  ##############################
  # BAR CHART FOR DEPARTMENT COMPARISON
  bar_chart_maker <- function(){
    df %>%
      filter(day_dc >= input$departamentos_day_select[1], day_dc <= input$departamentos_day_select[2]) %>%
      group_by(departamento) %>%
      dplyr::summarise(Cargado = n(),
                       menos35min = sum(time_validity == "Flagged", na.rm = T),
                       Median_Duration = round(median(duracion / 60),0),
                       Medidas_Anthropometricas = sum(count_nutrition, na.rm = T),
                       Nr_Incoherencias = sum(nr_inhoherencias, na.rm = T),
                       Nr_surveys_pd = round((n() / n_distinct(date_assessment, na.rm = T)),1) %>%
                         sort(menos35min, decreasing = T), 
                       Nr_surveys_less_20 = sum(duracion < 1200, na.rm = T)) %>%
      pivot_longer(cols = 2:6, names_to = "variable", values_to = "values") %>%
      arrange(desc(values)) %>%
      mutate(variable = recode(variable, 
                               "Cargado"="Número de encuestas completadas",
                               "menos35min" = "Número de encuestas completadas en < 35min",
                               "Median_Duration" = "Duración media de la entrevista",
                               "Medidas_Anthropometricas" = "Número de encuestas anthropometricas cargadas",
                               "Nr_surveys_less_20" = "Número de encuestas completadas en < 20min",
                               "Nr_surveys_pd" = "Número de encuestas por dia",
                               "Nr_Incoherencias" = "Número de incoherencias en las encuestas")) %>%

          dplyr::filter(
        variable == input$indicator_checks) %>%
      dplyr::slice_max(values,n = input$nr_departments) %>%
      hchart(type = "column", hcaes(x=departamento, y = values)) %>%
      hc_colors('rgb(201,232,246)') %>%
      hc_tooltip(valueSuffix = "") %>%
      hc_plotOptions(
        series = list(showInLegend = F)
      ) %>%
      hc_title(text = paste0(input$indicator_checks))
  }
  
  
  
  output$bar_graph <- renderHighchart(
    bar_chart_maker()
  )
  
  
  
  
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
      
      hc_title(text = "Distribución de los valores por departamento") %>%
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
  
  
    colors <- c('rgb(71,146,199)', 'rgb(7,82,135)', 'rgb(201,232,246)', 'rgb(118,210,242)')
  
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
  
  
  
  output$graph_explorer <- renderHighchart({
    if(input$plot_type == "Boxplots"){
      graph_explorer <- boxplot_maker()
    }
    else if (input$plot_type == "CARI") 
    {graph_explorer <- pie_chart_maker()}
    
  })
  
  
  analysis_maker_weighted <- eventReactive(input$go,{
      analysisplan$repeat.for.variable <- input$geographic_desagregation
      analysisplan$independent.variable <- input$popgroup_desagregacion
      analysisplan <- analysisplan %>% filter(Indicator.Group...Sector %in% input$sectores_desagregacion)
      
      withProgress(message = 'Realizando analisis', value = 10, {
      result <- from_analysisplan_map_to_output(df, analysisplan = analysisplan,
                                                weighting = weight_fun, cluster_variable_name = NULL,
                                                questionnaire = NULL, confidence_level = 0.95)
      
      })
      summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
      
      summary <- summary %>% left_join(sub_analysisplan, by = "dependent.var") %>%
                             select(indicator, sub_indicator, independent.var.value, numbers, min, max, repeat.var.value) %>%
                             rename(desagregacion = independent.var.value,
                                    porcentaje = numbers, 
                                    lower_ci = min,
                                    upper_ci = max,
                                    desag_geografica = repeat.var.value) %>%
                             mutate(porcentaje = paste0(round(porcentaje*100,0),"%"),
                                    lower_ci = paste0(round(lower_ci*100,0),"%"),
                                    upper_ci = paste0(round(upper_ci*100,0),"%"))
      
      })
  
  analysis_maker_unweighted <- eventReactive(input$go,{
    analysisplan$repeat.for.variable <- input$geographic_desagregation
    analysisplan$independent.variable <- input$popgroup_desagregacion
    analysisplan <- analysisplan %>% filter(Indicator.Group...Sector %in% input$sectores_desagregacion)
    
    withProgress(message = 'Realizando analisis', value = 10, {
      result <- from_analysisplan_map_to_output(df, analysisplan = analysisplan,
                                                weighting = NULL, cluster_variable_name = NULL,
                                                questionnaire = NULL, confidence_level = 0.95)
      
    })
    summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
    
    summary <- summary %>% left_join(sub_analysisplan, by = "dependent.var") %>%
      select(indicator, sub_indicator, independent.var.value, numbers, min, max, repeat.var.value) %>%
      rename(desagregacion = independent.var.value,
             porcentaje = numbers, 
             lower_ci = min,
             upper_ci = max,
             desag_geografica = repeat.var.value) %>%
      mutate(porcentaje = paste0(round(porcentaje*100,0),"%"),
             lower_ci = paste0(round(lower_ci*100,0),"%"),
             upper_ci = paste0(round(upper_ci*100,0),"%"))
    
  })
  
  
  
  # Create data table for analysisplan
  output$data_table_analysisplan <- DT::renderDT({
    if (input$weighting_analysis == "ponderado"){
    DT::datatable(analysis_maker_weighted(), options = list(
      lengthMenu = list(c(10, 50, -1), c('10', '50', 'Todos')),
      pageLength = 50), 
      rownames = F)} else if(input$weighting_analysis == "no_ponderado"){
      
      DT::datatable(analysis_maker_unweighted(), options = list(
          lengthMenu = list(c(10, 50, -1), c('10', '50', 'Todos')),
          pageLength = 50), 
          rownames = F)  
      }
    
  })
  
  
  
  output$download_analysis <- downloadHandler(
    filename = function() {
      paste("Resultados_Analisis_EFSA_", Sys.Date(),".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx2(analysis_maker(), file, row.names = FALSE, na = "")
    }
  )
  
  
}                                                                                 # close server function

shinyApp(ui = ui, server = server)                                                # run the application
