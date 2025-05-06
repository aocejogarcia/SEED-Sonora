#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
## app.R ##
library(shiny)
library(shinydashboard)
library(shinylive)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(stringr)

#defunciones <- read_rds('../snsp_inteligencia/defunciones/defunciones_app.rds')
#poblaciones <- readxl::read_xlsx('../snsp_inteligencia/defunciones/Poblaciones_Sonora.xlsx')

#readxl::read_xlsx('../snsp_inteligencia/defunciones/DIAGNOSTICOS_20240416.xlsx') %>% 
#  select(CATALOG_KEY, PRINMORTA, NOMBRE, CAPITULO) %>% 
#  rename(CIECAUSABASICA = CATALOG_KEY) %>%
#  right_join(defunciones) %>% 
#  saveRDS('../snsp_inteligencia/defunciones/defunciones_app.rds')

defunciones <- read_rds('defunciones_app.rds')
poblaciones <- read_rds('Poblaciones_Sonora.rds')

opciones_DSB <- defunciones %>%
  filter(ENTIDADRESIDENCIA %in% 26) %>% 
  count(DSBRESIDENCIA, MUNICIPIORESIDENCIA) %>%
  filter(!is.na(DSBRESIDENCIA)) %>% 
  select(-n) %>% 
  left_join(
    poblaciones %>% 
      count(CLAVE, MUN) %>% 
      select(-n) %>% 
      mutate(CLAVE = CLAVE - 26000) %>% 
      rename(MUNICIPIORESIDENCIA = CLAVE)
  )
# Distritos y municipios ----
lista <- list(
  '1' = opciones_DSB$MUN[opciones_DSB$DSBRESIDENCIA %in% 1],
  '2' = opciones_DSB$MUN[opciones_DSB$DSBRESIDENCIA %in% 2],
  '3' = opciones_DSB$MUN[opciones_DSB$DSBRESIDENCIA %in% 3],
  '4' = opciones_DSB$MUN[opciones_DSB$DSBRESIDENCIA %in% 4],
  '5' = opciones_DSB$MUN[opciones_DSB$DSBRESIDENCIA %in% 5],
  '6' = opciones_DSB$MUN[opciones_DSB$DSBRESIDENCIA %in% 6],
  'Todos' = opciones_DSB$MUN
)


# Grupos de edad ----
lista_edad <- list(
  'Menores de 1 año'= c(0, 'pobm_00_04'), 
  'De 1 a 4 años' = c(seq(1,4), 'pobm_00_04'), 
  'De 5 a 9 años' = c(seq(5,9), 'pobm_05_09'), 
  'De 10 a 19 años' = c(seq(10,19), 'pobm_10_14', 'pobm_15_19'), 
  'De 20 a 59 años' = c(seq(20,59), 'pobm_20_24', 'pobm_25_29', 'pobm_30_34', 'pobm_35_39', 'pobm_40_44', 'pobm_45_49', 'pobm_50_54', 'pobm_55_59'), 
  '60 años y más' = c(seq(60,124), 'pobm_60_64', 'pobm_65_mm'), 
  'General' = c(seq(0, 124), unique(poblaciones$EDAD_QUIN))
)

# Sexo -----
lista_sexo <- list(
  'Mujeres' = c('Mujeres', 'MUJER'),
  'Hombres' = c('HOMBRE', 'Hombres'),
  'Ambos' = c('Mujeres', 'MUJER', 'HOMBRE', 'Hombres', 'NO ESPECIFICADO', 'SE IGNORA')
)

## UI ##
ui <- dashboardPage(skin = 'red',
                    dashboardHeader(title = 'SEED-SONORA'),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
                      ),
                      selectInput(inputId = 'AÑO',
                                  label = 'Año de defunción',
                                  choices = c(2020, 2021, 2022, 2023, 2024, 2025),
                                  selected = 2024),
                      selectInput(inputId = 'DSB',
                                  label = 'Seleccione el Distrito de Salud para el Bienestar',
                                  choices = c(sort(unique(defunciones$DSBRESIDENCIA)), 'Todos'),
                                  selected = 'Todos'),
                      selectInput(inputId = 'MUNICIPIO',
                                  label = 'Seleccione el municipio',
                                  choices = c(unique(opciones_DSB$MUN), 'Todos'),
                                  selected = 'Todos'),
                      selectInput(inputId = 'EDAD',
                                  label = 'Seleccione el grupo de edad',
                                  choices = c('Menores de 1 año', 'De 1 a 4 años', 'De 5 a 9 años', 'De 10 a 19 años', 'De 20 a 59 años', '60 años y más', 'General'),
                                  selected = 'General'),
                      selectInput(inputId = 'SEXO',
                                  label = 'Seleccione el sexo',
                                  choices = c('Mujeres', 'Hombres', 'Ambos'),
                                  selected = 'Ambos')
                    ),
                    dashboardBody(
                      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                      tabItems(
                        tabItem(tabName = 'dashboard',
                                fluidRow(
                                  #box(title = 'Unidades por nivel de atención por municipio',
                                  DTOutput('data1')#)
                                ),
                                fluidRow(
                                  #box(title = 'Unidades por nivel de atención por municipio',
                                  DTOutput('data2')#)
                                )
                        )
                      )
                    )
)


server <- function(input, output, session) {
  ## Base de datos ##
  pob_cl <- reactive({
    if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'General' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>% 
        filter(AÑO %in% input$AÑO) %>% 
        summarise(poblacion = sum(POB, na.rm = T))
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'General' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               AÑO %in% input$AÑO) %>%
        summarise(poblacion = sum(POB, na.rm = T))
      
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'General' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO) %>%
        summarise(poblacion = sum(POB, na.rm = T))
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'General' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO) %>%
        summarise(poblacion = sum(POB, na.rm = T))
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'Menores de 1 año' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.20)
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'De 1 a 4 años' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.80)
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & !input$EDAD %in% c('General', 'De 1 a 4 años', 'Menores de 1 año') & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T))
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'Menores de 1 año' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               #MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.20)
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'De 1 a 4 años' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               #MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.80)
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & !input$EDAD %in% c('General', 'De 1 a 4 años', 'Menores de 1 año') & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               #MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T))
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'Menores de 1 año' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.20)
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'De 1 a 4 años' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.80)
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & !input$EDAD %in% c('General', 'De 1 a 4 años', 'Menores de 1 año') & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T))
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'Menores de 1 año' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          #MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.20)
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'De 1 a 4 años' & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          #MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.80)
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & !input$EDAD %in% c('General', 'De 1 a 4 años', 'Menores de 1 año') & input$SEXO %in% 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          #MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]]) %>%
        summarise(poblacion = sum(POB, na.rm = T))
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'General' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>% 
        filter(AÑO %in% input$AÑO, 
               SEXO %in% input$SEXO) %>% 
        summarise(poblacion = sum(POB, na.rm = T))
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'General' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               AÑO %in% input$AÑO, 
               SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T))
      
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'General' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO, 
               SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T))
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'General' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO, 
               SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T))
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'Menores de 1 año' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]], 
               SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.20)
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'De 1 a 4 años' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]], 
               SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.80)
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & !input$EDAD %in% c('General', 'De 1 a 4 años', 'Menores de 1 año') & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]], 
               SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T))
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'Menores de 1 año' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               #MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]], 
               SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.20)
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'De 1 a 4 años' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               #MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]], 
               SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.80)
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & !input$EDAD %in% c('General', 'De 1 a 4 años', 'Menores de 1 año') & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(DSB %in% input$DSB,
               #MUN %in% input$MUNICIPIO,
               AÑO %in% input$AÑO,
               EDAD_QUIN %in% lista_edad[[input$EDAD]], 
               SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T))
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'Menores de 1 año' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]], 
          SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.20)
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'De 1 a 4 años' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]], 
          SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.80)
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & !input$EDAD %in% c('General', 'De 1 a 4 años', 'Menores de 1 año') & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]], 
          SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T))
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'Menores de 1 año' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          #MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]], 
          SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.20)
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'De 1 a 4 años' & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          #MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]], 
          SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T)*.80)
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & !input$EDAD %in% c('General', 'De 1 a 4 años', 'Menores de 1 año') & input$SEXO != 'Ambos') {
      poblaciones <- poblaciones %>%
        filter(#DSB %in% input$DSB,
          #MUN %in% input$MUNICIPIO,
          AÑO %in% input$AÑO,
          EDAD_QUIN %in% lista_edad[[input$EDAD]], 
          SEXO %in% input$SEXO) %>%
        summarise(poblacion = sum(POB, na.rm = T))
    }
  })
  
  def_cl <- reactive({
    if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'General' & input$SEXO %in% 'Ambos') {
      defunciones <- defunciones %>% 
        filter(AÑO %in% input$AÑO)
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'General' & input$SEXO %in% 'Ambos') {
      defunciones <- defunciones %>%
        filter(DSBRESIDENCIA %in% input$DSB,
               AÑO %in% input$AÑO)
      
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'General' & input$SEXO %in% 'Ambos') {
      defunciones <- defunciones %>%
        filter(MUNICIPIORESIDENCIA %in% opciones_DSB$MUNICIPIORESIDENCIA[opciones_DSB$MUN %in% input$MUNICIPIO],
               AÑO %in% input$AÑO)
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'General' & input$SEXO %in% 'Ambos') {
      defunciones <- defunciones %>%
        filter(DSBRESIDENCIA %in% input$DSB,
               MUNICIPIORESIDENCIA %in% opciones_DSB$MUNICIPIORESIDENCIA[opciones_DSB$MUN %in% input$MUNICIPIO],
               AÑO %in% input$AÑO)
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD != 'General' & input$SEXO %in% 'Ambos') {
      defunciones <- defunciones %>% 
        filter(AÑO %in% input$AÑO, 
               EDAD_años %in% lista_edad[[input$EDAD]])
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD != 'General' & input$SEXO %in% 'Ambos') {
      defunciones <- defunciones %>%
        filter(DSBRESIDENCIA %in% input$DSB,
               AÑO %in% input$AÑO, 
               EDAD_años %in% lista_edad[[input$EDAD]])
      
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD != 'General' & input$SEXO %in% 'Ambos') {
      defunciones <- defunciones %>%
        filter(MUNICIPIORESIDENCIA %in% opciones_DSB$MUNICIPIORESIDENCIA[opciones_DSB$MUN %in% input$MUNICIPIO],
               AÑO %in% input$AÑO,
               EDAD_años %in% lista_edad[[input$EDAD]])
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD != 'General' & input$SEXO %in% 'Ambos') {
      defunciones <- defunciones %>%
        filter(DSBRESIDENCIA %in% input$DSB,
               MUNICIPIORESIDENCIA %in% opciones_DSB$MUNICIPIORESIDENCIA[opciones_DSB$MUN %in% input$MUNICIPIO],
               AÑO %in% input$AÑO, 
               EDAD_años %in% lista_edad[[input$EDAD]])
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'General' & input$SEXO != 'Ambos') {
      defunciones <- defunciones %>% 
        filter(AÑO %in% input$AÑO, 
               SEXOD %in% lista_sexo[[input$SEXO]])
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD %in% 'General' & input$SEXO != 'Ambos') {
      defunciones <- defunciones %>%
        filter(DSBRESIDENCIA %in% input$DSB,
               AÑO %in% input$AÑO, 
               SEXOD %in% lista_sexo[[input$SEXO]])
      
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'General' & input$SEXO != 'Ambos') {
      defunciones <- defunciones %>%
        filter(MUNICIPIORESIDENCIA %in% opciones_DSB$MUNICIPIORESIDENCIA[opciones_DSB$MUN %in% input$MUNICIPIO],
               AÑO %in% input$AÑO, 
               SEXOD %in% lista_sexo[[input$SEXO]])
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD %in% 'General' & input$SEXO != 'Ambos') {
      defunciones <- defunciones %>%
        filter(DSBRESIDENCIA %in% input$DSB,
               MUNICIPIORESIDENCIA %in% opciones_DSB$MUNICIPIORESIDENCIA[opciones_DSB$MUN %in% input$MUNICIPIO],
               AÑO %in% input$AÑO, 
               SEXOD %in% lista_sexo[[input$SEXO]])
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD != 'General' & input$SEXO != 'Ambos') {
      defunciones <- defunciones %>% 
        filter(AÑO %in% input$AÑO, 
               EDAD_años %in% lista_edad[[input$EDAD]], 
               SEXOD %in% lista_sexo[[input$SEXO]])
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO %in% 'Todos' & input$EDAD != 'General' & input$SEXO != 'Ambos') {
      defunciones <- defunciones %>%
        filter(DSBRESIDENCIA %in% input$DSB,
               AÑO %in% input$AÑO, 
               EDAD_años %in% lista_edad[[input$EDAD]], 
               SEXOD %in% lista_sexo[[input$SEXO]])
      
    } else if (input$DSB %in% 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD != 'General' & input$SEXO != 'Ambos') {
      defunciones <- defunciones %>%
        filter(MUNICIPIORESIDENCIA %in% opciones_DSB$MUNICIPIORESIDENCIA[opciones_DSB$MUN %in% input$MUNICIPIO],
               AÑO %in% input$AÑO,
               EDAD_años %in% lista_edad[[input$EDAD]], 
               SEXOD %in% lista_sexo[[input$SEXO]])
      
    } else if (input$DSB != 'Todos' & input$MUNICIPIO != 'Todos' & input$EDAD != 'General' & input$SEXO != 'Ambos') {
      defunciones <- defunciones %>%
        filter(DSBRESIDENCIA %in% input$DSB,
               MUNICIPIORESIDENCIA %in% opciones_DSB$MUNICIPIORESIDENCIA[opciones_DSB$MUN %in% input$MUNICIPIO],
               AÑO %in% input$AÑO, 
               EDAD_años %in% lista_edad[[input$EDAD]], 
               SEXOD %in% lista_sexo[[input$SEXO]])
    }
  })
  ## Tabla No.1 ##
  output$data1 <- renderDT({
    defunciones <- def_cl() %>%
      #mutate(PRINMORTA = substring(PRINMORTA, first = 1, last = 3)) %>% 
      count(CAPITULO) %>%
      rename(Casos = n, 
             #'Código CIE-10' = CIECAUSABASICA, 
             #'Causa de defunción' = CIECAUSABASICAD, 
             'Causas' = CAPITULO) %>% 
      cross_join(pob_cl()) %>% 
      mutate('Tasa de mortalidad' = round(Casos/poblacion*100000, 1)) %>% 
      arrange(desc(Casos)) %>% 
      select(-poblacion) %>% 
      datatable(extensions = 'Buttons', caption = 'Principales causas de defunción agrupadas', rownames = FALSE,
                options = list(dom = 'Blfrtip',
                               buttons = c('excel')#,
                               #lengthMenu = list(c(-1),
                               #                   c("All"))
                ))
  })
  ## Tabla No.2 ##
  output$data2 <- renderDT({
    defunciones <- def_cl() %>%
      #mutate(PRINMORTA = substring(PRINMORTA, first = 1, last = 3)) %>% 
      count(CAPITULO, CIECAUSABASICA, CIECAUSABASICAD) %>%
      rename(Casos = n, 
             'Código CIE-10' = CIECAUSABASICA, 
             'Causa de defunción' = CIECAUSABASICAD, 
             'Causas agrupadas' = CAPITULO) %>% 
      cross_join(pob_cl()) %>% 
      mutate('Tasa de mortalidad' = round(Casos/poblacion*100000, 1)) %>% 
      arrange(desc(Casos)) %>% 
      select(-poblacion) %>% 
      datatable(extensions = 'Buttons', caption = 'Principales causas de defunción', rownames = FALSE,
                options = list(dom = 'Blfrtip',
                               buttons = c('excel')#,
                               #lengthMenu = list(c(-1),
                               #                   c("All"))
                ))
  })
  ## Reactivity ##
  observeEvent(input$DSB, {
    municipios <- lista
    
    updateSelectInput(session = session,
                      inputId = 'MUNICIPIO',
                      label = 'Seleccione el municipio',
                      choices = c(lista[[input$DSB]], 'Todos'),
                      selected = 'Todos')
    
  })
}

shinyApp(ui, server)

#shinylive::export(appdir = '../snsp_inteligencia/defunciones', destdir = '../snsp_inteligencia/defunciones/docs')