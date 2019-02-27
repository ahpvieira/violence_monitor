## Carrega pacotes ------------------------
library(stringr)
library(tidyverse)
library(janitor)
library(maptools)
library(rgdal)
library(rgeos) 
library(scales) 
library(sp) 
library(shiny) 
library(plotly) 
library(shinythemes) 
library(htmlwidgets)
library(markdown) 
library(dygraphs) 
library(shinydashboard) 
library(zoo) 
library(xts) 
library(rCharts) 
library(leaflet) 
library(shinyjs)

## Carrega dados --------------------------
load("./data/labels.Rdata")

## Modifica dados -------------------------


includeMarkdown <- function (path) 
{
      html <- markdown::markdownToHTML(path, fragment.only = TRUE)
      return(HTML(html))
}

## Shiny UI -------------------------------

header <- dashboardHeader(
      title = "Monitor do Crime"
)

sidebar <- dashboardSidebar(width = 250,
                            useShinyjs(),
                            div(
                                  id = "form",
      selectizeInput("filter_bairro", "Bairro", 
                                              choices = c("", iconv(bairros, "latin1", "ASCII//TRANSLIT")), 
                                              width = "100%", 
                                              options = list(placeholder = "Selecione um bairro")),
      selectizeInput("filter_sexo", "Sexo", 
                     choices = c("", sexo), 
                     selected = NULL,
                     multiple = TRUE,
                     width = "100%", 
                     options = list(placeholder = "Selecione um sexo")),
      selectizeInput("filter_idade", "Idade", 
                     choices = c("", faixa_idade[c(7, 2, 3, 1, 4, 6, 5)]), 
                     multiple = TRUE,
                     selected = NULL,
                     width = "100%", 
                     options = list(placeholder = "Selecione uma faixa etária")),
      selectizeInput("filter_morte", "Tipo de morte", 
                     choices = c("", tipo_morte), 
                     multiple = TRUE,
                     selected = NULL,
                     width = "100%", 
                     options = list(placeholder = "Selecione um tipo de morte")),
      # column(width = 3, uiOutput("filter_idade")),
      # column(width = 3, uiOutput("filter_morte"))
      # HTML('<hr style="color: white;">')
      selectizeInput("filter_crime", 
                     "Tipo de crime", 
                     choices = c("", tipo_crime), 
                     selected = NULL,
                     multiple = TRUE,
                     width = "100%", 
                     options = list(placeholder = "Selecione um tipo de crime")),
      radioButtons("filter_indicador", 
                   "Indicador (em construção)",
                   choices = c("Número total", "Taxa por 10 mil habitantes"), 
                   width = "100%")),
      actionButton("reset_input", "Limpar filtro", 
                   width = "50%", 
                   style = "color: #fff; background-color: #666; border-color: black")
)

body <- dashboardBody(
      includeCSS("./www/custom.css"),
      useShinyjs(),
      # tags$head(tags$style(
      #       HTML('
      #            label, button, select {
      #            color: white;
      #            }'))),
      theme = shinytheme("slate"),
      title = " ", #footer=source("intro_foot.R", local=TRUE)$value, 
      windowTitle = "Monitor do crime", collapsible = TRUE, fluid = FALSE,
      id = "page",
      tabPanel(
            title = "Home",
            id = "app_home",
            # fluidRow(
            #       column(width = 8, 
            #              includeMarkdown("./intro_app.md"))
            # ),
            # tags$hr(color = "white"),
            br(),
            fluidRow(
                  column(width = 12,
                         shinydashboard::box(title = "Mapa de crimes violentos intencionais em bairros de Maceió (2017)",
                                             width = NULL, 
                                             solidHeader = TRUE,
                                             # background = "black",
                                             status = "danger",
                                             collapsible = TRUE,
                                             leafletOutput("mapa_mcz", height = 600, width = "100%")
                         ))),
            br(),
            fluidRow(
                  column(width = 12,
                         shinydashboard::box(title = "Total de ocorrências ao longo do ano (2017)",
                                             width = NULL, 
                                             solidHeader = TRUE,
                                             # background = "black",
                                             status = "danger",
                                             collapsible = TRUE,
                                             dygraphOutput("plot_serie", height = "500px")))
                  # column(width = 6,
                  #        shinydashboard::box(title = "Total de ocorrências ao longo do dia (2017)",
                  #                            width = NULL,
                  #                            height = 462,
                  #                            solidHeader = TRUE,
                  #                            # background = "black",
                  #                            status = "danger",
                  #                            collapsible = TRUE,
                  #                            # mainPanel(
                  #                            shiny::p(showOutput("plot_hora", lib = "nvd3")
                  #                                     # HTML("<style>.rChart {width: 100%; height: 100%}</style>")
                  #                                     # )
                  #                            )
                  #                            
                  #                            
                  #                            ))
            ),
            br(),
            fluidRow(
                  column(width = 8, 
                         includeMarkdown("credit_app.md"))
            )
      )
)

# tabPanel(
#       title = "Sobre",
#       id = 'about_tabs',
#       fluidRow(
#       )
# )

# shinyUI(
#       navbarPage(
#             theme = shinytheme("slate"),
#             title = " ", #footer=source("intro_foot.R", local=TRUE)$value, 
#             windowTitle = "Mapa do crime em Maceió", collapsible = TRUE, fluid = FALSE,
#             id = "page",
#             
#             tabPanel(
#                   title = "Home",
#                   id = "app_home",
#                   fluidRow(
#                         column(width = 8, includeMarkdown("intro_app.md"))
#                   ),
#                   # br(),
#                   tags$hr(color = "white"),
#                   tagList(
#                         fluidRow(
#                               column(width = 3, selectizeInput("filter_bairro", "Bairro", choices = c("Jatiuca"), selected = "", width = "100%")),
#                               column(width = 3, selectizeInput("filter_sexo", "Sexo", choices = c("Masculino"), selected = "", width = "100%")),
#                               column(width = 3, selectizeInput("filter_idade", "Idade", choices = c("11 ou menos"), selected = "", width = "100%")),
#                               column(width = 3, selectizeInput("filter_tipo", "Tipo de morte", choices = c("PAF"), selected = "", width = "100%"))
#                               # HTML('<hr style="color: white;">')
#                               ),
#                         br(),
#                         leafletOutput("mapa_mcz", height = 600),
#                         br(),
#                         # fluidRow(
#                         #       column(width = 12,
#                         #              shinydashboard::box(title = "Linha do tempo",
#                         #                                  width = NULL,
#                         #                                  solidHeader = TRUE,
#                         #                                  background = "black",
#                         #                                  status = "primary",
#                         #                                  collapsible = TRUE,
#                         dygraphOutput("plot_serie")
#                               # )))
#                         )
#             ),
#             tabPanel(
#                   title = "Sobre",
#                   id = 'about_tabs',
#                   fluidRow(
#                   )
#             )
# )
# )

ui <- dashboardPage(skin = "red",
              header,
              sidebar,
              body
)
