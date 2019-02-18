## Carrega pacotes ------------------------
library(pdftables)
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


# pacman::p_load(pdftables, stringr, tidyverse, janitor, maptools, rgdal,
#                rgeos, scales, sp, shiny, plotly, shinythemes, 
#                markdown, dygraphs, shinydashboard, zoo, xts, rCharts, leaflet, shinyjs)

## Carrega dados --------------------------
# setwd("H:/violencia")
# setwd("E:/violencia")
# setwd("D:/Users/B31099033/Documents/violencia")
# mcz <- readOGR(dsn = ".", layer = "shp", stringsAsFactors = FALSE)
# df <- readRDS("C:/Users/ahpvi/Documents/shiny/violencia/tb-cvli-al-2017.rds")
load("C:/Users/ahpvi/Documents/shiny/violence_monitor/data/labels.Rdata")

## Modifica dados -------------------------

# df <- dplyr::mutate(df, sexo = ifelse(is.na(sexo), "Sem informação", sexo))
# df <- dplyr::mutate(df, faixa_idade = ifelse(faixa_idade == "Sem informações", "Sem informação", faixa_idade))
# df <- dplyr::mutate(df, tipo_morte = ifelse(tipo_morte == "Sem informações", "Sem informação", tipo_morte))
# df <- dplyr::mutate(df,
#                     data_fato = na.locf(data_fato),
#                     mes_fato = str_sub(data_fato, 4, 5),
#                     data_fato = as.Date(data_fato, "%d/%m/%Y"),
#                     mes_ano = as.yearmon(str_sub(data_fato, 1, 7)))
# 
# tb_bairro <- df %>%
#       filter(cidade == "Maceió") %>%
#       mutate(bairro = iconv(bairro, "latin1", "latin1")) %>%
#       group_by(bairro) %>%
#       dplyr::summarize(n_cvli = n())
# 
# bairros <- pull(distinct(select(tb_bairro, bairro)))
# sexo <- pull(distinct(select(df, sexo)))
# faixa_idade <- pull(distinct(select(df, faixa_idade)))
# tipo_morte <- pull(distinct(select(df, tipo_morte)))
# tipo_crime <- pull(distinct(select(df, subjetividade_complementar)))
# tipo_crime[is.na(tipo_crime)] <- "Sem informação"
# save(bairros, sexo, faixa_idade, tipo_morte, tipo_crime, 
#      file = "C:/Users/ahpvi/Documents/shiny/violencia/labels.Rdata")
# 
# # quantile(tb_bairro$n_cvli, probs = seq(0, 1, by = .20))
# 
# mcz@data <- mutate(mcz@data, Bairro = iconv(Bairro, from = "UTF-8", to = "latin1"))
# mcz@data <- mcz@data %>%
#       # tabyl(Bairro)
#       left_join(tb_bairro, by = c("Bairro" = "bairro"))
# 
# mcz@data <- mcz@data %>%
#       mutate(faixa_cvli = cut(
#             n_cvli, breaks = c(quantile(n_cvli, probs = seq(0, 1, by = .2), na.rm = T)),
#             labels = c("1-2", "2-3", "3-5", "5-11", "11-30")))

includeMarkdown <- function (path) 
{
      html <- markdown::markdownToHTML(path, fragment.only = TRUE)
      # Encoding(html) <- "UTF-8"
      return(HTML(html))
}

## Shiny UI -------------------------------

header <- dashboardHeader(
      title = ""
)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
      includeCSS("C:/Users/ahpvi/Documents/shiny/violence_monitor/www/custom.css"),
      useShinyjs(),
      tags$head(tags$style(
            HTML('
                 label, button, select {
                 color: white;
                 }'))),
      theme = shinytheme("slate"),
      title = " ", #footer=source("intro_foot.R", local=TRUE)$value, 
      windowTitle = "Monitor do crime", collapsible = TRUE, fluid = FALSE,
      id = "page",
      
      tabPanel(
            title = "Home",
            id = "app_home",
            fluidRow(
                  column(width = 8, 
                         includeMarkdown("C:/Users/ahpvi/Documents/shiny/violence_monitor/intro_app.md"))
            ),
            tags$hr(color = "white"),
            div(
                  id = "form",
                  tagList(
                        fluidRow(
                              column(width = 3, 
                                     selectizeInput("filter_bairro", "Bairro", 
                                                    choices = c("", bairros), 
                                                    width = "100%", 
                                                    options = list(placeholder = "Selecione um bairro"))),
                              # column(width = 3, uiOutput("filter_sexo")),
                              column(width = 3, 
                                     selectizeInput("filter_sexo", "Sexo", 
                                                    choices = c("", sexo), 
                                                    width = "100%", 
                                                    options = list(placeholder = "Selecione um sexo"))),
                              column(width = 3, 
                                     selectizeInput("filter_idade", "Idade", 
                                                    choices = c("", faixa_idade), 
                                                    width = "100%", 
                                                    options = list(placeholder = "Selecione uma faixa etária"))),
                              column(width = 3, 
                                     selectizeInput("filter_morte", "Tipo de morte", 
                                                    choices = c("", tipo_morte), 
                                                    width = "100%", 
                                                    options = list(placeholder = "Selecione um tipo de morte")))
                              # column(width = 3, uiOutput("filter_idade")),
                              # column(width = 3, uiOutput("filter_morte"))
                              # HTML('<hr style="color: white;">')
                        ),
                        fluidRow(
                              # column(width = 3, uiOutput("filter_crime")),
                              column(width = 3, 
                                     selectizeInput("filter_crime", 
                                                    "Tipo de crime", 
                                                    choices = c("", tipo_crime), 
                                                    width = "100%", 
                                                    options = list(placeholder = "Selecione um tipo de crime"))),
                              column(width = 3, 
                                     radioButtons("filter_indicador", 
                                                  "Indicador (em construção)",
                                                  choices = c("Quantidade", "Taxa por 10 mil habitantes"), 
                                                  inline = T, width = "100%"))
                              # HTML('<hr style="color: white;">')
                        )
                        ),
            fluidRow(
                  # uiOutput('resetable_input'),
                  column(width = 3, 
                         actionButton("reset_input", "Limpar filtro", 
                                      width = "50%", 
                                      style = "color: #fff; background-color: #666; border-color: black")
                         )
            )),
            br(),
            leafletOutput("mapa_mcz", height = 600),
            br(),
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
                         includeMarkdown("C:/Users/ahpvi/Documents/shiny/violence_monitor/credit_app.md"))
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
