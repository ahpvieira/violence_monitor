## ACERTAR FILTROS REATIVOS DO MAPA
      # Paleta de cores bagunça quando o usuario filtra
      # Erro nos filtros depois de sexo

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
library(reshape2)

# pacman::p_load(pdftables, stringr, tidyverse, janitor, maptools, rgdal,
#                rgeos, scales, sp, shiny, plotly, shinythemes, htmlwidgets,
#              markdown, dygraphs, shinydashboard, zoo, xts, rCharts, leaflet, shinyjs)

# devtools::install_github('rCharts', 'ramnathv')

## Server ---------------------------------

server <- shinyServer(function(input, output, session) {
      
      # Carrega dados
      mcz <- readOGR(dsn = "C:/Users/ahpvi/Documents/shiny/violence_monitor/data", 
                     layer = "shp", stringsAsFactors = FALSE)
      df <- readRDS("C:/Users/ahpvi/Documents/shiny/violence_monitor/data/tb-cvli-al-2017.rds")
      
      # Modifica dados
      df <- dplyr::mutate(df, sexo = ifelse(is.na(sexo), "Sem informação", sexo))
      df <- dplyr::mutate(df, faixa_idade = ifelse(faixa_idade == "Sem informações", "Sem informação", faixa_idade))
      df <- dplyr::mutate(df, tipo_morte = ifelse(tipo_morte == "Sem informações", "Sem informação", tipo_morte))
      df <- dplyr::mutate(df,
                          data_fato = na.locf(data_fato),
                          mes_fato = str_sub(data_fato, 4, 5),
                          data_fato = as.Date(data_fato, "%d/%m/%Y"),
                          mes_ano = as.yearmon(str_sub(data_fato, 1, 7)))
      
      tb_stats <- df %>% 
            filter(cidade == "Maceió") %>% 
            # dplyr::mutate(bairro = iconv(bairro, "latin1", "latin1")) %>% 
            dplyr::mutate(bairro = iconv(bairro, "latin1", "ASCII//TRANSLIT")) %>%
            group_by(bairro) %>% 
            dplyr::mutate(n_cvli_total = n()) %>% 
            ungroup %>% 
            dplyr::group_by(bairro, sexo) %>% 
            dplyr::mutate(n_cvli_sexo = n()) %>% 
            ungroup %>% 
            dplyr::group_by(bairro, faixa_idade) %>% 
            dplyr::mutate(n_cvli_idade = n()) %>% 
            ungroup %>% 
            dplyr::group_by(bairro, subjetividade_complementar) %>% 
            dplyr::mutate(n_cvli_crime = n()) %>% 
            ungroup %>% 
            dplyr::group_by(bairro, tipo_morte) %>% 
            dplyr::mutate(n_cvli_morte = n()) %>% 
            ungroup %>% 
            dplyr::select(bairro, sexo, faixa_idade, tipo_crime = subjetividade_complementar,
                          tipo_morte, n_cvli_total, n_cvli_sexo, n_cvli_idade, 
                          n_cvli_crime, n_cvli_morte) %>% 
            melt(id.vars = c("bairro", "sexo", "faixa_idade", "n_cvli_total",
                             "tipo_crime", "tipo_morte")) %>%
            # dplyr::mutate(total = ifelse(variable == "n_cvli_total", "Total", NA)) %>% 
            dplyr::select(-variable) %>%
            dplyr::rename(n = value) %>%
            melt(id.vars = c("bairro", "n")) %>%
            dplyr::select(bairro, value, n) %>% 
            dplyr::mutate(value = case_when(value %in% c("1", "2", "3", "4", "5", "6", "8", "11", 
                                "14", "17", "19", "20", "25", "30") ~ "Total",
                                TRUE ~ as.character(value)))
      
      # tb_stats %>% 
      #       filter(value == "Total") %>% 
      #       distinct(bairro, .keep_all = T) %>% 
      #       arrange(bairro)
      
      df_mapa_sexo <- reactive({
            
            if (input$filter_sexo == ""){
                  # return(mcz@data %>% dplyr::filter(value == "Total") %>% dplyr::filter(!duplicated(Bairro)))
                  return(
                        distinct(filter(tb_stats, value == "Total"),
                                 bairro, .keep_all = T)
                        )
                        }
            else if (!input$filter_sexo == ""){
                  filter(tb_stats, value == input$filter_sexo)
                  }
            
      })
      
      df_mapa_idade <- reactive({
            if (input$filter_idade == ""){
                  return(df_mapa_sexo())
            }
            else if (!input$filter_idade == ""){
                  # df_mapa_sexo() %>% dplyr::filter(value == input$filter_idade)
                  filter(df_mapa_sexo(), value == input$filter_idade)
                  }
            
      })
      
      df_mapa_morte <- reactive({
            if (input$filter_morte == ""){
                  return(df_mapa_idade())
            }
            else if (!input$filter_morte == ""){
                  dplyr::filter(df_mapa_idade(), value == input$filter_morte)}
            
      })
      
      df_mapa_final <- reactive({
            if (input$filter_crime == ""){
                  return(df_mapa_morte())
            }
            else if (!input$filter_crime == ""){
                  dplyr::filter(df_mapa_morte(), value == input$filter_crime)}
            
      })
      
      mcz@data <- mcz@data %>% 
            clean_names %>% 
            rename(pop_total = popula_a_a) %>%
            mutate(pop_total = as.numeric(pop_total)) %>% 
            # dplyr::mutate(bairro = iconv(bairro, "UTF-8", "latin1")) %>% 
            dplyr::mutate(bairro = iconv(bairro, "UTF-8", "ASCII//TRANSLIT")) %>% 
            # dplyr::mutate(pop_total = format(round(as.numeric(popula_a_a), 1), 
            #                                 nsmall = 0, big.mark = "\\.")) %>% 
            select(bairro, pop_total)
            
      
      # mcz@data <- mcz@data %>% 
      #       mutate(faixa_cvli = cut(n_cvli, 
      #                               breaks = c(quantile(n_cvli, probs = seq(0, 1, by = .2), na.rm = T)),
      #                               labels = c("1-2", "2-3", "3-5", "5-11", "11-30")))
      
      # Dados reativos
      
      # output$filter_sexo <- renderUI({
      #       selectizeInput("filter_sexo", "Sexo", choices = c("Todos", unique(df$sexo)), selected = "", width = "100%")
      #       })
      
      df_bairro <- reactive({
            
            if (input$filter_bairro == "")
                  return(df)
            
            else if (!input$filter_bairro == "") {
                  return(df %>% filter(bairro == input$filter_bairro))
            }
      })
      
      df_sexo <- reactive({
            if (input$filter_sexo == ""){
                  return(df_bairro())
            }
            else if (!input$filter_sexo == ""){
                  df_bairro() %>% filter(sexo == input$filter_sexo)}
            
      })
      
      df_idade <- reactive({
            if (input$filter_idade == ""){
                  return(df_sexo())
            }
            else if (!input$filter_idade == ""){
                  df_sexo() %>% filter(faixa_idade == input$filter_idade)}
            
      })
      
      df_morte <- reactive({
            if (input$filter_morte == ""){
                  return(df_idade())
            }
            else if (!input$filter_morte == ""){
                  df_idade() %>% filter(tipo_morte == input$filter_morte)}
            
      })
      
      df_reactive <- reactive({
            if (input$filter_crime == ""){
                  return(df_morte())
            }
            else if (!input$filter_crime == ""){
                  df_morte() %>% filter(subjetividade_complementar == input$filter_crime)}
            
      })
      
      observeEvent(input$reset_input, {
            reset("form")
      })
      
      # cbind(mcz@data$Bairro, coordinates(mcz))
      
      # Mapa
      output$mapa_mcz <- renderLeaflet({ 
            
            tb_legend <- mcz@data %>%
                  left_join(tb_stats) %>% 
                  filter(value == "Total") %>% 
                  # dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
                  distinct(bairro, .keep_all = T)
            
            # bins <- c(1, 2, 3, 5, 11, 30)
            # pall <- colorBin("YlOrRd", domain = mcz@data$n_cvli, bins = bins)
            pall <- colorBin("YlOrRd", domain = tb_legend$n, 
                             bins = c(0, 2, 3, 5, 11, 30))
            
            # labels <- sprintf(
            #       "<b>Bairro:</b> %s <br> 
            #                 <b>População:</b> %s <br>
            #                 <b>Ocorrências:</b> %g",
            #       pull(distinct(select(mcz@data, bairro))), 
            #       mcz@data$pop_total,
            #       mcz@data$n) %>% lapply(htmltools::HTML)      
            
            leaflet::leaflet(options = leafletOptions(minZoom = 12, maxZoom = 18)) %>% 
                  # leaflet::leaflet(data = mcz) %>% 
                  setView(lng = -35.74, lat = -9.62, zoom = 12) %>% 
                  addTiles(layerId = "darkmatter",
                           options = providerTileOptions(minZoom = 12, maxZoom = 15)) %>% 
                  addProviderTiles(provider = "CartoDB.DarkMatter", 
                                   group = "Dark",
                                   options = providerTileOptions(minZoom = 12, 
                                                                 maxZoom = 15)) %>% 
                  # addPolygons(data = mcz, 
                  #             color = "white", 
                  #             # color = "#444444", 
                  #             weight = 1, smoothFactor = 0.5,
                  #             opacity = 1.0, fillOpacity = 0.5, 
                  #             dashArray = "3", 
                  #             
                  #             # label = labels,
                  #             highlightOptions = highlightOptions(
                  #                   # color = "#666",
                  #                   color = "white",
                  #                   weight = 1,
                  #                   dashArray = "",
                  #                   fillOpacity = 0.6,
                  #                   bringToFront = TRUE),
                  #             # ~paste0("<b>Bairro: </b>", 
                  #             #                unique(Bairro), 
                  #             #                "<br>",
                  #             #                "<b>Ocorrências: </b>",
                  #             #                n),
                  #             popup = ~unique(bairro),
                  #             # fillColor = ~colorQuantile("YlOrRd", n_cvli)(n_cvli)) %>% 
                  #             fillColor = ~pall(n)
                  #             ) %>% 
                  leaflet::addLegend(
                        pal = pall, 
                        values = c("0-2", "2-3", "3-5", "5-11", "11-30"), 
                        opacity = 0.7,
                        title = "Número de <br> ocorrências (2017)",
                        position = "bottomright")
            
            # return(mapa)
            
      })
      
      observe({ 
            
            if (input$filter_sexo == "" & input$filter_idade == "" &
                input$filter_morte == "" & input$filter_crime == ""
            ){
                
                  tb_legend <- mcz@data %>%
                        left_join(tb_stats) %>% 
                        filter(value == "Total") %>% 
                        # dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
                        distinct(bairro, .keep_all = T)
                  
                  # bins <- c(1, 2, 3, 5, 11, 30)
                  # pall <- colorBin("YlOrRd", domain = mcz@data$n_cvli, bins = bins)
                  
                  mcz@data <- mcz@data %>%
                        # mutate(bairro = iconv(bairro, from = "UTF-8", to = "latin1")) %>%
                        dplyr::left_join(df_mapa_final()) %>%
                        mutate(n = ifelse(is.na(n), 0, n))
                        # distinct(bairro, .keep_all = T) %>% 
                        # filter(!is.na(value))
                        # arrange(bairro)
                        # dplyr::left_join(tb_stats)
                        # filter(str_detect(bairro, "Cruz"))
                        # dplyr::mutate(faixa_cvli = cut(n,
                        #                                breaks = c(quantile(n, probs = seq(0, 1, by = .2), na.rm = T)),
                        #                                labels = c("0-2", "2-3", "3-5", "5-11", "11-30"), include.lowest = T))

                  pall <- colorBin("YlOrRd", domain = mcz@data$n, 
                                   # bins = c(0, 2, 3, 5, 11, 30),
                                   bins = 5)
                  
                  labels <- sprintf(
                        "<b>Bairro:</b> %s <br>
                        <b>População:</b> %s <br>
                        <b>Ocorrências:</b> %g",
                        pull(distinct(select(mcz@data, bairro))),
                        # mcz@data$bairro,
                        mcz@data$pop_total,
                        mcz@data$n) %>% 
                        lapply(htmltools::HTML)
                  
                  leafletProxy("mapa_mcz") %>%
                        addTiles(layerId = "darkmatter",
                                 options = providerTileOptions(minZoom = 12, maxZoom = 15)) %>% 
                        addProviderTiles(provider = "CartoDB.DarkMatter", 
                                         group = "Dark",
                                         options = providerTileOptions(minZoom = 12, maxZoom = 15)) %>% 
                        # addTiles() %>% 
                        clearShapes() %>% 
                        # addPolygons(data = mcz, 
                        #             fillColor = ~pal(sum_suicides), 
                        #             fillOpacity = 0.7, 
                        #             color = "white", weight = 2) %>% 
                        addPolygons(data = mcz, 
                                    color = "white", 
                                    weight = 1, 
                                    smoothFactor = 0.5,
                                    opacity = 1.0, 
                                    fillOpacity = 0.5, 
                                    dashArray = "3", 
                                    label = labels,
                                    highlightOptions = highlightOptions(
                                          color = "white",
                                          weight = 1,
                                          dashArray = "",
                                          fillOpacity = 0.6,
                                          bringToFront = TRUE),
                                    # ~paste0("<b>Bairro: </b>", 
                                    #                unique(Bairro), 
                                    #                "<br>",
                                    #                "<b>Ocorrências: </b>",
                                    #                n),
                                    popup = ~bairro,
                                    # fillColor = ~colorQuantile("YlOrRd", n_cvli)(n_cvli)) %>% 
                                    fillColor = ~pall(n)
                                    # fillColor = ~colorBin("YlOrRd", domain = n, 
                                    #                       bins = c(1, 2, 3, 5, 11, 30))
                                    )
                  
            }
            
            if (!input$filter_sexo == "" | !input$filter_idade == "" |
                !input$filter_morte == "" | !input$filter_crime == ""
               ){
                  
                  tb_legend <- mcz@data %>%
                        left_join(tb_stats) %>% 
                        filter(value == "Total") %>% 
                        # dplyr::mutate(n = ifelse(is.na(n), 0, n)) %>% 
                        distinct(bairro, .keep_all = T)
                  
                  mcz@data <- mcz@data %>% 
                        # mutate(bairro = iconv(bairro, from = "UTF-8", to = "latin1")) %>% 
                        dplyr::inner_join(df_mapa_final()) %>%
                        # dplyr::inner_join(filter(tb_stats, value == "Paf")) %>%
                        dplyr::mutate(n = ifelse(is.na(n), 0, n))
                        # distinct(bairro, .keep_all = T) %>% 
                        # filter(!is.na(value))
                        # mutate(faixa_cvli = cut(n,
                        #                         breaks = c(quantile(n, probs = seq(0, 1, by = .2), na.rm = T)),
                        #                         labels = c("1-2", "2-3", "3-5", "5-11", "11-30")))
            
                  # bins <- c(1, 2, 3, 5, 11, 30)
                  # pall <- colorBin("YlOrRd", domain = mcz@data$n_cvli, bins = bins)
                  # pall <- colorBin("YlOrRd", domain = tb_legend$n, 
                  #                  # bins = c(0, 2, 3, 5, 11, 30),
                  #                  bins = 5)
                  
                  pall <- colorBin("YlOrRd", domain = mcz@data$n, 
                                   # bins = c(0, 2, 3, 5, 11, 30),
                                   bins = 5)
                  
                  labels <- sprintf(
                        "<b>Bairro:</b> %s <br>
                        <b>População:</b> %s <br>
                        <b>Ocorrências:</b> %g",
                        pull(distinct(select(mcz@data, bairro))),
                        pull(select(distinct(mcz@data, bairro, .keep_all = T), pop_total)),
                        pull(select(distinct(mcz@data, bairro, .keep_all = T), n))) %>% 
                        lapply(htmltools::HTML)
                  
                  leafletProxy("mapa_mcz") %>%
                        addTiles(layerId = "darkmatter",
                                 options = providerTileOptions(minZoom = 12, maxZoom = 15)) %>% 
                        addProviderTiles(provider = "CartoDB.DarkMatter", 
                                         group = "Dark",
                                         options = providerTileOptions(minZoom = 12, maxZoom = 15)) %>% 
                        clearShapes() %>%
                        # addPolygons(data = mcz, 
                        #             fillColor = ~pal(sum_suicides), 
                        #             fillOpacity = 0.7, 
                        #             color = "white", weight = 2) %>% 
                        addPolygons(data = mcz, 
                                    color = "white", 
                                    weight = 1, smoothFactor = 0.5,
                                    opacity = 1.0, fillOpacity = 0.5, 
                                    dashArray = "3", 
                                    label = labels,
                                    highlightOptions = highlightOptions(
                                          color = "white",
                                          weight = 1,
                                          dashArray = "",
                                          fillOpacity = 0.6,
                                          bringToFront = TRUE),
                                    popup = ~unique(bairro),
                                    fillColor = ~pall(n)
                                    # fillColor = ~colorQuantile("YlOrRd", n_cvli)(n_cvli)
                                    # fillColor = ~colorBin("YlOrRd", domain = n, 
                                    #          bins = c(0, 2, 3, 5, 11, 30))
                                    # ) %>%
                                    # fillColor = ~pall(n)
                  )
                        
            }
            })
      
      # getColor <- function(mcz) {
      #   sapply(mcz@data$faixa_cvli, function(faixa_cvli) {
      #     if(faixa_cvli <= 4) {
      #       "#E31A1C"
      #     } else if(faixa_cvli <= 5) {
      #       "#808080"
      #     } else if(faixa_cvli <= 5) {
      #       "#FD8D3C"
      #     } else if(faixa_cvli <= 5) {
      #       "#FECC5"
      #     } else {
      #       "red"
      #     } })
      # }
      # 
      icons <- awesomeIcons(
            icon = "ios-close-circle",
            iconColor = "white",
            library = "ion", 
            markerColor = "black",
            spin = FALSE
      )
      
      # icon.ion <- makeAwesomeIcon(icon = 'ios-close', markerColor = 'grey40', prefix='ion')
      
      observeEvent(input$filter_bairro, {
            coord_bairro <- data.frame(cbind(mcz@data$bairro, coordinates(mcz)))
            coord_bairro <- coord_bairro %>% 
                  tbl_df %>% 
                  dplyr::rename(bairro = X1, lng = X2, lat = X3) %>% 
                  filter(!is.na(bairro)) %>% 
                  dplyr::mutate(lng = as.numeric(as.character(lng)), 
                                lat = as.numeric(as.character(lat)),
                                bairro = str_trim(bairro, "both"),
                                bairro = ifelse(bairro == "Centro Maceió", "Centro", bairro)) %>% 
                  dplyr::filter(bairro == input$filter_bairro)
            
            if(nrow(coord_bairro) == 0){
                  leafletProxy("mapa_mcz")
            } else {
                  leafletProxy(mapId = "mapa_mcz") %>%
                        clearMarkers() %>%
                        setView(lng = coord_bairro$lng,
                                lat = coord_bairro$lat,
                                zoom = 15) %>% 
                        # addTiles() %>%
                        # addMarkers(lng = coord_bairro$lng, lat = coord_bairro$lat, popup = as.character(coord_bairro$bairro), label = as.character(coord_bairro$bairro))
                        addAwesomeMarkers(lng = coord_bairro$lng, lat = coord_bairro$lat, icon = icons,
                                          popup = as.character(coord_bairro$bairro), label = as.character(coord_bairro$bairro))
                  # addTiles(options = providerTileOptions(maxZoom = 15))
            }
      })
      
      output$plot_serie <- renderDygraph({
            
            # if (is.null(df_reactive())) return(dygraph$new())
            
            shiny::validate(need(nrow(df_reactive()) > 0, message = "Seu filtro não retornou resultados."))
            
            df_reactive() %>%
                  # df %>% 
                  # filter(bairro == "Benedito Bentes") %>% 
                  # filter(sexo == "Feminino") %>% 
                  filter(!mes_fato == "rr", !is.na(mes_fato), mes_fato > 0) %>%
                  group_by(data_fato) %>%
                  dplyr::summarize(n = n()) %>%
                  dplyr::rename(Quantidade = n,
                                Data = data_fato) %>%
                  xts(., order.by = .$Data, frequency = .$Quantidade) %>%
                  dygraph(., main = "", ylab = "Número de ocorrências") %>% 
                  dyShading(from = 2.64, 
                            to = 8.46, axis = "y") %>% 
                  # dyShading(from = "2017-1-1", to = "2017-9-1", color = "grey20") %>% 
                  dyOptions(drawPoints = TRUE, pointSize = 4, colors = "grey40") %>% 
                  # dySeries(color = "grey40") %>% 
                  dyRangeSelector(.)
})

output$plot_hora <- renderChart({
      
      # if (is.null(df_reactive())) return(rCharts$new())
  
      shiny::validate(need(nrow(df_reactive()) > 0, message = "Seu filtro não retornou resultados."))
  
      tb_hora <- df_reactive() %>% 
            mutate(hora = as.numeric(hora)) %>%
            group_by(hora) %>% 
            dplyr::summarize(n = n()) %>% 
            arrange(hora) %>% 
            filter(!is.na(hora)) %>% 
            dplyr::rename(Quantidade = n,
                          `Hora` = hora)
      
      chart_width <- 0.95*as.numeric(htmlwidgets::JS("window.innerWidth"))
      
      plot <- nPlot(
            Quantidade ~ Hora,
            # group = "tx_sigla_uf",
            data = tb_hora,
            type = "lineChart",
            dom = "plot_hora"
            # width = session$clientData[["output_plot1_width"]]
            # width = session$clientData[["output_plot1_width"]]
      )
      
      # plot$params$width <- 600
      plot$params$height <- 400
      
      # plot <- xPlot(n ~ hora, data = tb_hora, type = "line-dotted")
      # # plot$print("chart2")
      # plot$print("chart2")
      # plot
      
      plot$yAxis(axisLabel = "Número de ocorrências", width = 50)
      plot$xAxis(axisLabel = "Hora", width = 70)
      plot$chart(color = "#! function(d){ return 'black'} !#")
      plot
      
})
})