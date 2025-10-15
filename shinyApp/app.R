library(shiny)
library(networkD3)
library(tidyverse)
library(bslib)
library(DT)
library(treemap)
library(leaflet)
library(htmltools) # Necesario para formatear los tooltips
library(scales)
library(forcats) # Necesario para fct_reorder()
library(showtext) 

showtext_auto()
font_add_google("Open Sans", "Open Sans")


df_od_mitre_raw <- read_csv('./data/OD_MITRE_LAT_LON.csv', locale = locale(decimal_mark = ","))
colnames(df_od_mitre_raw) <- c('Ramal', 'ID_Origen', 'Origen', 'ID_Destino', 'Destino', 'Pax','lon_origen','lat_origen','lon_destino','lat_destino', 'dist_retiro')

df_pax_pagos_raw <- read_csv('./data/PAX_PAGOS.csv')
colnames(df_pax_pagos_raw) <- c('Ramal', 'Origen', 'Pax')

df_od_mitre <- df_od_mitre_raw %>%
  mutate('Pax_new' = as.numeric(gsub("\\.", "", Pax))) %>% 
  filter(Pax != 0) %>% 
  mutate('ID_Destino' = ID_Destino - 1) %>%
  mutate('Estacion_O' = paste0(Origen,"_O")) %>%
  mutate('Estacion_D' = paste0(Destino,"_D"))

df_od_mitre_grp <- df_od_mitre %>% 
  group_by(Origen, dist_retiro) %>% 
  summarise('pax_totales' = sum(Pax))

df_pax_pagos_joined <- df_od_mitre_grp %>% 
  inner_join(df_pax_pagos_raw, join_by(Origen)) %>% 
  mutate('perc_checkout' = scales::percent(pax_totales / Pax, accuracy = 0.01))

lista_origenes_completa <-  df_od_mitre %>%
  distinct(Origen, .keep_all = TRUE) %>% 
  arrange(dist_retiro) %>%
  pull(Origen)
lista_destinos_completa <-  df_od_mitre %>%
  distinct(Destino, .keep_all = TRUE) %>% 
  arrange(dist_retiro) %>%
  pull(Destino)
lista_ramales <- unique(df_od_mitre$Ramal)

ui <- 
  page_navbar(
    tags$head(
      tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;700&display=swap');
      .dataTables_wrapper {
        font-family: 'Open Sans', sans-serif;
      }
      
      body {
        font-family: 'Open Sans', sans-serif !important;
      }

      thead th {
        border-bottom: 2px solid black !important;
      }
       .kpi-grid-container {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 20px; /* Espacio entre tarjetas */
       }
      #card_tabla_detalle {
        height: auto !important; /* Permite que la altura se ajuste al contenido */
      }
    
    .kpi-grid-container-dos {
      display: grid;
      grid-template-columns: repeat(2, 1fr);
      gap: 20px; /* Espacio entre tarjetas */
    }
    
    /* 2. Hace que la tarjeta (value_box) sea un contenedor flexible
       y ocupe toda la altura de su celda en la cuadrícula */
    .value-box {
      display: flex;
      flex-direction: column;
      height: 100%;
    }

    /* 3. Centra perfectamente el contenido (icono y texto)
       dentro del área principal de la tarjeta */
    .value-box .value-box-area {
      flex-grow: 1; /* Permite que el área crezca para llenar el espacio */
      display: flex;
      flex-direction: column;
      justify-content: center; /* Centra verticalmente */
      align-items: center; /* Centra horizontalmente */
    }
   .tab-content, .tab-pane {
      height: auto !important;
      min-height: unset !important;
      overflow: visible !important;
    }

    /* 2. Anulamos cualquier restricción de altura en la tarjeta
          y en su cuerpo interno. */
    .auto-height-card, .auto-height-card .card-body {
      height: auto !important;
      min-height: unset !important;
      overflow-y: visible !important;
    }
    dd {
    margin-bottom: 1em; /* Añade espacio debajo de cada definición */
    }
    "))
    ),
    title = "Análisis de pasajeros de los servicios eléctricos de la Línea Mitre - 2024",
    fluidRow(
      selectInput("filtro_ramales", "",
                  choices = c("Seleccione un ramal..." = "", lista_ramales),
                  selected = 'Retiro - Tigre',
                  multiple = FALSE,
                  width = '100%') 
    ),
    navset_card_tab(
      nav_panel("KPIs",
                div(class = "kpi-grid-container",
                    value_box(
                      title = "PP - Pax Pagos",
                      h3(textOutput("cant_pax_pagos")),
                      showcase = bsicons::bs_icon("ticket-perforated")
                    ),
                    value_box(
                      title = "PP - Estación Más Utilizada",
                      # h3(textOutput("estacion_mas_utilizada")),
                      uiOutput("estacion_mas_utilizada_ui"),
                      showcase = bsicons::bs_icon("geo-alt")
                    ),
                    value_box(
                      title = "PP - % sobre el total de la línea",
                      h3(textOutput('perc_total_linea')),
                      showcase = bsicons::bs_icon("pie-chart")
                    ),
                    value_box(
                      title = "OD - Viajes con Check-Out",
                      h3(textOutput("cant_pasajeros")),
                      showcase = bsicons::bs_icon("people")
                    ),
                    value_box(
                      title = "OD - Ruta más usual",
                      uiOutput("trayecto_popular_ui"),
                      showcase = bsicons::bs_icon("arrow-left-right")
                    ),
                    value_box(
                      title = "OD - Tasa de Check-Out",
                      h3(textOutput('perc_checkout')),
                      showcase = bsicons::bs_icon("check2-circle")
                    )
                ),
                fluidRow(
                  card(
                    card_header("Mapa de movimientos"),
                    leafletOutput("mapa_estaciones", height = "80vh")
                  )
                )  

                
      ),
      nav_panel("Pax Pagos",
                fluidRow(
                         card(
                           card_header("Pax Pagos por estación"),
                           plotOutput("treemap_origen")
                         )
                )
                ),
      nav_panel("Origen - Destino",
                  fluidRow(
                             column(6,
                                    selectInput("filtro_origen", "Origen",
                                                choices = NULL,
                                                multiple = TRUE,
                                                width = '100%')
                             ),
                             column(6,
                                    selectInput("filtro_destino", "Destino",
                                                choices = NULL,
                                                multiple = TRUE,
                                                width = '100%')
                             )
                          ),
                div(class = "kpi-grid-container-dos",
                    value_box(
                      title = "Cantidad de viajes",
                      h3(textOutput("cant_pasajeros_filtro")),
                      showcase = bsicons::bs_icon("ticket-perforated")
                    ),
                    value_box(
                      title = "OD - Viaje más usual",
                      uiOutput("trayecto_popular_ui_filtro"),
                      showcase = bsicons::bs_icon("arrow-left-right")
                    )
                ),
                fluidRow(
                card(
                      card_header("Pasajeros por origen y destino"),
                      sankeyNetworkOutput("sankeyPlot", height = "160vh")
                    )
                )
      ),
      nav_panel("OD vs Pax Pagos",
                  # column(12,
                         card(
                           card_header("% check-out según origen"),
                           plotOutput("checkout_barchart")
                         )
                # )
              ,
                card(
                  card_header("Pax pagos vs Pax con Check-out según estación de origen"),
                  id = "card_tabla_detalle", 
                  DTOutput("tabla_detalle"))
      ),
      nav_panel("Hallazgos",
                card(
                  class = "auto-height-card",
                  card_header("Flujo de pasajeros en Retiro - Tigre"),
                  fluidRow(
                    column(6,
                      plotOutput("plot_hallazgo_1")
                    ),
                    column(6,
                      uiOutput("texto_hallazgo_1")
                    )
                  )
                ),
                card(
                  class = "auto-height-card",
                  card_header("Flujo de pasajeros en Retiro - Suárez / Mitre"),
                  fluidRow(
                    column(6,
                           plotOutput("plot_hallazgo_2")
                    ),
                    column(6,
                           uiOutput("texto_hallazgo_2")
                    )
                  )
                ),
                card(
                  class = "auto-height-card",
                  card_header("Análisis de check-out"),
                  fluidRow(
                    column(6,
                           plotOutput("plot_hallazgo_3")
                    ),
                    column(6,
                           uiOutput("texto_hallazgo_3")
                    )
                  )
                )
                ),
      nav_panel("Glosario - Fuentes",
                tags$dl(
                  tags$dt(tags$strong("PP - Pax Pagos")),
                  tags$dd("Indica la cantidad de pasajeros y pasajeras que pagaron su boleto."),
                  tags$dt(tags$strong("PP - Estación más utilizada")),
                  tags$dd("Indica la suma de boletos vendidos en una estación junto con los check-out registrados en dicha estación. Permite identificar el flujo de pax por dicha estación."),
                  tags$dt(tags$strong("PP - % sobre el total de la línea")),
                  tags$dd("Indica que porcentaje representa la cantidad de pax del ramal sobre el total de la línea."),
                  tags$dt(tags$strong("OD - Viajes con Check-Out")),
                  tags$dd("Indica la cantidad de viajes con check-in y check out realizado."),
                  tags$dt(tags$strong("OD - Ruta más usual")),
                  tags$dd("Indica la combinación Origen - Destino más frecuente."),
                  tags$dt(tags$strong("Tasa de Check-out")),
                  tags$dd("Indica el cantidad de viajes con check-out registrado sobre la cantidad de boletos vendidos."),
                  tags$dt(tags$strong("Fuente: Informe estadístico 2024 - Red de Pasajeros - AMBA")),
                  tags$dd(tags$a(href = "https://www.argentina.gob.ar/sites/default/files/if-2025-36543746-apn-gfgfcnrt_-_informe_estadistico_amba_2024.pdf","https://www.argentina.gob.ar/sites/default/files/if-2025-36543746-apn-gfgfcnrt_-_informe_estadistico_amba_2024.pdf")),
                  tags$dt(tags$strong("Fuente: Boletos vendidos por estación: 1996 - 2025")),
                  tags$dd(
                    tags$a(href = "https://www.argentina.gob.ar/sites/default/files/agosto.zip", "https://www.argentina.gob.ar/sites/default/files/agosto.zip")),
                )
                
                )
    )
)

server <- function(input, output, session) {
  
  estaciones_validas <- reactive({
    estaciones <- df_od_mitre %>%
        filter(Ramal %in% input$filtro_ramales)
    estaciones
  })

  observe({
    lista_estaciones <- estaciones_validas()
    # Actualizamos el filtro de origen
    updateSelectInput(session, "filtro_origen",
                      choices = unique(lista_estaciones$Origen)
                      # , selected = intersect(input$filtro_origen, lista_estaciones)
                      )
    # Actualizamos el filtro de destino
    updateSelectInput(session, "filtro_destino",
                      choices = unique(lista_estaciones$Destino)
                      # , selected = intersect(input$filtro_destino, lista_estaciones)
                      )
  })

  # Datos filtrados reactivos
  df_filtrado <- reactive({
    df <- df_od_mitre %>% 
      filter(Ramal %in% input$filtro_ramales)
    if(!is.null(input$filtro_origen)){
      df <- df %>% filter(Origen %in% input$filtro_origen)
    }
    if(!is.null(input$filtro_destino)){
      df <- df %>% filter(Destino %in% input$filtro_destino)
    }
    df
  })
  
  df_filtrado_ramal <- reactive({
    df <- df_od_mitre %>% 
      filter(Ramal %in% input$filtro_ramales)
    df
  })
  
  df_filtrado_pax_pagos <- reactive({
    df <- df_pax_pagos_joined %>%
      filter(Ramal %in% input$filtro_ramales)
    df
  })

  ############################

  output$checkout_barchart <- renderPlot({
    
    # Hacemos que el gráfico sea reactivo al filtro de Corredor
    req(input$filtro_ramales) # Requerimos que se seleccione un corredor
    
    pax_filtrados <- df_filtrado_pax_pagos()
    
    # Preparamos los datos para el ordenamiento
    estaciones_ordenadas <- pax_filtrados %>%
      arrange(dist_retiro) %>%
      pull(Origen)
    
    pax_filtrados <- pax_filtrados %>%
      mutate(Origen = factor(Origen, levels = estaciones_ordenadas)) %>% 
      mutate(perc_checkout_num = gsub("%", "", perc_checkout)) %>% 
      mutate(perc_checkout_num = as.numeric(perc_checkout_num) / 100)

    ggplot(pax_filtrados, aes(x = Origen, y = perc_checkout_num, fill = perc_checkout_num)) +
      geom_col() +
      scale_fill_gradient(low = "lightblue", high = "navy", name = "Tasa de Checkout") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = NULL, # El título ya está en el header de la card
        x = NULL,
        y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none" # Opcional: ocultamos la leyenda de color si es redundante
      )
  })
  
  ############################
  
  output$perc_checkout <- renderText({
    pax_filtrados <- df_filtrado_pax_pagos()
    req(nrow(pax_filtrados) > 0)
    total <- sum(pax_filtrados$pax_totales) / sum(pax_filtrados$Pax)
    scales::percent(total, accuracy = 0.01)
  })
  
  output$tabla_detalle <- renderDT({
    datatable(df_filtrado_pax_pagos() %>% 
                arrange(dist_retiro) %>%
                select(c('Origen','Pax','pax_totales','perc_checkout')),
              colnames = c(
                'Estación', 
                'Pax Pagos', 
                'Pax con Check-out',
                '% Check-out vs Pagos'
              ),
              options = list(pageLength = 50, # Muestra 10 filas por página
                             searching = FALSE,
                             lengthChange = FALSE,
                             paging = FALSE,
                             info = FALSE,
                             columnDefs = list(list(className = 'dt-center', targets = '_all')),
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',
                             zeroRecords = "Tenés que elegir un ramal")
              ), 
              rownames = FALSE) %>% 
      formatRound(columns = c('Pax', 'pax_totales'), digits = 0, mark = '.')
  }
  )

  
  
  output$cant_pax_pagos <- renderText({
    df <- df_filtrado_pax_pagos()
    total <- sum(df$Pax)
    format(total, big.mark = ".")
  })
######
  output$estacion_mas_utilizada <- renderText({
    df_1 <- df_filtrado() %>%
      rename(Estacion = Destino) %>% 
      group_by(Estacion) %>% 
      summarise('Pax_Total' = sum(Pax))

    df_2 <- df_filtrado_pax_pagos() %>%
      rename(Estacion = Origen) %>%
      group_by(Estacion) %>% 
      summarise('Pax_Total' = sum(Pax))
    df_final <- rbind(df_1, df_2) %>% 
      group_by(Estacion) %>% 
      summarise('Pax_Total' = sum(Pax)) %>% 
      arrange(desc(Pax)) %>%
      slice(1)
    total <- sum(df_1$Pax_Total)
    format(total, big.mark = ".")
  })
#####
  
  
  output$cantidad_mas_utilizada <- renderText({
    df_1 <- df_filtrado() %>%
      rename(Estacion = Destino) %>% 
      group_by(Estacion) %>% 
      summarise('Pax_Total' = sum(Pax))
    df_2 <- df_filtrado_pax_pagos() %>%
      rename(Estacion = Origen) %>%
      group_by(Estacion) %>% 
      summarise('Pax_Total' = sum(Pax))
    df_final <- rbind(df_1, df_2) %>% 
      group_by(Estacion) %>% 
      summarise('Pax_Total' = sum(Pax_Total)) %>% 
      arrange(desc(Pax_Total)) %>%
      slice(1)
    total <- df_final$Pax_Total
    format(total, big.mark = ".")
  })
  output$estacion_mas_utilizada <- renderText({
  df_1 <- df_filtrado() %>%
    rename(Estacion = Destino) %>% 
    group_by(Estacion) %>% 
    summarise('Pax_Total' = sum(Pax))
  df_2 <- df_filtrado_pax_pagos() %>%
    rename(Estacion = Origen) %>%
    group_by(Estacion) %>% 
    summarise('Pax_Total' = sum(Pax))
  df_final <- rbind(df_1, df_2) %>% 
    group_by(Estacion) %>% 
    summarise('Pax_Total' = sum(Pax_Total)) %>% 
    arrange(desc(Pax_Total)) %>%
    slice(1)
  df_final$Estacion
  })
  
  output$estacion_mas_utilizada_ui <- renderUI({
    tagList(
      p(textOutput("cantidad_mas_utilizada", inline = TRUE)),
      h6(textOutput("estacion_mas_utilizada", inline = TRUE))
    )
  })
  
  
  
  output$perc_total_linea <- renderText({
    df_filtrado_pax_pagos <- df_filtrado_pax_pagos()
    total_mitre <- sum(df_pax_pagos_joined$Pax)
    total_linea <- sum(df_filtrado_pax_pagos$Pax)
    total <- total_linea / total_mitre
    scales::percent(total, accuracy = 0.01)
    # format(total, big.mark = ".")
  })
  #####
    
  output$cant_pasajeros <- renderText({
      links_filtrados <- df_filtrado_ramal()
      total <- sum(links_filtrados$Pax)
      format(total, big.mark = ".")
    })

    output$cantidad <- renderText({
      links_filtrados <- df_filtrado_ramal()
      links_filtrados <- links_filtrados %>%
        arrange(desc(Pax)) %>%
        slice(1)
      total <- links_filtrados$Pax
      format(total, big.mark = ".")
    })

    output$recorrido <- renderText({
      links_filtrados <- df_filtrado_ramal()
      links_filtrados <- links_filtrados %>%
        arrange(desc(Pax)) %>%
        slice(1)
      paste0(links_filtrados$Origen, ' - ', links_filtrados$Destino)
    })

    output$trayecto_popular_ui <- renderUI({
      tagList(
        p(textOutput("cantidad", inline = TRUE)),
        h6(textOutput("recorrido", inline = TRUE))
      )
    })
    #####
    
    output$cant_pasajeros_filtro <- renderText({
      links_filtrados <- df_filtrado()
      total <- sum(links_filtrados$Pax)
      format(total, big.mark = ".")
    })
    
    output$cantidad_filtro <- renderText({
      links_filtrados <- df_filtrado()
      links_filtrados <- links_filtrados %>%
        arrange(desc(Pax)) %>%
        slice(1)
      total <- links_filtrados$Pax
      format(total, big.mark = ".")
    })
    
    output$recorrido_filtro <- renderText({
      links_filtrados <- df_filtrado()
      links_filtrados <- links_filtrados %>%
        arrange(desc(Pax)) %>%
        slice(1)
      paste0(links_filtrados$Origen, ' - ', links_filtrados$Destino)
    })
    
    output$trayecto_popular_ui_filtro <- renderUI({
      tagList(
        p(textOutput("cantidad_filtro", inline = TRUE)),
        h6(textOutput("recorrido_filtro", inline = TRUE))
      )
    })
    output$sankeyPlot <- renderSankeyNetwork({
    links_filtrados <- df_filtrado()
    req(nrow(links_filtrados) > 0)
    outflows <- links_filtrados %>%
      group_by(estacion_etiqueta = Estacion_O) %>%
      summarise(out_pax = sum(Pax))
    inflows <- links_filtrados %>%
      group_by(estacion_etiqueta = Estacion_D) %>%
      summarise(in_pax = sum(Pax))
    pax_totals <- full_join(outflows, inflows, by = "estacion_etiqueta") %>%
      replace_na(list(out_pax = 0, in_pax = 0)) %>%
      mutate(total_pax = out_pax + in_pax) # El valor del nodo es la suma de todo lo que pasa por él
    nodos_activos <- data.frame(
      estacion_etiqueta = unique(c(links_filtrados$Estacion_O, links_filtrados$Estacion_D))
      ) %>%
      left_join(pax_totals, by = "estacion_etiqueta") %>%
      mutate(nombre_estacion = str_replace(estacion_etiqueta, "_O", "")) %>% 
      mutate(nombre_estacion = str_replace(nombre_estacion, "_D", "")) %>% 
      mutate(etiqueta_con_valor = paste0(nombre_estacion, " (",format(total_pax, big.mark="."), ")"))
    links_reindexados <- links_filtrados %>%
      mutate(
        ID_Origen_nuevo = match(Estacion_O, nodos_activos$estacion_etiqueta) - 1,
        ID_Destino_nuevo = match(Estacion_D, nodos_activos$estacion_etiqueta) - 1
      )

    sankeyNetwork(
      Links = links_reindexados,          # Usamos los links re-indexados
      Nodes = nodos_activos,              # Usamos los nodos dinámicos
      Source = "ID_Origen_nuevo",         # Nueva columna de ID de Origen
      Target = "ID_Destino_nuevo",        # Nueva columna de ID de Destino
      Value = "Pax", 
      NodeID = "etiqueta_con_valor",       # Columna con las etiquetas de los nodos
      fontSize = 14, nodeWidth = 30,
      sinksRight = TRUE # Cambiado a FALSE para un layout más natural
    )

  })
    
    output$treemap_origen <- renderPlot({
      
      # Requerimos que se seleccione un corredor para mostrar el gráfico
      pax_filtrados <- df_filtrado_pax_pagos() %>% 
        mutate(pax_label = paste0(sprintf("%.1f", Pax / 1000000), "M"))
      req(nrow(pax_filtrados) > 0)

        treemap(
          pax_filtrados,
          index = c("Origen", "pax_label"),       # Columna para las etiquetas (los rectángulos)
          vSize = "Pax",          # Columna para el tamaño de los rectángulos
          type = "index",         # Tipo de coloreado (cada Origen tendrá un color)
          title = "",             # Ocultamos el título principal, ya está en la card
          palette = "Blues",      # Usamos una paleta de colores azules
          fontsize.labels = c(14, 12),   # Tamaño de la fuente de las etiquetas
          align.labels = list(c("center", "center"), c("center", "bottom")), # Alineación
          border.col = "black",    # Color del borde de los rectángulos
          fontfamily.labels = "Open Sans"
        )
    })
    
    ##################
    
    output$mapa_estaciones <- renderLeaflet({
      
      df_1 <- df_filtrado() %>% 
        group_by(Destino,lon_destino, lat_destino) %>% 
        rename(Estacion = Destino, latitud = lat_destino, longitud = lon_destino) %>% 
        summarise('cant_checkout' = sum(Pax))
      
      df_2 <- df_filtrado_pax_pagos() %>%
        rename(Estacion = Origen) %>%
        group_by(Estacion) %>% 
        summarise('cant_checkin' = sum(Pax))

      df_mapa <- df_1 %>% 
        inner_join(df_2, join_by(Estacion)) %>% 
        mutate('total_movimientos' = cant_checkout + cant_checkin)
      
      # 1. Creamos la paleta de colores
      # Mapea los valores de 'total_movimientos' a un color de la escala "Blues"
      paleta_colores <- colorNumeric(
        palette = "Blues",
        domain = df_mapa$total_movimientos
      )
      
      # 2. Creamos el contenido del tooltip (etiqueta)
      # Usamos HTML para darle formato
      tooltip_content <- paste0(
        "<b>Estación: </b>", df_mapa$Estacion, "<br/>",
        "<b>Check-ins: </b>", paste0(sprintf("%.2f", df_mapa$cant_checkin / 1000000), "M"), "<br/>",
        "<b>Check-outs: </b>", paste0(sprintf("%.2f", df_mapa$cant_checkout / 1000000), "M"), "<br/>",
        "<b>Total Movimientos: </b>", paste0(sprintf("%.2f", df_mapa$total_movimientos / 1000000), "M")
      ) %>% lapply(HTML) # Convertimos el texto a HTML para que se renderice correctamente
      
      # 3. Construimos el mapa
      leaflet(data = df_mapa) %>%
        addProviderTiles("CartoDB.Positron") %>% # Un mapa base limpio
        setView(lng = -58.494526, lat = -34.542382, zoom = 12) %>% # Centramos el mapa
        
        # CAPA 1: Círculos
        addCircleMarkers(
          lng = ~longitud,
          lat = ~latitud,
          # El radio del círculo es proporcional a la raíz cuadrada del total
          radius = ~sqrt(total_movimientos) * 0.05, 
          # El color de relleno lo define la paleta
          fillColor = ~paleta_colores(total_movimientos), 
          fillOpacity = 0.8,
          stroke = FALSE # Quitamos el borde para un look más limpio
        ) %>%
        
        # CAPA 2: Marcadores con Tooltips
        addAwesomeMarkers(
          lng = ~longitud,
          lat = ~latitud,
          icon = awesomeIcons(
            icon = 'train',
            library = 'fa', # Font Awesome
            markerColor = 'cadetblue'
          ),
          # El 'label' es el tooltip que aparece al pasar el mouse por encima
          label = tooltip_content,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        
        # LEYENDA: Explica la escala de colores
        addLegend(
          position = "bottomright",
          pal = paleta_colores,
          values = ~total_movimientos,
          title = "Total de Movimientos",
          opacity = 1
        )
    })
    
    output$plot_hallazgo_1 <- renderPlot({
      df <- df_od_mitre %>% 
        filter(Ramal == 'Retiro - Tigre') %>% 
        arrange(desc(Pax)) %>% 
        slice_head(n = 10) %>% 
        mutate(
          # Etiqueta para el eje X
          ruta = paste(Origen, "-\n", Destino),
          ruta = str_replace_all(ruta, " \\(Tigre\\)", ""),
          # Etiqueta para encima de la barra
          pax_label = paste0(sprintf("%.2f", Pax / 1000000), "M"),
          # Convertimos 'ruta' a un factor ordenado para que el gráfico respete el orden
          ruta = factor(ruta, levels = ruta)
        )
      ggplot(df, aes(x = ruta, y = Pax, fill = Pax)) +
        # 1. Dibuja las barras
        geom_col() +
        
        # 2. Añade las etiquetas de texto encima de cada barra
        geom_text(
          aes(label = pax_label),
          vjust = -0.5, # Ajusta la posición vertical para que quede por encima
          color = "black",
          size = 3.5
        ) +
        
        # 3. Define la paleta de colores azules
        scale_fill_gradient(low = "#c6dbef", high = "#08519c") +
        
        # 4. Personaliza los ejes y títulos
        labs(
          title = "Top 10 rutas",
          x = NULL,
          y = NULL # Oculta el título del eje Y
        ) +
        # 5. Ajusta el tema del gráfico
        theme_minimal() +
        theme(
          # Oculta los textos y marcas del eje Y
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          # Rota el texto del eje X para mejor legibilidad
          # axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          # Oculta la leyenda de color, ya que es redundante
          legend.position = "none",
          # Centra el título
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          # Elimina las líneas de la cuadrícula del eje Y
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank()
        )
    })
    
    # 2. Renderizar el texto
    output$texto_hallazgo_1 <- renderUI({
      # Aquí creas el texto usando tagList, h4, p, etc.
      tags$ul(
        # Cada tags$li es un ítem de la lista
        tags$li("Los cuatros pares de origen y destino se repiten en la lista de las 10 rutas más populares en ambos sentidos siempre con mayor cantidad en sentido descendente que en el sentido ascendente."),
        tags$li("Los flujos entre las cuatro estaciones más usadas (Retiro, Belgrano C, San Isidro y Tigre) representan el 30% de los viajes con check-in y check-out registrados. Se debe efectivamente a que son las estaciones en las que más boletos se venden pero también a que son las que más controles tienen en las salidas."),
        tags$li("El 0.53% de los los viajes con check-in y check-out tienen el mismo origen y destino. No surge de los datos si se debe a errores de validación, devolución de pasajes o personas que no realizaron check-in y solo realizaron el check-out"),
      )
    })
    
    output$plot_hallazgo_2 <- renderPlot({
      df <- df_od_mitre %>% 
        filter(Ramal == 'Retiro - Suárez / Mitre') %>% 
        arrange(desc(Pax)) %>% 
        slice_head(n = 10) %>% 
        mutate(
          # Etiqueta para el eje X
          ruta = paste(Origen, "-\n", Destino),
          ruta = str_replace_all(ruta, " \\(Suárez / Mitre\\)", ""),
          # Etiqueta para encima de la barra
          pax_label = paste0(sprintf("%.2f", Pax / 1000000), "M"),
          # Convertimos 'ruta' a un factor ordenado para que el gráfico respete el orden
          ruta = factor(ruta, levels = ruta)
        )
      ggplot(df, aes(x = ruta, y = Pax, fill = Pax)) +
        # 1. Dibuja las barras
        geom_col() +
        
        # 2. Añade las etiquetas de texto encima de cada barra
        geom_text(
          aes(label = pax_label),
          vjust = -0.5, # Ajusta la posición vertical para que quede por encima
          color = "black",
          size = 3.5
        ) +
        
        # 3. Define la paleta de colores azules
        scale_fill_gradient(low = "#c6dbef", high = "#08519c") +
        
        # 4. Personaliza los ejes y títulos
        labs(
          title = "Top 10 rutas",
          x = NULL,
          y = NULL # Oculta el título del eje Y
        ) +
        # 5. Ajusta el tema del gráfico
        theme_minimal() +
        theme(
          # Oculta los textos y marcas del eje Y
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          # Rota el texto del eje X para mejor legibilidad
          # axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          # Oculta la leyenda de color, ya que es redundante
          legend.position = "none",
          # Centra el título
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          # Elimina las líneas de la cuadrícula del eje Y
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank()
        )
    })
    
    output$texto_hallazgo_2 <- renderUI({
      # Aquí creas el texto usando tagList, h4, p, etc.
      tags$ul(
        # Cada tags$li es un ítem de la lista
        tags$li("La ruta más usada para este ramal, al igual que en el ramal a Tigre, se la que une el barrio de Belgrano con la estación Retiro"),
        tags$li("Se observa mayor dispersión en las rutas que tienen la mayor cantidad de check-in y check-out. No aparece un grupo claro de pares de origen y destino que sean predominantes."),
        tags$li("Ninguna de las rutas principales incluye estaciones propias del ramal a Bartolomé Mitre.*100"),
        tags$li("El 0.78% de los los viajes con check-in y check-out tienen el mismo origen y destino. No surge de los datos si se debe a errores de validación, devolución de pasajes o personas que no realizaron check-in y solo realizaron el check-out"),
      )
    })
    
    output$plot_hallazgo_3 <- renderPlot({
      lista_cabeceras <- c("Retiro (Tigre)", "Tigre", "Retiro (Suárez / Mitre)", "J. L. Suárez", "Mitre")
      df <- df_pax_pagos_joined %>% 
        mutate(tipo_estacion = if_else(Origen %in% lista_cabeceras, "Cabecera", "Intermedia")) %>% 
        group_by(tipo_estacion) %>% 
        summarise('pax_checkout' = sum(pax_totales),'pax_totales' = sum(Pax)) %>% 
        mutate('perc_checkout' = pax_checkout / pax_totales) %>% 
        mutate('perc_checkout_label' = scales::percent(pax_checkout / pax_totales, accuracy = 0.01)) %>% 
        mutate(tipo_estacion = fct_reorder(tipo_estacion, perc_checkout, .desc = TRUE)) %>%
        arrange(desc(perc_checkout))
        
      ggplot(df, aes(x = tipo_estacion, y = perc_checkout, fill = perc_checkout)) +
        # 1. Dibuja las barras
        geom_col() +
        
        # 2. Añade las etiquetas de texto encima de cada barra
        geom_text(
          aes(label = perc_checkout_label),
          vjust = -0.5, # Ajusta la posición vertical para que quede por encima
          color = "black",
          size = 5,
          fontface = "bold"
        ) +
        
        # 3. Define la paleta de colores azules
        scale_fill_gradient(low = "#c6dbef", high = "#08519c") +
        
        # 4. Personaliza los ejes y títulos
        labs(
          title = "% de check-out por Tipo de Estación",
          x = NULL, # Oculta el título del eje X
          y = NULL  # Oculta el título del eje Y
        ) +
        
        # 5. Aplica el mismo tema limpio del gráfico anterior
        theme_minimal() +
        theme(
          # Oculta los textos y marcas del eje Y
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          # Pone el texto del eje X más grande y horizontal
          axis.text.x = element_text(size = 14),
          # Oculta la leyenda de color
          legend.position = "none",
          # Centra el título
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          # Elimina TODAS las líneas de la cuadrícula
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
  })
    
    output$texto_hallazgo_3 <- renderUI({
      # Aquí creas el texto usando tagList, h4, p, etc.
      tags$ul(
        # Cada tags$li es un ítem de la lista
        tags$li("En la línea Mitre se observa que alrededor del 70% de viajes iniciados no registran un checkout asociado. Algunos posibles motivos a considerar pueden ser"),
        tags$ol(
          tags$li("Son más estrictos los controles de ingresos que los de salidas de las estaciones"),
          tags$li("Algunas personas tiene claro que siempre se descuenta la tarifa máxima y que en caso de viajes que tengan esa tarifa no tiene sentido hacer el checkout."),
          tags$li("Para algunas personas la diferencia entre tarifas es poco representativa y consideran que no vale pena hacer el check-out para el cobro correcto"),
          tags$li("Algunas estaciones intermedias tienen tan solo un dispositivo validador de viajes en los que realizar el check-out y algunas personas eligen no perder el tiempo haciendo fila para hacerlo."),
          tags$li("Algunas personas desconocen que si no realizan el checkout se les cobra la tarifa máxima."),
        ),
        tags$li("Se observa una marcada diferencia en el porcentaje de check-out entre los viajes iniciados en las cabeceras y los viajes iniciados en estaciones intermedias. Algunas hipótesis sobre este punto:"),
        tags$ol(
          tags$li("Es más probable que una persona que subió en una intermedia vaya a una cabecera y que ahí haya más controles en la salida que lo fuercen al check-out"),
          tags$li("Las personas que suben en cabeceras tienen menos incentivos a realizar el checkout ya que para la mayoría de los posibles lugares de finalización les corresponde una tarifa superior a la mínima."),
        ),
        
      )
    })

}

shinyApp(ui, server)