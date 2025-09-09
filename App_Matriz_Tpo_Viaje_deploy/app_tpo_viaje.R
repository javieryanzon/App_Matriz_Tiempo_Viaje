# MOBILITY DATA SCIENCE ----
# MATRIZ TIEMPO DE VIAJE - APP ----
# Version 1

# APPLICATION DESCRIPTION ----
# - El usuario deberá subir unos puntos con latitud y longitud y con un nombre o identificador.
# - 1) En base a lo anterior se calculará una matriz de tiempo de viaje según el modo especificado (en gral coche, quizás a pie).
# - Se podrá descargar la matriz y verla en el plot.
# - Se verán los puntos en el mapa con su identificador.


# LIBRARIES ----
library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(fs)
library(purrr)

# Leer excel y tbl
library(tibble)
library(writexl)

# OSM y Leaflet
library(osmdata)
library(openrouteservice)
library(leaflet) 
library(leaflet.extras)

# Progreso en consola
library(progress)


source("00_scripts/funciones.R")  # carga leer_puntos() y calcular_ruta()

# UI ----

ui <- fluidPage(
            theme = shinytheme("darkly"),
            #themeSelector(),
    
    title = "Cálculo de tiempos de viaje",
    
    # 1.0 HEADER ----
    div(
        class = "container",
        id = "header",
        h1("Cálculo de tiempos de viaje", tags$small((tags$cite(title = "Javier Yanzón", "(@javieryanzon - ",
                                                                a(href = "https://x.com/javieryanzon" , "X", target = "_blank"),
                                                                " - ", 
                                                                a(href = "https://linkedin.com/in/javieryanzon" , "Likedin", target = "_blank"), 
                                                                " - ", 
                                                                a(href = "https://github.com/javieryanzon" , "Github", target = "_blank"),
                                                                  ")")))),
        p(
          "Proyecto Shiny:", 
          "obtiene la matriz de distancia y tiempos de viajes de los puntos indicados.",
          "Para ello es necesario crear una API key muy fácilmente en la siguiente url:",
          a(href = "https://account.heigit.org/login?redirect=https:%2F%2Fopenrouteservice.org%2Fdev%2F%23%2Fapi-docs",
            "openrouteservice API docs", target = "_blank")
          )
    ),
    
    
    # 2.0 APPLICATION UI ----
    
    div(
        class = "container",
        id = "application_ui",
        column(
            width = 4, 
            wellPanel(
                downloadButton("dl_template", "Descargar plantilla Excel"),
                hr(style="margin-top: 10px;
                            margin-bottom: 10px"),
                fileInput("xls", "Sube tu Excel de puntos", accept = ".xlsx"),
                passwordInput("api_key", "Introduce tu API Key", placeholder = "XXXXXXXXXXXX"),
                actionButton("go", "Calcular tiempos"),
 
                
                ),
                mainPanel(
                tableOutput("preview"),
                verbatimTextOutput("status")
    
            )
        ),
        
       # 3.0 MAPA
       column(
           width = 8,
           leafletOutput(outputId = "map", height = "600px")
       )
    ),
    
    # br(),
     hr(),
    # br(),
    # 4.0 MATRIZ PLOTLY
    
    div(
        class = "container",
        id = "matrices",
        column(
            width = 6,
            wellPanel(style="margin-top: 20px",
                div(h4("Heatmap Distancia (km)"),
                    plotOutput("heatmap_distancia", height = "420px"),
                    downloadButton("download_distancia", "Descargar matriz distancia (Excel)"),
                    ),
                    )
                ),
            column(
                width = 6,
                wellPanel(style="margin-top: 20px",
                        div(h4("Heatmap Duración (min)"),
                            plotOutput("heatmap_duracion", height = "420px"),
                            downloadButton("download_duracion", "Descargar matriz duración (Excel)")
                        )
            )
        )
        
    ),
    
    # SCATTER DISTANCIA vs DURACIÓN
    div(
        class = "container",
        id = "scatter",
        column(
            
            width = 12,
            wellPanel(style="margin-top: 20px",
            div(h4("Scatter Distancia vs Duración"),
                plotlyOutput("scatter_dt", height = "420px")
                    )
                )
            )
        )
    
)
    


# SERVER ----
server <- function(input, output, session) {
    # 20 puntos de ejemplo
    puntos_test <- tibble(
        nombre = c("PUNTO_1", "PUNTO_2", "PUNTO_3", "PUNTO_4", "PUNTO_5"),
        lat    = c(43.53784161704792, 43.527094266136274, 43.50714571734309, 43.48650783909523, 43.46606350968035),
        lon    = c(-5.675795382739842, -5.690707030886254, -5.726539090956012, -5.719692080136125, -5.726046707255108)
    )
    
    
    # Descarga de plantilla
    output$dl_template <- downloadHandler(
        filename = function() "template_puntos.xlsx",
        content  = function(file) writexl::write_xlsx(puntos_test, file)
    )
    
    # Reactivo: puntos subidos o por defecto (preserva orden tal cual vienen)
    puntos <- reactive({
        if (isTruthy(input$xls)) leer_puntos(input$xls$datapath) else puntos_test
    })
    
    # Preview y mapa
    output$preview <- renderTable({ head(puntos(), 10) })
    output$map <- renderLeaflet({
        df <- puntos()
        leaflet(df) %>%
            addTiles() %>%
            addCircleMarkers(~lon, ~lat, label = ~nombre, radius = 6, fillOpacity = 0.8) %>%
            fitBounds(min(df$lon), min(df$lat), max(df$lon), max(df$lat))
    })
    
    # ReactiveVals para resultados y matrices
    resultados_rv <- reactiveVal(NULL)
    mat_dist_rv   <- reactiveVal(NULL)
    mat_dur_rv    <- reactiveVal(NULL)
    
    # Cálculo de rutas y matrices
    observeEvent(input$go, {
        req(input$api_key)
        ors_api_key(input$api_key)  # fija la key para openrouteservice
        
        pts <- puntos()
        output$status <- renderText("Iniciando cálculo de rutas...")
        
        # Generar combinaciones origen-destino
        combos <- expand_grid(origen = pts, destino = pts) %>%
            transmute(
                nombre_o = origen$nombre, lon_o = origen$lon, lat_o = origen$lat,
                nombre_d = destino$nombre, lon_d = destino$lon, lat_d = destino$lat
            )
        
        # Índices no self
        idx_no_self <- which(combos$nombre_o != combos$nombre_d)
        
        # Barra de progreso en UI
        prog <- Progress$new(session, min = 0, max = length(idx_no_self))
        on.exit(prog$close(), add = TRUE)
        prog$set(message = "Calculando rutas", value = 0)
        
        # Resultados por combinación (self=0)
        combos$resultado <- vector("list", nrow(combos))
        zero_res <- list(distancia_km = 0, duracion_min = 0)
        combos$resultado[combos$nombre_o == combos$nombre_d] <- list(zero_res)
        
        # Calcular solo no-self
        for (i in seq_along(idx_no_self)) {
            rowi <- idx_no_self[i]
            combos$resultado[[rowi]] <- calcular_ruta(
                combos$lon_o[rowi], combos$lat_o[rowi],
                combos$lon_d[rowi], combos$lat_d[rowi]
            )
            prog$inc(1)
        }
        
        # Extraer resultados largos
        resultados <- combos %>%
            mutate(
                distancia_km = map_dbl(resultado, ~pluck(.x, "distancia_km", .default = NA_real_)),
                duracion_min = map_dbl(resultado, ~pluck(.x, "duracion_min",  .default = NA_real_))
            ) %>%
            select(nombre_o, nombre_d, distancia_km, duracion_min)
        
        # Guardar resultados
        resultados_rv(resultados)
        
        # Orden deseado = orden de aparición en 'puntos()'
        orden <- puntos()$nombre
        
        # Matrices (data.frame) en orden
        wide_dist <- resultados %>%
            pivot_wider(id_cols = nombre_o, names_from = nombre_d, values_from = distancia_km)
        mat_dist  <- as.data.frame(wide_dist)
        rownames(mat_dist) <- mat_dist$nombre_o
        mat_dist$nombre_o  <- NULL
        mat_dist  <- mat_dist[orden, orden, drop = FALSE]
        
        wide_dur  <- resultados %>%
            pivot_wider(id_cols = nombre_o, names_from = nombre_d, values_from = duracion_min)
        mat_dur   <- as.data.frame(wide_dur)
        rownames(mat_dur) <- mat_dur$nombre_o
        mat_dur$nombre_o  <- NULL
        mat_dur   <- mat_dur[orden, orden, drop = FALSE]
        
        mat_dist_rv(mat_dist)
        mat_dur_rv(mat_dur)
        
        output$status <- renderText("✅ Cálculo completado.")
    })
    
    # Heatmap DISTANCIA (ggplot)
    output$heatmap_distancia <- renderPlot({
        req(resultados_rv())
        res <- resultados_rv()
        orden <- puntos()$nombre
        mid <- median(res$distancia_km, na.rm = TRUE)
        
        long_dist <- res %>%
            mutate(
                nombre_o = factor(nombre_o, levels = orden),
                nombre_d = factor(nombre_d, levels = orden),
                label_txt = if_else(is.na(distancia_km), "", sprintf("%.2f", distancia_km)),
                txt_col = case_when(
                    is.na(distancia_km) ~ "#666666",
                    distancia_km >= mid ~ "#FFFFFF",
                    TRUE ~ "#000000"
                )
            )
        
        ggplot(long_dist, aes(x = nombre_d, y = nombre_o, fill = distancia_km)) +
            geom_tile(na.rm = FALSE) +
            geom_text(aes(label = label_txt, color = txt_col), size = 3, fontface = "bold", na.rm = FALSE) +
            scale_color_identity() +
            scale_x_discrete(limits = orden) +
            scale_y_discrete(limits = rev(orden)) +  # PUNTO_1 arriba
            scale_fill_gradient(low = "#f7fbff", high = "#08306b", na.value = "#f0f0f0") +
            coord_equal() +
            labs(x = "Destino", y = "Origen", fill = "km") +
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Heatmap DURACIÓN (ggplot)
    output$heatmap_duracion <- renderPlot({
        req(resultados_rv())
        res <- resultados_rv()
        orden <- puntos()$nombre
        mid <- median(res$duracion_min, na.rm = TRUE)
        
        long_dur <- res %>%
            mutate(
                nombre_o = factor(nombre_o, levels = orden),
                nombre_d = factor(nombre_d, levels = orden),
                label_txt = if_else(is.na(duracion_min), "", sprintf("%.2f", duracion_min)),
                txt_col = case_when(
                    is.na(duracion_min) ~ "#666666",
                    duracion_min >= mid ~ "#FFFFFF",
                    TRUE ~ "#000000"
                )
            )
        
        ggplot(long_dur, aes(x = nombre_d, y = nombre_o, fill = duracion_min)) +
            geom_tile(na.rm = FALSE) +
            geom_text(aes(label = label_txt, color = txt_col), size = 3, fontface = "bold", na.rm = FALSE) +
            scale_color_identity() +
            scale_x_discrete(limits = orden) +
            scale_y_discrete(limits = rev(orden)) +
            scale_fill_gradient(low = "#f7fbff", high = "#08306b", na.value = "#f0f0f0") +
            coord_equal() +
            labs(x = "Destino", y = "Origen", fill = "min") +
            theme_minimal(base_size = 12) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    
    # Scatter Distancia vs Duración (plotly)
    output$scatter_dt <- renderPlotly({
        req(resultados_rv())
        df <- resultados_rv() %>%
            filter(!is.na(distancia_km), !is.na(duracion_min), nombre_o != nombre_d)
        
        p <- plot_ly(
            df,
            x = ~distancia_km,
            y = ~duracion_min,
            type = "scatter",
            mode = "markers",
            text = ~paste0(nombre_o, " → ", nombre_d,
                           "<br>km: ", sprintf("%.2f", distancia_km),
                           "<br>min: ", sprintf("%.2f", duracion_min)),
            hovertemplate = "%{text}<extra></extra>",
            name = "Datos"
        )
        
        if (nrow(df) >= 2) {
            lm_mod <- lm(duracion_min ~ distancia_km, data = df)
            r2 <- summary(lm_mod)$r.squared
            x_vals <- seq(min(df$distancia_km), max(df$distancia_km), length.out = 100)
            y_vals <- predict(lm_mod, newdata = data.frame(distancia_km = x_vals))
            
            p <- p %>% add_lines(
                x = x_vals, y = y_vals,
                name = paste0("Ajuste (R²=", round(r2, 3), ")"),
                line = list(dash = "dash"),
                inherit = FALSE
            )
        }
        
        p %>% layout(
            xaxis = list(title = "Distancia (km)"),
            yaxis = list(title = "Duración (min)")
        )
    })
    
    # Descargas Excel
    output$download_distancia <- downloadHandler(
        filename = function() "matriz_distancia.xlsx",
        content = function(file) {
            req(mat_dist_rv())
            df_out <- as.data.frame(mat_dist_rv()) %>% rownames_to_column(var = "Origen")
            writexl::write_xlsx(list(Distancia = df_out), path = file)
        }
    )
    output$download_duracion <- downloadHandler(
        filename = function() "matriz_duracion.xlsx",
        content = function(file) {
            req(mat_dur_rv())
            df_out <- as.data.frame(mat_dur_rv()) %>% rownames_to_column(var = "Origen")
            writexl::write_xlsx(list(Duracion = df_out), path = file)
        }
    )
}
    
# RUN APP ----
shinyApp(ui, server)