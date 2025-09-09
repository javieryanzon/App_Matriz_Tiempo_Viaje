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
#library(progress)


source("00_Scripts/funciones.R")  # carga leer_puntos() y calcular_ruta()

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
                    plotOutput("heatmap_distancia", height = "560px"),
                    downloadButton("download_distancia", "Descargar matriz distancia (Excel)"),
                    ),
                    )
                ),
            column(
                width = 6,
                wellPanel(style="margin-top: 20px",
                        div(h4("Heatmap Duración (min)"),
                            plotOutput("heatmap_duracion", height = "560px"),
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
        key <- isolate(input$api_key)
        
        # Muestra solo prefijo/sufijo para comprobar que es la esperada
        output$status <- renderText(sprintf("Usando API key %s…%s",
                                            substr(key, 1, 4), substr(key, nchar(key)-3, nchar(key))))
        
        pts <- puntos()
        if (nrow(pts) > 40) {
            showNotification("Por ahora sube hasta 40 puntos.", type = "warning", duration = 6)
            output$status <- renderText("⚠️ Demasiados puntos: máximo 40.")
            return(invisible(NULL))
        }
        
        output$status <- renderText("Iniciando cálculo…")
        
        withProgress(message = "Calculando", value = 0, {
            coords <- purrr::pmap(pts[, c("lon","lat")], c)
            
            # 1) Intento con MATRIX (una sola request)
            incProgress(0.2, detail = "ORS Matrix")
            res <- tryCatch(
                openrouteservice::ors_matrix(
                    locations = coords,
                    profile   = "driving-car",           # o "foot-walking"
                    metrics   = c("distance","duration"),
                    api_key   = key
                    # , timeout = 60
                ),
                error = identity
            )
            
            if (inherits(res, "error") && grepl("\\[403\\]", conditionMessage(res))) {
                # 2) Fallback OD→OD si el servidor responde 403 (key sin permiso o mal leída)
                showNotification("Tu key devolvió 403 en 'Matrix'. Uso fallback punto a punto (más lento).",
                                 type = "warning", duration = 8)
                
                n <- nrow(pts)
                mat_dist <- matrix(NA_real_, n, n, dimnames = list(pts$nombre, pts$nombre))
                mat_dur  <- matrix(NA_real_, n, n, dimnames = list(pts$nombre, pts$nombre))
                diag(mat_dist) <- 0; diag(mat_dur) <- 0
                
                total <- n*(n-1)
                done  <- 0
                incProgress(0.2, detail = "Calculando rutas OD")
                for (i in seq_len(n)) {
                    for (j in seq_len(n)) {
                        if (i == j) next
                        done <- done + 1
                        incProgress(0.6/total, detail = sprintf("%s → %s", pts$nombre[i], pts$nombre[j]))
                        
                        ans <- calcular_ruta(
                            pts$lon[i], pts$lat[i], pts$lon[j], pts$lat[j],
                            api_key = key   # <- asegúrate de que tu función lo acepte y lo pase a ors_directions()
                        )
                        mat_dist[i, j] <- ans$distancia_km
                        mat_dur[i, j]  <- ans$duracion_min
                    }
                }
                
                mat_dist_rv(mat_dist)
                mat_dur_rv(mat_dur)
                
                # largo para gráficos
                ij <- expand.grid(i = seq_len(n), j = seq_len(n))
                resultados_rv(tibble::tibble(
                    nombre_o     = pts$nombre[ij$i],
                    nombre_d     = pts$nombre[ij$j],
                    distancia_km = mat_dist[cbind(ij$i, ij$j)],
                    duracion_min = mat_dur[cbind(ij$i, ij$j)]
                ))
                
            } else if (inherits(res, "error")) {
                # Cualquier otro error distinto de 403
                msg <- paste("❌ Error ORS:", conditionMessage(res))
                showNotification(msg, type = "error", duration = 8)
                output$status <- renderText(msg)
                return(invisible(NULL))
            } else {
                # 3) OK con MATRIX → parseo
                incProgress(0.7, detail = "Procesando matrices")
                
                to_matrix <- function(x, n) {
                    if (is.null(x)) return(matrix(NA_real_, n, n))
                    if (is.list(x)) do.call(rbind, x) else as.matrix(x)
                }
                
                n <- nrow(pts)
                mat_dist <- to_matrix(res$distances, n) / 1000  # m -> km
                mat_dur  <- to_matrix(res$durations, n) / 60    # s -> min
                
                rownames(mat_dist) <- colnames(mat_dist) <- pts$nombre
                rownames(mat_dur)  <- colnames(mat_dur)  <- pts$nombre
                
                mat_dist_rv(mat_dist)
                mat_dur_rv(mat_dur)
                
                ij <- expand.grid(i = seq_len(n), j = seq_len(n))
                resultados_rv(tibble::tibble(
                    nombre_o     = pts$nombre[ij$i],
                    nombre_d     = pts$nombre[ij$j],
                    distancia_km = mat_dist[cbind(ij$i, ij$j)],
                    duracion_min = mat_dur[cbind(ij$i, ij$j)]
                ))
            }
            
            incProgress(1, detail = "Listo")
        })
        
        output$status <- renderText("✅ Cálculo completado.")
    })
    
    
    # Heatmap DISTANCIA (ggplot)
    # output$heatmap_distancia <- renderPlot({
    #     req(resultados_rv())
    #     res   <- resultados_rv()
    #     orden <- puntos()$nombre
    #     mid   <- median(res$distancia_km, na.rm = TRUE)
    #     
    #     long_dist <- res %>%
    #         mutate(
    #             nombre_o = factor(nombre_o, levels = orden),
    #             nombre_d = factor(nombre_d, levels = orden),
    #             label_txt = if_else(is.na(distancia_km), "", sprintf("%.2f", distancia_km)),
    #             txt_col   = case_when(
    #                 is.na(distancia_km) ~ "#9aa0a6",      # gris para NA
    #                 distancia_km >= mid ~ "#FFFFFF",      # fondo más oscuro → texto blanco
    #                 TRUE                ~ "#111111"       # fondo claro → texto negro
    #             )
    #         )
    #     
    #     ggplot(long_dist, aes(x = nombre_d, y = nombre_o, fill = distancia_km)) +
    #         geom_tile() +
    #         geom_text(aes(label = label_txt, color = txt_col),
    #                   size = 3, fontface = "bold", na.rm = FALSE) +
    #         scale_color_identity() +
    #         scale_x_discrete(limits = orden) +
    #         scale_y_discrete(limits = rev(orden)) +  # PUNTO_1 arriba
    #         # Paleta amigable con darkly (probá también option = "inferno" o "magma")
    #         scale_fill_viridis_c(option = "inferno", direction = 1,
    #                              na.value = "#2f3a44") +
    #         coord_equal() +
    #         labs(x = "Destino", y = "Origen", fill = "km") +
    #         theme_minimal(base_size = 12) +
    #         theme(
    #             plot.background   = element_rect(fill = "transparent", color = NA),
    #             panel.background  = element_rect(fill = "#2b2b2b", color = NA),
    #             panel.grid.major  = element_line(color = "#3d4852"),
    #             panel.grid.minor  = element_blank(),
    #             axis.text         = element_text(color = "#e0e0e0"),
    #             axis.title        = element_text(color = "#e0e0e0"),
    #             legend.text       = element_text(color = "#e0e0e0"),
    #             legend.title      = element_text(color = "#e0e0e0"),
    #             axis.text.x       = element_text(angle = 45, hjust = 1)
    #         )
    # })
    
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
    # output$heatmap_duracion <- renderPlot({
    #     req(resultados_rv())
    #     res   <- resultados_rv()
    #     orden <- puntos()$nombre
    #     mid   <- median(res$duracion_min, na.rm = TRUE)
    #     
    #     long_dur <- res %>%
    #         mutate(
    #             nombre_o = factor(nombre_o, levels = orden),
    #             nombre_d = factor(nombre_d, levels = orden),
    #             label_txt = if_else(is.na(duracion_min), "", sprintf("%.2f", duracion_min)),
    #             txt_col   = case_when(
    #                 is.na(duracion_min) ~ "#9aa0a6",
    #                 duracion_min >= mid ~ "#FFFFFF",
    #                 TRUE                ~ "#111111"
    #             )
    #         )
    #     
    #     ggplot(long_dur, aes(x = nombre_d, y = nombre_o, fill = duracion_min)) +
    #         geom_tile() +
    #         geom_text(aes(label = label_txt, color = txt_col),
    #                   size = 3, fontface = "bold", na.rm = FALSE) +
    #         scale_color_identity() +
    #         scale_x_discrete(limits = orden) +
    #         scale_y_discrete(limits = rev(orden)) +
    #         scale_fill_viridis_c(option = "inferno", direction = 1,
    #                              na.value = "#2f3a44") +
    #         coord_equal() +
    #         labs(x = "Destino", y = "Origen", fill = "min") +
    #         theme_minimal(base_size = 12) +
    #         theme(
    #             plot.background   = element_rect(fill = "transparent", color = NA),
    #             panel.background  = element_rect(fill = "#2b2b2b", color = NA),
    #             panel.grid.major  = element_line(color = "#3d4852"),
    #             panel.grid.minor  = element_blank(),
    #             axis.text         = element_text(color = "#e0e0e0"),
    #             axis.title        = element_text(color = "#e0e0e0"),
    #             legend.text       = element_text(color = "#e0e0e0"),
    #             legend.title      = element_text(color = "#e0e0e0"),
    #             axis.text.x       = element_text(angle = 45, hjust = 1)
    #         )
    # })
    
    
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