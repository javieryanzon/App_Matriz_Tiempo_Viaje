leer_puntos <-
function(path) {
    # 1. Leer
    df <- readxl::read_excel(path, sheet = 1,
                             col_types = c("text", "numeric", "numeric"))
    
    # 2. Comprobar nombres de columnas
    esperadas <- c("nombre", "lat", "lon")
    if (!all(esperadas %in% names(df))) {
        stop("Faltan columnas. Debe haber exactamente: ", paste(esperadas, collapse = ", "))
    }
    # Reordenar en caso de que vengan desordenadas
    df <- df[esperadas]
    
    # 3. Comprobar tipos
    if (!is.character(df$nombre) ||
        !is.double(df$lat) ||
        !is.double(df$lon)) {
        stop("Tipos incorrectos: 'nombre' debe ser texto; 'lat' y 'lon' numéricos.")
    }
    
    # 4. Devolver tibble limpia
    tibble::as_tibble(df)
}
crear_combinaciones <-
function(puntos_input) {
    
    expand.grid(origen = 1:nrow(puntos_input), destino = 1:nrow(puntos_input)) %>%
        mutate(
            nombre_origen = puntos_input$nombre[origen],
            nombre_destino = puntos_input$nombre[destino],
            lat_o = puntos_input$lat[origen],
            lon_o = puntos_input$lon[origen],
            lat_d = puntos_input$lat[destino],
            lon_d = puntos_input$lon[destino]
        )
    
}
# funciones.R
calcular_ruta <- function(lon_o, lat_o, lon_d, lat_d,
                          api_key,
                          profile  = "driving-car",
                          retries  = 1,          # nº de reintentos si falla la request
                          backoff  = 1) {        # segundos de espera incremental
    for (att in 0:retries) {
        res <- tryCatch(
            openrouteservice::ors_directions(
                coordinates = list(c(lon_o, lat_o), c(lon_d, lat_d)),
                profile     = profile,
                output      = "parsed",
                api_key     = api_key   # <-- clave va aquí
            ),
            error = function(e) e
        )
        
        # Éxito
        if (!inherits(res, "error")) {
            sumry <- tryCatch(res$features[[1]]$properties$summary, error = function(e) NULL)
            if (!is.null(sumry)) {
                return(list(
                    distancia_km = as.numeric(sumry$distance) / 1000,
                    duracion_min = as.numeric(sumry$duration) / 60
                ))
            } else {
                return(list(distancia_km = NA_real_, duracion_min = NA_real_))
            }
        }
        
        # Error: decide si reintenta
        msg <- conditionMessage(res)
        if (att < retries && grepl("\\b(429|5\\d\\d)\\b", msg)) {
            Sys.sleep(backoff * (att + 1))  # backoff simple
            next
        }
        if (grepl("\\[403\\]", msg)) {
            message("ORS 403 (sin permiso o key incorrecta) para directions: ", msg)
        } else {
            message(sprintf(
                "Error entre (%.6f,%.6f) y (%.6f,%.6f): %s",
                lon_o, lat_o, lon_d, lat_d, msg
            ))
        }
        return(list(distancia_km = NA_real_, duracion_min = NA_real_))
    }
}

