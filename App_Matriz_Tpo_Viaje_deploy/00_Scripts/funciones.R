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
calcular_ruta <-
function(lon_o, lat_o, lon_d, lat_d) {
    tryCatch({
        ruta <- ors_directions(
            coordinates = list(c(lon_o, lat_o), c(lon_d, lat_d)),
            profile = "driving-car",
            output = "parsed"
        )
        resumen <- ruta$features[[1]]$properties$summary
        
        list(
            distancia_km = resumen$distance / 1000,
            duracion_min = resumen$duration / 60
        )
    }, error = function(e) {
        message("Error entre (", lon_o, ",", lat_o, ") y (", lon_d, ",", lat_d, "): ", e$message)
        list(
            distancia_km = NA_real_,
            duracion_min = NA_real_
        )
    })
}
