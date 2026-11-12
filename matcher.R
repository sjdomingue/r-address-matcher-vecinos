# ==============================================================================
# ADDRESS MATCHER - NEAREST NEIGHBOR SEARCH (Synthetic Data)
# Portfolio version - Safe for public sharing
# ==============================================================================

library(DBI)
library(RSQLite)
library(readr)
library(dplyr)

# ========= CONFIGURATION ==========
# Generic path (Change this to your local working directory)
ruta_descargas <- tempdir() 
db_name        <- "synthetic_addresses.sqlite"
nombre_tabla   <- "ADDRESS_DATA"
ruta_bd        <- file.path(ruta_descargas, db_name)

GUARDAR_CADA_N_PRODUCTOS <- 10
archivo_backup <- file.path(ruta_descargas, "backup_progreso_vecinos.csv")

# Weights for address similarity
PESO_NUMERO_PRINCIPAL <- 0.50
PESO_PARIDAD          <- 0.25
PESO_TIPO_VIA         <- 0.15
PESO_PALABRAS         <- 0.10

PALABRAS_EXCLUIDAS <- c("SAN", "LA", "LOS", "EL", "BARRIO", "SECTOR", "KM", "VIA")

# ========= GENERATE SYNTHETIC DATABASE ==========
generar_bd_sintetica <- function() {
  if (file.exists(ruta_bd)) file.remove(ruta_bd)
  con <- dbConnect(RSQLite::SQLite(), ruta_bd)
  
  # Invented IDs and addresses
  df_sintetico <- data.frame(
    PRODUCT_ID = as.character(10000:10020),
    DPTO = rep("DEPTO_PRUEBA", 21),
    MUNICIPIO = rep("MUNICIPIO_DEMO", 21),
    LOCALIDAD = c(rep("LOCALIDAD_A", 10), rep("LOCALIDAD_B", 11)),
    DIRECCION = c(
      "CALLE 10 # 5-20", "CL 10 5 22", "CALLE 10 5 24", "CARRERA 5 # 10-10",
      "CR 5 10 12", "AVENIDA 1 # 2-30", "AV 1 2 32", "DIAGONAL 4 # 5-10",
      "DG 4 5 12", "CALLE 10 # 5-26", "TRANSVERSAL 2 # 3-40", "TV 2 3 42",
      "CALLE 20 # 8-15", "CL 20 8 17", "CARRERA 8 # 20-20", "CR 8 20 22",
      "AVENIDA 3 # 4-50", "AV 3 4 52", "DIAGONAL 6 # 7-30", "DG 6 7 32",
      "CALLE 20 # 8-19"
    ),
    PUERTA = c("20", "22", "24", "10", "12", "30", "32", "10", "12", "26",
               "40", "42", "15", "17", "20", "22", "50", "52", "30", "32", "19"),
    SUPER_EJE = rep(NA_character_, 21),
    SESUCICL = rep("1234", 21),
    ROUTE_ID = paste0("R-", 1:21),
    stringsAsFactors = FALSE
  )
  
  dbWriteTable(con, nombre_tabla, df_sintetico, overwrite = TRUE)
  dbDisconnect(con)
  cat("Synthetic database created at:", ruta_bd, "\n")
}

# ========= BASE FUNCTIONS ==========
safe_normalize_str <- function(x) {
  if (is.na(x) || is.null(x)) return(NA_character_)
  x <- trimws(as.character(x))
  if (!nzchar(x)) return(NA_character_)
  tryCatch(iconv(enc2utf8(x), from = "UTF-8", to = "UTF-8", sub = " "),
           error = function(e) NA_character_)
}

extract_direccion_components <- function(direccion) {
  txt <- safe_normalize_str(direccion)
  if (is.na(txt)) {
    return(list(numero_principal = NA_real_, paridad = NA_character_,
                tipo_via = "", todos_numeros = numeric(0),
                palabras = character(0), texto_completo = NA_character_))
  }
  txt     <- toupper(txt)
  nums_raw <- regmatches(txt, gregexpr("[0-9]+", txt))[[1]]
  numeros  <- if (length(nums_raw) > 0) as.numeric(nums_raw) else numeric(0)
  numero_principal <- if (length(numeros) > 0) numeros[1L] else NA_real_
  paridad <- if (!is.na(numero_principal))
    if (numero_principal %% 2 == 0) "PAR" else "IMPAR"
  else NA_character_
  
  tipo_via <- dplyr::case_when(
    grepl("\\b(CL|CALLE)\\b",       txt) ~ "CALLE",
    grepl("\\b(CR|CARRERA)\\b",     txt) ~ "CARRERA",
    grepl("\\b(AV|AVENIDA)\\b",     txt) ~ "AVENIDA",
    grepl("\\b(DG|DIAGONAL)\\b",    txt) ~ "DIAGONAL",
    grepl("\\b(TV|TRANSVERSAL)\\b", txt) ~ "TRANSVERSAL",
    TRUE ~ ""
  )
  
  tokens   <- unlist(strsplit(gsub("[^A-Z0-9 ]", " ", txt), "\\s+"))
  palabras <- tokens[nchar(tokens) > 0L]
  
  list(numero_principal = numero_principal, paridad = paridad,
       tipo_via = tipo_via, todos_numeros = numeros,
       palabras = palabras, texto_completo = txt)
}

calcular_similitud_desde_componentes <- function(comp1, comp2, usar_paridad = TRUE) {
  sim <- 0
  if (!is.na(comp1$numero_principal) && !is.na(comp2$numero_principal)) {
    if (comp1$numero_principal == comp2$numero_principal) {
      sim <- sim + PESO_NUMERO_PRINCIPAL
    } else {
      diff <- abs(comp1$numero_principal - comp2$numero_principal)
      if (diff < 10) sim <- sim + PESO_NUMERO_PRINCIPAL * (1 - diff / 100)
    }
  }
  if (usar_paridad && !is.na(comp1$paridad) && !is.na(comp2$paridad))
    if (comp1$paridad == comp2$paridad) sim <- sim + PESO_PARIDAD
  if (nzchar(comp1$tipo_via) && comp1$tipo_via == comp2$tipo_via)
    sim <- sim + PESO_TIPO_VIA
  
  p1 <- setdiff(comp1$palabras, PALABRAS_EXCLUIDAS)
  p2 <- setdiff(comp2$palabras, PALABRAS_EXCLUIDAS)
  if (length(p1) > 0L && length(p2) > 0L) {
    prop <- length(intersect(p1, p2)) / max(length(p1), length(p2))
    sim  <- sim + prop * PESO_PALABRAS
  }
  round(sim, 3)
}

calcular_similitud_direccion <- function(dir1, dir2, usar_paridad = TRUE) {
  d1 <- safe_normalize_str(dir1); d2 <- safe_normalize_str(dir2)
  if (is.na(d1) || is.na(d2) || !nzchar(d1) || !nzchar(d2)) return(0)
  if (toupper(d1) == toupper(d2)) return(1.0)
  calcular_similitud_desde_componentes(
    extract_direccion_components(d1),
    extract_direccion_components(d2),
    usar_paridad
  )
}

obtener_paridad_puerta <- function(puerta) {
  if (!is.null(puerta) && !is.na(puerta) && nzchar(as.character(puerta))) {
    num <- suppressWarnings(as.numeric(as.character(puerta)))
    if (!is.na(num) && num > 0)
      return(if (num %% 2 == 0) "PAR" else "IMPAR")
  }
  NA_character_
}

# ========= MAIN FUNCTION ==========

enrutar_vecino_mas_cercano <- function(con, tabla, lista_ids, n_vecinos = 3, umbral_similitud = 0.40) {
  lista_ids <- as.character(lista_ids)
  total <- length(lista_ids)
  resultados_lista <- vector("list", total)
  
  for (i in seq_along(lista_ids)) {
    pid <- lista_ids[i]
    cat(sprintf("\n[%d/%d] PROCESSING ID: %s\n", i, total, pid))
    
    producto <- tryCatch(
      dbGetQuery(con, sprintf("SELECT * FROM %s WHERE PRODUCT_ID = ?", tabla), params = list(pid)),
      error = function(e) data.frame()
    )
    if (nrow(producto) == 0L) next
    p <- producto[1L, ]
    
    comp_ref <- extract_direccion_components(p$DIRECCION)
    paridad_ref <- obtener_paridad_puerta(p$PUERTA)
    
    candidatos <- tryCatch(
      dbGetQuery(con, sprintf("SELECT * FROM %s WHERE PRODUCT_ID != ?", tabla), params = list(pid)),
      error = function(e) data.frame()
    )
    if (nrow(candidatos) == 0L) next
    
    # Calculate similarity vectorized
    candidatos$similitud <- vapply(candidatos$DIRECCION, function(d) {
      calcular_similitud_desde_componentes(comp_ref, extract_direccion_components(d), TRUE)
    }, FUN.VALUE = numeric(1L), USE.NAMES = FALSE)
    
    candidatos$paridad_puerta <- vapply(candidatos$PUERTA, obtener_paridad_puerta, 
                                        FUN.VALUE = character(1L), USE.NAMES = FALSE)
    
    sobre_umbral <- candidatos[candidatos$similitud >= umbral_similitud, ]
    sobre_umbral <- sobre_umbral[order(-sobre_umbral$similitud), ]
    
    if (nrow(sobre_umbral) == 0L) next
    
    if (!is.na(paridad_ref)) {
      misma_paridad <- sobre_umbral[!is.na(sobre_umbral$paridad_puerta) & 
                                      sobre_umbral$paridad_puerta == paridad_ref, ]
      if (nrow(misma_paridad) > 0L) {
        top_n <- head(misma_paridad, n_vecinos)
      } else {
        top_n <- head(sobre_umbral, n_vecinos)
      }
    } else {
      top_n <- head(sobre_umbral, n_vecinos)
    }
    
    filas_producto <- lapply(seq_len(nrow(top_n)), function(j) {
      v <- top_n[j, ]
      data.frame(
        PRODUCT_ID = as.character(p$PRODUCT_ID),
        DIRECCION_REF = as.character(p$DIRECCION),
        PRODUCT_ID_VECINO = as.character(v$PRODUCT_ID),
        DIRECCION_VECINO = as.character(v$DIRECCION),
        SIMILITUD = as.numeric(v$similitud),
        POSICION_RANKING = as.numeric(j),
        stringsAsFactors = FALSE
      )
    })
    resultados_lista[[i]] <- bind_rows(filas_producto)
  }
  bind_rows(resultados_lista)
}

# ================= EXECUTION ======================

cat("Generating synthetic database...\n")
generar_bd_sintetica()

cat("Connecting to database...\n")
con <- dbConnect(RSQLite::SQLite(), ruta_bd)
on.exit(dbDisconnect(con), add = TRUE)

# Invented IDs to search
lista_ids_prueba <- as.character(c(10000, 10001, 10003, 10007, 10010))

resultados <- enrutar_vecino_mas_cercano(
  con = con, 
  tabla = nombre_tabla, 
  lista_ids = lista_ids_prueba, 
  n_vecinos = 2, 
  umbral_similitud = 0.40
)

if (nrow(resultados) > 0) {
  cat("\n=== RESUMEN ===\n")
  cat(sprintf("Productos procesados: %d\n", length(unique(resultados$PRODUCT_ID))))
  archivo_salida <- file.path(ruta_descargas, "Resultados_Vecinos.csv")
  write_csv(resultados, archivo_salida)
  cat(sprintf("Archivo guardado en: %s\n", archivo_salida))
  print(head(resultados))
} else {
  cat("No se encontraron resultados.\n")
}