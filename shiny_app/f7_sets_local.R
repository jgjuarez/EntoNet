# Shared by individual capture and the offline preview. No database calls.
f7_sets_grid <- function(set, stage, bottles, times) {
  prefix <- paste("f7sets", set, stage, sep = "_")
  tags$div(style = "overflow-x:auto;", tags$table(class = "table table-bordered",
    tags$thead(tags$tr(tags$th("Lectura"), lapply(bottles, function(b) tags$th(
      paste0(if (stage == "pretratamiento") "" else if (set == "sinergista") "Sin" else "EtOH", toupper(b)))))),
    tags$tbody(
      tags$tr(tags$th("Inicio (HH:MM)"), lapply(bottles, function(b)
        tags$td(textInput(paste(prefix, b, "inicio", sep = "_"), NULL, placeholder = "08:30", width = "105px")))),
      lapply(times, function(t) tags$tr(tags$th(paste(t, "min")), lapply(bottles, function(b) tags$td(
        numericInput(paste(prefix, b, t, "vivos", sep = "_"), "Vivos", NA, min = 0, step = 1, width = "105px"),
        numericInput(paste(prefix, b, t, "incapacitados", sep = "_"), "Incapacitados", NA, min = 0, step = 1, width = "105px")
      ))))
    )
  ))
}

f7_sets_ui <- function(stages = c("pretratamiento", "bioensayo", "kdr_24h"), tab_id = "f7_sets_tab") {
  tagList(
    div(class = "alert alert-info", "Los dos sets se guardan juntos en la base de Sinergistas. Complete cada etapa con los valores observados."),
    do.call(tabsetPanel, c(list(id = tab_id), lapply(c("sinergista", "etanol"), function(set) {
      number <- if (set == "sinergista") "1" else "2"
      label <- if (set == "sinergista") "Sinergista" else "Etanol (EtOH)"
      tabPanel(label,
        if ("pretratamiento" %in% stages) tagList(h4(paste0("8.", number, " · Exposición previa: ", label)),
        p("Registre los mosquitos vivos e incapacitados después de 60 minutos de exposición previa."),
        f7_sets_grid(set, "pretratamiento", paste0("e", 1:5), 60),
        textAreaInput(paste0("f7sets_", set, "observaciones_pre"), "Observaciones de la exposición previa", rows = 2)),
        if ("bioensayo" %in% stages) tagList(h4(paste0("9.", number, " · Lectura posterior: ", label)),
        p(if (set == "sinergista") "Mosquitos procedentes de 8.1. Botellas SinE1–SinE4 y SinC1." else "Mosquitos procedentes de 8.2. Botellas EtOHE1–EtOHE4 y EtOHC1."),
        f7_sets_grid(set, "bioensayo", c(paste0("e", 1:4), "c1"), c(0, 15, 30, 45)),
        textAreaInput(paste0("f7sets_", set, "observaciones_bio"), "Observaciones de la lectura posterior", rows = 2))
      )
    }))),
    if ("kdr_24h" %in% stages) tagList(checkboxInput("f7sets_incluir_24h", "Incluir lectura a 24 horas en ambos sets (opcional)", FALSE),
    conditionalPanel("input.f7sets_incluir_24h", lapply(c("sinergista", "etanol"), function(set) tagList(
      h4(paste("24 horas ·", set)),
      f7_sets_grid(set, "kdr_24h", c(paste0("e", 1:4), "c1"), 1440)
    ))))
  )
}

f7_sets_collect <- function(input, header = list()) {
  get <- function(id) {
    v <- input[[id]]
    if (is.null(v) || !length(v) || is.na(v[[1]]) || !nzchar(trimws(as.character(v[[1]])))) return(NULL)
    v[[1]]
  }
  readings <- list()
  sets <- list()
  for (set in c("sinergista", "etanol")) {
    sets[[length(sets) + 1L]] <- list(tipo_set = set,
      observaciones_pretratamiento = get(paste0("f7sets_", set, "observaciones_pre")),
      observaciones_bioensayo = get(paste0("f7sets_", set, "observaciones_bio")))
    stages <- c("pretratamiento", "bioensayo", if (isTRUE(input[["f7sets_incluir_24h"]])) "kdr_24h")
    for (stage in stages) {
      bottles <- if (stage == "pretratamiento") paste0("e", 1:5) else c(paste0("e", 1:4), "c1")
      times <- switch(stage, pretratamiento = 60, bioensayo = c(0, 15, 30, 45), kdr_24h = 1440)
      for (b in bottles) for (t in times) {
        prefix <- paste("f7sets", set, stage, b, sep = "_")
        readings[[length(readings) + 1L]] <- list(tipo_set = set, etapa = stage, botella = b,
          tiempo_minutos = t, hora_inicio = get(paste0(prefix, "_inicio")),
          vivos = get(paste(prefix, t, "vivos", sep = "_")),
          incapacitados = get(paste(prefix, t, "incapacitados", sep = "_")))
      }
    }
  }
  list(version_estructura = "f7_sets_local_v1", estado = "borrador_local",
    encabezado = header, sets = sets, lecturas = readings)
}

f7_sets_errors <- function(payload, complete = FALSE) {
  errors <- character()
  if (!identical(payload$version_estructura, "f7_sets_local_v1") ||
      !is.list(payload$lecturas) || !length(payload$lecturas) ||
      !is.list(payload$sets) || length(payload$sets) != 2L) return("Estructura de borrador inválida.")
  set_names <- vapply(payload$sets, function(s) if (is.character(s$tipo_set) && length(s$tipo_set) == 1) s$tipo_set else "", character(1))
  if (!setequal(set_names, c("sinergista", "etanol"))) return("El borrador requiere los sets sinergista y etanol.")
  keys <- character()
  for (r in payload$lecturas) {
    if (!is.list(r) || length(r$tipo_set) != 1 || length(r$etapa) != 1 ||
        length(r$botella) != 1 || length(r$tiempo_minutos) != 1) return("Identidad de lectura inválida.")
    allowed_bottles <- if (identical(r$etapa, "pretratamiento")) paste0("e", 1:5) else c(paste0("e", 1:4), "c1")
    allowed_times <- switch(as.character(r$etapa), pretratamiento = 60, bioensayo = c(0, 15, 30, 45), kdr_24h = 1440, numeric())
    if (!r$tipo_set %in% c("sinergista", "etanol") || !r$botella %in% allowed_bottles || !r$tiempo_minutos %in% allowed_times) return("Set, etapa, botella o tiempo no permitido.")
    keys <- c(keys, paste(r$tipo_set, r$etapa, r$botella, r$tiempo_minutos))
    label <- paste(r$tipo_set, r$etapa, toupper(r$botella), paste0(r$tiempo_minutos, " min"))
    counts <- list(r$vivos, r$incapacitados)
    missing <- vapply(counts, is.null, logical(1))
    if ((complete && any(missing)) || xor(missing[[1]], missing[[2]])) errors <- c(errors, paste(label, ": complete vivos e incapacitados."))
    for (v in counts) if (!is.null(v)) {
      n <- suppressWarnings(as.numeric(v))
      if (length(n) != 1 || !is.finite(n) || n < 0 || n != floor(n)) errors <- c(errors, paste(label, ": use enteros no negativos."))
    }
    if (!is.null(r$hora_inicio) && !grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", r$hora_inicio)) errors <- c(errors, paste(label, ": hora inválida, use HH:MM."))
    if (complete && is.null(r$hora_inicio)) errors <- c(errors, paste(label, ": indique la hora."))
  }
  if (anyDuplicated(keys)) errors <- c(errors, "Hay lecturas duplicadas para un set, etapa, botella y tiempo.")
  if (!length(payload$lecturas) %in% c(50L, 60L)) errors <- c(errors, "El borrador debe conservar las 50 lecturas base y, opcionalmente, las 10 de 24 horas.")
  unique(errors)
}

f7_sets_analysis <- function(payload, insecticida) {
  diagnostic_time <- f7_cdc_diagnostic_time(insecticida)
  pending <- function(detail) list(
    resultado_diagnostico = NA_character_,
    mortalidad_corregida_pct = NA_real_,
    mortalidad_control_pct = NA_real_,
    detalle = detail
  )
  if (is.na(diagnostic_time)) return(setNames(rep(list(pending("Insecticida sin tiempo diagnóstico CDC configurado")), 2L), c("sinergista", "etanol")))

  analyse_set <- function(set_name) {
    stage <- if (diagnostic_time == 1440L) "kdr_24h" else "bioensayo"
    readings <- Filter(function(reading) {
      identical(reading$tipo_set, set_name) && identical(reading$etapa, stage) &&
        identical(as.integer(reading$tiempo_minutos), as.integer(diagnostic_time))
    }, payload$lecturas)
    expected <- c("e1", "e2", "e3", "e4", "c1")
    indexed <- setNames(readings, vapply(readings, function(reading) reading$botella, character(1)))
    if (!all(expected %in% names(indexed))) return(pending("Faltan lecturas al tiempo diagnóstico."))
    row <- list(insecticida = insecticida)
    for (index in seq_along(expected)) {
      source <- indexed[[expected[[index]]]]
      bottle <- c("b1", "b2", "b3", "b4", "c1")[[index]]
      prefix <- f7_cdc_result_prefix(diagnostic_time, bottle)
      row[[paste0(prefix, "_vivos")]] <- source$vivos
      row[[paste0(prefix, "_incapacitados")]] <- source$incapacitados
    }
    classified <- f7_cdc_analysis_for_row(row)
    list(
      resultado_diagnostico = classified$cdc_resultado,
      mortalidad_corregida_pct = classified$cdc_mortalidad_corregida_pct,
      mortalidad_control_pct = classified$cdc_mortalidad_control_pct,
      detalle = classified$cdc_correccion
    )
  }

  setNames(lapply(c("sinergista", "etanol"), analyse_set), c("sinergista", "etanol"))
}

f7_sinergista_header_columns <- c(
  "version_estructura", "formulario_codigo", "formulario_nombre", "fecha_registro",
  "codigo_bioensayo", "nombre_poblacion", "pais", "id_institucion",
  "codigo_departamento", "codigo_municipio", "sinergista_tipo",
  "dosis_sinergista_ug_ml", "sinergista_resultado_diagnostico",
  "etanol_resultado_diagnostico", "sinergista_mortalidad_corregida_pct",
  "etanol_mortalidad_corregida_pct", "sinergista_mortalidad_control_pct",
  "etanol_mortalidad_control_pct", "incluir_24h", "fecha_realizacion_bioensayo",
  "insecticida", "solvente_utilizado", "solvente_otro", "dosis_intensidad_ug_ml",
  "lote_insecticida", "fecha_revestimiento_botellas", "numero_usos_botella_e1",
  "numero_usos_botella_e2", "numero_usos_botella_e3", "numero_usos_botella_e4",
  "numero_usos_botella_c1", "origen_material", "edad_dias", "edad_indefinida",
  "codigo_especie_mosquito", "fecha_separacion", "hora_separacion",
  "generacion_filial", "generacion_filial_indefinida",
  "codigo_responsable_revestimiento", "codigo_responsable_bioensayo",
  "codigo_control_calidad", "codigo_revision_24h", "temperatura_inicial_c",
  "temperatura_final_c", "humedad_relativa_inicial_pct", "humedad_relativa_final_pct",
  "hora_inicio_bioensayo", "hora_final_bioensayo", "fuente_formulario",
  "nombre_quien_ingreso"
)

f7_sinergista_tables <- function(row, payload) {
  if (nrow(row) != 1L) stop("La captura de Sinergistas debe contener un solo encabezado.")
  errors <- f7_sets_errors(payload, complete = TRUE)
  if (length(errors)) stop(paste(errors, collapse = "\n"))

  analysis <- f7_sets_analysis(payload, row$insecticida[[1]])
  header <- as.list(row[1, intersect(names(row), f7_sinergista_header_columns), drop = FALSE])
  header$version_estructura <- "f7_sinergistas_v1"
  header$incluir_24h <- any(vapply(payload$lecturas, function(reading) identical(reading$etapa, "kdr_24h"), logical(1)))
  header$sinergista_resultado_diagnostico <- analysis$sinergista$resultado_diagnostico
  header$etanol_resultado_diagnostico <- analysis$etanol$resultado_diagnostico
  header$sinergista_mortalidad_corregida_pct <- analysis$sinergista$mortalidad_corregida_pct
  header$etanol_mortalidad_corregida_pct <- analysis$etanol$mortalidad_corregida_pct
  header$sinergista_mortalidad_control_pct <- analysis$sinergista$mortalidad_control_pct
  header$etanol_mortalidad_control_pct <- analysis$etanol$mortalidad_control_pct

  for (field in c("edad_indefinida", "generacion_filial_indefinida")) {
    header[[field]] <- tolower(as.character(header[[field]])) %in% c("true", "t", "1")
  }
  for (field in c(
    "dosis_sinergista_ug_ml", "dosis_intensidad_ug_ml", "numero_usos_botella_e1",
    "numero_usos_botella_e2", "numero_usos_botella_e3", "numero_usos_botella_e4",
    "numero_usos_botella_c1", "edad_dias", "temperatura_inicial_c", "temperatura_final_c",
    "humedad_relativa_inicial_pct", "humedad_relativa_final_pct"
  )) header[[field]] <- suppressWarnings(as.numeric(header[[field]]))

  set_observation <- function(set_name, field) {
    current <- Filter(function(set) identical(set$tipo_set, set_name), payload$sets)[[1]]
    current[[field]]
  }
  comments <- list(list(
    comentario = row$comentario[[1]], nombre = row$comentario_nombre[[1]],
    sinergista_observaciones_pretratamiento = set_observation("sinergista", "observaciones_pretratamiento"),
    sinergista_observaciones_bioensayo = set_observation("sinergista", "observaciones_bioensayo"),
    etanol_observaciones_pretratamiento = set_observation("etanol", "observaciones_pretratamiento"),
    etanol_observaciones_bioensayo = set_observation("etanol", "observaciones_bioensayo")
  ))

  list(header = header, results = payload$lecturas, comments = comments, analysis = analysis)
}

f7_sets_save_local <- function(payload, directory = file.path("..", "output", "f7_capturas_locales")) {
  errors <- f7_sets_errors(payload)
  if (length(errors)) stop(paste(errors, collapse = "\n"))
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  destination <- tempfile("f7_", tmpdir = directory, fileext = ".json")
  pending <- tempfile(".pending_", tmpdir = directory)
  on.exit(unlink(pending), add = TRUE)
  jsonlite::write_json(payload, pending, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  if (!file.rename(pending, destination)) stop("No se pudo guardar el archivo local.")
  normalizePath(destination)
}
