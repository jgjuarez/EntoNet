# Run from the repository root: Rscript scripts/test_formularios_api.R
source("scripts/test_f7_review_rpc.R")
env <- new.env(parent = globalenv())
targets <- c(
  "value_or_default", "normalize_f7_diagnostic_result", "supabase_review_rpc",
  "formulario_7_codigo_bioensayo_final", "formulario_7_header_columns",
  "formulario_7_bottles", "formulario_7_result_columns", "formulario_7_non_24h_result_columns",
  "formulario_7_comment_columns", "formulario_7_intake_columns", "formulario_7_csv_columns",
  "formulario_7_csv_to_internal", "formulario_7_internal_to_csv", "formulario_7_external_to_internal_names",
  "formulario_7_is_temefos", "f7_clean_text", "f7_parse_boolean", "validate_formulario_7",
  "formulario_7_tables", "supabase_record_from_row", "supabase_records_from_data_frame",
  "f1_update_review_record", "f1_fetch_review_record", "f5_fetch_review_record",
  "f1_delete_review_record", "f5_delete_review_record", "f7_delete_review_record"
)
for (expression in expressions) collect(expression)
stopifnot(all(vapply(targets, exists, logical(1), envir = env, inherits = FALSE)))

row <- as.data.frame(setNames(rep(list(NA_character_), length(env$formulario_7_intake_columns)),
                              env$formulario_7_intake_columns), stringsAsFactors = FALSE)
values <- list(
  formulario_codigo = "F7", formulario_nombre = "Prueba", nombre_poblacion = "Prueba",
  codigo_bioensayo = "REI26GT2001P3DEL2F0", insecticida = "Deltametrina",
  solvente_utilizado = "Etanol", lote_insecticida = "TEST", origen_material = "Laboratorio",
  pais = "Guatemala", id_institucion = "UVG", codigo_departamento = "20",
  codigo_municipio = "01", codigo_especie_mosquito = "AE",
  codigo_responsable_revestimiento = "TEST", codigo_responsable_bioensayo = "TEST",
  codigo_revision_24h = "TEST", fecha_registro = "2026-09-07",
  fecha_realizacion_bioensayo = "2026-09-07", fecha_revestimiento_botellas = "2026-09-07",
  fecha_separacion = "2026-09-07", hora_separacion = "08:00",
  hora_inicio_bioensayo = "09:00", hora_final_bioensayo = "10:00",
  bioensayo_diagnostica_1x = "true", sinergista_def = "false", sinergista_pbo = "false",
  sinergista_dm = "false", edad_indefinida = "true", generacion_filial_indefinida = "true",
  dosis_intensidad_ug_ml = "1", temperatura_inicial_c = "25", temperatura_final_c = "25",
  humedad_relativa_inicial_pct = "70", humedad_relativa_final_pct = "70"
)
for (name in names(values)) row[[name]] <- values[[name]]
for (diagnostic in c("Susceptible", "Suceptible", " susceptible ", "Sospecha de Resistencia", "Resistente")) {
  row$resultado_diagnostico <- diagnostic
  for (mode in c("diagnostica", "exploratorio", "completa", "sinergista")) {
    fixture <- row
    fixture$bioensayo_diagnostica_1x <- if (mode == "diagnostica") "true" else "false"
    if (mode == "exploratorio") {
      fixture$bioensayo_intensidad <- "Exploratorio"
      if (!env$normalize_f7_diagnostic_result(diagnostic) %in% "Susceptible") fixture$dosis_intensidad <- "2X"
    }
    if (mode == "completa") {
      fixture$bioensayo_intensidad <- "Completa"
      fixture$dosis_intensidad <- "5X"
    }
    if (mode == "sinergista") {
      fixture$sinergista_pbo <- "true"
      fixture$sinergista_tipo <- "PBO"
      fixture$dosis_sinergista_ug_ml <- "100"
    }
    for (input_data in list(fixture, env$formulario_7_internal_to_csv(fixture))) {
      checked <- env$validate_formulario_7(input_data)
      if (length(checked$details)) stop(paste(mode, diagnostic, paste(checked$details, collapse = "; ")))
      stopifnot(checked$data$resultado_diagnostico == env$normalize_f7_diagnostic_result(diagnostic))
      tables <- env$formulario_7_tables(checked$data)
      stopifnot(tables$header$resultado_diagnostico == env$normalize_f7_diagnostic_result(diagnostic))
    }
  }
}
# Legacy CSVs remain accepted when no intensity multiplier is needed.
row$resultado_diagnostico <- "Suceptible"
legacy <- env$formulario_7_internal_to_csv(row)
legacy$dosis_intensidad <- NULL
stopifnot(length(env$validate_formulario_7(legacy)$details) == 0L)
# A mixed file must derive each row's synergist independently.
mixed <- env$formulario_7_internal_to_csv(row[rep(1, 3), , drop = FALSE])
mixed$sinergista_def <- c("true", "false", "false")
mixed$sinergista_pbo <- c("false", "true", "false")
mixed$sinergista_dm <- c("false", "false", "true")
stopifnot(identical(env$formulario_7_csv_to_internal(mixed)$sinergista_tipo, c("DEF", "PBO", "DM")))
temefos <- row
temefos$insecticida <- "Temefos"
temefos$resultado_24h_b1_vivos <- "5"
temefos$resultado_24h_b1_incapacitados <- "15"
checked <- env$validate_formulario_7(temefos)
stopifnot(length(checked$details) == 0L,
          env$formulario_7_tables(checked$data)$results$tiempo_minutos == 1440)
temefos$resultado_0min_b1_vivos <- "20"
temefos$resultado_0min_b1_incapacitados <- "0"
stopifnot(length(env$validate_formulario_7(temefos)$details) > 0L)
row$resultado_diagnostico <- "INVALID"
stopifnot(any(grepl("resultado_diagnostico", env$validate_formulario_7(row)$details)))
row$resultado_diagnostico <- "Susceptible"
row$humedad_relativa_inicial_pct <- "101"
stopifnot(any(grepl("humedad", env$validate_formulario_7(row)$details)))

calls <- list()
env$supabase_private_rpc <- function(name, body) {
  calls[[length(calls) + 1L]] <<- list(name = name, body = body)
  if (grepl("delete", name)) return(data.frame(intake_id = body$p_intake_id))
  body$p_intake_id
}
env$formulario_1_tables <- function(data) list(header = data["codigo_formulario"], detail = data["codigo_sustrato"])
env$f1_update_review_record(42, data.frame(codigo_formulario = c("F1TEST", "F1TEST"),
                                         codigo_sustrato = c("A", "B")))
stopifnot(length(calls[[1]]$body$p_details) == 2L)
for (n in c(1, 5, 7)) {
  fun <- env[[paste0("f", n, "_delete_review_record")]]
  expect_error(fun(42, "", "TEST"))
  deleted <- fun(42, "Prueba", "TEST")
  stopifnot(deleted$intake_id == 42, tail(calls, 1)[[1]]$name == paste0("entonet_delete_formulario_", n))
}
env$supabase_private_select <- function(...) data.frame()
stopifnot(is.null(env$f1_fetch_review_record(42)), nrow(env$f5_fetch_review_record(42)) == 0L)

# Prevent reintroducing database-password fallbacks anywhere in application workflows.
lines <- readLines("shiny_app/app.R", warn = FALSE)
stopifnot(!any(grepl("connect_to_supabase\\(", lines)))
stopifnot(!any(grepl("db(GetQuery|Execute|AppendTable)\\(", lines)))
cat("PASS: all F7 modes, legacy spelling, CSV/capture validation, F1/F5/F7 API routing\n")
