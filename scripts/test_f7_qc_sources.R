expressions <- parse("shiny_app/app.R")

assignment_expression <- function(name) {
  for (expression in expressions) {
    if (is.call(expression) && identical(expression[[1]], as.name("<-")) && identical(as.character(expression[[2]]), name)) {
      return(expression)
    }
  }
  stop("Assignment not found: ", name)
}

test_env <- new.env(parent = baseenv())
test_env$formulario_7_result_columns <- unlist(lapply(c("b1", "b2", "b3", "b4", "c1"), function(bottle) {
  unlist(lapply(c("0min", "15min", "30min", "45min", "60min", "24h"), function(time) {
    paste0("resultado_", time, "_", bottle, c("_vivos", "_incapacitados"))
  }), use.names = FALSE)
}), use.names = FALSE)
test_env$f7_cdc_count_value <- function(value) suppressWarnings(as.numeric(value))
test_env$supabase_private_select <- function(...) {
  data.frame(
    sinergista_intake_id = c(17, 17),
    tipo_set = c("sinergista", "etanol"),
    etapa = c("bioensayo", "bioensayo"),
    botella = c("e1", "e1"),
    tiempo_minutos = c(30, 30),
    vivos = c(1, 3),
    incapacitados = c(19, 17),
    stringsAsFactors = FALSE
  )
}

eval(assignment_expression("f7_bind_rows_fill"), envir = test_env)
eval(assignment_expression("f7_sinergista_qc_records"), envir = test_env)

headers <- data.frame(
  sinergista_intake_id = 17,
  sinergista_tipo = "PBO",
  codigo_bioensayo = "F7-PBO-001",
  nombre_poblacion = "Prueba",
  pais = "Guatemala",
  codigo_departamento = "01",
  codigo_municipio = "0101",
  insecticida = "Permetrina",
  stringsAsFactors = FALSE
)
converted <- test_env$f7_sinergista_qc_records(headers)
stopifnot(
  nrow(converted) == 2L,
  identical(as.character(converted$qc_set), c("sinergista", "etanol")),
  all(converted$review_key == "sinergistas:17"),
  isTRUE(converted$sinergista_pbo[[1]]),
  !isTRUE(converted$sinergista_pbo[[2]]),
  converted$resultado_30min_b1_vivos[[1]] == 1,
  converted$resultado_30min_b1_vivos[[2]] == 3
)

combined <- test_env$f7_bind_rows_fill(
  data.frame(a = 1, stringsAsFactors = FALSE),
  data.frame(b = 2, stringsAsFactors = FALSE)
)
stopifnot(nrow(combined) == 2L, all(c("a", "b") %in% names(combined)))

test_env$f7_diagnostic_capture_analysis <- function(row) list(resultado_diagnostico = "Susceptible", detalles = "")
test_env$f7_cdc_analysis_for_row <- function(row) {
  mortality <- switch(as.character(row$qc_set[[1]]), registro = 99, sinergista = 95, etanol = 90)
  list(cdc_mortalidad_corregida_pct = mortality)
}
test_env$f7_bottle_total_errors <- function(row) character()
eval(assignment_expression("f7_qc_findings"), envir = test_env)

qc_records <- data.frame(
  intake_id = c(1, 17, 17),
  review_key = c("bioensayo:1", "sinergistas:17", "sinergistas:17"),
  qc_source_label = c("Dosis / Intensidad", "Sinergistas", "Sinergistas"),
  qc_set = c("registro", "sinergista", "etanol"),
  qc_set_label = c("Registro", "Sinergista", "Etanol"),
  qc_validate_mortality = c(FALSE, TRUE, TRUE),
  sinergista_def = c(FALSE, FALSE, FALSE),
  sinergista_pbo = c(FALSE, TRUE, FALSE),
  sinergista_dm = c(FALSE, FALSE, FALSE),
  bioensayo_diagnostica_1x = c(TRUE, FALSE, FALSE),
  pais = rep("Guatemala", 3),
  codigo_departamento = rep("01", 3),
  codigo_municipio = rep("0101", 3),
  nombre_poblacion = rep("Prueba", 3),
  insecticida = rep("Permetrina", 3),
  stringsAsFactors = FALSE
)
findings <- test_env$f7_qc_findings(qc_records)
stopifnot(
  nrow(findings) == 1L,
  findings$review_key[[1]] == "sinergistas:17",
  findings$conjunto[[1]] == "Sinergista",
  findings$diagnostica[[1]] == 99
)

cat("F7_QC_SOURCES_OK\n")
