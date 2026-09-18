# Run from the repository root: Rscript scripts/test_f7_sinergista_visualization.R
expressions <- parse("shiny_app/app.R")

assignment_expression <- function(name) {
  for (expression in expressions) {
    if (is.call(expression) && identical(expression[[1]], as.name("<-")) && identical(as.character(expression[[2]]), name)) {
      return(expression)
    }
  }
  stop("Assignment not found: ", name)
}

env <- new.env(parent = baseenv())
env$formulario_7_result_columns <- unlist(lapply(c("b1", "b2", "b3", "b4", "c1"), function(bottle) {
  unlist(lapply(c("0min", "15min", "30min", "45min", "60min", "24h"), function(time) {
    paste0("resultado_", time, "_", bottle, c("_vivos", "_incapacitados"))
  }), use.names = FALSE)
}), use.names = FALSE)

for (name in c(
  "value_or_default", "f7_cdc_diagnostic_time", "f7_cdc_result_prefix",
  "f7_cdc_count_value", "f7_cdc_analysis_for_row", "f7_add_cdc_analysis",
  "f7_sinergista_qc_records", "f7_sinergista_visualization_records"
)) {
  eval(assignment_expression(name), envir = env)
}

env$supabase_private_select <- function(...) {
  bottles <- c("e1", "e2", "e3", "e4", "c1")
  do.call(rbind, lapply(c("sinergista", "etanol"), function(set_name) {
    data.frame(
      sinergista_intake_id = 27L,
      tipo_set = set_name,
      etapa = "bioensayo",
      botella = bottles,
      tiempo_minutos = 30L,
      vivos = if (set_name == "sinergista") c(0L, 0L, 0L, 0L, 10L) else c(5L, 5L, 5L, 5L, 10L),
      incapacitados = if (set_name == "sinergista") c(10L, 10L, 10L, 10L, 0L) else c(5L, 5L, 5L, 5L, 0L),
      stringsAsFactors = FALSE
    )
  }))
}

headers <- data.frame(
  sinergista_intake_id = 27L,
  codigo_bioensayo = "REI26GT0101P1SPBOPER1F1",
  nombre_poblacion = "Población prueba",
  sinergista_tipo = "PBO",
  insecticida = "Permetrina",
  codigo_departamento = "01",
  codigo_municipio = "0101",
  review_status = "reviewed",
  stringsAsFactors = FALSE
)

records <- env$f7_sinergista_visualization_records(headers)
stopifnot(
  nrow(records) == 1L,
  records$tipo_bioensayo[[1]] == "Sinergistas",
  records$sinergista_resultado_diagnostico[[1]] == "Susceptible",
  records$etanol_resultado_diagnostico[[1]] == "Resistente",
  records$sinergista_mortalidad_corregida_pct[[1]] == 100,
  records$etanol_mortalidad_corregida_pct[[1]] == 50,
  records$diferencia_sinergista_etanol_pct[[1]] == 50,
  records$comparacion_sinergista_etanol[[1]] == "Mayor mortalidad con sinergista",
  records$sinergista_resultado_30min_b1_incapacitados[[1]] == 10,
  records$etanol_resultado_30min_b1_incapacitados[[1]] == 5
)

cat("PASS: visualización F7 integra y compara los sets Sinergista y EtOH\n")
