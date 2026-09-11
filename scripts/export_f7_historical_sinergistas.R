#!/usr/bin/env Rscript

# Export historical Sinergistas records that predate the separate
# Sinergista/Etanol intake structure. Run from the EntoNet repository root.
args <- commandArgs(trailingOnly = TRUE)
output_file <- if (length(args)) args[[1]] else file.path(
  "output",
  paste0("formulario_7_sinergistas_historico_", format(Sys.Date(), "%Y%m%d"), ".csv")
)

repo_dir <- normalizePath(".", mustWork = TRUE)
app_dir <- file.path(repo_dir, "shiny_app")
if (!dir.exists(app_dir)) stop("Ejecute este script desde la raíz de EntoNet.")
if (!grepl("^/", output_file)) output_file <- file.path(repo_dir, output_file)
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
output_file <- normalizePath(output_file, mustWork = FALSE)

app_env <- new.env(parent = globalenv())
old_workdir <- getwd()
on.exit(setwd(old_workdir), add = TRUE)
setwd(app_dir)
source("app.R", local = app_env)

header <- app_env$supabase_private_select(
  "formulario_7_bioensayo_intake",
  select = "*",
  order = "fecha_registro.asc,intake_id.asc"
)

is_true <- function(values) {
  tolower(trimws(as.character(values))) %in% c("true", "t", "1", "si", "sí", "yes")
}
is_present <- function(values) {
  !is.na(values) & nzchar(trimws(as.character(values)))
}
historical <- is_true(header$sinergista_def) |
  is_true(header$sinergista_pbo) |
  is_true(header$sinergista_dm) |
  is_present(header$sinergista_tipo) |
  !is.na(header$dosis_sinergista_ug_ml)
header <- header[historical, , drop = FALSE]

if (!nrow(header)) stop("No se encontraron registros históricos de Sinergistas.")
for (column in setdiff(app_env$formulario_7_intake_columns, names(header))) header[[column]] <- NA

ids <- paste(as.integer(header$intake_id), collapse = ",")
results <- app_env$supabase_private_select(
  "formulario_7_bioensayo_resultado_intake",
  select = "intake_id,fase,botella,tiempo_minutos,hora_lectura,vivos,incapacitados",
  filters = list(intake_id = paste0("in.(", ids, ")")),
  order = "intake_id.asc,botella.asc,tiempo_minutos.asc"
)
comments <- app_env$supabase_private_select(
  "formulario_7_bioensayo_comentario_intake",
  select = "intake_id,comentario,nombre",
  filters = list(intake_id = paste0("in.(", ids, ")")),
  order = "intake_id.asc"
)

row_index <- setNames(seq_len(nrow(header)), as.character(header$intake_id))
for (index in seq_len(nrow(results))) {
  result <- results[index, , drop = FALSE]
  target <- row_index[[as.character(result$intake_id[[1]])]]
  bottle <- as.character(result$botella[[1]])
  minutes <- as.integer(result$tiempo_minutos[[1]])
  if (is.na(target) || !nzchar(bottle) || is.na(minutes)) next
  if (minutes == 1440L) {
    header[[paste0("resultado_hora_lectura_24h_", bottle)]][[target]] <- as.character(result$hora_lectura[[1]])
    header[[paste0("resultado_24h_", bottle, "_vivos")]][[target]] <- result$vivos[[1]]
    header[[paste0("resultado_24h_", bottle, "_incapacitados")]][[target]] <- result$incapacitados[[1]]
  } else {
    if (minutes == 0L) header[[paste0("resultado_hora_inicio_", bottle)]][[target]] <- as.character(result$hora_lectura[[1]])
    header[[paste0("resultado_", minutes, "min_", bottle, "_vivos")]][[target]] <- result$vivos[[1]]
    header[[paste0("resultado_", minutes, "min_", bottle, "_incapacitados")]][[target]] <- result$incapacitados[[1]]
  }
}

if (nrow(comments)) {
  for (index in seq_len(nrow(comments))) {
    target <- row_index[[as.character(comments$intake_id[[index]])]]
    if (is.na(target)) next
    header$comentario[[target]] <- comments$comentario[[index]]
    header$comentario_nombre[[target]] <- comments$nombre[[index]]
  }
}

legacy_columns <- c(
  "sinergista_def", "sinergista_pbo", "sinergista_dm",
  "sinergista_tipo", "dosis_sinergista_ug_ml"
)
export_columns <- c(
  "intake_id", "review_status", "review_notes", "reviewed_by", "reviewed_at",
  app_env$formulario_7_csv_columns[1:14], legacy_columns,
  app_env$formulario_7_csv_columns[-(1:14)], "creado_en", "actualizado_en"
)
for (column in setdiff(export_columns, names(header))) header[[column]] <- NA
export <- header[export_columns]

utils::write.csv(export, output_file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
message(sprintf("Exportados %s registros a %s", nrow(export), normalizePath(output_file, mustWork = FALSE)))
