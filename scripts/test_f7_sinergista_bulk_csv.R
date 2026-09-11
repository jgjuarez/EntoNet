# Run from the repository root: Rscript scripts/test_f7_sinergista_bulk_csv.R
old_workdir <- getwd()
on.exit(setwd(old_workdir), add = TRUE)
setwd("shiny_app")
source("app.R", local = globalenv())
setwd("..")

csv <- formulario_7_sinergista_template
values <- list(
  codigo_bioensayo = "REI26GT2001P3DEL2F0", nombre_poblacion = "Prueba", pais = "Guatemala",
  id_institucion = "UVG", codigo_departamento = "01", codigo_municipio = "01",
  sinergista_tipo = "PBO", dosis_sinergista_ug_ml = "100", fecha_realizacion_bioensayo = "2026-09-11",
  insecticida = "Deltametrina", solvente_utilizado = "Etanol", dosis_intensidad_ug_ml = "10",
  lote_insecticida = "TEST", fecha_revestimiento_botellas = "2026-09-11", origen_material = "Laboratorio",
  codigo_especie_mosquito = "AE", fecha_separacion = "2026-09-11", hora_separacion = "08:00",
  codigo_responsable_revestimiento = "TEST", codigo_responsable_bioensayo = "TEST", codigo_revision_24h = "TEST",
  temperatura_inicial_c = "25", temperatura_final_c = "25", humedad_relativa_inicial_pct = "70",
  humedad_relativa_final_pct = "70", hora_inicio_bioensayo = "08:00", hora_final_bioensayo = "09:00",
  nombre_quien_ingreso = "Codex"
)
for (name in names(values)) csv[[name]] <- values[[name]]
for (index in seq_len(nrow(f7_sinergista_csv_specs))) {
  spec <- f7_sinergista_csv_specs[index, ]
  if (spec$etapa == "kdr_24h") next
  csv[[spec$hora_columna]] <- "08:00"
  treated_ethanol <- spec$tipo_set == "etanol" && spec$etapa == "bioensayo" && spec$botella != "c1"
  csv[[spec$vivos_columna]] <- if (treated_ethanol) "0" else "20"
  csv[[spec$incapacitados_columna]] <- if (treated_ethanol) "20" else "0"
}
capture <- f7_sinergista_csv_to_capture(csv, 1)
tables <- f7_sinergista_tables(capture$header, capture$payload)
stopifnot(
  ncol(csv) == length(f7_sinergista_csv_columns),
  length(capture$payload$lecturas) == 50L,
  length(tables$results) == 50L,
  identical(tables$header$sinergista_resultado_diagnostico, "Resistente"),
  identical(tables$header$etanol_resultado_diagnostico, "Susceptible"),
  identical(capture$header$codigo_bioensayo[[1]], "REI26GT2001P3DEL2F0")
)
csv$insecticida <- "Temefos"
temefos_capture <- f7_sinergista_csv_to_capture(csv, 1)
temefos_error <- tryCatch({ f7_sinergista_tables(temefos_capture$header, temefos_capture$payload); NULL }, error = conditionMessage)
stopifnot(identical(temefos_error, "Temefos solo puede capturarse para Diagnóstica e Intensidad."))
cat("PASS: machote CSV de Sinergistas genera dos sets, 50 lecturas y bloquea Temefos\n")
