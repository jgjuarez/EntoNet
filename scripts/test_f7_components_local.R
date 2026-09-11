source("scripts/preview_f7_componentes_local.R")
stopifnot(!"sinergista_tipo" %in% f7_component_columns("diagnostica"),
  !"bioensayo_intensidad" %in% f7_component_columns("sinergistas"),
  "bioensayo_intensidad" %in% f7_component_columns("intensidad"),
  "etanol_bioensayo_e1_30min_vivos" %in% f7_component_columns("sinergistas"),
  identical(f7_component_columns("actual"), formulario_7_csv_columns),
  identical(f7_storage_key("diagnostica"), "actual"),
  identical(f7_storage_key("intensidad"), "actual"),
  identical(f7_storage_key("sinergistas"), "sinergistas"))
html <- as.character(ui)
ids <- regmatches(html, gregexpr(' id="[^"]+"', html))[[1]]
stopifnot(!anyDuplicated(ids))
stopifnot(!grepl("Descargar sábana del tipo seleccionado|Descargar machote vacío", html))
stopifnot(length(gregexpr("Validar y guardar captura completa", html, fixed = TRUE)[[1]]) == 1L)
tmp <- tempfile("f7_components_test_")
for (mode in names(f7_component_labels)) {
  shiny::testServer(f7_component_server, args = list(mode = mode, directory = tmp), {
    files_before_incomplete <- list.files(folder, pattern = "^captura_.*json$", full.names = TRUE)
    session$setInputs(f7_codigo_bioensayo = "PRUEBA", f7_insecticida = "Deltametrina",
      f7_bioensayo_intensidad = "Exploratorio", f7_sinergista_tipo = "DEF")
    row <- current()
    stopifnot(row$componente == mode)
    stopifnot(!isTRUE(save())) # incomplete capture cannot enter a sheet
    files <- list.files(folder, pattern = "^captura_.*json$", full.names = TRUE)
    stopifnot(identical(sort(files), sort(files_before_incomplete)))
    if (mode == "sinergistas") {
      session$setInputs(f7sets_sinergista_pretratamiento_e1_60_vivos = 20,
        f7sets_sinergista_pretratamiento_e1_60_incapacitados = 0,
        f7sets_etanol_pretratamiento_e1_60_vivos = 18,
        f7sets_etanol_pretratamiento_e1_60_incapacitados = 2)
      r <- current()
      stopifnot(r$sinergista_pretratamiento_e1_60min_vivos == "20", r$etanol_pretratamiento_e1_60min_vivos == "18")
    } else {
      session$setInputs(f7_resultado_0min_b1_vivos = -1, f7_resultado_0min_b1_incapacitados = 0)
      stopifnot(length(check(FALSE)) > 0)
    }
    # A fully populated, synthetic capture enters only its own component sheet.
    fixture <- list(f7_codigo_bioensayo = paste0("PRUEBA-COMPLETA-", mode),
      f7_pais = "Guatemala", f7_id_institucion = "TEST", departamento = "01",
      f7_codigo_municipio = "01", f7_nombre_poblacion = "Prueba", f7_creado_por = "Prueba",
      f7_insecticida = "Deltametrina", f7_solvente_utilizado = "Etanol",
      f7_dosis_intensidad_ug_ml = 10, f7_lote_insecticida = "TEST", f7_origen_material = "Laboratorio",
      f7_codigo_especie_mosquito = "AE", f7_codigo_responsable_revestimiento = "TEST",
      f7_codigo_responsable_bioensayo = "TEST", f7_codigo_revision_24h = "TEST",
      f7_temperatura_inicial_c = 25, f7_temperatura_final_c = 25,
      f7_humedad_relativa_inicial_pct = 70, f7_humedad_relativa_final_pct = 70,
      f7_edad_indefinida = TRUE, f7_generacion_filial_indefinida = TRUE,
      f7_sinergista_tipo = "DEF", f7_dosis_sinergista_ug_ml = 100)
    for (k in c("fecha_registro", "fecha_realizacion_bioensayo", "fecha_revestimiento_botellas", "fecha_separacion")) fixture[[paste0("f7_", k)]] <- "2026-09-10"
    for (k in c("hora_separacion", "hora_inicio_bioensayo", "hora_final_bioensayo")) fixture[[paste0("f7_", k)]] <- "08:00"
    for (k in formulario_7_result_columns) fixture[[paste0("f7_", k)]] <- if (grepl("hora_", k)) "08:00" else if (grepl("vivos$", k)) 20 else 0
    for (r in f7_sets_collect(list())$lecturas) {
      prefix <- paste("f7sets", r$tipo_set, r$etapa, r$botella, sep = "_")
      fixture[[paste0(prefix, "_inicio")]] <- "08:00"
      fixture[[paste(prefix, r$tiempo_minutos, "vivos", sep = "_")]] <- 20
      fixture[[paste(prefix, r$tiempo_minutos, "incapacitados", sep = "_")]] <- 0
    }
    do.call(session$setInputs, fixture)
    if (length(check(TRUE))) stop(paste(check(TRUE), collapse = "; "))
    stopifnot(isTRUE(save()), !isTRUE(save())) # reject duplicate completed codes
    complete_files <- list.files(folder, pattern = "^captura_.*json$", full.names = TRUE)
    complete_files <- complete_files[vapply(complete_files, function(f) {
      identical(jsonlite::read_json(f)$fila$codigo_bioensayo, paste0("PRUEBA-COMPLETA-", mode))
    }, logical(1))]
    stopifnot(length(complete_files) == 1L)
    expected_row <- current(); expected_row$estado_captura <- "completo_local"
    complete_row <- f7_component_row(mode, jsonlite::read_json(complete_files[[1]])$fila)
    sheet_row <- f7_component_row(f7_storage_key(mode), jsonlite::read_json(complete_files[[1]])$fila)
    exported <- f7_local_sheet(mode, tmp)
    exported <- exported[exported$codigo_bioensayo == paste0("PRUEBA-COMPLETA-", mode), , drop = FALSE]
    row.names(exported) <- NULL
    row.names(sheet_row) <- NULL
    stopifnot(identical(complete_row, expected_row), identical(exported, sheet_row))
    csv <- tempfile(fileext = ".csv")
    write.csv(sheet_row, csv, row.names = FALSE, na = "")
    back <- read.csv(csv, colClasses = "character", check.names = FALSE)
    stopifnot(back$codigo_departamento == "01")
    if (mode == "sinergistas") {
      stopifnot(back$componente == "sinergistas", back$estado_captura == "completo_local", "etanol_bioensayo_e1_30min_vivos" %in% names(back))
    } else {
      stopifnot(!"componente" %in% names(back), identical(names(back), formulario_7_csv_columns))
    }
    unlink(csv)
  })
}
actual_sheet <- f7_local_sheet("actual", tmp)
sinergistas_sheet <- f7_local_sheet("sinergistas", tmp)
stopifnot(nrow(actual_sheet) == 2L, nrow(sinergistas_sheet) == 1L)
stopifnot(all(actual_sheet$codigo_bioensayo %in% c("PRUEBA-COMPLETA-diagnostica", "PRUEBA-COMPLETA-intensidad")))
stopifnot(sinergistas_sheet$codigo_bioensayo == "PRUEBA-COMPLETA-sinergistas")
shiny::testServer(f7_component_server, args = list(mode = "flujo", directory = tmp), {
  session$setInputs(f7_tipo_bioensayo = "diagnostica_1x", f7_codigo_bioensayo = "PROYECTO-COMUN", f7_insecticida = "Deltametrina")
  stopifnot(current()$componente == "diagnostica", !"lectura_posterior" %in% steps(), steps()[[2]] == "tipo_bioensayo")
  session$setInputs(f7_tipo_bioensayo = "sinergistas")
  stopifnot(current()$componente == "sinergistas", "lectura_posterior" %in% steps(), current()$codigo_bioensayo == "PROYECTO-COMUN")
  session$setInputs(f7_tipo_bioensayo = "intensidad", f7_bioensayo_intensidad = "Exploratorio")
  stopifnot(current()$componente == "intensidad", !"lectura_posterior" %in% steps(), !"sinergista_tipo" %in% names(current()))
})
unlink(tmp, recursive = TRUE)
cat("PASS: independent modules, unique HTML IDs, schemas, complete-only persistence, incomplete/invalid capture protection\n")
