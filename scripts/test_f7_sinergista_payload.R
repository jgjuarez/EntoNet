# Run from the EntoNet repository root: Rscript scripts/test_f7_sinergista_payload.R
old_workdir <- getwd()
on.exit(setwd(old_workdir), add = TRUE)
setwd("shiny_app")
source("app.R", local = globalenv())

readings <- list()
for (set_name in c("sinergista", "etanol")) {
  for (bottle in paste0("e", 1:5)) readings[[length(readings) + 1L]] <- list(
    tipo_set = set_name, etapa = "pretratamiento", botella = bottle,
    tiempo_minutos = 60L, hora_inicio = "08:00", vivos = 20L, incapacitados = 0L
  )
  for (bottle in c(paste0("e", 1:4), "c1")) for (minutes in c(0L, 15L, 30L, 45L)) {
    treated <- bottle != "c1"
    readings[[length(readings) + 1L]] <- list(
      tipo_set = set_name, etapa = "bioensayo", botella = bottle,
      tiempo_minutos = minutes, hora_inicio = "08:00",
      vivos = if (identical(set_name, "etanol") && treated) 0L else 20L,
      incapacitados = if (identical(set_name, "etanol") && treated) 20L else 0L
    )
  }
}
payload <- list(
  version_estructura = "f7_sets_local_v1", estado = "borrador_local",
  sets = list(
    list(tipo_set = "sinergista", observaciones_pretratamiento = "Sin pre", observaciones_bioensayo = "Sin bio"),
    list(tipo_set = "etanol", observaciones_pretratamiento = "EtOH pre", observaciones_bioensayo = "EtOH bio")
  ),
  lecturas = readings
)
stopifnot(length(f7_sets_errors(payload, complete = TRUE)) == 0L)

row <- as.data.frame(setNames(rep(list(NA_character_), length(formulario_7_intake_columns)), formulario_7_intake_columns), stringsAsFactors = FALSE)
values <- list(
  formulario_codigo = "F7", formulario_nombre = "Prueba Sinergistas", fecha_registro = "2026-09-11",
  codigo_bioensayo = "TEST-SINERGISTA-INTEGRACION", nombre_poblacion = "Prueba", pais = "Guatemala",
  id_institucion = "UVG", codigo_departamento = "01", codigo_municipio = "01", sinergista_tipo = "PBO",
  dosis_sinergista_ug_ml = "100", fecha_realizacion_bioensayo = "2026-09-11", insecticida = "Deltametrina",
  solvente_utilizado = "Etanol", dosis_intensidad_ug_ml = "10", lote_insecticida = "TEST",
  fecha_revestimiento_botellas = "2026-09-11", origen_material = "Laboratorio", edad_indefinida = "true",
  codigo_especie_mosquito = "AE", fecha_separacion = "2026-09-11", hora_separacion = "08:00",
  generacion_filial_indefinida = "true", codigo_responsable_revestimiento = "TEST",
  codigo_responsable_bioensayo = "TEST", codigo_control_calidad = "NO APLICA",
  temperatura_inicial_c = "25", temperatura_final_c = "25", humedad_relativa_inicial_pct = "70",
  humedad_relativa_final_pct = "70", hora_inicio_bioensayo = "08:00", hora_final_bioensayo = "09:00",
  fuente_formulario = "Prueba", nombre_quien_ingreso = "Codex", comentario = "Comentario",
  comentario_nombre = "Prueba"
)
for (name in names(values)) row[[name]] <- values[[name]]

tables <- f7_sinergista_tables(row, payload)
stopifnot(
  identical(tables$header$version_estructura, "f7_sinergistas_v1"),
  identical(tables$header$sinergista_resultado_diagnostico, "Resistente"),
  identical(tables$header$etanol_resultado_diagnostico, "Susceptible"),
  length(tables$results) == 50L,
  length(tables$comments) == 1L,
  identical(tables$comments[[1]]$etanol_observaciones_bioensayo, "EtOH bio")
)
cat("PASS: payload de Sinergistas separa encabezado, 50 lecturas y observaciones de ambos sets\n")
