source("shiny_app/f7_sets_local.R")
empty <- f7_sets_collect(list())
stopifnot(length(empty$lecturas) == 50L, length(empty$sets) == 2L,
          length(f7_sets_errors(empty)) == 0L, length(f7_sets_errors(empty, TRUE)) > 0L)
input <- list()
for (r in empty$lecturas) {
  prefix <- paste("f7sets", r$tipo_set, r$etapa, r$botella, sep = "_")
  input[[paste0(prefix, "_inicio")]] <- if (r$etapa == "pretratamiento") "08:00" else "10:00"
  input[[paste(prefix, r$tiempo_minutos, "vivos", sep = "_")]] <- if (r$tipo_set == "etanol") 19 else 20
  input[[paste(prefix, r$tiempo_minutos, "incapacitados", sep = "_")]] <- if (r$tipo_set == "etanol") 1 else 0
}
p <- f7_sets_collect(input, list(codigo_bioensayo = "PRUEBA-LOCAL"))
stopifnot(length(f7_sets_errors(p, TRUE)) == 0L)
keys <- vapply(p$lecturas, function(r) paste(r$tipo_set, r$etapa, r$botella, r$tiempo_minutos), character(1))
stopifnot(!anyDuplicated(keys))
directory <- tempfile("f7_test_")
path <- f7_sets_save_local(p, directory)
readback <- jsonlite::read_json(path, simplifyVector = FALSE)
stopifnot(length(readback$lecturas) == 50L, readback$lecturas[[1]]$vivos == 20,
          readback$lecturas[[26]]$vivos == 19, readback$lecturas[[1]]$incapacitados == 0,
          readback$encabezado$codigo_bioensayo == "PRUEBA-LOCAL")
for (bad in list(-1, 0.5, Inf, "invalid")) {
  candidate <- p
  candidate$lecturas[[26]]$vivos <- bad
  stopifnot(length(f7_sets_errors(candidate)) > 0)
}
candidate <- p
candidate$lecturas[[1]]$incapacitados <- NULL
stopifnot(length(f7_sets_errors(candidate)) > 0)
candidate <- p
candidate$lecturas[[1]]$hora_inicio <- "25:99"
stopifnot(length(f7_sets_errors(candidate)) > 0)
input$f7sets_incluir_24h <- TRUE
stopifnot(length(f7_sets_collect(input)$lecturas) == 60L)
input$f7sets_incluir_24h <- FALSE
stopifnot(length(f7_sets_collect(input)$lecturas) == 50L)
unlink(directory, recursive = TRUE)
cat("PASS: two sets, 50/60 readings, distinct counts, zero/missing, validation and local JSON round-trip\n")
