# Run from the repository root: Rscript scripts/test_f7_review_rpc.R
expressions <- parse("shiny_app/app.R")
env <- new.env(parent = baseenv())
targets <- c("supabase_record_from_row", "supabase_records_from_data_frame",
             "f7_update_review_record", "f7_confirm_review_record",
             "f7_update_review_sinergista_record", "f7_confirm_review_sinergista_record")
collect <- function(node) {
  if (!is.call(node)) return(invisible(NULL))
  if (identical(node[[1]], as.name("<-")) && is.symbol(node[[2]]) &&
      as.character(node[[2]]) %in% targets) eval(node, env)
  if (identical(node[[1]], as.name("<-")) && identical(node[[2]], as.name("server"))) {
    for (statement in as.list(node[[3]][[3]])[-1]) collect(statement)
  }
}
for (expression in expressions) collect(expression)
stopifnot(all(vapply(targets, exists, logical(1), envir = env, inherits = FALSE)))

calls <- list()
env$supabase_private_rpc <- function(function_name, body) {
  calls[[length(calls) + 1L]] <<- list(name = function_name, body = body)
  if (!is.null(body$p_intake_id)) body$p_intake_id else body$p_sinergista_intake_id
}
env$formulario_7_tables <- function(data) list(
  header = data,
  results = data.frame(fase = "bioensayo", botella = "b1", tiempo_minutos = 0L,
                       hora_lectura = NA_character_, vivos = 20L, incapacitados = 0L),
  comments = data.frame()
)
env$f7_sinergista_tables <- function(row, payload, allow_missing_hours = FALSE) list(
  header = as.list(row),
  results = payload$lecturas,
  comments = payload$comentarios,
  allow_missing_hours = allow_missing_hours
)
env$connect_to_supabase <- function() stop("Direct PostgreSQL must not be used")
row <- data.frame(codigo_bioensayo = "TEST", solvente_otro = NA_character_)
env$f7_update_review_record(42L, row)
stopifnot(length(calls) == 1L,
          calls[[1]]$name == "entonet_update_formulario_7",
          calls[[1]]$body$p_intake_id == 42L,
          calls[[1]]$body$p_header$codigo_bioensayo == "TEST",
          is.null(calls[[1]]$body$p_header$solvente_otro),
          length(calls[[1]]$body$p_results) == 1L,
          length(calls[[1]]$body$p_comments) == 0L)
# Empty child lists must remain JSON arrays, not null or objects.
payload <- jsonlite::fromJSON(jsonlite::toJSON(calls[[1]]$body, auto_unbox = TRUE),
                              simplifyVector = FALSE)
stopifnot(identical(payload$p_comments, list()))
env$f7_confirm_review_record(42L, "Verificado", "Revisor")
stopifnot(calls[[2]]$name == "entonet_confirm_formulario_7",
          calls[[2]]$body$p_review_notes == "Verificado",
          calls[[2]]$body$p_reviewed_by == "Revisor")
syn_row <- data.frame(codigo_bioensayo = "SYN-TEST", stringsAsFactors = FALSE)
syn_payload <- list(
  lecturas = list(list(tipo_set = "sinergista", etapa = "bioensayo", botella = "e1",
                       tiempo_minutos = 30L, hora_inicio = "08:30", vivos = 5L, incapacitados = 15L)),
  comentarios = list(list(comentario = "Corregido"))
)
env$f7_update_review_sinergista_record(7L, syn_row, syn_payload)
stopifnot(calls[[3]]$name == "entonet_update_formulario_7_sinergista",
          calls[[3]]$body$p_sinergista_intake_id == 7L,
          calls[[3]]$body$p_header$codigo_bioensayo == "SYN-TEST",
          length(calls[[3]]$body$p_results) == 1L)
historical_syn_row <- transform(syn_row, version_estructura = "f7_sinergistas_historico_v1")
env$f7_update_review_sinergista_record(8L, historical_syn_row, syn_payload)
stopifnot(calls[[4]]$name == "entonet_update_formulario_7_sinergista",
          calls[[4]]$body$p_sinergista_intake_id == 8L,
          identical(calls[[4]]$body$p_header$version_estructura, "f7_sinergistas_historico_v1"))
env$f7_confirm_review_sinergista_record(7L, "Validado", "Revisor")
stopifnot(calls[[5]]$name == "entonet_confirm_formulario_7_sinergista",
          calls[[5]]$body$p_review_notes == "Validado",
          calls[[5]]$body$p_reviewed_by == "Revisor")

expect_error <- function(expression) {
  failed <- tryCatch({ force(expression); FALSE }, error = function(error) TRUE)
  stopifnot(failed)
}
env$supabase_private_rpc <- function(...) NULL
expect_error(env$f7_update_review_record(42L, row))
expect_error(env$f7_confirm_review_record(42L, "", ""))
expect_error(env$f7_update_review_sinergista_record(7L, syn_row, syn_payload))
expect_error(env$f7_confirm_review_sinergista_record(7L, "", ""))
env$supabase_private_rpc <- function(...) 99L
expect_error(env$f7_update_review_record(42L, row))
expect_error(env$f7_update_review_sinergista_record(7L, syn_row, syn_payload))
env$supabase_private_rpc <- function(...) stop("API unavailable")
expect_error(env$f7_update_review_record(42L, row))
expect_error(env$f7_confirm_review_record(42L, "", ""))
expect_error(env$f7_update_review_sinergista_record(7L, syn_row, syn_payload))
expect_error(env$f7_confirm_review_sinergista_record(7L, "", ""))
cat("PASS: F7 review payloads, Sinergistas editing, confirmation, empty children and API failures\n")
