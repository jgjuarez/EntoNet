# Run from the repository root: Rscript scripts/test_f7_review_rpc.R
expressions <- parse("shiny_app/app.R")
env <- new.env(parent = baseenv())
targets <- c("supabase_record_from_row", "supabase_records_from_data_frame",
             "f7_update_review_record", "f7_confirm_review_record")
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
  body$p_intake_id
}
env$formulario_7_tables <- function(data) list(
  header = data,
  results = data.frame(fase = "bioensayo", botella = "b1", tiempo_minutos = 0L,
                       hora_lectura = NA_character_, vivos = 20L, incapacitados = 0L),
  comments = data.frame()
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

expect_error <- function(expression) {
  failed <- tryCatch({ force(expression); FALSE }, error = function(error) TRUE)
  stopifnot(failed)
}
env$supabase_private_rpc <- function(...) NULL
expect_error(env$f7_update_review_record(42L, row))
expect_error(env$f7_confirm_review_record(42L, "", ""))
env$supabase_private_rpc <- function(...) 99L
expect_error(env$f7_update_review_record(42L, row))
env$supabase_private_rpc <- function(...) stop("API unavailable")
expect_error(env$f7_update_review_record(42L, row))
expect_error(env$f7_confirm_review_record(42L, "", ""))
cat("PASS: F7 review payloads, confirmation, empty children and API failures\n")
