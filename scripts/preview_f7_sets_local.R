# Run from the repository root: Rscript scripts/preview_f7_sets_local.R
# Loads only UI definitions, never the production server, env files or credentials.
library(shiny)
source("shiny_app/f7_sets_local.R")
targets <- c("formulario_7_insecticide_choices", "formulario_7_bottles",
  "formulario_7_bottle_labels", "formulario_7_count_pair", "formulario_7_bottle_panel",
  "formulario_7_capture_form", "formulario_7_header_columns", "formulario_7_comment_columns")
for (expr in parse("shiny_app/app.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% targets) eval(expr)
}
default_institution_id <- ""
profile_name <- ""
ui <- fluidPage(
  tags$head(tags$style(HTML("body{background:#f6f8fa} .container-fluid{max-width:1250px;background:white;padding:24px} .table th{background:#edf4f0} .nav-tabs{margin-bottom:20px} .table .form-group{margin-bottom:6px} .form-group:has(#f7_codigo_departamento){display:none}"))),
  h2("Formulario 7 · Captura local"),
  p("Prueba de colecta individual con Sinergista y Etanol. Los borradores se guardan en este equipo."),
  fileInput("restore_local", "Abrir un borrador local", accept = ".json"),
  formulario_7_capture_form(),
  downloadButton("download_local", "Descargar último borrador guardado")
)
server <- function(input, output, session) {
  status <- reactiveVal(NULL)
  saved <- reactiveVal(NULL)
  steps <- c("informacion_general", "informacion_bioensayo", "material_responsables", "condiciones", "resultados", "comentarios_envio")
  session$onFlushed(function() {
    updateRadioButtons(session, "f7_tipo_bioensayo", choices = c("Sinergistas" = "sinergistas"), selected = "sinergistas")
  }, once = TRUE)
  # Manual location codes in the offline preview, without a remote catalog.
  output$f7_codigo_municipio_ui <- renderUI(tagList(
    textInput("f7_departamento_local", "Código departamento *"),
    textInput("f7_codigo_municipio", "Código municipio *")
  ))
  output$f7_navigation_controls <- renderUI(div(
    actionButton("local_previous", "Anterior"), actionButton("local_next", "Siguiente")
  ))
  observeEvent(input$local_previous, {
    i <- match(input$f7_capture_tab, steps)
    if (!is.na(i) && i > 1) updateTabsetPanel(session, "f7_capture_tab", selected = steps[[i - 1]])
  })
  observeEvent(input$local_next, {
    i <- match(input$f7_capture_tab, steps)
    if (!is.na(i) && i < length(steps)) updateTabsetPanel(session, "f7_capture_tab", selected = steps[[i + 1]])
  })
  payload <- reactive({
    header <- setNames(lapply(formulario_7_header_columns, function(name) input[[paste0("f7_", name)]]), formulario_7_header_columns)
    header$codigo_departamento <- input$f7_departamento_local
    header$nombre_quien_ingreso <- input$f7_creado_por
    header$formulario_codigo <- "F7"
    header$sinergista_def <- identical(input$f7_sinergista_tipo, "DEF")
    header$sinergista_pbo <- identical(input$f7_sinergista_tipo, "PBO")
    header$sinergista_dm <- identical(input$f7_sinergista_tipo, "DM")
    header$comentario <- input$f7_comentario
    header$comentario_nombre <- input$f7_comentario_nombre
    f7_sets_collect(input, header)
  })
  output$f7_calculated_diagnostic_result_ui <- renderUI({
    p <- payload()
    done <- sum(vapply(p$lecturas, function(r) !is.null(r$vivos) && !is.null(r$incapacitados), logical(1)))
    div(class = "alert alert-info", paste(done, "de", length(p$lecturas), "lecturas capturadas. Puede guardar y volver a abrir un borrador incompleto."))
  })
  observeEvent(input$save_f7_sets_local, {
    tryCatch({
      path <- f7_sets_save_local(payload(), "output/f7_capturas_locales")
      saved(path)
      status(paste("Borrador guardado:", path))
    }, error = function(e) status(conditionMessage(e)))
  })
  output$f7_save_status <- renderUI({
    req(status())
    div(class = "alert alert-info", status())
  })
  output$download_local <- downloadHandler(
    filename = function() "formulario_7_borrador_local.json",
    content = function(file) { req(saved()); file.copy(saved(), file) }
  )
  observeEvent(input$restore_local, {
    tryCatch({
      p <- jsonlite::read_json(input$restore_local$datapath, simplifyVector = FALSE)
      if (!identical(p$version_estructura, "f7_sets_local_v1") || length(p$sets) != 2L || !length(p$lecturas)) stop("El archivo no es un borrador local de Formulario 7.")
      errors <- f7_sets_errors(p)
      if (length(errors)) stop(paste(errors, collapse = "\n"))
      # Clear current readings before restoring, including optional 24-hour fields.
      all <- f7_sets_collect(list(f7sets_incluir_24h = TRUE))
      for (r in all$lecturas) {
        prefix <- paste("f7sets", r$tipo_set, r$etapa, r$botella, sep = "_")
        updateTextInput(session, paste0(prefix, "_inicio"), value = "")
        for (field in c("vivos", "incapacitados")) updateNumericInput(session, paste(prefix, r$tiempo_minutos, field, sep = "_"), value = NA_real_)
      }
      for (name in names(p$encabezado)) {
        id <- paste0("f7_", name)
        if (name == "codigo_departamento") id <- "f7_departamento_local"
        if (name == "nombre_quien_ingreso") id <- "f7_creado_por"
        session$sendInputMessage(id, list(value = if (is.null(p$encabezado[[name]])) "" else p$encabezado[[name]]))
      }
      for (s in p$sets) {
        updateTextAreaInput(session, paste0("f7sets_", s$tipo_set, "observaciones_pre"), value = if (is.null(s$observaciones_pretratamiento)) "" else s$observaciones_pretratamiento)
        updateTextAreaInput(session, paste0("f7sets_", s$tipo_set, "observaciones_bio"), value = if (is.null(s$observaciones_bioensayo)) "" else s$observaciones_bioensayo)
      }
      updateCheckboxInput(session, "f7sets_incluir_24h", value = any(vapply(p$lecturas, function(r) r$etapa == "kdr_24h", logical(1))))
      for (r in p$lecturas) {
        prefix <- paste("f7sets", r$tipo_set, r$etapa, r$botella, sep = "_")
        updateTextInput(session, paste0(prefix, "_inicio"), value = if (is.null(r$hora_inicio)) "" else r$hora_inicio)
        for (field in c("vivos", "incapacitados")) updateNumericInput(session, paste(prefix, r$tiempo_minutos, field, sep = "_"), value = if (is.null(r[[field]])) NA_real_ else r[[field]])
      }
      status("Borrador recuperado. Puede continuar la colecta.")
    }, error = function(e) status(conditionMessage(e)))
  })
}
shiny::runApp(shinyApp(ui, server), host = "127.0.0.1", port = 3877, launch.browser = FALSE)
