# Offline component UI: reuse production fields in isolated Shiny modules.
f7_component_labels <- c(diagnostica = "Diagnóstica", intensidad = "Intensidad", sinergistas = "Sinergistas")
f7_sheet_labels <- c(actual = "Base actual: Diagnóstica e Intensidad", sinergistas = "Nueva base: Sinergistas")

f7_storage_key <- function(mode) {
  if (mode %in% c("diagnostica", "intensidad", "actual")) return("actual")
  if (identical(mode, "sinergistas")) return("sinergistas")
  stop("Componente de Formulario 7 no reconocido.")
}

f7_component_analysis <- function(mode, values) {
  analyse <- function(row, label) {
    time <- f7_cdc_diagnostic_time(row$insecticida)
    result <- list(grupo = label, resultado = "Pendiente", tiempo_minutos = time,
      mortalidad_pct = NA_real_, control_pct = NA_real_, detalle = "Complete las cinco botellas al tiempo diagnóstico.")
    if (is.na(time)) { result$detalle <- "Seleccione un insecticida con tiempo diagnóstico configurado."; return(result) }
    for (b in c("b1", "b2", "b3", "b4", "c1")) {
      prefix <- f7_cdc_result_prefix(time, b)
      counts <- vapply(c("vivos", "incapacitados"), function(field) {
        v <- suppressWarnings(as.numeric(row[[paste0(prefix, "_", field)]]))
        if (length(v) != 1) NA_real_ else v
      }, numeric(1))
      if (any(!is.finite(counts)) || any(counts < 0) || any(counts != floor(counts)) || sum(counts) <= 0) return(result)
    }
    a <- f7_cdc_analysis_for_row(row)
    result$resultado <- a$cdc_resultado
    result$mortalidad_pct <- a$cdc_mortalidad_corregida_pct
    result$control_pct <- a$cdc_mortalidad_control_pct
    result$detalle <- a$cdc_correccion
    result
  }
  if (mode == "sinergistas") return(lapply(c("sinergista", "etanol"), function(set) {
    row <- list(insecticida = values$insecticida)
    time <- f7_cdc_diagnostic_time(values$insecticida)
    if (!is.na(time)) for (i in 1:5) {
      old <- c("b1", "b2", "b3", "b4", "c1")[[i]]
      bottle <- c("e1", "e2", "e3", "e4", "c1")[[i]]
      stage <- if (time == 1440) "kdr_24h" else "bioensayo"
      for (metric in c("vivos", "incapacitados")) row[paste0(f7_cdc_result_prefix(time, old), "_", metric)] <- list(values[[paste(set, stage, bottle, paste0(time, "min"), metric, sep = "_")]])
    }
    analyse(row, if (set == "sinergista") "Sinergista" else "Etanol")
  }))
  base <- analyse(values, f7_component_labels[[mode]])
  if (mode == "intensidad" && identical(values$bioensayo_intensidad, "Exploratorio") && base$resultado != "Pendiente") {
    a <- f7_intensity_exploratory_analysis(values)
    base$resultado <- a$resultado_diagnostico
    base$detalle <- a$detalles
    base$mortalidad_pct <- NA_real_ # exploratory doses must not be pooled
  }
  list(base)
}

f7_local_sheet <- function(mode, directory = "output/f7_componentes") {
  sheet <- f7_storage_key(mode)
  files <- list.files(file.path(directory, sheet), pattern = "^captura_.*json$", full.names = TRUE)
  rows <- lapply(files, function(f) {
    p <- jsonlite::read_json(f)
    if (identical(sheet, "actual") && !p$componente %in% c("diagnostica", "intensidad")) stop("El archivo no corresponde a la base actual.")
    if (identical(sheet, "sinergistas") && !identical(p$componente, "sinergistas")) stop("El archivo no corresponde a la base de Sinergistas.")
    f7_component_row(sheet, p$fila)
  })
  if (length(rows)) do.call(rbind, rows) else f7_component_row(sheet, list())[FALSE, ]
}

f7_component_columns <- function(mode) {
  if (mode == "actual") return(formulario_7_csv_columns)
  specific <- c("bioensayo_diagnostica_1x", "bioensayo_intensidad", "dosis_intensidad",
    "sinergista_def", "sinergista_pbo", "sinergista_dm", "sinergista_tipo", "dosis_sinergista_ug_ml", "resultado_diagnostico")
  own <- switch(mode, diagnostica = c("bioensayo_diagnostica_1x", "resultado_diagnostico"),
    intensidad = c("bioensayo_intensidad", "dosis_intensidad", "resultado_diagnostico"),
    sinergistas = c("sinergista_tipo", "dosis_sinergista_ug_ml", "sinergista_resultado_diagnostico", "etanol_resultado_diagnostico", "sinergista_mortalidad_corregida_pct", "etanol_mortalidad_corregida_pct", "sinergista_mortalidad_control_pct", "etanol_mortalidad_control_pct"))
  common <- setdiff(formulario_7_header_columns, specific)
  results <- if (mode == "sinergistas") {
    p <- f7_sets_collect(list(f7sets_incluir_24h = TRUE))
    unique(unlist(lapply(p$lecturas, function(r) {
      prefix <- paste(r$tipo_set, r$etapa, r$botella, sep = "_")
      c(paste0(prefix, "_hora_inicio"), paste(prefix, paste0(r$tiempo_minutos, "min"), c("vivos", "incapacitados"), sep = "_"))
    })))
  } else grep("60min", formulario_7_result_columns, value = TRUE, invert = TRUE)
  c("version_estructura", "componente", "estado_captura", common, own,
    if (mode == "sinergistas") c("incluir_24h", "sinergista_observaciones_pretratamiento", "sinergista_observaciones_bioensayo", "etanol_observaciones_pretratamiento", "etanol_observaciones_bioensayo"),
    results, formulario_7_comment_columns)
}

f7_component_row <- function(mode, values) {
  columns <- f7_component_columns(mode)
  row <- setNames(lapply(columns, function(k) NA_character_), columns)
  for (k in intersect(names(values), columns)) {
    v <- values[[k]]
    if (!is.null(v) && length(v) && !is.na(v[[1]])) row[[k]] <- as.character(v[[1]])
  }
  if ("version_estructura" %in% names(row)) row$version_estructura <- "f7_componentes_v1"
  if ("componente" %in% names(row)) row$componente <- mode
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}

f7_standard_grid <- function(ns) {
  bottles <- c("b1", "b2", "b3", "b4", "c1")
  heading <- function() tags$thead(tags$tr(tags$th("Lectura"), lapply(seq_along(bottles), function(i) {
    tags$th(if (i == 5) "C1" else paste0("E", i),
      if (i < 5) conditionalPanel("input.f7_tipo_bioensayo == 'intensidad' && input.f7_bioensayo_intensidad == 'Exploratorio'",
        tags$small(c("1X", "2X", "5X", "10X")[[i]]), ns = ns))
  })))
  hours <- function(prefix, label) tags$tr(tags$th(label), lapply(bottles, function(b) tags$td(
    textInput(ns(paste0("f7_", prefix, b)), NULL, placeholder = "08:30", width = "105px"))))
  counts <- function(time, label) tags$tr(tags$th(label), lapply(bottles, function(b) tags$td(
    numericInput(ns(paste0("f7_resultado_", time, "_", b, "_vivos")), "Vivos", NA, min = 0, step = 1, width = "105px"),
    numericInput(ns(paste0("f7_resultado_", time, "_", b, "_incapacitados")), "Incapacitados", NA, min = 0, step = 1, width = "105px"))))
  tagList(
    conditionalPanel("input.f7_insecticida != 'Temefos'", ns = ns,
      tags$div(style = "overflow-x:auto;", tags$table(class = "table table-bordered", heading(), tags$tbody(
        hours("resultado_hora_inicio_", "Inicio (HH:MM)"),
        lapply(c(0, 15, 30, 45), function(t) counts(paste0(t, "min"), paste(t, "min"))))))),
    h4("Lectura a 24 horas"),
    tags$div(style = "overflow-x:auto;", tags$table(class = "table table-bordered", heading(), tags$tbody(
      hours("resultado_hora_lectura_24h_", "Hora de lectura (HH:MM)"), counts("24h", "24 horas"))))
  )
}

f7_component_ui <- function(id, mode) {
  ns <- NS(id)
  label <- if (mode == "flujo") "Formulario 7" else f7_component_labels[[mode]]
  sections <- list()
  env <- new.env(parent = environment(f7_component_ui))
  # Constructors namespace the reused form and conditional expressions correctly.
  for (name in c("textInput", "numericInput", "dateInput", "checkboxInput", "radioButtons", "selectInput", "textAreaInput", "uiOutput")) {
    env[[name]] <- local({
      constructor <- getExportedValue("shiny", name)
      function(inputId, ...) constructor(ns(inputId), ...)
    })
  }
  env$actionButton <- function(inputId, label, ...) {
    if (inputId == "save_formulario_7") label <- "Validar y guardar captura completa"
    shiny::actionButton(ns(inputId), label, ...)
  }
  env$tabsetPanel <- function(..., id = NULL) shiny::tabsetPanel(..., id = if (is.null(id)) NULL else ns(id))
  env$conditionalPanel <- function(condition, ...) shiny::conditionalPanel(condition, ..., ns = ns)
  env$radioButtons <- function(inputId, ...) {
    if (inputId == "f7_resultado_diagnostico") return(NULL)
    shiny::radioButtons(ns(inputId), ...)
  }
  env$tabPanel <- function(title, ..., value = title) {
    sections[[value]] <<- list(...)
    shiny::tabPanel(title, ..., value = value)
  }
  for (name in c("formulario_7_capture_form", "formulario_7_count_pair", "f7_sets_ui", "f7_sets_grid")) {
    env[[name]] <- get(name)
    environment(env[[name]]) <- env
  }
  env$all_sets_ui <- env$f7_sets_ui
  env$f7_sets_ui <- function() env$all_sets_ui(stages = "pretratamiento", tab_id = "f7_sets_pre_tab")
  # One DOM input per field; no duplicate hidden 24h input IDs.
  env$formulario_7_bottle_panel <- function(bottle) {
    tagList(
      conditionalPanel("input.f7_insecticida != 'Temefos'",
        textInput(paste0("f7_resultado_hora_inicio_", bottle), "Inicio (HH:MM)", placeholder = "08:30"),
        lapply(c(0, 15, 30, 45), function(t) formulario_7_count_pair(paste0("resultado_", t, "min_", bottle), paste(t, "minutos")))),
      h4("Lectura a 24 horas"),
      textInput(paste0("f7_resultado_hora_lectura_24h_", bottle), "Hora de lectura (HH:MM)", placeholder = "08:30"),
      formulario_7_count_pair(paste0("resultado_24h_", bottle), "24 horas")
    )
  }
  environment(env$formulario_7_bottle_panel) <- env
  env$default_institution_id <- ""
  env$profile_name <- ""
  localize <- function(tag) {
    if (inherits(tag, "shiny.tag")) {
      if (!is.null(tag$attribs[["for"]]) && !startsWith(tag$attribs[["for"]], ns(""))) tag$attribs[["for"]] <- ns(tag$attribs[["for"]])
      tag$children <- lapply(tag$children, localize)
    } else if (inherits(tag, "shiny.tag.list") || identical(class(tag), "list")) {
      tag[] <- lapply(tag, localize)
    } else if (is.character(tag) && length(tag) == 1L && startsWith(tag, "Complete los datos generales y las lecturas por botella.")) {
      tag <- paste("Complete las secciones de", label, "y revise sus lecturas antes de guardar la captura completa.")
    }
    tag
  }
  env$formulario_7_capture_form()
  general <- sections$informacion_general[[1]]$children
  material <- sections$material_responsables[[1]]$children
  conditions <- sections$condiciones[[1]]$children
  two_columns <- function(section) {
    children <- section$children
    if (length(children) < 2) return(shiny::fluidRow(section))
    split_at <- ceiling(length(children) / 2)
    shiny::fluidRow(
      shiny::column(6, children[seq_len(split_at)]),
      shiny::column(6, children[seq.int(split_at + 1L, length(children))])
    )
  }
  # Preserve the original controls, dividing the paired columns into paper sections.
  project <- general[[1]]
  type <- general[[2]]
  project$children <- c(project$children, tail(type$children, 1))
  type$children <- head(type$children, -1)
  form <- shiny::tabsetPanel(id = ns("f7_capture_tab"),
    shiny::tabPanel("1. Información del Proyecto", value = "informacion_general", two_columns(project)),
    shiny::tabPanel("2. Tipo de Bioensayo", value = "tipo_bioensayo", two_columns(type)),
    shiny::tabPanel("3. Información del Bioensayo", value = "informacion_bioensayo", sections$informacion_bioensayo),
    shiny::tabPanel("4. Material biológico", value = "material_biologico", two_columns(material[[1]])),
    shiny::tabPanel("5. Responsables", value = "responsables", two_columns(material[[2]])),
    shiny::tabPanel("6. Condiciones ambientales", value = "condiciones", two_columns(conditions[[1]])),
    shiny::tabPanel("7. Horario del Bioensayo", value = "horario", two_columns(conditions[[2]])),
    shiny::tabPanel("8. Resultados por botella", value = "resultados",
      env$conditionalPanel("input.f7_tipo_bioensayo == 'sinergistas'", env$f7_sets_ui()),
      env$conditionalPanel("input.f7_tipo_bioensayo != 'sinergistas'",
        uiOutput(ns("f7_bottle_totals_warning")), f7_standard_grid(ns))),
    shiny::tabPanel("9. Lectura posterior", value = "lectura_posterior", env$all_sets_ui(stages = c("bioensayo", "kdr_24h"), tab_id = "f7_sets_post_tab")),
    shiny::tabPanel("Revisión y guardado", value = "comentarios_envio", sections$comentarios_envio))
  tagList(
    tags$style(HTML(paste0(
      ".form-group:has(select[id='", ns("f7_codigo_departamento"), "']){display:none;}",
      "#", ns("f7_capture_tab"), " ~ .tab-content .table th{background:#edf4f0;}",
      "#", ns("f7_capture_tab"), " ~ .tab-content{padding-top:16px;}",
      "#", ns("f7_navigation_controls"), "{display:flex;gap:8px;justify-content:flex-end;margin:8px 0 14px 0;}",
      "#", ns("previous"), ",#", ns("next_step"), "{background:#008c8f;border-color:#008c8f;color:#fff;font-weight:700;}",
      "#", ns("previous"), ":hover,#", ns("next_step"), ":hover,#", ns("previous"), ":focus,#", ns("next_step"), ":focus{background:#006f72;border-color:#006f72;color:#fff;}"
    ))),
    p("Inicie con la información del proyecto. En la sección 2 seleccione el tipo de bioensayo para mostrar los campos correspondientes."),
    uiOutput(ns("f7_navigation_controls")),
    localize(form)
  )
}

f7_component_to_intake_row <- function(row, mode) {
  values <- setNames(rep(list(NA_character_), length(formulario_7_intake_columns)), formulario_7_intake_columns)
  for (field in intersect(names(row), names(values))) values[[field]] <- as.character(row[[field]][[1]])
  values$formulario_codigo <- "F7"
  values$formulario_nombre <- "Registro de datos del bioensayo de la botella CDC"
  values$bioensayo_diagnostica_1x <- as.character(identical(mode, "diagnostica"))
  values$sinergista_def <- as.character(identical(values$sinergista_tipo, "DEF"))
  values$sinergista_pbo <- as.character(identical(values$sinergista_tipo, "PBO"))
  values$sinergista_dm <- as.character(identical(values$sinergista_tipo, "DM"))
  values$codigo_bioensayo <- formulario_7_codigo_bioensayo_final(
    values$codigo_bioensayo, values$bioensayo_diagnostica_1x,
    values$bioensayo_intensidad, values$dosis_intensidad,
    values$sinergista_def, values$sinergista_pbo, values$sinergista_dm
  )
  as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

f7_component_server <- function(id, mode, directory = "output/f7_componentes", save_capture = NULL) {
  moduleServer(id, function(input, output, session) {
    folder <- file.path(directory, f7_storage_key(if (mode == "flujo") "diagnostica" else mode))
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    mode_value <- reactive({
      if (mode != "flujo") return(mode)
      switch(value_or_default(input$f7_tipo_bioensayo, "diagnostica_1x"), diagnostica_1x = "diagnostica", intensidad = "intensidad", sinergistas = "sinergistas", "diagnostica")
    })
    status <- reactiveVal("Cada componente guarda sus propios archivos locales. La sábana incluye únicamente capturas completas.")
    tick <- reactiveVal(0)
    steps <- reactive(c("informacion_general", "tipo_bioensayo", "informacion_bioensayo", "material_biologico", "responsables", "condiciones", "horario", "resultados", if (mode_value() == "sinergistas") "lectura_posterior", "comentarios_envio"))
    observeEvent(mode_value(), {
      if (mode_value() == "sinergistas") showTab(inputId = "f7_capture_tab", target = "lectura_posterior", session = session)
      else hideTab(inputId = "f7_capture_tab", target = "lectura_posterior", session = session)
    })
    session$onFlushed(function() {
      if (mode != "flujo") updateRadioButtons(session, "f7_tipo_bioensayo", choices = setNames(if (mode == "diagnostica") "diagnostica_1x" else mode, f7_component_labels[[mode]]))
      updateActionButton(session, "save_formulario_7", label = "Validar y guardar captura completa")
      updateTextInput(session, "f7_codigo_control_calidad", value = "NO APLICA")
    }, once = TRUE)
    output$f7_codigo_municipio_ui <- renderUI(tagList(
      textInput(session$ns("departamento"), "Código departamento *"),
      textInput(session$ns("f7_codigo_municipio"), "Código municipio *")))
    output$f7_navigation_controls <- renderUI(tagList(
      actionButton(session$ns("previous"), "Anterior"),
      actionButton(session$ns("next_step"), "Siguiente")
    ))
    observeEvent(input$previous, { s <- steps(); i <- match(input$f7_capture_tab, s); if (!is.na(i) && i > 1) updateTabsetPanel(session, "f7_capture_tab", selected = s[[i - 1]]) })
    observeEvent(input$next_step, { s <- steps(); i <- match(input$f7_capture_tab, s); if (!is.na(i) && i < length(s)) updateTabsetPanel(session, "f7_capture_tab", selected = s[[i + 1]]) })
    current <- reactive({
      mode <- mode_value()
      values <- reactiveValuesToList(input)
      names(values) <- sub("^f7_", "", names(values))
      for (k in setdiff(c(formulario_7_header_columns, formulario_7_result_columns), names(values))) values[[k]] <- NA_character_
      values$codigo_departamento <- input$departamento
      values$nombre_quien_ingreso <- input$f7_creado_por
      values$formulario_codigo <- "F7"
      values$formulario_nombre <- "Registro de datos del bioensayo de la botella CDC"
      values$bioensayo_diagnostica_1x <- mode == "diagnostica"
      if (mode == "sinergistas") {
        p <- f7_sets_collect(input)
        values$incluir_24h <- isTRUE(input$f7sets_incluir_24h)
        for (r in p$lecturas) {
          prefix <- paste(r$tipo_set, r$etapa, r$botella, sep = "_")
          values[paste0(prefix, "_hora_inicio")] <- list(r$hora_inicio)
          for (field in c("vivos", "incapacitados")) values[paste(prefix, paste0(r$tiempo_minutos, "min"), field, sep = "_")] <- list(r[[field]])
        }
        for (s in p$sets) for (field in c("observaciones_pretratamiento", "observaciones_bioensayo")) values[paste(s$tipo_set, field, sep = "_")] <- list(s[[field]])
      } else {
        if (identical(input$f7_insecticida, "Temefos")) for (k in grep("resultado_(hora_inicio|[0-9]+min)", names(values), value = TRUE)) values[k] <- list(NULL)
        analysis <- if (mode == "intensidad" && identical(input$f7_bioensayo_intensidad, "Exploratorio")) f7_intensity_exploratory_analysis(values) else f7_diagnostic_capture_analysis(values)
        values$resultado_diagnostico <- analysis$resultado_diagnostico
        if (mode == "intensidad" && identical(input$f7_bioensayo_intensidad, "Exploratorio")) values$dosis_intensidad <- analysis$dosis_intensidad
      }
      analyses <- f7_component_analysis(mode, values)
      if (mode == "sinergistas") {
        for (i in seq_along(analyses)) {
          prefix <- c("sinergista", "etanol")[[i]]
          a <- analyses[[i]]
          values[[paste0(prefix, "_resultado_diagnostico")]] <- if (a$resultado == "Pendiente") NA_character_ else a$resultado
          values[[paste0(prefix, "_mortalidad_corregida_pct")]] <- a$mortalidad_pct
          values[[paste0(prefix, "_mortalidad_control_pct")]] <- a$control_pct
        }
      } else values$resultado_diagnostico <- if (analyses[[1]]$resultado == "Pendiente") NA_character_ else analyses[[1]]$resultado
      f7_component_row(mode, values)
    })
    check <- function(complete = FALSE) {
      mode <- mode_value()
      row <- current()
      errors <- character()
      if (mode == "sinergistas") errors <- f7_sets_errors(f7_sets_collect(input), complete)
      if (mode == "sinergistas" && formulario_7_is_temefos(row$insecticida[[1]])) {
        errors <- c(errors, "Temefos solo puede capturarse para Diagnóstica e Intensidad.")
      }
      for (k in names(row)) {
        v <- row[[k]][[1]]
        if (is.na(v) || !nzchar(v)) next
        numeric_field <- grepl("(vivos|incapacitados)$|^numero_usos|^edad_dias$|^dosis_.*ug_ml$|^temperatura_|^humedad_", k)
        if (numeric_field) {
          n <- suppressWarnings(as.numeric(v))
          if (!is.finite(n) || (!startsWith(k, "temperatura_") && n < 0) ||
              (grepl("(vivos|incapacitados)$|^numero_usos|^edad_dias$", k) && n != floor(n)) ||
              (startsWith(k, "humedad_") && n > 100)) errors <- c(errors, paste("Valor inválido:", k))
        }
        if (grepl("hora_|_hora_inicio$", k) && !grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", v)) errors <- c(errors, paste("Use HH:MM:", k))
      }
      if (mode != "sinergistas") {
        for (base in sub("_vivos$", "", grep("_vivos$", names(row), value = TRUE))) {
          a <- row[[paste0(base, "_vivos")]]; b <- row[[paste0(base, "_incapacitados")]]
          if (xor(is.na(a), is.na(b))) errors <- c(errors, paste("Complete vivos e incapacitados:", base))
        }
      }
      if (complete) {
        required <- c("codigo_bioensayo", "pais", "id_institucion", "codigo_departamento", "codigo_municipio", "nombre_poblacion", "nombre_quien_ingreso", "fecha_registro", "fecha_realizacion_bioensayo", "insecticida", "solvente_utilizado", "dosis_intensidad_ug_ml", "lote_insecticida", "fecha_revestimiento_botellas", "origen_material", "codigo_especie_mosquito", "fecha_separacion", "hora_separacion", "codigo_responsable_revestimiento", "codigo_responsable_bioensayo", "temperatura_inicial_c", "temperatura_final_c", "humedad_relativa_inicial_pct", "humedad_relativa_final_pct", "hora_inicio_bioensayo", "hora_final_bioensayo")
        if (!isTRUE(input$f7_edad_indefinida)) required <- c(required, "edad_dias")
        if (!isTRUE(input$f7_generacion_filial_indefinida)) required <- c(required, "generacion_filial")
        if (identical(input$f7_solvente_utilizado, "Otro")) required <- c(required, "solvente_otro")
        if (mode == "sinergistas") required <- c(required, "sinergista_tipo", "dosis_sinergista_ug_ml", "sinergista_resultado_diagnostico", "etanol_resultado_diagnostico")
        if (mode == "intensidad") required <- c(required, "bioensayo_intensidad", if (identical(input$f7_bioensayo_intensidad, "Completa")) "dosis_intensidad")
        if (mode != "sinergistas") {
          required <- c(required, "codigo_revision_24h", "resultado_diagnostico", grep("^resultado_", names(row), value = TRUE))
          if (identical(input$f7_insecticida, "Temefos")) required <- setdiff(required, grep("resultado_(hora_inicio|[0-9]+min)", required, value = TRUE))
        } else if (isTRUE(input$f7sets_incluir_24h)) required <- c(required, "codigo_revision_24h")
        for (k in unique(required)) if (is.null(row[[k]]) || is.na(row[[k]]) || !nzchar(row[[k]])) errors <- c(errors, paste("Falta:", k))
      }
      unique(errors)
    }
    output$f7_calculated_diagnostic_result_ui <- renderUI({
      mode <- mode_value()
      row <- current()
      tagList(h4("Resultado calculado del bioensayo"), lapply(f7_component_analysis(mode, as.list(row)), function(a) {
        div(class = if (a$resultado == "Susceptible") "alert alert-success" else if (a$resultado == "Resistente" || a$resultado == "Ensayo inválido") "alert alert-danger" else "alert alert-warning",
          strong(paste0(a$grupo, ": ", a$resultado)),
          p(paste("Tiempo diagnóstico:", if (is.na(a$tiempo_minutos)) "pendiente" else paste(a$tiempo_minutos, "minutos"))),
          if (is.finite(a$mortalidad_pct)) p(paste("Mortalidad corregida:", a$mortalidad_pct, "% · Control:", a$control_pct, "%")),
          p(a$detalle))
      }))
    })
    output$f7_bottle_totals_warning <- renderUI({
      mode <- mode_value()
      if (mode != "intensidad") return(NULL)
      div(class = "alert alert-info", if (identical(input$f7_bioensayo_intensidad, "Exploratorio"))
        "Exploratorio: botella 1 = 1X; botella 2 = 2X; botella 3 = 5X; botella 4 = 10X; C1 = control."
        else paste("Completa: las cuatro botellas experimentales corresponden a", input$f7_dosis_intensidad, "; C1 = control."))
    })
    output$f7_save_status <- renderUI(div(class = "alert alert-info", status()))
    save <- function() {
      mode <- mode_value()
      folder <- file.path(directory, f7_storage_key(mode))
      dir.create(folder, recursive = TRUE, showWarnings = FALSE)
      errors <- check(TRUE)
      if (length(errors)) { status(paste(errors, collapse = " · ")); return(invisible(FALSE)) }
      row <- current()
      if (is.function(save_capture)) {
        payload <- if (identical(mode, "sinergistas")) f7_sets_collect(input) else NULL
        result <- tryCatch(
          save_capture(mode, f7_component_to_intake_row(row, mode), payload),
          error = function(error) error
        )
        if (inherits(result, "error")) { status(paste("No se pudo guardar en Supabase:", conditionMessage(result))); return(invisible(FALSE)) }
        status(value_or_default(result$message, "Captura completa guardada en Supabase."))
        return(invisible(TRUE))
      }
      row$estado_captura <- "completo_local"
      files <- list.files(folder, pattern = "^captura_.*json$", full.names = TRUE)
      codes <- vapply(files, function(f) jsonlite::read_json(f)$fila$codigo_bioensayo, character(1))
      if (row$codigo_bioensayo %in% codes) { status("Ese código ya tiene una captura completa en este componente."); return(invisible(FALSE)) }
      file <- tempfile("captura_", folder, ".json")
      snapshot <- reactiveValuesToList(input)
      keep <- grepl("^f7_|^f7sets_|^departamento$", names(snapshot))
      snapshot <- snapshot[keep]
      jsonlite::write_json(list(version = "f7_componentes_v1", componente = mode, fila = as.list(row), inputs = snapshot), file, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null")
      tick(tick() + 1)
      status(paste("Captura completa guardada localmente:", normalizePath(file)))
    }
    observeEvent(input$save_formulario_7, save())
  })
}
