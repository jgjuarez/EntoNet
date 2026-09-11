# Run from EntoNet: Rscript scripts/preview_f7_componentes_local.R
library(shiny)
source("shiny_app/f7_sets_local.R")
targets <- c("value_or_default", "formulario_7_insecticide_choices", "formulario_7_bottles",
  "formulario_7_bottle_labels", "formulario_7_count_pair", "formulario_7_capture_form",
  "formulario_7_header_columns", "formulario_7_comment_columns", "formulario_7_result_columns",
  "formulario_7_intake_columns", "formulario_7_csv_columns",
  "formulario_7_intensity_bottle_doses", "f7_cdc_diagnostic_time", "f7_cdc_result_prefix", "f7_cdc_count_value",
  "f7_cdc_classify_mortality", "f7_intensity_exploratory_analysis", "f7_cdc_analysis_for_row", "f7_diagnostic_capture_analysis")
for (expr in parse("shiny_app/app.R")) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) && is.symbol(expr[[2]]) && as.character(expr[[2]]) %in% targets) eval(expr)
}
source("shiny_app/f7_components_local.R")
ui <- fluidPage(
  tags$head(tags$style(HTML("body{background:#f4f7f6}.container-fluid{max-width:1300px;background:white;padding:24px}.nav-tabs{margin:18px 0}.table th{background:#edf4f0}.form-group:has(select[id$='f7_codigo_departamento']){display:none}.alert{white-space:normal;overflow-wrap:anywhere}"))),
  h2("Formulario 7 · Captura de bioensayo"),
  f7_component_ui("captura", "flujo")
)
server <- function(input, output, session) {
  f7_component_server("captura", "flujo")
}
if (sys.nframe() == 0L) shiny::runApp(shinyApp(ui, server), host = "127.0.0.1", port = 3877, launch.browser = FALSE)
