# Run from the repository root: Rscript scripts/test_f7_printable_xlsx.R
expressions <- parse("shiny_app/app.R")
env <- new.env(parent = globalenv())
targets <- c(
  "value_or_default", "f1_xml_escape", "f1_excel_col", "f1_excel_cell",
  "f1_excel_row", "f1_code39_value", "f1_entonet_logo_path",
  "f1_create_watermark_logo", "f1_write_file", "f7_printable_styles_xml",
  "f7_printable_sheet_xml", "f7_create_printable_xlsx"
)
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

env$f1_create_watermark_logo <- function(...) FALSE
output_file <- tempfile(fileext = ".xlsx")
env$f7_create_printable_xlsx(
  file = output_file,
  pais = "Guatemala",
  departamento = "Guatemala (01)",
  municipio = "Guatemala (0101)",
  codigo_bioensayo = "REI26GT0101P1SPBODEL1F1",
  nombre_poblacion = "Poblacion prueba",
  tipo_bioensayo = "Sinergista PBO",
  version_formulario = "3"
)
stopifnot(file.exists(output_file), file.info(output_file)$size > 0)

unzipped <- tempfile("f7_printable_")
dir.create(unzipped)
utils::unzip(output_file, exdir = unzipped)
workbook_xml <- paste(readLines(file.path(unzipped, "xl", "workbook.xml"), warn = FALSE), collapse = "")
sheet_xml <- paste(readLines(file.path(unzipped, "xl", "worksheets", "sheet1.xml"), warn = FALSE), collapse = "")

row_xml <- function(row_number) {
  match <- regexpr(paste0("<row r=\"", row_number, "\".*?</row>"), sheet_xml, perl = TRUE)
  stopifnot(match[[1]] > 0)
  regmatches(sheet_xml, match)
}

row11 <- row_xml(11)
row12 <- row_xml(12)
row15 <- row_xml(15)
row26 <- row_xml(26)
row33 <- row_xml(33)
stopifnot(
  !grepl("<t>Indefinida</t>", row11, fixed = TRUE),
  !grepl("<t>ug/mL</t>", row12, fixed = TRUE),
  !grepl("<t>Indefinida</t>", row15, fixed = TRUE),
  grepl("<t>E5</t>", row26, fixed = TRUE),
  !grepl("<t>C1</t>", row26, fixed = TRUE),
  grepl("<t>SinC1</t>", row33, fixed = TRUE),
  grepl("<t>EtOHC1</t>", row33, fixed = TRUE),
  grepl("Formulario 7'!$A$1:$N$42", workbook_xml, fixed = TRUE),
  !grepl("A45:N45", sheet_xml, fixed = TRUE),
  !grepl("<row r=\"45\"", sheet_xml, fixed = TRUE)
)

cat("PASS: Formulario 7 Sinergista printable xlsx matches version 3 structure\n")
