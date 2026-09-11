source("scripts/preview_f7_componentes_local.R")
fixture <- function(mortality, control = 0, insecticide = "Deltametrina") {
  row <- list(insecticida = insecticide)
  time <- f7_cdc_diagnostic_time(insecticide)
  for (b in c("b1", "b2", "b3", "b4", "c1")) {
    d <- if (b == "c1") control else mortality
    prefix <- f7_cdc_result_prefix(time, b)
    row[[paste0(prefix, "_vivos")]] <- 100 - d
    row[[paste0(prefix, "_incapacitados")]] <- d
  }
  row
}
for (m in c(98, 95, 90, 89)) {
  expected <- if (m >= 98) "Susceptible" else if (m >= 90) "Sospecha de Resistencia" else "Resistente"
  stopifnot(f7_component_analysis("diagnostica", fixture(m))[[1]]$resultado == expected)
}
stopifnot(f7_component_analysis("diagnostica", fixture(98, 21))[[1]]$resultado == "Ensayo inválido")
stopifnot(abs(f7_component_analysis("diagnostica", fixture(95, 10))[[1]]$mortalidad_pct - 94.4) < .01)
p <- fixture(98); p$resultado_30min_b4_vivos <- NULL
stopifnot(f7_component_analysis("diagnostica", p)[[1]]$resultado == "Pendiente")
p$bioensayo_intensidad <- "Exploratorio"
stopifnot(f7_component_analysis("intensidad", p)[[1]]$resultado == "Pendiente")
stopifnot(f7_component_analysis("diagnostica", fixture(98, insecticide = "DDT"))[[1]]$tiempo_minutos == 45)
stopifnot(f7_component_analysis("diagnostica", fixture(98, insecticide = "Temefos"))[[1]]$tiempo_minutos == 1440)
values <- list(insecticida = "Deltametrina")
for (set in c("sinergista", "etanol")) for (b in c("e1", "e2", "e3", "e4", "c1")) {
  d <- if (b == "c1") 0 else if (set == "sinergista") 98 else 80
  values[[paste0(set, "_bioensayo_", b, "_30min_vivos")]] <- 100 - d
  values[[paste0(set, "_bioensayo_", b, "_30min_incapacitados")]] <- d
}
a <- f7_component_analysis("sinergistas", values)
stopifnot(a[[1]]$resultado == "Susceptible", a[[2]]$resultado == "Resistente")
cat("PASS: classification boundaries, Abbott, invalid control, incomplete readings, insecticide time and independent sets\n")
