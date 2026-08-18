library(shiny)
library(DBI)
library(RPostgres)
library(httr2)
library(leaflet)

read_local_env_value <- function(name) {
  value <- Sys.getenv(name, unset = "")

  if (nzchar(value)) {
    return(value)
  }

  candidate_env_files <- c(
    normalizePath(".env.local", mustWork = FALSE),
    normalizePath(".env", mustWork = FALSE),
    normalizePath("../.env.local", mustWork = FALSE),
    normalizePath("../.env", mustWork = FALSE),
    normalizePath("../../.env.local", mustWork = FALSE)
  )

  for (local_env in candidate_env_files[file.exists(candidate_env_files)]) {
    lines <- readLines(local_env, warn = FALSE)
    assignment <- lines[startsWith(lines, paste0(name, "="))]
    if (length(assignment) > 0) {
      return(sub(paste0("^", name, "="), "", assignment[[1]]))
    }
  }

  ""
}

load_local_db_url <- function() {
  read_local_env_value("SUPABASE_DB_URL")
}

db_url <- load_local_db_url()
skip_login <- tolower(read_local_env_value("ENTONET_SKIP_LOGIN")) %in% c("1", "true", "yes", "si", "sí")
profile_name <- read_local_env_value("PROJECT_REI_PROFILE_NAME")
profile_institution <- read_local_env_value("PROJECT_REI_PROFILE_INSTITUTION")
profile_position <- read_local_env_value("PROJECT_REI_PROFILE_POSITION")
profile_country <- read_local_env_value("PROJECT_REI_PROFILE_COUNTRY")
support_email <- read_local_env_value("PROJECT_REI_SUPPORT_EMAIL")
supabase_url <- read_local_env_value("SUPABASE_URL")
auth_redirect_url <- read_local_env_value("ENTONET_AUTH_REDIRECT_URL")
supabase_anon_key <- read_local_env_value("SUPABASE_ANON_KEY")
if (!nzchar(supabase_anon_key)) {
  supabase_anon_key <- read_local_env_value("SUPABASE_PUBLISHABLE_KEY")
}
supabase_service_role_key <- read_local_env_value("SUPABASE_SERVICE_ROLE_KEY")
supabase_auth_api_key <- supabase_anon_key
if (!nzchar(supabase_auth_api_key)) {
  supabase_auth_api_key <- supabase_service_role_key
}

value_or_default <- function(value, default) {
  if (!is.null(value) && length(value) > 0 && !is.na(value[[1]]) && nzchar(as.character(value[[1]]))) {
    return(as.character(value[[1]]))
  }

  default
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x[[1]]) || !nzchar(as.character(x[[1]]))) {
    return(y)
  }

  x
}

default_institution_id <- value_or_default(read_local_env_value("ENTONET_DEFAULT_INSTITUTION_ID"), "UVG")

formulario_7_insecticide_choices <- c(
  "Seleccione" = "",
  "DDT" = "DDT",
  "Permetrina" = "Permetrina",
  "Deltametrina" = "Deltametrina",
  "Bendiocarb" = "Bendiocarb",
  "Malatión" = "Malatión",
  "Alfa-cipermetrina" = "Alfa-cipermetrina",
  "Lambda-cialotrina" = "Lambda-cialotrina",
  "Temefos" = "Temefos"
)

tr <- function(language, spanish, english) {
  if (identical(language, "en")) english else spanish
}

display_country <- function(country, language) {
  if (identical(language, "en")) {
    return(switch(
      country,
      "Belice" = "Belize",
      "República Dominicana" = "Dominican Republic",
      country
    ))
  }
  country
}

storage_project_url <- function() {
  configured_url <- sub("/+$", "", supabase_url)
  if (nzchar(configured_url) && !grepl("PROJECT_REF", configured_url, fixed = TRUE)) {
    return(configured_url)
  }

  matches <- regmatches(
    db_url,
    regexec("^postgres(?:ql)?://postgres\\.([a-z0-9]+):", db_url)
  )[[1]]

  if (length(matches) > 1) {
    return(paste0("https://", matches[[2]], ".supabase.co"))
  }

  ""
}

download_storage_object <- function(bucket, object_path, destination_file) {
  project_url <- storage_project_url()

  if (!nzchar(project_url)) {
    stop("SUPABASE_URL is not configured and could not be derived from SUPABASE_DB_URL.")
  }

  if (!nzchar(supabase_service_role_key)) {
    stop("SUPABASE_SERVICE_ROLE_KEY is not configured.")
  }

  request_url <- paste0(
    project_url,
    "/storage/v1/object/",
    bucket,
    "/",
    utils::URLencode(object_path, reserved = TRUE)
  )

  response <- request(request_url) |>
    req_headers(
      Authorization = paste("Bearer", supabase_service_role_key),
      apikey = supabase_service_role_key
    ) |>
    req_error(is_error = function(response) FALSE) |>
    req_perform(path = destination_file)

  if (resp_status(response) >= 300) {
    stop(
      sprintf(
        "No se pudo descargar el archivo desde Supabase Storage. HTTP %s.",
        resp_status(response)
      )
    )
  }
}

country_choices <- c(
  "Belice",
  "Guatemala",
  "El Salvador",
  "Honduras",
  "Nicaragua",
  "Costa Rica",
  "Panamá",
  "República Dominicana"
)

laboratory_protocols <- data.frame(
  id = "bottle_washing_sop",
  title = "Bottle Washing SOP",
  description = "Procedimiento operativo estándar de ejemplo para lavado de botellas.",
  file_name = "Bottle Washing SOP.docx",
  storage_path = "Laboratorio/Bottle Washing SOP.docx",
  content_type = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  stringsAsFactors = FALSE
)

sample_collection_sites <- data.frame(
  country = c("Guatemala", "Guatemala"),
  dataset = c("oviposition", "oviposition"),
  site = c("Coatepeque", "Jutiapa"),
  municipality = c("Coatepeque", "Jutiapa"),
  department = c("Quetzaltenango", "Jutiapa"),
  latitude = c(14.7040, 14.2917),
  longitude = c(-91.8640, -89.8958),
  records = c(18L, 12L),
  stringsAsFactors = FALSE
)

# Ubicaciones aproximadas para el prototipo de visualización del Formulario 7.
# En el modelo definitivo estas coordenadas serán reemplazadas por los puntos
# reales relacionados desde los formularios de colecta y crianza.
formulario_7_visualization_locations <- data.frame(
  pais = c("El Salvador", "Guatemala", "Guatemala", "Guatemala"),
  codigo_municipio = c("0102", "0920", "0921", "0922"),
  latitude = c(13.8617, 14.7040, 14.6196, 14.6325),
  longitude = c(-89.8016, -91.8640, -91.8356, -91.8654),
  stringsAsFactors = FALSE
)

network_country_summary <- data.frame(
  country = country_choices,
  latitude = c(17.1899, 15.7835, 13.7942, 15.2000, 12.8654, 9.7489, 8.5380, 18.7357),
  longitude = c(-88.4976, -90.2308, -88.8965, -86.2419, -85.2072, -83.7534, -80.7821, -70.1627),
  collection_sites = c(8L, 18L, 12L, 15L, 10L, 14L, 16L, 11L),
  municipalities = c(4L, 8L, 6L, 7L, 5L, 7L, 8L, 6L),
  records = c(460L, 1240L, 780L, 960L, 620L, 850L, 1030L, 710L),
  active_projects = c(2L, 4L, 3L, 3L, 2L, 3L, 4L, 3L),
  stringsAsFactors = FALSE
)

network_collection_sites <- data.frame(
  country = c(
    "Belice", "Belice", "Guatemala", "Guatemala", "El Salvador", "El Salvador", "Honduras",
    "Honduras", "Nicaragua", "Nicaragua", "Costa Rica", "Costa Rica",
    "Panamá", "Panamá", "República Dominicana", "República Dominicana"
  ),
  site = c(
    "Belize City", "Belmopan", "Coatepeque", "Jutiapa", "San Salvador", "Santa Ana", "Tegucigalpa",
    "San Pedro Sula", "Managua", "León", "San José", "Limón",
    "Ciudad de Panamá", "David", "Santo Domingo", "Santiago"
  ),
  latitude = c(
    17.5046, 17.2510, 14.7040, 14.2917, 13.6929, 13.9942, 14.0723, 15.5007, 12.1140,
    12.4379, 9.9281, 9.9907, 8.9824, 8.4273, 18.4861, 19.4517
  ),
  longitude = c(
    -88.1962, -88.7590, -91.8640, -89.8958, -89.2182, -89.5597, -87.1921, -88.0250, -86.2362,
    -86.8780, -84.0907, -83.0359, -79.5199, -82.4309, -69.9312, -70.6970
  ),
  stringsAsFactors = FALSE
)

network_collaborators <- data.frame(
  institution = c("UVG", "CECOMISCA", "UNAH", "INCIENSA", "INDICASAT-AIP"),
  country = c("Guatemala", "El Salvador", "Honduras", "Costa Rica", "Panamá"),
  city = c("Ciudad de Guatemala", "San Salvador", "Tegucigalpa", "Tres Ríos", "Ciudad de Panamá"),
  latitude = c(14.6047, 13.6929, 14.0723, 9.9060, 9.0074),
  longitude = c(-90.4890, -89.2182, -87.1921, -83.9870, -79.5340),
  stringsAsFactors = FALSE
)

egg_count_intake_columns <- c(
  "country",
  "cycle",
  "round_number",
  "quadrant",
  "oviposition_code",
  "substrate_code",
  "collection_site",
  "placement_date",
  "removal_date",
  "count_date",
  "count_responsible_code",
  "intact_eggs",
  "hatched_eggs",
  "canoe_eggs",
  "unfertilized_eggs",
  "other_species_count",
  "notes"
)

egg_count_template <- data.frame(
  country = "Guatemala",
  cycle = 1L,
  round_number = 1L,
  quadrant = 1L,
  oviposition_code = "SV1001",
  substrate_code = "A",
  collection_site = "Sitio de ejemplo",
  placement_date = "2026-06-01",
  removal_date = "2026-06-03",
  count_date = "2026-06-03",
  count_responsible_code = "JGJ",
  intact_eggs = 0L,
  hatched_eggs = 0L,
  canoe_eggs = 0L,
  unfertilized_eggs = 0L,
  other_species_count = 0L,
  notes = "Fila de ejemplo; elimine o reemplace antes de subir.",
  stringsAsFactors = FALSE
)

formulario_5_intake_columns <- c(
  "formulario_codigo",
  "pais",
  "id_institucion",
  "departamento_numero",
  "municipio_numero",
  "ciclo",
  "formulario_nombre",
  "fecha_registro",
  "cepa_poblacion",
  "especie",
  "generacion_filial_adultos",
  "responsable_ingreso_jaula",
  "fecha_jaula",
  "numero_hembras",
  "numero_machos",
  "total_huevos_viables",
  "responsable_alimentacion",
  "tipo_alimentacion_codigo",
  "tipo_alimentacion_descripcion",
  "fecha_alimentacion_sangre",
  "numero_charolas",
  "observaciones_alimentacion",
  "generacion_filial_huevos",
  "codigo_sustrato",
  "fecha_colocacion_sustrato",
  "fecha_retiro_sustrato",
  "numero_cuadro_sustrato",
  "hv_huevos_viables",
  "he_huevos_eclosionados",
  "hc_huevos_canoa",
  "hnf_huevos_no_fecundados",
  "responsable_conteo_huevos",
  "observaciones_generales",
  "fuente_formulario",
  "creado_por"
)

formulario_5_template <- data.frame(
  formulario_codigo = "F5",
  pais = "Guatemala",
  id_institucion = default_institution_id,
  departamento_numero = 1L,
  municipio_numero = 1L,
  ciclo = "Ciclo 3",
  formulario_nombre = "Alimentacion sanguinea y conteo huevecillos Aedes spp.",
  fecha_registro = "2026-07-29",
  cepa_poblacion = "Cepa de ejemplo",
  especie = "Ae. aegypti",
  generacion_filial_adultos = "F1",
  responsable_ingreso_jaula = "Iniciales",
  fecha_jaula = "2026-07-20",
  numero_hembras = 0L,
  numero_machos = 0L,
  total_huevos_viables = 0L,
  responsable_alimentacion = "Iniciales",
  tipo_alimentacion_codigo = "A",
  tipo_alimentacion_descripcion = "conejo",
  fecha_alimentacion_sangre = "2026-07-22",
  numero_charolas = 0L,
  observaciones_alimentacion = "",
  generacion_filial_huevos = "F1",
  codigo_sustrato = "S-001",
  fecha_colocacion_sustrato = "2026-07-23",
  fecha_retiro_sustrato = "2026-07-25",
  numero_cuadro_sustrato = 0L,
  hv_huevos_viables = 0L,
  he_huevos_eclosionados = 0L,
  hc_huevos_canoa = 0L,
  hnf_huevos_no_fecundados = 0L,
  responsable_conteo_huevos = "Iniciales",
  observaciones_generales = "Fila de ejemplo; elimine o reemplace antes de subir.",
  fuente_formulario = "",
  creado_por = "",
  stringsAsFactors = FALSE
)

formulario_7_departamento_catalogo <- data.frame(
  pais = c(rep("Guatemala", 22), rep("El Salvador", 14)),
  departamento_codigo = c(
    sprintf("%02d", 1:22),
    sprintf("%02d", 1:14)
  ),
  departamento = c(
    "Guatemala", "El Progreso", "Sacatepéquez", "Chimaltenango", "Escuintla", "Santa Rosa",
    "Sololá", "Totonicapán", "Quetzaltenango", "Suchitepéquez", "Retalhuleu", "San Marcos",
    "Huehuetenango", "Quiché", "Baja Verapaz", "Alta Verapaz", "Petén", "Izabal", "Zacapa",
    "Chiquimula", "Jalapa", "Jutiapa",
    "Ahuachapán", "Santa Ana", "Sonsonate", "Chalatenango", "La Libertad", "San Salvador",
    "Cuscatlán", "La Paz", "Cabañas", "San Vicente", "Usulután", "San Miguel", "Morazán", "La Unión"
  ),
  stringsAsFactors = FALSE
)

formulario_7_municipio_catalogo <- local({
  departamentos_gt <- c(
    "01" = "Guatemala", "02" = "El Progreso", "03" = "Sacatepéquez", "04" = "Chimaltenango",
    "05" = "Escuintla", "06" = "Santa Rosa", "07" = "Sololá", "08" = "Totonicapán",
    "09" = "Quetzaltenango", "10" = "Suchitepéquez", "11" = "Retalhuleu", "12" = "San Marcos",
    "13" = "Huehuetenango", "14" = "Quiché", "15" = "Baja Verapaz", "16" = "Alta Verapaz",
    "17" = "Petén", "18" = "Izabal", "19" = "Zacapa", "20" = "Chiquimula",
    "21" = "Jalapa", "22" = "Jutiapa"
  )
  municipios_gt <- c(
    "01|Guatemala;Santa Catarina Pinula;San José Pinula;San José del Golfo;Palencia;Chinautla;San Pedro Ayampuc;Mixco;San Pedro Sacatepéquez;San Juan Sacatepéquez;San Raymundo;Chuarrancho;Fraijanes;Amatitlán;Villa Nueva;Villa Canales;San Miguel Petapa",
    "02|Guastatoya;Morazán;San Agustín Acasaguastlán;San Cristóbal Acasaguastlán;El Jícaro;Sansare;Sanarate;San Antonio La Paz",
    "03|Antigua;Jocotenango;Pastores;Sumpango;Santo Domingo Xenacoj;Santiago Sacatepéquez;San Bartolomé Milpas Altas;San Lucas Sacatepéquez;Santa Lucía Milpas Altas;Magdalena Milpas Altas;Santa María de Jesús;Ciudad Vieja;San Miguel Dueñas;Alotenango;San Antonio Aguas Calientes;Santa Catarina Barahona",
    "04|Chimaltenango;San José Poaquil;San Martín Jilotepeque;San Juan Comalapa;Santa Apolonia;Tecpán Guatemala;Patzún;San Miguel Pochuta;Patzicía;Santa Cruz Balanyá;Acatenango;San Pedro Yepocapa;San Andrés Itzapa;Parramos;Zaragoza;El Tejar",
    "05|Escuintla;Santa Lucía Cotzumalguapa;La Democracia;Siquinalá;Masagua;Tiquisate;La Gomera;Guanagazapa;San José;Iztapa;Palín;San Vicente Pacaya;Nueva Concepción",
    "06|Cuilapa;Barberena;Santa Rosa de Lima;Casillas;San Rafael Las Flores;Oratorio;San Juan Tecuaco;Chiquimulilla;Taxisco;Santa María Ixhuatán;Guazacapán;Santa Cruz Naranjo;Pueblo Nuevo Viñas;Nueva Santa Rosa",
    "07|Sololá;San José Chacayá;Santa María Visitación;Santa Lucía Utatlán;Nahualá;Santa Catarina Ixtahuacán;Santa Clara La Laguna;Concepción;San Andrés Semetabaj;Panajachel;Santa Catarina Palopó;San Antonio Palopó;San Lucas Tolimán;Santa Cruz La Laguna;San Pablo La Laguna;San Marcos La Laguna;San Juan La Laguna;San Pedro La Laguna;Santiago Atitlán",
    "08|Totonicapán;San Cristóbal Totonicapán;San Francisco El Alto;San Andrés Xecul;Momostenango;Santa María Chiquimula;Santa Lucía La Reforma;San Bartolo Aguas Calientes",
    "09|Quetzaltenango;Salcajá;Olintepeque;San Carlos Sija;Sibilia;Cabricán;Cajolá;San Miguel Sigüilá;San Juan Ostuncalco;San Mateo;Concepción Chiquirichapa;San Martín Sacatepéquez;Almolonga;Cantel;Huitán;Zunil;Colomba Costa Cuca;San Francisco La Unión;El Palmar;Coatepeque;Génova Costa Cuca;Flores Costa Cuca;La Esperanza;Palestina de Los Altos",
    "10|Mazatenango;Cuyotenango;San Francisco Zapotitlán;San Bernardino;San José El Ídolo;Santo Domingo Suchitepéquez;San Lorenzo;Samayac;San Pablo Jocopilas;San Antonio Suchitepéquez;San Miguel Panán;San Gabriel;Chicacao;Patulul;Santa Bárbara;San Juan Bautista;Santo Tomás La Unión;Zunilito;Pueblo Nuevo;Río Bravo",
    "11|Retalhuleu;San Sebastián;Santa Cruz Muluá;San Martín Zapotitlán;San Felipe;San Andrés Villa Seca;Champerico;Nuevo San Carlos;El Asintal",
    "12|San Marcos;San Pedro Sacatepéquez;San Antonio Sacatepéquez;Comitancillo;San Miguel Ixtahuacán;Concepción Tutuapa;Tacaná;Sibinal;Tajumulco;Tejutla;San Rafael Pie de la Cuesta;Nuevo Progreso;El Tumbador;San José El Rodeo;Malacatán;Catarina;Ayutla (Tecún Umán);Ocós;San Pablo;El Quetzal;La Reforma;Pajapita;Ixchiguán;San José Ojetenam;San Cristóbal Cucho;Sipacapa;Esquipulas Palo Gordo;Río Blanco;San Lorenzo",
    "13|Huehuetenango;Chiantla;Malacatancito;Cuilco;Nentón;San Pedro Necta;Jacaltenango;San Pedro Soloma;San Ildefonso Ixtahuacán;Santa Bárbara;La Libertad;La Democracia;San Miguel Acatán;San Rafael La Independencia;Todos Santos Cuchumatán;San Juan Atitán;Santa Eulalia;San Mateo Ixtatán;Colotenango;San Sebastián Huehuetenango;Tectitán;Concepción Huista;San Juan Ixcoy;San Antonio Huista;San Sebastián Coatán;Santa Cruz Barillas;Aguacatán;San Rafael Petzal;San Gaspar Ixchil;Santiago Chimaltenango;Santa Ana Huista;Unión Cantinil",
    "14|Santa Cruz del Quiché;Chiché;Chinique;Zacualpa;Chajul;Santo Tomás Chichicastenango;Patzité;San Antonio Ilotenango;San Pedro Jocopilas;Cunén;San Juan Cotzal;Joyabaj;Nebaj;San Andrés Sajcabajá;San Miguel Uspantán;Sacapulas;San Bartolomé Jocotenango;Canillá;Chicamán;Ixcán;Pachalum",
    "15|Salamá;San Miguel Chicaj;Rabinal;Cubulco;Granados;Santa Cruz El Chol;San Jerónimo;Purulhá",
    "16|Cobán;Santa Cruz Verapaz;San Cristóbal Verapaz;Tactic;Tamahú;San Miguel Tucurú;Panzós;Senahú;San Pedro Carchá;San Juan Chamelco;Lanquín;Santa María Cahabón;Chisec;Chahal;Fray Bartolomé de Las Casas;La Tinta;Raxruhá",
    "17|Flores;San José;San Benito;San Andrés;La Libertad;San Francisco;Santa Ana;Dolores;San Luis;Sayaxché;Melchor de Mencos;Poptún",
    "18|Puerto Barrios;Livingston;El Estor;Morales;Los Amates",
    "19|Zacapa;Estanzuela;Río Hondo;Gualán;Teculután;Usumatlán;Cabañas;San Diego;La Unión;Huité",
    "20|Chiquimula;San José La Arada;San Juan La Ermita;Jocotán;Camotán;Olopa;Esquipulas;Concepción Las Minas;Quezaltepeque;San Jacinto;Ipala",
    "21|Jalapa;San Pedro Pinula;San Luis Jilotepeque;San Manuel Chaparrón;San Carlos Alzatate;Monjas;Mataquescuintla",
    "22|Jutiapa;El Progreso;Santa Catarina Mita;Agua Blanca;Asunción Mita;Yupiltepeque;Atescatempa;Jerez;El Adelanto;Zapotitlán;Comapa;Jalpatagua;Conguaco;Moyuta;Pasaco;San José Acatempa;Quesada"
  )
  guatemala_catalogo <- do.call(rbind, lapply(municipios_gt, function(entry) {
    parts <- strsplit(entry, "|", fixed = TRUE)[[1]]
    departamento_codigo <- parts[[1]]
    municipios <- strsplit(parts[[2]], ";", fixed = TRUE)[[1]]
    data.frame(
      pais = "Guatemala",
      departamento_codigo = departamento_codigo,
      departamento = unname(departamentos_gt[[departamento_codigo]]),
      municipio_codigo = paste0(departamento_codigo, sprintf("%02d", seq_along(municipios))),
      municipio = municipios,
      stringsAsFactors = FALSE
    )
  }))

  departamentos_sv <- c(
    "01" = "Ahuachapán", "02" = "Santa Ana", "03" = "Sonsonate", "04" = "Chalatenango",
    "05" = "La Libertad", "06" = "San Salvador", "07" = "Cuscatlán", "08" = "La Paz",
    "09" = "Cabañas", "10" = "San Vicente", "11" = "Usulután", "12" = "San Miguel",
    "13" = "Morazán", "14" = "La Unión"
  )
  municipios_sv <- c(
    "01|Ahuachapán;Apaneca;Atiquizaya;Concepción de Ataco;El Refugio;Guaymango;Jujutla;San Francisco Menéndez;San Lorenzo;San Pedro Puxtla;Tacuba;Turín",
    "02|Candelaria de la Frontera;Coatepeque;Chalchuapa;El Congo;El Porvenir;Masahuat;Metapán;San Antonio Pajonal;San Sebastián Salitrillo;Santa Ana;Santa Rosa Guachipilín;Santiago de la Frontera;Texistepeque",
    "03|Acajutla;Armenia;Caluco;Cuisnahuat;Santa Isabel Ishuatán;Izalco;Juayúa;Nahuizalco;Nahulingo;Salcoatitán;San Antonio del Monte;San Julián;Santa Catarina Masahuat;Santo Domingo de Guzmán;Sonsonate;Sonzacate",
    "04|Agua Caliente;Arcatao;Azacualpa;Citalá;Comalapa;Concepción Quezaltepeque;Chalatenango;Dulce Nombre de María;El Carrizal;El Paraíso;La Laguna;La Palma;La Reina;Las Vueltas;Nombre de Jesús;Nueva Concepción;Nueva Trinidad;Ojos de Agua;Potonico;San Antonio de la Cruz;San Antonio Los Ranchos;San Fernando;San Francisco Lempa;San Francisco Morazán;San Ignacio;San Isidro Labrador;San José Cancasque;San José Las Flores;San Luis del Carmen;San Miguel de Mercedes;San Rafael;Santa Rita;Tejutla",
    "05|Antiguo Cuscatlán;Ciudad Arce;Colón;Comasagua;Chiltiupán;Huizúcar;Jayaque;Jicalapa;La Libertad;Nuevo Cuscatlán;Santa Tecla;Quezaltepeque;Sacacoyo;San José Villanueva;San Juan Opico;San Matías;San Pablo Tacachico;Tamanique;Talnique;Teotepeque;Tepecoyo;Zaragoza",
    "06|Aguilares;Apopa;Ayutuxtepeque;Cuscatancingo;El Paisnal;Guazapa;Ilopango;Mejicanos;Nejapa;Panchimalco;Rosario de Mora;San Marcos;San Martín;San Salvador;Santiago Texacuangos;Santo Tomás;Soyapango;Tonacatepeque;Ciudad Delgado",
    "07|Candelaria;Cojutepeque;El Carmen;El Rosario;Monte San Juan;Oratorio de Concepción;San Bartolomé Perulapía;San Cristóbal;San José Guayabal;San Pedro Perulapán;San Rafael Cedros;San Ramón;Santa Cruz Analquito;Santa Cruz Michapa;Suchitoto;Tenancingo",
    "08|Cuyultitán;Rosario de La Paz;Jerusalén;Mercedes La Ceiba;Olocuilta;Paraíso de Osorio;San Antonio Masahuat;San Emigdio;San Francisco Chinameca;San Juan Nonualco;San Juan Talpa;San Juan Tepezontes;San Luis Talpa;San Miguel Tepezontes;San Pedro Masahuat;San Pedro Nonualco;San Rafael Obrajuelo;Santa María Ostuma;Santiago Nonualco;Tapalhuaca;Zacatecoluca;San Luis La Herradura",
    "09|Cinquera;Guacotecti;Ilobasco;Jutiapa;San Isidro;Sensuntepeque;Tejutepeque;Victoria;Dolores",
    "10|Apastepeque;Guadalupe;San Cayetano Istepeque;Santa Clara;Santo Domingo;San Esteban Catarina;San Ildefonso;San Lorenzo;San Sebastián;San Vicente;Tecoluca;Tepetitán;Verapaz",
    "11|Alegría;Berlín;California;Concepción Batres;El Triunfo;Ereguayquín;Estanzuelas;Jiquilisco;Jucuapa;Jucuarán;Mercedes Umaña;Nueva Granada;Ozatlán;Puerto El Triunfo;San Agustín;San Buenaventura;San Dionisio;Santa Elena;San Francisco Javier;Santa María;Santiago de María;Tecapán;Usulután",
    "12|Carolina;Ciudad Barrios;Comacarán;Chapeltique;Chinameca;Chirilagua;El Tránsito;Lolotique;Moncagua;Nueva Guadalupe;Nuevo Edén de San Juan;Quelepa;San Antonio del Mosco;San Gerardo;San Jorge;San Luis de la Reina;San Miguel;San Rafael Oriente;Sesori;Uluazapa",
    "13|Arambala;Cacaopera;Corinto;Chilanga;Delicias de Concepción;El Divisadero;El Rosario;Gualococti;Guatajiagua;Joateca;Jocoaitique;Jocoro;Lolotiquillo;Meanguera;Osicala;Perquín;San Carlos;San Fernando;San Francisco Gotera;San Isidro;San Simón;Sensembra;Sociedad;Torola;Yamabal;Yoloaiquín",
    "14|Anamorós;Bolívar;Concepción de Oriente;Conchagua;El Carmen;El Sauce;Intipucá;La Unión;Lislique;Meanguera del Golfo;Nueva Esparta;Pasaquina;Polorós;San Alejo;San José;Santa Rosa de Lima;Yayantique;Yucuaiquín"
  )
  el_salvador_catalogo <- do.call(rbind, lapply(municipios_sv, function(entry) {
    parts <- strsplit(entry, "|", fixed = TRUE)[[1]]
    departamento_codigo <- parts[[1]]
    municipios <- strsplit(parts[[2]], ";", fixed = TRUE)[[1]]
    data.frame(
      pais = "El Salvador",
      departamento_codigo = departamento_codigo,
      departamento = unname(departamentos_sv[[departamento_codigo]]),
      municipio_codigo = paste0(departamento_codigo, sprintf("%02d", seq_along(municipios))),
      municipio = municipios,
      stringsAsFactors = FALSE
    )
  }))

  rbind(guatemala_catalogo, el_salvador_catalogo)
})

ubicacion_departamento_catalogo <- formulario_7_departamento_catalogo
ubicacion_municipio_catalogo <- formulario_7_municipio_catalogo

ubicacion_normalizar_pais <- function(country) {
  country <- trimws(as.character(value_or_default(country, "")))
  country_upper <- toupper(chartr("ÁÉÍÓÚÜÑ", "AEIOUUN", country))
  if (country_upper %in% c("EL SALVADOR", "SALVADOR", "SV")) return("El Salvador")
  if (country_upper %in% c("GUATEMALA", "GT")) return("Guatemala")
  country
}

ubicacion_departamento_choices <- function(country) {
  country <- ubicacion_normalizar_pais(country)
  catalog <- ubicacion_departamento_catalogo[ubicacion_departamento_catalogo$pais == country, ]
  if (nrow(catalog) == 0) return(c("Seleccione país" = ""))
  c("Seleccione" = "", setNames(catalog$departamento_codigo, paste0(catalog$departamento, " (", catalog$departamento_codigo, ")")))
}

ubicacion_municipio_choices <- function(country, department_code, include_manual = TRUE) {
  country <- ubicacion_normalizar_pais(country)
  department_code <- trimws(as.character(value_or_default(department_code, "")))
  catalog <- ubicacion_municipio_catalogo[
    ubicacion_municipio_catalogo$pais == country &
      ubicacion_municipio_catalogo$departamento_codigo == department_code,
  ]
  choices <- c("Seleccione" = "")
  if (nrow(catalog) > 0) {
    choices <- c(choices, setNames(catalog$municipio_codigo, paste0(catalog$municipio, " (", catalog$municipio_codigo, ")")))
  }
  if (include_manual) choices <- c(choices, "Ingresar código manualmente" = "__manual__")
  choices
}

ubicacion_departamento_nombre <- function(country, department_code) {
  country <- ubicacion_normalizar_pais(country)
  department_code <- trimws(as.character(value_or_default(department_code, "")))
  catalog <- ubicacion_departamento_catalogo[
    ubicacion_departamento_catalogo$pais == country &
      ubicacion_departamento_catalogo$departamento_codigo == department_code,
  ]
  if (nrow(catalog) == 0) return(department_code)
  catalog$departamento[[1]]
}

ubicacion_municipio_nombre <- function(country, municipality_code) {
  country <- ubicacion_normalizar_pais(country)
  municipality_code <- trimws(as.character(value_or_default(municipality_code, "")))
  catalog <- ubicacion_municipio_catalogo[
    ubicacion_municipio_catalogo$pais == country &
      ubicacion_municipio_catalogo$municipio_codigo == municipality_code,
  ]
  if (nrow(catalog) == 0) return(municipality_code)
  catalog$municipio[[1]]
}

ubicacion_codigo_manual_o_seleccion <- function(selected, manual_value) {
  selected <- trimws(as.character(value_or_default(selected, "")))
  if (identical(selected, "__manual__") || !nzchar(selected)) {
    selected <- trimws(as.character(value_or_default(manual_value, "")))
  }
  gsub("[^0-9]+", "", selected)
}

ubicacion_normalizar_texto <- function(value) {
  if (is.null(value)) return("")
  value <- as.character(value)
  value[is.na(value)] <- ""
  value <- toupper(trimws(value))
  chartr("ÁÉÍÓÚÜÑ", "AEIOUUN", value)
}

extraer_puntos_geojson <- function(coordinates) {
  values <- suppressWarnings(as.numeric(unlist(coordinates, use.names = FALSE)))
  values <- values[!is.na(values)]
  if (length(values) < 2) return(matrix(numeric(0), ncol = 2))
  matrix(values[seq_len(length(values) - length(values) %% 2)], ncol = 2, byrow = TRUE)
}

obtener_geojson_municipios_gt_path <- function() {
  geojson_candidates <- c(
    file.path("www", "guatemala_municipios.geojson"),
    file.path("shiny_app", "www", "guatemala_municipios.geojson")
  )
  existing_geojson <- geojson_candidates[file.exists(geojson_candidates)]
  if (length(existing_geojson)) existing_geojson[[1]] else ""
}

construir_centroides_municipales_gt <- function() {
  geojson_path <- obtener_geojson_municipios_gt_path()
  if (!nzchar(geojson_path)) return(data.frame())

  geojson <- tryCatch(
    jsonlite::fromJSON(geojson_path, simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (is.null(geojson) || is.null(geojson$features)) return(data.frame())

  centroides <- do.call(rbind, lapply(geojson$features, function(feature) {
    props <- feature$properties
    puntos <- extraer_puntos_geojson(feature$geometry$coordinates)
    if (is.null(props) || !nrow(puntos)) return(NULL)

    data.frame(
      departamento_geo = value_or_default(props$DEPARTAMENTO, props$N_NIVEL2),
      municipio_geo = props$N_NIVEL3,
      latitude = mean(puntos[, 2], na.rm = TRUE),
      longitude = mean(puntos[, 1], na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  if (is.null(centroides) || !nrow(centroides)) return(data.frame())

  catalogo_gt <- ubicacion_municipio_catalogo[ubicacion_municipio_catalogo$pais == "Guatemala", , drop = FALSE]
  llave_catalogo <- paste(
    ubicacion_normalizar_texto(catalogo_gt$departamento),
    ubicacion_normalizar_texto(catalogo_gt$municipio),
    sep = "|"
  )
  llave_geo <- paste(
    ubicacion_normalizar_texto(centroides$departamento_geo),
    ubicacion_normalizar_texto(centroides$municipio_geo),
    sep = "|"
  )
  indice <- match(llave_catalogo, llave_geo)
  faltantes <- is.na(indice)
  if (any(faltantes)) {
    indice[faltantes] <- match(
      ubicacion_normalizar_texto(catalogo_gt$municipio[faltantes]),
      ubicacion_normalizar_texto(centroides$municipio_geo)
    )
  }
  encontrados <- !is.na(indice)
  ubicaciones_municipales <- data.frame(
    pais = "Guatemala",
    codigo_municipio = catalogo_gt$municipio_codigo[encontrados],
    latitude = centroides$latitude[indice[encontrados]],
    longitude = centroides$longitude[indice[encontrados]],
    stringsAsFactors = FALSE
  )
  sin_municipio <- catalogo_gt[!encontrados, , drop = FALSE]
  if (nrow(sin_municipio)) {
    departamentos <- unique(centroides$departamento_geo)
    centroides_departamento <- do.call(rbind, lapply(departamentos, function(departamento) {
      datos <- centroides[centroides$departamento_geo == departamento, , drop = FALSE]
      data.frame(
        departamento = departamento,
        latitude = mean(datos$latitude, na.rm = TRUE),
        longitude = mean(datos$longitude, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
    indice_departamento <- match(
      ubicacion_normalizar_texto(sin_municipio$departamento),
      ubicacion_normalizar_texto(centroides_departamento$departamento)
    )
    con_departamento <- !is.na(indice_departamento)
    ubicaciones_departamentales <- data.frame(
      pais = "Guatemala",
      codigo_municipio = sin_municipio$municipio_codigo[con_departamento],
      latitude = centroides_departamento$latitude[indice_departamento[con_departamento]],
      longitude = centroides_departamento$longitude[indice_departamento[con_departamento]],
      stringsAsFactors = FALSE
    )
    ubicaciones_municipales <- rbind(ubicaciones_municipales, ubicaciones_departamentales)
  }

  ubicaciones_municipales
}

crear_geojson_departamentos_resultados_gt <- function(departamentos) {
  departamentos <- ubicacion_normalizar_texto(unique(stats::na.omit(departamentos)))
  departamentos <- departamentos[nzchar(departamentos)]
  if (!length(departamentos)) return(NULL)

  geojson_path <- obtener_geojson_municipios_gt_path()
  if (!nzchar(geojson_path)) return(NULL)

  geojson <- tryCatch(
    jsonlite::fromJSON(geojson_path, simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (is.null(geojson) || is.null(geojson$features)) return(NULL)

  geojson$features <- Filter(function(feature) {
    props <- feature$properties
    if (is.null(props)) return(FALSE)
    ubicacion_normalizar_texto(value_or_default(props$DEPARTAMENTO, props$N_NIVEL2)) %in% departamentos
  }, geojson$features)
  if (!length(geojson$features)) return(NULL)

  jsonlite::toJSON(geojson, auto_unbox = TRUE, null = "null", digits = NA)
}

formulario_7_visualization_locations <- local({
  ubicaciones_gt <- construir_centroides_municipales_gt()
  ubicaciones <- rbind(formulario_7_visualization_locations, ubicaciones_gt)
  ubicaciones[!duplicated(paste(ubicaciones$pais, ubicaciones$codigo_municipio)), , drop = FALSE]
})

normalizar_codigo_municipio_mapa <- function(country, department_code, municipality_code) {
  country <- ubicacion_normalizar_pais(country)
  department_code <- gsub("[^0-9]+", "", as.character(value_or_default(department_code, "")))
  municipality_code <- gsub("[^0-9]+", "", as.character(value_or_default(municipality_code, "")))
  if (!nzchar(municipality_code)) return("")
  if (nchar(municipality_code) >= 4) return(municipality_code)
  if (identical(country, "Guatemala") && nzchar(department_code)) {
    return(paste0(sprintf("%02d", as.integer(department_code)), sprintf("%02d", as.integer(municipality_code))))
  }
  municipality_code
}

ubicacion_resolver_departamento_codigo <- function(country, value) {
  country <- ubicacion_normalizar_pais(country)
  value <- trimws(as.character(value_or_default(value, "")))
  if (!nzchar(value)) return("")
  catalog <- ubicacion_departamento_catalogo[ubicacion_departamento_catalogo$pais == country, ]
  if (value %in% catalog$departamento_codigo) return(value)
  normalized <- ubicacion_normalizar_texto(value)
  matched <- catalog$departamento_codigo[ubicacion_normalizar_texto(catalog$departamento) == normalized]
  if (length(matched)) matched[[1]] else value
}

ubicacion_resolver_municipio_codigo <- function(country, value) {
  country <- ubicacion_normalizar_pais(country)
  value <- trimws(as.character(value_or_default(value, "")))
  if (!nzchar(value)) return("")
  catalog <- ubicacion_municipio_catalogo[ubicacion_municipio_catalogo$pais == country, ]
  if (value %in% catalog$municipio_codigo) return(value)
  normalized <- ubicacion_normalizar_texto(value)
  matched <- catalog$municipio_codigo[ubicacion_normalizar_texto(catalog$municipio) == normalized]
  if (length(matched)) matched[[1]] else gsub("[^0-9]+", "", value)
}

formulario_1_intake_columns <- c(
  "formulario_codigo",
  "formulario_nombre",
  "fecha_registro",
  "pais",
  "id_institucion",
  "departamento",
  "municipio",
  "ciclo",
  "ronda",
  "codigo_formulario",
  "fecha_colocacion",
  "grupo_responsable_colocacion",
  "cuadrante",
  "codigo_casa",
  "Latitud",
  "Longitud",
  "codigo_gps",
  "Ovitrampas_colocadas",
  "codigo_sustrato",
  "fecha_retiro",
  "grupo_responsable_retiro",
  "Ovitrampas_retiradas",
  "retiro_buen_estado",
  "retiro_sin_agua",
  "retiro_sin_sustrato",
  "retiro_sin_ovitrampa",
  "retiro_movida",
  "retiro_volteada",
  "retiro_casa_cerrada",
  "retiro_casa_cerrada_descripcion",
  "fuente_formulario",
  "creado_por",
  "creado_en",
  "actualizado_en"
)

formulario_1_template <- data.frame(
  formulario_codigo = "F1",
  formulario_nombre = "Colocacion y retiro de ovitrampa",
  fecha_registro = "",
  pais = "El Salvador",
  id_institucion = default_institution_id,
  departamento = "",
  municipio = "",
  ciclo = "Ciclo 2",
  ronda = "",
  codigo_formulario = "REISV",
  fecha_colocacion = "",
  grupo_responsable_colocacion = "",
  cuadrante = "REI25SV0201C001",
  codigo_casa = "OV001",
  Latitud = "",
  Longitud = "",
  codigo_gps = "",
  Ovitrampas_colocadas = "",
  codigo_sustrato = "SV257A",
  fecha_retiro = "",
  grupo_responsable_retiro = "",
  Ovitrampas_retiradas = "",
  retiro_buen_estado = "",
  retiro_sin_agua = "",
  retiro_sin_sustrato = "",
  retiro_sin_ovitrampa = "",
  retiro_movida = "",
  retiro_volteada = "",
  retiro_casa_cerrada = "",
  retiro_casa_cerrada_descripcion = "",
  fuente_formulario = "Formulario 1-7.xlsx",
  creado_por = "",
  creado_en = "",
  actualizado_en = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

formulario_7_codigo_bioensayo_final <- function(
    codigo_bioensayo,
    bioensayo_diagnostica_1x = FALSE,
    bioensayo_intensidad = NA_character_,
    dosis_intensidad = NA_character_,
    sinergista_def = FALSE,
    sinergista_pbo = FALSE,
    sinergista_dm = FALSE) {
  clean_part <- function(value) {
    if (is.null(value) || length(value) == 0) return(NA_character_)
    trimws(as.character(value))
  }
  as_flag <- function(value) {
    cleaned <- tolower(clean_part(value))
    cleaned %in% c("true", "1", "si", "sí", "yes")
  }

  inputs <- list(
    clean_part(codigo_bioensayo), as_flag(bioensayo_diagnostica_1x), clean_part(bioensayo_intensidad),
    clean_part(dosis_intensidad), as_flag(sinergista_def), as_flag(sinergista_pbo), as_flag(sinergista_dm)
  )
  size <- max(vapply(inputs, length, integer(1)))
  inputs <- lapply(inputs, rep_len, length.out = size)
  bioensayo <- inputs[[1]]
  diagnostica <- inputs[[2]]
  modalidad <- inputs[[3]]
  intensidad <- inputs[[4]]
  def <- inputs[[5]]
  pbo <- inputs[[6]]
  dm <- inputs[[7]]
  bioensayo <- sub("-D$", "", bioensayo)
  already_final <- !is.na(bioensayo) & (
    grepl("REI[0-9]{2}[A-Z]{2}[0-9]{4}P[0-9]+(\\.[0-9]+)?(DEF|PBO|DM)?(DEL|PER|MAL|DDT|BEN|ALF|LAM|TEM)[0-9]+F[0-9]+$", bioensayo) |
      grepl("REI[0-9]{2}[A-Z]{2}[0-9]{4}(DEL|PER|MAL|DDT)[0-9]+(\\.[0-9]+)?(DD|IE|IC(2X|5X|10X)|S(DEF|PBO|DM))$", bioensayo) |
      grepl("REI[0-9]{2}[A-Z]{2}[0-9]{4}BIO[0-9]+(DD|IE|IC(2X|5X|10X)|S(DEF|PBO|DM))$", bioensayo) |
      grepl("-(I(-[0-9]+X)+|I-1X-2X-5X-10X|S(-[A-Z]+)+)$", bioensayo)
  )

  suffix <- rep(NA_character_, size)
  suffix[diagnostica] <- ""
  exploratorio <- !diagnostica & modalidad == "Exploratorio"
  suffix[exploratorio] <- "I-1X-2X-5X-10X"
  completa <- !diagnostica & modalidad == "Completa" & !is.na(intensidad) & nzchar(intensidad)
  suffix[completa] <- paste("I", intensidad[completa], sep = "-")
  for (index in seq_len(size)) {
    selected <- c("DEF"[def[[index]]], "PBO"[pbo[[index]]], "DM"[dm[[index]]])
    selected <- selected[!is.na(selected)]
    if (!diagnostica[[index]] && length(selected)) suffix[[index]] <- paste(c("S", selected), collapse = "-")
  }

  result <- ifelse(!is.na(suffix) & nzchar(suffix), paste(bioensayo, suffix, sep = "-"), bioensayo)
  result[already_final] <- bioensayo[already_final]
  result[is.na(bioensayo) | !nzchar(bioensayo) | is.na(suffix)] <- NA_character_
  result[already_final & !is.na(bioensayo) & nzchar(bioensayo)] <- bioensayo[already_final & !is.na(bioensayo) & nzchar(bioensayo)]
  result
}

formulario_7_header_columns <- c(
  "formulario_codigo", "formulario_nombre", "fecha_registro", "codigo_bioensayo", "nombre_poblacion",
  "pais", "id_institucion", "codigo_departamento", "codigo_municipio", "bioensayo_intensidad",
  "bioensayo_diagnostica_1x", "dosis_intensidad", "sinergista_def", "sinergista_pbo", "sinergista_dm",
  "sinergista_tipo", "dosis_sinergista_ug_ml",
  "resultado_diagnostico", "fecha_realizacion_bioensayo", "insecticida", "solvente_utilizado",
  "solvente_otro", "dosis_intensidad_ug_ml", "lote_insecticida", "fecha_revestimiento_botellas",
  "numero_usos_botella_e1", "numero_usos_botella_e2", "numero_usos_botella_e3",
  "numero_usos_botella_e4", "numero_usos_botella_c1", "origen_material",
  "edad_dias", "edad_indefinida", "codigo_especie_mosquito",
  "fecha_separacion", "hora_separacion", "generacion_filial", "generacion_filial_indefinida",
  "codigo_responsable_revestimiento", "codigo_responsable_bioensayo", "codigo_control_calidad",
  "codigo_revision_24h", "temperatura_inicial_c", "temperatura_final_c",
  "humedad_relativa_inicial_pct", "humedad_relativa_final_pct", "hora_inicio_bioensayo",
  "hora_final_bioensayo", "fuente_formulario", "nombre_quien_ingreso"
)

formulario_7_bottles <- c("b1", "b2", "b3", "b4", "c1")
formulario_7_bottle_labels <- c(
  b1 = "Botella experimental 1", b2 = "Botella experimental 2",
  b3 = "Botella experimental 3", b4 = "Botella experimental 4",
  c1 = "Botella control 1"
)
formulario_7_result_columns <- unlist(lapply(formulario_7_bottles, function(bottle) {
  c(
    paste0("resultado_hora_inicio_", bottle),
    unlist(lapply(c(0, 15, 30, 45), function(minutes) {
      paste0("resultado_", minutes, "min_", bottle, c("_vivos", "_incapacitados"))
    })),
    paste0("resultado_60min_", bottle, c("_vivos", "_incapacitados")),
    paste0("resultado_hora_lectura_24h_", bottle),
    paste0("resultado_24h_", bottle, c("_vivos", "_incapacitados"))
  )
}), use.names = FALSE)
formulario_7_non_24h_result_columns <- grep("resultado_(hora_inicio|0min|15min|30min|45min|60min)_", formulario_7_result_columns, value = TRUE)
formulario_7_is_temefos <- function(value) {
  identical(toupper(trimws(value_or_default(value, ""))), "TEMEFOS")
}
formulario_7_comment_columns <- c("comentario", "comentario_nombre")
formulario_7_intake_columns <- c(
  setdiff(formulario_7_header_columns, c("fuente_formulario")),
  formulario_7_result_columns,
  formulario_7_comment_columns,
  "fuente_formulario", "creado_en", "actualizado_en"
)
formulario_7_csv_columns <- c(
  "formulario_codigo", "formulario_nombre", "fecha_registro", "codigo_bioensayo",
  "pais", "id_institucion", "codigo_departamento", "codigo_municipio",
  "nombre_poblacion", "nombre_quien_ingreso", "bioensayo_diagnostica_1x",
  "bioensayo_intensidad", "dosis_intensidad_ug_ml", "sinergista_def",
  "sinergista_pbo", "sinergista_dm", "dosis_sinergista_ug_ml",
  "resultado_diagnostico", "fecha_realizacion_bioensayo", "insecticida",
  "solvente_utilizado", "solvente_otro", "lote_insecticida",
  "fecha_revestimiento_botellas", "numero_usos_botella_e1",
  "numero_usos_botella_e2", "numero_usos_botella_e3", "numero_usos_botella_e4",
  "numero_usos_botella_c1", "origen_material", "edad_dias", "edad_indefinida",
  "codigo_especie_mosquito", "fecha_separacion", "hora_separacion",
  "generacion_filial", "generacion_filial_indefinida",
  "codigo_responsable_revestimiento", "codigo_responsable_bioensayo",
  "codigo_revision_24h", "temperatura_inicial_c", "temperatura_final_c",
  "humedad_relativa_inicial_pct", "humedad_relativa_final_pct",
  "hora_inicio_bioensayo", "hora_final_bioensayo",
  formulario_7_result_columns,
  formulario_7_comment_columns
)

formulario_7_template <- as.data.frame(
  setNames(rep(list(""), length(formulario_7_intake_columns)), formulario_7_intake_columns),
  stringsAsFactors = FALSE
)
formulario_7_template$formulario_codigo <- "F7"
formulario_7_template$formulario_nombre <- "Registro de datos del bioensayo de la botella CDC"
formulario_7_template$fecha_registro <- as.character(Sys.Date())
formulario_7_template$id_institucion <- default_institution_id
formulario_7_template$fuente_formulario <- "Formulario 7_Bioensayo .docx"

formulario_7_external_to_internal_names <- c(
  creado_por = "nombre_quien_ingreso",
  bioensayo_diagnostica_1x = "bioensayo_diagnostica_1x",
  bioensayo_intensidad = "bioensayo_intensidad",
  dosis_intensidad_ug_ml = "dosis_intensidad_ug_ml",
  insecticida = "insecticida",
  lote_insecticida = "lote_insecticida"
)

formulario_7_csv_to_internal <- function(csv_data) {
  if (all(formulario_7_intake_columns %in% names(csv_data))) {
    return(csv_data[formulario_7_intake_columns])
  }

  data <- as.data.frame(
    setNames(rep(list(rep(NA_character_, nrow(csv_data))), length(formulario_7_intake_columns)), formulario_7_intake_columns),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  same_name_columns <- intersect(names(csv_data), formulario_7_intake_columns)
  for (column in same_name_columns) data[[column]] <- csv_data[[column]]
  for (external_name in names(formulario_7_external_to_internal_names)) {
    internal_name <- formulario_7_external_to_internal_names[[external_name]]
    if (external_name %in% names(csv_data)) data[[internal_name]] <- csv_data[[external_name]]
  }
  data$codigo_control_calidad[is.na(data$codigo_control_calidad) | !nzchar(trimws(data$codigo_control_calidad))] <- "NO APLICA"
  data$fuente_formulario[is.na(data$fuente_formulario) | !nzchar(trimws(data$fuente_formulario))] <- "Formulario 7_Bioensayo .docx"
  data$sinergista_tipo <- NA_character_
  data$sinergista_tipo[tolower(trimws(value_or_default(data$sinergista_def, ""))) %in% c("true", "1", "si", "sí", "yes")] <- "DEF"
  data$sinergista_tipo[tolower(trimws(value_or_default(data$sinergista_pbo, ""))) %in% c("true", "1", "si", "sí", "yes")] <- "PBO"
  data$sinergista_tipo[tolower(trimws(value_or_default(data$sinergista_dm, ""))) %in% c("true", "1", "si", "sí", "yes")] <- "DM"
  data[formulario_7_intake_columns]
}

formulario_7_internal_to_csv <- function(data) {
  output <- as.data.frame(
    setNames(rep(list(rep("", nrow(data))), length(formulario_7_csv_columns)), formulario_7_csv_columns),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  same_name_columns <- intersect(formulario_7_csv_columns, names(data))
  for (column in same_name_columns) output[[column]] <- data[[column]]
  output$bioensayo_diagnostica_1x <- data$bioensayo_diagnostica_1x
  output$bioensayo_intensidad <- data$bioensayo_intensidad
  output$dosis_intensidad_ug_ml <- data$dosis_intensidad_ug_ml
  output$insecticida <- data$insecticida
  output$lote_insecticida <- data$lote_insecticida
  output[formulario_7_csv_columns]
}

parse_postgres_url <- function(url) {
  pattern <- "^postgres(?:ql)?://([^:]+):([^@]+)@([^:/?]+):([0-9]+)/([^?]+)(?:\\?.*)?$"
  matches <- regmatches(url, regexec(pattern, url))[[1]]

  if (length(matches) == 0) {
    stop("SUPABASE_DB_URL is not a valid PostgreSQL connection URI.")
  }

  list(
    user = URLdecode(matches[[2]]),
    password = URLdecode(matches[[3]]),
    host = matches[[4]],
    port = as.integer(matches[[5]]),
    dbname = matches[[6]]
  )
}

connect_to_supabase <- function() {
  if (!nzchar(db_url)) {
    stop("SUPABASE_DB_URL is not configured.")
  }

  connection <- parse_postgres_url(db_url)

  dbConnect(
    RPostgres::Postgres(),
    dbname = connection$dbname,
    host = connection$host,
    port = connection$port,
    user = connection$user,
    password = connection$password,
    sslmode = "require"
  )
}

supabase_auth_sign_in <- function(login_identifier, password) {
  project_url <- storage_project_url()

  if (!nzchar(project_url)) {
    stop("SUPABASE_URL is not configured and could not be derived from SUPABASE_DB_URL.")
  }
  if (!nzchar(supabase_auth_api_key)) {
    stop("SUPABASE_ANON_KEY, SUPABASE_PUBLISHABLE_KEY, or SUPABASE_SERVICE_ROLE_KEY is not configured.")
  }

  response <- request(paste0(project_url, "/auth/v1/token?grant_type=password")) |>
    req_headers(
      apikey = supabase_auth_api_key,
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(email = login_identifier, password = password), auto_unbox = TRUE) |>
    req_error(is_error = function(response) FALSE) |>
    req_perform()

  body <- resp_body_json(response, check_type = FALSE)
  if (resp_status(response) >= 300) {
    message <- body$error_description %||% body$msg %||% body$message %||% "No se pudo iniciar sesión con Supabase Auth."
    stop(message)
  }

  body
}

supabase_auth_update_password <- function(access_token, password) {
  project_url <- storage_project_url()

  if (!nzchar(project_url)) {
    stop("SUPABASE_URL is not configured and could not be derived from SUPABASE_DB_URL.")
  }
  if (!nzchar(supabase_auth_api_key)) {
    stop("SUPABASE_ANON_KEY, SUPABASE_PUBLISHABLE_KEY, or SUPABASE_SERVICE_ROLE_KEY is not configured.")
  }

  response <- request(paste0(project_url, "/auth/v1/user")) |>
    req_method("PUT") |>
    req_headers(
      apikey = supabase_auth_api_key,
      Authorization = paste("Bearer", access_token),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(password = password), auto_unbox = TRUE) |>
    req_error(is_error = function(response) FALSE) |>
    req_perform()

  body <- resp_body_json(response, check_type = FALSE)
  if (resp_status(response) >= 300) {
    message <- body$error_description %||% body$msg %||% body$message %||% "No se pudo actualizar la contraseña."
    stop(message)
  }

  body
}

supabase_auth_send_password_recovery <- function(email) {
  project_url <- storage_project_url()
  redirect_url <- value_or_default(auth_redirect_url, "")

  if (!nzchar(project_url)) {
    stop("SUPABASE_URL is not configured and could not be derived from SUPABASE_DB_URL.")
  }
  if (!nzchar(supabase_auth_api_key)) {
    stop("SUPABASE_ANON_KEY, SUPABASE_PUBLISHABLE_KEY, or SUPABASE_SERVICE_ROLE_KEY is not configured.")
  }
  if (!nzchar(redirect_url)) {
    stop("ENTONET_AUTH_REDIRECT_URL is not configured.")
  }

  response <- request(paste0(project_url, "/auth/v1/recover")) |>
    req_headers(
      apikey = supabase_auth_api_key,
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(email = email, redirect_to = redirect_url), auto_unbox = TRUE) |>
    req_error(is_error = function(response) FALSE) |>
    req_perform()

  body <- resp_body_json(response, check_type = FALSE)
  if (resp_status(response) >= 300) {
    message <- body$error_description %||% body$msg %||% body$message %||% "No se pudo enviar el correo de recuperación."
    stop(message)
  }

  body
}

fetch_usuario_perfil <- function(login_identifier = NULL, auth_user_id = NULL, auth_email = NULL) {
  connection <- connect_to_supabase()
  on.exit(dbDisconnect(connection), add = TRUE)

  if (!is.null(auth_user_id) && nzchar(auth_user_id)) {
    profile <- dbGetQuery(
      connection,
      "select usuario, user_id::text, email, id_institucion, rol, pais, nombre, activo
       from public.usuario_perfil
       where user_id = $1::uuid
       limit 1",
      params = list(auth_user_id)
    )
    if (nrow(profile) > 0) return(profile[1, , drop = FALSE])
  }

  candidates <- unique(tolower(trimws(c(
    login_identifier,
    auth_email,
    sub("@.*$", "", value_or_default(auth_email, ""))
  ))))
  candidates <- candidates[nzchar(candidates)]
  if (length(candidates) == 0) {
    return(data.frame())
  }

  placeholders <- paste0("$", seq_along(candidates), collapse = ", ")
  query <- paste0(
    "select usuario, user_id::text, email, id_institucion, rol, pais, nombre, activo
     from public.usuario_perfil
     where lower(usuario) in (", placeholders, ") or lower(email) in (", placeholders, ")
     limit 1"
  )
  dbGetQuery(connection, query, params = as.list(candidates))
}

login_identifier_to_email <- function(login_identifier) {
  cleaned <- trimws(login_identifier)
  if (grepl("@", cleaned, fixed = TRUE)) {
    return(cleaned)
  }

  profile <- fetch_usuario_perfil(login_identifier = cleaned)
  if (nrow(profile) == 0) {
    stop("El usuario no está registrado en usuario_perfil.")
  }
  if (!isTRUE(profile$activo[[1]])) {
    stop("El usuario está inactivo.")
  }

  email <- value_or_default(profile$email[[1]], "")
  if (!nzchar(email)) {
    stop("El usuario existe, pero todavía no tiene correo enlazado para Supabase Auth.")
  }

  email
}

normalize_sat26_login <- function(value) {
  normalized <- iconv(trimws(value_or_default(value, "")), from = "", to = "ASCII//TRANSLIT")
  normalized <- tolower(value_or_default(normalized, ""))
  gsub("[^a-z0-9]+", "", normalized)
}

is_sat26_survey_login <- function(username, password) {
  normalize_sat26_login(username) %in% c("encuestasatisfaccion", "encuestasatisfacion") &&
    identical(value_or_default(password, ""), "26SAT")
}

is_sat26_survey_session <- function(username) {
  normalize_sat26_login(username) %in% c("encuestasatisfaccion", "encuestasatisfacion")
}

public_header <- function(active_tab = NULL, show_login_button = FALSE, language = "es") {
  tab_class <- function(tab) {
    paste(
      "landing-tab",
      if (identical(active_tab, tab)) "landing-tab-active" else ""
    )
  }

  tagList(
    div(
      class = "site-header",
      img(src = "entonet-header.jpeg", class = "header-logo", alt = "Logo EntoNet"),
      div(
        class = "public-header-actions",
        div(
          class = "language-switcher",
          actionButton("set_language_es", "🇬🇹 Español", class = paste("language-button", if (identical(language, "es")) "language-active" else "")),
          actionButton("set_language_en", "🇧🇿 English", class = paste("language-button", if (identical(language, "en")) "language-active" else ""))
        ),
        if (isTRUE(show_login_button)) {
          actionButton("return_to_login", tr(language, "Ingresar", "Log in"), class = "public-login-button")
        }
      )
    ),
    div(
      class = "landing-tabs",
      actionButton("landing_program", tr(language, "Programa", "Program"), class = tab_class("program")),
      actionButton("landing_governance", tr(language, "Gobernanza Regional", "Regional Governance"), class = tab_class("governance")),
      actionButton("landing_collaborators", tr(language, "Colaboradores", "Collaborators"), class = tab_class("collaborators")),
      actionButton("landing_network_map", tr(language, "Mapa de la Red", "Network Map"), class = tab_class("network_map")),
      actionButton("landing_network_impact", tr(language, "Impacto de la Red", "Network Impact"), class = tab_class("network_impact")),
      actionButton("landing_sat26", tr(language, "Encuesta SAT26", "SAT26 Survey"), class = tab_class("sat26"))
    )
  )
}

public_footer <- function(language = "es") {
  div(
    class = "institutional-footer",
    div(class = "footer-title", tr(language, "Coordinado por:", "Coordinated by:")),
    div(
      class = "footer-logos",
      tags$a(
        href = "https://www.ces.uvg.edu.gt/page/",
        target = "_blank",
        rel = "noopener noreferrer",
        img(src = "ces-logo-cropped.png", class = "footer-logo ces-logo", alt = "Logo CES")
      ),
      tags$a(
        href = "https://www.uvg.edu.gt/",
        target = "_blank",
        rel = "noopener noreferrer",
        img(src = "uvg-logo.jpg", class = "footer-logo uvg-footer-logo", alt = "Logo Universidad del Valle de Guatemala")
      )
    )
  )
}

sat26_public_page <- function(language = "es") {
  tagList(
    public_header("sat26", show_login_button = TRUE, language = language),
    div(
      class = "portal-shell sat26-public-shell",
      tags$main(
        class = "portal-workspace sat26-public-workspace",
        tags$div(id = "sat26_scroll_anchor", class = "sat26-scroll-anchor"),
        uiOutput("module_area")
      )
    ),
    public_footer(language)
  )
}

landing_page <- function(login_message = NULL, language = "es") {
  tagList(
    public_header(language = language),
    div(
      class = "landing-main",
      fluidRow(
        class = "landing-row",
        column(
          width = 8,
          div(
            class = "portal-intro",
            h1("EntoNet"),
            h2(tr(language, "Red Entomológica para la Vigilancia y Control de Vectores", "Entomological Network for Vector Surveillance and Control")),
            p(tr(language, "Portal institucional para la captura, revisión y gestión de datos entomológicos y experimentales.", "Institutional portal for capturing, reviewing, and managing entomological and experimental data."))
          ),
          div(
            class = "overview-panel",
            h3(tr(language, "Acerca del portal", "About the portal")),
            p(tr(language, "Esta plataforma apoya el registro estructurado de información generada por proyectos de vigilancia, control de vectores y ensayos de laboratorio.", "This platform supports the structured recording of information generated by surveillance projects, vector-control programs, and laboratory studies.")),
            tags$ul(
              tags$li(tr(language, "Seleccione el conjunto de datos que desea utilizar.", "Select the dataset you want to use.")),
              tags$li(tr(language, "Ingrese observaciones mediante formularios guiados.", "Enter observations using guided forms.")),
              tags$li(tr(language, "Guarde registros pendientes para revisión posterior.", "Save pending records for later review.")),
              tags$li(tr(language, "Mantenga separados los datos históricos sin procesar y los registros revisados.", "Keep historical raw data separate from reviewed records."))
            )
          )
        ),
        column(
          width = 4,
          div(
            class = "login-card",
            h3(tr(language, "Ingreso seguro", "Secure login")),
            p(class = "login-subtitle", tr(language, "Acceso restringido a usuarios autorizados.", "Access restricted to authorized users.")),
            if (!is.null(login_message)) login_message,
            textInput("login_user", tr(language, "Usuario o correo", "Username or email")),
            passwordInput("login_password", tr(language, "Contraseña", "Password")),
            actionButton("login", tr(language, "Ingresar", "Log in"), class = "btn-primary login-button"),
            actionButton("show_password_reset", tr(language, "Restablecer contraseña", "Reset password"), class = "btn-link password-reset-link"),
            div(class = "login-help", tr(language, "Si necesita acceso, contacte al administrador del proyecto.", "If you need access, contact the project administrator."))
          )
        )
      )
    ),
    public_footer(language)
  )
}

password_setup_page <- function(status_message = NULL, language = "es") {
  tagList(
    public_header(language = language),
    div(
      class = "landing-main",
      fluidRow(
        class = "landing-row",
        column(
          width = 7,
          div(
            class = "portal-intro",
            h1("EntoNet"),
            h2(tr(language, "Crear contraseña", "Create password")),
            p(tr(language, "Defina una contraseña para completar su invitación y activar el acceso al portal.", "Set a password to complete your invitation and activate portal access."))
          )
        ),
        column(
          width = 5,
          div(
            class = "login-card",
            h3(tr(language, "Nueva contraseña", "New password")),
            p(class = "login-subtitle", tr(language, "Use al menos 8 caracteres. Después podrá ingresar con su correo y contraseña.", "Use at least 8 characters. After this, you can log in with your email and password.")),
            if (!is.null(status_message)) status_message,
            passwordInput("setup_password", tr(language, "Contraseña", "Password")),
            passwordInput("setup_password_confirm", tr(language, "Confirmar contraseña", "Confirm password")),
            actionButton("setup_password_save", tr(language, "Guardar contraseña", "Save password"), class = "btn-primary login-button"),
            div(class = "login-help", actionButton("setup_return_to_login", tr(language, "Volver al ingreso", "Back to log in"), class = "btn-link"))
          )
        )
      )
    ),
    public_footer(language)
  )
}

program_page <- function(language = "es") {
  tagList(
    public_header("program", show_login_button = TRUE, language = language),
    tags$main(
      class = "public-content",
      div(
        class = "program-hero",
        div(
          class = "program-hero-copy",
          span(class = "program-eyebrow", tr(language, "Programa regional", "Regional program")),
          h1("EntoNet"),
          h2(tr(language, "Red Entomológica para la Vigilancia y Control de Vectores", "Entomological Network for Vector Surveillance and Control")),
          p(tr(language, "Una plataforma regional para fortalecer la vigilancia, la colaboración científica y la respuesta ante enfermedades transmitidas por vectores.", "A regional platform to strengthen surveillance, scientific collaboration, and response to vector-borne diseases."))
        )
      ),
      div(
        class = "program-content-card",
        h3(tr(language, "¿Qué es EntoNet?", "What is EntoNet?")),
        p(tr(language, "EntoNet es una iniciativa regional financiada por los Centros para el Control y la Prevención de Enfermedades de los Estados Unidos (CDC) que busca fortalecer la vigilancia entomológica y el control de vectores de importancia médica en Centroamérica y República Dominicana. La red promueve la interoperabilidad, estandarización e integración de datos entomológicos generados por ministerios de salud, instituciones académicas y programas nacionales de control vectorial.", "EntoNet is a regional initiative funded by the United States Centers for Disease Control and Prevention (CDC) that seeks to strengthen entomological surveillance and the control of medically important vectors in Central America and the Dominican Republic. The network promotes interoperability, standardization, and integration of entomological data generated by ministries of health, academic institutions, and national vector-control programs.")),
        p(tr(language, "A través de la armonización de protocolos, el fortalecimiento de capacidades técnicas y el intercambio de información en tiempo real, EntoNet facilita la generación de evidencia para apoyar la detección temprana de riesgos, la toma de decisiones basada en datos y la implementación de estrategias más efectivas para la prevención y control de enfermedades transmitidas por vectores como dengue, chikungunya, Zika, malaria, enfermedad de Chagas, leishmaniasis y otras arbovirosis emergentes.", "Through protocol harmonization, technical capacity building, and real-time information exchange, EntoNet facilitates evidence generation to support early risk detection, data-driven decision making, and more effective strategies for preventing and controlling vector-borne diseases such as dengue, chikungunya, Zika, malaria, Chagas disease, leishmaniasis, and emerging arboviruses.")),
        h3(tr(language, "Nuestra visión", "Our vision")),
        p(tr(language, "La visión de EntoNet es consolidar una plataforma regional de colaboración que fortalezca la preparación y respuesta ante amenazas vectoriales en la región, promoviendo una vigilancia moderna, integrada y sostenible bajo un enfoque de salud pública y cooperación internacional.", "EntoNet's vision is to consolidate a regional collaboration platform that strengthens preparedness and response to vector threats, promoting modern, integrated, and sustainable surveillance through public health and international cooperation."))
      ),
      div(
        class = "strategic-pillars-card",
        img(
          src = "pilares-estrategicos.png",
          class = "strategic-pillars-image",
          alt = tr(language, "Nuestros cuatro pilares estratégicos", "Our four strategic pillars")
        )
      ),
      div(
        class = "program-participants-card",
        span(class = "program-section-eyebrow", tr(language, "Participación regional", "Regional participation")),
        h2(tr(language, "Participantes", "Participants")),
        p(class = "participants-intro", tr(language, "EntoNet articula a ocho países de Centroamérica y República Dominicana para fortalecer la vigilancia y el control de vectores de importancia médica.", "EntoNet connects eight countries across Central America and the Dominican Republic to strengthen surveillance and control of medically important vectors.")),
        div(
          class = "participant-country-grid",
          lapply(country_choices, function(country) {
            div(
              class = "participant-country",
              span(class = "participant-country-marker"),
              strong(display_country(country, language))
            )
          })
        ),
        div(
          class = "implementers-component",
          h3(tr(language, "Componente de Implementadores", "Implementing Partners")),
          p(tr(language, "El éxito de EntoNet se basa en un modelo de implementación colaborativo y sostenible. Si bien los CDC proporcionan financiamiento estratégico para fortalecer las capacidades regionales, el desarrollo de herramientas, la capacitación y la interoperabilidad de los sistemas de información, cada país participante contribuye con recursos propios para ejecutar las actividades operativas necesarias dentro de sus programas nacionales de vigilancia y control de vectores.", "EntoNet's success is based on a collaborative and sustainable implementation model. While CDC provides strategic funding to strengthen regional capacity, tool development, training, and information-system interoperability, each participating country contributes its own resources to carry out operational activities within national vector-surveillance and control programs.")),
          p(tr(language, "Este esquema de cofinanciamiento permite fortalecer el compromiso institucional, promover la sostenibilidad de las acciones y asegurar que las actividades respondan a las prioridades y necesidades específicas de cada país. Los implementadores nacionales incluyen principalmente los Ministerios de Salud, programas nacionales de control de vectores, laboratorios de referencia e instituciones técnicas y académicas que participan en la generación y utilización de información entomológica.", "This co-financing approach strengthens institutional commitment, promotes sustainability, and ensures that activities respond to each country's priorities and needs. National implementers primarily include ministries of health, national vector-control programs, reference laboratories, and technical and academic institutions involved in generating and using entomological information.")),
          p(tr(language, "A nivel regional, la Universidad del Valle de Guatemala y la Secretaría Ejecutiva del Consejo de Ministros de Salud de Centroamérica y República Dominicana (CECOMISCA) desempeñan un papel clave en la coordinación y articulación entre los países participantes, facilitando el intercambio de experiencias, la armonización de procesos, la definición de prioridades regionales y el seguimiento de los acuerdos técnicos. Este modelo permite que EntoNet funcione como una red regional colaborativa, donde los recursos, conocimientos y capacidades de los diferentes socios convergen para fortalecer la vigilancia y el control de vectores de importancia médica en la región.", "At the regional level, Universidad del Valle de Guatemala and the Executive Secretariat of the Council of Ministers of Health of Central America and the Dominican Republic (CECOMISCA) play a key role in coordinating participating countries, facilitating experience exchange, process harmonization, regional priority setting, and follow-up of technical agreements. This model enables EntoNet to operate as a collaborative regional network."))
        )
      )
    ),
    public_footer(language)
  )
}

governance_page <- function(language = "es") {
  steering_committee <- list(
    list(
      group = tr(language, "Coordinación regional", "Regional coordination"),
      institution = "SICA",
      role = tr(language, "Presidencia Pro Tempore - República Dominicana", "Pro Tempore Presidency - Dominican Republic"),
      representative = tr(language, "Entomólogo/a designado/a por el país - Por confirmar", "Country-designated entomologist - To be confirmed")
    ),
    list(
      group = tr(language, "Coordinación regional", "Regional coordination"),
      institution = "CECOMISCA",
      role = tr(language, "Presidencia de CECOMISCA", "CECOMISCA Presidency"),
      representative = tr(language, "Presidente/a de CECOMISCA", "President of CECOMISCA")
    ),
    list(
      group = tr(language, "Coordinación académica", "Academic coordination"),
      institution = "Universidad del Valle de Guatemala",
      role = "Guatemala",
      representative = tr(language, "Norma Padilla y José Juárez", "Norma Padilla and José Juárez")
    ),
    list(
      group = tr(language, "Representación nacional", "National representation"),
      institution = "Honduras",
      role = tr(language, "Representante nacional", "National representative"),
      representative = "Denis Escobar"
    ),
    list(
      group = tr(language, "Representación nacional", "National representation"),
      institution = "El Salvador",
      role = tr(language, "Representante nacional", "National representative"),
      representative = tr(language, "Por confirmar (TBN)", "To be confirmed (TBN)")
    ),
    list(
      group = tr(language, "Representación nacional", "National representation"),
      institution = "Costa Rica",
      role = tr(language, "Institución representante", "Representing institution"),
      representative = "INCIENSA"
    ),
    list(
      group = tr(language, "Representación nacional", "National representation"),
      institution = tr(language, "Panamá", "Panama"),
      role = tr(language, "Representante nacional", "National representative"),
      representative = "José Loaiza"
    ),
    list(
      group = tr(language, "Representación nacional", "National representation"),
      institution = tr(language, "República Dominicana", "Dominican Republic"),
      role = tr(language, "Representante nacional", "National representative"),
      representative = tr(language, "Por confirmar (TBN)", "To be confirmed (TBN)")
    )
  )

  steering_committee_cards <- lapply(steering_committee, function(member) {
    div(
      class = "steering-member-card",
      span(class = "steering-member-group", member$group),
      h4(member$institution),
      p(class = "steering-member-role", member$role),
      p(class = "steering-member-name", member$representative)
    )
  })

  tagList(
    public_header("governance", show_login_button = TRUE, language = language),
    tags$main(
      class = "public-content",
      div(
        class = "governance-hero",
        span(class = "program-eyebrow", tr(language, "Dirección y coordinación regional", "Regional leadership and coordination")),
        h1(tr(language, "Gobernanza Regional", "Regional Governance")),
        p(tr(language, "La gobernanza de EntoNet facilita la definición conjunta de prioridades, la coordinación institucional y el seguimiento de los acuerdos regionales.", "EntoNet's governance facilitates joint priority setting, institutional coordination, and follow-up of regional agreements."))
      ),
      div(
        class = "steering-committee-component governance-steering-committee",
        span(class = "program-section-eyebrow", tr(language, "Gobernanza regional", "Regional governance")),
        h3(tr(language, "Comité Directivo", "Steering Committee")),
        p(class = "steering-committee-intro", tr(language, "El Comité Directivo orientará las prioridades estratégicas de EntoNet y facilitará la coordinación entre SICA, CECOMISCA, UVG y los representantes nacionales.", "The Steering Committee will guide EntoNet's strategic priorities and facilitate coordination among SICA, CECOMISCA, UVG, and national representatives.")),
        div(class = "steering-committee-grid", tagList(steering_committee_cards))
      )
    ),
    public_footer(language)
  )
}

collaborators_page <- function(language = "es") {
  collaborators <- list(
    list(
      acronym = "OPS",
      name = "Organización Panamericana de la Salud (OPS/PAHO)",
      lead = NULL,
      url = "https://www.paho.org/es",
      description = tr(language, "La OPS actúa como socio estratégico regional promoviendo el fortalecimiento de los sistemas de vigilancia, la gestión integrada de vectores, el monitoreo de resistencia a insecticidas y la implementación de estrategias basadas en evidencia para la prevención y control de enfermedades transmitidas por vectores. Su amplia experiencia en coordinación regional y apoyo a los Ministerios de Salud facilita la armonización de lineamientos técnicos y el intercambio de buenas prácticas entre países.", "PAHO serves as a regional strategic partner by promoting stronger surveillance systems, integrated vector management, insecticide-resistance monitoring, and evidence-based strategies for preventing and controlling vector-borne diseases.")
    ),
    list(
      acronym = "CHAI",
      name = "Clinton Health Access Initiative",
      lead = NULL,
      url = "https://www.clintonhealthaccess.org/",
      description = tr(language, "CHAI aporta experiencia en el fortalecimiento de programas nacionales de salud mediante el uso estratégico de datos, vigilancia entomológica y optimización de intervenciones para enfermedades transmitidas por vectores. Su trabajo en América Latina ha contribuido a mejorar la capacidad de los gobiernos para analizar información entomológica, fortalecer sistemas de vigilancia y apoyar la toma de decisiones basadas en evidencia para el control de dengue, malaria y otras enfermedades tropicales.", "CHAI contributes expertise in strengthening national health programs through strategic data use, entomological surveillance, and optimization of interventions for vector-borne diseases.")
    ),
    list(
      acronym = "UNAH",
      name = "Universidad Nacional Autónoma de Honduras",
      lead = "Dr. Denis Escobar",
      url = "https://www.unah.edu.hn/",
      description = tr(language, "La Universidad Nacional Autónoma de Honduras contribuye a EntoNet mediante su experiencia académica, investigación aplicada y formación de recursos humanos en salud pública, entomología y vigilancia epidemiológica. A través del liderazgo del Dr. Denis Escobar, la institución fortalece los esfuerzos de capacitación, análisis de datos y colaboración con los programas nacionales de vigilancia y control de vectores en Honduras.", "The National Autonomous University of Honduras contributes academic expertise, applied research, and workforce development in public health, entomology, and epidemiological surveillance under the leadership of Dr. Denis Escobar.")
    ),
    list(
      acronym = "INDICASAT-AIP",
      name = "Instituto de Investigaciones Científicas y Servicios de Alta Tecnología de Panamá",
      lead = "Dr. José Loaiza",
      url = "https://indicasat.org.pa/",
      description = tr(language, "INDICASAT-AIP es uno de los principales centros de investigación biomédica y ecológica de Panamá. A través del liderazgo del Dr. José Loaiza, reconocido investigador en ecología de enfermedades transmitidas por vectores y vigilancia de arbovirus, la institución aporta experiencia en investigación entomológica, análisis ecológico, enfermedades emergentes y enfoques One Health para comprender los factores ambientales que influyen en la transmisión de enfermedades vectoriales.", "INDICASAT-AIP is a leading biomedical and ecological research center in Panama. Under Dr. José Loaiza's leadership, it contributes expertise in entomological research, disease ecology, arbovirus surveillance, emerging diseases, and One Health approaches.")
    ),
    list(
      acronym = "INCIENSA",
      name = "Instituto Costarricense de Investigación y Enseñanza en Nutrición y Salud",
      lead = "Dra. Camila Conejo",
      url = "https://www.inciensa.sa.cr/",
      description = tr(language, "INCIENSA es la institución de referencia nacional de Costa Rica para la vigilancia, investigación y diagnóstico en salud pública. A través de la colaboración de la Dra. Camila Conejo y su equipo, la institución aporta capacidades en vigilancia entomológica, diagnóstico de enfermedades transmitidas por vectores, gestión de información y vinculación con los sistemas nacionales de vigilancia, contribuyendo a fortalecer la integración de datos y la generación de evidencia para la toma de decisiones en salud pública.", "INCIENSA is Costa Rica's national reference institution for public-health surveillance, research, and diagnostics. Dr. Camila Conejo and her team contribute expertise in entomological surveillance, vector-borne disease diagnostics, information management, and national surveillance systems.")
    )
  )

  collaborator_cards <- lapply(collaborators, function(collaborator) {
    div(
      class = "collaborator-card",
      div(class = "collaborator-mark", collaborator$acronym),
      div(
        class = "collaborator-copy",
        h3(collaborator$name),
        if (!is.null(collaborator$lead)) {
          p(class = "collaborator-lead", collaborator$lead)
        },
        p(collaborator$description),
        tags$a(
          href = collaborator$url,
          target = "_blank",
          rel = "noopener noreferrer",
          class = "collaborator-link",
          tr(language, "Visitar página institucional", "Visit institutional website")
        )
      )
    )
  })

  tagList(
    public_header("collaborators", show_login_button = TRUE, language = language),
    tags$main(
      class = "public-content",
      div(
        class = "collaborators-hero",
        span(class = "program-eyebrow", tr(language, "Cooperación técnica y científica", "Technical and scientific cooperation")),
        h1(tr(language, "Colaboradores Estratégicos", "Strategic Collaborators")),
        p(tr(language, "EntoNet cuenta con el apoyo de organizaciones regionales e instituciones académicas y de investigación que aportan experiencia técnica, científica y programática para fortalecer la vigilancia entomológica y el control de vectores en Centroamérica y República Dominicana.", "EntoNet is supported by regional organizations and academic and research institutions that contribute technical, scientific, and programmatic expertise to strengthen entomological surveillance and vector control across Central America and the Dominican Republic."))
      ),
      div(class = "collaborators-grid", tagList(collaborator_cards)),
      div(
        class = "collaborators-closing",
        p(tr(language, "En conjunto, estos colaboradores fortalecen la capacidad técnica y científica de EntoNet, promoviendo la interoperabilidad de datos, la innovación en vigilancia entomológica y la coordinación regional necesaria para enfrentar las amenazas actuales y emergentes asociadas a los vectores de importancia médica.", "Together, these collaborators strengthen EntoNet's technical and scientific capacity, promoting data interoperability, innovation in entomological surveillance, and regional coordination to address current and emerging vector threats."))
      )
    ),
    public_footer(language)
  )
}

network_map_page <- function(language = "es") {
  tagList(
    public_header("network_map", show_login_button = TRUE, language = language),
    tags$main(
      class = "public-content",
      div(
        class = "network-map-hero",
        span(class = "program-eyebrow", tr(language, "Cobertura regional", "Regional coverage")),
        h1(tr(language, "Mapa de la Red", "Network Map")),
        p(tr(language, "Explore la presencia regional de EntoNet, los sitios de colecta y las instituciones colaboradoras. Seleccione un país en el mapa para consultar un dashboard general con datos simulados.", "Explore EntoNet's regional presence, collection sites, and collaborating institutions. Select a country on the map to view a general dashboard with mock data."))
      ),
      div(
        class = "network-country-selector",
        selectInput(
          "network_country_selector",
          tr(language, "Seleccione un país para consultar su dashboard", "Select a country to view its dashboard"),
          choices = stats::setNames(country_choices, vapply(country_choices, display_country, character(1), language = language)),
          selected = "Guatemala"
        )
      ),
      div(
        class = "network-map-layout",
        div(
          class = "network-map-card",
          div(
            class = "network-map-legend",
            span(class = "network-legend-item", span(class = "network-legend-dot country-dot"), tr(language, "País participante", "Participating country")),
            span(class = "network-legend-item", span(class = "network-legend-dot collection-dot"), tr(language, "Sitio de colecta", "Collection site")),
            span(class = "network-legend-item", span(class = "network-legend-dot collaborator-dot"), tr(language, "Colaborador estratégico", "Strategic collaborator"))
          ),
          leafletOutput("regional_network_leaflet", height = "650px")
        ),
        div(
          class = "network-dashboard-card",
          uiOutput("network_country_dashboard")
        )
      )
    ),
    public_footer(language)
  )
}

network_impact_page <- function(language = "es") {
  impact_components <- list(
    list(
      number = "01",
      title = tr(language, "Distribución de insecticidas", "Insecticide distribution"),
      description = tr(language, "Apoyo a la coordinación regional para facilitar el acceso oportuno a insecticidas, materiales de referencia y herramientas necesarias para fortalecer las actividades de vigilancia y control de vectores.", "Regional coordination support to facilitate timely access to insecticides, reference materials, and tools needed to strengthen vector surveillance and control activities.")
    ),
    list(
      number = "02",
      title = tr(language, "Armonización de procedimientos regionales", "Harmonization of regional procedures"),
      description = tr(language, "Desarrollo y adopción de procedimientos estandarizados que permitan comparar información, mejorar la calidad de los datos y fortalecer la interoperabilidad entre los países participantes.", "Development and adoption of standardized procedures that enable information comparison, improve data quality, and strengthen interoperability among participating countries.")
    ),
    list(
      number = "03",
      title = tr(language, "Capacitación técnica regional", "Regional technical training"),
      description = tr(language, "Fortalecimiento de capacidades mediante capacitaciones en resistencia a insecticidas, manejo de insectarios, identificación de mosquitos y uso de QGIS para análisis y visualización espacial.", "Capacity strengthening through training in insecticide resistance, insectary management, mosquito identification, and QGIS for spatial analysis and visualization.")
    ),
    list(
      number = "04",
      title = tr(language, "Protocolos", "Protocols"),
      description = tr(language, "Elaboración, revisión y difusión de protocolos regionales para actividades de campo, laboratorio, control de calidad y gestión estandarizada de información entomológica.", "Development, review, and dissemination of regional protocols for field activities, laboratory procedures, quality control, and standardized entomological information management.")
    ),
    list(
      number = "05",
      title = tr(language, "Programa de pasantías", "Internship program"),
      description = tr(language, "Intercambio de conocimientos y experiencia práctica entre instituciones mediante pasantías técnicas que permitan fortalecer competencias, redes profesionales y colaboración regional.", "Exchange of knowledge and practical experience among institutions through technical internships that strengthen skills, professional networks, and regional collaboration.")
    )
  )

  impact_cards <- lapply(impact_components, function(component) {
    div(
      class = "impact-component-card",
      span(class = "impact-component-number", component$number),
      div(
        class = "impact-component-copy",
        h3(component$title),
        p(component$description)
      )
    )
  })

  training_topics <- c(
    tr(language, "Resistencia a insecticidas", "Insecticide resistance"),
    tr(language, "Manejo de insectarios", "Insectary management"),
    tr(language, "Identificación de mosquitos", "Mosquito identification"),
    "QGIS"
  )

  tagList(
    public_header("network_impact", show_login_button = TRUE, language = language),
    tags$main(
      class = "public-content",
      div(
        class = "impact-hero",
        span(class = "program-eyebrow", tr(language, "Resultados y fortalecimiento regional", "Regional results and capacity building")),
        h1(tr(language, "Impacto de la Red", "Network Impact")),
        p(tr(language, "EntoNet fortalece la capacidad regional mediante recursos compartidos, procedimientos armonizados, formación técnica y oportunidades de intercambio entre instituciones.", "EntoNet strengthens regional capacity through shared resources, harmonized procedures, technical training, and exchange opportunities among institutions."))
      ),
      div(class = "impact-component-grid", tagList(impact_cards)),
      div(
        class = "impact-training-panel",
        div(
          class = "impact-training-copy",
          span(class = "program-section-eyebrow", tr(language, "Capacidades prioritarias", "Priority capacities")),
          h2(tr(language, "Áreas de capacitación", "Training areas")),
          p(tr(language, "Las capacitaciones regionales buscan fortalecer habilidades prácticas y promover metodologías compartidas entre los equipos participantes.", "Regional training activities strengthen practical skills and promote shared methodologies among participating teams."))
        ),
        div(
          class = "impact-training-tags",
          lapply(training_topics, function(topic) span(class = "impact-training-tag", topic))
        )
      ),
      div(
        class = "impact-next-steps",
        h2(tr(language, "Medición futura del impacto", "Future impact measurement")),
        p(tr(language, "Esta sección será ampliada con indicadores regionales, número de personas capacitadas, protocolos armonizados, instituciones participantes, pasantías realizadas y recursos distribuidos.", "This section will be expanded with regional indicators, numbers of people trained, harmonized protocols, participating institutions, completed internships, and distributed resources."))
      )
    ),
    public_footer(language)
  )
}

authenticated_page <- function() {
  tagList(
    div(
      class = "site-header",
      img(src = "entonet-header.jpeg", class = "header-logo", alt = "Logo EntoNet"),
      div(
        class = "header-actions",
        span(class = "header-user", strong(uiOutput("header_user_label", inline = TRUE))),
        actionButton("profile", "Perfil", class = "btn-default header-profile"),
        actionButton("logout", "Cerrar sesión", class = "btn-default header-logout")
      )
    ),
    div(
      class = "portal-shell",
      tags$aside(class = "portal-sidebar", uiOutput("portal_sidebar")),
      tags$main(
        class = "portal-workspace",
        uiOutput("connect_user_greeting"),
        tags$div(id = "sat26_scroll_anchor", class = "sat26-scroll-anchor"),
        uiOutput("module_area")
      )
    ),
    div(
      class = "institutional-footer",
      div(class = "footer-title", "Coordinado por:"),
      div(
        class = "footer-logos",
        tags$a(
          href = "https://www.ces.uvg.edu.gt/page/",
          target = "_blank",
          rel = "noopener noreferrer",
              img(src = "ces-logo-cropped.png", class = "footer-logo ces-logo", alt = "Logo CES")
        ),
        tags$a(
          href = "https://www.uvg.edu.gt/",
          target = "_blank",
          rel = "noopener noreferrer",
          img(src = "uvg-logo.jpg", class = "footer-logo uvg-footer-logo", alt = "Logo Universidad del Valle de Guatemala")
        )
      )
    )
  )
}

individual_egg_count_form <- function() {
  tagList(
    div(class = "top-alerts", uiOutput("validation_message")),
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h4("Observación"),
          selectInput("country", "País", choices = country_choices, selected = "Guatemala"),
          numericInput("cycle", "Ciclo", value = 1, min = 1, step = 1),
          selectInput("round_number", "Ronda", choices = 1:4),
          numericInput("quadrant", "Cuadrante", value = NA, min = 1, step = 1),
          textInput("oviposition_code", "Código de oviposición", placeholder = "Ejemplo: SV1001"),
          textInput("substrate_code", "Código de sustrato", placeholder = "Ejemplo: A"),
          textInput("collection_site", "Sitio de colecta"),
          dateInput("placement_date", "Fecha de colocación", value = NA),
          dateInput("removal_date", "Fecha de retiro", value = NA),
          dateInput("count_date", "Fecha de conteo", value = Sys.Date()),
          textInput("count_responsible_code", "Código de responsable de conteo")
        )
      ),
      column(
        width = 6,
        wellPanel(
          h4("Conteo de huevos"),
          numericInput("intact_eggs", "Huevos intactos", value = 0, min = 0, step = 1),
          numericInput("hatched_eggs", "Huevos eclosionados", value = 0, min = 0, step = 1),
          numericInput("canoe_eggs", "Huevos canoa", value = 0, min = 0, step = 1),
          numericInput("unfertilized_eggs", "Huevos no fecundados", value = 0, min = 0, step = 1),
          numericInput("other_species_count", "Conteo de otras especies", value = 0, min = 0, step = 1),
          textAreaInput("notes", "Notas", rows = 4),
          div(
            class = "summary-box",
            strong("Total calculado: "),
            textOutput("calculated_total", inline = TRUE)
          ),
          div(
            class = "submit-row",
            actionButton("submit", "Enviar registro pendiente", class = "btn-primary")
          )
        )
      )
    ),
    h4("Vista previa del envío"),
    tableOutput("preview"),
    h4("Estado del envío"),
    verbatimTextOutput("submission_status")
  )
}

formulario_5_capture_form <- function() {
  tagList(
    div(
      class = "alert alert-info",
      "Complete los metadatos y las secciones de alimentación/conteo. Puede moverse entre pestañas para revisar la captura."
    ),
    tabsetPanel(
      id = "f5_capture_tab",
      tabPanel(
        "Metadatos",
        value = "metadatos",
        fluidRow(
          column(
            width = 8,
            wellPanel(
              h4("Metadatos"),
              textInput("f5_formulario_codigo", "Código del formulario", value = "F5"),
              selectInput("f5_pais", "País", choices = c("El Salvador", "Guatemala"), selected = "El Salvador"),
              textInput("f5_id_institucion", "ID Institución", value = default_institution_id),
              numericInput("f5_departamento_numero", "Departamento #", value = NA, min = 0, step = 1),
              numericInput("f5_municipio_numero", "Municipio #", value = NA, min = 0, step = 1),
              textInput("f5_ciclo", "Ciclo", value = "", placeholder = "Ingrese ciclo"),
              textInput("f5_formulario_nombre", "Nombre del formulario", value = "Alimentación sanguínea y conteo huevecillos Aedes spp."),
              dateInput("f5_fecha_registro", "Fecha de registro", value = Sys.Date())
            )
          )
        )
      ),
      tabPanel(
        "Datos generales",
        value = "datos_generales",
        fluidRow(
          column(
            width = 8,
            wellPanel(
              h4("Datos generales"),
              textInput("f5_cepa_poblacion", "Cepa / población"),
              selectInput("f5_especie", "Especie", choices = c("Ae. aegypti", "Ae. albopictus")),
              textInput("f5_generacion_filial_adultos", "Generación filial adultos"),
              textInput("f5_responsable_ingreso_jaula", "Responsable ingreso jaula"),
              dateInput("f5_fecha_jaula", "Fecha jaula", value = Sys.Date()),
              uiOutput("f5_fecha_jaula_alert"),
              numericInput("f5_numero_hembras", "Número de hembras", value = 0, min = 0, step = 1),
              numericInput("f5_numero_machos", "Número de machos", value = 0, min = 0, step = 1),
              div(class = "summary-box", strong("Total individuos: "), textOutput("f5_total_individuos", inline = TRUE)),
              numericInput("f5_total_huevos_viables", "Total huevos viables", value = NA, min = 0, step = 1)
            )
          )
        )
      ),
      tabPanel(
        "Alimentación",
        value = "alimentacion",
        fluidRow(
          column(
            width = 8,
            wellPanel(
              h4("Alimentación sanguínea"),
              textInput("f5_responsable_alimentacion", "Responsable alimentación"),
              selectInput("f5_tipo_alimentacion_codigo", "Tipo alimentación código", choices = c("A", "B", "C", "D", "E")),
              selectInput(
                "f5_tipo_alimentacion_descripcion",
                "Tipo alimentación descripción",
                choices = c(
                  "Sin dato" = "",
                  "conejo",
                  "humano",
                  "hemotek-conejo",
                  "hemotek-humano",
                  "hemotek-carnero"
                )
              ),
              dateInput("f5_fecha_alimentacion_sangre", "Fecha alimentación sangre", value = Sys.Date()),
              uiOutput("f5_fecha_alimentacion_alert"),
              numericInput("f5_numero_charolas", "Número de charolas", value = 0, min = 0, step = 1),
              textAreaInput("f5_observaciones_alimentacion", "Observaciones alimentación", rows = 4)
            )
          )
        )
      ),
      tabPanel(
        "Conteo de huevecillos",
        value = "conteo_huevecillos",
        fluidRow(
          column(
            width = 8,
            wellPanel(
              h4("Conteo de huevecillos"),
              textInput("f5_generacion_filial_huevos", "Generación filial huevos"),
              textInput("f5_codigo_sustrato", "Código sustrato"),
              dateInput("f5_fecha_colocacion_sustrato", "Fecha colocación sustrato", value = Sys.Date()),
              dateInput("f5_fecha_retiro_sustrato", "Fecha retiro sustrato", value = Sys.Date()),
              uiOutput("f5_fecha_sustrato_alert"),
              numericInput("f5_numero_cuadro_sustrato", "Número cuadro sustrato", value = 0, min = 0, step = 1),
              numericInput("f5_hv_huevos_viables", "HV - huevos viables", value = 0, min = 0, step = 1),
              numericInput("f5_he_huevos_eclosionados", "HE - huevos eclosionados", value = 0, min = 0, step = 1),
              numericInput("f5_hc_huevos_canoa", "HC - huevos canoa", value = 0, min = 0, step = 1),
              numericInput("f5_hnf_huevos_no_fecundados", "HNF - huevos no fecundados", value = 0, min = 0, step = 1),
              div(class = "summary-box", strong("Total huevos: "), textOutput("f5_total_huevos", inline = TRUE)),
              textInput("f5_responsable_conteo_huevos", "Responsable conteo huevos")
            )
          )
        )
      ),
      tabPanel(
        "Observaciones",
        value = "observaciones",
        fluidRow(
          column(
            width = 8,
            wellPanel(
              h4("Observaciones y auditoría"),
              textAreaInput("f5_observaciones_generales", "Observaciones generales", rows = 4),
              textInput("f5_fuente_formulario", "Fuente formulario"),
              textInput("f5_creado_por", "Creado por"),
              dateInput("f5_creado_en", "Fecha creación", value = Sys.Date())
            )
          )
        )
      )
    ),
    div(
      class = "submit-row",
      uiOutput("f5_capture_navigation"),
      uiOutput("f5_save_button_area")
    ),
    uiOutput("f5_certification_overlay")
  )
}

formulario_1_capture_form <- function() {
  tagList(
    div(
      class = "alert alert-info",
      "Complete los datos generales, configure la colocación y luego capture cada cuadrante con sus casas y ovitrampas."
    ),
    tabsetPanel(
      id = "f1_capture_tab",
      tabPanel(
        "Datos generales",
        wellPanel(
          h4("Continuar formulario existente"),
          fluidRow(
            column(
              8,
              textInput("f1_resume_codigo_formulario", "Código de formulario", placeholder = "Ingrese el código guardado parcialmente")
            ),
            column(
              4,
              br(),
              actionButton("f1_resume_form", "Cargar formulario", class = "btn-default")
            )
          ),
          uiOutput("f1_resume_status")
        ),
        fluidRow(
          column(
            6,
            selectInput("f1_pais", "País *", choices = c("Seleccione" = "", "El Salvador" = "El Salvador", "Guatemala" = "Guatemala"), selected = ""),
            textInput("f1_id_institucion", "ID Institución *", value = default_institution_id),
            selectInput("f1_departamento", "Departamento *", choices = c("Seleccione país" = "")),
            uiOutput("f1_municipio_ui"),
            textInput("f1_ciclo", "Ciclo *"),
            textInput("f1_ronda", "Ronda"),
            textInput("f1_codigo_formulario", "Código de formulario *"),
            dateInput("f1_fecha_registro", "Fecha de ingreso formulario *", value = NA)
          ),
          column(
            6,
            numericInput("f1_Latitud", "Latitud", value = NA, min = -90, max = 90),
            numericInput("f1_Longitud", "Longitud", value = NA, min = -180, max = 180),
            textInput("f1_codigo_gps", "Código GPS"),
            textInput("f1_fuente_formulario", "Versión del formulario"),
            textInput("f1_creado_por", "Nombre de quien ingresó")
          )
        ),
        div(
          class = "submit-row",
          actionButton("f1_confirm_placement", "Siguiente", class = "btn-primary"),
          uiOutput("f1_placement_status")
        )
      ),
      tabPanel(
        "Colocación",
        fluidRow(
          column(
            6,
            dateInput("f1_fecha_colocacion", "Fecha de colocación *", value = NA),
            textInput("f1_grupo_responsable_colocacion", "Grupo responsable colocación"),
            numericInput("f1_num_quadrants", "Número de cuadrantes *", value = NA, min = 1, step = 1),
            numericInput("f1_codigo_cuadrante_numero", "# cuadrante inicial", value = 1, min = 1, step = 1),
            textInput("f1_codigo_cuadrante_base", "Código cuadrante *", placeholder = "REI25GT0503C001"),
            tags$small("Estructura nueva: REI + año + país + código municipio + C###. Ejemplo: REI25GT0503C001. Por ahora puede editarse para datos antiguos."),
            uiOutput("f1_codigo_cuadrante_preview"),
            numericInput("f1_casas_por_cuadrante", "Casas por cuadrante *", value = NA, min = 1, step = 1)
          ),
          column(
            6,
            textInput("f1_codigo_casa_base", "Código casa *"),
            numericInput("f1_ovitrampas_por_casa", "Ovitrampas por casa *", value = NA, min = 1, max = 8, step = 1),
            textInput("f1_codigo_sustrato_base", "Código sustrato *"),
            dateInput("f1_fecha_retiro", "Fecha de retiro", value = NA),
            textInput("f1_grupo_responsable_retiro", "Código responsable retiro")
          )
        ),
        div(
          class = "submit-row",
          actionButton("f1_generate_quadrants", "Generar cuadrantes", class = "btn-primary"),
          uiOutput("f1_placement_status")
        )
      ),
      tabPanel(
        "Cuadrantes",
        uiOutput("f1_save_status"),
        uiOutput("f1_quadrant_tabs"),
        div(
          class = "submit-row",
          actionButton("save_formulario_1", "Guardar registro pendiente", class = "btn-primary")
        ),
        uiOutput("f1_save_status_bottom")
      )
    )
  )
}

formulario_7_count_pair <- function(prefix, label) {
  fluidRow(
    column(4, strong(label)),
    column(4, numericInput(paste0("f7_", prefix, "_vivos"), "Vivos", value = NA, min = 0, step = 1)),
    column(4, numericInput(paste0("f7_", prefix, "_incapacitados"), "Incapacitados", value = NA, min = 0, step = 1))
  )
}

formulario_7_bottle_panel <- function(bottle) {
  reading_rows <- lapply(c(0, 15, 30, 45), function(minutes) {
    formulario_7_count_pair(paste0("resultado_", minutes, "min_", bottle), paste(minutes, "minutos"))
  })
  insecticide_after_synergist_rows <- lapply(c(0, 15, 30, 45), function(minutes) {
    formulario_7_count_pair(paste0("resultado_", minutes, "min_", bottle), paste(minutes, "minutos"))
  })

  tagList(
    conditionalPanel(
      "input.f7_insecticida != 'Temefos'",
      textInput(
        paste0("f7_resultado_hora_inicio_", bottle),
        "Hora de inicio de la botella (HH:MM)",
        placeholder = "08:30"
      )
    ),
    conditionalPanel(
      "input.f7_insecticida == 'Temefos'",
      div(class = "alert alert-info", "Para Temefos solo se registra la lectura de 24 horas.")
    ),
    conditionalPanel(
      "input.f7_tipo_bioensayo != 'sinergistas' && input.f7_insecticida != 'Temefos'",
      reading_rows,
      tagList(
        tags$hr(),
        h5("Lectura KDR a 24 horas"),
        textInput(paste0("f7_resultado_hora_lectura_24h_", bottle), "Hora de lectura (HH:MM)", placeholder = "08:30"),
        formulario_7_count_pair(paste0("resultado_24h_", bottle), "24 horas")
      )
    ),
    conditionalPanel(
      "input.f7_tipo_bioensayo != 'sinergistas' && input.f7_insecticida == 'Temefos'",
      h5("Lectura a 24 horas"),
      textInput(paste0("f7_resultado_hora_lectura_24h_", bottle), "Hora de lectura (HH:MM)", placeholder = "08:30"),
      formulario_7_count_pair(paste0("resultado_24h_", bottle), "24 horas")
    ),
    conditionalPanel(
      "input.f7_tipo_bioensayo == 'sinergistas' && input.f7_insecticida != 'Temefos'",
      h4("8. Sinergista"),
      div(
        class = "alert alert-info",
        "Registre la lectura del sinergista a los 60 minutos antes de aplicar el insecticida."
      ),
      formulario_7_count_pair(paste0("resultado_60min_", bottle), "60 minutos"),
      tags$hr(),
      h4("9. Lectura por botella"),
      div(
        class = "alert alert-info",
        "Después del sinergista se registra la dosis diagnóstica 1X del insecticida a 0, 15, 30, 45 minutos y 24 horas."
      ),
      insecticide_after_synergist_rows,
      tagList(
        tags$hr(),
        h5("Lectura KDR a 24 horas"),
        textInput(paste0("f7_resultado_hora_lectura_24h_", bottle), "Hora de lectura (HH:MM)", placeholder = "08:30"),
        formulario_7_count_pair(paste0("resultado_24h_", bottle), "24 horas")
      )
    ),
    conditionalPanel(
      "input.f7_tipo_bioensayo == 'sinergistas' && input.f7_insecticida == 'Temefos'",
      h4("Lectura por botella"),
      div(class = "alert alert-info", "Para Temefos solo se registra la lectura de 24 horas."),
      textInput(paste0("f7_resultado_hora_lectura_24h_", bottle), "Hora de lectura (HH:MM)", placeholder = "08:30"),
      formulario_7_count_pair(paste0("resultado_24h_", bottle), "24 horas")
    )
  )
}

formulario_7_capture_form <- function() {
  bottle_tabs <- lapply(formulario_7_bottles, function(bottle) {
    tabPanel(formulario_7_bottle_labels[[bottle]], formulario_7_bottle_panel(bottle))
  })

  tagList(
    div(class = "alert alert-info", "Complete los campos obligatorios. Las lecturas están agrupadas por botella y se guardarán como un registro pendiente de revisión."),
    tabsetPanel(
      id = "f7_capture_tab",
      tabPanel(
        "Información General",
        value = "informacion_general",
        fluidRow(
          column(6,
            div(
              class = "f7-help-field",
              div(
                class = "f7-help-label-row",
                tags$label(`for` = "f7_codigo_bioensayo", "Código de bioensayo *"),
                actionButton(
                  "f7_codigo_bioensayo_help",
                  label = "?",
                  class = "f7-help-button",
                  title = "Ayuda sobre el código de bioensayo",
                  `aria-label` = "Mostrar ayuda sobre el código de bioensayo"
                )
              ),
              textInput("f7_codigo_bioensayo", label = NULL),
              conditionalPanel(
                "input.f7_codigo_bioensayo_help % 2 == 1",
                div(
                  class = "f7-help-message",
                  "Para registros nuevos, genere este código antes de ingresar datos desde la sección Imprimir formulario del Formulario 7. El código parte del código de cuadrante del Formulario 1: REI + año + país + departamento + municipio + cuadrante. Al consolidar población, el cuadrante cambia a P#. En bioensayo se usa: REI + año + país + departamento + municipio + P# + código de sinergista solo si aplica (DEF, PBO o DM) + insecticida (DDT, PER, DEL, BEN, MAL, ALF, LAM o TEM) + correlativo incremental + generación filial (F#). No se agrega sufijo -D; el correlativo después del insecticida identifica el bioensayo. Ejemplo: REI26GT0920P2DEFDEL1F1. Para datos antiguos puede ingresar el código histórico tal como aparece en el formulario."
                )
              )
            ),
            selectInput("f7_pais", "País *", choices = c("El Salvador", "Guatemala")),
            textInput("f7_id_institucion", "ID Institución *", value = default_institution_id),
            selectInput("f7_codigo_departamento", "Departamento *", choices = c("Seleccione" = "")),
            uiOutput("f7_codigo_municipio_ui"),
            textInput("f7_nombre_poblacion", "Nombre de la población *"),
            textInput("f7_creado_por", "Nombre de quien ingresó", value = profile_name)
          ),
          column(6,
            radioButtons(
              "f7_tipo_bioensayo",
              "Tipo de Bioensayo *",
              choices = c("Diagnóstica 1X" = "diagnostica_1x", "Intensidad" = "intensidad", "Sinergistas" = "sinergistas"),
              inline = TRUE
            ),
            conditionalPanel(
              "input.f7_tipo_bioensayo == 'diagnostica_1x' || input.f7_tipo_bioensayo == 'intensidad' || input.f7_tipo_bioensayo == 'sinergistas'",
              radioButtons(
                "f7_resultado_diagnostico",
                "Resultado de la prueba diagnóstica *",
                choices = c("Suceptible", "Sospecha de Resistencia", "Resistente"),
                selected = character(0)
              )
            ),
            conditionalPanel(
              "input.f7_tipo_bioensayo == 'intensidad'",
              radioButtons("f7_bioensayo_intensidad", "Intensidad *", choices = c("Exploratorio", "Completa"), inline = TRUE),
              conditionalPanel(
                "input.f7_bioensayo_intensidad == 'Exploratorio'",
                div(class = "alert alert-info", strong("Dosis incluidas: "), "1X, 2X, 5X y 10X, más su control. Si el resultado es Resistente o Sospecha de Resistencia, indique la concentración mayor asociada.")
              ),
              conditionalPanel(
                "input.f7_bioensayo_intensidad == 'Completa' || (input.f7_bioensayo_intensidad == 'Exploratorio' && (input.f7_resultado_diagnostico == 'Resistente' || input.f7_resultado_diagnostico == 'Sospecha de Resistencia'))",
                selectInput("f7_dosis_intensidad", "Concentración asociada *", choices = c("Seleccione" = "", "1X" = "1X", "2X" = "2X", "5X" = "5X", "10X" = "10X"))
              )
            ),
            conditionalPanel(
              "input.f7_tipo_bioensayo == 'sinergistas'",
              selectInput("f7_sinergista_tipo", "Sinergista *", choices = c("Seleccione" = "", "DEF" = "DEF", "PBO" = "PBO", "DM" = "DM")),
              numericInput("f7_dosis_sinergista_ug_ml", "Dosis sinergista (µg/ml) *", value = NA, min = 0)
            ),
            dateInput("f7_fecha_registro", "Fecha de registro *", value = Sys.Date())
          )
        )
      ),
      tabPanel(
        "Información del Bioensayo",
        value = "informacion_bioensayo",
        fluidRow(
          column(6,
            dateInput("f7_fecha_realizacion_bioensayo", "Fecha de realización *", value = Sys.Date()),
            selectInput(
              "f7_insecticida",
              "Insecticida *",
              choices = formulario_7_insecticide_choices
            ),
            selectInput("f7_solvente_utilizado", "Solvente utilizado *", choices = c("Etanol", "Otro")),
            conditionalPanel("input.f7_solvente_utilizado == 'Otro'", textInput("f7_solvente_otro", "Especifique el solvente *")),
            numericInput("f7_dosis_intensidad_ug_ml", "Concentración (µg/ml) *", value = NA, min = 0),
            div(
              class = "f7-help-field",
              div(
                class = "f7-help-label-row",
                tags$label(`for` = "f7_lote_insecticida", "# lote insecticida *"),
                actionButton(
                  "f7_lote_insecticida_help",
                  label = "?",
                  class = "f7-help-button",
                  title = "Ayuda sobre el lote del insecticida",
                  `aria-label` = "Mostrar ayuda sobre el lote del insecticida"
                )
              ),
              textInput("f7_lote_insecticida", label = NULL),
              conditionalPanel(
                "input.f7_lote_insecticida_help % 2 == 1",
                div(
                  class = "f7-help-message",
                  "Este hace referencia al lote del insecticida que se está evaluando, incluyendo marca, lote, fecha de preparación o información equivalente disponible."
                )
              )
            ),
            dateInput("f7_fecha_revestimiento_botellas", "Fecha de revestimiento *", value = Sys.Date())
          ),
          column(6,
            tags$hr(),
            h4("# Veces se han utilizado las botellas"),
            numericInput("f7_numero_usos_botella_e1", "Botella expuesta E1", value = NA, min = 0, step = 1),
            numericInput("f7_numero_usos_botella_e2", "Botella expuesta E2", value = NA, min = 0, step = 1),
            numericInput("f7_numero_usos_botella_e3", "Botella expuesta E3", value = NA, min = 0, step = 1),
            numericInput("f7_numero_usos_botella_e4", "Botella expuesta E4", value = NA, min = 0, step = 1),
            numericInput("f7_numero_usos_botella_c1", "Control C1", value = NA, min = 0, step = 1)
          )
        )
      ),
      tabPanel(
        "Material y responsables",
        value = "material_responsables",
        fluidRow(
          column(6,
            selectInput("f7_origen_material", "Origen del material *", choices = c("Silvestre", "Laboratorio")),
            numericInput("f7_edad_dias", "Edad (días)", value = NA, min = 0, step = 1),
            checkboxInput("f7_edad_indefinida", "Edad indefinida", FALSE),
            textInput("f7_codigo_especie_mosquito", "Código de especie *"),
            dateInput("f7_fecha_separacion", "Fecha de separación *", value = Sys.Date()),
            textInput("f7_hora_separacion", "Hora de separación (HH:MM) *", placeholder = "08:30"),
            textInput("f7_generacion_filial", "Generación filial"),
            checkboxInput("f7_generacion_filial_indefinida", "Generación filial indefinida", FALSE)
          ),
          column(6,
            textInput("f7_codigo_responsable_revestimiento", "Responsable de revestimiento *"),
            textInput("f7_codigo_responsable_bioensayo", "Responsable del bioensayo *"),
            tags$div(style = "display:none;", textInput("f7_codigo_control_calidad", "Código de control de calidad *", value = "NO APLICA")),
            textInput("f7_codigo_revision_24h", "Código de revisión a 24 h *")
          )
        )
      ),
      tabPanel(
        "Condiciones",
        value = "condiciones",
        fluidRow(
          column(6,
            numericInput("f7_temperatura_inicial_c", "Temperatura inicial (°C) *", value = NA),
            numericInput("f7_temperatura_final_c", "Temperatura final (°C) *", value = NA),
            numericInput("f7_humedad_relativa_inicial_pct", "Humedad relativa inicial (%) *", value = NA, min = 0, max = 100),
            numericInput("f7_humedad_relativa_final_pct", "Humedad relativa final (%) *", value = NA, min = 0, max = 100)
          ),
          column(6,
            textInput("f7_hora_inicio_bioensayo", "Hora de inicio del bioensayo (HH:MM) *", placeholder = "08:30"),
            textInput("f7_hora_final_bioensayo", "Hora final del bioensayo (HH:MM) *", placeholder = "09:15")
          )
        )
      ),
      do.call(tabPanel, c(list(title = "Resultados por botella", value = "resultados"), list(do.call(tabsetPanel, c(list(id = "f7_result_bottle"), bottle_tabs))))),
      tabPanel(
        "Comentarios y envío",
        value = "comentarios_envio",
        fluidRow(
          column(8, textAreaInput("f7_comentario", "Comentario", rows = 3)),
          column(4, textInput("f7_comentario_nombre", "Nombre"))
        ),
        uiOutput("f7_save_status"),
        div(class = "submit-row", actionButton("save_formulario_7", "Guardar registro pendiente", class = "btn-primary"))
      )
    ),
    uiOutput("f7_navigation_status"),
    uiOutput("f7_navigation_controls")
  )
}

formulario_7_print_form <- function() {
  tagList(
    div(
      class = "alert alert-info",
      "Genere el machote de Formulario 7 con el Código Bioensayo y la ubicación nacional prellenados."
    ),
    wellPanel(
      h4("Código Bioensayo"),
      div(
        class = "alert alert-info",
        "La población se define como el grupo de cuadrantes que se emplearán para la evaluación de los bioensayos. Puede estar compuesta por 1 cuadrante o por tantos como la unidad considere necesarios para evaluar un municipio o puntos geográficos más pequeños."
      ),
      fluidRow(
        column(
          6,
          selectInput("f7_print_pais", "País", choices = c("El Salvador", "Guatemala"), selected = "El Salvador"),
          selectInput("f7_print_codigo_bioensayo_departamento", "Departamento", choices = c("Seleccione" = "")),
          uiOutput("f7_print_codigo_bioensayo_municipio_ui"),
          selectInput(
            "f7_print_insecticida",
            "Insecticida",
            choices = formulario_7_insecticide_choices
          )
        ),
        column(
          6,
          textInput("f7_print_codigo_bioensayo_poblacion_numero", "Código población", placeholder = "Ej. P2 o P2.1"),
          numericInput("f7_print_codigo_bioensayo_correlativo", "# Bioensayo", value = 1, min = 1, step = 1),
          textInput("f7_print_generacion_filial", "Generación filial", value = "F1", placeholder = "Ej. F1"),
          numericInput("f7_print_codigo_bioensayo_anio", "Año", value = as.integer(format(Sys.Date(), "%y")), min = 0, max = 99, step = 1),
          textInput("f7_print_version_formulario", "Versión del formulario", value = "2"),
          textInput("f7_print_nombre_poblacion", "Nombre de población"),
          radioButtons(
            "f7_print_tipo_bioensayo",
            "Tipo de Bioensayo",
            choices = c(
              "Dosis Diagnóstica" = "DD",
              "Intensidad Exploratoria" = "IE",
              "Intensidad Completa" = "IC",
              "Sinergista" = "S"
            ),
            selected = "DD"
          ),
          conditionalPanel(
            "input.f7_print_tipo_bioensayo == 'IC'",
            selectInput("f7_print_intensidad_completa_dosis", "Dosis intensidad completa", choices = c("2X", "5X", "10X"))
          ),
          conditionalPanel(
            "input.f7_print_tipo_bioensayo == 'S'",
            selectInput("f7_print_sinergista", "Sinergista", choices = c("DEF", "PBO", "DM"))
          )
        )
      ),
      tags$small("El municipio seleccionado aporta el código nacional. Si no aparece en el catálogo inicial, use la opción de código manual."),
      uiOutput("f7_print_codigo_bioensayo_preview")
    ),
    div(
      class = "submit-row",
      downloadButton("download_formulario_7_printable", "Descargar formulario", class = "btn-primary")
    )
  )
}

formulario_5_review_form <- function() {
  tagList(
    div(
      class = "alert alert-info",
      "Busque un registro por intake_id o genere una muestra aleatoria del 10% de registros ingresados entre dos fechas. Al abrir un ID, redigite el formulario para comparar contra la captura original."
    ),
    wellPanel(
      h4("Selección de registros"),
      fluidRow(
        column(3, numericInput("f5_review_search_id", "Buscar intake_id", value = NA, min = 1, step = 1)),
        column(3, dateInput("f5_review_start_date", "Fecha inicio", value = Sys.Date() - 30)),
        column(3, dateInput("f5_review_end_date", "Fecha fin", value = Sys.Date())),
        column(
          3,
          selectInput(
            "f5_review_filter_status",
            "Estado",
            choices = c("Pendiente" = "pending", "Revisado" = "reviewed", "Rechazado" = "rejected", "Todos" = "all"),
            selected = "pending"
          )
        )
      ),
      fluidRow(
        column(
          6,
          textInput(
            "f5_review_exclude_submitter",
            "Excluir persona que ingresó",
            value = profile_name,
            placeholder = "Ej. Alfredo Camey"
          )
        )
      ),
      div(
        class = "submit-row",
        actionButton("f5_review_find_id", "Buscar ID", class = "btn-primary"),
        actionButton("f5_review_generate_sample", "Generar muestra 10%", class = "btn-primary"),
        actionButton("f5_review_refresh_list", "Actualizar listado")
      ),
      uiOutput("f5_review_status_message"),
      uiOutput("f5_review_sample_list")
    ),
    uiOutput("f5_review_selected_record"),
    uiOutput("f5_review_redigit_form"),
    uiOutput("f5_review_comparison_result")
  )
}

formulario_7_review_form <- function() {
  tagList(
    div(
      class = "alert alert-info",
      "Seleccione un registro de Formulario 7. Los valores se muestran bloqueados hasta presionar Editar. Confirmar cambia el estado del registro a reviewed."
    ),
    wellPanel(
      h4("Registros de Formulario 7"),
      fluidRow(
        column(3, textInput("f7_review_search_code", "Buscar Código de bioensayo")),
        column(3, dateInput("f7_review_start_date", "Fecha inicio", value = as.Date("2026-01-01"))),
        column(3, dateInput("f7_review_end_date", "Fecha fin", value = Sys.Date())),
        column(
          3,
          selectInput(
            "f7_review_filter_status",
            "Estado",
            choices = c("Pendiente" = "pending", "Revisado" = "reviewed", "Rechazado" = "rejected", "Todos" = "all"),
            selected = "pending"
          )
        )
      ),
      fluidRow(
        column(
          6,
          textInput(
            "f7_review_exclude_submitter",
            "Excluir persona que ingresó",
            value = profile_name,
            placeholder = "Ej. Alfredo Camey"
          )
        )
      ),
      div(
        class = "submit-row",
        actionButton("f7_review_find", "Buscar código", class = "btn-default"),
        actionButton("f7_review_generate_sample", "Generar muestra 10%", class = "btn-primary"),
        actionButton("f7_review_refresh", "Actualizar listado", class = "btn-default")
      ),
      uiOutput("f7_review_status_message"),
      uiOutput("f7_review_record_list")
    ),
    uiOutput("f7_review_record_detail")
  )
}

formulario_1_review_form <- function() {
  tagList(
    div(
      class = "alert alert-info",
      "Busque por código de formulario y seleccione la casa a revisar. Los valores se muestran bloqueados hasta presionar Editar. Confirmar cambia el estado del registro a reviewed."
    ),
    wellPanel(
      h4("Registros de Formulario 1"),
      fluidRow(
        column(3, textInput("f1_review_search_code", "Buscar código de formulario", value = "")),
        column(3, dateInput("f1_review_start_date", "Fecha inicio", value = Sys.Date() - 30)),
        column(3, dateInput("f1_review_end_date", "Fecha fin", value = Sys.Date())),
        column(
          3,
          selectInput(
            "f1_review_filter_status",
            "Estado",
            choices = c("Pendiente" = "pending", "Revisado" = "reviewed", "Rechazado" = "rejected", "Todos" = "all"),
            selected = "pending"
          )
        )
      ),
      fluidRow(
        column(
          6,
          textInput(
            "f1_review_exclude_submitter",
            "Excluir persona que ingresó",
            value = profile_name,
            placeholder = "Ej. Alfredo Camey"
          )
        )
      ),
      div(
        class = "submit-row",
        actionButton("f1_review_find", "Buscar código", class = "btn-default"),
        actionButton("f1_review_generate_sample", "Generar muestra 10%", class = "btn-primary"),
        actionButton("f1_review_refresh", "Actualizar listado", class = "btn-default")
      ),
      uiOutput("f1_review_status_message"),
      uiOutput("f1_review_record_list")
    ),
    uiOutput("f1_review_record_detail")
  )
}

formulario_1_print_form <- function() {
  tagList(
    div(
      class = "alert alert-info",
      "Genere un machote imprimible de Formulario 1 para llevar a campo. El archivo se descarga en Excel con una hoja lista para imprimir."
    ),
    wellPanel(
      h4("Datos del formulario de campo"),
      fluidRow(
        column(
          6,
          selectInput("f1_print_pais", "País *", choices = c("Seleccione" = "", "El Salvador" = "El Salvador", "Guatemala" = "Guatemala"), selected = ""),
          selectInput("f1_print_departamento", "Departamento", choices = c("Seleccione país" = "")),
          uiOutput("f1_print_municipio_ui"),
          numericInput("f1_print_ciclo", "Ciclo *", value = NA, min = 1, step = 1),
          numericInput("f1_print_ronda", "Ronda *", value = NA, min = 1, step = 1),
          textInput("f1_print_codigo_encuestadores", "Código de encuestadores")
        ),
        column(
          6,
          numericInput("f1_print_num_quadrants", "Número de cuadrantes por formulario *", value = NA, min = 1, max = 50, step = 1),
          numericInput("f1_print_codigo_cuadrante_numero", "# cuadrante inicial", value = 1, min = 1, step = 1),
          textInput("f1_print_codigo_cuadrante_base", "Código de cuadrante inicial", placeholder = "REI25GT0503C001"),
          tags$small("Para impresión nueva se usa: REI + año + país + código municipio + C###. Ejemplo: REI25GT0503C001."),
          uiOutput("f1_print_codigo_cuadrante_preview"),
          numericInput("f1_print_casas_por_cuadrante", "Número de casas por cuadrante", value = NA, min = 1, max = 50, step = 1),
          textInput("f1_print_codigo_casa_base", "Código inicial de casa"),
          textInput("f1_print_codigo_sustrato_base", "Código inicial sustrato"),
          uiOutput("f1_print_codigo_formulario_preview"),
          tags$small("El código de sustrato se autogenera desde el código inicial de sustrato.")
        )
      ),
      uiOutput("f1_print_status"),
      div(
        class = "submit-row",
        downloadButton("download_formulario_1_printable", "Descargar formulario imprimible", class = "btn-primary")
      )
    )
  )
}

sat26_authenticated_page <- function() {
  tagList(
    div(
      class = "sat26-standalone-header",
      img(src = "entonet-header.jpeg", class = "sat26-standalone-logo", alt = "Logo EntoNet"),
      div(
        class = "sat26-standalone-actions",
        actionButton("logout", "Salir", class = "btn-default header-logout")
      )
    ),
    div(
      class = "sat26-standalone-shell",
      tags$main(
        class = "sat26-standalone-workspace",
        tags$div(id = "sat26_scroll_anchor", class = "sat26-scroll-anchor"),
        uiOutput("module_area")
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "leaflet/leaflet.css"),
    tags$script(src = "leaflet/leaflet.js"),
    tags$script(HTML("
      (function storeInitialSupabaseAuthParams() {
        var params = new URLSearchParams('');
        if (window.location.hash && window.location.hash.length > 1) {
          params = new URLSearchParams(window.location.hash.substring(1));
        } else if (window.location.search && window.location.search.length > 1) {
          params = new URLSearchParams(window.location.search.substring(1));
        }
        if (params.get('access_token') || params.get('error_description')) {
          try {
            window.sessionStorage.setItem('entonet_supabase_auth_params', params.toString());
          } catch (error) {}
        }
      })();
      function sendSupabaseAuthHashToShiny() {
        if (typeof Shiny === 'undefined') {
          return;
        }
        var params = new URLSearchParams('');
        if (window.location.hash && window.location.hash.length > 1) {
          params = new URLSearchParams(window.location.hash.substring(1));
        } else if (window.location.search && window.location.search.length > 1) {
          params = new URLSearchParams(window.location.search.substring(1));
        } else {
          try {
            params = new URLSearchParams(window.sessionStorage.getItem('entonet_supabase_auth_params') || '');
          } catch (error) {
            params = new URLSearchParams('');
          }
        }
        var accessToken = params.get('access_token');
        var refreshToken = params.get('refresh_token');
        var authType = params.get('type');
        var errorDescription = params.get('error_description');
        if (errorDescription) {
          Shiny.setInputValue('supabase_auth_error', errorDescription, {priority: 'event'});
          return;
        }
        if (accessToken && (!authType || authType === 'invite' || authType === 'recovery' || authType === 'signup')) {
          Shiny.setInputValue('supabase_auth_hash', {
            access_token: accessToken,
            refresh_token: refreshToken || '',
            type: authType || 'recovery'
          }, {priority: 'event'});
          try {
            window.sessionStorage.removeItem('entonet_supabase_auth_params');
          } catch (error) {}
          if (window.history && window.history.replaceState) {
            window.history.replaceState(null, document.title, window.location.pathname + window.location.search);
          }
        }
      }
      document.addEventListener('DOMContentLoaded', function() {
        setTimeout(sendSupabaseAuthHashToShiny, 250);
      });
      document.addEventListener('shiny:connected', function() {
        sendSupabaseAuthHashToShiny();
      });
    ")),
    tags$script(HTML("
      function sendEntonetPublicRouteToShiny() {
        if (typeof Shiny === 'undefined') {
          return;
        }
        var params = new URLSearchParams('');
        if (window.location.search && window.location.search.length > 1) {
          params = new URLSearchParams(window.location.search.substring(1));
        }
        var survey = (params.get('survey') || '').toLowerCase();
        var sat26 = (params.get('sat26') || '').toLowerCase();
        if (survey === 'sat26' || ['1', 'true', 'yes', 'si', 'sí'].indexOf(sat26) >= 0) {
          Shiny.setInputValue('entonet_public_route', {
            page: 'sat26',
            search: window.location.search || '',
            href: window.location.href || ''
          }, {priority: 'event'});
        }
      }
      document.addEventListener('DOMContentLoaded', function() {
        setTimeout(sendEntonetPublicRouteToShiny, 250);
      });
      document.addEventListener('shiny:connected', function() {
        sendEntonetPublicRouteToShiny();
        setTimeout(sendEntonetPublicRouteToShiny, 500);
      });
    ")),
    tags$script(HTML("
      document.addEventListener('input', function(event) {
        var target = event.target;
        if (!target || !target.id || !target.id.startsWith('f1_')) {
          return;
        }
        if (!target.matches('input[type=\"text\"], textarea')) {
          return;
        }
        var start = target.selectionStart;
        var end = target.selectionEnd;
        var upperValue = target.value.toUpperCase();
        if (target.value !== upperValue) {
          target.value = upperValue;
          if (typeof start === 'number' && typeof end === 'number') {
            target.setSelectionRange(start, end);
          }
          target.dispatchEvent(new Event('change', { bubbles: true }));
        }
      }, true);
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('renderGuatemalaCollectionMap', function(message) {
        setTimeout(function() {
          var mapElement = document.getElementById('guatemala_collection_map');
          if (!mapElement || typeof L === 'undefined') {
            return;
          }

          if (window.entonetGuatemalaMap) {
            window.entonetGuatemalaMap.remove();
          }

          var map = L.map('guatemala_collection_map', {
            scrollWheelZoom: true
          }).setView([15.55, -90.25], 7);
          window.entonetGuatemalaMap = map;

          L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
            maxZoom: 18,
            attribution: '&copy; OpenStreetMap contributors'
          }).addTo(map);

          fetch('guatemala_municipios.geojson')
            .then(function(response) { return response.json(); })
            .then(function(geojson) {
              var municipalityLayer = L.geoJSON(geojson, {
                style: function() {
                  return {
                    color: '#4d284a',
                    weight: 1,
                    opacity: 0.65,
                    fillColor: '#008c8f',
                    fillOpacity: 0.08
                  };
                },
                onEachFeature: function(feature, layer) {
                  var props = feature.properties || {};
                  var municipality = props.N_NIVEL3 || 'Municipio';
                  var department = props.DEPARTAMENTO || props.N_NIVEL2 || '';
                  layer.bindPopup('<strong>' + municipality + '</strong><br>' + department);
                }
              }).addTo(map);

              var siteLayer = L.layerGroup().addTo(map);
              (message.sites || []).forEach(function(site) {
                var marker = L.circleMarker([site.latitude, site.longitude], {
                  radius: 8,
                  color: '#082243',
                  weight: 2,
                  fillColor: '#d96c27',
                  fillOpacity: 0.95
                }).addTo(siteLayer);

                marker.bindPopup(
                  '<strong>' + site.site + '</strong><br>' +
                  'Municipio: ' + site.municipality + '<br>' +
                  'Departamento: ' + site.department + '<br>' +
                  'Registros: ' + site.records
                );
              });

              if ((message.sites || []).length > 0) {
                map.fitBounds(siteLayer.getBounds().pad(0.35));
              } else {
                map.fitBounds(municipalityLayer.getBounds());
              }
          });
        }, 100);
      });

      Shiny.addCustomMessageHandler('renderRegionalNetworkMap', function(message) {
        var renderMap = function(attempt) {
          var mapElement = document.getElementById('regional_network_map');
          if (!mapElement || typeof L === 'undefined') {
            if (attempt < 30) {
              setTimeout(function() {
                renderMap(attempt + 1);
              }, 100);
            }
            return;
          }

          if (window.entonetRegionalMap) {
            window.entonetRegionalMap.remove();
          }

          var map = L.map('regional_network_map', {
            minZoom: 4,
            maxZoom: 10,
            scrollWheelZoom: false
          }).setView([13.8, -83.2], 5);
          window.entonetRegionalMap = map;
          var english = message.language === 'en';

          L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
            maxZoom: 18,
            attribution: '&copy; OpenStreetMap contributors'
          }).addTo(map);

          (message.countries || []).forEach(function(country) {
            var marker = L.circleMarker([country.latitude, country.longitude], {
              radius: 15,
              color: '#082243',
              weight: 3,
              fillColor: '#008c8f',
              fillOpacity: 0.82
            }).addTo(map);

            marker.bindTooltip(country.display_name || country.country, {
              permanent: true,
              direction: 'top',
              className: 'network-country-label'
            });
            marker.bindPopup(
              '<strong>' + (country.display_name || country.country) + '</strong><br>' +
              (english ? 'Collection sites: ' : 'Sitios de colecta: ') + country.collection_sites + '<br>' +
              (english ? 'Records: ' : 'Registros: ') + country.records + '<br>' +
              '<em>' + (english ? 'Click to open dashboard' : 'Click para abrir dashboard') + '</em>'
            );
            marker.on('click', function() {
              Shiny.setInputValue('network_map_country', country.country, {priority: 'event'});
            });
          });

          (message.sites || []).forEach(function(site) {
            L.circleMarker([site.latitude, site.longitude], {
              radius: 5,
              color: '#9f4516',
              weight: 1,
              fillColor: '#d96c27',
              fillOpacity: 0.95
            }).addTo(map).bindPopup(
              '<strong>' + (english ? 'Collection site' : 'Sitio de colecta') + '</strong><br>' +
              site.site + '<br>' + site.country
            );
          });

          (message.collaborators || []).forEach(function(collaborator) {
            var icon = L.divIcon({
              className: 'network-collaborator-icon',
              html: '<span>' + collaborator.institution + '</span>',
              iconSize: [64, 30],
              iconAnchor: [32, 15]
            });

            L.marker([collaborator.latitude, collaborator.longitude], {
              icon: icon
            }).addTo(map).bindPopup(
              '<strong>' + collaborator.institution + '</strong><br>' +
              collaborator.city + ', ' + collaborator.country
            );
          });
          setTimeout(function() {
            map.invalidateSize();
          }, 150);
        };

        renderMap(0);
      });

      Shiny.addCustomMessageHandler('setF7TabAccess', function(message) {
        setTimeout(function() {
          document.querySelectorAll('#f7_capture_tab > li > a[data-value]').forEach(function(tab) {
            tab.dataset.f7Locked = 'false';
            tab.setAttribute('aria-disabled', 'false');
            tab.parentElement.classList.remove('f7-tab-locked');
          });
        }, 25);
      });

      Shiny.addCustomMessageHandler('setF7BottleLabels', function(message) {
        setTimeout(function() {
          var exploratory = message.mode === 'Exploratorio';
          var labels = exploratory
            ? ['1X', '2X', '5X', '10X', 'Control']
            : ['Botella experimental 1', 'Botella experimental 2', 'Botella experimental 3', 'Botella experimental 4', 'Botella control 1'];
          document.querySelectorAll('#f7_result_bottle > li').forEach(function(tab, index) {
            var link = tab.querySelector('a');
            if (link && labels[index]) link.textContent = labels[index];
          });
        }, 25);
      });

      document.addEventListener('click', function(event) {
        var button = event.target.closest('[data-sat26-audio]');
        if (!button) return;

        var action = button.getAttribute('data-sat26-audio');
        var targetId = button.getAttribute('data-sat26-target');
        var target = document.getElementById(targetId);
        if (!window.speechSynthesis || !target) return;

        if (action === 'stop') {
          window.speechSynthesis.cancel();
          return;
        }

        if (action === 'pause') {
          if (window.speechSynthesis.paused) {
            window.speechSynthesis.resume();
          } else if (window.speechSynthesis.speaking) {
            window.speechSynthesis.pause();
          }
          return;
        }

        if (action === 'play') {
          window.speechSynthesis.cancel();
          var text = target.innerText || target.textContent || '';
          var utterance = new SpeechSynthesisUtterance(text.replace(/\\s+/g, ' ').trim());
          utterance.lang = 'es-ES';
          utterance.rate = 0.92;
          window.speechSynthesis.speak(utterance);
        }
      });

      document.addEventListener('click', function(event) {
        var navButton = event.target.closest('button[id^=\"sat26_part_\"]');
        if (!navButton) return;
        var runScroll = function() {
          var anchor = document.getElementById('sat26_scroll_anchor');
          var workspace = document.querySelector('.portal-workspace');
          var panel = document.querySelector('.sat26-questionnaire-panel');
          if (workspace) workspace.scrollTop = 0;
          if (document.scrollingElement) document.scrollingElement.scrollTop = 0;
          document.documentElement.scrollTop = 0;
          document.body.scrollTop = 0;
          window.scrollTo(0, 0);
          (anchor || panel || workspace || document.body).scrollIntoView({ behavior: 'auto', block: 'start' });
        };
        window.setTimeout(runScroll, 60);
        window.setTimeout(runScroll, 350);
        window.setTimeout(runScroll, 900);
      });

      Shiny.addCustomMessageHandler('sat26SaveDraft', function(message) {
        try {
          if (!message || !message.code) return;
          window.localStorage.setItem('sat26-last-code', message.code);
          window.localStorage.setItem('sat26-draft-' + message.code, JSON.stringify(message));
        } catch (error) {}
      });

      Shiny.addCustomMessageHandler('sat26LoadDraft', function(message) {
        try {
          var code = (message && message.code ? message.code : '').trim();
          if (!code) return;
          var raw = window.localStorage.getItem('sat26-draft-' + code);
          if (!raw) {
            Shiny.setInputValue('sat26_resume_payload', { code: code, found: false }, { priority: 'event' });
            return;
          }
          var payload = JSON.parse(raw);
          payload.found = true;
          Shiny.setInputValue('sat26_resume_payload', payload, { priority: 'event' });
        } catch (error) {
          Shiny.setInputValue('sat26_resume_payload', { code: message && message.code ? message.code : '', found: false }, { priority: 'event' });
        }
      });

      Shiny.addCustomMessageHandler('sat26RestoreDraft', function(message) {
        try {
          if (!message) return;
          if (message.code) {
            window.localStorage.setItem('sat26-last-code', message.code);
            window.localStorage.setItem('sat26-draft-' + message.code, JSON.stringify(message));
          }
          message.found = true;
          Shiny.setInputValue('sat26_resume_payload', message, { priority: 'event' });
        } catch (error) {
          Shiny.setInputValue('sat26_resume_payload', { code: message && message.code ? message.code : '', found: false }, { priority: 'event' });
        }
      });

      Shiny.addCustomMessageHandler('sat26RequestNextCode', function() {
        try {
          var nextNumber = parseInt(window.localStorage.getItem('sat26-next-number') || '1', 10);
          if (!Number.isFinite(nextNumber) || nextNumber < 1) nextNumber = 1;
          var code = '26SAT' + String(nextNumber).padStart(2, '0');
          window.localStorage.setItem('sat26-next-number', String(nextNumber + 1));
          window.localStorage.setItem('sat26-last-code', code);
          Shiny.setInputValue('sat26_generated_code', code, { priority: 'event' });
        } catch (error) {}
      });

      Shiny.addCustomMessageHandler('sat26ScrollTop', function(message) {
        var delay = message && Number.isFinite(Number(message.delay)) ? Number(message.delay) : 80;
        var doScroll = function() {
          var anchor = document.getElementById('sat26_scroll_anchor');
          var workspace = document.querySelector('.portal-workspace');
          var panel = document.querySelector('.sat26-questionnaire-panel, .sat26-consent-panel');
          var target = anchor || panel || workspace;

          if (workspace) workspace.scrollTop = 0;
          if (document.scrollingElement) document.scrollingElement.scrollTop = 0;
          document.documentElement.scrollTop = 0;
          document.body.scrollTop = 0;
          window.scrollTo(0, 0);

          if (target && target.scrollIntoView) {
            target.scrollIntoView({ behavior: 'auto', block: 'start' });
          }
          if (target && target.focus) {
            target.setAttribute('tabindex', '-1');
            target.focus({ preventScroll: true });
          }
        };
        doScroll();
        window.setTimeout(doScroll, delay);
        window.setTimeout(doScroll, delay + 250);
        window.setTimeout(doScroll, delay + 700);
      });
    ")),
    tags$style(HTML("
      body { background: linear-gradient(90deg, #eef5f6 0%, #d3e8ed 48%, #4a9bb0 100%); color: #10223d; }
      .container-fluid { max-width: 100%; padding-left: 0; padding-right: 0; }
      .site-header {
        align-items: center;
        background: #ffffff;
        box-shadow: 0 2px 8px rgba(16, 34, 61, 0.10);
        display: flex;
        justify-content: space-between;
        padding: 12px 10%;
      }
      .header-logo {
        display: block;
        max-height: 92px;
        max-width: 620px;
        width: 100%;
        object-fit: contain;
      }
      .sat26-standalone-header {
        align-items: center;
        background: #ffffff;
        box-shadow: 0 2px 12px rgba(16, 34, 61, 0.10);
        display: flex;
        justify-content: center;
        padding: 18px 24px;
        position: relative;
      }
      .sat26-standalone-logo {
        display: block;
        max-height: 92px;
        max-width: 520px;
        object-fit: contain;
        width: min(100%, 520px);
      }
      .sat26-standalone-actions {
        position: absolute;
        right: 24px;
        top: 50%;
        transform: translateY(-50%);
      }
      .sat26-standalone-shell {
        background: linear-gradient(180deg, #eef5f6 0%, #ffffff 42%, #eef5f6 100%);
        min-height: calc(100vh - 128px);
        padding: 34px 20px 64px;
      }
      .sat26-standalone-workspace {
        margin: 0 auto;
        max-width: 1120px;
        width: 100%;
      }
      .landing-tabs {
        align-items: stretch;
        background: #4d284a;
        box-shadow: 0 3px 10px rgba(16, 34, 61, 0.14);
        display: flex;
        justify-content: stretch;
        padding: 0;
        width: 100%;
      }
      .landing-tab {
        background: transparent;
        border: 0;
        border-bottom: 4px solid transparent;
        border-right: 1px solid rgba(255, 255, 255, 0.16);
        border-radius: 0;
        color: #ffffff;
        flex: 1 1 20%;
        font-size: 15px;
        font-weight: 700;
        letter-spacing: 0.01em;
        min-height: 56px;
        padding: 17px 24px 14px 24px;
        text-align: center;
      }
      .landing-tab:last-child {
        border-right: 0;
      }
      .landing-tab:hover,
      .landing-tab:focus {
        background: rgba(255, 255, 255, 0.12);
        border-bottom-color: #d96c27;
        color: #ffffff;
        outline: none;
      }
      .landing-tab-active {
        background: rgba(255, 255, 255, 0.14);
        border-bottom-color: #d96c27;
      }
      .public-content {
        padding: 58px 10% 72px 10%;
      }
      .program-hero {
        background: linear-gradient(115deg, #082243 0%, #008c8f 72%, #4a9bb0 100%);
        border-radius: 12px;
        box-shadow: 0 12px 32px rgba(16, 34, 61, 0.18);
        color: #ffffff;
        margin-bottom: 28px;
        padding: 48px 54px;
      }
      .program-hero-copy {
        max-width: 920px;
      }
      .program-eyebrow {
        color: #ffffff;
        display: block;
        font-size: 13px;
        font-weight: 800;
        letter-spacing: 0.12em;
        margin-bottom: 12px;
        text-transform: uppercase;
      }
      .program-hero h1 {
        font-size: 48px;
        font-weight: 800;
        margin: 0 0 8px 0;
      }
      .program-hero h2 {
        color: #ffffff;
        font-size: 28px;
        font-weight: 700;
        line-height: 1.3;
        margin: 0 0 18px 0;
      }
      .program-hero p {
        font-size: 17px;
        line-height: 1.7;
        margin-bottom: 0;
        max-width: 820px;
      }
      .program-content-card {
        background: #ffffff;
        border-left: 6px solid #008c8f;
        border-radius: 10px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.11);
        padding: 38px 46px;
      }
      .program-content-card h3 {
        color: #082243;
        font-size: 25px;
        font-weight: 800;
        margin-top: 28px;
      }
      .program-content-card h3:first-child {
        margin-top: 0;
      }
      .program-content-card p {
        color: #46576a;
        font-size: 17px;
        line-height: 1.85;
        margin-bottom: 20px;
      }
      .strategic-pillars-card {
        background: #ffffff;
        border-radius: 10px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.11);
        margin-top: 28px;
        overflow: hidden;
        padding: 14px;
      }
      .strategic-pillars-image {
        border-radius: 6px;
        display: block;
        height: auto;
        width: 100%;
      }
      .program-participants-card {
        background: #ffffff;
        border-left: 6px solid #4d284a;
        border-radius: 10px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.11);
        margin-top: 28px;
        padding: 38px 46px;
      }
      .program-section-eyebrow {
        color: #008c8f;
        display: block;
        font-size: 13px;
        font-weight: 800;
        letter-spacing: 0.12em;
        margin-bottom: 8px;
        text-transform: uppercase;
      }
      .program-participants-card h2 {
        color: #082243;
        font-size: 30px;
        font-weight: 800;
        margin: 0 0 12px 0;
      }
      .participants-intro {
        color: #526070;
        font-size: 17px;
        line-height: 1.7;
        margin-bottom: 24px;
        max-width: 920px;
      }
      .participant-country-grid {
        display: grid;
        gap: 14px;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        margin-bottom: 32px;
      }
      .participant-country {
        align-items: center;
        background: #f7fbfc;
        border: 1px solid #d7e5e8;
        border-radius: 8px;
        color: #082243;
        display: flex;
        font-size: 15px;
        gap: 10px;
        min-height: 64px;
        padding: 14px 16px;
      }
      .participant-country-marker {
        background: #008c8f;
        border: 4px solid #d7eef0;
        border-radius: 50%;
        flex: 0 0 auto;
        height: 18px;
        width: 18px;
      }
      .implementers-component {
        background: linear-gradient(115deg, #f4f8f9 0%, #eef5f6 100%);
        border-top: 5px solid #008c8f;
        border-radius: 8px;
        padding: 28px 32px 18px 32px;
      }
      .implementers-component h3 {
        color: #4d284a;
        font-size: 24px;
        font-weight: 800;
        margin: 0 0 18px 0;
      }
      .implementers-component p {
        color: #46576a;
        font-size: 16px;
        line-height: 1.8;
        margin-bottom: 18px;
      }
      .steering-committee-component {
        background: #f7fbfc;
        border-top: 5px solid #4d284a;
        border-radius: 8px;
        margin-top: 28px;
        padding: 28px 32px 32px 32px;
      }
      .steering-committee-component h3 {
        color: #082243;
        font-size: 25px;
        font-weight: 800;
        margin: 0 0 10px 0;
      }
      .steering-committee-intro {
        color: #526070;
        font-size: 16px;
        line-height: 1.7;
        margin-bottom: 24px;
        max-width: 920px;
      }
      .steering-committee-grid {
        display: grid;
        gap: 16px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      .steering-member-card {
        background: #ffffff;
        border-left: 4px solid #008c8f;
        border-radius: 7px;
        box-shadow: 0 4px 14px rgba(16, 34, 61, 0.08);
        padding: 20px 22px;
      }
      .steering-member-group {
        color: #008c8f;
        display: block;
        font-size: 11px;
        font-weight: 800;
        letter-spacing: 0.1em;
        margin-bottom: 7px;
        text-transform: uppercase;
      }
      .steering-member-card h4 {
        color: #082243;
        font-size: 18px;
        font-weight: 800;
        line-height: 1.35;
        margin: 0 0 6px 0;
      }
      .steering-member-role {
        color: #657283;
        font-size: 13px;
        font-weight: 700;
        margin: 0 0 8px 0;
      }
      .steering-member-name {
        color: #4d284a;
        font-size: 15px;
        font-weight: 800;
        margin: 0;
      }
      .governance-hero {
        background: linear-gradient(115deg, #4d284a 0%, #713f69 55%, #008c8f 100%);
        border-radius: 12px;
        box-shadow: 0 12px 32px rgba(16, 34, 61, 0.18);
        color: #ffffff;
        margin-bottom: 28px;
        padding: 48px 54px;
      }
      .governance-hero h1 {
        font-size: 42px;
        font-weight: 800;
        margin: 0 0 14px 0;
      }
      .governance-hero p {
        font-size: 17px;
        line-height: 1.7;
        margin-bottom: 0;
        max-width: 940px;
      }
      .governance-steering-committee {
        margin-top: 0;
      }
      .collaborators-hero {
        background: linear-gradient(115deg, #4d284a 0%, #713f69 58%, #008c8f 100%);
        border-radius: 12px;
        box-shadow: 0 12px 32px rgba(16, 34, 61, 0.18);
        color: #ffffff;
        margin-bottom: 28px;
        padding: 48px 54px;
      }
      .collaborators-hero h1 {
        font-size: 42px;
        font-weight: 800;
        margin: 0 0 16px 0;
      }
      .collaborators-hero p {
        font-size: 17px;
        line-height: 1.75;
        margin-bottom: 0;
        max-width: 940px;
      }
      .collaborators-grid {
        display: grid;
        gap: 22px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      .collaborator-card {
        align-items: flex-start;
        background: #ffffff;
        border-top: 5px solid #008c8f;
        border-radius: 10px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.11);
        display: flex;
        gap: 22px;
        padding: 28px;
      }
      .collaborator-mark {
        align-items: center;
        background: linear-gradient(145deg, #082243 0%, #008c8f 100%);
        border-radius: 10px;
        color: #ffffff;
        display: flex;
        flex: 0 0 124px;
        font-size: 18px;
        font-weight: 800;
        justify-content: center;
        letter-spacing: 0.02em;
        min-height: 124px;
        padding: 14px;
        text-align: center;
      }
      .collaborator-copy h3 {
        color: #082243;
        font-size: 20px;
        font-weight: 800;
        line-height: 1.35;
        margin: 0 0 8px 0;
      }
      .collaborator-lead {
        color: #4d284a !important;
        font-weight: 800;
        margin-bottom: 10px !important;
      }
      .collaborator-copy p {
        color: #526070;
        font-size: 15px;
        line-height: 1.7;
        margin-bottom: 14px;
      }
      .collaborator-link {
        color: #008c8f;
        font-size: 14px;
        font-weight: 800;
        text-decoration: underline;
      }
      .collaborator-link:hover,
      .collaborator-link:focus {
        color: #4d284a;
      }
      .collaborators-closing {
        background: #ffffff;
        border-left: 6px solid #d96c27;
        border-radius: 8px;
        box-shadow: 0 6px 18px rgba(16, 34, 61, 0.09);
        margin-top: 28px;
        padding: 24px 30px;
      }
      .collaborators-closing p {
        color: #46576a;
        font-size: 17px;
        line-height: 1.8;
        margin: 0;
      }
      .network-map-hero {
        background: linear-gradient(115deg, #082243 0%, #176b7c 58%, #008c8f 100%);
        border-radius: 12px;
        box-shadow: 0 12px 32px rgba(16, 34, 61, 0.18);
        color: #ffffff;
        margin-bottom: 28px;
        padding: 42px 48px;
      }
      .network-map-hero h1 {
        font-size: 42px;
        font-weight: 800;
        margin: 0 0 14px 0;
      }
      .network-map-hero p {
        font-size: 17px;
        line-height: 1.7;
        margin-bottom: 0;
        max-width: 960px;
      }
      .network-map-layout {
        display: grid;
        gap: 24px;
        grid-template-columns: minmax(0, 2fr) minmax(310px, 1fr);
      }
      .network-country-selector {
        background: #ffffff;
        border-left: 5px solid #008c8f;
        border-radius: 8px;
        box-shadow: 0 6px 18px rgba(16, 34, 61, 0.09);
        margin-bottom: 24px;
        padding: 18px 22px 8px 22px;
      }
      .network-country-selector .form-group {
        margin-bottom: 10px;
        max-width: 520px;
      }
      .network-map-card,
      .network-dashboard-card {
        background: #ffffff;
        border-radius: 10px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.11);
        padding: 20px;
      }
      .network-map-legend {
        display: flex;
        flex-wrap: wrap;
        gap: 18px;
        margin-bottom: 14px;
      }
      .network-legend-item {
        align-items: center;
        color: #46576a;
        display: flex;
        font-size: 13px;
        font-weight: 700;
        gap: 7px;
      }
      .network-legend-dot {
        border-radius: 50%;
        display: inline-block;
        height: 14px;
        width: 14px;
      }
      .country-dot { background: #008c8f; border: 2px solid #082243; }
      .collection-dot { background: #d96c27; border: 1px solid #9f4516; }
      .collaborator-dot { background: #4d284a; border-radius: 3px; }
      .regional-network-static-map {
        background: #eef5f6;
        border: 1px solid #d8dde4;
        border-radius: 8px;
        display: block;
        height: auto;
        width: 100%;
      }
      .network-country-label {
        color: #082243;
        font-weight: 800;
      }
      .network-collaborator-icon {
        background: transparent;
      }
      .network-collaborator-icon span {
        align-items: center;
        background: #4d284a;
        border: 2px solid #ffffff;
        border-radius: 5px;
        box-shadow: 0 3px 8px rgba(16, 34, 61, 0.28);
        color: #ffffff;
        display: flex;
        font-size: 9px;
        font-weight: 800;
        height: 28px;
        justify-content: center;
        padding: 3px 5px;
        text-align: center;
        width: 62px;
      }
      .network-dashboard-card h2 {
        color: #082243;
        font-size: 25px;
        font-weight: 800;
        margin: 0 0 8px 0;
      }
      .network-dashboard-subtitle {
        color: #526070;
        font-size: 14px;
        line-height: 1.6;
        margin-bottom: 20px;
      }
      .network-stat-grid {
        display: grid;
        gap: 12px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        margin-bottom: 20px;
      }
      .network-stat {
        background: #f4f8f9;
        border-left: 4px solid #008c8f;
        border-radius: 6px;
        padding: 14px;
      }
      .network-stat-value {
        color: #082243;
        display: block;
        font-size: 24px;
        font-weight: 800;
      }
      .network-stat-label {
        color: #526070;
        display: block;
        font-size: 12px;
        font-weight: 700;
        margin-top: 3px;
      }
      .network-country-sites,
      .network-country-collaborators {
        background: #f7fbfc;
        border-radius: 7px;
        margin-top: 14px;
        padding: 16px;
      }
      .network-country-sites h3,
      .network-country-collaborators h3 {
        color: #4d284a;
        font-size: 16px;
        font-weight: 800;
        margin: 0 0 10px 0;
      }
      .network-country-sites ul,
      .network-country-collaborators ul {
        margin-bottom: 0;
        padding-left: 20px;
      }
      .impact-hero {
        background: linear-gradient(115deg, #4d284a 0%, #713f69 52%, #d96c27 100%);
        border-radius: 12px;
        box-shadow: 0 12px 32px rgba(16, 34, 61, 0.18);
        color: #ffffff;
        margin-bottom: 28px;
        padding: 44px 50px;
      }
      .impact-hero h1 {
        font-size: 42px;
        font-weight: 800;
        margin: 0 0 14px 0;
      }
      .impact-hero p {
        font-size: 17px;
        line-height: 1.75;
        margin-bottom: 0;
        max-width: 960px;
      }
      .impact-component-grid {
        display: grid;
        gap: 20px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
      }
      .impact-component-card {
        align-items: flex-start;
        background: #ffffff;
        border-top: 5px solid #008c8f;
        border-radius: 10px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.11);
        display: flex;
        gap: 20px;
        padding: 28px;
      }
      .impact-component-card:last-child {
        grid-column: 1 / -1;
      }
      .impact-component-number {
        align-items: center;
        background: #082243;
        border-radius: 50%;
        color: #ffffff;
        display: flex;
        flex: 0 0 52px;
        font-size: 16px;
        font-weight: 800;
        height: 52px;
        justify-content: center;
      }
      .impact-component-copy h3 {
        color: #082243;
        font-size: 21px;
        font-weight: 800;
        margin: 0 0 10px 0;
      }
      .impact-component-copy p {
        color: #526070;
        font-size: 15px;
        line-height: 1.75;
        margin-bottom: 0;
      }
      .impact-training-panel {
        align-items: center;
        background: #ffffff;
        border-left: 6px solid #d96c27;
        border-radius: 10px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.11);
        display: grid;
        gap: 24px;
        grid-template-columns: minmax(0, 1.4fr) minmax(280px, 1fr);
        margin-top: 28px;
        padding: 34px 38px;
      }
      .impact-training-copy h2,
      .impact-next-steps h2 {
        color: #082243;
        font-size: 26px;
        font-weight: 800;
        margin: 0 0 12px 0;
      }
      .impact-training-copy p,
      .impact-next-steps p {
        color: #526070;
        font-size: 16px;
        line-height: 1.75;
        margin-bottom: 0;
      }
      .impact-training-tags {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
      }
      .impact-training-tag {
        background: #008c8f;
        border-radius: 999px;
        color: #ffffff;
        font-size: 13px;
        font-weight: 800;
        padding: 9px 14px;
      }
      .impact-next-steps {
        background: #eef5f6;
        border-radius: 10px;
        margin-top: 28px;
        padding: 30px 36px;
      }
      .public-login-button {
        background: #4d284a;
        border: 1px solid #4d284a;
        border-radius: 4px;
        color: #ffffff;
        font-size: 14px;
        font-weight: 800;
        margin-left: 24px;
        min-width: 110px;
        padding: 10px 22px;
        white-space: nowrap;
      }
      .public-login-button:hover,
      .public-login-button:focus {
        background: #3d1f3a;
        border-color: #3d1f3a;
        color: #ffffff;
      }
      .public-header-actions {
        align-items: center;
        display: flex;
        gap: 14px;
        margin-left: 24px;
      }
      .language-switcher {
        align-items: center;
        background: #f4f8f9;
        border: 1px solid #d8e3e6;
        border-radius: 6px;
        display: flex;
        padding: 3px;
      }
      .language-button {
        background: transparent;
        border: 0;
        border-radius: 4px;
        color: #526070;
        font-size: 12px;
        font-weight: 800;
        padding: 7px 9px;
      }
      .language-button:hover,
      .language-button:focus,
      .language-active {
        background: #008c8f;
        color: #ffffff;
      }
      .header-actions {
        align-items: center;
        display: flex;
        gap: 12px;
        margin-left: 24px;
      }
      .header-user {
        color: #082243;
        font-size: 15px;
        white-space: nowrap;
      }
      .header-profile,
      .header-logout {
        border-color: #d8dde4;
        color: #4d284a;
        font-weight: 700;
        white-space: nowrap;
      }
      .header-profile {
        background: #008c8f;
        border-color: #008c8f;
        color: #ffffff;
      }
      .header-profile:hover,
      .header-profile:focus {
        background: #006f72;
        border-color: #006f72;
        color: #ffffff;
      }
      .landing-main {
        padding: 96px 10%;
      }
      .landing-row {
        display: flex;
        align-items: flex-start;
      }
      .portal-intro {
        background: white;
        border: 1px solid #16324f;
        border-radius: 3px;
        box-shadow: 0 10px 24px rgba(16, 34, 61, 0.10);
        margin: 0 0 34px 0;
        max-width: 680px;
        padding: 36px 42px;
      }
      .portal-intro h1 {
        color: #082243;
        font-size: 42px;
        font-weight: 700;
        margin-top: 0;
        margin-bottom: 8px;
      }
      .portal-intro h2 {
        color: #008c8f;
        font-size: 25px;
        font-weight: 700;
        line-height: 1.25;
        margin-top: 0;
      }
      .portal-intro p {
        color: #526070;
        font-size: 16px;
        line-height: 1.7;
      }
      .overview-panel {
        background: #ffffff;
        border-left: 6px solid #008c8f;
        border-radius: 8px;
        box-shadow: 0 6px 18px rgba(16, 34, 61, 0.08);
        margin: 0;
        max-width: 680px;
        padding: 22px 28px;
      }
      .login-card {
        background: white;
        border-radius: 3px;
        box-shadow: 0 10px 30px rgba(16, 34, 61, 0.14);
        margin: 0 0 0 auto;
        max-width: 420px;
        padding: 38px;
      }
      .login-card h3 {
        border-bottom: 1px solid #d8dde4;
        color: #4d284a;
        font-size: 28px;
        font-weight: 700;
        margin-top: 0;
        padding-bottom: 18px;
      }
      .login-subtitle {
        color: #5b6778;
        margin-bottom: 20px;
      }
      .login-button {
        background-color: #4d284a;
        border-color: #4d284a;
        font-weight: 700;
        width: 100%;
      }
      .login-button:hover,
      .login-button:focus {
        background-color: #3d1f3a;
        border-color: #3d1f3a;
      }
      .password-reset-link {
        background: transparent;
        border: 0;
        box-shadow: none;
        color: #4d284a;
        font-weight: 700;
        margin-top: 12px;
        padding: 0;
        text-decoration: underline;
      }
      .password-reset-link:hover,
      .password-reset-link:focus {
        background: transparent;
        color: #2f1730;
        text-decoration: underline;
      }
      .login-help {
        color: #5b6778;
        font-size: 13px;
        margin-top: 14px;
      }
      .capture-main {
        padding: 54px 10% 80px 10%;
      }
      .portal-shell {
        align-items: stretch;
        display: grid;
        grid-template-columns: 280px minmax(0, 1fr);
        min-height: calc(100vh - 150px);
      }
      .portal-sidebar {
        background: #f4f8f9;
        border-left: 8px solid #008c8f;
        border-right: 1px solid #d8e3e6;
        padding: 30px 20px 42px 18px;
      }
      .portal-sidebar-title {
        color: #082243;
        font-size: 13px;
        font-weight: 800;
        letter-spacing: 0.08em;
        margin: 0 10px 18px;
        text-transform: uppercase;
      }
      .sidebar-category {
        background: transparent;
        border: 0;
        border-radius: 7px;
        color: #16324f;
        display: flex;
        font-size: 16px;
        font-weight: 800;
        justify-content: space-between;
        margin-bottom: 5px;
        padding: 13px 14px;
        text-align: left;
        width: 100%;
      }
      .sidebar-category:hover,
      .sidebar-category:focus,
      .sidebar-category-active {
        background: #e1f1f1;
        color: #006f72;
      }
      .sidebar-chevron {
        font-size: 12px;
        margin-left: 12px;
      }
      .sidebar-submenu {
        border-left: 2px solid #9fcfd0;
        margin: 2px 0 14px 18px;
        padding: 2px 0 2px 10px;
      }
      .sidebar-subitem {
        background: transparent;
        border: 0;
        border-radius: 5px;
        color: #526070;
        display: block;
        font-size: 14px;
        font-weight: 700;
        margin: 2px 0;
        padding: 10px 12px;
        text-align: left;
        width: 100%;
      }
      .sidebar-subitem:hover,
      .sidebar-subitem:focus,
      .sidebar-subitem-active {
        background: #ffffff;
        box-shadow: 0 2px 8px rgba(16, 34, 61, 0.08);
        color: #4d284a;
      }
      .sidebar-form-list {
        border-left: 1px solid #d8e3e6;
        margin: 0 0 10px 14px;
        padding-left: 10px;
      }
      .sidebar-form-group-title {
        background: transparent;
        border: 0;
        border-radius: 5px;
        color: #293241;
        display: block;
        font-size: 14px;
        font-weight: 800;
        margin: 6px 0 4px;
        padding: 9px 10px;
        text-align: left;
        text-transform: uppercase;
        width: 100%;
      }
      .sidebar-form-group-title:hover,
      .sidebar-form-group-title:focus,
      .sidebar-form-group-title-active {
        background: #ffffff;
        color: #006f72;
      }
      .sidebar-form-item {
        background: transparent;
        border: 0;
        border-radius: 5px;
        color: #5f6b78;
        display: block;
        font-size: 14px;
        font-weight: 700;
        line-height: 1.35;
        margin: 1px 0;
        padding: 9px 10px;
        text-align: left;
        white-space: normal;
        width: 100%;
      }
      .sidebar-form-item:hover,
      .sidebar-form-item:focus,
      .sidebar-form-item-active {
        background: #ffffff;
        color: #006f72;
      }
      .capture-action-list {
        display: grid;
        gap: 12px;
        margin-bottom: 22px;
        max-width: 760px;
      }
      .capture-form-choice-list {
        display: grid;
        gap: 14px;
        max-width: 760px;
      }
      .capture-form-choice {
        background: #ffffff;
        border: 1px solid #d8e3e6;
        border-left: 6px solid #008c8f;
        border-radius: 10px;
        box-shadow: 0 6px 18px rgba(16, 34, 61, 0.08);
        color: #10223d;
        padding: 18px 20px;
        text-align: left;
        white-space: normal;
        width: 100%;
      }
      .capture-form-choice:hover,
      .capture-form-choice:focus {
        box-shadow: 0 10px 24px rgba(16, 34, 61, 0.14);
        color: #10223d;
        transform: translateY(-1px);
      }
      .capture-form-choice strong {
        color: #082243;
        display: block;
        font-size: 18px;
        font-weight: 800;
        line-height: 1.2;
        margin-bottom: 6px;
      }
      .capture-form-choice span {
        color: #526070;
        display: block;
        font-size: 14px;
        line-height: 1.45;
      }
      .capture-form-choice-secondary {
        border-left-color: #4d284a;
      }
      .capture-subdivision-list {
        display: grid;
        gap: 22px;
        grid-template-columns: repeat(3, minmax(285px, 1fr));
        justify-content: center;
        margin: 10px auto 0;
        max-width: 1240px;
        width: 100%;
      }
      .capture-subdivision-panel {
        background: #ffffff;
        border: 1px solid #d8e3e6;
        border-left-width: 6px;
        border-radius: 8px;
        box-shadow: 0 6px 18px rgba(16, 34, 61, 0.06);
        display: flex;
        flex-direction: column;
        gap: 12px;
        min-height: 330px;
        overflow: hidden;
        padding: 0;
        white-space: normal;
      }
      .capture-subdivision-panel-action {
        appearance: none;
        background: #ffffff;
        cursor: pointer;
        line-height: normal;
        text-align: left;
        white-space: normal;
        width: 100%;
      }
      .capture-subdivision-panel-action:focus {
        outline: none;
      }
      .capture-subdivision-panel-active {
        box-shadow: 0 10px 24px rgba(16, 34, 61, 0.12);
        transform: translateY(-1px);
      }
      .capture-subdivision-panel:nth-child(1) {
        border-left-color: #0d7a82;
      }
      .capture-subdivision-panel:nth-child(2) {
        border-left-color: #1769aa;
      }
      .capture-subdivision-panel:nth-child(3) {
        border-left-color: #6a9f2b;
      }
      .capture-subdivision-panel h4 {
        font-size: 17px;
        font-weight: 800;
        line-height: 1.2;
        margin: 0;
        white-space: normal;
      }
      .capture-subdivision-panel p {
        color: #5f6b78;
        font-size: 13.5px;
        line-height: 1.45;
        margin: 0;
        overflow-wrap: anywhere;
        white-space: normal;
      }
      .capture-subdivision-panel-image {
        aspect-ratio: 16 / 10;
        background: linear-gradient(180deg, #f8fbfd 0%, #eef6f8 100%);
        border-bottom: 1px solid #d8e3e6;
        overflow: hidden;
      }
      .capture-subdivision-panel-image img {
        display: block;
        height: 100%;
        object-fit: cover;
        width: 100%;
      }
      .capture-subdivision-panel-body {
        display: flex;
        flex: 1 1 auto;
        flex-direction: column;
        gap: 8px;
        justify-content: flex-start;
        min-width: 0;
        padding: 18px 20px 22px;
        white-space: normal;
      }
      .reactivos-intent-copy {
        margin: 0 auto 18px;
        max-width: 1120px;
        width: 100%;
      }
      .reactivos-intent-copy h4 {
        color: #082243;
        font-size: 21px;
        font-weight: 800;
        margin: 0 0 8px;
      }
      .reactivos-intent-copy p {
        color: #526070;
        font-size: 15px;
        line-height: 1.55;
        margin: 0;
        max-width: 980px;
      }
      .reactivos-detail-bubble {
        background: linear-gradient(180deg, #ffffff 0%, #f8fbfd 100%);
        border: 1px solid #d8e3e6;
        border-left: 6px solid #0d7a82;
        border-radius: 16px;
        box-shadow: 0 8px 20px rgba(16, 34, 61, 0.07);
        margin: 18px auto 0;
        max-width: 1120px;
        overflow: hidden;
        width: 100%;
      }
      .reactivos-detail-bubble-header {
        align-items: center;
        background: linear-gradient(90deg, rgba(13, 122, 130, 0.10), rgba(13, 122, 130, 0.03));
        display: flex;
        justify-content: space-between;
        gap: 14px;
        padding: 18px 20px;
      }
      .reactivos-detail-bubble-header h4 {
        color: #082243;
        font-size: 20px;
        margin: 0;
      }
      .reactivos-detail-bubble-header p {
        color: #5f6b78;
        margin: 6px 0 0;
      }
      .reactivos-detail-bubble-body {
        display: grid;
        gap: 16px;
        grid-template-columns: minmax(0, 1.1fr) minmax(0, 0.9fr);
        padding: 18px 20px 20px;
      }
      .reactivos-detail-definition {
        background: #ffffff;
        border: 1px solid #dbe7ee;
        border-radius: 14px;
        padding: 18px;
      }
      .reactivos-detail-definition strong {
        color: #1769aa;
        display: block;
        font-size: 13px;
        letter-spacing: 0.08em;
        margin-bottom: 8px;
        text-transform: uppercase;
      }
      .reactivos-detail-definition p {
        color: #304352;
        font-size: 15px;
        line-height: 1.55;
        margin: 0;
      }
      .reactivos-detail-products {
        display: grid;
        gap: 12px;
      }
      .reactivos-detail-product {
        align-items: center;
        background: #ffffff;
        border: 1px solid #dbe7ee;
        border-radius: 14px;
        color: #10223d;
        display: flex;
        gap: 14px;
        padding: 12px;
        text-align: left;
        white-space: normal;
        width: 100%;
      }
      .reactivos-detail-product:hover,
      .reactivos-detail-product:focus,
      .reactivos-detail-product-active {
        border-color: #0d7a82;
        box-shadow: 0 6px 16px rgba(16, 34, 61, 0.10);
        color: #10223d;
      }
      .reactivos-detail-product-image {
        border-radius: 12px;
        flex: 0 0 auto;
        height: 72px;
        overflow: hidden;
        width: 72px;
      }
      .reactivos-detail-product-image img {
        display: block;
        height: 100%;
        object-fit: cover;
        width: 100%;
      }
      .reactivos-detail-product-name {
        color: #082243;
        font-size: 15px;
        font-weight: 800;
      }
      .reactivos-detail-product-price {
        color: #123d6b;
        font-size: 14px;
        font-weight: 700;
      }
      .reactivos-detail-product-status {
        display: inline-flex;
        align-items: center;
        gap: 0.35rem;
        padding: 0.22rem 0.65rem;
        border-radius: 999px;
        background: #e7f6ef;
        color: #126b50;
        font-size: 0.78rem;
        font-weight: 700;
        margin-top: 0.3rem;
      }
      .reactivos-product-card {
        background: #ffffff;
        border: 1px solid #dbe7ee;
        border-radius: 14px;
        padding: 18px;
      }
      .reactivos-product-card h5 {
        color: #082243;
        font-size: 22px;
        font-weight: 800;
        margin: 0 0 8px;
      }
      .reactivos-product-card p {
        color: #526070;
        font-size: 15px;
        line-height: 1.5;
        margin: 0 0 14px;
      }
      .reactivos-product-spec-grid {
        display: grid;
        gap: 10px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        margin-bottom: 16px;
      }
      .reactivos-product-spec {
        background: #f8fbfd;
        border: 1px solid #dbe7ee;
        border-radius: 10px;
        padding: 10px 12px;
      }
      .reactivos-product-spec span {
        color: #5f6b78;
        display: block;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.04em;
        margin-bottom: 3px;
        text-transform: uppercase;
      }
      .reactivos-product-spec strong {
        color: #082243;
        display: block;
        font-size: 14px;
      }
      .reactivos-order-procedure {
        background: #ffffff;
        border: 1px solid #d8e3e6;
        border-left: 6px solid #1769aa;
        border-radius: 14px;
        box-shadow: 0 8px 20px rgba(16, 34, 61, 0.07);
        margin: 20px auto 0;
        max-width: 1120px;
        padding: 22px;
        width: 100%;
      }
      .reactivos-order-procedure h4 {
        color: #082243;
        font-size: 21px;
        font-weight: 800;
        margin: 0 0 10px;
      }
      .reactivos-order-procedure p {
        color: #526070;
        font-size: 15px;
        line-height: 1.55;
        margin: 0 0 14px;
      }
      .reactivos-order-steps {
        display: grid;
        gap: 12px;
        margin: 16px 0;
      }
      .reactivos-order-step {
        background: #f8fbfd;
        border: 1px solid #dbe7ee;
        border-radius: 10px;
        padding: 14px 16px;
      }
      .reactivos-order-step strong {
        color: #082243;
        display: block;
        font-size: 15px;
        margin-bottom: 4px;
      }
      .reactivos-form-placeholders {
        display: grid;
        gap: 14px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        margin-top: 16px;
      }
      .reactivos-form-placeholder {
        background: linear-gradient(180deg, #ffffff 0%, #f8fbfd 100%);
        border: 1px dashed #9fcfd0;
        border-radius: 12px;
        padding: 16px;
      }
      .reactivos-form-placeholder strong {
        color: #082243;
        display: block;
        font-size: 15px;
        margin-bottom: 6px;
      }
      .reactivos-form-placeholder span {
        color: #5f6b78;
        display: block;
        font-size: 13px;
        line-height: 1.45;
      }
      .request-flow-panel {
        background: #ffffff;
        border: 1px solid #d8e3e6;
        border-radius: 10px;
        box-shadow: 0 8px 22px rgba(16, 34, 61, 0.10);
        overflow: hidden;
        padding: 18px;
      }
      .request-flow-panel img {
        display: block;
        height: auto;
        width: 100%;
      }
      .capture-code-guide {
        border: 1px solid #d8e3e6;
        border-radius: 8px;
        margin-bottom: 20px;
        max-width: 920px;
        padding: 18px;
      }
      .capture-code-guide h4 {
        font-size: 18px;
        font-weight: 800;
        margin: 0 0 10px;
      }
      .capture-code-guide p,
      .capture-code-guide li {
        color: #5f6b78;
        font-size: 14px;
      }
      .capture-code-pattern {
        background: #f6f8f9;
        border: 1px solid #d8e3e6;
        border-radius: 6px;
        color: #293241;
        font-family: Menlo, Monaco, Consolas, monospace;
        font-size: 13px;
        margin: 8px 0;
        padding: 8px 10px;
        overflow-wrap: anywhere;
      }
      .capture-code-flow {
        align-items: stretch;
        display: grid;
        gap: 10px;
        grid-template-columns: 1fr auto 1fr auto 1fr;
        margin-top: 14px;
      }
      .capture-code-flow-step {
        background: #ffffff;
        border: 1px solid #d8e3e6;
        border-radius: 8px;
        padding: 12px;
      }
      .capture-code-flow-step strong {
        color: #293241;
        display: block;
        font-size: 14px;
        margin-bottom: 4px;
      }
      .capture-code-flow-step span {
        color: #5f6b78;
        display: block;
        font-size: 13px;
      }
      .capture-code-flow-arrow {
        align-self: center;
        color: #006f72;
        font-size: 22px;
        font-weight: 800;
      }
      .formulario-1-capture-layout {
        align-items: start;
        display: grid;
        gap: 22px;
        grid-template-columns: minmax(360px, 0.95fr) minmax(280px, 0.7fr);
        margin-bottom: 22px;
      }
      .formulario-1-capture-layout .capture-action-list {
        margin-bottom: 0;
        max-width: none;
      }
      .capture-action-row {
        align-items: center;
        background: #ffffff;
        border: 1px solid #d8e3e6;
        border-left: 5px solid #008c8f;
        border-radius: 7px;
        display: grid;
        gap: 14px;
        grid-template-columns: minmax(0, 1fr) auto;
        padding: 14px 16px;
      }
      .capture-action-row h4 {
        color: #082243;
        font-size: 19px;
        font-weight: 800;
        margin: 0 0 6px 0;
      }
      .capture-action-row p {
        color: #526070;
        font-size: 16px;
        line-height: 1.45;
        margin: 0;
      }
      .capture-action-row .btn {
        font-size: 17px;
      }
      .capture-module-title {
        font-size: 29px;
      }
      .capture-dataset-title {
        font-size: 27px;
      }
      .form-preview-panel {
        background: #ffffff;
        border: 1px solid #d8e3e6;
        border-radius: 8px;
        box-shadow: 0 6px 18px rgba(16, 34, 61, 0.08);
        padding: 14px;
      }
      .form-preview-panel img {
        border: 1px solid #b7b7b7;
        border-radius: 4px;
        display: block;
        height: auto;
        width: 100%;
      }
      .form-preview-panel h4 {
        color: #082243;
        font-size: 19px;
        font-weight: 800;
        margin: 12px 0 6px 0;
      }
      .form-preview-panel p {
        color: #526070;
        font-size: 16px;
        line-height: 1.45;
        margin: 0;
      }
      @media (max-width: 1100px) {
        .formulario-1-capture-layout {
          grid-template-columns: 1fr;
        }
      }
      .portal-workspace {
        min-width: 0;
        padding: 34px 5% 72px;
      }
      .portal-workspace > .module-panel,
      .portal-workspace > .capture-title-card {
        margin-top: 0;
      }
      .portal-empty-state {
        background: #ffffff;
        border: 1px dashed #9fcfd0;
        border-radius: 8px;
        color: #526070;
        font-size: 15px;
        padding: 22px 24px;
      }
      .capture-title-card {
        background: white;
        border-left: 6px solid #008c8f;
        border-radius: 8px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.10);
        margin-bottom: 24px;
        padding: 26px 32px;
      }
      .capture-title-card h2 {
        color: #008c8f;
        font-size: 34px;
        font-weight: 700;
        line-height: 1.25;
        margin-top: 0;
      }
      .capture-title-card h2 strong {
        color: #082243;
        font-weight: 800;
      }
      .capture-title-card p {
        color: #526070;
        font-size: 16px;
        line-height: 1.6;
        margin-bottom: 0;
      }
      .sat26-form-shell {
        background: #ffffff;
        border: 1px solid #d5dde1;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.08);
        margin: 0 auto 24px auto;
        max-width: 980px;
        padding: 34px 42px 30px 42px;
      }
      .sat26-form-header {
        text-align: center;
      }
      .sat26-form-logo-row {
        align-items: center;
        display: flex;
        flex-wrap: wrap;
        gap: 16px;
        justify-content: center;
        margin-bottom: 26px;
      }
      .sat26-form-logo {
        object-fit: contain;
      }
      .sat26-form-logo-uvg { height: 58px; }
      .sat26-form-logo-ces { height: 64px; }
      .sat26-form-logo-entonet { height: 60px; }
      .sat26-form-logo-comisca { height: 88px; }
      .sat26-form-header h2 {
        color: #082243;
        font-size: 28px;
        font-weight: 800;
        line-height: 1.25;
        margin: 0 auto 18px auto;
        max-width: 860px;
      }
      .sat26-form-welcome {
        color: #082243;
        font-size: 18px;
        margin-bottom: 18px;
      }
      .sat26-form-header p {
        color: #000000;
        font-size: 16px;
        line-height: 1.55;
        margin-left: auto;
        margin-right: auto;
        max-width: 860px;
      }
      .sat26-form-emphasis {
        color: #082243;
        font-weight: 700;
      }
      .sat26-form-subtitle {
        color: #000000;
        font-size: 14px;
        font-style: italic;
        margin: 18px auto 0 auto;
        max-width: 860px;
      }
      .sat26-form-info-panel {
        background: #ffffff;
        border: 1px solid #dbe6e8;
        border-radius: 12px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.06);
        margin: 0 auto 24px auto;
        max-width: 980px;
        padding: 24px 30px 28px 30px;
      }
      .sat26-form-info-panel h3 {
        color: #008c8f;
        font-size: 22px;
        font-weight: 700;
        margin-top: 0;
        text-align: center;
      }
      .sat26-form-info-panel > p {
        color: #000000;
        margin-bottom: 18px;
        text-align: center;
      }
      .sat26-resume-panel {
        background: #f8fbfc;
        border: 1px solid #dde8eb;
        border-radius: 10px;
        margin-top: 18px;
        padding: 16px 18px 18px 18px;
      }
      .sat26-resume-panel h4 {
        color: #000000;
        font-size: 16px;
        font-weight: 700;
        margin: 0 0 10px 0;
      }
      .sat26-resume-row {
        align-items: end;
        display: grid;
        gap: 12px;
        grid-template-columns: minmax(0, 1fr) auto;
      }
      .sat26-resume-status {
        color: #000000;
        font-size: 13pt;
        margin-top: 10px;
      }
      .sat26-form-section-grid {
        display: grid;
        gap: 14px;
        grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
        margin-top: 12px;
      }
      .sat26-form-section-card {
        background: #f8fbfc;
        border: 1px solid #dde8eb;
        border-radius: 10px;
        padding: 16px;
        text-align: center;
      }
      .sat26-form-section-card h4 {
        color: #000000;
        font-size: 16px;
        margin: 0 0 8px 0;
      }
      .sat26-form-section-card p {
        color: #000000;
        font-size: 14px;
        margin-bottom: 0;
      }
      .sat26-form-actions {
        display: flex;
        gap: 12px;
        justify-content: center;
        margin-top: 22px;
        flex-wrap: wrap;
      }
      .sat26-consent-panel {
        background: #ffffff;
        border: 1px solid #dbe6e8;
        border-radius: 12px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.06);
        margin: 0 auto 24px auto;
        max-width: 980px;
        padding: 24px 30px 28px 30px;
      }
      .sat26-consent-panel h3 {
        color: #082243;
        font-size: 22px;
        font-weight: 700;
        margin-top: 0;
        text-align: center;
      }
      .sat26-consent-panel p {
        color: #000000;
        font-size: 15px;
        line-height: 1.6;
      }
      .sat26-consent-modal {
        max-width: none;
      }
      .sat26-consent-section {
        background: #f8fbfc;
        border: 1px solid #dde8eb;
        border-radius: 10px;
        margin-bottom: 14px;
        padding: 16px 18px;
      }
      .sat26-consent-section h4 {
        color: #000000;
        font-size: 17px;
        margin: 0 0 10px 0;
      }
      .sat26-consent-section p,
      .sat26-consent-section li {
        color: #000000;
        font-size: 15px;
        line-height: 1.55;
      }
      .sat26-audio-controls {
        align-items: center;
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        justify-content: center;
        margin: 0 0 18px 0;
      }
      .sat26-audio-controls .btn {
        min-width: 110px;
      }
      .sat26-consent-check {
        background: #f8fbfc;
        border: 1px solid #dde8eb;
        border-radius: 10px;
        margin-top: 18px;
        padding: 16px 18px;
      }
      .sat26-consent-question {
        color: #000000;
        font-size: 16px;
        font-weight: 700;
        margin: 0 0 14px 0;
        text-align: center;
      }
      .sat26-consent-choice-row {
        display: grid;
        gap: 14px;
        grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
      }
      .sat26-consent-choice {
        border-radius: 999px;
        font-size: 16px;
        font-weight: 700;
        padding: 14px 22px;
        white-space: normal;
        width: 100%;
      }
      .sat26-consent-choice-yes {
        background: #008c8f;
        border-color: #00777a;
        color: #ffffff;
      }
      .sat26-consent-choice-no {
        background: #ffffff;
        border-color: #9fb6bc;
        color: #000000;
      }
      .sat26-questionnaire-panel {
        background: #ffffff;
        border: 1px solid #dbe6e8;
        border-radius: 12px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.06);
        margin: 0 auto 24px auto;
        max-width: 980px;
        padding: 28px 34px 32px 34px;
      }
      .sat26-section-logo-header {
        align-items: center;
        border-bottom: 1px solid #edf2f3;
        display: flex;
        justify-content: space-between;
        margin-bottom: 18px;
        padding-bottom: 14px;
      }
      .sat26-section-logo-entonet {
        max-height: 44px;
        max-width: 280px;
        object-fit: contain;
      }
      .sat26-section-logo-comisca {
        max-height: 48px;
        max-width: 300px;
        object-fit: contain;
      }
      .sat26-questionnaire-panel h3 {
        color: #082243;
        font-size: 24px;
        font-weight: 800;
        margin: 0 0 16px 0;
        text-align: center;
      }
      .sat26-questionnaire-panel h4 {
        border-top: 1px solid #e7eff1;
        color: #082243;
        font-size: 17px;
        font-weight: 800;
        margin: 26px 0 12px 0;
        padding-top: 18px;
      }
      .sat26-draft-code {
        color: #000000;
        font-size: 13pt;
        margin-bottom: 16px;
        text-align: center;
      }
      .sat26-questionnaire-intro {
        border-bottom: 1px solid #dde8eb;
        margin-bottom: 22px;
        padding-bottom: 18px;
        text-align: center;
      }
      .sat26-questionnaire-intro h4 {
        color: #000000;
        font-size: 18px;
        font-weight: 800;
        line-height: 1.35;
        margin: 0 0 14px 0;
      }
      .sat26-questionnaire-intro p {
        color: #000000;
        font-size: 15px;
        line-height: 1.6;
        margin-left: auto;
        margin-right: auto;
        max-width: 860px;
      }
      .sat26-left-intro {
        text-align: left;
      }
      .sat26-left-intro p {
        max-width: none;
      }
      .sat26-compact-intro {
        border-bottom: 0;
        margin-bottom: 10px;
        padding-bottom: 0;
      }
      .sat26-section-note {
        color: #000000;
        font-size: 13pt;
        line-height: 1.5;
        margin: 8px 0 14px 0;
      }
      .sat26-form-field {
        margin-bottom: 18px;
      }
      .sat26-form-field .form-group,
      .sat26-form-field .shiny-input-container,
      .sat26-form-field .shiny-input-radiogroup,
      .sat26-form-field .shiny-input-checkboxgroup {
        max-width: none;
        width: 100%;
      }
      .sat26-form-field label,
      .sat26-form-field .control-label {
        color: #000000;
        font-size: 13pt;
        font-weight: 400;
        line-height: 1.45;
        width: 100%;
      }
      .sat26-form-field input,
      .sat26-form-field select,
      .sat26-form-field textarea,
      .sat26-form-field .radio,
      .sat26-form-field .checkbox,
      .sat26-form-field .shiny-input-radiogroup label {
        font-size: 13pt;
      }
      .sat26-form-field .radio,
      .sat26-form-field .checkbox {
        margin-bottom: 8px;
        max-width: 100%;
        width: 100%;
      }
      .sat26-form-field .radio label,
      .sat26-form-field .checkbox label {
        display: flex;
        gap: 8px;
        line-height: 1.45;
        max-width: 100%;
        width: 100%;
      }
      .sat26-form-field .radio input[type='radio'],
      .sat26-form-field .checkbox input[type='checkbox'] {
        flex: 0 0 auto;
        margin-left: 0;
        margin-top: 5px;
        position: static;
      }
      .sat26-inline-reset {
        align-items: end;
        display: grid;
        gap: 12px;
        grid-template-columns: minmax(0, 1fr) auto;
      }
      .sat26-form-reset-row {
        display: flex;
        justify-content: flex-end;
        margin: -4px 0 18px 0;
      }
      .sat26-form-reset-row .btn {
        font-size: 12px;
        padding: 5px 12px;
        text-transform: lowercase;
      }
      .sat26-optional-note {
        color: #000000;
        font-size: 13px;
        font-weight: 700;
        letter-spacing: 0.04em;
        margin: -6px 0 10px 0;
        text-transform: uppercase;
      }
      .capture-bubble-note {
        background: #f2fbfb;
        border: 1px solid #c7ecec;
        border-radius: 12px;
        color: #23414a;
        margin-top: 18px;
        padding: 14px 16px;
      }
      .capture-module-card {
        background: white;
        border: 1px solid #d9e7ea;
        border-radius: 12px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.08);
        margin-bottom: 24px;
        padding: 24px 28px;
      }
      .capture-module-card h3 {
        color: #082243;
        font-size: 22px;
        font-weight: 700;
        margin-top: 0;
      }
      .capture-module-card p {
        color: #526070;
        font-size: 15px;
        line-height: 1.6;
      }
      .capture-bubble-section-grid {
        display: grid;
        gap: 14px;
        grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
        margin-top: 16px;
      }
      .capture-bubble-section {
        background: #f8fbfc;
        border: 1px solid #dde8eb;
        border-radius: 10px;
        padding: 16px;
      }
      .capture-bubble-section h4 {
        color: #008c8f;
        font-size: 17px;
        margin: 0 0 8px 0;
      }
      .capture-bubble-section p {
        font-size: 14px;
        margin-bottom: 0;
      }
      .capture-actions {
        text-align: right;
      }
      .capture-actions .btn {
        margin-top: 24px;
      }
      .portal-module-row {
        margin-bottom: 26px;
      }
      .nested-module-row {
        margin-top: 18px;
        margin-bottom: 0;
      }
      .module-card {
        background: #ffffff;
        border: 0;
        border-radius: 10px;
        box-shadow: 0 8px 24px rgba(16, 34, 61, 0.12);
        color: #10223d;
        display: block;
        min-height: 154px;
        padding: 28px 24px;
        text-align: left;
        white-space: normal;
        width: 100%;
      }
      .module-card:hover,
      .module-card:focus {
        box-shadow: 0 12px 28px rgba(16, 34, 61, 0.18);
        color: #10223d;
        transform: translateY(-2px);
      }
      .module-card::before {
        background: #008c8f;
        border-radius: 999px;
        content: '';
        display: block;
        height: 6px;
        margin-bottom: 18px;
        width: 58px;
      }
      .module-card-visualization::before {
        background: #4d284a;
      }
      .module-card-request::before {
        background: #082243;
      }
      .module-card-title {
        color: #082243;
        display: block;
        font-size: 24px;
        font-weight: 800;
        line-height: 1.15;
        margin-bottom: 12px;
      }
      .module-card-text {
        color: #526070;
        display: block;
        font-size: 15px;
        line-height: 1.5;
      }
      .module-panel {
        background: rgba(255, 255, 255, 0.94);
        border-radius: 10px;
        box-shadow: 0 8px 22px rgba(16, 34, 61, 0.10);
        margin-top: 10px;
        padding: 24px;
      }
      .module-panel h3 {
        color: #082243;
        font-weight: 800;
        margin-top: 0;
      }
      .reactivos-hero {
        align-items: stretch;
        display: grid;
        gap: 0;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        margin: -24px -24px 26px;
        overflow: hidden;
        border-radius: 10px 10px 0 0;
        background: #ffffff;
        border: 1px solid #d8e3e6;
        box-shadow: 0 10px 28px rgba(16, 34, 61, 0.10);
      }
      .reactivos-hero-image-wrap {
        min-height: 320px;
        background: linear-gradient(180deg, #ffffff, #f7fbfc);
      }
      .reactivos-hero-image {
        display: block;
        width: 100%;
        height: 100%;
        min-height: 320px;
        object-fit: cover;
      }
      .reactivos-hero-copy {
        display: flex;
        flex-direction: column;
        justify-content: center;
        gap: 14px;
        padding: 28px 30px;
        background: linear-gradient(180deg, #ffffff 0%, #fbfdff 100%);
        border-left: 1px solid #dde7ee;
      }
      .reactivos-hero-kicker {
        color: #1769aa;
        font-size: 12px;
        font-weight: 800;
        letter-spacing: 0.12em;
        text-transform: uppercase;
      }
      .reactivos-hero-copy h3 {
        color: #082243;
        font-size: 34px;
        line-height: 1.08;
        margin-bottom: 0;
      }
      .reactivos-hero-copy > p {
        color: #526070;
        font-size: 16px;
        line-height: 1.5;
        margin: 0;
        max-width: 360px;
      }
      .reactivos-hero-benefits {
        display: grid;
        gap: 12px 14px;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        margin-top: 8px;
      }
      .reactivos-hero-benefit {
        align-items: flex-start;
        display: flex;
        gap: 12px;
      }
      .reactivos-hero-benefit strong {
        color: #082243;
        font-size: 15px;
      }
      .reactivos-hero-benefit {
        color: #526070;
        font-size: 13px;
        line-height: 1.35;
      }
      .reactivos-hero-benefit-icon {
        align-items: center;
        background: #edf7fd;
        border-radius: 999px;
        color: #0d7a82;
        display: grid;
        flex: 0 0 auto;
        height: 32px;
        justify-items: center;
        width: 32px;
      }
      .reactivos-hero-notes {
        border-top: 1px solid #dde7ee;
        display: grid;
        gap: 14px;
        margin-top: 4px;
        padding-top: 14px;
      }
      .reactivos-hero-note strong {
        color: #082243;
        display: block;
        font-size: 15px;
        margin-bottom: 3px;
      }
      .reactivos-hero-note div {
        color: #526070;
        font-size: 13px;
        line-height: 1.4;
      }
      @media (max-width: 1100px) {
        .reactivos-hero {
          grid-template-columns: 1fr;
          margin: -24px -24px 26px;
        }
        .reactivos-hero-image-wrap,
        .reactivos-hero-image {
          min-height: 260px;
        }
        .capture-subdivision-list {
          grid-template-columns: repeat(2, minmax(285px, 1fr));
        }
      }
      @media (max-width: 720px) {
        .capture-subdivision-list {
          grid-template-columns: 1fr;
        }
        .reactivos-detail-bubble-body,
        .reactivos-product-spec-grid {
          grid-template-columns: 1fr;
        }
        .reactivos-form-placeholders {
          grid-template-columns: 1fr;
        }
      }
      .option-card,
      .training-item {
        background: #f7fbfc;
        border-left: 5px solid #008c8f;
        border-radius: 8px;
        box-shadow: 0 4px 14px rgba(16, 34, 61, 0.08);
        margin-bottom: 16px;
        min-height: 150px;
        padding: 20px 22px;
      }
      .option-card h4,
      .training-item h4 {
        color: #082243;
        font-weight: 800;
        margin-top: 0;
      }
      .option-card p,
      .training-item p {
        color: #526070;
        line-height: 1.6;
        margin-bottom: 0;
      }
      .training-list {
        margin-top: 18px;
      }
      .profile-grid {
        display: grid;
        gap: 12px;
        grid-template-columns: 1fr 1fr;
        margin-bottom: 18px;
      }
      .profile-grid .form-group {
        margin-bottom: 0;
        min-width: 0;
        width: 100%;
      }
      .profile-grid input {
        max-width: 100%;
        width: 100%;
      }
      .profile-comment-field,
      .profile-comment-field .form-group,
      #profile_comment {
        max-width: 100%;
        width: 100%;
      }
      .profile-comment-field textarea#profile_comment {
        display: block;
        box-sizing: border-box;
        resize: vertical;
      }
      .profile-field {
        background: #f7fbfc;
        border-left: 4px solid #008c8f;
        border-radius: 6px;
        padding: 12px 14px;
      }
      .profile-label {
        color: #526070;
        display: block;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.03em;
        text-transform: uppercase;
      }
      .profile-value {
        color: #082243;
        display: block;
        font-size: 16px;
        font-weight: 700;
        margin-top: 4px;
      }
      .bulk-upload-bar {
        align-items: center;
        background: #f7fbfc;
        border-left: 5px solid #008c8f;
        border-radius: 8px;
        box-shadow: 0 4px 14px rgba(16, 34, 61, 0.08);
        display: flex;
        gap: 18px;
        justify-content: space-between;
        margin-bottom: 18px;
        padding: 18px 20px;
      }
      .bulk-upload-copy {
        color: #526070;
        line-height: 1.5;
      }
      .bulk-upload-copy strong {
        color: #082243;
        display: block;
        font-size: 18px;
      }
      .capture-option-card {
        background: #f7fbfc;
        border-left: 5px solid #008c8f;
        border-radius: 8px;
        box-shadow: 0 4px 14px rgba(16, 34, 61, 0.08);
        min-height: 190px;
        padding: 24px;
      }
      .capture-option-card h4 {
        color: #082243;
        font-weight: 800;
        margin-top: 0;
      }
      .capture-option-card p {
        color: #526070;
        line-height: 1.6;
        min-height: 48px;
      }
      .protocol-download-item {
        align-items: center;
        background: #f7fbfc;
        border-left: 5px solid #008c8f;
        border-radius: 8px;
        box-shadow: 0 4px 14px rgba(16, 34, 61, 0.08);
        display: flex;
        gap: 18px;
        justify-content: space-between;
        margin-bottom: 14px;
        padding: 18px 20px;
      }
      .protocol-download-copy strong {
        color: #082243;
        display: block;
        font-size: 17px;
        margin-bottom: 6px;
      }
      .protocol-download-copy p {
        color: #526070;
        line-height: 1.5;
        margin-bottom: 6px;
      }
      .protocol-file-name {
        color: #4d284a;
        font-size: 13px;
        font-weight: 700;
      }
      .visualization-map {
        background: #eef5f6;
        border: 1px solid #d8dde4;
        border-radius: 10px;
        box-shadow: 0 8px 22px rgba(16, 34, 61, 0.10);
        height: 560px;
        margin-top: 18px;
        width: 100%;
      }
      .visualization-results-card {
        background: #ffffff;
        border-left: 5px solid #008c8f;
        border-radius: 8px;
        box-shadow: 0 4px 14px rgba(16, 34, 61, 0.08);
        margin-top: 18px;
        padding: 18px 20px;
      }
      .f7-viz-kpi-grid {
        display: grid;
        gap: 14px;
        grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
        margin-top: 18px;
      }
      .f7-viz-toolbar {
        align-items: center;
        display: flex;
        gap: 12px;
        justify-content: space-between;
        margin-top: 12px;
      }
      .f7-viz-refresh-copy {
        color: #526070;
        font-size: 13px;
        font-weight: 700;
      }
      .f7-viz-kpi-card {
        background: #ffffff;
        border-top: 4px solid #008c8f;
        border-radius: 8px;
        box-shadow: 0 4px 14px rgba(16, 34, 61, 0.08);
        padding: 16px 18px;
      }
      .f7-viz-kpi-label {
        color: #526070;
        font-size: 13px;
        font-weight: 700;
      }
      .f7-viz-kpi-value {
        color: #082243;
        display: block;
        font-size: 28px;
        font-weight: 800;
        margin-top: 4px;
      }
      .f7-viz-diagnostic-layout {
        align-items: start;
        display: grid;
        gap: 18px;
        grid-template-columns: minmax(0, 3fr) minmax(300px, 2fr);
      }
      .f7-viz-diagnostic-layout > div {
        min-width: 0;
      }
      .f7-viz-diagnostic-panel h4 {
        font-size: 17px;
        margin-bottom: 4px;
      }
      .f7-viz-diagnostic-panel p {
        font-size: 12px;
        margin-bottom: 4px;
      }
      .f7-viz-diagnostic-plot {
        margin-top: 14px;
      }
      .f7-viz-summary-table th:first-child,
      .f7-viz-summary-table td:first-child {
        white-space: nowrap;
      }
      .f7-viz-summary-table table {
        font-size: 11px;
      }
      @media (max-width: 900px) {
        .f7-viz-kpi-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .f7-viz-toolbar {
          align-items: flex-start;
          flex-direction: column;
        }
        .f7-viz-diagnostic-layout {
          grid-template-columns: 1fr;
        }
      }
      .modal-title-row {
        align-items: center;
        display: flex;
        justify-content: space-between;
        width: 100%;
      }
      .modal-close-button {
        background: transparent;
        border: 0;
        color: #4d284a;
        font-size: 24px;
        font-weight: 800;
        line-height: 1;
        padding: 0 4px;
      }
      .modal-close-button:hover,
      .modal-close-button:focus {
        background: transparent;
        color: #082243;
      }
      .required-label { font-weight: 600; }
      .summary-box { background: #f5f7fa; padding: 14px; border-radius: 6px; }
      .submit-row { margin-top: 16px; }
      .f7-navigation-row {
        align-items: center;
        border-top: 1px solid #d8dde4;
        display: flex;
        gap: 10px;
        justify-content: flex-end;
        margin-top: 20px;
        padding-top: 16px;
      }
      .f7-help-label-row {
        align-items: center;
        display: flex;
        gap: 7px;
        margin-bottom: 5px;
      }
      .f7-help-label-row label {
        margin-bottom: 0;
      }
      .f7-help-button {
        align-items: center;
        background: #008c8f;
        border: 0;
        border-radius: 50%;
        color: #ffffff;
        display: inline-flex;
        font-size: 12px;
        font-weight: 800;
        height: 20px;
        justify-content: center;
        line-height: 1;
        min-width: 20px;
        padding: 0;
        width: 20px;
      }
      .f7-help-button:hover,
      .f7-help-button:focus {
        background: #006f72;
        color: #ffffff;
      }
      .f7-help-field > .form-group {
        margin-bottom: 6px;
      }
      .f7-help-message {
        background: #eef8f8;
        border-left: 4px solid #008c8f;
        border-radius: 5px;
        color: #304757;
        line-height: 1.45;
        margin: 2px 0 15px;
        padding: 10px 12px;
      }
      .f7-unique-code {
        background: #eef8f8;
        border-left: 4px solid #008c8f;
        border-radius: 5px;
        color: #304757;
        margin-top: 8px;
        padding: 10px 12px;
      }
      .f7-unique-code code {
        color: #4d284a;
        display: block;
        font-size: 14px;
        margin-top: 4px;
        overflow-wrap: anywhere;
      }
      .f5-certification-backdrop {
        align-items: flex-start;
        background: rgba(8, 34, 67, 0.42);
        bottom: 0;
        display: flex;
        justify-content: center;
        left: 0;
        overflow-y: auto;
        padding: 48px 18px;
        position: fixed;
        right: 0;
        top: 0;
        z-index: 1060;
      }
      .f5-certification-dialog {
        background: #ffffff;
        border-radius: 8px;
        box-shadow: 0 18px 45px rgba(8, 34, 67, 0.28);
        max-width: 860px;
        padding: 22px 24px;
        width: min(860px, 100%);
      }
      .f5-certification-dialog-small {
        max-width: 620px;
      }
      .f5-certification-actions {
        display: flex;
        gap: 10px;
        justify-content: flex-end;
        margin-top: 18px;
      }
      .top-alerts { margin-top: 12px; margin-bottom: 18px; }
      .selector-box { background: #eef4fb; padding: 16px; border-radius: 6px; margin-bottom: 18px; }
      .btn-primary { background-color: #008c8f; border-color: #008c8f; }
      .btn-primary:hover,
      .btn-primary:focus { background-color: #006f72; border-color: #006f72; }
      .institutional-footer {
        align-items: center;
        background: #4d284a;
        color: white;
        display: flex;
        gap: 34px;
        justify-content: center;
        min-height: 132px;
        padding: 24px 10%;
      }
      .footer-title {
        font-size: 16px;
        font-weight: 700;
      }
      .footer-logos {
        align-items: center;
        display: flex;
        gap: 44px;
      }
      .footer-logo {
        background: white;
        border-radius: 4px;
        display: block;
        max-height: 72px;
        object-fit: contain;
        padding: 8px 12px;
      }
      .ces-logo { max-width: 300px; }
      .uvg-footer-logo { max-width: 300px; }
      @media (max-width: 900px) {
        .site-header { padding: 12px 24px; }
        .public-header-actions {
          align-items: flex-end;
          flex-direction: column;
          gap: 8px;
          margin-left: 12px;
        }
        .language-button {
          font-size: 11px;
          padding: 6px;
        }
        .landing-tabs {
          flex-wrap: wrap;
          padding: 0;
        }
        .landing-tab {
          flex: 1 1 33.333%;
          padding-left: 12px;
          padding-right: 12px;
        }
        .landing-tab:nth-child(-n + 3) {
          border-bottom-color: rgba(255, 255, 255, 0.16);
        }
        .public-content {
          padding: 32px 18px 48px 18px;
        }
        .program-hero {
          padding: 32px 26px;
        }
        .program-hero h1 {
          font-size: 38px;
        }
        .program-hero h2 {
          font-size: 23px;
        }
        .program-content-card {
          padding: 28px 24px;
        }
        .strategic-pillars-card {
          padding: 7px;
        }
        .program-participants-card {
          padding: 28px 24px;
        }
        .participant-country-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .implementers-component {
          padding: 24px 22px 12px 22px;
        }
        .steering-committee-component {
          padding: 24px 22px;
        }
        .steering-committee-grid {
          grid-template-columns: 1fr;
        }
        .governance-hero {
          padding: 32px 26px;
        }
        .governance-hero h1 {
          font-size: 34px;
        }
        .collaborators-hero {
          padding: 32px 26px;
        }
        .collaborators-hero h1 {
          font-size: 34px;
        }
        .collaborators-grid {
          grid-template-columns: 1fr;
        }
        .collaborator-card {
          flex-direction: column;
        }
        .collaborator-mark {
          flex-basis: auto;
          min-height: 82px;
          width: 100%;
        }
        .network-map-hero {
          padding: 32px 26px;
        }
        .network-map-hero h1 {
          font-size: 34px;
        }
        .network-map-layout {
          grid-template-columns: 1fr;
        }
        .impact-hero {
          padding: 32px 26px;
        }
        .impact-hero h1 {
          font-size: 34px;
        }
        .impact-component-grid {
          grid-template-columns: 1fr;
        }
        .impact-component-card:last-child {
          grid-column: auto;
        }
        .capture-code-flow {
          grid-template-columns: 1fr;
        }
        .capture-code-flow-arrow {
          text-align: center;
        }
        .impact-training-panel {
          grid-template-columns: 1fr;
          padding: 28px 24px;
        }
        .header-actions { margin-left: 16px; }
        .landing-main { padding: 36px 24px; }
        .capture-main { padding: 36px 24px 56px 24px; }
        .portal-shell { grid-template-columns: 1fr; }
        .portal-sidebar {
          border-bottom: 1px solid #d8e3e6;
          border-left-width: 6px;
          border-right: 0;
          padding: 22px 18px;
        }
        .portal-workspace { padding: 26px 24px 56px; }
        .capture-actions { text-align: left; }
        .capture-actions .btn { margin-top: 0; margin-bottom: 18px; }
        .module-card { margin-bottom: 18px; min-height: auto; }
        .landing-row { display: block; }
        .portal-intro,
        .overview-panel,
        .login-card {
          margin-left: 0;
          margin-right: 0;
          max-width: none;
        }
        .login-card { margin-top: 24px; }
        .institutional-footer,
        .footer-logos {
          flex-direction: column;
        }
        .profile-grid { grid-template-columns: 1fr; }
        .bulk-upload-bar {
          align-items: flex-start;
          flex-direction: column;
        }
      }
      @media (max-width: 520px) {
        .participant-country-grid {
          grid-template-columns: 1fr;
        }
      }
    "))
  ),
  uiOutput("app_page")
)

server <- function(input, output, session) {
  submission_status <- reactiveVal("No se ha enviado ningún registro en esta sesión.")
  active_area <- reactiveVal(NULL)
  active_module <- reactiveVal(NULL)
  active_capture_subdivision <- reactiveVal(NULL)
  active_request_subdivision <- reactiveVal(NULL)
  active_request_data_subdivision <- reactiveVal(NULL)
  active_dataset <- reactiveVal(NULL)
  sat26_unique_code <- reactiveVal("")
  sat26_next_code_number <- reactiveVal(1L)
  sat26_resume_status <- reactiveVal(NULL)
  active_request_reactivos_category <- reactiveVal("larvicidas")
  active_request_reactivos_product <- reactiveVal(1L)
  request_reactivos_catalog <- list(
    larvicidas = list(
      title_es = "Larvicidas",
      title_en = "Larvicides",
      subtitle_es = "Productos para intervenir criaderos y etapas inmaduras.",
      subtitle_en = "Products to target breeding sites and immature stages.",
      items = data.frame(
        name = c("Temefos 1 L", "Bti granulado 1 kg", "Larvex Pro 500"),
        price = c("$10.50 USD", "$14.50 USD", "$12.25 USD"),
        status = c("En stock", "En stock", "En stock"),
        concentration = c("1% SG", "Bacillus thuringiensis israelensis", "0.5% granulado"),
        expiration = c("2027-12", "2028-03", "2027-09"),
        technical_description = c(
          "Larvicida organofosforado de uso focal para criaderos controlados.",
          "Larvicida biológico granulado para criaderos y depósitos temporales.",
          "Formulación granulada de referencia para control de etapas inmaduras."
        ),
        image = c("reactivos-larvicidas.png", "reactivos-larvicidas.png", "reactivos-larvicidas.png"),
        stringsAsFactors = FALSE
      )
    ),
    adulticidas = list(
      title_es = "Adulticidas",
      title_en = "Adulticides",
      subtitle_es = "Control focalizado para insectos adultos.",
      subtitle_en = "Focused control for adult mosquitoes.",
      items = data.frame(
        name = c("AdultiMax 450", "PyroControl ULV", "CipraNeo 1 L"),
        price = c("$18.90 USD", "$22.40 USD", "$16.75 USD"),
        status = c("En stock", "En stock", "En stock"),
        concentration = c("450 g/L", "ULV 10%", "100 g/L"),
        expiration = c("2027-10", "2028-01", "2027-11"),
        technical_description = c(
          "Adulticida de aplicación focal para reducción rápida de población adulta.",
          "Concentrado para nebulización espacial en operaciones de respuesta.",
          "Formulación líquida para aplicaciones dirigidas contra mosquitos adultos."
        ),
        image = c("reactivos-adulticidas.png", "reactivos-adulticidas.png", "reactivos-adulticidas.png"),
        stringsAsFactors = FALSE
      )
    ),
    residuales = list(
      title_es = "Residuales",
      title_en = "Residuals",
      subtitle_es = "Formulaciones para aplicaciones dirigidas de efecto prolongado.",
      subtitle_en = "Formulations for targeted long-lasting applications.",
      items = data.frame(
        name = c("ResiShield 2 L", "LongGuard 1 kg", "MuroPlus 5 L"),
        price = c("$25.00 USD", "$27.50 USD", "$31.20 USD"),
        status = c("En stock", "En stock", "En stock"),
        concentration = c("250 g/L", "10% WP", "50 g/L"),
        expiration = c("2028-02", "2027-08", "2028-05"),
        technical_description = c(
          "Formulación residual para superficies internas y externas seleccionadas.",
          "Polvo humectable de efecto prolongado para superficies tratadas.",
          "Concentrado residual para aplicaciones controladas en paredes y refugios."
        ),
        image = c("reactivos-residuales.png", "reactivos-residuales.png", "reactivos-residuales.png"),
        stringsAsFactors = FALSE
      )
    )
  )
  f5_capture_steps <- c("metadatos", "datos_generales", "alimentacion", "conteo_huevecillos", "observaciones")
  f5_capture_step_labels <- c(
    metadatos = "Metadatos",
    datos_generales = "Datos generales",
    alimentacion = "Alimentación sanguínea",
    conteo_huevecillos = "Conteo de huevecillos",
    observaciones = "Observaciones y auditoría"
  )
  f5_capture_step <- reactiveVal("metadatos")
  f5_certification_complete <- reactiveVal(FALSE)
  f5_certification_panel <- reactiveVal("closed")
  f5_certification_alerts <- reactiveVal(character())
  f5_save_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  f5_review_records <- reactiveVal(data.frame())
  f5_review_selected <- reactiveVal(NULL)
  f5_review_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  f5_review_comparison <- reactiveVal(NULL)
  f5_review_delete_mode <- reactiveVal(FALSE)
  formulario_1_bulk_upload_result <- reactiveVal(NULL)
  f1_save_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  f1_resume_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  f1_placement_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  f1_confirmed_ovitrampa_count <- reactiveVal(NULL)
  f1_quadrant_config <- reactiveVal(NULL)
  f1_generated_quadrants <- reactiveVal(integer())
  f1_editable_quadrants <- reactiveVal(integer())
  f1_review_records <- reactiveVal(data.frame())
  f1_review_selected <- reactiveVal(NULL)
  f1_review_edit_mode <- reactiveVal(FALSE)
  f1_review_delete_mode <- reactiveVal(FALSE)
  f1_review_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  f7_save_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  formulario_7_bulk_upload_result <- reactiveVal(NULL)
  f7_review_records <- reactiveVal(data.frame())
  f7_review_selected <- reactiveVal(NULL)
  f7_review_edit_mode <- reactiveVal(FALSE)
  f7_review_delete_mode <- reactiveVal(FALSE)
  f7_review_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  f7_capture_steps <- c("informacion_general", "informacion_bioensayo", "material_responsables", "condiciones", "resultados", "comentarios_envio")
  f7_capture_step <- reactiveVal("informacion_general")
  f7_unlocked_step <- reactiveVal(length(f7_capture_steps))
  f7_navigation_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  logged_in <- reactiveVal(skip_login)
  public_page <- reactiveVal("login")
  public_language <- reactiveVal("es")
  login_error <- reactiveVal(NULL)
  password_setup_status <- reactiveVal(NULL)
  password_setup_token <- reactiveVal("")
  password_setup_refresh_token <- reactiveVal("")
  user_profile <- reactiveValues(
    username = "",
    email = "",
    user_id = "",
    access_token = "",
    refresh_token = "",
    name = value_or_default(profile_name, "Usuario autorizado"),
    institution = value_or_default(profile_institution, default_institution_id),
    position = value_or_default(profile_position, "Rol no configurado"),
    country = value_or_default(profile_country, "País no configurado")
  )

  apply_profile_to_capture_inputs <- function() {
    institution <- value_or_default(user_profile$institution, default_institution_id)
    name <- value_or_default(user_profile$name, "")

    updateTextInput(session, "f5_id_institucion", value = institution)
    updateTextInput(session, "f1_id_institucion", value = institution)
    updateTextInput(session, "f7_id_institucion", value = institution)
    updateTextInput(session, "f5_creado_por", value = name)
    updateTextInput(session, "f1_creado_por", value = name)
    updateTextInput(session, "f7_creado_por", value = name)
  }

  output$header_user_label <- renderUI({
    span(value_or_default(user_profile$name, value_or_default(user_profile$username, "Usuario")))
  })

  output$connect_user_greeting <- renderUI({
    req(logged_in())
    identifiers <- unique(tolower(trimws(c(
      value_or_default(session$user, ""),
      value_or_default(user_profile$username, ""),
      value_or_default(user_profile$email, ""),
      value_or_default(user_profile$name, "")
    ))))
    identifiers <- identifiers[nzchar(identifiers)]
    is_billy <- any(identifiers %in% c(
      "billy",
      "bhernandez",
      "bahernandez",
      "bahernandez@uvg.edu.gt"
    ))
    if (!is_billy) return(NULL)
    div(
      class = "alert alert-info",
      "Hola Billy espero que estes teniendo un lindo día."
    )
  })

  request_user_role <- function() {
    trimws(value_or_default(user_profile$position, ""))
  }

  request_is_local_review_session <- function() {
    host <- value_or_default(session$clientData$url_hostname, "")
    isTRUE(skip_login) || host %in% c("127.0.0.1", "localhost", "::1")
  }

  request_is_global_admin <- function() {
    if (request_is_local_review_session()) return(TRUE)
    role <- tolower(request_user_role())
    email <- tolower(value_or_default(user_profile$email, ""))
    username <- tolower(value_or_default(user_profile$username, ""))
    grepl("administrador", role) && (
      grepl("jgjuarez|jjuarez", email) ||
        grepl("jgjuarez|jjuarez", username) ||
        identical(email, "jjuarezvaldez@gmail.com")
    )
  }

  request_is_admin <- function() {
    grepl("administrador", tolower(request_user_role()))
  }

  request_is_supervisor <- function() {
    grepl("supervisor", tolower(request_user_role()))
  }

  request_is_laboratory_supervisor <- function() {
    email <- tolower(value_or_default(user_profile$email, ""))
    username <- tolower(value_or_default(user_profile$username, ""))
    request_is_supervisor() && (
      identical(email, "sambrocio@uvg.edu.gt") ||
        identical(username, "sambrocio@uvg.edu.gt")
    )
  }

  request_allowed_data_subdivisions <- reactive({
    if (request_is_global_admin()) return(c("campo", "insectario", "laboratorio"))
    if (request_is_admin()) return(c("campo", "insectario", "laboratorio"))
    if (request_is_laboratory_supervisor()) return(c("campo", "insectario", "laboratorio"))
    if (request_is_supervisor()) return(c("campo", "insectario"))
    character()
  })

  request_filter_country <- function() {
    if (request_is_global_admin()) {
      selected <- value_or_default(input$request_download_country, "all")
      if (identical(selected, "all")) return(NULL)
      return(selected)
    }
    value_or_default(user_profile$country, "País no configurado")
  }

  request_filter_institution <- function() {
    if (request_is_global_admin()) {
      selected <- value_or_default(input$request_download_institution, "all")
      if (identical(selected, "all")) return(NULL)
      return(selected)
    }
    value_or_default(user_profile$institution, default_institution_id)
  }

  request_data_dataset_choices <- function(subdivision) {
    switch(
      subdivision,
      campo = c("Formulario 1: Colocación y retiro de ovitrampa" = "formulario_1"),
      insectario = c(
        "Formulario 5: Alimentación y conteo" = "formulario_5",
        "Formulario 7: Bioensayo de botella CDC" = "formulario_7"
      ),
      laboratorio = character(),
      character()
    )
  }

  request_append_scope_filters <- function(where_clauses, params, table_alias = "") {
    prefix <- if (nzchar(table_alias)) paste0(table_alias, ".") else ""
    country <- request_filter_country()
    institution <- request_filter_institution()
    if (!is.null(country) && nzchar(country)) {
      where_clauses <- c(where_clauses, paste0(prefix, "pais = $", length(params) + 1L))
      params <- c(params, list(country))
    }
    if (!is.null(institution) && nzchar(institution)) {
      where_clauses <- c(where_clauses, paste0(prefix, "id_institucion = $", length(params) + 1L))
      params <- c(params, list(institution))
    }
    list(where = where_clauses, params = params)
  }

  request_where_sql <- function(where_clauses) {
    if (!length(where_clauses)) return("")
    paste("where", paste(where_clauses, collapse = " and "))
  }

  request_fetch_formulario_1 <- function(connection) {
    filters <- request_append_scope_filters(character(), list(), "h")
    query <- paste(
      "select h.formulario_codigo, h.formulario_nombre, h.fecha_registro, h.pais, h.id_institucion, h.departamento, h.municipio,",
      "h.ciclo, h.ronda, h.codigo_formulario, h.fecha_colocacion, h.grupo_responsable_colocacion,",
      "h.cuadrante, h.codigo_casa, h.latitud as \"Latitud\", h.longitud as \"Longitud\", h.codigo_gps,",
      "h.ovitrampas_colocadas as \"Ovitrampas_colocadas\", d.codigo_sustrato, h.fecha_retiro,",
      "h.grupo_responsable_retiro, h.ovitrampas_retiradas as \"Ovitrampas_retiradas\",",
      "h.retiro_buen_estado, h.retiro_sin_agua, h.retiro_sin_sustrato, h.retiro_sin_ovitrampa,",
      "h.retiro_movida, h.retiro_volteada, h.retiro_casa_cerrada, h.retiro_casa_cerrada_descripcion,",
      "h.fuente_formulario, h.creado_por, h.creado_en, h.actualizado_en",
      "from public.formulario_1_ovitrampa_intake h",
      "left join public.formulario_1_ovitrampa_detalle_intake d on d.intake_id = h.intake_id",
      request_where_sql(filters$where),
      "order by h.fecha_registro desc, h.codigo_formulario, h.cuadrante, h.codigo_casa, d.codigo_sustrato"
    )
    rows <- dbGetQuery(connection, query, params = filters$params)
    rows[intersect(formulario_1_intake_columns, names(rows))]
  }

  request_fetch_formulario_5 <- function(connection) {
    filters <- request_append_scope_filters(character(), list())
    query <- paste(
      "select *",
      "from public.formulario_5_alimentacion_conteo_intake",
      request_where_sql(filters$where),
      "order by fecha_registro desc, intake_id desc"
    )
    dbGetQuery(connection, query, params = filters$params)
  }

  request_fetch_formulario_7 <- function(connection) {
    filters <- request_append_scope_filters(character(), list(), "h")
    query <- paste(
      "select h.*",
      "from public.formulario_7_bioensayo_intake h",
      request_where_sql(filters$where),
      "order by h.fecha_registro desc, h.intake_id desc"
    )
    header <- dbGetQuery(connection, query, params = filters$params)
    if (!nrow(header)) return(formulario_7_internal_to_csv(formulario_7_template[0, , drop = FALSE]))

    for (column in setdiff(formulario_7_intake_columns, names(header))) header[[column]] <- NA
    ids <- as.integer(header$intake_id)
    placeholders <- paste0("$", seq_along(ids), collapse = ", ")
    results <- dbGetQuery(
      connection,
      paste(
        "select intake_id, fase, botella, tiempo_minutos, hora_lectura, vivos, incapacitados",
        "from public.formulario_7_bioensayo_resultado_intake",
        "where intake_id in (", placeholders, ")"
      ),
      params = as.list(ids)
    )
    comments <- dbGetQuery(
      connection,
      paste(
        "select intake_id, comentario, nombre as comentario_nombre",
        "from public.formulario_7_bioensayo_comentario_intake",
        "where intake_id in (", placeholders, ")"
      ),
      params = as.list(ids)
    )
    row_index <- setNames(seq_len(nrow(header)), as.character(header$intake_id))
    if (nrow(results)) {
      for (index in seq_len(nrow(results))) {
        row <- results[index, ]
        target <- row_index[[as.character(row$intake_id)]]
        bottle <- as.character(row$botella)
        minutes <- as.integer(row$tiempo_minutos)
        if (is.na(target) || !nzchar(bottle) || is.na(minutes)) next
        if (minutes == 1440L) {
          header[[paste0("resultado_hora_lectura_24h_", bottle)]][[target]] <- as.character(row$hora_lectura)
          header[[paste0("resultado_24h_", bottle, "_vivos")]][[target]] <- row$vivos
          header[[paste0("resultado_24h_", bottle, "_incapacitados")]][[target]] <- row$incapacitados
        } else {
          if (minutes == 0L) header[[paste0("resultado_hora_inicio_", bottle)]][[target]] <- as.character(row$hora_lectura)
          header[[paste0("resultado_", minutes, "min_", bottle, "_vivos")]][[target]] <- row$vivos
          header[[paste0("resultado_", minutes, "min_", bottle, "_incapacitados")]][[target]] <- row$incapacitados
        }
      }
    }
    if (nrow(comments)) {
      for (index in seq_len(nrow(comments))) {
        target <- row_index[[as.character(comments$intake_id[[index]])]]
        if (is.na(target)) next
        header$comentario[[target]] <- comments$comentario[[index]]
        header$comentario_nombre[[target]] <- comments$comentario_nombre[[index]]
      }
    }
    formulario_7_internal_to_csv(header)
  }

  show_password_setup_modal <- function() {
    showModal(modalDialog(
      title = "Crear o restablecer contraseña",
      size = "m",
      easyClose = FALSE,
      div(
        class = "alert alert-info",
        "Enlace verificado. Ingrese una nueva contraseña para su cuenta EntoNet."
      ),
      passwordInput("setup_password", "Nueva contraseña"),
      passwordInput("setup_password_confirm", "Confirmar nueva contraseña"),
      uiOutput("password_setup_modal_status"),
      footer = tagList(
        actionButton("setup_return_to_login", "Cancelar", class = "btn-default"),
        actionButton("setup_password_save", "Guardar contraseña", class = "btn-primary")
      )
    ))
  }

  output$password_setup_modal_status <- renderUI({
    password_setup_status()
  })

  observe({
    req(logged_in())
    active_area()
    active_module()
    active_dataset()
    apply_profile_to_capture_inputs()
  })

  output$app_page <- renderUI({
    if (identical(public_page(), "sat26")) {
      return(sat26_public_page(public_language()))
    }

    if (!logged_in()) {
      if (identical(public_page(), "password_setup")) {
        return(password_setup_page(password_setup_status(), public_language()))
      }
      if (identical(public_page(), "program")) {
        return(program_page(public_language()))
      }
      if (identical(public_page(), "governance")) {
        return(governance_page(public_language()))
      }
      if (identical(public_page(), "collaborators")) {
        return(collaborators_page(public_language()))
      }
      if (identical(public_page(), "network_map")) {
        return(network_map_page(public_language()))
      }
      if (identical(public_page(), "network_impact")) {
        return(network_impact_page(public_language()))
      }
      message <- login_error()
      if (is.null(message) && (!nzchar(supabase_auth_api_key) || !nzchar(storage_project_url()))) {
        message <- div(
          class = "alert alert-warning",
          "Supabase Auth no está configurado. Agregue SUPABASE_URL y SUPABASE_ANON_KEY o SUPABASE_SERVICE_ROLE_KEY a las variables secretas del servidor."
        )
      }
      return(landing_page(message, public_language()))
    }

    if (is_sat26_survey_session(user_profile$username)) {
      return(sat26_authenticated_page())
    }

    authenticated_page()
  })

  observeEvent(input$landing_program, {
    public_page("program")
  })

  observeEvent(input$landing_governance, {
    public_page("governance")
  })

  observeEvent(input$set_language_es, {
    public_language("es")
  })

  observeEvent(input$set_language_en, {
    public_language("en")
  })

  observeEvent(input$landing_collaborators, {
    public_page("collaborators")
  })

  selected_network_country <- reactiveVal("Guatemala")

  observeEvent(input$landing_network_map, {
    public_page("network_map")
  })

  observeEvent(input$landing_network_impact, {
    public_page("network_impact")
  })

  observeEvent(input$landing_sat26, {
    public_page("sat26")
    active_area("sat26")
    active_module("intro")
    sat26_resume_status(NULL)
  })

  observeEvent(input$entonet_public_route, {
    route <- input$entonet_public_route
    if (identical(value_or_default(route$page, ""), "sat26")) {
      public_page("sat26")
      active_area("sat26")
      if (is.null(active_module()) || !nzchar(value_or_default(active_module(), ""))) {
        active_module("intro")
      }
      sat26_resume_status(NULL)
      session$sendCustomMessage("sat26ScrollTop", list(delay = 120))
    }
  }, ignoreInit = FALSE)

  observe({
    search <- value_or_default(session$clientData$url_search, "")
    if (!nzchar(search)) return()
    query <- parseQueryString(search)
    requested_sat26 <- identical(tolower(value_or_default(query$survey, "")), "sat26") ||
      value_or_default(query$sat26, "") %in% c("1", "true", "yes", "si", "sí")
    if (!isTRUE(requested_sat26)) return()
    public_page("sat26")
    active_area("sat26")
    if (is.null(active_module()) || !nzchar(value_or_default(active_module(), ""))) {
      active_module("intro")
    }
  })

  observeEvent(input$network_country_selector, {
    req(input$network_country_selector %in% country_choices)
    selected_network_country(input$network_country_selector)
  })

  output$regional_network_leaflet <- renderLeaflet({
    country_labels <- sprintf(
      "<strong>%s</strong><br>%s: %s<br>%s: %s",
      vapply(network_country_summary$country, display_country, character(1), language = public_language()),
      tr(public_language(), "Sitios de colecta", "Collection sites"),
      network_country_summary$collection_sites,
      tr(public_language(), "Registros", "Records"),
      format(network_country_summary$records, big.mark = ",")
    )

    site_labels <- sprintf(
      "<strong>%s</strong><br>%s<br>%s",
      tr(public_language(), "Sitio de colecta", "Collection site"),
      network_collection_sites$site,
      vapply(network_collection_sites$country, display_country, character(1), language = public_language())
    )

    collaborator_labels <- sprintf(
      "<strong>%s</strong><br>%s, %s",
      network_collaborators$institution,
      network_collaborators$city,
      vapply(network_collaborators$country, display_country, character(1), language = public_language())
    )

    leaflet() |>
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(noWrap = TRUE)
      ) |>
      setView(lng = -83.5, lat = 14.4, zoom = 5) |>
      addCircleMarkers(
        data = network_collection_sites,
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        stroke = TRUE,
        color = "#9f4516",
        weight = 1,
        fillColor = "#d96c27",
        fillOpacity = 0.95,
        popup = site_labels,
        group = tr(public_language(), "Sitios de colecta", "Collection sites")
      ) |>
      addCircleMarkers(
        data = network_country_summary,
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~country,
        radius = 15,
        stroke = TRUE,
        color = "#082243",
        weight = 3,
        fillColor = "#008c8f",
        fillOpacity = 0.82,
        popup = country_labels,
        label = vapply(network_country_summary$country, display_country, character(1), language = public_language()),
        labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = FALSE),
        group = tr(public_language(), "Países participantes", "Participating countries")
      ) |>
      addCircleMarkers(
        data = network_collaborators,
        lng = ~longitude,
        lat = ~latitude,
        radius = 9,
        stroke = TRUE,
        color = "#ffffff",
        weight = 2,
        fillColor = "#4d284a",
        fillOpacity = 1,
        label = ~institution,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textOnly = FALSE,
          style = list(
            "background-color" = "#4d284a",
            "border-color" = "#ffffff",
            "color" = "#ffffff",
            "font-weight" = "700"
          )
        ),
        popup = collaborator_labels,
        group = tr(public_language(), "Colaboradores estratégicos", "Strategic collaborators")
      ) |>
      addLayersControl(
        overlayGroups = c(
          tr(public_language(), "Países participantes", "Participating countries"),
          tr(public_language(), "Sitios de colecta", "Collection sites"),
          tr(public_language(), "Colaboradores estratégicos", "Strategic collaborators")
        ),
        options = layersControlOptions(collapsed = TRUE)
      )
  })

  observeEvent(input$regional_network_leaflet_marker_click, {
    clicked_country <- input$regional_network_leaflet_marker_click$id
    if (!is.null(clicked_country) && clicked_country %in% country_choices) {
      selected_network_country(clicked_country)
      updateSelectInput(session, "network_country_selector", selected = clicked_country)
    }
  })

  output$network_country_dashboard <- renderUI({
    country <- selected_network_country()
    summary <- network_country_summary[network_country_summary$country == country, ]
    sites <- network_collection_sites[network_collection_sites$country == country, ]
    collaborators <- network_collaborators[network_collaborators$country == country, ]

    collaborator_items <- if (nrow(collaborators) > 0) {
      lapply(seq_len(nrow(collaborators)), function(index) {
        tags$li(paste(collaborators$institution[index], "-", collaborators$city[index]))
      })
    } else {
      list(tags$li("Sin colaborador estratégico registrado en este mock."))
    }

    tagList(
      h2(display_country(country, public_language())),
      p(class = "network-dashboard-subtitle", tr(public_language(), "Dashboard general de ejemplo. Los indicadores serán reemplazados por consultas a Supabase cuando se defina la estructura final.", "General mock dashboard. These indicators will be replaced by Supabase queries once the final data structure is defined.")),
      div(
        class = "network-stat-grid",
        div(class = "network-stat", span(class = "network-stat-value", summary$collection_sites), span(class = "network-stat-label", tr(public_language(), "Sitios de colecta", "Collection sites"))),
        div(class = "network-stat", span(class = "network-stat-value", summary$municipalities), span(class = "network-stat-label", tr(public_language(), "Municipios", "Municipalities"))),
        div(class = "network-stat", span(class = "network-stat-value", format(summary$records, big.mark = ",")), span(class = "network-stat-label", tr(public_language(), "Registros", "Records"))),
        div(class = "network-stat", span(class = "network-stat-value", summary$active_projects), span(class = "network-stat-label", tr(public_language(), "Proyectos activos", "Active projects")))
      ),
      div(
        class = "network-country-sites",
        h3(tr(public_language(), "Sitios destacados", "Highlighted sites")),
        tags$ul(lapply(sites$site, tags$li))
      ),
      div(
        class = "network-country-collaborators",
        h3(tr(public_language(), "Colaboradores localizados", "Located collaborators")),
        tags$ul(collaborator_items)
      )
    )
  })

  observeEvent(input$return_to_login, {
    public_page("login")
  })

  observeEvent(input$setup_return_to_login, {
    removeModal()
    password_setup_status(NULL)
    public_page("login")
  })

  observeEvent(input$show_password_reset, {
    showModal(modalDialog(
      title = "Restablecer contraseña",
      size = "m",
      easyClose = TRUE,
      textInput(
        "password_reset_email",
        "Correo registrado",
        value = value_or_default(input$login_user, "")
      ),
      div(
        class = "alert alert-info",
        "Recibirá un enlace para definir una nueva contraseña. El enlace regresará a EntoNet en Connect Cloud."
      ),
      uiOutput("password_reset_status"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("send_password_reset", "Enviar enlace", class = "btn-primary")
      )
    ))
  })

  password_reset_status <- reactiveVal(NULL)

  output$password_reset_status <- renderUI({
    password_reset_status()
  })

  observeEvent(input$send_password_reset, {
    email <- trimws(value_or_default(input$password_reset_email, ""))
    password_reset_status(NULL)

    if (!nzchar(email) || !grepl("@", email, fixed = TRUE)) {
      password_reset_status(div(class = "alert alert-warning", "Ingrese un correo válido."))
      return()
    }

    result <- tryCatch({
      supabase_auth_send_password_recovery(email)
    }, error = function(error) {
      password_reset_status(div(class = "alert alert-danger", paste("No se pudo enviar el enlace:", conditionMessage(error))))
      NULL
    })

    if (!is.null(result)) {
      password_reset_status(div(class = "alert alert-success", "Enlace enviado. Revise su correo y abra el enlace para definir una nueva contraseña."))
    }
  })

  observeEvent(input$supabase_auth_error, {
    login_error(div(class = "alert alert-danger", paste("Supabase no pudo completar la invitación:", input$supabase_auth_error)))
    public_page("login")
  }, ignoreInit = TRUE)

  observeEvent(input$supabase_auth_hash, {
    auth_hash <- input$supabase_auth_hash
    token <- value_or_default(auth_hash$access_token, "")
    if (!nzchar(token)) {
      login_error(div(class = "alert alert-danger", "El enlace de invitación no contiene un token válido."))
      public_page("login")
      return()
    }

    password_setup_token(token)
    password_setup_refresh_token(value_or_default(auth_hash$refresh_token, ""))
    password_setup_status(NULL)
    login_error(NULL)
    public_page("login")
    show_password_setup_modal()
  }, ignoreInit = TRUE)

  observeEvent(input$setup_password_save, {
    password <- value_or_default(input$setup_password, "")
    password_confirm <- value_or_default(input$setup_password_confirm, "")
    token <- password_setup_token()

    if (!nzchar(token)) {
      password_setup_status(div(class = "alert alert-danger", "El enlace de invitación expiró o no está disponible. Solicite una nueva invitación."))
      return()
    }
    if (nchar(password) < 8) {
      password_setup_status(div(class = "alert alert-warning", "La contraseña debe tener al menos 8 caracteres."))
      return()
    }
    if (!identical(password, password_confirm)) {
      password_setup_status(div(class = "alert alert-warning", "Las contraseñas no coinciden."))
      return()
    }

    result <- tryCatch({
      supabase_auth_update_password(token, password)
    }, error = function(error) {
      password_setup_status(div(class = "alert alert-danger", paste("No se pudo guardar la contraseña:", conditionMessage(error))))
      NULL
    })

    if (!is.null(result)) {
      password_setup_token("")
      password_setup_refresh_token("")
      password_setup_status(NULL)
      removeModal()
      login_error(div(class = "alert alert-success", "Contraseña creada correctamente. Ingrese con su correo y nueva contraseña."))
      public_page("login")
    }
  })

  observeEvent(input$login, {
    login_user <- trimws(value_or_default(input$login_user, ""))
    login_password <- value_or_default(input$login_password, "")

    if (!nzchar(login_user) || !nzchar(login_password)) {
      login_error(div(class = "alert alert-warning", "Ingrese usuario/correo y contraseña."))
      return()
    }

    if (is_sat26_survey_login(login_user, login_password)) {
      user_profile$username <- "EncuestaSatisfaccion"
      user_profile$email <- ""
      user_profile$user_id <- ""
      user_profile$access_token <- ""
      user_profile$refresh_token <- ""
      user_profile$name <- "Encuesta SAT26"
      user_profile$institution <- "EntoNet"
      user_profile$position <- "Encuesta SAT26"
      user_profile$country <- "Regional"
      logged_in(TRUE)
      public_page("login")
      active_area("sat26")
      active_module("intro")
      active_capture_subdivision(NULL)
      active_request_subdivision(NULL)
      active_request_data_subdivision(NULL)
      active_dataset(NULL)
      sat26_unique_code("")
      sat26_resume_status(NULL)
      login_error(NULL)
      session$sendCustomMessage("sat26ScrollTop", list())
      return()
    }

    result <- tryCatch({
      auth_email <- login_identifier_to_email(login_user)
      auth_session <- supabase_auth_sign_in(auth_email, login_password)
      auth_user <- auth_session$user
      profile <- fetch_usuario_perfil(
        login_identifier = login_user,
        auth_user_id = auth_user$id %||% "",
        auth_email = auth_user$email %||% auth_email
      )

      if (nrow(profile) == 0) {
        stop("La autenticación fue correcta, pero el usuario no tiene perfil autorizado.")
      }
      if (!isTRUE(profile$activo[[1]])) {
        stop("El usuario está inactivo.")
      }

      list(auth_session = auth_session, auth_user = auth_user, profile = profile[1, , drop = FALSE])
    }, error = function(error) {
      login_error(div(class = "alert alert-danger", paste("No se pudo iniciar sesión:", conditionMessage(error))))
      NULL
    })

    if (!is.null(result)) {
      profile <- result$profile
      user_profile$username <- value_or_default(profile$usuario[[1]], login_user)
      user_profile$email <- value_or_default(profile$email[[1]], result$auth_user$email %||% "")
      user_profile$user_id <- result$auth_user$id %||% value_or_default(profile$user_id[[1]], "")
      user_profile$access_token <- result$auth_session$access_token %||% ""
      user_profile$refresh_token <- result$auth_session$refresh_token %||% ""
      user_profile$name <- value_or_default(profile$nombre[[1]], user_profile$username)
      user_profile$institution <- value_or_default(profile$id_institucion[[1]], default_institution_id)
      user_profile$position <- value_or_default(profile$rol[[1]], "Rol no configurado")
      user_profile$country <- value_or_default(profile$pais[[1]], "País no configurado")
      logged_in(TRUE)
      public_page("login")
      login_error(NULL)
      apply_profile_to_capture_inputs()
      return()
    }
  })

  observeEvent(input$logout, {
    logged_in(FALSE)
    user_profile$access_token <- ""
    user_profile$refresh_token <- ""
    active_area(NULL)
    active_module(NULL)
    active_capture_subdivision(NULL)
    active_request_subdivision(NULL)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
    submission_status("No se ha enviado ningún registro en esta sesión.")
  })

  observeEvent(input$profile, {
    showModal(modalDialog(
      title = "Perfil de usuario",
      size = "m",
      easyClose = TRUE,
      div(
        class = "profile-grid",
        div(strong("Usuario"), div(value_or_default(user_profile$username, "Sin usuario"))),
        div(strong("Correo"), div(value_or_default(user_profile$email, "Sin correo enlazado"))),
        div(strong("Nombre"), div(value_or_default(user_profile$name, "Usuario autorizado"))),
        div(strong("Institución"), div(value_or_default(user_profile$institution, default_institution_id))),
        div(strong("Rol"), div(value_or_default(user_profile$position, "Rol no configurado"))),
        div(strong("País"), div(value_or_default(user_profile$country, "País no configurado")))
      ),
      div(class = "alert alert-info", "Este perfil se lee desde Supabase y define los permisos de acceso de la sesión."),
      tags$hr(),
      div(
        class = "profile-comment-field",
        textAreaInput(
          "profile_comment",
          "Comentarios o solicitud",
          placeholder = "Escriba aquí comentarios, preguntas o solicitudes sobre el portal EntoNet.",
          rows = 5
        )
      ),
      uiOutput("profile_email_link"),
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$save_profile, {
    user_profile$name <- value_or_default(trimws(input$profile_name_input), "Usuario autorizado")
    user_profile$institution <- value_or_default(trimws(input$profile_institution_input), "Institución no configurada")
    user_profile$position <- value_or_default(trimws(input$profile_position_input), "Puesto no configurado")
    user_profile$country <- value_or_default(trimws(input$profile_country_input), "País no configurado")
  })

  output$profile_save_message <- renderUI({
    req(input$save_profile)
    div(class = "alert alert-success profile-save-message", "Perfil actualizado para esta sesión.")
  })

  output$profile_email_link <- renderUI({
    recipient <- value_or_default(support_email, "jjuarezvaldez@gmail.com")
    comment <- input$profile_comment
    if (is.null(comment)) {
      comment <- ""
    }
    comment <- trimws(comment)
    profile_summary <- paste(
      "Usuario:", user_profile$name,
      "\nInstitución:", user_profile$institution,
      "\nPuesto:", user_profile$position,
      "\nPaís:", user_profile$country,
      "\n\nComentario:\n", comment,
      sep = ""
    )
    mailto <- paste0(
      "mailto:",
      URLencode(recipient, reserved = FALSE),
      "?subject=",
      URLencode("Comentario desde el portal EntoNet", reserved = TRUE),
      "&body=",
      URLencode(profile_summary, reserved = TRUE)
    )

    tags$a(
      href = mailto,
      class = "btn btn-primary",
      "Enviar comentario por correo"
    )
  })

  reset_form <- function() {
    updateSelectInput(session, "country", selected = "Guatemala")
    updateNumericInput(session, "cycle", value = 1)
    updateSelectInput(session, "round_number", selected = "1")
    updateNumericInput(session, "quadrant", value = NA)
    updateTextInput(session, "oviposition_code", value = "")
    updateTextInput(session, "substrate_code", value = "")
    updateTextInput(session, "collection_site", value = "")
    updateDateInput(session, "placement_date", value = NA)
    updateDateInput(session, "removal_date", value = NA)
    updateDateInput(session, "count_date", value = Sys.Date())
    updateTextInput(session, "count_responsible_code", value = "")
    updateNumericInput(session, "intact_eggs", value = 0)
    updateNumericInput(session, "hatched_eggs", value = 0)
    updateNumericInput(session, "canoe_eggs", value = 0)
    updateNumericInput(session, "unfertilized_eggs", value = 0)
    updateNumericInput(session, "other_species_count", value = 0)
    updateTextAreaInput(session, "notes", value = "")
  }

  show_formulario_5_modal <- function() {
    showModal(modalDialog(
      title = div(
        class = "modal-title-row",
        span("Formulario 5: Alimentación conteo"),
        actionButton("close_formulario_5_entry", HTML("&times;"), class = "modal-close-button")
      ),
      size = "l",
      easyClose = TRUE,
      formulario_5_capture_form(),
      footer = modalButton("Cerrar")
    ))
  }

  show_formulario_5_review_modal <- function() {
    showModal(modalDialog(
      title = div(
        class = "modal-title-row",
        span("Revisión formularios"),
        actionButton("close_formulario_5_review", HTML("&times;"), class = "modal-close-button")
      ),
      size = "l",
      easyClose = TRUE,
      formulario_5_review_form(),
      footer = modalButton("Cerrar")
    ))
  }

  show_formulario_7_review_modal <- function() {
    showModal(modalDialog(
      title = div(
        class = "modal-title-row",
        span("Revisión de Formulario 7"),
        actionButton("close_formulario_7_review", HTML("&times;"), class = "modal-close-button")
      ),
      size = "l",
      easyClose = TRUE,
      formulario_7_review_form(),
      footer = modalButton("Cerrar")
    ))
  }

  show_formulario_1_review_modal <- function() {
    showModal(modalDialog(
      title = div(
        class = "modal-title-row",
        span("Revisión de Formulario 1"),
        actionButton("close_formulario_1_review", HTML("&times;"), class = "modal-close-button")
      ),
      size = "l",
      easyClose = TRUE,
      formulario_1_review_form(),
      footer = modalButton("Cerrar")
    ))
  }

  show_formulario_1_print_modal <- function() {
    showModal(modalDialog(
      title = div(
        class = "modal-title-row",
        span("Imprimir Formulario 1"),
        actionButton("close_formulario_1_print", HTML("&times;"), class = "modal-close-button")
      ),
      size = "l",
      easyClose = TRUE,
      formulario_1_print_form(),
      footer = modalButton("Cerrar")
    ))
  }

  f5_number <- function(value, default = NA_real_) {
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return(default)
    }

    text_value <- trimws(as.character(value[[1]]))
    if (!nzchar(text_value)) {
      return(default)
    }

    suppressWarnings(as.numeric(text_value))
  }

  f5_text <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return("")
    }

    trimws(as.character(value))
  }

  f5_optional_text <- function(value) {
    text <- f5_text(value)
    if (!nzchar(text)) {
      return(NA_character_)
    }

    text
  }

  f5_integer <- function(value, default = NA_integer_) {
    number <- f5_number(value, default)
    if (is.na(number)) {
      return(default)
    }

    as.integer(number)
  }

  f5_date <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return(as.Date(NA))
    }

    as.Date(value)
  }

  f7_print_country_acronym <- function(country) {
    country <- toupper(trimws(value_or_default(country, "")))
    country <- chartr("ÁÉÍÓÚÜÑ", "AEIOUUN", country)
    if (country %in% c("GUATEMALA", "GT")) return("GT")
    if (country %in% c("EL SALVADOR", "SALVADOR", "SV")) return("SV")
    NA_character_
  }

  f7_print_department_choices <- function(country) {
    ubicacion_departamento_choices(country)
  }

  f7_print_municipality_choices <- function(country, department_code) {
    ubicacion_municipio_choices(country, department_code, include_manual = TRUE)
  }

  f7_print_selected_municipality_code <- function() {
    ubicacion_codigo_manual_o_seleccion(
      input$f7_print_codigo_bioensayo_municipio,
      input$f7_print_codigo_bioensayo_municipio_manual
    )
  }

  f7_print_bioassay_type_suffix <- function() {
    type <- value_or_default(input$f7_print_tipo_bioensayo, "DD")
    if (identical(type, "DD")) return("DD")
    if (identical(type, "IE")) return("IE")
    if (identical(type, "IC")) {
      dose <- toupper(trimws(value_or_default(input$f7_print_intensidad_completa_dosis, "")))
      if (!dose %in% c("2X", "5X", "10X")) return(NA_character_)
      return(paste0("IC", dose))
    }
    if (identical(type, "S")) {
      synergist <- toupper(trimws(value_or_default(input$f7_print_sinergista, "")))
      if (!synergist %in% c("DEF", "PBO", "DM")) return(NA_character_)
      return(paste0("S", synergist))
    }
    NA_character_
  }

  f7_print_synergist_code <- function() {
    if (!identical(value_or_default(input$f7_print_tipo_bioensayo, "DD"), "S")) return("")
    synergist <- toupper(trimws(value_or_default(input$f7_print_sinergista, "")))
    if (!synergist %in% c("DEF", "PBO", "DM")) return(NA_character_)
    synergist
  }

  f7_insecticide_code <- function(value) {
    cleaned <- toupper(trimws(value_or_default(value, "")))
    cleaned <- chartr("ÁÉÍÓÚÜÑ", "AEIOUUN", cleaned)
    if (cleaned %in% c("DEL", "DELTAMETRINA")) return("DEL")
    if (cleaned %in% c("PER", "PERMETRINA")) return("PER")
    if (cleaned %in% c("MAL", "MALATION", "MALATHION")) return("MAL")
    if (cleaned %in% c("DDT")) return("DDT")
    if (cleaned %in% c("BEN", "BENDIOCARB")) return("BEN")
    if (cleaned %in% c("ALF", "ALFA-CIPERMETRINA", "ALFACIPERMETRINA")) return("ALF")
    if (cleaned %in% c("LAM", "LAMBDA-CIALOTRINA", "LAMBDACIALOTRINA")) return("LAM")
    if (cleaned %in% c("TEM", "TEMEFOS")) return("TEM")
    NA_character_
  }

  f7_population_code <- function(value) {
    cleaned <- toupper(gsub("\\s+", "", value_or_default(value, "")))
    cleaned <- gsub(",", ".", cleaned, fixed = TRUE)
    cleaned <- sub("^P", "", cleaned)
    if (!grepl("^[0-9]+(\\.[0-9]+)?$", cleaned)) return(NA_character_)
    paste0("P", cleaned)
  }

  f7_generation_code <- function(value) {
    cleaned <- toupper(gsub("\\s+", "", value_or_default(value, "")))
    cleaned <- sub("^F", "", cleaned)
    if (!grepl("^[0-9]+$", cleaned)) return(NA_character_)
    paste0("F", cleaned)
  }

  f7_print_codigo_bioensayo_code <- function() {
    country_code <- f7_print_country_acronym(input$f7_print_pais)
    population_code <- f7_population_code(input$f7_print_codigo_bioensayo_poblacion_numero)
    municipality <- f7_print_selected_municipality_code()
    synergist <- f7_print_synergist_code()
    insecticide <- f7_insecticide_code(input$f7_print_insecticida)
    bioassay_number <- f5_integer(input$f7_print_codigo_bioensayo_correlativo)
    generation <- f7_generation_code(input$f7_print_generacion_filial)
    year <- f5_integer(input$f7_print_codigo_bioensayo_anio)
    if (
      is.na(country_code) || is.na(population_code) || !nzchar(municipality) ||
        is.na(synergist) || is.na(insecticide) || is.na(bioassay_number) ||
        bioassay_number < 1 || is.na(generation) || is.na(year)
    ) {
      return(NA_character_)
    }
    paste0("REI", sprintf("%02d", year %% 100L), country_code, municipality, population_code, synergist, insecticide, bioassay_number, generation)
  }

  f1_clean_text <- function(value) {
    value <- trimws(as.character(value))
    value[value %in% c("", "NA", "NaN")] <- NA_character_
    value[!is.na(value)] <- toupper(value[!is.na(value)])
    value
  }

  f1_parse_boolean <- function(value) {
    cleaned <- tolower(f1_clean_text(value))
    result <- rep(NA, length(cleaned))
    result[cleaned %in% c("true", "1", "si", "sí", "yes", "x")] <- TRUE
    result[cleaned %in% c("false", "0", "no")] <- FALSE
    result[is.na(cleaned)] <- FALSE
    result
  }

  f1_parse_date <- function(value) {
    cleaned <- f1_clean_text(value)
    parsed <- rep(as.Date(NA), length(cleaned))
    valid <- !is.na(cleaned) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", cleaned)
    parsed[valid] <- suppressWarnings(as.Date(cleaned[valid], format = "%Y-%m-%d"))
    parsed
  }

  f1_parse_number <- function(value) {
    cleaned <- f1_clean_text(value)
    parsed <- suppressWarnings(as.numeric(cleaned))
    parsed[is.na(cleaned)] <- NA_real_
    parsed
  }

  f1_parse_integer <- function(value) {
    number <- f1_parse_number(value)
    as.integer(number)
  }

  validate_formulario_1 <- function(csv_data) {
    details <- character()
    missing_columns <- setdiff(formulario_1_intake_columns, names(csv_data))
    extra_columns <- setdiff(names(csv_data), formulario_1_intake_columns)
    if (length(missing_columns)) details <- c(details, paste("Faltan columnas:", paste(missing_columns, collapse = ", ")))
    if (length(extra_columns)) details <- c(details, paste("Columnas no esperadas:", paste(extra_columns, collapse = ", ")))
    if (length(details)) return(list(data = NULL, details = details))

    data <- csv_data[formulario_1_intake_columns]
    if (nrow(data) == 0) return(list(data = NULL, details = "El archivo no contiene registros."))

    text_columns <- c(
      "formulario_codigo", "formulario_nombre", "pais", "id_institucion", "departamento", "municipio", "ciclo", "ronda",
      "codigo_formulario", "grupo_responsable_colocacion", "cuadrante", "codigo_casa", "codigo_gps",
      "codigo_sustrato", "grupo_responsable_retiro", "retiro_casa_cerrada_descripcion",
      "fuente_formulario", "creado_por"
    )
    for (column in text_columns) data[[column]] <- f1_clean_text(data[[column]])
    data$id_institucion[is.na(data$id_institucion)] <- default_institution_id
    data$pais[data$pais == "EL SALVADOR"] <- "El Salvador"
    data$pais[data$pais == "GUATEMALA"] <- "Guatemala"

    date_columns <- c("fecha_registro", "fecha_colocacion", "fecha_retiro")
    raw_dates <- lapply(data[date_columns], f1_clean_text)
    for (column in date_columns) data[[column]] <- f1_parse_date(data[[column]])

    numeric_columns <- c("Latitud", "Longitud")
    raw_numeric <- lapply(data[numeric_columns], f1_clean_text)
    for (column in numeric_columns) data[[column]] <- f1_parse_number(data[[column]])

    state_count_columns <- c(
      "retiro_buen_estado", "retiro_sin_agua", "retiro_sin_sustrato", "retiro_sin_ovitrampa",
      "retiro_movida", "retiro_volteada", "retiro_casa_cerrada"
    )
    visible_state_count_columns <- c(
      "retiro_buen_estado", "retiro_sin_agua", "retiro_sin_sustrato",
      "retiro_sin_ovitrampa", "retiro_casa_cerrada"
    )
    integer_columns <- c("Ovitrampas_colocadas", "Ovitrampas_retiradas", state_count_columns)
    raw_integer <- lapply(data[integer_columns], f1_clean_text)
    parsed_integer_numbers <- lapply(data[integer_columns], f1_parse_number)
    for (column in integer_columns) data[[column]] <- as.integer(parsed_integer_numbers[[column]])
    for (column in state_count_columns) data[[column]][is.na(data[[column]])] <- 0L
    data$retiro_movida <- 0L
    data$retiro_volteada <- 0L

    required_text <- c("formulario_codigo", "formulario_nombre", "pais", "id_institucion", "ciclo", "cuadrante", "codigo_casa", "codigo_sustrato")
    for (column in required_text) {
      bad <- which(is.na(data[[column]]))
      if (length(bad)) details <- c(details, paste0(column, " es obligatorio. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }

    for (column in c("fecha_registro", "fecha_colocacion")) {
      bad <- which(is.na(raw_dates[[column]]) | is.na(data[[column]]))
      if (length(bad)) details <- c(details, paste0(column, " es obligatorio y debe usar YYYY-MM-DD. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    bad_optional_retiro <- which(!is.na(raw_dates$fecha_retiro) & is.na(data$fecha_retiro))
    if (length(bad_optional_retiro)) details <- c(details, paste0("fecha_retiro debe usar YYYY-MM-DD o quedar vacía. Filas: ", paste(head(bad_optional_retiro, 10), collapse = ", ")))

    for (column in numeric_columns) {
      bad <- which(!is.na(raw_numeric[[column]]) & is.na(data[[column]]))
      if (length(bad)) details <- c(details, paste0(column, " debe ser numérico o quedar vacío. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    bad_lat <- which(!is.na(data$Latitud) & (data$Latitud < -90 | data$Latitud > 90))
    if (length(bad_lat)) details <- c(details, paste0("Latitud debe estar entre -90 y 90. Filas: ", paste(head(bad_lat, 10), collapse = ", ")))
    bad_lon <- which(!is.na(data$Longitud) & (data$Longitud < -180 | data$Longitud > 180))
    if (length(bad_lon)) details <- c(details, paste0("Longitud debe estar entre -180 y 180. Filas: ", paste(head(bad_lon, 10), collapse = ", ")))

    for (column in integer_columns) {
      parsed <- parsed_integer_numbers[[column]]
      bad <- which(!is.na(raw_integer[[column]]) & (is.na(parsed) | parsed < 0 | parsed != floor(parsed)))
      if (length(bad)) details <- c(details, paste0(column, " debe ser entero no negativo o quedar vacío. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    missing_placed <- which(is.na(data$Ovitrampas_colocadas) | data$Ovitrampas_colocadas < 1)
    if (length(missing_placed)) details <- c(details, paste0("Ovitrampas_colocadas es obligatorio y debe ser entero mayor que cero. Filas: ", paste(head(missing_placed, 10), collapse = ", ")))
    bad_retiradas <- which(!is.na(data$Ovitrampas_retiradas) & !is.na(data$Ovitrampas_colocadas) & data$Ovitrampas_retiradas > data$Ovitrampas_colocadas)
    if (length(bad_retiradas)) details <- c(details, paste0("Ovitrampas_retiradas no puede ser mayor que Ovitrampas_colocadas. Filas: ", paste(head(bad_retiradas, 10), collapse = ", ")))
    house_key_columns <- c("codigo_formulario", "cuadrante", "codigo_casa")
    house_keys <- do.call(paste, c(lapply(data[house_key_columns], function(value) ifelse(is.na(value), "", as.character(value))), sep = "\r"))
    representative_rows <- vapply(split(seq_len(nrow(data)), house_keys), function(rows) rows[[1]], integer(1))
    representative_rows <- sort(unname(representative_rows))
    collected_state_columns <- c("retiro_buen_estado", "retiro_sin_agua", "retiro_sin_sustrato")
    state_totals <- rowSums(data[visible_state_count_columns], na.rm = TRUE)
    collected_state_totals <- rowSums(data[collected_state_columns], na.rm = TRUE)
    missing_retiradas_for_states <- representative_rows[
      state_totals[representative_rows] > 0 & is.na(data$Ovitrampas_retiradas[representative_rows])
    ]
    if (length(missing_retiradas_for_states)) details <- c(details, paste0("Ingrese Ovitrampas_retiradas cuando registre estados de retiro. Filas: ", paste(head(missing_retiradas_for_states, 10), collapse = ", ")))

    format_house_state_summary <- function(row_index) {
      paste0(
        "Fila ", row_index,
        " (cuadrante ", value_or_default(data$cuadrante[[row_index]], "sin dato"),
        ", casa ", value_or_default(data$codigo_casa[[row_index]], "sin dato"),
        "): colocadas=", data$Ovitrampas_colocadas[[row_index]],
        ", retiradas=", data$Ovitrampas_retiradas[[row_index]],
        ", suma estados=", state_totals[[row_index]],
        " [buen estado=", data$retiro_buen_estado[[row_index]],
        ", sin agua=", data$retiro_sin_agua[[row_index]],
        ", sin sustrato=", data$retiro_sin_sustrato[[row_index]],
        ", dañada=", data$retiro_sin_ovitrampa[[row_index]],
        ", casa cerrada=", data$retiro_casa_cerrada[[row_index]], "]"
      )
    }

    bad_state_total_placed <- representative_rows[
      !is.na(data$Ovitrampas_colocadas[representative_rows]) &
        state_totals[representative_rows] > data$Ovitrampas_colocadas[representative_rows]
    ]
    if (length(bad_state_total_placed)) {
      bad_summary <- unique(vapply(head(bad_state_total_placed, 10), format_house_state_summary, character(1)))
      details <- c(details, paste0(
        "La suma de estados visibles de retiro no puede ser mayor que Ovitrampas_colocadas. ",
        paste(bad_summary, collapse = "; ")
      ))
    }

    bad_collected_state_total <- representative_rows[
      !is.na(data$Ovitrampas_retiradas[representative_rows]) &
        collected_state_totals[representative_rows] > data$Ovitrampas_retiradas[representative_rows]
    ]
    if (length(bad_collected_state_total)) {
      bad_summary <- unique(vapply(head(bad_collected_state_total, 10), format_house_state_summary, character(1)))
      details <- c(details, paste0(
        "La suma de Buen estado, Sin agua y Sin sustrato no puede ser mayor que Ovitrampas_retiradas. ",
        paste(bad_summary, collapse = "; ")
      ))
    }

    bad_zero_retiradas_accounting <- representative_rows[
      !is.na(data$Ovitrampas_retiradas[representative_rows]) &
        !is.na(data$Ovitrampas_colocadas[representative_rows]) &
        data$Ovitrampas_retiradas[representative_rows] == 0 &
        state_totals[representative_rows] != data$Ovitrampas_colocadas[representative_rows]
    ]
    if (length(bad_zero_retiradas_accounting)) {
      bad_summary <- unique(vapply(head(bad_zero_retiradas_accounting, 10), format_house_state_summary, character(1)))
      details <- c(details, paste0(
        "Cuando Ovitrampas_retiradas es 0, la suma de estados visibles debe ser igual a Ovitrampas_colocadas. ",
        paste(bad_summary, collapse = "; ")
      ))
    }

    bad_country <- which(!is.na(data$pais) & !(data$pais %in% c("El Salvador", "Guatemala")))
    if (length(bad_country)) details <- c(details, paste0("pais debe ser El Salvador o Guatemala. Filas: ", paste(head(bad_country, 10), collapse = ", ")))

    bad_dates <- which(!is.na(data$fecha_retiro) & data$fecha_colocacion > data$fecha_retiro)
    if (length(bad_dates)) details <- c(details, paste0("fecha_colocacion no puede ser posterior a fecha_retiro. Filas: ", paste(head(bad_dates, 10), collapse = ", ")))
    data$retiro_casa_cerrada_descripcion[data$retiro_casa_cerrada == 0] <- NA_character_
    bad_codigo_sustrato <- which(!is.na(data$codigo_sustrato) & !grepl("^[A-Z]+[0-9]+[A-H]$", data$codigo_sustrato))
    if (length(bad_codigo_sustrato)) details <- c(details, paste0("codigo_sustrato debe tener letras, dígitos y una letra final A-H. Filas: ", paste(head(bad_codigo_sustrato, 10), collapse = ", ")))

    header_key_columns <- c(
      "formulario_codigo", "formulario_nombre", "fecha_registro", "pais", "departamento", "municipio",
      "ciclo", "ronda", "codigo_formulario", "fecha_colocacion", "grupo_responsable_colocacion",
      "cuadrante", "codigo_casa", "Latitud", "Longitud", "codigo_gps", "Ovitrampas_colocadas", "fecha_retiro",
      "grupo_responsable_retiro", "Ovitrampas_retiradas",
      state_count_columns, "retiro_casa_cerrada_descripcion", "fuente_formulario", "creado_por"
    )
    header_keys <- do.call(paste, c(lapply(data[header_key_columns], function(value) ifelse(is.na(value), "", as.character(value))), sep = "\r"))
    for (key in unique(header_keys)) {
      rows <- which(header_keys == key)
      expected <- unique(data$Ovitrampas_colocadas[rows])
      expected <- expected[!is.na(expected)]
      if (length(expected) == 1 && length(rows) != expected[[1]]) {
        display_code <- data$codigo_formulario[rows[[1]]]
        if (is.na(display_code) || !nzchar(display_code)) display_code <- "(sin código)"
        details <- c(details, paste0(
          "El grupo con codigo_formulario ",
          display_code,
          " indica ", expected[[1]], " ovitrampas colocadas, pero contiene ",
          length(rows), " fila(s) de ovitrampa."
        ))
      }
      duplicate_sustrato <- duplicated(data$codigo_sustrato[rows]) | duplicated(data$codigo_sustrato[rows], fromLast = TRUE)
      if (any(duplicate_sustrato)) {
        display_code <- data$codigo_formulario[rows[[1]]]
        if (is.na(display_code) || !nzchar(display_code)) display_code <- "(sin código)"
        details <- c(details, paste0(
          "El grupo con codigo_formulario ",
          display_code,
          " tiene codigo_sustrato repetido. Corrija los duplicados antes de guardar."
        ))
      }
    }

    list(data = data, details = unique(details))
  }

  formulario_1_tables <- function(row) {
    header <- data.frame(
      formulario_codigo = row$formulario_codigo,
      formulario_nombre = row$formulario_nombre,
      fecha_registro = row$fecha_registro,
      pais = row$pais,
      departamento = row$departamento,
      municipio = row$municipio,
      ciclo = row$ciclo,
      ronda = row$ronda,
      codigo_formulario = row$codigo_formulario,
      fecha_colocacion = row$fecha_colocacion,
      grupo_responsable_colocacion = row$grupo_responsable_colocacion,
      cuadrante = row$cuadrante,
      codigo_casa = row$codigo_casa,
      latitud = row$Latitud,
      longitud = row$Longitud,
      codigo_gps = row$codigo_gps,
      ovitrampas_colocadas = row$Ovitrampas_colocadas,
      fecha_retiro = row$fecha_retiro,
      grupo_responsable_retiro = row$grupo_responsable_retiro,
      ovitrampas_retiradas = row$Ovitrampas_retiradas,
      retiro_buen_estado = row$retiro_buen_estado,
      retiro_sin_agua = row$retiro_sin_agua,
      retiro_sin_sustrato = row$retiro_sin_sustrato,
      retiro_sin_ovitrampa = row$retiro_sin_ovitrampa,
      retiro_movida = row$retiro_movida,
      retiro_volteada = row$retiro_volteada,
      retiro_casa_cerrada = row$retiro_casa_cerrada,
      retiro_casa_cerrada_descripcion = row$retiro_casa_cerrada_descripcion,
      fuente_formulario = row$fuente_formulario,
      creado_por = row$creado_por,
      stringsAsFactors = FALSE
    )
    detail <- data.frame(
      codigo_sustrato = row$codigo_sustrato,
      stringsAsFactors = FALSE
    )
    list(header = header, detail = detail)
  }

  insert_formulario_1 <- function(connection, data, progress_callback = NULL) {
    header_columns <- c(
      "formulario_codigo", "formulario_nombre", "fecha_registro", "pais", "departamento", "municipio",
      "ciclo", "ronda", "codigo_formulario", "fecha_colocacion", "grupo_responsable_colocacion",
      "cuadrante", "codigo_casa", "Latitud", "Longitud", "codigo_gps", "Ovitrampas_colocadas", "fecha_retiro",
      "grupo_responsable_retiro", "Ovitrampas_retiradas",
      "retiro_buen_estado", "retiro_sin_agua", "retiro_sin_sustrato", "retiro_sin_ovitrampa",
      "retiro_movida", "retiro_volteada", "retiro_casa_cerrada", "retiro_casa_cerrada_descripcion",
      "fuente_formulario", "creado_por"
    )
    header_keys <- do.call(paste, c(lapply(data[header_columns], function(value) ifelse(is.na(value), "", as.character(value))), sep = "\r"))
    grouped_rows <- split(seq_len(nrow(data)), header_keys)
    intake_ids <- character(length(grouped_rows))
    dbWithTransaction(connection, {
      for (group_index in seq_along(grouped_rows)) {
        rows <- grouped_rows[[group_index]]
        tables <- formulario_1_tables(data[rows[[1]], , drop = FALSE])
        dbAppendTable(connection, Id(schema = "public", table = "formulario_1_ovitrampa_intake"), tables$header)
        intake_id <- dbGetQuery(connection, "select currval(pg_get_serial_sequence('public.formulario_1_ovitrampa_intake', 'intake_id'))::bigint as intake_id")$intake_id[[1]]
        intake_ids[[group_index]] <- as.character(intake_id)
        detail_rows <- do.call(rbind, lapply(rows, function(row_index) formulario_1_tables(data[row_index, , drop = FALSE])$detail))
        detail_rows$intake_id <- intake_id
        detail_rows <- detail_rows[c(
          "intake_id", "codigo_sustrato"
        )]
        dbAppendTable(connection, Id(schema = "public", table = "formulario_1_ovitrampa_detalle_intake"), detail_rows)
        if (is.function(progress_callback)) progress_callback(group_index, length(grouped_rows))
      }
    })
    intake_ids
  }

  f1_fetch_existing_formulario_1_rows <- function(connection, codigo_formulario) {
    code <- toupper(trimws(value_or_default(codigo_formulario, "")))
    if (!nzchar(code)) stop("Ingrese un código de formulario para continuar.")
    rows <- dbGetQuery(
      connection,
      paste(
        "select h.formulario_codigo, h.formulario_nombre, h.fecha_registro, h.pais, h.departamento, h.municipio,",
        "h.ciclo, h.ronda, h.codigo_formulario, h.fecha_colocacion, h.grupo_responsable_colocacion,",
        "h.cuadrante, h.codigo_casa, h.latitud as \"Latitud\", h.longitud as \"Longitud\", h.codigo_gps,",
        "h.ovitrampas_colocadas as \"Ovitrampas_colocadas\", d.codigo_sustrato, h.fecha_retiro,",
        "h.grupo_responsable_retiro, h.ovitrampas_retiradas as \"Ovitrampas_retiradas\",",
        "h.retiro_buen_estado, h.retiro_sin_agua, h.retiro_sin_sustrato, h.retiro_sin_ovitrampa,",
        "h.retiro_movida, h.retiro_volteada, h.retiro_casa_cerrada, h.retiro_casa_cerrada_descripcion,",
        "h.fuente_formulario, h.creado_por, h.creado_en, h.actualizado_en",
        "from public.formulario_1_ovitrampa_intake h",
        "join public.formulario_1_ovitrampa_detalle_intake d on d.intake_id = h.intake_id",
        "where upper(h.codigo_formulario) = $1",
        "order by h.cuadrante, h.codigo_casa, d.codigo_sustrato"
      ),
      params = list(code)
    )
    rows[formulario_1_intake_columns]
  }

  f1_existing_sustratos_for_form <- function(connection, codigo_formulario) {
    code <- toupper(trimws(value_or_default(codigo_formulario, "")))
    if (!nzchar(code)) return(character())
    rows <- dbGetQuery(
      connection,
      paste(
        "select d.codigo_sustrato",
        "from public.formulario_1_ovitrampa_intake h",
        "join public.formulario_1_ovitrampa_detalle_intake d on d.intake_id = h.intake_id",
        "where upper(h.codigo_formulario) = $1"
      ),
      params = list(code)
    )
    codes <- as.character(rows$codigo_sustrato)
    toupper(trimws(codes[!is.na(codes)]))
  }

  f1_rows_without_existing_sustratos <- function(connection, data) {
    existing <- unique(unlist(lapply(unique(data$codigo_formulario), function(code) {
      f1_existing_sustratos_for_form(connection, code)
    }), use.names = FALSE))
    if (!length(existing)) return(list(data = data, skipped = character()))
    current <- toupper(trimws(as.character(data$codigo_sustrato)))
    keep <- !(current %in% existing)
    list(data = data[keep, , drop = FALSE], skipped = data$codigo_sustrato[!keep])
  }

  f1_first_non_empty <- function(values, default = "") {
    values <- as.character(values)
    values <- values[!is.na(values) & nzchar(trimws(values))]
    if (length(values)) values[[1]] else default
  }

  f1_update_date_input <- function(input_id, value) {
    value <- as.Date(value)
    updateDateInput(session, input_id, value = if (is.na(value)) as.Date(character(0)) else value)
  }

  f1_substrate_base_from_code <- function(code) {
    code <- as.character(code)
    code <- if (length(code) && !is.na(code[[1]])) toupper(trimws(code[[1]])) else ""
    sub("[A-H]$", "", code)
  }

  f1_review_detail_field <- "codigo_sustrato"
  f1_review_protected_fields <- c("creado_en", "actualizado_en")
  f1_review_date_fields <- c("fecha_registro", "fecha_colocacion", "fecha_retiro")
  f1_review_numeric_fields <- c(
    "Latitud", "Longitud", "Ovitrampas_colocadas", "Ovitrampas_retiradas",
    "retiro_buen_estado", "retiro_sin_agua", "retiro_sin_sustrato",
    "retiro_sin_ovitrampa", "retiro_movida", "retiro_volteada", "retiro_casa_cerrada"
  )

  f1_review_field_label <- function(field) {
    labels <- c(
      formulario_codigo = "Código técnico", formulario_nombre = "Nombre del formulario",
      fecha_registro = "Fecha de ingreso formulario", pais = "País", departamento = "Departamento",
      municipio = "Municipio", ciclo = "Ciclo", ronda = "Ronda", codigo_formulario = "Código de formulario",
      fecha_colocacion = "Fecha de colocación", grupo_responsable_colocacion = "Grupo responsable colocación",
      cuadrante = "Código cuadrante", codigo_casa = "Código casa", Latitud = "Latitud",
      Longitud = "Longitud", codigo_gps = "Código GPS", Ovitrampas_colocadas = "Ovitrampas colocadas",
      codigo_sustrato = "Códigos de sustrato", fecha_retiro = "Fecha de retiro",
      grupo_responsable_retiro = "Código responsable retiro", Ovitrampas_retiradas = "Ovitrampas retiradas",
      retiro_buen_estado = "Buen estado", retiro_sin_agua = "Sin agua", retiro_sin_sustrato = "Sin sustrato",
      retiro_sin_ovitrampa = "Dañada", retiro_movida = "Movida", retiro_volteada = "Volteada",
      retiro_casa_cerrada = "Casa cerrada", retiro_casa_cerrada_descripcion = "Descripción de casa cerrada",
      fuente_formulario = "Versión del formulario", creado_por = "Nombre de quien ingresó",
      creado_en = "Creado en", actualizado_en = "Actualizado en"
    )
    if (field %in% names(labels)) return(unname(labels[[field]]))
    label <- gsub("_", " ", field, fixed = TRUE)
    paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
  }

  f1_review_section <- function(field) {
    if (field %in% c("retiro_movida", "retiro_volteada")) return("Oculto")
    if (field %in% c(
      "formulario_codigo", "formulario_nombre", "fecha_registro", "pais", "departamento",
      "municipio", "ciclo", "ronda", "codigo_formulario", "fuente_formulario", "creado_por"
    )) return("Datos generales")
    if (field %in% c(
      "fecha_colocacion", "grupo_responsable_colocacion", "cuadrante", "codigo_casa",
      "Latitud", "Longitud", "codigo_gps", "Ovitrampas_colocadas", "codigo_sustrato"
    )) return("Colocación")
    if (field %in% c(
      "fecha_retiro", "grupo_responsable_retiro", "Ovitrampas_retiradas",
      "retiro_buen_estado", "retiro_sin_agua", "retiro_sin_sustrato",
      "retiro_sin_ovitrampa", "retiro_movida", "retiro_volteada",
      "retiro_casa_cerrada", "retiro_casa_cerrada_descripcion"
    )) return("Retiro")
    "Auditoría"
  }

  f1_review_data_from_record <- function(header, details) {
    values <- setNames(rep(list(NA_character_), length(formulario_1_intake_columns)), formulario_1_intake_columns)
    mapping <- c(
      formulario_codigo = "formulario_codigo", formulario_nombre = "formulario_nombre",
      fecha_registro = "fecha_registro", pais = "pais", departamento = "departamento", municipio = "municipio",
      ciclo = "ciclo", ronda = "ronda", codigo_formulario = "codigo_formulario",
      fecha_colocacion = "fecha_colocacion", grupo_responsable_colocacion = "grupo_responsable_colocacion",
      cuadrante = "cuadrante", codigo_casa = "codigo_casa", Latitud = "latitud", Longitud = "longitud",
      codigo_gps = "codigo_gps", Ovitrampas_colocadas = "ovitrampas_colocadas",
      fecha_retiro = "fecha_retiro", grupo_responsable_retiro = "grupo_responsable_retiro",
      Ovitrampas_retiradas = "ovitrampas_retiradas", retiro_buen_estado = "retiro_buen_estado",
      retiro_sin_agua = "retiro_sin_agua", retiro_sin_sustrato = "retiro_sin_sustrato",
      retiro_sin_ovitrampa = "retiro_sin_ovitrampa", retiro_movida = "retiro_movida",
      retiro_volteada = "retiro_volteada", retiro_casa_cerrada = "retiro_casa_cerrada",
      retiro_casa_cerrada_descripcion = "retiro_casa_cerrada_descripcion", fuente_formulario = "fuente_formulario",
      creado_por = "creado_por", creado_en = "creado_en", actualizado_en = "actualizado_en"
    )
    for (field in names(mapping)) {
      source <- unname(mapping[[field]])
      if (source %in% names(header)) values[[field]] <- header[[source]][[1]]
    }
    values$codigo_sustrato <- paste(details$codigo_sustrato, collapse = "\n")
    as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
  }

  f1_fetch_review_record <- function(intake_id) {
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    header <- dbGetQuery(
      connection,
      "select * from public.formulario_1_ovitrampa_intake where intake_id = $1",
      params = list(as.integer(intake_id))
    )
    if (nrow(header) == 0) return(NULL)
    details <- dbGetQuery(
      connection,
      "select detalle_id, codigo_sustrato from public.formulario_1_ovitrampa_detalle_intake where intake_id = $1 order by codigo_sustrato",
      params = list(as.integer(intake_id))
    )
    list(header = header, details = details, data = f1_review_data_from_record(header, details))
  }

  f1_load_review_records <- function(random_sample = FALSE) {
    start_date <- as.Date(input$f1_review_start_date)
    end_date <- as.Date(input$f1_review_end_date)
    if (is.na(start_date) || is.na(end_date) || start_date > end_date) stop("Seleccione un rango de fechas válido.")
    status <- f5_text(input$f1_review_filter_status)
    if (!status %in% c("pending", "reviewed", "rejected", "all")) status <- "pending"
    exclude_submitter <- f7_clean_text(input$f1_review_exclude_submitter)[[1]]
    where_clause <- "where fecha_registro between $1 and $2 and ($3 = 'all' or review_status = $3)"
    params <- list(as.character(start_date), as.character(end_date), status)
    if (!is.na(exclude_submitter)) {
      where_clause <- paste0(where_clause, " and lower(coalesce(nullif(trim(creado_por), ''), 'Sin nombre')) <> lower($4)")
      params <- c(params, list(exclude_submitter))
    }
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    total <- dbGetQuery(
      connection,
      paste("select count(*)::integer as total from public.formulario_1_ovitrampa_intake", where_clause),
      params = params
    )$total[[1]]
    if (total == 0) return(data.frame())
    limit <- if (random_sample) max(1L, ceiling(as.integer(total) * 0.10)) else min(as.integer(total), 50L)
    order_clause <- if (random_sample) "order by random()" else "order by creado_en desc nulls last, intake_id desc"
    query <- paste(
      "select intake_id, codigo_formulario, fecha_registro, pais, cuadrante, codigo_casa, ovitrampas_colocadas, ovitrampas_retiradas, creado_por, review_status, actualizado_en from public.formulario_1_ovitrampa_intake",
      where_clause,
      order_clause,
      paste0("limit $", length(params) + 1L)
    )
    dbGetQuery(connection, query, params = c(params, list(as.integer(limit))))
  }

  f1_find_review_records_by_code <- function(codigo_formulario) {
    code <- toupper(trimws(value_or_default(codigo_formulario, "")))
    if (!nzchar(code)) stop("Ingrese un código de formulario válido.")
    status <- f5_text(input$f1_review_filter_status)
    if (!status %in% c("pending", "reviewed", "rejected", "all")) status <- "pending"
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    dbGetQuery(
      connection,
      "select intake_id, codigo_formulario, fecha_registro, pais, cuadrante, codigo_casa, ovitrampas_colocadas, ovitrampas_retiradas, review_status, actualizado_en from public.formulario_1_ovitrampa_intake where upper(codigo_formulario) = $1 and ($2 = 'all' or review_status = $2) order by cuadrante, codigo_casa, intake_id",
      params = list(code, status)
    )
  }

  f1_review_input_id <- function(field) paste0("f1_review_value_", field)

  f1_review_input_rows <- function() {
    selected <- f1_review_selected()
    base <- selected$data[rep(1, 1), formulario_1_intake_columns, drop = FALSE]
    for (field in setdiff(formulario_1_intake_columns, f1_review_protected_fields)) {
      current <- input[[f1_review_input_id(field)]]
      base[[field]] <- if (is.null(current) || length(current) == 0) NA_character_ else as.character(current[[1]])
    }
    raw_codes <- gsub("\\\\[nr]", "\n", base$codigo_sustrato[[1]])
    codes <- unlist(strsplit(raw_codes, "[,\n\r;]+"))
    codes <- trimws(codes)
    codes <- codes[nzchar(codes)]
    if (!length(codes)) codes <- ""
    rows <- base[rep(1, length(codes)), , drop = FALSE]
    rows$codigo_sustrato <- codes
    rows$creado_en <- selected$data$creado_en[[1]]
    rows$actualizado_en <- selected$data$actualizado_en[[1]]
    rows
  }

  f1_update_review_record <- function(intake_id, data) {
    tables <- formulario_1_tables(data[1, , drop = FALSE])
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    dbWithTransaction(connection, {
      columns <- names(tables$header)
      assignments <- paste0(as.character(dbQuoteIdentifier(connection, columns)), " = $", seq_along(columns))
      query <- paste0(
        "update public.formulario_1_ovitrampa_intake set ", paste(assignments, collapse = ", "),
        ", review_status = 'pending', review_notes = null, reviewed_by = null, reviewed_at = null, actualizado_en = now() where intake_id = $",
        length(columns) + 1L
      )
      params <- c(unname(as.list(tables$header[1, columns, drop = TRUE])), list(as.integer(intake_id)))
      updated <- dbExecute(connection, query, params = params)
      if (updated != 1L) stop("No se actualizó el registro seleccionado.")
      dbExecute(connection, "delete from public.formulario_1_ovitrampa_detalle_intake where intake_id = $1", params = list(as.integer(intake_id)))
      detail_rows <- do.call(rbind, lapply(seq_len(nrow(data)), function(row_index) formulario_1_tables(data[row_index, , drop = FALSE])$detail))
      detail_rows$intake_id <- as.integer(intake_id)
      detail_rows <- detail_rows[c("intake_id", "codigo_sustrato")]
      dbAppendTable(connection, Id(schema = "public", table = "formulario_1_ovitrampa_detalle_intake"), detail_rows)
    })
  }

  f1_delete_review_record <- function(intake_id, reason, deleted_by) {
    reason <- f7_clean_text(reason)[[1]]
    deleted_by <- f7_clean_text(deleted_by)[[1]]
    if (is.na(reason)) stop("El comentario de eliminación es obligatorio.")
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    dbWithTransaction(connection, {
      selected_header <- dbGetQuery(
        connection,
        "select intake_id, codigo_formulario, cuadrante, codigo_casa, review_status from public.formulario_1_ovitrampa_intake where intake_id = $1 for update",
        params = list(as.integer(intake_id))
      )
      if (nrow(selected_header) != 1) stop("No se encontró el registro seleccionado para eliminar.")
      audit_exists <- dbGetQuery(
        connection,
        "select to_regclass('public.formulario_1_ovitrampa_eliminacion_audit') is not null as exists"
      )$exists[[1]]
      if (isTRUE(audit_exists)) {
        dbExecute(
          connection,
          "insert into public.formulario_1_ovitrampa_eliminacion_audit (intake_id, codigo_formulario, cuadrante, codigo_casa, review_status, eliminado_por, motivo_eliminacion) values ($1, $2, $3, $4, $5, nullif($6, ''), $7)",
          params = list(
            as.integer(selected_header$intake_id[[1]]),
            as.character(selected_header$codigo_formulario[[1]]),
            as.character(selected_header$cuadrante[[1]]),
            as.character(selected_header$codigo_casa[[1]]),
            as.character(selected_header$review_status[[1]]),
            value_or_default(deleted_by, ""),
            reason
          )
        )
      }
      dbExecute(connection, "delete from public.formulario_1_ovitrampa_detalle_intake where intake_id = $1", params = list(as.integer(intake_id)))
      deleted <- dbExecute(connection, "delete from public.formulario_1_ovitrampa_intake where intake_id = $1", params = list(as.integer(intake_id)))
      if (deleted != 1L) stop("No se eliminó el registro seleccionado.")
      selected_header
    })
  }

  f1_xml_escape <- function(value) {
    value <- as.character(value)
    value <- gsub("&", "&amp;", value, fixed = TRUE)
    value <- gsub("<", "&lt;", value, fixed = TRUE)
    value <- gsub(">", "&gt;", value, fixed = TRUE)
    value <- gsub("\"", "&quot;", value, fixed = TRUE)
    value
  }

  f1_excel_col <- function(index) {
    letters <- character()
    while (index > 0) {
      index <- index - 1L
      letters <- c(LETTERS[(index %% 26L) + 1L], letters)
      index <- index %/% 26L
    }
    paste0(letters, collapse = "")
  }

  f1_excel_cell <- function(row, col, value = "", style = 0L) {
    ref <- paste0(f1_excel_col(col), row)
    style_attr <- if (style > 0L) paste0(' s="', style, '"') else ""
    if (is.null(value) || length(value) == 0 || is.na(value) || !nzchar(as.character(value))) {
      return(paste0('<c r="', ref, '"', style_attr, "/>"))
    }
    paste0(
      '<c r="', ref, '" t="inlineStr"', style_attr, "><is><t>",
      f1_xml_escape(value),
      "</t></is></c>"
    )
  }

  f1_excel_row <- function(row, cells, height = NULL) {
    height_attr <- if (is.null(height)) "" else paste0(' ht="', height, '" customHeight="1"')
    paste0('<row r="', row, '"', height_attr, ">", paste0(cells, collapse = ""), "</row>")
  }

  f1_code39_value <- function(value) {
    code <- toupper(trimws(value_or_default(value, "")))
    code <- gsub("[^A-Z0-9 .$/+%-]", "", code)
    if (!nzchar(code)) return("")
    paste0("*", code, "*")
  }

  f1_entonet_logo_path <- function() {
    candidates <- c(
      file.path("shiny_app", "www", "entonet-logo.png"),
      file.path("www", "entonet-logo.png")
    )
    found <- candidates[file.exists(candidates)]
    if (length(found)) found[[1]] else NA_character_
  }

  f1_rotate_rgba <- function(image, degrees) {
    angle <- degrees * pi / 180
    height <- dim(image)[[1]]
    width <- dim(image)[[2]]
    center_x <- (width + 1) / 2
    center_y <- (height + 1) / 2
    corner_x <- c(1, width, width, 1) - center_x
    corner_y <- c(1, 1, height, height) - center_y
    rotated_x <- (corner_x * cos(angle)) - (corner_y * sin(angle))
    rotated_y <- (corner_x * sin(angle)) + (corner_y * cos(angle))
    new_width <- as.integer(ceiling(max(rotated_x) - min(rotated_x)) + 2L)
    new_height <- as.integer(ceiling(max(rotated_y) - min(rotated_y)) + 2L)
    new_center_x <- (new_width + 1) / 2
    new_center_y <- (new_height + 1) / 2

    dest_row <- matrix(rep(seq_len(new_height), new_width), nrow = new_height, ncol = new_width)
    dest_col <- matrix(rep(seq_len(new_width), each = new_height), nrow = new_height, ncol = new_width)
    x <- dest_col - new_center_x
    y <- dest_row - new_center_y
    source_col <- round((x * cos(angle)) + (y * sin(angle)) + center_x)
    source_row <- round((-x * sin(angle)) + (y * cos(angle)) + center_y)
    valid <- source_row >= 1 & source_row <= height & source_col >= 1 & source_col <= width

    output <- array(0, dim = c(new_height, new_width, 4))
    for (channel in seq_len(4)) {
      source_channel <- image[, , channel]
      output_channel <- output[, , channel]
      output_channel[valid] <- source_channel[cbind(source_row[valid], source_col[valid])]
      output[, , channel] <- output_channel
    }
    output
  }

  f1_create_watermark_logo <- function(source_file, destination_file, opacity = 0.30, max_width = 1250L) {
    if (!requireNamespace("png", quietly = TRUE) || is.na(source_file) || !file.exists(source_file)) {
      return(FALSE)
    }
    logo <- png::readPNG(source_file)
    height <- dim(logo)[[1]]
    width <- dim(logo)[[2]]
    if (width > max_width) {
      new_width <- as.integer(max_width)
      new_height <- max(1L, as.integer(round(height * new_width / width)))
      row_index <- pmax(1L, pmin(height, as.integer(round(seq(1, height, length.out = new_height)))))
      col_index <- pmax(1L, pmin(width, as.integer(round(seq(1, width, length.out = new_width)))))
      logo <- logo[row_index, col_index, , drop = FALSE]
    }
    red <- logo[, , 1]
    green <- logo[, , 2]
    blue <- logo[, , 3]
    alpha <- if (dim(logo)[[3]] >= 4) logo[, , 4] else matrix(1, nrow = dim(logo)[[1]], ncol = dim(logo)[[2]])
    gray <- (0.299 * red) + (0.587 * green) + (0.114 * blue)
    visible_alpha <- alpha * opacity
    visible_alpha[gray > 0.98] <- 0

    watermark <- array(0, dim = c(dim(logo)[[1]], dim(logo)[[2]], 4))
    watermark[, , 1] <- gray
    watermark[, , 2] <- gray
    watermark[, , 3] <- gray
    watermark[, , 4] <- visible_alpha
    watermark <- f1_rotate_rgba(watermark, -25)
    canvas_height <- 1600L
    canvas_width <- 2200L
    canvas <- array(0, dim = c(canvas_height, canvas_width, 4))
    paste_height <- min(dim(watermark)[[1]], canvas_height)
    paste_width <- min(dim(watermark)[[2]], canvas_width)
    source_row_start <- max(1L, as.integer(floor((dim(watermark)[[1]] - paste_height) / 2)) + 1L)
    source_col_start <- max(1L, as.integer(floor((dim(watermark)[[2]] - paste_width) / 2)) + 1L)
    target_row_start <- max(1L, as.integer(floor((canvas_height - paste_height) / 2)) + 1L)
    target_col_start <- max(1L, as.integer(floor((canvas_width - paste_width) / 2)) + 1L)
    canvas[
      target_row_start:(target_row_start + paste_height - 1L),
      target_col_start:(target_col_start + paste_width - 1L),
      seq_len(4)
    ] <- watermark[
      source_row_start:(source_row_start + paste_height - 1L),
      source_col_start:(source_col_start + paste_width - 1L),
      seq_len(4)
    ]
    watermark <- canvas
    dir.create(dirname(destination_file), recursive = TRUE, showWarnings = FALSE)
    png::writePNG(watermark, destination_file)
    TRUE
  }

  f1_printable_sheet_xml <- function(
    pais, departamento, municipio, codigo_formulario, version_formulario,
    codigo_encuestadores, ciclo, ronda, codigo_cuadrante_base, casas_por_cuadrante,
    codigo_casa_base, codigo_sustrato_base, quadrants, include_watermark = FALSE
  ) {
    rows <- character()
    merges <- c("A1:N1", "C2:D2", "C3:D3", "C4:D4", "I2:K2", "L2:N2", "J3:K3", "L3:N3", "J4:K4", "L4:N4")

    add_row <- function(row, values, styles = rep(0L, length(values)), height = NULL) {
      cells <- lapply(seq_along(values), function(col) f1_excel_cell(row, col, values[[col]], styles[[col]]))
      rows <<- c(rows, f1_excel_row(row, cells, height))
    }

    add_row(1, c("ENTONET - FORMULARIO 1: COLOCACIÓN Y RETIRO DE OVITRAMPA", rep("", 13)), c(1L, rep(1L, 13)), 21)
    add_row(2, c("", "PAÍS", pais, "", "CICLO", ciclo, "RONDA", ronda, "CÓDIGOS GPS", "", "", codigo_formulario, "", ""), c(0, 8, 9, 9, 8, 9, 8, 9, 8, 8, 0, 10, 10, 10), 18)
    add_row(3, c("", "DEPARTAMENTO", departamento, "", "FECHA COLOCACIÓN", "", "FECHA RETIRO", "", "GPS INICIAL", "", "", paste("VERSIÓN", version_formulario), "", ""), c(0, 8, 9, 9, 8, 9, 8, 9, 8, 9, 9, 11, 11, 11), 18)
    add_row(4, c("", "MUNICIPIO", municipio, "", "GRUPO COLOCACIÓN", "", "GRUPO RETIRO", "", "GPS FINAL", "", "", f1_code39_value(codigo_formulario), "", ""), c(0, 8, 9, 9, 8, 9, 8, 9, 8, 9, 9, 12, 12, 12), 22)

    current_row <- 6L
    headers <- c(
      "NO.", "CÓDIGO CASA", "LATITUD", "LONGITUD", "CÓDIGO SUSTRATO",
      "BARRAS SUSTRATO", "", "COLOCADAS", "RETIRADAS", "BUEN ESTADO", "SIN AGUA",
      "SIN SUSTRATO", "DAÑADA", "CASA CERRADA"
    )

    for (quadrant in seq_len(quadrants)) {
      quadrant_code <- f1_increment_quadrant_code(codigo_cuadrante_base, quadrant - 1L)
      if (!nzchar(quadrant_code) || is.na(quadrant_code)) quadrant_code <- paste("CUADRANTE", quadrant)
      merges <- c(
        merges,
        paste0("A", current_row, ":E", current_row),
        paste0("F", current_row, ":H", current_row),
        paste0("I", current_row, ":N", current_row)
      )
      add_row(
        current_row,
        c(quadrant_code, "", "", "", "", f1_code39_value(quadrant_code), "", "", "Comentario:", "", "", "", "", ""),
        c(rep(14L, 5), rep(12L, 3), rep(9L, 6)),
        27.8
      )
      current_row <- current_row + 1L
      merges <- c(merges, paste0("F", current_row, ":G", current_row))
      header_styles <- rep(5L, length(headers))
      header_styles[8:9] <- 13L
      add_row(current_row, headers, header_styles, 26)
      current_row <- current_row + 1L
      for (house_row in seq_len(casas_por_cuadrante)) {
        house_offset <- (quadrant - 1L) * casas_por_cuadrante + house_row - 1L
        house_code <- f1_increment_code(codigo_casa_base, house_offset)
        if (!nzchar(house_code) || is.na(house_code)) house_code <- ""
        sustrato_code <- f1_increment_code(codigo_sustrato_base, house_offset)
        if (!nzchar(sustrato_code) || is.na(sustrato_code)) sustrato_code <- ""
        merges <- c(merges, paste0("C", current_row, ":D", current_row), paste0("F", current_row, ":G", current_row))
        add_row(
          current_row,
          c(as.character(house_row), house_code, "", "", sustrato_code, f1_code39_value(sustrato_code), "", "", "", "", "", "", "", ""),
          c(6L, 6L, 6L, 6L, 6L, 12L, 12L, 6L, 6L, 6L, 6L, 6L, 6L, 6L),
          44.3
        )
        current_row <- current_row + 1L
      }
    }

    merge_xml <- paste0(
      '<mergeCells count="', length(merges), '">',
      paste0(sprintf('<mergeCell ref="%s"/>', merges), collapse = ""),
      "</mergeCells>"
    )

    paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
      '<sheetPr><pageSetUpPr fitToPage="1"/></sheetPr>',
      '<sheetViews><sheetView showGridLines="0" workbookViewId="0"/></sheetViews>',
      '<sheetFormatPr defaultRowHeight="18"/>',
      '<cols>',
      '<col min="1" max="1" width="4" customWidth="1"/>',
      '<col min="2" max="2" width="9.35" customWidth="1"/>',
      '<col min="3" max="3" width="9.79" customWidth="1"/>',
      '<col min="4" max="4" width="10.12" customWidth="1"/>',
      '<col min="5" max="5" width="11" customWidth="1"/>',
      '<col min="6" max="7" width="10" customWidth="1"/>',
      '<col min="8" max="14" width="7.5" customWidth="1"/>',
      '</cols>',
      '<sheetData>', paste0(rows, collapse = ""), '</sheetData>',
      merge_xml,
      '<pageMargins left="0.25" right="0.25" top="0.25" bottom="0.25" header="0.511811023622047" footer="0.511811023622047"/>',
      '<pageSetup paperSize="9" orientation="portrait" fitToWidth="1" fitToHeight="0"/>',
      if (include_watermark) '<picture r:id="rId1"/>' else '',
      '</worksheet>'
    )
  }

  f1_printable_header_watermark_vml <- function() {
    paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<xml xmlns:v="urn:schemas-microsoft-com:vml" xmlns:o="urn:schemas-microsoft-com:office:office" xmlns:x="urn:schemas-microsoft-com:office:excel">',
      '<o:shapelayout v:ext="edit"><o:idmap v:ext="edit" data="1"/></o:shapelayout>',
      '<v:shapetype id="_x0000_t75" coordsize="21600,21600" o:spt="75" o:preferrelative="t" path="m@4@5l@4@11@9@11@9@5xe" filled="f" stroked="f">',
      '<v:stroke joinstyle="miter"/>',
      '<v:formulas><v:f eqn="if lineDrawn pixelLineWidth 0"/><v:f eqn="sum @0 1 0"/><v:f eqn="sum 0 0 @1"/><v:f eqn="prod @2 1 2"/><v:f eqn="prod @3 21600 pixelWidth"/><v:f eqn="prod @3 21600 pixelHeight"/><v:f eqn="sum @0 0 1"/><v:f eqn="prod @6 1 2"/><v:f eqn="prod @7 21600 pixelWidth"/><v:f eqn="sum @8 21600 0"/><v:f eqn="prod @7 21600 pixelHeight"/><v:f eqn="sum @10 21600 0"/></v:formulas>',
      '<v:path o:extrusionok="f" gradientshapeok="t" o:connecttype="rect"/>',
      '<o:lock v:ext="edit" aspectratio="t"/>',
      '</v:shapetype>',
      '<v:shape id="CH" o:spid="_x0000_s1025" type="#_x0000_t75" style="position:absolute;margin-left:0;margin-top:95pt;width:560pt;height:350pt;z-index:1">',
      '<v:imagedata o:relid="rId1" o:title="entonet-watermark"/>',
      '<o:lock v:ext="edit" rotation="t"/>',
      '</v:shape>',
      '</xml>'
    )
  }

  f1_printable_styles_xml <- function() {
    paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
      '<fonts count="10">',
      '<font><sz val="10"/><name val="Calibri"/></font>',
      '<font><b/><sz val="14"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><b/><sz val="8"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><i/><sz val="10"/><color rgb="FF404040"/><name val="Calibri"/></font>',
      '<font><b/><sz val="8"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><b/><sz val="12"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><sz val="8"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><sz val="20"/><color rgb="FF000000"/><name val="Libre Barcode 39"/></font>',
      '<font><b/><sz val="7"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><b/><sz val="18"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '</fonts>',
      '<fills count="6">',
      '<fill><patternFill patternType="none"/></fill>',
      '<fill><patternFill patternType="gray125"/></fill>',
      '<fill><patternFill patternType="solid"><fgColor rgb="FFD9D9D9"/><bgColor indexed="64"/></patternFill></fill>',
      '<fill><patternFill patternType="solid"><fgColor rgb="FFF2F2F2"/><bgColor indexed="64"/></patternFill></fill>',
      '<fill><patternFill patternType="solid"><fgColor rgb="FFFFFFFF"/><bgColor indexed="64"/></patternFill></fill>',
      '<fill><patternFill patternType="none"/></fill>',
      '</fills>',
      '<borders count="3">',
      '<border><left/><right/><top/><bottom/><diagonal/></border>',
      '<border><left style="thin"><color rgb="FF000000"/></left><right style="thin"><color rgb="FF000000"/></right><top style="thin"><color rgb="FF000000"/></top><bottom style="thin"><color rgb="FF000000"/></bottom><diagonal/></border>',
      '<border><left style="medium"><color rgb="FF000000"/></left><right style="medium"><color rgb="FF000000"/></right><top style="medium"><color rgb="FF000000"/></top><bottom style="medium"><color rgb="FF000000"/></bottom><diagonal/></border>',
      '</borders>',
      '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>',
      '<cellXfs count="15">',
      '<xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>',
      '<xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1" applyAlignment="1"><alignment horizontal="center" vertical="center"/></xf>',
      '<xf numFmtId="0" fontId="2" fillId="3" borderId="2" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center"/></xf>',
      '<xf numFmtId="0" fontId="2" fillId="2" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="0" fillId="0" borderId="1" xfId="0" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="2" fillId="3" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="0" fillId="0" borderId="1" xfId="0" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="3" fillId="0" borderId="1" xfId="0" applyFont="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="4" fillId="2" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="0" fillId="0" borderId="1" xfId="0" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="5" fillId="4" borderId="2" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="6" fillId="4" borderId="2" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="7" fillId="0" borderId="1" xfId="0" applyFont="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center"/></xf>',
      '<xf numFmtId="0" fontId="8" fillId="3" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="9" fillId="3" borderId="2" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '</cellXfs>',
      '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>',
      '</styleSheet>'
    )
  }

  f7_printable_styles_xml <- function() {
    paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
      '<fonts count="7">',
      '<font><sz val="9"/><name val="Calibri"/></font>',
      '<font><b/><sz val="14"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><b/><sz val="9"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><sz val="9"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><b/><sz val="8"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><sz val="8"/><color rgb="FF000000"/><name val="Calibri"/></font>',
      '<font><sz val="18"/><color rgb="FF000000"/><name val="Libre Barcode 39"/></font>',
      '</fonts>',
      '<fills count="5">',
      '<fill><patternFill patternType="none"/></fill>',
      '<fill><patternFill patternType="gray125"/></fill>',
      '<fill><patternFill patternType="solid"><fgColor rgb="FFD9D9D9"/><bgColor indexed="64"/></patternFill></fill>',
      '<fill><patternFill patternType="solid"><fgColor rgb="FFF2F2F2"/><bgColor indexed="64"/></patternFill></fill>',
      '<fill><patternFill patternType="solid"><fgColor rgb="FFFFFFFF"/><bgColor indexed="64"/></patternFill></fill>',
      '</fills>',
      '<borders count="3">',
      '<border><left/><right/><top/><bottom/><diagonal/></border>',
      '<border><left style="thin"><color rgb="FF000000"/></left><right style="thin"><color rgb="FF000000"/></right><top style="thin"><color rgb="FF000000"/></top><bottom style="thin"><color rgb="FF000000"/></bottom><diagonal/></border>',
      '<border><left style="medium"><color rgb="FF000000"/></left><right style="medium"><color rgb="FF000000"/></right><top style="medium"><color rgb="FF000000"/></top><bottom style="medium"><color rgb="FF000000"/></bottom><diagonal/></border>',
      '</borders>',
      '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>',
      '<cellXfs count="20">',
      '<xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>',
      '<xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1" applyAlignment="1"><alignment horizontal="center" vertical="center"/></xf>',
      '<xf numFmtId="0" fontId="2" fillId="2" borderId="2" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center"/></xf>',
      '<xf numFmtId="0" fontId="2" fillId="2" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="3" fillId="0" borderId="1" xfId="0" applyFont="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="4" fillId="3" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="5" fillId="0" borderId="1" xfId="0" applyFont="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="3" fillId="3" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="4" fillId="2" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="3" fillId="0" borderId="1" xfId="0" applyFont="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="2" fillId="3" borderId="2" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="5" fillId="3" borderId="2" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="6" fillId="0" borderId="1" xfId="0" applyFont="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center"/></xf>',
      '<xf numFmtId="0" fontId="4" fillId="2" borderId="2" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1" applyAlignment="1"><alignment horizontal="center" vertical="center"/></xf>',
      '<xf numFmtId="0" fontId="2" fillId="2" borderId="2" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center"/></xf>',
      '<xf numFmtId="0" fontId="4" fillId="3" borderId="1" xfId="0" applyFont="1" applyFill="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="3" fillId="0" borderId="1" xfId="0" applyFont="1" applyBorder="1" applyAlignment="1"><alignment horizontal="left" vertical="center" wrapText="1"/></xf>',
      '<xf numFmtId="0" fontId="6" fillId="0" borderId="1" xfId="0" applyFont="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center"/></xf>',
      '<xf numFmtId="0" fontId="1" fillId="0" borderId="1" xfId="0" applyFont="1" applyBorder="1" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>',
      '</cellXfs>',
      '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>',
      '</styleSheet>'
    )
  }

  f1_write_file <- function(path, text) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, path, useBytes = TRUE)
  }

  f1_create_printable_xlsx <- function(
    file, pais, departamento, municipio, codigo_formulario, version_formulario,
    codigo_encuestadores, ciclo, ronda, codigo_cuadrante_base, casas_por_cuadrante,
    codigo_casa_base, codigo_sustrato_base, quadrants
  ) {
    root <- tempfile("f1_print_xlsx_")
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    logo_path <- f1_entonet_logo_path()
    watermark_path <- file.path(root, "xl", "media", "entonet-watermark.png")
    include_watermark <- f1_create_watermark_logo(logo_path, watermark_path)
    last_print_row <- 5L + as.integer(quadrants) * (as.integer(casas_por_cuadrante) + 2L)

    f1_write_file(file.path(root, "[Content_Types].xml"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
      '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
      '<Default Extension="xml" ContentType="application/xml"/>',
      if (include_watermark) '<Default Extension="png" ContentType="image/png"/>' else '',
      '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
      '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
      '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
      '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>',
      '<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>',
      '</Types>'
    ))
    f1_write_file(file.path(root, "_rels", ".rels"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
      '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>',
      '<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>',
      '</Relationships>'
    ))
    f1_write_file(file.path(root, "docProps", "app.xml"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">',
      '<Application>EntoNet</Application></Properties>'
    ))
    f1_write_file(file.path(root, "docProps", "core.xml"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
      '<dc:title>Formulario 1 imprimible</dc:title><dc:creator>EntoNet</dc:creator>',
      '<dcterms:created xsi:type="dcterms:W3CDTF">', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), '</dcterms:created>',
      '</cp:coreProperties>'
    ))
    f1_write_file(file.path(root, "xl", "workbook.xml"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
      '<sheets><sheet name="Formulario 1" sheetId="1" r:id="rId1"/></sheets>',
      '<definedNames>',
      '<definedName name="_xlnm.Print_Titles" localSheetId="0">\'Formulario 1\'!$1:$4</definedName>',
      '<definedName name="_xlnm.Print_Area" localSheetId="0">\'Formulario 1\'!$A$1:$N$', last_print_row, '</definedName>',
      '</definedNames>',
      '</workbook>'
    ))
    f1_write_file(file.path(root, "xl", "_rels", "workbook.xml.rels"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>',
      '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>',
      '</Relationships>'
    ))
    f1_write_file(file.path(root, "xl", "styles.xml"), f1_printable_styles_xml())
    f1_write_file(
      file.path(root, "xl", "worksheets", "sheet1.xml"),
      f1_printable_sheet_xml(
        pais, departamento, municipio, codigo_formulario, version_formulario,
        codigo_encuestadores, ciclo, ronda, codigo_cuadrante_base, casas_por_cuadrante,
        codigo_casa_base, codigo_sustrato_base, quadrants, include_watermark
      )
    )
    if (include_watermark) {
      dir.create(file.path(root, "xl", "worksheets", "_rels"), recursive = TRUE, showWarnings = FALSE)
      f1_write_file(file.path(root, "xl", "worksheets", "_rels", "sheet1.xml.rels"), paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
        '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="../media/entonet-watermark.png"/>',
        '</Relationships>'
      ))
    }

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(root)
    files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
    if (file.exists(file)) unlink(file)
    utils::zip(zipfile = file, files = files, flags = "-q")
  }

  f7_printable_sheet_xml <- function(
    pais, departamento, municipio, codigo_bioensayo, nombre_poblacion,
    tipo_bioensayo, version_formulario, include_watermark = FALSE
  ) {
    rows <- character()
    is_synergist_print <- grepl("^Sinergista", value_or_default(tipo_bioensayo, ""), ignore.case = TRUE)
    merges <- c(
      "A1:N1", "A2:N2", "A3:B3", "C3:G3", "I3:J3", "L3:N3",
      "A4:B4", "C4:G4", "I4:N4", "A5:B5", "C5:G5", "I5:N5",
      "A6:N6", "B7:N7",
      "A9:G9", "H9:N9", "A10:B10", "C10:G10", "H10:I10", "J10:K10", "L10:M10",
      "A11:B11", "C11:G11", "H11:I11", "J11:N11",
      "A12:B12", "C12:G12", "H12:I12", "J12:N12",
      "A13:B13", "C13:G13", "H13:I13", "J13:N13",
      "A14:B14", "C14:G14", "H14:I14", "J14:N14",
      "A15:B15", "C15:G15", "H15:I15", "J15:N15",
      "A16:B16",
      "A18:N18", "A19:B19", "C19:G19", "H19:I19", "J19:N19",
      "A20:B20", "C20:G20", "H20:I20", "J20:N20",
      "A22:G22", "H22:N22", "B23:C23", "E23:F23", "I23:J23", "L23:N23",
      "A25:N25", "A32:N32"
    )
    if (is_synergist_print) {
      merges <- setdiff(merges, c("C13:G13", "C14:G14"))
      merges <- c(merges, "C13:D13", "F13:G13", "C14:D14", "F14:G14", "A39:N39")
    }

    add_row <- function(row, values, styles = rep(0L, length(values)), height = NULL) {
      cells <- lapply(seq_along(values), function(col) f1_excel_cell(row, col, values[[col]], styles[[col]]))
      rows <<- c(rows, f1_excel_row(row, cells, height))
    }

    add_row(1, c("FORMULARIO 7. REGISTRO DE DATOS DEL BIOENSAYO DE LA BOTELLA CDC", rep("", 13)), c(14L, rep(14L, 13)), 22)
    add_row(2, c("1. INFORMACIÓN DE PROYECTO", rep("", 13)), c(15L, rep(15L, 13)), 18)
    add_row(3, c("Nombre de la población", "", nombre_poblacion, "", "", "", "", "País", pais, "", "Código", codigo_bioensayo, "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 17L, 17L, 16L, 17L, 17L, 17L), 20)
    add_row(4, c("Departamento", "", departamento, "", "", "", "", "Barras", f1_code39_value(codigo_bioensayo), "", "", "", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 18L, 18L, 18L, 18L, 18L, 18L), 23)
    add_row(5, c("Municipio", "", municipio, "", "", "", "", "Versión", version_formulario, "", "", "", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 17L, 17L, 17L, 17L, 17L, 17L), 20)

    add_row(6, c("2. TIPO DE BIOENSAYO", rep("", 13)), c(15L, rep(15L, 13)), 18)
    add_row(7, c("Tipo generado", tipo_bioensayo, "", "", "", "", "", "", "", "", "", "", "", ""), c(16L, rep(19L, 13)), 24)

    add_row(9, c("3. INFORMACIÓN DEL BIOENSAYO", rep("", 6), "4. INFORMACIÓN DEL MATERIAL BIOLÓGICO", rep("", 6)), c(rep(15L, 7), rep(15L, 7)), 18)
    add_row(10, c("Fecha realización (dd/mm/aa)", "", "", "", "", "", "", "Origen", "", "Silvestre__", "", "Laboratorio__", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(11, c(if (is_synergist_print) "Sinergista" else "Insecticida", "", "", "", "", "", "", "Edad", "", "", "", "Indefinida", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(12, c(if (is_synergist_print) "Dosis sinergista" else "Solvente utilizado", "", if (is_synergist_print) "" else "Etanol", "", if (is_synergist_print) "ug/mL" else "Otro:", "", "", "Código especie mosquito", "", "", "", "", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(13, c(if (is_synergist_print) "Insecticida" else "Concentración", "", "", "", if (is_synergist_print) "Solvente" else "", if (is_synergist_print) "" else "ug/mL", "", "Hora separación (hh:mm)", "", "", "h", "", "m", ""), c(16L, 16L, 17L, 17L, if (is_synergist_print) 16L else 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(14, c(if (is_synergist_print) "Concentración ug/mL" else "# lote insecticida", "", "", "", if (is_synergist_print) "# lote" else "", if (is_synergist_print) "" else "", "", "Fecha separación (dd/mm/aa)", "", "", "", "", "", ""), c(16L, 16L, 17L, 17L, if (is_synergist_print) 16L else 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(15, c("Fecha revestimiento (dd/mm/aa)", "", "", "", "", "", "", "Generación filial", "", "", "", "Indefinida", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(16, c("# Veces se han utilizado las botellas", "", "E1__", "", "E2__", "", "E3__", "", "E4__", "", "C1__", "", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L), 21)

    add_row(18, c("5. RESPONSABLES", rep("", 13)), c(15L, rep(15L, 13)), 18)
    add_row(19, c("Código quien realizó revestimiento", "", "", "", "", "", "", "Código quien realiza bioensayo", "", "", "", "", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(
      20,
      c("", "", "", "", "", "", "", "Código revisión 24h", "", "", "", "", "", ""),
      c(17L, 17L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L),
      21
    )

    add_row(22, c("6. CONDICIONES AMBIENTALES DEL BIOENSAYO", rep("", 6), "7. HORARIO DEL BIOENSAYO", rep("", 6)), c(rep(15L, 7), rep(15L, 7)), 18)
    add_row(23, c("Temp. inicial", "", "", "Temp. final", "", "", "", "Hora inicial (hh:mm)", "", "", "Hora final (hh:mm)", "", "", ""), c(16L, 17L, 17L, 16L, 17L, 17L, 17L, 16L, 17L, 17L, 16L, 17L, 17L, 17L), 21)
    add_row(24, c("HR inicial", "", "", "HR final", "", "", "", "", "", "", "", "", "", ""), c(16L, 17L, 17L, 16L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L), 21)

    bottle_labels <- c("E1", "E2", "E3", "E4", "C1")
    if (is_synergist_print) {
      add_row(25, c("8. SINERGISTA", rep("", 13)), c(15L, rep(15L, 13)), 18)
      add_row(26, c("BOTELLA", "INICIO (hh:mm)", "60 V", "60 I", "OBS.", "", "", "", "", "", "", "", "", ""), rep(16L, 14), 24)
      for (index in seq_along(bottle_labels)) {
        add_row(26L + index, c(bottle_labels[[index]], rep("", 13)), rep(17L, 14), 19)
      }
      add_row(32, c("9. LECTURA POR BOTELLA", rep("", 13)), c(15L, rep(15L, 13)), 18)
      add_row(33, c("BOTELLA", "INICIO (hh:mm)", "0 V", "0 I", "15 V", "15 I", "30 V", "30 I", "45 V", "45 I", "24H HORA (hh:mm)", "24H V", "24H I", "OBS."), rep(16L, 14), 24)
      for (index in seq_along(bottle_labels)) {
        add_row(33L + index, c(bottle_labels[[index]], rep("", 13)), rep(17L, 14), 19)
      }
      add_row(39, c("COMENTARIO", rep("", 13)), c(15L, rep(15L, 13)), 18)
      add_row(40, c("", rep("", 13)), rep(17L, 14), 34)
    } else {
      add_row(25, c("8. LECTURAS POR BOTELLA", rep("", 13)), c(15L, rep(15L, 13)), 18)
      add_row(26, c("BOTELLA", "INICIO (hh:mm)", "0 V", "0 I", "15 V", "15 I", "30 V", "30 I", "45 V", "45 I", "24H HORA (hh:mm)", "24H V", "24H I", "OBS."), rep(16L, 14), 24)
      for (index in seq_along(bottle_labels)) {
        add_row(26L + index, c(bottle_labels[[index]], rep("", 13)), rep(17L, 14), 19)
      }
      add_row(32, c("COMENTARIO", rep("", 13)), c(15L, rep(15L, 13)), 18)
      add_row(33, c("", rep("", 13)), rep(17L, 14), 34)
    }

    merge_xml <- paste0(
      '<mergeCells count="', length(merges), '">',
      paste0(sprintf('<mergeCell ref="%s"/>', merges), collapse = ""),
      "</mergeCells>"
    )

    paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
      '<sheetPr><pageSetUpPr fitToPage="1"/></sheetPr>',
      '<sheetViews><sheetView showGridLines="0" workbookViewId="0"/></sheetViews>',
      '<sheetFormatPr defaultRowHeight="18"/>',
      '<cols>',
      '<col min="1" max="14" width="9" customWidth="1"/>',
      '</cols>',
      '<sheetData>', paste0(rows, collapse = ""), '</sheetData>',
      merge_xml,
      '<pageMargins left="0.25" right="0.25" top="0.25" bottom="0.25" header="0.1" footer="0.1"/>',
      '<pageSetup paperSize="9" orientation="portrait" fitToWidth="1" fitToHeight="1"/>',
      if (include_watermark) '<picture r:id="rId1"/>' else '',
      '</worksheet>'
    )
  }

  f7_create_printable_xlsx <- function(
    file, pais, departamento, municipio, codigo_bioensayo, nombre_poblacion,
    tipo_bioensayo, version_formulario
  ) {
    is_synergist_print <- grepl("^Sinergista", value_or_default(tipo_bioensayo, ""), ignore.case = TRUE)
    last_print_row <- if (is_synergist_print) 40L else 33L
    root <- tempfile("f7_print_xlsx_")
    dir.create(root, recursive = TRUE)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    logo_path <- f1_entonet_logo_path()
    watermark_path <- file.path(root, "xl", "media", "entonet-watermark.png")
    include_watermark <- f1_create_watermark_logo(logo_path, watermark_path)

    f1_write_file(file.path(root, "[Content_Types].xml"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
      '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
      '<Default Extension="xml" ContentType="application/xml"/>',
      if (include_watermark) '<Default Extension="png" ContentType="image/png"/>' else '',
      '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
      '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
      '<Override PartName="/xl/styles.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
      '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>',
      '<Override PartName="/docProps/app.xml" ContentType="application/vnd.openxmlformats-officedocument.extended-properties+xml"/>',
      '</Types>'
    ))
    f1_write_file(file.path(root, "_rels", ".rels"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
      '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>',
      '<Relationship Id="rId3" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/extended-properties" Target="docProps/app.xml"/>',
      '</Relationships>'
    ))
    f1_write_file(file.path(root, "docProps", "app.xml"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Properties xmlns="http://schemas.openxmlformats.org/officeDocument/2006/extended-properties" xmlns:vt="http://schemas.openxmlformats.org/officeDocument/2006/docPropsVTypes">',
      '<Application>EntoNet</Application></Properties>'
    ))
    f1_write_file(file.path(root, "docProps", "core.xml"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:dcterms="http://purl.org/dc/terms/" xmlns:dcmitype="http://purl.org/dc/dcmitype/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">',
      '<dc:title>Formulario 7 imprimible</dc:title><dc:creator>EntoNet</dc:creator>',
      '<dcterms:created xsi:type="dcterms:W3CDTF">', format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), '</dcterms:created>',
      '</cp:coreProperties>'
    ))
    f1_write_file(file.path(root, "xl", "workbook.xml"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
      '<sheets><sheet name="Formulario 7" sheetId="1" r:id="rId1"/></sheets>',
      '<definedNames>',
      '<definedName name="_xlnm.Print_Titles" localSheetId="0">\'Formulario 7\'!$1:$4</definedName>',
      '<definedName name="_xlnm.Print_Area" localSheetId="0">\'Formulario 7\'!$A$1:$N$', last_print_row, '</definedName>',
      '</definedNames>',
      '</workbook>'
    ))
    f1_write_file(file.path(root, "xl", "_rels", "workbook.xml.rels"), paste0(
      '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>',
      '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" Target="styles.xml"/>',
      '</Relationships>'
    ))
    f1_write_file(file.path(root, "xl", "styles.xml"), f7_printable_styles_xml())
    f1_write_file(
      file.path(root, "xl", "worksheets", "sheet1.xml"),
      f7_printable_sheet_xml(
        pais, departamento, municipio, codigo_bioensayo, nombre_poblacion,
        tipo_bioensayo, version_formulario, include_watermark
      )
    )
    if (include_watermark) {
      dir.create(file.path(root, "xl", "worksheets", "_rels"), recursive = TRUE, showWarnings = FALSE)
      f1_write_file(file.path(root, "xl", "worksheets", "_rels", "sheet1.xml.rels"), paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
        '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="../media/entonet-watermark.png"/>',
        '</Relationships>'
      ))
    }

    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(root)
    files <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
    if (file.exists(file)) unlink(file)
    utils::zip(zipfile = file, files = files, flags = "-q")
  }

  f1_ovitrampa_count <- reactive({
    count <- f1_confirmed_ovitrampa_count()
    if (is.null(count) || is.na(count) || count < 1) return(NULL)
    min(as.integer(count), 50L)
  })

  f1_quadrant_count <- reactive({
    cfg <- f1_quadrant_config()
    if (is.null(cfg) || is.null(cfg$quadrants)) return(NULL)
    as.integer(cfg$quadrants)
  })

  f1_house_count <- reactive({
    cfg <- f1_quadrant_config()
    if (is.null(cfg) || is.null(cfg$houses_per_quadrant)) return(NULL)
    as.integer(cfg$houses_per_quadrant)
  })

  f1_traps_per_house <- reactive({
    cfg <- f1_quadrant_config()
    if (is.null(cfg) || is.null(cfg$traps_per_house)) return(NULL)
    as.integer(cfg$traps_per_house)
  })

  f1_increment_code <- function(code, offset) {
    code <- toupper(trimws(value_or_default(code, "")))
    match <- regexec("^([A-Z]+)([0-9]+)$", code)
    parts <- regmatches(code, match)[[1]]
    if (length(parts) != 3) return(NA_character_)
    paste0(parts[[2]], sprintf(paste0("%0", nchar(parts[[3]]), "d"), as.integer(parts[[3]]) + offset))
  }

  f1_code_has_counter <- function(code) {
    grepl("^[A-Za-z]+[0-9]+$", trimws(value_or_default(code, "")))
  }

  f1_country_acronym <- function(country) {
    country <- toupper(trimws(value_or_default(country, "")))
    country <- chartr("ÁÉÍÓÚÜÑ", "AEIOUUN", country)
    if (country %in% c("GUATEMALA", "GT")) return("GT")
    if (country %in% c("EL SALVADOR", "SALVADOR", "SV")) return("SV")
    NA_character_
  }

  f1_quadrant_code_parts <- function(code) {
    code <- toupper(trimws(value_or_default(code, "")))
    match <- regexec("^(REI)([0-9]{2})([A-Z]{2})([0-9]{4})(C)([0-9]+)$", code)
    parts <- regmatches(code, match)[[1]]
    if (length(parts) == 7) {
      return(list(
        format = "rei_municipio_c",
        prefix = parts[[2]],
        year = parts[[3]],
        country = parts[[4]],
        municipio = parts[[5]],
        c_prefix = parts[[6]],
        counter = as.integer(parts[[7]]),
        counter_width = nchar(parts[[7]])
      ))
    }

    match <- regexec("^(REI)([0-9]{2})([A-Z]{2})([0-9]{3})$", code)
    parts <- regmatches(code, match)[[1]]
    if (length(parts) != 5) return(NULL)
    list(
      format = "legacy_rei_country",
      prefix = parts[[2]],
      year = parts[[3]],
      country = parts[[4]],
      municipio = "",
      c_prefix = "",
      counter = as.integer(parts[[5]]),
      counter_width = nchar(parts[[5]])
    )
  }

  f1_quadrant_code_has_structure <- function(code, country = NULL) {
    parts <- f1_quadrant_code_parts(code)
    if (is.null(parts)) return(FALSE)
    expected_country <- f1_country_acronym(country)
    is.na(expected_country) || identical(parts$country, expected_country)
  }

  f1_increment_quadrant_code <- function(code, offset) {
    parts <- f1_quadrant_code_parts(code)
    if (is.null(parts)) return(f1_increment_code(code, offset))
    paste0(
      parts$prefix,
      parts$year,
      parts$country,
      parts$municipio,
      parts$c_prefix,
      sprintf(paste0("%0", parts$counter_width, "d"), parts$counter + offset)
    )
  }

  f1_new_quadrant_code <- function(country, municipality_code, year = Sys.Date(), quadrant_number = 1L) {
    country_code <- f1_country_acronym(country)
    municipality_code <- gsub("[^0-9]", "", value_or_default(municipality_code, ""))
    quadrant_number <- f5_integer(quadrant_number)
    if (is.na(quadrant_number) || quadrant_number < 1) quadrant_number <- 1L
    if (is.na(country_code) || !nzchar(country_code) || !grepl("^[0-9]{4}$", municipality_code)) {
      return(NA_character_)
    }
    year_number <- suppressWarnings(as.integer(format(as.Date(year), "%y")))
    if (is.na(year_number)) year_number <- as.integer(format(Sys.Date(), "%y"))
    paste0("REI", sprintf("%02d", year_number %% 100L), country_code, municipality_code, "C", sprintf("%03d", quadrant_number))
  }

  f1_new_form_code <- function(country, municipality_code, year = Sys.Date(), ronda = NA, ciclo = NA) {
    country_code <- f1_country_acronym(country)
    municipality_code <- gsub("[^0-9]", "", value_or_default(municipality_code, ""))
    ronda_number <- f5_integer(ronda)
    ciclo_number <- f5_integer(ciclo)
    if (
      is.na(country_code) || !nzchar(country_code) ||
        !grepl("^[0-9]{4}$", municipality_code) ||
        is.na(ronda_number) || ronda_number < 1 ||
        is.na(ciclo_number) || ciclo_number < 1
    ) {
      return(NA_character_)
    }
    year_number <- suppressWarnings(as.integer(format(as.Date(year), "%y")))
    if (is.na(year_number)) year_number <- as.integer(format(Sys.Date(), "%y"))
    paste0("REI", sprintf("%02d", year_number %% 100L), country_code, municipality_code, "R", ronda_number, "C", ciclo_number)
  }

  f1_quadrant_code <- function(quadrant_index) {
    f1_increment_quadrant_code(input$f1_codigo_cuadrante_base, quadrant_index - 1L)
  }

  f1_house_code <- function(quadrant_index, house_index) {
    house_count <- f1_house_count()
    offset <- (quadrant_index - 1L) * house_count + house_index - 1L
    f1_increment_code(input$f1_codigo_casa_base, offset)
  }

  f1_substrate_base_code <- function(quadrant_index, house_index) {
    house_count <- f1_house_count()
    offset <- (quadrant_index - 1L) * house_count + house_index - 1L
    f1_increment_code(input$f1_codigo_sustrato_base, offset)
  }

  f1_quadrant_house_codes <- function(quadrant_index, house_index) {
    base_code <- f1_substrate_base_code(quadrant_index, house_index)
    traps <- f1_traps_per_house()
    if (!nzchar(base_code) || is.null(traps)) return(character())
    paste0(base_code, LETTERS[seq_len(traps)])
  }

  f1_quadrant_editable <- function(quadrant_index) {
    quadrant_index %in% f1_editable_quadrants()
  }

  f1_adjusted_value <- function(input_id, generated_value) {
    value <- input[[input_id]]
    value <- if (is.null(value) || length(value) == 0) "" else trimws(as.character(value[[1]]))
    if (nzchar(value)) value else generated_value
  }

  f1_adjusted_quadrant_code <- function(quadrant_index) {
    f1_adjusted_value(paste0("f1_edit_cuadrante_", quadrant_index), f1_quadrant_code(quadrant_index))
  }

  f1_adjusted_house_code <- function(quadrant_index, house_index) {
    f1_adjusted_value(paste0("f1_edit_casa_", quadrant_index, "_", house_index), f1_house_code(quadrant_index, house_index))
  }

  f1_adjusted_substrate_base_code <- function(quadrant_index, house_index) {
    f1_adjusted_value(paste0("f1_edit_sustrato_", quadrant_index, "_", house_index), f1_substrate_base_code(quadrant_index, house_index))
  }

  f1_adjusted_trap_codes <- function(quadrant_index, house_index) {
    base_code <- f1_adjusted_substrate_base_code(quadrant_index, house_index)
    traps <- f1_traps_per_house()
    if (!nzchar(base_code) || is.null(traps)) return(character())
    paste0(base_code, LETTERS[seq_len(traps)])
  }

  f1_build_quadrant_panel <- function(quadrant_index) {
    house_count <- f1_house_count()
    if (is.null(house_count)) return(NULL)
    editable <- f1_quadrant_editable(quadrant_index)
    tags$div(
      class = "well",
      div(
        class = "submit-row",
        h4(paste("Cuadrante", quadrant_index)),
        actionButton(
          paste0("f1_toggle_edit_quadrant_", quadrant_index),
          if (editable) "Bloquear códigos" else "Editar códigos",
          class = if (editable) "btn-warning" else "btn-default"
        )
      ),
      if (editable) div(class = "alert alert-warning", "Modo edición activo para este cuadrante. Revise códigos de cuadrante, casa y sustrato base antes de guardar."),
      fluidRow(
        column(
          4,
          if (editable) {
            textInput(paste0("f1_edit_cuadrante_", quadrant_index), "Código cuadrante", value = value_or_default(f1_adjusted_quadrant_code(quadrant_index), ""))
          } else {
            tagList(
              tags$label("Código cuadrante"),
              tags$input(type = "text", class = "form-control", value = value_or_default(f1_adjusted_quadrant_code(quadrant_index), ""), readonly = "readonly")
            )
          }
        ),
        column(
          4,
          tags$label("Código casa inicial"),
          tags$input(type = "text", class = "form-control", value = value_or_default(f1_adjusted_house_code(quadrant_index, 1L), ""), readonly = "readonly")
        ),
        column(
          4,
          tags$label("Código sustrato inicial"),
          tags$input(type = "text", class = "form-control", value = value_or_default(f1_adjusted_substrate_base_code(quadrant_index, 1L), ""), readonly = "readonly")
        )
      ),
      if (quadrant_index %in% f1_generated_quadrants()) {
        lapply(seq_len(house_count), function(house_index) {
          trap_codes <- f1_adjusted_trap_codes(quadrant_index, house_index)
          house_suffix <- paste(quadrant_index, house_index, sep = "_")
          wellPanel(
            h5(paste("Casa", house_index)),
            fluidRow(
              column(
                4,
                if (editable) {
                  textInput(paste0("f1_edit_casa_", house_suffix), "Código casa", value = value_or_default(f1_adjusted_house_code(quadrant_index, house_index), ""))
                } else {
                  tagList(
                    tags$label("Código casa"),
                    tags$input(type = "text", class = "form-control", value = value_or_default(f1_adjusted_house_code(quadrant_index, house_index), ""), readonly = "readonly")
                  )
                }
              ),
              column(
                4,
                if (editable) {
                  tagList(
                    textInput(paste0("f1_edit_sustrato_", house_suffix), "Código sustrato base", value = value_or_default(f1_adjusted_substrate_base_code(quadrant_index, house_index), "")),
                    tags$label("Ovitrampas generadas"),
                    if (length(trap_codes)) tags$ul(class = "compact-list", lapply(trap_codes, function(code) tags$li(code))) else tags$span("Sin configuración")
                  )
                } else {
                  tagList(
                    tags$label("Ovitrampas de la casa"),
                    if (length(trap_codes)) tags$ul(class = "compact-list", lapply(trap_codes, function(code) tags$li(code))) else tags$span("Sin configuración")
                  )
                }
              ),
              column(
                4,
                numericInput(paste0("f1_Ovitrampas_retiradas_", house_suffix), "Ovitrampas retiradas", value = NA, min = 0, step = 1)
              )
            ),
            fluidRow(
              column(
                6,
                numericInput(paste0("f1_retiro_buen_estado_", house_suffix), "Buen estado", value = NA, min = 0, step = 1)
              ),
              column(
                6,
                numericInput(paste0("f1_retiro_sin_agua_", house_suffix), "Sin agua", value = NA, min = 0, step = 1)
              )
            ),
            fluidRow(
              column(
                4,
                numericInput(paste0("f1_retiro_sin_sustrato_", house_suffix), "Sin sustrato", value = NA, min = 0, step = 1)
              ),
              column(
                4,
                numericInput(paste0("f1_retiro_sin_ovitrampa_", house_suffix), "Dañada", value = NA, min = 0, step = 1)
              ),
              column(
                4,
                numericInput(paste0("f1_retiro_casa_cerrada_", house_suffix), "Casa cerrada", value = NA, min = 0, step = 1)
              )
            )
          )
        })
      }
    )
  }

  output$f1_ovitrampa_entries <- renderUI({
    qcount <- f1_quadrant_count()
    if (is.null(qcount)) {
      return(div(class = "alert alert-info", "Configure la colocación y genere los cuadrantes."))
    }
    NULL
  })

  output$f1_quadrant_tabs <- renderUI({
    qcount <- f1_quadrant_count()
    if (is.null(qcount)) {
      return(div(class = "alert alert-info", "Configure la colocación y genere los cuadrantes."))
    }
    do.call(tabsetPanel, c(
      list(id = "f1_quadrant_tabset"),
      lapply(seq_len(qcount), function(quadrant_index) {
        tabPanel(
          paste("Cuadrante", quadrant_index),
          value = paste0("quadrant_", quadrant_index),
          f1_build_quadrant_panel(quadrant_index)
        )
      })
    ))
  })

  formulario_1_input_row <- reactive({
    qcount <- f1_quadrant_count()
    house_count <- f1_house_count()
    traps <- f1_traps_per_house()
    if (is.null(qcount) || is.null(house_count) || is.null(traps)) {
      return(as.data.frame(setNames(rep(list(character()), length(formulario_1_intake_columns)), formulario_1_intake_columns), stringsAsFactors = FALSE, check.names = FALSE))
    }
    rows <- list()
    row_index <- 1L
    for (quadrant_index in seq_len(qcount)) {
      quadrant_code <- f1_adjusted_quadrant_code(quadrant_index)
      for (house_index in seq_len(house_count)) {
        house_code <- f1_adjusted_house_code(quadrant_index, house_index)
        base_code <- f1_adjusted_substrate_base_code(quadrant_index, house_index)
        for (trap_index in seq_len(traps)) {
          values <- setNames(rep(list(""), length(formulario_1_intake_columns)), formulario_1_intake_columns)
          values$formulario_codigo <- "F1"
          values$formulario_nombre <- "Colocacion y retiro de ovitrampa"
          for (column in setdiff(formulario_1_intake_columns, c("formulario_codigo", "formulario_nombre", "creado_en", "actualizado_en", "codigo_sustrato", "cuadrante", "codigo_casa", "Ovitrampas_colocadas"))) {
            input_value <- input[[paste0("f1_", column)]]
            if (!is.null(input_value) && length(input_value)) values[[column]] <- as.character(input_value[[1]])
          }
          values$municipio <- ubicacion_codigo_manual_o_seleccion(input$f1_municipio, input$f1_municipio_manual)
          values$cuadrante <- as.character(quadrant_code)
          values$codigo_casa <- as.character(house_code)
          values$Ovitrampas_colocadas <- as.character(traps)
          values$codigo_sustrato <- paste0(base_code, LETTERS[[trap_index]])
          house_suffix <- paste(quadrant_index, house_index, sep = "_")
          values$retiro_movida <- "0"
          values$retiro_volteada <- "0"
          for (retirement_column in c(
            "Ovitrampas_retiradas", "retiro_buen_estado", "retiro_sin_agua", "retiro_sin_sustrato",
            "retiro_sin_ovitrampa", "retiro_casa_cerrada"
          )) {
            input_value <- input[[paste0("f1_", retirement_column, "_", house_suffix)]]
            if (!is.null(input_value) && length(input_value)) values[[retirement_column]] <- as.character(input_value[[1]])
          }
          rows[[row_index]] <- as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
          row_index <- row_index + 1L
        }
      }
    }
    do.call(rbind, rows)
  })

  f5_formulario_5_record <- function() {
    data.frame(
      formulario_codigo = f5_text(input$f5_formulario_codigo),
      pais = f5_text(input$f5_pais),
      id_institucion = f5_text(value_or_default(input$f5_id_institucion, user_profile$institution)),
      departamento_numero = f5_integer(input$f5_departamento_numero),
      municipio_numero = f5_integer(input$f5_municipio_numero),
      ciclo = f5_text(input$f5_ciclo),
      formulario_nombre = f5_text(input$f5_formulario_nombre),
      fecha_registro = as.character(f5_date(input$f5_fecha_registro)),
      cepa_poblacion = f5_text(input$f5_cepa_poblacion),
      especie = f5_text(input$f5_especie),
      generacion_filial_adultos = f5_text(input$f5_generacion_filial_adultos),
      responsable_ingreso_jaula = f5_text(input$f5_responsable_ingreso_jaula),
      fecha_jaula = as.character(f5_date(input$f5_fecha_jaula)),
      numero_hembras = f5_integer(input$f5_numero_hembras, 0L),
      numero_machos = f5_integer(input$f5_numero_machos, 0L),
      total_huevos_viables = f5_integer(input$f5_total_huevos_viables),
      responsable_alimentacion = f5_text(input$f5_responsable_alimentacion),
      tipo_alimentacion_codigo = f5_text(input$f5_tipo_alimentacion_codigo),
      tipo_alimentacion_descripcion = f5_optional_text(input$f5_tipo_alimentacion_descripcion),
      fecha_alimentacion_sangre = as.character(f5_date(input$f5_fecha_alimentacion_sangre)),
      numero_charolas = f5_integer(input$f5_numero_charolas, 0L),
      observaciones_alimentacion = f5_optional_text(input$f5_observaciones_alimentacion),
      generacion_filial_huevos = f5_text(input$f5_generacion_filial_huevos),
      codigo_sustrato = f5_text(input$f5_codigo_sustrato),
      fecha_colocacion_sustrato = as.character(f5_date(input$f5_fecha_colocacion_sustrato)),
      fecha_retiro_sustrato = as.character(f5_date(input$f5_fecha_retiro_sustrato)),
      numero_cuadro_sustrato = f5_integer(input$f5_numero_cuadro_sustrato, 0L),
      hv_huevos_viables = f5_integer(input$f5_hv_huevos_viables, 0L),
      he_huevos_eclosionados = f5_integer(input$f5_he_huevos_eclosionados, 0L),
      hc_huevos_canoa = f5_integer(input$f5_hc_huevos_canoa, 0L),
      hnf_huevos_no_fecundados = f5_integer(input$f5_hnf_huevos_no_fecundados, 0L),
      responsable_conteo_huevos = f5_text(input$f5_responsable_conteo_huevos),
      observaciones_generales = f5_optional_text(input$f5_observaciones_generales),
      fuente_formulario = f5_optional_text(input$f5_fuente_formulario),
      creado_por = f5_optional_text(value_or_default(input$f5_creado_por, user_profile$name)),
      creado_en = as.character(f5_date(input$f5_creado_en)),
      stringsAsFactors = FALSE
    )
  }

  f5_required_missing_fields <- function(record) {
    required_fields <- c(
      "formulario_codigo",
      "pais",
      "id_institucion",
      "ciclo",
      "formulario_nombre",
      "fecha_registro",
      "cepa_poblacion",
      "especie",
      "generacion_filial_adultos",
      "responsable_ingreso_jaula",
      "fecha_jaula",
      "responsable_alimentacion",
      "tipo_alimentacion_codigo",
      "fecha_alimentacion_sangre",
      "generacion_filial_huevos",
      "codigo_sustrato",
      "fecha_colocacion_sustrato",
      "fecha_retiro_sustrato",
      "responsable_conteo_huevos"
    )

    required_labels <- c(
      formulario_codigo = "Código del formulario",
      pais = "País",
      id_institucion = "ID Institución",
      ciclo = "Ciclo",
      formulario_nombre = "Nombre del formulario",
      fecha_registro = "Fecha de registro",
      cepa_poblacion = "Cepa / población",
      especie = "Especie",
      generacion_filial_adultos = "Generación filial adultos",
      responsable_ingreso_jaula = "Responsable ingreso jaula",
      fecha_jaula = "Fecha jaula",
      responsable_alimentacion = "Responsable alimentación",
      tipo_alimentacion_codigo = "Tipo alimentación código",
      fecha_alimentacion_sangre = "Fecha alimentación sangre",
      generacion_filial_huevos = "Generación filial huevos",
      codigo_sustrato = "Código sustrato",
      fecha_colocacion_sustrato = "Fecha colocación sustrato",
      fecha_retiro_sustrato = "Fecha retiro sustrato",
      responsable_conteo_huevos = "Responsable conteo huevos"
    )

    missing <- required_fields[vapply(required_fields, function(field) {
      value <- record[[field]][[1]]
      is.na(value) || !nzchar(trimws(as.character(value)))
    }, logical(1))]

    unname(required_labels[missing])
  }

  f5_review_field_labels <- c(
    formulario_codigo = "Código del formulario",
    pais = "País",
    departamento_numero = "Departamento #",
    municipio_numero = "Municipio #",
    ciclo = "Ciclo",
    formulario_nombre = "Nombre del formulario",
    fecha_registro = "Fecha de registro",
    cepa_poblacion = "Cepa / población",
    especie = "Especie",
    generacion_filial_adultos = "Generación filial adultos",
    responsable_ingreso_jaula = "Responsable ingreso jaula",
    fecha_jaula = "Fecha jaula",
    numero_hembras = "Número de hembras",
    numero_machos = "Número de machos",
    total_huevos_viables = "Total huevos viables",
    responsable_alimentacion = "Responsable alimentación",
    tipo_alimentacion_codigo = "Tipo alimentación código",
    tipo_alimentacion_descripcion = "Tipo alimentación descripción",
    fecha_alimentacion_sangre = "Fecha alimentación sangre",
    numero_charolas = "Número de charolas",
    observaciones_alimentacion = "Observaciones alimentación",
    generacion_filial_huevos = "Generación filial huevos",
    codigo_sustrato = "Código sustrato",
    fecha_colocacion_sustrato = "Fecha colocación sustrato",
    fecha_retiro_sustrato = "Fecha retiro sustrato",
    numero_cuadro_sustrato = "Número cuadro sustrato",
    hv_huevos_viables = "HV - huevos viables",
    he_huevos_eclosionados = "HE - huevos eclosionados",
    hc_huevos_canoa = "HC - huevos canoa",
    hnf_huevos_no_fecundados = "HNF - huevos no fecundados",
    responsable_conteo_huevos = "Responsable conteo huevos",
    observaciones_generales = "Observaciones generales",
    fuente_formulario = "Fuente formulario",
    creado_por = "Creado por",
    creado_en = "Fecha creación"
  )

  f5_review_date_fields <- c(
    "fecha_registro",
    "fecha_jaula",
    "fecha_alimentacion_sangre",
    "fecha_colocacion_sustrato",
    "fecha_retiro_sustrato",
    "creado_en"
  )

  f5_review_integer_fields <- c(
    "departamento_numero",
    "municipio_numero",
    "numero_hembras",
    "numero_machos",
    "total_huevos_viables",
    "numero_charolas",
    "numero_cuadro_sustrato",
    "hv_huevos_viables",
    "he_huevos_eclosionados",
    "hc_huevos_canoa",
    "hnf_huevos_no_fecundados"
  )

  f5_review_multiline_fields <- c(
    "observaciones_alimentacion",
    "observaciones_generales"
  )

  f5_review_text_value <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return("")
    }

    trimws(as.character(value[[1]]))
  }

  review_datetime_gt_value <- function(value) {
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return("")
    }
    parsed <- suppressWarnings(as.POSIXct(value[[1]], tz = "UTC"))
    if (is.na(parsed)) {
      return(trimws(as.character(value[[1]])))
    }
    format(parsed, "%Y-%m-%d %H:%M", tz = "America/Guatemala")
  }

  f5_review_compare_value <- function(value, field) {
    if (is.null(value) || length(value) == 0 || is.na(value)) {
      return("")
    }

    if (field %in% f5_review_date_fields) {
      return(as.character(as.Date(value[[1]])))
    }

    if (field %in% f5_review_integer_fields) {
      number <- suppressWarnings(as.integer(as.numeric(value[[1]])))
      if (is.na(number)) {
        return("")
      }
      return(as.character(number))
    }

    trimws(as.character(value[[1]]))
  }

  f5_review_input_id <- function(field) {
    paste0("f5_review_redigit_", field)
  }

  f5_review_redigit_record <- function() {
    values <- lapply(names(f5_review_field_labels), function(field) {
      input[[f5_review_input_id(field)]]
    })
    names(values) <- names(f5_review_field_labels)
    values
  }

  f5_delete_review_record <- function(intake_id, reason, deleted_by) {
    reason <- f7_clean_text(reason)[[1]]
    deleted_by <- f7_clean_text(deleted_by)[[1]]
    if (is.na(reason)) stop("El comentario de eliminación es obligatorio.")
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    dbWithTransaction(connection, {
      selected_record <- dbGetQuery(
        connection,
        "select intake_id, formulario_codigo, cepa_poblacion, especie, review_status from public.formulario_5_alimentacion_conteo_intake where intake_id = $1 for update",
        params = list(as.integer(intake_id))
      )
      if (nrow(selected_record) != 1) stop("No se encontró el registro seleccionado para eliminar.")
      audit_exists <- dbGetQuery(
        connection,
        "select to_regclass('public.formulario_5_alimentacion_eliminacion_audit') is not null as exists"
      )$exists[[1]]
      if (isTRUE(audit_exists)) {
        dbExecute(
          connection,
          "insert into public.formulario_5_alimentacion_eliminacion_audit (intake_id, formulario_codigo, cepa_poblacion, especie, review_status, eliminado_por, motivo_eliminacion) values ($1, $2, $3, $4, $5, nullif($6, ''), $7)",
          params = list(
            as.integer(selected_record$intake_id[[1]]),
            as.character(selected_record$formulario_codigo[[1]]),
            as.character(selected_record$cepa_poblacion[[1]]),
            as.character(selected_record$especie[[1]]),
            as.character(selected_record$review_status[[1]]),
            value_or_default(deleted_by, ""),
            reason
          )
        )
      }
      deleted <- dbExecute(connection, "delete from public.formulario_5_alimentacion_conteo_intake where intake_id = $1", params = list(as.integer(intake_id)))
      if (deleted != 1L) stop("No se eliminó el registro seleccionado.")
      selected_record
    })
  }

  f5_review_discrepancies <- function(original, redigit) {
    rows <- lapply(names(f5_review_field_labels), function(field) {
      original_value <- f5_review_compare_value(original[[field]], field)
      redigit_value <- f5_review_compare_value(redigit[[field]], field)

      if (identical(original_value, redigit_value)) {
        return(NULL)
      }

      data.frame(
        Columna = field,
        Campo = unname(f5_review_field_labels[[field]]),
        Original = original_value,
        Redigitado = redigit_value,
        stringsAsFactors = FALSE
      )
    })

    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) {
      return(data.frame(
        Columna = character(),
        Campo = character(),
        Original = character(),
        Redigitado = character(),
        stringsAsFactors = FALSE
      ))
    }

    do.call(rbind, rows)
  }

  f5_fetch_review_record <- function(intake_id) {
    connection <- NULL
    on.exit({
      if (!is.null(connection)) {
        dbDisconnect(connection)
      }
    }, add = TRUE)

    connection <- connect_to_supabase()
    dbGetQuery(
      connection,
      "
        select *
        from public.formulario_5_alimentacion_conteo_intake
        where intake_id = $1
      ",
      params = list(as.integer(intake_id))
    )
  }

  f5_review_record_summary <- function(record) {
    if (is.null(record) || nrow(record) == 0) {
      return(data.frame())
    }

    data.frame(
      Campo = c(
        "Intake ID",
        "Estado",
        "Fecha registro",
        "País",
        "Cepa / población",
        "Especie",
        "Fuente formulario",
        "Actualizado en"
      ),
      Valor = c(
        as.character(record$intake_id[[1]]),
        f5_review_text_value(record$review_status),
        as.character(as.Date(record$fecha_registro[[1]])),
        f5_review_text_value(record$pais),
        f5_review_text_value(record$cepa_poblacion),
        f5_review_text_value(record$especie),
        f5_review_text_value(record$fuente_formulario),
        as.character(record$actualizado_en[[1]])
      ),
      stringsAsFactors = FALSE
    )
  }

  f5_total_huevos_ingresados <- function() {
    sum(
      f5_number(input$f5_hv_huevos_viables, 0),
      f5_number(input$f5_he_huevos_eclosionados, 0),
      f5_number(input$f5_hc_huevos_canoa, 0),
      f5_number(input$f5_hnf_huevos_no_fecundados, 0),
      na.rm = TRUE
    )
  }

  f5_capture_summary <- function() {
    data.frame(
      Campo = c(
        "Código del formulario",
        "País",
        "Departamento #",
        "Municipio #",
        "Ciclo",
        "Cepa / población",
        "Especie",
        "Fecha jaula",
        "Número de hembras",
        "Número de machos",
        "Total individuos",
        "Total huevos viables",
        "Tipo alimentación",
        "Fecha alimentación sangre",
        "Fecha colocación sustrato",
        "Fecha retiro sustrato",
        "HV - huevos viables",
        "HE - huevos eclosionados",
        "HC - huevos canoa",
        "HNF - huevos no fecundados",
        "Total huevos ingresados",
        "Creado por"
      ),
      Valor = c(
        f5_text(input$f5_formulario_codigo),
        f5_text(input$f5_pais),
        as.character(f5_number(input$f5_departamento_numero)),
        as.character(f5_number(input$f5_municipio_numero)),
        f5_text(input$f5_ciclo),
        f5_text(input$f5_cepa_poblacion),
        f5_text(input$f5_especie),
        as.character(f5_date(input$f5_fecha_jaula)),
        as.character(f5_number(input$f5_numero_hembras, 0)),
        as.character(f5_number(input$f5_numero_machos, 0)),
        as.character(f5_number(input$f5_numero_hembras, 0) + f5_number(input$f5_numero_machos, 0)),
        as.character(f5_number(input$f5_total_huevos_viables)),
        f5_text(input$f5_tipo_alimentacion_descripcion),
        as.character(f5_date(input$f5_fecha_alimentacion_sangre)),
        as.character(f5_date(input$f5_fecha_colocacion_sustrato)),
        as.character(f5_date(input$f5_fecha_retiro_sustrato)),
        as.character(f5_number(input$f5_hv_huevos_viables, 0)),
        as.character(f5_number(input$f5_he_huevos_eclosionados, 0)),
        as.character(f5_number(input$f5_hc_huevos_canoa, 0)),
        as.character(f5_number(input$f5_hnf_huevos_no_fecundados, 0)),
        as.character(f5_total_huevos_ingresados()),
        f5_text(input$f5_creado_por)
      ),
      stringsAsFactors = FALSE
    )
  }

  f5_quality_checks <- function() {
    alerts <- character()
    total_declarado <- f5_number(input$f5_total_huevos_viables)
    total_ingresado <- f5_total_huevos_ingresados()
    fecha_jaula <- f5_date(input$f5_fecha_jaula)
    fecha_alimentacion <- f5_date(input$f5_fecha_alimentacion_sangre)
    fecha_colocacion <- f5_date(input$f5_fecha_colocacion_sustrato)
    fecha_retiro <- f5_date(input$f5_fecha_retiro_sustrato)

    if (!is.na(total_declarado) && total_declarado < total_ingresado) {
      alerts <- c(
        alerts,
        sprintf(
          "Total huevos viables (%s) no puede ser menor que la suma de huevos ingresados (%s).",
          total_declarado,
          total_ingresado
        )
      )
    }

    if (!is.na(fecha_jaula) && !is.na(fecha_alimentacion) && fecha_jaula > fecha_alimentacion) {
      alerts <- c(
        alerts,
        "Fecha jaula no puede ser posterior a Fecha alimentación sangre."
      )
    }

    if (!is.na(fecha_jaula) && !is.na(fecha_colocacion) && fecha_colocacion <= fecha_jaula) {
      alerts <- c(
        alerts,
        "Fecha colocación sustrato debe ser posterior a Fecha jaula."
      )
    }

    if (!is.na(fecha_alimentacion) && !is.na(fecha_colocacion)) {
      dias_alimentacion_sustrato <- as.integer(fecha_colocacion - fecha_alimentacion)
      if (dias_alimentacion_sustrato < 2) {
        alerts <- c(
          alerts,
          sprintf(
            "Entre Fecha alimentación sangre y Fecha colocación sustrato debe haber mínimo 2 días; actualmente hay %s día(s).",
            dias_alimentacion_sustrato
          )
        )
      }
    }

    if (!is.na(fecha_colocacion) && !is.na(fecha_retiro)) {
      dias_sustrato <- as.integer(fecha_retiro - fecha_colocacion)
      if (dias_sustrato < 3) {
        alerts <- c(
          alerts,
          sprintf(
            "Entre Fecha colocación sustrato y Fecha retiro sustrato debe haber mínimo 3 días; actualmente hay %s día(s).",
            dias_sustrato
          )
        )
      }
    }

    alerts
  }

  output$f5_capture_step_indicator <- renderUI({
    current_step <- f5_capture_step()
    current_index <- match(current_step, f5_capture_steps)

    div(
      class = "alert alert-info",
      sprintf(
        "Sección %s de %s: %s",
        current_index,
        length(f5_capture_steps),
        f5_capture_step_labels[[current_step]]
      )
    )
  })

  output$f7_print_codigo_bioensayo_preview <- renderUI({
    codigo <- f7_print_codigo_bioensayo_code()
    if (is.na(codigo)) {
      return(div(
        class = "alert alert-warning",
        strong("Código Bioensayo: "),
        "Complete país, año, municipio, población, insecticida, correlativo, generación filial y tipo de bioensayo para generar el código."
      ))
    }
    div(
      class = "summary-box",
      strong("Código Bioensayo generado: "),
      tags$code(codigo),
      tags$br(),
      tags$small("Estructura: REI + año + país + departamento/municipio + P# + sinergista si aplica + insecticida + correlativo + F#.")
    )
  })

  output$f7_print_codigo_bioensayo_municipio_ui <- renderUI({
    country <- value_or_default(input$f7_print_pais, "")
    department_code <- value_or_default(input$f7_print_codigo_bioensayo_departamento, "")
    if (!nzchar(country) || !nzchar(department_code)) {
      return(selectInput("f7_print_codigo_bioensayo_municipio", "Municipio", choices = c("Seleccione departamento" = "")))
    }
    tagList(
      selectInput(
        "f7_print_codigo_bioensayo_municipio",
        "Municipio",
        choices = f7_print_municipality_choices(country, department_code)
      ),
      conditionalPanel(
        "input.f7_print_codigo_bioensayo_municipio == '__manual__'",
        textInput("f7_print_codigo_bioensayo_municipio_manual", "Código nacional de municipio", placeholder = "Ej. 2201 o 0210")
      )
    )
  })

  output$f7_codigo_municipio_ui <- renderUI({
    country <- value_or_default(input$f7_pais, "")
    department_code <- value_or_default(input$f7_codigo_departamento, "")
    if (!nzchar(country) || !nzchar(department_code)) {
      return(selectInput("f7_codigo_municipio", "Municipio *", choices = c("Seleccione departamento" = "")))
    }
    tagList(
      selectInput(
        "f7_codigo_municipio",
        "Municipio *",
        choices = f7_print_municipality_choices(country, department_code)
      ),
      conditionalPanel(
        "input.f7_codigo_municipio == '__manual__'",
        textInput("f7_codigo_municipio_manual", "Código nacional de municipio *", placeholder = "Ej. 2201 o 0210")
      )
    )
  })

  observeEvent(input$f7_print_pais, {
    updateSelectInput(
      session,
      "f7_print_codigo_bioensayo_departamento",
      choices = c("Seleccione" = "", f7_print_department_choices(input$f7_print_pais)),
      selected = ""
    )
  }, ignoreInit = FALSE)

  observeEvent(input$f7_pais, {
    updateSelectInput(
      session,
      "f7_codigo_departamento",
      choices = c("Seleccione" = "", f7_print_department_choices(input$f7_pais)),
      selected = ""
    )
  }, ignoreInit = FALSE)

  output$f5_capture_step_area <- renderUI({
    step <- f5_capture_step()

    if (identical(step, "metadatos")) {
      return(fluidRow(
        column(
          width = 8,
          wellPanel(
            h4("Metadatos"),
            textInput("f5_formulario_codigo", "Código del formulario", value = "F5"),
            selectInput("f5_pais", "País", choices = c("El Salvador", "Guatemala"), selected = "El Salvador"),
            numericInput("f5_departamento_numero", "Departamento #", value = NA, min = 0, step = 1),
            numericInput("f5_municipio_numero", "Municipio #", value = NA, min = 0, step = 1),
            textInput("f5_ciclo", "Ciclo", value = "", placeholder = "Ingrese ciclo"),
            textInput("f5_formulario_nombre", "Nombre del formulario", value = "Alimentación sanguínea y conteo huevecillos Aedes spp."),
            dateInput("f5_fecha_registro", "Fecha de registro", value = Sys.Date())
          )
        )
      ))
    }

    if (identical(step, "datos_generales")) {
      return(fluidRow(
        column(
          width = 8,
          wellPanel(
            h4("Datos generales"),
            textInput("f5_cepa_poblacion", "Cepa / población"),
            selectInput("f5_especie", "Especie", choices = c("Ae. aegypti", "Ae. albopictus")),
            textInput("f5_generacion_filial_adultos", "Generación filial adultos"),
            textInput("f5_responsable_ingreso_jaula", "Responsable ingreso jaula"),
            dateInput("f5_fecha_jaula", "Fecha jaula", value = Sys.Date()),
            uiOutput("f5_fecha_jaula_alert"),
            numericInput("f5_numero_hembras", "Número de hembras", value = 0, min = 0, step = 1),
            numericInput("f5_numero_machos", "Número de machos", value = 0, min = 0, step = 1),
            div(class = "summary-box", strong("Total individuos: "), textOutput("f5_total_individuos", inline = TRUE)),
            numericInput("f5_total_huevos_viables", "Total huevos viables", value = NA, min = 0, step = 1)
          )
        )
      ))
    }

    if (identical(step, "alimentacion")) {
      return(fluidRow(
        column(
          width = 8,
          wellPanel(
            h4("Alimentación sanguínea"),
            textInput("f5_responsable_alimentacion", "Responsable alimentación"),
            selectInput("f5_tipo_alimentacion_codigo", "Tipo alimentación código", choices = c("A", "B", "C", "D", "E")),
            selectInput(
              "f5_tipo_alimentacion_descripcion",
              "Tipo alimentación descripción",
              choices = c(
                "Sin dato" = "",
                "conejo",
                "humano",
                "hemotek-conejo",
                "hemotek-humano",
                "hemotek-carnero"
              )
            ),
            dateInput("f5_fecha_alimentacion_sangre", "Fecha alimentación sangre", value = Sys.Date()),
            uiOutput("f5_fecha_alimentacion_alert"),
            numericInput("f5_numero_charolas", "Número de charolas", value = 0, min = 0, step = 1),
            textAreaInput("f5_observaciones_alimentacion", "Observaciones alimentación", rows = 4)
          )
        )
      ))
    }

    if (identical(step, "conteo_huevecillos")) {
      return(fluidRow(
        column(
          width = 8,
          wellPanel(
            h4("Conteo de huevecillos"),
            textInput("f5_generacion_filial_huevos", "Generación filial huevos"),
            textInput("f5_codigo_sustrato", "Código sustrato"),
            dateInput("f5_fecha_colocacion_sustrato", "Fecha colocación sustrato", value = Sys.Date()),
            dateInput("f5_fecha_retiro_sustrato", "Fecha retiro sustrato", value = Sys.Date()),
            uiOutput("f5_fecha_sustrato_alert"),
            numericInput("f5_numero_cuadro_sustrato", "Número cuadro sustrato", value = 0, min = 0, step = 1),
            numericInput("f5_hv_huevos_viables", "HV - huevos viables", value = 0, min = 0, step = 1),
            numericInput("f5_he_huevos_eclosionados", "HE - huevos eclosionados", value = 0, min = 0, step = 1),
            numericInput("f5_hc_huevos_canoa", "HC - huevos canoa", value = 0, min = 0, step = 1),
            numericInput("f5_hnf_huevos_no_fecundados", "HNF - huevos no fecundados", value = 0, min = 0, step = 1),
            div(class = "summary-box", strong("Total huevos: "), textOutput("f5_total_huevos", inline = TRUE)),
            textInput("f5_responsable_conteo_huevos", "Responsable conteo huevos")
          )
        )
      ))
    }

    fluidRow(
      column(
        width = 8,
        wellPanel(
          h4("Observaciones y auditoría"),
          textAreaInput("f5_observaciones_generales", "Observaciones generales", rows = 4),
          textInput("f5_fuente_formulario", "Fuente formulario"),
          textInput("f5_creado_por", "Creado por"),
          dateInput("f5_creado_en", "Fecha creación", value = Sys.Date())
        )
      )
    )
  })

  output$f5_save_button_area <- renderUI({
    if (!identical(f5_capture_step(), tail(f5_capture_steps, 1))) {
      return(NULL)
    }

    save_status <- f5_save_status()
    is_saving <- identical(save_status$type, "saving")

    certification_status <- if (f5_certification_complete()) {
      div(
        class = "alert alert-success",
        "Certificación de datos aprobada. El registro está listo para guardarse en Supabase."
      )
    } else {
      div(
        class = "alert alert-warning",
        "Antes de guardar el registro pendiente debe completar la certificación de datos."
      )
    }

    tagList(
      certification_status,
      uiOutput("f5_save_status"),
      div(
        class = "submit-row",
        actionButton("f5_open_certification", "Certificación de datos", class = "btn-primary"),
        if (f5_certification_complete() && !is_saving) {
          actionButton(
            "f5_save_pending",
            "Guardar registro pendiente",
            class = "btn-primary"
          )
        } else {
          actionButton(
            "f5_save_pending",
            if (is_saving) "Guardando..." else "Guardar registro pendiente",
            class = "btn-default",
            disabled = "disabled"
          )
        }
      )
    )
  })

  output$f5_save_status <- renderUI({
    status <- f5_save_status()

    if (is.null(status$type) || identical(status$type, "idle")) {
      return(NULL)
    }

    alert_class <- switch(
      status$type,
      saving = "alert alert-info",
      success = "alert alert-success",
      error = "alert alert-danger",
      warning = "alert alert-warning",
      "alert alert-info"
    )

    details <- status$details
    if (is.null(details)) {
      details <- character()
    }

    tagList(
      div(
        class = alert_class,
        strong(status$message),
        if (identical(status$type, "saving")) {
          div(
            class = "progress",
            style = "margin-top: 10px; margin-bottom: 0;",
            div(
              class = "progress-bar progress-bar-striped active",
              role = "progressbar",
              style = "width: 100%;",
              "Subiendo a Supabase"
            )
          )
        },
        if (length(details) > 0) {
          tags$ul(lapply(details, tags$li))
        }
      )
    )
  })

  output$f5_fecha_jaula_alert <- renderUI({
    req(input$f5_fecha_jaula)
    fecha_registro <- input$f5_fecha_registro
    if (is.null(fecha_registro) || is.na(fecha_registro)) {
      fecha_registro <- Sys.Date()
    }

    if (identical(as.Date(input$f5_fecha_jaula), as.Date(fecha_registro))) {
      return(div(
        class = "alert alert-warning",
        "Revise la fecha: Fecha jaula es el mismo día que la fecha de registro."
      ))
    }

    NULL
  })

  output$f5_fecha_alimentacion_alert <- renderUI({
    req(input$f5_fecha_alimentacion_sangre)
    fecha_registro <- input$f5_fecha_registro
    if (is.null(fecha_registro) || is.na(fecha_registro)) {
      fecha_registro <- Sys.Date()
    }

    fecha_jaula <- input$f5_fecha_jaula
    if (!is.null(fecha_jaula) && !is.na(fecha_jaula) && as.Date(fecha_jaula) > as.Date(input$f5_fecha_alimentacion_sangre)) {
      return(div(
        class = "alert alert-danger",
        "Fecha jaula no puede ser posterior a Fecha alimentación sangre."
      ))
    }

    if (identical(as.Date(input$f5_fecha_alimentacion_sangre), as.Date(fecha_registro))) {
      return(div(
        class = "alert alert-warning",
        "Revise la fecha: Fecha alimentación sangre es el mismo día que la fecha de registro."
      ))
    }

    NULL
  })

  output$f5_fecha_sustrato_alert <- renderUI({
    req(input$f5_fecha_colocacion_sustrato)

    alerts <- list()
    fecha_jaula <- input$f5_fecha_jaula
    fecha_alimentacion <- input$f5_fecha_alimentacion_sangre
    fecha_colocacion <- as.Date(input$f5_fecha_colocacion_sustrato)
    fecha_retiro <- input$f5_fecha_retiro_sustrato

    if (!is.null(fecha_jaula) && !is.na(fecha_jaula) && fecha_colocacion <= as.Date(fecha_jaula)) {
      alerts <- append(alerts, list(div(
        class = "alert alert-danger",
        "Fecha colocación sustrato debe ser posterior a Fecha jaula."
      )))
    }

    if (!is.null(fecha_alimentacion) && !is.na(fecha_alimentacion)) {
      dias_alimentacion_sustrato <- as.integer(fecha_colocacion - as.Date(fecha_alimentacion))
      if (dias_alimentacion_sustrato < 2) {
        alerts <- append(alerts, list(div(
          class = "alert alert-warning",
          sprintf(
            "Entre Fecha alimentación sangre y Fecha colocación sustrato debe haber mínimo 2 días; actualmente hay %s día(s).",
            dias_alimentacion_sustrato
          )
        )))
      }
    }

    if (!is.null(fecha_retiro) && !is.na(fecha_retiro)) {
      dias_sustrato <- as.integer(as.Date(fecha_retiro) - fecha_colocacion)
      if (dias_sustrato <= 0) {
        alerts <- append(alerts, list(div(
          class = "alert alert-danger",
          "Fecha retiro sustrato debe ser posterior a Fecha colocación sustrato."
        )))
      } else if (dias_sustrato < 3) {
        alerts <- append(alerts, list(div(
          class = "alert alert-warning",
          sprintf(
            "Entre Fecha colocación sustrato y Fecha retiro sustrato debe haber mínimo 3 días; actualmente hay %s día(s).",
            dias_sustrato
          )
        )))
      }
    }

    if (length(alerts) == 0) {
      return(NULL)
    }

    do.call(tagList, alerts)
  })

  output$f5_capture_navigation <- renderUI({
    current_index <- match(f5_capture_step(), f5_capture_steps)
    tagList(
      if (current_index > 1) {
        actionButton("f5_previous_step", "Atrás")
      },
      if (current_index < length(f5_capture_steps)) {
        actionButton("f5_next_step", "Seguir", class = "btn-primary")
      }
    )
  })

  observeEvent(input$f5_capture_tab, {
    if (input$f5_capture_tab %in% f5_capture_steps) {
      f5_capture_step(input$f5_capture_tab)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$f5_previous_step, {
    current_index <- match(f5_capture_step(), f5_capture_steps)
    if (!is.na(current_index) && current_index > 1) {
      previous_step <- f5_capture_steps[[current_index - 1]]
      f5_capture_step(previous_step)
      updateTabsetPanel(session, "f5_capture_tab", selected = previous_step)
    }
  })

  observeEvent(input$f5_next_step, {
    current_index <- match(f5_capture_step(), f5_capture_steps)
    if (!is.na(current_index) && current_index < length(f5_capture_steps)) {
      next_step <- f5_capture_steps[[current_index + 1]]
      f5_capture_step(next_step)
      updateTabsetPanel(session, "f5_capture_tab", selected = next_step)
    }
  })

  observeEvent(input$f5_open_certification, {
    f5_certification_alerts(character())
    f5_certification_panel("summary")
  })

  output$f5_certification_summary <- renderTable({
    f5_capture_summary()
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$f5_certification_overlay <- renderUI({
    panel <- f5_certification_panel()

    if (identical(panel, "closed")) {
      return(NULL)
    }

    if (identical(panel, "summary")) {
      return(div(
        class = "f5-certification-backdrop",
        div(
          class = "f5-certification-dialog",
          h4("Certificación de datos"),
          p("Revise el resumen del registro antes de ejecutar el control de calidad."),
          tableOutput("f5_certification_summary"),
          div(
            class = "f5-certification-actions",
            actionButton("f5_close_certification", "Cancelar"),
            actionButton("f5_confirm_certification", "OK", class = "btn-primary")
          )
        )
      ))
    }

    div(
      class = "f5-certification-backdrop",
      div(
        class = "f5-certification-dialog f5-certification-dialog-small",
        h4("Control de calidad"),
        div(
          class = "alert alert-danger",
          strong("Revise el registro antes de guardarlo.")
        ),
        tags$ul(lapply(f5_certification_alerts(), tags$li)),
        div(
          class = "f5-certification-actions",
          actionButton("f5_close_certification", "Cerrar", class = "btn-primary")
        )
      )
    )
  })

  observeEvent(input$f5_confirm_certification, {
    quality_alerts <- f5_quality_checks()
    f5_certification_complete(length(quality_alerts) == 0)

    if (length(quality_alerts) > 0) {
      f5_certification_alerts(quality_alerts)
      f5_certification_panel("errors")
      return()
    }

    f5_certification_panel("closed")
    showNotification("Certificación aprobada. Ya puede guardar el registro pendiente.", type = "message")
  })

  observeEvent(input$f5_close_certification, {
    f5_certification_panel("closed")
  })

  observeEvent(input$f5_save_pending, {
    if (!f5_certification_complete()) {
      f5_save_status(list(
        type = "warning",
        message = "Complete la certificación de datos antes de guardar el registro.",
        details = character()
      ))
      showNotification(
        "Complete la certificación de datos antes de guardar el registro.",
        type = "warning"
      )
      return()
    }

    quality_alerts <- f5_quality_checks()
    if (length(quality_alerts) > 0) {
      f5_certification_complete(FALSE)
      f5_certification_alerts(quality_alerts)
      f5_certification_panel("errors")
      f5_save_status(list(
        type = "error",
        message = "El registro cambió después de certificarse. Revise el control de calidad.",
        details = quality_alerts
      ))
      showNotification(
        "El registro cambió después de certificarse. Revise el control de calidad.",
        type = "error"
      )
      return()
    }

    record <- f5_formulario_5_record()
    missing_fields <- f5_required_missing_fields(record)
    if (length(missing_fields) > 0) {
      f5_save_status(list(
        type = "error",
        message = "Complete los campos obligatorios antes de guardar.",
        details = missing_fields
      ))
      showNotification(
        paste(
          "Complete los campos obligatorios:",
          paste(missing_fields, collapse = ", ")
        ),
        type = "error",
        duration = 10
      )
      return()
    }

    f5_save_status(list(
      type = "saving",
      message = "Subiendo registro pendiente a Supabase...",
      details = character()
    ))

    connection <- NULL
    tryCatch({
      inserted <- withProgress(message = "Subiendo registro a Supabase", value = 0, {
        incProgress(0.2, detail = "Abriendo conexión")
        connection <- connect_to_supabase()

        incProgress(0.5, detail = "Guardando registro")
        dbWithTransaction(connection, {
          dbAppendTable(
            connection,
            Id(schema = "public", table = "formulario_5_alimentacion_conteo_intake"),
            record
          )
        })

        incProgress(0.2, detail = "Confirmando ID")
        inserted <- dbGetQuery(
          connection,
          "
            select currval(
              pg_get_serial_sequence(
                'public.formulario_5_alimentacion_conteo_intake',
                'intake_id'
              )
            )::text as intake_id
          "
        )

        incProgress(0.1, detail = "Listo")
        inserted
      })

      submission_status(sprintf(
        "Formulario 5 guardado en Supabase como registro pendiente. Intake ID: %s.",
        inserted$intake_id[[1]]
      ))
      f5_save_status(list(
        type = "success",
        message = sprintf("Formulario 5 guardado en Supabase. Intake ID: %s.", inserted$intake_id[[1]]),
        details = "El registro quedó como pending para revisión."
      ))
      showNotification(
        sprintf("Formulario 5 guardado. Intake ID: %s.", inserted$intake_id[[1]]),
        type = "message"
      )
      f5_certification_complete(FALSE)
    }, error = function(error) {
      f5_save_status(list(
        type = "error",
        message = "No se pudo guardar en Supabase.",
        details = conditionMessage(error)
      ))
      showNotification(
        paste("No se pudo guardar en Supabase:", conditionMessage(error)),
        type = "error",
        duration = 12
      )
    }, finally = {
      if (!is.null(connection)) {
        dbDisconnect(connection)
      }
    })
  })

  f5_load_review_records <- function(random_sample = FALSE) {
    start_date <- as.Date(input$f5_review_start_date)
    end_date <- as.Date(input$f5_review_end_date)
    status_filter <- f5_text(input$f5_review_filter_status)

    if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
      stop("Seleccione un rango de fechas válido.")
    }

    if (!nzchar(status_filter)) {
      status_filter <- "pending"
    }
    exclude_submitter <- f7_clean_text(input$f5_review_exclude_submitter)[[1]]
    where_clause <- "
      where fecha_registro between $1 and $2
        and ($3 = 'all' or review_status = $3)
    "
    params <- list(as.character(start_date), as.character(end_date), status_filter)
    if (!is.na(exclude_submitter)) {
      where_clause <- paste0(where_clause, " and lower(coalesce(nullif(trim(creado_por), ''), 'Sin nombre')) <> lower($4)")
      params <- c(params, list(exclude_submitter))
    }

    connection <- NULL
    on.exit({
      if (!is.null(connection)) {
        dbDisconnect(connection)
      }
    }, add = TRUE)

    connection <- connect_to_supabase()
    count_query <- paste(
      "
      select count(*)::integer as total
      from public.formulario_5_alimentacion_conteo_intake
      ",
      where_clause
    )
    total <- dbGetQuery(
      connection,
      count_query,
      params = params
    )$total[[1]]

    if (total == 0) {
      return(data.frame())
    }

    limit <- if (random_sample) {
      max(1L, ceiling(total * 0.10))
    } else {
      min(total, 50L)
    }

    order_clause <- if (random_sample) "order by random()" else "order by creado_en desc nulls last, intake_id desc"
    query <- paste(
      "
        select
          intake_id,
          review_status,
          fecha_registro,
          pais,
          cepa_poblacion,
          especie,
          fuente_formulario,
          creado_por,
          actualizado_en
        from public.formulario_5_alimentacion_conteo_intake
      ",
      where_clause,
      order_clause,
      paste0("limit $", length(params) + 1L)
    )

    dbGetQuery(
      connection,
      query,
      params = c(params, list(as.integer(limit)))
    )
  }

  output$f5_review_status_message <- renderUI({
    status <- f5_review_status()

    if (is.null(status$type) || identical(status$type, "idle")) {
      return(NULL)
    }
    selected <- f5_review_selected()
    detail_messages <- c(
      "Abra primero un intake_id para revisar.",
      "La redigitación coincide",
      "Se encontraron discrepancias",
      "No se pudo guardar la revisión.",
      "Revisión guardada para intake_id",
      "No se puede eliminar el registro sin comentario.",
      "No se pudo eliminar el registro.",
      "Registro %s eliminado definitivamente."
    )
    if (!is.null(selected) && nrow(selected) > 0 && any(startsWith(status$message, detail_messages))) {
      return(NULL)
    }

    alert_class <- switch(
      status$type,
      success = "alert alert-success",
      error = "alert alert-danger",
      warning = "alert alert-warning",
      loading = "alert alert-info",
      info = "alert alert-info",
      "alert alert-info"
    )

    details <- status$details
    if (is.null(details)) {
      details <- character()
    }

    div(
      class = alert_class,
      strong(status$message),
      if (identical(status$type, "loading")) {
        div(
          class = "progress",
          style = "margin-top: 10px; margin-bottom: 0;",
          div(
            class = "progress-bar progress-bar-striped active",
            role = "progressbar",
            style = "width: 100%;",
            "Consultando Supabase"
          )
        )
      },
      if (length(details) > 0) {
        tags$ul(lapply(details, tags$li))
      }
    )
  })

  output$f5_review_detail_status_message <- renderUI({
    selected <- f5_review_selected()
    if (is.null(selected) || nrow(selected) == 0) return(NULL)
    status <- f5_review_status()
    if (is.null(status$type) || identical(status$type, "idle")) return(NULL)
    alert_class <- switch(
      status$type,
      success = "alert alert-success",
      error = "alert alert-danger",
      warning = "alert alert-warning",
      loading = "alert alert-info",
      info = "alert alert-info",
      "alert alert-info"
    )
    details <- status$details
    if (is.null(details)) details <- character()
    div(
      class = alert_class,
      strong(status$message),
      if (identical(status$type, "loading")) {
        div(
          class = "progress",
          style = "margin-top: 10px; margin-bottom: 0;",
          div(class = "progress-bar progress-bar-striped active", role = "progressbar", style = "width: 100%;", "Consultando Supabase")
        )
      },
      if (length(details) > 0) tags$ul(lapply(details, tags$li))
    )
  })

  output$f5_review_sample_list <- renderUI({
    records <- f5_review_records()

    if (is.null(records) || nrow(records) == 0) {
      return(NULL)
    }

    table_rows <- lapply(seq_len(nrow(records)), function(index) {
      record <- records[index, ]
      intake_id <- record$intake_id[[1]]
      tags$tr(
        tags$td(tags$a(
          href = "#",
          onclick = sprintf(
            "Shiny.setInputValue('f5_review_select_intake_id', %s, {priority: 'event'}); return false;",
            intake_id
          ),
          as.character(intake_id)
        )),
        tags$td(record$review_status),
        tags$td(as.character(record$fecha_registro)),
        tags$td(record$pais),
        tags$td(record$cepa_poblacion),
        tags$td(record$especie),
        tags$td(record$fuente_formulario),
        tags$td(f5_review_text_value(record$creado_por))
      )
    })

    tagList(
      h4("Listado para revisión"),
      tags$table(
        class = "table table-striped table-condensed",
        tags$thead(tags$tr(
          tags$th("intake_id"),
          tags$th("Estado"),
          tags$th("Fecha registro"),
          tags$th("País"),
          tags$th("Cepa / población"),
          tags$th("Especie"),
          tags$th("Fuente formulario"),
          tags$th("Ingresado por")
        )),
        tags$tbody(table_rows)
      )
    )
  })

  output$f5_review_selected_record <- renderUI({
    record <- f5_review_selected()
    if (is.null(record) || nrow(record) == 0) {
      return(NULL)
    }

    tagList(
      wellPanel(
        h4(sprintf("Formulario original seleccionado: intake_id %s", record$intake_id[[1]])),
        tableOutput("f5_review_original_summary")
      )
    )
  })

  output$f5_review_original_summary <- renderTable({
    f5_review_record_summary(f5_review_selected())
  }, striped = TRUE, bordered = TRUE, spacing = "xs", sanitize.text.function = identity)

  output$f5_review_redigit_form <- renderUI({
    record <- f5_review_selected()
    if (is.null(record) || nrow(record) == 0) {
      return(NULL)
    }

    input_for_field <- function(field) {
      label <- unname(f5_review_field_labels[[field]])
      input_id <- f5_review_input_id(field)

      if (identical(field, "pais")) {
        return(selectInput(input_id, label, choices = c("", "El Salvador", "Guatemala"), selected = ""))
      }

      if (identical(field, "especie")) {
        return(selectInput(input_id, label, choices = c("", "Ae. aegypti", "Ae. albopictus"), selected = ""))
      }

      if (identical(field, "tipo_alimentacion_codigo")) {
        return(selectInput(input_id, label, choices = c("", "A", "B", "C", "D", "E"), selected = ""))
      }

      if (identical(field, "tipo_alimentacion_descripcion")) {
        return(selectInput(
          input_id,
          label,
          choices = c("", "conejo", "humano", "hemotek-conejo", "hemotek-humano", "hemotek-carnero"),
          selected = ""
        ))
      }

      if (field %in% f5_review_date_fields) {
        return(dateInput(input_id, label, value = NA))
      }

      if (field %in% f5_review_integer_fields) {
        return(numericInput(input_id, label, value = NA, min = 0, step = 1))
      }

      if (field %in% f5_review_multiline_fields) {
        return(textAreaInput(input_id, label, rows = 3))
      }

      textInput(input_id, label)
    }

    fields <- names(f5_review_field_labels)
    split_fields <- split(fields, ceiling(seq_along(fields) / 12))

    tagList(
      wellPanel(
        h4("Redigitación para control de calidad"),
        uiOutput("f5_review_detail_status_message"),
        p("Digite nuevamente los valores del formulario físico. Luego compare contra la captura original."),
        fluidRow(lapply(split_fields, function(field_group) {
          column(4, lapply(field_group, input_for_field))
        })),
        tags$hr(),
        fluidRow(
          column(
            4,
            selectInput(
              "f5_review_final_status",
              "Resultado de revisión",
              choices = c("Revisado" = "reviewed", "Rechazado" = "rejected", "Pendiente" = "pending"),
              selected = "reviewed"
            )
          ),
          column(4, textInput("f5_reviewed_by", "Revisado por", value = user_profile$name)),
          column(4, dateInput("f5_reviewed_at", "Fecha de revisión", value = Sys.Date()))
        ),
        textAreaInput("f5_review_notes", "Notas de revisión", rows = 4),
        div(
          class = "submit-row",
          actionButton("f5_review_compare", "Comparar redigitación", class = "btn-primary"),
          actionButton("f5_review_save", "Guardar revisión", class = "btn-default")
        ),
        tags$hr(),
        div(
          class = "f5-review-delete-zone",
          if (!isTRUE(f5_review_delete_mode())) {
            actionButton("f5_review_request_delete", "Eliminar registro", class = "btn-danger")
          } else {
            tagList(
              div(
                class = "alert alert-danger",
                tags$strong("¿Está seguro que desea eliminar este registro?"),
                tags$p("No hay vuelta atrás. Luego de su eliminación, este registro será borrado de la base de datos.")
              ),
              textAreaInput(
                "f5_review_delete_reason",
                "Comentario obligatorio: indique por qué se elimina este registro",
                value = "",
                rows = 3
              ),
              div(
                class = "submit-row",
                actionButton("f5_review_delete_confirm", "Sí, eliminar definitivamente", class = "btn-danger"),
                actionButton("f5_review_delete_cancel", "Cancelar eliminación", class = "btn-default")
              )
            )
          }
        )
      )
    )
  })

  output$f5_review_comparison_result <- renderUI({
    comparison <- f5_review_comparison()

    if (is.null(comparison)) {
      return(NULL)
    }

    if (nrow(comparison) == 0) {
      return(div(
        class = "alert alert-success",
        strong("Redigitación coincide con el registro original."),
        " Puede guardar la revisión."
      ))
    }

    tagList(
      div(
        class = "alert alert-danger",
        strong("Se encontraron discrepancias."),
        " Revise estas columnas contra el formulario físico antes de guardar la revisión."
      ),
      tableOutput("f5_review_discrepancy_table")
    )
  })

  output$f5_review_discrepancy_table <- renderTable({
    f5_review_comparison()
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  observeEvent(input$f5_review_find_id, {
    intake_id <- f5_integer(input$f5_review_search_id)
    if (is.na(intake_id)) {
      f5_review_status(list(
        type = "warning",
        message = "Ingrese un intake_id válido.",
        details = character()
      ))
      return()
    }

    tryCatch({
      record <- f5_fetch_review_record(intake_id)
      if (nrow(record) == 0) {
        f5_review_selected(NULL)
        f5_review_comparison(NULL)
        f5_review_delete_mode(FALSE)
        f5_review_status(list(
          type = "warning",
          message = sprintf("No se encontró intake_id %s.", intake_id),
          details = character()
        ))
        return()
      }

      f5_review_records(record[, intersect(c(
        "intake_id",
        "review_status",
        "fecha_registro",
        "pais",
        "cepa_poblacion",
        "especie",
        "fuente_formulario",
        "actualizado_en"
      ), names(record)), drop = FALSE])
      f5_review_selected(record)
      f5_review_comparison(NULL)
      f5_review_delete_mode(FALSE)
      f5_review_status(list(
        type = "success",
        message = sprintf("Registro intake_id %s cargado para revisión.", intake_id),
        details = character()
      ))
    }, error = function(error) {
      f5_review_status(list(
        type = "error",
        message = "No se pudo buscar el registro.",
        details = conditionMessage(error)
      ))
    })
  })

  observeEvent(input$f5_review_generate_sample, {
    f5_review_status(list(
      type = "loading",
      message = "Generando muestra aleatoria del 10%...",
      details = character()
    ))
    f5_review_records(data.frame())
    f5_review_selected(NULL)
    f5_review_comparison(NULL)
    f5_review_delete_mode(FALSE)

    tryCatch({
      records <- withProgress(message = "Generando muestra 10%", value = 0, {
        incProgress(0.25, detail = "Validando rango de fechas")
        Sys.sleep(0.1)
        incProgress(0.35, detail = "Consultando registros elegibles")
        records <- f5_load_review_records(random_sample = TRUE)
        incProgress(0.3, detail = "Preparando listado")
        records
      })
      f5_review_records(records)
      f5_review_selected(NULL)
      f5_review_comparison(NULL)
      f5_review_delete_mode(FALSE)

      if (nrow(records) == 0) {
        f5_review_status(list(
          type = "warning",
          message = "No hay registros en el rango seleccionado.",
          details = character()
        ))
      } else {
        f5_review_status(list(
          type = "success",
          message = sprintf("Muestra generada: %s registro(s), equivalente al 10%% del rango seleccionado.", nrow(records)),
          details = character()
        ))
      }
    }, error = function(error) {
      f5_review_status(list(
        type = "error",
        message = "No se pudo generar la muestra.",
        details = conditionMessage(error)
      ))
    })
  })

  observeEvent(input$f5_review_refresh_list, {
    tryCatch({
      records <- f5_load_review_records(random_sample = FALSE)
      f5_review_records(records)
      f5_review_selected(NULL)
      f5_review_comparison(NULL)
      f5_review_delete_mode(FALSE)

      if (nrow(records) == 0) {
        f5_review_status(list(
          type = "warning",
          message = "No hay registros en el rango seleccionado.",
          details = character()
        ))
      } else {
        f5_review_status(list(
          type = "success",
          message = sprintf("Listado actualizado: %s registro(s).", nrow(records)),
          details = character()
        ))
      }
    }, error = function(error) {
      f5_review_status(list(
        type = "error",
        message = "No se pudo actualizar el listado.",
        details = conditionMessage(error)
      ))
    })
  })

  observeEvent(input$f5_review_select_intake_id, {
    intake_id <- f5_integer(input$f5_review_select_intake_id)
    if (is.na(intake_id)) {
      return()
    }

    tryCatch({
      record <- f5_fetch_review_record(intake_id)
      if (nrow(record) == 0) {
        f5_review_status(list(
          type = "warning",
          message = sprintf("No se encontró intake_id %s.", intake_id),
          details = character()
        ))
        return()
      }

      f5_review_selected(record)
      f5_review_comparison(NULL)
      f5_review_delete_mode(FALSE)
      f5_review_status(list(
        type = "success",
        message = sprintf("Registro intake_id %s abierto para redigitación.", intake_id),
        details = character()
      ))
    }, error = function(error) {
      f5_review_status(list(
        type = "error",
        message = "No se pudo abrir el registro seleccionado.",
        details = conditionMessage(error)
      ))
    })
  })

  observeEvent(input$f5_review_compare, {
    record <- f5_review_selected()
    if (is.null(record) || nrow(record) == 0) {
      f5_review_status(list(
        type = "warning",
        message = "Abra primero un intake_id para revisar.",
        details = character()
      ))
      return()
    }

    comparison <- f5_review_discrepancies(record, f5_review_redigit_record())
    f5_review_comparison(comparison)

    if (nrow(comparison) == 0) {
      updateSelectInput(session, "f5_review_final_status", selected = "reviewed")
      f5_review_status(list(
        type = "success",
        message = "La redigitación coincide con el registro original.",
        details = character()
      ))
    } else {
      updateSelectInput(session, "f5_review_final_status", selected = "rejected")
      f5_review_status(list(
        type = "warning",
        message = sprintf("Se encontraron discrepancias en %s columna(s).", nrow(comparison)),
        details = comparison$Campo
      ))
    }
  })

  observeEvent(input$f5_review_request_delete, {
    record <- f5_review_selected()
    if (is.null(record) || nrow(record) == 0) return()
    f5_review_delete_mode(TRUE)
  })

  observeEvent(input$f5_review_delete_cancel, {
    f5_review_delete_mode(FALSE)
    updateTextAreaInput(session, "f5_review_delete_reason", value = "")
  })

  observeEvent(input$f5_review_save, {
    record <- f5_review_selected()
    if (is.null(record) || nrow(record) == 0) {
      f5_review_status(list(
        type = "warning",
        message = "Abra primero un intake_id para revisar.",
        details = character()
      ))
      return()
    }

    comparison <- f5_review_comparison()
    if (is.null(comparison)) {
      comparison <- f5_review_discrepancies(record, f5_review_redigit_record())
      f5_review_comparison(comparison)
    }

    status <- f5_text(input$f5_review_final_status)
    if (!status %in% c("pending", "reviewed", "rejected")) {
      status <- if (nrow(comparison) == 0) "reviewed" else "rejected"
    }

    notes <- f5_optional_text(input$f5_review_notes)
    if (nrow(comparison) > 0) {
      discrepancy_note <- paste(
        "Discrepancias:",
        paste(comparison$Columna, collapse = ", ")
      )
      notes <- paste(na.omit(c(notes, discrepancy_note)), collapse = "\n")
    }

    connection <- NULL
    tryCatch({
      withProgress(message = "Actualizando revisión en Supabase", value = 0, {
        incProgress(0.25, detail = "Abriendo conexión")
        connection <- connect_to_supabase()
        incProgress(0.5, detail = "Guardando revisión")
        updated <- dbGetQuery(
          connection,
          "
            update public.formulario_5_alimentacion_conteo_intake
            set
              review_status = $1,
              review_notes = $2,
              reviewed_by = nullif($3, ''),
              reviewed_at = $4::timestamptz,
              actualizado_en = now()
            where intake_id = $5
            returning intake_id, review_status, actualizado_en
          ",
          params = list(
            status,
            notes,
            f5_text(input$f5_reviewed_by),
            as.character(f5_date(input$f5_reviewed_at)),
            as.integer(record$intake_id[[1]])
          )
        )
        incProgress(0.25, detail = "Listo")

        if (nrow(updated) == 0) {
          stop("No se actualizó ningún registro.")
        }

        updated
      })

      refreshed <- f5_fetch_review_record(record$intake_id[[1]])
      f5_review_selected(refreshed)
      f5_review_delete_mode(FALSE)
      f5_review_status(list(
        type = "success",
        message = sprintf(
          "Revisión guardada para intake_id %s. Estado: %s. Actualizado en: %s.",
          record$intake_id[[1]],
          status,
          as.character(refreshed$actualizado_en[[1]])
        ),
        details = character()
      ))
      showNotification(sprintf("Revisión guardada para intake_id %s.", record$intake_id[[1]]), type = "message", duration = 6)
    }, error = function(error) {
      f5_review_status(list(
        type = "error",
        message = "No se pudo guardar la revisión.",
        details = conditionMessage(error)
      ))
      showNotification("No se pudo guardar la revisión del Formulario 5.", type = "error", duration = 8)
    }, finally = {
      if (!is.null(connection)) {
        dbDisconnect(connection)
      }
    })
  })

  observeEvent(input$f5_review_delete_confirm, {
    record <- f5_review_selected()
    if (is.null(record) || nrow(record) == 0) return()
    reason <- f7_clean_text(input$f5_review_delete_reason)[[1]]
    if (is.na(reason)) {
      f5_review_status(list(
        type = "error",
        message = "No se puede eliminar el registro sin comentario.",
        details = "Ingrese el motivo de eliminación antes de confirmar."
      ))
      return()
    }
    intake_id <- as.integer(record$intake_id[[1]])
    tryCatch({
      deleted <- withProgress(message = "Eliminando registro de Formulario 5", value = 0, {
        incProgress(0.20, detail = "Validando el comentario de eliminación")
        incProgress(0.30, detail = "Borrando registro en Supabase")
        removed <- f5_delete_review_record(intake_id, reason, value_or_default(user_profile$name, f5_text(input$f5_reviewed_by)))
        incProgress(0.30, detail = "Actualizando la lista de revisión")
        refreshed_records <- tryCatch(f5_load_review_records(random_sample = FALSE), error = function(error) data.frame())
        f5_review_records(refreshed_records)
        incProgress(0.20, detail = "Eliminación completada")
        removed
      })
      f5_review_selected(NULL)
      f5_review_comparison(NULL)
      f5_review_delete_mode(FALSE)
      f5_review_status(list(
        type = "success",
        message = sprintf("Registro %s eliminado definitivamente.", intake_id),
        details = sprintf("Cepa/población eliminada: %s.", deleted$cepa_poblacion[[1]])
      ))
    }, error = function(error) {
      f5_review_status(list(type = "error", message = "No se pudo eliminar el registro.", details = conditionMessage(error)))
    })
  })

  observeEvent({
    list(
      input$f5_formulario_codigo,
      input$f5_pais,
      input$f5_departamento_numero,
      input$f5_municipio_numero,
      input$f5_ciclo,
      input$f5_cepa_poblacion,
      input$f5_especie,
      input$f5_fecha_jaula,
      input$f5_numero_hembras,
      input$f5_numero_machos,
      input$f5_total_huevos_viables,
      input$f5_tipo_alimentacion_descripcion,
      input$f5_fecha_alimentacion_sangre,
      input$f5_fecha_colocacion_sustrato,
      input$f5_fecha_retiro_sustrato,
      input$f5_hv_huevos_viables,
      input$f5_he_huevos_eclosionados,
      input$f5_hc_huevos_canoa,
      input$f5_hnf_huevos_no_fecundados,
      input$f5_creado_por
    )
  }, {
    f5_certification_complete(FALSE)
    f5_save_status(list(type = "idle", message = NULL, details = character()))
  }, ignoreInit = TRUE)

  f7_clean_text <- function(value) {
    value <- trimws(as.character(value))
    value[value %in% c("", "NA", "NaN")] <- NA_character_
    value
  }

  f7_parse_boolean <- function(value) {
    cleaned <- tolower(f7_clean_text(value))
    result <- rep(NA, length(cleaned))
    result[cleaned %in% c("true", "1", "si", "sí", "yes")] <- TRUE
    result[cleaned %in% c("false", "0", "no")] <- FALSE
    result
  }

  validate_formulario_7 <- function(csv_data) {
    details <- character()
    expected_columns <- if (all(formulario_7_intake_columns %in% names(csv_data))) formulario_7_intake_columns else formulario_7_csv_columns
    missing_columns <- setdiff(expected_columns, names(csv_data))
    extra_columns <- setdiff(names(csv_data), expected_columns)
    if (length(missing_columns) > 0) details <- c(details, paste("Faltan columnas:", paste(missing_columns, collapse = ", ")))
    if (length(extra_columns) > 0) details <- c(details, paste("Columnas no esperadas:", paste(extra_columns, collapse = ", ")))
    if (length(details) > 0) return(list(data = NULL, details = details))

    data <- formulario_7_csv_to_internal(csv_data)
    if (nrow(data) == 0) return(list(data = NULL, details = "El archivo no contiene registros."))
    for (column in names(data)) data[[column]] <- f7_clean_text(data[[column]])
    data$codigo_control_calidad[is.na(data$codigo_control_calidad)] <- "NO APLICA"

    required_text <- c(
      "formulario_codigo", "formulario_nombre", "nombre_poblacion", "codigo_bioensayo",
      "insecticida", "solvente_utilizado", "lote_insecticida",
      "origen_material", "pais", "id_institucion", "codigo_departamento", "codigo_municipio", "codigo_especie_mosquito",
      "codigo_responsable_revestimiento", "codigo_responsable_bioensayo"
    )
    required_dates <- c("fecha_registro", "fecha_realizacion_bioensayo", "fecha_revestimiento_botellas", "fecha_separacion")
    required_times <- c("hora_separacion", "hora_inicio_bioensayo", "hora_final_bioensayo")
    boolean_columns <- c(
      "bioensayo_diagnostica_1x", "sinergista_def", "sinergista_pbo", "sinergista_dm",
      "edad_indefinida", "generacion_filial_indefinida"
    )
    numeric_columns <- c(
      "dosis_intensidad_ug_ml", "dosis_sinergista_ug_ml", paste0("numero_usos_botella_", c("e1", "e2", "e3", "e4", "c1")), "edad_dias",
      "temperatura_inicial_c", "temperatura_final_c", "humedad_relativa_inicial_pct", "humedad_relativa_final_pct",
      grep("_(vivos|incapacitados)$", formulario_7_intake_columns, value = TRUE)
    )
    integer_columns <- c(
      paste0("numero_usos_botella_", c("e1", "e2", "e3", "e4", "c1")), "edad_dias",
      grep("_(vivos|incapacitados)$", formulario_7_intake_columns, value = TRUE)
    )
    result_time_columns <- grep("(hora_inicio|hora_lectura)", formulario_7_result_columns, value = TRUE)

    for (column in required_text) {
      bad <- which(is.na(data[[column]]))
      if (length(bad)) details <- c(details, paste0(column, " es obligatorio. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    for (column in required_dates) {
      raw <- data[[column]]
      parsed <- suppressWarnings(as.Date(raw, format = "%Y-%m-%d"))
      bad <- which(is.na(raw) | is.na(parsed) | !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", raw))
      if (length(bad)) details <- c(details, paste0(column, " debe usar YYYY-MM-DD. Filas: ", paste(head(bad, 10), collapse = ", ")))
      data[[column]] <- parsed
    }
    for (column in c(required_times, result_time_columns)) {
      raw <- data[[column]]
      bad_format <- !is.na(raw) & !grepl("^([01][0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$", raw)
      if (column %in% required_times) bad_format <- bad_format | is.na(raw)
      bad <- which(bad_format)
      if (length(bad)) details <- c(details, paste0(column, " debe usar HH:MM. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    for (column in boolean_columns) {
      parsed <- f7_parse_boolean(data[[column]])
      bad <- which(is.na(parsed))
      if (length(bad)) details <- c(details, paste0(column, " debe usar true o false. Filas: ", paste(head(bad, 10), collapse = ", ")))
      data[[column]] <- parsed
    }
    has_synergist_rows <- apply(data[c("sinergista_def", "sinergista_pbo", "sinergista_dm")], 1, function(values) any(values, na.rm = TRUE))
    missing_review_24h <- which(is.na(data$codigo_revision_24h))
    if (length(missing_review_24h)) {
      details <- c(details, paste0("codigo_revision_24h es obligatorio. Filas: ", paste(head(missing_review_24h, 10), collapse = ", ")))
    }
    data$codigo_bioensayo <- formulario_7_codigo_bioensayo_final(
      data$codigo_bioensayo, data$bioensayo_diagnostica_1x, data$bioensayo_intensidad, data$dosis_intensidad,
      data$sinergista_def, data$sinergista_pbo, data$sinergista_dm
    )
    bad_generated_code <- which(is.na(data$codigo_bioensayo))
    if (length(bad_generated_code)) details <- c(details, paste0("No se pudo generar codigo_bioensayo. Revise el tipo de bioensayo. Filas: ", paste(head(bad_generated_code, 10), collapse = ", ")))
    for (column in numeric_columns) {
      raw <- data[[column]]
      parsed <- suppressWarnings(as.numeric(raw))
      bad <- which(!is.na(raw) & (is.na(parsed) | parsed < 0))
      if (length(bad)) details <- c(details, paste0(column, " debe ser numérico y no negativo. Filas: ", paste(head(bad, 10), collapse = ", ")))
      if (column %in% integer_columns) {
        non_integer <- which(!is.na(parsed) & parsed != floor(parsed))
        if (length(non_integer)) details <- c(details, paste0(column, " debe ser entero. Filas: ", paste(head(non_integer, 10), collapse = ", ")))
        parsed <- as.integer(parsed)
      }
      data[[column]] <- parsed
    }

    required_numeric <- c("dosis_intensidad_ug_ml", "temperatura_inicial_c", "temperatura_final_c", "humedad_relativa_inicial_pct", "humedad_relativa_final_pct")
    for (column in required_numeric) {
      bad <- which(is.na(data[[column]]))
      if (length(bad)) details <- c(details, paste0(column, " es obligatorio. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    for (column in c("humedad_relativa_inicial_pct", "humedad_relativa_final_pct")) {
      bad <- which(!is.na(data[[column]]) & data[[column]] > 100)
      if (length(bad)) details <- c(details, paste0(column, " no puede ser mayor que 100. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    allowed <- list(
      formulario_codigo = "F7", bioensayo_intensidad = c("Exploratorio", "Completa"), solvente_utilizado = c("Etanol", "Otro"),
      origen_material = c("Silvestre", "Laboratorio"), pais = c("El Salvador", "Guatemala"),
      dosis_intensidad = c("1X", "2X", "5X", "10X"),
      sinergista_tipo = c("DEF", "PBO", "DM"),
      resultado_diagnostico = c("Suceptible", "Sospecha de Resistencia", "Resistente")
    )
    for (column in names(allowed)) {
      bad <- which(!is.na(data[[column]]) & !(data[[column]] %in% allowed[[column]]))
      if (length(bad)) details <- c(details, paste0(column, " debe usar: ", paste(allowed[[column]], collapse = ", "), ". Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    temefos_rows <- which(vapply(data$insecticida, formulario_7_is_temefos, logical(1)))
    if (length(temefos_rows)) {
      for (column in formulario_7_non_24h_result_columns) {
        bad <- temefos_rows[!is.na(data[[column]][temefos_rows])]
        if (length(bad)) details <- c(details, paste0(column, " no aplica para Temefos; solo registre lecturas de 24h. Filas: ", paste(head(bad, 10), collapse = ", ")))
      }
    }
    duplicate_codes <- unique(data$codigo_bioensayo[duplicated(data$codigo_bioensayo) & !is.na(data$codigo_bioensayo)])
    if (length(duplicate_codes)) details <- c(details, paste0("Código de bioensayo duplicado dentro del archivo: ", paste(duplicate_codes, collapse = ", "), "."))

    for (row in seq_len(nrow(data))) {
      has_synergist <- any(c(data$sinergista_def[[row]], data$sinergista_pbo[[row]], data$sinergista_dm[[row]]), na.rm = TRUE)
      is_diagnostic <- isTRUE(data$bioensayo_diagnostica_1x[[row]])
      is_intensity <- !is.na(data$bioensayo_intensidad[[row]])
      selected_types <- sum(is_diagnostic, is_intensity, has_synergist)
      if (selected_types != 1) details <- c(details, paste("Fila", row, ": seleccione exactamente un Tipo de Bioensayo: Diagnóstica 1X, Intensidad o Sinergistas."))
      if (is_diagnostic) {
        if (is.na(data$resultado_diagnostico[[row]])) details <- c(details, paste("Fila", row, ": indique el resultado de la prueba diagnóstica."))
        if (!is.na(data$dosis_intensidad[[row]]) || has_synergist || is_intensity) details <- c(details, paste("Fila", row, ": Diagnóstica 1X no admite modalidad de intensidad, dosis de intensidad ni sinergistas."))
      }
      if (is_intensity) {
        current_result <- data$resultado_diagnostico[[row]]
        current_dose <- data$dosis_intensidad[[row]]
        if (is.na(current_result)) details <- c(details, paste("Fila", row, ": indique el resultado de la prueba diagnóstica para Intensidad."))
        if (identical(data$bioensayo_intensidad[[row]], "Exploratorio")) {
          if (identical(current_result, "Suceptible") && !is.na(current_dose)) details <- c(details, paste("Fila", row, ": Intensidad Exploratorio Suceptible no debe llevar dosis_intensidad."))
          if (current_result %in% c("Sospecha de Resistencia", "Resistente") && is.na(current_dose)) details <- c(details, paste("Fila", row, ": Intensidad Exploratorio con Resistente o Sospecha de Resistencia requiere dosis_intensidad 1X, 2X, 5X o 10X."))
        }
        if (identical(data$bioensayo_intensidad[[row]], "Completa") && is.na(data$dosis_intensidad[[row]])) details <- c(details, paste("Fila", row, ": Intensidad Completa requiere dosis_intensidad 1X, 2X, 5X o 10X."))
        if (is_diagnostic || has_synergist) details <- c(details, paste("Fila", row, ": Intensidad no puede combinarse con Diagnóstica 1X o Sinergistas."))
      } else if (!is.na(data$dosis_intensidad[[row]])) {
        details <- c(details, paste("Fila", row, ": dosis_intensidad solo corresponde al Tipo de Bioensayo Intensidad."))
      }
      if (has_synergist) {
        if (is.na(data$sinergista_tipo[[row]])) details <- c(details, paste("Fila", row, ": indique sinergista_tipo DEF, PBO o DM."))
        if (is.na(data$dosis_sinergista_ug_ml[[row]])) details <- c(details, paste("Fila", row, ": indique dosis_sinergista_ug_ml."))
        if (is.na(data$resultado_diagnostico[[row]])) details <- c(details, paste("Fila", row, ": indique el resultado de la prueba diagnóstica para Sinergistas."))
      } else {
        data$sinergista_tipo[[row]] <- NA_character_
        data$dosis_sinergista_ug_ml[[row]] <- NA_real_
      }
      if (identical(data$solvente_utilizado[[row]], "Otro") && is.na(data$solvente_otro[[row]])) details <- c(details, paste("Fila", row, ": especifique el otro solvente."))
      if (identical(data$solvente_utilizado[[row]], "Etanol")) data$solvente_otro[[row]] <- NA_character_
      if (isTRUE(data$edad_indefinida[[row]])) data$edad_dias[[row]] <- NA_integer_ else if (is.na(data$edad_dias[[row]])) details <- c(details, paste("Fila", row, ": indique edad_dias o marque edad_indefinida."))
      if (isTRUE(data$generacion_filial_indefinida[[row]])) data$generacion_filial[[row]] <- NA_character_ else if (is.na(data$generacion_filial[[row]])) details <- c(details, paste("Fila", row, ": indique generacion_filial o marque generacion_filial_indefinida."))
    }

    count_columns <- grep("_(vivos|incapacitados)$", formulario_7_intake_columns, value = TRUE)
    pair_bases <- unique(sub("_(vivos|incapacitados)$", "", count_columns))
    for (base in pair_bases) {
      lives <- data[[paste0(base, "_vivos")]]
      disabled <- data[[paste0(base, "_incapacitados")]]
      bad <- which(xor(is.na(lives), is.na(disabled)))
      if (length(bad)) details <- c(details, paste0(base, " debe incluir vivos e incapacitados juntos. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }

    list(data = data, details = unique(details))
  }

  formulario_7_tables <- function(row) {
    header <- row[formulario_7_header_columns]
    results <- list()
    synergist_values <- tolower(as.character(unlist(row[c("sinergista_def", "sinergista_pbo", "sinergista_dm")])))
    has_synergist <- any(synergist_values %in% c("true", "1", "si", "sí", "yes"), na.rm = TRUE)
    is_temefos <- formulario_7_is_temefos(row$insecticida[[1]])
    for (bottle in formulario_7_bottles) {
      if (is_temefos) {
        result_24h_base <- paste0("resultado_24h_", bottle)
        if (!is.na(row[[paste0(result_24h_base, "_vivos")]])) results[[length(results) + 1]] <- data.frame(
          fase = "kdr_24h", botella = bottle, tiempo_minutos = 1440,
          hora_lectura = row[[paste0("resultado_hora_lectura_24h_", bottle)]],
          vivos = row[[paste0(result_24h_base, "_vivos")]], incapacitados = row[[paste0(result_24h_base, "_incapacitados")]], stringsAsFactors = FALSE
        )
        next
      }
      if (has_synergist) {
        base <- paste0("resultado_60min_", bottle)
        if (!is.na(row[[paste0(base, "_vivos")]])) results[[length(results) + 1]] <- data.frame(
          fase = "bioensayo", botella = bottle, tiempo_minutos = 60,
          hora_lectura = row[[paste0("resultado_hora_inicio_", bottle)]],
          vivos = row[[paste0(base, "_vivos")]], incapacitados = row[[paste0(base, "_incapacitados")]], stringsAsFactors = FALSE
        )
        for (minutes in c(0, 15, 30, 45)) {
          base <- paste0("resultado_", minutes, "min_", bottle)
          if (!is.na(row[[paste0(base, "_vivos")]])) results[[length(results) + 1]] <- data.frame(
            fase = "bioensayo", botella = bottle, tiempo_minutos = minutes,
            hora_lectura = row[[paste0("resultado_hora_inicio_", bottle)]],
            vivos = row[[paste0(base, "_vivos")]], incapacitados = row[[paste0(base, "_incapacitados")]], stringsAsFactors = FALSE
          )
        }
        result_24h_base <- paste0("resultado_24h_", bottle)
        if (!is.na(row[[paste0(result_24h_base, "_vivos")]])) results[[length(results) + 1]] <- data.frame(
          fase = "kdr_24h", botella = bottle, tiempo_minutos = 1440,
          hora_lectura = row[[paste0("resultado_hora_lectura_24h_", bottle)]],
          vivos = row[[paste0(result_24h_base, "_vivos")]], incapacitados = row[[paste0(result_24h_base, "_incapacitados")]], stringsAsFactors = FALSE
        )
        next
      }
      for (minutes in c(0, 15, 30, 45)) {
        base <- paste0("resultado_", minutes, "min_", bottle)
        if (!is.na(row[[paste0(base, "_vivos")]])) results[[length(results) + 1]] <- data.frame(
          fase = "bioensayo", botella = bottle, tiempo_minutos = minutes,
          hora_lectura = row[[paste0("resultado_hora_inicio_", bottle)]],
          vivos = row[[paste0(base, "_vivos")]], incapacitados = row[[paste0(base, "_incapacitados")]], stringsAsFactors = FALSE
        )
      }
      result_24h_base <- paste0("resultado_24h_", bottle)
      if (!is.na(row[[paste0(result_24h_base, "_vivos")]])) results[[length(results) + 1]] <- data.frame(
        fase = "kdr_24h", botella = bottle, tiempo_minutos = 1440,
        hora_lectura = row[[paste0("resultado_hora_lectura_24h_", bottle)]],
        vivos = row[[paste0(result_24h_base, "_vivos")]], incapacitados = row[[paste0(result_24h_base, "_incapacitados")]], stringsAsFactors = FALSE
      )
    }
    comment <- row[["comentario"]]
    comments <- if (is.na(comment)) {
      data.frame()
    } else {
      data.frame(comentario = comment, nombre = row[["comentario_nombre"]], stringsAsFactors = FALSE)
    }
    list(
      header = as.data.frame(header, stringsAsFactors = FALSE),
      results = if (length(results)) do.call(rbind, results) else data.frame(),
      comments = comments
    )
  }

  formulario_7_unique_code_lookup <- function(codes) {
    codes <- unique(f7_clean_text(codes))
    codes <- codes[!is.na(codes)]
    if (!length(codes)) return(list(query = NULL, params = list()))
    placeholders <- paste0("$", seq_along(codes))
    list(
      query = paste0(
        "select codigo_bioensayo from public.formulario_7_bioensayo_intake ",
        "where codigo_bioensayo in (", paste(placeholders, collapse = ", "), ") ",
        "order by codigo_bioensayo"
      ),
      params = as.list(codes)
    )
  }

  formulario_7_existing_unique_codes <- function(connection, codes) {
    lookup <- formulario_7_unique_code_lookup(codes)
    if (is.null(lookup$query)) return(character())
    existing <- dbGetQuery(
      connection,
      lookup$query,
      params = lookup$params
    )
    as.character(existing$codigo_bioensayo)
  }

  insert_formulario_7 <- function(connection, data, progress_callback = NULL) {
    intake_ids <- character(nrow(data))
    dbWithTransaction(connection, {
      for (row_index in seq_len(nrow(data))) {
        tables <- formulario_7_tables(data[row_index, , drop = FALSE])
        dbAppendTable(connection, Id(schema = "public", table = "formulario_7_bioensayo_intake"), tables$header)
        intake_id <- dbGetQuery(connection, "select currval(pg_get_serial_sequence('public.formulario_7_bioensayo_intake', 'intake_id'))::bigint as intake_id")$intake_id[[1]]
        intake_ids[[row_index]] <- as.character(intake_id)
        if (nrow(tables$results)) {
          tables$results$intake_id <- intake_id
          tables$results <- tables$results[c("intake_id", "fase", "botella", "tiempo_minutos", "hora_lectura", "vivos", "incapacitados")]
          dbAppendTable(connection, Id(schema = "public", table = "formulario_7_bioensayo_resultado_intake"), tables$results)
        }
        if (nrow(tables$comments)) {
          tables$comments$intake_id <- intake_id
          tables$comments <- tables$comments[c("intake_id", "comentario", "nombre")]
          dbAppendTable(connection, Id(schema = "public", table = "formulario_7_bioensayo_comentario_intake"), tables$comments)
        }
        if (is.function(progress_callback)) progress_callback(row_index, nrow(data))
      }
    })
    intake_ids
  }

  f7_review_boolean_fields <- c(
    "bioensayo_diagnostica_1x", "sinergista_def", "sinergista_pbo", "sinergista_dm",
    "edad_indefinida", "generacion_filial_indefinida"
  )
  f7_review_date_fields <- c(
    "fecha_registro", "fecha_realizacion_bioensayo", "fecha_revestimiento_botellas", "fecha_separacion"
  )
  f7_review_numeric_fields <- c(
    "dosis_intensidad_ug_ml", "dosis_sinergista_ug_ml", paste0("numero_usos_botella_", c("e1", "e2", "e3", "e4", "c1")), "edad_dias",
    "temperatura_inicial_c", "temperatura_final_c", "humedad_relativa_inicial_pct", "humedad_relativa_final_pct",
    grep("_(vivos|incapacitados)$", formulario_7_intake_columns, value = TRUE)
  )
  f7_review_protected_fields <- c("creado_en", "actualizado_en")

  f7_review_field_label <- function(field) {
    special <- c(
      formulario_codigo = "Código del formulario", formulario_nombre = "Nombre del formulario",
      codigo_bioensayo = "Código de bioensayo",
      bioensayo_diagnostica_1x = "Diagnóstica 1X", dosis_intensidad = "Dosis de intensidad",
      sinergista_def = "Sinergista DEF", sinergista_pbo = "Sinergista PBO", sinergista_dm = "Sinergista DM",
      sinergista_tipo = "Sinergista", dosis_sinergista_ug_ml = "Dosis sinergista (µg/mL)",
      resultado_diagnostico = "Resultado diagnóstico", insecticida = "Insecticida",
      dosis_intensidad_ug_ml = "Concentración (µg/mL)", lote_insecticida = "# lote insecticida",
      humedad_relativa_inicial_pct = "Humedad relativa inicial (%)",
      humedad_relativa_final_pct = "Humedad relativa final (%)",
      temperatura_inicial_c = "Temperatura inicial (°C)", temperatura_final_c = "Temperatura final (°C)",
      creado_en = "Creado en", actualizado_en = "Actualizado en"
    )
    if (field %in% names(special)) return(unname(special[[field]]))
    label <- gsub("_", " ", field, fixed = TRUE)
    label <- gsub("\\bb([1-4])\\b", "Botella \\1", label)
    label <- gsub("\\bc1\\b", "Control", label)
    paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
  }

  f7_review_section <- function(field) {
    if (field %in% formulario_7_result_columns) return("Resultados por botella")
    if (field %in% formulario_7_comment_columns) return("Comentarios y auditoría")
    if (field %in% c("fuente_formulario", "nombre_quien_ingreso", "creado_en", "actualizado_en")) return("Comentarios y auditoría")
    if (field %in% c(
      "formulario_codigo", "formulario_nombre", "fecha_registro", "codigo_bioensayo", "nombre_poblacion",
      "bioensayo_intensidad", "bioensayo_diagnostica_1x", "dosis_intensidad", "sinergista_def", "sinergista_pbo",
      "sinergista_dm", "sinergista_tipo", "dosis_sinergista_ug_ml", "resultado_diagnostico", "pais", "codigo_departamento", "codigo_municipio"
    )) return("Información general")
    if (field %in% c(
      "fecha_realizacion_bioensayo", "insecticida", "solvente_utilizado", "solvente_otro",
      "dosis_intensidad_ug_ml", "lote_insecticida", "fecha_revestimiento_botellas", paste0("numero_usos_botella_", c("e1", "e2", "e3", "e4", "c1"))
    )) return("Información del bioensayo")
    if (field %in% c(
      "origen_material", "codigo_pais", "edad_dias", "edad_indefinida", "codigo_especie_mosquito",
      "fecha_separacion", "hora_separacion", "generacion_filial", "generacion_filial_indefinida",
      "codigo_responsable_revestimiento", "codigo_responsable_bioensayo",
      "codigo_control_calidad", "codigo_revision_24h"
    )) return("Material y responsables")
    "Condiciones"
  }

  f7_review_base_bioassay_code <- function(row) {
    final_code <- f7_clean_text(row$codigo_bioensayo)[[1]]
    if (is.na(final_code)) return("")
    expected <- formulario_7_codigo_bioensayo_final(
      "__BASE__", row$bioensayo_diagnostica_1x, row$bioensayo_intensidad, row$dosis_intensidad,
      row$sinergista_def, row$sinergista_pbo, row$sinergista_dm
    )
    suffix <- sub("^__BASE__", "", expected[[1]])
    if (nzchar(suffix) && endsWith(final_code, suffix)) {
      return(substr(final_code, 1, nchar(final_code) - nchar(suffix)))
    }
    final_code
  }

  f7_fetch_review_record <- function(intake_id) {
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    header <- dbGetQuery(
      connection,
      "select * from public.formulario_7_bioensayo_intake where intake_id = $1",
      params = list(as.integer(intake_id))
    )
    if (nrow(header) == 0) return(NULL)
    results <- dbGetQuery(
      connection,
      "select fase, botella, tiempo_minutos, hora_lectura, vivos, incapacitados from public.formulario_7_bioensayo_resultado_intake where intake_id = $1 order by fase, botella, tiempo_minutos",
      params = list(as.integer(intake_id))
    )
    comments <- dbGetQuery(
      connection,
      "select comentario, nombre from public.formulario_7_bioensayo_comentario_intake where intake_id = $1",
      params = list(as.integer(intake_id))
    )

    values <- setNames(rep(list(NA_character_), length(formulario_7_intake_columns)), formulario_7_intake_columns)
    for (field in intersect(names(values), names(header))) values[[field]] <- header[[field]][[1]]
    if (nrow(results)) for (index in seq_len(nrow(results))) {
      result <- results[index, ]
      bottle <- as.character(result$botella[[1]])
      if (identical(as.character(result$fase[[1]]), "bioensayo")) {
        minutes <- as.integer(result$tiempo_minutos[[1]])
        values[[paste0("resultado_hora_inicio_", bottle)]] <- result$hora_lectura[[1]]
        if (identical(minutes, 60L)) {
          values[[paste0("resultado_60min_", bottle, "_vivos")]] <- result$vivos[[1]]
          values[[paste0("resultado_60min_", bottle, "_incapacitados")]] <- result$incapacitados[[1]]
        } else {
          values[[paste0("resultado_", minutes, "min_", bottle, "_vivos")]] <- result$vivos[[1]]
          values[[paste0("resultado_", minutes, "min_", bottle, "_incapacitados")]] <- result$incapacitados[[1]]
        }
      } else if (identical(as.character(result$fase[[1]]), "kdr_24h")) {
        values[[paste0("resultado_hora_lectura_24h_", bottle)]] <- result$hora_lectura[[1]]
        values[[paste0("resultado_24h_", bottle, "_vivos")]] <- result$vivos[[1]]
        values[[paste0("resultado_24h_", bottle, "_incapacitados")]] <- result$incapacitados[[1]]
      }
    }
    if (nrow(comments)) {
      values$comentario <- comments$comentario[[1]]
      values$comentario_nombre <- comments$nombre[[1]]
    }
    data <- as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
    data$codigo_bioensayo <- f7_review_base_bioassay_code(data)
    list(header = header, data = data)
  }

  f7_fetch_review_record_by_code <- function(codigo_bioensayo) {
    code <- toupper(trimws(value_or_default(codigo_bioensayo, "")))
    if (!nzchar(code)) stop("Ingrese un Código de bioensayo para buscar.")
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    record <- dbGetQuery(
      connection,
      "select intake_id from public.formulario_7_bioensayo_intake where upper(codigo_bioensayo) = $1 order by actualizado_en desc nulls last, intake_id desc limit 1",
      params = list(code)
    )
    if (!nrow(record)) return(NULL)
    f7_fetch_review_record(record$intake_id[[1]])
  }

  f7_load_review_records <- function(random_sample = FALSE) {
    start_date <- as.Date(input$f7_review_start_date)
    end_date <- as.Date(input$f7_review_end_date)
    if (is.na(start_date) || is.na(end_date) || start_date > end_date) stop("Seleccione un rango de fechas válido.")
    status <- f5_text(input$f7_review_filter_status)
    if (!status %in% c("pending", "reviewed", "rejected", "all")) status <- "pending"
    exclude_submitter <- if (random_sample) f7_clean_text(input$f7_review_exclude_submitter)[[1]] else NA_character_
    where_clause <- "where fecha_registro between $1 and $2 and ($3 = 'all' or review_status = $3)"
    params <- list(as.character(start_date), as.character(end_date), status)
    if (!is.na(exclude_submitter)) {
      where_clause <- paste0(where_clause, " and lower(coalesce(nullif(trim(nombre_quien_ingreso), ''), 'Sin nombre')) <> lower($4)")
      params <- c(params, list(exclude_submitter))
    }
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    total <- dbGetQuery(
      connection,
      paste("select count(*)::integer as total from public.formulario_7_bioensayo_intake", where_clause),
      params = params
    )$total[[1]]
    if (total == 0) return(data.frame())
    limit <- if (random_sample) max(1L, ceiling(as.integer(total) * 0.10)) else min(as.integer(total), 50L)
    order_clause <- if (random_sample) "order by random()" else "order by creado_en desc nulls last, intake_id desc"
    query <- paste(
      "select intake_id, codigo_bioensayo, fecha_registro, pais, nombre_poblacion, nombre_quien_ingreso, review_status, creado_en, actualizado_en from public.formulario_7_bioensayo_intake",
      where_clause,
      order_clause,
      paste0("limit $", length(params) + 1L)
    )
    dbGetQuery(
      connection,
      query,
      params = c(params, list(as.integer(limit)))
    )
  }

  f7_review_input_id <- function(field) paste0("f7_review_value_", field)

  f7_review_input_row <- function() {
    selected <- f7_review_selected()
    values <- selected$data[1, formulario_7_intake_columns, drop = FALSE]
    for (field in setdiff(formulario_7_intake_columns, f7_review_protected_fields)) {
      current <- input[[f7_review_input_id(field)]]
      values[[field]] <- if (is.null(current) || length(current) == 0) NA_character_ else as.character(current[[1]])
    }
    values$creado_en <- selected$data$creado_en[[1]]
    values$actualizado_en <- selected$data$actualizado_en[[1]]
    if (formulario_7_is_temefos(values$insecticida[[1]])) {
      values[formulario_7_non_24h_result_columns] <- NA_character_
    }
    values
  }

  f7_update_review_record <- function(intake_id, data) {
    tables <- formulario_7_tables(data[1, , drop = FALSE])
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    dbWithTransaction(connection, {
      columns <- names(tables$header)
      assignments <- paste0(as.character(dbQuoteIdentifier(connection, columns)), " = $", seq_along(columns))
      query <- paste0(
        "update public.formulario_7_bioensayo_intake set ", paste(assignments, collapse = ", "),
        ", review_status = 'pending', review_notes = null, reviewed_by = null, reviewed_at = null, actualizado_en = now() where intake_id = $",
        length(columns) + 1L
      )
      params <- c(unname(as.list(tables$header[1, columns, drop = TRUE])), list(as.integer(intake_id)))
      updated <- dbExecute(connection, query, params = params)
      if (updated != 1L) stop("No se actualizó el registro seleccionado.")
      dbExecute(connection, "delete from public.formulario_7_bioensayo_resultado_intake where intake_id = $1", params = list(as.integer(intake_id)))
      dbExecute(connection, "delete from public.formulario_7_bioensayo_comentario_intake where intake_id = $1", params = list(as.integer(intake_id)))
      if (nrow(tables$results)) {
        tables$results$intake_id <- as.integer(intake_id)
        tables$results <- tables$results[c("intake_id", "fase", "botella", "tiempo_minutos", "hora_lectura", "vivos", "incapacitados")]
        dbAppendTable(connection, Id(schema = "public", table = "formulario_7_bioensayo_resultado_intake"), tables$results)
      }
      if (nrow(tables$comments)) {
        tables$comments$intake_id <- as.integer(intake_id)
        tables$comments <- tables$comments[c("intake_id", "comentario", "nombre")]
        dbAppendTable(connection, Id(schema = "public", table = "formulario_7_bioensayo_comentario_intake"), tables$comments)
      }
    })
  }

  f7_delete_review_record <- function(intake_id, reason, deleted_by) {
    reason <- f7_clean_text(reason)[[1]]
    deleted_by <- f7_clean_text(deleted_by)[[1]]
    if (is.na(reason)) stop("El comentario de eliminación es obligatorio.")
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    dbWithTransaction(connection, {
      selected_header <- dbGetQuery(
        connection,
        "select intake_id, codigo_bioensayo, review_status from public.formulario_7_bioensayo_intake where intake_id = $1 for update",
        params = list(as.integer(intake_id))
      )
      if (nrow(selected_header) != 1) stop("No se encontró el registro seleccionado para eliminar.")
      audit_exists <- dbGetQuery(
        connection,
        "select to_regclass('public.formulario_7_bioensayo_eliminacion_audit') is not null as exists"
      )$exists[[1]]
      if (isTRUE(audit_exists)) {
        dbExecute(
          connection,
          "insert into public.formulario_7_bioensayo_eliminacion_audit (intake_id, codigo_bioensayo, review_status, eliminado_por, motivo_eliminacion) values ($1, $2, $3, nullif($4, ''), $5)",
          params = list(
            as.integer(selected_header$intake_id[[1]]),
            as.character(selected_header$codigo_bioensayo[[1]]),
            as.character(selected_header$review_status[[1]]),
            value_or_default(deleted_by, ""),
            reason
          )
        )
      }
      dbExecute(connection, "delete from public.formulario_7_bioensayo_resultado_intake where intake_id = $1", params = list(as.integer(intake_id)))
      dbExecute(connection, "delete from public.formulario_7_bioensayo_comentario_intake where intake_id = $1", params = list(as.integer(intake_id)))
      deleted <- dbExecute(connection, "delete from public.formulario_7_bioensayo_intake where intake_id = $1", params = list(as.integer(intake_id)))
      if (deleted != 1L) stop("No se eliminó el registro seleccionado.")
      selected_header
    })
  }

  observeEvent(input$open_dataset, {
    active_dataset(input$dataset_choice)
    submission_status("No se ha enviado ningún registro en esta sesión.")
  })

  observeEvent(input$open_bulk_upload, {
    showModal(modalDialog(
      title = "Subida de datos masiva",
      size = "l",
      easyClose = TRUE,
      p("Use esta opción para cargar múltiples registros de ovipostura/conteo de huevos desde un archivo CSV. Cada fila será guardada como registro pendiente para revisión."),
      tags$ol(
        tags$li("Descargue el machote CSV oficial."),
        tags$li("Complete una fila por observación. Mantenga los nombres de columnas sin cambios."),
        tags$li("Use fechas en formato YYYY-MM-DD, por ejemplo 2026-06-03."),
        tags$li("Use valores numéricos enteros para ciclo, ronda, cuadrante y conteos."),
        tags$li("Guarde el archivo como CSV y súbalo en esta ventana.")
      ),
      div(
        class = "alert alert-info",
        strong("Columnas esperadas: "),
        paste(egg_count_intake_columns, collapse = ", ")
      ),
      downloadButton("download_egg_count_template", "Descargar machote CSV", class = "btn-primary"),
      tags$hr(),
      fileInput(
        "bulk_upload_file",
        "Seleccione archivo CSV",
        accept = c(".csv", "text/csv", "text/comma-separated-values")
      ),
      actionButton("process_bulk_upload", "Validar y subir CSV", class = "btn-primary"),
      uiOutput("bulk_upload_status"),
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$open_individual_entry, {
    showModal(modalDialog(
      title = div(
        class = "modal-title-row",
        span("Ingreso individual de datos"),
        actionButton("close_individual_entry", HTML("&times;"), class = "modal-close-button")
      ),
      size = "l",
      easyClose = TRUE,
      individual_egg_count_form(),
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$close_individual_entry, {
    removeModal()
  })

  observeEvent(input$open_formulario_5_bulk_upload, {
    formulario_5_bulk_upload_result(NULL)
    showModal(modalDialog(
      title = "Formulario 5: subida de datos masiva",
      size = "l",
      easyClose = TRUE,
      p("Use esta opción para cargar múltiples registros de Formulario 5 desde un archivo CSV. Cada fila válida se guardará como registro pendiente para revisión."),
      tags$ol(
        tags$li("Descargue el machote CSV oficial de Formulario 5."),
        tags$li("Complete una fila por registro y no cambie los nombres de las columnas."),
        tags$li("Use fechas en formato YYYY-MM-DD, por ejemplo 2026-07-29."),
        tags$li("Use números enteros iguales o mayores que cero para los conteos."),
        tags$li("Guarde el archivo como CSV y súbalo en esta ventana.")
      ),
      div(
        class = "alert alert-info",
        strong("Columnas esperadas: "),
        paste(formulario_5_intake_columns, collapse = ", ")
      ),
      downloadButton("download_formulario_5_template", "Descargar machote CSV", class = "btn-primary"),
      tags$hr(),
      fileInput(
        "formulario_5_bulk_upload_file",
        "Seleccione archivo CSV",
        accept = c(".csv", "text/csv", "text/comma-separated-values")
      ),
      actionButton("process_formulario_5_bulk_upload", "Validar y subir CSV", class = "btn-primary"),
      uiOutput("formulario_5_bulk_upload_status"),
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$open_formulario_1_bulk_upload, {
    formulario_1_bulk_upload_result(NULL)
    showModal(modalDialog(
      title = "Formulario 1: subida de datos masiva",
      size = "l",
      easyClose = TRUE,
      p("Use esta opción para cargar múltiples registros de colocación y retiro de ovitrampas desde el machote CSV oficial."),
      tags$ol(
        tags$li("Descargue el machote oficial de Formulario 1."),
        tags$li("Complete una fila por ovitrampa/sustrato y no cambie los nombres de las columnas."),
        tags$li("Use fechas en formato YYYY-MM-DD y coordenadas en grados decimales."),
        tags$li("Use números enteros iguales o mayores que cero para ovitrampas retiradas y estados de retiro."),
        tags$li("Los registros válidos se guardarán con estado pending para revisión.")
      ),
      div(
        class = "alert alert-info",
        strong("Columnas esperadas: "),
        paste(formulario_1_intake_columns, collapse = ", ")
      ),
      downloadButton("download_formulario_1_template", "Descargar machote CSV", class = "btn-primary"),
      tags$hr(),
      fileInput(
        "formulario_1_bulk_upload_file",
        "Seleccione archivo CSV",
        accept = c(".csv", "text/csv", "text/comma-separated-values")
      ),
      actionButton("process_formulario_1_bulk_upload", "Validar y subir CSV", class = "btn-primary"),
      uiOutput("formulario_1_bulk_upload_status"),
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$open_formulario_1_entry, {
    f1_save_status(list(type = "idle", message = NULL, details = character()))
    f1_placement_status(list(type = "idle", message = NULL, details = character()))
    f1_reset_block_fields()
    showModal(modalDialog(
      title = div(
        class = "modal-title-row",
        span("Formulario 1: ingreso individual"),
        tagAppendAttributes(
          modalButton(HTML("&times;")),
          class = "modal-close-button",
          title = "Cerrar formulario",
          `aria-label` = "Cerrar formulario"
        )
      ),
      size = "l",
      easyClose = TRUE,
      formulario_1_capture_form(),
      footer = modalButton("Cerrar")
    ))
    session$onFlushed(function() {
      f1_reset_capture_inputs()
    }, once = TRUE)
  })

  observeEvent(input$close_formulario_1_entry, {
    removeModal()
  })

  observeEvent(input$f1_pais, {
    updateSelectInput(session, "f1_departamento", choices = ubicacion_departamento_choices(input$f1_pais), selected = "")
  }, ignoreInit = FALSE)

  output$f1_municipio_ui <- renderUI({
    country <- value_or_default(input$f1_pais, "")
    department_code <- value_or_default(input$f1_departamento, "")
    choices <- ubicacion_municipio_choices(country, department_code, include_manual = TRUE)
    tagList(
      selectInput("f1_municipio", "Municipio *", choices = choices),
      conditionalPanel(
        "input.f1_municipio == '__manual__'",
        textInput("f1_municipio_manual", "Código nacional de municipio *", placeholder = "Ej. 0201")
      )
    )
  })

  f1_capture_recommended_quadrant_code <- reactive({
    municipality_code <- ubicacion_codigo_manual_o_seleccion(input$f1_municipio, input$f1_municipio_manual)
    f1_new_quadrant_code(
      country = input$f1_pais,
      municipality_code = municipality_code,
      year = value_or_default(input$f1_fecha_colocacion, Sys.Date()),
      quadrant_number = input$f1_codigo_cuadrante_numero
    )
  })

  observeEvent(
    list(input$f1_pais, input$f1_municipio, input$f1_municipio_manual, input$f1_fecha_colocacion, input$f1_codigo_cuadrante_numero),
    {
      code <- f1_capture_recommended_quadrant_code()
      if (!is.na(code) && nzchar(code)) {
        updateTextInput(session, "f1_codigo_cuadrante_base", value = code)
      }
    },
    ignoreInit = TRUE
  )

  output$f1_codigo_cuadrante_preview <- renderUI({
    code <- f1_capture_recommended_quadrant_code()
    if (is.na(code) || !nzchar(code)) {
      return(div(class = "alert alert-info", "Seleccione país, municipio y fecha de colocación para sugerir el código nuevo."))
    }
    div(class = "summary-box", strong("Código sugerido: "), tags$code(code))
  })

  observeEvent(input$f1_print_pais, {
    updateSelectInput(session, "f1_print_departamento", choices = ubicacion_departamento_choices(input$f1_print_pais), selected = "")
  }, ignoreInit = FALSE)

  output$f1_print_municipio_ui <- renderUI({
    country <- value_or_default(input$f1_print_pais, "")
    department_code <- value_or_default(input$f1_print_departamento, "")
    choices <- ubicacion_municipio_choices(country, department_code, include_manual = TRUE)
    tagList(
      selectInput("f1_print_municipio", "Municipio", choices = choices),
      conditionalPanel(
        "input.f1_print_municipio == '__manual__'",
        textInput("f1_print_municipio_manual", "Código nacional de municipio", placeholder = "Ej. 0201")
      )
    )
  })

  f1_print_recommended_quadrant_code <- reactive({
    municipality_code <- ubicacion_codigo_manual_o_seleccion(input$f1_print_municipio, input$f1_print_municipio_manual)
    f1_new_quadrant_code(
      country = input$f1_print_pais,
      municipality_code = municipality_code,
      year = Sys.Date(),
      quadrant_number = input$f1_print_codigo_cuadrante_numero
    )
  })

  observeEvent(
    list(input$f1_print_pais, input$f1_print_municipio, input$f1_print_municipio_manual, input$f1_print_codigo_cuadrante_numero),
    {
      code <- f1_print_recommended_quadrant_code()
      if (!is.na(code) && nzchar(code)) {
        updateTextInput(session, "f1_print_codigo_cuadrante_base", value = code)
      }
    },
    ignoreInit = TRUE
  )

  output$f1_print_codigo_cuadrante_preview <- renderUI({
    code <- f1_print_recommended_quadrant_code()
    if (is.na(code) || !nzchar(code)) {
      return(div(class = "alert alert-info", "Seleccione país y municipio para generar el código de cuadrante de impresión."))
    }
    div(class = "summary-box", strong("Código de cuadrante generado: "), tags$code(code))
  })

  f1_print_recommended_form_code <- reactive({
    municipality_code <- ubicacion_codigo_manual_o_seleccion(input$f1_print_municipio, input$f1_print_municipio_manual)
    f1_new_form_code(
      country = input$f1_print_pais,
      municipality_code = municipality_code,
      year = Sys.Date(),
      ronda = input$f1_print_ronda,
      ciclo = input$f1_print_ciclo
    )
  })

  output$f1_print_codigo_formulario_preview <- renderUI({
    code <- f1_print_recommended_form_code()
    if (is.na(code) || !nzchar(code)) {
      return(div(class = "alert alert-info", "Complete país, municipio, ronda y ciclo para generar el código de formulario."))
    }
    div(class = "summary-box", strong("Código de formulario generado: "), tags$code(code))
  })

  output$f1_placement_status <- renderUI({
    status <- f1_placement_status()
    if (identical(status$type, "idle")) return(NULL)
    div(
      class = if (identical(status$type, "success")) "alert alert-success" else "alert alert-warning",
      strong(status$message),
      if (length(status$details)) tags$ul(lapply(status$details, tags$li))
    )
  })

  observeEvent(input$f1_confirm_placement, {
    details <- character()
    if (is.null(input$f1_fecha_registro) || is.na(input$f1_fecha_registro)) {
      details <- c(details, "Ingrese la fecha de ingreso formulario.")
    }
    if (!nzchar(trimws(value_or_default(input$f1_pais, "")))) {
      details <- c(details, "Seleccione el país.")
    }
    if (!nzchar(trimws(value_or_default(input$f1_departamento, "")))) {
      details <- c(details, "Seleccione el departamento.")
    }
    if (!nzchar(ubicacion_codigo_manual_o_seleccion(input$f1_municipio, input$f1_municipio_manual))) {
      details <- c(details, "Seleccione el municipio.")
    }
    if (!nzchar(trimws(value_or_default(input$f1_ciclo, "")))) {
      details <- c(details, "Ingrese el ciclo.")
    }
    if (!nzchar(trimws(value_or_default(input$f1_codigo_formulario, "")))) {
      details <- c(details, "Ingrese el código de formulario.")
    }

    if (length(details)) {
      f1_placement_status(list(type = "error", message = "Revise los datos generales antes de continuar.", details = details))
      return()
    }

    f1_placement_status(list(type = "idle", message = NULL, details = character()))
    updateTabsetPanel(session, "f1_capture_tab", selected = "Colocación")
  })

  observeEvent(input$f1_generate_quadrants, {
    details <- character()
    quadrants <- f5_integer(input$f1_num_quadrants)
    houses <- f5_integer(input$f1_casas_por_cuadrante)
    traps <- f5_integer(input$f1_ovitrampas_por_casa)

    if (is.null(input$f1_fecha_colocacion) || is.na(input$f1_fecha_colocacion)) {
      details <- c(details, "Ingrese la fecha de colocación.")
    }
    if (is.na(quadrants) || quadrants < 1) {
      details <- c(details, "Ingrese un número de cuadrantes mayor que cero.")
    }
    if (is.na(houses) || houses < 1) {
      details <- c(details, "Ingrese un número de casas por cuadrante mayor que cero.")
    }
    if (is.na(traps) || traps < 1 || traps > 8) {
      details <- c(details, "Ingrese entre 1 y 8 ovitrampas por casa.")
    }
    if (!f1_quadrant_code_has_structure(input$f1_codigo_cuadrante_base, input$f1_pais) &&
        !f1_code_has_counter(input$f1_codigo_cuadrante_base)) {
      details <- c(details, "Código cuadrante debe tener un correlativo para incrementar. Puede usar el formato nuevo REI25GT0503C001 o un formato anterior con letras seguidas de dígitos.")
    }
    if (!f1_code_has_counter(input$f1_codigo_casa_base)) {
      details <- c(details, "Código casa debe tener letras seguidas de dígitos, por ejemplo HS010.")
    }
    if (!f1_code_has_counter(input$f1_codigo_sustrato_base)) {
      details <- c(details, "Código sustrato debe tener letras seguidas de dígitos, por ejemplo GT001.")
    }

    if (length(details)) {
      f1_placement_status(list(
        type = "error", message = "Revise la configuración de colocación.", details = details
      ))
      return()
    }

    f1_quadrant_config(list(
      quadrants = quadrants,
      houses_per_quadrant = houses,
      traps_per_house = traps
    ))
    f1_generated_quadrants(seq_len(quadrants))
    f1_editable_quadrants(integer())
    f1_placement_status(list(
      type = "success",
      message = paste0("Se generaron ", quadrants, " tab(s) de cuadrante."),
      details = character()
    ))
    updateTabsetPanel(session, "f1_capture_tab", selected = "Cuadrantes")
  })

  lapply(seq_len(50), function(quadrant_index) {
    local({
      q <- quadrant_index
      observeEvent(input[[paste0("f1_toggle_edit_quadrant_", q)]], {
        qcount <- f1_quadrant_count()
        if (is.null(qcount) || q > qcount) return()
        editable <- f1_editable_quadrants()
        if (q %in% editable) {
          f1_editable_quadrants(setdiff(editable, q))
        } else {
          f1_editable_quadrants(sort(unique(c(editable, q))))
        }
      }, ignoreInit = TRUE)
    })
  })

  f1_reset_block_fields <- function() {
    f1_confirmed_ovitrampa_count(NULL)
    f1_quadrant_config(NULL)
    f1_generated_quadrants(integer())
    f1_editable_quadrants(integer())
    f1_placement_status(list(type = "idle", message = NULL, details = character()))
    f1_resume_status(list(type = "idle", message = NULL, details = character()))
  }

  f1_reset_capture_inputs <- function() {
    updateSelectInput(session, "f1_pais", selected = "")
    updateSelectInput(session, "f1_departamento", choices = c("Seleccione país" = ""), selected = "")
    updateSelectInput(session, "f1_municipio", selected = "")
    updateTextInput(session, "f1_municipio_manual", value = "")
    updateTextInput(session, "f1_id_institucion", value = value_or_default(user_profile$institution, default_institution_id))
    for (input_id in c(
      "f1_ciclo", "f1_ronda", "f1_codigo_formulario",
      "f1_codigo_gps", "f1_fuente_formulario", "f1_creado_por",
      "f1_grupo_responsable_colocacion", "f1_codigo_cuadrante_base",
      "f1_codigo_casa_base", "f1_codigo_sustrato_base", "f1_grupo_responsable_retiro",
      "f1_resume_codigo_formulario"
    )) {
      updateTextInput(session, input_id, value = "")
    }
    for (input_id in c("f1_fecha_registro", "f1_fecha_colocacion", "f1_fecha_retiro")) {
      updateDateInput(session, input_id, value = as.Date(character(0)))
    }
    for (input_id in c(
      "f1_Latitud", "f1_Longitud", "f1_num_quadrants",
      "f1_casas_por_cuadrante", "f1_ovitrampas_por_casa"
    )) {
      updateNumericInput(session, input_id, value = NA)
    }
    f1_reset_block_fields()
    updateTabsetPanel(session, "f1_capture_tab", selected = "Datos generales")
  }

  observeEvent(input$open_formulario_5_entry, {
    f5_capture_step("metadatos")
    f5_certification_complete(FALSE)
    f5_certification_panel("closed")
    f5_certification_alerts(character())
    f5_save_status(list(type = "idle", message = NULL, details = character()))
    show_formulario_5_modal()
    session$onFlushed(function() {
      updateTabsetPanel(session, "f5_capture_tab", selected = "metadatos")
    }, once = TRUE)
  })

  observeEvent(input$open_formulario_7_bulk_upload, {
    formulario_7_bulk_upload_result(NULL)
    showModal(modalDialog(
      title = "Formulario 7: subida de datos masiva",
      size = "l",
      easyClose = TRUE,
      p("Cada fila del machote representa un bioensayo. La aplicación transformará las lecturas planas en registros relacionados por botella y tiempo."),
      tags$ol(
        tags$li("Descargue y complete el machote oficial sin cambiar los nombres ni el orden de sus 111 columnas visibles."),
        tags$li("Use fechas YYYY-MM-DD, horas HH:MM, y true/false para campos lógicos."),
        tags$li("Cada lectura debe incluir juntos los conteos de vivos e incapacitados."),
        tags$li("Los registros válidos se guardarán con estado pending para revisión.")
      ),
      downloadButton("download_formulario_7_template", "Descargar machote CSV", class = "btn-primary"),
      tags$hr(),
      fileInput("formulario_7_bulk_upload_file", "Seleccione archivo CSV", accept = c(".csv", "text/csv", "text/comma-separated-values")),
      actionButton("process_formulario_7_bulk_upload", "Validar y subir CSV", class = "btn-primary"),
      uiOutput("formulario_7_bulk_upload_status"),
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$open_formulario_7_print, {
    showModal(modalDialog(
      title = div(
        class = "modal-title-row",
        span("Imprimir Formulario 7"),
        actionButton("close_formulario_7_print", HTML("&times;"), class = "modal-close-button")
      ),
      size = "l",
      easyClose = TRUE,
      formulario_7_print_form(),
      footer = modalButton("Cerrar")
    ))
    session$onFlushed(function() {
      updateSelectInput(
        session,
        "f7_print_codigo_bioensayo_departamento",
        choices = c("Seleccione" = "", f7_print_department_choices("El Salvador")),
        selected = ""
      )
    }, once = TRUE)
  })

  observeEvent(input$close_formulario_7_print, removeModal())

  observeEvent(input$open_formulario_7_entry, {
    f7_save_status(list(type = "idle", message = NULL, details = character()))
    f7_capture_step("informacion_general")
    f7_unlocked_step(length(f7_capture_steps))
    f7_navigation_status(list(type = "idle", message = NULL, details = character()))
    showModal(modalDialog(
      title = div(
        class = "modal-title-row",
        span("Formulario 7: ingreso individual"),
        tagAppendAttributes(
          modalButton(HTML("&times;")),
          class = "modal-close-button",
          title = "Cerrar formulario",
          `aria-label` = "Cerrar formulario"
        )
      ),
      size = "l", easyClose = FALSE,
      formulario_7_capture_form(),
      footer = NULL
    ))
    initially_allowed_steps <- f7_capture_steps[seq_len(f7_unlocked_step())]
    session$onFlushed(function() {
      session$sendCustomMessage("setF7TabAccess", list(allowed = initially_allowed_steps))
      session$sendCustomMessage("setF7BottleLabels", list(mode = "Completa"))
      updateTabsetPanel(session, "f7_capture_tab", selected = "informacion_general")
    }, once = TRUE)
  })

  observeEvent({
    list(input$f7_tipo_bioensayo, input$f7_bioensayo_intensidad)
  }, {
    bottle_mode <- if (identical(input$f7_tipo_bioensayo, "intensidad") && identical(input$f7_bioensayo_intensidad, "Exploratorio")) "Exploratorio" else "Completa"
    session$sendCustomMessage("setF7BottleLabels", list(mode = bottle_mode))
  }, ignoreInit = TRUE)

  formulario_7_input_row <- reactive({
    values <- setNames(rep(list(NA_character_), length(formulario_7_intake_columns)), formulario_7_intake_columns)
    values$formulario_codigo <- "F7"
    values$formulario_nombre <- "Registro de datos del bioensayo de la botella CDC"
    for (column in setdiff(formulario_7_intake_columns, c("formulario_codigo", "formulario_nombre", "creado_en", "actualizado_en"))) {
      input_value <- input[[paste0("f7_", column)]]
      if (!is.null(input_value) && length(input_value)) values[[column]] <- as.character(input_value[[1]])
    }
    selected_bioassay_type <- value_or_default(input$f7_tipo_bioensayo, "diagnostica_1x")
    selected_modality <- if (identical(selected_bioassay_type, "intensidad")) value_or_default(input$f7_bioensayo_intensidad, "Exploratorio") else NA_character_
    selected_synergist <- if (identical(selected_bioassay_type, "sinergistas")) value_or_default(input$f7_sinergista_tipo, NA_character_) else NA_character_
    diagnostic_result <- if (selected_bioassay_type %in% c("diagnostica_1x", "intensidad", "sinergistas")) value_or_default(input$f7_resultado_diagnostico, NA_character_) else NA_character_
    intensity_requires_dose <- identical(selected_bioassay_type, "intensidad") &&
      (identical(selected_modality, "Completa") || (identical(selected_modality, "Exploratorio") && diagnostic_result %in% c("Sospecha de Resistencia", "Resistente")))
    if (identical(value_or_default(input$f7_codigo_municipio, ""), "__manual__")) {
      values$codigo_municipio <- gsub("[^0-9]+", "", value_or_default(input$f7_codigo_municipio_manual, ""))
    }
    values$bioensayo_intensidad <- selected_modality
    values$bioensayo_diagnostica_1x <- as.character(identical(selected_bioassay_type, "diagnostica_1x"))
    values$dosis_intensidad <- if (intensity_requires_dose) input$f7_dosis_intensidad else NA_character_
    values$sinergista_tipo <- selected_synergist
    values$sinergista_def <- as.character(identical(selected_synergist, "DEF"))
    values$sinergista_pbo <- as.character(identical(selected_synergist, "PBO"))
    values$sinergista_dm <- as.character(identical(selected_synergist, "DM"))
    values$resultado_diagnostico <- diagnostic_result
    values$nombre_quien_ingreso <- value_or_default(input$f7_creado_por, user_profile$name)
    values$codigo_bioensayo <- formulario_7_codigo_bioensayo_final(
      values$codigo_bioensayo, values$bioensayo_diagnostica_1x, values$bioensayo_intensidad, values$dosis_intensidad,
      values$sinergista_def, values$sinergista_pbo, values$sinergista_dm
    )
    if (!identical(selected_bioassay_type, "sinergistas")) {
      synergist_results <- grep("resultado_60min_", formulario_7_result_columns, value = TRUE)
      values[synergist_results] <- NA_character_
      values$sinergista_tipo <- NA_character_
      values$dosis_sinergista_ug_ml <- NA_character_
    }
    if (formulario_7_is_temefos(values$insecticida)) {
      values[formulario_7_non_24h_result_columns] <- NA_character_
    }
    if (is.na(f7_clean_text(values$codigo_control_calidad)[[1]])) values$codigo_control_calidad <- "NO APLICA"
    values$edad_indefinida <- as.character(isTRUE(input$f7_edad_indefinida))
    values$generacion_filial_indefinida <- as.character(isTRUE(input$f7_generacion_filial_indefinida))
    as.data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
  })

  f7_step_errors <- function(step, row) {
    errors <- character()
    value <- function(column) f7_clean_text(row[[column]])[[1]]
    missing_value <- function(column) is.na(value(column))
    valid_time <- function(column) {
      current <- value(column)
      !is.na(current) && grepl("^([01][0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$", current)
    }
    require_fields <- function(columns, labels = columns) {
      for (index in seq_along(columns)) {
        if (missing_value(columns[[index]])) errors <<- c(errors, paste0("Complete: ", labels[[index]], "."))
      }
    }

    if (identical(step, "informacion_general")) {
      require_fields(
        c("codigo_bioensayo", "pais", "codigo_departamento", "codigo_municipio", "nombre_poblacion", "fecha_registro"),
        c("código de bioensayo", "país", "departamento", "municipio", "nombre de la población", "fecha de registro")
      )
      if (input$f7_tipo_bioensayo %in% c("diagnostica_1x", "intensidad", "sinergistas")) {
        if (missing_value("resultado_diagnostico")) errors <- c(errors, "Seleccione el resultado: Suceptible, Sospecha de Resistencia o Resistente.")
      }
      if (identical(input$f7_tipo_bioensayo, "intensidad")) {
        if (missing_value("bioensayo_intensidad")) errors <- c(errors, "Seleccione Intensidad Exploratorio o Completa.")
        if (identical(input$f7_bioensayo_intensidad, "Completa") && missing_value("dosis_intensidad")) errors <- c(errors, "Seleccione la concentración de intensidad 1X, 2X, 5X o 10X.")
        if (identical(input$f7_bioensayo_intensidad, "Exploratorio") && value("resultado_diagnostico") %in% c("Sospecha de Resistencia", "Resistente") && missing_value("dosis_intensidad")) {
          errors <- c(errors, "Seleccione la concentración asociada a la resistencia o sospecha.")
        }
      }
      if (identical(input$f7_tipo_bioensayo, "sinergistas")) {
        if (missing_value("sinergista_tipo")) errors <- c(errors, "Seleccione el sinergista: DEF, PBO o DM.")
        if (missing_value("dosis_sinergista_ug_ml")) errors <- c(errors, "Indique la dosis del sinergista.")
        synergist_dose <- suppressWarnings(as.numeric(value("dosis_sinergista_ug_ml")))
        if (!missing_value("dosis_sinergista_ug_ml") && (is.na(synergist_dose) || synergist_dose < 0)) errors <- c(errors, "La dosis del sinergista debe ser un número igual o mayor que cero.")
      }
    }

    if (identical(step, "informacion_bioensayo")) {
      require_fields(
        c("fecha_realizacion_bioensayo", "insecticida", "solvente_utilizado", "dosis_intensidad_ug_ml", "lote_insecticida", "fecha_revestimiento_botellas"),
        c("fecha de realización", "código de insecticida", "solvente utilizado", "dosis", "código de dosis", "fecha de revestimiento")
      )
      if (identical(value("solvente_utilizado"), "Otro") && missing_value("solvente_otro")) errors <- c(errors, "Especifique el otro solvente utilizado.")
      dose <- suppressWarnings(as.numeric(value("dosis_intensidad_ug_ml")))
      if (!is.na(value("dosis_intensidad_ug_ml")) && (is.na(dose) || dose < 0)) errors <- c(errors, "La dosis debe ser un número igual o mayor que cero.")
    }

    if (identical(step, "material_responsables")) {
      require_fields(
        c("origen_material", "codigo_especie_mosquito", "hora_separacion", "fecha_separacion", "codigo_responsable_revestimiento", "codigo_responsable_bioensayo"),
        c("origen del material", "código de especie", "hora de separación", "fecha de separación", "responsable de revestimiento", "responsable del bioensayo")
      )
      require_fields("codigo_revision_24h", "revisión a 24 horas")
      if (!isTRUE(input$f7_edad_indefinida) && missing_value("edad_dias")) errors <- c(errors, "Indique la edad en días o marque Edad indefinida.")
      if (!isTRUE(input$f7_generacion_filial_indefinida) && missing_value("generacion_filial")) errors <- c(errors, "Indique la generación filial o márquela como indefinida.")
      if (!missing_value("hora_separacion") && !valid_time("hora_separacion")) errors <- c(errors, "La hora de separación debe usar HH:MM.")
    }

    if (identical(step, "condiciones")) {
      require_fields(
        c("temperatura_inicial_c", "temperatura_final_c", "humedad_relativa_inicial_pct", "humedad_relativa_final_pct", "hora_inicio_bioensayo", "hora_final_bioensayo"),
        c("temperatura inicial", "temperatura final", "humedad inicial", "humedad final", "hora de inicio", "hora final")
      )
      for (column in c("temperatura_inicial_c", "temperatura_final_c")) {
        if (!missing_value(column) && is.na(suppressWarnings(as.numeric(value(column))))) errors <- c(errors, paste0(column, " debe ser numérico."))
      }
      for (column in c("humedad_relativa_inicial_pct", "humedad_relativa_final_pct")) {
        humidity <- suppressWarnings(as.numeric(value(column)))
        if (!missing_value(column) && (is.na(humidity) || humidity < 0 || humidity > 100)) errors <- c(errors, "La humedad relativa debe estar entre 0 y 100%.")
      }
      for (column in c("hora_inicio_bioensayo", "hora_final_bioensayo")) {
        if (!missing_value(column) && !valid_time(column)) errors <- c(errors, paste0(column, " debe usar HH:MM."))
      }
    }

    validate_readings <- function(columns, hour_columns, section_label) {
      count_columns <- grep("_(vivos|incapacitados)$", columns, value = TRUE)
      pair_bases <- unique(sub("_(vivos|incapacitados)$", "", count_columns))
      for (base in pair_bases) {
        lives <- value(paste0(base, "_vivos"))
        disabled <- value(paste0(base, "_incapacitados"))
        if (xor(is.na(lives), is.na(disabled))) errors <<- c(errors, paste0(section_label, ": complete juntos vivos e incapacitados para ", base, "."))
        for (count in c(lives, disabled)) {
          parsed <- suppressWarnings(as.numeric(count))
          if (!is.na(count) && (is.na(parsed) || parsed < 0 || parsed != floor(parsed))) errors <<- c(errors, paste0(section_label, ": los conteos deben ser enteros no negativos."))
        }
      }
      for (column in hour_columns) {
        if (!missing_value(column) && !valid_time(column)) errors <<- c(errors, paste0(section_label, ": ", column, " debe usar HH:MM."))
      }
    }

    if (identical(step, "resultados")) {
      validate_readings(
        formulario_7_result_columns,
        grep("hora_", formulario_7_result_columns, value = TRUE),
        "Resultados"
      )
    }
    unique(errors)
  }

  sync_f7_tab_access <- function() {
    session$sendCustomMessage("setF7TabAccess", list(allowed = f7_capture_steps))
  }

  observeEvent(f7_unlocked_step(), sync_f7_tab_access(), ignoreInit = TRUE)

  observeEvent(input$f7_capture_tab, {
    requested_index <- match(input$f7_capture_tab, f7_capture_steps)
    if (is.na(requested_index)) return()
    f7_capture_step(input$f7_capture_tab)
    f7_navigation_status(list(type = "idle", message = NULL, details = character()))
  }, ignoreInit = TRUE)

  output$f7_navigation_status <- renderUI({
    status <- f7_navigation_status()
    if (identical(status$type, "idle")) return(NULL)
    div(
      class = if (identical(status$type, "success")) "alert alert-success" else "alert alert-warning",
      strong(status$message),
      if (length(status$details)) tags$ul(lapply(status$details, tags$li))
    )
  })

  output$f7_navigation_controls <- renderUI({
    current_index <- match(f7_capture_step(), f7_capture_steps)
    div(
      class = "f7-navigation-row",
      if (current_index > 1) actionButton("f7_previous_step", "Anterior"),
      if (current_index < length(f7_capture_steps)) actionButton("f7_next_step", "Seguir", class = "btn-primary")
    )
  })

  observeEvent(input$f7_previous_step, {
    current_index <- match(f7_capture_step(), f7_capture_steps)
    if (!is.na(current_index) && current_index > 1) {
      previous_step <- f7_capture_steps[[current_index - 1]]
      f7_capture_step(previous_step)
      f7_navigation_status(list(type = "idle", message = NULL, details = character()))
      updateTabsetPanel(session, "f7_capture_tab", selected = previous_step)
    }
  })

  observeEvent(input$f7_next_step, {
    current_step <- f7_capture_step()
    current_index <- match(current_step, f7_capture_steps)
    errors <- f7_step_errors(current_step, formulario_7_input_row())
    if (length(errors)) {
      f7_navigation_status(list(type = "error", message = "Complete esta sección antes de continuar.", details = errors))
      return()
    }
    if (!is.na(current_index) && current_index < length(f7_capture_steps)) {
      next_index <- current_index + 1L
      f7_unlocked_step(max(f7_unlocked_step(), next_index))
      next_step <- f7_capture_steps[[next_index]]
      f7_capture_step(next_step)
      f7_navigation_status(list(type = "idle", message = NULL, details = character()))
      sync_f7_tab_access()
      updateTabsetPanel(session, "f7_capture_tab", selected = next_step)
    }
  })

  output$f7_save_status <- renderUI({
    status <- f7_save_status()
    if (identical(status$type, "idle")) return(NULL)
    div(
      class = if (identical(status$type, "success")) "alert alert-success" else "alert alert-warning",
      strong(status$message),
      if (length(status$details)) tags$ul(lapply(status$details, tags$li))
    )
  })

  observeEvent(input$save_formulario_7, {
    validated <- validate_formulario_7(formulario_7_input_row())
    if (length(validated$details)) {
      f7_save_status(list(type = "error", message = "Revise los campos del Formulario 7.", details = validated$details))
      return()
    }
    connection <- NULL
    tryCatch({
      connection <- connect_to_supabase()
      existing_codes <- formulario_7_existing_unique_codes(connection, validated$data$codigo_bioensayo)
      if (length(existing_codes)) {
        f7_save_status(list(
          type = "error",
          message = "No se guardó el Formulario 7 porque el código de bioensayo ya existe en Supabase.",
          details = paste0("Código de bioensayo repetido: ", existing_codes)
        ))
        return()
      }
      intake_ids <- insert_formulario_7(connection, validated$data)
      f7_save_status(list(type = "success", message = paste0("Registro guardado con intake_id ", intake_ids[[1]], " y estado pending."), details = character()))
      submission_status(paste0("Formulario 7 guardado con intake_id ", intake_ids[[1]], ". Estado de revisión: pending."))
    }, error = function(error) {
      f7_save_status(list(type = "error", message = "No se pudo guardar el Formulario 7 en Supabase.", details = conditionMessage(error)))
    }, finally = {
      if (!is.null(connection)) dbDisconnect(connection)
    })
  })

  output$f7_review_status_message <- renderUI({
    status <- f7_review_status()
    if (is.null(status$type) || identical(status$type, "idle")) return(NULL)
    selected <- f7_review_selected()
    detail_scoped_messages <- c(
      "Revise los valores editados.",
      "No se pudieron guardar los cambios.",
      "No se puede eliminar el registro sin comentario.",
      "No se pudo eliminar el registro.",
      "Registro %s eliminado definitivamente.",
      "No se pudo confirmar el registro.",
      "Cambios guardados para intake_id"
    )
    if (!is.null(selected) && any(startsWith(status$message, detail_scoped_messages))) return(NULL)
    css <- switch(status$type, success = "alert alert-success", error = "alert alert-danger", warning = "alert alert-warning", "alert alert-info")
    div(class = css, strong(status$message), if (length(status$details)) tags$ul(lapply(status$details, tags$li)))
  })

  output$f7_review_detail_status_message <- renderUI({
    selected <- f7_review_selected()
    if (is.null(selected)) return(NULL)
    status <- f7_review_status()
    if (is.null(status$type) || identical(status$type, "idle")) return(NULL)
    css <- switch(status$type, success = "alert alert-success", error = "alert alert-danger", warning = "alert alert-warning", "alert alert-info")
    div(class = css, strong(status$message), if (length(status$details)) tags$ul(lapply(status$details, tags$li)))
  })

  output$f7_review_record_list <- renderUI({
    records <- f7_review_records()
    if (is.null(records) || nrow(records) == 0) return(NULL)
    rows <- lapply(seq_len(nrow(records)), function(index) {
      record <- records[index, ]
      intake_id <- as.character(record$intake_id[[1]])
      tags$tr(
        tags$td(tags$a(
          href = "#",
          onclick = sprintf("Shiny.setInputValue('f7_review_select_id', '%s', {priority: 'event'}); return false;", intake_id),
          intake_id
        )),
        tags$td(f5_review_text_value(record$codigo_bioensayo)),
          tags$td(as.character(record$fecha_registro[[1]])),
          tags$td(review_datetime_gt_value(record$creado_en)),
          tags$td(f5_review_text_value(record$pais)),
          tags$td(f5_review_text_value(record$nombre_poblacion)),
          tags$td(f5_review_text_value(record$nombre_quien_ingreso)),
          tags$td(f5_review_text_value(record$review_status))
        )
      })
    tagList(
      h4("Listado para revisión"),
      tags$table(
        class = "table table-striped table-condensed",
        tags$thead(tags$tr(
          tags$th("intake_id"), tags$th("Código bioensayo"), tags$th("Fecha formulario"), tags$th("Creado en GT"),
          tags$th("País"), tags$th("Población"), tags$th("Ingresado por"), tags$th("Estado")
        )),
        tags$tbody(rows)
      )
    )
  })

  output$f7_review_record_detail <- renderUI({
    selected <- f7_review_selected()
    if (is.null(selected) || is.null(selected$header) || nrow(selected$header) == 0) return(NULL)
    row <- selected$data
    header <- selected$header
    edit_mode <- f7_review_edit_mode()
    delete_mode <- f7_review_delete_mode()
    is_temefos_record <- formulario_7_is_temefos(row$insecticida[[1]])

    value_for <- function(field) {
      value <- row[[field]][[1]]
      if (is.null(value) || length(value) == 0 || is.na(value)) return("")
      as.character(value)
    }
    input_for_field <- function(field) {
      label <- f7_review_field_label(field)
      value <- value_for(field)
      if (is_temefos_record && field %in% formulario_7_non_24h_result_columns) {
        return(div(class = "form-group", tags$label(label), tags$p(class = "form-control-static", "No aplica para Temefos")))
      }
      if (field %in% f7_review_protected_fields) {
        return(div(class = "form-group", tags$label(label), tags$p(class = "form-control-static", if (nzchar(value)) value else "—")))
      }
      input_id <- f7_review_input_id(field)
      if (field %in% f7_review_boolean_fields) {
        selected_value <- if (tolower(value) %in% c("true", "1")) "true" else "false"
        return(selectInput(input_id, label, choices = c("Sí" = "true", "No" = "false"), selected = selected_value))
      }
      choices <- switch(
        field,
        pais = c("El Salvador", "Guatemala"),
        bioensayo_intensidad = c("No aplica" = "", "Exploratorio" = "Exploratorio", "Completa" = "Completa"),
        dosis_intensidad = c("Vacío" = "", "1X" = "1X", "2X" = "2X", "5X" = "5X", "10X" = "10X"),
        resultado_diagnostico = c("No aplica" = "", "Suceptible" = "Suceptible", "Sospecha de Resistencia" = "Sospecha de Resistencia", "Resistente" = "Resistente"),
        solvente_utilizado = c("Etanol", "Otro"),
        origen_material = c("Silvestre", "Laboratorio"),
        NULL
      )
      if (!is.null(choices)) {
        selected_choice <- if (value %in% unname(choices)) value else ""
        return(selectInput(input_id, label, choices = choices, selected = selected_choice))
      }
      if (field %in% f7_review_date_fields) return(dateInput(input_id, label, value = suppressWarnings(as.Date(value))))
      if (field %in% f7_review_numeric_fields) return(numericInput(input_id, label, value = suppressWarnings(as.numeric(value)), min = 0))
      if (grepl("^comentario_[1-4]$", field)) return(textAreaInput(input_id, label, value = value, rows = 3))
      textInput(input_id, label, value = value)
    }

    section_names <- c(
      "Información general", "Información del bioensayo", "Material y responsables",
      "Condiciones", "Resultados por botella", "Comentarios y auditoría"
    )
    tabs <- lapply(section_names, function(section) {
      fields <- formulario_7_intake_columns[vapply(formulario_7_intake_columns, f7_review_section, character(1)) == section]
      group_count <- min(3L, length(fields))
      groups <- split(fields, cut(seq_along(fields), breaks = group_count, labels = FALSE))
      tabPanel(
        section,
        tags$fieldset(
          disabled = if (edit_mode) NULL else "disabled",
          fluidRow(lapply(groups, function(group) column(12 / length(groups), lapply(group, input_for_field))))
        )
      )
    })

    wellPanel(
      h4(sprintf("Formulario 7 — intake_id %s", as.character(header$intake_id[[1]]))),
      p(tags$strong("Estado: "), header$review_status[[1]], " · ", tags$strong("Código bioensayo: "), header$codigo_bioensayo[[1]]),
      uiOutput("f7_review_detail_status_message"),
      if (edit_mode) div(class = "alert alert-warning", "Modo edición activo. Al guardar, el registro volverá a estado pending hasta que sea confirmado."),
      do.call(tabsetPanel, c(list(id = "f7_review_detail_tabs"), tabs)),
      tags$hr(),
      fluidRow(
        column(6, textInput("f7_reviewed_by", "Revisado por", value = user_profile$name)),
        column(6, textAreaInput("f7_review_notes", "Notas de revisión", value = f5_review_text_value(header$review_notes), rows = 2))
      ),
      div(
        class = "submit-row",
        if (!edit_mode) actionButton("f7_review_confirm", "Confirmar registro", class = "btn-primary"),
        if (!edit_mode) actionButton("f7_review_enable_edit", "Editar", class = "btn-default"),
        if (edit_mode) actionButton("f7_review_save_changes", "Guardar cambios", class = "btn-primary"),
        if (edit_mode) actionButton("f7_review_cancel_edit", "Cancelar edición", class = "btn-default")
      ),
      tags$hr(),
      div(
        class = "f7-review-delete-zone",
        if (!delete_mode) {
          actionButton("f7_review_request_delete", "Eliminar registro", class = "btn-danger")
        } else {
          tagList(
            div(
              class = "alert alert-danger",
              tags$strong("¿Está seguro que desea eliminar este registro?"),
              tags$p("No hay vuelta atrás. Luego de su eliminación, este registro y sus lecturas serán borrados de la base de datos.")
            ),
            textAreaInput(
              "f7_review_delete_reason",
              "Comentario obligatorio: indique por qué se elimina este registro",
              value = "",
              rows = 3
            ),
            div(
              class = "submit-row",
              actionButton("f7_review_delete_confirm", "Sí, eliminar definitivamente", class = "btn-danger"),
              actionButton("f7_review_delete_cancel", "Cancelar eliminación", class = "btn-default")
            )
          )
        }
      )
    )
  })

  observeEvent(input$close_formulario_5_entry, {
    removeModal()
  })

  observeEvent(input$open_formulario_5_review, {
    f5_review_records(data.frame())
    f5_review_selected(NULL)
    f5_review_comparison(NULL)
    f5_review_delete_mode(FALSE)
    f5_review_status(list(type = "idle", message = NULL, details = character()))
    show_formulario_5_review_modal()
    updateTextInput(session, "f5_review_exclude_submitter", value = value_or_default(user_profile$name, ""))
  })

  observeEvent(input$close_formulario_5_review, {
    removeModal()
  })

  f7_select_review_record <- function(intake_id) {
    record <- f7_fetch_review_record(intake_id)
    if (is.null(record)) stop(sprintf("No se encontró intake_id %s.", intake_id))
    f7_review_selected(record)
    f7_review_edit_mode(FALSE)
    record
  }

  observeEvent(input$open_formulario_7_review, {
    f7_review_records(data.frame())
    f7_review_selected(NULL)
    f7_review_edit_mode(FALSE)
    f7_review_delete_mode(FALSE)
    f7_review_status(list(type = "idle", message = NULL, details = character()))
    show_formulario_7_review_modal()
    updateTextInput(session, "f7_review_exclude_submitter", value = value_or_default(user_profile$name, ""))
  })

  observeEvent(input$close_formulario_7_review, removeModal())

  observeEvent(input$f7_review_generate_sample, {
    f7_review_status(list(type = "info", message = "Generando muestra aleatoria del 10%...", details = character()))
    f7_review_records(data.frame())
    f7_review_selected(NULL)
    f7_review_edit_mode(FALSE)
    f7_review_delete_mode(FALSE)
    tryCatch({
      records <- withProgress(message = "Generando muestra 10%", value = 0, {
        incProgress(0.4, detail = "Consultando registros elegibles")
        sample_records <- f7_load_review_records(random_sample = TRUE)
        incProgress(0.6, detail = "Preparando listado")
        sample_records
      })
      f7_review_records(records)
      f7_review_status(list(
        type = if (nrow(records)) "success" else "warning",
        message = if (nrow(records)) sprintf("Muestra generada: %s registro(s), equivalente al 10%% del rango seleccionado.", nrow(records)) else "No hay registros en el rango y estado seleccionados.",
        details = character()
      ))
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudo generar la muestra.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f7_review_refresh, {
    tryCatch({
      records <- f7_load_review_records()
      f7_review_records(records)
      f7_review_delete_mode(FALSE)
      f7_review_status(list(
        type = if (nrow(records)) "success" else "warning",
        message = if (nrow(records)) {
          sprintf(
            "Se cargaron %s registro(s) entre %s y %s.",
            nrow(records),
            as.character(as.Date(input$f7_review_start_date)),
            as.character(as.Date(input$f7_review_end_date))
          )
        } else {
          sprintf(
            "No hay registros con ese estado entre %s y %s.",
            as.character(as.Date(input$f7_review_start_date)),
            as.character(as.Date(input$f7_review_end_date))
          )
        },
        details = character()
      ))
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudo actualizar el listado.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f7_review_find, {
    code <- f7_clean_text(input$f7_review_search_code)[[1]]
    if (is.na(code)) {
      f7_review_status(list(type = "warning", message = "Ingrese un Código de bioensayo válido.", details = character()))
      return()
    }
    tryCatch({
      record <- f7_fetch_review_record_by_code(code)
      if (is.null(record)) stop(sprintf("No se encontró el Código de bioensayo %s.", code))
      f7_review_selected(record)
      f7_review_edit_mode(FALSE)
      f7_review_delete_mode(FALSE)
      f7_review_status(list(
        type = "success",
        message = sprintf(
          "Código de bioensayo %s abierto para revisión. Estado actual: %s.",
          record$header$codigo_bioensayo[[1]],
          record$header$review_status[[1]]
        ),
        details = character()
      ))
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudo abrir el registro.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f7_review_select_id, {
    intake_id <- f5_integer(input$f7_review_select_id)
    if (is.na(intake_id)) return()
    tryCatch({
      f7_select_review_record(intake_id)
      f7_review_delete_mode(FALSE)
      f7_review_status(list(type = "success", message = sprintf("Registro %s abierto para revisión.", intake_id), details = character()))
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudo abrir el registro.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f7_review_enable_edit, {
    f7_review_delete_mode(FALSE)
    f7_review_edit_mode(TRUE)
  })
  observeEvent(input$f7_review_cancel_edit, f7_review_edit_mode(FALSE))

  observeEvent(input$f7_review_request_delete, {
    if (is.null(f7_review_selected())) return()
    f7_review_edit_mode(FALSE)
    f7_review_delete_mode(TRUE)
  })

  observeEvent(input$f7_review_delete_cancel, {
    f7_review_delete_mode(FALSE)
    updateTextAreaInput(session, "f7_review_delete_reason", value = "")
  })

  observeEvent(input$f7_review_save_changes, {
    selected <- f7_review_selected()
    if (is.null(selected)) return()
    f7_review_status(list(type = "info", message = "Guardando cambios del Formulario 7...", details = character()))
    validated <- validate_formulario_7(f7_review_input_row())
    if (length(validated$details)) {
      f7_review_status(list(type = "error", message = "Revise los valores editados.", details = validated$details))
      return()
    }
    intake_id <- as.integer(selected$header$intake_id[[1]])
    tryCatch({
      withProgress(message = "Guardando cambios", value = 0, {
        incProgress(0.25, detail = "Validando datos editados")
        f7_update_review_record(intake_id, validated$data)
        incProgress(0.40, detail = "Recargando el registro")
        f7_select_review_record(intake_id)
        incProgress(0.20, detail = "Actualizando el listado")
        refreshed_records <- tryCatch(f7_load_review_records(), error = function(error) NULL)
        if (!is.null(refreshed_records)) f7_review_records(refreshed_records)
        incProgress(0.15, detail = "Cambios guardados")
      })
      f7_review_edit_mode(FALSE)
      f7_review_delete_mode(FALSE)
      f7_review_status(list(type = "success", message = sprintf("Cambios guardados para intake_id %s. Estado: pending.", intake_id), details = character()))
      showNotification(sprintf("Cambios guardados para intake_id %s.", intake_id), type = "message", duration = 6)
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudieron guardar los cambios.", details = conditionMessage(error)))
      showNotification("No se pudieron guardar los cambios del Formulario 7.", type = "error", duration = 8)
    })
  })

  observeEvent(input$f7_review_delete_confirm, {
    selected <- f7_review_selected()
    if (is.null(selected)) return()
    reason <- f7_clean_text(input$f7_review_delete_reason)[[1]]
    if (is.na(reason)) {
      f7_review_status(list(
        type = "error",
        message = "No se puede eliminar el registro sin comentario.",
        details = "Ingrese el motivo de eliminación antes de confirmar."
      ))
      return()
    }
    intake_id <- as.integer(selected$header$intake_id[[1]])
    tryCatch({
      deleted <- withProgress(message = "Eliminando registro de Formulario 7", value = 0, {
        incProgress(0.20, detail = "Validando el comentario de eliminación")
        incProgress(0.25, detail = "Bloqueando y borrando el registro en Supabase")
        removed <- f7_delete_review_record(intake_id, reason, value_or_default(user_profile$name, f5_text(input$f7_reviewed_by)))
        incProgress(0.35, detail = "Actualizando la lista de revisión")
        f7_review_records(f7_load_review_records())
        incProgress(0.20, detail = "Eliminación completada")
        removed
      })
      f7_review_selected(NULL)
      f7_review_edit_mode(FALSE)
      f7_review_delete_mode(FALSE)
      f7_review_status(list(
        type = "success",
        message = sprintf("Registro %s eliminado definitivamente.", intake_id),
        details = sprintf("Código de bioensayo eliminado: %s.", deleted$codigo_bioensayo[[1]])
      ))
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudo eliminar el registro.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f7_review_confirm, {
    selected <- f7_review_selected()
    if (is.null(selected)) return()
    intake_id <- as.integer(selected$header$intake_id[[1]])
    connection <- NULL
    f7_review_status(list(type = "info", message = sprintf("Confirmando el registro %s...", intake_id), details = character()))
    tryCatch({
      withProgress(message = "Confirmando registro", value = 0, {
        incProgress(0.20, detail = "Abriendo conexión con Supabase")
        connection <- connect_to_supabase()
        incProgress(0.40, detail = "Guardando la revisión")
        updated <- dbGetQuery(
          connection,
          "update public.formulario_7_bioensayo_intake set review_status = 'reviewed', review_notes = nullif($1, ''), reviewed_by = nullif($2, ''), reviewed_at = now(), actualizado_en = now() where intake_id = $3 returning intake_id, review_status, reviewed_by, reviewed_at",
          params = list(f5_text(input$f7_review_notes), f5_text(input$f7_reviewed_by), intake_id)
        )
        if (nrow(updated) != 1) stop("No se actualizó el registro seleccionado.")
        incProgress(0.25, detail = "Actualizando el formulario")
        f7_select_review_record(intake_id)
        f7_review_records(f7_load_review_records())
        incProgress(0.15, detail = "Confirmación completada")
      })
      f7_review_status(list(type = "success", message = sprintf("Registro %s confirmado. Estado: reviewed.", intake_id), details = character()))
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudo confirmar el registro.", details = conditionMessage(error)))
    }, finally = {
      if (!is.null(connection)) dbDisconnect(connection)
    })
  })

  observeEvent(input$open_laboratory_protocols, {
    protocol_rows <- lapply(seq_len(nrow(laboratory_protocols)), function(index) {
      protocol <- laboratory_protocols[index, ]
      download_id <- paste0("download_laboratory_protocol_", protocol$id)

      div(
        class = "protocol-download-item",
        div(
          class = "protocol-download-copy",
          strong(protocol$title),
          p(protocol$description),
          span(class = "protocol-file-name", protocol$file_name)
        ),
        downloadButton(
          download_id,
          "Descargar",
          class = "btn-primary"
        )
      )
    })

    showModal(modalDialog(
      title = "Protocolos de laboratorio",
      easyClose = TRUE,
      size = "l",
      div(
        class = "protocol-modal-body",
        p("Seleccione el SOP que desea descargar. Los documentos se almacenan en Supabase Storage y se entregan desde el portal."),
        tagList(protocol_rows)
      ),
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$show_data_area, {
    active_area(if (identical(active_area(), "data")) NULL else "data")
    active_module(NULL)
    active_capture_subdivision(NULL)
    active_request_subdivision(NULL)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
  })

  observeEvent(input$show_protocols_area, {
    active_area(if (identical(active_area(), "protocols")) NULL else "protocols")
    active_module(NULL)
    active_capture_subdivision(NULL)
    active_request_subdivision(NULL)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
  })

  observeEvent(input$show_training_area, {
    active_area(if (identical(active_area(), "training")) NULL else "training")
    active_module(NULL)
    active_capture_subdivision(NULL)
    active_request_subdivision(NULL)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
  })

  observeEvent(input$show_capture, {
    active_area("data")
    active_module("capture")
    active_capture_subdivision(NULL)
    active_request_subdivision(NULL)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
  })

  observeEvent(input$sat26_open_consent, {
    showModal(modalDialog(
      title = "Consentimiento informado",
      easyClose = TRUE,
      size = "l",
      div(
        class = "sat26-consent-panel sat26-consent-modal",
        h3("Consentimiento informado"),
        div(
          class = "sat26-audio-controls",
          tags$button(type = "button", class = "btn btn-primary", `data-sat26-audio` = "play", `data-sat26-target` = "sat26_consent_popup_text", "Escuchar"),
          tags$button(type = "button", class = "btn btn-default", `data-sat26-audio` = "pause", `data-sat26-target` = "sat26_consent_popup_text", "Pausar"),
          tags$button(type = "button", class = "btn btn-default", `data-sat26-audio` = "stop", `data-sat26-target` = "sat26_consent_popup_text", "Detener")
        ),
        div(
          id = "sat26_consent_popup_text",
          div(class = "sat26-consent-section",
            h4("Información general"),
            p("Investigadora principal: Norma Padilla"),
            p("Este formulario de consentimiento en línea forma parte del proceso de consentimiento informado para una evaluación del programa de vigilancia y control de vectores en Centroamérica y República Dominicana. Le proporcionará información que le ayudará a decidir si desea participar o no en esta evaluación. Su participación es completamente voluntaria. Si decide no participar, no habrá ningún tipo de penalización ni consecuencias en su trabajo."),
            p("Este proyecto está siendo llevado a cabo por la Universidad del Valle de Guatemala, en colaboración con EntoNet y SE-COMISCA, como un esfuerzo conjunto para evaluar las necesidades específicas de la región y apoyar el fortalecimiento de los programas de vigilancia y control de vectores en Centroamérica y República Dominicana.")
          ),
          div(class = "sat26-consent-section",
            h4("¿Quién realiza esta evaluación y de qué trata?"),
            p("Usted ha sido invitado/a a participar en una entrevista realizada por la Dra. Norma Padilla, investigadora principal del Centro de Estudios en Salud de la Universidad del Valle de Guatemala. El objetivo de este estudio es desarrollar una comprensión general de las prácticas de vigilancia y control de vectores en su país, e identificar necesidades y brechas para implementar estrategias adaptadas a la región."),
            p("Específicamente, esta evaluación busca:"),
            tags$ol(
              tags$li("Comprender la capacidad actual de los países para prevenir y controlar enfermedades transmitidas por mosquitos."),
              tags$li("Apoyar a los países en el seguimiento de su progreso frente a indicadores estandarizados.")
            )
          ),
          div(class = "sat26-consent-section",
            h4("¿Qué se me pedirá que haga si decido participar?"),
            p("Se le pedirá que complete una encuesta enfocada en los programas de enfermedades transmitidas por vectores, incluyendo aspectos como gobernanza, financiamiento, recursos humanos, infraestructura, logística, sistemas de información y cooperación entre países. La encuesta tomará aproximadamente 60 minutos.")
          ),
          div(class = "sat26-consent-section",
            h4("¿Qué pasará con la información proporcionada?"),
            p("Sus respuestas se conservarán únicamente hasta que se presenten y publiquen los resultados del estudio. La información no será utilizada ni distribuida para otras investigaciones.")
          ),
          div(class = "sat26-consent-section",
            h4("¿Qué sucede si no quiero participar o si decido retirarme más adelante?"),
            p("Su participación es voluntaria. Puede decidir no participar o retirarse en cualquier momento. Si no hace clic en el botón de \"enviar\" al finalizar la encuesta, sus respuestas no serán registradas. Se le preguntará si está dispuesto(a) a ser contactado(a) por correo electrónico o teléfono con fines de aclaración, en caso de que alguna de sus respuestas requiera seguimiento. Si desea retirar sus respuestas después de haber enviado la encuesta, comuníquese con la investigadora principal.")
          ),
          div(class = "sat26-consent-section",
            h4("¿A quién puedo contactar si tengo preguntas?"),
            p("Si tiene preguntas sobre su participación o desea retirar sus respuestas, puede comunicarse con la investigadora principal: Norma Padilla, Investigadora Principal, Centro de Estudios en Salud, Universidad del Valle de Guatemala, Guatemala. Correo electrónico: npadilla@uvg.edu.gt."),
            p("Este proyecto fue aprobado por el Comité de Ética en Investigación del Centro de Estudios en Salud de la Universidad del Valle de Guatemala. Si desea más información, puede contactar a Estela García al teléfono (502) 2507-1500 ext. 21513. Si desea información respecto al cuestionario o implementación de la evaluación comunicarse con la Dra. Norma Padilla (Whatsapp +502 5204 9300) y/o el investigador encargado de su país.")
          ),
          div(class = "sat26-consent-section",
            h4("Instrucciones finales"),
            p("Puede imprimir este formulario si desea conservar una copia para sus archivos."),
            p("Si no desea participar en la evaluación, cierre esta página web."),
            p("Si desea participar, siga las instrucciones a continuación."),
            p("Al comenzar esta evaluación, confirmo que tengo 18 años o más y que he leído y comprendido esta información. Acepto participar en la evaluación, sabiendo que puedo retirarme en cualquier momento sin penalización.")
          )
        ),
        div(
          class = "sat26-consent-check",
          div(class = "sat26-consent-question", "Pregunta 1. Confirma su consentimiento de participación:"),
          div(
            class = "sat26-consent-choice-row",
            actionButton("sat26_consent_yes_popup", "Sí, acepto participar", class = "sat26-consent-choice sat26-consent-choice-yes"),
            actionButton("sat26_consent_no_popup", "No, no deseo participar", class = "sat26-consent-choice sat26-consent-choice-no")
          )
        )
      ),
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$sat26_open_sections, {
    showModal(modalDialog(
      title = "Secciones de la encuesta SAT26",
      easyClose = TRUE,
      size = "l",
      tags$ol(
        tags$li("Consentimiento e información general"),
        tags$li("Parte A. Información del encuestado"),
        tags$li("Parte B. Informe de situacion de enfermedades"),
        tags$li("Parte C. Gobernanza"),
        tags$li("Parte D. Finanzas"),
        tags$li("Parte E. Recursos Humanos"),
        tags$li("Parte F. Actividades de Control de Vectores para mosquitos"),
        tags$li("Parte G. Actividades de Vigilancia Vectorial"),
        tags$li("Parte H. Infraestructura y Logística"),
        tags$li("Parte I. Sistemas de información"),
        tags$li("Parte J. Participación comunitaria y comunicación de riesgos"),
        tags$li("Parte K. Investigación operativa liderada por el Ministerio de Salud"),
        tags$li("Parte L. Colaboración regional")
      ),
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$sat26_start, {
    sat26_unique_code("")
    sat26_resume_status(NULL)
    active_area("sat26")
    active_module("intro")
    active_capture_subdivision(NULL)
    active_request_subdivision(NULL)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
  })

  sat26_generate_server_code <- function() {
    if (!nzchar(db_url)) {
      stop("SUPABASE_DB_URL no esta disponible para la app en este servidor.", call. = FALSE)
    }
    connection <- connect_to_supabase()
    on.exit(DBI::dbDisconnect(connection), add = TRUE)
    result <- dbGetQuery(
      connection,
      "select nextval('public.encuesta_sat26_codigo_seq')::integer as next_code"
    )
    if (!nrow(result) || is.na(result$next_code[[1]])) {
      stop("Supabase no devolvió un número de secuencia SAT26.")
    }
    sprintf("26SAT%02d", as.integer(result$next_code[[1]]))
  }

  sat26_ensure_unique_code <- function(reset = FALSE) {
    if (isTRUE(reset)) {
      sat26_unique_code("")
    }
    if (!nzchar(sat26_unique_code())) {
      code <- tryCatch(
        sat26_generate_server_code(),
        error = function(error) {
          error_detail <- conditionMessage(error)
          message <- if (!nzchar(db_url)) {
            "SUPABASE_DB_URL no esta disponible en el servidor. Agregue la variable de entorno y reinicie/redeploy la app."
          } else {
            paste(
              "SUPABASE_DB_URL si esta definido, pero la conexion a Supabase fallo.",
              "Revise host, usuario, password, puerto y que use el Session pooler."
            )
          }
          warning(paste(message, error_detail), call. = FALSE)
          sat26_resume_status(message)
          showNotification(message, type = "error", duration = NULL)
          if (tolower(Sys.getenv("ENTONET_ALLOW_LOCAL_SAT26_CODE", unset = "")) %in% c("1", "true", "yes", "si", "sí")) {
            return(sprintf("26SATLOCAL%02d", as.integer(sat26_next_code_number())))
          }
          ""
        }
      )
      if (nzchar(code)) {
        sat26_unique_code(code)
        sat26_next_code_number(as.integer(sat26_next_code_number()) + 1L)
      }
    }
  }

  observeEvent(input$sat26_resume_lookup, {
    resume_code <- trimws(value_or_default(input$sat26_resume_code, ""))
    if (!nzchar(resume_code)) {
      sat26_resume_status("Ingrese un codigo para buscar el borrador local.")
      return()
    }
    remote_payload <- tryCatch({
      connection <- connect_to_supabase()
      on.exit(DBI::dbDisconnect(connection), add = TRUE)
      record <- dbGetQuery(
        connection,
        "
          select payload::text as payload
          from public.encuesta_sat26_intake
          where codigo_unico = $1
          order by submitted_at desc
          limit 1
        ",
        params = list(resume_code)
      )
      if (!nrow(record)) return(NULL)
      payload <- jsonlite::fromJSON(record$payload[[1]], simplifyVector = FALSE)
      payload$code <- resume_code
      payload$found <- TRUE
      payload
    }, error = function(error) {
      sat26_resume_status(paste("No se pudo consultar Supabase; se buscará el borrador local del navegador.", conditionMessage(error)))
      NULL
    })
    if (!is.null(remote_payload)) {
      sat26_resume_status("Se encontró la encuesta en Supabase. Restaurando respuestas guardadas.")
      session$sendCustomMessage("sat26RestoreDraft", remote_payload)
      return()
    }
    session$sendCustomMessage("sat26LoadDraft", list(code = resume_code))
  })

  observeEvent(active_module(), {
    if (identical(active_area(), "sat26") && (active_module() %in% c("intro", "part_a", "part_b", "part_c", "part_d", "part_e", "part_e2", "part_f", "part_g", "part_g2", "part_h", "part_i", "part_j", "part_k", "part_l"))) {
      session$sendCustomMessage("sat26ScrollTop", list())
    }
  }, ignoreInit = TRUE)

  sat26_generate_code <- function() {
    current_code <- sat26_unique_code()
    if (nzchar(current_code)) {
      return(current_code)
    }

    resume_code <- trimws(value_or_default(input$sat26_resume_code, ""))
    if (nzchar(resume_code)) {
      sat26_unique_code(resume_code)
      return(resume_code)
    }

    ""
  }

  sat26_hr_job_suffixes <- c(
    supervisor = "Supervisor / Gerente",
    director = "Director de control vectorial",
    coordinador = "Coordinador de campo de entomología",
    ambiental = "Oficial de salud ambiental",
    tecnico_campo = "Técnico de campo",
    tecnico_laboratorio = "Técnico de laboratorio",
    comunitaria = "Oficial de participación comunitaria",
    datos = "Oficial de datos"
  )
  sat26_hr_scalar_ids <- c(
    "sat26_rrhh_plan",
    "sat26_rrhh_organigrama",
    paste0("sat26_rrhh_nacional_conoce_", names(sat26_hr_job_suffixes)),
    paste0("sat26_rrhh_nacional_cantidad_", names(sat26_hr_job_suffixes)),
    paste0("sat26_rrhh_subnacional_conoce_", names(sat26_hr_job_suffixes)),
    paste0("sat26_rrhh_subnacional_cantidad_", names(sat26_hr_job_suffixes)),
    "sat26_rrhh_suficiencia_vigilancia",
    "sat26_rrhh_suficiencia_control",
    paste0("sat26_rrhh_brecha_", names(sat26_hr_job_suffixes)),
    paste0("sat26_rrhh_adicional_", names(sat26_hr_job_suffixes)),
    "sat26_rrhh_capacitacion_otro"
  )
  sat26_hr_skill_suffixes <- c(
    gestion_control = "Planificación y gestión de programas de control vectorial",
    gestion_vigilancia = "Planificación y gestión de programas de vigilancia vectorial",
    identificacion_mosquitos = "Identificación de mosquitos",
    operaciones_vigilancia = "Operaciones de vigilancia vectorial (trampeo)",
    resistencia_insecticidas = "Pruebas de resistencia a insecticidas",
    participacion_comunitaria = "Participación comunitaria",
    comunicacion_riesgos = "Comunicación sobre riesgos sanitarios",
    soporte_datos = "Soporte para gestión de datos",
    gis = "Cartografía / GIS",
    analisis_datos = "Gestión y análisis de datos",
    informes = "Redacción de informes",
    investigacion_operativa = "Investigación operativa"
  )
  sat26_hr_cont_scalar_ids <- c(
    "sat26_rrhh_necesidad_capacitacion",
    paste0("sat26_rrhh_prioridad_", names(sat26_hr_skill_suffixes)),
    "sat26_rrhh_otras_areas",
    "sat26_rrhh_capacitados_nacional",
    "sat26_rrhh_capacitados_subnacional",
    "sat26_rrhh_modalidad_preferida",
    "sat26_rrhh_online_implementacion",
    "sat26_rrhh_online_modalidad",
    "sat26_rrhh_acceso_computadora",
    "sat26_rrhh_cursos_previos",
    "sat26_rrhh_cursos_previos_bien",
    "sat26_rrhh_cursos_previos_mal",
    "sat26_rrhh_personal_cargo",
    paste0("sat26_rrhh_formadores_", names(sat26_hr_skill_suffixes))
  )
  sat26_control_aedes_activities <- c(
    criaderos = "Reducción de criaderos (ej. limpieza comunitaria)",
    larvicidas = "Larvicidas (ej. temephos, IGRs, Bti)",
    irs_aedes = "Rociado residual dirigido en interiores - <em>Aedes</em> (IRS-<em>Aedes</em>)",
    ors_aedes = "Rociado residual en exteriores - <em>Aedes</em> (ORS-<em>Aedes</em>)",
    nebulizacion_interiores = "Nebulización en interiores (insecticida no residual)",
    nebulizacion_exteriores = "Nebulización en exteriores (insecticida no residual)",
    wolbachia = "<em>Wolbachia</em>",
    mosquiteros_febriles = "Distribución de mosquiteros a pacientes febriles",
    repelentes_febriles = "Distribución de repelentes a pacientes febriles"
  )
  sat26_control_anopheles_activities <- c(
    itns = "Mosquiteros tratados con insecticida (ITNs)",
    llins = "Mosquiteros con insecticida de larga duración (LLINs)",
    irs = "Rociado residual en interiores (IRS)",
    larvicidas = "Aplicación de larvicidas (ej. temephos, IGRs, Bti)",
    criaderos = "Reducción de criaderos (modificación física, limpiezas comunitarias, etc.)"
  )
  sat26_control_quality_activities <- c(
    calidad_intervenciones = "Calidad de las intervenciones de control vectorial",
    durabilidad_llin = "Durabilidad física de los LLIN",
    eficacia_llin = "Eficacia residual del insecticida en LLIN",
    eficacia_irs = "Eficacia residual del rociado residual en interiores (IRS)",
    impacto_larvicidas = "Impacto de la aplicación de larvicidas",
    eficacia_irs_aedes = "Eficacia residual del insecticida en IRS-<em>Aedes</em>",
    nebulizacion_espacial = "Eficacia en intervenciones de nebulización espacial"
  )
  sat26_control_scalar_ids <- c(
    "sat26_control_aedes_conocimiento",
    paste0("sat26_control_aedes_frecuencia_", names(sat26_control_aedes_activities)),
    "sat26_control_aedes_otras_implemento",
    "sat26_control_aedes_otras_descripcion",
    "sat26_control_anopheles_conocimiento",
    paste0("sat26_control_anopheles_frecuencia_", names(sat26_control_anopheles_activities)),
    "sat26_control_anopheles_otras_implemento",
    "sat26_control_anopheles_otras_descripcion",
    paste0("sat26_control_calidad_", names(sat26_control_quality_activities)),
    "sat26_control_irs_otro",
    "sat26_control_larvicidas_otro",
    "sat26_control_irs_aedes_otro",
    "sat26_control_nebulizacion_otro"
  )
  sat26_surveillance_indicators <- c(
    presencia_adultos = "Presencia de adultos",
    densidad_adultos = "Densidad de adultos",
    tasa_picadura = "Tasa de picadura humana",
    horario_picadura = "Horario de picadura",
    lugar_picadura = "Lugar de picadura (interior/exterior)",
    descanso_interior = "Lugar de descanso interior",
    descanso_exterior = "Lugar de descanso exterior",
    resistencia_adultos = "Frecuencia de resistencia - adultos (prueba WHO o botella CDC)",
    resistencia_larvas = "Frecuencia de resistencia - larvas (bioensayo)",
    intensidad_resistencia = "Intensidad de resistencia",
    habitat_larvario = "Disponibilidad de hábitats larvarios",
    habitats_clave = "Hábitats larvarios clave"
  )
  sat26_surveillance_aedes_traps <- c(
    hlc_red = "Recolección con cebo humano (HLC o red de barrido)",
    luz_cdc = "Trampa de luz CDC",
    bg_sentinel = "Trampa BG Sentinel",
    bg_pro = "BG Pro",
    ventilador = "Otras trampas con ventilador",
    gravidas = "Trampas grávidas",
    aspiracion_interiores = "Aspiración en interiores (aspiración manual, Prokopac)",
    aspiracion_exteriores = "Aspiración en exteriores por aspiración",
    descanso_exterior = "Otro método de descanso exterior (trampa de pozo, barrera de tela)",
    cebo_humano = "Trampa con cebo humano",
    cebo_animal = "Trampa con cebo animal",
    otro = "Otro",
    desconozco = "Desconozco"
  )
  sat26_surveillance_anopheles_traps <- c(
    aterrizaje_humano = "Captura por aterrizaje humano",
    cebo_humano = "Trampa con cebo humano",
    cebo_animal = "Trampa con cebo animal",
    ventilador_cdc = "Trampa de ventilador CDC",
    bg_sentinel = "Trampa BG Sentinel",
    gravidas = "Trampas grávidas",
    aspiracion_interiores = "Aspiración en interiores (aspiración manual, Prokopac)",
    aspiracion_exteriores = "Aspiración en exteriores por aspiración",
    descanso_exterior = "Otro método de descanso exterior (trampa de pozo, barrera de tela)",
    salida_ventana = "Trampa de salida por ventana",
    caja_pasiva = "Trampas de caja pasiva",
    otro = "Otro",
    desconozco = "Desconozco"
  )
  sat26_surveillance_decisions <- c(
    estratificar = "Cómo estratificar el control vectorial",
    estrategias = "Dónde implementar diferentes estrategias de control",
    larvicidas = "Selección de larvicidas",
    insecticidas_irs_aedes = "Selección de insecticidas para IRS-<em>Aedes</em>",
    hogares = "Dónde aplicar insecticidas en hogares y alrededores",
    recipientes = "Recipientes larvarios clave para reducción de criaderos",
    control_larval_anopheles = "Dónde implementar control larval (<em>Anopheles</em>)",
    llin_itn = "Elección de LLIN/ITN a comprar",
    habitats_anopheles = "Hábitats larvarios clave para manejo de criaderos (<em>Anopheles</em>)",
    sitios_vigilancia = "Dónde establecer sitios de vigilancia vectorial",
    mensajes = "Cómo optimizar mensajes de participación comunitaria",
    otro = "Otro",
    desconozco = "Desconozco"
  )
  sat26_surveillance_scalar_ids <- c(
    paste0("sat26_vigilancia_aedes_ind_", names(sat26_surveillance_indicators)),
    paste0("sat26_vigilancia_anopheles_ind_", names(sat26_surveillance_indicators)),
    "sat26_vigilancia_aedes_sitios",
    "sat26_vigilancia_aedes_conoce_trampas",
    "sat26_vigilancia_aedes_trampa_otro",
    "sat26_vigilancia_aedes_ident_adultos",
    "sat26_vigilancia_aedes_ident_adultos_otro",
    "sat26_vigilancia_aedes_ident_larvas",
    "sat26_vigilancia_aedes_ident_larvas_otro",
    "sat26_vigilancia_anopheles_sitios",
    "sat26_vigilancia_anopheles_conoce_trampas",
    "sat26_vigilancia_anopheles_trampa_otro",
    "sat26_vigilancia_anopheles_ident_adultos",
    "sat26_vigilancia_anopheles_ident_adultos_otro",
    "sat26_vigilancia_anopheles_ident_larvas",
    "sat26_vigilancia_anopheles_ident_larvas_otro",
    "sat26_vigilancia_uso_datos",
    "sat26_vigilancia_decisiones_otro"
  )
  sat26_infra_vigilancia_items <- c(
    sistema_logistico = "Sistema logístico",
    transporte = "Transporte",
    oficina = "Espacio de oficina",
    laboratorio = "Laboratorio",
    insectario = "Insectario",
    computadoras = "Computadoras",
    trampas = "Trampas para mosquitos",
    suministros = "Suministros entomológicos (aspiradores, vasos, etc.)",
    moviles = "Dispositivos móviles (tablets, teléfonos, GPS)",
    adquisicion = "Adquisición de recursos (cadena de suministro)"
  )
  sat26_infra_control_items <- c(
    sistema_logistico = "Sistema logístico",
    transporte = "Transporte",
    oficina = "Espacio de oficina",
    bodega = "Almacenamiento en bodega",
    computadoras = "Computadoras",
    rociado = "Equipos de rociado (IRS o nebulizadores)",
    llins = "Mosquiteros tratados de larga duración (LLINs)",
    insecticidas_irs = "Insecticidas para IRS",
    mosquiteros_viremicos = "Mosquiteros para pacientes virémicos",
    larvicidas = "Larvicidas",
    epp = "Equipos de protección personal (EPP)",
    otros_equipos = "Otros equipos / consumibles / suministros",
    cadena_suministro = "Cadena de suministro"
  )
  sat26_infra_scalar_ids <- c(
    paste0("sat26_infra_vigilancia_", names(sat26_infra_vigilancia_items)),
    "sat26_infra_laboratorio_gestionado",
    "sat26_infra_conoce_laboratorio",
    "sat26_infra_laboratorio_capacidad_otro",
    "sat26_infra_insectario_gestionado",
    "sat26_infra_colonia_aedes",
    "sat26_infra_colonia_anopheles",
    paste0("sat26_infra_control_", names(sat26_infra_control_items))
  )
  sat26_info_scalar_ids <- c(
    "sat26_info_vigilancia_recoleccion_usa",
    "sat26_info_vigilancia_recoleccion_otro",
    "sat26_info_vigilancia_apps",
    "sat26_info_vigilancia_almacenamiento_otro",
    "sat26_info_vigilancia_reporte_otro",
    "sat26_info_vigilancia_limitacion",
    "sat26_info_control_recoleccion_usa",
    "sat26_info_control_recoleccion_otro",
    "sat26_info_control_apps",
    "sat26_info_control_almacenamiento_otro",
    "sat26_info_control_reporte_otro",
    "sat26_info_control_limitacion"
  )
  sat26_community_scalar_ids <- c(
    "sat26_comunidad_participacion",
    "sat26_comunidad_actividades_otro",
    "sat26_comunidad_momento_otro"
  )
  sat26_research_reference_items <- c(
    informes = "Informes oficiales del proyecto",
    articulos = "Artículos de revistas científicas",
    congresos = "Ponencias y actas en congreso",
    multilaterales = "Documentos de agencias multilaterales u ONG",
    tesis = "Tesis y trabajos académicos",
    financiamiento = "Propuestas, contratos y documentos de financiamiento",
    prensa = "Comunicados de prensa"
  )
  sat26_research_scalar_ids <- c(
    "sat26_investigacion_agenda",
    "sat26_investigacion_agenda_vectores",
    "sat26_investigacion_operativa_aedes",
    "sat26_investigacion_titulo",
    "sat26_investigacion_referencias",
    paste0("sat26_investigacion_ref_nombre_", names(sat26_research_reference_items)),
    paste0("sat26_investigacion_ref_link_", names(sat26_research_reference_items)),
    paste0("sat26_investigacion_ref_documento_", names(sat26_research_reference_items)),
    "sat26_investigacion_compartir_entonet",
    "sat26_investigacion_compartir_ops"
  )
  sat26_research_reference_ids <- paste0("sat26_investigacion_ref_tipo_", names(sat26_research_reference_items))
  sat26_regional_scalar_ids <- c(
    "sat26_regional_punto_focal",
    "sat26_regional_acuerdos",
    "sat26_regional_frecuencia_intercambio",
    "sat26_regional_sistemas_invasoras",
    "sat26_regional_cambio_climatico",
    "sat26_regional_redes",
    "sat26_regional_mecanismos",
    "sat26_regional_plataformas"
  )

  sat26_build_payload <- function() {
    if (!identical(active_area(), "sat26") || !(active_module() %in% c("part_a", "part_b", "part_c", "part_d", "part_e", "part_e2", "part_f", "part_g", "part_g2", "part_h", "part_i", "part_j", "part_k", "part_l"))) {
      return(NULL)
    }

    code <- sat26_generate_code()
    if (!nzchar(code)) {
      return(NULL)
    }
    sat26_multi <- function(id) input[[id]] %||% character(0)
    sat26_value <- function(id) value_or_default(input[[id]], "")
    list(
      code = code,
      section = active_module(),
      nombre = value_or_default(input$sat26_nombre, ""),
      cargo = value_or_default(input$sat26_cargo, ""),
      organizacion = value_or_default(input$sat26_organizacion, ""),
      country = value_or_default(input$sat26_country, ""),
      contact_after = value_or_default(input$sat26_contact_after, ""),
      dengue_2025 = value_or_default(input$sat26_dengue_2025, ""),
      arbovirus_2025 = input$sat26_arbovirus_2025 %||% character(0),
      filariasis_activa_2025 = value_or_default(input$sat26_filariasis_activa_2025, ""),
      filariasis_escenario_2025 = value_or_default(input$sat26_filariasis_escenario_2025, ""),
      malaria_2025 = value_or_default(input$sat26_malaria_2025, ""),
      plan_tipo = sat26_value("sat26_plan_tipo"),
      plan_estado = sat26_value("sat26_plan_estado"),
      plan_caracteristicas_tipo = sat26_value("sat26_plan_caracteristicas_tipo"),
      plan_caracteristicas = sat26_multi("sat26_plan_caracteristicas"),
      plan_aedes_estado = sat26_value("sat26_plan_aedes_estado"),
      plan_anopheles_estado = sat26_value("sat26_plan_anopheles_estado"),
      plan_integrado_estado = sat26_value("sat26_plan_integrado_estado"),
      plan_aedes_caracteristicas = sat26_multi("sat26_plan_aedes_caracteristicas"),
      plan_anopheles_caracteristicas = sat26_multi("sat26_plan_anopheles_caracteristicas"),
      plan_integrado_caracteristicas = sat26_multi("sat26_plan_integrado_caracteristicas"),
      plan_aedes_nombre = sat26_value("sat26_plan_aedes_nombre"),
      plan_aedes_anio = sat26_value("sat26_plan_aedes_anio"),
      plan_aedes_documento = sat26_value("sat26_plan_aedes_documento"),
      plan_anopheles_nombre = sat26_value("sat26_plan_anopheles_nombre"),
      plan_anopheles_anio = sat26_value("sat26_plan_anopheles_anio"),
      plan_anopheles_documento = sat26_value("sat26_plan_anopheles_documento"),
      plan_integrado_nombre = sat26_value("sat26_plan_integrado_nombre"),
      plan_integrado_anio = sat26_value("sat26_plan_integrado_anio"),
      plan_integrado_documento = sat26_value("sat26_plan_integrado_documento"),
      reglamentos_nacionales = sat26_value("sat26_reglamentos_nacionales"),
      reglamentos_documento = sat26_value("sat26_reglamentos_documento"),
      prioridad_planes = sat26_value("sat26_prioridad_planes"),
      asistencia_planes = sat26_value("sat26_asistencia_planes"),
      normas_vigilancia_aedes = sat26_value("sat26_normas_vigilancia_aedes"),
      normas_control_aedes = sat26_value("sat26_normas_control_aedes"),
      normas_vigilancia_anopheles = sat26_value("sat26_normas_vigilancia_anopheles"),
      normas_control_anopheles = sat26_value("sat26_normas_control_anopheles"),
      plan_elemento_control_aedes = sat26_value("sat26_plan_elemento_control_aedes"),
      plan_elemento_vigilancia_aedes = sat26_value("sat26_plan_elemento_vigilancia_aedes"),
      plan_elemento_control_anopheles = sat26_value("sat26_plan_elemento_control_anopheles"),
      plan_elemento_vigilancia_anopheles = sat26_value("sat26_plan_elemento_vigilancia_anopheles"),
      plan_elemento_equipos_insecticidas = sat26_value("sat26_plan_elemento_equipos_insecticidas"),
      plan_elemento_resistencia = sat26_value("sat26_plan_elemento_resistencia"),
      plan_elemento_recursos_humanos = sat26_value("sat26_plan_elemento_recursos_humanos"),
      plan_elemento_formacion = sat26_value("sat26_plan_elemento_formacion"),
      plan_elemento_participacion = sat26_value("sat26_plan_elemento_participacion"),
      plan_elemento_sistemas_info = sat26_value("sat26_plan_elemento_sistemas_info"),
      plan_elemento_investigacion = sat26_value("sat26_plan_elemento_investigacion"),
      plan_elemento_vulnerables = sat26_value("sat26_plan_elemento_vulnerables"),
      plan_elemento_monitoreo = sat26_value("sat26_plan_elemento_monitoreo"),
      descentralizacion = sat26_value("sat26_descentralizacion"),
      legislacion_24_48 = sat26_value("sat26_legislacion_24_48"),
      grupo_interministerial = sat26_value("sat26_grupo_interministerial"),
      grupo_interministerial_reunion = sat26_value("sat26_grupo_interministerial_reunion"),
      prioridad_agenda = sat26_value("sat26_prioridad_agenda"),
      factores_prioridad = sat26_multi("sat26_factores_prioridad"),
      factores_prioridad_otros = sat26_value("sat26_factores_prioridad_otros"),
      frecuencia_interministerial = sat26_value("sat26_frecuencia_interministerial"),
      motivadores_institucion = sat26_multi("sat26_motivadores_institucion"),
      motivadores_institucion_otros = sat26_value("sat26_motivadores_institucion_otros"),
      presupuesto_vigilancia = sat26_value("sat26_presupuesto_vigilancia"),
      presupuesto_control = sat26_value("sat26_presupuesto_control"),
      fin_vigilancia_presupuesto = sat26_value("sat26_fin_vigilancia_presupuesto"),
      fin_vigilancia_gestion = sat26_value("sat26_fin_vigilancia_gestion"),
      fin_control_presupuesto = sat26_value("sat26_fin_control_presupuesto"),
      fin_control_gestion = sat26_value("sat26_fin_control_gestion"),
      rrhh_capacitacion_sistemas = sat26_multi("sat26_rrhh_capacitacion_sistemas"),
      rrhh = setNames(lapply(sat26_hr_scalar_ids, sat26_value), sat26_hr_scalar_ids),
      rrhh_cont = setNames(lapply(sat26_hr_cont_scalar_ids, sat26_value), sat26_hr_cont_scalar_ids),
      control_aedes_actividades = sat26_multi("sat26_control_aedes_actividades"),
      control_aedes_otras = sat26_multi("sat26_control_aedes_otras"),
      control_anopheles_actividades = sat26_multi("sat26_control_anopheles_actividades"),
      control_anopheles_otras = sat26_multi("sat26_control_anopheles_otras"),
      control_irs_metodos = sat26_multi("sat26_control_irs_metodos"),
      control_larvicidas_metodos = sat26_multi("sat26_control_larvicidas_metodos"),
      control_irs_aedes_metodos = sat26_multi("sat26_control_irs_aedes_metodos"),
      control_nebulizacion_metodos = sat26_multi("sat26_control_nebulizacion_metodos"),
      control = setNames(lapply(sat26_control_scalar_ids, sat26_value), sat26_control_scalar_ids),
      vigilancia_aedes_trampas = sat26_multi("sat26_vigilancia_aedes_trampas"),
      vigilancia_anopheles_trampas = sat26_multi("sat26_vigilancia_anopheles_trampas"),
      vigilancia_decisiones = sat26_multi("sat26_vigilancia_decisiones"),
      vigilancia = setNames(lapply(sat26_surveillance_scalar_ids, sat26_value), sat26_surveillance_scalar_ids),
      infra_laboratorio_capacidades = sat26_multi("sat26_infra_laboratorio_capacidades"),
      infraestructura = setNames(lapply(sat26_infra_scalar_ids, sat26_value), sat26_infra_scalar_ids),
      info_vigilancia_herramientas = sat26_multi("sat26_info_vigilancia_herramientas"),
      info_control_herramientas = sat26_multi("sat26_info_control_herramientas"),
      info_vigilancia_recoleccion = sat26_multi("sat26_info_vigilancia_recoleccion"),
      info_vigilancia_almacenamiento = sat26_multi("sat26_info_vigilancia_almacenamiento"),
      info_vigilancia_reporte = sat26_multi("sat26_info_vigilancia_reporte"),
      info_control_recoleccion = sat26_multi("sat26_info_control_recoleccion"),
      info_control_almacenamiento = sat26_multi("sat26_info_control_almacenamiento"),
      info_control_reporte = sat26_multi("sat26_info_control_reporte"),
      sistemas_info = setNames(lapply(sat26_info_scalar_ids, sat26_value), sat26_info_scalar_ids),
      comunidad_actividades = sat26_multi("sat26_comunidad_actividades"),
      comunidad_momento = sat26_multi("sat26_comunidad_momento"),
      comunidad = setNames(lapply(sat26_community_scalar_ids, sat26_value), sat26_community_scalar_ids),
      investigacion_ref_tipos = setNames(lapply(sat26_research_reference_ids, sat26_multi), sat26_research_reference_ids),
      investigacion = setNames(lapply(sat26_research_scalar_ids, sat26_value), sat26_research_scalar_ids),
      colaboracion_regional = setNames(lapply(sat26_regional_scalar_ids, sat26_value), sat26_regional_scalar_ids),
      saved_at = as.character(Sys.time())
    )
  }

  sat26_save_draft <- function() {
    payload <- sat26_build_payload()
    if (is.null(payload)) {
      return()
    }
    session$sendCustomMessage("sat26SaveDraft", payload)
  }

  sat26_submit_to_supabase <- function() {
    payload <- sat26_build_payload()
    if (is.null(payload)) {
      stop("No hay respuestas SAT26 listas para enviar.")
    }
    code <- value_or_default(payload$code, "")
    if (!nzchar(code)) {
      stop("No se encontró el código único de encuesta.")
    }

    payload_json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null")
    connection <- connect_to_supabase()
    on.exit(DBI::dbDisconnect(connection), add = TRUE)
    dbGetQuery(
      connection,
      "
        insert into public.encuesta_sat26_intake (
          codigo_unico,
          encuesta_codigo,
          encuesta_nombre,
          formulario_version,
          payload,
          actualizado_en
        )
        values (
          $1,
          'SAT26',
          'Encuesta SAT26',
          'web-local-2026-08-15',
          $2::jsonb,
          now()
        )
        on conflict (codigo_unico) do update
        set
          payload = excluded.payload,
          submitted_at = now(),
          formulario_version = excluded.formulario_version,
          actualizado_en = now()
        returning intake_id::integer, codigo_unico, submitted_at
      ",
      params = list(code, as.character(payload_json))
    )
  }

  observeEvent(input$sat26_generated_code, {
    req(nzchar(value_or_default(input$sat26_generated_code, "")))
    if (!nzchar(sat26_unique_code())) {
      sat26_unique_code(value_or_default(input$sat26_generated_code, ""))
    }
    if (identical(active_area(), "sat26") && active_module() %in% c("part_a", "part_b", "part_c", "part_d", "part_e", "part_e2", "part_f", "part_g", "part_g2", "part_h", "part_i", "part_j", "part_k", "part_l")) {
      sat26_save_draft()
    }
  })

  observeEvent(input$sat26_resume_payload, {
    payload <- input$sat26_resume_payload
    if (is.null(payload)) return()

    resume_code <- value_or_default(payload$code, "")
    if (!isTRUE(payload$found)) {
      sat26_resume_status(sprintf("No encontramos un borrador local para el codigo %s.", resume_code))
      return()
    }

    sat26_unique_code(resume_code)
    sat26_resume_status(sprintf("Se cargo el borrador %s.", resume_code))
    active_area("sat26")
    active_module(value_or_default(payload$section, "part_a"))

    updateTextInput(session, "sat26_nombre", value = value_or_default(payload$nombre, ""))
    updateTextInput(session, "sat26_cargo", value = value_or_default(payload$cargo, ""))
    updateTextInput(session, "sat26_organizacion", value = value_or_default(payload$organizacion, ""))
    updateSelectInput(session, "sat26_country", selected = value_or_default(payload$country, ""))
    updateRadioButtons(session, "sat26_contact_after", selected = value_or_default(payload$contact_after, ""))
    updateRadioButtons(session, "sat26_dengue_2025", selected = value_or_default(payload$dengue_2025, ""))
    updateCheckboxGroupInput(session, "sat26_arbovirus_2025", selected = unlist(payload$arbovirus_2025 %||% character(0), use.names = FALSE))
    updateRadioButtons(session, "sat26_filariasis_activa_2025", selected = value_or_default(payload$filariasis_activa_2025, ""))
    updateRadioButtons(session, "sat26_filariasis_escenario_2025", selected = value_or_default(payload$filariasis_escenario_2025, ""))
    updateRadioButtons(session, "sat26_malaria_2025", selected = value_or_default(payload$malaria_2025, ""))
    updateRadioButtons(session, "sat26_plan_tipo", selected = value_or_default(payload$plan_tipo, ""))
    updateRadioButtons(session, "sat26_plan_estado", selected = value_or_default(payload$plan_estado, ""))
    updateRadioButtons(session, "sat26_plan_caracteristicas_tipo", selected = value_or_default(payload$plan_caracteristicas_tipo, ""))
    updateCheckboxGroupInput(session, "sat26_plan_caracteristicas", selected = unlist(payload$plan_caracteristicas %||% character(0), use.names = FALSE))
    updateRadioButtons(session, "sat26_plan_aedes_estado", selected = value_or_default(payload$plan_aedes_estado, ""))
    updateRadioButtons(session, "sat26_plan_anopheles_estado", selected = value_or_default(payload$plan_anopheles_estado, ""))
    updateRadioButtons(session, "sat26_plan_integrado_estado", selected = value_or_default(payload$plan_integrado_estado, ""))
    updateCheckboxGroupInput(session, "sat26_plan_aedes_caracteristicas", selected = unlist(payload$plan_aedes_caracteristicas %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_plan_anopheles_caracteristicas", selected = unlist(payload$plan_anopheles_caracteristicas %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_plan_integrado_caracteristicas", selected = unlist(payload$plan_integrado_caracteristicas %||% character(0), use.names = FALSE))
    updateTextInput(session, "sat26_plan_aedes_nombre", value = value_or_default(payload$plan_aedes_nombre, ""))
    updateTextInput(session, "sat26_plan_aedes_anio", value = value_or_default(payload$plan_aedes_anio, ""))
    updateTextInput(session, "sat26_plan_aedes_documento", value = value_or_default(payload$plan_aedes_documento, ""))
    updateTextInput(session, "sat26_plan_anopheles_nombre", value = value_or_default(payload$plan_anopheles_nombre, ""))
    updateTextInput(session, "sat26_plan_anopheles_anio", value = value_or_default(payload$plan_anopheles_anio, ""))
    updateTextInput(session, "sat26_plan_anopheles_documento", value = value_or_default(payload$plan_anopheles_documento, ""))
    updateTextInput(session, "sat26_plan_integrado_nombre", value = value_or_default(payload$plan_integrado_nombre, ""))
    updateTextInput(session, "sat26_plan_integrado_anio", value = value_or_default(payload$plan_integrado_anio, ""))
    updateTextInput(session, "sat26_plan_integrado_documento", value = value_or_default(payload$plan_integrado_documento, ""))
    updateRadioButtons(session, "sat26_reglamentos_nacionales", selected = value_or_default(payload$reglamentos_nacionales, ""))
    updateTextInput(session, "sat26_reglamentos_documento", value = value_or_default(payload$reglamentos_documento, ""))
    updateRadioButtons(session, "sat26_prioridad_planes", selected = value_or_default(payload$prioridad_planes, ""))
    updateRadioButtons(session, "sat26_asistencia_planes", selected = value_or_default(payload$asistencia_planes, ""))
    updateRadioButtons(session, "sat26_normas_vigilancia_aedes", selected = value_or_default(payload$normas_vigilancia_aedes, ""))
    updateRadioButtons(session, "sat26_normas_control_aedes", selected = value_or_default(payload$normas_control_aedes, ""))
    updateRadioButtons(session, "sat26_normas_vigilancia_anopheles", selected = value_or_default(payload$normas_vigilancia_anopheles, ""))
    updateRadioButtons(session, "sat26_normas_control_anopheles", selected = value_or_default(payload$normas_control_anopheles, ""))
    updateRadioButtons(session, "sat26_plan_elemento_control_aedes", selected = value_or_default(payload$plan_elemento_control_aedes, ""))
    updateRadioButtons(session, "sat26_plan_elemento_vigilancia_aedes", selected = value_or_default(payload$plan_elemento_vigilancia_aedes, ""))
    updateRadioButtons(session, "sat26_plan_elemento_control_anopheles", selected = value_or_default(payload$plan_elemento_control_anopheles, ""))
    updateRadioButtons(session, "sat26_plan_elemento_vigilancia_anopheles", selected = value_or_default(payload$plan_elemento_vigilancia_anopheles, ""))
    updateRadioButtons(session, "sat26_plan_elemento_equipos_insecticidas", selected = value_or_default(payload$plan_elemento_equipos_insecticidas, ""))
    updateRadioButtons(session, "sat26_plan_elemento_resistencia", selected = value_or_default(payload$plan_elemento_resistencia, ""))
    updateRadioButtons(session, "sat26_plan_elemento_recursos_humanos", selected = value_or_default(payload$plan_elemento_recursos_humanos, ""))
    updateRadioButtons(session, "sat26_plan_elemento_formacion", selected = value_or_default(payload$plan_elemento_formacion, ""))
    updateRadioButtons(session, "sat26_plan_elemento_participacion", selected = value_or_default(payload$plan_elemento_participacion, ""))
    updateRadioButtons(session, "sat26_plan_elemento_sistemas_info", selected = value_or_default(payload$plan_elemento_sistemas_info, ""))
    updateRadioButtons(session, "sat26_plan_elemento_investigacion", selected = value_or_default(payload$plan_elemento_investigacion, ""))
    updateRadioButtons(session, "sat26_plan_elemento_vulnerables", selected = value_or_default(payload$plan_elemento_vulnerables, ""))
    updateRadioButtons(session, "sat26_plan_elemento_monitoreo", selected = value_or_default(payload$plan_elemento_monitoreo, ""))
    updateRadioButtons(session, "sat26_descentralizacion", selected = value_or_default(payload$descentralizacion, ""))
    updateRadioButtons(session, "sat26_legislacion_24_48", selected = value_or_default(payload$legislacion_24_48, ""))
    updateRadioButtons(session, "sat26_grupo_interministerial", selected = value_or_default(payload$grupo_interministerial, ""))
    updateTextAreaInput(session, "sat26_grupo_interministerial_reunion", value = value_or_default(payload$grupo_interministerial_reunion, ""))
    updateRadioButtons(session, "sat26_prioridad_agenda", selected = value_or_default(payload$prioridad_agenda, ""))
    updateCheckboxGroupInput(session, "sat26_factores_prioridad", selected = unlist(payload$factores_prioridad %||% character(0), use.names = FALSE))
    updateTextAreaInput(session, "sat26_factores_prioridad_otros", value = value_or_default(payload$factores_prioridad_otros, ""))
    updateRadioButtons(session, "sat26_frecuencia_interministerial", selected = value_or_default(payload$frecuencia_interministerial, ""))
    updateCheckboxGroupInput(session, "sat26_motivadores_institucion", selected = unlist(payload$motivadores_institucion %||% character(0), use.names = FALSE))
    updateTextAreaInput(session, "sat26_motivadores_institucion_otros", value = value_or_default(payload$motivadores_institucion_otros, ""))
    updateRadioButtons(session, "sat26_presupuesto_vigilancia", selected = value_or_default(payload$presupuesto_vigilancia, ""))
    updateRadioButtons(session, "sat26_presupuesto_control", selected = value_or_default(payload$presupuesto_control, ""))
    updateRadioButtons(session, "sat26_fin_vigilancia_presupuesto", selected = value_or_default(payload$fin_vigilancia_presupuesto, ""))
    updateRadioButtons(session, "sat26_fin_vigilancia_gestion", selected = value_or_default(payload$fin_vigilancia_gestion, ""))
    updateRadioButtons(session, "sat26_fin_control_presupuesto", selected = value_or_default(payload$fin_control_presupuesto, ""))
    updateRadioButtons(session, "sat26_fin_control_gestion", selected = value_or_default(payload$fin_control_gestion, ""))
    updateCheckboxGroupInput(session, "sat26_rrhh_capacitacion_sistemas", selected = unlist(payload$rrhh_capacitacion_sistemas %||% character(0), use.names = FALSE))
    rrhh_payload <- payload$rrhh %||% list()
    for (id in sat26_hr_scalar_ids) {
      value <- value_or_default(rrhh_payload[[id]], "")
      if (startsWith(id, "sat26_rrhh_organigrama") ||
          startsWith(id, "sat26_rrhh_nacional_cantidad_") ||
          startsWith(id, "sat26_rrhh_subnacional_cantidad_") ||
          startsWith(id, "sat26_rrhh_adicional_") ||
          identical(id, "sat26_rrhh_capacitacion_otro")) {
        updateTextInput(session, id, value = value)
      } else {
        updateRadioButtons(session, id, selected = value)
      }
    }
    rrhh_cont_payload <- payload$rrhh_cont %||% list()
    for (id in sat26_hr_cont_scalar_ids) {
      value <- value_or_default(rrhh_cont_payload[[id]], "")
      if (identical(id, "sat26_rrhh_otras_areas") ||
          identical(id, "sat26_rrhh_cursos_previos_bien") ||
          identical(id, "sat26_rrhh_cursos_previos_mal")) {
        updateTextAreaInput(session, id, value = value)
      } else {
        updateRadioButtons(session, id, selected = value)
      }
    }
    updateCheckboxGroupInput(session, "sat26_control_aedes_actividades", selected = unlist(payload$control_aedes_actividades %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_control_aedes_otras", selected = unlist(payload$control_aedes_otras %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_control_anopheles_actividades", selected = unlist(payload$control_anopheles_actividades %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_control_anopheles_otras", selected = unlist(payload$control_anopheles_otras %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_control_irs_metodos", selected = unlist(payload$control_irs_metodos %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_control_larvicidas_metodos", selected = unlist(payload$control_larvicidas_metodos %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_control_irs_aedes_metodos", selected = unlist(payload$control_irs_aedes_metodos %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_control_nebulizacion_metodos", selected = unlist(payload$control_nebulizacion_metodos %||% character(0), use.names = FALSE))
    control_payload <- payload$control %||% list()
    for (id in sat26_control_scalar_ids) {
      value <- value_or_default(control_payload[[id]], "")
      if (grepl("_otro$|_descripcion$", id)) {
        updateTextAreaInput(session, id, value = value)
      } else {
        updateRadioButtons(session, id, selected = value)
      }
    }
    updateCheckboxGroupInput(session, "sat26_vigilancia_aedes_trampas", selected = unlist(payload$vigilancia_aedes_trampas %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_vigilancia_anopheles_trampas", selected = unlist(payload$vigilancia_anopheles_trampas %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_vigilancia_decisiones", selected = unlist(payload$vigilancia_decisiones %||% character(0), use.names = FALSE))
    vigilancia_payload <- payload$vigilancia %||% list()
    for (id in sat26_surveillance_scalar_ids) {
      value <- value_or_default(vigilancia_payload[[id]], "")
      if (grepl("_otro$", id)) {
        updateTextAreaInput(session, id, value = value)
      } else {
        updateRadioButtons(session, id, selected = value)
      }
    }
    updateCheckboxGroupInput(session, "sat26_infra_laboratorio_capacidades", selected = unlist(payload$infra_laboratorio_capacidades %||% character(0), use.names = FALSE))
    infraestructura_payload <- payload$infraestructura %||% list()
    for (id in sat26_infra_scalar_ids) {
      value <- value_or_default(infraestructura_payload[[id]], "")
      if (grepl("_otro$", id)) {
        updateTextAreaInput(session, id, value = value)
      } else {
        updateRadioButtons(session, id, selected = value)
      }
    }
    updateCheckboxGroupInput(session, "sat26_info_vigilancia_herramientas", selected = unlist(payload$info_vigilancia_herramientas %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_info_control_herramientas", selected = unlist(payload$info_control_herramientas %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_info_vigilancia_recoleccion", selected = unlist(payload$info_vigilancia_recoleccion %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_info_vigilancia_almacenamiento", selected = unlist(payload$info_vigilancia_almacenamiento %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_info_vigilancia_reporte", selected = unlist(payload$info_vigilancia_reporte %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_info_control_recoleccion", selected = unlist(payload$info_control_recoleccion %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_info_control_almacenamiento", selected = unlist(payload$info_control_almacenamiento %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_info_control_reporte", selected = unlist(payload$info_control_reporte %||% character(0), use.names = FALSE))
    sistemas_info_payload <- payload$sistemas_info %||% list()
    for (id in sat26_info_scalar_ids) {
      value <- value_or_default(sistemas_info_payload[[id]], "")
      if (grepl("_otro$|_apps$", id)) {
        updateTextAreaInput(session, id, value = value)
      } else {
        updateRadioButtons(session, id, selected = value)
      }
    }
    updateCheckboxGroupInput(session, "sat26_comunidad_actividades", selected = unlist(payload$comunidad_actividades %||% character(0), use.names = FALSE))
    updateCheckboxGroupInput(session, "sat26_comunidad_momento", selected = unlist(payload$comunidad_momento %||% character(0), use.names = FALSE))
    comunidad_payload <- payload$comunidad %||% list()
    for (id in sat26_community_scalar_ids) {
      value <- value_or_default(comunidad_payload[[id]], "")
      if (grepl("_otro$", id)) {
        updateTextAreaInput(session, id, value = value)
      } else {
        updateRadioButtons(session, id, selected = value)
      }
    }
    investigacion_ref_tipos_payload <- payload$investigacion_ref_tipos %||% list()
    for (id in sat26_research_reference_ids) {
      updateCheckboxGroupInput(session, id, selected = unlist(investigacion_ref_tipos_payload[[id]] %||% character(0), use.names = FALSE))
    }
    investigacion_payload <- payload$investigacion %||% list()
    for (id in sat26_research_scalar_ids) {
      value <- value_or_default(investigacion_payload[[id]], "")
      if (grepl("_titulo$|_referencias$|_nombre_|_link_|_documento_", id)) {
        updateTextAreaInput(session, id, value = value)
      } else {
        updateRadioButtons(session, id, selected = value)
      }
    }
    colaboracion_regional_payload <- payload$colaboracion_regional %||% list()
    for (id in sat26_regional_scalar_ids) {
      updateRadioButtons(session, id, selected = value_or_default(colaboracion_regional_payload[[id]], ""))
    }
  })

  observeEvent(input$sat26_back_home, {
    active_area(NULL)
    active_module(NULL)
    active_capture_subdivision(NULL)
    active_request_subdivision(NULL)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
  })

  sat26_show_next_step <- function() {
    showModal(modalDialog(
      title = "Siguiente paso",
      easyClose = TRUE,
      div(
        class = "module-panel",
        p("Perfecto. El siguiente paso será construir la primera página de preguntas de la sección A para que el cuestionario avance por bloques."),
        p("Por ahora la versión local ya tiene la portada formal y el consentimiento funcional.")
      ),
      footer = modalButton("Cerrar")
    ))
  }

  sat26_close_survey <- function() {
    removeModal()
    active_area(NULL)
    active_module(NULL)
    active_capture_subdivision(NULL)
    active_request_subdivision(NULL)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
    showModal(modalDialog(
      title = "Encuesta cerrada",
      easyClose = TRUE,
      div(
        class = "module-panel",
        p("Gracias por su tiempo. Como no aceptó participar, el cuestionario no se abrirá.")
      ),
      footer = modalButton("Cerrar")
    ))
  }

  observeEvent(input$sat26_consent_yes_popup, {
    removeModal()
    active_area("sat26")
    sat26_ensure_unique_code()
    if (nzchar(sat26_unique_code())) {
      active_module("part_a")
    }
  })

  observeEvent(input$sat26_consent_no_popup, {
    sat26_close_survey()
  })

  observeEvent(input$sat26_consent_yes, {
    sat26_ensure_unique_code()
    if (nzchar(sat26_unique_code())) {
      active_module("part_a")
    }
  })

  observeEvent(input$sat26_consent_no, {
    sat26_close_survey()
  })

  observeEvent(input$sat26_country_reset, {
    updateSelectInput(session, "sat26_country", selected = "")
  })

  observeEvent(input$sat26_contact_reset, {
    updateRadioButtons(session, "sat26_contact_after", selected = character(0))
  })

  observeEvent(input$sat26_back_to_consent, {
    active_module("intro")
  })

  sat26_scroll_top <- function(delay = 120) {
    session$sendCustomMessage("sat26ScrollTop", list(delay = delay))
  }

  sat26_go_to <- function(module) {
    sat26_save_draft()
    active_module(module)
    sat26_scroll_top()
  }

  observeEvent(input$sat26_part_a_continue, {
    sat26_go_to("part_b")
  })

  observeEvent(
    list(input$sat26_nombre, input$sat26_cargo, input$sat26_organizacion, input$sat26_country, input$sat26_contact_after),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    list(
      input$sat26_dengue_2025,
      input$sat26_arbovirus_2025,
      input$sat26_filariasis_activa_2025,
      input$sat26_filariasis_escenario_2025,
      input$sat26_malaria_2025
    ),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(input$sat26_part_b_back, {
    sat26_go_to("part_a")
  })

  observeEvent(input$sat26_part_b_continue, {
    sat26_go_to("part_c")
  })

  observeEvent(input$sat26_part_c_back, {
    sat26_go_to("part_b")
  })

  observeEvent(input$sat26_part_c_continue, {
    sat26_go_to("part_d")
  })

  observeEvent(input$sat26_part_d_back, {
    sat26_go_to("part_c")
  })

  observeEvent(input$sat26_part_d_continue, {
    sat26_go_to("part_e")
  })

  observeEvent(input$sat26_part_e_back, {
    sat26_go_to("part_d")
  })

  observeEvent(input$sat26_part_e_continue, {
    sat26_go_to("part_e2")
  })

  observeEvent(input$sat26_part_e2_back, {
    sat26_go_to("part_e")
  })

  observeEvent(input$sat26_part_e2_continue, {
    sat26_go_to("part_f")
  })

  observeEvent(input$sat26_part_f_back, {
    sat26_go_to("part_e2")
  })

  observeEvent(input$sat26_part_f_continue, {
    sat26_go_to("part_g")
  })

  observeEvent(input$sat26_part_g_back, {
    sat26_go_to("part_f")
  })

  observeEvent(input$sat26_part_g_continue, {
    sat26_go_to("part_g2")
  })

  observeEvent(input$sat26_part_g2_back, {
    sat26_go_to("part_g")
  })

  observeEvent(input$sat26_part_g2_continue, {
    sat26_go_to("part_h")
  })

  observeEvent(input$sat26_part_h_back, {
    sat26_go_to("part_g2")
  })

  observeEvent(input$sat26_part_h_continue, {
    sat26_go_to("part_i")
  })

  observeEvent(input$sat26_part_i_back, {
    sat26_go_to("part_h")
  })

  observeEvent(input$sat26_part_i_continue, {
    sat26_go_to("part_j")
  })

  observeEvent(input$sat26_part_j_back, {
    sat26_go_to("part_i")
  })

  observeEvent(input$sat26_part_j_continue, {
    sat26_go_to("part_k")
  })

  observeEvent(input$sat26_part_k_back, {
    sat26_go_to("part_j")
  })

  observeEvent(input$sat26_part_k_continue, {
    sat26_go_to("part_l")
  })

  observeEvent(input$sat26_part_l_back, {
    sat26_go_to("part_k")
  })

  observeEvent(input$sat26_part_l_finish, {
    tryCatch({
      sat26_save_draft()
      saved_record <- withProgress(message = "Guardando encuesta SAT26 en Supabase", value = 0, {
        incProgress(0.35, detail = "Preparando respuestas")
        record <- sat26_submit_to_supabase()
        incProgress(1, detail = "Encuesta guardada")
        record
      })
      showModal(modalDialog(
        title = NULL,
        easyClose = TRUE,
        size = "m",
        div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Gracias por completar la encuesta"),
          p("Gracias por tomarse el tiempo para completar esta encuesta. Esta información será analizada y los resultados se resumirán a nivel regional."),
          p("Estamos comprometidos con la protección de la información y no proporcionaremos información a nivel de país sin autorización."),
          p(class = "sat26-draft-code", paste("Código único guardado:", saved_record$codigo_unico[[1]]))
        ),
        footer = modalButton("Cerrar")
      ))
    }, error = function(error) {
      showModal(modalDialog(
        title = "No se pudo guardar en Supabase",
        easyClose = TRUE,
        div(
          class = "module-panel",
          p("La encuesta permanece guardada localmente en este navegador con su código único. Revise la conexión a Supabase y vuelva a intentar finalizar."),
          p(class = "text-danger", conditionMessage(error))
        ),
        footer = modalButton("Cerrar")
      ))
    })
  })

  observeEvent(input$sat26_plan_integrado_estado, {
    if (nzchar(value_or_default(input$sat26_plan_integrado_estado, ""))) {
      updateRadioButtons(session, "sat26_plan_aedes_estado", selected = character(0))
      updateRadioButtons(session, "sat26_plan_anopheles_estado", selected = character(0))
    }
    sat26_save_draft()
  }, ignoreInit = TRUE)

  observeEvent(list(input$sat26_plan_aedes_estado, input$sat26_plan_anopheles_estado), {
    if (nzchar(value_or_default(input$sat26_plan_aedes_estado, "")) || nzchar(value_or_default(input$sat26_plan_anopheles_estado, ""))) {
      updateRadioButtons(session, "sat26_plan_integrado_estado", selected = character(0))
    }
    sat26_save_draft()
  }, ignoreInit = TRUE)

  observeEvent(input$sat26_plan_integrado_caracteristicas, {
    if (length(input$sat26_plan_integrado_caracteristicas %||% character(0)) > 0) {
      updateCheckboxGroupInput(session, "sat26_plan_aedes_caracteristicas", selected = character(0))
      updateCheckboxGroupInput(session, "sat26_plan_anopheles_caracteristicas", selected = character(0))
    }
    sat26_save_draft()
  }, ignoreInit = TRUE)

  observeEvent(list(input$sat26_plan_aedes_caracteristicas, input$sat26_plan_anopheles_caracteristicas), {
    if (length(input$sat26_plan_aedes_caracteristicas %||% character(0)) > 0 || length(input$sat26_plan_anopheles_caracteristicas %||% character(0)) > 0) {
      updateCheckboxGroupInput(session, "sat26_plan_integrado_caracteristicas", selected = character(0))
    }
    sat26_save_draft()
  }, ignoreInit = TRUE)

  observeEvent(input$sat26_plan_estado_reset, {
    updateRadioButtons(session, "sat26_plan_aedes_estado", selected = character(0))
    updateRadioButtons(session, "sat26_plan_anopheles_estado", selected = character(0))
    updateRadioButtons(session, "sat26_plan_integrado_estado", selected = character(0))
    sat26_save_draft()
  })

  observeEvent(input$sat26_plan_caracteristicas_reset, {
    updateCheckboxGroupInput(session, "sat26_plan_aedes_caracteristicas", selected = character(0))
    updateCheckboxGroupInput(session, "sat26_plan_anopheles_caracteristicas", selected = character(0))
    updateCheckboxGroupInput(session, "sat26_plan_integrado_caracteristicas", selected = character(0))
    sat26_save_draft()
  })

  observeEvent(
    list(
      input$sat26_plan_tipo,
      input$sat26_plan_estado,
      input$sat26_plan_caracteristicas_tipo,
      input$sat26_plan_caracteristicas,
      input$sat26_plan_aedes_estado,
      input$sat26_plan_anopheles_estado,
      input$sat26_plan_integrado_estado,
      input$sat26_plan_aedes_caracteristicas,
      input$sat26_plan_anopheles_caracteristicas,
      input$sat26_plan_integrado_caracteristicas,
      input$sat26_plan_aedes_nombre,
      input$sat26_plan_aedes_anio,
      input$sat26_plan_aedes_documento,
      input$sat26_plan_anopheles_nombre,
      input$sat26_plan_anopheles_anio,
      input$sat26_plan_anopheles_documento,
      input$sat26_plan_integrado_nombre,
      input$sat26_plan_integrado_anio,
      input$sat26_plan_integrado_documento,
      input$sat26_reglamentos_nacionales,
      input$sat26_reglamentos_documento,
      input$sat26_prioridad_planes,
      input$sat26_asistencia_planes,
      input$sat26_normas_vigilancia_aedes,
      input$sat26_normas_control_aedes,
      input$sat26_normas_vigilancia_anopheles,
      input$sat26_normas_control_anopheles,
      input$sat26_plan_elemento_control_aedes,
      input$sat26_plan_elemento_vigilancia_aedes,
      input$sat26_plan_elemento_control_anopheles,
      input$sat26_plan_elemento_vigilancia_anopheles,
      input$sat26_plan_elemento_equipos_insecticidas,
      input$sat26_plan_elemento_resistencia,
      input$sat26_plan_elemento_recursos_humanos,
      input$sat26_plan_elemento_formacion,
      input$sat26_plan_elemento_participacion,
      input$sat26_plan_elemento_sistemas_info,
      input$sat26_plan_elemento_investigacion,
      input$sat26_plan_elemento_vulnerables,
      input$sat26_plan_elemento_monitoreo,
      input$sat26_descentralizacion,
      input$sat26_legislacion_24_48,
      input$sat26_grupo_interministerial,
      input$sat26_grupo_interministerial_reunion,
      input$sat26_prioridad_agenda,
      input$sat26_factores_prioridad,
      input$sat26_factores_prioridad_otros,
      input$sat26_frecuencia_interministerial,
      input$sat26_motivadores_institucion,
      input$sat26_motivadores_institucion_otros
    ),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    list(
      input$sat26_presupuesto_vigilancia,
      input$sat26_presupuesto_control,
      input$sat26_fin_vigilancia_presupuesto,
      input$sat26_fin_vigilancia_gestion,
      input$sat26_fin_control_presupuesto,
      input$sat26_fin_control_gestion
    ),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    c(
      lapply(sat26_hr_scalar_ids, function(id) input[[id]]),
      list(input$sat26_rrhh_capacitacion_sistemas)
    ),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    lapply(sat26_hr_cont_scalar_ids, function(id) input[[id]]),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    c(
      lapply(sat26_control_scalar_ids, function(id) input[[id]]),
      list(
        input$sat26_control_aedes_actividades,
        input$sat26_control_aedes_otras,
        input$sat26_control_anopheles_actividades,
        input$sat26_control_anopheles_otras,
        input$sat26_control_irs_metodos,
        input$sat26_control_larvicidas_metodos,
        input$sat26_control_irs_aedes_metodos,
        input$sat26_control_nebulizacion_metodos
      )
    ),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    c(
      lapply(sat26_surveillance_scalar_ids, function(id) input[[id]]),
      list(
        input$sat26_vigilancia_aedes_trampas,
        input$sat26_vigilancia_anopheles_trampas,
        input$sat26_vigilancia_decisiones
      )
    ),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    c(
      lapply(sat26_infra_scalar_ids, function(id) input[[id]]),
      list(input$sat26_infra_laboratorio_capacidades)
    ),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    c(
      lapply(sat26_info_scalar_ids, function(id) input[[id]]),
      list(
        input$sat26_info_vigilancia_herramientas,
        input$sat26_info_control_herramientas,
        input$sat26_info_vigilancia_recoleccion,
        input$sat26_info_vigilancia_almacenamiento,
        input$sat26_info_vigilancia_reporte,
        input$sat26_info_control_recoleccion,
        input$sat26_info_control_almacenamiento,
        input$sat26_info_control_reporte
      )
    ),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    c(
      lapply(sat26_community_scalar_ids, function(id) input[[id]]),
      list(
        input$sat26_comunidad_actividades,
        input$sat26_comunidad_momento
      )
    ),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    c(
      lapply(sat26_research_scalar_ids, function(id) input[[id]]),
      lapply(sat26_research_reference_ids, function(id) input[[id]])
    ),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  observeEvent(
    lapply(sat26_regional_scalar_ids, function(id) input[[id]]),
    {
      sat26_save_draft()
    },
    ignoreInit = TRUE
  )

  select_capture_subdivision <- function(subdivision) {
    active_area("data")
    active_module("capture")
    active_capture_subdivision(if (identical(active_capture_subdivision(), subdivision)) NULL else subdivision)
    active_dataset(NULL)
  }

  select_capture_dataset <- function(dataset) {
    active_area("data")
    active_module("capture")
    active_capture_subdivision(switch(
      dataset,
      formulario_1_colocacion_retiro_ovitrampa = "campo",
      formulario_5_alimentacion_conteo = "insectario",
      formulario_7_bioensayo_botella_cdc = "insectario",
      NULL
    ))
    active_dataset(dataset)
    submission_status("No se ha enviado ningún registro en esta sesión.")
  }

  observeEvent(input$show_capture_campo, {
    select_capture_subdivision("campo")
  })

  observeEvent(input$show_capture_insectario, {
    select_capture_subdivision("insectario")
  })

  observeEvent(input$show_capture_laboratorio, {
    select_capture_subdivision("laboratorio")
  })

  observeEvent(input$select_formulario_1_capture, {
    select_capture_dataset("formulario_1_colocacion_retiro_ovitrampa")
  })

  observeEvent(input$select_formulario_5_capture, {
    select_capture_dataset("formulario_5_alimentacion_conteo")
  })

  observeEvent(input$select_formulario_7_capture, {
    select_capture_dataset("formulario_7_bioensayo_botella_cdc")
  })

  observeEvent(input$show_visualization, {
    active_area("data")
    active_module("visualization")
    active_capture_subdivision(NULL)
    active_request_subdivision(NULL)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
  })

  observeEvent(input$show_request, {
    active_area("data")
    active_module("request")
    active_capture_subdivision(NULL)
    active_request_subdivision(NULL)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
  })

  select_request_subdivision <- function(subdivision) {
    active_area("data")
    active_module("request")
    active_capture_subdivision(NULL)
    active_request_subdivision(if (identical(active_request_subdivision(), subdivision)) NULL else subdivision)
    active_request_data_subdivision(NULL)
    active_dataset(NULL)
  }

  observeEvent(input$show_request_datos, {
    select_request_subdivision("datos")
  })

  observeEvent(input$show_request_reactivos, {
    select_request_subdivision("reactivos")
  })

  observeEvent(input$show_request_reactivos_larvicidas, {
    active_request_reactivos_category("larvicidas")
    active_request_reactivos_product(1L)
  })

  observeEvent(input$show_request_reactivos_adulticidas, {
    active_request_reactivos_category("adulticidas")
    active_request_reactivos_product(1L)
  })

  observeEvent(input$show_request_reactivos_residuales, {
    active_request_reactivos_category("residuales")
    active_request_reactivos_product(1L)
  })

  observeEvent(input$show_request_reactivos_product_1, {
    active_request_reactivos_product(1L)
  })

  observeEvent(input$show_request_reactivos_product_2, {
    active_request_reactivos_product(2L)
  })

  observeEvent(input$show_request_reactivos_product_3, {
    active_request_reactivos_product(3L)
  })

  observeEvent(input$show_request_equipo, {
    select_request_subdivision("equipo")
  })

  observeEvent(input$show_request_apoyo_tecnico, {
    select_request_subdivision("apoyo_tecnico")
  })

  select_request_data_subdivision <- function(subdivision) {
    active_area("data")
    active_module("request")
    active_capture_subdivision(NULL)
    active_request_subdivision("datos")
    active_request_data_subdivision(if (identical(active_request_data_subdivision(), subdivision)) NULL else subdivision)
    active_dataset(NULL)
  }

  observeEvent(input$show_request_data_campo, {
    select_request_data_subdivision("campo")
  })

  observeEvent(input$show_request_data_insectario, {
    select_request_data_subdivision("insectario")
  })

  observeEvent(input$show_request_data_laboratorio, {
    select_request_data_subdivision("laboratorio")
  })

  observeEvent(input$show_field_protocols, {
    active_area("protocols")
    active_module("protocol_field")
  })

  observeEvent(input$show_laboratory_protocols, {
    active_area("protocols")
    active_module("protocol_laboratory")
  })

  observeEvent(input$show_live_training, {
    active_area("training")
    active_module("training_live")
  })

  observeEvent(input$show_workshops_training, {
    active_area("training")
    active_module("training_workshops")
  })

  observeEvent(input$show_materials_training, {
    active_area("training")
    active_module("training_materials")
  })

  visualization_query <- reactiveVal(NULL)
  f7_visualization_records <- reactiveVal(data.frame())
  f7_visualization_error <- reactiveVal(NULL)
  f7_visualization_last_refresh <- reactiveVal(NULL)

  load_f7_visualization_records <- function(query) {
    f7_visualization_records(data.frame())
    f7_visualization_error(NULL)
    connection <- NULL
    tryCatch({
      connection <- connect_to_supabase()
      records <- dbGetQuery(
        connection,
        "
          select
            f.intake_id, f.codigo_bioensayo, f.fecha_realizacion_bioensayo,
            f.nombre_poblacion, f.bioensayo_intensidad, f.bioensayo_diagnostica_1x,
            f.sinergista_def, f.sinergista_pbo, f.sinergista_dm, f.resultado_diagnostico,
            f.insecticida, f.codigo_departamento, f.codigo_municipio, f.review_status,
            f.creado_en, f.actualizado_en,
            coalesce(d.departamento, f.codigo_departamento) as departamento,
            coalesce(m.municipio, f.codigo_municipio) as municipio
          from public.formulario_7_bioensayo_intake f
          left join public.catalogo_ubicacion_departamento d
            on d.pais = f.pais and d.codigo_departamento = f.codigo_departamento
          left join public.catalogo_ubicacion_municipio m
            on m.pais = f.pais and m.codigo_municipio = f.codigo_municipio
          where f.pais = $1
          order by f.fecha_realizacion_bioensayo, f.intake_id
        ",
        params = list(query$country)
      )
      if (nrow(records)) {
        records$fecha_realizacion_bioensayo <- as.Date(records$fecha_realizacion_bioensayo)
        records$creado_en <- as.POSIXct(records$creado_en)
        records$actualizado_en <- as.POSIXct(records$actualizado_en)
        records$tipo_bioensayo <- ifelse(
          records$bioensayo_diagnostica_1x,
          "Diagnóstica 1X",
          ifelse(
            !is.na(records$bioensayo_intensidad),
            paste("Intensidad", records$bioensayo_intensidad),
            "Sinergistas"
          )
        )
        records$codigo_municipio_mapa <- mapply(
          normalizar_codigo_municipio_mapa,
          query$country,
          records$codigo_departamento,
          records$codigo_municipio,
          USE.NAMES = FALSE
        )
        location_key <- paste(query$country, records$codigo_municipio_mapa, sep = "|")
        reference_key <- paste(
          formulario_7_visualization_locations$pais,
          formulario_7_visualization_locations$codigo_municipio,
          sep = "|"
        )
        location_index <- match(location_key, reference_key)
        records$latitude <- formulario_7_visualization_locations$latitude[location_index]
        records$longitude <- formulario_7_visualization_locations$longitude[location_index]
        municipio_index <- match(
          paste(query$country, records$codigo_municipio_mapa, sep = "|"),
          paste(ubicacion_municipio_catalogo$pais, ubicacion_municipio_catalogo$municipio_codigo, sep = "|")
        )
        records$municipio <- ifelse(
          !is.na(municipio_index),
          ubicacion_municipio_catalogo$municipio[municipio_index],
          records$municipio
        )
      }
      f7_visualization_records(records)
      f7_visualization_last_refresh(Sys.time())
    }, error = function(error) {
      f7_visualization_error(conditionMessage(error))
    }, finally = {
      if (!is.null(connection)) dbDisconnect(connection)
    })
  }

  observeEvent(input$search_visualization, {
    query <- list(
      country = input$visualization_country,
      dataset = input$visualization_dataset
    )
    visualization_query(query)

    if (identical(query$dataset, "formulario_7_bioensayo_botella_cdc")) {
      load_f7_visualization_records(query)
    }

    sites <- sample_collection_sites[
      sample_collection_sites$country == query$country &
        sample_collection_sites$dataset == query$dataset,
    ]

    if (identical(query$country, "Guatemala")) {
      session$onFlushed(function() {
        site_payload <- lapply(seq_len(nrow(sites)), function(index) {
          site <- sites[index, ]
          list(
            site = site$site,
            municipality = site$municipality,
            department = site$department,
            latitude = site$latitude,
            longitude = site$longitude,
            records = site$records
          )
        })

        session$sendCustomMessage(
          "renderGuatemalaCollectionMap",
          list(sites = site_payload)
        )
      }, once = TRUE)
    }
  })

  observeEvent(input$f7_visualization_refresh, {
    query <- visualization_query()
    if (is.null(query) || !identical(query$dataset, "formulario_7_bioensayo_botella_cdc")) return()
    load_f7_visualization_records(query)
  })

  output$f7_visualization_filters <- renderUI({
    records <- f7_visualization_records()
    if (!nrow(records)) return(NULL)
    dates <- records$fecha_realizacion_bioensayo
    filter_choices <- function(values, all_label = "Todos") {
      values <- sort(unique(stats::na.omit(as.character(values))))
      values <- values[nzchar(trimws(values))]
      c(setNames("all", all_label), setNames(values, values))
    }
    tagList(
      fluidRow(
        column(
          4,
          dateRangeInput(
            "f7_viz_date_range",
            "Fecha de realización",
            start = min(dates, na.rm = TRUE),
            end = max(dates, na.rm = TRUE),
            min = min(dates, na.rm = TRUE),
            max = max(dates, na.rm = TRUE),
            format = "yyyy-mm-dd"
          )
        ),
        column(
          4,
          selectInput(
            "f7_viz_project",
            "Código de bioensayo",
            choices = filter_choices(records$codigo_bioensayo)
          )
        ),
        column(
          4,
          selectInput(
            "f7_viz_type",
            "Tipo de bioensayo",
            choices = filter_choices(records$tipo_bioensayo)
          )
        )
      ),
      fluidRow(
        column(
          4,
          selectInput(
            "f7_viz_department",
            "Departamento",
            choices = filter_choices(records$departamento)
          )
        ),
        column(
          4,
          selectInput(
            "f7_viz_municipality",
            "Municipio",
            choices = filter_choices(records$municipio)
          )
        ),
        column(
          4,
          selectInput(
            "f7_viz_insecticide",
            "Insecticida",
            choices = filter_choices(records$insecticida)
          )
        ),
        column(
          4,
          selectInput(
            "f7_viz_population",
            "Población",
            choices = filter_choices(records$nombre_poblacion, "Todas")
          )
        ),
        column(
          4,
          selectInput(
            "f7_viz_diagnostic_result",
            "Resultado diagnóstico",
            choices = c(
              "Todos" = "all",
              "Susceptible" = "Suceptible",
              "Sospecha Resistencia" = "Sospecha de Resistencia",
              "Resistencia" = "Resistente",
              "No aplica" = "not_applicable"
            )
          )
        )
      )
    )
  })

  output$f7_visualization_error_message <- renderUI({
    error <- f7_visualization_error()
    if (is.null(error) || !nzchar(error)) return(NULL)
    div(
      class = "alert alert-danger",
      strong("No se pudieron consultar los datos del Formulario 7."),
      div(error)
    )
  })

  output$f7_visualization_refresh_status <- renderUI({
    refresh_time <- f7_visualization_last_refresh()
    records <- f7_visualization_records()
    tagList(
      div(
        class = "f7-viz-toolbar",
        div(
          class = "f7-viz-refresh-copy",
          if (is.null(refresh_time)) {
            "Presione Buscar para consultar los registros actuales."
          } else {
            paste0("Última actualización local: ", format(refresh_time, "%Y-%m-%d %H:%M:%S"), ". Registros cargados: ", nrow(records), ".")
          }
        ),
        actionButton("f7_visualization_refresh", "Actualizar", class = "btn-default")
      ),
      if (!is.null(refresh_time) && !nrow(records)) {
        div(
          class = "alert alert-info",
          "No hay registros de Formulario 7 para el país seleccionado. Los datos nuevos aparecerán aquí después de presionar Actualizar."
        )
      }
    )
  })

  f7_visualization_filtered <- reactive({
    records <- f7_visualization_records()
    if (!nrow(records)) return(records)
    date_range <- input$f7_viz_date_range
    if (!is.null(date_range) && length(date_range) == 2 && all(!is.na(date_range))) {
      records <- records[
        records$fecha_realizacion_bioensayo >= as.Date(date_range[[1]]) &
          records$fecha_realizacion_bioensayo <= as.Date(date_range[[2]]),
        , drop = FALSE
      ]
    }
    selected_filters <- list(
      codigo_bioensayo = input$f7_viz_project,
      tipo_bioensayo = input$f7_viz_type,
      departamento = input$f7_viz_department,
      municipio = input$f7_viz_municipality,
      insecticida = input$f7_viz_insecticide,
      nombre_poblacion = input$f7_viz_population
    )
    for (field in names(selected_filters)) {
      selected <- selected_filters[[field]]
      if (!is.null(selected) && length(selected) && !identical(selected, "all")) {
        records <- records[!is.na(records[[field]]) & records[[field]] == selected, , drop = FALSE]
      }
    }
    diagnostic_result <- input$f7_viz_diagnostic_result
    if (!is.null(diagnostic_result) && length(diagnostic_result) && !identical(diagnostic_result, "all")) {
      if (identical(diagnostic_result, "not_applicable")) {
        records <- records[is.na(records$resultado_diagnostico), , drop = FALSE]
      } else {
        records <- records[!is.na(records$resultado_diagnostico) & records$resultado_diagnostico == diagnostic_result, , drop = FALSE]
      }
    }
    records
  })

  f7_visualization_map_points <- reactive({
    records <- f7_visualization_filtered()
    records <- records[!is.na(records$latitude) & !is.na(records$longitude), , drop = FALSE]
    if (!nrow(records)) return(data.frame())
    group_key <- paste(
      records$codigo_departamento,
      records$codigo_municipio_mapa,
      records$nombre_poblacion,
      sep = "|"
    )
    groups <- split(records, group_key)
    points <- do.call(rbind, lapply(groups, function(group) {
      data.frame(
        location_id = paste(group$codigo_departamento[[1]], group$codigo_municipio_mapa[[1]], group$nombre_poblacion[[1]], sep = "|"),
        departamento = group$departamento[[1]],
        municipio = group$municipio[[1]],
        nombre_poblacion = group$nombre_poblacion[[1]],
        latitude = group$latitude[[1]],
        longitude = group$longitude[[1]],
        bioensayos = nrow(group),
        susceptible = sum(group$resultado_diagnostico %in% c("Suceptible", "Susceptible"), na.rm = TRUE),
        sospecha = sum(group$resultado_diagnostico == "Sospecha de Resistencia", na.rm = TRUE),
        resistente = sum(group$resultado_diagnostico == "Resistente", na.rm = TRUE),
        fecha_ultima = max(group$fecha_realizacion_bioensayo, na.rm = TRUE),
        tipos = paste(sort(unique(group$tipo_bioensayo)), collapse = ", "),
        stringsAsFactors = FALSE
      )
    }))
    municipality_key <- paste(points$departamento, points$municipio, sep = "|")
    point_order <- ave(seq_len(nrow(points)), municipality_key, FUN = seq_along)
    latitude_offsets <- c(0, 0.008, -0.008, 0.006, -0.006)
    longitude_offsets <- c(0, 0.008, -0.008, -0.006, 0.006)
    offset_index <- pmin(point_order, length(latitude_offsets))
    points$latitude <- points$latitude + latitude_offsets[offset_index]
    points$longitude <- points$longitude + longitude_offsets[offset_index]
    points$clasificacion <- ifelse(
      points$resistente > 0,
      "Resistencia",
      ifelse(
        points$sospecha > 0,
        "Sospecha Resistencia",
        ifelse(points$susceptible > 0, "Susceptible", "Sin resultado diagnóstico")
      )
    )
    points
  })

  f7_visualization_insecticide_counts <- reactive({
    records <- f7_visualization_filtered()
    insecticide_levels <- c("DDT", "Permetrina", "Deltametrina", "Bendiocarb", "Malatión", "Alfa-cipermetrina", "Lambda-cialotrina", "Temefos")
    result_levels <- c("Resistencia", "Sospecha Resistencia", "Susceptible")
    insecticide_codes <- toupper(trimws(as.character(records$insecticida)))
    insecticide <- ifelse(
      grepl("DDT", insecticide_codes),
      "DDT",
      ifelse(
        grepl("PER|PERMETRINA", insecticide_codes),
        "Permetrina",
        ifelse(
          grepl("DEL|DELTAMETRINA", insecticide_codes),
          "Deltametrina",
          ifelse(
            grepl("BEN|BENDIOCARB", insecticide_codes),
            "Bendiocarb",
            ifelse(
              grepl("MAL|MALATION|MALATHION|MALATIÓN", insecticide_codes),
              "Malatión",
              ifelse(
                grepl("ALF|ALFA", insecticide_codes),
                "Alfa-cipermetrina",
                ifelse(
                  grepl("LAM|LAMBDA", insecticide_codes),
                  "Lambda-cialotrina",
                  ifelse(grepl("TEM|TEMEFOS", insecticide_codes), "Temefos", NA_character_)
                )
              )
            )
          )
        )
      )
    )
    diagnostic_result <- ifelse(
      records$resultado_diagnostico == "Resistente",
      "Resistencia",
      ifelse(
        records$resultado_diagnostico == "Sospecha de Resistencia",
        "Sospecha Resistencia",
        ifelse(records$resultado_diagnostico %in% c("Suceptible", "Susceptible"), "Susceptible", NA_character_)
      )
    )
    as.matrix(table(
      factor(diagnostic_result, levels = result_levels),
      factor(insecticide, levels = insecticide_levels)
    ))
  })

  f7_visualization_active_insecticide_counts <- reactive({
    counts <- f7_visualization_insecticide_counts()
    if (!length(counts) || !ncol(counts)) return(counts[, 0, drop = FALSE])
    counts[, colSums(counts) > 0, drop = FALSE]
  })

  output$f7_visualization_kpis <- renderUI({
    records <- f7_visualization_filtered()
    mapped <- records[!is.na(records$latitude) & !is.na(records$longitude), , drop = FALSE]
    municipalities <- if (nrow(records)) length(unique(paste(records$codigo_departamento, records$codigo_municipio_mapa))) else 0L
    mapped_municipalities <- if (nrow(mapped)) length(unique(paste(mapped$codigo_departamento, mapped$codigo_municipio_mapa))) else 0L
    div(
      class = "f7-viz-kpi-grid",
      div(class = "f7-viz-kpi-card", span(class = "f7-viz-kpi-label", "Bioensayos"), span(class = "f7-viz-kpi-value", nrow(records))),
      div(class = "f7-viz-kpi-card", span(class = "f7-viz-kpi-label", "Poblaciones"), span(class = "f7-viz-kpi-value", length(unique(records$nombre_poblacion)))),
      div(class = "f7-viz-kpi-card", span(class = "f7-viz-kpi-label", "Municipios representados"), span(class = "f7-viz-kpi-value", municipalities)),
      div(class = "f7-viz-kpi-card", span(class = "f7-viz-kpi-label", "Municipios en mapa"), span(class = "f7-viz-kpi-value", mapped_municipalities)),
      div(class = "f7-viz-kpi-card", span(class = "f7-viz-kpi-label", "Pendientes de revisión"), span(class = "f7-viz-kpi-value", sum(records$review_status == "pending", na.rm = TRUE))),
      div(class = "f7-viz-kpi-card", span(class = "f7-viz-kpi-label", "Resultados resistentes"), span(class = "f7-viz-kpi-value", sum(records$resultado_diagnostico == "Resistente", na.rm = TRUE)))
    )
  })

  output$f7_visualization_map <- renderLeaflet({
    points <- f7_visualization_map_points()
    center <- switch(
      value_or_default(input$visualization_country, "Guatemala"),
      "El Salvador" = list(lng = -88.95, lat = 13.75, zoom = 8),
      "Guatemala" = list(lng = -90.35, lat = 15.45, zoom = 7),
      list(lng = -89.5, lat = 14.6, zoom = 6)
    )
    map <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = center$lng, lat = center$lat, zoom = center$zoom) |>
      addControl(
        html = "Ubicaciones aproximadas por municipio. Las coordenadas reales vendrán del vínculo con colecta y crianza.",
        position = "bottomleft"
      )
    if (!nrow(points)) {
      return(map |> addControl(html = "No hay registros para los filtros seleccionados.", position = "topright"))
    }
    department_overlay <- NULL
    if (identical(value_or_default(input$visualization_country, "Guatemala"), "Guatemala")) {
      department_overlay <- crear_geojson_departamentos_resultados_gt(points$departamento)
    }
    if (!is.null(department_overlay)) {
      map <- map |>
        addGeoJSON(
          geojson = department_overlay,
          group = "Departamentos con resultados",
          color = "#005F73",
          weight = 1.4,
          opacity = 0.72,
          fillColor = "#0A9396",
          fillOpacity = 0.16,
          smoothFactor = 0.5
        )
    }
    palette <- colorFactor(
      palette = c("#C62828", "#F9A825", "#9CA3AF", "#757575"),
      domain = c("Resistencia", "Sospecha Resistencia", "Susceptible", "Sin resultado diagnóstico")
    )
    popup <- paste0(
      "<strong>", htmltools::htmlEscape(points$nombre_poblacion), "</strong>",
      "<br>Municipio: ", htmltools::htmlEscape(points$municipio),
      "<br>Departamento: ", htmltools::htmlEscape(points$departamento),
      "<br>Bioensayos: ", points$bioensayos,
      "<br>Susceptible: ", points$susceptible,
      "<br>Sospecha Resistencia: ", points$sospecha,
      "<br>Resistencia: ", points$resistente,
      "<br>Última prueba: ", points$fecha_ultima,
      "<br>Tipos: ", htmltools::htmlEscape(points$tipos)
    )
    map <- map |>
      addCircleMarkers(
        data = points,
        lng = ~longitude,
        lat = ~latitude,
        layerId = ~location_id,
        radius = ~pmin(22, 7 + sqrt(bioensayos) * 3),
        color = "#ffffff",
        weight = 2,
        fillColor = ~palette(clasificacion),
        fillOpacity = 0.88,
        label = ~paste0(nombre_poblacion, ": ", bioensayos, " bioensayos"),
        popup = popup
      ) |>
      addLegend(
        position = "bottomright",
        pal = palette,
        values = points$clasificacion,
        title = "Resultado diagnóstico"
      )
    if (!is.null(department_overlay)) {
      map <- map |>
        addLayersControl(
          overlayGroups = c("Departamentos con resultados"),
          options = layersControlOptions(collapsed = FALSE)
        )
    }
    map |>
      fitBounds(
        lng1 = min(points$longitude, na.rm = TRUE),
        lat1 = min(points$latitude, na.rm = TRUE),
        lng2 = max(points$longitude, na.rm = TRUE),
        lat2 = max(points$latitude, na.rm = TRUE)
      )
  })

  output$f7_visualization_diagnostic_bar_chart <- renderPlot({
    counts <- f7_visualization_active_insecticide_counts()
    if (!ncol(counts)) {
      plot.new()
      text(0.5, 0.55, "Sin resultados diagnósticos para graficar", cex = 0.95, font = 2, col = "#526070")
      text(0.5, 0.45, "La figura se actualizará cuando existan registros con resultado diagnóstico.", cex = 0.78, col = "#6B7280")
      return()
    }
    totals <- colSums(counts)
    percentages <- sweep(counts, 2, pmax(totals, 1), "/") * 100
    previous_margins <- par(mar = c(4.8, 4.4, 1.2, 0.8))
    on.exit(par(previous_margins), add = TRUE)
    bar_positions <- barplot(
      percentages,
      col = c("#C62828", "#F9A825", "#9CA3AF"),
      border = NA,
      width = 0.62,
      space = 0.85,
      ylim = c(0, 118),
      ylab = "Porcentaje de bioensayos",
      names.arg = rep("", ncol(counts)),
      axes = FALSE,
      cex.lab = 0.82
    )
    axis(
      2,
      at = c(0, 25, 50, 75, 100),
      labels = paste0(c(0, 25, 50, 75, 100), "%"),
      las = 1,
      cex.axis = 0.72,
      lwd = 0,
      lwd.ticks = 1
    )
    plot_limits <- par("usr")
    segments(plot_limits[[1]], 0, plot_limits[[1]], 100, col = "#6B7280", lwd = 1)
    segments(plot_limits[[1]], 0, plot_limits[[2]], 0, col = "#6B7280", lwd = 1)
    label_y <- plot_limits[[3]] - 0.035 * diff(plot_limits[3:4])
    text(
      x = bar_positions,
      y = label_y,
      labels = colnames(counts),
      srt = 45,
      adj = 1,
      cex = 0.68,
      xpd = NA
    )
    text(
      x = bar_positions,
      y = 103,
      labels = totals,
      cex = 0.72,
      font = 2,
      xpd = NA
    )
    legend(
      "top",
      legend = c("Resistencia", "Sospecha\nResistencia", "Susceptible"),
      fill = c("#C62828", "#F9A825", "#9CA3AF"),
      border = NA,
      bty = "n",
      horiz = TRUE,
      cex = 0.8,
      inset = c(0, -0.05)
    )
  }, bg = "transparent", res = 110)

  output$f7_visualization_diagnostic_summary_table <- renderTable({
    counts <- f7_visualization_active_insecticide_counts()
    if (!ncol(counts)) return(data.frame(Mensaje = "Sin resultados diagnósticos para resumir."))
    data.frame(
      Insecticida = colnames(counts),
      Resistencia = as.integer(counts["Resistencia", ]),
      `Sospecha Resistencia` = as.integer(counts["Sospecha Resistencia", ]),
      Susceptible = as.integer(counts["Susceptible", ]),
      Total = as.integer(colSums(counts)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "xs")

  output$f7_visualization_type_status_table <- renderTable({
    records <- f7_visualization_filtered()
    if (!nrow(records)) return(data.frame(Mensaje = "Sin registros para resumir."))
    type_levels <- sort(unique(records$tipo_bioensayo))
    status_levels <- c("pending", "reviewed", "rejected")
    counts <- table(
      factor(records$tipo_bioensayo, levels = type_levels),
      factor(records$review_status, levels = status_levels)
    )
    data.frame(
      `Tipo de bioensayo` = rownames(counts),
      Pendiente = as.integer(counts[, "pending"]),
      Revisado = as.integer(counts[, "reviewed"]),
      Rechazado = as.integer(counts[, "rejected"]),
      Total = as.integer(rowSums(counts)),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "xs")

  output$f7_visualization_table <- renderTable({
    records <- f7_visualization_filtered()
    if (!nrow(records)) return(data.frame(Mensaje = "No hay registros para los filtros seleccionados."))
    display_date <- function(values) {
      formatted <- format(as.Date(values), "%d/%m/%Y")
      ifelse(is.na(formatted), "", formatted)
    }
    data.frame(
      `Código bioensayo` = records$codigo_bioensayo,
      Fecha = display_date(records$fecha_realizacion_bioensayo),
      Población = records$nombre_poblacion,
      Departamento = ifelse(is.na(records$departamento), "", records$departamento),
      Municipio = ifelse(is.na(records$municipio), "Sin ubicación aproximada", records$municipio),
      `Tipo de bioensayo` = records$tipo_bioensayo,
      Insecticida = records$insecticida,
      `Resultado diagnóstico` = ifelse(
        is.na(records$resultado_diagnostico),
        "No aplica",
        ifelse(
          records$resultado_diagnostico %in% c("Suceptible", "Susceptible"),
          "Susceptible",
          ifelse(records$resultado_diagnostico == "Sospecha de Resistencia", "Sospecha Resistencia", "Resistencia")
        )
      ),
      Estado = records$review_status,
      `Ingresado en` = display_date(records$creado_en),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  visualization_sites <- reactive({
    query <- visualization_query()
    if (is.null(query)) {
      return(sample_collection_sites[0, ])
    }

    sample_collection_sites[
      sample_collection_sites$country == query$country &
        sample_collection_sites$dataset == query$dataset,
    ]
  })

  output$visualization_results_area <- renderUI({
    query <- visualization_query()

    if (is.null(query)) {
      return(div(
        class = "alert alert-info",
        "Seleccione el país y el tipo de dato, luego presione Buscar para visualizar los sitios de colecta."
      ))
    }

    if (identical(query$dataset, "formulario_7_bioensayo_botella_cdc")) {
      return(tagList(
        div(
          class = "visualization-results-card",
          h4(paste("Mapa de bioensayos y poblaciones -", query$country)),
          p("Utilice los filtros para explorar los registros del Formulario 7. Los puntos actuales son centroides aproximados por municipio; los registros sin coordenada aproximada permanecen disponibles en las tablas."),
          uiOutput("f7_visualization_refresh_status"),
          uiOutput("f7_visualization_filters")
        ),
        uiOutput("f7_visualization_error_message"),
        uiOutput("f7_visualization_kpis"),
        div(
          class = "visualization-results-card",
          leafletOutput("f7_visualization_map", height = "365px")
        ),
        div(
          class = "f7-viz-diagnostic-layout",
          div(
            class = "visualization-results-card f7-viz-diagnostic-panel",
            h4("Resultados diagnósticos"),
            p("Distribución porcentual por insecticida; el total aparece arriba de cada barra."),
            div(
              class = "f7-viz-diagnostic-plot",
              plotOutput("f7_visualization_diagnostic_bar_chart", height = "455px")
            )
          ),
          div(
            class = "visualization-results-card f7-viz-summary-table",
            h4("Tabla resumen"),
            tableOutput("f7_visualization_diagnostic_summary_table"),
            tags$hr(),
            h4("Tipo y revisión"),
            tableOutput("f7_visualization_type_status_table")
          )
        ),
        div(
          class = "visualization-results-card",
          h4("Registros filtrados"),
          tableOutput("f7_visualization_table")
        )
      ))
    }

    if (!identical(query$country, "Guatemala")) {
      return(div(
        class = "alert alert-warning",
        "El mapa municipal interactivo está disponible primero para Guatemala. Agregaremos los mapas de los demás países cuando incorporemos sus capas administrativas."
      ))
    }

    if (!identical(query$dataset, "oviposition")) {
      return(div(
        class = "alert alert-warning",
        "Por ahora el prototipo de visualización usa puntos de ovipostura. Adultos y resistencia se activarán cuando esas tablas estén finalizadas."
      ))
    }

    tagList(
      div(
        class = "visualization-results-card",
        h4("Mapa de sitios de colecta - Guatemala"),
        p("Los límites municipales se cargan desde una capa GeoJSON local de 340 municipios. Los puntos actuales son ejemplos para validar el flujo visual."),
        div(id = "guatemala_collection_map", class = "visualization-map")
      ),
      div(
        class = "visualization-results-card",
        h4("Sitios encontrados"),
        tableOutput("visualization_sites_table")
      )
    )
  })

  output$visualization_sites_table <- renderTable({
    sites <- visualization_sites()
    if (nrow(sites) == 0) {
      return(data.frame(Mensaje = "No hay sitios disponibles para esta selección.", stringsAsFactors = FALSE))
    }

    data.frame(
      Sitio = sites$site,
      Municipio = sites$municipality,
      Departamento = sites$department,
      Registros = sites$records,
      Latitud = sites$latitude,
      Longitud = sites$longitude,
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$portal_intro_area <- renderUI({
    fluidRow(
      column(
        width = 12,
        div(
          class = "sat26-form-shell",
          div(
            class = "sat26-form-header",
            div(
              class = "sat26-form-logo-row",
              img(src = "entonet-header.jpeg", class = "header-logo sat26-form-logo-entonet", alt = "EntoNet"),
              img(src = "COMISCA.png", class = "sat26-form-logo sat26-form-logo-comisca", alt = "COMISCA y SICA")
            ),
            h2(HTML("Evaluación Regional de Necesidades y Fortalecimiento de Capacidades para la Vigilancia y el Control de Vectores en Centroamérica y República Dominicana")),
            p(class = "sat26-form-welcome", "¡Bienvenido(a)!"),
            p("Le invitamos a participar en esta encuesta, cuyo propósito es identificar las necesidades, fortalezas y oportunidades para fortalecer las capacidades regionales en materia de vigilancia y control de vectores en Centroamérica y República Dominicana."),
            p("Su participación es fundamental y sus respuestas contribuirán a orientar acciones, estrategias e iniciativas de fortalecimiento de capacidades en la región. La información proporcionada será tratada de manera confidencial y utilizada únicamente con fines relacionados con esta evaluación."),
            p(class = "sat26-form-emphasis", "Por favor, complete la siguiente encuesta."),
            p(class = "sat26-form-emphasis", "¡Muchas gracias por su valiosa colaboración!"),
            div(
              class = "sat26-form-subtitle",
              "Esta encuesta responde a la estrategia regional 2025-2030 del Sistema de Integración Centroamericano para el control de vectores."
            )
          )
        ),
        div(
          class = "sat26-form-info-panel",
          h3("Encuesta SAT26"),
          p("Antes de iniciar, la persona participante debe leer el consentimiento y confirmar que acepta participar."),
          div(
            class = "sat26-form-section-grid",
            div(
              class = "sat26-form-section-card",
              h4("Consentimiento"),
              p("Explica el propósito del estudio, el carácter voluntario de la participación, la confidencialidad de las respuestas y la posibilidad de retirarse en cualquier momento.")
            ),
            div(
              class = "sat26-form-section-card",
              h4("Contexto"),
              p("La encuesta se alinea con la estrategia 2025-2030 del Sistema de Integración Centroamericano para el control de vectores y con el fortalecimiento regional de EntoNet.")
            ),
            div(
              class = "sat26-form-section-card",
              h4("Flujo"),
              p("Después del consentimiento, la interfaz avanzará por secciones A-K para completar la evaluación de forma ordenada.")
            )
          ),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_open_consent", "Iniciar nueva encuesta", class = "btn-primary"),
            actionButton("sat26_open_sections", "Ver secciones", class = "btn-secondary")
          ),
          div(
            class = "sat26-resume-panel",
            h4("Continuar con un codigo existente"),
            div(
              class = "sat26-resume-row",
              textInput("sat26_resume_code", "Ingrese su codigo unico:", value = ""),
              actionButton("sat26_resume_lookup", "Buscar", class = "btn-default")
            ),
            uiOutput("sat26_resume_status_ui")
          )
        )
      )
    )
  })

  output$sat26_resume_status_ui <- renderUI({
    status <- sat26_resume_status()
    if (is.null(status) || !nzchar(status)) {
      return(NULL)
    }
    div(class = "sat26-resume-status", status)
  })

  output$portal_sidebar <- renderUI({
    area <- active_area()
    module <- active_module()
    capture_subdivision <- active_capture_subdivision()
    request_subdivision <- active_request_subdivision()
    request_data_subdivision <- active_request_data_subdivision()
    category_class <- function(value) paste(
      "sidebar-category",
      if (identical(area, value)) "sidebar-category-active" else ""
    )
    subitem_class <- function(value) paste(
      "sidebar-subitem",
      if (identical(module, value)) "sidebar-subitem-active" else ""
    )
    form_item_class <- function(value) paste(
      "sidebar-form-item",
      if (identical(active_dataset(), value)) "sidebar-form-item-active" else ""
    )
    subdivision_class <- function(value) paste(
      "sidebar-form-group-title",
      if (identical(capture_subdivision, value)) "sidebar-form-group-title-active" else ""
    )
    request_subdivision_class <- function(value) paste(
      "sidebar-form-group-title",
      if (identical(request_subdivision, value)) "sidebar-form-group-title-active" else ""
    )
    request_data_subdivision_class <- function(value) paste(
      "sidebar-form-item",
      if (identical(request_data_subdivision, value)) "sidebar-form-item-active" else ""
    )
    category_button <- function(id, label, value) {
      actionButton(
        id,
        tagList(span(label), span(class = "sidebar-chevron", if (identical(area, value)) "▼" else "▶")),
        class = category_class(value)
      )
    }

    tagList(
      div(class = "portal-sidebar-title", "Secciones"),
      category_button("show_data_area", "Datos", "data"),
      if (identical(area, "data")) div(
        class = "sidebar-submenu",
        actionButton("show_capture", "Captura de Datos", class = subitem_class("capture")),
        if (identical(module, "capture")) div(
          class = "sidebar-form-list",
          actionButton("show_capture_campo", "Campo", class = subdivision_class("campo")),
          actionButton("show_capture_insectario", "Insectario", class = subdivision_class("insectario")),
          actionButton("show_capture_laboratorio", "Laboratorio", class = subdivision_class("laboratorio"))
        ),
        actionButton("show_visualization", "Visualización de Datos", class = subitem_class("visualization")),
        actionButton("show_request", "Solicitudes", class = subitem_class("request")),
        if (identical(module, "request")) div(
          class = "sidebar-form-list",
          actionButton("show_request_datos", "Datos", class = request_subdivision_class("datos")),
          if (identical(request_subdivision, "datos")) tagList(
            actionButton("show_request_data_campo", "Campo", class = request_data_subdivision_class("campo")),
            actionButton("show_request_data_insectario", "Insectario", class = request_data_subdivision_class("insectario")),
            actionButton("show_request_data_laboratorio", "Laboratorio", class = request_data_subdivision_class("laboratorio"))
          ),
          actionButton("show_request_reactivos", "Reactivos", class = request_subdivision_class("reactivos")),
          actionButton("show_request_equipo", "Equipo", class = request_subdivision_class("equipo")),
          actionButton("show_request_apoyo_tecnico", "Apoyo Técnico", class = request_subdivision_class("apoyo_tecnico"))
        )
      ),
      category_button("show_protocols_area", "Protocolos", "protocols"),
      if (identical(area, "protocols")) div(
        class = "sidebar-submenu",
        actionButton("show_field_protocols", "Protocolos de Campo", class = subitem_class("protocol_field")),
        actionButton("show_laboratory_protocols", "Protocolos de Laboratorio", class = subitem_class("protocol_laboratory"))
      ),
      category_button("show_training_area", "Entrenamientos", "training"),
      if (identical(area, "training")) div(
        class = "sidebar-submenu",
        actionButton("show_live_training", "Capacitaciones en vivo", class = subitem_class("training_live")),
        actionButton("show_workshops_training", "Talleres prácticos", class = subitem_class("training_workshops")),
        actionButton("show_materials_training", "Materiales de apoyo", class = subitem_class("training_materials"))
      )
    )
  })

  output$module_area <- renderUI({
    area <- active_area()
    module <- active_module()

    if (is.null(area)) {
      return(uiOutput("portal_intro_area"))
    }

    if (identical(area, "sat26")) {
      sat26_plan_estado_choices <- c(
        "Versión finalizada" = "finalizada",
        "Versión borrador" = "borrador",
        "No existe plan" = "no_existe",
        "Desconozco" = "desconozco"
      )
      sat26_si_no_desconozco_choices <- c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")
      sat26_normas_choices <- c("Sí" = "si", "No" = "no", "En desarrollo" = "en_desarrollo", "Desconozco" = "desconozco")
      sat26_plan_elementos_choices <- c("Plan general" = "plan_general", "Plan separado" = "plan_separado", "No incluido" = "no_incluido", "Desconozco" = "desconozco")
      sat26_fin_suficiencia_choices <- c("Suficiente" = "suficiente", "Insuficiente" = "insuficiente", "Desconozco" = "desconozco")
      sat26_radio_field <- function(id, label, choices) {
        div(class = "sat26-form-field", radioButtons(id, HTML(label), choices = choices, selected = character(0)))
      }
      sat26_radio_field_html <- function(id, label, choice_names, choice_values) {
        div(
          class = "sat26-form-field",
          radioButtons(
            id,
            HTML(label),
            choiceNames = choice_names,
            choiceValues = choice_values,
            selected = character(0)
          )
        )
      }
      sat26_text_field <- function(id, label, placeholder = "") {
        div(class = "sat26-form-field", textInput(id, HTML(label), placeholder = placeholder))
      }
      sat26_textarea_field <- function(id, label, placeholder = "") {
        div(class = "sat26-form-field", textAreaInput(id, HTML(label), placeholder = placeholder, rows = 3))
      }

      if (identical(module, "part_i")) {
        sat26_info_system_choices <- c(
          "Recolección de datos" = "recoleccion",
          "Almacenamiento de datos" = "almacenamiento",
          "Presentación de datos" = "presentacion",
          "Desconozco" = "desconozco"
        )
        sat26_info_collect_choices <- c(
          "Formularios en papel" = "papel",
          "Smartphone o tablet" = "smartphone_tablet",
          "Fotografías" = "fotografias",
          "Escáneres de código de barras" = "codigos_barras",
          "Sistemas de posicionamiento global (GPS)" = "gps",
          "Actualmente en planificación para captura electrónica de datos" = "planificacion_electronica",
          "Otro" = "otro",
          "Desconozco" = "desconozco"
        )
        sat26_info_storage_choices <- c(
          "Papel" = "papel",
          "Excel" = "excel",
          "DHIS2" = "dhis2",
          "Otra base de datos en línea" = "otra_bd_linea",
          "Base de datos en Access" = "access",
          "Actualmente desarrollando almacenamiento en línea" = "desarrollando_linea",
          "Otro" = "otro",
          "Desconozco" = "desconozco"
        )
        sat26_info_report_choices <- c(
          "Manualmente en Excel" = "excel_manual",
          "Paneles en línea (gráficas, tablas, etc.)" = "paneles_linea",
          "Mapas electrónicos" = "mapas_electronicos",
          "Informes generados automáticamente desde una base de datos en línea" = "informes_automaticos",
          "Otro" = "otro",
          "Desconozco" = "desconozco"
        )
        sat26_info_limit_choices <- c(
          "Suficiente" = "suficiente",
          "Menor a lo suficiente" = "menor_suficiente",
          "Gravemente limitante" = "gravemente_limitante",
          "Desconozco" = "desconozco"
        )
        sat26_checkbox_field <- function(id, label, choices) {
          div(class = "sat26-form-field", checkboxGroupInput(id, HTML(label), choices = choices, selected = character(0)))
        }

        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte I: Sistemas de Información"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p("Esta sección documenta las herramientas utilizadas para recolectar, almacenar, presentar y utilizar datos de vigilancia y control vectorial.")
          ),
          h4("I1. Uso conocido de herramientas"),
          sat26_checkbox_field("sat26_info_vigilancia_herramientas", "I1a. Indique para cuál de los siguientes sistemas conoce el uso de herramientas en vigilancia vectorial:", sat26_info_system_choices),
          sat26_checkbox_field("sat26_info_control_herramientas", "I1b. Indique para cuál de los siguientes sistemas conoce el uso de herramientas en control vectorial:", sat26_info_system_choices),
          h4("I2. Recolección de datos de vigilancia vectorial"),
          sat26_radio_field("sat26_info_vigilancia_recoleccion_usa", "I2a. ¿Utiliza alguna herramienta para la recolección de datos de vigilancia vectorial?", sat26_si_no_desconozco_choices),
          sat26_checkbox_field("sat26_info_vigilancia_recoleccion", "I2b. Manejo de datos: ¿qué herramientas se utilizan actualmente en el campo para registrar datos de vigilancia vectorial?", sat26_info_collect_choices),
          conditionalPanel(condition = "input.sat26_info_vigilancia_recoleccion && input.sat26_info_vigilancia_recoleccion.indexOf('otro') >= 0", sat26_textarea_field("sat26_info_vigilancia_recoleccion_otro", "I2b.1 Si ha seleccionado “Otro”, indique qué otras herramientas se utilizan para registrar datos de vigilancia vectorial:")),
          sat26_textarea_field("sat26_info_vigilancia_apps", "I2c. ¿Qué aplicaciones o software se utilizan actualmente para recolectar datos de vigilancia vectorial?"),
          h4("I3. Almacenamiento de datos de vigilancia vectorial"),
          sat26_checkbox_field("sat26_info_vigilancia_almacenamiento", "I3. ¿Qué herramientas se utilizan actualmente para almacenar datos de vigilancia vectorial?", sat26_info_storage_choices),
          conditionalPanel(condition = "input.sat26_info_vigilancia_almacenamiento && input.sat26_info_vigilancia_almacenamiento.indexOf('otro') >= 0", sat26_textarea_field("sat26_info_vigilancia_almacenamiento_otro", "I3.1 Si ha seleccionado “Otro”, indique qué otras herramientas se utilizan para almacenar datos de vigilancia vectorial:")),
          h4("I4. Reporte de datos de vigilancia vectorial"),
          sat26_checkbox_field("sat26_info_vigilancia_reporte", "I4. ¿Qué herramientas se utilizan actualmente para presentar o reportar datos de vigilancia vectorial?", sat26_info_report_choices),
          conditionalPanel(condition = "input.sat26_info_vigilancia_reporte && input.sat26_info_vigilancia_reporte.indexOf('otro') >= 0", sat26_textarea_field("sat26_info_vigilancia_reporte_otro", "I4.1 Si ha seleccionado “Otro”, indique qué otras herramientas se utilizan para presentar o reportar datos de vigilancia vectorial:")),
          sat26_radio_field("sat26_info_vigilancia_limitacion", "I5. ¿Los sistemas de información actuales limitan el uso de datos de vigilancia vectorial para la toma de decisiones?", sat26_info_limit_choices),
          h4("I6. Recolección de datos de control vectorial"),
          sat26_radio_field("sat26_info_control_recoleccion_usa", "I6a. ¿Utiliza herramientas de recolección de datos para control vectorial?", sat26_si_no_desconozco_choices),
          sat26_checkbox_field("sat26_info_control_recoleccion", "I6b. Manejo de datos: ¿qué herramientas se utilizan actualmente en el campo para registrar datos de control vectorial?", sat26_info_collect_choices),
          conditionalPanel(condition = "input.sat26_info_control_recoleccion && input.sat26_info_control_recoleccion.indexOf('otro') >= 0", sat26_textarea_field("sat26_info_control_recoleccion_otro", "I6b.1 Si ha seleccionado “Otro”, indique qué otras herramientas se utilizan para registrar datos de control vectorial:")),
          sat26_textarea_field("sat26_info_control_apps", "I6c. ¿Qué aplicaciones o software se utilizan actualmente para recolectar datos de control vectorial?"),
          h4("I7. Almacenamiento de datos de control vectorial"),
          sat26_checkbox_field("sat26_info_control_almacenamiento", "I7. ¿Qué herramientas se utilizan actualmente para almacenar datos de control vectorial?", sat26_info_storage_choices),
          conditionalPanel(condition = "input.sat26_info_control_almacenamiento && input.sat26_info_control_almacenamiento.indexOf('otro') >= 0", sat26_textarea_field("sat26_info_control_almacenamiento_otro", "I7.1 Si ha seleccionado “Otro”, indique qué otras herramientas se utilizan para almacenar datos de control vectorial:")),
          h4("I8. Reporte de datos de control vectorial"),
          sat26_checkbox_field("sat26_info_control_reporte", "I8. ¿Qué herramientas se utilizan actualmente para presentar o reportar datos de control vectorial?", sat26_info_report_choices),
          conditionalPanel(condition = "input.sat26_info_control_reporte && input.sat26_info_control_reporte.indexOf('otro') >= 0", sat26_textarea_field("sat26_info_control_reporte_otro", "I8.1 Si ha seleccionado “Otro”, indique qué otras herramientas se utilizan para presentar o reportar datos de control vectorial:")),
          sat26_radio_field("sat26_info_control_limitacion", "I9. ¿Los sistemas de información actuales limitan el uso de datos de control vectorial para la toma de decisiones?", sat26_info_limit_choices),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_i_back", "Volver a Parte H", class = "btn-secondary"),
            actionButton("sat26_part_i_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_j")) {
        sat26_community_participation_choices <- c(
          "Sí" = "si",
          "No" = "no",
          "Desconozco las contribuciones que la comunidad ha hecho en la vigilancia o control vectorial" = "desconozco"
        )
        sat26_community_activity_choices <- c(
          "Vigilancia vectorial" = "vigilancia_vectorial",
          "Control vectorial" = "control_vectorial",
          "Comunicación de riesgos / educación" = "comunicacion_riesgos_educacion",
          "Reporte de enfermedades o brotes" = "reporte_enfermedades_brotes",
          "Investigación" = "investigacion",
          "Otro" = "otro",
          "Desconozco" = "desconozco"
        )
        sat26_community_timing_choices <- c(
          "Después de la declaración de un brote y la movilización de una fuerza de tarea" = "despues_brote",
          "Regularmente durante el año" = "regularmente_anio",
          "Antes del inicio de la temporada de lluvias" = "antes_lluvias",
          "Otro" = "otro",
          "Desconozco" = "desconozco"
        )
        sat26_checkbox_field <- function(id, label, choices) {
          div(class = "sat26-form-field", checkboxGroupInput(id, HTML(label), choices = choices, selected = character(0)))
        }

        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte J: Participación comunitaria y comunicación de riesgos"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p("Esta sección documenta si las comunidades participaron en actividades estructuradas de vigilancia o control vectorial, y en qué momentos o actividades se involucraron.")
          ),
          sat26_radio_field("sat26_comunidad_participacion", "J1. ¿Participaron miembros de la comunidad en la implementación de actividades de vigilancia o control vectorial entre 2022 y 2024? Se refiere a actividades comunitarias estructuradas, no a reportes espontáneos o contribuciones informales.", sat26_community_participation_choices),
          sat26_checkbox_field("sat26_comunidad_actividades", "J2. ¿En qué actividades participaron los miembros de la comunidad entre 2022 y 2024?", sat26_community_activity_choices),
          conditionalPanel(condition = "input.sat26_comunidad_actividades && input.sat26_comunidad_actividades.indexOf('otro') >= 0", sat26_textarea_field("sat26_comunidad_actividades_otro", "J2.1 Si seleccionó la opción de “Otro”, describa brevemente en qué actividades participaron miembros de la comunidad:")),
          sat26_checkbox_field("sat26_comunidad_momento", "J3. ¿Cuándo se involucró a las comunidades entre 2022 y 2024?", sat26_community_timing_choices),
          conditionalPanel(condition = "input.sat26_comunidad_momento && input.sat26_comunidad_momento.indexOf('otro') >= 0", sat26_textarea_field("sat26_comunidad_momento_otro", "J3.1 Si seleccionó la opción de “Otro”, indique en qué periodo de tiempo se involucró a las comunidades:")),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_j_back", "Volver a Parte I", class = "btn-secondary"),
            actionButton("sat26_part_j_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_k")) {
        sat26_research_yes_no_agenda <- c(
          "Sí" = "si",
          "No" = "no",
          "Desconozco si el ministerio cuenta con una agenda de investigación" = "desconozco"
        )
        sat26_research_yes_no_priorities <- c(
          "Sí" = "si",
          "No" = "no",
          "Desconozco si la agenda de investigación prioriza el control y vigilancia de vectores" = "desconozco"
        )
        sat26_research_yes_no_operations <- c(
          "Sí" = "si",
          "No" = "no",
          "Desconozco las investigaciones operativas que el Ministerio ha realizado respecto a la vigilancia y control de vectores" = "desconozco"
        )
        sat26_reference_type_choices <- c(
          "Nombre" = "nombre",
          "Hipervínculo" = "hipervinculo",
          "Documento" = "documento",
          "Desconozco" = "desconozco"
        )
        sat26_reference_fields <- function() {
          tagList(lapply(seq_along(sat26_research_reference_items), function(index) {
            ref_id <- names(sat26_research_reference_items)[[index]]
            ref_label <- sat26_research_reference_items[[ref_id]]
            tagList(
              div(
                class = "sat26-form-field",
                checkboxGroupInput(
                  paste0("sat26_investigacion_ref_tipo_", ref_id),
                  paste0("K3b.", index, ".1 Seleccione el tipo de referencia disponible para: ", ref_label),
                  choices = sat26_reference_type_choices,
                  selected = character(0)
                )
              ),
              sat26_textarea_field(paste0("sat26_investigacion_ref_nombre_", ref_id), paste0("K3c.", index, ".1 Ingrese los nombres que conozca para: ", ref_label)),
              sat26_textarea_field(paste0("sat26_investigacion_ref_link_", ref_id), paste0("K3d.", index, ".1 Ingrese los hipervínculos disponibles para: ", ref_label)),
              sat26_textarea_field(paste0("sat26_investigacion_ref_documento_", ref_id), paste0("K3e.", index, ".1 Describa los documentos disponibles o indique dónde se encuentran para: ", ref_label))
            )
          }))
        }

        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte K: Investigación operativa liderada por el Ministerio de Salud"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p(HTML("Esta sección documenta si el Ministerio de Salud cuenta con una agenda de investigación y si ha desarrollado investigación operativa para mejorar la vigilancia o el control de vectores, incluyendo <em>Aedes</em>."))
          ),
          h4("K1. Agenda de investigación"),
          sat26_radio_field("sat26_investigacion_agenda", "K1a. ¿El Ministerio de Salud cuenta con una agenda priorizada de investigación básica y aplicada?", sat26_research_yes_no_agenda),
          sat26_radio_field("sat26_investigacion_agenda_vectores", "K1b. ¿La agenda de investigación incluye prioridades relacionadas con el control y vigilancia de vectores?", sat26_research_yes_no_priorities),
          h4(HTML("K2. Investigación operativa para <em>Aedes</em>")),
          sat26_radio_field("sat26_investigacion_operativa_aedes", "K2. ¿El Ministerio de Salud llevó a cabo investigación operativa para mejorar la vigilancia o el control de <em>Aedes</em> entre 2021 y 2023?", sat26_research_yes_no_operations),
          h4("K3. Datos del proyecto de investigación operativa"),
          p(class = "sat26-section-note", "En caso de haber respondido “Sí”, proporcione los siguientes datos del proyecto de investigación operativa."),
          sat26_textarea_field("sat26_investigacion_titulo", "K3a. ¿Cuál fue el título del proyecto?"),
          sat26_textarea_field("sat26_investigacion_referencias", "K3b. Referencias. Incluya referencias a publicaciones, informes o conjuntos de datos relevantes."),
          p(class = "sat26-section-note", "K3b-K3e. Para cada tipo de referencia, seleccione primero si cuenta con nombre, hipervínculo o documento; luego complete los campos disponibles. En esta versión local, los documentos se describen como texto; más adelante podemos convertirlos en carga de archivos."),
          sat26_reference_fields(),
          h4("K5. Permiso para compartir con EntoNet"),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p("La Red de Entomología de Salud Pública (EntoNet) es una iniciativa regional establecida en 2017 que reúne a países de América Central y República Dominicana para fortalecer la vigilancia entomológica y el control vectorial."),
            p("Con sede en la UVG y en coordinación con SECOMISCA, EntoNet fomenta la colaboración técnica para mejorar el manejo integrado de vectores y el monitoreo de resistencia a insecticidas en la región.")
          ),
          sat26_radio_field("sat26_investigacion_compartir_entonet", "K5. ¿Está de acuerdo en que la información proporcionada en esta encuesta sea compartida con EntoNet?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          h4("K6. Permiso para compartir con OPS/OMS"),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p("La OMS (Organización Mundial de la Salud) es el organismo especializado de las Naciones Unidas dedicado a la salud y seguridad mundiales.")
          ),
          sat26_radio_field("sat26_investigacion_compartir_ops", "K6. ¿Está de acuerdo en que la información proporcionada en esta encuesta sea compartida con la OPS/OMS si así se solicita?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_k_back", "Volver a Parte J", class = "btn-secondary"),
            actionButton("sat26_part_k_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_l")) {
        sat26_regional_punto_focal_choices <- c(
          "Sí" = "si",
          "No" = "no",
          "Desconozco si existe un punto focal para colaborar con socios regionales" = "desconozco",
          "No entiendo qué es un punto focal" = "no_entiendo"
        )
        sat26_regional_agreements_choices <- c(
          "Sí" = "si",
          "No" = "no",
          "En desarrollo" = "en_desarrollo",
          "Desconozco los planes que se tienen respecto a acuerdos entre socios regionales" = "desconozco"
        )
        sat26_regional_frequency_choices <- c(
          "No se comparte información con otros países" = "no_comparte",
          "Cada semana" = "semanal",
          "Cada 2 semanas" = "cada_2_semanas",
          "Cada mes" = "mensual",
          "Cada 3 meses" = "cada_3_meses",
          "Cada 6 meses" = "cada_6_meses",
          "Cada año" = "anual",
          "Desconozco si se comparte información con otros países" = "desconozco"
        )
        sat26_regional_invasive_choices <- c(
          "Sí" = "si",
          "No" = "no",
          "En desarrollo" = "en_desarrollo",
          "Existen sistemas operativos para el monitoreo de vectores, pero se encuentran en desuso" = "en_desuso",
          "Desconozco si existen sistemas operativos para el monitoreo de vectores" = "desconozco"
        )
        sat26_regional_climate_choices <- c(
          "Sí" = "si",
          "No" = "no",
          "En desarrollo" = "en_desarrollo",
          "Desconozco la coordinación que se tiene con socios regionales" = "desconozco"
        )
        sat26_regional_network_choices <- c(
          "Sí, EntoNet" = "si_entonet",
          "Sí, otra red equivalente" = "si_otra_red",
          "No" = "no",
          "Desconozco si mi país participa activamente en redes regionales" = "desconozco",
          "No entiendo qué es una red regional como EntoNet" = "no_entiendo"
        )
        sat26_regional_mechanism_choices <- c(
          "Sí" = "si",
          "No" = "no",
          "Desconozco si mi país cuenta con mecanismos para compartir conocimientos con socios regionales" = "desconozco"
        )
        sat26_regional_platform_choices <- c(
          "Sí" = "si",
          "No" = "no",
          "Ocasionalmente" = "ocasionalmente",
          "Desconozco si mi país comparte información con otros países de la región" = "desconozco",
          "Sé que mi país comparte información con otros, pero desconozco qué tan frecuentemente lo hace" = "comparte_desconoce_frecuencia"
        )

        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte L: Colaboración regional"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p("Esta sección documenta la colaboración regional, el intercambio de información y los mecanismos de coordinación para vigilancia y control vectorial.")
          ),
          sat26_radio_field("sat26_regional_punto_focal", "L1. ¿Existe un punto focal designado para colaborar con socios regionales en vigilancia y control vectorial? Un punto focal se refiere a una persona designada específicamente para apoyar en estos temas y avanzar los esfuerzos de varias entidades hacia un objetivo común.", sat26_regional_punto_focal_choices),
          sat26_radio_field("sat26_regional_acuerdos", "L2. ¿Existen acuerdos formales con socios regionales para apoyar la vigilancia y control vectorial, por ejemplo intercambio de datos o asistencia técnica?", sat26_regional_agreements_choices),
          sat26_radio_field("sat26_regional_frecuencia_intercambio", "L3. ¿Qué tan frecuentemente se comparte información, datos y buenas prácticas con otros países a través de plataformas regionales?", sat26_regional_frequency_choices),
          sat26_radio_field("sat26_regional_sistemas_invasoras", "L4. ¿Existen sistemas operativos para el monitoreo de especies invasoras de vectores que representen amenazas transfronterizas y para contribuir a sistemas regionales de alerta temprana?", sat26_regional_invasive_choices),
          sat26_radio_field("sat26_regional_cambio_climatico", "L5. ¿Existe coordinación sistemática con socios regionales para abordar los impactos del cambio climático sobre la distribución de vectores entre países?", sat26_regional_climate_choices),
          sat26_radio_field("sat26_regional_redes", "L6. ¿Su país participa activamente en redes regionales como EntoNet o su equivalente, incluyendo participación y contribuciones consistentes?", sat26_regional_network_choices),
          sat26_radio_field("sat26_regional_mecanismos", "L7. ¿Existen mecanismos formales para intercambiar conocimientos técnicos con socios regionales, por ejemplo a través de EntoNet u otra red equivalente, que incluyan participación y contribuciones consistentes?", sat26_regional_mechanism_choices),
          sat26_radio_field("sat26_regional_plataformas", "L8. ¿Se comparten regularmente información, datos y buenas prácticas a través de plataformas regionales establecidas con otros países de la región?", sat26_regional_platform_choices),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_l_back", "Volver a Parte K", class = "btn-secondary"),
            actionButton("sat26_part_l_finish", "Finalizar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_h")) {
        sat26_infra_vig_choices <- c("Suficiente" = "suficiente", "Insuficiente" = "insuficiente", "Desconozco" = "desconozco")
        sat26_infra_control_choices <- c("Suficiente" = "suficiente", "Menor a lo suficiente" = "menor_suficiente", "Gravemente limitante" = "gravemente_limitante", "Desconozco" = "desconozco")
        sat26_lab_capacity_choices <- c("PCR" = "pcr", "ELISA" = "elisa", "Laboratorio entomológico de campo (bioensayos, microscopía)" = "ento_campo", "Semi-campo" = "semi_campo", "Otro" = "otro", "Desconozco" = "desconozco")
        sat26_infra_fields <- function(prefix, label_prefix, items, choices) {
          tagList(lapply(seq_along(items), function(index) {
            item_id <- names(items)[[index]]
            sat26_radio_field(paste0(prefix, item_id), paste0(label_prefix, index, ". ", items[[item_id]]), choices)
          }))
        }

        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte H: Infraestructura y Logística"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p("Esta sección documenta la suficiencia de infraestructura y logística para actividades de vigilancia y control vectorial, así como capacidades de laboratorio e insectario.")
          ),
          h4("H1. Infraestructura para vigilancia vectorial"),
          p(class = "sat26-section-note", "H1a. En cuanto a la vigilancia vectorial, ¿qué formas de infraestructura tienen los recursos adecuados para desarrollar sus actividades?"),
          sat26_infra_fields("sat26_infra_vigilancia_", "H1a.", sat26_infra_vigilancia_items, sat26_infra_vig_choices),
          sat26_radio_field("sat26_infra_laboratorio_gestionado", "H1b. ¿El laboratorio de la zona está gestionado y mantenido por el programa de control vectorial?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          sat26_radio_field("sat26_infra_conoce_laboratorio", "H1c. ¿Conoce acerca de las capacidades de los laboratorios en la zona?", c("Sí" = "si", "No hay laboratorios cercanos en la zona" = "no_laboratorios_cercanos", "Desconozco" = "desconozco")),
          div(class = "sat26-form-field", checkboxGroupInput("sat26_infra_laboratorio_capacidades", "H1c.1 ¿Cuál es la capacidad de los laboratorios?", choices = sat26_lab_capacity_choices, selected = character(0))),
          conditionalPanel(condition = "input.sat26_infra_laboratorio_capacidades && input.sat26_infra_laboratorio_capacidades.indexOf('otro') >= 0", sat26_textarea_field("sat26_infra_laboratorio_capacidad_otro", "H1c.2 Si ha seleccionado “Otro”, describa brevemente con qué otras capacidades cuenta su laboratorio:")),
          sat26_radio_field("sat26_infra_insectario_gestionado", "H1d. ¿El insectario está gestionado y mantenido por el programa de control vectorial?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          sat26_radio_field("sat26_infra_colonia_aedes", "H1e. ¿Mantienen una colonia de laboratorio de mosquitos <em>Aedes</em>?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          sat26_radio_field("sat26_infra_colonia_anopheles", "H1f. ¿Mantienen una colonia de laboratorio de mosquitos <em>Anopheles</em>?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          h4("H2. Infraestructura para control vectorial"),
          p(class = "sat26-section-note", "H2a. En cuanto al control vectorial, ¿cuáles de las siguientes infraestructuras son suficientes o limitantes?"),
          sat26_infra_fields("sat26_infra_control_", "H2a.", sat26_infra_control_items, sat26_infra_control_choices),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_h_back", "Volver a Parte G", class = "btn-secondary"),
            actionButton("sat26_part_h_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_g2")) {
        sat26_sites_choices <- c("0" = "0", "1-4" = "1_4", "5-9" = "5_9", "10+" = "10_mas", "Desconozco" = "desconozco")
        sat26_ident_adult_choices <- c("Morfológicamente con microscopio" = "morfologia", "Molecular (PCR)" = "pcr", "No se identifican" = "no_identifican", "Otro" = "otro", "Desconozco" = "desconozco")
        sat26_ident_larva_choices <- c("Como adultos con microscopio" = "adultos_microscopio", "Como larvas con microscopio" = "larvas_microscopio", "Molecular (PCR)" = "pcr", "No se identifican" = "no_identifican", "Otro" = "otro", "Desconozco" = "desconozco")
        sat26_checkbox_html <- function(id, label, choices) {
          div(class = "sat26-form-field", checkboxGroupInput(id, HTML(label), choiceNames = lapply(names(choices), HTML), choiceValues = unname(choices), selected = character(0)))
        }

        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte G: Vigilancia Vectorial (cont.)"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p(HTML("Esta continuación documenta el manejo rutinario de la vigilancia de <em>Anopheles</em> y el uso de datos entomológicos para orientar decisiones de control vectorial."))
          ),
          h4(HTML("G3. Manejo de vigilancia rutinaria de <em>Anopheles</em>")),
          sat26_radio_field("sat26_vigilancia_anopheles_sitios", "G3a. Respecto a la rutina de vigilancia, ¿cuántos sitios fueron monitoreados? Un sitio de vigilancia equivale a toda una aldea o área, que puede contener múltiples estaciones de muestreo o trampas.", sat26_sites_choices),
          sat26_radio_field("sat26_vigilancia_anopheles_conoce_trampas", "G3b. ¿Conoce qué trampas se usan para recolectar mosquitos <em>Anopheles</em> adultos?", sat26_si_no_desconozco_choices),
          sat26_checkbox_html("sat26_vigilancia_anopheles_trampas", "G3c. ¿Qué trampas utiliza para recolectar mosquitos <em>Anopheles</em> adultos?", sat26_surveillance_anopheles_traps),
          conditionalPanel(condition = "input.sat26_vigilancia_anopheles_trampas && input.sat26_vigilancia_anopheles_trampas.indexOf('otro') >= 0", sat26_textarea_field("sat26_vigilancia_anopheles_trampa_otro", "G3c.1 Si ha seleccionado “Otro”, describa brevemente cómo es la trampa usada para recolectar mosquitos <em>Anopheles</em> adultos:")),
          sat26_radio_field("sat26_vigilancia_anopheles_ident_adultos", "G3d. ¿Cómo se identifican los mosquitos <em>Anopheles</em> adultos para la vigilancia rutinaria?", sat26_ident_adult_choices),
          conditionalPanel(condition = "input.sat26_vigilancia_anopheles_ident_adultos == 'otro'", sat26_textarea_field("sat26_vigilancia_anopheles_ident_adultos_otro", "G3d.1 Si ha seleccionado “Otro”, describa brevemente el método de identificación usado para mosquitos <em>Anopheles</em> adultos:")),
          sat26_radio_field("sat26_vigilancia_anopheles_ident_larvas", "G3e. ¿Cómo se identifican las larvas de <em>Anopheles</em> para la vigilancia rutinaria?", sat26_ident_larva_choices),
          conditionalPanel(condition = "input.sat26_vigilancia_anopheles_ident_larvas == 'otro'", sat26_textarea_field("sat26_vigilancia_anopheles_ident_larvas_otro", "G3e.1 Si ha seleccionado “Otro”, describa brevemente el método de identificación usado para larvas de <em>Anopheles</em>:")),
          h4("G4. Uso de datos entomológicos"),
          sat26_radio_field("sat26_vigilancia_uso_datos", "G4a. ¿Se utilizan los datos entomológicos para orientar las decisiones de control vectorial?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          sat26_checkbox_html("sat26_vigilancia_decisiones", "G4b. ¿Qué decisiones de control vectorial se informan mediante datos entomológicos?", sat26_surveillance_decisions),
          conditionalPanel(condition = "input.sat26_vigilancia_decisiones && input.sat26_vigilancia_decisiones.indexOf('otro') >= 0", sat26_textarea_field("sat26_vigilancia_decisiones_otro", "G4b.1 Si ha seleccionado “Otro”, describa brevemente qué decisiones de control vectorial se informan mediante datos entomológicos:")),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_g2_back", "Volver a Parte G", class = "btn-secondary"),
            actionButton("sat26_part_g2_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_g")) {
        sat26_indicator_choices <- c("1+ (una vez al año o más)" = "1_mas", "1- (menos de una vez al año)" = "1_menos", "0 (no se monitorea)" = "0", "Desconozco" = "desconozco")
        sat26_sites_choices <- c("0" = "0", "1-4" = "1_4", "5-9" = "5_9", "10+" = "10_mas", "Desconozco" = "desconozco")
        sat26_ident_adult_choices <- c("Morfológicamente con microscopio" = "morfologia", "Molecular (PCR)" = "pcr", "No se identifican" = "no_identifican", "Otro" = "otro", "Desconozco" = "desconozco")
        sat26_ident_larva_choices <- c("Como adultos con microscopio" = "adultos_microscopio", "Como larvas con microscopio" = "larvas_microscopio", "Molecular (PCR)" = "pcr", "No se identifican" = "no_identifican", "Otro" = "otro", "Desconozco" = "desconozco")
        sat26_checkbox_html <- function(id, label, choices) {
          div(class = "sat26-form-field", checkboxGroupInput(id, HTML(label), choiceNames = lapply(names(choices), HTML), choiceValues = unname(choices), selected = character(0)))
        }
        sat26_indicator_fields <- function(prefix, label_prefix) {
          tagList(lapply(seq_along(sat26_surveillance_indicators), function(index) {
            indicator_id <- names(sat26_surveillance_indicators)[[index]]
            sat26_radio_field(paste0(prefix, indicator_id), paste0(label_prefix, index, ". ", sat26_surveillance_indicators[[indicator_id]]), sat26_indicator_choices)
          }))
        }

        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte G: Actividades de Vigilancia Vectorial"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p(HTML("Esta primera pantalla documenta indicadores de vigilancia para vectores <em>Aedes</em> y el manejo rutinario de la vigilancia de <em>Aedes</em>. La siguiente pantalla continuará con <em>Anopheles</em> y uso de datos entomológicos."))
          ),
          h4(HTML("G1. Indicadores de vigilancia vectorial")),
          p(class = "sat26-section-note", HTML("G1a. En cualquier región del país, ¿con qué frecuencia se midieron los siguientes indicadores de vigilancia para vectores <em>Aedes</em> entre 2022 y 2024?")),
          sat26_indicator_fields("sat26_vigilancia_aedes_ind_", "G1a."),
          p(class = "sat26-section-note", HTML("G1b. En cualquier región del país, ¿con qué frecuencia se midieron los siguientes indicadores de vigilancia para vectores <em>Anopheles</em> entre 2022 y 2024?")),
          sat26_indicator_fields("sat26_vigilancia_anopheles_ind_", "G1b."),
          h4(HTML("G2. Manejo de vigilancia rutinaria de <em>Aedes</em>")),
          sat26_radio_field("sat26_vigilancia_aedes_sitios", "G2a. Respecto a la rutina de vigilancia, ¿cuántos sitios fueron monitoreados? Un sitio de vigilancia equivale a toda una aldea o área, que puede contener múltiples estaciones de muestreo o trampas.", sat26_sites_choices),
          sat26_radio_field("sat26_vigilancia_aedes_conoce_trampas", "G2b. ¿Conoce qué trampas se usan para recolectar mosquitos <em>Aedes</em> adultos?", sat26_si_no_desconozco_choices),
          sat26_checkbox_html("sat26_vigilancia_aedes_trampas", "G2c. ¿Qué trampas utiliza para recolectar mosquitos <em>Aedes</em> adultos?", sat26_surveillance_aedes_traps),
          conditionalPanel(condition = "input.sat26_vigilancia_aedes_trampas && input.sat26_vigilancia_aedes_trampas.indexOf('otro') >= 0", sat26_textarea_field("sat26_vigilancia_aedes_trampa_otro", "G2c.1 Si ha seleccionado “Otro”, describa brevemente cómo es la trampa usada para recolectar mosquitos <em>Aedes</em> adultos:")),
          sat26_radio_field("sat26_vigilancia_aedes_ident_adultos", "G2d. ¿Cómo se identifican los mosquitos <em>Aedes</em> adultos para la vigilancia rutinaria?", sat26_ident_adult_choices),
          conditionalPanel(condition = "input.sat26_vigilancia_aedes_ident_adultos == 'otro'", sat26_textarea_field("sat26_vigilancia_aedes_ident_adultos_otro", "G2d.1 Si ha seleccionado “Otro”, describa brevemente el método de identificación usado para mosquitos <em>Aedes</em> adultos:")),
          sat26_radio_field("sat26_vigilancia_aedes_ident_larvas", "G2e. ¿Cómo se identifican las larvas de <em>Aedes</em> para la vigilancia rutinaria?", sat26_ident_larva_choices),
          conditionalPanel(condition = "input.sat26_vigilancia_aedes_ident_larvas == 'otro'", sat26_textarea_field("sat26_vigilancia_aedes_ident_larvas_otro", "G2e.1 Si ha seleccionado “Otro”, describa brevemente el método de identificación usado para larvas de mosquitos <em>Aedes</em>:")),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_g_back", "Volver a Parte F", class = "btn-secondary"),
            actionButton("sat26_part_g_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_f")) {
        sat26_freq_choices <- c("Rutinaria" = "rutinaria", "Estacional" = "estacional", "Solo durante brotes" = "brotes", "Nunca" = "nunca", "Desconozco" = "desconozco")
        sat26_quality_choices <- c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")
        sat26_activity_checkbox <- function(id, label, choices) {
          div(class = "sat26-form-field", checkboxGroupInput(id, HTML(label), choiceNames = lapply(names(choices), HTML), choiceValues = unname(choices), selected = character(0)))
        }
        sat26_activity_frequency_fields <- function(prefix, label_prefix, activities) {
          tagList(lapply(seq_along(activities), function(index) {
            activity_id <- names(activities)[[index]]
            sat26_radio_field(paste0(prefix, activity_id), paste0(label_prefix, index, ". ", activities[[activity_id]]), sat26_freq_choices)
          }))
        }
        sat26_quality_fields <- function(prefix, activities) {
          tagList(lapply(seq_along(activities), function(index) {
            activity_id <- names(activities)[[index]]
            sat26_radio_field(paste0(prefix, activity_id), paste0("F5a.", index, ". ", activities[[activity_id]]), sat26_quality_choices)
          }))
        }

        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte F: Actividades de Control de Vectores para mosquitos"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p(HTML("Esta sección documenta las actividades de control vectorial implementadas para vectores <em>Aedes</em> y <em>Anopheles</em>, su frecuencia y los métodos de garantía de calidad utilizados."))
          ),
          h4(HTML("F1. Control de vectores <em>Aedes</em>")),
          sat26_radio_field("sat26_control_aedes_conocimiento", "F1a. ¿Tiene conocimiento de las actividades implementadas en 2025 respecto al control de vectores <em>Aedes</em>?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          sat26_activity_checkbox(
            "sat26_control_aedes_actividades",
            "F1b. Para el control de vectores <em>Aedes</em>, ¿cuáles de las siguientes actividades se implementaron en 2025?",
            c(sat26_control_aedes_activities, "Desconozco" = "desconozco")
          ),
          p(class = "sat26-section-note", HTML("F1c. Para el control de vectores <em>Aedes</em>, ¿qué tan frecuentemente se implementaron estas actividades en 2025?")),
          sat26_activity_frequency_fields("sat26_control_aedes_frecuencia_", "F1c.", sat26_control_aedes_activities),
          h4(HTML("F2. Otras actividades de control para <em>Aedes</em>")),
          sat26_radio_field("sat26_control_aedes_otras_implemento", "F2a. ¿Implementó alguna otra actividad de control vectorial para <em>Aedes</em> durante 2022-2024?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          sat26_activity_checkbox(
            "sat26_control_aedes_otras",
            "F2b. ¿Qué otras actividades de control de <em>Aedes</em> se utilizaron en 2025? Marque todas las que apliquen.",
            c("Casas a prueba de mosquitos" = "casas_prueba", "Control biológico larval (peces o copépodos)" = "control_biologico", "Redes para hamacas" = "redes_hamacas", "Técnica de insecto estéril" = "insecto_esteril", "Otro" = "otro", "Desconozco" = "desconozco")
          ),
          conditionalPanel(condition = "input.sat26_control_aedes_otras && input.sat26_control_aedes_otras.indexOf('otro') >= 0", sat26_textarea_field("sat26_control_aedes_otras_descripcion", "F2b.1 Si seleccionó “Otro”, describa brevemente qué otras actividades se realizaron para el control de <em>Aedes</em>:")),
          h4(HTML("F3. Control de vectores <em>Anopheles</em>")),
          sat26_radio_field("sat26_control_anopheles_conocimiento", "F3a. ¿Tiene conocimiento de las actividades implementadas en 2025 respecto al control de vectores <em>Anopheles</em>?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          sat26_activity_checkbox(
            "sat26_control_anopheles_actividades",
            "F3b. Para el control de vectores <em>Anopheles</em>, ¿cuáles de las siguientes actividades se implementaron en 2025?",
            c(sat26_control_anopheles_activities, "Desconozco" = "desconozco")
          ),
          p(class = "sat26-section-note", HTML("F3c. Para el control de vectores <em>Anopheles</em>, ¿qué tan frecuentemente se implementaron estas actividades en 2025?")),
          sat26_activity_frequency_fields("sat26_control_anopheles_frecuencia_", "F3c.", sat26_control_anopheles_activities),
          h4(HTML("F4. Otras actividades de control para <em>Anopheles</em>")),
          sat26_radio_field("sat26_control_anopheles_otras_implemento", "F4a. ¿Implementó alguna otra actividad de control vectorial para <em>Anopheles</em> durante 2022-2024?", c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")),
          sat26_activity_checkbox(
            "sat26_control_anopheles_otras",
            "F4b. ¿Qué otras actividades de control para <em>Anopheles</em> se han utilizado en 2025? Marque todas las que correspondan.",
            c("Rociado en exteriores" = "rociado_exteriores", "Casas a prueba de mosquitos" = "casas_prueba", "Rociado de barrera" = "rociado_barrera", "Control biológico larval (peces o copépodos)" = "control_biologico", "Redes para hamacas" = "redes_hamacas", "Otro" = "otro", "Desconozco" = "desconozco")
          ),
          conditionalPanel(condition = "input.sat26_control_anopheles_otras && input.sat26_control_anopheles_otras.indexOf('otro') >= 0", sat26_textarea_field("sat26_control_anopheles_otras_descripcion", "F4b.1 Si seleccionó “Otro”, describa brevemente qué otras actividades se realizaron para el control de <em>Anopheles</em>:")),
          h4("F5. Garantía de calidad en intervenciones de control vectorial"),
          p(class = "sat26-section-note", "F5a. ¿Existe un método o sistema para evaluar las siguientes actividades?"),
          sat26_quality_fields("sat26_control_calidad_", sat26_control_quality_activities),
          h4("F5b. Métodos de evaluación"),
          sat26_activity_checkbox("sat26_control_irs_metodos", "F5b.1 Con respecto al rociado residual en interiores (IRS), ¿qué métodos se utilizan para medir la eficacia residual del insecticida?", c("Tasa de mortalidad de mosquitos adultos" = "mortalidad_adultos", "Densidad de mosquitos" = "densidad", "Índice de mosquitos inmaduros" = "indice_inmaduros", "Pruebas de contacto en superficie" = "contacto_superficie", "Análisis de la alteración de la pared rociada" = "pared_rociada", "Otro" = "otro", "Desconozco" = "desconozco")),
          conditionalPanel(condition = "input.sat26_control_irs_metodos && input.sat26_control_irs_metodos.indexOf('otro') >= 0", sat26_textarea_field("sat26_control_irs_otro", "F5b.1.1 Si seleccionó “Otro”, coloque los nombres de otros métodos para medir la eficacia residual del IRS:")),
          sat26_activity_checkbox("sat26_control_larvicidas_metodos", "F5b.2 Con respecto a los larvicidas, ¿qué métodos se utilizan para monitorear el impacto de su aplicación?", c("Evaluación de hábitats tratados con insecticida (presencia o abundancia de larvas)" = "habitats_tratados", "Inspecciones aleatorias de hábitats larvarios tratados" = "inspecciones", "Densidad de mosquitos adultos" = "densidad_adultos", "Otro" = "otro", "Desconozco" = "desconozco")),
          conditionalPanel(condition = "input.sat26_control_larvicidas_metodos && input.sat26_control_larvicidas_metodos.indexOf('otro') >= 0", sat26_textarea_field("sat26_control_larvicidas_otro", "F5b.2.1 Si seleccionó “Otro”, coloque los nombres de otros métodos para medir el impacto de la aplicación de larvicidas:")),
          sat26_activity_checkbox("sat26_control_irs_aedes_metodos", "F5b.3 En el caso de IRS-<em>Aedes</em>, ¿qué métodos se utilizan para medir la eficacia residual del insecticida?", c("Bioensayos de pared" = "bioensayos_pared", "Otro" = "otro", "Desconozco" = "desconozco")),
          conditionalPanel(condition = "input.sat26_control_irs_aedes_metodos && input.sat26_control_irs_aedes_metodos.indexOf('otro') >= 0", sat26_textarea_field("sat26_control_irs_aedes_otro", "F5b.3.1 Si seleccionó “Otro”, coloque los nombres de otros métodos para medir la eficacia residual del insecticida contra IRS-<em>Aedes</em>:")),
          sat26_activity_checkbox("sat26_control_nebulizacion_metodos", "F5b.4 En cuanto a la nebulización espacial, ¿qué métodos se utilizan para medir la eficacia de la intervención?", c("Evaluación de protocolos de nebulización" = "protocolos", "Evaluación de residuos" = "residuos", "Número de mosquitos adultos" = "adultos", "Otro" = "otro", "Desconozco" = "desconozco")),
          conditionalPanel(condition = "input.sat26_control_nebulizacion_metodos && input.sat26_control_nebulizacion_metodos.indexOf('otro') >= 0", sat26_textarea_field("sat26_control_nebulizacion_otro", "F5b.4.1 Si seleccionó “Otro”, coloque los nombres de otros métodos para medir la eficacia de la nebulización espacial:")),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_f_back", "Volver a Parte E", class = "btn-secondary"),
            actionButton("sat26_part_f_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_e2")) {
        sat26_priority_choices <- c("Alta" = "alta", "Media" = "media", "Baja" = "baja", "Desconozco" = "desconozco")
        sat26_trained_choices_national <- c("Desconozco" = "desconozco", "0" = "0", "1-5" = "1_5", "6-10" = "6_10", "10+" = "10_mas")
        sat26_trained_choices_subnational <- c("Desconozco" = "desconozco", "0" = "0", "1-10" = "1_10", "10+" = "10_mas")
        sat26_formador_choices <- c("0" = "0", "1-5" = "1_5", "5+" = "5_mas", "Desconozco" = "desconozco")
        sat26_hr_skill_fields <- function(prefix, label_prefix, choices) {
          tagList(lapply(seq_along(sat26_hr_skill_suffixes), function(index) {
            skill_id <- names(sat26_hr_skill_suffixes)[[index]]
            sat26_radio_field(
              paste0(prefix, skill_id),
              paste0(label_prefix, index, ". ", sat26_hr_skill_suffixes[[skill_id]]),
              choices
            )
          }))
        }

        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte E: Recursos Humanos (cont.)"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p("Esta continuación documenta necesidades de capacitación, formatos preferidos de aprendizaje, experiencias previas y potenciales instructores dentro del programa.")
          ),
          h4("E7. Acceso a capacitación"),
          sat26_radio_field(
            "sat26_rrhh_necesidad_capacitacion",
            "E7. ¿Quién tiene mayor necesidad de acceso a la capacitación: el personal nacional o subnacional?",
            c("Nacional" = "nacional", "Subnacional" = "subnacional", "Ambos" = "ambos", "Desconozco" = "desconozco")
          ),
          h4("E8. Priorización de la capacitación"),
          p(class = "sat26-section-note", "E8a. ¿Qué nivel de prioridad tienen las siguientes habilidades para la formación del personal en apoyo a la vigilancia vectorial?"),
          sat26_hr_skill_fields("sat26_rrhh_prioridad_", "E8a.", sat26_priority_choices),
          sat26_textarea_field(
            "sat26_rrhh_otras_areas",
            "E8b. ¿Hay otras áreas donde se requiera desarrollo de capacidades para apoyar las operaciones de vigilancia vectorial? Si la respuesta es sí, descríbalas:"
          ),
          h4("E9-E10. Personal capacitado en entomología en salud pública"),
          sat26_radio_field(
            "sat26_rrhh_capacitados_nacional",
            "E9. Indique el número de miembros del personal nacional que recibieron capacitación en entomología en salud pública durante 2022-2024.",
            sat26_trained_choices_national
          ),
          sat26_radio_field(
            "sat26_rrhh_capacitados_subnacional",
            "E10. Indique el número de miembros del personal subnacional que recibieron capacitación en entomología en salud pública durante 2022-2024.",
            sat26_trained_choices_subnational
          ),
          h4("E11-E13. Formato de los cursos de capacitación"),
          sat26_radio_field(
            "sat26_rrhh_modalidad_preferida",
            "E11. ¿Cuál es su modalidad de aprendizaje preferida?",
            c("Presencial / capacitación práctica" = "presencial_practica", "Presencial / curso corto basado en conferencias" = "presencial_conferencias", "Online / capacitación impartida mediante plataforma en línea" = "online", "Desconozco" = "desconozco")
          ),
          sat26_radio_field(
            "sat26_rrhh_online_implementacion",
            "E12. En caso de utilizar aprendizaje en línea, ¿cómo preferiría que se implementen los cursos?",
            c("Curso intensivo, durante periodo de tiempo corto (5 días o menos en una sola semana)" = "intensivo_corto", "Curso intensivo, durante periodo de tiempo largo (1 día por semana, durante 5 semanas)" = "intensivo_largo", "Desconozco" = "desconozco")
          ),
          sat26_radio_field(
            "sat26_rrhh_online_modalidad",
            "E13. ¿Qué modalidad de capacitación en línea le sería más útil para su aprendizaje?",
            c("Reuniones en línea interactivas en vivo" = "reuniones_vivo", "Videos pregrabados" = "videos_pregrabados", "Basada en conferencias" = "conferencias", "Basada en actividades" = "actividades", "Desconozco" = "desconozco")
          ),
          h4("E14-E15. Acceso y experiencia previa"),
          sat26_radio_field(
            "sat26_rrhh_acceso_computadora",
            "E14. ¿Tiene acceso a una computadora con conectividad a internet suficiente para completar la capacitación en línea?",
            c("Sí" = "si", "No" = "no", "Tengo acceso a una computadora, pero tendría poca conectividad" = "computadora_poca_conectividad", "Desconozco" = "desconozco")
          ),
          sat26_radio_field(
            "sat26_rrhh_cursos_previos",
            "E15. ¿Ha participado en cursos de capacitación previamente, ya sea por su cuenta o con su personal?",
            c("Sí" = "si", "No" = "no", "No lo recuerdo" = "no_recuerdo", "Desconozco" = "desconozco")
          ),
          sat26_textarea_field(
            "sat26_rrhh_cursos_previos_bien",
            "E15a. Mencione qué ha funcionado bien en cursos de capacitación previos en los que usted o su personal hayan participado:"
          ),
          sat26_textarea_field(
            "sat26_rrhh_cursos_previos_mal",
            "E15b. Mencione qué NO ha funcionado bien en cursos de capacitación previos en los que usted o su personal hayan participado:"
          ),
          h4("E16. Potenciales instructores"),
          sat26_radio_field(
            "sat26_rrhh_personal_cargo",
            "E16. ¿Tiene personal a su cargo?",
            c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")
          ),
          p(class = "sat26-section-note", "E16a. Potenciales instructores: ¿Cuenta con personal que se sienta con la confianza para capacitar a otros participantes en las siguientes habilidades?"),
          sat26_hr_skill_fields("sat26_rrhh_formadores_", "E16a.", sat26_formador_choices),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_e2_back", "Volver a Parte E", class = "btn-secondary"),
            actionButton("sat26_part_e2_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_e")) {
        sat26_hr_knowledge_choices <- c(
          "Desconozco existencia" = "desconozco_existencia",
          "No existe" = "no_existe",
          "Desconozco números" = "desconozco_numeros",
          "Conozco números" = "conozco_numeros"
        )
        sat26_hr_sufficiency_choices <- c(
          "Suficiente" = "suficiente",
          "Menor a lo suficiente" = "menor_suficiente",
          "Gravemente limitante" = "gravemente_limitante",
          "Desconozco" = "desconozco"
        )
        sat26_hr_gap_choices <- c(
          "Desconozco" = "desconozco",
          "Tenemos suficientes" = "suficientes",
          "No hay" = "no_hay",
          "Necesitamos más" = "necesitamos_mas"
        )
        sat26_hr_job_fields <- function(prefix, label_prefix, choices) {
          tagList(lapply(seq_along(sat26_hr_job_suffixes), function(index) {
            job_id <- names(sat26_hr_job_suffixes)[[index]]
            sat26_radio_field(
              paste0(prefix, job_id),
              paste0(label_prefix, index, ". ", sat26_hr_job_suffixes[[job_id]]),
              choices
            )
          }))
        }
        sat26_hr_count_fields <- function(prefix, label_prefix) {
          tagList(lapply(seq_along(sat26_hr_job_suffixes), function(index) {
            job_id <- names(sat26_hr_job_suffixes)[[index]]
            sat26_text_field(
              paste0(prefix, job_id),
              paste0(label_prefix, index, ". ", sat26_hr_job_suffixes[[job_id]], ":")
            )
          }))
        }

        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte E: Recursos Humanos"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p("Esta primera parte de Recursos Humanos documenta la existencia de un plan de personal, la disponibilidad de puestos clave a nivel nacional y subnacional, brechas de personal y sistemas de capacitación utilizados.")
          ),
          h4("E1. Plan de recursos humanos"),
          sat26_radio_field(
            "sat26_rrhh_plan",
            "E1. ¿Existe un plan de recursos humanos adecuado para el programa de control de enfermedades transmitidas por vectores? Un plan adecuado debería incluir los puestos del personal involucrado en el programa y explicar las funciones de cada uno.",
            c("Sí" = "si", "En desarrollo" = "en_desarrollo", "No" = "no", "Desconozco" = "desconozco")
          ),
          sat26_text_field("sat26_rrhh_organigrama", "E1.1 Si respondió “sí”, coloque el enlace o referencia al organigrama:"),
          h4("E2. Puestos a nivel nacional"),
          p(class = "sat26-section-note", "E2a. Respecto a los siguientes puestos de trabajo, indique de cuáles conoce la cantidad de miembros activos a nivel nacional."),
          sat26_hr_job_fields("sat26_rrhh_nacional_conoce_", "E2a.", sat26_hr_knowledge_choices),
          h4("E2b. Cantidad de personas a nivel nacional"),
          sat26_hr_count_fields("sat26_rrhh_nacional_cantidad_", "E2b."),
          h4("E3. Puestos a nivel subnacional"),
          p(class = "sat26-section-note", "E3a. Respecto a los siguientes puestos de trabajo, indique de cuáles conoce la cantidad de miembros activos a nivel subnacional."),
          sat26_hr_job_fields("sat26_rrhh_subnacional_conoce_", "E3a.", sat26_hr_knowledge_choices),
          h4("E3b. Cantidad de personas a nivel subnacional"),
          sat26_hr_count_fields("sat26_rrhh_subnacional_cantidad_", "E3b."),
          h4("E4. Suficiencia de personal"),
          p(class = "sat26-section-note", "Suficiente = existen recursos para llevar a cabo las actividades; Menor a lo suficiente = existen recursos para algunas, pero no todas las actividades; Gravemente limitante = no es posible implementar la mayoría de actividades; Desconozco = no conozco la cantidad de personal disponible."),
          sat26_radio_field("sat26_rrhh_suficiencia_vigilancia", "E4a. En cuanto a la vigilancia vectorial, ¿hay suficiente personal para realizar las actividades planificadas?", sat26_hr_sufficiency_choices),
          sat26_radio_field("sat26_rrhh_suficiencia_control", "E4b. En cuanto al control vectorial, ¿hay suficiente personal para realizar las actividades planificadas?", sat26_hr_sufficiency_choices),
          h4("E5. Brechas de personal"),
          p(class = "sat26-section-note", "E5a. Indique si se necesita más personal en los siguientes puestos para llevar a cabo las actividades de vigilancia y control vectorial. Incluye puestos vacantes y puestos nuevos que deben crearse."),
          sat26_hr_job_fields("sat26_rrhh_brecha_", "E5a.", sat26_hr_gap_choices),
          h4("E5b. Trabajadores adicionales necesarios"),
          sat26_hr_count_fields("sat26_rrhh_adicional_", "E5b."),
          h4("E6. Sistemas de capacitación y fortalecimiento de capacidades"),
          div(
            class = "sat26-form-field",
            checkboxGroupInput(
              "sat26_rrhh_capacitacion_sistemas",
              "E6. Marque los sistemas de capacitación y fortalecimiento de capacidades que se utilizan para entrenar al personal en su programa.",
              choices = c(
                "Capacitación en el puesto de trabajo" = "puesto_trabajo",
                "Cursos nacionales" = "cursos_nacionales",
                "Cursos regionales/internacionales" = "cursos_regionales_internacionales",
                "Formación de posgrado" = "posgrado",
                "Programa de formación de formadores" = "formadores",
                "Capacitación brindada por países vecinos (cooperación Sur-Sur)" = "sur_sur",
                "Otro" = "otro",
                "Desconozco" = "desconozco"
              ),
              selected = character(0)
            )
          ),
          conditionalPanel(
            condition = "input.sat26_rrhh_capacitacion_sistemas && input.sat26_rrhh_capacitacion_sistemas.indexOf('otro') >= 0",
            sat26_text_field("sat26_rrhh_capacitacion_otro", "E6.1 Si ha seleccionado “Otro”, describa brevemente los sistemas que se usan para entrenar al personal:")
          ),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_e_back", "Volver a Parte D", class = "btn-secondary"),
            actionButton("sat26_part_e_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_d")) {
        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte D: Finanzas"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p("Esta sección documenta si la vigilancia y el control vectorial de mosquitos están incluidos en el presupuesto nacional, y si las finanzas limitan las operaciones.")
          ),
          sat26_radio_field(
            "sat26_presupuesto_vigilancia",
            "D1. ¿Está incluida la vigilancia vectorial de mosquitos en el presupuesto anual del ministerio de salud nacional? Esto incluye, por ejemplo, el número de casos reportados de mosquitos.",
            c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")
          ),
          sat26_radio_field(
            "sat26_presupuesto_control",
            "D2. ¿Está incluido el control vectorial de mosquitos en el presupuesto anual del ministerio de salud nacional? Esto se refiere a intervenciones físicas, químicas o biológicas en los mosquitos. Por ejemplo, la aplicación de insecticidas.",
            c("Sí" = "si", "No" = "no", "Desconozco" = "desconozco")
          ),
          h4("D3. Finanzas para vigilancia vectorial"),
          p(class = "sat26-section-note", "Con respecto a la vigilancia o monitoreo vectorial, indique si las finanzas limitan las operaciones. Suficiente = existen recursos para llevar a cabo las actividades; Insuficiente = no es posible implementar la mayoría de las actividades; Desconozco = desconozco los recursos que se necesitan para llevar a cabo las actividades."),
          sat26_radio_field(
            "sat26_fin_vigilancia_presupuesto",
            "D3a. Presupuesto disponible: ¿se ha asignado un presupuesto adecuado para la vigilancia vectorial?",
            sat26_fin_suficiencia_choices
          ),
          sat26_radio_field(
            "sat26_fin_vigilancia_gestion",
            "D3b. Gestión financiera: ¿el sistema de gestión financiera facilita el acceso oportuno a los fondos?",
            sat26_fin_suficiencia_choices
          ),
          h4("D4. Finanzas para control vectorial"),
          p(class = "sat26-section-note", "Con respecto al control o intervención vectorial, indique si las finanzas limitan las operaciones. Suficiente = existen recursos para llevar a cabo las actividades; Insuficiente = no es posible implementar la mayoría de las actividades; Desconozco = desconozco los recursos que se necesitan para llevar a cabo las actividades."),
          sat26_radio_field(
            "sat26_fin_control_presupuesto",
            "D4a. Presupuesto disponible: ¿se ha calculado adecuadamente un presupuesto dentro del plan para control vectorial?",
            sat26_fin_suficiencia_choices
          ),
          sat26_radio_field(
            "sat26_fin_control_gestion",
            "D4b. Gestión financiera: ¿el sistema de gestión financiera apoya las actividades de vigilancia vectorial?",
            sat26_fin_suficiencia_choices
          ),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_d_back", "Volver a Parte C", class = "btn-secondary"),
            actionButton("sat26_part_d_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_c")) {
        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte C: Gobernanza"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro sat26-left-intro sat26-compact-intro",
            p("Esta sección documenta planes estratégicos, normativas, estructuras de coordinación y priorización nacional para la vigilancia y control de vectores transmitidos por mosquitos.")
          ),
          h4("C1. Planes estratégicos nacionales"),
          p(class = "sat26-section-note", HTML("C1a. ¿Cuenta su país con un plan estratégico de respuesta para enfermedades transmitidas por mosquitos que incorpore el control de <em>Aedes</em> y/o <em>Anopheles</em>?")),
          conditionalPanel(
            condition = "!input.sat26_plan_integrado_estado",
            sat26_radio_field("sat26_plan_aedes_estado", "C1a.1 Solo para <em>Aedes</em>", sat26_plan_estado_choices),
            sat26_radio_field("sat26_plan_anopheles_estado", "C1a.2 Solo para <em>Anopheles</em>", sat26_plan_estado_choices)
          ),
          conditionalPanel(
            condition = "!input.sat26_plan_aedes_estado && !input.sat26_plan_anopheles_estado",
            sat26_radio_field("sat26_plan_integrado_estado", "C1a.3 <em>Aedes</em> y <em>Anopheles</em> integrado", sat26_plan_estado_choices)
          ),
          div(
            class = "sat26-form-reset-row",
            actionButton("sat26_plan_estado_reset", "reset", class = "btn-default")
          ),
          p(class = "sat26-section-note", "C1b. De las siguientes características de planes estratégicos, seleccione aquellas de las cuales conoce el dato correspondiente:"),
          conditionalPanel(
            condition = "!input.sat26_plan_integrado_caracteristicas || input.sat26_plan_integrado_caracteristicas.length == 0",
            div(class = "sat26-form-field", checkboxGroupInput("sat26_plan_aedes_caracteristicas", HTML("C1b.1 Solo para <em>Aedes</em>"), choices = c("Nombre" = "nombre", "Último año de actualización" = "anio_actualizacion", "Documento del plan" = "documento", "Desconozco" = "desconozco"), selected = character(0))),
            div(class = "sat26-form-field", checkboxGroupInput("sat26_plan_anopheles_caracteristicas", HTML("C1b.2 Solo para <em>Anopheles</em>"), choices = c("Nombre" = "nombre", "Último año de actualización" = "anio_actualizacion", "Documento del plan" = "documento", "Desconozco" = "desconozco"), selected = character(0)))
          ),
          conditionalPanel(
            condition = "(!input.sat26_plan_aedes_caracteristicas || input.sat26_plan_aedes_caracteristicas.length == 0) && (!input.sat26_plan_anopheles_caracteristicas || input.sat26_plan_anopheles_caracteristicas.length == 0)",
            div(class = "sat26-form-field", checkboxGroupInput("sat26_plan_integrado_caracteristicas", HTML("C1b.3 <em>Aedes</em> y <em>Anopheles</em> integrado"), choices = c("Nombre" = "nombre", "Último año de actualización" = "anio_actualizacion", "Documento del plan" = "documento", "Desconozco" = "desconozco"), selected = character(0)))
          ),
          div(
            class = "sat26-form-reset-row",
            actionButton("sat26_plan_caracteristicas_reset", "reset", class = "btn-default")
          ),
          conditionalPanel(
            condition = "input.sat26_plan_aedes_caracteristicas && input.sat26_plan_aedes_caracteristicas.indexOf('nombre') >= 0",
            sat26_text_field("sat26_plan_aedes_nombre", "C1b.1.1 Ingrese el nombre del plan estratégico para <em>Aedes</em>:")
          ),
          conditionalPanel(
            condition = "input.sat26_plan_aedes_caracteristicas && input.sat26_plan_aedes_caracteristicas.indexOf('anio_actualizacion') >= 0",
            sat26_text_field("sat26_plan_aedes_anio", "C1b.1.2 Ingrese el año en que el plan de <em>Aedes</em> se actualizó por última vez:", "YYYY")
          ),
          conditionalPanel(
            condition = "input.sat26_plan_aedes_caracteristicas && input.sat26_plan_aedes_caracteristicas.indexOf('documento') >= 0",
            sat26_text_field("sat26_plan_aedes_documento", "C1b.1.3 Coloque el enlace o referencia al documento del plan de <em>Aedes</em>:")
          ),
          conditionalPanel(
            condition = "input.sat26_plan_anopheles_caracteristicas && input.sat26_plan_anopheles_caracteristicas.indexOf('nombre') >= 0",
            sat26_text_field("sat26_plan_anopheles_nombre", "C1b.2.1 Ingrese el nombre del plan estratégico para <em>Anopheles</em>:")
          ),
          conditionalPanel(
            condition = "input.sat26_plan_anopheles_caracteristicas && input.sat26_plan_anopheles_caracteristicas.indexOf('anio_actualizacion') >= 0",
            sat26_text_field("sat26_plan_anopheles_anio", "C1b.2.2 Ingrese el año en que el plan de <em>Anopheles</em> se actualizó por última vez:", "YYYY")
          ),
          conditionalPanel(
            condition = "input.sat26_plan_anopheles_caracteristicas && input.sat26_plan_anopheles_caracteristicas.indexOf('documento') >= 0",
            sat26_text_field("sat26_plan_anopheles_documento", "C1b.2.3 Coloque el enlace o referencia al documento del plan de <em>Anopheles</em>:")
          ),
          conditionalPanel(
            condition = "input.sat26_plan_integrado_caracteristicas && input.sat26_plan_integrado_caracteristicas.indexOf('nombre') >= 0",
            sat26_text_field("sat26_plan_integrado_nombre", "C1b.3.1 Ingrese el nombre del plan estratégico integrado de <em>Aedes</em> y <em>Anopheles</em>:")
          ),
          conditionalPanel(
            condition = "input.sat26_plan_integrado_caracteristicas && input.sat26_plan_integrado_caracteristicas.indexOf('anio_actualizacion') >= 0",
            sat26_text_field("sat26_plan_integrado_anio", "C1b.3.2 Ingrese el año en que el plan integrado se actualizó por última vez:", "YYYY")
          ),
          conditionalPanel(
            condition = "input.sat26_plan_integrado_caracteristicas && input.sat26_plan_integrado_caracteristicas.indexOf('documento') >= 0",
            sat26_text_field("sat26_plan_integrado_documento", "C1b.3.3 Coloque el enlace o referencia al documento del plan integrado de <em>Aedes</em> y <em>Anopheles</em>:")
          ),
          sat26_radio_field("sat26_reglamentos_nacionales", "C1e. ¿Existen Reglamentos, Normativas o Guías Nacionales que rigen el plan estratégico de vigilancia y control vectorial de mosquitos?", sat26_si_no_desconozco_choices),
          sat26_text_field("sat26_reglamentos_documento", "C1e.1 Si corresponde, coloque el enlace o referencia al reglamento, normativa, guía nacional y/o plan estratégico:"),
          h4("C2. Priorización y asistencia técnica"),
          sat26_radio_field("sat26_prioridad_planes", "C2a. A nivel personal, ¿qué nivel de prioridad considera que tiene la formulación/revisión de los planes estratégicos nacionales?", c("Alta" = "alta", "Media" = "media", "Baja" = "baja", "Desconozco" = "desconozco")),
          sat26_radio_field("sat26_asistencia_planes", "C2b. ¿Necesita asistencia técnica para formular/actualizar los planes estratégicos nacionales?", c("Sí" = "si", "No" = "no", "No me encargo de ello" = "no_encargo", "Desconozco" = "desconozco")),
          h4("C3. Normas para implementación de planes"),
          sat26_radio_field("sat26_normas_vigilancia_aedes", "C3a.1 Vigilancia del vector <em>Aedes</em>", sat26_normas_choices),
          sat26_radio_field("sat26_normas_control_aedes", "C3a.2 Control del vector <em>Aedes</em>", sat26_normas_choices),
          sat26_radio_field("sat26_normas_vigilancia_anopheles", "C3a.3 Vigilancia del vector <em>Anopheles</em>", sat26_normas_choices),
          sat26_radio_field("sat26_normas_control_anopheles", "C3a.4 Control del vector <em>Anopheles</em>", sat26_normas_choices),
          h4("C4. Elementos incluidos en los planes"),
          p(class = "sat26-section-note", "Seleccione si los elementos están incluidos en el plan general, en un plan separado, no están incluidos o si lo desconoce."),
          sat26_radio_field("sat26_plan_elemento_control_aedes", "C4a.1 Control del vector <em>Aedes</em>", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_vigilancia_aedes", "C4a.2 Vigilancia del vector <em>Aedes</em>", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_control_anopheles", "C4a.3 Control del vector <em>Anopheles</em>", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_vigilancia_anopheles", "C4a.4 Vigilancia del vector <em>Anopheles</em>", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_equipos_insecticidas", "C4a.5 Gestión de equipos e insecticidas para control de mosquitos", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_resistencia", "C4a.6 Gestión de resistencia a insecticidas", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_recursos_humanos", "C4a.7 Recursos humanos (personal necesario)", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_formacion", "C4a.8 Plan de formación para personal de entomología en salud pública", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_participacion", "C4a.9 Participación comunitaria y comunicación de riesgos", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_sistemas_info", "C4a.10 Sistemas de información para datos de vigilancia y control vectorial", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_investigacion", "C4a.11 Investigación operativa (estadística, etc.)", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_vulnerables", "C4a.12 Grupos vulnerables por género, discapacidad y otros factores", sat26_plan_elementos_choices),
          sat26_radio_field("sat26_plan_elemento_monitoreo", "C4a.13 Plan de monitoreo y evaluación del progreso del programa de vigilancia y control vectorial", sat26_plan_elementos_choices),
          h4("C5-C7. Gestión, legislación y coordinación"),
          sat26_radio_field("sat26_descentralizacion", "C5. ¿La gestión del control de enfermedades transmitidas por vectores está centralizada o descentralizada?", c("Centralizada" = "centralizada", "En proceso de descentralización" = "en_proceso", "Descentralizada" = "descentralizada", "El país es demasiado pequeño para descentralizarse" = "pais_pequeno", "Desconozco qué es el concepto de descentralización" = "desconozco_concepto", "Entiendo el concepto, pero no sabría decir si la gestión está descentralizada" = "desconozco_estado", "Desconozco" = "desconozco")),
          sat26_radio_field("sat26_legislacion_24_48", "C6. ¿Existe legislación vigente que clasifique la malaria y el dengue como enfermedades de notificación obligatoria en un plazo de 24-48 horas?", sat26_si_no_desconozco_choices),
          sat26_radio_field("sat26_grupo_interministerial", "C7a. ¿Existe un grupo de trabajo interministerial funcional que colabore con el control vectorial?", c("Sí" = "si", "En desarrollo" = "en_desarrollo", "No" = "no", "No entiendo qué es un grupo de trabajo interministerial" = "no_entiendo", "Entiendo qué es, pero desconozco si existe uno" = "desconozco_existencia", "Desconozco" = "desconozco")),
          conditionalPanel(condition = "input.sat26_grupo_interministerial == 'si'", sat26_textarea_field("sat26_grupo_interministerial_reunion", "C7a.1 Si respondió “sí”, ¿tuvieron una reunión en 2025? Describa:")),
          h4("C8. Priorización en la agenda nacional"),
          sat26_radio_field("sat26_prioridad_agenda", "C8a. ¿Cómo percibe que la vigilancia y control de vectores transmitidos por mosquitos es priorizada actualmente dentro de la agenda nacional de salud pública?", c("Muy alta prioridad" = "muy_alta", "Alta prioridad" = "alta", "Prioridad media" = "media", "Baja prioridad" = "baja", "No se considera una prioridad actualmente" = "no_prioridad", "Desconozco" = "desconozco")),
          div(class = "sat26-form-field", checkboxGroupInput("sat26_factores_prioridad", "C8b. ¿Qué factores considera que han influido en ese nivel de prioridad? Marque todas las que apliquen.", choices = c("Casos reportados / carga de enfermedad" = "casos", "Opinión o presión de autoridades superiores" = "autoridades", "Requisitos de cooperación internacional" = "cooperacion", "Costos operativos de la vigilancia y control" = "costos", "Interés de la comunidad" = "comunidad", "Experiencias pasadas" = "experiencias", "Otros" = "otros", "Desconozco" = "desconozco"), selected = character(0))),
          conditionalPanel(condition = "input.sat26_factores_prioridad && input.sat26_factores_prioridad.indexOf('otros') >= 0", sat26_textarea_field("sat26_factores_prioridad_otros", "C8b.1 Si ha seleccionado “Otros”, describa brevemente las razones:")),
          sat26_radio_field("sat26_frecuencia_interministerial", "C8c. ¿Con qué frecuencia se discuten temas estratégicos de vigilancia y control de vectores en espacios interministeriales?", c("Frecuentemente (3+ veces al año)" = "frecuentemente", "Ocasionalmente (1-3 veces al año)" = "ocasionalmente", "Solo en contextos de emergencia o brotes" = "emergencias", "Casi nunca" = "casi_nunca", "No existe un espacio interministerial activo" = "no_espacio", "No participo en los espacios interministeriales" = "no_participo", "Desconozco" = "desconozco")),
          div(class = "sat26-form-field", checkboxGroupInput("sat26_motivadores_institucion", "C8d. ¿Qué motivaría a su institución a darle un mayor enfoque al programa de vigilancia y control de vectores? Marque las 3 más relevantes.", choices = c("Disponibilidad de recursos sostenibles" = "recursos", "Presión o apoyo político de alto nivel" = "apoyo_politico", "Éxitos visibles en territorio" = "exitos", "Mejora de los indicadores de salud" = "indicadores", "Mayor participación comunitaria" = "participacion", "Alianzas con cooperación o sector privado" = "alianzas", "No sabría decir qué puede motivar a mi institución" = "no_sabria", "Otras" = "otras", "Desconozco" = "desconozco"), selected = character(0))),
          conditionalPanel(condition = "input.sat26_motivadores_institucion && input.sat26_motivadores_institucion.indexOf('otras') >= 0", sat26_textarea_field("sat26_motivadores_institucion_otros", "C8d.1 Si ha seleccionado “Otras”, describa brevemente qué puede motivar a su institución:")),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_c_back", "Volver a Parte B", class = "btn-secondary"),
            actionButton("sat26_part_c_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_b")) {
        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte B: Informe de situacion de enfermedades"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-form-field",
            radioButtons(
              "sat26_dengue_2025",
              "B1. ¿Cuál de los siguientes escenarios describe mejor la transmisión del dengue en 2025? Incluya casos confirmados o sospechosos.",
              choices = c(
                "No hubo casos (confirmados o sospechosos) de dengue." = "sin_casos",
                "Zona no endémica con un brote (>1 caso donde normalmente no hay casos)." = "zona_no_endemica_brote",
                "Transmision endemica (transmision continua durante todo el anio; puede o no superar el umbral de brote)." = "transmision_endemica",
                "Desconozco acerca de la transmisión de dengue." = "desconozco",
                "No estoy autorizado/a a compartir esta información" = "no_autorizado"
              ),
              selected = character(0)
            )
          ),
          div(
            class = "sat26-form-field",
            checkboxGroupInput(
              "sat26_arbovirus_2025",
              "B2. ¿Se confirmó la transmisión de algunos de los siguientes arbovirus en 2025? Seleccione los virus de los cuales hay pacientes confirmados.",
              choices = c(
                "Zika" = "zika",
                "Chikungunya" = "chikungunya",
                "Ninguno" = "ninguno",
                "Desconozco" = "desconozco"
              ),
              selected = character(0)
            )
          ),
          div(
            class = "sat26-form-field",
            radioButtons(
              "sat26_filariasis_activa_2025",
              "B3a. ¿Hubo transmisión activa de filariasis linfática (elefantiasis) en 2025?",
              choices = c(
                "Sí" = "si",
                "No" = "no",
                "Desconozco" = "desconozco"
              ),
              selected = character(0)
            )
          ),
          conditionalPanel(
            condition = "input.sat26_filariasis_activa_2025 == 'si'",
            div(
              class = "sat26-form-field sat26-conditional-field",
              radioButtons(
                "sat26_filariasis_escenario_2025",
                "B3b. Si la respuesta es sí, ¿cuál de los siguientes escenarios describe mejor la transmisión de filariasis linfática?",
                choices = c(
                  "Más de 1 caso en donde normalmente no hay casos (zona no endémica con un brote)." = "zona_no_endemica_brote",
                  "Transmisión continua durante todo el año; puede o no superar el umbral de brote (transmisión endémica)." = "transmision_endemica"
                ),
                selected = character(0)
              )
            )
          ),
          div(
            class = "sat26-form-field",
            radioButtons(
              "sat26_malaria_2025",
              "B4. ¿Cuál de las siguientes opciones describe mejor la transmisión de malaria en 2025, a lo largo del continuo de transmisión?",
              choices = c(
                "Manteniendo cero (sin transmisión)." = "manteniendo_cero",
                "Muy baja (menos de 100-250 casos por cada 1000 habitantes en riesgo, API)." = "muy_baja",
                "Moderada (250-450 casos por cada 1000 API, incidencia anual parasitaria)." = "moderada",
                "Alta (más de 450 casos por cada 1000 API)." = "alta",
                "Desconozco la situación de transmisión de malaria." = "desconozco"
              ),
              selected = character(0)
            )
          ),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_part_b_back", "Volver a Parte A", class = "btn-secondary"),
            actionButton("sat26_part_b_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      if (identical(module, "part_a")) {
        return(div(
          class = "sat26-questionnaire-panel",
          div(
            class = "sat26-section-logo-header",
            img(src = "entonet-header.jpeg", class = "sat26-section-logo-entonet", alt = "EntoNet"),
            img(src = "COMISCA.png", class = "sat26-section-logo-comisca", alt = "COMISCA y SICA")
          ),
          h3("Parte A: Información del encuestado"),
          div(class = "sat26-draft-code", paste("Codigo unico de encuesta:", sat26_generate_code())),
          div(
            class = "sat26-questionnaire-intro",
            h4("Evaluación Regional de Necesidades y Fortalecimiento de Capacidades para la Vigilancia y el Control de Vectores en Centroamérica y República Dominicana."),
            p("Esta encuesta fue diseñada para evaluar la capacidad de los programas nacionales de control de enfermedades transmitidas por vectores para facilitar respuestas eficaces ante brotes de enfermedades transmitidas por mosquitos y para mejorar la vigilancia y el control vectorial de forma rutinaria. La encuesta fue desarrollada por el consorcio Pacific Mosquito Surveillance Strengthening for Impact (PacMOSSI), gestionado por la Universidad James Cook y la Comunidad del Pacífico (SPC), y adaptada localmente por la Universidad del Valle de Guatemala y la red de \"Vigilancia y Control de Vectores de Centroamérica y la República Dominicana\" (EntoNet)."),
            p("El objetivo de esta evaluación de necesidades es doble: 1) comprender la capacidad actual de los países para prevenir y controlar las enfermedades transmitidas por mosquitos, y 2) ayudar a los países a hacer seguimiento de su progreso en relación con indicadores estandarizados. Agradecemos que se tome el tiempo para completar esta encuesta, la cual no debería tomar más de 60 minutos."),
            p("Después de completar la encuesta, le enviaremos un informe resumen con sus respuestas. También podríamos publicar en una revista científica un resumen regional de los resultados de la encuesta en Centroamérica y República Dominicana. Sin embargo, respetamos la confidencialidad de los datos por país y no se divulgarán sin autorización.")
          ),
          div(
            class = "sat26-form-field",
            textInput("sat26_nombre", "A1. Ingrese su nombre:")
          ),
          div(
            class = "sat26-form-field",
            textInput("sat26_cargo", "A2. Ingrese el cargo que ocupa en su unidad:")
          ),
          div(
            class = "sat26-form-field",
            textInput("sat26_organizacion", "A3. Ingrese el nombre de la organización a la que pertenece:")
          ),
          div(
            class = "sat26-form-field sat26-inline-reset",
            selectInput(
              "sat26_country",
              "A4. Elija el país al que pertenece:",
              choices = c(
                "Seleccione" = "",
                "Guatemala" = "Guatemala",
                "El Salvador" = "El Salvador",
                "Honduras" = "Honduras",
                "Nicaragua" = "Nicaragua",
                "Costa Rica" = "Costa Rica",
                "Panamá" = "Panamá",
                "Belice" = "Belice",
                "República Dominicana" = "República Dominicana"
              ),
              selected = ""
            ),
            actionButton("sat26_country_reset", "reset", class = "btn-default")
          ),
          div(
            class = "sat26-form-field",
            p(class = "sat26-optional-note", "Opcional"),
            div(
              class = "sat26-inline-reset",
              radioButtons(
                "sat26_contact_after",
                "A5. ¿Está de acuerdo en ser contactado por teléfono o correo electrónico después de completar esta encuesta, en caso de que sea necesario aclarar alguna respuesta?",
                choices = c("Sí" = "si", "No" = "no"),
                selected = character(0),
                inline = TRUE
              ),
              actionButton("sat26_contact_reset", "reset", class = "btn-default")
            )
          ),
          div(
            class = "sat26-form-actions",
            actionButton("sat26_back_to_consent", "Volver al consentimiento", class = "btn-secondary"),
            actionButton("sat26_part_a_continue", "Continuar", class = "btn-primary")
          )
        ))
      }

      return(div(
        class = "sat26-consent-panel",
        h3("Consentimiento informado"),
        div(
          class = "sat26-audio-controls",
          tags$button(type = "button", class = "btn btn-primary", `data-sat26-audio` = "play", `data-sat26-target` = "sat26_consent_page_text", "Escuchar"),
          tags$button(type = "button", class = "btn btn-default", `data-sat26-audio` = "pause", `data-sat26-target` = "sat26_consent_page_text", "Pausar"),
          tags$button(type = "button", class = "btn btn-default", `data-sat26-audio` = "stop", `data-sat26-target` = "sat26_consent_page_text", "Detener")
        ),
        div(
          id = "sat26_consent_page_text",
          div(class = "sat26-consent-section",
            h4("Información general"),
            p("Investigadora principal: Norma Padilla"),
            p("Este formulario de consentimiento en línea forma parte del proceso de consentimiento informado para una evaluación del programa de vigilancia y control de vectores en Centroamérica y República Dominicana. Le proporcionará información que le ayudará a decidir si desea participar o no en esta evaluación. Su participación es completamente voluntaria. Si decide no participar, no habrá ningún tipo de penalización ni consecuencias en su trabajo."),
            p("Este proyecto está siendo llevado a cabo por la Universidad del Valle de Guatemala, en colaboración con EntoNet y SE-COMISCA, como un esfuerzo conjunto para evaluar las necesidades específicas de la región y apoyar el fortalecimiento de los programas de vigilancia y control de vectores en Centroamérica y República Dominicana.")
          ),
          div(class = "sat26-consent-section",
            h4("¿Quién realiza esta evaluación y de qué trata?"),
            p("Usted ha sido invitado/a a participar en una entrevista realizada por la Dra. Norma Padilla, investigadora principal del Centro de Estudios en Salud de la Universidad del Valle de Guatemala. El objetivo de este estudio es desarrollar una comprensión general de las prácticas de vigilancia y control de vectores en su país, e identificar necesidades y brechas para implementar estrategias adaptadas a la región."),
            p("Específicamente, esta evaluación busca:"),
            tags$ol(
              tags$li("Comprender la capacidad actual de los países para prevenir y controlar enfermedades transmitidas por mosquitos."),
              tags$li("Apoyar a los países en el seguimiento de su progreso frente a indicadores estandarizados.")
            )
          ),
          div(class = "sat26-consent-section",
            h4("¿Qué se me pedirá que haga si decido participar?"),
            p("Se le pedirá que complete una encuesta enfocada en los programas de enfermedades transmitidas por vectores, incluyendo aspectos como gobernanza, financiamiento, recursos humanos, infraestructura, logística, sistemas de información y cooperación entre países. La encuesta tomará aproximadamente 60 minutos.")
          ),
          div(class = "sat26-consent-section",
            h4("¿Qué pasará con la información proporcionada?"),
            p("Sus respuestas se conservarán únicamente hasta que se presenten y publiquen los resultados del estudio. La información no será utilizada ni distribuida para otras investigaciones.")
          ),
          div(class = "sat26-consent-section",
            h4("¿Qué sucede si no quiero participar o si decido retirarme más adelante?"),
            p("Su participación es voluntaria. Puede decidir no participar o retirarse en cualquier momento. Si no hace clic en el botón de \"enviar\" al finalizar la encuesta, sus respuestas no serán registradas. Se le preguntará si está dispuesto(a) a ser contactado(a) por correo electrónico o teléfono con fines de aclaración, en caso de que alguna de sus respuestas requiera seguimiento. Si desea retirar sus respuestas después de haber enviado la encuesta, comuníquese con la investigadora principal.")
          ),
          div(class = "sat26-consent-section",
            h4("¿A quién puedo contactar si tengo preguntas?"),
            p("Si tiene preguntas sobre su participación o desea retirar sus respuestas, puede comunicarse con la investigadora principal: Norma Padilla, Investigadora Principal, Centro de Estudios en Salud, Universidad del Valle de Guatemala, Guatemala. Correo electrónico: npadilla@uvg.edu.gt."),
            p("Este proyecto fue aprobado por el Comité de Ética en Investigación del Centro de Estudios en Salud de la Universidad del Valle de Guatemala. Si desea más información, puede contactar a Estela García al teléfono (502) 2507-1500 ext. 21513. Si desea información respecto al cuestionario o implementación de la evaluación comunicarse con la Dra. Norma Padilla (Whatsapp +502 5204 9300) y/o el investigador encargado de su país.")
          ),
          div(class = "sat26-consent-section",
            h4("Instrucciones finales"),
            p("Puede imprimir este formulario si desea conservar una copia para sus archivos."),
            p("Si no desea participar en la evaluación, cierre esta página web."),
            p("Si desea participar, siga las instrucciones a continuación."),
            p("Al comenzar esta evaluación, confirmo que tengo 18 años o más y que he leído y comprendido esta información. Acepto participar en la evaluación, sabiendo que puedo retirarme en cualquier momento sin penalización.")
          )
        ),
        div(
          class = "sat26-consent-check",
          div(class = "sat26-consent-question", "Pregunta 1. Confirma su consentimiento de participación:"),
          div(
            class = "sat26-consent-choice-row",
            actionButton("sat26_consent_yes", "Sí, acepto participar", class = "sat26-consent-choice sat26-consent-choice-yes"),
            actionButton("sat26_consent_no", "No, no deseo participar", class = "sat26-consent-choice sat26-consent-choice-no")
          ),
          checkboxInput("sat26_contact_ok", "Acepto ser contactado(a) si alguna respuesta requiere aclaración.", value = FALSE)
        ),
        div(
          class = "sat26-form-actions",
          actionButton("sat26_back_home", "Volver a portada", class = "btn-secondary")
        )
      ))
    }

    if (identical(area, "data")) {
      if (is.null(module)) return(tagList(
        uiOutput("portal_intro_area"),
        div(class = "portal-empty-state", "Seleccione una opción de Datos en el menú lateral.")
      ))
      return(uiOutput("data_module_area"))
    }

    if (identical(area, "protocols")) {
      if (is.null(module)) return(tagList(
        uiOutput("portal_intro_area"),
        div(class = "portal-empty-state", "Seleccione Protocolos de Campo o Protocolos de Laboratorio.")
      ))
      if (identical(module, "protocol_field")) return(div(
        class = "module-panel",
        h3("Protocolos de Campo"),
        p("Procedimientos para colocación y retiro de ovitrampas, muestreo de adultos, georreferenciación, transporte de muestras y registro de información en sitio."),
        div(class = "alert alert-info", "Los documentos de campo se incorporarán a este repositorio conforme sean aprobados por la red.")
      ))
      if (identical(module, "protocol_laboratory")) return(div(
        class = "module-panel",
        h3("Protocolos de Laboratorio"),
        p("Procedimientos para conteo de huevos, identificación taxonómica, cría de colonias, bioensayos de resistencia y control de calidad de datos."),
        actionButton("open_laboratory_protocols", "Abrir protocolos de laboratorio", class = "btn-primary")
      ))
    }

    if (identical(area, "training")) {
      if (is.null(module)) return(tagList(
        uiOutput("portal_intro_area"),
        div(class = "portal-empty-state", "Seleccione un tipo de entrenamiento en el menú lateral.")
      ))
      if (identical(module, "training_live")) return(div(
        class = "module-panel",
        h3("Próximas capacitaciones en vivo"),
        p("Fechas por confirmar. Se priorizarán sesiones regionales sobre captura estandarizada de datos, uso del portal y control de calidad.")
      ))
      if (identical(module, "training_workshops")) return(div(
        class = "module-panel",
        h3("Talleres prácticos"),
        p("Fechas por confirmar. Talleres enfocados en protocolos de campo, procedimientos de laboratorio y análisis inicial de datos.")
      ))
      if (identical(module, "training_materials")) return(div(
        class = "module-panel",
        h3("Materiales de apoyo"),
        p("Espacio previsto para guías rápidas, grabaciones, presentaciones y documentos de referencia para usuarios nuevos y equipos técnicos.")
      ))
    }

    NULL
  })

  output$data_module_area <- renderUI({
    module <- active_module()

    if (is.null(module)) {
      return(NULL)
    }

    if (identical(module, "capture")) {
      return(div(
        class = "module-panel",
        h3(class = "capture-module-title", "Captura de Datos"),
        uiOutput("active_dataset_header"),
        uiOutput("data_entry_area")
      ))
    }

    if (identical(module, "visualization")) {
      return(div(
        class = "module-panel",
        h3("Visualización de Datos"),
        p("Seleccione primero el país y después el tipo de datos que desea consultar."),
        div(
          class = "selector-box",
          fluidRow(
            column(
              width = 5,
              selectInput(
                "visualization_country",
                "País",
                choices = country_choices,
                selected = "Guatemala"
              )
            ),
            column(
              width = 5,
              selectInput(
                "visualization_dataset",
                  "Tipo de datos",
                  choices = c(
                  "Formulario 7: Bioensayo de botella CDC" = "formulario_7_bioensayo_botella_cdc"
                )
              )
            ),
            column(
              width = 2,
              br(),
              actionButton("search_visualization", "Buscar", class = "btn-primary")
            )
          )
        ),
        uiOutput("visualization_results_area")
      ))
    }

    if (identical(module, "request")) {
      subdivision <- active_request_subdivision()
      data_subdivision <- active_request_data_subdivision()
      request_labels <- c(
        datos = "Datos",
        reactivos = "Reactivos",
        equipo = "Equipo",
        apoyo_tecnico = "Apoyo Técnico"
      )
      render_request_reactivos_panel <- function() {
        language <- public_language()
        category_key <- active_request_reactivos_category()
        category_data <- request_reactivos_catalog[[category_key]]
        category_definitions <- list(
          larvicidas = tr(
            language,
            "La OMS/WHOPES describe los larvicidas como productos químicos o biológicos aplicados en hábitats acuáticos para destruir las fases inmaduras del mosquito antes de que emerjan como adultos.",
            "WHO/WHOPES describes larvicides as chemical or biological products applied in aquatic habitats to destroy immature mosquito stages before they emerge as adults."
          ),
          adulticidas = tr(
            language,
            "Los adulticidas se emplean para reducir poblaciones de mosquitos adultos mediante aplicaciones dirigidas en espacios interiores o exteriores.",
            "Adulticides are used to reduce adult mosquito populations through targeted applications in indoor or outdoor spaces."
          ),
          residuales = tr(
            language,
            "Los residuales corresponden a formulaciones de efecto prolongado que permanecen activas sobre superficies tratadas para el control sostenido de vectores.",
            "Residuals are long-lasting formulations that remain active on treated surfaces for sustained vector control."
          )
        )
        selected_definition <- category_definitions[[category_key]]
        product_index <- active_request_reactivos_product()
        if (is.null(product_index) || is.na(product_index) || product_index < 1L || product_index > nrow(category_data$items)) {
          product_index <- 1L
        }
        selected_product <- category_data$items[product_index, ]

        div(
          class = "module-panel",
          div(
            class = "reactivos-hero",
            div(
              class = "reactivos-hero-image-wrap",
              img(src = "reactivos-hero.png", class = "reactivos-hero-image", alt = "Vista superior de reactivos de laboratorio")
            ),
            div(
              class = "reactivos-hero-copy",
              div(class = "reactivos-hero-kicker", tr(language, "UNIDAD DE MALARIA Y BIOLOGIA DE VECTORES", "MALARIA AND VECTOR BIOLOGY UNIT")),
              h3(tr(language, "Reactivos de Laboratorio", "Laboratory Reagents")),
              p(tr(language, "Distribución eficiente y segura para fortalecer la vigilancia entomológica en la región.", "Efficient and safe distribution to strengthen entomological surveillance in the region.")),
              div(
                class = "reactivos-hero-benefits",
                div(class = "reactivos-hero-benefit", span(class = "reactivos-hero-benefit-icon", "▣"), div(strong(tr(language, "Inventario", "Inventory")), tags$br(), tr(language, "Seguimiento de insumos disponibles.", "Tracking of available supplies."))),
                div(class = "reactivos-hero-benefit", span(class = "reactivos-hero-benefit-icon", "⇄"), div(strong(tr(language, "Distribución", "Distribution")), tags$br(), tr(language, "Preparación para entrega y reposición.", "Preparation for delivery and replenishment."))),
                div(class = "reactivos-hero-benefit", span(class = "reactivos-hero-benefit-icon", "✓"), div(strong(tr(language, "Calidad", "Quality")), tags$br(), tr(language, "Reactivos certificados y verificados.", "Certified and verified reagents."))),
                div(class = "reactivos-hero-benefit", span(class = "reactivos-hero-benefit-icon", "▤"), div(strong(tr(language, "Trazabilidad", "Traceability")), tags$br(), tr(language, "Control en cada etapa del proceso.", "Control at every stage of the process.")))
              ),
              tags$div(
                class = "reactivos-hero-notes",
                tags$div(class = "reactivos-hero-note", strong(tr(language, "Larvicidas", "Larvicides")), tags$div(tr(language, "Productos para intervenir criaderos y etapas inmaduras.", "Products to target breeding sites and immature stages."))),
                tags$div(class = "reactivos-hero-note", strong(tr(language, "Adulticidas", "Adulticides")), tags$div(tr(language, "Control focalizado para insectos adultos.", "Focused control for adult mosquitoes."))),
                tags$div(class = "reactivos-hero-note", strong(tr(language, "Residuales", "Residuals")), tags$div(tr(language, "Aplicaciones dirigidas de efecto prolongado.", "Targeted long-lasting applications.")))
              )
            )
          ),
          div(
            class = "reactivos-intent-copy",
            h4(tr(language, "Envío y compra de reactivos", "Reagent shipment and procurement")),
            p(tr(
              language,
              "EntoNet busca facilitar el acceso a materiales clave para armonizar procedimientos y resultados en la evaluación de resistencia a insecticidas entre los miembros del consorcio en Centroamérica y República Dominicana.",
              "EntoNet aims to facilitate access to key materials that support harmonized procedures and comparable results for insecticide resistance evaluation among consortium members in Central America and the Dominican Republic."
            ))
          ),
          div(
            class = "reactivos-order-procedure",
            h4(tr(language, "Procedimiento para solicitar reactivos", "Procedure to request reagents")),
            p(tr(
              language,
              "Este flujo sirve como machote inicial para validar solicitudes, autorizar envíos y ordenar los pasos administrativos asociados a la compra o distribución de insecticidas dentro de EntoNet.",
              "This workflow is an initial template to validate requests, authorize shipments, and organize the administrative steps related to insecticide procurement or distribution within EntoNet."
            )),
            div(
              class = "reactivos-order-steps",
              div(
                class = "reactivos-order-step",
                strong(tr(language, "1. Formulario de autorización", "1. Authorization form")),
                span(tr(
                  language,
                  "La solicitud inicia con el Formulario de Autorización para Compra de Insecticidas. EntoNet utilizará esta información para validar la institución solicitante, el tipo de reactivo requerido y la autorización para el envío.",
                  "The request begins with the Insecticide Purchase Authorization Form. EntoNet will use this information to validate the requesting institution, the reagent type requested, and shipment authorization."
                ))
              ),
              div(
                class = "reactivos-order-step",
                strong(tr(language, "2. Ruta para Ministerios de Salud", "2. Ministry of Health route")),
                span(tr(
                  language,
                  "Cuando la solicitud provenga de un Ministerio de Salud y existan fondos CDC disponibles, los costos del producto y envío serán cubiertos hasta la aduana del país solicitante. A partir de ese punto, la institución solicitante deberá cubrir transporte local, liberación, manejo u otros costos nacionales aplicables.",
                  "When the request comes from a Ministry of Health and CDC funds are available, product and shipping costs will be covered up to customs in the requesting country. From that point forward, the requesting institution must cover local transport, clearance, handling, or other applicable national costs."
                ))
              ),
              div(
                class = "reactivos-order-step",
                strong(tr(language, "3. Ruta para universidades e institutos de investigación", "3. University and research institute route")),
                span(tr(
                  language,
                  "Las instituciones que no sean Ministerios de Salud deberán completar un formulario adicional de cotización y compra para documentar el pago del producto, envío y costos asociados.",
                  "Institutions that are not Ministries of Health must complete an additional quotation and purchase form to document payment for the product, shipment, and associated costs."
                ))
              )
            ),
            div(
              class = "reactivos-form-placeholders",
              div(
                class = "reactivos-form-placeholder",
                strong(tr(language, "Formulario de Autorización", "Authorization Form")),
                span(tr(language, "Espacio reservado para conectar el formulario de validación y autorización de envío.", "Reserved space to connect the validation and shipment authorization form."))
              ),
              div(
                class = "reactivos-form-placeholder",
                strong(tr(language, "Formulario de Cotización y Compra", "Quotation and Purchase Form")),
                span(tr(language, "Espacio reservado para instituciones que deberán cubrir costos de producto, envío o gestión local.", "Reserved space for institutions that must cover product, shipment, or local management costs."))
              )
            )
          ),
          div(
            class = "capture-subdivision-list",
            actionButton(
              "show_request_reactivos_larvicidas",
              tagList(
                div(class = "capture-subdivision-panel-image", img(src = "reactivos-larvicidas.png", alt = tr(language, "Larvicidas", "Larvicides"))),
                div(
                  class = "capture-subdivision-panel-body",
                  h4(tr(language, "Larvicidas", "Larvicides")),
                  p(tr(language, "Productos para intervenir criaderos y etapas inmaduras.", "Products to target breeding sites and immature stages."))
                )
              ),
              class = paste(
                "capture-subdivision-panel capture-subdivision-panel-action",
                if (identical(category_key, "larvicidas")) "capture-subdivision-panel-active" else ""
              )
            ),
            actionButton(
              "show_request_reactivos_adulticidas",
              tagList(
                div(class = "capture-subdivision-panel-image", img(src = "reactivos-adulticidas.png", alt = tr(language, "Adulticidas", "Adulticides"))),
                div(
                  class = "capture-subdivision-panel-body",
                  h4(tr(language, "Adulticidas", "Adulticides")),
                  p(tr(language, "Control focalizado para insectos adultos.", "Focused control for adult mosquitoes."))
                )
              ),
              class = paste(
                "capture-subdivision-panel capture-subdivision-panel-action",
                if (identical(category_key, "adulticidas")) "capture-subdivision-panel-active" else ""
              )
            ),
            actionButton(
              "show_request_reactivos_residuales",
              tagList(
                div(class = "capture-subdivision-panel-image", img(src = "reactivos-residuales.png", alt = tr(language, "Residuales", "Residuals"))),
                div(
                  class = "capture-subdivision-panel-body",
                  h4(tr(language, "Residuales", "Residuals")),
                  p(tr(language, "Formulaciones para aplicaciones dirigidas de efecto prolongado.", "Targeted long-lasting applications."))
                )
              ),
              class = paste(
                "capture-subdivision-panel capture-subdivision-panel-action",
                if (identical(category_key, "residuales")) "capture-subdivision-panel-active" else ""
              )
            )
          ),
          div(
            class = "reactivos-detail-bubble",
            div(
              class = "reactivos-detail-bubble-header",
              div(
                tags$div(style = "color:#1769aa;font-size:12px;font-weight:800;letter-spacing:0.08em;text-transform:uppercase;", tr(language, "Definición técnica", "Technical definition")),
                h4(if (identical(category_key, "larvicidas")) tr(language, "Larvicidas", "Larvicides") else if (identical(category_key, "adulticidas")) tr(language, "Adulticidas", "Adulticides") else tr(language, "Residuales", "Residuals")),
                tags$p(selected_definition)
              ),
              div(style = "flex:0 0 auto;color:#0d7a82;font-size:13px;font-weight:700;", tr(language, "Seleccione un producto", "Select a product"))
            ),
            div(
              class = "reactivos-detail-bubble-body",
              div(
                class = "reactivos-detail-products",
                lapply(seq_len(nrow(category_data$items)), function(i) {
                  item <- category_data$items[i, ]
                  actionButton(
                    paste0("show_request_reactivos_product_", i),
                    tagList(
                      div(class = "reactivos-detail-product-image", img(src = item$image, alt = item$name)),
                      div(
                        style = "flex:1 1 auto;",
                        div(class = "reactivos-detail-product-name", item$name),
                        div(class = "reactivos-detail-product-price", item$price),
                        div(class = "reactivos-detail-product-status", item$status)
                      )
                    ),
                    class = paste(
                      "reactivos-detail-product",
                      if (identical(as.integer(product_index), as.integer(i))) "reactivos-detail-product-active" else ""
                    )
                  )
                })
              ),
              div(
                class = "reactivos-product-card",
                h5(selected_product$name),
                p(selected_product$technical_description),
                div(
                  class = "reactivos-product-spec-grid",
                  div(
                    class = "reactivos-product-spec",
                    span(tr(language, "Concentración", "Concentration")),
                    strong(selected_product$concentration)
                  ),
                  div(
                    class = "reactivos-product-spec",
                    span(tr(language, "Caducidad", "Expiration")),
                    strong(selected_product$expiration)
                  ),
                  div(
                    class = "reactivos-product-spec",
                    span(tr(language, "Costo", "Cost")),
                    strong(selected_product$price)
                  ),
                  div(
                    class = "reactivos-product-spec",
                    span(tr(language, "Disponibilidad", "Availability")),
                    strong(selected_product$status)
                  )
                ),
                div(
                  class = "reactivos-form-placeholder",
                  strong(tr(language, "Solicitud del producto", "Product request")),
                  span(tr(
                    language,
                    "Espacio reservado para activar la solicitud de este reactivo y vincularla con el formulario de autorización o compra.",
                    "Reserved space to activate the request for this reagent and connect it to the authorization or purchase form."
                  )),
                  tags$button(
                    type = "button",
                    class = "btn btn-primary",
                    style = "margin-top:12px;",
                    tr(language, "Solicitar producto", "Request product")
                  )
                )
              )
            )
          ),
          div(class = "selector-box", h4(tr(language, "Lista inicial de reactivos", "Initial reagent list")), tableOutput("request_reactivos_preview_table"))
        )
      }
      if (is.null(subdivision)) {
        return(div(
          class = "module-panel",
          div(
            class = "request-flow-panel",
            img(src = "solicitudes-flow.png", alt = "Flujo de Solicitudes EntoNet")
          )
        ))
      }
      if (identical(subdivision, "reactivos")) {
        return(render_request_reactivos_panel())
      }
      if (identical(subdivision, "datos")) {
        allowed_subdivisions <- request_allowed_data_subdivisions()
        data_labels <- c(campo = "Campo", insectario = "Insectario", laboratorio = "Laboratorio")
        if (!length(allowed_subdivisions)) {
          return(div(
            class = "module-panel",
            h3("Datos"),
            div(
              class = "alert alert-warning",
              "Su perfil actual no tiene permisos de descarga. Solicite acceso a un administrador de EntoNet."
            )
          ))
        }
        if (is.null(data_subdivision)) {
          return(div(
            class = "module-panel",
            h3("Datos"),
            p("Seleccione el área de datos que desea descargar. Las opciones disponibles dependen de su perfil, institución y país."),
            div(
              class = "capture-subdivision-list",
              div(class = "capture-subdivision-panel", h4("Campo"), p("Datos de captura en campo, incluyendo colocación y retiro de ovitrampas.")),
              div(class = "capture-subdivision-panel", h4("Insectario"), p("Datos de cría, alimentación, conteo y bioensayos.")),
              div(class = "capture-subdivision-panel", h4("Laboratorio"), p("Datos de laboratorio disponibles solo para perfiles autorizados."))
            ),
            div(
              class = "alert alert-info",
              "Use el menú lateral debajo de Datos para seleccionar Campo, Insectario o Laboratorio."
            )
          ))
        }
        if (!(data_subdivision %in% allowed_subdivisions)) {
          return(div(
            class = "module-panel",
            h3(data_labels[[data_subdivision]]),
            div(
              class = "alert alert-warning",
              "Su perfil no tiene permiso para descargar datos de esta área."
            )
          ))
        }
        dataset_choices <- request_data_dataset_choices(data_subdivision)
        if (!length(dataset_choices)) {
          return(div(
            class = "module-panel",
            h3(data_labels[[data_subdivision]]),
            div(
              class = "alert alert-info",
              "Esta área aún no tiene conjuntos de datos activos para descarga."
            )
          ))
        }
        return(div(
          class = "module-panel",
          h3(paste("Datos -", data_labels[[data_subdivision]])),
          p("Descargue datos de acuerdo con su perfil. Los supervisores descargan únicamente la información de su país e institución."),
          div(
            class = "selector-box",
            fluidRow(
              column(
                4,
                selectInput(
                  "request_download_dataset",
                  "Conjunto de datos",
                  choices = dataset_choices
                )
              ),
              column(
                4,
                if (request_is_global_admin()) {
                  selectInput("request_download_country", "País", choices = c("Todos" = "all", country_choices), selected = "all")
                } else {
                  div(class = "form-group", tags$label("País"), tags$p(class = "form-control-static", value_or_default(user_profile$country, "País no configurado")))
                }
              ),
              column(
                4,
                if (request_is_global_admin()) {
                  selectInput("request_download_institution", "Institución", choices = c("Todas" = "all", default_institution_id), selected = "all")
                } else {
                  div(class = "form-group", tags$label("Institución"), tags$p(class = "form-control-static", value_or_default(user_profile$institution, default_institution_id)))
                }
              )
            ),
            div(
              class = "submit-row",
              downloadButton("download_request_data_csv", "Descargar CSV", class = "btn-primary")
            )
          ),
          div(
            class = "alert alert-info",
            tags$strong("Perfil activo: "),
            value_or_default(user_profile$position, "Rol no configurado"),
            " · ",
            tags$strong("Alcance: "),
            if (request_is_global_admin()) "todos los países e instituciones" else paste(value_or_default(user_profile$country, "País no configurado"), "-", value_or_default(user_profile$institution, default_institution_id))
          )
        ))
      }
      return(div(
        class = "module-panel",
        h3(request_labels[[subdivision]]),
        div(
          class = "alert alert-info",
          "Esta subdivisión de Solicitudes queda preparada para formularios o flujos futuros."
        )
      ))
    }

    div(
      class = "module-panel",
      h3("Solicitudes"),
      div(
        class = "alert alert-info",
        "Las solicitudes de datos se habilitarán únicamente para los formularios activos aprobados por EntoNet."
      )
    )
  })

  output$request_reactivos_preview_table <- renderTable({
    category_key <- active_request_reactivos_category()
    if (identical(category_key, "larvicidas")) {
      data.frame(Reactivo = c("Temefos 1 L", "Bti granulado 1 kg", "Larvex Pro 500"), Presentación = c("Ejemplo", "Ejemplo", "Ejemplo"), Cantidad = c("1", "2", "1"), `Uso previsto` = c("Larvicida", "Larvicida", "Larvicida"), check.names = FALSE, stringsAsFactors = FALSE)
    } else if (identical(category_key, "adulticidas")) {
      data.frame(Reactivo = c("AdultiMax 450", "PyroControl ULV", "CipraNeo 1 L"), Presentación = c("Ejemplo", "Ejemplo", "Ejemplo"), Cantidad = c("1", "1", "2"), `Uso previsto` = c("Adulticida", "Adulticida", "Adulticida"), check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      data.frame(Reactivo = c("ResiShield 2 L", "LongGuard 1 kg", "MuroPlus 5 L"), Presentación = c("Ejemplo", "Ejemplo", "Ejemplo"), Cantidad = c("1", "1", "1"), `Uso previsto` = c("Residual", "Residual", "Residual"), check.names = FALSE, stringsAsFactors = FALSE)
    }
  }, striped = TRUE, bordered = TRUE, spacing = "s", align = "lccc")

  output$active_dataset_header <- renderUI({
    dataset <- active_dataset()
    subdivision <- active_capture_subdivision()

    if (is.null(dataset)) {
      if (identical(subdivision, "campo")) {
        return(h3(class = "capture-dataset-title", "Campo"))
      }
      if (identical(subdivision, "insectario")) {
        return(h3(class = "capture-dataset-title", "Insectario"))
      }
      if (identical(subdivision, "laboratorio")) {
        return(h3(class = "capture-dataset-title", "Laboratorio"))
      }
      return(tagList(
        div(
          class = "capture-code-guide",
          h4("Construcción de códigos"),
          p("Los códigos se construyen en secuencia para mantener la trazabilidad desde el campo hasta los bioensayos."),
          tags$ul(
            tags$li(tags$strong("Código cuadrante: "), "REI + año + país + código departamento/municipio + C###. Ejemplo: ", tags$code("REI25GT0503C001")),
            tags$li(tags$strong("Código población: "), "se deriva del código territorial del cuadrante y reemplaza el cuadrante por P#. Ejemplo: ", tags$code("REI25GT0503P2")),
            tags$li(tags$strong("Código bioensayo: "), "REI + año + país + código departamento/municipio + P# + sinergista si aplica + insecticida + correlativo + generación filial. Ejemplo: ", tags$code("REI26GT0920P2DEFDEL1F1"))
          ),
          div(
            class = "capture-code-flow",
            div(
              class = "capture-code-flow-step",
              strong("1. Campo"),
              span("Se define el código cuadrante desde país, ubicación y número de cuadrante."),
              div(class = "capture-code-pattern", "REI + AA + PAIS + DEP/MUNI + C###")
            ),
            div(class = "capture-code-flow-arrow", "->"),
            div(
              class = "capture-code-flow-step",
              strong("2. Población"),
              span("Uno o más cuadrantes forman una población para evaluación."),
              div(class = "capture-code-pattern", "REI + AA + PAIS + DEP/MUNI + P#")
            ),
            div(class = "capture-code-flow-arrow", "->"),
            div(
              class = "capture-code-flow-step",
              strong("3. Bioensayo"),
              span("La población se combina con sinergista, insecticida, correlativo y generación filial."),
              div(class = "capture-code-pattern", "REI + AA + PAIS + DEP/MUNI + P# + SIN + INS + # + F#")
            )
          )
        )
      ))
    }

    if (identical(dataset, "formulario_5_alimentacion_conteo")) {
      return(h3(class = "capture-dataset-title", "Formulario 5: Alimentación conteo"))
    }

    if (identical(dataset, "formulario_1_colocacion_retiro_ovitrampa")) {
      return(h3(class = "capture-dataset-title", "Formulario 1: Colocación y retiro de ovitrampa"))
    }

    if (identical(dataset, "formulario_7_bioensayo_botella_cdc")) {
      return(h3(class = "capture-dataset-title", "Formulario 7: Bioensayo de botella CDC"))
    }

    div(class = "alert alert-info", "Seleccione un formulario activo en el menú lateral.")
  })

  capture_action_row <- function(title, description, button_id, button_label) {
    div(
      class = "capture-action-row",
      div(
        h4(title),
        p(description)
      ),
      actionButton(button_id, button_label, class = "btn-primary")
    )
  }

  output$data_entry_area <- renderUI({
    dataset <- active_dataset()
    subdivision <- active_capture_subdivision()

    if (is.null(dataset)) {
      if (identical(subdivision, "campo")) {
        return(div(
          class = "capture-form-choice-list",
          actionButton(
            "select_formulario_1_capture",
            tagList(
              strong("Formulario 1: Colocación y retiro de ovitrampa"),
              span("Ingrese a las opciones de subida masiva, ingreso individual, revisión e impresión del Formulario 1.")
            ),
            class = "capture-form-choice"
          )
        ))
      }
      if (identical(subdivision, "insectario")) {
        return(div(
          class = "capture-form-choice-list",
          actionButton(
            "select_formulario_5_capture",
            tagList(
              strong("Formulario 5: Alimentación y conteo"),
              span("Ingrese a las opciones de subida masiva, ingreso individual y revisión del Formulario 5.")
            ),
            class = "capture-form-choice"
          ),
          actionButton(
            "select_formulario_7_capture",
            tagList(
              strong("Formulario 7: Bioensayo de botella CDC"),
              span("Ingrese a las opciones de subida masiva, ingreso individual, revisión e impresión del Formulario 7.")
            ),
            class = "capture-form-choice capture-form-choice-secondary"
          )
        ))
      }
      if (identical(subdivision, "laboratorio")) {
        return(div(
          class = "capture-form-choice-list",
          div(
            class = "capture-form-choice capture-form-choice-secondary",
            strong("Laboratorio"),
            span("Sin formularios activos por el momento. Este espacio queda reservado para futuros formularios de laboratorio.")
          )
        ))
      }
      return(NULL)
    }

    if (identical(dataset, "formulario_1_colocacion_retiro_ovitrampa")) {
      return(tagList(
        div(
          class = "formulario-1-capture-layout",
          div(
            class = "capture-action-list",
            capture_action_row("Subida de datos masiva", "Cargue registros de colocación y retiro desde el machote CSV oficial de Formulario 1.", "open_formulario_1_bulk_upload", "Abrir subida masiva"),
            capture_action_row("Ingreso individual de datos", "Capture una ovitrampa/sustrato con coordenadas, códigos y estado de retiro.", "open_formulario_1_entry", "Abrir ingreso individual"),
            capture_action_row("Revisar formulario", "Abra registros de Formulario 1 por casa para confirmar la revisión o editar sus valores.", "open_formulario_1_review", "Abrir revisión"),
            capture_action_row("Imprimir formulario", "Genere el machote de campo en Excel con los datos territoriales y cuadrantes necesarios.", "open_formulario_1_print", "Abrir impresión")
          ),
          div(
            class = "form-preview-panel",
            tags$img(src = "formulario_1_version_2_preview.png", alt = "Vista previa del machote actual del Formulario 1"),
            h4("Machote actual del Formulario 1"),
            p("Esta es la versión 2 elaborada el 4 de Agosto del 2026."),
            p(
              "Para que los códigos de barras se vean al imprimir, instale la fuente ",
              tags$a(
                href = "https://fonts.google.com/specimen/Libre+Barcode+39",
                target = "_blank",
                rel = "noopener noreferrer",
                "Libre Barcode 39"
              ),
              ". En Windows, descargue la familia desde Google Fonts, descomprima el archivo y haga doble click en el .ttf para seleccionar Instalar. En Mac, abra el .ttf con Font Book y seleccione Install Font. Luego cierre y vuelva a abrir Excel o LibreOffice antes de imprimir."
            )
          )
        ),
        h4("Estado del envío"),
        verbatimTextOutput("submission_status")
      ))
    }

    if (identical(dataset, "formulario_5_alimentacion_conteo")) {
      return(tagList(
        div(
          class = "capture-action-list",
          capture_action_row("Subida de datos masiva", "Cargue varios registros de Formulario 5 desde un archivo CSV usando el machote oficial.", "open_formulario_5_bulk_upload", "Abrir subida masiva"),
          capture_action_row("Ingreso individual de datos", "Ingrese un registro a la vez mediante el formulario guiado de Formulario 5.", "open_formulario_5_entry", "Abrir ingreso individual"),
          capture_action_row("Revisar formulario", "Consulte los registros capturados, confirme la revisión o habilite sus valores para editarlos.", "open_formulario_5_review", "Abrir revisión")
        ),
        h4("Estado del envío"),
        verbatimTextOutput("submission_status")
      ))
    }

    if (identical(dataset, "formulario_7_bioensayo_botella_cdc")) {
      return(tagList(
        div(
          class = "formulario-1-capture-layout",
          div(
            class = "capture-action-list",
            capture_action_row("Subida de datos masiva", "Cargue varios bioensayos desde el machote CSV oficial de 118 columnas visibles.", "open_formulario_7_bulk_upload", "Abrir subida masiva"),
            capture_action_row("Ingreso individual de datos", "Capture un bioensayo con las lecturas agrupadas por botella y tiempo.", "open_formulario_7_entry", "Abrir ingreso individual"),
            capture_action_row("Revisar formulario", "Abra registros de Formulario 7 para confirmar la revisión o activar y editar sus valores.", "open_formulario_7_review", "Abrir revisión"),
            capture_action_row("Imprimir formulario", "Genere el machote de Formulario 7 con Código Bioensayo y ubicación prellenados.", "open_formulario_7_print", "Abrir impresión")
          ),
          div(
            class = "form-preview-panel",
            tags$img(src = "formulario_7_version_2_preview.jpg", alt = "Vista previa del machote actual del Formulario 7"),
            h4("Machote actual del Formulario 7"),
            p("Esta es la versión 2 del machote de campo para el registro de datos del bioensayo de la botella CDC."),
            p(
              "Para que los códigos de barras se vean al imprimir, instale la fuente ",
              tags$a(
                href = "https://fonts.google.com/specimen/Libre+Barcode+39",
                target = "_blank",
                rel = "noopener noreferrer",
                "Libre Barcode 39"
              ),
              ". En Windows, descargue la familia desde Google Fonts, descomprima el archivo y haga doble click en el .ttf para seleccionar Instalar. En Mac, abra el .ttf con Font Book y seleccione Install Font. Luego cierre y vuelva a abrir Excel o LibreOffice antes de imprimir."
            )
          )
        ),
        h4("Estado del envío"),
        verbatimTextOutput("submission_status")
      ))
    }

    NULL
  })

  form_data <- reactive({
    req(active_dataset() == "egg_count_raw")

    optional_date <- function(value) {
      if (is.null(value) || length(value) == 0 || is.na(value)) {
        return(as.Date(NA))
      }
      value
    }
    optional_number <- function(value) {
      if (is.null(value) || length(value) == 0 || is.na(value)) {
        return(NA_real_)
      }
      value
    }
    optional_text <- function(value) {
      if (is.null(value) || length(value) == 0 || is.na(value)) {
        return("")
      }
      trimws(value)
    }

    counts <- c(
      intact_eggs = optional_number(input$intact_eggs),
      hatched_eggs = optional_number(input$hatched_eggs),
      canoe_eggs = optional_number(input$canoe_eggs),
      unfertilized_eggs = optional_number(input$unfertilized_eggs),
      other_species_count = optional_number(input$other_species_count)
    )

    list(
      country = optional_text(input$country),
      cycle = optional_number(input$cycle),
      round_number = as.integer(optional_number(input$round_number)),
      quadrant = optional_number(input$quadrant),
      oviposition_code = optional_text(input$oviposition_code),
      substrate_code = optional_text(input$substrate_code),
      collection_site = optional_text(input$collection_site),
      placement_date = optional_date(input$placement_date),
      removal_date = optional_date(input$removal_date),
      count_date = optional_date(input$count_date),
      count_responsible_code = optional_text(input$count_responsible_code),
      intact_eggs = counts[["intact_eggs"]],
      hatched_eggs = counts[["hatched_eggs"]],
      canoe_eggs = counts[["canoe_eggs"]],
      unfertilized_eggs = counts[["unfertilized_eggs"]],
      other_species_count = counts[["other_species_count"]],
      calculated_total = sum(counts, na.rm = TRUE),
      notes = optional_text(input$notes)
    )
  })

  validation_errors <- reactive({
    if (!identical(active_dataset(), "egg_count_raw")) {
      return(character())
    }

    record <- form_data()
    errors <- character()

    if (!nzchar(record$country) || !(record$country %in% country_choices)) {
      errors <- c(errors, "El país es obligatorio y debe seleccionarse de la lista permitida.")
    }
    if (is.na(record$quadrant) || record$quadrant < 1) {
      errors <- c(errors, "El cuadrante es obligatorio y debe ser al menos 1.")
    }
    if (!nzchar(record$oviposition_code)) {
      errors <- c(errors, "El código de oviposición es obligatorio.")
    }
    if (is.na(record$count_date)) {
      errors <- c(errors, "La fecha de conteo es obligatoria.")
    }
    if (!nzchar(record$count_responsible_code)) {
      errors <- c(errors, "El código de responsable de conteo es obligatorio.")
    }
    if (!is.na(record$placement_date) &&
        !is.na(record$removal_date) &&
        record$placement_date > record$removal_date) {
      errors <- c(errors, "La fecha de colocación no puede ser posterior a la fecha de retiro.")
    }

    counts <- unlist(record[c(
      "intact_eggs",
      "hatched_eggs",
      "canoe_eggs",
      "unfertilized_eggs",
      "other_species_count"
    )])
    if (any(is.na(counts)) || any(counts < 0)) {
      errors <- c(errors, "Todos los conteos de huevos son obligatorios y deben ser cero o mayores.")
    }

    errors
  })

  validation_warnings <- reactive({
    if (!identical(active_dataset(), "egg_count_raw")) {
      return(character())
    }

    record <- form_data()
    warnings <- character()

    if (!is.na(record$placement_date) && record$placement_date > Sys.Date()) {
      warnings <- c(warnings, "La fecha de colocación es posterior a la fecha actual. Confirme la fecha antes de enviar.")
    }
    if (!is.na(record$placement_date) &&
        !is.na(record$removal_date) &&
        record$removal_date > record$placement_date + 2) {
      warnings <- c(warnings, "La fecha de retiro es más de 2 días posterior a la fecha de colocación. Confirme el intervalo de colecta.")
    }

    warnings
  })

  output$calculated_total <- renderText({
    req(active_dataset() == "egg_count_raw")
    form_data()$calculated_total
  })

  output$validation_message <- renderUI({
    if (is.null(active_dataset())) {
      return(NULL)
    }

    if (!identical(active_dataset(), "egg_count_raw")) {
      return(div(
        class = "alert alert-info",
        "Este formulario aún no está activo porque las variables finales de este conjunto de datos están pendientes."
      ))
    }

    errors <- validation_errors()
    warnings <- validation_warnings()

    messages <- list()

    if (length(errors) > 0) {
      messages <- c(
        messages,
        list(div(
          class = "alert alert-warning",
          strong("Por favor revise:"),
          tags$ul(lapply(errors, tags$li))
        ))
      )
    }

    if (length(warnings) > 0) {
      messages <- c(
        messages,
        list(div(
          class = "alert alert-info",
          strong("Alertas de fecha:"),
          tags$ul(lapply(warnings, tags$li))
        ))
      )
    }

    if (length(messages) == 0) {
      return(div(class = "alert alert-success", "El formulario actual cumple con la validación."))
    }

    tagList(messages)
  })

  output$preview <- renderTable({
    req(active_dataset() == "egg_count_raw")

    record <- form_data()
    data.frame(
      Field = names(record),
      Value = vapply(record, as.character, character(1)),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$submission_status <- renderText({
    req(active_dataset() %in% c("formulario_1_colocacion_retiro_ovitrampa", "formulario_5_alimentacion_conteo", "formulario_7_bioensayo_botella_cdc"))
    submission_status()
  })

  output$f5_total_individuos <- renderText({
    sum(
      as.numeric(value_or_default(input$f5_numero_hembras, 0)),
      as.numeric(value_or_default(input$f5_numero_machos, 0)),
      na.rm = TRUE
    )
  })

  output$f5_total_huevos <- renderText({
    sum(
      as.numeric(value_or_default(input$f5_hv_huevos_viables, 0)),
      as.numeric(value_or_default(input$f5_he_huevos_eclosionados, 0)),
      as.numeric(value_or_default(input$f5_hc_huevos_canoa, 0)),
      as.numeric(value_or_default(input$f5_hnf_huevos_no_fecundados, 0)),
      na.rm = TRUE
    )
  })

  output$download_egg_count_template <- downloadHandler(
    filename = function() {
      paste0("machote_ovipostura_conteo_huevos_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(egg_count_template, file, row.names = FALSE, na = "")
    }
  )

  output$download_formulario_5_template <- downloadHandler(
    filename = function() {
      paste0("machote_formulario_5_alimentacion_conteo_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      template <- formulario_5_template
      template$fecha_registro <- as.character(Sys.Date())
      write.csv(template, file, row.names = FALSE, na = "")
    }
  )

  output$download_formulario_1_template <- downloadHandler(
    filename = function() {
      paste0("machote_formulario_1_colocacion_retiro_ovitrampa_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      template <- formulario_1_template
      template$fecha_registro <- as.character(Sys.Date())
      write.csv(template, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  output$download_formulario_7_template <- downloadHandler(
    filename = function() {
      paste0("machote_formulario_7_bioensayo_botella_cdc_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      template <- formulario_7_template
      template$fecha_registro <- as.character(Sys.Date())
      write.csv(formulario_7_internal_to_csv(template), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  output$download_formulario_7_printable <- downloadHandler(
    filename = function() {
      code <- f7_print_codigo_bioensayo_code()
      code <- if (!is.na(code) && nzchar(code)) code else format(Sys.Date(), "%Y%m%d")
      paste0("formulario_7_", code, ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      code <- f7_print_codigo_bioensayo_code()
      municipality_code <- f7_print_selected_municipality_code()
      department_code <- value_or_default(input$f7_print_codigo_bioensayo_departamento, "")
      if (is.na(code) || !nzchar(code)) {
        stop("Complete país, departamento, municipio, población, insecticida, correlativo, generación filial, tipo de bioensayo y año antes de descargar.")
      }
      version_formulario <- toupper(trimws(value_or_default(input$f7_print_version_formulario, "2")))
      if (!nzchar(version_formulario)) version_formulario <- "2"
      type <- value_or_default(input$f7_print_tipo_bioensayo, "DD")
      type_label <- switch(
        type,
        DD = "Dosis Diagnostica",
        IE = "Intensidad Exploratoria",
        IC = paste("Intensidad Completa", toupper(trimws(value_or_default(input$f7_print_intensidad_completa_dosis, "")))),
        S = paste("Sinergista", toupper(trimws(value_or_default(input$f7_print_sinergista, "")))),
        "Bioensayo"
      )
      department_label <- department_code
      department_row <- formulario_7_departamento_catalogo[
        formulario_7_departamento_catalogo$pais == input$f7_print_pais &
          formulario_7_departamento_catalogo$departamento_codigo == department_code,
      ]
      if (nrow(department_row) > 0) department_label <- paste0(department_row$departamento[[1]], " (", department_code, ")")
      municipality_label <- municipality_code
      municipality_row <- formulario_7_municipio_catalogo[
        formulario_7_municipio_catalogo$pais == input$f7_print_pais &
          formulario_7_municipio_catalogo$municipio_codigo == municipality_code,
      ]
      if (nrow(municipality_row) > 0) municipality_label <- paste0(municipality_row$municipio[[1]], " (", municipality_code, ")")
      f7_create_printable_xlsx(
        file = file,
        pais = f5_text(input$f7_print_pais),
        departamento = department_label,
        municipio = municipality_label,
        codigo_bioensayo = code,
        nombre_poblacion = f5_optional_text(input$f7_print_nombre_poblacion),
        tipo_bioensayo = type_label,
        version_formulario = version_formulario
      )
    }
  )

  output$download_formulario_7_printable_csv_legacy <- downloadHandler(
    filename = function() {
      code <- f7_print_codigo_bioensayo_code()
      code <- if (!is.na(code) && nzchar(code)) code else format(Sys.Date(), "%Y%m%d")
      paste0("formulario_7_", code, ".csv")
    },
    content = function(file) {
      code <- f7_print_codigo_bioensayo_code()
      municipality_code <- f7_print_selected_municipality_code()
      department_code <- value_or_default(input$f7_print_codigo_bioensayo_departamento, "")
      if (is.na(code) || !nzchar(code)) {
        stop("Complete país, departamento, municipio, población, insecticida, correlativo, generación filial, tipo de bioensayo y año antes de descargar.")
      }
      template <- formulario_7_template
      template$fecha_registro <- as.character(Sys.Date())
      template$pais <- f5_text(input$f7_print_pais)
      template$codigo_departamento <- department_code
      template$codigo_municipio <- municipality_code
      template$codigo_bioensayo <- code
      template$insecticida <- f5_text(input$f7_print_insecticida)
      template$nombre_poblacion <- f5_optional_text(input$f7_print_nombre_poblacion)
      template$generacion_filial <- f7_generation_code(input$f7_print_generacion_filial)
      type <- value_or_default(input$f7_print_tipo_bioensayo, "DD")
      if (identical(type, "DD")) {
        template$bioensayo_diagnostica_1x <- "true"
      } else if (identical(type, "IE")) {
        template$bioensayo_diagnostica_1x <- "false"
        template$bioensayo_intensidad <- "Exploratorio"
      } else if (identical(type, "IC")) {
        template$bioensayo_diagnostica_1x <- "false"
        template$bioensayo_intensidad <- "Completa"
        template$dosis_intensidad <- toupper(trimws(value_or_default(input$f7_print_intensidad_completa_dosis, "")))
      } else if (identical(type, "S")) {
        template$bioensayo_diagnostica_1x <- "false"
        selected_synergist <- tolower(trimws(value_or_default(input$f7_print_sinergista, "")))
        template$sinergista_def <- as.character(identical(selected_synergist, "def"))
        template$sinergista_pbo <- as.character(identical(selected_synergist, "pbo"))
        template$sinergista_dm <- as.character(identical(selected_synergist, "dm"))
      }
      write.csv(formulario_7_internal_to_csv(template), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  for (index in seq_len(nrow(laboratory_protocols))) {
    local({
      protocol <- laboratory_protocols[index, ]
      output_id <- paste0("download_laboratory_protocol_", protocol$id)

      output[[output_id]] <- downloadHandler(
        filename = function() {
          protocol$file_name
        },
        contentType = protocol$content_type,
        content = function(file) {
          download_storage_object(
            bucket = "project-rei-protocols",
            object_path = protocol$storage_path,
            destination_file = file
          )
        }
      )
    })
  }

  bulk_upload_result <- reactiveVal(NULL)

  formulario_5_bulk_upload_result <- reactiveVal(NULL)

  output$formulario_1_bulk_upload_status <- renderUI({
    result <- formulario_1_bulk_upload_result()
    if (is.null(result)) return(NULL)
    div(
      class = if (identical(result$type, "success")) "alert alert-success" else "alert alert-warning",
      strong(result$message),
      if (length(result$details)) tags$ul(lapply(result$details, tags$li))
    )
  })

  f1_save_status_ui <- function() {
    status <- f1_save_status()
    if (identical(status$type, "idle")) return(NULL)
    div(
      class = if (identical(status$type, "success")) "alert alert-success" else "alert alert-warning",
      strong(status$message),
      if (length(status$details)) tags$ul(lapply(status$details, tags$li))
    )
  }

  output$f1_save_status <- renderUI(f1_save_status_ui())
  output$f1_save_status_bottom <- renderUI(f1_save_status_ui())

  output$f1_resume_status <- renderUI({
    status <- f1_resume_status()
    if (identical(status$type, "idle")) return(NULL)
    div(
      class = if (identical(status$type, "success")) "alert alert-success" else "alert alert-warning",
      strong(status$message),
      if (length(status$details)) tags$ul(lapply(status$details, tags$li))
    )
  })

  observeEvent(input$f1_resume_form, {
    codigo_formulario <- toupper(trimws(value_or_default(input$f1_resume_codigo_formulario, "")))
    if (!nzchar(codigo_formulario)) {
      f1_resume_status(list(type = "error", message = "Ingrese un código de formulario para cargar.", details = character()))
      return()
    }

    connection <- NULL
    tryCatch({
      withProgress(message = "Cargando Formulario 1", value = 0, {
        incProgress(0.25, detail = "Buscando datos guardados")
        connection <- connect_to_supabase()
        rows <- f1_fetch_existing_formulario_1_rows(connection, codigo_formulario)
        if (nrow(rows) == 0) {
          f1_resume_status(list(type = "error", message = "No se encontraron registros con ese código de formulario.", details = character()))
          return()
        }

        first <- rows[1, , drop = FALSE]
        unique_quadrants <- unique(rows$cuadrante[!is.na(rows$cuadrante) & nzchar(trimws(rows$cuadrante))])
        if (!length(unique_quadrants)) stop("El formulario encontrado no tiene códigos de cuadrante guardados.")
        house_counts <- vapply(unique_quadrants, function(quadrant_code) {
          length(unique(rows$codigo_casa[rows$cuadrante == quadrant_code & !is.na(rows$codigo_casa) & nzchar(trimws(rows$codigo_casa))]))
        }, integer(1))
        house_keys <- paste(rows$cuadrante, rows$codigo_casa, sep = "\r")
        trap_counts <- vapply(unique(house_keys), function(house_key) {
          length(unique(rows$codigo_sustrato[house_keys == house_key & !is.na(rows$codigo_sustrato) & nzchar(trimws(rows$codigo_sustrato))]))
        }, integer(1))

        configured_quadrants <- f5_integer(input$f1_num_quadrants)
        configured_houses <- f5_integer(input$f1_casas_por_cuadrante)
        configured_traps <- f5_integer(input$f1_ovitrampas_por_casa)
        quadrants <- max(c(length(unique_quadrants), configured_quadrants), na.rm = TRUE)
        houses <- max(c(house_counts, configured_houses, 1L), na.rm = TRUE)
        traps <- max(c(trap_counts, suppressWarnings(as.integer(rows$Ovitrampas_colocadas)), configured_traps), na.rm = TRUE)
        traps <- min(max(as.integer(traps), 1L), 8L)

        incProgress(0.35, detail = "Repoblando datos generales")
        pais <- ubicacion_normalizar_pais(f1_first_non_empty(first$pais))
        departamento_codigo <- ubicacion_resolver_departamento_codigo(pais, f1_first_non_empty(first$departamento))
        municipio_codigo <- ubicacion_resolver_municipio_codigo(pais, f1_first_non_empty(first$municipio))
        updateSelectInput(session, "f1_pais", selected = pais)
        updateTextInput(session, "f1_id_institucion", value = f1_first_non_empty(first$id_institucion, default_institution_id))
        updateSelectInput(session, "f1_departamento", choices = ubicacion_departamento_choices(pais), selected = departamento_codigo)
        session$onFlushed(function() {
          municipio_choices <- ubicacion_municipio_choices(pais, departamento_codigo, include_manual = TRUE)
          if (municipio_codigo %in% unname(municipio_choices)) {
            updateSelectInput(session, "f1_municipio", choices = municipio_choices, selected = municipio_codigo)
            updateTextInput(session, "f1_municipio_manual", value = "")
          } else {
            updateSelectInput(session, "f1_municipio", choices = municipio_choices, selected = "__manual__")
            updateTextInput(session, "f1_municipio_manual", value = municipio_codigo)
          }
        }, once = TRUE)
        updateTextInput(session, "f1_ciclo", value = f1_first_non_empty(first$ciclo))
        updateTextInput(session, "f1_ronda", value = f1_first_non_empty(first$ronda))
        updateTextInput(session, "f1_codigo_formulario", value = f1_first_non_empty(first$codigo_formulario, codigo_formulario))
        f1_update_date_input("f1_fecha_registro", first$fecha_registro[[1]])
        updateNumericInput(session, "f1_Latitud", value = suppressWarnings(as.numeric(first$Latitud[[1]])))
        updateNumericInput(session, "f1_Longitud", value = suppressWarnings(as.numeric(first$Longitud[[1]])))
        updateTextInput(session, "f1_codigo_gps", value = f1_first_non_empty(first$codigo_gps))
        updateTextInput(session, "f1_fuente_formulario", value = f1_first_non_empty(first$fuente_formulario))
        updateTextInput(session, "f1_creado_por", value = f1_first_non_empty(first$creado_por))
        f1_update_date_input("f1_fecha_colocacion", first$fecha_colocacion[[1]])
        updateTextInput(session, "f1_grupo_responsable_colocacion", value = f1_first_non_empty(first$grupo_responsable_colocacion))
        updateNumericInput(session, "f1_num_quadrants", value = quadrants)
        updateTextInput(session, "f1_codigo_cuadrante_base", value = unique_quadrants[[1]])
        updateNumericInput(session, "f1_casas_por_cuadrante", value = houses)
        updateTextInput(session, "f1_codigo_casa_base", value = f1_first_non_empty(rows$codigo_casa))
        updateNumericInput(session, "f1_ovitrampas_por_casa", value = traps)
        updateTextInput(session, "f1_codigo_sustrato_base", value = f1_substrate_base_from_code(f1_first_non_empty(rows$codigo_sustrato)))
        f1_update_date_input("f1_fecha_retiro", first$fecha_retiro[[1]])
        updateTextInput(session, "f1_grupo_responsable_retiro", value = f1_first_non_empty(first$grupo_responsable_retiro))

        incProgress(0.25, detail = "Generando tabs de cuadrantes")
        f1_quadrant_config(list(
          quadrants = quadrants,
          houses_per_quadrant = houses,
          traps_per_house = traps
        ))
        f1_generated_quadrants(seq_len(quadrants))
        f1_editable_quadrants(seq_len(quadrants))
        updateTabsetPanel(session, "f1_capture_tab", selected = "Cuadrantes")

        session$onFlushed(function() {
          for (quadrant_index in seq_along(unique_quadrants)) {
            if (quadrant_index > quadrants) next
            quadrant_code <- unique_quadrants[[quadrant_index]]
            updateTextInput(session, paste0("f1_edit_cuadrante_", quadrant_index), value = quadrant_code)
            quadrant_rows <- rows[rows$cuadrante == quadrant_code, , drop = FALSE]
            house_codes <- unique(quadrant_rows$codigo_casa[!is.na(quadrant_rows$codigo_casa) & nzchar(trimws(quadrant_rows$codigo_casa))])
            for (house_index in seq_along(house_codes)) {
              if (house_index > houses) next
              house_code <- house_codes[[house_index]]
              house_suffix <- paste(quadrant_index, house_index, sep = "_")
              house_rows <- quadrant_rows[quadrant_rows$codigo_casa == house_code, , drop = FALSE]
              house_first <- house_rows[1, , drop = FALSE]
              updateTextInput(session, paste0("f1_edit_casa_", house_suffix), value = house_code)
              updateTextInput(session, paste0("f1_edit_sustrato_", house_suffix), value = f1_substrate_base_from_code(f1_first_non_empty(house_rows$codigo_sustrato)))
              updateNumericInput(session, paste0("f1_Ovitrampas_retiradas_", house_suffix), value = suppressWarnings(as.integer(house_first$Ovitrampas_retiradas[[1]])))
              updateNumericInput(session, paste0("f1_retiro_buen_estado_", house_suffix), value = suppressWarnings(as.integer(house_first$retiro_buen_estado[[1]])))
              updateNumericInput(session, paste0("f1_retiro_sin_agua_", house_suffix), value = suppressWarnings(as.integer(house_first$retiro_sin_agua[[1]])))
              updateNumericInput(session, paste0("f1_retiro_sin_sustrato_", house_suffix), value = suppressWarnings(as.integer(house_first$retiro_sin_sustrato[[1]])))
              updateNumericInput(session, paste0("f1_retiro_sin_ovitrampa_", house_suffix), value = suppressWarnings(as.integer(house_first$retiro_sin_ovitrampa[[1]])))
              updateNumericInput(session, paste0("f1_retiro_casa_cerrada_", house_suffix), value = suppressWarnings(as.integer(house_first$retiro_casa_cerrada[[1]])))
            }
          }
        }, once = TRUE)

        incProgress(0.15, detail = "Listo")
        f1_resume_status(list(
          type = "success",
          message = paste0("Se cargó el formulario ", codigo_formulario, " para continuar el ingreso."),
          details = c(
            paste0("Registros/sustratos existentes encontrados: ", nrow(rows), "."),
            paste0("Configuración activa: ", quadrants, " cuadrante(s), ", houses, " casa(s) por cuadrante y ", traps, " ovitrampa(s) por casa."),
            "Al guardar, la app omitirá los sustratos que ya existen y guardará solo los nuevos."
          )
        ))
      })
    }, error = function(error) {
      f1_resume_status(list(type = "error", message = "No se pudo cargar el Formulario 1 desde Supabase.", details = conditionMessage(error)))
    }, finally = {
      if (!is.null(connection)) dbDisconnect(connection)
    })
  })

  observeEvent(input$save_formulario_1, {
    qcount <- f1_quadrant_count()
    generated <- f1_generated_quadrants()
    if (is.null(qcount) || !setequal(seq_len(qcount), generated)) {
      f1_save_status(list(
        type = "error",
        message = "Genere todos los cuadrantes antes de guardar.",
        details = "Use el botón Generar cuadrantes en la pestaña Colocación."
      ))
      return()
    }
    connection <- NULL
    tryCatch({
      withProgress(message = "Guardando Formulario 1", value = 0, {
        incProgress(0.25, detail = "Validando datos de colocación y retiro")
        validated <- validate_formulario_1(formulario_1_input_row())
        if (length(validated$details)) {
          f1_save_status(list(type = "error", message = "Revise los campos del Formulario 1.", details = validated$details))
          return()
        }

        incProgress(0.25, detail = "Conectando con Supabase")
        connection <- connect_to_supabase()

        incProgress(0.15, detail = "Revisando sustratos ya guardados")
        filtered <- f1_rows_without_existing_sustratos(connection, validated$data)
        skipped_sustratos <- unique(filtered$skipped)
        data_to_insert <- filtered$data
        if (nrow(data_to_insert) == 0) {
          f1_save_status(list(
            type = "error",
            message = "No hay ovitrampas nuevas para guardar.",
            details = c(
              "Todos los códigos de sustrato generados para este código de formulario ya existen en Supabase.",
              if (length(skipped_sustratos)) paste0("Sustratos ya existentes: ", paste(head(skipped_sustratos, 20), collapse = ", "), if (length(skipped_sustratos) > 20) "..." else "")
            )
          ))
          return()
        }

        incProgress(0.35, detail = "Guardando registro y sustratos")
        intake_ids <- insert_formulario_1(connection, data_to_insert)

        incProgress(0.15, detail = "Actualizando pantalla")
        f1_save_status(list(
          type = "success",
          message = paste0("Registro guardado con intake_id ", intake_ids[[1]], " y estado pending."),
          details = if (length(skipped_sustratos)) {
            paste0("Se omitieron ", length(skipped_sustratos), " sustrato(s) que ya existían para este código de formulario.")
          } else {
            character()
          }
        ))
        submission_status(paste0("Formulario 1 guardado con intake_id ", intake_ids[[1]], ". Estado de revisión: pending."))
        f1_reset_capture_inputs()
      })
    }, error = function(error) {
      f1_save_status(list(type = "error", message = "No se pudo guardar el Formulario 1 en Supabase.", details = conditionMessage(error)))
    }, finally = {
      if (!is.null(connection)) dbDisconnect(connection)
    })
  })

  output$f1_review_status_message <- renderUI({
    status <- f1_review_status()
    if (is.null(status$type) || identical(status$type, "idle")) return(NULL)
    selected <- f1_review_selected()
    detail_messages <- c(
      "Revise los valores editados.",
      "No se pudieron guardar los cambios.",
      "No se puede eliminar el registro sin comentario.",
      "No se pudo eliminar el registro.",
      "Cambios guardados para intake_id",
      "Registro %s eliminado definitivamente.",
      "No se pudo confirmar el registro."
    )
    if (!is.null(selected) && any(startsWith(status$message, detail_messages))) return(NULL)
    css <- switch(status$type, success = "alert alert-success", error = "alert alert-danger", warning = "alert alert-warning", "alert alert-info")
    div(class = css, strong(status$message), if (length(status$details)) tags$ul(lapply(status$details, tags$li)))
  })

  output$f1_review_detail_status_message <- renderUI({
    selected <- f1_review_selected()
    if (is.null(selected)) return(NULL)
    status <- f1_review_status()
    if (is.null(status$type) || identical(status$type, "idle")) return(NULL)
    css <- switch(status$type, success = "alert alert-success", error = "alert alert-danger", warning = "alert alert-warning", "alert alert-info")
    div(class = css, strong(status$message), if (length(status$details)) tags$ul(lapply(status$details, tags$li)))
  })

  output$f1_review_record_list <- renderUI({
    records <- f1_review_records()
    if (is.null(records) || nrow(records) == 0) return(NULL)
    rows <- lapply(seq_len(nrow(records)), function(index) {
      record <- records[index, ]
      intake_id <- as.character(record$intake_id[[1]])
      tags$tr(
        tags$td(tags$a(
          href = "#",
          onclick = sprintf("Shiny.setInputValue('f1_review_select_id', '%s', {priority: 'event'}); return false;", intake_id),
          intake_id
        )),
        tags$td(f5_review_text_value(record$codigo_formulario)),
        tags$td(as.character(record$fecha_registro[[1]])),
        tags$td(f5_review_text_value(record$pais)),
        tags$td(f5_review_text_value(record$cuadrante)),
        tags$td(f5_review_text_value(record$codigo_casa)),
        tags$td(f5_review_text_value(record$creado_por)),
        tags$td(f5_review_text_value(record$review_status))
      )
    })
    tagList(
      h4("Listado para revisión"),
      tags$table(
        class = "table table-striped table-condensed",
        tags$thead(tags$tr(
          tags$th("intake_id"), tags$th("Código formulario"), tags$th("Fecha"),
          tags$th("País"), tags$th("Cuadrante"), tags$th("Casa"), tags$th("Ingresado por"), tags$th("Estado")
        )),
        tags$tbody(rows)
      )
    )
  })

  output$f1_review_record_detail <- renderUI({
    selected <- f1_review_selected()
    if (is.null(selected) || is.null(selected$header) || nrow(selected$header) == 0) return(NULL)
    row <- selected$data
    header <- selected$header
    edit_mode <- f1_review_edit_mode()
    delete_mode <- f1_review_delete_mode()

    value_for <- function(field) {
      value <- row[[field]][[1]]
      if (is.null(value) || length(value) == 0 || is.na(value)) return("")
      as.character(value)
    }
    input_for_field <- function(field) {
      label <- f1_review_field_label(field)
      value <- value_for(field)
      if (field %in% f1_review_protected_fields) {
        return(div(class = "form-group", tags$label(label), tags$p(class = "form-control-static", if (nzchar(value)) value else "—")))
      }
      input_id <- f1_review_input_id(field)
      if (identical(field, f1_review_detail_field)) {
        return(textAreaInput(input_id, label, value = value, rows = 6, placeholder = "Un código por línea, por ejemplo SV001A"))
      }
      choices <- switch(field, pais = c("El Salvador", "Guatemala"), NULL)
      if (!is.null(choices)) return(selectInput(input_id, label, choices = choices, selected = value))
      if (field %in% f1_review_date_fields) return(dateInput(input_id, label, value = suppressWarnings(as.Date(value))))
      if (field %in% f1_review_numeric_fields) return(numericInput(input_id, label, value = suppressWarnings(as.numeric(value)), min = 0))
      if (identical(field, "retiro_casa_cerrada_descripcion")) return(textAreaInput(input_id, label, value = value, rows = 2))
      textInput(input_id, label, value = value)
    }

    section_names <- c("Datos generales", "Colocación", "Retiro", "Auditoría")
    tabs <- lapply(section_names, function(section) {
      fields <- formulario_1_intake_columns[vapply(formulario_1_intake_columns, f1_review_section, character(1)) == section]
      group_count <- min(3L, length(fields))
      groups <- split(fields, cut(seq_along(fields), breaks = group_count, labels = FALSE))
      tabPanel(
        section,
        tags$fieldset(
          disabled = if (edit_mode) NULL else "disabled",
          fluidRow(lapply(groups, function(group) column(12 / length(groups), lapply(group, input_for_field))))
        )
      )
    })

    wellPanel(
      h4(sprintf("Formulario 1 — intake_id %s", as.character(header$intake_id[[1]]))),
      p(
        tags$strong("Estado: "), header$review_status[[1]], " · ",
        tags$strong("Cuadrante: "), header$cuadrante[[1]], " · ",
        tags$strong("Casa: "), header$codigo_casa[[1]]
      ),
      uiOutput("f1_review_detail_status_message"),
      if (edit_mode) div(class = "alert alert-warning", "Modo edición activo. Al guardar, el registro volverá a estado pending hasta que sea confirmado."),
      do.call(tabsetPanel, c(list(id = "f1_review_detail_tabs"), tabs)),
      tags$hr(),
      fluidRow(
        column(6, textInput("f1_reviewed_by", "Revisado por", value = user_profile$name)),
        column(6, textAreaInput("f1_review_notes", "Notas de revisión", value = f5_review_text_value(header$review_notes), rows = 2))
      ),
      div(
        class = "submit-row",
        if (!edit_mode) actionButton("f1_review_confirm", "Confirmar registro", class = "btn-primary"),
        if (!edit_mode) actionButton("f1_review_enable_edit", "Editar", class = "btn-default"),
        if (edit_mode) actionButton("f1_review_save_changes", "Guardar cambios", class = "btn-primary"),
        if (edit_mode) actionButton("f1_review_cancel_edit", "Cancelar edición", class = "btn-default")
      ),
      tags$hr(),
      div(
        class = "f1-review-delete-zone",
        if (!delete_mode) {
          actionButton("f1_review_request_delete", "Eliminar registro", class = "btn-danger")
        } else {
          tagList(
            div(
              class = "alert alert-danger",
              tags$strong("¿Está seguro que desea eliminar este registro?"),
              tags$p("No hay vuelta atrás. Luego de su eliminación, este registro y sus sustratos serán borrados de la base de datos.")
            ),
            textAreaInput(
              "f1_review_delete_reason",
              "Comentario obligatorio: indique por qué se elimina este registro",
              value = "",
              rows = 3
            ),
            div(
              class = "submit-row",
              actionButton("f1_review_delete_confirm", "Sí, eliminar definitivamente", class = "btn-danger"),
              actionButton("f1_review_delete_cancel", "Cancelar eliminación", class = "btn-default")
            )
          )
        }
      )
    )
  })

  f1_select_review_record <- function(intake_id) {
    record <- f1_fetch_review_record(intake_id)
    if (is.null(record)) stop(sprintf("No se encontró intake_id %s.", intake_id))
    f1_review_selected(record)
    f1_review_edit_mode(FALSE)
    record
  }

  observeEvent(input$open_formulario_1_review, {
    f1_review_records(data.frame())
    f1_review_selected(NULL)
    f1_review_edit_mode(FALSE)
    f1_review_delete_mode(FALSE)
    f1_review_status(list(type = "idle", message = NULL, details = character()))
    show_formulario_1_review_modal()
    updateTextInput(session, "f1_review_exclude_submitter", value = value_or_default(user_profile$name, ""))
  })

  observeEvent(input$close_formulario_1_review, removeModal())

  observeEvent(input$open_formulario_1_print, {
    show_formulario_1_print_modal()
  })

  observeEvent(input$close_formulario_1_print, removeModal())

  output$f1_print_status <- renderUI({
    details <- character()
    if (!nzchar(trimws(value_or_default(input$f1_print_pais, "")))) details <- c(details, "Seleccione el país.")
    if (!nzchar(trimws(value_or_default(input$f1_print_departamento, "")))) details <- c(details, "Seleccione el departamento.")
    if (!nzchar(ubicacion_codigo_manual_o_seleccion(input$f1_print_municipio, input$f1_print_municipio_manual))) details <- c(details, "Seleccione el municipio.")
    if (is.na(f5_integer(input$f1_print_ciclo)) || f5_integer(input$f1_print_ciclo) < 1) details <- c(details, "Ingrese el ciclo.")
    if (is.na(f5_integer(input$f1_print_ronda)) || f5_integer(input$f1_print_ronda) < 1) details <- c(details, "Ingrese la ronda.")
    if (is.na(f5_integer(input$f1_print_num_quadrants)) || f5_integer(input$f1_print_num_quadrants) < 1) details <- c(details, "Ingrese un número de cuadrantes mayor que cero.")
    if (!f1_quadrant_code_has_structure(input$f1_print_codigo_cuadrante_base, input$f1_print_pais)) {
      details <- c(details, "Para impresión nueva use el formato REI + año + país + código municipio + C###. Ejemplo: REI25GT0503C001.")
    }
    if (is.na(f1_print_recommended_form_code()) || !nzchar(f1_print_recommended_form_code())) {
      details <- c(details, "El código de formulario se generará al completar país, municipio, ronda y ciclo.")
    }
    if (is.na(f5_integer(input$f1_print_casas_por_cuadrante)) || f5_integer(input$f1_print_casas_por_cuadrante) < 1) details <- c(details, "Ingrese un número de casas por cuadrante mayor que cero.")
    if (!f1_code_has_counter(input$f1_print_codigo_casa_base)) details <- c(details, "Código inicial de casa debe tener letras seguidas de dígitos, por ejemplo HS001.")
    if (!f1_code_has_counter(input$f1_print_codigo_sustrato_base)) details <- c(details, "Código inicial sustrato debe tener letras seguidas de dígitos, por ejemplo SV001.")
    if (!length(details)) {
      return(div(class = "alert alert-success", "Listo para generar el machote imprimible."))
    }
    div(class = "alert alert-warning", strong("Complete los datos requeridos:"), tags$ul(lapply(details, tags$li)))
  })

  output$download_formulario_1_printable <- downloadHandler(
    filename = function() {
      code <- toupper(trimws(value_or_default(f1_print_recommended_form_code(), "")))
      code <- if (nzchar(code)) gsub("[^A-Z0-9_-]+", "_", code) else format(Sys.Date(), "%Y%m%d")
      paste0("formulario_1_imprimible_", code, ".xlsx")
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    content = function(file) {
      quadrants <- f5_integer(input$f1_print_num_quadrants)
      if (is.na(quadrants) || quadrants < 1) stop("Ingrese un número de cuadrantes mayor que cero.")
      houses <- f5_integer(input$f1_print_casas_por_cuadrante)
      if (is.na(houses) || houses < 1) stop("Ingrese un número de casas por cuadrante mayor que cero.")
      ciclo <- f5_integer(input$f1_print_ciclo)
      ronda <- f5_integer(input$f1_print_ronda)
      if (is.na(ciclo) || ciclo < 1) stop("Ingrese el ciclo.")
      if (is.na(ronda) || ronda < 1) stop("Ingrese la ronda.")
      if (!f1_quadrant_code_has_structure(input$f1_print_codigo_cuadrante_base, input$f1_print_pais)) {
        stop("Código de cuadrante inicial debe usar el formato nuevo REI25GT0503C001.")
      }
      if (!f1_code_has_counter(input$f1_print_codigo_casa_base)) stop("Código inicial de casa debe tener letras seguidas de dígitos.")
      if (!f1_code_has_counter(input$f1_print_codigo_sustrato_base)) stop("Código inicial sustrato debe tener letras seguidas de dígitos.")
      pais_raw <- ubicacion_normalizar_pais(input$f1_print_pais)
      pais <- toupper(trimws(value_or_default(pais_raw, "")))
      departamento_codigo <- trimws(value_or_default(input$f1_print_departamento, ""))
      municipio_codigo <- ubicacion_codigo_manual_o_seleccion(input$f1_print_municipio, input$f1_print_municipio_manual)
      if (!nzchar(pais)) stop("Seleccione el país.")
      if (!nzchar(departamento_codigo)) stop("Seleccione el departamento.")
      if (!nzchar(municipio_codigo)) stop("Seleccione el municipio.")
      codigo_formulario <- f1_new_form_code(
        country = pais_raw,
        municipality_code = municipio_codigo,
        year = Sys.Date(),
        ronda = ronda,
        ciclo = ciclo
      )
      if (is.na(codigo_formulario) || !nzchar(codigo_formulario)) stop("No se pudo generar el código de formulario.")
      f1_create_printable_xlsx(
        file = file,
        pais = pais,
        departamento = toupper(paste0(ubicacion_departamento_nombre(pais_raw, departamento_codigo), " (", departamento_codigo, ")")),
        municipio = toupper(paste0(ubicacion_municipio_nombre(pais_raw, municipio_codigo), " (", municipio_codigo, ")")),
        codigo_formulario = codigo_formulario,
        version_formulario = "2",
        codigo_encuestadores = toupper(trimws(value_or_default(input$f1_print_codigo_encuestadores, ""))),
        ciclo = as.character(ciclo),
        ronda = as.character(ronda),
        codigo_cuadrante_base = toupper(trimws(value_or_default(input$f1_print_codigo_cuadrante_base, ""))),
        casas_por_cuadrante = min(as.integer(houses), 50L),
        codigo_casa_base = toupper(trimws(value_or_default(input$f1_print_codigo_casa_base, ""))),
        codigo_sustrato_base = toupper(trimws(value_or_default(input$f1_print_codigo_sustrato_base, ""))),
        quadrants = min(as.integer(quadrants), 50L)
      )
    }
  )

  observeEvent(input$f1_review_generate_sample, {
    f1_review_status(list(type = "info", message = "Generando muestra aleatoria del 10%...", details = character()))
    f1_review_records(data.frame())
    f1_review_selected(NULL)
    f1_review_edit_mode(FALSE)
    f1_review_delete_mode(FALSE)
    tryCatch({
      records <- withProgress(message = "Generando muestra 10%", value = 0, {
        incProgress(0.4, detail = "Consultando registros elegibles")
        sample_records <- f1_load_review_records(random_sample = TRUE)
        incProgress(0.6, detail = "Preparando listado")
        sample_records
      })
      f1_review_records(records)
      f1_review_status(list(
        type = if (nrow(records)) "success" else "warning",
        message = if (nrow(records)) sprintf("Muestra generada: %s registro(s), equivalente al 10%% del rango seleccionado.", nrow(records)) else "No hay registros en el rango y estado seleccionados.",
        details = character()
      ))
    }, error = function(error) {
      f1_review_status(list(type = "error", message = "No se pudo generar la muestra.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f1_review_refresh, {
    tryCatch({
      records <- f1_load_review_records()
      f1_review_records(records)
      f1_review_delete_mode(FALSE)
      f1_review_status(list(
        type = if (nrow(records)) "success" else "warning",
        message = if (nrow(records)) sprintf("Se cargaron %s registro(s).", nrow(records)) else "No hay registros con ese estado.",
        details = character()
      ))
    }, error = function(error) {
      f1_review_status(list(type = "error", message = "No se pudo actualizar el listado.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f1_review_find, {
    code <- toupper(trimws(value_or_default(input$f1_review_search_code, "")))
    if (!nzchar(code)) {
      f1_review_status(list(type = "warning", message = "Ingrese un código de formulario válido.", details = character()))
      return()
    }
    f1_review_selected(NULL)
    f1_review_edit_mode(FALSE)
    f1_review_delete_mode(FALSE)
    tryCatch({
      records <- f1_find_review_records_by_code(code)
      f1_review_records(records)
      f1_review_status(list(
        type = if (nrow(records)) "success" else "warning",
        message = if (nrow(records)) sprintf("Se encontraron %s casa(s) para el código %s.", nrow(records), code) else sprintf("No hay registros para el código %s con el estado seleccionado.", code),
        details = character()
      ))
    }, error = function(error) {
      f1_review_status(list(type = "error", message = "No se pudo buscar el código de formulario.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f1_review_select_id, {
    intake_id <- f5_integer(input$f1_review_select_id)
    if (is.na(intake_id)) return()
    tryCatch({
      f1_select_review_record(intake_id)
      f1_review_delete_mode(FALSE)
      f1_review_status(list(type = "success", message = sprintf("Registro %s abierto para revisión.", intake_id), details = character()))
    }, error = function(error) {
      f1_review_status(list(type = "error", message = "No se pudo abrir el registro.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f1_review_enable_edit, {
    f1_review_delete_mode(FALSE)
    f1_review_edit_mode(TRUE)
  })
  observeEvent(input$f1_review_cancel_edit, f1_review_edit_mode(FALSE))

  observeEvent(input$f1_review_request_delete, {
    if (is.null(f1_review_selected())) return()
    f1_review_edit_mode(FALSE)
    f1_review_delete_mode(TRUE)
  })

  observeEvent(input$f1_review_delete_cancel, {
    f1_review_delete_mode(FALSE)
    updateTextAreaInput(session, "f1_review_delete_reason", value = "")
  })

  observeEvent(input$f1_review_save_changes, {
    selected <- f1_review_selected()
    if (is.null(selected)) return()
    f1_review_status(list(type = "info", message = "Guardando cambios del Formulario 1...", details = character()))
    validated <- validate_formulario_1(f1_review_input_rows())
    if (length(validated$details)) {
      f1_review_status(list(type = "error", message = "Revise los valores editados.", details = validated$details))
      return()
    }
    intake_id <- as.integer(selected$header$intake_id[[1]])
    tryCatch({
      withProgress(message = "Guardando cambios", value = 0, {
        incProgress(0.25, detail = "Validando datos editados")
        f1_update_review_record(intake_id, validated$data)
        incProgress(0.40, detail = "Recargando el registro")
        f1_select_review_record(intake_id)
        incProgress(0.20, detail = "Actualizando el listado")
        refreshed_records <- tryCatch(f1_load_review_records(), error = function(error) NULL)
        if (!is.null(refreshed_records)) f1_review_records(refreshed_records)
        incProgress(0.15, detail = "Cambios guardados")
      })
      f1_review_edit_mode(FALSE)
      f1_review_delete_mode(FALSE)
      f1_review_status(list(type = "success", message = sprintf("Cambios guardados para intake_id %s. Estado: pending.", intake_id), details = character()))
      showNotification(sprintf("Cambios guardados para intake_id %s.", intake_id), type = "message", duration = 6)
    }, error = function(error) {
      f1_review_status(list(type = "error", message = "No se pudieron guardar los cambios.", details = conditionMessage(error)))
      showNotification("No se pudieron guardar los cambios del Formulario 1.", type = "error", duration = 8)
    })
  })

  observeEvent(input$f1_review_delete_confirm, {
    selected <- f1_review_selected()
    if (is.null(selected)) return()
    reason <- f7_clean_text(input$f1_review_delete_reason)[[1]]
    if (is.na(reason)) {
      f1_review_status(list(
        type = "error",
        message = "No se puede eliminar el registro sin comentario.",
        details = "Ingrese el motivo de eliminación antes de confirmar."
      ))
      return()
    }
    intake_id <- as.integer(selected$header$intake_id[[1]])
    tryCatch({
      deleted <- withProgress(message = "Eliminando registro de Formulario 1", value = 0, {
        incProgress(0.20, detail = "Validando el comentario de eliminación")
        incProgress(0.25, detail = "Borrando registro y sustratos en Supabase")
        removed <- f1_delete_review_record(intake_id, reason, value_or_default(user_profile$name, f5_text(input$f1_reviewed_by)))
        incProgress(0.35, detail = "Actualizando la lista de revisión")
        refreshed_records <- tryCatch(f1_load_review_records(), error = function(error) data.frame())
        f1_review_records(refreshed_records)
        incProgress(0.20, detail = "Eliminación completada")
        removed
      })
      f1_review_selected(NULL)
      f1_review_edit_mode(FALSE)
      f1_review_delete_mode(FALSE)
      f1_review_status(list(
        type = "success",
        message = sprintf("Registro %s eliminado definitivamente.", intake_id),
        details = sprintf("Código de formulario eliminado: %s.", deleted$codigo_formulario[[1]])
      ))
    }, error = function(error) {
      f1_review_status(list(type = "error", message = "No se pudo eliminar el registro.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f1_review_confirm, {
    selected <- f1_review_selected()
    if (is.null(selected)) return()
    intake_id <- as.integer(selected$header$intake_id[[1]])
    connection <- NULL
    f1_review_status(list(type = "info", message = sprintf("Confirmando el registro %s...", intake_id), details = character()))
    tryCatch({
      withProgress(message = "Confirmando registro", value = 0, {
        incProgress(0.20, detail = "Abriendo conexión con Supabase")
        connection <- connect_to_supabase()
        incProgress(0.40, detail = "Guardando la revisión")
        updated <- dbGetQuery(
          connection,
          "update public.formulario_1_ovitrampa_intake set review_status = 'reviewed', review_notes = nullif($1, ''), reviewed_by = nullif($2, ''), reviewed_at = now(), actualizado_en = now() where intake_id = $3 returning intake_id, review_status, reviewed_by, reviewed_at",
          params = list(f5_text(input$f1_review_notes), f5_text(input$f1_reviewed_by), intake_id)
        )
        if (nrow(updated) != 1) stop("No se actualizó el registro seleccionado.")
        incProgress(0.25, detail = "Actualizando el formulario")
        f1_select_review_record(intake_id)
        f1_review_records(f1_load_review_records())
        incProgress(0.15, detail = "Confirmación completada")
      })
      f1_review_status(list(type = "success", message = sprintf("Registro %s confirmado. Estado: reviewed.", intake_id), details = character()))
    }, error = function(error) {
      f1_review_status(list(type = "error", message = "No se pudo confirmar el registro.", details = conditionMessage(error)))
    }, finally = {
      if (!is.null(connection)) dbDisconnect(connection)
    })
  })

  observeEvent(input$process_formulario_1_bulk_upload, {
    req(input$formulario_1_bulk_upload_file)
    formulario_1_bulk_upload_result(NULL)
    connection <- NULL
    tryCatch({
      intake_ids <- withProgress(message = "Subiendo archivo del Formulario 1", value = 0, {
        incProgress(0.15, detail = "Leyendo archivo CSV")
        csv_data <- tryCatch(
          read.csv(
            input$formulario_1_bulk_upload_file$datapath,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            na.strings = c("", "NA"),
            colClasses = "character",
            fileEncoding = "UTF-8-BOM"
          ),
          error = function(error) {
            formulario_1_bulk_upload_result(list(
              type = "error",
              message = "No se pudo leer el CSV de Formulario 1.",
              details = conditionMessage(error)
            ))
            NULL
          }
        )
        if (is.null(csv_data)) return(NULL)

        incProgress(0.25, detail = "Validando columnas, fechas, coordenadas y estados")
        validated <- validate_formulario_1(csv_data)
        if (length(validated$details)) {
          formulario_1_bulk_upload_result(list(
            type = "error",
            message = "El archivo tiene errores de validación. Corrija el CSV y vuelva a subirlo.",
            details = validated$details
          ))
          return(NULL)
        }

        incProgress(0.15, detail = "Conectando con Supabase")
        connection <- connect_to_supabase()
        total_records <- nrow(validated$data)
        insert_formulario_1(
          connection,
          validated$data,
          progress_callback = function(current_record, total_records) {
            incProgress(0.35 / total_records, detail = paste0("Guardando registro ", current_record, " de ", total_records))
          }
        )
      })
      if (is.null(intake_ids)) return()
      formulario_1_bulk_upload_result(list(
        type = "success",
        message = "Archivo subido correctamente.",
        details = paste0(length(intake_ids), " registros guardados en Supabase con estado pending. Intake ID: ", paste(intake_ids, collapse = ", "))
      ))
      submission_status(paste0(length(intake_ids), " registros de Formulario 1 cargados. Estado de revisión: pending."))
      showNotification(paste0("Formulario 1: ", length(intake_ids), " registros guardados."), type = "message", duration = 8)
    }, error = function(error) {
      formulario_1_bulk_upload_result(list(
        type = "error",
        message = "La carga de Formulario 1 a Supabase falló. Ningún registro del documento fue guardado.",
        details = conditionMessage(error)
      ))
      showNotification("La carga de Formulario 1 falló.", type = "error", duration = 10)
    }, finally = {
      if (!is.null(connection)) dbDisconnect(connection)
    })
  })

  output$formulario_5_bulk_upload_status <- renderUI({
    result <- formulario_5_bulk_upload_result()

    if (is.null(result)) {
      return(NULL)
    }

    if (identical(result$type, "success")) {
      return(div(class = "alert alert-success", result$message))
    }

    div(
      class = "alert alert-warning",
      strong(result$message),
      if (length(result$details) > 0) tags$ul(lapply(result$details, tags$li))
    )
  })

  observeEvent(input$process_formulario_5_bulk_upload, {
    req(input$formulario_5_bulk_upload_file)

    formulario_5_bulk_upload_result(NULL)

    csv_data <- tryCatch(
      read.csv(
        input$formulario_5_bulk_upload_file$datapath,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c("", "NA")
      ),
      error = function(error) {
        formulario_5_bulk_upload_result(list(
          type = "error",
          message = "No se pudo leer el archivo CSV de Formulario 5.",
          details = conditionMessage(error)
        ))
        NULL
      }
    )

    if (is.null(csv_data)) {
      return()
    }

    missing_columns <- setdiff(formulario_5_intake_columns, names(csv_data))
    extra_columns <- setdiff(names(csv_data), formulario_5_intake_columns)
    validation_details <- character()

    if (length(missing_columns) > 0) {
      validation_details <- c(
        validation_details,
        paste("Faltan columnas:", paste(missing_columns, collapse = ", "))
      )
    }

    if (length(extra_columns) > 0) {
      validation_details <- c(
        validation_details,
        paste("El archivo contiene columnas no esperadas:", paste(extra_columns, collapse = ", "))
      )
    }

    if (length(validation_details) > 0) {
      formulario_5_bulk_upload_result(list(
        type = "error",
        message = "El archivo no tiene la estructura esperada para Formulario 5.",
        details = validation_details
      ))
      return()
    }

    csv_data <- csv_data[formulario_5_intake_columns]

    if (nrow(csv_data) == 0) {
      formulario_5_bulk_upload_result(list(
        type = "error",
        message = "El archivo no contiene registros para subir.",
        details = character()
      ))
      return()
    }

    clean_text <- function(value) {
      value <- trimws(as.character(value))
      value[value %in% c("", "NA", "NaN")] <- NA_character_
      value
    }

    parse_integer <- function(value) {
      value <- clean_text(value)
      valid <- is.na(value) | grepl("^[+-]?[0-9]+$", value)
      parsed <- rep(NA_integer_, length(value))
      parsed[valid & !is.na(value)] <- suppressWarnings(as.integer(value[valid & !is.na(value)]))
      parsed
    }

    parse_date <- function(value) {
      value <- clean_text(value)
      valid <- is.na(value) | grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)
      parsed <- rep(as.Date(NA), length(value))
      parsed[valid & !is.na(value)] <- suppressWarnings(as.Date(value[valid & !is.na(value)], format = "%Y-%m-%d"))
      parsed
    }

    required_text_columns <- c(
      "formulario_codigo",
      "pais",
      "id_institucion",
      "ciclo",
      "formulario_nombre",
      "cepa_poblacion",
      "especie",
      "generacion_filial_adultos",
      "responsable_ingreso_jaula",
      "responsable_alimentacion",
      "tipo_alimentacion_codigo",
      "generacion_filial_huevos",
      "codigo_sustrato",
      "responsable_conteo_huevos"
    )
    optional_text_columns <- c(
      "tipo_alimentacion_descripcion",
      "observaciones_alimentacion",
      "observaciones_generales",
      "fuente_formulario",
      "creado_por"
    )
    required_integer_columns <- c(
      "numero_hembras",
      "numero_machos",
      "numero_charolas",
      "numero_cuadro_sustrato",
      "hv_huevos_viables",
      "he_huevos_eclosionados",
      "hc_huevos_canoa",
      "hnf_huevos_no_fecundados"
    )
    optional_integer_columns <- c(
      "departamento_numero",
      "municipio_numero",
      "total_huevos_viables"
    )
    date_columns <- c(
      "fecha_registro",
      "fecha_jaula",
      "fecha_alimentacion_sangre",
      "fecha_colocacion_sustrato",
      "fecha_retiro_sustrato"
    )

    for (column in c(required_text_columns, optional_text_columns)) {
      csv_data[[column]] <- clean_text(csv_data[[column]])
    }

    raw_integer_values <- lapply(
      csv_data[c(required_integer_columns, optional_integer_columns)],
      clean_text
    )
    for (column in c(required_integer_columns, optional_integer_columns)) {
      csv_data[[column]] <- parse_integer(csv_data[[column]])
    }

    raw_date_values <- lapply(csv_data[date_columns], clean_text)
    for (column in date_columns) {
      csv_data[[column]] <- parse_date(csv_data[[column]])
    }

    for (column in required_text_columns) {
      bad_rows <- which(is.na(csv_data[[column]]))
      if (length(bad_rows) > 0) {
        validation_details <- c(
          validation_details,
          paste0("La columna ", column, " tiene valores vacíos en filas: ", paste(head(bad_rows, 10), collapse = ", "))
        )
      }
    }

    for (column in required_integer_columns) {
      bad_rows <- which(is.na(csv_data[[column]]))
      if (length(bad_rows) > 0) {
        validation_details <- c(
          validation_details,
          paste0("La columna ", column, " debe contener enteros en filas: ", paste(head(bad_rows, 10), collapse = ", "))
        )
      }
    }

    for (column in optional_integer_columns) {
      bad_rows <- which(!is.na(raw_integer_values[[column]]) & is.na(csv_data[[column]]))
      if (length(bad_rows) > 0) {
        validation_details <- c(
          validation_details,
          paste0("La columna ", column, " debe contener enteros o quedar vacía. Filas: ", paste(head(bad_rows, 10), collapse = ", "))
        )
      }
    }

    for (column in c(required_integer_columns, optional_integer_columns)) {
      bad_rows <- which(!is.na(csv_data[[column]]) & csv_data[[column]] < 0)
      if (length(bad_rows) > 0) {
        validation_details <- c(
          validation_details,
          paste0("La columna ", column, " no puede tener valores negativos. Filas: ", paste(head(bad_rows, 10), collapse = ", "))
        )
      }
    }

    for (column in date_columns) {
      bad_rows <- which(is.na(raw_date_values[[column]]) | is.na(csv_data[[column]]))
      if (length(bad_rows) > 0) {
        validation_details <- c(
          validation_details,
          paste0("La columna ", column, " es obligatoria y debe usar YYYY-MM-DD. Filas: ", paste(head(bad_rows, 10), collapse = ", "))
        )
      }
    }

    allowed_values <- list(
      formulario_codigo = "F5",
      pais = c("El Salvador", "Guatemala"),
      especie = c("Ae. aegypti", "Ae. albopictus"),
      tipo_alimentacion_codigo = c("A", "B", "C", "D", "E"),
      tipo_alimentacion_descripcion = c(
        "conejo",
        "humano",
        "hemotek-conejo",
        "hemotek-humano",
        "hemotek-carnero"
      )
    )

    for (column in names(allowed_values)) {
      bad_rows <- which(!is.na(csv_data[[column]]) & !(csv_data[[column]] %in% allowed_values[[column]]))
      if (length(bad_rows) > 0) {
        validation_details <- c(
          validation_details,
          paste0(
            "La columna ", column, " debe usar: ",
            paste(allowed_values[[column]], collapse = ", "),
            ". Filas: ", paste(head(bad_rows, 10), collapse = ", ")
          )
        )
      }
    }

    bad_substrate_date_rows <- which(
      !is.na(csv_data$fecha_colocacion_sustrato) &
        !is.na(csv_data$fecha_retiro_sustrato) &
        csv_data$fecha_colocacion_sustrato > csv_data$fecha_retiro_sustrato
    )
    if (length(bad_substrate_date_rows) > 0) {
      validation_details <- c(
        validation_details,
        paste0(
          "La fecha de colocación del sustrato no puede ser posterior a la fecha de retiro. Filas: ",
          paste(head(bad_substrate_date_rows, 10), collapse = ", ")
        )
      )
    }

    if (length(validation_details) > 0) {
      formulario_5_bulk_upload_result(list(
        type = "error",
        message = "El archivo tiene errores de validación. Corrija el CSV y vuelva a subirlo.",
        details = validation_details
      ))
      return()
    }

    connection <- NULL
    tryCatch({
      connection <- connect_to_supabase()
      dbWithTransaction(connection, {
        dbAppendTable(
          connection,
          Id(schema = "public", table = "formulario_5_alimentacion_conteo_intake"),
          csv_data
        )
      })

      formulario_5_bulk_upload_result(list(
        type = "success",
        message = paste0(nrow(csv_data), " registros de Formulario 5 guardados como pendientes de revisión."),
        details = character()
      ))
      submission_status(paste0(
        nrow(csv_data),
        " registros de Formulario 5 cargados por subida masiva. Estado de revisión: pending."
      ))
    }, error = function(error) {
      formulario_5_bulk_upload_result(list(
        type = "error",
        message = "La carga de Formulario 5 a Supabase falló.",
        details = conditionMessage(error)
      ))
    }, finally = {
      if (!is.null(connection)) {
        dbDisconnect(connection)
      }
    })
  })

  output$formulario_7_bulk_upload_status <- renderUI({
    result <- formulario_7_bulk_upload_result()
    if (is.null(result)) return(NULL)
    div(
      class = if (identical(result$type, "success")) "alert alert-success" else "alert alert-warning",
      strong(result$message),
      if (length(result$details)) tags$ul(lapply(result$details, tags$li))
    )
  })

  observeEvent(input$process_formulario_7_bulk_upload, {
    req(input$formulario_7_bulk_upload_file)
    formulario_7_bulk_upload_result(NULL)
    connection <- NULL
    tryCatch({
      intake_ids <- withProgress(message = "Subiendo archivo del Formulario 7", value = 0, {
        upload_aborted <- FALSE
        incProgress(0.10, detail = "Leyendo archivo CSV")
        csv_data <- tryCatch(
          read.csv(
            input$formulario_7_bulk_upload_file$datapath,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            na.strings = c("", "NA"),
            colClasses = "character",
            fileEncoding = "UTF-8-BOM"
          ),
          error = function(error) {
            formulario_7_bulk_upload_result(list(
              type = "error",
              message = "No se pudo leer el CSV de Formulario 7.",
              details = conditionMessage(error)
            ))
            NULL
          }
        )
        if (is.null(csv_data)) {
          upload_aborted <- TRUE
        } else {
          incProgress(0.15, detail = "Validando las columnas visibles y sus registros")
          validated <- validate_formulario_7(csv_data)
          if (length(validated$details)) {
            formulario_7_bulk_upload_result(list(
              type = "error",
              message = "El archivo tiene errores de validación. Corrija el CSV y vuelva a subirlo.",
              details = validated$details
            ))
            showNotification(
              "El archivo no fue subido porque contiene errores o códigos repetidos.",
              type = "error",
              duration = 10
            )
            upload_aborted <- TRUE
          }
        }

        if (upload_aborted) {
          NULL
        } else {
          incProgress(0.10, detail = "Conectando con Supabase")
          connection <- connect_to_supabase()
          incProgress(0.10, detail = "Verificando que el código de bioensayo no esté registrado")
          existing_codes <- formulario_7_existing_unique_codes(connection, validated$data$codigo_bioensayo)
          if (length(existing_codes)) {
            formulario_7_bulk_upload_result(list(
              type = "error",
              message = "El archivo no fue subido porque contiene códigos de bioensayo ya registrados.",
              details = paste0("Código de bioensayo repetido: ", existing_codes)
            ))
            showNotification(
              paste0("Carga cancelada: ", length(existing_codes), " códigos de bioensayo ya existen en Supabase."),
              type = "error",
              duration = 12
            )
            NULL
          } else {
            total_records <- nrow(validated$data)
            intake_ids <- insert_formulario_7(
              connection,
              validated$data,
              progress_callback = function(current_record, total_records) {
                incProgress(
                  0.45 / total_records,
                  detail = paste0("Guardando registro ", current_record, " de ", total_records)
                )
              }
            )
            incProgress(0.10, detail = "Confirmando la carga")
            intake_ids
          }
        }
      })
      if (is.null(intake_ids)) return()

      formulario_7_bulk_upload_result(list(
        type = "success",
        message = "Archivo subido correctamente.",
        details = paste0(length(intake_ids), " registros guardados en Supabase con estado pending. Intake ID: ", paste(intake_ids, collapse = ", "))
      ))
      submission_status(paste0(length(intake_ids), " registros de Formulario 7 cargados. Estado de revisión: pending."))
      showNotification(
        paste0("Archivo subido correctamente: ", length(intake_ids), " registros guardados."),
        type = "message",
        duration = 8
      )
    }, error = function(error) {
      error_detail <- conditionMessage(error)
      duplicate_error <- grepl("formulario_7_codigo_bioensayo_unique_idx", error_detail, fixed = TRUE)
      error_message <- if (duplicate_error) {
        "El archivo no fue subido porque Supabase detectó un código de bioensayo repetido. Ningún registro del documento fue guardado."
      } else {
        "La carga de Formulario 7 a Supabase falló. Ningún registro del documento fue guardado."
      }
      formulario_7_bulk_upload_result(list(type = "error", message = error_message, details = error_detail))
      showNotification(error_message, type = "error", duration = 12)
    }, finally = {
      if (!is.null(connection)) dbDisconnect(connection)
    })
  })

  output$bulk_upload_status <- renderUI({
    result <- bulk_upload_result()

    if (is.null(result)) {
      return(NULL)
    }

    if (identical(result$type, "success")) {
      return(div(class = "alert alert-success", result$message))
    }

    div(
      class = "alert alert-warning",
      strong(result$message),
      if (length(result$details) > 0) tags$ul(lapply(result$details, tags$li))
    )
  })

  observeEvent(input$process_bulk_upload, {
    req(input$bulk_upload_file)

    bulk_upload_result(NULL)

    csv_data <- tryCatch(
      read.csv(
        input$bulk_upload_file$datapath,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c("", "NA")
      ),
      error = function(error) {
        bulk_upload_result(list(
          type = "error",
          message = "No se pudo leer el archivo CSV.",
          details = conditionMessage(error)
        ))
        NULL
      }
    )

    if (is.null(csv_data)) {
      return()
    }

    missing_columns <- setdiff(egg_count_intake_columns, names(csv_data))
    extra_columns <- setdiff(names(csv_data), egg_count_intake_columns)
    validation_details <- character()

    if (length(missing_columns) > 0) {
      validation_details <- c(
        validation_details,
        paste("Faltan columnas:", paste(missing_columns, collapse = ", "))
      )
    }

    if (length(extra_columns) > 0) {
      validation_details <- c(
        validation_details,
        paste("El archivo contiene columnas no esperadas:", paste(extra_columns, collapse = ", "))
      )
    }

    if (length(validation_details) > 0) {
      bulk_upload_result(list(
        type = "error",
        message = "El archivo no tiene la estructura esperada.",
        details = validation_details
      ))
      return()
    }

    csv_data <- csv_data[egg_count_intake_columns]

    if (nrow(csv_data) == 0) {
      bulk_upload_result(list(
        type = "error",
        message = "El archivo no contiene registros para subir.",
        details = character()
      ))
      return()
    }

    clean_text <- function(value) {
      value <- trimws(as.character(value))
      value[value %in% c("", "NA", "NaN")] <- NA_character_
      value
    }

    parse_integer <- function(value) {
      suppressWarnings(as.integer(value))
    }

    parse_date <- function(value) {
      value <- clean_text(value)
      suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
    }

    required_text_columns <- c("country", "oviposition_code", "count_responsible_code")
    integer_columns <- c(
      "cycle",
      "round_number",
      "quadrant",
      "intact_eggs",
      "hatched_eggs",
      "canoe_eggs",
      "unfertilized_eggs",
      "other_species_count"
    )
    date_columns <- c("placement_date", "removal_date", "count_date")

    for (column in c("country", "oviposition_code", "substrate_code", "collection_site", "count_responsible_code", "notes")) {
      csv_data[[column]] <- clean_text(csv_data[[column]])
    }

    for (column in integer_columns) {
      csv_data[[column]] <- parse_integer(csv_data[[column]])
    }

    raw_date_values <- lapply(csv_data[date_columns], clean_text)
    for (column in date_columns) {
      csv_data[[column]] <- parse_date(csv_data[[column]])
    }

    for (column in required_text_columns) {
      bad_rows <- which(is.na(csv_data[[column]]))
      if (length(bad_rows) > 0) {
        validation_details <- c(
          validation_details,
          paste0("La columna ", column, " tiene valores vacíos en filas: ", paste(head(bad_rows, 10), collapse = ", "))
        )
      }
    }

    bad_country_rows <- which(!is.na(csv_data$country) & !(csv_data$country %in% country_choices))
    if (length(bad_country_rows) > 0) {
      validation_details <- c(
        validation_details,
        paste0(
          "La columna country debe usar uno de estos valores: ",
          paste(country_choices, collapse = ", "),
          ". Filas: ",
          paste(head(bad_country_rows, 10), collapse = ", ")
        )
      )
    }

    for (column in integer_columns) {
      bad_rows <- which(is.na(csv_data[[column]]))
      if (length(bad_rows) > 0) {
        validation_details <- c(
          validation_details,
          paste0("La columna ", column, " debe contener números enteros en filas: ", paste(head(bad_rows, 10), collapse = ", "))
        )
      }
    }

    count_columns <- c("intact_eggs", "hatched_eggs", "canoe_eggs", "unfertilized_eggs", "other_species_count")
    for (column in count_columns) {
      bad_rows <- which(!is.na(csv_data[[column]]) & csv_data[[column]] < 0)
      if (length(bad_rows) > 0) {
        validation_details <- c(
          validation_details,
          paste0("La columna ", column, " no puede tener valores negativos. Filas: ", paste(head(bad_rows, 10), collapse = ", "))
        )
      }
    }

    bad_round_rows <- which(!is.na(csv_data$round_number) & !(csv_data$round_number %in% 1:4))
    if (length(bad_round_rows) > 0) {
      validation_details <- c(
        validation_details,
        paste0("La ronda debe estar entre 1 y 4. Filas: ", paste(head(bad_round_rows, 10), collapse = ", "))
      )
    }

    bad_count_date_rows <- which(is.na(csv_data$count_date))
    if (length(bad_count_date_rows) > 0) {
      validation_details <- c(
        validation_details,
        paste0("La fecha de conteo es obligatoria y debe usar YYYY-MM-DD. Filas: ", paste(head(bad_count_date_rows, 10), collapse = ", "))
      )
    }

    for (column in c("placement_date", "removal_date")) {
      bad_rows <- which(!is.na(raw_date_values[[column]]) & is.na(csv_data[[column]]))
      if (length(bad_rows) > 0) {
        validation_details <- c(
          validation_details,
          paste0("La columna ", column, " debe usar formato YYYY-MM-DD. Filas: ", paste(head(bad_rows, 10), collapse = ", "))
        )
      }
    }

    bad_date_order_rows <- which(
      !is.na(csv_data$placement_date) &
        !is.na(csv_data$removal_date) &
        csv_data$placement_date > csv_data$removal_date
    )
    if (length(bad_date_order_rows) > 0) {
      validation_details <- c(
        validation_details,
        paste0("La fecha de colocación no puede ser posterior a la fecha de retiro. Filas: ", paste(head(bad_date_order_rows, 10), collapse = ", "))
      )
    }

    if (length(validation_details) > 0) {
      bulk_upload_result(list(
        type = "error",
        message = "El archivo tiene errores de validación. Corrija el CSV y vuelva a subirlo.",
        details = validation_details
      ))
      return()
    }

    csv_data$submitted_by <- Sys.getenv("PROJECT_REI_SUBMITTED_BY", unset = value_or_default(app_user, "local-prototype-user"))

    connection <- NULL
    tryCatch({
      connection <- connect_to_supabase()
      dbWithTransaction(connection, {
        dbAppendTable(
          connection,
          Id(schema = "rei", table = "egg_count_intake"),
          csv_data
        )
      })

      bulk_upload_result(list(
        type = "success",
        message = paste0(nrow(csv_data), " registros guardados como pendientes de revisión."),
        details = character()
      ))
      submission_status(paste0(nrow(csv_data), " registros cargados por subida masiva. Estado de revisión: pending."))
    }, error = function(error) {
      bulk_upload_result(list(
        type = "error",
        message = "La carga a Supabase falló.",
        details = conditionMessage(error)
      ))
    }, finally = {
      if (!is.null(connection)) {
        dbDisconnect(connection)
      }
    })
  })

  output$download_request_data_csv <- downloadHandler(
    filename = function() {
      dataset <- value_or_default(input$request_download_dataset, "datos")
      date_stamp <- format(Sys.Date(), "%Y%m%d")
      scope_country <- request_filter_country()
      scope_institution <- request_filter_institution()
      scope <- paste(
        if (is.null(scope_country)) "todos_paises" else gsub("[^A-Za-z0-9]+", "_", tolower(scope_country)),
        if (is.null(scope_institution)) "todas_instituciones" else gsub("[^A-Za-z0-9]+", "_", tolower(scope_institution)),
        sep = "_"
      )
      dataset_name <- switch(
        dataset,
        formulario_1 = "formulario_1_campo",
        formulario_5 = "formulario_5_insectario",
        formulario_7 = "formulario_7_insectario",
        "datos_entonet"
      )
      paste0(dataset_name, "_", scope, "_", date_stamp, ".csv")
    },
    content = function(file) {
      subdivision <- active_request_data_subdivision()
      allowed_subdivisions <- request_allowed_data_subdivisions()
      if (is.null(subdivision) || !(subdivision %in% allowed_subdivisions)) {
        stop("Su perfil no tiene permiso para descargar datos de esta área.")
      }

      dataset <- value_or_default(input$request_download_dataset, "")
      allowed_datasets <- unname(request_data_dataset_choices(subdivision))
      if (!nzchar(dataset) || !(dataset %in% allowed_datasets)) {
        stop("Seleccione un conjunto de datos permitido para su perfil.")
      }

      connection <- NULL
      data <- data.frame()
      tryCatch({
        connection <- connect_to_supabase()
        data <- switch(
          dataset,
          formulario_1 = request_fetch_formulario_1(connection),
          formulario_5 = request_fetch_formulario_5(connection),
          formulario_7 = request_fetch_formulario_7(connection),
          data.frame()
        )
      }, finally = {
        if (!is.null(connection)) {
          dbDisconnect(connection)
        }
      })

      write.csv(data, file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  observeEvent(input$submit, {
    req(active_dataset() == "egg_count_raw")

    errors <- validation_errors()
    if (length(errors) > 0) {
      submission_status("El envío fue bloqueado. Resuelva los mensajes de validación anteriores.")
      return()
    }

    record <- form_data()
    connection <- NULL

    tryCatch({
      connection <- connect_to_supabase()
      submitted_by <- Sys.getenv("PROJECT_REI_SUBMITTED_BY", unset = "local-prototype-user")

      result <- dbGetQuery(
        connection,
        "
          insert into rei.egg_count_intake (
            country,
            cycle,
            round_number,
            quadrant,
            oviposition_code,
            substrate_code,
            collection_site,
            placement_date,
            removal_date,
            count_date,
            count_responsible_code,
            intact_eggs,
            hatched_eggs,
            canoe_eggs,
            unfertilized_eggs,
            other_species_count,
            notes,
            submitted_by
          )
          values (
            $1, $2, $3, $4, $5, nullif($6, ''), nullif($7, ''),
            $8, $9, $10, $11, $12, $13, $14, $15, $16,
            nullif($17, ''), $18
          )
          returning intake_id, calculated_total, review_status, submitted_at
        ",
        params = list(
          record$country,
          as.integer(record$cycle),
          as.integer(record$round_number),
          as.integer(record$quadrant),
          record$oviposition_code,
          record$substrate_code,
          record$collection_site,
          as.character(record$placement_date),
          as.character(record$removal_date),
          as.character(record$count_date),
          record$count_responsible_code,
          as.integer(record$intact_eggs),
          as.integer(record$hatched_eggs),
          as.integer(record$canoe_eggs),
          as.integer(record$unfertilized_eggs),
          as.integer(record$other_species_count),
          record$notes,
          submitted_by
        )
      )

      submission_status(sprintf(
        "Registro de ingreso %s guardado con total %s. Estado de revisión: %s.",
        result$intake_id,
        result$calculated_total,
        result$review_status
      ))
      reset_form()
    }, error = function(error) {
      submission_status(paste("El envío falló:", conditionMessage(error)))
    }, finally = {
      if (!is.null(connection)) {
        dbDisconnect(connection)
      }
    })
  })
}

shinyApp(ui = ui, server = server)
