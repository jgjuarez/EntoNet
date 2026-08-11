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
formulario_7_el_salvador_locations <- data.frame(
  codigo_departamento_num = c(2L, 3L, 5L, 6L),
  codigo_municipio_num = c(201L, 301L, 501L, 601L),
  departamento = c("Santa Ana", "Sonsonate", "La Libertad", "San Salvador"),
  municipio = c("Santa Ana", "Sonsonate", "La Libertad", "San Salvador"),
  latitude = c(13.9942, 13.7189, 13.4883, 13.6929),
  longitude = c(-89.5597, -89.7240, -89.3222, -89.2182),
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
  value <- toupper(trimws(as.character(value_or_default(value, ""))))
  chartr("ÁÉÍÓÚÜÑ", "AEIOUUN", value)
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
    dosis_diagnostica_1x = FALSE,
    modalidad_bioensayo = NA_character_,
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
    clean_part(codigo_bioensayo), as_flag(dosis_diagnostica_1x), clean_part(modalidad_bioensayo),
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
  already_final <- !is.na(bioensayo) & (
    grepl("REI[0-9]{2}[A-Z]{2}[0-9]{4}(DEL|PER|MAL|DDT)[0-9]+(\\.[0-9]+)?(DD|IE|IC(2X|5X|10X)|S(DEF|PBO|DM))$", bioensayo) |
      grepl("REI[0-9]{2}[A-Z]{2}[0-9]{4}BIO[0-9]+(DD|IE|IC(2X|5X|10X)|S(DEF|PBO|DM))$", bioensayo) |
      grepl("-(D|I(-[0-9]+X)+|I-1X-2X-5X-10X|S(-[A-Z]+)+)$", bioensayo)
  )

  suffix <- rep(NA_character_, size)
  suffix[diagnostica] <- "D"
  exploratorio <- !diagnostica & modalidad == "Exploratorio"
  suffix[exploratorio] <- "I-1X-2X-5X-10X"
  completa <- !diagnostica & modalidad == "Completa" & !is.na(intensidad) & nzchar(intensidad)
  suffix[completa] <- paste("I", intensidad[completa], sep = "-")
  for (index in seq_len(size)) {
    selected <- c("DEF"[def[[index]]], "PBO"[pbo[[index]]], "DM"[dm[[index]]])
    selected <- selected[!is.na(selected)]
    if (!diagnostica[[index]] && length(selected)) suffix[[index]] <- paste(c("S", selected), collapse = "-")
  }

  result <- paste(bioensayo, suffix, sep = "-")
  result[already_final] <- bioensayo[already_final]
  result[is.na(bioensayo) | !nzchar(bioensayo) | is.na(suffix)] <- NA_character_
  result[already_final & !is.na(bioensayo) & nzchar(bioensayo)] <- bioensayo[already_final & !is.na(bioensayo) & nzchar(bioensayo)]
  result
}

formulario_7_header_columns <- c(
  "formulario_codigo", "formulario_nombre", "fecha_registro", "codigo_bioensayo", "nombre_poblacion",
  "pais", "id_institucion", "codigo_departamento", "codigo_municipio", "modalidad_bioensayo",
  "dosis_diagnostica_1x", "dosis_intensidad", "sinergista_def", "sinergista_pbo", "sinergista_dm",
  "resultado_diagnostico", "fecha_realizacion_bioensayo", "codigo_insecticida", "solvente_utilizado",
  "solvente_otro", "dosis_ug_ml", "codigo_dosis", "fecha_revestimiento_botellas",
  "numero_usos_botella_e1", "numero_usos_botella_e2", "numero_usos_botella_e3",
  "numero_usos_botella_e4", "numero_usos_botella_c1", "origen_material",
  "edad_dias", "edad_indefinida", "codigo_especie_mosquito",
  "fecha_separacion", "hora_separacion", "generacion_filial", "generacion_filial_indefinida",
  "codigo_responsable_revestimiento", "codigo_responsable_bioensayo", "codigo_control_calidad",
  "codigo_revision_24h", "temperatura_inicial_c", "temperatura_final_c",
  "humedad_relativa_inicial_pct", "humedad_relativa_final_pct", "hora_inicio_bioensayo",
  "hora_final_bioensayo", "fuente_formulario", "creado_por"
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
formulario_7_comment_columns <- c("comentario", "comentario_nombre")
formulario_7_intake_columns <- c(
  setdiff(formulario_7_header_columns, c("fuente_formulario", "creado_por")),
  formulario_7_result_columns,
  formulario_7_comment_columns,
  "fuente_formulario", "creado_por", "creado_en", "actualizado_en"
)
formulario_7_csv_columns <- formulario_7_intake_columns

formulario_7_template <- as.data.frame(
  setNames(rep(list(""), length(formulario_7_intake_columns)), formulario_7_intake_columns),
  stringsAsFactors = FALSE
)
formulario_7_template$formulario_codigo <- "F7"
formulario_7_template$formulario_nombre <- "Registro de datos del bioensayo de la botella CDC"
formulario_7_template$fecha_registro <- as.character(Sys.Date())
formulario_7_template$id_institucion <- default_institution_id
formulario_7_template$fuente_formulario <- "Formulario 7_Bioensayo .docx"

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

  dbGetQuery(
    connection,
    "select usuario, user_id::text, email, id_institucion, rol, pais, nombre, activo
     from public.usuario_perfil
     where lower(usuario) = any($1) or lower(email) = any($1)
     limit 1",
    params = list(candidates)
  )
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
      actionButton("landing_network_impact", tr(language, "Impacto de la Red", "Network Impact"), class = tab_class("network_impact"))
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

  tagList(
    textInput(
      paste0("f7_resultado_hora_inicio_", bottle),
      "Hora de inicio de la botella (HH:MM)",
      placeholder = "08:30"
    ),
    conditionalPanel(
      "input.f7_tipo_bioensayo != 'sinergistas'",
      reading_rows,
      tagList(
        tags$hr(),
        h5("Lectura KDR a 24 horas"),
        textInput(paste0("f7_resultado_hora_lectura_24h_", bottle), "Hora de lectura (HH:MM)", placeholder = "08:30"),
        formulario_7_count_pair(paste0("resultado_24h_", bottle), "24 horas")
      )
    ),
    conditionalPanel(
      "input.f7_tipo_bioensayo == 'sinergistas'",
      div(
        class = "alert alert-info",
        "Ensayo con sinergistas: registre únicamente la lectura a los 60 minutos. No aplica lectura a 24 horas."
      ),
      formulario_7_count_pair(paste0("resultado_60min_", bottle), "60 minutos")
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
                  "Para registros nuevos, genere este código antes de ingresar datos desde la sección Imprimir formulario del Formulario 7. La estructura nueva es REI + año + país + código municipio + insecticida (DEL, PER, MAL, DDT) + # población + tipo. Para datos antiguos puede ingresar el código histórico tal como aparece en el formulario."
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
              "input.f7_tipo_bioensayo == 'diagnostica_1x'",
              radioButtons(
                "f7_resultado_diagnostico",
                "Resultado de la prueba diagnóstica *",
                choices = c("Suceptible", "Sospecha de Resistencia", "Resistente"),
                selected = character(0)
              )
            ),
            conditionalPanel(
              "input.f7_tipo_bioensayo == 'intensidad'",
              radioButtons("f7_modalidad_bioensayo", "Intensidad *", choices = c("Exploratorio", "Completa"), inline = TRUE),
              conditionalPanel(
                "input.f7_modalidad_bioensayo == 'Exploratorio'",
                div(class = "alert alert-info", strong("Dosis incluidas: "), "1X, 2X, 5X y 10X, más su control.")
              ),
              conditionalPanel(
                "input.f7_modalidad_bioensayo == 'Completa'",
                selectInput("f7_dosis_intensidad", "Dosis de intensidad *", choices = c("1X", "2X", "5X", "10X"))
              )
            ),
            conditionalPanel(
              "input.f7_tipo_bioensayo == 'sinergistas'",
              checkboxGroupInput("f7_sinergistas", "Sinergistas *", choices = c("DEF" = "def", "PBO" = "pbo", "DM" = "dm"), inline = TRUE)
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
              "f7_codigo_insecticida",
              "Insecticida *",
              choices = c("Seleccione" = "", "Deltametrina" = "Deltametrina", "Permetrina" = "Permetrina", "Malation" = "Malation", "DDT" = "DDT")
            ),
            selectInput("f7_solvente_utilizado", "Solvente utilizado *", choices = c("Etanol", "Otro")),
            conditionalPanel("input.f7_solvente_utilizado == 'Otro'", textInput("f7_solvente_otro", "Especifique el solvente *")),
            numericInput("f7_dosis_ug_ml", "Dosis (µg/ml) *", value = NA, min = 0),
            div(
              class = "f7-help-field",
              div(
                class = "f7-help-label-row",
                tags$label(`for` = "f7_codigo_dosis", "Código de dosis *"),
                actionButton(
                  "f7_codigo_dosis_help",
                  label = "?",
                  class = "f7-help-button",
                  title = "Ayuda sobre el código de dosis",
                  `aria-label` = "Mostrar ayuda sobre el código de dosis"
                )
              ),
              textInput("f7_codigo_dosis", label = NULL),
              conditionalPanel(
                "input.f7_codigo_dosis_help % 2 == 1",
                div(
                  class = "f7-help-message",
                  "Este hace referencia al código asignado a la dosis del insecticida que se está evaluando, el cual proporciona información de la fecha de preparación, quién la preparó, fecha de expiración, dosis, tipo de insecticida, lote y marca."
                )
              )
            ),
            dateInput("f7_fecha_revestimiento_botellas", "Fecha de revestimiento *", value = Sys.Date())
          ),
          column(6,
            tags$hr(),
            h4("# Veces se han utilizado"),
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
            textInput("f7_codigo_control_calidad", "Código de control de calidad *"),
            conditionalPanel(
              "input.f7_tipo_bioensayo != 'sinergistas'",
              textInput("f7_codigo_revision_24h", "Código de revisión a 24 h *")
            )
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
            "f7_print_codigo_insecticida",
            "Insecticida",
            choices = c("Seleccione" = "", "Deltametrina" = "Deltametrina", "Permetrina" = "Permetrina", "Malation" = "Malation", "DDT" = "DDT")
          )
        ),
        column(
          6,
          textInput("f7_print_codigo_bioensayo_poblacion_numero", "# Población", placeholder = "Ej. 2 o 2.1"),
          numericInput("f7_print_codigo_bioensayo_anio", "Año", value = as.integer(format(Sys.Date(), "%y")), min = 0, max = 99, step = 1),
          textInput("f7_print_version_formulario", "Versión del formulario"),
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
        column(3, numericInput("f7_review_search_id", "Buscar intake_id", value = NA, min = 1, step = 1)),
        column(3, dateInput("f7_review_start_date", "Fecha inicio", value = Sys.Date() - 30)),
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
      div(
        class = "submit-row",
        actionButton("f7_review_find", "Buscar ID", class = "btn-default"),
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
        grid-template-columns: repeat(4, minmax(0, 1fr));
        margin-top: 18px;
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
  active_dataset <- reactiveVal(NULL)
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
  f1_review_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  f7_save_status <- reactiveVal(list(type = "idle", message = NULL, details = character()))
  formulario_7_bulk_upload_result <- reactiveVal(NULL)
  f7_review_records <- reactiveVal(data.frame())
  f7_review_selected <- reactiveVal(NULL)
  f7_review_edit_mode <- reactiveVal(FALSE)
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

  f7_insecticide_code <- function(value) {
    cleaned <- toupper(trimws(value_or_default(value, "")))
    cleaned <- chartr("ÁÉÍÓÚÜÑ", "AEIOUUN", cleaned)
    if (cleaned %in% c("DEL", "DELTAMETRINA")) return("DEL")
    if (cleaned %in% c("PER", "PERMETRINA")) return("PER")
    if (cleaned %in% c("MAL", "MALATION", "MALATHION")) return("MAL")
    if (cleaned %in% c("DDT")) return("DDT")
    NA_character_
  }

  f7_population_code <- function(value) {
    cleaned <- gsub("\\s+", "", value_or_default(value, ""))
    cleaned <- gsub(",", ".", cleaned, fixed = TRUE)
    if (!grepl("^[0-9]+(\\.[0-9]+)?$", cleaned)) return(NA_character_)
    cleaned
  }

  f7_print_codigo_bioensayo_code <- function() {
    country_code <- f7_print_country_acronym(input$f7_print_pais)
    population_code <- f7_population_code(input$f7_print_codigo_bioensayo_poblacion_numero)
    municipality <- f7_print_selected_municipality_code()
    insecticide <- f7_insecticide_code(input$f7_print_codigo_insecticida)
    year <- f5_integer(input$f7_print_codigo_bioensayo_anio)
    suffix <- f7_print_bioassay_type_suffix()
    if (is.na(country_code) || is.na(population_code) || !nzchar(municipality) || is.na(insecticide) || is.na(year) || is.na(suffix)) {
      return(NA_character_)
    }
    paste0("REI", sprintf("%02d", year %% 100L), country_code, municipality, insecticide, population_code, suffix)
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
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    total <- dbGetQuery(
      connection,
      "select count(*)::integer as total from public.formulario_1_ovitrampa_intake where fecha_registro between $1 and $2 and ($3 = 'all' or review_status = $3)",
      params = list(as.character(start_date), as.character(end_date), status)
    )$total[[1]]
    if (total == 0) return(data.frame())
    limit <- if (random_sample) max(1L, ceiling(as.integer(total) * 0.10)) else min(as.integer(total), 50L)
    order_clause <- if (random_sample) "order by random()" else "order by creado_en desc nulls last, intake_id desc"
    query <- paste(
      "select intake_id, codigo_formulario, fecha_registro, pais, cuadrante, codigo_casa, ovitrampas_colocadas, ovitrampas_retiradas, review_status, actualizado_en from public.formulario_1_ovitrampa_intake where fecha_registro between $1 and $2 and ($3 = 'all' or review_status = $3)",
      order_clause,
      "limit $4"
    )
    dbGetQuery(connection, query, params = list(as.character(start_date), as.character(end_date), status, as.integer(limit)))
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
    add_row(11, c("Código insecticida", "", "", "", "", "", "", "Edad", "", "", "", "Indefinida", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(12, c("Solvente utilizado", "", "Etanol", "", "Otro:", "", "", "Código especie mosquito", "", "", "", "", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(13, c("Dosis", "", "", "", "", "ug/mL", "", "Hora separación (hh:mm)", "", "", "h", "", "m", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(14, c("Código de dosis", "", "", "", "", "", "", "Generación filial", "", "", "", "Indefinida", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(15, c("Fecha revestimiento (dd/mm/aa)", "", "", "", "", "", "", "Fecha separación (dd/mm/aa)", "", "", "", "", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(16, c("# Veces se han utilizado", "", "E1__", "", "E2__", "", "E3__", "", "E4__", "", "C1__", "", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L), 21)

    add_row(18, c("5. RESPONSABLES", rep("", 13)), c(15L, rep(15L, 13)), 18)
    add_row(19, c("Código quien realizó revestimiento", "", "", "", "", "", "", "Código control de calidad", "", "", "", "", "", ""), c(16L, 16L, 17L, 17L, 17L, 17L, 17L, 16L, 16L, 17L, 17L, 17L, 17L, 17L), 21)
    add_row(
      20,
      c("Código quien realiza bioensayo", "", "", "", "", "", "", if (is_synergist_print) "" else "Código revisión 24h", "", "", "", "", "", ""),
      c(16L, 16L, 17L, 17L, 17L, 17L, 17L, if (is_synergist_print) 17L else 16L, 16L, 17L, 17L, 17L, 17L, 17L),
      21
    )

    add_row(22, c("6. CONDICIONES AMBIENTALES DEL BIOENSAYO", rep("", 6), "7. HORARIO DEL BIOENSAYO", rep("", 6)), c(rep(15L, 7), rep(15L, 7)), 18)
    add_row(23, c("Temp. inicial", "", "", "Temp. final", "", "", "", "Hora inicial (hh:mm)", "", "", "Hora final (hh:mm)", "", "", ""), c(16L, 17L, 17L, 16L, 17L, 17L, 17L, 16L, 17L, 17L, 16L, 17L, 17L, 17L), 21)
    add_row(24, c("HR inicial", "", "", "HR final", "", "", "", "", "", "", "", "", "", ""), c(16L, 17L, 17L, 16L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L, 17L), 21)

    add_row(25, c("8. LECTURAS POR BOTELLA", rep("", 13)), c(15L, rep(15L, 13)), 18)
    add_row(
      26,
      if (is_synergist_print) {
        c("BOTELLA", "INICIO (hh:mm)", "60 V", "60 I", "OBS.", "", "", "", "", "", "", "", "", "")
      } else {
        c("BOTELLA", "INICIO (hh:mm)", "0 V", "0 I", "15 V", "15 I", "30 V", "30 I", "45 V", "45 I", "24H HORA (hh:mm)", "24H V", "24H I", "OBS.")
      },
      rep(16L, 14),
      24
    )
    bottle_labels <- c("E1", "E2", "E3", "E4", "C1")
    for (index in seq_along(bottle_labels)) {
      add_row(26L + index, c(bottle_labels[[index]], rep("", 13)), rep(17L, 14), 19)
    }

    add_row(32, c("COMENTARIO", rep("", 13)), c(15L, rep(15L, 13)), 18)
    add_row(33, c("", rep("", 13)), rep(17L, 14), 34)

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
      '<definedName name="_xlnm.Print_Area" localSheetId="0">\'Formulario 7\'!$A$1:$N$33</definedName>',
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
        "Complete país, año, municipio, insecticida, # población y tipo de bioensayo para generar el código."
      ))
    }
    div(
      class = "summary-box",
      strong("Código Bioensayo generado: "),
      tags$code(codigo),
      tags$br(),
      tags$small("Estructura: REI + año + país + código municipio + insecticida (DEL/PER/MAL/DDT) + # población + tipo.")
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

    connection <- NULL
    on.exit({
      if (!is.null(connection)) {
        dbDisconnect(connection)
      }
    }, add = TRUE)

    connection <- connect_to_supabase()
    count_query <- "
      select count(*)::integer as total
      from public.formulario_5_alimentacion_conteo_intake
      where fecha_registro between $1 and $2
        and ($3 = 'all' or review_status = $3)
    "
    total <- dbGetQuery(
      connection,
      count_query,
      params = list(as.character(start_date), as.character(end_date), status_filter)
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
          actualizado_en
        from public.formulario_5_alimentacion_conteo_intake
        where fecha_registro between $1 and $2
          and ($3 = 'all' or review_status = $3)
      ",
      order_clause,
      "limit $4"
    )

    dbGetQuery(
      connection,
      query,
      params = list(as.character(start_date), as.character(end_date), status_filter, as.integer(limit))
    )
  }

  output$f5_review_status_message <- renderUI({
    status <- f5_review_status()

    if (is.null(status$type) || identical(status$type, "idle")) {
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
        tags$td(record$fuente_formulario)
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
          tags$th("Fuente formulario")
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
    }, error = function(error) {
      f5_review_status(list(
        type = "error",
        message = "No se pudo guardar la revisión.",
        details = conditionMessage(error)
      ))
    }, finally = {
      if (!is.null(connection)) {
        dbDisconnect(connection)
      }
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
    missing_columns <- setdiff(formulario_7_intake_columns, names(csv_data))
    extra_columns <- setdiff(names(csv_data), formulario_7_intake_columns)
    if (length(missing_columns) > 0) details <- c(details, paste("Faltan columnas:", paste(missing_columns, collapse = ", ")))
    if (length(extra_columns) > 0) details <- c(details, paste("Columnas no esperadas:", paste(extra_columns, collapse = ", ")))
    if (length(details) > 0) return(list(data = NULL, details = details))

    data <- csv_data[formulario_7_intake_columns]
    if (nrow(data) == 0) return(list(data = NULL, details = "El archivo no contiene registros."))
    for (column in names(data)) data[[column]] <- f7_clean_text(data[[column]])

    required_text <- c(
      "formulario_codigo", "formulario_nombre", "nombre_poblacion", "codigo_bioensayo",
      "codigo_insecticida", "solvente_utilizado", "codigo_dosis",
      "origen_material", "pais", "id_institucion", "codigo_departamento", "codigo_municipio", "codigo_especie_mosquito",
      "codigo_responsable_revestimiento", "codigo_responsable_bioensayo", "codigo_control_calidad"
    )
    required_dates <- c("fecha_registro", "fecha_realizacion_bioensayo", "fecha_revestimiento_botellas", "fecha_separacion")
    required_times <- c("hora_separacion", "hora_inicio_bioensayo", "hora_final_bioensayo")
    boolean_columns <- c(
      "dosis_diagnostica_1x", "sinergista_def", "sinergista_pbo", "sinergista_dm",
      "edad_indefinida", "generacion_filial_indefinida"
    )
    numeric_columns <- c(
      "dosis_ug_ml", paste0("numero_usos_botella_", c("e1", "e2", "e3", "e4", "c1")), "edad_dias",
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
    missing_review_24h <- which(!has_synergist_rows & is.na(data$codigo_revision_24h))
    if (length(missing_review_24h)) {
      details <- c(details, paste0("codigo_revision_24h es obligatorio excepto para sinergistas. Filas: ", paste(head(missing_review_24h, 10), collapse = ", ")))
    }
    data$codigo_revision_24h[has_synergist_rows] <- NA_character_
    data$codigo_bioensayo <- formulario_7_codigo_bioensayo_final(
      data$codigo_bioensayo, data$dosis_diagnostica_1x, data$modalidad_bioensayo, data$dosis_intensidad,
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

    required_numeric <- c("dosis_ug_ml", "temperatura_inicial_c", "temperatura_final_c", "humedad_relativa_inicial_pct", "humedad_relativa_final_pct")
    for (column in required_numeric) {
      bad <- which(is.na(data[[column]]))
      if (length(bad)) details <- c(details, paste0(column, " es obligatorio. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    for (column in c("humedad_relativa_inicial_pct", "humedad_relativa_final_pct")) {
      bad <- which(!is.na(data[[column]]) & data[[column]] > 100)
      if (length(bad)) details <- c(details, paste0(column, " no puede ser mayor que 100. Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    allowed <- list(
      formulario_codigo = "F7", modalidad_bioensayo = c("Exploratorio", "Completa"), solvente_utilizado = c("Etanol", "Otro"),
      origen_material = c("Silvestre", "Laboratorio"), pais = c("El Salvador", "Guatemala"),
      dosis_intensidad = c("1X", "2X", "5X", "10X"),
      resultado_diagnostico = c("Suceptible", "Sospecha de Resistencia", "Resistente")
    )
    for (column in names(allowed)) {
      bad <- which(!is.na(data[[column]]) & !(data[[column]] %in% allowed[[column]]))
      if (length(bad)) details <- c(details, paste0(column, " debe usar: ", paste(allowed[[column]], collapse = ", "), ". Filas: ", paste(head(bad, 10), collapse = ", ")))
    }
    duplicate_codes <- unique(data$codigo_bioensayo[duplicated(data$codigo_bioensayo) & !is.na(data$codigo_bioensayo)])
    if (length(duplicate_codes)) details <- c(details, paste0("Código de bioensayo duplicado dentro del archivo: ", paste(duplicate_codes, collapse = ", "), "."))

    for (row in seq_len(nrow(data))) {
      has_synergist <- any(c(data$sinergista_def[[row]], data$sinergista_pbo[[row]], data$sinergista_dm[[row]]), na.rm = TRUE)
      is_diagnostic <- isTRUE(data$dosis_diagnostica_1x[[row]])
      is_intensity <- !is.na(data$modalidad_bioensayo[[row]])
      selected_types <- sum(is_diagnostic, is_intensity, has_synergist)
      if (selected_types != 1) details <- c(details, paste("Fila", row, ": seleccione exactamente un Tipo de Bioensayo: Diagnóstica 1X, Intensidad o Sinergistas."))
      if (is_diagnostic) {
        if (is.na(data$resultado_diagnostico[[row]])) details <- c(details, paste("Fila", row, ": indique el resultado de la prueba diagnóstica."))
        if (!is.na(data$dosis_intensidad[[row]]) || has_synergist || is_intensity) details <- c(details, paste("Fila", row, ": Diagnóstica 1X no admite modalidad de intensidad, dosis de intensidad ni sinergistas."))
      } else {
        if (!is.na(data$resultado_diagnostico[[row]])) details <- c(details, paste("Fila", row, ": resultado_diagnostico solo corresponde a Diagnóstica 1X."))
      }
      if (is_intensity) {
        if (identical(data$modalidad_bioensayo[[row]], "Exploratorio") && !is.na(data$dosis_intensidad[[row]])) details <- c(details, paste("Fila", row, ": Intensidad Exploratorio usa las botellas 1X, 2X, 5X y 10X y no selecciona una dosis única."))
        if (identical(data$modalidad_bioensayo[[row]], "Completa") && is.na(data$dosis_intensidad[[row]])) details <- c(details, paste("Fila", row, ": Intensidad Completa requiere dosis_intensidad 1X, 2X, 5X o 10X."))
        if (is_diagnostic || has_synergist) details <- c(details, paste("Fila", row, ": Intensidad no puede combinarse con Diagnóstica 1X o Sinergistas."))
      } else if (!is.na(data$dosis_intensidad[[row]])) {
        details <- c(details, paste("Fila", row, ": dosis_intensidad solo corresponde al Tipo de Bioensayo Intensidad."))
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
    for (bottle in formulario_7_bottles) {
      if (has_synergist) {
        base <- paste0("resultado_60min_", bottle)
        if (!is.na(row[[paste0(base, "_vivos")]])) results[[length(results) + 1]] <- data.frame(
          fase = "bioensayo", botella = bottle, tiempo_minutos = 60,
          hora_lectura = row[[paste0("resultado_hora_inicio_", bottle)]],
          vivos = row[[paste0(base, "_vivos")]], incapacitados = row[[paste0(base, "_incapacitados")]], stringsAsFactors = FALSE
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
    "dosis_diagnostica_1x", "sinergista_def", "sinergista_pbo", "sinergista_dm",
    "edad_indefinida", "generacion_filial_indefinida"
  )
  f7_review_date_fields <- c(
    "fecha_registro", "fecha_realizacion_bioensayo", "fecha_revestimiento_botellas", "fecha_separacion"
  )
  f7_review_numeric_fields <- c(
    "dosis_ug_ml", paste0("numero_usos_botella_", c("e1", "e2", "e3", "e4", "c1")), "edad_dias",
    "temperatura_inicial_c", "temperatura_final_c", "humedad_relativa_inicial_pct", "humedad_relativa_final_pct",
    grep("_(vivos|incapacitados)$", formulario_7_intake_columns, value = TRUE)
  )
  f7_review_protected_fields <- c("creado_en", "actualizado_en")

  f7_review_field_label <- function(field) {
    special <- c(
      formulario_codigo = "Código del formulario", formulario_nombre = "Nombre del formulario",
      codigo_bioensayo = "Código de bioensayo",
      dosis_diagnostica_1x = "Diagnóstica 1X", dosis_intensidad = "Dosis de intensidad",
      sinergista_def = "Sinergista DEF", sinergista_pbo = "Sinergista PBO", sinergista_dm = "Sinergista DM",
      resultado_diagnostico = "Resultado diagnóstico", dosis_ug_ml = "Dosis (µg/mL)",
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
    if (field %in% c("fuente_formulario", "creado_por", "creado_en", "actualizado_en")) return("Comentarios y auditoría")
    if (field %in% c(
      "formulario_codigo", "formulario_nombre", "fecha_registro", "codigo_bioensayo", "nombre_poblacion",
      "modalidad_bioensayo", "dosis_diagnostica_1x", "dosis_intensidad", "sinergista_def", "sinergista_pbo",
      "sinergista_dm", "resultado_diagnostico", "pais", "codigo_departamento", "codigo_municipio"
    )) return("Información general")
    if (field %in% c(
      "fecha_realizacion_bioensayo", "codigo_insecticida", "solvente_utilizado", "solvente_otro",
      "dosis_ug_ml", "codigo_dosis", "fecha_revestimiento_botellas", paste0("numero_usos_botella_", c("e1", "e2", "e3", "e4", "c1"))
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
      "__BASE__", row$dosis_diagnostica_1x, row$modalidad_bioensayo, row$dosis_intensidad,
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

  f7_load_review_records <- function(random_sample = FALSE) {
    start_date <- as.Date(input$f7_review_start_date)
    end_date <- as.Date(input$f7_review_end_date)
    if (is.na(start_date) || is.na(end_date) || start_date > end_date) stop("Seleccione un rango de fechas válido.")
    status <- f5_text(input$f7_review_filter_status)
    if (!status %in% c("pending", "reviewed", "rejected", "all")) status <- "pending"
    connection <- connect_to_supabase()
    on.exit(dbDisconnect(connection), add = TRUE)
    total <- dbGetQuery(
      connection,
      "select count(*)::integer as total from public.formulario_7_bioensayo_intake where fecha_registro between $1 and $2 and ($3 = 'all' or review_status = $3)",
      params = list(as.character(start_date), as.character(end_date), status)
    )$total[[1]]
    if (total == 0) return(data.frame())
    limit <- if (random_sample) max(1L, ceiling(as.integer(total) * 0.10)) else min(as.integer(total), 50L)
    order_clause <- if (random_sample) "order by random()" else "order by creado_en desc nulls last, intake_id desc"
    query <- paste(
      "select intake_id, codigo_bioensayo, fecha_registro, pais, nombre_poblacion, review_status, actualizado_en from public.formulario_7_bioensayo_intake where fecha_registro between $1 and $2 and ($3 = 'all' or review_status = $3)",
      order_clause,
      "limit $4"
    )
    dbGetQuery(
      connection,
      query,
      params = list(as.character(start_date), as.character(end_date), status, as.integer(limit))
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
    list(input$f7_tipo_bioensayo, input$f7_modalidad_bioensayo)
  }, {
    bottle_mode <- if (identical(input$f7_tipo_bioensayo, "intensidad") && identical(input$f7_modalidad_bioensayo, "Exploratorio")) "Exploratorio" else "Completa"
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
    selected_modality <- if (identical(selected_bioassay_type, "intensidad")) value_or_default(input$f7_modalidad_bioensayo, "Exploratorio") else NA_character_
    selected_synergists <- if (identical(selected_bioassay_type, "sinergistas")) value_or_default(input$f7_sinergistas, character()) else character()
    diagnostic_result <- if (identical(selected_bioassay_type, "diagnostica_1x")) value_or_default(input$f7_resultado_diagnostico, NA_character_) else NA_character_
    if (identical(value_or_default(input$f7_codigo_municipio, ""), "__manual__")) {
      values$codigo_municipio <- gsub("[^0-9]+", "", value_or_default(input$f7_codigo_municipio_manual, ""))
    }
    values$modalidad_bioensayo <- selected_modality
    values$dosis_diagnostica_1x <- as.character(identical(selected_bioassay_type, "diagnostica_1x"))
    values$dosis_intensidad <- if (identical(selected_bioassay_type, "intensidad") && identical(selected_modality, "Completa")) input$f7_dosis_intensidad else NA_character_
    values$sinergista_def <- as.character("def" %in% selected_synergists)
    values$sinergista_pbo <- as.character("pbo" %in% selected_synergists)
    values$sinergista_dm <- as.character("dm" %in% selected_synergists)
    values$resultado_diagnostico <- diagnostic_result
    values$codigo_bioensayo <- formulario_7_codigo_bioensayo_final(
      values$codigo_bioensayo, values$dosis_diagnostica_1x, values$modalidad_bioensayo, values$dosis_intensidad,
      values$sinergista_def, values$sinergista_pbo, values$sinergista_dm
    )
    if (identical(selected_bioassay_type, "sinergistas")) {
      non_synergist_results <- grep("resultado_(0|15|30|45)min_|resultado_hora_lectura_24h_|resultado_24h_", formulario_7_result_columns, value = TRUE)
      values[non_synergist_results] <- NA_character_
      values$codigo_revision_24h <- NA_character_
    } else {
      synergist_results <- grep("resultado_60min_", formulario_7_result_columns, value = TRUE)
      values[synergist_results] <- NA_character_
    }
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
      if (identical(input$f7_tipo_bioensayo, "diagnostica_1x")) {
        if (missing_value("resultado_diagnostico")) errors <- c(errors, "Seleccione el resultado: Suceptible, Sospecha de Resistencia o Resistente.")
      }
      if (identical(input$f7_tipo_bioensayo, "intensidad")) {
        if (missing_value("modalidad_bioensayo")) errors <- c(errors, "Seleccione Intensidad Exploratorio o Completa.")
        if (identical(input$f7_modalidad_bioensayo, "Completa") && missing_value("dosis_intensidad")) errors <- c(errors, "Seleccione la dosis de intensidad 1X, 2X, 5X o 10X.")
      }
      if (identical(input$f7_tipo_bioensayo, "sinergistas") && length(value_or_default(input$f7_sinergistas, character())) == 0) errors <- c(errors, "Seleccione al menos un sinergista: DEF, PBO o DM.")
    }

    if (identical(step, "informacion_bioensayo")) {
      require_fields(
        c("fecha_realizacion_bioensayo", "codigo_insecticida", "solvente_utilizado", "dosis_ug_ml", "codigo_dosis", "fecha_revestimiento_botellas"),
        c("fecha de realización", "código de insecticida", "solvente utilizado", "dosis", "código de dosis", "fecha de revestimiento")
      )
      if (identical(value("solvente_utilizado"), "Otro") && missing_value("solvente_otro")) errors <- c(errors, "Especifique el otro solvente utilizado.")
      dose <- suppressWarnings(as.numeric(value("dosis_ug_ml")))
      if (!is.na(value("dosis_ug_ml")) && (is.na(dose) || dose < 0)) errors <- c(errors, "La dosis debe ser un número igual o mayor que cero.")
    }

    if (identical(step, "material_responsables")) {
      require_fields(
        c("origen_material", "codigo_especie_mosquito", "hora_separacion", "fecha_separacion", "codigo_responsable_revestimiento", "codigo_responsable_bioensayo", "codigo_control_calidad"),
        c("origen del material", "código de especie", "hora de separación", "fecha de separación", "responsable de revestimiento", "responsable del bioensayo", "control de calidad")
      )
      if (!identical(input$f7_tipo_bioensayo, "sinergistas")) {
        require_fields("codigo_revision_24h", "revisión a 24 horas")
      }
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
        tags$td(f5_review_text_value(record$pais)),
        tags$td(f5_review_text_value(record$nombre_poblacion)),
        tags$td(f5_review_text_value(record$review_status))
      )
    })
    tagList(
      h4("Listado para revisión"),
      tags$table(
        class = "table table-striped table-condensed",
        tags$thead(tags$tr(
          tags$th("intake_id"), tags$th("Código bioensayo"), tags$th("Fecha"),
          tags$th("País"), tags$th("Población"), tags$th("Estado")
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

    value_for <- function(field) {
      value <- row[[field]][[1]]
      if (is.null(value) || length(value) == 0 || is.na(value)) return("")
      as.character(value)
    }
    input_for_field <- function(field) {
      label <- f7_review_field_label(field)
      value <- value_for(field)
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
        modalidad_bioensayo = c("No aplica" = "", "Exploratorio" = "Exploratorio", "Completa" = "Completa"),
        dosis_intensidad = c("No aplica" = "", "1X" = "1X", "2X" = "2X", "5X" = "5X", "10X" = "10X"),
        resultado_diagnostico = c("No aplica" = "", "Suceptible" = "Suceptible", "Sospecha de Resistencia" = "Sospecha de Resistencia", "Resistente" = "Resistente"),
        solvente_utilizado = c("Etanol", "Otro"),
        origen_material = c("Silvestre", "Laboratorio"),
        NULL
      )
      if (!is.null(choices)) return(selectInput(input_id, label, choices = choices, selected = value))
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
    f5_review_status(list(type = "idle", message = NULL, details = character()))
    show_formulario_5_review_modal()
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
    f7_review_status(list(type = "idle", message = NULL, details = character()))
    show_formulario_7_review_modal()
  })

  observeEvent(input$close_formulario_7_review, removeModal())

  observeEvent(input$f7_review_generate_sample, {
    f7_review_status(list(type = "info", message = "Generando muestra aleatoria del 10%...", details = character()))
    f7_review_records(data.frame())
    f7_review_selected(NULL)
    f7_review_edit_mode(FALSE)
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
      f7_review_status(list(
        type = if (nrow(records)) "success" else "warning",
        message = if (nrow(records)) sprintf("Se cargaron %s registro(s).", nrow(records)) else "No hay registros con ese estado.",
        details = character()
      ))
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudo actualizar el listado.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f7_review_find, {
    intake_id <- f5_integer(input$f7_review_search_id)
    if (is.na(intake_id)) {
      f7_review_status(list(type = "warning", message = "Ingrese un intake_id válido.", details = character()))
      return()
    }
    tryCatch({
      f7_select_review_record(intake_id)
      f7_review_status(list(type = "success", message = sprintf("Registro %s abierto para revisión.", intake_id), details = character()))
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudo abrir el registro.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f7_review_select_id, {
    intake_id <- f5_integer(input$f7_review_select_id)
    if (is.na(intake_id)) return()
    tryCatch({
      f7_select_review_record(intake_id)
      f7_review_status(list(type = "success", message = sprintf("Registro %s abierto para revisión.", intake_id), details = character()))
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudo abrir el registro.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f7_review_enable_edit, f7_review_edit_mode(TRUE))
  observeEvent(input$f7_review_cancel_edit, f7_review_edit_mode(FALSE))

  observeEvent(input$f7_review_save_changes, {
    selected <- f7_review_selected()
    if (is.null(selected)) return()
    validated <- validate_formulario_7(f7_review_input_row())
    if (length(validated$details)) {
      f7_review_status(list(type = "error", message = "Revise los valores editados.", details = validated$details))
      return()
    }
    intake_id <- as.integer(selected$header$intake_id[[1]])
    tryCatch({
      f7_update_review_record(intake_id, validated$data)
      f7_select_review_record(intake_id)
      f7_review_records(f7_load_review_records())
      f7_review_status(list(type = "success", message = sprintf("Cambios guardados para intake_id %s. Estado: pending.", intake_id), details = character()))
    }, error = function(error) {
      f7_review_status(list(type = "error", message = "No se pudieron guardar los cambios.", details = conditionMessage(error)))
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
    active_dataset(NULL)
  })

  observeEvent(input$show_protocols_area, {
    active_area(if (identical(active_area(), "protocols")) NULL else "protocols")
    active_module(NULL)
    active_dataset(NULL)
  })

  observeEvent(input$show_training_area, {
    active_area(if (identical(active_area(), "training")) NULL else "training")
    active_module(NULL)
    active_dataset(NULL)
  })

  observeEvent(input$show_capture, {
    active_area("data")
    active_module("capture")
  })

  select_capture_dataset <- function(dataset) {
    active_area("data")
    active_module("capture")
    active_dataset(dataset)
    submission_status("No se ha enviado ningún registro en esta sesión.")
  }

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
    active_dataset(NULL)
  })

  observeEvent(input$show_request, {
    active_area("data")
    active_module("request")
    active_dataset(NULL)
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

  observeEvent(input$search_visualization, {
    query <- list(
      country = input$visualization_country,
      dataset = input$visualization_dataset
    )
    visualization_query(query)

    if (identical(query$dataset, "formulario_7_bioensayo_botella_cdc")) {
      f7_visualization_records(data.frame())
      f7_visualization_error(NULL)
      if (identical(query$country, "El Salvador")) {
        connection <- NULL
        tryCatch({
          connection <- connect_to_supabase()
          records <- dbGetQuery(
            connection,
            "
              select
                intake_id, codigo_bioensayo, fecha_realizacion_bioensayo,
                nombre_poblacion, modalidad_bioensayo, dosis_diagnostica_1x,
                sinergista_def, sinergista_pbo, sinergista_dm, resultado_diagnostico,
                codigo_insecticida, codigo_departamento, codigo_municipio, review_status
              from public.formulario_7_bioensayo_intake
              where pais = $1
              order by fecha_realizacion_bioensayo, intake_id
            ",
            params = list("El Salvador")
          )
          if (nrow(records)) {
            records$fecha_realizacion_bioensayo <- as.Date(records$fecha_realizacion_bioensayo)
            records$tipo_bioensayo <- ifelse(
              records$dosis_diagnostica_1x,
              "Diagnóstica 1X",
              ifelse(
                !is.na(records$modalidad_bioensayo),
                paste("Intensidad", records$modalidad_bioensayo),
                "Sinergistas"
              )
            )
            records$codigo_departamento_num <- suppressWarnings(as.integer(records$codigo_departamento))
            records$codigo_municipio_num <- suppressWarnings(as.integer(records$codigo_municipio))
            location_key <- paste(records$codigo_departamento_num, records$codigo_municipio_num, sep = "|")
            reference_key <- paste(
              formulario_7_el_salvador_locations$codigo_departamento_num,
              formulario_7_el_salvador_locations$codigo_municipio_num,
              sep = "|"
            )
            location_index <- match(location_key, reference_key)
            records$departamento <- formulario_7_el_salvador_locations$departamento[location_index]
            records$municipio <- formulario_7_el_salvador_locations$municipio[location_index]
            records$latitude <- formulario_7_el_salvador_locations$latitude[location_index]
            records$longitude <- formulario_7_el_salvador_locations$longitude[location_index]
          }
          f7_visualization_records(records)
        }, error = function(error) {
          f7_visualization_error(conditionMessage(error))
        }, finally = {
          if (!is.null(connection)) dbDisconnect(connection)
        })
      }
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

  output$f7_visualization_filters <- renderUI({
    records <- f7_visualization_records()
    if (!nrow(records)) return(NULL)
    dates <- records$fecha_realizacion_bioensayo
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
            choices = c("Todos" = "all", sort(unique(records$codigo_bioensayo)))
          )
        ),
        column(
          4,
          selectInput(
            "f7_viz_type",
            "Tipo de bioensayo",
            choices = c("Todos" = "all", sort(unique(records$tipo_bioensayo)))
          )
        )
      ),
      fluidRow(
        column(
          4,
          selectInput(
            "f7_viz_insecticide",
            "Insecticida",
            choices = c("Todos" = "all", sort(unique(records$codigo_insecticida)))
          )
        ),
        column(
          4,
          selectInput(
            "f7_viz_population",
            "Población",
            choices = c("Todas" = "all", sort(unique(records$nombre_poblacion)))
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
      codigo_insecticida = input$f7_viz_insecticide,
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
      records$codigo_departamento_num,
      records$codigo_municipio_num,
      records$nombre_poblacion,
      sep = "|"
    )
    groups <- split(records, group_key)
    points <- do.call(rbind, lapply(groups, function(group) {
      data.frame(
        location_id = paste(group$codigo_departamento_num[[1]], group$codigo_municipio_num[[1]], group$nombre_poblacion[[1]], sep = "|"),
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
    insecticide_levels <- c("Deltametrina", "Permetrina", "DDT", "Malathion", "Bendiocarb")
    result_levels <- c("Resistencia", "Sospecha Resistencia", "Susceptible")
    insecticide_codes <- toupper(trimws(as.character(records$codigo_insecticida)))
    insecticide <- ifelse(
      grepl("DEL|DELTAMETRINA", insecticide_codes),
      "Deltametrina",
      ifelse(
        grepl("PER|PERMETRINA", insecticide_codes),
        "Permetrina",
        ifelse(
          grepl("DDT", insecticide_codes),
          "DDT",
          ifelse(grepl("MAL|MALATHION", insecticide_codes), "Malathion", ifelse(grepl("BEN|BENDIOCARB", insecticide_codes), "Bendiocarb", NA_character_))
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

  output$f7_visualization_kpis <- renderUI({
    records <- f7_visualization_filtered()
    mapped <- records[!is.na(records$latitude) & !is.na(records$longitude), , drop = FALSE]
    municipalities <- if (nrow(mapped)) length(unique(paste(mapped$codigo_departamento_num, mapped$codigo_municipio_num))) else 0L
    div(
      class = "f7-viz-kpi-grid",
      div(class = "f7-viz-kpi-card", span(class = "f7-viz-kpi-label", "Bioensayos"), span(class = "f7-viz-kpi-value", nrow(records))),
      div(class = "f7-viz-kpi-card", span(class = "f7-viz-kpi-label", "Poblaciones"), span(class = "f7-viz-kpi-value", length(unique(records$nombre_poblacion)))),
      div(class = "f7-viz-kpi-card", span(class = "f7-viz-kpi-label", "Municipios representados"), span(class = "f7-viz-kpi-value", municipalities)),
      div(class = "f7-viz-kpi-card", span(class = "f7-viz-kpi-label", "Resultados resistentes"), span(class = "f7-viz-kpi-value", sum(records$resultado_diagnostico == "Resistente", na.rm = TRUE)))
    )
  })

  output$f7_visualization_map <- renderLeaflet({
    points <- f7_visualization_map_points()
    map <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -89.20, lat = 13.75, zoom = 8) |>
      addControl(
        html = "Ubicaciones aproximadas para datos ficticios. Las coordenadas reales vendrán del formulario de colecta.",
        position = "bottomleft"
      )
    if (!nrow(points)) {
      return(map |> addControl(html = "No hay registros para los filtros seleccionados.", position = "topright"))
    }
    palette <- colorFactor(
      palette = c("#C62828", "#F9A825", "#2E7D32", "#757575"),
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
    map |>
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
  })

  output$f7_visualization_diagnostic_bar_chart <- renderPlot({
    counts <- f7_visualization_insecticide_counts()
    totals <- colSums(counts)
    percentages <- sweep(counts, 2, pmax(totals, 1), "/") * 100
    previous_margins <- par(mar = c(4.8, 4.4, 1.2, 0.8))
    on.exit(par(previous_margins), add = TRUE)
    bar_positions <- barplot(
      percentages,
      col = c("#C62828", "#F9A825", "#2E7D32"),
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
      fill = c("#C62828", "#F9A825", "#2E7D32"),
      border = NA,
      bty = "n",
      horiz = TRUE,
      cex = 0.8,
      inset = c(0, -0.05)
    )
  }, bg = "transparent", res = 110)

  output$f7_visualization_diagnostic_summary_table <- renderTable({
    counts <- f7_visualization_insecticide_counts()
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

  output$f7_visualization_table <- renderTable({
    records <- f7_visualization_filtered()
    if (!nrow(records)) return(data.frame(Mensaje = "No hay registros para los filtros seleccionados."))
    data.frame(
      `Código bioensayo` = records$codigo_bioensayo,
      Fecha = records$fecha_realizacion_bioensayo,
      Población = records$nombre_poblacion,
      Municipio = ifelse(is.na(records$municipio), "Sin ubicación aproximada", records$municipio),
      `Tipo de bioensayo` = records$tipo_bioensayo,
      Insecticida = records$codigo_insecticida,
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
      if (!identical(query$country, "El Salvador")) {
        return(div(
          class = "alert alert-warning",
          "El mapa piloto del Formulario 7 está disponible por ahora para El Salvador. Seleccione El Salvador y presione Buscar."
        ))
      }
      return(tagList(
        div(
          class = "visualization-results-card",
          h4("Mapa de bioensayos y poblaciones - El Salvador"),
          p("Utilice los filtros para explorar los registros del Formulario 7. Los puntos actuales son ubicaciones aproximadas de las poblaciones ficticias y no coordenadas reales de trampas."),
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
            tableOutput("f7_visualization_diagnostic_summary_table")
          )
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
    if (!is.null(active_area())) {
      return(NULL)
    }

    fluidRow(
      column(
        width = 12,
        div(
          class = "capture-title-card",
          h2(HTML("<strong>EntoNet</strong> - Red Entomológica para la Vigilancia y Control de Vectores")),
          p("EntoNet es una iniciativa regional financiada por los Centros para el Control y la Prevención de Enfermedades de los Estados Unidos (CDC) que busca fortalecer la vigilancia entomológica y el control de vectores de importancia médica en Centroamérica y República Dominicana. La red promueve la interoperabilidad, estandarización e integración de datos entomológicos generados por ministerios de salud, instituciones académicas y programas nacionales de control vectorial.")
        )
      )
    )
  })

  output$portal_sidebar <- renderUI({
    area <- active_area()
    module <- active_module()
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
          actionButton("select_formulario_1_capture", "Formulario 1: Colocación y retiro", class = form_item_class("formulario_1_colocacion_retiro_ovitrampa")),
          actionButton("select_formulario_5_capture", "Formulario 5: Alimentación conteo", class = form_item_class("formulario_5_alimentacion_conteo")),
          actionButton("select_formulario_7_capture", "Formulario 7: Bioensayo CDC", class = form_item_class("formulario_7_bioensayo_botella_cdc"))
        ),
        actionButton("show_visualization", "Visualización de Datos", class = subitem_class("visualization")),
        actionButton("show_request", "Solicitud de Datos", class = subitem_class("request"))
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

    if (identical(area, "data")) {
      if (is.null(module)) return(div(class = "portal-empty-state", "Seleccione una opción de Datos en el menú lateral."))
      return(uiOutput("data_module_area"))
    }

    if (identical(area, "protocols")) {
      if (is.null(module)) return(div(class = "portal-empty-state", "Seleccione Protocolos de Campo o Protocolos de Laboratorio."))
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
      if (is.null(module)) return(div(class = "portal-empty-state", "Seleccione un tipo de entrenamiento en el menú lateral."))
      if (identical(module, "training_live")) return(div(
        class = "module-panel",
        h3("Próximas capacitaciones en vivo"),
        p("Fechas por confirmar. Se priorizarán sesiones regionales sobre captura estandarizada de datos, uso del portal y control de calidad.")
      ))
      if (identical(module, "training_workshops")) return(div(
        class = "module-panel",
        h3("Talleres prácticos"),
        p("Fechas por confirmar. Talleres enfocados en protocolos de campo, procedimientos de laboratorio y análisis inicial de datos entomológicos.")
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

    div(
      class = "module-panel",
      h3("Solicitud de Datos"),
      div(
        class = "alert alert-info",
        "Las solicitudes de datos se habilitarán únicamente para los formularios activos aprobados por EntoNet."
      )
    )
  })

  output$active_dataset_header <- renderUI({
    dataset <- active_dataset()

    if (is.null(dataset)) {
      return(div(
        class = "alert alert-info",
        "Seleccione un formulario disponible en el menú lateral."
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

    if (is.null(dataset)) {
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
            capture_action_row("Subida de datos masiva", "Cargue varios bioensayos desde el machote CSV oficial de 111 columnas visibles.", "open_formulario_7_bulk_upload", "Abrir subida masiva"),
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
      write.csv(template[formulario_7_csv_columns], file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    }
  )

  output$download_formulario_7_printable <- downloadHandler(
    filename = function() {
      code <- f7_print_codigo_bioensayo_code()
      code <- if (!is.na(code) && nzchar(code)) code else format(Sys.Date(), "%Y%m%d")
      paste0("formulario_7_", code, ".xlsx")
    },
    content = function(file) {
      code <- f7_print_codigo_bioensayo_code()
      municipality_code <- f7_print_selected_municipality_code()
      department_code <- value_or_default(input$f7_print_codigo_bioensayo_departamento, "")
      if (is.na(code) || !nzchar(code)) {
        stop("Complete país, departamento, municipio, insecticida, # población, tipo de bioensayo y año antes de descargar.")
      }
      version_formulario <- toupper(trimws(value_or_default(input$f7_print_version_formulario, "")))
      if (!nzchar(version_formulario)) {
        stop("Ingrese la versión del formulario antes de descargar.")
      }
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
        stop("Complete país, departamento, municipio, insecticida, # población, tipo de bioensayo y año antes de descargar.")
      }
      template <- formulario_7_template
      template$fecha_registro <- as.character(Sys.Date())
      template$pais <- f5_text(input$f7_print_pais)
      template$codigo_departamento <- department_code
      template$codigo_municipio <- municipality_code
      template$codigo_bioensayo <- code
      template$codigo_insecticida <- f5_text(input$f7_print_codigo_insecticida)
      template$nombre_poblacion <- f5_optional_text(input$f7_print_nombre_poblacion)
      type <- value_or_default(input$f7_print_tipo_bioensayo, "DD")
      if (identical(type, "DD")) {
        template$dosis_diagnostica_1x <- "true"
      } else if (identical(type, "IE")) {
        template$dosis_diagnostica_1x <- "false"
        template$modalidad_bioensayo <- "Exploratorio"
      } else if (identical(type, "IC")) {
        template$dosis_diagnostica_1x <- "false"
        template$modalidad_bioensayo <- "Completa"
        template$dosis_intensidad <- toupper(trimws(value_or_default(input$f7_print_intensidad_completa_dosis, "")))
      } else if (identical(type, "S")) {
        template$dosis_diagnostica_1x <- "false"
        selected_synergist <- tolower(trimws(value_or_default(input$f7_print_sinergista, "")))
        template$sinergista_def <- as.character(identical(selected_synergist, "def"))
        template$sinergista_pbo <- as.character(identical(selected_synergist, "pbo"))
        template$sinergista_dm <- as.character(identical(selected_synergist, "dm"))
      }
      write.csv(template[formulario_7_csv_columns], file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
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
        tags$td(f5_review_text_value(record$review_status))
      )
    })
    tagList(
      h4("Listado para revisión"),
      tags$table(
        class = "table table-striped table-condensed",
        tags$thead(tags$tr(
          tags$th("intake_id"), tags$th("Código formulario"), tags$th("Fecha"),
          tags$th("País"), tags$th("Cuadrante"), tags$th("Casa"), tags$th("Estado")
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
    f1_review_status(list(type = "idle", message = NULL, details = character()))
    show_formulario_1_review_modal()
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
      f1_review_status(list(type = "success", message = sprintf("Registro %s abierto para revisión.", intake_id), details = character()))
    }, error = function(error) {
      f1_review_status(list(type = "error", message = "No se pudo abrir el registro.", details = conditionMessage(error)))
    })
  })

  observeEvent(input$f1_review_enable_edit, f1_review_edit_mode(TRUE))
  observeEvent(input$f1_review_cancel_edit, f1_review_edit_mode(FALSE))

  observeEvent(input$f1_review_save_changes, {
    selected <- f1_review_selected()
    if (is.null(selected)) return()
    validated <- validate_formulario_1(f1_review_input_rows())
    if (length(validated$details)) {
      f1_review_status(list(type = "error", message = "Revise los valores editados.", details = validated$details))
      return()
    }
    intake_id <- as.integer(selected$header$intake_id[[1]])
    tryCatch({
      f1_update_review_record(intake_id, validated$data)
      f1_select_review_record(intake_id)
      f1_review_records(f1_load_review_records())
      f1_review_status(list(type = "success", message = sprintf("Cambios guardados para intake_id %s. Estado: pending.", intake_id), details = character()))
    }, error = function(error) {
      f1_review_status(list(type = "error", message = "No se pudieron guardar los cambios.", details = conditionMessage(error)))
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

  output$download_csv <- downloadHandler(
    filename = function() {
      dataset <- input$request_dataset
      date_stamp <- format(Sys.Date(), "%Y%m%d")

      switch(
        dataset,
        egg_intake = paste0("entonet_ovipostura_registros_pendientes_", date_stamp, ".csv"),
        paste0("entonet_ovipostura_observaciones_", date_stamp, ".csv")
      )
    },
    content = function(file) {
      dataset <- input$request_dataset
      query <- switch(
        dataset,
        egg_intake = "
          select *
          from rei.egg_count_intake
          order by submitted_at desc
        ",
        "
          select *
          from rei.egg_count_observations
          order by observation_id
        "
      )

      connection <- NULL
      data <- data.frame()

      tryCatch({
        connection <- connect_to_supabase()
        data <- dbGetQuery(connection, query)
      }, finally = {
        if (!is.null(connection)) {
          dbDisconnect(connection)
        }
      })

      write.csv(data, file, row.names = FALSE, na = "")
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
