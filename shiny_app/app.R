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
    normalizePath("../.env.local", mustWork = FALSE),
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
app_user <- read_local_env_value("PROJECT_REI_APP_USER")
app_password <- read_local_env_value("PROJECT_REI_APP_PASSWORD")
skip_login <- tolower(read_local_env_value("ENTONET_SKIP_LOGIN")) %in% c("1", "true", "yes", "si", "sí")
profile_name <- read_local_env_value("PROJECT_REI_PROFILE_NAME")
profile_institution <- read_local_env_value("PROJECT_REI_PROFILE_INSTITUTION")
profile_position <- read_local_env_value("PROJECT_REI_PROFILE_POSITION")
profile_country <- read_local_env_value("PROJECT_REI_PROFILE_COUNTRY")
support_email <- read_local_env_value("PROJECT_REI_SUPPORT_EMAIL")
supabase_url <- read_local_env_value("SUPABASE_URL")
supabase_service_role_key <- read_local_env_value("SUPABASE_SERVICE_ROLE_KEY")

value_or_default <- function(value, default) {
  if (!is.null(value) && length(value) > 0 && nzchar(value[[1]])) {
    return(value)
  }

  default
}

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
            textInput("login_user", tr(language, "Usuario", "Username")),
            passwordInput("login_password", tr(language, "Contraseña", "Password")),
            actionButton("login", tr(language, "Ingresar", "Log in"), class = "btn-primary login-button"),
            div(class = "login-help", tr(language, "Si necesita acceso, contacte al administrador del proyecto.", "If you need access, contact the project administrator."))
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
        span(class = "header-user", strong(value_or_default(app_user, "Usuario"))),
        actionButton("profile", "Perfil", class = "btn-default header-profile"),
        actionButton("logout", "Cerrar sesión", class = "btn-default header-logout")
      )
    ),
    div(
      class = "capture-main",
      uiOutput("portal_intro_area"),
      fluidRow(
        class = "portal-module-row",
        column(
          width = 4,
          actionButton(
            "show_data_area",
            label = tagList(
              span(class = "module-card-title", "Datos"),
              span(class = "module-card-text", "Captura, visualización y solicitud de datos entomológicos.")
            ),
            class = "module-card module-card-capture"
          )
        ),
        column(
          width = 4,
          actionButton(
            "show_protocols_area",
            label = tagList(
              span(class = "module-card-title", "Protocolos"),
              span(class = "module-card-text", "Procedimientos de campo y laboratorio para la red.")
            ),
            class = "module-card module-card-visualization"
          )
        ),
        column(
          width = 4,
          actionButton(
            "show_training_area",
            label = tagList(
              span(class = "module-card-title", "Entrenamientos"),
              span(class = "module-card-text", "Capacitaciones en vivo, talleres y materiales de apoyo.")
            ),
            class = "module-card module-card-request"
          )
        )
      ),
      uiOutput("module_area")
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
      "Formulario de captura preliminar con todas las columnas disponibles en la tabla de ingreso. Los campos se pueden editar y depurar en la siguiente iteración."
    ),
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h4("Metadatos"),
          textInput("f5_formulario_codigo", "Código del formulario", value = "F5"),
          selectInput("f5_pais", "País", choices = c("El Salvador", "Guatemala"), selected = "El Salvador"),
          numericInput("f5_departamento_numero", "Departamento #", value = NA, min = 0, step = 1),
          numericInput("f5_municipio_numero", "Municipio #", value = NA, min = 0, step = 1),
          textInput("f5_ciclo", "Ciclo", value = "Ciclo 3"),
          textInput("f5_formulario_nombre", "Nombre del formulario", value = "Alimentación sanguínea y conteo huevecillos Aedes spp."),
          dateInput("f5_fecha_registro", "Fecha de registro", value = Sys.Date())
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h4("Datos generales"),
          textInput("f5_cepa_poblacion", "Cepa / población"),
          selectInput("f5_especie", "Especie", choices = c("Ae. aegypti", "Ae. albopictus")),
          textInput("f5_generacion_filial_adultos", "Generación filial adultos"),
          textInput("f5_responsable_ingreso_jaula", "Responsable ingreso jaula"),
          dateInput("f5_fecha_jaula", "Fecha jaula", value = Sys.Date()),
          numericInput("f5_numero_hembras", "Número de hembras", value = 0, min = 0, step = 1),
          numericInput("f5_numero_machos", "Número de machos", value = 0, min = 0, step = 1),
          div(class = "summary-box", strong("Total individuos: "), textOutput("f5_total_individuos", inline = TRUE)),
          numericInput("f5_total_huevos_viables", "Total huevos viables", value = NA, min = 0, step = 1)
        )
      ),
      column(
        width = 6,
        wellPanel(
          h4("Alimentación sanguínea"),
          textInput("f5_responsable_alimentacion", "Responsable alimentación"),
          selectInput(
            "f5_tipo_alimentacion_codigo",
            "Tipo alimentación código",
            choices = c("A", "B", "C", "D", "E")
          ),
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
          numericInput("f5_numero_charolas", "Número de charolas", value = 0, min = 0, step = 1),
          textAreaInput("f5_observaciones_alimentacion", "Observaciones alimentación", rows = 4)
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        wellPanel(
          h4("Conteo de huevecillos"),
          textInput("f5_generacion_filial_huevos", "Generación filial huevos"),
          textInput("f5_codigo_sustrato", "Código sustrato"),
          dateInput("f5_fecha_colocacion_sustrato", "Fecha colocación sustrato", value = Sys.Date()),
          dateInput("f5_fecha_retiro_sustrato", "Fecha retiro sustrato", value = Sys.Date()),
          numericInput("f5_numero_cuadro_sustrato", "Número cuadro sustrato", value = 0, min = 0, step = 1),
          numericInput("f5_hv_huevos_viables", "HV - huevos viables", value = 0, min = 0, step = 1),
          numericInput("f5_he_huevos_eclosionados", "HE - huevos eclosionados", value = 0, min = 0, step = 1),
          numericInput("f5_hc_huevos_canoa", "HC - huevos canoa", value = 0, min = 0, step = 1),
          numericInput("f5_hnf_huevos_no_fecundados", "HNF - huevos no fecundados", value = 0, min = 0, step = 1),
          div(class = "summary-box", strong("Total huevos: "), textOutput("f5_total_huevos", inline = TRUE)),
          textInput("f5_responsable_conteo_huevos", "Responsable conteo huevos")
        )
      ),
      column(
        width = 6,
        wellPanel(
          h4("Observaciones y auditoría"),
          textAreaInput("f5_observaciones_generales", "Observaciones generales", rows = 4),
          textInput("f5_fuente_formulario", "Fuente formulario"),
          textInput("f5_creado_por", "Creado por"),
          dateInput("f5_creado_en", "Fecha creación", value = Sys.Date()),
          dateInput("f5_actualizado_en", "Fecha actualización", value = Sys.Date())
        )
      )
    ),
    div(
      class = "submit-row",
      tags$button(
        type = "button",
        class = "btn btn-primary",
        disabled = "disabled",
        "Guardar registro pendiente"
      ),
      span(
        class = "help-block",
        "El guardado en Supabase se conectará después de revisar y ajustar los campos."
      )
    )
  )
}

formulario_5_review_form <- function() {
  tagList(
    div(
      class = "alert alert-info",
      "Componente preliminar para revisar registros de Formulario 5 guardados en intake. Las columnas de revisión ya existen en Supabase; la conexión a registros pendientes se hará después."
    ),
    wellPanel(
      h4("Revisión de formularios"),
      selectInput(
        "f5_review_status",
        "Estado de revisión",
        choices = c("Pendiente" = "pending", "Revisado" = "reviewed", "Rechazado" = "rejected"),
        selected = "pending"
      ),
      textAreaInput("f5_review_notes", "Notas de revisión", rows = 5),
      textInput("f5_reviewed_by", "Revisado por"),
      dateInput("f5_reviewed_at", "Fecha de revisión", value = Sys.Date())
    ),
    div(
      class = "submit-row",
      tags$button(
        type = "button",
        class = "btn btn-primary",
        disabled = "disabled",
        "Guardar revisión"
      ),
      span(
        class = "help-block",
        "El guardado de revisión se conectará cuando activemos la consulta de registros pendientes."
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "leaflet/leaflet.css"),
    tags$script(src = "leaflet/leaflet.js"),
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
      .login-help {
        color: #5b6778;
        font-size: 13px;
        margin-top: 14px;
      }
      .capture-main {
        padding: 54px 10% 80px 10%;
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
  logged_in <- reactiveVal(skip_login)
  public_page <- reactiveVal("login")
  public_language <- reactiveVal("es")
  login_error <- reactiveVal(NULL)
  user_profile <- reactiveValues(
    name = value_or_default(profile_name, value_or_default(app_user, "Usuario autorizado")),
    institution = value_or_default(profile_institution, "Institución no configurada"),
    position = value_or_default(profile_position, "Puesto no configurado"),
    country = value_or_default(profile_country, "País no configurado")
  )

  output$app_page <- renderUI({
    if (!logged_in()) {
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
      if (is.null(message) && (!nzchar(app_user) || !nzchar(app_password))) {
        message <- div(
          class = "alert alert-warning",
          "Las credenciales de ingreso no están configuradas. Agregue PROJECT_REI_APP_USER y PROJECT_REI_APP_PASSWORD al archivo .env.local."
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

  observeEvent(input$login, {
    if (!nzchar(app_user) || !nzchar(app_password)) {
      login_error(div(class = "alert alert-warning", "Las credenciales de ingreso no están configuradas para esta aplicación."))
      return()
    }

    if (identical(input$login_user, app_user) &&
        identical(input$login_password, app_password)) {
      logged_in(TRUE)
      public_page("login")
      login_error(NULL)
      return()
    }

    login_error(div(class = "alert alert-danger", "Usuario o contraseña incorrectos."))
  })

  observeEvent(input$logout, {
    logged_in(FALSE)
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
        textInput(
          "profile_name_input",
          "Nombre y apellido",
          value = user_profile$name
        ),
        textInput(
          "profile_institution_input",
          "Institución",
          value = user_profile$institution
        ),
        textInput(
          "profile_position_input",
          "Puesto",
          value = user_profile$position
        ),
        textInput(
          "profile_country_input",
          "País",
          value = user_profile$country
        )
      ),
      div(class = "alert alert-info", "Los cambios se guardan para esta sesión. Cuando activemos usuarios en Supabase, este perfil podrá quedar asociado a cada cuenta."),
      actionButton("save_profile", "Guardar perfil", class = "btn-primary"),
      uiOutput("profile_save_message"),
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

  observeEvent(input$open_dataset, {
    active_dataset(input$dataset_choice)
    submission_status("No se ha enviado ningún registro en esta sesión.")

    if (identical(input$dataset_choice, "formulario_5_alimentacion_conteo")) {
      show_formulario_5_modal()
    }
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

  observeEvent(input$open_formulario_5_entry, {
    show_formulario_5_modal()
  })

  observeEvent(input$close_formulario_5_entry, {
    removeModal()
  })

  observeEvent(input$open_formulario_5_review, {
    show_formulario_5_review_modal()
  })

  observeEvent(input$close_formulario_5_review, {
    removeModal()
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
    active_area("data")
    active_module(NULL)
    active_dataset(NULL)
  })

  observeEvent(input$show_protocols_area, {
    active_area("protocols")
    active_module(NULL)
    active_dataset(NULL)
  })

  observeEvent(input$show_training_area, {
    active_area("training")
    active_module(NULL)
    active_dataset(NULL)
  })

  observeEvent(input$show_capture, {
    active_module("capture")
  })

  observeEvent(input$show_visualization, {
    active_module("visualization")
    active_dataset(NULL)
  })

  observeEvent(input$show_request, {
    active_module("request")
    active_dataset(NULL)
  })

  visualization_query <- reactiveVal(NULL)

  observeEvent(input$search_visualization, {
    query <- list(
      country = input$visualization_country,
      dataset = input$visualization_dataset
    )
    visualization_query(query)

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

  output$module_area <- renderUI({
    area <- active_area()

    if (is.null(area)) {
      return(div(
        class = "alert alert-info",
        "Seleccione Datos, Protocolos o Entrenamientos para continuar."
      ))
    }

    if (identical(area, "data")) {
      module <- active_module()

      return(tagList(
        div(
          class = "module-panel",
          h3("Datos"),
          p("Seleccione el tipo de acción que desea realizar con los datos de EntoNet."),
          fluidRow(
            class = "portal-module-row nested-module-row",
            column(
              width = 4,
              actionButton(
                "show_capture",
                label = tagList(
                  span(class = "module-card-title", "Captura de Datos"),
                  span(class = "module-card-text", "Ingreso de registros mediante formularios guiados.")
                ),
                class = "module-card module-card-capture"
              )
            ),
            column(
              width = 4,
              actionButton(
                "show_visualization",
                label = tagList(
                  span(class = "module-card-title", "Visualización de Datos"),
                  span(class = "module-card-text", "Consulta por país y tipo de información entomológica.")
                ),
                class = "module-card module-card-visualization"
              )
            ),
            column(
              width = 4,
              actionButton(
                "show_request",
                label = tagList(
                  span(class = "module-card-title", "Solicitud de Datos"),
                  span(class = "module-card-text", "Descarga de conjuntos disponibles en formato CSV.")
                ),
                class = "module-card module-card-request"
              )
            )
          )
        ),
        uiOutput("data_module_area")
      ))
    }

    if (identical(area, "protocols")) {
      return(div(
        class = "module-panel",
        h3("Protocolos"),
        p("Seleccione el tipo de protocolo que desea consultar. Esta sección servirá como repositorio de procedimientos estandarizados para los equipos de la red."),
        fluidRow(
          column(
            width = 6,
            div(
              class = "option-card",
              h4("Campo"),
              p("Protocolos para colocación y retiro de ovitrampas, muestreo de adultos, georreferenciación, transporte de muestras y registro de información en sitio.")
            )
          ),
          column(
            width = 6,
            div(
              class = "option-card",
              h4("Laboratorio"),
              p("Protocolos para conteo de huevos, identificación taxonómica, cría de colonias, bioensayos de resistencia y control de calidad de datos."),
              actionButton(
                "open_laboratory_protocols",
                "Abrir protocolos de laboratorio",
                class = "btn-primary"
              )
            )
          )
        )
      ))
    }

    if (identical(area, "training")) {
      return(div(
        class = "module-panel",
        h3("Entrenamientos"),
        p("Esta sección reunirá el calendario de capacitaciones de EntoNet, incluyendo sesiones en vivo, talleres prácticos y materiales asincrónicos."),
        div(
          class = "training-list",
          div(
            class = "training-item",
            h4("Próximas capacitaciones en vivo"),
            p("Fechas por confirmar. Se priorizarán sesiones regionales sobre captura estandarizada de datos, uso del portal y control de calidad.")
          ),
          div(
            class = "training-item",
            h4("Talleres prácticos"),
            p("Fechas por confirmar. Talleres enfocados en protocolos de campo, procedimientos de laboratorio y análisis inicial de datos entomológicos.")
          ),
          div(
            class = "training-item",
            h4("Materiales de apoyo"),
            p("Espacio previsto para guías rápidas, grabaciones, presentaciones y documentos de referencia para usuarios nuevos y equipos técnicos.")
          )
        )
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
        h3("Captura de Datos"),
        div(
          class = "selector-box",
          fluidRow(
            column(
              width = 8,
              selectInput(
                "dataset_choice",
                "Formulario disponible",
                choices = c(
                  "Formulario 5: Alimentación conteo" = "formulario_5_alimentacion_conteo",
                  "Ovipostura / conteo de huevos" = "egg_count_raw",
                  "Conteo de adultos" = "adult_count_raw",
                  "Resistencia / bioensayo" = "bioassay_raw"
                )
              )
            ),
            column(
              width = 4,
              br(),
              actionButton("open_dataset", "Abrir formulario", class = "btn-primary"),
              actionButton("open_formulario_5_review", "Revisión de formularios", class = "btn-primary")
            )
          )
        ),
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
                  "Ovipostura" = "oviposition",
                  "Adultos" = "adults",
                  "Resistencia" = "resistance"
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
      p("Seleccione el conjunto de datos disponible y descargue una copia en formato CSV."),
      div(
        class = "selector-box",
        fluidRow(
          column(
            width = 8,
            selectInput(
              "request_dataset",
              "Conjunto de datos",
              choices = c(
                "Ovipostura - observaciones revisadas" = "egg_observations",
                "Ovipostura - registros pendientes de captura" = "egg_intake"
              )
            )
          ),
          column(
            width = 4,
            br(),
            downloadButton("download_csv", "Descargar CSV", class = "btn-primary")
          )
        )
      ),
      div(
        class = "alert alert-info",
        "Por ahora la descarga incluye los conjuntos de ovipostura disponibles en Supabase. Adultos y resistencia se agregarán cuando esas tablas estén finalizadas."
      )
    )
  })

  output$active_dataset_header <- renderUI({
    dataset <- active_dataset()

    if (is.null(dataset)) {
      return(div(
        class = "alert alert-info",
        "Seleccione un conjunto de datos y haga clic en Abrir formulario."
      ))
    }

    dataset_label <- switch(
      dataset,
      formulario_5_alimentacion_conteo = "Formulario 5: Alimentación conteo",
      egg_count_raw = "Conteo de huevos - datos crudos",
      adult_count_raw = "Conteo de adultos - datos crudos",
      bioassay_raw = "Bioensayo - datos crudos"
    )

    h3(dataset_label)
  })

  output$data_entry_area <- renderUI({
    dataset <- active_dataset()

    if (is.null(dataset)) {
      return(NULL)
    }

    if (identical(dataset, "formulario_5_alimentacion_conteo")) {
      return(tagList(
        fluidRow(
          column(
            width = 12,
            div(
              class = "capture-option-card",
              h4("Formulario 5: Alimentación conteo"),
              p("Capture la información de alimentación sanguínea y conteo de huevecillos con todas las columnas disponibles en Supabase."),
              actionButton("open_formulario_5_entry", "Abrir captura Formulario 5", class = "btn-primary")
            )
          )
        ),
        h4("Estado del envío"),
        verbatimTextOutput("submission_status")
      ))
    }

    if (dataset != "egg_count_raw") {
      dataset_label <- switch(
        dataset,
        adult_count_raw = "Conteo de adultos - datos crudos",
        bioassay_raw = "Bioensayo - datos crudos"
      )

      return(wellPanel(
        h4(paste(dataset_label, "- formulario pendiente")),
        p("La ruta del menú ya está lista. El siguiente paso es construir los campos de captura para este formulario."),
        p("La tabla de ingreso en Supabase para Formulario 5 ya está creada como public.formulario_5_alimentacion_conteo_intake.")
      ))
    }

    tagList(
      fluidRow(
        column(
          width = 6,
          div(
            class = "capture-option-card",
            h4("Subida de datos masiva"),
            p("Cargue varios registros desde un archivo CSV usando el machote oficial."),
            actionButton("open_bulk_upload", "Abrir subida masiva", class = "btn-primary")
          )
        ),
        column(
          width = 6,
          div(
            class = "capture-option-card",
            h4("Ingreso individual de datos"),
            p("Ingrese una observación a la vez mediante el formulario guiado."),
            actionButton("open_individual_entry", "Abrir ingreso individual", class = "btn-primary")
          )
        )
      ),
      h4("Estado del envío"),
      verbatimTextOutput("submission_status")
    )
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
    req(active_dataset() %in% c("egg_count_raw", "formulario_5_alimentacion_conteo"))
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
