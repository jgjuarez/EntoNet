# Full EntoNet app with local F7 capture and local data requests, no Supabase credentials.
Sys.setenv(ENTONET_LOCAL_F7 = "1", ENTONET_SKIP_LOGIN = "1",
  PROJECT_REI_PROFILE_NAME = "Revisión local", PROJECT_REI_PROFILE_POSITION = "Administrador",
  PROJECT_REI_PROFILE_INSTITUTION = "UVG", PROJECT_REI_PROFILE_COUNTRY = "Guatemala")
shiny::runApp("shiny_app", host = "127.0.0.1", port = 3877, launch.browser = FALSE)
