# Shiny app EntoNet

Colocar aqui el codigo de la aplicacion Shiny cuando se traiga desde la computadora personal.

Estructura esperada, si es una app Shiny simple:

- `app.R`
- `global.R` opcional
- `R/` para funciones auxiliares
- `www/` para archivos estaticos

Estructura esperada, si es una app modular:

- `app.R`
- `R/mod_*.R`
- `R/db_*.R`
- `R/utils_*.R`

No guardar claves de Supabase dentro del codigo. Usar `.Renviron` local o variables de entorno.

