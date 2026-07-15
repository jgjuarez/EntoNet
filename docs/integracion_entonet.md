# Integracion de trabajo EntoNet

## Objetivo

Unificar el trabajo de EntoNet entre la computadora de oficina, la computadora personal, GitHub y Supabase.

## Fuente central

GitHub debe funcionar como la fuente central del proyecto:

- Documentos de formularios y diccionarios de datos.
- Codigo de la aplicacion Shiny.
- Scripts SQL de Supabase.
- Documentacion de procedimientos.

Supabase debe funcionar como la base operacional donde el personal ingresa y consulta datos.

## Plan por etapas

### 1. Consolidar archivos locales

En esta computadora ya existe:

- `base_datos_entonet/formulario_5_alimentacion_conteo.csv`
- `base_datos_entonet/diccionario_formulario_5.csv`
- `base_datos_entonet/listas_validacion_formulario_5.csv`

Pendiente por traer desde la computadora personal:

- Codigo de la aplicacion Shiny.
- Cualquier version previa de formularios, CSV, Excel, Word o PDF.
- Notas de procedimientos o capturas.

### 2. Normalizar estructura

Ubicaciones recomendadas:

- Codigo Shiny: `shiny_app/`
- Scripts SQL: `supabase/`
- Diccionarios y CSV: `base_datos_entonet/`
- Procedimientos y decisiones: `docs/`

### 3. Preparar Supabase

Crear tablas a partir del diccionario de datos. Para el Formulario 5 se inicia con una tabla plana compatible con el CSV actual; despues se puede normalizar a tablas relacionadas:

- formularios
- jaulas
- alimentaciones
- sustratos
- conteos_huevos
- responsables

### 4. Conectar Shiny

El Shiny app debe conectarse a Supabase usando variables de entorno:

- `SUPABASE_URL`
- `SUPABASE_ANON_KEY`
- `SUPABASE_SERVICE_ROLE_KEY` solo para procesos administrativos, no para clientes publicos.

Las claves no deben guardarse en GitHub.

### 5. Sincronizacion entre computadoras

Cuando Git este disponible en ambas computadoras:

```powershell
git clone <url-del-repositorio>
git pull
git add .
git commit -m "Actualiza estructura EntoNet"
git push
```

Si una computadora ya tiene archivos locales, primero se deben copiar a una carpeta temporal y comparar antes de sobrescribir.

## Informacion que falta confirmar

- URL exacta del repositorio GitHub.
- Si Supabase ya tiene proyecto creado y nombre del proyecto.
- Si la app Shiny usa `DBI`, `RPostgres`, `httr2`, `supabaseR` u otro metodo de conexion.
- Si la captura en linea sera solo interna o tambien accesible fuera de la red del laboratorio.

