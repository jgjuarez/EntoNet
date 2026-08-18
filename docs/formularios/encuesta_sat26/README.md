# Encuesta SAT26

Carpeta de trabajo para migrar el cuestionario de REDCap al sitio de EntoNet y almacenar respuestas en Supabase.

## Objetivo

- Replicar la encuesta en una interfaz pública sin login.
- Enviar las respuestas a Supabase.

## Enlace público

La encuesta puede abrirse sin login usando el parámetro:

```text
?survey=sat26
```

En local, el enlace esperado es:

```text
http://127.0.0.1:3840/?survey=sat26
```

También se acepta:

```text
?sat26=1
```

Este flujo renderiza una página pública dedicada de SAT26, sin sidebar ni botones de perfil/cierre de sesión.

## Comunicación con Supabase

- El código único visible al participante se genera desde la secuencia central `public.encuesta_sat26_codigo_seq`.
- Las respuestas finales se guardan en `public.encuesta_sat26_intake`.
- `codigo_unico` es el identificador lógico único.
- Al finalizar, la app hace `upsert` por `codigo_unico`.
- La opción de continuar con un código existente busca primero en Supabase y luego en el borrador local del navegador.
- El JSONB de `public.encuesta_sat26_intake` conserva las selecciones múltiples originales como arreglos.
- La vista `public.encuesta_sat26_export` presenta cada opción múltiple en una columna binaria (`1` seleccionada, `0` no seleccionada y `NULL` si la pregunta no existe en el registro).
- La vista analítica usa `security_invoker = true` y solo concede lectura a `service_role`; no está expuesta a participantes anónimos.
- Documentar el mapeo de preguntas, validaciones y lógica de salto.

## Archivos previstos

- `00_resumen.md`
- `01_estructura.md`
- `02_consultas.sql`
- `03_soluciones.md`
- `04_estilos.md`
- `05_cambios.md`
- `06_guia_elaboracion_cuestionarios.md`
- `encuesta_sat26_columnas_captura.csv`
- `encuesta_sat26_opciones_multiples.csv`
- `encuesta_sat26_captura_template.csv`

## Estado inicial

- Revisión del enlace público de REDCap: no accesible desde este entorno.
- Se creará primero una tabla flexible de captura para poder avanzar con la integración.
