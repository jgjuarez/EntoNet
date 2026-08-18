# Cambios

## 2026-08-14

- Se creó la carpeta de trabajo para la Encuesta SAT26.
- Se incorporó una tabla flexible de captura en Supabase.
- Se documentó la estructura inicial del instrumento a partir de los tres PDF fuente.

## 2026-08-15

- Se integró el botón final de la encuesta SAT26 con Supabase.
- La tabla `public.encuesta_sat26_intake` usa `codigo_unico` como identificador lógico único de la respuesta.
- El envío final realiza `upsert` por `codigo_unico`, evitando duplicados si una persona vuelve a finalizar con el mismo código.
- Se generaron los CSV de estructura plana:
  - `encuesta_sat26_columnas_captura.csv`
  - `encuesta_sat26_captura_template.csv`
- Se normalizó la numeración visible y los nombres de columnas para las preguntas C3/C4, F5b y K3/K4.
- Se agregó una vista pública de la encuesta para abrir SAT26 sin login con `?survey=sat26` o `?sat26=1`.
- Se agregó una secuencia central en Supabase para generar códigos únicos SAT26 sin depender del navegador.
- La recuperación por código ahora consulta primero Supabase y luego el borrador local del navegador.
