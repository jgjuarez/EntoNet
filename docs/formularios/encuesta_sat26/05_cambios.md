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

## 2026-08-18

- Se reemplazó la exportación de selecciones múltiples separadas por `;` por columnas binarias `1/0`.
- Se mantuvieron intactos los arreglos JSONB originales en `public.encuesta_sat26_intake` como fuente primaria.
- Se agregó `public.encuesta_sat26_export`, una vista plana con una fila por `codigo_unico` y acceso restringido a `service_role`.
- Se amplió el diccionario con `option_value` y `option_label`.
- Se agregó `encuesta_sat26_opciones_multiples.csv` para documentar las 227 columnas binarias derivadas.
- El campo técnico heredado `plan_caracteristicas`, que ya no tiene control visible, se excluyó de la exportación plana; permanecen las variables vigentes para planes de *Aedes*, *Anopheles* e integrado.
- Se agregó la opción administrativa `Solicitudes > Encuestas`, visible únicamente para la cuenta personal administradora autorizada.
- La tarjeta `Encuesta de satisfacción 2026` permite descargar en CSV la vista protegida `public.encuesta_sat26_export` directamente desde Supabase.
- La descarga mantiene una fila por `codigo_unico`, las 530 columnas analíticas y las selecciones múltiples separadas en columnas binarias `1/0`.
- El inicio de sesión consulta `usuario_perfil` mediante la API privada de Supabase y deja la conexión PostgreSQL directa únicamente como respaldo, evitando bloquear el acceso cuando las credenciales del pooler están desactualizadas.
- La pantalla administrativa de descarga de encuestas adoptó la misma estructura visual de `Datos - Campo`: panel de selección, alcance por país e institución, botón CSV y resumen del perfil activo.
