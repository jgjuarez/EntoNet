# Resumen

La Encuesta SAT26 corresponde al instrumento de "Needs Assessment for surveillance and control of mosquito-borne disease vectors" adaptado para EntoNet.

## Fuentes

- `0_Need Assesment Protocol_V2_28_07_2025.pdf`
- `1_Annex 1 Key Informant Interview Consent Form_V2(ESP).pdf`
- `2_Annex 2 Needs Assessment Instrument_V3(ESP).pdf`

## Hallazgos clave

- El consentimiento es una pantalla/flujo previo al cuestionario.
- La encuesta principal es extensa y está organizada por secciones A-K.
- La captura incluye:
  - opciones de selección única,
  - selección múltiple,
  - campos de texto libre,
  - entradas numéricas,
  - enlaces a Dropbox,
  - carga de archivos,
  - lógica condicional por respuesta.
- El instrumento menciona 60 minutos aproximados de duración.
- La encuesta es pública y no requiere usuario para responder.

## Secciones identificadas

- A. Informe de situación de enfermedades transmitidas por vectores
- B. Gobernanza
- C. Finanzas
- D. Recursos Humanos
- E. Actividades de Control de Vectores de Mosquitos
- F. Actividades de Vigilancia Vectorial
- G. Infraestructura y Logística
- H. Sistemas de información
- I. Participación comunitaria y comunicación de riesgos
- J. Investigación operativa liderada por el Ministerio de Salud
- K. Colaboración regional

## Recomendación de implementación

1. Construir primero el flujo de consentimiento.
2. Separar la encuesta por secciones para que sea fácil de navegar.
3. Guardar respuestas crudas en una sola tabla JSONB al inicio.
4. Normalizar después solo si una sección lo requiere por reportes o análisis.
