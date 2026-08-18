# Navegacion

## Patrón general

- Inicio con datos generales o metadatos.
- Continuacion por bloques tematicos.
- Confirmacion o avance antes de liberar el siguiente bloque.
- Guardado final como registro pendiente.

## Aplicacion por formulario

- Formulario 1:
  - Datos generales
  - Colocación
  - Cuadrantes
  - Guardado
- Formulario 5:
  - Metadatos
  - Datos generales
  - Alimentación
  - Conteo de huevecillos
  - Observaciones
  - Guardado
- Formulario 7:
  - Información general
  - Información del bioensayo
  - Material y responsables
  - Condiciones
  - Resultados por botella
  - Comentarios y envío

## Regla reutilizable

- El usuario nunca debe ver todo el formulario sin contexto.
- Cada bloque debe tener un objetivo claro y una accion de avance.
- Si el formulario es largo, usar tabs o pasos con estado visible.
