# Guardado y revision

## Patrón general

- Guardar primero en una tabla intake.
- Marcar como `pending` al ingresar.
- Permitir revision posterior con filtros.
- Confirmar, editar o rechazar desde un modal dedicado.
- Guardar motivo cuando se elimina un registro.

## Aplicacion por formulario

- Formulario 1:
  - carga parcial por codigo
  - confirmacion, edicion y eliminacion con auditoria
- Formulario 5:
  - redigitacion para comparar diferencias
  - muestra aleatoria del 10 por ciento
- Formulario 7:
  - busqueda por codigo de bioensayo
  - confirmacion de registro a `reviewed`
  - borrado con comentario

## Regla reutilizable

- El usuario no debe escribir directamente sobre la tabla final.
- La tabla intake debe conservar el contexto minimo para auditar.
- Revisar no es solo ver, tambien validar y dejar trazabilidad.
