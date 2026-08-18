# Formulario 5

## Soluciones aplicadas

- Problema: se necesita revisar y cargar registros sin exponer la base operacional.
- Solucion: separar tabla plana historica y tabla intake con `review_status`.
- Motivo: permite validacion antes de usar los datos en analisis.
- Problema: el formulario depende de sumas derivadas que no deberian editarse manualmente.
- Solucion: usar columnas generadas para `total_individuos` y `total_huevos`.
- Motivo: evita errores de captura y mantiene coherencia en la base.
- Problema: el CSV masivo necesita ser compatible con revision posterior.
- Solucion: el intake conserva la estructura y agrega columnas de auditoria y estado.
- Motivo: facilita control antes de promover datos a analisis.
