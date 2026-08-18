# Validaciones

## Validaciones recurrentes

- Fechas con formato consistente.
- Enteros iguales o mayores que cero.
- Campos obligatorios antes de avanzar.
- Catálogos cerrados para pais, especie, tipo o modalidad.
- Coherencia entre campos relacionados.
- Sumas derivadas que no deben capturarse manualmente.

## Ejemplos ya usados

- Formulario 1:
  - `ovitrampas_colocadas > 0`
  - `fecha_colocacion <= fecha_retiro`
  - suma de estados de retiro controlada
- Formulario 5:
  - `total_individuos = numero_hembras + numero_machos`
  - `total_huevos = HV + HE + HC + HNF`
  - fechas de sustrato coherentes
- Formulario 7:
  - modalidad y tipo de bioensayo consistentes
  - solvente otro requiere texto adicional
  - edad y generacion dependen de banderas de indefinicion

## Regla reutilizable

- Las validaciones deben existir en UI y en base de datos.
- Los calculos derivados deben ser automaticos.
- Las inconsistencias deben mostrarse antes de guardar.
