# Formulario 5

## Estructura

- Secciones:
  - Metadatos
  - Datos generales
  - Alimentacion sanguinea
  - Conteo de huevecillos
  - Observaciones
- Campos clave:
  - `formulario_codigo`, `pais`, `ciclo`, `formulario_nombre`, `fecha_registro`
  - `cepa_poblacion`, `especie`, `generacion_filial_adultos`, `responsable_ingreso_jaula`
  - `fecha_jaula`, `numero_hembras`, `numero_machos`, `total_individuos`
  - `responsable_alimentacion`, `tipo_alimentacion_codigo`, `tipo_alimentacion_descripcion`
  - `fecha_alimentacion_sangre`, `numero_charolas`, `observaciones_alimentacion`
  - `generacion_filial_huevos`, `codigo_sustrato`, `fecha_colocacion_sustrato`, `fecha_retiro_sustrato`
  - `hv_huevos_viables`, `he_huevos_eclosionados`, `hc_huevos_canoa`, `hnf_huevos_no_fecundados`, `total_huevos`
- Validaciones:
  - especie solo admite `Ae. aegypti` o `Ae. albopictus`
  - codigo de alimentacion solo admite `A` a `E`
  - `total_individuos` se genera como suma de hembras y machos
  - `total_huevos` se genera como suma de las cuatro categorias de huevo
  - `fecha_colocacion_sustrato <= fecha_retiro_sustrato`
- Relaciones:
  - `formulario_5_alimentacion_conteo` es la tabla plana historica
  - `formulario_5_alimentacion_conteo_intake` es la tabla de captura con revision
- Observaciones:
  - el ciclo base del proyecto es `Ciclo 3`
  - la aplicacion conserva el machote CSV oficial para subida masiva
  - la version intake agrega `review_status`, `review_notes`, `reviewed_by` y `reviewed_at`
  - la captura guiada separa el formulario en cinco pasos: metadatos, datos generales, alimentacion, conteo y observaciones
