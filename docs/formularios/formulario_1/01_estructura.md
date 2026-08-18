# Formulario 1

## Estructura

- Secciones:
  - Metadatos
  - Colocacion de ovitrampa
  - Retiro de ovitrampa/sustrato
  - Informacion de ovitrampas
  - Auditoria
- Campos clave:
  - `formulario_codigo`, `formulario_nombre`, `fecha_registro`
  - `pais`, `departamento`, `municipio`, `ciclo`, `ronda`, `codigo_formulario`
  - `fecha_colocacion`, `grupo_responsable_colocacion`, `cuadrante`, `codigo_casa`
  - `latitud`, `longitud`, `codigo_gps`, `ovitrampas_colocadas`
  - `fecha_retiro`, `grupo_responsable_retiro`, `ovitrampas_retiradas`
  - `retiro_buen_estado`, `retiro_sin_agua`, `retiro_sin_sustrato`, `retiro_sin_ovitrampa`, `retiro_movida`, `retiro_volteada`, `retiro_casa_cerrada`
  - `codigo_sustrato`, `fuente_formulario`, `creado_por`
- Validaciones:
  - `ovitrampas_colocadas > 0`
  - `latitud` entre -90 y 90, `longitud` entre -180 y 180
  - `fecha_colocacion <= fecha_retiro` cuando exista retiro
  - `ovitrampas_retiradas <= ovitrampas_colocadas`
  - la suma de estados de retiro no puede superar las ovitrampas colocadas
  - `codigo_sustrato` debe ser unico dentro del ingreso
- Relaciones:
  - `formulario_1_ovitrampa_intake` guarda el encabezado
  - `formulario_1_ovitrampa_detalle_intake` guarda un registro por sustrato
- Observaciones:
  - el codigo de formulario se prellena con `ReiSV` en la captura historica
  - `cuadrante` y `codigo_casa` se repiten para todas las ovitrampas del mismo ingreso
  - la pantalla de impresion genera el codigo de formulario nuevo con pais, municipio, ciclo y ronda
  - la captura principal usa tabs por cuadrante y luego por casa
