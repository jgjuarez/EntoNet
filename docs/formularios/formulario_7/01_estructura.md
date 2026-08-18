# Formulario 7

## Estructura

- Secciones:
  - Informacion general
  - Informacion del bioensayo
  - Informacion del material biologico
  - Responsables
  - Condiciones ambientales
  - Horario del bioensayo
  - Resultados por botella
  - Comentarios
- Campos clave:
  - `formulario_codigo`, `formulario_nombre`, `fecha_registro`
  - `nombre_poblacion`, `id_proyecto`, `codigo_bioensayo`, `codigo_unico`
  - `modalidad_bioensayo`, `dosis_diagnostica_1x`, `dosis_intensidad`
  - `sinergista_def`, `sinergista_pbo`, `sinergista_dm`, `resultado_diagnostico`
  - `codigo_insecticida`, `solvente_utilizado`, `solvente_otro`, `dosis_ug_ml`, `codigo_dosis`
  - `origen_material`, `codigo_pais`, `codigo_departamento`, `codigo_municipio`, `codigo_crianza`
  - `temperatura_inicial_c`, `temperatura_final_c`, `humedad_relativa_inicial_pct`, `humedad_relativa_final_pct`
  - resultados `b1` a `b4` y `c1` a `c2`
  - `comentario_1` a `comentario_4`
- Validaciones:
  - `modalidad_bioensayo` solo admite `Exploratorio` o `Completa`
  - diagnostica 1X requiere `resultado_diagnostico`
  - intensidad requiere `dosis_intensidad` solo en modalidad completa
  - sinergistas no se combinan con dosis de intensidad
  - si `solvente_utilizado` es `Otro`, `solvente_otro` debe tener valor
  - `edad_dias` y `generacion_filial` dependen de sus banderas de indefinicion
  - lecturas de resultados se normalizan por `fase`, `botella` y `tiempo_minutos`
- Relaciones:
  - `formulario_7_bioensayo_intake` es el encabezado
  - `formulario_7_bioensayo_resultado_intake` guarda lecturas normalizadas
  - `formulario_7_bioensayo_comentario_intake` guarda hasta cuatro comentarios numerados
- Observaciones:
  - `codigo_unico` se genera como `codigo_crianza-codigo_bioensayo_final-fecha`
  - la captura visual conserva una fila por bioensayo para facilitar el flujo de usuario
  - la version intake separa resultados y comentarios para apoyar la revision
  - los tabs de resultados se organizan por botella y cambian segun el tipo de ensayo
