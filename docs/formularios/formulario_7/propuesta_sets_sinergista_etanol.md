# Propuesta de estructura: Formulario 7 con sinergista y etanol

Estado: borrador para revisión, 10 de septiembre de 2026. No es una migración y no se ha aplicado a Supabase. No modifica la captura ni la impresión actuales.

## Base de la propuesta

Plantilla revisada: `formulario_7_REI26SV0202P1DEFDEL1F1.xlsx`, hoja `Formulario 7`, versión impresa 2. La última revisión tiene 42 filas y 14 columnas: incorpora los encabezados 9.1 SINERGISTA y 9.2 Control EtOH en la fila 32. El esquema actual se reconstruyó desde las migraciones locales y `shiny_app/app.R`; todavía no se contrastó con el catálogo de la base remota.

| Ubicación de la plantilla | Contenido observado | Necesidad de almacenamiento |
| --- | --- | --- |
| A25:G29 | Sinergista, E1–E5, inicio, vivos e incapacitados a 60 minutos, observaciones | Set sinergista y etapa de pretratamiento |
| H25:N29 | Control EtOH, E1–E5, inicio, vivos e incapacitados a 60 minutos, observaciones | Set etanol y etapa de pretratamiento |
| A32:G42 | 9.1 SINERGISTA: SinE1–SinE4/SinC1; lecturas 0, 15, 30 y 45 minutos | Continuación del set de 8.1 |
| H32:N42 | 9.2 Control EtOH: EtOHE1–EtOHE4/EtOHC1; lecturas 0, 15, 30 y 45 minutos | Continuación del set de 8.2 |
| A31 | Texto «debe esperar 60min adicionales» | Aclarar el intervalo antes de imponer una validación temporal |
| A16:K16 | Una sola línea de usos E1–E4/C1 | Aclarar si se ampliará para distinguir botellas de ambos sets y etapas |
| H20 | Responsable de revisión a 24 h | La plantilla no contiene una tabla de resultados a 24 h; confirmar si se conserva |

La plantilla es evidencia de los campos y del diseño propuesto. Sus notas no se interpretan como autorización para cambiar el protocolo o ejecutar cambios remotos.

## Decisión principal

Conservar un encabezado y un `codigo_bioensayo` para el ensayo completo. Debajo del mismo registro, guardar dos sets: `sinergista` y `etanol`. Cada set tiene botellas y lecturas propias. El set etanol no se representa mediante C1 del set sinergista: la sección 9 muestra un C1 para cada set.

Separar también la etapa: `pretratamiento`, `bioensayo` y, si se confirma su uso, `kdr_24h`. El minuto 60 de pretratamiento y el minuto 0 del bioensayo tienen orígenes temporales distintos.

Se propone extender la base existente, no crear otra base ni duplicar toda la información del ensayo.

### Aclaración del flujo por el usuario

La sección 8.1 registra los mosquitos expuestos previamente al sinergista. Esos mosquitos pasan después a la sección 9.1, SinE1–SinE4 y SinC1. La sección 8.2 registra los mosquitos expuestos previamente al etanol, que continúan en la sección 9.2, EtOHE1–EtOHE4 y EtOHC1. Los nuevos encabezados de la fila 32, junto con la aclaración del usuario, resuelven la identificación de ambas series.

| Set persistente | Exposición previa | Lectura posterior |
| --- | --- | --- |
| sinergista | 8.1, E1–E5, lectura a 60 minutos | 9.1, SinE1–SinE4/SinC1, lecturas a 0/15/30/45 minutos |
| etanol | 8.2, E1–E5, lectura a 60 minutos | 9.2, EtOHE1–EtOHE4/EtOHC1, lecturas a 0/15/30/45 minutos |

Los números 8.1/9.1 y 8.2/9.2 son etiquetas de presentación; no crean cuatro sets. El modelo conserva dos sets con sus respectivas etapas. La nueva fila aclara el flujo sin agregar conteos ni cambiar las 50 lecturas disponibles.

Por tanto, el set identifica el tratamiento previo de los mosquitos y se conserva entre etapas. La etiqueta de la botella identifica su recipiente en cada etapa. Esta aclaración confirma continuidad de los grupos, pero todavía no especifica la distribución individual de cada botella inicial hacia las posteriores, ni si hay mezcla o redistribución.

## Modelo lógico propuesto

Los nombres siguientes son una propuesta, no tablas ya creadas.

### 1. Encabezado existente: formulario_7_bioensayo_intake

Conservar `intake_id`, código único de bioensayo, país, institución, población, material biológico, responsables, sinergista y dosis, insecticida y dosis, revisión y auditoría.

Agregar una versión de estructura de captura independiente de la versión impresa. Esto permite reconocer registros históricos incompletos para el nuevo diseño sin tratarlos como capturas nuevas de dos sets.

### 2. Nueva tabla: formulario_7_bioensayo_set_intake

| Campo | Tipo lógico | Regla o significado |
| --- | --- | --- |
| set_id | bigint, PK | Identificador interno |
| intake_id | bigint, FK al encabezado | Ensayo al que pertenece |
| tipo_set | text | `sinergista`, `etanol`, `estandar` o `historico_sin_clasificar` |
| observaciones_pretratamiento | text, nullable | Columna G o N de la sección 8, según el set |
| observaciones_bioensayo | text, nullable | Observación específica del set, cuando se capture |
| creado_en | timestamptz | Auditoría |

Unicidad: `(intake_id, tipo_set)`. Las capturas nuevas de sinergistas requieren exactamente los dos sets `sinergista` y `etanol`. Diagnóstica e intensidad utilizan `estandar`. `historico_sin_clasificar` se reserva para migración revisada; no se ofrece al capturista.

La observación común de la sección 9 puede conservarse en el comentario del ensayo. No duplicarla automáticamente en ambos sets.

### 3. Nueva tabla: formulario_7_bioensayo_botella_etapa_intake

Una fila representa una botella dentro de un set y una etapa. El set conserva la procedencia de los mosquitos confirmada por el usuario; las botellas de las dos etapas se identifican por separado. Esta separación permite que E5 y C1 tengan etiquetas distintas sin asumir que son la misma botella física.

| Campo | Tipo lógico | Regla o significado |
| --- | --- | --- |
| botella_etapa_id | bigint, PK | Identificador interno |
| set_id | bigint, FK al set | Distingue sinergista y etanol |
| etapa | text | `pretratamiento`, `bioensayo`, `kdr_24h` |
| botella | text | Etiqueta de la etapa: `e1`–`e5` o `c1`, según corresponda |
| inicio_etapa | timestamp sin zona, nullable durante borrador | Fecha y hora local de inicio; no confundir con hora de lectura |
| numero_usos | integer, nullable | Mayor o igual a cero; solo si se captura para esa botella |
| fecha_revestimiento | date, nullable | Solo cuando se registre por botella/etapa |

Unicidad: `(set_id, etapa, botella)`. Para esta plantilla, pretratamiento muestra E1–E5 y bioensayo E1–E4/C1. La continuidad del set está confirmada; la correspondencia botella por botella queda pendiente. No crear una relación individual E5 → C1 sin confirmación. Si se necesita trazabilidad individual del traslado, definirla después de aclarar si es uno a uno o hay redistribución, sin inventar una relación a partir de la numeración.

Las fechas y horas se capturan en hora local del ensayo. No convertirlas automáticamente a la zona horaria del servidor. Los instantes de auditoría siguen siendo `timestamptz`.

Los campos de usos del encabezado actual se preservan durante la transición. No copiarlos a las diez botellas o a todas las etapas, pues la fuente no acredita esa equivalencia. Si se adopta captura por botella, esta será la fuente canónica para registros nuevos.

### 4. Adaptar formulario_7_bioensayo_resultado_intake

| Campo | Tipo lógico | Regla o significado |
| --- | --- | --- |
| resultado_id | bigint, PK | Preservar identificadores existentes |
| botella_etapa_id | bigint, FK | Obtiene ensayo, set, etapa y botella a través de relaciones |
| tiempo_minutos | integer | Tiempo relativo al inicio de esa etapa |
| vivos | integer | Obligatorio en una lectura registrada, mayor o igual a cero |
| incapacitados | integer | Obligatorio en una lectura registrada, mayor o igual a cero |
| fecha_hora_lectura | timestamp sin zona, nullable | Hora real si se recoge; no inventarla a partir del inicio |
| observaciones | text, nullable | Observación de esa lectura, si aplica |
| creado_en | timestamptz | Auditoría existente |

Nueva unicidad lógica: `(botella_etapa_id, tiempo_minutos)`. Reemplaza la actual `(intake_id, fase, botella, tiempo_minutos)` tras migrar y verificar datos y consumidores. No mantener dos fuentes editables de set/etapa/ensayo que puedan contradecirse.

Durante la transición pueden conservarse las columnas antiguas para compatibilidad, con correspondencia controlada y sin escrituras independientes. El cambio de interfaz debe coordinarse con el RPC y la aplicación.

## Ejemplos de identidad de lectura

Ejemplos estructurales; no son datos experimentales.

| Código | Set | Etapa | Botella | Minuto |
| --- | --- | --- | --- | --- |
| REI26SV0202P1DEFDEL1F1 | sinergista | pretratamiento | e1 | 60 |
| REI26SV0202P1DEFDEL1F1 | etanol | pretratamiento | e1 | 60 |
| REI26SV0202P1DEFDEL1F1 | sinergista | bioensayo | e1 | 0 |
| REI26SV0202P1DEFDEL1F1 | etanol | bioensayo | e1 | 0 |
| REI26SV0202P1DEFDEL1F1 | sinergista | bioensayo | c1 | 30 |
| REI26SV0202P1DEFDEL1F1 | etanol | bioensayo | c1 | 30 |

La sección 8 permite 2 sets × 5 botellas × 1 tiempo = 10 lecturas. La sección 9 permite 2 × 5 × 4 = 40 lecturas. Total de la plantilla: 50 lecturas con 100 conteos entre vivos e incapacitados, sin incluir 24 horas. Esto describe los espacios disponibles; la obligatoriedad depende del protocolo confirmado.

## Validación y compatibilidad que debe cubrir la implementación posterior

- Guardar encabezado, sets, botellas y lecturas en una sola transacción. Si falla un set, no dejar el ensayo parcialmente ingresado.
- Validar al enviar la composición de sets y las combinaciones de etapa, botella y tiempo. Las reglas entre tablas requieren validación transaccional en el RPC o triggers; no basta con un CHECK de una sola fila.
- Aceptar cero como valor medido y conservar ausencias como ausencias. Nunca rellenar etanol con ceros ni copiar resultados del sinergista.
- Distinguir ensayos antiguos y nuevos. Revisar antes de migrar qué representan las lecturas históricas de 60 minutos. Una lectura de 60 minutos no prueba por sí sola el set de origen.
- Preservar IDs, código, estado de revisión, autoría y comentarios históricos. No borrar ni reconstruir tablas con DROP/CASCADE.
- Mantener relaciones de pertenencia por FK y evitar huérfanos. La eliminación debe respetar el flujo de auditoría actual antes de definir cascadas nuevas.
- Conservar el acceso de servidor de EntoNet y habilitar RLS en tablas nuevas. No conceder acceso público por este cambio.
- Actualizar `entonet_insert_formulario_7`: actualmente inserta explícitamente las columnas antiguas; agregar columnas a la tabla no actualiza el RPC automáticamente.
- Actualizar captura, edición, carga masiva, exportación, impresión, consultas de revisión, visualizaciones y eliminación auditada. Filtrar por set y etapa en estadísticas para evitar combinar ambos tratamientos.
- Versionar el diccionario de exportación. Si se requiere formato ancho, derivar campos como `etanol_bioensayo_e1_30min_vivos` desde el modelo normalizado.
- Comparar conteos y valores antes/después de migrar. Verificar que la misma botella y minuto se guardan para ambos sets y que un duplicado dentro del mismo set/etapa se rechaza.

## Decisiones pendientes de confirmar

1. Confirmar si el traslado individual es E1 → E1, E2 → E2, E3 → E3, E4 → E4 y E5 → C1 en cada set, o si hay redistribución. El flujo entre secciones ya está aclarado: 8.1 → 9.1 y 8.2 → 9.2.
2. ¿Se mantiene lectura de 24 horas en ambos sets? La nueva plantilla conserva responsable, pero no las filas de resultados.
3. ¿Los 60 minutos adicionales son una espera entre pretratamiento e insecticida? Hasta aclararlo, no fijar una espera obligatoria ni calcular horas automáticamente.
4. ¿El registro de usos y revestimiento debe separarse por set y etapa? La plantilla todavía presenta una sola línea.

## Alcance de esta entrega

Solo propuesta local. Antes de preparar SQL ejecutable: resolver las decisiones anteriores y contrastar, mediante lectura, el esquema remoto con las migraciones locales. La migración, los cambios de aplicación y el despliegue quedan para una etapa posterior autorizada.
