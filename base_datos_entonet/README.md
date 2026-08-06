# Base de datos EntoNet

Este directorio contiene la primera plantilla CSV para digitalizar el formulario:

**REI Ciclo 3 - Formulario 5. Alimentacion sanguinea y conteo huevecillos Aedes spp.**

Archivos:

- `formulario_5_alimentacion_conteo.csv`: tabla principal para captura inicial. La unidad de registro recomendada es una fila por conteo de huevecillos, repitiendo los datos generales y de alimentacion que correspondan.
- `diccionario_formulario_5.csv`: definicion de campos, tipos de datos, obligatoriedad y descripcion.
- `listas_validacion_formulario_5.csv`: catalogos iniciales para especies y tipos de alimentacion.

Notas de captura:

- Usar fechas en formato `yyyy-mm-dd`.
- `total_individuos` debe corresponder a `numero_hembras + numero_machos`.
- `total_huevos` debe corresponder a `hv_huevos_viables + he_huevos_eclosionados + hc_huevos_canoa + hnf_huevos_no_fecundados`.
- Los codigos de alimentacion del formulario son: A = conejo, B = humano, C = hemotek-conejo, D = hemotek-humano, E = hemotek-carnero.
- Las abreviaturas del conteo son: HV = huevos viables, HE = huevos eclosionados, HC = huevos en canoa, HNF = huevos no fecundados.

Esta version esta pensada como punto de partida. Cuando se definan los demas formularios de EntoNet, conviene separar la base final en tablas relacionadas: formularios, jaulas, alimentaciones, sustratos, conteos y usuarios/responsables.

## Formulario 1

Archivos:

- `formulario_1_colocacion_retiro_ovitrampa.csv`: plantilla plana para captura inicial. La unidad de registro es una ovitrampa/sustrato por fila, repitiendo los datos generales del formulario fisico.
- `diccionario_formulario_1.csv`: definicion de campos para metadatos, colocacion, coordenadas GPS, conteos, retiro y detalle de ovitrampas.
- `listas_validacion_formulario_1.csv`: catalogo de estados de retiro.

Notas de captura:

- Usar fechas en formato `yyyy-mm-dd`.
- `Latitud`, `Longitud` y `codigo_gps` pertenecen a la sección de colocación.
- `cuadrante` y `codigo_casa` se definen una vez por ingreso en la sección de colocación y aplican a todas las ovitrampas generadas.
- `Ovitrampas_colocadas` es obligatorio, debe ser un entero mayor que cero y determina cuantas filas de ovitrampa/sustrato deben registrarse.
- `Ovitrampas_retiradas` debe ser un entero igual o mayor que cero cuando aplique y no puede superar `Ovitrampas_colocadas`.
- Los estados de retiro se capturan como conteos; su suma no puede superar `Ovitrampas_retiradas`.
- `codigo_formulario` se pre-popula con el prefijo ReiSV y el digitador completa el resto del identificador.
- `codigo_sustrato` mantiene prefijo, digitos y letra final A-H, por ejemplo `SV001A`, `GT001A` o `HS010A`.
- `fuente_formulario` registra la versión del formulario utilizada.
- `creado_por` registra el nombre de quien ingresó el formulario.

## Formulario 7

Archivos:

- `formulario_7_bioensayo_botella_cdc.csv`: plantilla plana de 121 campos para el registro completo de un bioensayo de botella CDC. La unidad de registro es un formulario o bioensayo por fila y `codigo_unico` se genera automáticamente usando el código de bioensayo final: D para Diagnóstica, I más la dosis para Intensidad, o S más los sinergistas seleccionados.
- `diccionario_formulario_7.csv`: definición de los 121 campos, incluidos CodigoUnico, país, códigos de departamento, municipio y crianza, datos del proyecto, tipo y modalidad del bioensayo, clasificación diagnóstica, material biológico, responsables, condiciones ambientales, resultados de cuatro botellas experimentales y un control, KDR fenotípico y comentarios. En Intensidad Exploratorio, b1=1X, b2=2X, b3=5X, b4=10X y c1 es el único control utilizado.

Notas de captura:

- Usar fechas en formato `yyyy-mm-dd` y horas en formato `hh:mm` de 24 horas.
- Los conteos de mosquitos deben ser enteros iguales o mayores que cero.
- Los campos repetidos usan los códigos `b1` a `b4` para botellas expuestas y `c1` a `c2` para controles.
- Los sufijos `vivos` e `incapacitados` corresponden a las abreviaturas V e I del formulario.
- Los resultados están ordenados por botella: hora de inicio, lecturas de 0, 15, 30 y 45 minutos, y lectura KDR a las 24 horas; después inicia el bloque de la siguiente botella.
- La plantilla conserva una fila por bioensayo para facilitar la captura inicial; para la base final se recomienda normalizar las lecturas repetidas en una tabla relacionada por bioensayo, botella, tiempo y estado.
