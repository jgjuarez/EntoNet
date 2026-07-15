# Base de datos EntoNet - Formulario 5

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
