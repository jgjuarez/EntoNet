# Formulario 7: tres componentes de captura local

## Fuente y alcance

Se revisó `formulario_7_insectario_todos_paises_todas_instituciones_20260907.csv`: 106 filas y 119 columnas. Sus indicadores corresponden a 68 registros de Diagnóstica, 14 de Intensidad y 24 de Sinergistas. El archivo original se mantiene intacto; no se importó ni se dividieron registros históricos automáticamente.

La captura está integrada en el app Shiny completo bajo el modo local `ENTONET_LOCAL_F7=1`. Reutiliza la definición de captura individual de `app.R`, con un módulo que adapta los campos al tipo de bioensayo. El lanzador local omite el inicio de sesión y la lectura de credenciales Supabase.

## Abrir

Desde EntoNet, ejecutar `Rscript scripts/run_entonet_f7_local.R` y abrir http://127.0.0.1:3877. Detener previamente la vista anterior si ocupa ese puerto.

Captura: **Datos → Captura → Insectario → Formulario 7 → Ingreso individual**. Descarga: **Solicitudes → Datos → Insectario**, donde se elige la sábana. Diagnóstica e Intensidad se descargan juntas en la base actual; Sinergistas se descarga en la nueva estructura Sinergista/Etanol. Ambos destinos leen los mismos guardados locales y solamente las capturas completas entran a la descarga. El lanzador `preview_f7_componentes_local.R` conserva la vista aislada para pruebas.

La captura inicia en **1. Información del Proyecto**. La diferenciación ocurre en **2. Tipo de Bioensayo**, donde se selecciona Diagnóstica, Intensidad o Sinergistas. Se retiraron las tres pestañas superiores por tipo.

El formulario continúa con **3. Información del Bioensayo**, **4. Material biológico**, **5. Responsables**, **6. Condiciones ambientales**, **7. Horario del Bioensayo**, **8. Resultados por botella** y **Revisión y guardado**. Para Sinergistas, la sección 8 contiene la exposición previa 8.1/8.2 y aparece **9. Lectura posterior** con 9.1/9.2 y la lectura opcional de 24 horas. Para Diagnóstica e Intensidad se omite la sección 9.

Los datos comunes se conservan al cambiar el tipo. El destino de guardado se selecciona según el tipo activo en la sección 2: Diagnóstica e Intensidad van a `actual`; Sinergistas va a `sinergistas`. Los códigos de departamento y municipio se ingresan manualmente en esta vista local.

| Destino | Componentes | Resultados | Columnas CSV |
| --- | --- | --- | --- |
| Base actual | Diagnóstica e Intensidad | B1–B4/C1, tiempos 0/15/30/45/60 y 24 h; Temefos solo 24 h | 119 |
| Nueva base Sinergistas | Sinergistas | Dos sets, pretratamiento y bioensayo; 24 h opcionales | 209 |

La base actual conserva las columnas vigentes del Formulario 7. La nueva base Sinergistas agrega `version_estructura`, `componente` y `estado_captura`, además de los campos compartidos. Los nombres comunes se conservan para facilitar una unión posterior de filas. Una combinación por columnas entre ensayos requerirá definir su relación experimental; no asumir que códigos distintos identifican el mismo ensayo.

Las columnas del CSV son una exportación ancha, no una instrucción de crear esas mismas columnas en Supabase. Las lecturas de Sinergistas siguen siendo identificables por set, etapa, botella y tiempo. El diccionario final y la integración remota quedan para la siguiente fase.

## Guardado

- El guardado local exige completar el bioensayo en una sola sesión. Si faltan campos obligatorios o lecturas, no se crea archivo.
- **Validar y guardar captura completa** verifica datos generales, campos específicos, horarios, conteos y lecturas requeridas antes de incorporarlos a la descarga. Un código repetido dentro del mismo componente no crea otra captura completa.
- **Solicitudes → Datos → Insectario** exporta únicamente capturas completas del destino seleccionado. Sin registros, descarga solo las cabeceras.

Archivos locales: `output/f7_componentes/actual` para Diagnóstica e Intensidad, y `output/f7_componentes/sinergistas` para Sinergistas. Cada guardado crea una versión nueva. No se sincronizan archivos entre equipos.

Los archivos de la vista anterior (`f7_sets_local_v1`) siguen disponibles en su carpeta original y con su lanzador anterior; no se importan como si fueran el nuevo formato `f7_componentes_v1`.

## Verificación y límites

`scripts/test_f7_components_local.R` verifica esquemas por componente, IDs únicos de interfaz, persistencia solo de capturas completas, rechazo de datos incompletos e inválidos, rechazo de duplicados, ruteo a `actual`/`sinergistas` y exportación/lectura CSV con ceros iniciales conservados. Se compara cada fila completa exportada con todos los valores esperados y con la misma función de descarga usada en Solicitudes. `scripts/test_f7_local_classification.R` verifica límites de clasificación, corrección de Abbott, controles inválidos, lecturas incompletas, tiempos por insecticida y ambos sets independientes. Los datos de prueba se generan en carpetas temporales.

Se reutiliza la lógica de cálculo existente para Diagnóstica e Intensidad. No se modifica ni se valida científicamente el protocolo en esta entrega. En Revisión y guardado se muestra Resistente, Sospecha de Resistencia o Susceptible, junto con mortalidad, control y tiempo diagnóstico. Los datos incompletos aparecen como Pendiente y un control inválido impide la clasificación. Sinergistas calcula cada set por separado con su propio control y exporta seis columnas derivadas adicionales; esto no equivale a una conclusión conjunta sobre el efecto del sinergista. La exigencia definitiva de 24 horas y la correspondencia individual E5/C1 siguen pendientes.

Los códigos territoriales deben importarse como texto en Excel para evitar perder ceros iniciales. Los vacíos representan valores no capturados, no ceros experimentales.

No se realizaron migraciones, cambios en Supabase, modificaciones de la carga masiva remota ni despliegues. La app de producción todavía necesita una integración posterior del modelo acordado.
