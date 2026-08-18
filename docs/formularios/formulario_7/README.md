# Formulario 7

## Guia rapida

Ruta principal:
- `00_resumen.md`
- `01_estructura.md`
- `02_consultas.sql`
- `03_soluciones.md`
- `04_estilos.md`
- `05_cambios.md`

## Que reconstruye este bloque

- Captura de bioensayo de botella CDC.
- Logica de diagnostica, intensidad y sinergistas.
- Resultados por botella, fase y tiempo.
- Comentarios numerados y codigo unico.

## Componentes UI

- Bloque de codigo de bioensayo con ayuda visual
- Informacion general y codigo territorial
- Selector de tipo de bioensayo
- Bloque de modalidad o dosis segun tipo
- Bloque de informacion del bioensayo
- Bloque de material biologico
- Bloque de responsables
- Bloque de condiciones ambientales
- Pestañas por botella con resultados
- Panel de comentario unico
- Panel de navegacion por pasos con desbloqueo secuencial
- Modal de impresion con parametros para machote XLSX
- Modal de revision con filtros, listado y edicion o eliminacion

## Reglas practicas

- `codigo_unico` se construye con codigo de crianza, codigo de bioensayo y fecha.
- La modalidad y el tipo de ensayo condicionan los campos validos.
- La carga masiva rechaza codigos de bioensayo repetidos.
- Los resultados se guardan normalizados por botella, fase y tiempo.
- La captura guiada desbloquea secciones en orden y conserva las anteriores editables.

## Archivos relacionados

- Base historica: `EntoNet/base_datos_entonet/formulario_7_bioensayo_botella_cdc.csv`
- Diccionario: `EntoNet/base_datos_entonet/diccionario_formulario_7.csv`
- Intake SQL: `EntoNet/supabase/004_formulario_7_bioensayo_intake.sql`
- Reglas extra: `EntoNet/supabase/005_formulario_7_modalidad_tipo_bioensayo.sql`
- Reglas extra: `EntoNet/supabase/006_formulario_7_exploratorio.sql`
- Reglas extra: `EntoNet/supabase/007_formulario_7_flujo_diagnostico.sql`
- Reglas extra: `EntoNet/supabase/010_formulario_7_codigo_unico.sql`
- App Shiny: `EntoNet/shiny_app/app.R`

## Guia de reconstruccion

1. Construir `Información General` con código de bioensayo, país, institución, ubicación, población y tipo de bioensayo.
2. Mostrar la ayuda del código para orientar la composición del identificador.
3. Resolver los campos dependientes de diagnóstica, intensidad o sinergistas.
4. Llenar `Información del Bioensayo` con fecha, insecticida, solvente, dosis, lote y fechas de revestimiento.
5. Llenar `Material y responsables` con origen, edad, especie, separación, generación filial y responsables.
6. Capturar `Condiciones` con temperatura, humedad y horario.
7. Repetir por botella en `Resultados por botella`, cambiando el bloque según el tipo de bioensayo.
8. Cerrar en `Comentarios y envío` y guardar como pendiente.
9. Usar el modal de impresión para generar el machote con el código de bioensayo prellenado.
10. Usar el modal de revisión para buscar por código, filtrar por estado y editar o confirmar.
