# Formulario 5

## Guia rapida

Ruta principal:
- `00_resumen.md`
- `01_estructura.md`
- `02_consultas.sql`
- `03_soluciones.md`
- `04_estilos.md`
- `05_cambios.md`

## Que reconstruye este bloque

- Captura de alimentacion sanguinea.
- Registro de cepa, jaula, especie y responsables.
- Conteo de huevos con totales calculados.
- Subida masiva, ingreso individual y revision.

## Componentes UI

- Barra de pasos de captura
- Seccion de metadatos
- Seccion de datos generales
- Seccion de alimentacion
- Seccion de conteo de huevecillos
- Seccion de observaciones y auditoria
- Panel de certificacion antes de guardar
- Modal de revision con busqueda por `intake_id`, muestra 10% y comparacion de redigitacion

## Reglas practicas

- El archivo CSV debe respetar exactamente las columnas esperadas.
- Los totales se calculan en la app y en la base.
- La subida masiva valida tipos, fechas y catálogos antes de insertar.
- El registro queda con estado `pending` hasta revision.
- La captura guiada avanza por pasos y permite regresar para corregir.

## Archivos relacionados

- Base historica: `EntoNet/base_datos_entonet/formulario_5_alimentacion_conteo.csv`
- Diccionario: `EntoNet/base_datos_entonet/diccionario_formulario_5.csv`
- Intake SQL: `EntoNet/supabase/001_formulario_5_flat.sql`
- Intake SQL: `EntoNet/supabase/002_formulario_5_intake.sql`
- App Shiny: `EntoNet/shiny_app/app.R`

## Guia de reconstruccion

1. Iniciar en `Metadatos` con formulario, país, institucion, ciclo y fecha.
2. Pasar a `Datos generales` con cepa, especie, generacion, responsable de ingreso, fecha de jaula y conteos basicos.
3. Continuar con `Alimentacion` para responsable, tipo, descripcion, fecha, numero de charolas y observaciones.
4. Llenar `Conteo de huevecillos` con generacion, sustrato, fechas, cuadro y los cuatro conteos.
5. Revisar `Observaciones` y los datos de auditoria.
6. Validar el total de individuos y el total de huevos mostrados en la pantalla.
7. Guardar el registro en estado pendiente.
8. Usar el modal de revision para buscar por `intake_id`, generar muestra aleatoria o comparar redigitacion.
