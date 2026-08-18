# Formulario 1

## Guia rapida

Ruta principal:
- `00_resumen.md`
- `01_estructura.md`
- `02_consultas.sql`
- `03_soluciones.md`
- `04_estilos.md`
- `05_cambios.md`

## Que reconstruye este bloque

- Captura de colocacion y retiro de ovitrampas.
- Generacion de cuadrantes, casas y sustratos.
- Resumen de revision, edicion y eliminacion con comentario obligatorio.
- Generacion de machote imprimible en Excel.

## Componentes UI

- Carga parcial por `codigo_formulario`
- Pestaña de colocacion con datos generales, ubicacion y codigo base
- Pestaña de generacion de cuadrantes
- Tabs por cuadrante y casa para capturar sustratos
- Panel de guardado y estado de envio
- Modal de revision con filtros, listado y detalle editable
- Modal de impresion con parametros territoriales y de codificacion

## Reglas practicas

- El ingreso se organiza por cuadrantes y casas.
- La app carga datos previos por `codigo_formulario`.
- Al guardar, solo inserta sustratos nuevos que no existan ya.
- La revison puede confirmar, editar o eliminar con auditoria.
- La impresion requiere parametros de pais, departamento, municipio, ciclo, ronda y codigos base.

## Archivos relacionados

- Base historica: `EntoNet/base_datos_entonet/formulario_1_colocacion_retiro_ovitrampa.csv`
- Diccionario: `EntoNet/base_datos_entonet/diccionario_formulario_1.csv`
- Intake SQL: `EntoNet/supabase/011_formulario_1_ovitrampa_intake.sql`
- App Shiny: `EntoNet/shiny_app/app.R`

## Guia de reconstruccion

1. Armar la pantalla de `Datos generales` con carga parcial por `codigo_formulario`, pais, institucion, ubicacion, ciclo y fecha de registro.
2. Resolver la ubicación dependiente con `f1_municipio_ui` después de elegir país y departamento.
3. Configurar la pantalla de `Colocación` con fecha, grupo responsable, número de cuadrantes, código cuadrante base, casas por cuadrante y códigos base de casa y sustrato.
4. Generar cuadrantes y pintar tabs por cuadrante y casa.
5. Capturar cada casa con sus ovitrampas y estados de retiro en los tabs generados.
6. Guardar el conjunto como un registro pendiente en la tabla intake.
7. Abrir el modal de revisión para buscar por código, filtrar por estado y editar o confirmar.
8. Abrir el modal de impresión para generar el machote Excel con los códigos territoriales y de campo.
