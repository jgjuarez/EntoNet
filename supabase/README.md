# Supabase EntoNet

Este directorio guarda scripts y notas para montar la base de datos de captura en Supabase.

Orden sugerido de ejecucion:

1. `001_formulario_5_flat.sql`
2. `002_formulario_5_intake.sql`
3. `003_add_formulario_5_location_metadata.sql`
4. `004_formulario_7_bioensayo_intake.sql`
5. `005_formulario_7_modalidad_tipo_bioensayo.sql`
6. `006_formulario_7_exploratorio.sql`
7. `007_formulario_7_flujo_diagnostico.sql`
8. `008_formulario_7_ubicacion.sql`
9. `009_formulario_7_codigo_crianza.sql`
10. `010_formulario_7_codigo_unico.sql`
11. `011_formulario_1_ovitrampa_intake.sql`
12. `016_formulario_7_rebuild_capture_schema.sql`
13. `017_formulario_7_single_comment.sql`
14. `018_catalogo_ubicacion_compartido.sql`
15. `019_fase_1_institucion_uvg.sql`
16. `023_formulario_7_sinergistas_60min.sql`
17. `024_formulario_7_sinergista_dosis.sql`
18. Scripts futuros de catalogos, permisos y vistas.

Para ejecutar todos los scripts desde la raiz del repositorio:

```bash
./scripts/run_supabase_migrations.sh
```

El script busca `.env.local` en la raiz del repositorio y, si no existe,
tambien acepta el `.env.local` del directorio padre usado por la configuracion
local anterior. No guardar credenciales reales en GitHub.

La tabla inicial es plana para facilitar la importacion del CSV. La tabla
`formulario_5_alimentacion_conteo_intake` esta preparada para captura de datos
desde Shiny: guarda registros pendientes de revision, campos de auditoria y
totales calculados. Luego se puede normalizar cuando ya esten definidos todos
los formularios.

Catálogos compartidos:

- `catalogo_ubicacion_departamento`: códigos nacionales de departamentos por país.
- `catalogo_ubicacion_municipio`: códigos nacionales de municipios por país y departamento. Formulario 1 y Formulario 7 usan estos códigos para selección de ubicación.
- `catalogo_institucion`: instituciones propietarias de datos. Fase 1 inicia con `UVG`.

Formulario 7 usa un modelo normalizado alineado con el ingreso individual:

- `formulario_7_bioensayo_intake`: información general, bioensayo, material, responsables, condiciones y estado de revisión. `codigo_bioensayo` es el identificador del registro.
- `formulario_7_bioensayo_resultado_intake`: lecturas por botella y tiempo. Para sinergistas se registra primero la lectura de 60 minutos del sinergista y luego la lectura del insecticida a 0, 15, 30, 45 minutos y 24h.
- `formulario_7_bioensayo_comentario_intake`: un comentario asociado.

El CSV oficial de Formulario 7 usa 118 columnas visibles para carga y descarga.
La aplicación traduce nombres operativos como `nombre_quien_ingreso`,
`bioensayo_diagnostica_1x`, `dosis_intensidad_ug_ml`, `insecticida` y
`lote_insecticida` hacia las columnas internas normalizadas antes de guardar en
Supabase.

Formulario 1 usa un modelo encabezado-detalle:

- `formulario_1_ovitrampa_intake`: metadatos de colocacion/retiro, fechas, ubicacion administrativa, cuadrante, casa, GPS, total de ovitrampas colocadas, total retirado, conteos por estado y responsables.
- `formulario_1_ovitrampa_detalle_intake`: una fila por ovitrampa/sustrato, con codigo de sustrato.

Estas tablas tienen RLS activado y deniegan acceso directo a los roles
`anon` y `authenticated`; la aplicación local escribe mediante la conexión
PostgreSQL restringida configurada en `SUPABASE_DB_URL`.
