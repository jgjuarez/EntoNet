# Supabase EntoNet

Este directorio guarda scripts y notas para montar la base de datos de captura en Supabase.

Orden sugerido de ejecucion:

1. `001_formulario_5_flat.sql`
2. `002_formulario_5_intake.sql`
3. `003_add_formulario_5_location_metadata.sql`
4. Scripts futuros de catalogos, permisos y vistas.

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
