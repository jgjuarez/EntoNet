# Guardado y revision por API

Actualizado: 2026-09-07.

La app usa la API privada del servidor para captura, lectura, actualizacion
y revision de los formularios vigentes. No intenta una conexion PostgreSQL
como respaldo. La consulta de perfiles tambien usa exclusivamente la API.
La contrasena de inicio de sesion del usuario sigue siendo necesaria.

| Formato | Captura | Revision y actualizacion |
| --- | --- | --- |
| F1 | RPC existente, individual y CSV | Lectura API; RPC de edicion, confirmacion y eliminacion auditada |
| F5 | RPC existente, individual y CSV | Lectura API; RPC de estado, notas, revisor y eliminacion auditada |
| F7 | RPC existente, individual y CSV | Lectura API; RPC de edicion, confirmacion y eliminacion auditada |
| SAT26 | API para codigo y envio | API para recuperar y actualizar una encuesta por codigo |
| F6 | Solo impresion disponible | No existe captura digital habilitada |

El ingreso antiguo egg_count_raw no esta habilitado: rei.egg_count_intake
no existe en esta base. Sus botones muestran una indicacion para usar
los formularios vigentes; no intentan guardar ni presentan un exito falso.

## Diagnostico F7

El valor guardado es Susceptible. La escritura antigua Suceptible se
normaliza antes de validar, tanto en edicion como en captura y CSV.
Sospecha de Resistencia y Resistente conservan sus valores.
El CSV actualizado incluye dosis_intensidad; se aceptan CSV antiguos
sin esa columna cuando la modalidad no la necesita. Si falta una dosis
requerida, se solicita corregirla antes del envio.
Los sinergistas se interpretan por fila, no a partir de la primera fila.

## Base de datos

La migracion formularios_api_only_updates fue aplicada tras autorizacion
explicita del usuario. El SQL reproducible esta en 07_actualizaciones_api.sql.
Requiere los esquemas y RPC de captura existentes, las tablas de auditoria,
y la correccion previa del diagnostico F7 a Susceptible.

Las funciones nuevas son SECURITY INVOKER y solo service_role puede
ejecutarlas. Las eliminaciones requieren motivo y crean auditoria en la
misma transaccion. Un error revierte toda la operacion.

## Verificacion

- Rscript scripts/test_formularios_api.R: diagnostica, intensidad exploratoria,
  intensidad completa, sinergistas, Temefos, CSV antiguo y nuevo, conversion
  por fila, validaciones, errores y rutas API sin fallback PostgreSQL.
- Supabase: F1 edicion/confirmacion/eliminacion, F5 pending/reviewed/rejected
  y eliminacion, F7 los tres diagnosticos y eliminacion; auditorias verificadas.
  Todas las modificaciones de prueba se revirtieron.
- Se verifico que anon y authenticated no pueden ejecutar las funciones.
- Existe al menos un registro historico F1 incompatible con la regla actual
  de retiro cero. No se alteraron sus datos: debe corregirse antes de
  confirmar. La app muestra un mensaje especifico para esa regla.

La version de GitHub debe desplegarse en Connect Cloud para activar la
correccion de la interfaz. La prueba de interfaz autenticada queda pendiente.
