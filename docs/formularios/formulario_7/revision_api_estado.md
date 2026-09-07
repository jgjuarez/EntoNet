# Correccion de revision por API

Estado al 2026-09-07: SQL autorizado por el usuario y aplicado en Supabase
como migracion formulario_7_review_rpc. Pendiente publicar/reiniciar la
aplicacion con el app.R corregido y comprobar el flujo en la interfaz.

## Diagnostico

La lectura ya usa supabase_private_select, pero editar y confirmar usaban
connect_to_supabase. Una contrasena PostgreSQL incorrecta puede bloquear estas
operaciones aunque el inicio de sesion y la lectura por API funcionen.
No se ha verificado la contrasena del despliegue ni el mensaje exacto de su log.
Se confirmo que service_role tiene lectura, pero no UPDATE sobre el encabezado.

## Solucion preparada

- Guardado mediante entonet_update_formulario_7: actualiza el encabezado y
  reemplaza resultados/comentarios dentro de una sola transaccion. Mantiene
  intake_id y creado_en; restablece la revision a pending.
- Confirmacion mediante entonet_confirm_formulario_7: registra reviewed,
  notas, revisor y fecha.
- Ambas funciones son SECURITY INVOKER, solo ejecutables por service_role.
  El SQL concede UPDATE del encabezado e INSERT/DELETE de las tablas hijas
  a ese rol de servidor. No concede acceso a anon ni authenticated.
- Se conservan las validaciones del formulario. La eliminacion completa
  de formularios sigue utilizando su flujo anterior.

## Verificacion y activacion

1. Ejecutar Rscript scripts/test_f7_review_rpc.R desde la raiz de EntoNet.
2. Con autorizacion, aplicar revision_api_propuesta.sql mediante una migracion.
3. Verificar permisos y probar guardado, confirmacion, limpieza de campos
   opcionales y rollback ante un resultado invalido, con datos de prueba.
4. Publicar/reiniciar la app y comprobar el flujo de revision en la interfaz.

La revision automatica bloqueo el primer intento; tras la autorizacion
explicita del usuario, la migracion fue aplicada correctamente.

## Resultado de las pruebas

- Funciones SECURITY INVOKER: service_role puede ejecutarlas; anon y
  authenticated no pueden.
- Prueba SQL bajo service_role: confirmacion reviewed, edicion pending,
  reemplazo de tablas hijas y rollback ante un resultado invalido correctos.
  La transaccion de prueba termino con ROLLBACK; no quedaron cambios
  de prueba en los formularios.
- Las pruebas locales de R comprobaron payloads, listas vacias, confirmacion
  y propagacion de errores de API.
- El asesor de seguridad no reporto hallazgos sobre las dos funciones nuevas.
  Reporto avisos en objetos ajenos a este cambio: funciones rei con search_path
  mutable, rei.Correction_log sin politicas, public.rls_auto_enable ejecutable
  por roles de cliente y proteccion de contrasenas filtradas desactivada.
  Referencias:
  https://supabase.com/docs/guides/database/database-linter?lint=0011_function_search_path_mutable
  https://supabase.com/docs/guides/database/database-linter?lint=0008_rls_enabled_no_policy
  https://supabase.com/docs/guides/database/database-linter?lint=0028_anon_security_definer_function_executable
  https://supabase.com/docs/guides/database/database-linter?lint=0029_authenticated_security_definer_function_executable
  https://supabase.com/docs/guides/auth/password-security#password-strength-and-leaked-password-protection
