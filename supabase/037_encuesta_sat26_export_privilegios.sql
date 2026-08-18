-- La vista debe ser exclusivamente de lectura para el rol de servicio.
-- Se revocan privilegios heredados por defecto antes de conceder SELECT.
revoke all on public.encuesta_sat26_export from service_role;
grant select on public.encuesta_sat26_export to service_role;
