begin;

grant usage on schema public to service_role;
grant select, insert, update on table public.encuesta_sat26_intake to service_role;
grant usage, select on sequence public.encuesta_sat26_intake_intake_id_seq to service_role;

revoke all on table public.encuesta_sat26_intake from public;

comment on table public.encuesta_sat26_intake is
  'Captura SAT26. La API privada del servidor puede consultar, insertar y actualizar; no se concede lectura pública.';

commit;
