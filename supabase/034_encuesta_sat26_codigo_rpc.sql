begin;

create or replace function public.next_encuesta_sat26_code()
returns text
language sql
security invoker
set search_path = ''
as $$
  select '26SAT' || lpad(
    nextval('public.encuesta_sat26_codigo_seq')::text,
    2,
    '0'
  );
$$;

revoke all on function public.next_encuesta_sat26_code() from public, anon, authenticated;
grant execute on function public.next_encuesta_sat26_code() to service_role;
grant usage, select on sequence public.encuesta_sat26_codigo_seq to service_role;

comment on function public.next_encuesta_sat26_code() is
  'Genera el siguiente código SAT26. Acceso restringido a la clave privada del servidor.';

commit;
