begin;

create sequence if not exists public.encuesta_sat26_codigo_seq
  as integer
  increment by 1
  minvalue 1
  start with 1
  cache 1;

with codigo_estado as (
  select coalesce((
    select max(nullif(regexp_replace(codigo_unico, '^26SAT', ''), '')::integer)
    from public.encuesta_sat26_intake
    where codigo_unico ~ '^26SAT[0-9]+$'
  ), 0) as ultimo_codigo
)
select setval(
  'public.encuesta_sat26_codigo_seq',
  greatest(ultimo_codigo, 1),
  ultimo_codigo > 0
)
from codigo_estado;

revoke all on sequence public.encuesta_sat26_codigo_seq from anon, authenticated;

comment on sequence public.encuesta_sat26_codigo_seq is
  'Secuencia central para generar códigos únicos públicos de la Encuesta SAT26.';

commit;
