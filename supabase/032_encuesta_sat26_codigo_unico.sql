begin;

alter table public.encuesta_sat26_intake
  add column if not exists codigo_unico text;

update public.encuesta_sat26_intake
set codigo_unico = coalesce(
  nullif(codigo_unico, ''),
  nullif(payload ->> 'code', ''),
  '26SAT' || lpad(intake_id::text, 2, '0')
)
where codigo_unico is null or codigo_unico = '';

alter table public.encuesta_sat26_intake
  alter column codigo_unico set not null;

create unique index if not exists encuesta_sat26_codigo_unico_idx
  on public.encuesta_sat26_intake (codigo_unico);

comment on column public.encuesta_sat26_intake.codigo_unico is
  'Identificador único de la encuesta visible para el participante. Ejemplo: 26SAT01.';

commit;
