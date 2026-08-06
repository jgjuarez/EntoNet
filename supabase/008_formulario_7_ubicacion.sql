begin;

alter table public.formulario_7_bioensayo_intake
  add column if not exists pais text,
  add column if not exists codigo_departamento text,
  add column if not exists codigo_municipio text;

-- La columna anterior se conserva para no eliminar datos históricos, pero las
-- nuevas capturas utilizan país, código de departamento y código de municipio.
alter table public.formulario_7_bioensayo_intake
  alter column codigo_pais drop not null;

alter table public.formulario_7_bioensayo_intake
  drop constraint if exists formulario_7_pais_check;

alter table public.formulario_7_bioensayo_intake
  add constraint formulario_7_pais_check
  check (pais is null or pais in ('El Salvador', 'Guatemala')) not valid;

comment on column public.formulario_7_bioensayo_intake.pais is
  'País del material biológico: El Salvador o Guatemala.';
comment on column public.formulario_7_bioensayo_intake.codigo_departamento is
  'Código de departamento conservado como texto para mantener ceros iniciales.';
comment on column public.formulario_7_bioensayo_intake.codigo_municipio is
  'Código de municipio conservado como texto para mantener ceros iniciales.';

commit;
