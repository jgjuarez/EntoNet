alter table public.formulario_5_alimentacion_conteo_intake
  add column if not exists pais text not null default 'El Salvador',
  add column if not exists departamento_numero integer,
  add column if not exists municipio_numero integer;

alter table public.formulario_5_alimentacion_conteo_intake
  drop constraint if exists formulario_5_intake_pais_chk,
  drop constraint if exists formulario_5_intake_departamento_numero_chk,
  drop constraint if exists formulario_5_intake_municipio_numero_chk;

alter table public.formulario_5_alimentacion_conteo_intake
  add constraint formulario_5_intake_pais_chk
    check (pais in ('El Salvador', 'Guatemala')),
  add constraint formulario_5_intake_departamento_numero_chk
    check (departamento_numero >= 0),
  add constraint formulario_5_intake_municipio_numero_chk
    check (municipio_numero >= 0);
