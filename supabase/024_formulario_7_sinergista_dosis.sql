-- Separate synergist identity and dose from insecticide fields in Formulario 7.

alter table public.formulario_7_bioensayo_intake
  add column if not exists sinergista_tipo text,
  add column if not exists dosis_sinergista_ug_ml numeric;

alter table public.formulario_7_bioensayo_intake
  drop constraint if exists formulario_7_sinergista_tipo_chk;

alter table public.formulario_7_bioensayo_intake
  add constraint formulario_7_sinergista_tipo_chk
  check (sinergista_tipo is null or sinergista_tipo in ('DEF', 'PBO', 'DM'));

alter table public.formulario_7_bioensayo_intake
  drop constraint if exists formulario_7_dosis_sinergista_nonnegative_chk;

alter table public.formulario_7_bioensayo_intake
  add constraint formulario_7_dosis_sinergista_nonnegative_chk
  check (dosis_sinergista_ug_ml is null or dosis_sinergista_ug_ml >= 0);
