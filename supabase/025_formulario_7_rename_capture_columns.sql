-- Align Formulario 7 capture column names with the official 118-column CSV.

do $$
begin
  if exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'creado_por'
  ) and not exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'nombre_quien_ingreso'
  ) then
    alter table public.formulario_7_bioensayo_intake
      rename column creado_por to nombre_quien_ingreso;
  end if;

  if exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'dosis_diagnostica_1x'
  ) and not exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'bioensayo_diagnostica_1x'
  ) then
    alter table public.formulario_7_bioensayo_intake
      rename column dosis_diagnostica_1x to bioensayo_diagnostica_1x;
  end if;

  if exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'modalidad_bioensayo'
  ) and not exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'bioensayo_intensidad'
  ) then
    alter table public.formulario_7_bioensayo_intake
      rename column modalidad_bioensayo to bioensayo_intensidad;
  end if;

  if exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'dosis_ug_ml'
  ) and not exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'dosis_intensidad_ug_ml'
  ) then
    alter table public.formulario_7_bioensayo_intake
      rename column dosis_ug_ml to dosis_intensidad_ug_ml;
  end if;

  if exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'codigo_insecticida'
  ) and not exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'insecticida'
  ) then
    alter table public.formulario_7_bioensayo_intake
      rename column codigo_insecticida to insecticida;
  end if;

  if exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'codigo_dosis'
  ) and not exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_7_bioensayo_intake'
      and column_name = 'lote_insecticida'
  ) then
    alter table public.formulario_7_bioensayo_intake
      rename column codigo_dosis to lote_insecticida;
  end if;
end $$;

comment on column public.formulario_7_bioensayo_intake.nombre_quien_ingreso is
  'Nombre de quien ingreso el registro en EntoNet.';
comment on column public.formulario_7_bioensayo_intake.bioensayo_diagnostica_1x is
  'Indica si el bioensayo corresponde a dosis diagnostica 1X.';
comment on column public.formulario_7_bioensayo_intake.bioensayo_intensidad is
  'Modalidad del bioensayo de intensidad: Exploratorio o Completa.';
comment on column public.formulario_7_bioensayo_intake.dosis_intensidad_ug_ml is
  'Concentracion del insecticida en ug/mL.';
comment on column public.formulario_7_bioensayo_intake.insecticida is
  'Insecticida evaluado.';
comment on column public.formulario_7_bioensayo_intake.lote_insecticida is
  'Lote del insecticida evaluado.';
