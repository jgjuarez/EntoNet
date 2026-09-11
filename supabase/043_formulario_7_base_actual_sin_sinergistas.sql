begin;

-- Existing records stay intact for audit and review. These triggers only block
-- new data entering the former shared Formulario 7 tables.
create or replace function public.formulario_7_rechazar_sinergistas_base_actual()
returns trigger
language plpgsql
set search_path = ''
as $$
begin
  if coalesce(new.sinergista_def, false)
    or coalesce(new.sinergista_pbo, false)
    or coalesce(new.sinergista_dm, false)
    or nullif(btrim(new.sinergista_tipo), '') is not null
    or new.dosis_sinergista_ug_ml is not null then
    raise exception
      'Sinergistas ya no se captura en formulario_7_bioensayo_intake; use formulario_7_sinergista_intake.'
      using errcode = '23514';
  end if;
  return new;
end;
$$;

drop trigger if exists formulario_7_rechazar_sinergistas_base_actual_insert
  on public.formulario_7_bioensayo_intake;
create trigger formulario_7_rechazar_sinergistas_base_actual_insert
before insert on public.formulario_7_bioensayo_intake
for each row execute function public.formulario_7_rechazar_sinergistas_base_actual();

create or replace function public.formulario_7_rechazar_lectura_60min_base_actual()
returns trigger
language plpgsql
set search_path = ''
as $$
begin
  if new.tiempo_minutos = 60 then
    raise exception
      'La lectura a 60 minutos corresponde exclusivamente a la base nueva de Sinergistas.'
      using errcode = '23514';
  end if;
  return new;
end;
$$;

drop trigger if exists formulario_7_rechazar_lectura_60min_base_actual_insert
  on public.formulario_7_bioensayo_resultado_intake;
create trigger formulario_7_rechazar_lectura_60min_base_actual_insert
before insert on public.formulario_7_bioensayo_resultado_intake
for each row execute function public.formulario_7_rechazar_lectura_60min_base_actual();

revoke all on function public.formulario_7_rechazar_sinergistas_base_actual() from public, anon, authenticated;
revoke all on function public.formulario_7_rechazar_lectura_60min_base_actual() from public, anon, authenticated;
grant execute on function public.formulario_7_rechazar_sinergistas_base_actual() to service_role;
grant execute on function public.formulario_7_rechazar_lectura_60min_base_actual() to service_role;

comment on function public.formulario_7_rechazar_sinergistas_base_actual() is
  'Impide nuevos registros Sinergistas en la base histórica de Diagnóstica e Intensidad; no modifica registros previos.';
comment on function public.formulario_7_rechazar_lectura_60min_base_actual() is
  'Impide lecturas de 60 minutos en la base histórica de Diagnóstica e Intensidad.';

commit;
