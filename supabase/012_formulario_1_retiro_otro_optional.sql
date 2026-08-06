begin;

alter table if exists public.formulario_1_ovitrampa_intake
  drop constraint if exists formulario_1_retiro_otro_chk,
  drop constraint if exists formulario_1_retiro_casa_cerrada_chk;

do $$
begin
  if exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_1_ovitrampa_intake'
      and column_name = 'retiro_casa_cerrada_descripcion'
  ) then
    execute 'comment on column public.formulario_1_ovitrampa_intake.retiro_casa_cerrada_descripcion is ''Campo heredado opcional. La captura individual actual registra Casa cerrada como conteo simple.''';
  elsif exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_1_ovitrampa_intake'
      and column_name = 'retiro_otro_descripcion'
  ) then
    execute 'comment on column public.formulario_1_ovitrampa_intake.retiro_otro_descripcion is ''Campo heredado opcional. La captura individual actual registra Casa cerrada como conteo simple.''';
  end if;
end $$;

commit;
