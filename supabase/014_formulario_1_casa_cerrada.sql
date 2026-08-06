begin;

alter table if exists public.formulario_1_ovitrampa_intake
  drop constraint if exists formulario_1_retiro_estado_total_chk,
  drop constraint if exists formulario_1_retiro_estado_colocadas_chk,
  drop constraint if exists formulario_1_retiro_cero_estado_chk;

do $$
begin
  if exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_1_ovitrampa_intake'
      and column_name = 'retiro_otro'
  ) and not exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_1_ovitrampa_intake'
      and column_name = 'retiro_casa_cerrada'
  ) then
    alter table public.formulario_1_ovitrampa_intake
      rename column retiro_otro to retiro_casa_cerrada;
  end if;

  if exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_1_ovitrampa_intake'
      and column_name = 'retiro_otro_descripcion'
  ) and not exists (
    select 1 from information_schema.columns
    where table_schema = 'public'
      and table_name = 'formulario_1_ovitrampa_intake'
      and column_name = 'retiro_casa_cerrada_descripcion'
  ) then
    alter table public.formulario_1_ovitrampa_intake
      rename column retiro_otro_descripcion to retiro_casa_cerrada_descripcion;
  end if;
end $$;

alter table if exists public.formulario_1_ovitrampa_intake
  add constraint formulario_1_retiro_estado_total_chk
    check (
      (
        retiro_buen_estado +
        retiro_sin_agua +
        retiro_sin_sustrato
      ) <= coalesce(ovitrampas_retiradas, ovitrampas_colocadas)
    ) not valid,
  add constraint formulario_1_retiro_estado_colocadas_chk
    check (
      (
        retiro_buen_estado +
        retiro_sin_agua +
        retiro_sin_sustrato +
        retiro_sin_ovitrampa +
        retiro_movida +
        retiro_volteada +
        retiro_casa_cerrada
      ) <= ovitrampas_colocadas
    ) not valid,
  add constraint formulario_1_retiro_cero_estado_chk
    check (
      ovitrampas_retiradas is null
      or ovitrampas_retiradas <> 0
      or (
        retiro_buen_estado +
        retiro_sin_agua +
        retiro_sin_sustrato +
        retiro_sin_ovitrampa +
        retiro_movida +
        retiro_volteada +
        retiro_casa_cerrada
      ) = ovitrampas_colocadas
    ) not valid;

comment on column public.formulario_1_ovitrampa_intake.retiro_casa_cerrada is
  'Número de ovitrampas no retiradas o sin lectura porque la casa estaba cerrada.';

comment on column public.formulario_1_ovitrampa_intake.retiro_casa_cerrada_descripcion is
  'Campo heredado opcional.';

commit;
