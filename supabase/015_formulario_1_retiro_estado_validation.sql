begin;

alter table if exists public.formulario_1_ovitrampa_intake
  drop constraint if exists formulario_1_retiro_estado_total_chk,
  drop constraint if exists formulario_1_retiro_estado_colocadas_chk,
  drop constraint if exists formulario_1_retiro_cero_estado_chk;

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

commit;
