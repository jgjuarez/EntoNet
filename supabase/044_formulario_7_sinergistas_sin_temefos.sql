-- Formulario 7: Temefos se registra únicamente en Diagnóstica o Intensidad.
-- La base separada de Sinergistas no acepta este insecticida.

begin;

alter table public.formulario_7_sinergista_intake
  drop constraint if exists formulario_7_sinergista_sin_temefos_check;

alter table public.formulario_7_sinergista_intake
  add constraint formulario_7_sinergista_sin_temefos_check
  check (upper(btrim(insecticida)) not like '%TEMEFOS%');

commit;
