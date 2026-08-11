begin;

alter table public.formulario_7_bioensayo_intake
  alter column codigo_revision_24h drop not null;

alter table public.formulario_7_bioensayo_resultado_intake
  drop constraint if exists formulario_7_bioensayo_resultado_intake_tiempo_minutos_check,
  drop constraint if exists formulario_7_fase_tiempo_consistente;

alter table public.formulario_7_bioensayo_resultado_intake
  add constraint formulario_7_bioensayo_resultado_intake_tiempo_minutos_check
    check (tiempo_minutos in (0, 15, 30, 45, 60, 1440)),
  add constraint formulario_7_fase_tiempo_consistente
    check (
      (fase = 'bioensayo' and tiempo_minutos in (0, 15, 30, 45, 60))
      or (fase = 'kdr_24h' and tiempo_minutos = 1440)
    );

comment on constraint formulario_7_fase_tiempo_consistente on public.formulario_7_bioensayo_resultado_intake is
  'Permite lectura unica a 60 minutos para ensayos con sinergistas; mantiene 0/15/30/45 y 24h para los demas flujos.';

comment on column public.formulario_7_bioensayo_intake.codigo_revision_24h is
  'Codigo de revision a 24 horas. No aplica para ensayos con sinergistas.';

commit;
