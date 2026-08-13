-- Allow Formulario 7 synergist assays to capture the diagnostic test result.

alter table public.formulario_7_bioensayo_intake
  drop constraint if exists formulario_7_tipo_bioensayo_consistente;

alter table public.formulario_7_bioensayo_intake
  add constraint formulario_7_tipo_bioensayo_consistente check (
    (
      bioensayo_diagnostica_1x
      and bioensayo_intensidad is null
      and dosis_intensidad is null
      and not sinergista_def
      and not sinergista_pbo
      and not sinergista_dm
      and resultado_diagnostico is not null
    )
    or (
      not bioensayo_diagnostica_1x
      and bioensayo_intensidad = 'Exploratorio'
      and dosis_intensidad is null
      and not sinergista_def
      and not sinergista_pbo
      and not sinergista_dm
      and resultado_diagnostico is null
    )
    or (
      not bioensayo_diagnostica_1x
      and bioensayo_intensidad = 'Completa'
      and dosis_intensidad is not null
      and not sinergista_def
      and not sinergista_pbo
      and not sinergista_dm
      and resultado_diagnostico is null
    )
    or (
      not bioensayo_diagnostica_1x
      and bioensayo_intensidad is null
      and dosis_intensidad is null
      and (sinergista_def or sinergista_pbo or sinergista_dm)
      and resultado_diagnostico is not null
    )
  ) not valid;

comment on column public.formulario_7_bioensayo_intake.resultado_diagnostico is
  'Resultado de la prueba diagnostica. Obligatorio para Diagnostica 1X y Sinergistas; no aplica para Intensidad.';
