begin;

alter table public.formulario_7_bioensayo_intake
  add column if not exists resultado_diagnostico text;

alter table public.formulario_7_bioensayo_intake
  alter column modalidad_bioensayo drop not null;

update public.formulario_7_bioensayo_intake
set resultado_diagnostico = case resultado_diagnostico
  when 'Susceptible' then 'Suceptible'
  when 'Sospecha de resistencia' then 'Sospecha de Resistencia'
  else resultado_diagnostico
end
where resultado_diagnostico in ('Susceptible', 'Sospecha de resistencia');

alter table public.formulario_7_bioensayo_intake
  drop constraint if exists formulario_7_modalidad_bioensayo_check,
  drop constraint if exists formulario_7_resultado_diagnostico_check,
  drop constraint if exists formulario_7_tipo_bioensayo_consistente,
  drop constraint if exists formulario_7_bioensayo_intake_dosis_intensidad_check;

alter table public.formulario_7_bioensayo_intake
  add constraint formulario_7_modalidad_bioensayo_check
    check (modalidad_bioensayo is null or modalidad_bioensayo in ('Exploratorio', 'Completa')),
  add constraint formulario_7_resultado_diagnostico_check
    check (resultado_diagnostico is null or resultado_diagnostico in ('Suceptible', 'Sospecha de Resistencia', 'Resistente')),
  add constraint formulario_7_bioensayo_intake_dosis_intensidad_check
    check (dosis_intensidad is null or dosis_intensidad in ('1X', '2X', '5X', '10X')),
  add constraint formulario_7_tipo_bioensayo_consistente check (
    (
      dosis_diagnostica_1x
      and modalidad_bioensayo is null
      and dosis_intensidad is null
      and not sinergista_def and not sinergista_pbo and not sinergista_dm
      and resultado_diagnostico is not null
    )
    or (
      not dosis_diagnostica_1x
      and resultado_diagnostico is null
      and (
        (
          modalidad_bioensayo = 'Exploratorio'
          and dosis_intensidad is null
          and not sinergista_def and not sinergista_pbo and not sinergista_dm
        )
        or (
          modalidad_bioensayo = 'Completa'
          and dosis_intensidad is not null
          and not sinergista_def and not sinergista_pbo and not sinergista_dm
        )
        or (
          modalidad_bioensayo is null
          and dosis_intensidad is null
          and (sinergista_def or sinergista_pbo or sinergista_dm)
        )
      )
    )
  ) not valid;

comment on column public.formulario_7_bioensayo_intake.resultado_diagnostico is
  'Clasificación de una prueba Diagnóstica 1X: Suceptible, Sospecha de Resistencia o Resistente.';

commit;
