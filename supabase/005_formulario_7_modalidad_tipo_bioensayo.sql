begin;

alter table public.formulario_7_bioensayo_intake
  add column if not exists modalidad_bioensayo text not null default 'Exploratorio';

alter table public.formulario_7_bioensayo_intake
  alter column modalidad_bioensayo drop default;

alter table public.formulario_7_bioensayo_intake
  drop constraint if exists formulario_7_modalidad_bioensayo_check;

alter table public.formulario_7_bioensayo_intake
  add constraint formulario_7_modalidad_bioensayo_check
  check (modalidad_bioensayo in ('Exploratorio', 'Completa'));

alter table public.formulario_7_bioensayo_intake
  drop constraint if exists formulario_7_dosis_consistente;

alter table public.formulario_7_bioensayo_intake
  drop constraint if exists formulario_7_tipo_bioensayo_consistente;

alter table public.formulario_7_bioensayo_intake
  add constraint formulario_7_tipo_bioensayo_consistente check (
    (
      modalidad_bioensayo = 'Exploratorio'
      and not dosis_diagnostica_1x
      and dosis_intensidad is null
      and not sinergista_def and not sinergista_pbo and not sinergista_dm
    )
    or (
      modalidad_bioensayo = 'Completa'
      and (
        (dosis_diagnostica_1x and dosis_intensidad is null and not sinergista_def and not sinergista_pbo and not sinergista_dm)
        or (not dosis_diagnostica_1x and dosis_intensidad is not null and not sinergista_def and not sinergista_pbo and not sinergista_dm)
        or (not dosis_diagnostica_1x and dosis_intensidad is null and (sinergista_def or sinergista_pbo or sinergista_dm))
      )
    )
  );

comment on column public.formulario_7_bioensayo_intake.modalidad_bioensayo is
  'Modalidad de ejecución: Exploratorio (b1=1X, b2=2X, b3=5X, b4=10X y c1=control) o Completa.';

commit;
