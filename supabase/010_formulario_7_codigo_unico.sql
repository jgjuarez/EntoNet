begin;

alter table public.formulario_7_bioensayo_intake
  add column if not exists codigo_unico text;

create unique index if not exists formulario_7_codigo_unico_unique_idx
  on public.formulario_7_bioensayo_intake (codigo_unico)
  where codigo_unico is not null;

comment on column public.formulario_7_bioensayo_intake.codigo_unico is
  'Código único generado como Código de crianza-Código de bioensayo final-Fecha de registro. El código final agrega D, I con dosis, o S con sinergistas.';

commit;
