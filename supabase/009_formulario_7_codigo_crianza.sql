begin;

alter table public.formulario_7_bioensayo_intake
  add column if not exists codigo_crianza text;

comment on column public.formulario_7_bioensayo_intake.codigo_crianza is
  'Código de crianza del material biológico utilizado en el bioensayo.';

commit;
