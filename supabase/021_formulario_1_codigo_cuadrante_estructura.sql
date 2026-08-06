begin;

comment on column public.formulario_1_ovitrampa_intake.cuadrante is
  'Codigo de cuadrante. Para registros nuevos se recomienda REI + año + pais + codigo municipio + C###, por ejemplo REI25GT0503C001. No es obligatorio para preservar datos historicos.';

create index if not exists formulario_1_ovitrampa_intake_cuadrante_idx
  on public.formulario_1_ovitrampa_intake (cuadrante);

commit;
