create table if not exists public.formulario_5_alimentacion_conteo_intake (
  intake_id bigserial primary key,
  review_status text not null default 'pending'
    check (review_status in ('pending', 'reviewed', 'rejected')),
  review_notes text,
  reviewed_by text,
  reviewed_at timestamptz,

  formulario_codigo text not null default 'F5'
    check (formulario_codigo = 'F5'),
  ciclo text not null default 'Ciclo 3',
  formulario_nombre text not null default 'Alimentacion sanguinea y conteo huevecillos Aedes spp.',
  fecha_registro date not null default current_date,

  cepa_poblacion text not null,
  especie text not null
    check (especie in ('Ae. aegypti', 'Ae. albopictus')),
  generacion_filial_adultos text not null,
  responsable_ingreso_jaula text not null,
  fecha_jaula date not null,
  numero_hembras integer not null check (numero_hembras >= 0),
  numero_machos integer not null check (numero_machos >= 0),
  total_individuos integer generated always as (
    numero_hembras + numero_machos
  ) stored,
  total_huevos_viables integer check (total_huevos_viables >= 0),

  responsable_alimentacion text not null,
  tipo_alimentacion_codigo text not null
    check (tipo_alimentacion_codigo in ('A', 'B', 'C', 'D', 'E')),
  tipo_alimentacion_descripcion text
    check (
      tipo_alimentacion_descripcion is null
      or tipo_alimentacion_descripcion in (
        'conejo',
        'humano',
        'hemotek-conejo',
        'hemotek-humano',
        'hemotek-carnero'
      )
    ),
  fecha_alimentacion_sangre date not null,
  numero_charolas integer not null check (numero_charolas >= 0),
  observaciones_alimentacion text,

  generacion_filial_huevos text not null,
  codigo_sustrato text not null,
  fecha_colocacion_sustrato date not null,
  fecha_retiro_sustrato date not null,
  numero_cuadro_sustrato integer not null check (numero_cuadro_sustrato >= 0),
  hv_huevos_viables integer not null check (hv_huevos_viables >= 0),
  he_huevos_eclosionados integer not null check (he_huevos_eclosionados >= 0),
  hc_huevos_canoa integer not null check (hc_huevos_canoa >= 0),
  hnf_huevos_no_fecundados integer not null check (hnf_huevos_no_fecundados >= 0),
  total_huevos integer generated always as (
    hv_huevos_viables +
    he_huevos_eclosionados +
    hc_huevos_canoa +
    hnf_huevos_no_fecundados
  ) stored,
  responsable_conteo_huevos text not null,

  observaciones_generales text,
  fuente_formulario text,
  creado_por text,
  creado_en timestamptz not null default now(),
  actualizado_en timestamptz not null default now(),

  constraint formulario_5_intake_fecha_sustrato_chk
    check (fecha_colocacion_sustrato <= fecha_retiro_sustrato)
);

create index if not exists idx_formulario_5_intake_review_status
  on public.formulario_5_alimentacion_conteo_intake (review_status);

create index if not exists idx_formulario_5_intake_cepa
  on public.formulario_5_alimentacion_conteo_intake (cepa_poblacion);

create index if not exists idx_formulario_5_intake_fechas
  on public.formulario_5_alimentacion_conteo_intake (
    fecha_registro,
    fecha_jaula,
    fecha_alimentacion_sangre
  );

alter table public.formulario_5_alimentacion_conteo_intake enable row level security;

drop policy if exists formulario_5_intake_no_api_access
  on public.formulario_5_alimentacion_conteo_intake;

create policy formulario_5_intake_no_api_access
  on public.formulario_5_alimentacion_conteo_intake
  for all
  to anon, authenticated
  using (false)
  with check (false);

revoke all on table public.formulario_5_alimentacion_conteo_intake from anon, authenticated;
revoke all on sequence public.formulario_5_alimentacion_conteo_intake_intake_id_seq from anon, authenticated;

comment on table public.formulario_5_alimentacion_conteo_intake is
  'Data-entry intake table for EntoNet Formulario 5 Alimentacion sanguinea y conteo huevecillos. Records are captured by Shiny and reviewed before analysis use.';
