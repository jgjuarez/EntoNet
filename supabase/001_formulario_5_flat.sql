create table if not exists public.formulario_5_alimentacion_conteo (
  id bigserial primary key,
  formulario_codigo text not null default 'F5',
  ciclo text not null default 'Ciclo 3',
  formulario_nombre text not null default 'Alimentacion sanguinea y conteo huevecillos Aedes spp.',
  fecha_registro date,
  cepa_poblacion text,
  especie text check (especie in ('Ae. aegypti', 'Ae. albopictus')),
  generacion_filial_adultos text,
  responsable_ingreso_jaula text,
  fecha_jaula date,
  numero_hembras integer check (numero_hembras >= 0),
  numero_machos integer check (numero_machos >= 0),
  total_individuos integer generated always as (
    coalesce(numero_hembras, 0) + coalesce(numero_machos, 0)
  ) stored,
  total_huevos_viables integer check (total_huevos_viables >= 0),
  responsable_alimentacion text,
  tipo_alimentacion_codigo text check (tipo_alimentacion_codigo in ('A', 'B', 'C', 'D', 'E')),
  tipo_alimentacion_descripcion text check (
    tipo_alimentacion_descripcion in (
      'conejo',
      'humano',
      'hemotek-conejo',
      'hemotek-humano',
      'hemotek-carnero'
    )
  ),
  fecha_alimentacion_sangre date,
  numero_charolas integer check (numero_charolas >= 0),
  observaciones_alimentacion text,
  generacion_filial_huevos text,
  codigo_sustrato text,
  fecha_colocacion_sustrato date,
  fecha_retiro_sustrato date,
  numero_cuadro_sustrato integer check (numero_cuadro_sustrato >= 0),
  hv_huevos_viables integer check (hv_huevos_viables >= 0),
  he_huevos_eclosionados integer check (he_huevos_eclosionados >= 0),
  hc_huevos_canoa integer check (hc_huevos_canoa >= 0),
  hnf_huevos_no_fecundados integer check (hnf_huevos_no_fecundados >= 0),
  total_huevos integer generated always as (
    coalesce(hv_huevos_viables, 0) +
    coalesce(he_huevos_eclosionados, 0) +
    coalesce(hc_huevos_canoa, 0) +
    coalesce(hnf_huevos_no_fecundados, 0)
  ) stored,
  responsable_conteo_huevos text,
  observaciones_generales text,
  fuente_formulario text,
  creado_por text,
  creado_en timestamptz not null default now(),
  actualizado_en timestamptz not null default now()
);

create index if not exists idx_formulario_5_cepa
  on public.formulario_5_alimentacion_conteo (cepa_poblacion);

create index if not exists idx_formulario_5_fechas
  on public.formulario_5_alimentacion_conteo (fecha_jaula, fecha_alimentacion_sangre);

