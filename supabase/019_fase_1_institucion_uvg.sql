begin;

create table if not exists public.catalogo_institucion (
  id_institucion text primary key,
  nombre_institucion text not null,
  pais text,
  activa boolean not null default true,
  creado_en timestamptz not null default now(),
  actualizado_en timestamptz not null default now()
);

insert into public.catalogo_institucion (id_institucion, nombre_institucion, pais)
values ('UVG', 'Universidad del Valle de Guatemala', 'Guatemala')
on conflict (id_institucion) do update set
  nombre_institucion = excluded.nombre_institucion,
  pais = excluded.pais,
  activa = true,
  actualizado_en = now();

alter table if exists public.formulario_5_alimentacion_conteo
  add column if not exists id_institucion text not null default 'UVG';

alter table if exists public.formulario_5_alimentacion_conteo_intake
  add column if not exists id_institucion text not null default 'UVG';

alter table if exists public.formulario_1_ovitrampa_intake
  add column if not exists id_institucion text not null default 'UVG';

alter table if exists public.formulario_7_bioensayo_intake
  add column if not exists id_institucion text not null default 'UVG';

update public.formulario_5_alimentacion_conteo
set id_institucion = 'UVG'
where id_institucion is null or btrim(id_institucion) = '';

update public.formulario_5_alimentacion_conteo_intake
set id_institucion = 'UVG'
where id_institucion is null or btrim(id_institucion) = '';

update public.formulario_1_ovitrampa_intake
set id_institucion = 'UVG'
where id_institucion is null or btrim(id_institucion) = '';

update public.formulario_7_bioensayo_intake
set id_institucion = 'UVG'
where id_institucion is null or btrim(id_institucion) = '';

create index if not exists formulario_5_flat_institucion_idx
  on public.formulario_5_alimentacion_conteo (id_institucion);
create index if not exists formulario_5_intake_institucion_idx
  on public.formulario_5_alimentacion_conteo_intake (id_institucion);
create index if not exists formulario_1_intake_institucion_idx
  on public.formulario_1_ovitrampa_intake (id_institucion);
create index if not exists formulario_7_intake_institucion_idx
  on public.formulario_7_bioensayo_intake (id_institucion);

alter table public.catalogo_institucion enable row level security;

drop policy if exists catalogo_institucion_deny_anon on public.catalogo_institucion;
create policy catalogo_institucion_deny_anon
  on public.catalogo_institucion for all to anon using (false) with check (false);

drop policy if exists catalogo_institucion_deny_authenticated on public.catalogo_institucion;
create policy catalogo_institucion_deny_authenticated
  on public.catalogo_institucion for all to authenticated using (false) with check (false);

revoke all on table public.catalogo_institucion from anon, authenticated;

comment on table public.catalogo_institucion is
  'Catalogo de instituciones para control de acceso por institucion. Fase 1 inicia con UVG.';
comment on column public.formulario_1_ovitrampa_intake.id_institucion is
  'Identificador de institucion propietaria del registro. Fase 1: UVG.';
comment on column public.formulario_5_alimentacion_conteo_intake.id_institucion is
  'Identificador de institucion propietaria del registro. Fase 1: UVG.';
comment on column public.formulario_7_bioensayo_intake.id_institucion is
  'Identificador de institucion propietaria del registro. Fase 1: UVG.';

commit;
