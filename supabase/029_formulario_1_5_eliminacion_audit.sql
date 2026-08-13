-- Audit deleted Formulario 1 and Formulario 5 records without keeping full captured datasets.

create table if not exists public.formulario_1_ovitrampa_eliminacion_audit (
  eliminacion_id bigserial primary key,
  intake_id bigint not null,
  codigo_formulario text,
  cuadrante text,
  codigo_casa text,
  review_status text,
  eliminado_por text,
  motivo_eliminacion text not null,
  eliminado_en timestamptz not null default now()
);

create index if not exists formulario_1_eliminacion_audit_codigo_idx
  on public.formulario_1_ovitrampa_eliminacion_audit (codigo_formulario);

alter table public.formulario_1_ovitrampa_eliminacion_audit enable row level security;

drop policy if exists formulario_1_eliminacion_audit_deny_anon
  on public.formulario_1_ovitrampa_eliminacion_audit;
create policy formulario_1_eliminacion_audit_deny_anon
  on public.formulario_1_ovitrampa_eliminacion_audit for all to anon using (false) with check (false);

drop policy if exists formulario_1_eliminacion_audit_deny_authenticated
  on public.formulario_1_ovitrampa_eliminacion_audit;
create policy formulario_1_eliminacion_audit_deny_authenticated
  on public.formulario_1_ovitrampa_eliminacion_audit for all to authenticated using (false) with check (false);

revoke all on table public.formulario_1_ovitrampa_eliminacion_audit from anon, authenticated;
revoke all on sequence public.formulario_1_ovitrampa_eliminacion_audit_eliminacion_id_seq from anon, authenticated;

comment on table public.formulario_1_ovitrampa_eliminacion_audit is
  'Auditoria minima de registros eliminados desde la revision del Formulario 1.';

create table if not exists public.formulario_5_alimentacion_eliminacion_audit (
  eliminacion_id bigserial primary key,
  intake_id bigint not null,
  formulario_codigo text,
  cepa_poblacion text,
  especie text,
  review_status text,
  eliminado_por text,
  motivo_eliminacion text not null,
  eliminado_en timestamptz not null default now()
);

create index if not exists formulario_5_eliminacion_audit_codigo_idx
  on public.formulario_5_alimentacion_eliminacion_audit (formulario_codigo);

alter table public.formulario_5_alimentacion_eliminacion_audit enable row level security;

drop policy if exists formulario_5_eliminacion_audit_deny_anon
  on public.formulario_5_alimentacion_eliminacion_audit;
create policy formulario_5_eliminacion_audit_deny_anon
  on public.formulario_5_alimentacion_eliminacion_audit for all to anon using (false) with check (false);

drop policy if exists formulario_5_eliminacion_audit_deny_authenticated
  on public.formulario_5_alimentacion_eliminacion_audit;
create policy formulario_5_eliminacion_audit_deny_authenticated
  on public.formulario_5_alimentacion_eliminacion_audit for all to authenticated using (false) with check (false);

revoke all on table public.formulario_5_alimentacion_eliminacion_audit from anon, authenticated;
revoke all on sequence public.formulario_5_alimentacion_eliminacion_audit_eliminacion_id_seq from anon, authenticated;

comment on table public.formulario_5_alimentacion_eliminacion_audit is
  'Auditoria minima de registros eliminados desde la revision del Formulario 5.';
