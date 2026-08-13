-- Audit deleted Formulario 7 records without keeping the full captured dataset.

create table if not exists public.formulario_7_bioensayo_eliminacion_audit (
  eliminacion_id bigserial primary key,
  intake_id bigint not null,
  codigo_bioensayo text,
  review_status text,
  eliminado_por text,
  motivo_eliminacion text not null,
  eliminado_en timestamptz not null default now()
);

create index if not exists formulario_7_eliminacion_audit_codigo_idx
  on public.formulario_7_bioensayo_eliminacion_audit (codigo_bioensayo);

alter table public.formulario_7_bioensayo_eliminacion_audit enable row level security;

drop policy if exists formulario_7_eliminacion_audit_deny_anon
  on public.formulario_7_bioensayo_eliminacion_audit;
create policy formulario_7_eliminacion_audit_deny_anon
  on public.formulario_7_bioensayo_eliminacion_audit for all to anon using (false) with check (false);

drop policy if exists formulario_7_eliminacion_audit_deny_authenticated
  on public.formulario_7_bioensayo_eliminacion_audit;
create policy formulario_7_eliminacion_audit_deny_authenticated
  on public.formulario_7_bioensayo_eliminacion_audit for all to authenticated using (false) with check (false);

revoke all on table public.formulario_7_bioensayo_eliminacion_audit from anon, authenticated;
revoke all on sequence public.formulario_7_bioensayo_eliminacion_audit_eliminacion_id_seq from anon, authenticated;

comment on table public.formulario_7_bioensayo_eliminacion_audit is
  'Auditoria minima de registros eliminados desde la revision del Formulario 7.';
