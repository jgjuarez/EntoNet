-- Track EntoNet authentication events and public, unauthenticated traffic.

create table if not exists public.entonet_auth_access_log (
  access_log_id bigserial primary key,
  occurred_at timestamptz not null default now(),
  event_type text not null,
  success boolean,
  login_identifier text,
  email text,
  user_id uuid,
  usuario text,
  nombre text,
  rol text,
  pais text,
  id_institucion text,
  error_message text,
  ip_address text,
  user_agent text,
  referrer text,
  page text,
  path text,
  query_string text,
  session_token text
);

create index if not exists entonet_auth_access_log_occurred_at_idx
  on public.entonet_auth_access_log (occurred_at desc);
create index if not exists entonet_auth_access_log_email_idx
  on public.entonet_auth_access_log (lower(email));
create index if not exists entonet_auth_access_log_user_id_idx
  on public.entonet_auth_access_log (user_id);
create index if not exists entonet_auth_access_log_event_type_idx
  on public.entonet_auth_access_log (event_type);

alter table public.entonet_auth_access_log enable row level security;

drop policy if exists entonet_auth_access_log_deny_anon
  on public.entonet_auth_access_log;
create policy entonet_auth_access_log_deny_anon
  on public.entonet_auth_access_log for all to anon using (false) with check (false);

drop policy if exists entonet_auth_access_log_deny_authenticated
  on public.entonet_auth_access_log;
create policy entonet_auth_access_log_deny_authenticated
  on public.entonet_auth_access_log for all to authenticated using (false) with check (false);

revoke all on table public.entonet_auth_access_log from anon, authenticated;
revoke all on sequence public.entonet_auth_access_log_access_log_id_seq from anon, authenticated;

grant select, insert on table public.entonet_auth_access_log to service_role;
grant usage, select on sequence public.entonet_auth_access_log_access_log_id_seq to service_role;

comment on table public.entonet_auth_access_log is
  'Auditoria de intentos de acceso, ingresos exitosos, salidas y eventos de recuperacion de contrasena de EntoNet.';

create table if not exists public.entonet_public_traffic_log (
  traffic_log_id bigserial primary key,
  occurred_at timestamptz not null default now(),
  page text not null,
  path text,
  query_string text,
  href text,
  anonymous_session_id text,
  ip_address text,
  user_agent text,
  referrer text
);

create index if not exists entonet_public_traffic_log_occurred_at_idx
  on public.entonet_public_traffic_log (occurred_at desc);
create index if not exists entonet_public_traffic_log_page_idx
  on public.entonet_public_traffic_log (page);
create index if not exists entonet_public_traffic_log_anonymous_session_idx
  on public.entonet_public_traffic_log (anonymous_session_id);

alter table public.entonet_public_traffic_log enable row level security;

drop policy if exists entonet_public_traffic_log_deny_anon
  on public.entonet_public_traffic_log;
create policy entonet_public_traffic_log_deny_anon
  on public.entonet_public_traffic_log for all to anon using (false) with check (false);

drop policy if exists entonet_public_traffic_log_deny_authenticated
  on public.entonet_public_traffic_log;
create policy entonet_public_traffic_log_deny_authenticated
  on public.entonet_public_traffic_log for all to authenticated using (false) with check (false);

revoke all on table public.entonet_public_traffic_log from anon, authenticated;
revoke all on sequence public.entonet_public_traffic_log_traffic_log_id_seq from anon, authenticated;

grant select, insert on table public.entonet_public_traffic_log to service_role;
grant usage, select on sequence public.entonet_public_traffic_log_traffic_log_id_seq to service_role;

comment on table public.entonet_public_traffic_log is
  'Bitacora de trafico publico no autenticado en las paginas abiertas de EntoNet.';
