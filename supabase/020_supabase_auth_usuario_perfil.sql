begin;

create table if not exists public.usuario_perfil (
  usuario text primary key,
  user_id uuid unique,
  email text unique,
  id_institucion text not null default 'UVG',
  rol text not null,
  pais text not null,
  nombre text,
  activo boolean not null default true,
  creado_en timestamptz not null default now(),
  actualizado_en timestamptz not null default now(),
  constraint usuario_perfil_rol_check check (rol in ('Administrador', 'Supervisor', 'Digitador', 'Admin pais', 'Visor'))
);

create index if not exists usuario_perfil_institucion_idx
  on public.usuario_perfil (id_institucion);

create index if not exists usuario_perfil_pais_idx
  on public.usuario_perfil (pais);

create index if not exists usuario_perfil_activo_idx
  on public.usuario_perfil (activo);

insert into public.usuario_perfil (usuario, email, id_institucion, rol, pais, nombre, activo)
values
  ('jjuarezvaldez@gmail.com', 'jjuarezvaldez@gmail.com', 'UVG', 'Administrador', 'Guatemala', 'jjuarezvaldez@gmail.com', true),
  ('bahernandez@uvg.edu.gt', 'bahernandez@uvg.edu.gt', 'UVG', 'Supervisor', 'Guatemala', 'bahernandez@uvg.edu.gt', true),
  ('sambrocio@uvg.edu.gt', 'sambrocio@uvg.edu.gt', 'UVG', 'Supervisor', 'Guatemala', 'sambrocio@uvg.edu.gt', true)
on conflict (usuario) do update set
  email = excluded.email,
  id_institucion = excluded.id_institucion,
  rol = excluded.rol,
  pais = excluded.pais,
  nombre = coalesce(public.usuario_perfil.nombre, excluded.nombre),
  activo = excluded.activo,
  actualizado_en = now();

delete from public.usuario_perfil
where usuario in ('jjuarez', 'bhernandez', 'sambrocio', 'JGJuarez@uvg.edu.gt');

alter table public.usuario_perfil enable row level security;

drop policy if exists usuario_perfil_server_only on public.usuario_perfil;
create policy usuario_perfil_server_only
  on public.usuario_perfil
  as restrictive
  for all
  using (false)
  with check (false);

comment on table public.usuario_perfil is
  'Perfiles autorizados para EntoNet. Supabase Auth valida credenciales; esta tabla define institución, rol y país.';
comment on column public.usuario_perfil.usuario is
  'Usuario visible en EntoNet. Para Fase 1 corresponde al correo de Supabase Auth.';
comment on column public.usuario_perfil.user_id is
  'UUID de auth.users.id cuando el usuario de Supabase Auth haya sido creado.';
comment on column public.usuario_perfil.email is
  'Correo usado por Supabase Auth para iniciar sesión.';

commit;
