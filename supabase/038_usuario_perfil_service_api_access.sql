begin;

grant usage on schema public to service_role;
revoke all on table public.usuario_perfil from service_role;
grant select on table public.usuario_perfil to service_role;

revoke all on table public.usuario_perfil from anon, authenticated;

comment on table public.usuario_perfil is
  'Perfiles autorizados para EntoNet. Supabase Auth valida credenciales; la API privada del servidor consulta institución, rol y país mediante service_role.';

commit;
