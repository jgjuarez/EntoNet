begin;

insert into public.usuario_perfil (usuario, email, id_institucion, rol, pais, nombre, activo)
values ('npadilla@uvg.edu.gt', 'npadilla@uvg.edu.gt', 'UVG', 'Supervisor', 'Guatemala', 'npadilla@uvg.edu.gt', true)
on conflict (usuario) do update set
  email = excluded.email,
  id_institucion = excluded.id_institucion,
  rol = excluded.rol,
  pais = excluded.pais,
  nombre = excluded.nombre,
  activo = excluded.activo,
  actualizado_en = now();

commit;
