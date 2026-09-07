-- API-only updates for existing form workflows. No user records are changed by installation.
grant update, delete on public.formulario_1_ovitrampa_intake to service_role;
grant update (review_status, review_notes, reviewed_by, reviewed_at, actualizado_en),
  delete on public.formulario_5_alimentacion_conteo_intake to service_role;
grant delete on public.formulario_7_bioensayo_intake to service_role;
grant insert, delete on public.formulario_1_ovitrampa_detalle_intake to service_role;
grant insert on public.formulario_1_ovitrampa_eliminacion_audit,
  public.formulario_5_alimentacion_eliminacion_audit,
  public.formulario_7_bioensayo_eliminacion_audit to service_role;
grant usage on sequence public.formulario_1_ovitrampa_eliminacion_audit_eliminacion_id_seq,
  public.formulario_5_alimentacion_eliminacion_audit_eliminacion_id_seq,
  public.formulario_7_bioensayo_eliminacion_audit_eliminacion_id_seq to service_role;

create or replace function public.entonet_update_formulario_1(
  p_intake_id bigint, p_header jsonb, p_details jsonb
) returns bigint language plpgsql security invoker set search_path = ''
as $$
declare v_header public.formulario_1_ovitrampa_intake%rowtype;
begin
  if current_user <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  if jsonb_typeof(p_header) is distinct from 'object'
    or jsonb_typeof(p_details) is distinct from 'array' then
    raise exception 'Encabezado o sustratos invalidos';
  end if;
  if jsonb_array_length(p_details) = 0 then raise exception 'Se requiere al menos un sustrato'; end if;
  v_header := jsonb_populate_record(null::public.formulario_1_ovitrampa_intake, p_header);
  update public.formulario_1_ovitrampa_intake set
    formulario_codigo = v_header.formulario_codigo,
    formulario_nombre = v_header.formulario_nombre,
    fecha_registro = v_header.fecha_registro,
    pais = v_header.pais,
    id_institucion = v_header.id_institucion,
    departamento = v_header.departamento,
    municipio = v_header.municipio,
    ciclo = v_header.ciclo,
    ronda = v_header.ronda,
    codigo_formulario = v_header.codigo_formulario,
    fecha_colocacion = v_header.fecha_colocacion,
    grupo_responsable_colocacion = v_header.grupo_responsable_colocacion,
    cuadrante = v_header.cuadrante,
    codigo_casa = v_header.codigo_casa,
    latitud = v_header.latitud,
    longitud = v_header.longitud,
    codigo_gps = v_header.codigo_gps,
    ovitrampas_colocadas = v_header.ovitrampas_colocadas,
    fecha_retiro = v_header.fecha_retiro,
    grupo_responsable_retiro = v_header.grupo_responsable_retiro,
    ovitrampas_retiradas = v_header.ovitrampas_retiradas,
    retiro_buen_estado = v_header.retiro_buen_estado,
    retiro_sin_agua = v_header.retiro_sin_agua,
    retiro_sin_sustrato = v_header.retiro_sin_sustrato,
    retiro_sin_ovitrampa = v_header.retiro_sin_ovitrampa,
    retiro_movida = v_header.retiro_movida,
    retiro_volteada = v_header.retiro_volteada,
    retiro_casa_cerrada = v_header.retiro_casa_cerrada,
    retiro_casa_cerrada_descripcion = v_header.retiro_casa_cerrada_descripcion,
    fuente_formulario = v_header.fuente_formulario,
    creado_por = v_header.creado_por,
    review_status = 'pending', review_notes = null, reviewed_by = null,
    reviewed_at = null, actualizado_en = now()
  where intake_id = p_intake_id;
  if not found then raise exception 'No se encontro el registro seleccionado'; end if;
  delete from public.formulario_1_ovitrampa_detalle_intake where intake_id = p_intake_id;
  insert into public.formulario_1_ovitrampa_detalle_intake(intake_id, codigo_sustrato)
  select p_intake_id, d.codigo_sustrato from jsonb_to_recordset(p_details) as d(codigo_sustrato text);
  return p_intake_id;
end;
$$;

create or replace function public.entonet_confirm_formulario_1(
  p_intake_id bigint, p_review_notes text, p_reviewed_by text
) returns bigint language plpgsql security invoker set search_path = ''
as $$
begin
  if current_user <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  update public.formulario_1_ovitrampa_intake set review_status = 'reviewed',
    review_notes = nullif(p_review_notes, ''), reviewed_by = nullif(p_reviewed_by, ''),
    reviewed_at = now(), actualizado_en = now() where intake_id = p_intake_id;
  if not found then raise exception 'No se encontro el registro seleccionado'; end if;
  return p_intake_id;
end;
$$;

create or replace function public.entonet_review_formulario_5(
  p_intake_id bigint, p_status text, p_notes text, p_reviewed_by text, p_reviewed_at timestamptz
) returns bigint language plpgsql security invoker set search_path = ''
as $$
begin
  if current_user <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  if p_status is null or p_status not in ('pending','reviewed','rejected') then
    raise exception 'Estado de revision invalido';
  end if;
  update public.formulario_5_alimentacion_conteo_intake set review_status = p_status,
    review_notes = nullif(p_notes, ''), reviewed_by = nullif(p_reviewed_by, ''),
    reviewed_at = p_reviewed_at, actualizado_en = now() where intake_id = p_intake_id;
  if not found then raise exception 'No se encontro el registro seleccionado'; end if;
  return p_intake_id;
end;
$$;

create or replace function public.entonet_delete_formulario_1(
  p_intake_id bigint, p_reason text, p_deleted_by text
) returns table(intake_id bigint, codigo_formulario text, cuadrante text, codigo_casa text, review_status text)
language plpgsql security invoker set search_path = ''
as $$
declare v_record public.formulario_1_ovitrampa_intake%rowtype;
begin
  if current_user <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  if nullif(btrim(p_reason), '') is null then raise exception 'El motivo de eliminacion es obligatorio'; end if;
  select t.* into v_record from public.formulario_1_ovitrampa_intake t where t.intake_id = p_intake_id for update;
  if not found then raise exception 'No se encontro el registro seleccionado'; end if;
  insert into public.formulario_1_ovitrampa_eliminacion_audit (intake_id, codigo_formulario, cuadrante, codigo_casa, review_status, eliminado_por, motivo_eliminacion)
  values (p_intake_id, v_record.codigo_formulario, v_record.cuadrante, v_record.codigo_casa, v_record.review_status, nullif(p_deleted_by, ''), btrim(p_reason));
  delete from public.formulario_1_ovitrampa_detalle_intake t where t.intake_id = p_intake_id;
  delete from public.formulario_1_ovitrampa_intake t where t.intake_id = p_intake_id;
  return query select v_record.intake_id, v_record.codigo_formulario, v_record.cuadrante, v_record.codigo_casa, v_record.review_status;
end;
$$;

create or replace function public.entonet_delete_formulario_5(
  p_intake_id bigint, p_reason text, p_deleted_by text
) returns table(intake_id bigint, formulario_codigo text, cepa_poblacion text, especie text, review_status text)
language plpgsql security invoker set search_path = ''
as $$
declare v_record public.formulario_5_alimentacion_conteo_intake%rowtype;
begin
  if current_user <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  if nullif(btrim(p_reason), '') is null then raise exception 'El motivo de eliminacion es obligatorio'; end if;
  select t.* into v_record from public.formulario_5_alimentacion_conteo_intake t where t.intake_id = p_intake_id for update;
  if not found then raise exception 'No se encontro el registro seleccionado'; end if;
  insert into public.formulario_5_alimentacion_eliminacion_audit (intake_id, formulario_codigo, cepa_poblacion, especie, review_status, eliminado_por, motivo_eliminacion)
  values (p_intake_id, v_record.formulario_codigo, v_record.cepa_poblacion, v_record.especie, v_record.review_status, nullif(p_deleted_by, ''), btrim(p_reason));

  delete from public.formulario_5_alimentacion_conteo_intake t where t.intake_id = p_intake_id;
  return query select v_record.intake_id, v_record.formulario_codigo, v_record.cepa_poblacion, v_record.especie, v_record.review_status;
end;
$$;

create or replace function public.entonet_delete_formulario_7(
  p_intake_id bigint, p_reason text, p_deleted_by text
) returns table(intake_id bigint, codigo_bioensayo text, review_status text)
language plpgsql security invoker set search_path = ''
as $$
declare v_record public.formulario_7_bioensayo_intake%rowtype;
begin
  if current_user <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  if nullif(btrim(p_reason), '') is null then raise exception 'El motivo de eliminacion es obligatorio'; end if;
  select t.* into v_record from public.formulario_7_bioensayo_intake t where t.intake_id = p_intake_id for update;
  if not found then raise exception 'No se encontro el registro seleccionado'; end if;
  insert into public.formulario_7_bioensayo_eliminacion_audit (intake_id, codigo_bioensayo, review_status, eliminado_por, motivo_eliminacion)
  values (p_intake_id, v_record.codigo_bioensayo, v_record.review_status, nullif(p_deleted_by, ''), btrim(p_reason));
  delete from public.formulario_7_bioensayo_resultado_intake t where t.intake_id = p_intake_id;
  delete from public.formulario_7_bioensayo_comentario_intake t where t.intake_id = p_intake_id;
  delete from public.formulario_7_bioensayo_intake t where t.intake_id = p_intake_id;
  return query select v_record.intake_id, v_record.codigo_bioensayo, v_record.review_status;
end;
$$;

revoke all on function public.entonet_update_formulario_1(bigint, jsonb, jsonb) from public, anon, authenticated;
grant execute on function public.entonet_update_formulario_1(bigint, jsonb, jsonb) to service_role;

revoke all on function public.entonet_confirm_formulario_1(bigint, text, text) from public, anon, authenticated;
grant execute on function public.entonet_confirm_formulario_1(bigint, text, text) to service_role;

revoke all on function public.entonet_review_formulario_5(bigint, text, text, text, timestamptz) from public, anon, authenticated;
grant execute on function public.entonet_review_formulario_5(bigint, text, text, text, timestamptz) to service_role;

revoke all on function public.entonet_delete_formulario_1(bigint, text, text) from public, anon, authenticated;
grant execute on function public.entonet_delete_formulario_1(bigint, text, text) to service_role;

revoke all on function public.entonet_delete_formulario_5(bigint, text, text) from public, anon, authenticated;
grant execute on function public.entonet_delete_formulario_5(bigint, text, text) to service_role;

revoke all on function public.entonet_delete_formulario_7(bigint, text, text) from public, anon, authenticated;
grant execute on function public.entonet_delete_formulario_7(bigint, text, text) to service_role;
notify pgrst, 'reload schema';
