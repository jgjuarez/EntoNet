-- Server-only, atomic editing and confirmation for Formulario 7.
grant update on public.formulario_7_bioensayo_intake to service_role;
grant insert, delete on public.formulario_7_bioensayo_resultado_intake,
  public.formulario_7_bioensayo_comentario_intake to service_role;

create or replace function public.entonet_update_formulario_7(
  p_intake_id bigint, p_header jsonb, p_results jsonb, p_comments jsonb
) returns bigint
language plpgsql security invoker set search_path = ''
as $$
declare
  v_header public.formulario_7_bioensayo_intake%rowtype;
begin
  if current_user <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  if jsonb_typeof(p_header) is distinct from 'object'
    or jsonb_typeof(p_results) is distinct from 'array'
    or jsonb_typeof(p_comments) is distinct from 'array' then
    raise exception 'Encabezado, resultados y comentarios invalidos';
  end if;
  v_header := jsonb_populate_record(null::public.formulario_7_bioensayo_intake, p_header);
  update public.formulario_7_bioensayo_intake set
    formulario_codigo = v_header.formulario_codigo,
    formulario_nombre = v_header.formulario_nombre,
    fecha_registro = v_header.fecha_registro,
    codigo_bioensayo = v_header.codigo_bioensayo,
    nombre_poblacion = v_header.nombre_poblacion,
    pais = v_header.pais,
    id_institucion = v_header.id_institucion,
    codigo_departamento = v_header.codigo_departamento,
    codigo_municipio = v_header.codigo_municipio,
    bioensayo_intensidad = v_header.bioensayo_intensidad,
    bioensayo_diagnostica_1x = v_header.bioensayo_diagnostica_1x,
    dosis_intensidad = v_header.dosis_intensidad,
    sinergista_def = v_header.sinergista_def,
    sinergista_pbo = v_header.sinergista_pbo,
    sinergista_dm = v_header.sinergista_dm,
    sinergista_tipo = v_header.sinergista_tipo,
    dosis_sinergista_ug_ml = v_header.dosis_sinergista_ug_ml,
    resultado_diagnostico = v_header.resultado_diagnostico,
    fecha_realizacion_bioensayo = v_header.fecha_realizacion_bioensayo,
    insecticida = v_header.insecticida,
    solvente_utilizado = v_header.solvente_utilizado,
    solvente_otro = v_header.solvente_otro,
    dosis_intensidad_ug_ml = v_header.dosis_intensidad_ug_ml,
    lote_insecticida = v_header.lote_insecticida,
    fecha_revestimiento_botellas = v_header.fecha_revestimiento_botellas,
    numero_usos_botella_e1 = v_header.numero_usos_botella_e1,
    numero_usos_botella_e2 = v_header.numero_usos_botella_e2,
    numero_usos_botella_e3 = v_header.numero_usos_botella_e3,
    numero_usos_botella_e4 = v_header.numero_usos_botella_e4,
    numero_usos_botella_c1 = v_header.numero_usos_botella_c1,
    origen_material = v_header.origen_material,
    edad_dias = v_header.edad_dias,
    edad_indefinida = v_header.edad_indefinida,
    codigo_especie_mosquito = v_header.codigo_especie_mosquito,
    fecha_separacion = v_header.fecha_separacion,
    hora_separacion = v_header.hora_separacion,
    generacion_filial = v_header.generacion_filial,
    generacion_filial_indefinida = v_header.generacion_filial_indefinida,
    codigo_responsable_revestimiento = v_header.codigo_responsable_revestimiento,
    codigo_responsable_bioensayo = v_header.codigo_responsable_bioensayo,
    codigo_control_calidad = v_header.codigo_control_calidad,
    codigo_revision_24h = v_header.codigo_revision_24h,
    temperatura_inicial_c = v_header.temperatura_inicial_c,
    temperatura_final_c = v_header.temperatura_final_c,
    humedad_relativa_inicial_pct = v_header.humedad_relativa_inicial_pct,
    humedad_relativa_final_pct = v_header.humedad_relativa_final_pct,
    hora_inicio_bioensayo = v_header.hora_inicio_bioensayo,
    hora_final_bioensayo = v_header.hora_final_bioensayo,
    fuente_formulario = v_header.fuente_formulario,
    nombre_quien_ingreso = v_header.nombre_quien_ingreso,
    review_status = 'pending', review_notes = null, reviewed_by = null,
    reviewed_at = null, actualizado_en = now()
  where intake_id = p_intake_id;
  if not found then raise exception 'No se encontro el registro seleccionado'; end if;

  delete from public.formulario_7_bioensayo_resultado_intake where intake_id = p_intake_id;
  delete from public.formulario_7_bioensayo_comentario_intake where intake_id = p_intake_id;
  insert into public.formulario_7_bioensayo_resultado_intake
    (intake_id, fase, botella, tiempo_minutos, hora_lectura, vivos, incapacitados)
  select p_intake_id, r.fase, r.botella, r.tiempo_minutos, r.hora_lectura, r.vivos, r.incapacitados
  from jsonb_to_recordset(p_results) as r(
    fase text, botella text, tiempo_minutos integer, hora_lectura time, vivos integer, incapacitados integer
  );
  insert into public.formulario_7_bioensayo_comentario_intake (intake_id, comentario, nombre)
  select p_intake_id, c.comentario, c.nombre
  from jsonb_to_recordset(p_comments) as c(comentario text, nombre text);
  return p_intake_id;
end;
$$;

create or replace function public.entonet_confirm_formulario_7(
  p_intake_id bigint, p_review_notes text, p_reviewed_by text
) returns bigint
language plpgsql security invoker set search_path = ''
as $$
begin
  if current_user <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  update public.formulario_7_bioensayo_intake set
    review_status = 'reviewed', review_notes = nullif(p_review_notes, ''),
    reviewed_by = nullif(p_reviewed_by, ''), reviewed_at = now(), actualizado_en = now()
  where intake_id = p_intake_id;
  if not found then raise exception 'No se encontro el registro seleccionado'; end if;
  return p_intake_id;
end;
$$;

revoke all on function public.entonet_update_formulario_7(bigint, jsonb, jsonb, jsonb) from public, anon, authenticated;
revoke all on function public.entonet_confirm_formulario_7(bigint, text, text) from public, anon, authenticated;
grant execute on function public.entonet_update_formulario_7(bigint, jsonb, jsonb, jsonb) to service_role;
grant execute on function public.entonet_confirm_formulario_7(bigint, text, text) to service_role;
notify pgrst, 'reload schema';
