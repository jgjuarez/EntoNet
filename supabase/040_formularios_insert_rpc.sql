begin;

create or replace function public.entonet_insert_formulario_1(p_header jsonb, p_details jsonb)
returns bigint
language plpgsql
security definer
set search_path = ''
as $$
declare
  v_claims text := current_setting('request.jwt.claims', true);
  v_header public.formulario_1_ovitrampa_intake%rowtype;
  v_intake_id bigint;
begin
  if coalesce(v_claims, '') = '' or coalesce(v_claims::jsonb ->> 'role', '') <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  if p_header is null or p_details is null or jsonb_typeof(p_details) <> 'array' or jsonb_array_length(p_details) = 0 then
    raise exception 'El Formulario 1 requiere encabezado y al menos un sustrato';
  end if;

  v_header := jsonb_populate_record(null::public.formulario_1_ovitrampa_intake, p_header);
  insert into public.formulario_1_ovitrampa_intake (
    formulario_codigo, formulario_nombre, fecha_registro, pais, id_institucion,
    departamento, municipio, ciclo, ronda, codigo_formulario, fecha_colocacion,
    grupo_responsable_colocacion, cuadrante, codigo_casa, latitud, longitud,
    codigo_gps, ovitrampas_colocadas, fecha_retiro, grupo_responsable_retiro,
    ovitrampas_retiradas, retiro_buen_estado, retiro_sin_agua, retiro_sin_sustrato,
    retiro_sin_ovitrampa, retiro_movida, retiro_volteada, retiro_casa_cerrada,
    retiro_casa_cerrada_descripcion, fuente_formulario, creado_por
  ) values (
    v_header.formulario_codigo, v_header.formulario_nombre, v_header.fecha_registro,
    v_header.pais, v_header.id_institucion, v_header.departamento, v_header.municipio,
    v_header.ciclo, v_header.ronda, v_header.codigo_formulario, v_header.fecha_colocacion,
    v_header.grupo_responsable_colocacion, v_header.cuadrante, v_header.codigo_casa,
    v_header.latitud, v_header.longitud, v_header.codigo_gps, v_header.ovitrampas_colocadas,
    v_header.fecha_retiro, v_header.grupo_responsable_retiro, v_header.ovitrampas_retiradas,
    coalesce(v_header.retiro_buen_estado, 0), coalesce(v_header.retiro_sin_agua, 0),
    coalesce(v_header.retiro_sin_sustrato, 0), coalesce(v_header.retiro_sin_ovitrampa, 0),
    coalesce(v_header.retiro_movida, 0), coalesce(v_header.retiro_volteada, 0),
    coalesce(v_header.retiro_casa_cerrada, 0), v_header.retiro_casa_cerrada_descripcion,
    v_header.fuente_formulario, v_header.creado_por
  ) returning intake_id into v_intake_id;

  insert into public.formulario_1_ovitrampa_detalle_intake (intake_id, codigo_sustrato)
  select v_intake_id, detail.codigo_sustrato
  from jsonb_to_recordset(p_details) as detail(codigo_sustrato text);

  return v_intake_id;
end;
$$;

create or replace function public.entonet_insert_formulario_5(p_record jsonb)
returns bigint
language plpgsql
security definer
set search_path = ''
as $$
declare
  v_claims text := current_setting('request.jwt.claims', true);
  v_record public.formulario_5_alimentacion_conteo_intake%rowtype;
  v_intake_id bigint;
begin
  if coalesce(v_claims, '') = '' or coalesce(v_claims::jsonb ->> 'role', '') <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  if p_record is null then raise exception 'El Formulario 5 está vacío'; end if;

  v_record := jsonb_populate_record(null::public.formulario_5_alimentacion_conteo_intake, p_record);
  insert into public.formulario_5_alimentacion_conteo_intake (
    formulario_codigo, pais, id_institucion, departamento_numero, municipio_numero,
    ciclo, formulario_nombre, fecha_registro, cepa_poblacion, especie,
    generacion_filial_adultos, responsable_ingreso_jaula, fecha_jaula, numero_hembras,
    numero_machos, total_huevos_viables, responsable_alimentacion,
    tipo_alimentacion_codigo, tipo_alimentacion_descripcion, fecha_alimentacion_sangre,
    numero_charolas, observaciones_alimentacion, generacion_filial_huevos,
    codigo_sustrato, fecha_colocacion_sustrato, fecha_retiro_sustrato,
    numero_cuadro_sustrato, hv_huevos_viables, he_huevos_eclosionados,
    hc_huevos_canoa, hnf_huevos_no_fecundados, responsable_conteo_huevos,
    observaciones_generales, fuente_formulario, creado_por, creado_en
  ) values (
    v_record.formulario_codigo, v_record.pais, v_record.id_institucion,
    v_record.departamento_numero, v_record.municipio_numero, v_record.ciclo,
    v_record.formulario_nombre, v_record.fecha_registro, v_record.cepa_poblacion,
    v_record.especie, v_record.generacion_filial_adultos, v_record.responsable_ingreso_jaula,
    v_record.fecha_jaula, v_record.numero_hembras, v_record.numero_machos,
    v_record.total_huevos_viables, v_record.responsable_alimentacion,
    v_record.tipo_alimentacion_codigo, v_record.tipo_alimentacion_descripcion,
    v_record.fecha_alimentacion_sangre, v_record.numero_charolas,
    v_record.observaciones_alimentacion, v_record.generacion_filial_huevos,
    v_record.codigo_sustrato, v_record.fecha_colocacion_sustrato,
    v_record.fecha_retiro_sustrato, v_record.numero_cuadro_sustrato,
    v_record.hv_huevos_viables, v_record.he_huevos_eclosionados,
    v_record.hc_huevos_canoa, v_record.hnf_huevos_no_fecundados,
    v_record.responsable_conteo_huevos, v_record.observaciones_generales,
    v_record.fuente_formulario, v_record.creado_por, coalesce(v_record.creado_en, now())
  ) returning intake_id into v_intake_id;

  return v_intake_id;
end;
$$;

create or replace function public.entonet_insert_formulario_7(p_header jsonb, p_results jsonb, p_comments jsonb)
returns bigint
language plpgsql
security definer
set search_path = ''
as $$
declare
  v_claims text := current_setting('request.jwt.claims', true);
  v_header public.formulario_7_bioensayo_intake%rowtype;
  v_intake_id bigint;
begin
  if coalesce(v_claims, '') = '' or coalesce(v_claims::jsonb ->> 'role', '') <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;
  if p_header is null then raise exception 'El Formulario 7 está vacío'; end if;

  v_header := jsonb_populate_record(null::public.formulario_7_bioensayo_intake, p_header);
  insert into public.formulario_7_bioensayo_intake (
    formulario_codigo, formulario_nombre, fecha_registro, codigo_bioensayo,
    nombre_poblacion, pais, id_institucion, codigo_departamento, codigo_municipio,
    bioensayo_intensidad, bioensayo_diagnostica_1x, dosis_intensidad, sinergista_def,
    sinergista_pbo, sinergista_dm, sinergista_tipo, dosis_sinergista_ug_ml,
    resultado_diagnostico, fecha_realizacion_bioensayo, insecticida,
    solvente_utilizado, solvente_otro, dosis_intensidad_ug_ml, lote_insecticida,
    fecha_revestimiento_botellas, numero_usos_botella_e1, numero_usos_botella_e2,
    numero_usos_botella_e3, numero_usos_botella_e4, numero_usos_botella_c1,
    origen_material, edad_dias, edad_indefinida, codigo_especie_mosquito,
    fecha_separacion, hora_separacion, generacion_filial,
    generacion_filial_indefinida, codigo_responsable_revestimiento,
    codigo_responsable_bioensayo, codigo_control_calidad, codigo_revision_24h,
    temperatura_inicial_c, temperatura_final_c, humedad_relativa_inicial_pct,
    humedad_relativa_final_pct, hora_inicio_bioensayo, hora_final_bioensayo,
    fuente_formulario, nombre_quien_ingreso
  ) values (
    v_header.formulario_codigo, v_header.formulario_nombre, v_header.fecha_registro,
    v_header.codigo_bioensayo, v_header.nombre_poblacion, v_header.pais,
    v_header.id_institucion, v_header.codigo_departamento, v_header.codigo_municipio,
    v_header.bioensayo_intensidad, v_header.bioensayo_diagnostica_1x,
    v_header.dosis_intensidad, v_header.sinergista_def, v_header.sinergista_pbo,
    v_header.sinergista_dm, v_header.sinergista_tipo, v_header.dosis_sinergista_ug_ml,
    v_header.resultado_diagnostico, v_header.fecha_realizacion_bioensayo,
    v_header.insecticida, v_header.solvente_utilizado, v_header.solvente_otro,
    v_header.dosis_intensidad_ug_ml, v_header.lote_insecticida,
    v_header.fecha_revestimiento_botellas, v_header.numero_usos_botella_e1,
    v_header.numero_usos_botella_e2, v_header.numero_usos_botella_e3,
    v_header.numero_usos_botella_e4, v_header.numero_usos_botella_c1,
    v_header.origen_material, v_header.edad_dias, v_header.edad_indefinida,
    v_header.codigo_especie_mosquito, v_header.fecha_separacion,
    v_header.hora_separacion, v_header.generacion_filial,
    v_header.generacion_filial_indefinida, v_header.codigo_responsable_revestimiento,
    v_header.codigo_responsable_bioensayo, v_header.codigo_control_calidad,
    v_header.codigo_revision_24h, v_header.temperatura_inicial_c,
    v_header.temperatura_final_c, v_header.humedad_relativa_inicial_pct,
    v_header.humedad_relativa_final_pct, v_header.hora_inicio_bioensayo,
    v_header.hora_final_bioensayo, v_header.fuente_formulario,
    v_header.nombre_quien_ingreso
  ) returning intake_id into v_intake_id;

  if p_results is not null and jsonb_typeof(p_results) = 'array' and jsonb_array_length(p_results) > 0 then
    insert into public.formulario_7_bioensayo_resultado_intake
      (intake_id, fase, botella, tiempo_minutos, hora_lectura, vivos, incapacitados)
    select v_intake_id, result.fase, result.botella, result.tiempo_minutos,
      result.hora_lectura, result.vivos, result.incapacitados
    from jsonb_to_recordset(p_results) as result(
      fase text, botella text, tiempo_minutos integer, hora_lectura time,
      vivos integer, incapacitados integer
    );
  end if;

  if p_comments is not null and jsonb_typeof(p_comments) = 'array' and jsonb_array_length(p_comments) > 0 then
    insert into public.formulario_7_bioensayo_comentario_intake (intake_id, comentario, nombre)
    select v_intake_id, comment.comentario, comment.nombre
    from jsonb_to_recordset(p_comments) as comment(comentario text, nombre text);
  end if;

  return v_intake_id;
end;
$$;

revoke all on function public.entonet_insert_formulario_1(jsonb, jsonb) from public, anon, authenticated;
revoke all on function public.entonet_insert_formulario_5(jsonb) from public, anon, authenticated;
revoke all on function public.entonet_insert_formulario_7(jsonb, jsonb, jsonb) from public, anon, authenticated;

grant execute on function public.entonet_insert_formulario_1(jsonb, jsonb) to service_role;
grant execute on function public.entonet_insert_formulario_5(jsonb) to service_role;
grant execute on function public.entonet_insert_formulario_7(jsonb, jsonb, jsonb) to service_role;

commit;
