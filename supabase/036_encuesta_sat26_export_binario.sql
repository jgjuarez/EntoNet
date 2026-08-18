-- Vista analítica plana de SAT26.
-- El JSONB original permanece intacto; las opciones múltiples se presentan como columnas 1/0.
drop view if exists public.encuesta_sat26_export;

create view public.encuesta_sat26_export
with (security_invoker = true)
as
select
  codigo_unico as "codigo_unico",
  submitted_at as "submitted_at",
  formulario_version as "formulario_version",
  review_status as "review_status",
  payload #>> '{nombre}' as "nombre",
  payload #>> '{cargo}' as "cargo",
  payload #>> '{organizacion}' as "organizacion",
  payload #>> '{country}' as "country",
  payload #>> '{contact_after}' as "contact_after",
  payload #>> '{dengue_2025}' as "dengue_2025",
  payload #>> '{filariasis_activa_2025}' as "filariasis_activa_2025",
  payload #>> '{filariasis_escenario_2025}' as "filariasis_escenario_2025",
  payload #>> '{malaria_2025}' as "malaria_2025",
  payload #>> '{plan_tipo}' as "plan_tipo",
  payload #>> '{plan_estado}' as "plan_estado",
  payload #>> '{plan_caracteristicas_tipo}' as "plan_caracteristicas_tipo",
  payload #>> '{plan_aedes_estado}' as "plan_aedes_estado",
  payload #>> '{plan_anopheles_estado}' as "plan_anopheles_estado",
  payload #>> '{plan_integrado_estado}' as "plan_integrado_estado",
  payload #>> '{plan_aedes_nombre}' as "plan_aedes_nombre",
  payload #>> '{plan_aedes_anio}' as "plan_aedes_anio",
  payload #>> '{plan_aedes_documento}' as "plan_aedes_documento",
  payload #>> '{plan_anopheles_nombre}' as "plan_anopheles_nombre",
  payload #>> '{plan_anopheles_anio}' as "plan_anopheles_anio",
  payload #>> '{plan_anopheles_documento}' as "plan_anopheles_documento",
  payload #>> '{plan_integrado_nombre}' as "plan_integrado_nombre",
  payload #>> '{plan_integrado_anio}' as "plan_integrado_anio",
  payload #>> '{plan_integrado_documento}' as "plan_integrado_documento",
  payload #>> '{reglamentos_nacionales}' as "reglamentos_nacionales",
  payload #>> '{reglamentos_documento}' as "reglamentos_documento",
  payload #>> '{prioridad_planes}' as "prioridad_planes",
  payload #>> '{asistencia_planes}' as "asistencia_planes",
  payload #>> '{normas_vigilancia_aedes}' as "c3a_1_normas_vigilancia_aedes",
  payload #>> '{normas_control_aedes}' as "c3a_2_normas_control_aedes",
  payload #>> '{normas_vigilancia_anopheles}' as "c3a_3_normas_vigilancia_anopheles",
  payload #>> '{normas_control_anopheles}' as "c3a_4_normas_control_anopheles",
  payload #>> '{plan_elemento_control_aedes}' as "c4a_1_plan_elemento_control_aedes",
  payload #>> '{plan_elemento_vigilancia_aedes}' as "c4a_2_plan_elemento_vigilancia_aedes",
  payload #>> '{plan_elemento_control_anopheles}' as "c4a_3_plan_elemento_control_anopheles",
  payload #>> '{plan_elemento_vigilancia_anopheles}' as "c4a_4_plan_elemento_vigilancia_anopheles",
  payload #>> '{plan_elemento_equipos_insecticidas}' as "c4a_5_plan_elemento_equipos_insecticidas",
  payload #>> '{plan_elemento_resistencia}' as "c4a_6_plan_elemento_resistencia",
  payload #>> '{plan_elemento_recursos_humanos}' as "c4a_7_plan_elemento_recursos_humanos",
  payload #>> '{plan_elemento_formacion}' as "c4a_8_plan_elemento_formacion",
  payload #>> '{plan_elemento_participacion}' as "c4a_9_plan_elemento_participacion",
  payload #>> '{plan_elemento_sistemas_info}' as "c4a_10_plan_elemento_sistemas_info",
  payload #>> '{plan_elemento_investigacion}' as "c4a_11_plan_elemento_investigacion",
  payload #>> '{plan_elemento_vulnerables}' as "c4a_12_plan_elemento_vulnerables",
  payload #>> '{plan_elemento_monitoreo}' as "c4a_13_plan_elemento_monitoreo",
  payload #>> '{descentralizacion}' as "descentralizacion",
  payload #>> '{legislacion_24_48}' as "legislacion_24_48",
  payload #>> '{grupo_interministerial}' as "grupo_interministerial",
  payload #>> '{grupo_interministerial_reunion}' as "grupo_interministerial_reunion",
  payload #>> '{prioridad_agenda}' as "prioridad_agenda",
  payload #>> '{factores_prioridad_otros}' as "factores_prioridad_otros",
  payload #>> '{frecuencia_interministerial}' as "frecuencia_interministerial",
  payload #>> '{motivadores_institucion_otros}' as "motivadores_institucion_otros",
  payload #>> '{presupuesto_vigilancia}' as "presupuesto_vigilancia",
  payload #>> '{presupuesto_control}' as "presupuesto_control",
  payload #>> '{fin_vigilancia_presupuesto}' as "fin_vigilancia_presupuesto",
  payload #>> '{fin_vigilancia_gestion}' as "fin_vigilancia_gestion",
  payload #>> '{fin_control_presupuesto}' as "fin_control_presupuesto",
  payload #>> '{fin_control_gestion}' as "fin_control_gestion",
  case
    when jsonb_typeof(payload #> '{arbovirus_2025}') = 'array'
      then case when (payload #> '{arbovirus_2025}') ? 'zika' then 1 else 0 end
    else null
  end as "arbovirus_2025_sel_zika",
  case
    when jsonb_typeof(payload #> '{arbovirus_2025}') = 'array'
      then case when (payload #> '{arbovirus_2025}') ? 'chikungunya' then 1 else 0 end
    else null
  end as "arbovirus_2025_sel_chikungunya",
  case
    when jsonb_typeof(payload #> '{arbovirus_2025}') = 'array'
      then case when (payload #> '{arbovirus_2025}') ? 'ninguno' then 1 else 0 end
    else null
  end as "arbovirus_2025_sel_ninguno",
  case
    when jsonb_typeof(payload #> '{arbovirus_2025}') = 'array'
      then case when (payload #> '{arbovirus_2025}') ? 'desconozco' then 1 else 0 end
    else null
  end as "arbovirus_2025_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{plan_aedes_caracteristicas}') = 'array'
      then case when (payload #> '{plan_aedes_caracteristicas}') ? 'nombre' then 1 else 0 end
    else null
  end as "plan_aedes_caracteristicas_sel_nombre",
  case
    when jsonb_typeof(payload #> '{plan_aedes_caracteristicas}') = 'array'
      then case when (payload #> '{plan_aedes_caracteristicas}') ? 'anio_actualizacion' then 1 else 0 end
    else null
  end as "plan_aedes_caracteristicas_sel_anio_actualizacion",
  case
    when jsonb_typeof(payload #> '{plan_aedes_caracteristicas}') = 'array'
      then case when (payload #> '{plan_aedes_caracteristicas}') ? 'documento' then 1 else 0 end
    else null
  end as "plan_aedes_caracteristicas_sel_documento",
  case
    when jsonb_typeof(payload #> '{plan_aedes_caracteristicas}') = 'array'
      then case when (payload #> '{plan_aedes_caracteristicas}') ? 'desconozco' then 1 else 0 end
    else null
  end as "plan_aedes_caracteristicas_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{plan_anopheles_caracteristicas}') = 'array'
      then case when (payload #> '{plan_anopheles_caracteristicas}') ? 'nombre' then 1 else 0 end
    else null
  end as "plan_anopheles_caracteristicas_sel_nombre",
  case
    when jsonb_typeof(payload #> '{plan_anopheles_caracteristicas}') = 'array'
      then case when (payload #> '{plan_anopheles_caracteristicas}') ? 'anio_actualizacion' then 1 else 0 end
    else null
  end as "plan_anopheles_caracteristicas_sel_anio_actualizacion",
  case
    when jsonb_typeof(payload #> '{plan_anopheles_caracteristicas}') = 'array'
      then case when (payload #> '{plan_anopheles_caracteristicas}') ? 'documento' then 1 else 0 end
    else null
  end as "plan_anopheles_caracteristicas_sel_documento",
  case
    when jsonb_typeof(payload #> '{plan_anopheles_caracteristicas}') = 'array'
      then case when (payload #> '{plan_anopheles_caracteristicas}') ? 'desconozco' then 1 else 0 end
    else null
  end as "plan_anopheles_caracteristicas_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{plan_integrado_caracteristicas}') = 'array'
      then case when (payload #> '{plan_integrado_caracteristicas}') ? 'nombre' then 1 else 0 end
    else null
  end as "plan_integrado_caracteristicas_sel_nombre",
  case
    when jsonb_typeof(payload #> '{plan_integrado_caracteristicas}') = 'array'
      then case when (payload #> '{plan_integrado_caracteristicas}') ? 'anio_actualizacion' then 1 else 0 end
    else null
  end as "plan_integrado_caracteristicas_sel_anio_actualizacion",
  case
    when jsonb_typeof(payload #> '{plan_integrado_caracteristicas}') = 'array'
      then case when (payload #> '{plan_integrado_caracteristicas}') ? 'documento' then 1 else 0 end
    else null
  end as "plan_integrado_caracteristicas_sel_documento",
  case
    when jsonb_typeof(payload #> '{plan_integrado_caracteristicas}') = 'array'
      then case when (payload #> '{plan_integrado_caracteristicas}') ? 'desconozco' then 1 else 0 end
    else null
  end as "plan_integrado_caracteristicas_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{factores_prioridad}') = 'array'
      then case when (payload #> '{factores_prioridad}') ? 'casos' then 1 else 0 end
    else null
  end as "factores_prioridad_sel_casos",
  case
    when jsonb_typeof(payload #> '{factores_prioridad}') = 'array'
      then case when (payload #> '{factores_prioridad}') ? 'autoridades' then 1 else 0 end
    else null
  end as "factores_prioridad_sel_autoridades",
  case
    when jsonb_typeof(payload #> '{factores_prioridad}') = 'array'
      then case when (payload #> '{factores_prioridad}') ? 'cooperacion' then 1 else 0 end
    else null
  end as "factores_prioridad_sel_cooperacion",
  case
    when jsonb_typeof(payload #> '{factores_prioridad}') = 'array'
      then case when (payload #> '{factores_prioridad}') ? 'costos' then 1 else 0 end
    else null
  end as "factores_prioridad_sel_costos",
  case
    when jsonb_typeof(payload #> '{factores_prioridad}') = 'array'
      then case when (payload #> '{factores_prioridad}') ? 'comunidad' then 1 else 0 end
    else null
  end as "factores_prioridad_sel_comunidad",
  case
    when jsonb_typeof(payload #> '{factores_prioridad}') = 'array'
      then case when (payload #> '{factores_prioridad}') ? 'experiencias' then 1 else 0 end
    else null
  end as "factores_prioridad_sel_experiencias",
  case
    when jsonb_typeof(payload #> '{factores_prioridad}') = 'array'
      then case when (payload #> '{factores_prioridad}') ? 'otros' then 1 else 0 end
    else null
  end as "factores_prioridad_sel_otros",
  case
    when jsonb_typeof(payload #> '{factores_prioridad}') = 'array'
      then case when (payload #> '{factores_prioridad}') ? 'desconozco' then 1 else 0 end
    else null
  end as "factores_prioridad_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{motivadores_institucion}') = 'array'
      then case when (payload #> '{motivadores_institucion}') ? 'recursos' then 1 else 0 end
    else null
  end as "motivadores_institucion_sel_recursos",
  case
    when jsonb_typeof(payload #> '{motivadores_institucion}') = 'array'
      then case when (payload #> '{motivadores_institucion}') ? 'apoyo_politico' then 1 else 0 end
    else null
  end as "motivadores_institucion_sel_apoyo_politico",
  case
    when jsonb_typeof(payload #> '{motivadores_institucion}') = 'array'
      then case when (payload #> '{motivadores_institucion}') ? 'exitos' then 1 else 0 end
    else null
  end as "motivadores_institucion_sel_exitos",
  case
    when jsonb_typeof(payload #> '{motivadores_institucion}') = 'array'
      then case when (payload #> '{motivadores_institucion}') ? 'indicadores' then 1 else 0 end
    else null
  end as "motivadores_institucion_sel_indicadores",
  case
    when jsonb_typeof(payload #> '{motivadores_institucion}') = 'array'
      then case when (payload #> '{motivadores_institucion}') ? 'participacion' then 1 else 0 end
    else null
  end as "motivadores_institucion_sel_participacion",
  case
    when jsonb_typeof(payload #> '{motivadores_institucion}') = 'array'
      then case when (payload #> '{motivadores_institucion}') ? 'alianzas' then 1 else 0 end
    else null
  end as "motivadores_institucion_sel_alianzas",
  case
    when jsonb_typeof(payload #> '{motivadores_institucion}') = 'array'
      then case when (payload #> '{motivadores_institucion}') ? 'no_sabria' then 1 else 0 end
    else null
  end as "motivadores_institucion_sel_no_sabria",
  case
    when jsonb_typeof(payload #> '{motivadores_institucion}') = 'array'
      then case when (payload #> '{motivadores_institucion}') ? 'otras' then 1 else 0 end
    else null
  end as "motivadores_institucion_sel_otras",
  case
    when jsonb_typeof(payload #> '{motivadores_institucion}') = 'array'
      then case when (payload #> '{motivadores_institucion}') ? 'desconozco' then 1 else 0 end
    else null
  end as "motivadores_institucion_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{rrhh_capacitacion_sistemas}') = 'array'
      then case when (payload #> '{rrhh_capacitacion_sistemas}') ? 'puesto_trabajo' then 1 else 0 end
    else null
  end as "rrhh_capacitacion_sistemas_sel_puesto_trabajo",
  case
    when jsonb_typeof(payload #> '{rrhh_capacitacion_sistemas}') = 'array'
      then case when (payload #> '{rrhh_capacitacion_sistemas}') ? 'cursos_nacionales' then 1 else 0 end
    else null
  end as "rrhh_capacitacion_sistemas_sel_cursos_nacionales",
  case
    when jsonb_typeof(payload #> '{rrhh_capacitacion_sistemas}') = 'array'
      then case when (payload #> '{rrhh_capacitacion_sistemas}') ? 'cursos_regionales_internacionales' then 1 else 0 end
    else null
  end as "rrhh_capacitacion_sistemas_sel_cursos_reg_int",
  case
    when jsonb_typeof(payload #> '{rrhh_capacitacion_sistemas}') = 'array'
      then case when (payload #> '{rrhh_capacitacion_sistemas}') ? 'posgrado' then 1 else 0 end
    else null
  end as "rrhh_capacitacion_sistemas_sel_posgrado",
  case
    when jsonb_typeof(payload #> '{rrhh_capacitacion_sistemas}') = 'array'
      then case when (payload #> '{rrhh_capacitacion_sistemas}') ? 'formadores' then 1 else 0 end
    else null
  end as "rrhh_capacitacion_sistemas_sel_formadores",
  case
    when jsonb_typeof(payload #> '{rrhh_capacitacion_sistemas}') = 'array'
      then case when (payload #> '{rrhh_capacitacion_sistemas}') ? 'sur_sur' then 1 else 0 end
    else null
  end as "rrhh_capacitacion_sistemas_sel_sur_sur",
  case
    when jsonb_typeof(payload #> '{rrhh_capacitacion_sistemas}') = 'array'
      then case when (payload #> '{rrhh_capacitacion_sistemas}') ? 'otro' then 1 else 0 end
    else null
  end as "rrhh_capacitacion_sistemas_sel_otro",
  case
    when jsonb_typeof(payload #> '{rrhh_capacitacion_sistemas}') = 'array'
      then case when (payload #> '{rrhh_capacitacion_sistemas}') ? 'desconozco' then 1 else 0 end
    else null
  end as "rrhh_capacitacion_sistemas_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{control_aedes_actividades}') = 'array'
      then case when (payload #> '{control_aedes_actividades}') ? 'Reducción de criaderos (ej. limpieza comunitaria)' then 1 else 0 end
    else null
  end as "control_aedes_actividades_sel_criaderos",
  case
    when jsonb_typeof(payload #> '{control_aedes_actividades}') = 'array'
      then case when (payload #> '{control_aedes_actividades}') ? 'Larvicidas (ej. temephos, IGRs, Bti)' then 1 else 0 end
    else null
  end as "control_aedes_actividades_sel_larvicidas",
  case
    when jsonb_typeof(payload #> '{control_aedes_actividades}') = 'array'
      then case when (payload #> '{control_aedes_actividades}') ? 'Rociado residual dirigido en interiores - <em>Aedes</em> (IRS-<em>Aedes</em>)' then 1 else 0 end
    else null
  end as "control_aedes_actividades_sel_irs_aedes",
  case
    when jsonb_typeof(payload #> '{control_aedes_actividades}') = 'array'
      then case when (payload #> '{control_aedes_actividades}') ? 'Rociado residual en exteriores - <em>Aedes</em> (ORS-<em>Aedes</em>)' then 1 else 0 end
    else null
  end as "control_aedes_actividades_sel_ors_aedes",
  case
    when jsonb_typeof(payload #> '{control_aedes_actividades}') = 'array'
      then case when (payload #> '{control_aedes_actividades}') ? 'Nebulización en interiores (insecticida no residual)' then 1 else 0 end
    else null
  end as "control_aedes_actividades_sel_nebulizacion_interiores",
  case
    when jsonb_typeof(payload #> '{control_aedes_actividades}') = 'array'
      then case when (payload #> '{control_aedes_actividades}') ? 'Nebulización en exteriores (insecticida no residual)' then 1 else 0 end
    else null
  end as "control_aedes_actividades_sel_nebulizacion_exteriores",
  case
    when jsonb_typeof(payload #> '{control_aedes_actividades}') = 'array'
      then case when (payload #> '{control_aedes_actividades}') ? '<em>Wolbachia</em>' then 1 else 0 end
    else null
  end as "control_aedes_actividades_sel_wolbachia",
  case
    when jsonb_typeof(payload #> '{control_aedes_actividades}') = 'array'
      then case when (payload #> '{control_aedes_actividades}') ? 'Distribución de mosquiteros a pacientes febriles' then 1 else 0 end
    else null
  end as "control_aedes_actividades_sel_mosquiteros_febriles",
  case
    when jsonb_typeof(payload #> '{control_aedes_actividades}') = 'array'
      then case when (payload #> '{control_aedes_actividades}') ? 'Distribución de repelentes a pacientes febriles' then 1 else 0 end
    else null
  end as "control_aedes_actividades_sel_repelentes_febriles",
  case
    when jsonb_typeof(payload #> '{control_aedes_actividades}') = 'array'
      then case when (payload #> '{control_aedes_actividades}') ? 'desconozco' then 1 else 0 end
    else null
  end as "control_aedes_actividades_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{control_aedes_otras}') = 'array'
      then case when (payload #> '{control_aedes_otras}') ? 'casas_prueba' then 1 else 0 end
    else null
  end as "control_aedes_otras_sel_casas_prueba",
  case
    when jsonb_typeof(payload #> '{control_aedes_otras}') = 'array'
      then case when (payload #> '{control_aedes_otras}') ? 'control_biologico' then 1 else 0 end
    else null
  end as "control_aedes_otras_sel_control_biologico",
  case
    when jsonb_typeof(payload #> '{control_aedes_otras}') = 'array'
      then case when (payload #> '{control_aedes_otras}') ? 'redes_hamacas' then 1 else 0 end
    else null
  end as "control_aedes_otras_sel_redes_hamacas",
  case
    when jsonb_typeof(payload #> '{control_aedes_otras}') = 'array'
      then case when (payload #> '{control_aedes_otras}') ? 'insecto_esteril' then 1 else 0 end
    else null
  end as "control_aedes_otras_sel_insecto_esteril",
  case
    when jsonb_typeof(payload #> '{control_aedes_otras}') = 'array'
      then case when (payload #> '{control_aedes_otras}') ? 'otro' then 1 else 0 end
    else null
  end as "control_aedes_otras_sel_otro",
  case
    when jsonb_typeof(payload #> '{control_aedes_otras}') = 'array'
      then case when (payload #> '{control_aedes_otras}') ? 'desconozco' then 1 else 0 end
    else null
  end as "control_aedes_otras_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{control_anopheles_actividades}') = 'array'
      then case when (payload #> '{control_anopheles_actividades}') ? 'Mosquiteros tratados con insecticida (ITNs)' then 1 else 0 end
    else null
  end as "control_anopheles_actividades_sel_itns",
  case
    when jsonb_typeof(payload #> '{control_anopheles_actividades}') = 'array'
      then case when (payload #> '{control_anopheles_actividades}') ? 'Mosquiteros con insecticida de larga duración (LLINs)' then 1 else 0 end
    else null
  end as "control_anopheles_actividades_sel_llins",
  case
    when jsonb_typeof(payload #> '{control_anopheles_actividades}') = 'array'
      then case when (payload #> '{control_anopheles_actividades}') ? 'Rociado residual en interiores (IRS)' then 1 else 0 end
    else null
  end as "control_anopheles_actividades_sel_irs",
  case
    when jsonb_typeof(payload #> '{control_anopheles_actividades}') = 'array'
      then case when (payload #> '{control_anopheles_actividades}') ? 'Aplicación de larvicidas (ej. temephos, IGRs, Bti)' then 1 else 0 end
    else null
  end as "control_anopheles_actividades_sel_larvicidas",
  case
    when jsonb_typeof(payload #> '{control_anopheles_actividades}') = 'array'
      then case when (payload #> '{control_anopheles_actividades}') ? 'Reducción de criaderos (modificación física, limpiezas comunitarias, etc.)' then 1 else 0 end
    else null
  end as "control_anopheles_actividades_sel_criaderos",
  case
    when jsonb_typeof(payload #> '{control_anopheles_actividades}') = 'array'
      then case when (payload #> '{control_anopheles_actividades}') ? 'desconozco' then 1 else 0 end
    else null
  end as "control_anopheles_actividades_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{control_anopheles_otras}') = 'array'
      then case when (payload #> '{control_anopheles_otras}') ? 'rociado_exteriores' then 1 else 0 end
    else null
  end as "control_anopheles_otras_sel_rociado_exteriores",
  case
    when jsonb_typeof(payload #> '{control_anopheles_otras}') = 'array'
      then case when (payload #> '{control_anopheles_otras}') ? 'casas_prueba' then 1 else 0 end
    else null
  end as "control_anopheles_otras_sel_casas_prueba",
  case
    when jsonb_typeof(payload #> '{control_anopheles_otras}') = 'array'
      then case when (payload #> '{control_anopheles_otras}') ? 'rociado_barrera' then 1 else 0 end
    else null
  end as "control_anopheles_otras_sel_rociado_barrera",
  case
    when jsonb_typeof(payload #> '{control_anopheles_otras}') = 'array'
      then case when (payload #> '{control_anopheles_otras}') ? 'control_biologico' then 1 else 0 end
    else null
  end as "control_anopheles_otras_sel_control_biologico",
  case
    when jsonb_typeof(payload #> '{control_anopheles_otras}') = 'array'
      then case when (payload #> '{control_anopheles_otras}') ? 'redes_hamacas' then 1 else 0 end
    else null
  end as "control_anopheles_otras_sel_redes_hamacas",
  case
    when jsonb_typeof(payload #> '{control_anopheles_otras}') = 'array'
      then case when (payload #> '{control_anopheles_otras}') ? 'otro' then 1 else 0 end
    else null
  end as "control_anopheles_otras_sel_otro",
  case
    when jsonb_typeof(payload #> '{control_anopheles_otras}') = 'array'
      then case when (payload #> '{control_anopheles_otras}') ? 'desconozco' then 1 else 0 end
    else null
  end as "control_anopheles_otras_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{control_irs_metodos}') = 'array'
      then case when (payload #> '{control_irs_metodos}') ? 'mortalidad_adultos' then 1 else 0 end
    else null
  end as "f5b_1_control_irs_metodos_sel_mortalidad_adultos",
  case
    when jsonb_typeof(payload #> '{control_irs_metodos}') = 'array'
      then case when (payload #> '{control_irs_metodos}') ? 'densidad' then 1 else 0 end
    else null
  end as "f5b_1_control_irs_metodos_sel_densidad",
  case
    when jsonb_typeof(payload #> '{control_irs_metodos}') = 'array'
      then case when (payload #> '{control_irs_metodos}') ? 'indice_inmaduros' then 1 else 0 end
    else null
  end as "f5b_1_control_irs_metodos_sel_indice_inmaduros",
  case
    when jsonb_typeof(payload #> '{control_irs_metodos}') = 'array'
      then case when (payload #> '{control_irs_metodos}') ? 'contacto_superficie' then 1 else 0 end
    else null
  end as "f5b_1_control_irs_metodos_sel_contacto_superficie",
  case
    when jsonb_typeof(payload #> '{control_irs_metodos}') = 'array'
      then case when (payload #> '{control_irs_metodos}') ? 'pared_rociada' then 1 else 0 end
    else null
  end as "f5b_1_control_irs_metodos_sel_pared_rociada",
  case
    when jsonb_typeof(payload #> '{control_irs_metodos}') = 'array'
      then case when (payload #> '{control_irs_metodos}') ? 'otro' then 1 else 0 end
    else null
  end as "f5b_1_control_irs_metodos_sel_otro",
  case
    when jsonb_typeof(payload #> '{control_irs_metodos}') = 'array'
      then case when (payload #> '{control_irs_metodos}') ? 'desconozco' then 1 else 0 end
    else null
  end as "f5b_1_control_irs_metodos_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{control_larvicidas_metodos}') = 'array'
      then case when (payload #> '{control_larvicidas_metodos}') ? 'habitats_tratados' then 1 else 0 end
    else null
  end as "f5b_2_control_larvicidas_metodos_sel_habitats_tratados",
  case
    when jsonb_typeof(payload #> '{control_larvicidas_metodos}') = 'array'
      then case when (payload #> '{control_larvicidas_metodos}') ? 'inspecciones' then 1 else 0 end
    else null
  end as "f5b_2_control_larvicidas_metodos_sel_inspecciones",
  case
    when jsonb_typeof(payload #> '{control_larvicidas_metodos}') = 'array'
      then case when (payload #> '{control_larvicidas_metodos}') ? 'densidad_adultos' then 1 else 0 end
    else null
  end as "f5b_2_control_larvicidas_metodos_sel_densidad_adultos",
  case
    when jsonb_typeof(payload #> '{control_larvicidas_metodos}') = 'array'
      then case when (payload #> '{control_larvicidas_metodos}') ? 'otro' then 1 else 0 end
    else null
  end as "f5b_2_control_larvicidas_metodos_sel_otro",
  case
    when jsonb_typeof(payload #> '{control_larvicidas_metodos}') = 'array'
      then case when (payload #> '{control_larvicidas_metodos}') ? 'desconozco' then 1 else 0 end
    else null
  end as "f5b_2_control_larvicidas_metodos_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{control_irs_aedes_metodos}') = 'array'
      then case when (payload #> '{control_irs_aedes_metodos}') ? 'bioensayos_pared' then 1 else 0 end
    else null
  end as "f5b_3_control_irs_aedes_metodos_sel_bioensayos_pared",
  case
    when jsonb_typeof(payload #> '{control_irs_aedes_metodos}') = 'array'
      then case when (payload #> '{control_irs_aedes_metodos}') ? 'otro' then 1 else 0 end
    else null
  end as "f5b_3_control_irs_aedes_metodos_sel_otro",
  case
    when jsonb_typeof(payload #> '{control_irs_aedes_metodos}') = 'array'
      then case when (payload #> '{control_irs_aedes_metodos}') ? 'desconozco' then 1 else 0 end
    else null
  end as "f5b_3_control_irs_aedes_metodos_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{control_nebulizacion_metodos}') = 'array'
      then case when (payload #> '{control_nebulizacion_metodos}') ? 'protocolos' then 1 else 0 end
    else null
  end as "f5b_4_control_nebulizacion_metodos_sel_protocolos",
  case
    when jsonb_typeof(payload #> '{control_nebulizacion_metodos}') = 'array'
      then case when (payload #> '{control_nebulizacion_metodos}') ? 'residuos' then 1 else 0 end
    else null
  end as "f5b_4_control_nebulizacion_metodos_sel_residuos",
  case
    when jsonb_typeof(payload #> '{control_nebulizacion_metodos}') = 'array'
      then case when (payload #> '{control_nebulizacion_metodos}') ? 'adultos' then 1 else 0 end
    else null
  end as "f5b_4_control_nebulizacion_metodos_sel_adultos",
  case
    when jsonb_typeof(payload #> '{control_nebulizacion_metodos}') = 'array'
      then case when (payload #> '{control_nebulizacion_metodos}') ? 'otro' then 1 else 0 end
    else null
  end as "f5b_4_control_nebulizacion_metodos_sel_otro",
  case
    when jsonb_typeof(payload #> '{control_nebulizacion_metodos}') = 'array'
      then case when (payload #> '{control_nebulizacion_metodos}') ? 'desconozco' then 1 else 0 end
    else null
  end as "f5b_4_control_nebulizacion_metodos_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Recolección con cebo humano (HLC o red de barrido)' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_hlc_red",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Trampa de luz CDC' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_luz_cdc",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Trampa BG Sentinel' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_bg_sentinel",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'BG Pro' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_bg_pro",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Otras trampas con ventilador' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_ventilador",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Trampas grávidas' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_gravidas",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Aspiración en interiores (aspiración manual, Prokopac)' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_aspiracion_interiores",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Aspiración en exteriores por aspiración' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_aspiracion_exteriores",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Otro método de descanso exterior (trampa de pozo, barrera de tela)' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_descanso_exterior",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Trampa con cebo humano' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_cebo_humano",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Trampa con cebo animal' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_cebo_animal",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Otro' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_otro",
  case
    when jsonb_typeof(payload #> '{vigilancia_aedes_trampas}') = 'array'
      then case when (payload #> '{vigilancia_aedes_trampas}') ? 'Desconozco' then 1 else 0 end
    else null
  end as "vigilancia_aedes_trampas_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Captura por aterrizaje humano' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_aterrizaje_humano",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Trampa con cebo humano' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_cebo_humano",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Trampa con cebo animal' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_cebo_animal",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Trampa de ventilador CDC' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_ventilador_cdc",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Trampa BG Sentinel' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_bg_sentinel",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Trampas grávidas' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_gravidas",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Aspiración en interiores (aspiración manual, Prokopac)' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_aspiracion_interiores",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Aspiración en exteriores por aspiración' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_aspiracion_exteriores",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Otro método de descanso exterior (trampa de pozo, barrera de tela)' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_descanso_exterior",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Trampa de salida por ventana' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_salida_ventana",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Trampas de caja pasiva' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_caja_pasiva",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Otro' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_otro",
  case
    when jsonb_typeof(payload #> '{vigilancia_anopheles_trampas}') = 'array'
      then case when (payload #> '{vigilancia_anopheles_trampas}') ? 'Desconozco' then 1 else 0 end
    else null
  end as "vigilancia_anopheles_trampas_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Cómo estratificar el control vectorial' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_estratificar",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Dónde implementar diferentes estrategias de control' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_estrategias",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Selección de larvicidas' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_larvicidas",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Selección de insecticidas para IRS-<em>Aedes</em>' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_insecticidas_irs_aedes",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Dónde aplicar insecticidas en hogares y alrededores' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_hogares",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Recipientes larvarios clave para reducción de criaderos' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_recipientes",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Dónde implementar control larval (<em>Anopheles</em>)' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_control_larval_anopheles",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Elección de LLIN/ITN a comprar' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_llin_itn",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Hábitats larvarios clave para manejo de criaderos (<em>Anopheles</em>)' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_habitats_anopheles",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Dónde establecer sitios de vigilancia vectorial' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_sitios_vigilancia",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Cómo optimizar mensajes de participación comunitaria' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_mensajes",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Otro' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_otro",
  case
    when jsonb_typeof(payload #> '{vigilancia_decisiones}') = 'array'
      then case when (payload #> '{vigilancia_decisiones}') ? 'Desconozco' then 1 else 0 end
    else null
  end as "vigilancia_decisiones_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{infra_laboratorio_capacidades}') = 'array'
      then case when (payload #> '{infra_laboratorio_capacidades}') ? 'pcr' then 1 else 0 end
    else null
  end as "infra_laboratorio_capacidades_sel_pcr",
  case
    when jsonb_typeof(payload #> '{infra_laboratorio_capacidades}') = 'array'
      then case when (payload #> '{infra_laboratorio_capacidades}') ? 'elisa' then 1 else 0 end
    else null
  end as "infra_laboratorio_capacidades_sel_elisa",
  case
    when jsonb_typeof(payload #> '{infra_laboratorio_capacidades}') = 'array'
      then case when (payload #> '{infra_laboratorio_capacidades}') ? 'ento_campo' then 1 else 0 end
    else null
  end as "infra_laboratorio_capacidades_sel_ento_campo",
  case
    when jsonb_typeof(payload #> '{infra_laboratorio_capacidades}') = 'array'
      then case when (payload #> '{infra_laboratorio_capacidades}') ? 'semi_campo' then 1 else 0 end
    else null
  end as "infra_laboratorio_capacidades_sel_semi_campo",
  case
    when jsonb_typeof(payload #> '{infra_laboratorio_capacidades}') = 'array'
      then case when (payload #> '{infra_laboratorio_capacidades}') ? 'otro' then 1 else 0 end
    else null
  end as "infra_laboratorio_capacidades_sel_otro",
  case
    when jsonb_typeof(payload #> '{infra_laboratorio_capacidades}') = 'array'
      then case when (payload #> '{infra_laboratorio_capacidades}') ? 'desconozco' then 1 else 0 end
    else null
  end as "infra_laboratorio_capacidades_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_herramientas}') = 'array'
      then case when (payload #> '{info_vigilancia_herramientas}') ? 'recoleccion' then 1 else 0 end
    else null
  end as "info_vigilancia_herramientas_sel_recoleccion",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_herramientas}') = 'array'
      then case when (payload #> '{info_vigilancia_herramientas}') ? 'almacenamiento' then 1 else 0 end
    else null
  end as "info_vigilancia_herramientas_sel_almacenamiento",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_herramientas}') = 'array'
      then case when (payload #> '{info_vigilancia_herramientas}') ? 'presentacion' then 1 else 0 end
    else null
  end as "info_vigilancia_herramientas_sel_presentacion",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_herramientas}') = 'array'
      then case when (payload #> '{info_vigilancia_herramientas}') ? 'desconozco' then 1 else 0 end
    else null
  end as "info_vigilancia_herramientas_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{info_control_herramientas}') = 'array'
      then case when (payload #> '{info_control_herramientas}') ? 'recoleccion' then 1 else 0 end
    else null
  end as "info_control_herramientas_sel_recoleccion",
  case
    when jsonb_typeof(payload #> '{info_control_herramientas}') = 'array'
      then case when (payload #> '{info_control_herramientas}') ? 'almacenamiento' then 1 else 0 end
    else null
  end as "info_control_herramientas_sel_almacenamiento",
  case
    when jsonb_typeof(payload #> '{info_control_herramientas}') = 'array'
      then case when (payload #> '{info_control_herramientas}') ? 'presentacion' then 1 else 0 end
    else null
  end as "info_control_herramientas_sel_presentacion",
  case
    when jsonb_typeof(payload #> '{info_control_herramientas}') = 'array'
      then case when (payload #> '{info_control_herramientas}') ? 'desconozco' then 1 else 0 end
    else null
  end as "info_control_herramientas_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_recoleccion}') = 'array'
      then case when (payload #> '{info_vigilancia_recoleccion}') ? 'papel' then 1 else 0 end
    else null
  end as "info_vigilancia_recoleccion_sel_papel",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_recoleccion}') = 'array'
      then case when (payload #> '{info_vigilancia_recoleccion}') ? 'smartphone_tablet' then 1 else 0 end
    else null
  end as "info_vigilancia_recoleccion_sel_smartphone_tablet",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_recoleccion}') = 'array'
      then case when (payload #> '{info_vigilancia_recoleccion}') ? 'fotografias' then 1 else 0 end
    else null
  end as "info_vigilancia_recoleccion_sel_fotografias",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_recoleccion}') = 'array'
      then case when (payload #> '{info_vigilancia_recoleccion}') ? 'codigos_barras' then 1 else 0 end
    else null
  end as "info_vigilancia_recoleccion_sel_codigos_barras",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_recoleccion}') = 'array'
      then case when (payload #> '{info_vigilancia_recoleccion}') ? 'gps' then 1 else 0 end
    else null
  end as "info_vigilancia_recoleccion_sel_gps",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_recoleccion}') = 'array'
      then case when (payload #> '{info_vigilancia_recoleccion}') ? 'planificacion_electronica' then 1 else 0 end
    else null
  end as "info_vigilancia_recoleccion_sel_planificacion_electronica",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_recoleccion}') = 'array'
      then case when (payload #> '{info_vigilancia_recoleccion}') ? 'otro' then 1 else 0 end
    else null
  end as "info_vigilancia_recoleccion_sel_otro",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_recoleccion}') = 'array'
      then case when (payload #> '{info_vigilancia_recoleccion}') ? 'desconozco' then 1 else 0 end
    else null
  end as "info_vigilancia_recoleccion_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_almacenamiento}') = 'array'
      then case when (payload #> '{info_vigilancia_almacenamiento}') ? 'papel' then 1 else 0 end
    else null
  end as "info_vigilancia_almacenamiento_sel_papel",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_almacenamiento}') = 'array'
      then case when (payload #> '{info_vigilancia_almacenamiento}') ? 'excel' then 1 else 0 end
    else null
  end as "info_vigilancia_almacenamiento_sel_excel",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_almacenamiento}') = 'array'
      then case when (payload #> '{info_vigilancia_almacenamiento}') ? 'dhis2' then 1 else 0 end
    else null
  end as "info_vigilancia_almacenamiento_sel_dhis2",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_almacenamiento}') = 'array'
      then case when (payload #> '{info_vigilancia_almacenamiento}') ? 'otra_bd_linea' then 1 else 0 end
    else null
  end as "info_vigilancia_almacenamiento_sel_otra_bd_linea",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_almacenamiento}') = 'array'
      then case when (payload #> '{info_vigilancia_almacenamiento}') ? 'access' then 1 else 0 end
    else null
  end as "info_vigilancia_almacenamiento_sel_access",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_almacenamiento}') = 'array'
      then case when (payload #> '{info_vigilancia_almacenamiento}') ? 'desarrollando_linea' then 1 else 0 end
    else null
  end as "info_vigilancia_almacenamiento_sel_desarrollando_linea",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_almacenamiento}') = 'array'
      then case when (payload #> '{info_vigilancia_almacenamiento}') ? 'otro' then 1 else 0 end
    else null
  end as "info_vigilancia_almacenamiento_sel_otro",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_almacenamiento}') = 'array'
      then case when (payload #> '{info_vigilancia_almacenamiento}') ? 'desconozco' then 1 else 0 end
    else null
  end as "info_vigilancia_almacenamiento_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_reporte}') = 'array'
      then case when (payload #> '{info_vigilancia_reporte}') ? 'excel_manual' then 1 else 0 end
    else null
  end as "info_vigilancia_reporte_sel_excel_manual",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_reporte}') = 'array'
      then case when (payload #> '{info_vigilancia_reporte}') ? 'paneles_linea' then 1 else 0 end
    else null
  end as "info_vigilancia_reporte_sel_paneles_linea",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_reporte}') = 'array'
      then case when (payload #> '{info_vigilancia_reporte}') ? 'mapas_electronicos' then 1 else 0 end
    else null
  end as "info_vigilancia_reporte_sel_mapas_electronicos",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_reporte}') = 'array'
      then case when (payload #> '{info_vigilancia_reporte}') ? 'informes_automaticos' then 1 else 0 end
    else null
  end as "info_vigilancia_reporte_sel_informes_automaticos",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_reporte}') = 'array'
      then case when (payload #> '{info_vigilancia_reporte}') ? 'otro' then 1 else 0 end
    else null
  end as "info_vigilancia_reporte_sel_otro",
  case
    when jsonb_typeof(payload #> '{info_vigilancia_reporte}') = 'array'
      then case when (payload #> '{info_vigilancia_reporte}') ? 'desconozco' then 1 else 0 end
    else null
  end as "info_vigilancia_reporte_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{info_control_recoleccion}') = 'array'
      then case when (payload #> '{info_control_recoleccion}') ? 'papel' then 1 else 0 end
    else null
  end as "info_control_recoleccion_sel_papel",
  case
    when jsonb_typeof(payload #> '{info_control_recoleccion}') = 'array'
      then case when (payload #> '{info_control_recoleccion}') ? 'smartphone_tablet' then 1 else 0 end
    else null
  end as "info_control_recoleccion_sel_smartphone_tablet",
  case
    when jsonb_typeof(payload #> '{info_control_recoleccion}') = 'array'
      then case when (payload #> '{info_control_recoleccion}') ? 'fotografias' then 1 else 0 end
    else null
  end as "info_control_recoleccion_sel_fotografias",
  case
    when jsonb_typeof(payload #> '{info_control_recoleccion}') = 'array'
      then case when (payload #> '{info_control_recoleccion}') ? 'codigos_barras' then 1 else 0 end
    else null
  end as "info_control_recoleccion_sel_codigos_barras",
  case
    when jsonb_typeof(payload #> '{info_control_recoleccion}') = 'array'
      then case when (payload #> '{info_control_recoleccion}') ? 'gps' then 1 else 0 end
    else null
  end as "info_control_recoleccion_sel_gps",
  case
    when jsonb_typeof(payload #> '{info_control_recoleccion}') = 'array'
      then case when (payload #> '{info_control_recoleccion}') ? 'planificacion_electronica' then 1 else 0 end
    else null
  end as "info_control_recoleccion_sel_planificacion_electronica",
  case
    when jsonb_typeof(payload #> '{info_control_recoleccion}') = 'array'
      then case when (payload #> '{info_control_recoleccion}') ? 'otro' then 1 else 0 end
    else null
  end as "info_control_recoleccion_sel_otro",
  case
    when jsonb_typeof(payload #> '{info_control_recoleccion}') = 'array'
      then case when (payload #> '{info_control_recoleccion}') ? 'desconozco' then 1 else 0 end
    else null
  end as "info_control_recoleccion_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{info_control_almacenamiento}') = 'array'
      then case when (payload #> '{info_control_almacenamiento}') ? 'papel' then 1 else 0 end
    else null
  end as "info_control_almacenamiento_sel_papel",
  case
    when jsonb_typeof(payload #> '{info_control_almacenamiento}') = 'array'
      then case when (payload #> '{info_control_almacenamiento}') ? 'excel' then 1 else 0 end
    else null
  end as "info_control_almacenamiento_sel_excel",
  case
    when jsonb_typeof(payload #> '{info_control_almacenamiento}') = 'array'
      then case when (payload #> '{info_control_almacenamiento}') ? 'dhis2' then 1 else 0 end
    else null
  end as "info_control_almacenamiento_sel_dhis2",
  case
    when jsonb_typeof(payload #> '{info_control_almacenamiento}') = 'array'
      then case when (payload #> '{info_control_almacenamiento}') ? 'otra_bd_linea' then 1 else 0 end
    else null
  end as "info_control_almacenamiento_sel_otra_bd_linea",
  case
    when jsonb_typeof(payload #> '{info_control_almacenamiento}') = 'array'
      then case when (payload #> '{info_control_almacenamiento}') ? 'access' then 1 else 0 end
    else null
  end as "info_control_almacenamiento_sel_access",
  case
    when jsonb_typeof(payload #> '{info_control_almacenamiento}') = 'array'
      then case when (payload #> '{info_control_almacenamiento}') ? 'desarrollando_linea' then 1 else 0 end
    else null
  end as "info_control_almacenamiento_sel_desarrollando_linea",
  case
    when jsonb_typeof(payload #> '{info_control_almacenamiento}') = 'array'
      then case when (payload #> '{info_control_almacenamiento}') ? 'otro' then 1 else 0 end
    else null
  end as "info_control_almacenamiento_sel_otro",
  case
    when jsonb_typeof(payload #> '{info_control_almacenamiento}') = 'array'
      then case when (payload #> '{info_control_almacenamiento}') ? 'desconozco' then 1 else 0 end
    else null
  end as "info_control_almacenamiento_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{info_control_reporte}') = 'array'
      then case when (payload #> '{info_control_reporte}') ? 'excel_manual' then 1 else 0 end
    else null
  end as "info_control_reporte_sel_excel_manual",
  case
    when jsonb_typeof(payload #> '{info_control_reporte}') = 'array'
      then case when (payload #> '{info_control_reporte}') ? 'paneles_linea' then 1 else 0 end
    else null
  end as "info_control_reporte_sel_paneles_linea",
  case
    when jsonb_typeof(payload #> '{info_control_reporte}') = 'array'
      then case when (payload #> '{info_control_reporte}') ? 'mapas_electronicos' then 1 else 0 end
    else null
  end as "info_control_reporte_sel_mapas_electronicos",
  case
    when jsonb_typeof(payload #> '{info_control_reporte}') = 'array'
      then case when (payload #> '{info_control_reporte}') ? 'informes_automaticos' then 1 else 0 end
    else null
  end as "info_control_reporte_sel_informes_automaticos",
  case
    when jsonb_typeof(payload #> '{info_control_reporte}') = 'array'
      then case when (payload #> '{info_control_reporte}') ? 'otro' then 1 else 0 end
    else null
  end as "info_control_reporte_sel_otro",
  case
    when jsonb_typeof(payload #> '{info_control_reporte}') = 'array'
      then case when (payload #> '{info_control_reporte}') ? 'desconozco' then 1 else 0 end
    else null
  end as "info_control_reporte_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{comunidad_actividades}') = 'array'
      then case when (payload #> '{comunidad_actividades}') ? 'vigilancia_vectorial' then 1 else 0 end
    else null
  end as "comunidad_actividades_sel_vigilancia_vectorial",
  case
    when jsonb_typeof(payload #> '{comunidad_actividades}') = 'array'
      then case when (payload #> '{comunidad_actividades}') ? 'control_vectorial' then 1 else 0 end
    else null
  end as "comunidad_actividades_sel_control_vectorial",
  case
    when jsonb_typeof(payload #> '{comunidad_actividades}') = 'array'
      then case when (payload #> '{comunidad_actividades}') ? 'comunicacion_riesgos_educacion' then 1 else 0 end
    else null
  end as "comunidad_actividades_sel_comunicacion_riesgos_educacion",
  case
    when jsonb_typeof(payload #> '{comunidad_actividades}') = 'array'
      then case when (payload #> '{comunidad_actividades}') ? 'reporte_enfermedades_brotes' then 1 else 0 end
    else null
  end as "comunidad_actividades_sel_reporte_enfermedades_brotes",
  case
    when jsonb_typeof(payload #> '{comunidad_actividades}') = 'array'
      then case when (payload #> '{comunidad_actividades}') ? 'investigacion' then 1 else 0 end
    else null
  end as "comunidad_actividades_sel_investigacion",
  case
    when jsonb_typeof(payload #> '{comunidad_actividades}') = 'array'
      then case when (payload #> '{comunidad_actividades}') ? 'otro' then 1 else 0 end
    else null
  end as "comunidad_actividades_sel_otro",
  case
    when jsonb_typeof(payload #> '{comunidad_actividades}') = 'array'
      then case when (payload #> '{comunidad_actividades}') ? 'desconozco' then 1 else 0 end
    else null
  end as "comunidad_actividades_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{comunidad_momento}') = 'array'
      then case when (payload #> '{comunidad_momento}') ? 'despues_brote' then 1 else 0 end
    else null
  end as "comunidad_momento_sel_despues_brote",
  case
    when jsonb_typeof(payload #> '{comunidad_momento}') = 'array'
      then case when (payload #> '{comunidad_momento}') ? 'regularmente_anio' then 1 else 0 end
    else null
  end as "comunidad_momento_sel_regularmente_anio",
  case
    when jsonb_typeof(payload #> '{comunidad_momento}') = 'array'
      then case when (payload #> '{comunidad_momento}') ? 'antes_lluvias' then 1 else 0 end
    else null
  end as "comunidad_momento_sel_antes_lluvias",
  case
    when jsonb_typeof(payload #> '{comunidad_momento}') = 'array'
      then case when (payload #> '{comunidad_momento}') ? 'otro' then 1 else 0 end
    else null
  end as "comunidad_momento_sel_otro",
  case
    when jsonb_typeof(payload #> '{comunidad_momento}') = 'array'
      then case when (payload #> '{comunidad_momento}') ? 'desconozco' then 1 else 0 end
    else null
  end as "comunidad_momento_sel_desconozco",
  payload #>> '{rrhh,sat26_rrhh_plan}' as "sat26_rrhh_plan",
  payload #>> '{rrhh,sat26_rrhh_organigrama}' as "sat26_rrhh_organigrama",
  payload #>> '{rrhh,sat26_rrhh_nacional_conoce_supervisor}' as "sat26_rrhh_nacional_conoce_supervisor",
  payload #>> '{rrhh,sat26_rrhh_nacional_conoce_director}' as "sat26_rrhh_nacional_conoce_director",
  payload #>> '{rrhh,sat26_rrhh_nacional_conoce_coordinador}' as "sat26_rrhh_nacional_conoce_coordinador",
  payload #>> '{rrhh,sat26_rrhh_nacional_conoce_ambiental}' as "sat26_rrhh_nacional_conoce_ambiental",
  payload #>> '{rrhh,sat26_rrhh_nacional_conoce_tecnico_campo}' as "sat26_rrhh_nacional_conoce_tecnico_campo",
  payload #>> '{rrhh,sat26_rrhh_nacional_conoce_tecnico_laboratorio}' as "sat26_rrhh_nacional_conoce_tecnico_laboratorio",
  payload #>> '{rrhh,sat26_rrhh_nacional_conoce_comunitaria}' as "sat26_rrhh_nacional_conoce_comunitaria",
  payload #>> '{rrhh,sat26_rrhh_nacional_conoce_datos}' as "sat26_rrhh_nacional_conoce_datos",
  payload #>> '{rrhh,sat26_rrhh_nacional_cantidad_supervisor}' as "sat26_rrhh_nacional_cantidad_supervisor",
  payload #>> '{rrhh,sat26_rrhh_nacional_cantidad_director}' as "sat26_rrhh_nacional_cantidad_director",
  payload #>> '{rrhh,sat26_rrhh_nacional_cantidad_coordinador}' as "sat26_rrhh_nacional_cantidad_coordinador",
  payload #>> '{rrhh,sat26_rrhh_nacional_cantidad_ambiental}' as "sat26_rrhh_nacional_cantidad_ambiental",
  payload #>> '{rrhh,sat26_rrhh_nacional_cantidad_tecnico_campo}' as "sat26_rrhh_nacional_cantidad_tecnico_campo",
  payload #>> '{rrhh,sat26_rrhh_nacional_cantidad_tecnico_laboratorio}' as "sat26_rrhh_nacional_cantidad_tecnico_laboratorio",
  payload #>> '{rrhh,sat26_rrhh_nacional_cantidad_comunitaria}' as "sat26_rrhh_nacional_cantidad_comunitaria",
  payload #>> '{rrhh,sat26_rrhh_nacional_cantidad_datos}' as "sat26_rrhh_nacional_cantidad_datos",
  payload #>> '{rrhh,sat26_rrhh_subnacional_conoce_supervisor}' as "sat26_rrhh_subnacional_conoce_supervisor",
  payload #>> '{rrhh,sat26_rrhh_subnacional_conoce_director}' as "sat26_rrhh_subnacional_conoce_director",
  payload #>> '{rrhh,sat26_rrhh_subnacional_conoce_coordinador}' as "sat26_rrhh_subnacional_conoce_coordinador",
  payload #>> '{rrhh,sat26_rrhh_subnacional_conoce_ambiental}' as "sat26_rrhh_subnacional_conoce_ambiental",
  payload #>> '{rrhh,sat26_rrhh_subnacional_conoce_tecnico_campo}' as "sat26_rrhh_subnacional_conoce_tecnico_campo",
  payload #>> '{rrhh,sat26_rrhh_subnacional_conoce_tecnico_laboratorio}' as "sat26_rrhh_subnacional_conoce_tecnico_laboratorio",
  payload #>> '{rrhh,sat26_rrhh_subnacional_conoce_comunitaria}' as "sat26_rrhh_subnacional_conoce_comunitaria",
  payload #>> '{rrhh,sat26_rrhh_subnacional_conoce_datos}' as "sat26_rrhh_subnacional_conoce_datos",
  payload #>> '{rrhh,sat26_rrhh_subnacional_cantidad_supervisor}' as "sat26_rrhh_subnacional_cantidad_supervisor",
  payload #>> '{rrhh,sat26_rrhh_subnacional_cantidad_director}' as "sat26_rrhh_subnacional_cantidad_director",
  payload #>> '{rrhh,sat26_rrhh_subnacional_cantidad_coordinador}' as "sat26_rrhh_subnacional_cantidad_coordinador",
  payload #>> '{rrhh,sat26_rrhh_subnacional_cantidad_ambiental}' as "sat26_rrhh_subnacional_cantidad_ambiental",
  payload #>> '{rrhh,sat26_rrhh_subnacional_cantidad_tecnico_campo}' as "sat26_rrhh_subnacional_cantidad_tecnico_campo",
  payload #>> '{rrhh,sat26_rrhh_subnacional_cantidad_tecnico_laboratorio}' as "sat26_rrhh_subnacional_cantidad_tecnico_laboratorio",
  payload #>> '{rrhh,sat26_rrhh_subnacional_cantidad_comunitaria}' as "sat26_rrhh_subnacional_cantidad_comunitaria",
  payload #>> '{rrhh,sat26_rrhh_subnacional_cantidad_datos}' as "sat26_rrhh_subnacional_cantidad_datos",
  payload #>> '{rrhh,sat26_rrhh_suficiencia_vigilancia}' as "sat26_rrhh_suficiencia_vigilancia",
  payload #>> '{rrhh,sat26_rrhh_suficiencia_control}' as "sat26_rrhh_suficiencia_control",
  payload #>> '{rrhh,sat26_rrhh_brecha_supervisor}' as "sat26_rrhh_brecha_supervisor",
  payload #>> '{rrhh,sat26_rrhh_brecha_director}' as "sat26_rrhh_brecha_director",
  payload #>> '{rrhh,sat26_rrhh_brecha_coordinador}' as "sat26_rrhh_brecha_coordinador",
  payload #>> '{rrhh,sat26_rrhh_brecha_ambiental}' as "sat26_rrhh_brecha_ambiental",
  payload #>> '{rrhh,sat26_rrhh_brecha_tecnico_campo}' as "sat26_rrhh_brecha_tecnico_campo",
  payload #>> '{rrhh,sat26_rrhh_brecha_tecnico_laboratorio}' as "sat26_rrhh_brecha_tecnico_laboratorio",
  payload #>> '{rrhh,sat26_rrhh_brecha_comunitaria}' as "sat26_rrhh_brecha_comunitaria",
  payload #>> '{rrhh,sat26_rrhh_brecha_datos}' as "sat26_rrhh_brecha_datos",
  payload #>> '{rrhh,sat26_rrhh_adicional_supervisor}' as "sat26_rrhh_adicional_supervisor",
  payload #>> '{rrhh,sat26_rrhh_adicional_director}' as "sat26_rrhh_adicional_director",
  payload #>> '{rrhh,sat26_rrhh_adicional_coordinador}' as "sat26_rrhh_adicional_coordinador",
  payload #>> '{rrhh,sat26_rrhh_adicional_ambiental}' as "sat26_rrhh_adicional_ambiental",
  payload #>> '{rrhh,sat26_rrhh_adicional_tecnico_campo}' as "sat26_rrhh_adicional_tecnico_campo",
  payload #>> '{rrhh,sat26_rrhh_adicional_tecnico_laboratorio}' as "sat26_rrhh_adicional_tecnico_laboratorio",
  payload #>> '{rrhh,sat26_rrhh_adicional_comunitaria}' as "sat26_rrhh_adicional_comunitaria",
  payload #>> '{rrhh,sat26_rrhh_adicional_datos}' as "sat26_rrhh_adicional_datos",
  payload #>> '{rrhh,sat26_rrhh_capacitacion_otro}' as "sat26_rrhh_capacitacion_otro",
  payload #>> '{rrhh_cont,sat26_rrhh_necesidad_capacitacion}' as "sat26_rrhh_necesidad_capacitacion",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_gestion_control}' as "sat26_rrhh_prioridad_gestion_control",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_gestion_vigilancia}' as "sat26_rrhh_prioridad_gestion_vigilancia",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_identificacion_mosquitos}' as "sat26_rrhh_prioridad_identificacion_mosquitos",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_operaciones_vigilancia}' as "sat26_rrhh_prioridad_operaciones_vigilancia",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_resistencia_insecticidas}' as "sat26_rrhh_prioridad_resistencia_insecticidas",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_participacion_comunitaria}' as "sat26_rrhh_prioridad_participacion_comunitaria",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_comunicacion_riesgos}' as "sat26_rrhh_prioridad_comunicacion_riesgos",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_soporte_datos}' as "sat26_rrhh_prioridad_soporte_datos",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_gis}' as "sat26_rrhh_prioridad_gis",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_analisis_datos}' as "sat26_rrhh_prioridad_analisis_datos",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_informes}' as "sat26_rrhh_prioridad_informes",
  payload #>> '{rrhh_cont,sat26_rrhh_prioridad_investigacion_operativa}' as "sat26_rrhh_prioridad_investigacion_operativa",
  payload #>> '{rrhh_cont,sat26_rrhh_otras_areas}' as "sat26_rrhh_otras_areas",
  payload #>> '{rrhh_cont,sat26_rrhh_capacitados_nacional}' as "sat26_rrhh_capacitados_nacional",
  payload #>> '{rrhh_cont,sat26_rrhh_capacitados_subnacional}' as "sat26_rrhh_capacitados_subnacional",
  payload #>> '{rrhh_cont,sat26_rrhh_modalidad_preferida}' as "sat26_rrhh_modalidad_preferida",
  payload #>> '{rrhh_cont,sat26_rrhh_online_implementacion}' as "sat26_rrhh_online_implementacion",
  payload #>> '{rrhh_cont,sat26_rrhh_online_modalidad}' as "sat26_rrhh_online_modalidad",
  payload #>> '{rrhh_cont,sat26_rrhh_acceso_computadora}' as "sat26_rrhh_acceso_computadora",
  payload #>> '{rrhh_cont,sat26_rrhh_cursos_previos}' as "sat26_rrhh_cursos_previos",
  payload #>> '{rrhh_cont,sat26_rrhh_cursos_previos_bien}' as "sat26_rrhh_cursos_previos_bien",
  payload #>> '{rrhh_cont,sat26_rrhh_cursos_previos_mal}' as "sat26_rrhh_cursos_previos_mal",
  payload #>> '{rrhh_cont,sat26_rrhh_personal_cargo}' as "sat26_rrhh_personal_cargo",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_gestion_control}' as "sat26_rrhh_formadores_gestion_control",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_gestion_vigilancia}' as "sat26_rrhh_formadores_gestion_vigilancia",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_identificacion_mosquitos}' as "sat26_rrhh_formadores_identificacion_mosquitos",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_operaciones_vigilancia}' as "sat26_rrhh_formadores_operaciones_vigilancia",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_resistencia_insecticidas}' as "sat26_rrhh_formadores_resistencia_insecticidas",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_participacion_comunitaria}' as "sat26_rrhh_formadores_participacion_comunitaria",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_comunicacion_riesgos}' as "sat26_rrhh_formadores_comunicacion_riesgos",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_soporte_datos}' as "sat26_rrhh_formadores_soporte_datos",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_gis}' as "sat26_rrhh_formadores_gis",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_analisis_datos}' as "sat26_rrhh_formadores_analisis_datos",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_informes}' as "sat26_rrhh_formadores_informes",
  payload #>> '{rrhh_cont,sat26_rrhh_formadores_investigacion_operativa}' as "sat26_rrhh_formadores_investigacion_operativa",
  payload #>> '{control,sat26_control_aedes_conocimiento}' as "sat26_control_aedes_conocimiento",
  payload #>> '{control,sat26_control_aedes_frecuencia_criaderos}' as "sat26_control_aedes_frecuencia_criaderos",
  payload #>> '{control,sat26_control_aedes_frecuencia_larvicidas}' as "sat26_control_aedes_frecuencia_larvicidas",
  payload #>> '{control,sat26_control_aedes_frecuencia_irs_aedes}' as "sat26_control_aedes_frecuencia_irs_aedes",
  payload #>> '{control,sat26_control_aedes_frecuencia_ors_aedes}' as "sat26_control_aedes_frecuencia_ors_aedes",
  payload #>> '{control,sat26_control_aedes_frecuencia_nebulizacion_interiores}' as "sat26_control_aedes_frecuencia_nebulizacion_interiores",
  payload #>> '{control,sat26_control_aedes_frecuencia_nebulizacion_exteriores}' as "sat26_control_aedes_frecuencia_nebulizacion_exteriores",
  payload #>> '{control,sat26_control_aedes_frecuencia_wolbachia}' as "sat26_control_aedes_frecuencia_wolbachia",
  payload #>> '{control,sat26_control_aedes_frecuencia_mosquiteros_febriles}' as "sat26_control_aedes_frecuencia_mosquiteros_febriles",
  payload #>> '{control,sat26_control_aedes_frecuencia_repelentes_febriles}' as "sat26_control_aedes_frecuencia_repelentes_febriles",
  payload #>> '{control,sat26_control_aedes_otras_implemento}' as "sat26_control_aedes_otras_implemento",
  payload #>> '{control,sat26_control_aedes_otras_descripcion}' as "sat26_control_aedes_otras_descripcion",
  payload #>> '{control,sat26_control_anopheles_conocimiento}' as "sat26_control_anopheles_conocimiento",
  payload #>> '{control,sat26_control_anopheles_frecuencia_itns}' as "sat26_control_anopheles_frecuencia_itns",
  payload #>> '{control,sat26_control_anopheles_frecuencia_llins}' as "sat26_control_anopheles_frecuencia_llins",
  payload #>> '{control,sat26_control_anopheles_frecuencia_irs}' as "sat26_control_anopheles_frecuencia_irs",
  payload #>> '{control,sat26_control_anopheles_frecuencia_larvicidas}' as "sat26_control_anopheles_frecuencia_larvicidas",
  payload #>> '{control,sat26_control_anopheles_frecuencia_criaderos}' as "sat26_control_anopheles_frecuencia_criaderos",
  payload #>> '{control,sat26_control_anopheles_otras_implemento}' as "sat26_control_anopheles_otras_implemento",
  payload #>> '{control,sat26_control_anopheles_otras_descripcion}' as "sat26_control_anopheles_otras_descripcion",
  payload #>> '{control,sat26_control_calidad_calidad_intervenciones}' as "sat26_control_calidad_calidad_intervenciones",
  payload #>> '{control,sat26_control_calidad_durabilidad_llin}' as "sat26_control_calidad_durabilidad_llin",
  payload #>> '{control,sat26_control_calidad_eficacia_llin}' as "sat26_control_calidad_eficacia_llin",
  payload #>> '{control,sat26_control_calidad_eficacia_irs}' as "sat26_control_calidad_eficacia_irs",
  payload #>> '{control,sat26_control_calidad_impacto_larvicidas}' as "sat26_control_calidad_impacto_larvicidas",
  payload #>> '{control,sat26_control_calidad_eficacia_irs_aedes}' as "sat26_control_calidad_eficacia_irs_aedes",
  payload #>> '{control,sat26_control_calidad_nebulizacion_espacial}' as "sat26_control_calidad_nebulizacion_espacial",
  payload #>> '{control,sat26_control_irs_otro}' as "f5b_1_1_control_irs_otro",
  payload #>> '{control,sat26_control_larvicidas_otro}' as "f5b_2_1_control_larvicidas_otro",
  payload #>> '{control,sat26_control_irs_aedes_otro}' as "f5b_3_1_control_irs_aedes_otro",
  payload #>> '{control,sat26_control_nebulizacion_otro}' as "f5b_4_1_control_nebulizacion_otro",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_presencia_adultos}' as "sat26_vigilancia_aedes_ind_presencia_adultos",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_densidad_adultos}' as "sat26_vigilancia_aedes_ind_densidad_adultos",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_tasa_picadura}' as "sat26_vigilancia_aedes_ind_tasa_picadura",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_horario_picadura}' as "sat26_vigilancia_aedes_ind_horario_picadura",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_lugar_picadura}' as "sat26_vigilancia_aedes_ind_lugar_picadura",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_descanso_interior}' as "sat26_vigilancia_aedes_ind_descanso_interior",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_descanso_exterior}' as "sat26_vigilancia_aedes_ind_descanso_exterior",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_resistencia_adultos}' as "sat26_vigilancia_aedes_ind_resistencia_adultos",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_resistencia_larvas}' as "sat26_vigilancia_aedes_ind_resistencia_larvas",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_intensidad_resistencia}' as "sat26_vigilancia_aedes_ind_intensidad_resistencia",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_habitat_larvario}' as "sat26_vigilancia_aedes_ind_habitat_larvario",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ind_habitats_clave}' as "sat26_vigilancia_aedes_ind_habitats_clave",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_presencia_adultos}' as "sat26_vigilancia_anopheles_ind_presencia_adultos",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_densidad_adultos}' as "sat26_vigilancia_anopheles_ind_densidad_adultos",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_tasa_picadura}' as "sat26_vigilancia_anopheles_ind_tasa_picadura",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_horario_picadura}' as "sat26_vigilancia_anopheles_ind_horario_picadura",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_lugar_picadura}' as "sat26_vigilancia_anopheles_ind_lugar_picadura",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_descanso_interior}' as "sat26_vigilancia_anopheles_ind_descanso_interior",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_descanso_exterior}' as "sat26_vigilancia_anopheles_ind_descanso_exterior",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_resistencia_adultos}' as "sat26_vigilancia_anopheles_ind_resistencia_adultos",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_resistencia_larvas}' as "sat26_vigilancia_anopheles_ind_resistencia_larvas",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_intensidad_resistencia}' as "sat26_vigilancia_anopheles_ind_intensidad_resistencia",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_habitat_larvario}' as "sat26_vigilancia_anopheles_ind_habitat_larvario",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ind_habitats_clave}' as "sat26_vigilancia_anopheles_ind_habitats_clave",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_sitios}' as "sat26_vigilancia_aedes_sitios",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_conoce_trampas}' as "sat26_vigilancia_aedes_conoce_trampas",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_trampa_otro}' as "sat26_vigilancia_aedes_trampa_otro",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ident_adultos}' as "sat26_vigilancia_aedes_ident_adultos",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ident_adultos_otro}' as "sat26_vigilancia_aedes_ident_adultos_otro",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ident_larvas}' as "sat26_vigilancia_aedes_ident_larvas",
  payload #>> '{vigilancia,sat26_vigilancia_aedes_ident_larvas_otro}' as "sat26_vigilancia_aedes_ident_larvas_otro",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_sitios}' as "sat26_vigilancia_anopheles_sitios",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_conoce_trampas}' as "sat26_vigilancia_anopheles_conoce_trampas",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_trampa_otro}' as "sat26_vigilancia_anopheles_trampa_otro",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ident_adultos}' as "sat26_vigilancia_anopheles_ident_adultos",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ident_adultos_otro}' as "sat26_vigilancia_anopheles_ident_adultos_otro",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ident_larvas}' as "sat26_vigilancia_anopheles_ident_larvas",
  payload #>> '{vigilancia,sat26_vigilancia_anopheles_ident_larvas_otro}' as "sat26_vigilancia_anopheles_ident_larvas_otro",
  payload #>> '{vigilancia,sat26_vigilancia_uso_datos}' as "sat26_vigilancia_uso_datos",
  payload #>> '{vigilancia,sat26_vigilancia_decisiones_otro}' as "sat26_vigilancia_decisiones_otro",
  payload #>> '{infraestructura,sat26_infra_vigilancia_sistema_logistico}' as "sat26_infra_vigilancia_sistema_logistico",
  payload #>> '{infraestructura,sat26_infra_vigilancia_transporte}' as "sat26_infra_vigilancia_transporte",
  payload #>> '{infraestructura,sat26_infra_vigilancia_oficina}' as "sat26_infra_vigilancia_oficina",
  payload #>> '{infraestructura,sat26_infra_vigilancia_laboratorio}' as "sat26_infra_vigilancia_laboratorio",
  payload #>> '{infraestructura,sat26_infra_vigilancia_insectario}' as "sat26_infra_vigilancia_insectario",
  payload #>> '{infraestructura,sat26_infra_vigilancia_computadoras}' as "sat26_infra_vigilancia_computadoras",
  payload #>> '{infraestructura,sat26_infra_vigilancia_trampas}' as "sat26_infra_vigilancia_trampas",
  payload #>> '{infraestructura,sat26_infra_vigilancia_suministros}' as "sat26_infra_vigilancia_suministros",
  payload #>> '{infraestructura,sat26_infra_vigilancia_moviles}' as "sat26_infra_vigilancia_moviles",
  payload #>> '{infraestructura,sat26_infra_vigilancia_adquisicion}' as "sat26_infra_vigilancia_adquisicion",
  payload #>> '{infraestructura,sat26_infra_laboratorio_gestionado}' as "sat26_infra_laboratorio_gestionado",
  payload #>> '{infraestructura,sat26_infra_conoce_laboratorio}' as "sat26_infra_conoce_laboratorio",
  payload #>> '{infraestructura,sat26_infra_laboratorio_capacidad_otro}' as "sat26_infra_laboratorio_capacidad_otro",
  payload #>> '{infraestructura,sat26_infra_insectario_gestionado}' as "sat26_infra_insectario_gestionado",
  payload #>> '{infraestructura,sat26_infra_colonia_aedes}' as "sat26_infra_colonia_aedes",
  payload #>> '{infraestructura,sat26_infra_colonia_anopheles}' as "sat26_infra_colonia_anopheles",
  payload #>> '{infraestructura,sat26_infra_control_sistema_logistico}' as "sat26_infra_control_sistema_logistico",
  payload #>> '{infraestructura,sat26_infra_control_transporte}' as "sat26_infra_control_transporte",
  payload #>> '{infraestructura,sat26_infra_control_oficina}' as "sat26_infra_control_oficina",
  payload #>> '{infraestructura,sat26_infra_control_bodega}' as "sat26_infra_control_bodega",
  payload #>> '{infraestructura,sat26_infra_control_computadoras}' as "sat26_infra_control_computadoras",
  payload #>> '{infraestructura,sat26_infra_control_rociado}' as "sat26_infra_control_rociado",
  payload #>> '{infraestructura,sat26_infra_control_llins}' as "sat26_infra_control_llins",
  payload #>> '{infraestructura,sat26_infra_control_insecticidas_irs}' as "sat26_infra_control_insecticidas_irs",
  payload #>> '{infraestructura,sat26_infra_control_mosquiteros_viremicos}' as "sat26_infra_control_mosquiteros_viremicos",
  payload #>> '{infraestructura,sat26_infra_control_larvicidas}' as "sat26_infra_control_larvicidas",
  payload #>> '{infraestructura,sat26_infra_control_epp}' as "sat26_infra_control_epp",
  payload #>> '{infraestructura,sat26_infra_control_otros_equipos}' as "sat26_infra_control_otros_equipos",
  payload #>> '{infraestructura,sat26_infra_control_cadena_suministro}' as "sat26_infra_control_cadena_suministro",
  payload #>> '{sistemas_info,sat26_info_vigilancia_recoleccion_usa}' as "sat26_info_vigilancia_recoleccion_usa",
  payload #>> '{sistemas_info,sat26_info_vigilancia_recoleccion_otro}' as "sat26_info_vigilancia_recoleccion_otro",
  payload #>> '{sistemas_info,sat26_info_vigilancia_apps}' as "sat26_info_vigilancia_apps",
  payload #>> '{sistemas_info,sat26_info_vigilancia_almacenamiento_otro}' as "sat26_info_vigilancia_almacenamiento_otro",
  payload #>> '{sistemas_info,sat26_info_vigilancia_reporte_otro}' as "sat26_info_vigilancia_reporte_otro",
  payload #>> '{sistemas_info,sat26_info_vigilancia_limitacion}' as "sat26_info_vigilancia_limitacion",
  payload #>> '{sistemas_info,sat26_info_control_recoleccion_usa}' as "sat26_info_control_recoleccion_usa",
  payload #>> '{sistemas_info,sat26_info_control_recoleccion_otro}' as "sat26_info_control_recoleccion_otro",
  payload #>> '{sistemas_info,sat26_info_control_apps}' as "sat26_info_control_apps",
  payload #>> '{sistemas_info,sat26_info_control_almacenamiento_otro}' as "sat26_info_control_almacenamiento_otro",
  payload #>> '{sistemas_info,sat26_info_control_reporte_otro}' as "sat26_info_control_reporte_otro",
  payload #>> '{sistemas_info,sat26_info_control_limitacion}' as "sat26_info_control_limitacion",
  payload #>> '{comunidad,sat26_comunidad_participacion}' as "sat26_comunidad_participacion",
  payload #>> '{comunidad,sat26_comunidad_actividades_otro}' as "sat26_comunidad_actividades_otro",
  payload #>> '{comunidad,sat26_comunidad_momento_otro}' as "sat26_comunidad_momento_otro",
  payload #>> '{investigacion,sat26_investigacion_agenda}' as "sat26_investigacion_agenda",
  payload #>> '{investigacion,sat26_investigacion_agenda_vectores}' as "sat26_investigacion_agenda_vectores",
  payload #>> '{investigacion,sat26_investigacion_operativa_aedes}' as "sat26_investigacion_operativa_aedes",
  payload #>> '{investigacion,sat26_investigacion_titulo}' as "k3a_investigacion_titulo",
  payload #>> '{investigacion,sat26_investigacion_referencias}' as "k3b_investigacion_referencias",
  payload #>> '{investigacion,sat26_investigacion_ref_nombre_informes}' as "k3c_1_1_ref_nombre_informes",
  payload #>> '{investigacion,sat26_investigacion_ref_nombre_articulos}' as "k3c_2_1_ref_nombre_articulos",
  payload #>> '{investigacion,sat26_investigacion_ref_nombre_congresos}' as "k3c_3_1_ref_nombre_congresos",
  payload #>> '{investigacion,sat26_investigacion_ref_nombre_multilaterales}' as "k3c_4_1_ref_nombre_multilaterales",
  payload #>> '{investigacion,sat26_investigacion_ref_nombre_tesis}' as "k3c_5_1_ref_nombre_tesis",
  payload #>> '{investigacion,sat26_investigacion_ref_nombre_financiamiento}' as "k3c_6_1_ref_nombre_financiamiento",
  payload #>> '{investigacion,sat26_investigacion_ref_nombre_prensa}' as "k3c_7_1_ref_nombre_prensa",
  payload #>> '{investigacion,sat26_investigacion_ref_link_informes}' as "k3d_1_1_ref_link_informes",
  payload #>> '{investigacion,sat26_investigacion_ref_link_articulos}' as "k3d_2_1_ref_link_articulos",
  payload #>> '{investigacion,sat26_investigacion_ref_link_congresos}' as "k3d_3_1_ref_link_congresos",
  payload #>> '{investigacion,sat26_investigacion_ref_link_multilaterales}' as "k3d_4_1_ref_link_multilaterales",
  payload #>> '{investigacion,sat26_investigacion_ref_link_tesis}' as "k3d_5_1_ref_link_tesis",
  payload #>> '{investigacion,sat26_investigacion_ref_link_financiamiento}' as "k3d_6_1_ref_link_financiamiento",
  payload #>> '{investigacion,sat26_investigacion_ref_link_prensa}' as "k3d_7_1_ref_link_prensa",
  payload #>> '{investigacion,sat26_investigacion_ref_documento_informes}' as "k3e_1_1_ref_documento_informes",
  payload #>> '{investigacion,sat26_investigacion_ref_documento_articulos}' as "k3e_2_1_ref_documento_articulos",
  payload #>> '{investigacion,sat26_investigacion_ref_documento_congresos}' as "k3e_3_1_ref_documento_congresos",
  payload #>> '{investigacion,sat26_investigacion_ref_documento_multilaterales}' as "k3e_4_1_ref_documento_multilaterales",
  payload #>> '{investigacion,sat26_investigacion_ref_documento_tesis}' as "k3e_5_1_ref_documento_tesis",
  payload #>> '{investigacion,sat26_investigacion_ref_documento_financiamiento}' as "k3e_6_1_ref_documento_financiamiento",
  payload #>> '{investigacion,sat26_investigacion_ref_documento_prensa}' as "k3e_7_1_ref_documento_prensa",
  payload #>> '{investigacion,sat26_investigacion_compartir_entonet}' as "k5_compartir_entonet",
  payload #>> '{investigacion,sat26_investigacion_compartir_ops}' as "k6_compartir_ops",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_informes}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_informes}') ? 'nombre' then 1 else 0 end
    else null
  end as "k3b_1_1_ref_tipo_informes_sel_nombre",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_informes}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_informes}') ? 'hipervinculo' then 1 else 0 end
    else null
  end as "k3b_1_1_ref_tipo_informes_sel_hipervinculo",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_informes}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_informes}') ? 'documento' then 1 else 0 end
    else null
  end as "k3b_1_1_ref_tipo_informes_sel_documento",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_informes}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_informes}') ? 'desconozco' then 1 else 0 end
    else null
  end as "k3b_1_1_ref_tipo_informes_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_articulos}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_articulos}') ? 'nombre' then 1 else 0 end
    else null
  end as "k3b_2_1_ref_tipo_articulos_sel_nombre",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_articulos}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_articulos}') ? 'hipervinculo' then 1 else 0 end
    else null
  end as "k3b_2_1_ref_tipo_articulos_sel_hipervinculo",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_articulos}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_articulos}') ? 'documento' then 1 else 0 end
    else null
  end as "k3b_2_1_ref_tipo_articulos_sel_documento",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_articulos}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_articulos}') ? 'desconozco' then 1 else 0 end
    else null
  end as "k3b_2_1_ref_tipo_articulos_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_congresos}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_congresos}') ? 'nombre' then 1 else 0 end
    else null
  end as "k3b_3_1_ref_tipo_congresos_sel_nombre",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_congresos}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_congresos}') ? 'hipervinculo' then 1 else 0 end
    else null
  end as "k3b_3_1_ref_tipo_congresos_sel_hipervinculo",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_congresos}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_congresos}') ? 'documento' then 1 else 0 end
    else null
  end as "k3b_3_1_ref_tipo_congresos_sel_documento",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_congresos}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_congresos}') ? 'desconozco' then 1 else 0 end
    else null
  end as "k3b_3_1_ref_tipo_congresos_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_multilaterales}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_multilaterales}') ? 'nombre' then 1 else 0 end
    else null
  end as "k3b_4_1_ref_tipo_multilaterales_sel_nombre",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_multilaterales}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_multilaterales}') ? 'hipervinculo' then 1 else 0 end
    else null
  end as "k3b_4_1_ref_tipo_multilaterales_sel_hipervinculo",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_multilaterales}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_multilaterales}') ? 'documento' then 1 else 0 end
    else null
  end as "k3b_4_1_ref_tipo_multilaterales_sel_documento",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_multilaterales}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_multilaterales}') ? 'desconozco' then 1 else 0 end
    else null
  end as "k3b_4_1_ref_tipo_multilaterales_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_tesis}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_tesis}') ? 'nombre' then 1 else 0 end
    else null
  end as "k3b_5_1_ref_tipo_tesis_sel_nombre",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_tesis}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_tesis}') ? 'hipervinculo' then 1 else 0 end
    else null
  end as "k3b_5_1_ref_tipo_tesis_sel_hipervinculo",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_tesis}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_tesis}') ? 'documento' then 1 else 0 end
    else null
  end as "k3b_5_1_ref_tipo_tesis_sel_documento",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_tesis}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_tesis}') ? 'desconozco' then 1 else 0 end
    else null
  end as "k3b_5_1_ref_tipo_tesis_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_financiamiento}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_financiamiento}') ? 'nombre' then 1 else 0 end
    else null
  end as "k3b_6_1_ref_tipo_financiamiento_sel_nombre",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_financiamiento}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_financiamiento}') ? 'hipervinculo' then 1 else 0 end
    else null
  end as "k3b_6_1_ref_tipo_financiamiento_sel_hipervinculo",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_financiamiento}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_financiamiento}') ? 'documento' then 1 else 0 end
    else null
  end as "k3b_6_1_ref_tipo_financiamiento_sel_documento",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_financiamiento}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_financiamiento}') ? 'desconozco' then 1 else 0 end
    else null
  end as "k3b_6_1_ref_tipo_financiamiento_sel_desconozco",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_prensa}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_prensa}') ? 'nombre' then 1 else 0 end
    else null
  end as "k3b_7_1_ref_tipo_prensa_sel_nombre",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_prensa}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_prensa}') ? 'hipervinculo' then 1 else 0 end
    else null
  end as "k3b_7_1_ref_tipo_prensa_sel_hipervinculo",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_prensa}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_prensa}') ? 'documento' then 1 else 0 end
    else null
  end as "k3b_7_1_ref_tipo_prensa_sel_documento",
  case
    when jsonb_typeof(payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_prensa}') = 'array'
      then case when (payload #> '{investigacion_ref_tipos,sat26_investigacion_ref_tipo_prensa}') ? 'desconozco' then 1 else 0 end
    else null
  end as "k3b_7_1_ref_tipo_prensa_sel_desconozco",
  payload #>> '{colaboracion_regional,sat26_regional_punto_focal}' as "sat26_regional_punto_focal",
  payload #>> '{colaboracion_regional,sat26_regional_acuerdos}' as "sat26_regional_acuerdos",
  payload #>> '{colaboracion_regional,sat26_regional_frecuencia_intercambio}' as "sat26_regional_frecuencia_intercambio",
  payload #>> '{colaboracion_regional,sat26_regional_sistemas_invasoras}' as "sat26_regional_sistemas_invasoras",
  payload #>> '{colaboracion_regional,sat26_regional_cambio_climatico}' as "sat26_regional_cambio_climatico",
  payload #>> '{colaboracion_regional,sat26_regional_redes}' as "sat26_regional_redes",
  payload #>> '{colaboracion_regional,sat26_regional_mecanismos}' as "sat26_regional_mecanismos",
  payload #>> '{colaboracion_regional,sat26_regional_plataformas}' as "sat26_regional_plataformas"
from public.encuesta_sat26_intake;

comment on view public.encuesta_sat26_export is
  'Exportación analítica SAT26: una fila por codigo_unico y una columna binaria 1/0 por opción de respuesta múltiple.';

revoke all on public.encuesta_sat26_export from public, anon, authenticated;
grant select on public.encuesta_sat26_export to service_role;
