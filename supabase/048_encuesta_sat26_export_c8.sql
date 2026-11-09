-- Export view for the revised C8 questions; legacy columns remain available.
create or replace view public.encuesta_sat26_export_v2
with (security_invoker = true)
as
select
  export.*,
  intake.payload #>> '{prioridad_agenda_vigilancia}' as "prioridad_agenda_vigilancia",
  intake.payload #>> '{prioridad_agenda_control}' as "prioridad_agenda_control",
  intake.payload #>> '{frecuencia_interministerial_vigilancia}' as "frecuencia_interministerial_vigilancia",
  intake.payload #>> '{frecuencia_interministerial_control}' as "frecuencia_interministerial_control",
  intake.payload #>> '{motivadores_institucion_vigilancia_otros}' as "motivadores_institucion_vigilancia_otros",
  intake.payload #>> '{motivadores_institucion_control_otros}' as "motivadores_institucion_control_otros",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_vigilancia}') = 'array' then case when (intake.payload #> '{motivadores_institucion_vigilancia}') ? 'recursos' then 1 else 0 end else null end as "motivadores_institucion_vigilancia_sel_recursos",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_vigilancia}') = 'array' then case when (intake.payload #> '{motivadores_institucion_vigilancia}') ? 'apoyo_politico' then 1 else 0 end else null end as "motivadores_institucion_vigilancia_sel_apoyo_politico",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_vigilancia}') = 'array' then case when (intake.payload #> '{motivadores_institucion_vigilancia}') ? 'exitos' then 1 else 0 end else null end as "motivadores_institucion_vigilancia_sel_exitos",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_vigilancia}') = 'array' then case when (intake.payload #> '{motivadores_institucion_vigilancia}') ? 'indicadores' then 1 else 0 end else null end as "motivadores_institucion_vigilancia_sel_indicadores",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_vigilancia}') = 'array' then case when (intake.payload #> '{motivadores_institucion_vigilancia}') ? 'participacion' then 1 else 0 end else null end as "motivadores_institucion_vigilancia_sel_participacion",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_vigilancia}') = 'array' then case when (intake.payload #> '{motivadores_institucion_vigilancia}') ? 'alianzas' then 1 else 0 end else null end as "motivadores_institucion_vigilancia_sel_alianzas",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_vigilancia}') = 'array' then case when (intake.payload #> '{motivadores_institucion_vigilancia}') ? 'no_sabria' then 1 else 0 end else null end as "motivadores_institucion_vigilancia_sel_no_sabria",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_vigilancia}') = 'array' then case when (intake.payload #> '{motivadores_institucion_vigilancia}') ? 'otras' then 1 else 0 end else null end as "motivadores_institucion_vigilancia_sel_otras",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_vigilancia}') = 'array' then case when (intake.payload #> '{motivadores_institucion_vigilancia}') ? 'desconozco' then 1 else 0 end else null end as "motivadores_institucion_vigilancia_sel_desconozco",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_control}') = 'array' then case when (intake.payload #> '{motivadores_institucion_control}') ? 'recursos' then 1 else 0 end else null end as "motivadores_institucion_control_sel_recursos",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_control}') = 'array' then case when (intake.payload #> '{motivadores_institucion_control}') ? 'apoyo_politico' then 1 else 0 end else null end as "motivadores_institucion_control_sel_apoyo_politico",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_control}') = 'array' then case when (intake.payload #> '{motivadores_institucion_control}') ? 'exitos' then 1 else 0 end else null end as "motivadores_institucion_control_sel_exitos",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_control}') = 'array' then case when (intake.payload #> '{motivadores_institucion_control}') ? 'indicadores' then 1 else 0 end else null end as "motivadores_institucion_control_sel_indicadores",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_control}') = 'array' then case when (intake.payload #> '{motivadores_institucion_control}') ? 'participacion' then 1 else 0 end else null end as "motivadores_institucion_control_sel_participacion",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_control}') = 'array' then case when (intake.payload #> '{motivadores_institucion_control}') ? 'alianzas' then 1 else 0 end else null end as "motivadores_institucion_control_sel_alianzas",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_control}') = 'array' then case when (intake.payload #> '{motivadores_institucion_control}') ? 'no_sabria' then 1 else 0 end else null end as "motivadores_institucion_control_sel_no_sabria",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_control}') = 'array' then case when (intake.payload #> '{motivadores_institucion_control}') ? 'otras' then 1 else 0 end else null end as "motivadores_institucion_control_sel_otras",
  case when jsonb_typeof(intake.payload #> '{motivadores_institucion_control}') = 'array' then case when (intake.payload #> '{motivadores_institucion_control}') ? 'desconozco' then 1 else 0 end else null end as "motivadores_institucion_control_sel_desconozco"
from public.encuesta_sat26_export as export
join public.encuesta_sat26_intake as intake using (codigo_unico);

comment on view public.encuesta_sat26_export_v2 is
  'Exportacion SAT26 con columnas adicionales para las preguntas separadas de C8; conserva columnas historicas.';

revoke all on public.encuesta_sat26_export_v2 from public, anon, authenticated;
grant select on public.encuesta_sat26_export_v2 to service_role;
