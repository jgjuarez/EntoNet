begin;

-- Preserve a private, immutable trace between every legacy record and its new
-- paired Sinergista/Etanol record. The JSON snapshots permit a full audit even
-- after the legacy row and its cascading details are removed.
create table if not exists public.formulario_7_sinergista_migracion_historica (
  old_intake_id bigint primary key,
  new_sinergista_intake_id bigint not null unique
    references public.formulario_7_sinergista_intake(sinergista_intake_id) on delete restrict,
  codigo_bioensayo text not null unique,
  old_review_status text,
  old_review_notes text,
  old_reviewed_by text,
  old_reviewed_at timestamptz,
  old_header_snapshot jsonb not null,
  old_results_snapshot jsonb not null default '[]'::jsonb,
  old_comments_snapshot jsonb not null default '[]'::jsonb,
  migrado_en timestamptz not null default now()
);

alter table public.formulario_7_sinergista_migracion_historica enable row level security;

drop policy if exists formulario_7_sinergista_migracion_deny_anon
  on public.formulario_7_sinergista_migracion_historica;
create policy formulario_7_sinergista_migracion_deny_anon
  on public.formulario_7_sinergista_migracion_historica
  for all to anon using (false) with check (false);

drop policy if exists formulario_7_sinergista_migracion_deny_authenticated
  on public.formulario_7_sinergista_migracion_historica;
create policy formulario_7_sinergista_migracion_deny_authenticated
  on public.formulario_7_sinergista_migracion_historica
  for all to authenticated using (false) with check (false);

revoke all on table public.formulario_7_sinergista_migracion_historica
  from public, anon, authenticated;
grant select on table public.formulario_7_sinergista_migracion_historica
  to service_role;

-- Abort instead of merging two independent records that happen to share a
-- code. A migrated code must always have one unambiguous source and target.
do $$
begin
  if exists (
    select 1
    from public.formulario_7_bioensayo_intake old_header
    join public.formulario_7_sinergista_intake new_header
      on new_header.codigo_bioensayo = old_header.codigo_bioensayo
    where not coalesce(old_header.bioensayo_diagnostica_1x, false)
      and nullif(btrim(old_header.bioensayo_intensidad), '') is null
      and (
        coalesce(old_header.sinergista_def, false)
        or coalesce(old_header.sinergista_pbo, false)
        or coalesce(old_header.sinergista_dm, false)
        or nullif(btrim(old_header.sinergista_tipo), '') is not null
        or old_header.dosis_sinergista_ug_ml is not null
      )
      and not exists (
        select 1
        from public.formulario_7_sinergista_migracion_historica migration
        where migration.old_intake_id = old_header.intake_id
          and migration.new_sinergista_intake_id = new_header.sinergista_intake_id
      )
  ) then
    raise exception 'Hay codigos de Sinergistas repetidos entre la base anterior y la nueva; la migracion fue cancelada.';
  end if;

  if exists (
    select 1
    from public.formulario_7_bioensayo_intake old_header
    where not coalesce(old_header.bioensayo_diagnostica_1x, false)
      and nullif(btrim(old_header.bioensayo_intensidad), '') is null
      and (
        coalesce(old_header.sinergista_def, false)
        or coalesce(old_header.sinergista_pbo, false)
        or coalesce(old_header.sinergista_dm, false)
        or nullif(btrim(old_header.sinergista_tipo), '') is not null
        or old_header.dosis_sinergista_ug_ml is not null
      )
      and (
        old_header.dosis_sinergista_ug_ml is null
        or coalesce(
          nullif(btrim(old_header.sinergista_tipo), ''),
          case
            when old_header.sinergista_def then 'DEF'
            when old_header.sinergista_pbo then 'PBO'
            when old_header.sinergista_dm then 'DM'
          end
        ) is null
      )
  ) then
    raise exception 'Hay registros historicos sin tipo o dosis de Sinergista; la migracion fue cancelada.';
  end if;
end;
$$;

insert into public.formulario_7_sinergista_intake (
  review_status, review_notes, reviewed_by, reviewed_at, version_estructura,
  formulario_codigo, formulario_nombre, fecha_registro, codigo_bioensayo,
  nombre_poblacion, pais, id_institucion, codigo_departamento, codigo_municipio,
  sinergista_tipo, dosis_sinergista_ug_ml, sinergista_resultado_diagnostico,
  etanol_resultado_diagnostico, sinergista_mortalidad_corregida_pct,
  etanol_mortalidad_corregida_pct, sinergista_mortalidad_control_pct,
  etanol_mortalidad_control_pct, incluir_24h, fecha_realizacion_bioensayo,
  insecticida, solvente_utilizado, solvente_otro, dosis_intensidad_ug_ml,
  lote_insecticida, fecha_revestimiento_botellas, numero_usos_botella_e1,
  numero_usos_botella_e2, numero_usos_botella_e3, numero_usos_botella_e4,
  numero_usos_botella_c1, origen_material, edad_dias, edad_indefinida,
  codigo_especie_mosquito, fecha_separacion, hora_separacion, generacion_filial,
  generacion_filial_indefinida, codigo_responsable_revestimiento,
  codigo_responsable_bioensayo, codigo_control_calidad, codigo_revision_24h,
  temperatura_inicial_c, temperatura_final_c, humedad_relativa_inicial_pct,
  humedad_relativa_final_pct, hora_inicio_bioensayo, hora_final_bioensayo,
  fuente_formulario, nombre_quien_ingreso, creado_en, actualizado_en
)
select
  'pending',
  concat_ws(
    E'\n',
    nullif(btrim(old_header.review_notes), ''),
    'Migrado de la base historica de Formulario 7. Las lecturas de Etanol estan pendientes de ingreso.'
  ),
  null,
  null,
  'f7_sinergistas_historico_v1',
  old_header.formulario_codigo,
  old_header.formulario_nombre,
  old_header.fecha_registro,
  old_header.codigo_bioensayo,
  old_header.nombre_poblacion,
  old_header.pais,
  old_header.id_institucion,
  old_header.codigo_departamento,
  old_header.codigo_municipio,
  coalesce(
    nullif(btrim(old_header.sinergista_tipo), ''),
    case
      when old_header.sinergista_def then 'DEF'
      when old_header.sinergista_pbo then 'PBO'
      when old_header.sinergista_dm then 'DM'
    end
  ),
  old_header.dosis_sinergista_ug_ml,
  case old_header.resultado_diagnostico
    when 'Suceptible' then 'Susceptible'
    else old_header.resultado_diagnostico
  end,
  null,
  null,
  null,
  null,
  null,
  exists (
    select 1
    from public.formulario_7_bioensayo_resultado_intake result_24h
    where result_24h.intake_id = old_header.intake_id
      and result_24h.fase = 'kdr_24h'
      and result_24h.tiempo_minutos = 1440
  ),
  old_header.fecha_realizacion_bioensayo,
  old_header.insecticida,
  old_header.solvente_utilizado,
  old_header.solvente_otro,
  old_header.dosis_intensidad_ug_ml,
  old_header.lote_insecticida,
  old_header.fecha_revestimiento_botellas,
  old_header.numero_usos_botella_e1,
  old_header.numero_usos_botella_e2,
  old_header.numero_usos_botella_e3,
  old_header.numero_usos_botella_e4,
  old_header.numero_usos_botella_c1,
  old_header.origen_material,
  old_header.edad_dias,
  old_header.edad_indefinida,
  old_header.codigo_especie_mosquito,
  old_header.fecha_separacion,
  old_header.hora_separacion,
  old_header.generacion_filial,
  old_header.generacion_filial_indefinida,
  old_header.codigo_responsable_revestimiento,
  old_header.codigo_responsable_bioensayo,
  old_header.codigo_control_calidad,
  old_header.codigo_revision_24h,
  old_header.temperatura_inicial_c,
  old_header.temperatura_final_c,
  old_header.humedad_relativa_inicial_pct,
  old_header.humedad_relativa_final_pct,
  old_header.hora_inicio_bioensayo,
  old_header.hora_final_bioensayo,
  old_header.fuente_formulario,
  old_header.nombre_quien_ingreso,
  old_header.creado_en,
  now()
from public.formulario_7_bioensayo_intake old_header
where not coalesce(old_header.bioensayo_diagnostica_1x, false)
  and nullif(btrim(old_header.bioensayo_intensidad), '') is null
  and (
    coalesce(old_header.sinergista_def, false)
    or coalesce(old_header.sinergista_pbo, false)
    or coalesce(old_header.sinergista_dm, false)
    or nullif(btrim(old_header.sinergista_tipo), '') is not null
    or old_header.dosis_sinergista_ug_ml is not null
  );

insert into public.formulario_7_sinergista_migracion_historica (
  old_intake_id, new_sinergista_intake_id, codigo_bioensayo,
  old_review_status, old_review_notes, old_reviewed_by, old_reviewed_at,
  old_header_snapshot, old_results_snapshot, old_comments_snapshot
)
select
  old_header.intake_id,
  new_header.sinergista_intake_id,
  old_header.codigo_bioensayo,
  old_header.review_status,
  old_header.review_notes,
  old_header.reviewed_by,
  old_header.reviewed_at,
  to_jsonb(old_header),
  coalesce((
    select jsonb_agg(to_jsonb(old_result) order by old_result.resultado_id)
    from public.formulario_7_bioensayo_resultado_intake old_result
    where old_result.intake_id = old_header.intake_id
  ), '[]'::jsonb),
  coalesce((
    select jsonb_agg(to_jsonb(old_comment) order by old_comment.comentario_id)
    from public.formulario_7_bioensayo_comentario_intake old_comment
    where old_comment.intake_id = old_header.intake_id
  ), '[]'::jsonb)
from public.formulario_7_bioensayo_intake old_header
join public.formulario_7_sinergista_intake new_header
  on new_header.codigo_bioensayo = old_header.codigo_bioensayo
where not coalesce(old_header.bioensayo_diagnostica_1x, false)
  and nullif(btrim(old_header.bioensayo_intensidad), '') is null
  and (
    coalesce(old_header.sinergista_def, false)
    or coalesce(old_header.sinergista_pbo, false)
    or coalesce(old_header.sinergista_dm, false)
    or nullif(btrim(old_header.sinergista_tipo), '') is not null
    or old_header.dosis_sinergista_ug_ml is not null
  )
on conflict (old_intake_id) do nothing;

-- Legacy b1-b4 become e1-e4. The fifth legacy column c1 represented the fifth
-- pre-exposure group at 60 minutes, so it becomes e5 only in pretratamiento.
insert into public.formulario_7_sinergista_resultado_intake (
  sinergista_intake_id, tipo_set, etapa, botella, tiempo_minutos,
  hora_inicio, vivos, incapacitados, creado_en
)
select
  migration.new_sinergista_intake_id,
  'sinergista',
  case
    when old_result.fase = 'bioensayo' and old_result.tiempo_minutos = 60
      then 'pretratamiento'
    when old_result.fase = 'bioensayo' then 'bioensayo'
    else 'kdr_24h'
  end,
  case old_result.botella
    when 'b1' then 'e1'
    when 'b2' then 'e2'
    when 'b3' then 'e3'
    when 'b4' then 'e4'
    when 'c1' then case when old_result.tiempo_minutos = 60 then 'e5' else 'c1' end
  end,
  old_result.tiempo_minutos,
  old_result.hora_lectura,
  old_result.vivos,
  old_result.incapacitados,
  old_result.creado_en
from public.formulario_7_bioensayo_resultado_intake old_result
join public.formulario_7_sinergista_migracion_historica migration
  on migration.old_intake_id = old_result.intake_id
where (
    old_result.fase = 'bioensayo'
    and old_result.tiempo_minutos in (0, 15, 30, 45, 60)
  ) or (
    old_result.fase = 'kdr_24h'
    and old_result.tiempo_minutos = 1440
  )
on conflict (sinergista_intake_id, tipo_set, etapa, botella, tiempo_minutos)
do nothing;

insert into public.formulario_7_sinergista_comentario_intake (
  sinergista_intake_id, comentario, nombre, creado_en
)
select
  migration.new_sinergista_intake_id,
  old_comment.comentario,
  old_comment.nombre,
  old_comment.creado_en
from public.formulario_7_bioensayo_comentario_intake old_comment
join public.formulario_7_sinergista_migracion_historica migration
  on migration.old_intake_id = old_comment.intake_id
on conflict (sinergista_intake_id) do nothing;

-- Validate the complete copy before the old parent rows are deleted. Any
-- mismatch raises an exception and rolls the entire transaction back.
do $$
declare
  source_headers integer;
  mapped_headers integer;
  source_results integer;
  copied_results integer;
  source_comments integer;
  copied_comments integer;
begin
  select count(*) into source_headers
  from public.formulario_7_bioensayo_intake old_header
  where not coalesce(old_header.bioensayo_diagnostica_1x, false)
    and nullif(btrim(old_header.bioensayo_intensidad), '') is null
    and (
      coalesce(old_header.sinergista_def, false)
      or coalesce(old_header.sinergista_pbo, false)
      or coalesce(old_header.sinergista_dm, false)
      or nullif(btrim(old_header.sinergista_tipo), '') is not null
      or old_header.dosis_sinergista_ug_ml is not null
    );

  select count(*) into mapped_headers
  from public.formulario_7_bioensayo_intake old_header
  join public.formulario_7_sinergista_migracion_historica migration
    on migration.old_intake_id = old_header.intake_id
  join public.formulario_7_sinergista_intake new_header
    on new_header.sinergista_intake_id = migration.new_sinergista_intake_id
   and new_header.codigo_bioensayo = old_header.codigo_bioensayo
  where not coalesce(old_header.bioensayo_diagnostica_1x, false)
    and nullif(btrim(old_header.bioensayo_intensidad), '') is null
    and (
      coalesce(old_header.sinergista_def, false)
      or coalesce(old_header.sinergista_pbo, false)
      or coalesce(old_header.sinergista_dm, false)
      or nullif(btrim(old_header.sinergista_tipo), '') is not null
      or old_header.dosis_sinergista_ug_ml is not null
    );

  select count(*) into source_results
  from public.formulario_7_bioensayo_resultado_intake old_result
  join public.formulario_7_sinergista_migracion_historica migration
    on migration.old_intake_id = old_result.intake_id;

  select count(*) into copied_results
  from public.formulario_7_sinergista_resultado_intake new_result
  join public.formulario_7_sinergista_migracion_historica migration
    on migration.new_sinergista_intake_id = new_result.sinergista_intake_id
  join public.formulario_7_bioensayo_intake old_header
    on old_header.intake_id = migration.old_intake_id
  where new_result.tipo_set = 'sinergista';

  select count(*) into source_comments
  from public.formulario_7_bioensayo_comentario_intake old_comment
  join public.formulario_7_sinergista_migracion_historica migration
    on migration.old_intake_id = old_comment.intake_id;

  select count(*) into copied_comments
  from public.formulario_7_sinergista_comentario_intake new_comment
  join public.formulario_7_sinergista_migracion_historica migration
    on migration.new_sinergista_intake_id = new_comment.sinergista_intake_id
  join public.formulario_7_bioensayo_intake old_header
    on old_header.intake_id = migration.old_intake_id;

  if source_headers <> mapped_headers then
    raise exception 'Validacion fallida: % encabezados de origen y % encabezados migrados.', source_headers, mapped_headers;
  end if;
  if source_results <> copied_results then
    raise exception 'Validacion fallida: % lecturas de origen y % lecturas migradas.', source_results, copied_results;
  end if;
  if source_comments <> copied_comments then
    raise exception 'Validacion fallida: % comentarios de origen y % comentarios migrados.', source_comments, copied_comments;
  end if;
  if exists (
    select 1
    from public.formulario_7_sinergista_resultado_intake new_result
    join public.formulario_7_sinergista_migracion_historica migration
      on migration.new_sinergista_intake_id = new_result.sinergista_intake_id
    join public.formulario_7_bioensayo_intake old_header
      on old_header.intake_id = migration.old_intake_id
    where new_result.tipo_set = 'etanol'
  ) then
    raise exception 'Validacion fallida: los registros historicos no deben inventar lecturas de Etanol.';
  end if;
end;
$$;

delete from public.formulario_7_bioensayo_intake old_header
using public.formulario_7_sinergista_migracion_historica migration
where old_header.intake_id = migration.old_intake_id;

do $$
begin
  if exists (
    select 1
    from public.formulario_7_bioensayo_intake old_header
    where not coalesce(old_header.bioensayo_diagnostica_1x, false)
      and nullif(btrim(old_header.bioensayo_intensidad), '') is null
      and (
        coalesce(old_header.sinergista_def, false)
        or coalesce(old_header.sinergista_pbo, false)
        or coalesce(old_header.sinergista_dm, false)
        or nullif(btrim(old_header.sinergista_tipo), '') is not null
        or old_header.dosis_sinergista_ug_ml is not null
      )
  ) then
    raise exception 'La limpieza de Sinergistas en la base anterior no se completo.';
  end if;
end;
$$;

-- A migrated record cannot be confirmed until both paired sets contain every
-- required count. The 45-minute rows and individual historical times remain
-- optional; 24-hour counts are required only when incluir_24h is true.
create or replace function public.entonet_confirm_formulario_7_sinergista(
  p_sinergista_intake_id bigint,
  p_review_notes text,
  p_reviewed_by text
) returns bigint
language plpgsql
security definer
set search_path = ''
as $$
declare
  v_claims text := current_setting('request.jwt.claims', true);
  v_include_24h boolean;
  v_required_rows integer;
  v_complete_rows integer;
begin
  if coalesce(v_claims, '') = '' or coalesce(v_claims::jsonb ->> 'role', '') <> 'service_role' then
    raise exception 'Acceso restringido al servidor EntoNet' using errcode = '42501';
  end if;

  select incluir_24h into v_include_24h
  from public.formulario_7_sinergista_intake
  where sinergista_intake_id = p_sinergista_intake_id;
  if not found then raise exception 'No se encontro el registro seleccionado'; end if;

  v_required_rows := case when v_include_24h then 50 else 40 end;
  select count(*) into v_complete_rows
  from public.formulario_7_sinergista_resultado_intake result
  where result.sinergista_intake_id = p_sinergista_intake_id
    and result.tipo_set in ('sinergista', 'etanol')
    and (
      (result.etapa = 'pretratamiento' and result.tiempo_minutos = 60)
      or (result.etapa = 'bioensayo' and result.tiempo_minutos in (0, 15, 30))
      or (v_include_24h and result.etapa = 'kdr_24h' and result.tiempo_minutos = 1440)
    )
    and result.vivos is not null
    and result.incapacitados is not null;

  if v_complete_rows <> v_required_rows then
    raise exception
      'Complete las lecturas obligatorias de Sinergista y Etanol antes de confirmar (% de %).',
      v_complete_rows, v_required_rows
      using errcode = '23514';
  end if;

  update public.formulario_7_sinergista_intake set
    review_status = 'reviewed',
    review_notes = nullif(p_review_notes, ''),
    reviewed_by = nullif(p_reviewed_by, ''),
    reviewed_at = now(),
    actualizado_en = now()
  where sinergista_intake_id = p_sinergista_intake_id;
  return p_sinergista_intake_id;
end;
$$;

revoke all on function public.entonet_confirm_formulario_7_sinergista(bigint, text, text)
  from public, anon, authenticated;
grant execute on function public.entonet_confirm_formulario_7_sinergista(bigint, text, text)
  to service_role;

comment on table public.formulario_7_sinergista_migracion_historica is
  'Traza privada y snapshots de los registros historicos movidos de formulario_7_bioensayo_* a formulario_7_sinergista_*; Etanol queda pendiente de captura.';

comment on column public.formulario_7_sinergista_intake.version_estructura is
  'f7_sinergistas_v1 para captura pareada nativa; f7_sinergistas_historico_v1 para registros migrados cuyo set Etanol debe completarse.';

notify pgrst, 'reload schema';

commit;
