-- Formulario 5
-- Consultas utiles, vistas, checks y transformaciones.

-- Lista base de registros para reconstruir o revisar capturas.
select
  intake_id,
  formulario_codigo,
  fecha_registro,
  pais,
  cepa_poblacion,
  especie,
  fecha_jaula,
  fecha_alimentacion_sangre,
  review_status,
  creado_por,
  creado_en
from public.formulario_5_alimentacion_conteo_intake
order by creado_en desc;

-- Verificacion de totales.
select
  intake_id,
  numero_hembras,
  numero_machos,
  total_individuos,
  hv_huevos_viables,
  he_huevos_eclosionados,
  hc_huevos_canoa,
  hnf_huevos_no_fecundados,
  total_huevos
from public.formulario_5_alimentacion_conteo_intake
order by intake_id desc;

-- Registros por especie y tipo de alimentacion.
select
  especie,
  tipo_alimentacion_codigo,
  count(*) as total
from public.formulario_5_alimentacion_conteo_intake
group by especie, tipo_alimentacion_codigo
order by especie, tipo_alimentacion_codigo;

-- Control de fechas de sustrato.
select
  intake_id,
  cepa_poblacion,
  fecha_colocacion_sustrato,
  fecha_retiro_sustrato
from public.formulario_5_alimentacion_conteo_intake
where fecha_colocacion_sustrato > fecha_retiro_sustrato;
