-- Formulario 1
-- Consultas utiles, vistas, checks y transformaciones.

-- Lista base de ingresos para reconstruir o revisar capturas.
select
  intake_id,
  codigo_formulario,
  fecha_registro,
  pais,
  ciclo,
  ronda,
  cuadrante,
  codigo_casa,
  ovitrampas_colocadas,
  ovitrampas_retiradas,
  review_status,
  creado_por,
  creado_en
from public.formulario_1_ovitrampa_intake
order by creado_en desc;

-- Detalle de sustratos por ingreso.
select
  h.intake_id,
  h.codigo_formulario,
  h.cuadrante,
  h.codigo_casa,
  d.codigo_sustrato
from public.formulario_1_ovitrampa_intake h
left join public.formulario_1_ovitrampa_detalle_intake d
  on d.intake_id = h.intake_id
order by h.intake_id desc, d.codigo_sustrato;

-- Resumen por cuadrante y casa.
select
  cuadrante,
  codigo_casa,
  count(*) as sustratos
from public.formulario_1_ovitrampa_detalle_intake d
join public.formulario_1_ovitrampa_intake h on h.intake_id = d.intake_id
group by cuadrante, codigo_casa
order by cuadrante, codigo_casa;

-- Chequeo de retiro contra colocación.
select
  intake_id,
  codigo_formulario,
  ovitrampas_colocadas,
  ovitrampas_retiradas,
  retiro_buen_estado,
  retiro_sin_agua,
  retiro_sin_sustrato
from public.formulario_1_ovitrampa_intake
where ovitrampas_retiradas is not null
  and ovitrampas_retiradas > ovitrampas_colocadas;
