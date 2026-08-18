-- Formulario 7
-- Consultas utiles, vistas, checks y transformaciones.

-- Lista base de bioensayos para reconstruir o revisar capturas.
select
  intake_id,
  codigo_bioensayo,
  codigo_unico,
  fecha_registro,
  fecha_realizacion_bioensayo,
  nombre_poblacion,
  modalidad_bioensayo,
  review_status,
  creado_por,
  creado_en
from public.formulario_7_bioensayo_intake
order by creado_en desc;

-- Reconstruccion de resultados por bioensayo.
select
  i.codigo_bioensayo,
  r.fase,
  r.botella,
  r.tiempo_minutos,
  r.hora_lectura,
  r.vivos,
  r.incapacitados
from public.formulario_7_bioensayo_intake i
join public.formulario_7_bioensayo_resultado_intake r
  on r.intake_id = i.intake_id
order by i.creado_en desc, r.botella, r.fase, r.tiempo_minutos;

-- Comentarios asociados a cada bioensayo.
select
  i.codigo_bioensayo,
  c.orden,
  c.comentario,
  c.nombre
from public.formulario_7_bioensayo_intake i
join public.formulario_7_bioensayo_comentario_intake c
  on c.intake_id = i.intake_id
order by i.creado_en desc, c.orden;

-- Deteccion de codigos unicos duplicados o nulos.
select
  codigo_unico,
  count(*) as total
from public.formulario_7_bioensayo_intake
where codigo_unico is not null
group by codigo_unico
having count(*) > 1;
