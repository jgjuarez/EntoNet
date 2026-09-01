-- Validar totales calculados de adultos en la tabla plana.
select
  codigo_formulario,
  cuadrante,
  codigo_casa,
  codigo_sustrato,
  fecha_conteo_adultos,
  numero_hembras_vivas,
  numero_machos_vivos,
  total_adultos_vivos,
  numero_hembras_muertas,
  numero_machos_muertos,
  total_adultos_muertos,
  total_adultos
from formulario_6_crianza_conteo_adultos
where coalesce(total_adultos_vivos, 0) <> coalesce(numero_hembras_vivas, 0) + coalesce(numero_machos_vivos, 0)
   or coalesce(total_adultos_muertos, 0) <> coalesce(numero_hembras_muertas, 0) + coalesce(numero_machos_muertos, 0)
   or coalesce(total_adultos, 0) <> coalesce(total_adultos_vivos, 0) + coalesce(total_adultos_muertos, 0);

-- Verificar que el destino de adultos vivos no exceda el total disponible.
select
  codigo_formulario,
  cuadrante,
  codigo_casa,
  codigo_sustrato,
  fecha_conteo_adultos,
  total_adultos_vivos,
  adultos_destino_bioensayo,
  adultos_destino_colonia,
  adultos_destino_descartados
from formulario_6_crianza_conteo_adultos
where coalesce(adultos_destino_bioensayo, 0)
    + coalesce(adultos_destino_colonia, 0)
    + coalesce(adultos_destino_descartados, 0) > coalesce(total_adultos_vivos, 0);
