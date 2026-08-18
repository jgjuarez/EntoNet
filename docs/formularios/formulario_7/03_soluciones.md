# Formulario 7

## Soluciones aplicadas

- Problema: el formulario tiene muchos campos y reglas condicionales fuertes.
- Solucion: separar tabla de encabezado, tabla de resultados y tabla de comentarios.
- Motivo: simplifica revision, validacion y reconstruccion del formulario.
- Problema: el codigo final depende del tipo de bioensayo y de la trazabilidad de crianza.
- Solucion: generar `codigo_unico` con codigo de crianza, codigo final y fecha.
- Motivo: conserva el flujo historico y permite detectar repetidos.
- Problema: los sinergistas y la modalidad cambian la combinacion de campos validos.
- Solucion: imponer checks de consistencia en Supabase y reflejar la logica en Shiny.
- Motivo: evita capturas ambiguas entre diagnostica, intensidad y sinergistas.
- Problema: el formulario imprime una version resumida con campos de ubicacion y codigo bioensayo.
- Solucion: el modulo de impresion separa el codigo para generar machote XLSX.
- Motivo: mantiene un flujo de campo consistente con el registro digital.
