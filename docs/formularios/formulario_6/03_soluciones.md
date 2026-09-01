# Formulario 6

## Soluciones aplicadas

- Problema: el Formulario 6 no tiene poblacion como identificador principal.
- Solucion: usar el bloque de codigo del Formulario 1: `codigo_formulario`, `cuadrante`, `codigo_casa` y `codigo_sustrato`.
- Motivo: conserva trazabilidad territorial y de sustrato sin inventar un campo de poblacion.
- Problema: el conteo de adultos depende de sumas que pueden capturarse con error.
- Solucion: definir `total_adultos_vivos`, `total_adultos_muertos` y `total_adultos` como campos derivados.
- Motivo: reduce errores de digitacion y facilita validaciones de revision.
- Problema: un lote de adultos puede dividirse entre bioensayo, colonia y descarte.
- Solucion: agregar campos de destino para documentar la distribucion inicial.
- Motivo: permite auditar disponibilidad de adultos sin duplicar registros de crianza.
