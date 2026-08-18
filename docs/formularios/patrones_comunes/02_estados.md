# Estados

## Estados de captura

- `idle`
- `pending`
- `reviewed`
- `rejected`

## Estados visuales

- Informativo: instrucciones, ayuda, pasos y cargas parciales.
- Exito: guardado correcto, validacion correcta, descarga lista.
- Advertencia: campos faltantes, fechas dudosas, consistencia incompleta.
- Error: lectura fallida, columnas invalidas, codigo duplicado, datos fuera de regla.

## Regla reutilizable

- Todo formulario debe mostrar el estado actual del flujo.
- El estado de guardado debe verse separado del formulario principal.
- La revision debe dejar rastro de quien reviso y cuando.
