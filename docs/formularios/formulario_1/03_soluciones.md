# Formulario 1

## Soluciones aplicadas

- Problema: el formulario necesita una sola captura por ingreso, pero varios sustratos por casa/cuadrante.
- Solucion: separar encabezado y detalle normalizado, con `intake_id` como llave de enlace.
- Motivo: facilita validacion, revision y reconstruccion del machote sin duplicar datos.
- Problema: el retiro puede existir parcial o completamente ausente.
- Solucion: permitir `fecha_retiro` nula y usar controles por suma de estados.
- Motivo: el campo se adapta mejor a trabajo de campo incompleto o historico.
- Problema: se necesita reabrir un formulario ya guardado para continuar captura.
- Solucion: el modulo de resumen consulta el `codigo_formulario` y repuebla toda la estructura.
- Motivo: evita volver a digitar datos ya capturados.
