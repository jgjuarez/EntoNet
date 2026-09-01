# Impresion

## Patrón general

- Generar un machote imprimible antes de trabajo de campo.
- Pedir ubicacion, codigos base y parametros de secuencia.
- Mostrar una vista previa del codigo generado cuando aplique.
- Descargar en Excel o CSV segun el flujo.

## Aplicacion por formulario

- Formulario 1:
  - machote de cuadrantes, casas y sustratos
  - codigo de formulario calculado
- Formulario 5:
  - machote CSV oficial
  - salida simple sin gran complejidad visual
- Formulario 7:
  - machote Excel con ubicacion, codigo de bioensayo y version
  - vista previa del codigo de bioensayo
- Formulario 6:
  - machote Excel con codigo de formulario, cuadrante, casa y sustrato
  - usa el codigo territorial del Formulario 1 porque no depende de poblacion
  - incluye secciones imprimibles de crianza larvaria, emergencia, conteo de adultos y destino

## Regla reutilizable

- La impresion debe ayudar a capturar, no solo a exportar.
- Si el codigo se construye, mostrar la formula al usuario.
- Si el machote depende de catalogos, validar esos catalogos antes de descargar.
