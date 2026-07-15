# EntoNet

Repositorio de trabajo para organizar formularios, bases de datos y herramientas de captura de informacion del insectario EntoNet.

## Estructura inicial

- `base_datos_entonet/`: plantillas CSV, diccionarios de datos y listas de validacion de formularios.
- `docs/`: notas de arquitectura, integracion entre computadoras y decisiones de base de datos.
- `shiny_app/`: espacio para incorporar el codigo de la aplicacion Shiny generada previamente.
- `supabase/`: scripts SQL y notas para montar la base de datos en Supabase.

## Flujo recomendado

1. Mantener GitHub como fuente central del proyecto.
2. Sincronizar cambios desde la computadora personal y la computadora de oficina mediante ramas o commits pequenos.
3. Guardar datos crudos y plantillas en CSV.
4. Usar Supabase como base operacional para captura en linea.
5. Conectar Shiny a Supabase usando variables de entorno, nunca claves escritas directamente en el codigo.

