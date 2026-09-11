# Captura individual local: Sinergista y Etanol

Desde la raíz de EntoNet:

```sh
Rscript scripts/preview_f7_sets_local.R
```

Abrir http://127.0.0.1:3877. La vista utiliza la definición de captura individual de `shiny_app/app.R` y el módulo compartido `shiny_app/f7_sets_local.R`. Solo carga las definiciones necesarias para la interfaz. No inicia el servidor de producción, no lee credenciales y no llama a Supabase.

## Uso

1. Completar los datos del ensayo. En esta vista los códigos territoriales se ingresan manualmente.
2. Abrir **Resultados por botella**. La pestaña Sinergista contiene 8.1 y 9.1; Etanol contiene 8.2 y 9.2.
3. Capturar E1–E5 a los 60 minutos de preexposición. En la etapa posterior, capturar E1–E4/C1 a 0, 15, 30 y 45 minutos. Los horarios son independientes por set, botella y etapa.
4. Si se necesita conservar la lectura de 24 horas, activar la opción para ambos sets. Su obligatoriedad aún está pendiente de definición.
5. En **Comentarios y envío**, pulsar **Guardar borrador local**. Se permite un borrador incompleto, conservando las ausencias como null. Se rechazan conteos negativos/fraccionarios, pares incompletos y horas inválidas.
6. Descargar el último archivo o encontrarlo en `output/f7_capturas_locales`. Usar **Abrir un borrador local** para continuar una captura en la vista local.

Cada guardado crea un archivo nuevo y conserva las versiones anteriores. Los archivos están excluidos de Git por la regla existente `output/`. El JSON conserva encabezado, observaciones y lecturas con claves de set, etapa, botella y tiempo; es un formato de intercambio local, no una migración ni el contrato final de la API.

## Integración y límites

La captura de Sinergistas en `app.R` usa el nuevo módulo y un botón de borrador local. El manejador antiguo rechaza enviar este flujo por la API anterior, evitando perder el set Etanol. Diagnóstica e Intensidad conservan su ruta anterior. La vista de prueba está limitada a Sinergistas.

No se calcula una clasificación de resistencia a partir de la serie antigua oculta. La interpretación de ambos sets, el traslado individual de mosquitos, el intervalo adicional de 60 minutos y el uso obligatorio de 24 horas siguen pendientes de definición.

Esta entrega no actualiza migraciones, manifest de despliegue, carga masiva, edición remota ni impresión. No publicar estos cambios antes de coordinar el nuevo contrato de almacenamiento. Supabase permanece sin cambios.

## Verificación

`Rscript scripts/test_f7_sets_local.R` verifica independencia de sets, 50/60 lecturas, conservación de ceros y ausencias, validación de conteos/horas y guardado/lectura JSON. La vista se prueba adicionalmente en navegador.

La prueba existente `scripts/test_formularios_api.R` falla por falta de `f7_diagnostic_capture_analysis` en su entorno de prueba. Se reprodujo también con los archivos de HEAD previos a estos cambios.
