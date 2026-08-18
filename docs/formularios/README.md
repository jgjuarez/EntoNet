# Formularios EntoNet

Este directorio guarda la memoria de trabajo de cada formulario para poder
replicar, auditar y extender implementaciones futuras sin empezar desde cero.

La idea es que cada formulario tenga su propia carpeta con la misma estructura:

```text
formularios/
  README.md
  plantilla_formulario.md
  formulario_1/
    00_resumen.md
    01_estructura.md
    02_consultas.sql
    03_soluciones.md
    04_estilos.md
    05_cambios.md
    assets/
  formulario_5/
    ...
  formulario_7/
    ...
```

## Que guardar aqui

- `00_resumen.md`: objetivo del formulario, alcance y criterio de uso.
- `01_estructura.md`: campos, secciones, validaciones y relaciones.
- `02_consultas.sql`: consultas utiles, vistas, checks y transformaciones.
- `03_soluciones.md`: decisiones tecnicas y respuestas a problemas resueltos.
- `04_estilos.md`: reglas visuales, componentes, colores, espaciados y patrones.
- `05_cambios.md`: historial breve de ajustes y razones.
- `README.md` dentro de cada formulario: indice rapido y archivos relacionados.
- `06_mapeo_ui.md` dentro de cada formulario: tabla de controles y funcion.
- `patrones_comunes/`: decisiones reutilizables entre formularios.
- `assets/`: capturas, diagramas o ejemplos visuales.

## Regla practica

- Si es una decision estructural, va en `01_estructura.md`.
- Si es una query reutilizable, va en `02_consultas.sql`.
- Si es una solucion o workaround probado, va en `03_soluciones.md`.
- Si afecta apariencia o comportamiento visual, va en `04_estilos.md`.
- Si cambia algo importante, se agrega una nota en `05_cambios.md`.

## Recomendacion de uso

1. Crear una carpeta por formulario.
2. Copiar la plantilla base antes de empezar un nuevo formulario.
3. Mantener el SQL y la documentacion cerca del formulario correspondiente.
4. Usar nombres consistentes para que sea facil comparar versiones entre
   proyectos distintos.

Este directorio no sustituye `supabase/` ni `base_datos_entonet/`; los
complementa como capa de documentacion operativa y reutilizable.
