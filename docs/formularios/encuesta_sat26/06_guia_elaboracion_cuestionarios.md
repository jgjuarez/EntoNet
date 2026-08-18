# Guía de elaboración de cuestionarios web EntoNet

Esta guía documenta decisiones tomadas durante la construcción local de la encuesta SAT26 para reutilizarlas en futuros cuestionarios.

## Numeración visible de preguntas

- La numeración visible debe seguir la estructura del website, no necesariamente la del anexo original.
- Si una sección cambia de letra al migrarse al website, todas sus preguntas deben renumerarse con la nueva letra.
  - Ejemplo: Gobernanza era sección B en el anexo, pero en el website es Parte C, por lo que sus preguntas se muestran como C1, C2, C3, etc.
- Las preguntas tipo matriz o listas de variables no deben repetir el mismo número en cada fila.
- Cada fila que genera una variable/columna de captura debe tener un número visible único.
  - Ejemplo correcto:
    - F1c.1 Reducción de criaderos
    - F1c.2 Larvicidas
    - F1c.3 Rociado residual dirigido en interiores
  - Evitar:
    - F1c Reducción de criaderos
    - F1c Larvicidas
    - F1c Rociado residual dirigido en interiores
- Para matrices principales se puede usar una pregunta madre y filas numeradas:
  - C4. Elementos incluidos en los planes
  - C4.1 Control del vector *Aedes*
  - C4.2 Vigilancia del vector *Aedes*
  - C4.3 Control del vector *Anopheles*
- Si dentro de una pregunta hay subbloques, usar letra para el subbloque y número para la fila:
  - E2a.1, E2a.2, E2a.3 para filas del primer subbloque.
  - E2b.1, E2b.2, E2b.3 para filas del segundo subbloque.
  - E5a.1 para brechas por puesto; E5b.1 para número adicional requerido por puesto.
- Evitar mezclar niveles como `E2.2.1` cuando el subbloque puede expresarse más claramente como `E2b.1`.
- En esta fase, la numeración visible puede corregirse sin cambiar los IDs internos. Cuando el formulario esté validado, se debe hacer una limpieza final para alinear IDs de captura, nombres de columnas y numeración visible.

## IDs internos y variables de captura

- Durante el prototipo se pueden mantener IDs internos estables para no romper el guardado local.
- Antes de pasar a Supabase/producción, cada variable debe revisarse para que:
  - tenga nombre consistente con la sección y pregunta visible;
  - no repita número lógico;
  - sea fácil de mapear a una columna de base de datos;
  - preserve compatibilidad con respuestas tipo texto, selección única y selección múltiple.
- Las preguntas tipo matriz deben mapearse a una columna por fila/variable.

## Opciones de respuesta

- De aquí en adelante, las preguntas de selección deben incluir una opción “Desconozco” cuando aplique.
- Si el anexo usa “No sé”, “Desconocido” o “Desconozco su inclusión”, se normaliza visualmente como “Desconozco” salvo que se necesite preservar un matiz específico.
- Si “Otro” u “Otras” aparece como opción, debe agregarse un campo condicional para describirlo.
- Si una pregunta selecciona qué datos conoce la persona, los campos de detalle deben aparecer solo cuando la opción correspondiente fue seleccionada.
  - Ejemplo: si C1b.1 marca “Nombre”, mostrar C1b.1.1.
  - Si C1b.1 marca “Último año de actualización”, mostrar C1b.1.2.
  - Si C1b.1 marca “Documento del plan”, mostrar C1b.1.3.
- No mostrar campos de detalle de una fila si la fila madre no fue seleccionada.

## Cursiva para géneros y nombres biológicos

- Los géneros biológicos deben mostrarse en cursiva:
  - *Aedes*
  - *Anopheles*
  - *Wolbachia*
- En Shiny, los labels que contienen HTML deben pasar por `HTML(...)`.
- Para opciones de `checkboxGroupInput()` o `radioButtons()` que requieran HTML, usar `choiceNames` y `choiceValues` cuando sea necesario para evitar que aparezca literalmente `<em>...</em>`.

## Navegación y experiencia de usuario

- Al presionar “Continuar” o “Volver”, la nueva parte debe abrir al inicio de la burbuja/formulario.
- Cada sección debe mostrar logos discretos en la parte superior.
- El texto de preguntas y campos debe mantenerse legible, con contraste negro y ancho completo dentro de la burbuja.
- Si una lógica condicional oculta opciones, debe existir un botón `reset` para limpiar la selección y mostrar nuevamente todas las rutas posibles.

## Guardado local y continuidad

- Cada encuesta debe tener un código único visible.
- El código permite retomar un borrador.
- El prototipo usa guardado local del navegador; la versión productiva debe migrar a Supabase.
- El guardado debe incluir la sección activa para llevar a la persona al punto pendiente.
