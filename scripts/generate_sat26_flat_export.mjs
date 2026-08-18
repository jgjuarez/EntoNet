import fs from "node:fs";
import path from "node:path";

const repoRoot = path.resolve(import.meta.dirname, "..");
const docsDir = path.join(repoRoot, "docs", "formularios", "encuesta_sat26");
const dictionaryPath = path.join(docsDir, "encuesta_sat26_columnas_captura.csv");
const optionsPath = path.join(docsDir, "encuesta_sat26_opciones_multiples.csv");
const templatePath = path.join(docsDir, "encuesta_sat26_captura_template.csv");
const migrationPath = path.join(repoRoot, "supabase", "036_encuesta_sat26_export_binario.sql");

const option = (key, label, value = key) => ({ key, label, value });
const simple = (...keys) => keys.map((key) => option(key, key));
const referenceOptions = [
  option("nombre", "Nombre"),
  option("hipervinculo", "Hipervínculo"),
  option("documento", "Documento"),
  option("desconozco", "Desconozco"),
];

// `value` reproduce exactamente lo guardado por Shiny en el arreglo JSON.
// `key` es el sufijo estable y legible usado en la columna analítica.
const multipleOptions = {
  arbovirus_2025: [option("zika", "Zika"), option("chikungunya", "Chikungunya"), option("ninguno", "Ninguno"), option("desconozco", "Desconozco")],
  plan_caracteristicas: [], // Campo heredado sin control visible en la versión vigente.
  plan_aedes_caracteristicas: [option("nombre", "Nombre"), option("anio_actualizacion", "Último año de actualización"), option("documento", "Documento del plan"), option("desconozco", "Desconozco")],
  plan_anopheles_caracteristicas: [option("nombre", "Nombre"), option("anio_actualizacion", "Último año de actualización"), option("documento", "Documento del plan"), option("desconozco", "Desconozco")],
  plan_integrado_caracteristicas: [option("nombre", "Nombre"), option("anio_actualizacion", "Último año de actualización"), option("documento", "Documento del plan"), option("desconozco", "Desconozco")],
  factores_prioridad: [
    option("casos", "Casos reportados / carga de enfermedad"), option("autoridades", "Opinión o presión de autoridades superiores"),
    option("cooperacion", "Requisitos de cooperación internacional"), option("costos", "Costos operativos de la vigilancia y control"),
    option("comunidad", "Interés de la comunidad"), option("experiencias", "Experiencias pasadas"), option("otros", "Otros"), option("desconozco", "Desconozco"),
  ],
  motivadores_institucion: [
    option("recursos", "Disponibilidad de recursos sostenibles"), option("apoyo_politico", "Presión o apoyo político de alto nivel"),
    option("exitos", "Éxitos visibles en territorio"), option("indicadores", "Mejora de los indicadores de salud"),
    option("participacion", "Mayor participación comunitaria"), option("alianzas", "Alianzas con cooperación o sector privado"),
    option("no_sabria", "No sabría decir qué puede motivar a mi institución"), option("otras", "Otras"), option("desconozco", "Desconozco"),
  ],
  rrhh_capacitacion_sistemas: [
    option("puesto_trabajo", "Capacitación en el puesto de trabajo"), option("cursos_nacionales", "Cursos nacionales"),
    option("cursos_regionales_internacionales", "Cursos regionales/internacionales"), option("posgrado", "Formación de posgrado"),
    option("formadores", "Programa de formación de formadores"), option("sur_sur", "Capacitación brindada por países vecinos (cooperación Sur-Sur)"),
    option("otro", "Otro"), option("desconozco", "Desconozco"),
  ],
  control_aedes_actividades: [
    option("criaderos", "Reducción de criaderos (ej. limpieza comunitaria)", "Reducción de criaderos (ej. limpieza comunitaria)"),
    option("larvicidas", "Larvicidas (ej. temephos, IGRs, Bti)", "Larvicidas (ej. temephos, IGRs, Bti)"),
    option("irs_aedes", "Rociado residual dirigido en interiores - Aedes (IRS-Aedes)", "Rociado residual dirigido en interiores - <em>Aedes</em> (IRS-<em>Aedes</em>)"),
    option("ors_aedes", "Rociado residual en exteriores - Aedes (ORS-Aedes)", "Rociado residual en exteriores - <em>Aedes</em> (ORS-<em>Aedes</em>)"),
    option("nebulizacion_interiores", "Nebulización en interiores (insecticida no residual)", "Nebulización en interiores (insecticida no residual)"),
    option("nebulizacion_exteriores", "Nebulización en exteriores (insecticida no residual)", "Nebulización en exteriores (insecticida no residual)"),
    option("wolbachia", "Wolbachia", "<em>Wolbachia</em>"),
    option("mosquiteros_febriles", "Distribución de mosquiteros a pacientes febriles", "Distribución de mosquiteros a pacientes febriles"),
    option("repelentes_febriles", "Distribución de repelentes a pacientes febriles", "Distribución de repelentes a pacientes febriles"),
    option("desconozco", "Desconozco"),
  ],
  control_aedes_otras: [option("casas_prueba", "Casas a prueba de mosquitos"), option("control_biologico", "Control biológico larval (peces o copépodos)"), option("redes_hamacas", "Redes para hamacas"), option("insecto_esteril", "Técnica de insecto estéril"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  control_anopheles_actividades: [
    option("itns", "Mosquiteros tratados con insecticida (ITNs)", "Mosquiteros tratados con insecticida (ITNs)"),
    option("llins", "Mosquiteros con insecticida de larga duración (LLINs)", "Mosquiteros con insecticida de larga duración (LLINs)"),
    option("irs", "Rociado residual en interiores (IRS)", "Rociado residual en interiores (IRS)"),
    option("larvicidas", "Aplicación de larvicidas (ej. temephos, IGRs, Bti)", "Aplicación de larvicidas (ej. temephos, IGRs, Bti)"),
    option("criaderos", "Reducción de criaderos (modificación física, limpiezas comunitarias, etc.)", "Reducción de criaderos (modificación física, limpiezas comunitarias, etc.)"),
    option("desconozco", "Desconozco"),
  ],
  control_anopheles_otras: [option("rociado_exteriores", "Rociado en exteriores"), option("casas_prueba", "Casas a prueba de mosquitos"), option("rociado_barrera", "Rociado de barrera"), option("control_biologico", "Control biológico larval (peces o copépodos)"), option("redes_hamacas", "Redes para hamacas"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  f5b_1_control_irs_metodos: [option("mortalidad_adultos", "Tasa de mortalidad de mosquitos adultos"), option("densidad", "Densidad de mosquitos"), option("indice_inmaduros", "Índice de mosquitos inmaduros"), option("contacto_superficie", "Pruebas de contacto en superficie"), option("pared_rociada", "Análisis de la alteración de la pared rociada"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  f5b_2_control_larvicidas_metodos: [option("habitats_tratados", "Evaluación de hábitats tratados con insecticida"), option("inspecciones", "Inspecciones aleatorias de hábitats larvarios tratados"), option("densidad_adultos", "Densidad de mosquitos adultos"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  f5b_3_control_irs_aedes_metodos: [option("bioensayos_pared", "Bioensayos de pared"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  f5b_4_control_nebulizacion_metodos: [option("protocolos", "Evaluación de protocolos de nebulización"), option("residuos", "Evaluación de residuos"), option("adultos", "Número de mosquitos adultos"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  vigilancia_aedes_trampas: [
    option("hlc_red", "Recolección con cebo humano (HLC o red de barrido)", "Recolección con cebo humano (HLC o red de barrido)"), option("luz_cdc", "Trampa de luz CDC", "Trampa de luz CDC"),
    option("bg_sentinel", "Trampa BG Sentinel", "Trampa BG Sentinel"), option("bg_pro", "BG Pro", "BG Pro"), option("ventilador", "Otras trampas con ventilador", "Otras trampas con ventilador"),
    option("gravidas", "Trampas grávidas", "Trampas grávidas"), option("aspiracion_interiores", "Aspiración en interiores (aspiración manual, Prokopac)", "Aspiración en interiores (aspiración manual, Prokopac)"),
    option("aspiracion_exteriores", "Aspiración en exteriores por aspiración", "Aspiración en exteriores por aspiración"), option("descanso_exterior", "Otro método de descanso exterior (trampa de pozo, barrera de tela)", "Otro método de descanso exterior (trampa de pozo, barrera de tela)"),
    option("cebo_humano", "Trampa con cebo humano", "Trampa con cebo humano"), option("cebo_animal", "Trampa con cebo animal", "Trampa con cebo animal"), option("otro", "Otro", "Otro"), option("desconozco", "Desconozco", "Desconozco"),
  ],
  vigilancia_anopheles_trampas: [
    option("aterrizaje_humano", "Captura por aterrizaje humano", "Captura por aterrizaje humano"), option("cebo_humano", "Trampa con cebo humano", "Trampa con cebo humano"), option("cebo_animal", "Trampa con cebo animal", "Trampa con cebo animal"),
    option("ventilador_cdc", "Trampa de ventilador CDC", "Trampa de ventilador CDC"), option("bg_sentinel", "Trampa BG Sentinel", "Trampa BG Sentinel"), option("gravidas", "Trampas grávidas", "Trampas grávidas"),
    option("aspiracion_interiores", "Aspiración en interiores (aspiración manual, Prokopac)", "Aspiración en interiores (aspiración manual, Prokopac)"), option("aspiracion_exteriores", "Aspiración en exteriores por aspiración", "Aspiración en exteriores por aspiración"),
    option("descanso_exterior", "Otro método de descanso exterior (trampa de pozo, barrera de tela)", "Otro método de descanso exterior (trampa de pozo, barrera de tela)"), option("salida_ventana", "Trampa de salida por ventana", "Trampa de salida por ventana"),
    option("caja_pasiva", "Trampas de caja pasiva", "Trampas de caja pasiva"), option("otro", "Otro", "Otro"), option("desconozco", "Desconozco", "Desconozco"),
  ],
  vigilancia_decisiones: [
    option("estratificar", "Cómo estratificar el control vectorial", "Cómo estratificar el control vectorial"), option("estrategias", "Dónde implementar diferentes estrategias de control", "Dónde implementar diferentes estrategias de control"),
    option("larvicidas", "Selección de larvicidas", "Selección de larvicidas"), option("insecticidas_irs_aedes", "Selección de insecticidas para IRS-Aedes", "Selección de insecticidas para IRS-<em>Aedes</em>"),
    option("hogares", "Dónde aplicar insecticidas en hogares y alrededores", "Dónde aplicar insecticidas en hogares y alrededores"), option("recipientes", "Recipientes larvarios clave para reducción de criaderos", "Recipientes larvarios clave para reducción de criaderos"),
    option("control_larval_anopheles", "Dónde implementar control larval (Anopheles)", "Dónde implementar control larval (<em>Anopheles</em>)"), option("llin_itn", "Elección de LLIN/ITN a comprar", "Elección de LLIN/ITN a comprar"),
    option("habitats_anopheles", "Hábitats larvarios clave para manejo de criaderos (Anopheles)", "Hábitats larvarios clave para manejo de criaderos (<em>Anopheles</em>)"), option("sitios_vigilancia", "Dónde establecer sitios de vigilancia vectorial", "Dónde establecer sitios de vigilancia vectorial"),
    option("mensajes", "Cómo optimizar mensajes de participación comunitaria", "Cómo optimizar mensajes de participación comunitaria"), option("otro", "Otro", "Otro"), option("desconozco", "Desconozco", "Desconozco"),
  ],
  infra_laboratorio_capacidades: [option("pcr", "PCR"), option("elisa", "ELISA"), option("ento_campo", "Laboratorio entomológico de campo"), option("semi_campo", "Semi-campo"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  info_vigilancia_herramientas: [option("recoleccion", "Recolección de datos"), option("almacenamiento", "Almacenamiento de datos"), option("presentacion", "Presentación de datos"), option("desconozco", "Desconozco")],
  info_control_herramientas: [option("recoleccion", "Recolección de datos"), option("almacenamiento", "Almacenamiento de datos"), option("presentacion", "Presentación de datos"), option("desconozco", "Desconozco")],
  info_vigilancia_recoleccion: [option("papel", "Formularios en papel"), option("smartphone_tablet", "Smartphone o tablet"), option("fotografias", "Fotografías"), option("codigos_barras", "Escáneres de código de barras"), option("gps", "GPS"), option("planificacion_electronica", "Planificación para captura electrónica"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  info_control_recoleccion: [option("papel", "Formularios en papel"), option("smartphone_tablet", "Smartphone o tablet"), option("fotografias", "Fotografías"), option("codigos_barras", "Escáneres de código de barras"), option("gps", "GPS"), option("planificacion_electronica", "Planificación para captura electrónica"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  info_vigilancia_almacenamiento: [option("papel", "Papel"), option("excel", "Excel"), option("dhis2", "DHIS2"), option("otra_bd_linea", "Otra base de datos en línea"), option("access", "Access"), option("desarrollando_linea", "Desarrollando almacenamiento en línea"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  info_control_almacenamiento: [option("papel", "Papel"), option("excel", "Excel"), option("dhis2", "DHIS2"), option("otra_bd_linea", "Otra base de datos en línea"), option("access", "Access"), option("desarrollando_linea", "Desarrollando almacenamiento en línea"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  info_vigilancia_reporte: [option("excel_manual", "Manualmente en Excel"), option("paneles_linea", "Paneles en línea"), option("mapas_electronicos", "Mapas electrónicos"), option("informes_automaticos", "Informes generados automáticamente"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  info_control_reporte: [option("excel_manual", "Manualmente en Excel"), option("paneles_linea", "Paneles en línea"), option("mapas_electronicos", "Mapas electrónicos"), option("informes_automaticos", "Informes generados automáticamente"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  comunidad_actividades: [option("vigilancia_vectorial", "Vigilancia vectorial"), option("control_vectorial", "Control vectorial"), option("comunicacion_riesgos_educacion", "Comunicación de riesgos / educación"), option("reporte_enfermedades_brotes", "Reporte de enfermedades o brotes"), option("investigacion", "Investigación"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  comunidad_momento: [option("despues_brote", "Después de la declaración de un brote"), option("regularmente_anio", "Regularmente durante el año"), option("antes_lluvias", "Antes de la temporada de lluvias"), option("otro", "Otro"), option("desconozco", "Desconozco")],
  k3b_1_1_ref_tipo_informes: referenceOptions,
  k3b_2_1_ref_tipo_articulos: referenceOptions,
  k3b_3_1_ref_tipo_congresos: referenceOptions,
  k3b_4_1_ref_tipo_multilaterales: referenceOptions,
  k3b_5_1_ref_tipo_tesis: referenceOptions,
  k3b_6_1_ref_tipo_financiamiento: referenceOptions,
  k3b_7_1_ref_tipo_prensa: referenceOptions,
};

function parseCsv(text) {
  const rows = [];
  let row = [], field = "", quoted = false;
  for (let i = 0; i < text.length; i += 1) {
    const char = text[i];
    if (quoted) {
      if (char === '"' && text[i + 1] === '"') { field += '"'; i += 1; }
      else if (char === '"') quoted = false;
      else field += char;
    } else if (char === '"') quoted = true;
    else if (char === ',') { row.push(field); field = ""; }
    else if (char === '\n') { row.push(field.replace(/\r$/, "")); rows.push(row); row = []; field = ""; }
    else field += char;
  }
  if (field.length || row.length) { row.push(field); rows.push(row); }
  const headers = rows.shift();
  return rows.filter((r) => r.some((v) => v !== "")).map((r) => Object.fromEntries(headers.map((h, i) => [h, r[i] ?? ""])));
}

const csvCell = (value) => `"${String(value ?? "").replaceAll('"', '""')}"`;
const toCsv = (headers, rows) => [headers, ...rows.map((row) => headers.map((header) => row[header] ?? ""))]
  .map((row) => row.map(csvCell).join(",")).join("\n") + "\n";
const sqlLiteral = (value) => `'${String(value).replaceAll("'", "''")}'`;
const sqlIdentifier = (value) => `"${String(value).replaceAll('"', '""')}"`;
const pgPath = (jsonPath) => jsonPath.replace(/^payload\./, "").split(".");
const sqlPath = (jsonPath) => `'{${pgPath(jsonPath).join(",")}}'`;
const columnKeyOverrides = {
  cursos_regionales_internacionales: "cursos_reg_int",
};

const sourceRows = parseCsv(fs.readFileSync(dictionaryPath, "utf8"));
let dictionaryRows = [];
let optionRows = [];
const expectedMultiple = sourceRows.filter((row) => row.data_type === "multi_select_semicolon").map((row) => row.csv_column);

if (expectedMultiple.length) {
  const missingMappings = expectedMultiple.filter((column) => !(column in multipleOptions));
  const extraMappings = Object.keys(multipleOptions).filter((column) => !expectedMultiple.includes(column));
  if (missingMappings.length || extraMappings.length) {
    throw new Error(`Mapeo inconsistente. Faltan: ${missingMappings.join(", ") || "ninguna"}. Sobran: ${extraMappings.join(", ") || "ninguna"}.`);
  }

  for (const row of sourceRows) {
    if (row.data_type !== "multi_select_semicolon") {
      dictionaryRows.push({ ...row, option_value: "", option_label: "" });
      continue;
    }
    for (const item of multipleOptions[row.csv_column]) {
      const columnKey = columnKeyOverrides[item.key] ?? item.key;
      const csvColumn = `${row.csv_column}_sel_${columnKey}`;
      if (csvColumn.length > 63) throw new Error(`Columna PostgreSQL mayor de 63 caracteres: ${csvColumn}`);
      dictionaryRows.push({
        csv_column: csvColumn,
        source_variable: row.source_variable,
        data_type: "binary_1_0",
        section: row.section,
        json_path: row.json_path,
        option_value: item.value,
        option_label: item.label,
      });
      optionRows.push({
        source_csv_column: row.csv_column,
        source_variable: row.source_variable,
        json_path: row.json_path,
        option_value: item.value,
        option_label: item.label,
        binary_csv_column: csvColumn,
        selected_value: "1",
        not_selected_value: "0",
      });
    }
  }
} else {
  dictionaryRows = sourceRows;
  optionRows = parseCsv(fs.readFileSync(optionsPath, "utf8"));
}

const duplicateColumns = dictionaryRows.map((row) => row.csv_column).filter((column, index, all) => all.indexOf(column) !== index);
if (duplicateColumns.length) throw new Error(`Columnas duplicadas: ${[...new Set(duplicateColumns)].join(", ")}`);

const dictionaryHeaders = ["csv_column", "source_variable", "data_type", "section", "json_path", "option_value", "option_label"];
const optionHeaders = ["source_csv_column", "source_variable", "json_path", "option_value", "option_label", "binary_csv_column", "selected_value", "not_selected_value"];
fs.writeFileSync(dictionaryPath, toCsv(dictionaryHeaders, dictionaryRows));
fs.writeFileSync(optionsPath, toCsv(optionHeaders, optionRows));
fs.writeFileSync(templatePath, dictionaryRows.map((row) => csvCell(row.csv_column)).join(",") + "\n");

const metadataSql = {
  codigo_unico: "codigo_unico",
  submitted_at: "submitted_at",
  formulario_version: "formulario_version",
  review_status: "review_status",
};
const expressions = dictionaryRows.map((row) => {
  if (metadataSql[row.json_path]) return `  ${metadataSql[row.json_path]} as ${sqlIdentifier(row.csv_column)}`;
  if (row.data_type === "binary_1_0") {
    return `  case\n    when jsonb_typeof(payload #> ${sqlPath(row.json_path)}) = 'array'\n      then case when (payload #> ${sqlPath(row.json_path)}) ? ${sqlLiteral(row.option_value)} then 1 else 0 end\n    else null\n  end as ${sqlIdentifier(row.csv_column)}`;
  }
  return `  payload #>> ${sqlPath(row.json_path)} as ${sqlIdentifier(row.csv_column)}`;
});

const migration = `-- Vista analítica plana de SAT26.
-- El JSONB original permanece intacto; las opciones múltiples se presentan como columnas 1/0.
drop view if exists public.encuesta_sat26_export;

create view public.encuesta_sat26_export
with (security_invoker = true)
as
select
${expressions.join(",\n")}
from public.encuesta_sat26_intake;

comment on view public.encuesta_sat26_export is
  'Exportación analítica SAT26: una fila por codigo_unico y una columna binaria 1/0 por opción de respuesta múltiple.';

revoke all on public.encuesta_sat26_export from public, anon, authenticated;
grant select on public.encuesta_sat26_export to service_role;
`;
fs.writeFileSync(migrationPath, migration);

console.log(JSON.stringify({
  source_columns: sourceRows.length,
  source_multiple_columns: expectedMultiple.length || new Set(optionRows.map((row) => row.source_csv_column)).size,
  binary_columns: optionRows.length,
  final_columns: dictionaryRows.length,
  retired_without_options: Object.entries(multipleOptions).filter(([, options]) => options.length === 0).map(([column]) => column),
}, null, 2));
