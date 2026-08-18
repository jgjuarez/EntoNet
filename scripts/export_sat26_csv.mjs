import fs from "node:fs";
import path from "node:path";

const repoRoot = path.resolve(import.meta.dirname, "..");
const code = process.argv[2];
const outputPath = process.argv[3];
if (!code || !outputPath) {
  throw new Error("Uso: node scripts/export_sat26_csv.mjs CODIGO_UNICO ARCHIVO_SALIDA.csv");
}

function readEnv(file) {
  return Object.fromEntries(
    fs.readFileSync(file, "utf8")
      .split(/\r?\n/)
      .filter((line) => line && !line.startsWith("#") && line.includes("="))
      .map((line) => {
        const index = line.indexOf("=");
        return [line.slice(0, index), line.slice(index + 1).replace(/^['"]|['"]$/g, "")];
      }),
  );
}

function parseCsvHeader(text) {
  const firstLine = text.split(/\r?\n/, 1)[0];
  const fields = [];
  let field = "", quoted = false;
  for (let i = 0; i < firstLine.length; i += 1) {
    const char = firstLine[i];
    if (quoted && char === '"' && firstLine[i + 1] === '"') { field += '"'; i += 1; }
    else if (char === '"') quoted = !quoted;
    else if (char === ',' && !quoted) { fields.push(field); field = ""; }
    else field += char;
  }
  fields.push(field);
  return fields;
}

const csvCell = (value) => {
  if (value === null || value === undefined) return '""';
  return `"${String(value).replaceAll('"', '""')}"`;
};

const env = readEnv(path.resolve(repoRoot, "..", ".env.local"));
if (!env.SUPABASE_SERVICE_ROLE_KEY) {
  throw new Error("Falta SUPABASE_SERVICE_ROLE_KEY en ../.env.local.");
}
const configuredUrl = env.SUPABASE_URL ?? "";
const supabaseUrl = !configuredUrl || configuredUrl.toLowerCase().includes("project_ref")
  ? "https://shqxseqrtodcwdksokuv.supabase.co"
  : configuredUrl;

const dictionary = fs.readFileSync(path.join(repoRoot, "docs", "formularios", "encuesta_sat26", "encuesta_sat26_captura_template.csv"), "utf8");
const columns = parseCsvHeader(dictionary);
const url = new URL(`${supabaseUrl.replace(/\/$/, "")}/rest/v1/encuesta_sat26_export`);
url.searchParams.set("codigo_unico", `eq.${code}`);
url.searchParams.set("select", "*");
const response = await fetch(url, {
  headers: {
    apikey: env.SUPABASE_SERVICE_ROLE_KEY,
    Authorization: `Bearer ${env.SUPABASE_SERVICE_ROLE_KEY}`,
    Accept: "application/json",
  },
});
if (!response.ok) {
  throw new Error(`Supabase respondió HTTP ${response.status}: ${await response.text()}`);
}
const rows = await response.json();
if (rows.length !== 1) throw new Error(`Se esperaba un registro para ${code}; se recibieron ${rows.length}.`);
const missing = columns.filter((column) => !(column in rows[0]));
if (missing.length) throw new Error(`La vista no devolvió ${missing.length} columnas esperadas.`);

const csv = `${columns.map(csvCell).join(",")}\n${columns.map((column) => csvCell(rows[0][column])).join(",")}\n`;
fs.mkdirSync(path.dirname(path.resolve(outputPath)), { recursive: true });
fs.writeFileSync(path.resolve(outputPath), csv);
console.log(JSON.stringify({ codigo_unico: code, columnas: columns.length, filas: rows.length, archivo: path.resolve(outputPath) }, null, 2));
