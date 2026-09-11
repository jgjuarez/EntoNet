"""Generate the proposed wide CSV and its field mapping; no database access."""
import csv
import re
from pathlib import Path

root = Path(__file__).resolve().parents[1]
out = root / "output" / "f7_machote_csv"
out.mkdir(parents=True, exist_ok=True)
app = (root / "shiny_app/app.R").read_text()
block = re.search(r"formulario_7_header_columns <- c\((.*?)\n\)", app, re.S).group(1)
headers = [s for s in re.findall(r'"([^"]+)"', block) if s != "resultado_diagnostico"]
fields = []

def add(name, kind, section, target, rule, change, set_="", stage="", bottle="", time=""):
    fields.append(dict(columna_csv=name, tipo=kind, seccion=section, set=set_, etapa=stage,
                       botella=bottle, tiempo_minutos=time, destino_propuesto=target,
                       regla=rule, cambio=change))

add("version_estructura", "texto", "Formato", "Contrato CSV", "f7_sets_csv_v1; formato propuesto, sin importador todavía", "Nuevo")
for h in headers:
    kind = "texto"
    rule = "Dato general conservado de la captura local"
    if h.startswith("fecha_"):
        kind, rule = "fecha", "AAAA-MM-DD"
    elif h.startswith("hora_"):
        kind, rule = "hora", "HH:MM, hora local"
    elif h in ("sinergista_def", "sinergista_pbo", "sinergista_dm", "edad_indefinida", "generacion_filial_indefinida", "bioensayo_diagnostica_1x"):
        kind, rule = "booleano", "TRUE o FALSE; flags del sinergista coherentes con sinergista_tipo"
    elif h.startswith("numero_usos_") or h == "edad_dias":
        kind, rule = "entero", "Mayor o igual a cero; no duplicar usos entre sets ni etapas"
    elif h.startswith(("temperatura_", "humedad_", "dosis_")) and h != "dosis_intensidad":
        kind, rule = "decimal", "Separador decimal punto; humedad 0–100, dosis no negativa"
    if h in ("codigo_departamento", "codigo_municipio", "codigo_bioensayo"):
        rule = "Importar como texto para conservar ceros iniciales y códigos"
    if h in ("bioensayo_intensidad", "dosis_intensidad"):
        rule = "Vacío en Sinergistas; conservado por compatibilidad con encabezado actual"
    if h == "codigo_revision_24h":
        rule = "Completar si se activa incluir_24h; obligatoriedad definitiva pendiente"
    add(h, kind, "Datos generales", "formulario_7_bioensayo_intake." + h, rule, "Conservado")
add("incluir_24h", "booleano", "24 horas", "Control del importador futuro", "FALSE por defecto; TRUE activa 10 lecturas adicionales", "Nuevo")

for set_, number in (("sinergista", "1"), ("etanol", "2")):
    for stage, section, bottles, times in (
        ("pretratamiento", "8." + number, [f"e{i}" for i in range(1, 6)], [60]),
        ("bioensayo", "9." + number, ["e1", "e2", "e3", "e4", "c1"], [0, 15, 30, 45]),
        ("kdr_24h", "24 horas opcional", ["e1", "e2", "e3", "e4", "c1"], [1440]),
    ):
        for bottle in bottles:
            prefix = f"{set_}_{stage}_{bottle}"
            add(prefix + "_hora_inicio", "hora", section,
                "formulario_7_bioensayo_botella_etapa_intake.inicio_etapa",
                "HH:MM; en 24 h conserva el campo inicio del prototipo, semántica final pendiente; combinar con fecha de etapa confirmada, no inferir fecha",
                "Nuevo: distingue set y etapa", set_, stage, bottle)
            for minute in times:
                for metric in ("vivos", "incapacitados"):
                    add(f"{prefix}_{minute}min_{metric}", "entero", section,
                        "formulario_7_bioensayo_resultado_intake." + metric,
                        "Entero no negativo; vivos/incapacitados en pareja; vacío es ausente, 0 es observado; 24 h solo si incluir_24h=TRUE",
                        "Reemplaza lectura sin set" if set_ == "sinergista" else "Nuevo: lectura de Etanol",
                        set_, stage, bottle, minute)
        if stage != "kdr_24h":
            add(f"{set_}_{stage}_observaciones", "texto", section,
                "formulario_7_bioensayo_set_intake.observaciones_" + stage,
                "Observación propia de este set y etapa", "Nuevo", set_, stage)

for h in ("comentario", "comentario_nombre"):
    add(h, "texto", "Comentario común", "formulario_7_bioensayo_comentario_intake." + ("nombre" if h.endswith("nombre") else "comentario"),
        "Comentario del ensayo completo; no duplicar entre sets", "Conservado")

names = [f["columna_csv"] for f in fields]
assert len(names) == len(set(names))
defaults = dict(version_estructura="f7_sets_csv_v1", formulario_codigo="F7",
    formulario_nombre="Registro de datos del bioensayo de la botella CDC",
    bioensayo_diagnostica_1x="FALSE", incluir_24h="FALSE")
with (out / "machote_formulario_7_sinergista_etanol.csv").open("w", encoding="utf-8-sig", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=names)
    writer.writeheader()
    writer.writerow(defaults)
with (out / "diccionario_formulario_7_sinergista_etanol.csv").open("w", encoding="utf-8-sig", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=fields[0].keys())
    writer.writeheader()
    writer.writerows(fields)
counts = [f for f in fields if f["tiempo_minutos"] != ""]
assert len(counts) == 120
assert sum(f["etapa"] != "kdr_24h" for f in counts) == 100
for name in ("machote_formulario_7_sinergista_etanol.csv", "diccionario_formulario_7_sinergista_etanol.csv"):
    with (out / name).open(encoding="utf-8-sig", newline="") as f:
        rows = list(csv.reader(f))
        assert all(len(row) == len(rows[0]) for row in rows)
print(f"Verified: {len(names)} columns; 100 base counts + 20 optional 24h counts; unique headers and aligned rows.")
