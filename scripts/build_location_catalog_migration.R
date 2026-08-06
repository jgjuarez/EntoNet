source("shiny_app/app.R", local = TRUE)

sql_quote <- function(value) {
  paste0("'", gsub("'", "''", as.character(value), fixed = TRUE), "'")
}

country_code <- function(country) {
  ifelse(country == "Guatemala", "GT", ifelse(country == "El Salvador", "SV", NA_character_))
}

dept <- unique(ubicacion_departamento_catalogo)
dept$codigo_pais <- country_code(dept$pais)
dept <- dept[c("pais", "codigo_pais", "departamento_codigo", "departamento")]

muni <- unique(ubicacion_municipio_catalogo)
muni$codigo_pais <- country_code(muni$pais)
muni <- muni[c("pais", "codigo_pais", "departamento_codigo", "municipio_codigo", "municipio")]

row_values <- function(data) {
  apply(data, 1, function(row) {
    paste0("  (", paste(sql_quote(row), collapse = ", "), ")")
  })
}

sql <- c(
  "begin;",
  "",
  "create table if not exists public.catalogo_ubicacion_departamento (",
  "  pais text not null,",
  "  codigo_pais text not null,",
  "  codigo_departamento text not null,",
  "  departamento text not null,",
  "  actualizado_en timestamptz not null default now(),",
  "  primary key (pais, codigo_departamento),",
  "  unique (codigo_pais, codigo_departamento)",
  ");",
  "",
  "create table if not exists public.catalogo_ubicacion_municipio (",
  "  pais text not null,",
  "  codigo_pais text not null,",
  "  codigo_departamento text not null,",
  "  codigo_municipio text not null,",
  "  municipio text not null,",
  "  actualizado_en timestamptz not null default now(),",
  "  primary key (pais, codigo_municipio),",
  "  unique (codigo_pais, codigo_municipio),",
  "  foreign key (pais, codigo_departamento)",
  "    references public.catalogo_ubicacion_departamento (pais, codigo_departamento)",
  "    on update cascade on delete restrict",
  ");",
  "",
  "delete from public.catalogo_ubicacion_municipio where pais in ('Guatemala', 'El Salvador');",
  "delete from public.catalogo_ubicacion_departamento where pais in ('Guatemala', 'El Salvador');",
  "",
  "insert into public.catalogo_ubicacion_departamento (pais, codigo_pais, codigo_departamento, departamento)",
  "values",
  paste(row_values(dept), collapse = ",\n"),
  "on conflict (pais, codigo_departamento) do update set",
  "  codigo_pais = excluded.codigo_pais,",
  "  departamento = excluded.departamento,",
  "  actualizado_en = now();",
  "",
  "insert into public.catalogo_ubicacion_municipio (pais, codigo_pais, codigo_departamento, codigo_municipio, municipio)",
  "values",
  paste(row_values(muni), collapse = ",\n"),
  "on conflict (pais, codigo_municipio) do update set",
  "  codigo_pais = excluded.codigo_pais,",
  "  codigo_departamento = excluded.codigo_departamento,",
  "  municipio = excluded.municipio,",
  "  actualizado_en = now();",
  "",
  "create index if not exists catalogo_ubicacion_municipio_departamento_idx",
  "  on public.catalogo_ubicacion_municipio (pais, codigo_departamento, municipio);",
  "",
  "alter table public.catalogo_ubicacion_departamento enable row level security;",
  "alter table public.catalogo_ubicacion_municipio enable row level security;",
  "",
  "drop policy if exists catalogo_ubicacion_departamento_deny_anon on public.catalogo_ubicacion_departamento;",
  "create policy catalogo_ubicacion_departamento_deny_anon",
  "  on public.catalogo_ubicacion_departamento for all to anon using (false) with check (false);",
  "drop policy if exists catalogo_ubicacion_departamento_deny_authenticated on public.catalogo_ubicacion_departamento;",
  "create policy catalogo_ubicacion_departamento_deny_authenticated",
  "  on public.catalogo_ubicacion_departamento for all to authenticated using (false) with check (false);",
  "",
  "drop policy if exists catalogo_ubicacion_municipio_deny_anon on public.catalogo_ubicacion_municipio;",
  "create policy catalogo_ubicacion_municipio_deny_anon",
  "  on public.catalogo_ubicacion_municipio for all to anon using (false) with check (false);",
  "drop policy if exists catalogo_ubicacion_municipio_deny_authenticated on public.catalogo_ubicacion_municipio;",
  "create policy catalogo_ubicacion_municipio_deny_authenticated",
  "  on public.catalogo_ubicacion_municipio for all to authenticated using (false) with check (false);",
  "",
  "revoke all on table public.catalogo_ubicacion_departamento from anon, authenticated;",
  "revoke all on table public.catalogo_ubicacion_municipio from anon, authenticated;",
  "",
  "comment on table public.catalogo_ubicacion_departamento is",
  "  'Catalogo compartido de departamentos usado por formularios EntoNet.';",
  "comment on table public.catalogo_ubicacion_municipio is",
  "  'Catalogo compartido de municipios usado por formularios EntoNet.';",
  "",
  "commit;"
)

writeLines(sql, "supabase/018_catalogo_ubicacion_compartido.sql", useBytes = TRUE)
cat(sprintf(
  "wrote supabase/018_catalogo_ubicacion_compartido.sql dept=%s muni=%s\n",
  nrow(dept),
  nrow(muni)
))
