#!/usr/bin/env bash
set -euo pipefail

repo_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
env_file="${repo_dir}/.env.local"
psql_bin="${PSQL_BIN:-/opt/homebrew/opt/libpq/bin/psql}"

if [[ ! -f "${env_file}" ]]; then
  parent_env_file="${repo_dir}/../.env.local"
  if [[ -f "${parent_env_file}" ]]; then
    env_file="${parent_env_file}"
  else
    printf 'Missing %s. Copy .env.example to .env.local and add the Supabase Session pooler URL.\n' "${env_file}" >&2
    exit 1
  fi
fi

if [[ ! -x "${psql_bin}" ]]; then
  printf 'psql not found at %s. Set PSQL_BIN to your psql path and retry.\n' "${psql_bin}" >&2
  exit 1
fi

db_url="$(sed -n 's/^SUPABASE_DB_URL=//p' "${env_file}")"

if [[ -z "${db_url}" || "${db_url}" == *YOUR_PASSWORD* || "${db_url}" == *PROJECT_REF* || "${db_url}" == *REGION* ]]; then
  printf 'SUPABASE_DB_URL is missing or still contains example placeholders.\n' >&2
  exit 1
fi

if [[ "${db_url}" == *\?*\?sslmode=require ]]; then
  db_url="${db_url%\?sslmode=require}"
fi

for migration in "${repo_dir}"/supabase/*.sql; do
  printf 'Running %s\n' "$(basename "${migration}")"
  "${psql_bin}" "${db_url}" --set ON_ERROR_STOP=1 --file "${migration}"
done
