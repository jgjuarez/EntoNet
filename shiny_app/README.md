# EntoNet Shiny App

Run the app locally from the repository root:

```r
shiny::runApp("shiny_app")
```

Or from the workspace root:

```r
shiny::runApp("EntoNet/shiny_app")
```

The app reads configuration from environment variables first. For local
development, it also looks for `.env.local` in the repository root and then in
the parent workspace directory used by the earlier local setup.

Required local values:

```text
SUPABASE_DB_URL=postgresql://...
SUPABASE_URL=https://PROJECT_REF.supabase.co
SUPABASE_ANON_KEY=your_publishable_or_anon_key
SUPABASE_SERVICE_ROLE_KEY=your_service_role_key
```

Optional profile and support values:

```text
ENTONET_SKIP_LOGIN=false
PROJECT_REI_SUPPORT_EMAIL=email@example.org
ENTONET_DEFAULT_INSTITUTION_ID=UVG
ENTONET_AUTH_REDIRECT_URL=http://127.0.0.1:3841
```

For local development only, set `ENTONET_SKIP_LOGIN=true` in `.env.local` to
open the authenticated portal directly while checking UI updates. Keep it
disabled for deployed or shared environments.

## Posit Connect Cloud deployment

Connect Cloud deploys R Shiny apps from GitHub. The `manifest.json` file in
this directory records the R package dependencies needed during deployment.
Regenerate it after package changes with:

```r
rsconnect::writeManifest(appDir = "shiny_app")
```

Use `shiny_app/app.R` as the primary file when publishing from Connect Cloud.
Set these values as secret environment variables on the content item:

```text
SUPABASE_DB_URL
SUPABASE_URL
SUPABASE_ANON_KEY
SUPABASE_SERVICE_ROLE_KEY
ENTONET_SKIP_LOGIN=false
ENTONET_DEFAULT_INSTITUTION_ID=UVG
ENTONET_AUTH_REDIRECT_URL=https://CONNECT_CLOUD_CONTENT_URL
```

After the first successful publish, copy the public content URL and add it in
Supabase under Authentication > URL Configuration:

```text
Site URL: https://CONNECT_CLOUD_CONTENT_URL
Redirect URLs: https://CONNECT_CLOUD_CONTENT_URL/**
```

Invitation and password-recovery links must redirect to the deployed content
URL, not to `127.0.0.1`, once users outside the local machine start logging in.

The authenticated portal uses a left navigation rail. Its top-level sections
are Datos, Protocolos, and Entrenamientos; selecting one expands its submenu
vertically. The main workspace remains on the right and renders a module only
after a submenu choice, placing that module directly below the EntoNet header.

New oviposition records are saved to `rei.egg_count_intake` with a `pending`
review status. They do not modify the historical observations table.

Formulario 5 supports both guided individual entry and validated bulk CSV
upload. Both capture paths save new rows to
`public.formulario_5_alimentacion_conteo_intake` with a `pending` review
status. The bulk-upload window provides a downloadable CSV template whose
column names must remain unchanged.

Opening Formulario 5 or Formulario 7 now presents the same three-action menu:
bulk CSV upload, individual entry, and form review. Review access belongs inside
this menu rather than beside the main `Abrir formulario` button; this is the
standard navigation pattern for future forms.

Formulario 7 also supports guided individual entry and validated bulk CSV
upload. The website preserves the official 121-column flat template, generates
`codigo_unico` from rearing code, the finalized bioassay code, and registration date. The finalized bioassay code adds `D` for diagnostic assays, `I` plus dose for intensity assays, or `S` plus the selected synergists. It groups
the individual-entry readings by bottle, and writes each upload atomically to
the normalized `public.formulario_7_bioensayo_*_intake` tables. New records
start with `pending` review status. Its individual-entry tabs unlock in order:
the user must complete the current section and press `Seguir`; every section
already unlocked remains available for corrections.

The Formulario 7 review window lists records by review status and reconstructs
all header, bottle-result, and comment values from Supabase. Values are locked
until the reviewer selects `Editar`. Saving edits returns the record to
`pending`; selecting `Confirmar registro` records the reviewer and changes the
status to `reviewed`. Reviewers can also choose a date range and generate a
random 10% sample, with a minimum of one record when eligible records exist.
Confirming a Formulario 7 record displays progress while connecting to Supabase,
saving the review, and refreshing the selected record.

Formulario 7 first asks for the bioassay type. A diagnostic 1X record stores
its classification as Suceptible, Sospecha de Resistencia, or Resistente and
then continues directly with the remaining capture sections. Intensidad
offers `Exploratorio` and `Completa`: Exploratorio
shows 1X, 2X, 5X, 10X, and one control, while Completa keeps the standard
experimental and control bottle layout.

Before publishing online, replace the local prototype login with managed
authentication and use restricted database credentials. Do not deploy an
administrator connection string or commit real secrets.
