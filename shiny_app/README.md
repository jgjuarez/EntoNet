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
PROJECT_REI_APP_USER=your_user_name
PROJECT_REI_APP_PASSWORD=your_private_password
```

Optional profile and support values:

```text
PROJECT_REI_PROFILE_NAME=Nombre Apellido
PROJECT_REI_PROFILE_INSTITUTION=Institucion
PROJECT_REI_PROFILE_POSITION=Puesto
PROJECT_REI_PROFILE_COUNTRY=Pais
PROJECT_REI_SUPPORT_EMAIL=email@example.org
SUPABASE_URL=https://PROJECT_REF.supabase.co
SUPABASE_SERVICE_ROLE_KEY=your_service_role_key
```

New oviposition records are saved to `rei.egg_count_intake` with a `pending`
review status. They do not modify the historical observations table.

Before publishing online, replace the local prototype login with managed
authentication and use restricted database credentials. Do not deploy an
administrator connection string or commit real secrets.
