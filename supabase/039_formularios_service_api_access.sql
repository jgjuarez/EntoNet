begin;

grant usage on schema public to service_role;

revoke all on table
  public.formulario_1_ovitrampa_intake,
  public.formulario_1_ovitrampa_detalle_intake,
  public.formulario_1_ovitrampa_eliminacion_audit,
  public.formulario_5_alimentacion_conteo_intake,
  public.formulario_5_alimentacion_eliminacion_audit,
  public.formulario_7_bioensayo_intake,
  public.formulario_7_bioensayo_resultado_intake,
  public.formulario_7_bioensayo_comentario_intake,
  public.formulario_7_bioensayo_eliminacion_audit,
  public.catalogo_ubicacion_departamento,
  public.catalogo_ubicacion_municipio,
  public.catalogo_institucion
from service_role;

grant select on table
  public.formulario_1_ovitrampa_intake,
  public.formulario_1_ovitrampa_detalle_intake,
  public.formulario_1_ovitrampa_eliminacion_audit,
  public.formulario_5_alimentacion_conteo_intake,
  public.formulario_5_alimentacion_eliminacion_audit,
  public.formulario_7_bioensayo_intake,
  public.formulario_7_bioensayo_resultado_intake,
  public.formulario_7_bioensayo_comentario_intake,
  public.formulario_7_bioensayo_eliminacion_audit
to service_role;

grant select on table
  public.catalogo_ubicacion_departamento,
  public.catalogo_ubicacion_municipio,
  public.catalogo_institucion
to service_role;

revoke all on table
  public.formulario_1_ovitrampa_intake,
  public.formulario_1_ovitrampa_detalle_intake,
  public.formulario_1_ovitrampa_eliminacion_audit,
  public.formulario_5_alimentacion_conteo_intake,
  public.formulario_5_alimentacion_eliminacion_audit,
  public.formulario_7_bioensayo_intake,
  public.formulario_7_bioensayo_resultado_intake,
  public.formulario_7_bioensayo_comentario_intake,
  public.formulario_7_bioensayo_eliminacion_audit,
  public.catalogo_ubicacion_departamento,
  public.catalogo_ubicacion_municipio,
  public.catalogo_institucion
from anon, authenticated;

commit;
