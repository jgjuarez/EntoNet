begin;

-- Historical Formulario 7 records may legitimately omit metadata that was not
-- collected by earlier paper versions. Keep the record identifiable and in
-- pending review while allowing those fields to remain null.
alter table public.formulario_7_bioensayo_intake
  alter column nombre_poblacion drop not null,
  alter column pais drop not null,
  alter column id_institucion drop not null,
  alter column codigo_departamento drop not null,
  alter column codigo_municipio drop not null,
  alter column fecha_realizacion_bioensayo drop not null,
  alter column insecticida drop not null,
  alter column solvente_utilizado drop not null,
  alter column dosis_intensidad_ug_ml drop not null,
  alter column lote_insecticida drop not null,
  alter column fecha_revestimiento_botellas drop not null,
  alter column origen_material drop not null,
  alter column codigo_especie_mosquito drop not null,
  alter column fecha_separacion drop not null,
  alter column hora_separacion drop not null,
  alter column codigo_responsable_revestimiento drop not null,
  alter column codigo_responsable_bioensayo drop not null,
  alter column temperatura_inicial_c drop not null,
  alter column temperatura_final_c drop not null,
  alter column humedad_relativa_inicial_pct drop not null,
  alter column humedad_relativa_final_pct drop not null,
  alter column hora_inicio_bioensayo drop not null,
  alter column hora_final_bioensayo drop not null;

alter table public.formulario_7_bioensayo_intake
  drop constraint if exists formulario_7_pais_check,
  drop constraint if exists formulario_7_bioensayo_intake_pais_check,
  drop constraint if exists formulario_7_tipo_bioensayo_consistente,
  drop constraint if exists formulario_7_solvente_consistente,
  drop constraint if exists formulario_7_edad_consistente,
  drop constraint if exists formulario_7_generacion_consistente;

alter table public.formulario_7_bioensayo_intake
  add constraint formulario_7_pais_check check (
    pais is null or pais in (
      'Belice', 'Guatemala', 'El Salvador', 'Honduras', 'Nicaragua',
      'Costa Rica', 'Panamá', 'República Dominicana'
    )
  ) not valid,
  add constraint formulario_7_tipo_bioensayo_consistente check (
    (
      bioensayo_diagnostica_1x
      and bioensayo_intensidad is null
      and not sinergista_def and not sinergista_pbo and not sinergista_dm
    )
    or (
      not bioensayo_diagnostica_1x
      and bioensayo_intensidad in ('Exploratorio', 'Completa')
      and not sinergista_def and not sinergista_pbo and not sinergista_dm
    )
  ) not valid,
  add constraint formulario_7_solvente_consistente check (
    solvente_utilizado is null
    or solvente_utilizado = 'Otro'
    or (solvente_utilizado = 'Etanol' and nullif(btrim(solvente_otro), '') is null)
  ) not valid,
  add constraint formulario_7_edad_consistente check (
    not edad_indefinida or edad_dias is null
  ) not valid,
  add constraint formulario_7_generacion_consistente check (
    not generacion_filial_indefinida or nullif(btrim(generacion_filial), '') is null
  ) not valid;

alter table public.formulario_7_sinergista_intake
  alter column nombre_poblacion drop not null,
  alter column pais drop not null,
  alter column id_institucion drop not null,
  alter column codigo_departamento drop not null,
  alter column codigo_municipio drop not null,
  alter column sinergista_tipo drop not null,
  alter column dosis_sinergista_ug_ml drop not null,
  alter column fecha_realizacion_bioensayo drop not null,
  alter column insecticida drop not null,
  alter column solvente_utilizado drop not null,
  alter column dosis_intensidad_ug_ml drop not null,
  alter column lote_insecticida drop not null,
  alter column fecha_revestimiento_botellas drop not null,
  alter column origen_material drop not null,
  alter column codigo_especie_mosquito drop not null,
  alter column fecha_separacion drop not null,
  alter column hora_separacion drop not null,
  alter column codigo_responsable_revestimiento drop not null,
  alter column codigo_responsable_bioensayo drop not null,
  alter column temperatura_inicial_c drop not null,
  alter column temperatura_final_c drop not null,
  alter column humedad_relativa_inicial_pct drop not null,
  alter column humedad_relativa_final_pct drop not null,
  alter column hora_inicio_bioensayo drop not null,
  alter column hora_final_bioensayo drop not null;

alter table public.formulario_7_sinergista_intake
  drop constraint if exists formulario_7_sinergista_intake_pais_check,
  drop constraint if exists formulario_7_sinergista_pais_check,
  drop constraint if exists formulario_7_sinergista_solvente_consistente,
  drop constraint if exists formulario_7_sinergista_edad_consistente,
  drop constraint if exists formulario_7_sinergista_generacion_consistente;

alter table public.formulario_7_sinergista_intake
  add constraint formulario_7_sinergista_pais_check check (
    pais is null or pais in (
      'Belice', 'Guatemala', 'El Salvador', 'Honduras', 'Nicaragua',
      'Costa Rica', 'Panamá', 'República Dominicana'
    )
  ) not valid,
  add constraint formulario_7_sinergista_solvente_consistente check (
    solvente_utilizado is null
    or solvente_utilizado = 'Otro'
    or (solvente_utilizado = 'Etanol' and nullif(btrim(solvente_otro), '') is null)
  ) not valid,
  add constraint formulario_7_sinergista_edad_consistente check (
    not edad_indefinida or edad_dias is null
  ) not valid,
  add constraint formulario_7_sinergista_generacion_consistente check (
    not generacion_filial_indefinida or nullif(btrim(generacion_filial), '') is null
  ) not valid;

insert into public.catalogo_ubicacion_departamento
  (pais, codigo_pais, codigo_departamento, departamento)
values
  ('Belice','BZ','01','Belize'),('Belice','BZ','02','Cayo'),('Belice','BZ','03','Corozal'),
  ('Belice','BZ','04','Orange Walk'),('Belice','BZ','05','Stann Creek'),('Belice','BZ','06','Toledo'),
  ('Honduras','HN','01','Atlántida'),('Honduras','HN','02','Colón'),('Honduras','HN','03','Comayagua'),
  ('Honduras','HN','04','Copán'),('Honduras','HN','05','Cortés'),('Honduras','HN','06','Choluteca'),
  ('Honduras','HN','07','El Paraíso'),('Honduras','HN','08','Francisco Morazán'),('Honduras','HN','09','Gracias a Dios'),
  ('Honduras','HN','10','Intibucá'),('Honduras','HN','11','Islas de la Bahía'),('Honduras','HN','12','La Paz'),
  ('Honduras','HN','13','Lempira'),('Honduras','HN','14','Ocotepeque'),('Honduras','HN','15','Olancho'),
  ('Honduras','HN','16','Santa Bárbara'),('Honduras','HN','17','Valle'),('Honduras','HN','18','Yoro'),
  ('Nicaragua','NI','01','Boaco'),('Nicaragua','NI','02','Carazo'),('Nicaragua','NI','03','Chinandega'),
  ('Nicaragua','NI','04','Chontales'),('Nicaragua','NI','05','Estelí'),('Nicaragua','NI','06','Granada'),
  ('Nicaragua','NI','07','Jinotega'),('Nicaragua','NI','08','León'),('Nicaragua','NI','09','Madriz'),
  ('Nicaragua','NI','10','Managua'),('Nicaragua','NI','11','Masaya'),('Nicaragua','NI','12','Matagalpa'),
  ('Nicaragua','NI','13','Nueva Segovia'),('Nicaragua','NI','14','Río San Juan'),('Nicaragua','NI','15','Rivas'),
  ('Nicaragua','NI','16','Región Autónoma de la Costa Caribe Norte'),
  ('Nicaragua','NI','17','Región Autónoma de la Costa Caribe Sur'),
  ('Costa Rica','CR','01','San José'),('Costa Rica','CR','02','Alajuela'),('Costa Rica','CR','03','Cartago'),
  ('Costa Rica','CR','04','Heredia'),('Costa Rica','CR','05','Guanacaste'),('Costa Rica','CR','06','Puntarenas'),
  ('Costa Rica','CR','07','Limón'),
  ('Panamá','PA','01','Bocas del Toro'),('Panamá','PA','02','Coclé'),('Panamá','PA','03','Colón'),
  ('Panamá','PA','04','Chiriquí'),('Panamá','PA','05','Darién'),('Panamá','PA','06','Herrera'),
  ('Panamá','PA','07','Los Santos'),('Panamá','PA','08','Panamá'),('Panamá','PA','09','Veraguas'),
  ('Panamá','PA','10','Panamá Oeste'),('Panamá','PA','11','Emberá-Wounaan'),('Panamá','PA','12','Guna Yala'),
  ('Panamá','PA','13','Ngäbe-Buglé'),('Panamá','PA','14','Naso Tjër Di'),
  ('República Dominicana','DO','01','Distrito Nacional'),('República Dominicana','DO','02','Azua'),
  ('República Dominicana','DO','03','Baoruco'),('República Dominicana','DO','04','Barahona'),
  ('República Dominicana','DO','05','Dajabón'),('República Dominicana','DO','06','Duarte'),
  ('República Dominicana','DO','07','Elías Piña'),('República Dominicana','DO','08','El Seibo'),
  ('República Dominicana','DO','09','Espaillat'),('República Dominicana','DO','10','Independencia'),
  ('República Dominicana','DO','11','La Altagracia'),('República Dominicana','DO','12','La Romana'),
  ('República Dominicana','DO','13','La Vega'),('República Dominicana','DO','14','María Trinidad Sánchez'),
  ('República Dominicana','DO','15','Monte Cristi'),('República Dominicana','DO','16','Pedernales'),
  ('República Dominicana','DO','17','Peravia'),('República Dominicana','DO','18','Puerto Plata'),
  ('República Dominicana','DO','19','Hermanas Mirabal'),('República Dominicana','DO','20','Samaná'),
  ('República Dominicana','DO','21','San Cristóbal'),('República Dominicana','DO','22','San Juan'),
  ('República Dominicana','DO','23','San Pedro de Macorís'),('República Dominicana','DO','24','Sánchez Ramírez'),
  ('República Dominicana','DO','25','Santiago'),('República Dominicana','DO','26','Santiago Rodríguez'),
  ('República Dominicana','DO','27','Valverde'),('República Dominicana','DO','28','Monseñor Nouel'),
  ('República Dominicana','DO','29','Monte Plata'),('República Dominicana','DO','30','Hato Mayor'),
  ('República Dominicana','DO','31','San José de Ocoa'),('República Dominicana','DO','32','Santo Domingo')
on conflict (pais, codigo_departamento) do update set
  codigo_pais = excluded.codigo_pais,
  departamento = excluded.departamento,
  actualizado_en = now();

comment on column public.formulario_7_bioensayo_intake.pais is
  'País de Centroamérica o República Dominicana; puede faltar en capturas históricas pendientes.';
comment on column public.formulario_7_sinergista_intake.pais is
  'País de Centroamérica o República Dominicana; puede faltar en capturas históricas pendientes.';

commit;
