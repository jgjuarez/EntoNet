-- Remove obsolete Formulario 7 diagnostic suffix from stored bioassay codes.

update public.formulario_7_bioensayo_intake
set codigo_bioensayo = regexp_replace(codigo_bioensayo, '-D$', '')
where codigo_bioensayo ~ '-D$';
