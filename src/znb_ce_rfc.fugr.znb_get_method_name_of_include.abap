function znb_get_method_name_of_include.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PROGNAME) TYPE  PROGNAME
*"  EXPORTING
*"     VALUE(ES_METHOD_KEY) TYPE  SEOCPDKEY
*"  EXCEPTIONS
*"      INCLUDE_NOT_FOUND
*"----------------------------------------------------------------------
  cl_oo_classname_service=>get_method_by_include(
    exporting
      incname             = iv_progname
    receiving
      mtdkey              = es_method_key
    exceptions
      class_not_existing  = 1
      method_not_existing = 2
      others              = 3 ).
  if sy-subrc <> 0.
    raise include_not_found.
  endif.


endfunction.
