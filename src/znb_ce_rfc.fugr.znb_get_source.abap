function znb_get_source.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PROGNAME) TYPE  PROGNAME
*"  EXPORTING
*"     VALUE(ET_SOURCE) TYPE  STRING_TABLE
*"----------------------------------------------------------------------
  read report iv_progname into et_source.

endfunction.
