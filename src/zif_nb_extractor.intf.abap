interface zif_nb_extractor public.

  types:
    begin of ty_code_snippet,
      objname  type sobj_name,
      objtype  type trobjtype,
      progname type progname,
      test     type sci_chk,
      code     type sci_errc,
      line     type i,
      message  type string,
      before   type string,
      after    type string,
    end of   ty_code_snippet.
  types tt_code_snippet type standard table of ty_code_snippet with empty key.

  types:
    begin of ty_content,
      code_snippets type tt_code_snippet,
    end of   ty_content.

  types tt_progname type standard table of progname.

  methods extract returning value(r_serialized_data) type xstring.

endinterface.
