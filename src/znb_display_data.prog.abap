*&---------------------------------------------------------------------*
*& Report znb_display_data
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report znb_display_data.

types: begin of ty_demo_output,
         line   type i,
         before type string,
         after  type string,
       end of ty_demo_output.
types: tt_demo_output type standard table of ty_demo_output with empty key.

data lt_solix_data type solix_tab.
data filetable type filetable.
data rc type i.
cl_gui_frontend_services=>file_open_dialog(
  changing
    file_table = filetable
    rc         = rc ).
data(filename) = filetable[ 1 ]-filename.
cl_gui_frontend_services=>gui_upload(
  exporting
    filename = conv #( filename )
    filetype = 'BIN'
  changing
    data_tab = lt_solix_data ).

data(xstring) = cl_bcs_convert=>solix_to_xstring( lt_solix_data ).

data(json_reader) = cl_sxml_string_reader=>create( xstring ).
data ls_content type zif_nb_extractor=>ty_content.
call transformation zxslt_code_snippets
            source xml json_reader
            result root = ls_content.

data(regex) = '\\u([0-9,a-z]{4})'.
data a type string_table.
data lv_newline(1) type c value cl_abap_char_utilities=>cr_lf.
data(c) = 'BEFORE:'.
data(d) = 'AFTER:'.
loop at ls_content-code_snippets assigning field-symbol(<code_snippet>).

  find all occurrences of regex regex in <code_snippet>-before results data(matches) ignoring case.
  loop at matches assigning field-symbol(<match>).
    data(b) = <code_snippet>-before+<match>-offset(<match>-length).
    insert b into table a.
  endloop.
  loop at a assigning field-symbol(<a>).
    data(r) = <a>+2(4).
    replace all occurrences of <a> in <code_snippet>-before with cl_abap_conv_in_ce=>uccp( r ).
  endloop.
  clear: matches, a, b.
  find all occurrences of regex regex in <code_snippet>-after results matches ignoring case.
  loop at matches assigning <match>.
    b = <code_snippet>-after+<match>-offset(<match>-length).
    insert b into table a.
  endloop.
  loop at a assigning <a>.
    r = <a>+2(4).
    replace all occurrences of <a> in <code_snippet>-after with cl_abap_conv_in_ce=>uccp( r ).

  endloop.
*  replace all occurrences of '\r' in <code_snippet>-before with lv_newline.
*  replace all occurrences of '\r' in <code_snippet>-after with lv_newline.
*  concatenate c  <code_snippet>-before lv_newline d <code_snippet>-after into data(output) separated by lv_newline.

  data demo_output type tt_demo_output.
  clear demo_output.
  split <code_snippet>-before at cl_abap_char_utilities=>cr_lf(1) into table data(lt_string) in character mode.
  loop at lt_string assigning field-symbol(<ls_string>).
    insert value #( before = <ls_string>
                    line   = sy-tabix ) into table demo_output.
  endloop.
  split <code_snippet>-after at cl_abap_char_utilities=>cr_lf(1) into table data(lt_string2) in character mode.
  loop at lt_string2 assigning field-symbol(<ls_string2>).
    read table demo_output index sy-tabix assigning field-symbol(<ls_demo_output>).
    if sy-subrc = 0.
      <ls_demo_output>-after = <ls_string2>.
    else.
      insert value #( after = <ls_string2>
                      line  = sy-tabix ) into table demo_output.
    endif.
  endloop.

  cl_demo_output=>write_text( text = |Object Name: { <code_snippet>-objname }| ).
  cl_demo_output=>write_text( text = |Object Type: { <code_snippet>-objtype }| ).
  cl_demo_output=>write_text( text = |Program Name: { <code_snippet>-progname }| ).
  if <code_snippet>-line <> 999.
    cl_demo_output=>write_text( text = |Test: { <code_snippet>-test }| ).
    cl_demo_output=>write_text( text = |Code: { <code_snippet>-code }| ).
    cl_demo_output=>write_text( text = |Line: { <code_snippet>-line }| ).
    cl_demo_output=>write_text( text = |Message: { <code_snippet>-message }| ).
  endif.
  cl_demo_output=>write_data( demo_output ).
  cl_demo_output=>display( ).
*  cl_demo_output=>display_text( output ).
*  cl_demo_output=>display_data( demo_output ).
endloop.
