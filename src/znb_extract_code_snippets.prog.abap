*&---------------------------------------------------------------------*
*& Report znb_extract_code_snippets
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report znb_extract_code_snippets.

data lv_progname type progname.

parameters p_var type char10 as listbox visible length 40 user-command ord.

selection-screen begin of block variant_a with frame title text-vaa.
  selection-screen comment /1(79) coma7 modif id a.
  selection-screen comment /1(79) coma1 modif id a.
  selection-screen comment /1(79) coma2 modif id a.
  selection-screen comment /1(79) coma3 modif id a.
  selection-screen comment /1(79) coma4 modif id a.
  selection-screen comment /1(79) coma5 modif id a.
  selection-screen comment /1(79) coma6 modif id a.
  selection-screen skip.

  select-options s_pnam for lv_progname modif id a.
  parameters p_begin type string lower case modif id a.
  parameters p_end type string lower case modif id a.
  parameters p_cona type i modif id a.
selection-screen end of block variant_a.

selection-screen begin of block variant_b with frame title text-vab.
  selection-screen comment /1(79) comb1 modif id b.
  selection-screen comment /1(79) comb2 modif id b.
  selection-screen comment /1(79) comb3 modif id b.
  selection-screen comment /1(79) comb4 modif id b.
  selection-screen skip.

  parameters p_run_a type satc_d_ac_display_id modif id b.
  parameters p_run_b type satc_d_ac_display_id modif id b.
  parameters p_rfc type rfcdest modif id b.
  parameters p_conb type i modif id b.
selection-screen end of block variant_b.

start-of-selection.
  data lt_progname type zif_nb_extractor=>tt_progname.

  loop at s_pnam assigning field-symbol(<ls_option>) where sign = 'I' and option = 'EQ'.
    insert <ls_option>-low into table lt_progname.
  endloop.
  sort lt_progname ascending.
  delete adjacent duplicates from lt_progname.

  if p_var = 'A'.
    data(lo_extractor) = zcl_nb_extractor_factory=>create_extractor_type_a( exporting iv_begin_of_change_indicator = p_begin
                                                                                      iv_end_of_change_indicator   = p_end
                                                                                      it_program_names             = lt_progname
                                                                                      iv_context_lines             = p_cona ).
  elseif p_var = 'B'.
    lo_extractor = zcl_nb_extractor_factory=>create_extractor_type_b( iv_reference_run_id_a = p_run_a
                                                                      iv_reference_run_id_b = p_run_b
                                                                      iv_rfcdest            = p_rfc
                                                                      iv_context_lines      = p_conb ).
  endif.

  data(lv_serialized_data) = lo_extractor->extract( ).

  " User input for file destination
  data lv_filename type string.
  data lv_path type string.
  data lv_fullpath type string.
  call method cl_gui_frontend_services=>file_save_dialog
    exporting
      default_extension         = 'JSON'
      default_file_name         = 'codesnippets.json'
      prompt_on_overwrite       = 'X'
    changing
      filename                  = lv_filename
      path                      = lv_path
      fullpath                  = lv_fullpath
    exceptions
      cntl_error                = 1
      error_no_gui              = 2
      not_supported_by_gui      = 3
      invalid_default_file_name = 4
      others                    = 5.
  if sy-subrc <> 0.
    assert 1 = 0.
  endif.

  " Download
  data(lt_solix_data) = cl_bcs_convert=>xstring_to_solix( iv_xstring = lv_serialized_data ).
  cl_gui_frontend_services=>gui_download(
    exporting
      filetype                = 'BIN'
      filename                = lv_filename
      bin_filesize            = xstrlen( lv_serialized_data )
    changing
      data_tab                = lt_solix_data
    exceptions
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      not_supported_by_gui    = 22
      error_no_gui            = 23
      others                  = 24 ).
  if sy-subrc <> 0.
    assert 1 = 0.
  endif.


initialization.
  call function 'SELECT_OPTIONS_RESTRICT'
    exporting
      program     = 'ZNB_EXTRACT_CODE_SNIPPETS'
      restriction = value sscr_restrict(
                      opt_list_tab = value #( ( name       = 'EQ_OR_NEQ'
                                                options-eq = abap_true ) )
                      ass_tab      = value #( ( kind       = 'S'
                                                name       = 'S_PNAM'
                                                sg_main    = '*' "Both include and exclude
                                                op_main    = 'EQ_OR_NEQ' ) ) )
    exceptions
      others      = 1.


at selection-screen on value-request for p_var.
  data(lt_variant) = value vrm_values( ( key  = 'A'
                                         text = 'Variant A' )
                                       ( key  = 'B'
                                         text = 'Variant B' ) ).
  call function 'VRM_SET_VALUES'
    exporting
      id     = 'P_VAR'
      values = lt_variant.


at selection-screen output.
  coma7 = 'Before/After Code Snippets are recognized by pattern:'.
  coma1 = '< Any Source Code (Context) >'.
  coma2 = '< Begin of Change Indicator >'.
  coma3 = '< Old Source Code (commented with *) >'.
  coma4 = '< New Source Code (productive) >'.
  coma5 = '< End of Change Indicator >'.
  coma6 = '< Any Source Code (Context) >'.
  comb1 = 'System A (before migration) and system B (post migration) are'.
  comb2 = 'compared directly. Finding locations are identified automatically'.
  comb3 = 'via reference S4HANA Readiness run results.'.
  comb4 = 'The report is to be executed in the post migration system B.'.
  loop at screen.
    case screen-group1.
      when 'A'.
        if p_var <> 'A'.
          screen-active = '0'.
          screen-invisible = '1'.
        else.
          screen-active = '1'.
          screen-invisible = '0'.
        endif.
        modify screen.
      when 'B'.
        if p_var <> 'B'.
          screen-active = '0'.
          screen-invisible = '1'.
        else.
          screen-active = '1'.
          screen-invisible = '0'.
        endif.
        modify screen.
    endcase.
  endloop.


at selection-screen.
  data(user_command) = sy-ucomm.
  clear sy-ucomm.

  if user_command = 'ORD'.
    return.
  endif.

  if p_var is initial.
    set cursor field 'P_VAR'.
    message 'Choose a variant'(var) type 'E'.
  elseif p_var = 'A'.
    if p_cona < 0 or p_cona > 10.
      set cursor field 'P_CONA'.
      message 'Choose context between 0 and 10 lines'(coa) type 'E'.
    endif.

    if p_begin is initial.
      set cursor field 'P_BEGIN'.
      message 'Begin of Change Indicator must not be initial'(beg) type 'E'.
    endif.

    if p_end is initial.
      set cursor field 'P_END'.
      message 'End of Change Indicator must not be initial'(end) type 'E'.
    endif.
  elseif p_var = 'B'.
    if p_run_a is initial.
      set cursor field 'P_RUN'.
      message 'Enter display ID of remote run against A'(rua) type 'E'.
    endif.

    if p_run_b is initial.
      set cursor field 'P_RUN'.
      message 'Enter display ID of local run in B'(rub) type 'E'.
    endif.

    if p_rfc is initial.
      set cursor field 'P_RFC'.
      message 'Enter RFC destination to pre migration system'(rfc) type 'E'.
    endif.

    if p_conb < 0 or p_conb > 10.
      set cursor field 'P_CONB'.
      message 'Choose context between 0 and 10 lines'(cob) type 'E'.
    endif.
  endif.
