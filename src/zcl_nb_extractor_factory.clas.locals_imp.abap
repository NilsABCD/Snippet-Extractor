class lcl_source_reader definition final.
  public section.
    methods read_program importing iv_program_name  type progname
                         returning value(rt_source) type string_table.
endclass.

class lcl_source_reader implementation.

  method read_program.
    read report iv_program_name into rt_source.
    if sy-subrc <> 0.
      "assert 1 = 0.
    endif.
  endmethod.

endclass.


class lcl_failure_log definition final.
  public section.
    types:
      begin of ty_failure,
        progname type progname,
        test     type sci_chk,
        code     type sci_errc,
        line     type i,
        reason   type string,
      end of ty_failure.
    types tt_failure type standard table of ty_failure with empty key.

    methods add_failure importing is_failure type ty_failure.
    methods write_output.

  private section.
    data mt_failure type tt_failure.

endclass.

class lcl_failure_log implementation.

  method add_failure.
    insert is_failure into table mt_failure.
  endmethod.

  method write_output.

  endmethod.

endclass.


class lcl_extractor_type_a definition final.
  public section.
    interfaces zif_nb_extractor.
    methods constructor importing iv_begin_of_change_indicator type string
                                  iv_end_of_change_indicator   type string
                                  it_program_names             type zif_nb_extractor=>tt_progname
                                  iv_context_lines             type i.

  private section.
    data mo_source_reader type ref to lcl_source_reader.
    data mt_progname type zif_nb_extractor=>tt_progname.
    data mv_begin_of_change_indicator type string.
    data mv_end_of_change_indicator type string.
    data mv_context_lines type i.

    methods extract_from_program importing iv_program_name         type progname
                                 returning value(rt_code_snippets) type zif_nb_extractor=>tt_code_snippet.



endclass.


class lcl_extractor_type_a implementation.

  method constructor.
    mo_source_reader = new #( ).
    mt_progname                  = it_program_names.
    mv_begin_of_change_indicator = iv_begin_of_change_indicator.
    mv_end_of_change_indicator   = iv_end_of_change_indicator.
    mv_context_lines             = iv_context_lines.
  endmethod.

  method zif_nb_extractor~extract.

    data ls_content type zif_nb_extractor=>ty_content.

    loop at mt_progname assigning field-symbol(<lv_progname>).
      data(lt_code_snippets) = extract_from_program( iv_program_name = <lv_progname> ).
      insert lines of lt_code_snippets into table ls_content-code_snippets.
    endloop.

    data writer type ref to cl_sxml_string_writer.
    writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    try.
        call transformation zxslt_code_snippets
              source root = ls_content
              result xml writer.
      catch cx_st_error into data(st_exc).
    endtry.
    r_serialized_data = writer->get_output( ).

  endmethod.

  method extract_from_program.

    types:
      begin of ty_index,
        begin type i,
        end   type i,
      end of ty_index.

    data ls_tadir type tadir.
    data lv_index type ty_index.
    data lt_index type standard table of ty_index.
    data lt_content type zif_nb_extractor=>ty_content.
    data ls_code_snippet like line of lt_content-code_snippets.
    data lv_newline(1) type c value cl_abap_char_utilities=>cr_lf.

    " Determine object information
    data(lt_source) = mo_source_reader->read_program( iv_program_name ).
    call function 'TR_TRANSFORM_TRDIR_TO_TADIR'
      exporting
        iv_trdir_name       = iv_program_name
      importing
        es_tadir_keys       = ls_tadir
      exceptions
        invalid_name_syntax = 1
        others              = 2.
    if sy-subrc <> 0.
      " Should not happen
      assert 1 = 0.
    endif.

    " Ignore empty lines and "-commented lines, also remove trailing blanks
    loop at lt_source assigning field-symbol(<ls_source>).
      if <ls_source> is initial.
        delete lt_source.
        continue.
      endif.
      shift <ls_source> left deleting leading ' '.
      if <ls_source>(1) = '"'.
        delete lt_source.
        continue.
      endif.
    endloop.

    " Find all begin and corresponding end indicators in current source
    data(lv_start_detected) = abap_false.
    loop at lt_source assigning <ls_source>.
      " We always look for starts
      find pcre mv_begin_of_change_indicator in <ls_source>.
      if sy-subrc = 0.
        " If a second start is found before an end, the first start will be overwritten
        lv_start_detected = abap_true.
        lv_index-begin = sy-tabix.
        continue.
      endif.
      if lv_start_detected = abap_false and <ls_source>(1) = '*'.
        " Ignore all commented lines that are not inside begin/end indicators
        delete lt_source.
        continue.
      endif..
      " But we only look for ends if a start was detected
      if lv_start_detected = abap_true.
        find pcre mv_end_of_change_indicator in <ls_source>.
        if sy-subrc = 0.
          lv_start_detected = abap_false.
          lv_index-end = sy-tabix.
          insert lv_index into table lt_index.
        endif.
      endif.
    endloop.

    " Extract code snippets for each found begin/end pair
    loop at lt_index assigning field-symbol(<ls_index>).
      clear ls_code_snippet.

      " Determine context before
      if mv_context_lines > 0.
        data(lv_current_index) = <ls_index>-begin - mv_context_lines.
        if lv_current_index < 1.
          lv_current_index = 1.
        endif.
        if sy-tabix > 1.
          data(lv_most_recent_end_index) = lt_index[ sy-tabix - 1 ]-end.
          if lv_current_index <= lv_most_recent_end_index.
            lv_current_index = lv_most_recent_end_index + 1.
          endif.
        endif.

        while lv_current_index <> <ls_index>-begin.
          data(lv_current_line) = lt_source[ lv_current_index ].

          if ls_code_snippet-before is not initial.
            concatenate ls_code_snippet-before lv_current_line into ls_code_snippet-before separated by lv_newline.
          else.
            concatenate ls_code_snippet-before lv_current_line into ls_code_snippet-before.
          endif.

          if ls_code_snippet-after is not initial.
            concatenate ls_code_snippet-after lv_current_line into ls_code_snippet-after separated by lv_newline.
          else.
            concatenate ls_code_snippet-after lv_current_line into ls_code_snippet-after.
          endif.

          lv_current_index = lv_current_index + 1.
        endwhile.
      endif.

      " Determine code before change
      data(lv_work_done_indicator) = abap_false.
      lv_current_index = <ls_index>-begin.
      while lv_work_done_indicator = abap_false and lv_current_index < ( <ls_index>-end - 1 ).
        lv_current_index = lv_current_index + 1.

        lv_current_line = lt_source[ lv_current_index ].
        if sy-subrc <> 0.
          " Should not happen
          assert 1 = 0.
        endif.

        if lv_current_line(1) = '*'.
          " We expect old code to be in * comments, " comments not supported at the moment
          replace first occurrence of '*' in lv_current_line with ''.
          shift lv_current_line left deleting leading ' '.
          if ls_code_snippet-before is not initial.
            concatenate ls_code_snippet-before lv_current_line into ls_code_snippet-before separated by lv_newline.
          else.
            concatenate ls_code_snippet-before lv_current_line into ls_code_snippet-before.
          endif.
        else.
          " End of code before changes reached
          lv_current_index -= 1.
          lv_work_done_indicator = abap_true.
        endif.
      endwhile.

      " Determine code after change
      while lv_current_index < <ls_index>-end - 1.
        lv_current_index = lv_current_index + 1.

        lv_current_line = lt_source[ lv_current_index ].
        if sy-subrc <> 0.
          " Should not happen
          assert 1 = 0.
        endif.

        " We just take everything for now
        if ls_code_snippet-after is not initial.
          concatenate ls_code_snippet-after lv_current_line into ls_code_snippet-after separated by lv_newline.
        else.
          concatenate ls_code_snippet-after lv_current_line into ls_code_snippet-after.
        endif.

      endwhile.

      " Determine context after
      if mv_context_lines > 0.
        lv_current_index = <ls_index>-end + 1.
        data(lv_max_index) = <ls_index>-end + mv_context_lines.
        if lv_max_index > lines( lt_source ).
          lv_max_index = lines( lt_source ).
        endif.
        if lines( lt_index ) > sy-tabix.
          data(lv_next_begin_index) = lt_index[ sy-tabix + 1 ]-begin.
          if lv_max_index >= lv_next_begin_index.
            lv_max_index = lv_next_begin_index - 1.
          endif.
        endif.

        while lv_current_index <> lv_max_index + 1.
          lv_current_line = lt_source[ lv_current_index ].

          if ls_code_snippet-before is not initial.
            concatenate ls_code_snippet-before lv_current_line into ls_code_snippet-before separated by lv_newline.
          else.
            concatenate ls_code_snippet-before lv_current_line into ls_code_snippet-before.
          endif.

          if ls_code_snippet-after is not initial.
            concatenate ls_code_snippet-after lv_current_line into ls_code_snippet-after separated by lv_newline.
          else.
            concatenate ls_code_snippet-after lv_current_line into ls_code_snippet-after.
          endif.

          lv_current_index = lv_current_index + 1.
        endwhile.
      endif.

      ls_code_snippet-objname = ls_tadir-obj_name.
      ls_code_snippet-objtype = ls_tadir-object.
      ls_code_snippet-progname = iv_program_name.
      ls_code_snippet-test = 'Unknown'.
      ls_code_snippet-code = 'Unknown'.
      ls_code_snippet-line = '999'.
      ls_code_snippet-message = 'Unknown'.
      insert ls_code_snippet into table rt_code_snippets.
    endloop.


  endmethod.

endclass.


class lcl_extractor_type_b definition final.
  public section.
    interfaces zif_nb_extractor.
    methods constructor importing iv_reference_run_id_a type satc_d_ac_display_id
                                  iv_reference_run_id_b type satc_d_ac_display_id
                                  iv_rfcdest            type rfcdest
                                  iv_context_lines      type i.

  private section.
    types:
      begin of ty_finding_detail,
        test     type sci_chk,
        code     type sci_errc,
        line     type i,
        message  type string,
        checksum type int4,
      end of ty_finding_detail.
    types tt_finding_detail type standard table of ty_finding_detail with empty key.

    types:
      begin of ty_finding_info,
        progname        type progname,
        finding_details type tt_finding_detail,
      end of ty_finding_info.
    types tt_finding_info type standard table of ty_finding_info with empty key.

    types:
      begin of ty_compare_source,
        original_line_nr type i,
        line             type string,
      end of ty_compare_source.
    types: tt_compare_source type standard table of ty_compare_source with empty key.

    data mo_failure_log type ref to lcl_failure_log.
    data mo_source_reader type ref to lcl_source_reader.
    data mv_rfcdest type rfcdest.
    data mv_reference_run_id_a type satc_d_ac_display_id.
    data mv_reference_run_id_b type satc_d_ac_display_id.
    data mv_context_lines type i.

    methods get_finding_infos returning value(rt_finding_info) type tt_finding_info.

    methods get_finding_infos_single importing iv_reference_run_id    type satc_d_ac_display_id
                                     returning value(rt_finding_info) type tt_finding_info.

    methods extract_with_finding_info importing is_finding_info         type ty_finding_info
                                      returning value(rt_code_snippets) type zif_nb_extractor=>tt_code_snippet.

    methods format_source_for_comparison importing it_source                type string_table
                                         returning value(rt_compare_source) type tt_compare_source.

endclass.

class lcl_extractor_type_b implementation.

  method constructor.
    mo_source_reader = new #( ).
    mo_failure_log = new #( ).
    mv_reference_run_id_a = iv_reference_run_id_a.
    mv_reference_run_id_b = iv_reference_run_id_b.
    mv_rfcdest = iv_rfcdest.
    mv_context_lines = iv_context_lines.
  endmethod.

  method zif_nb_extractor~extract.

    data ls_content type zif_nb_extractor=>ty_content.

    data(lt_finding_info) = get_finding_infos( ).

    loop at lt_finding_info assigning field-symbol(<ls_finding_info>).
      data(lt_code_snippets) = extract_with_finding_info( is_finding_info = <ls_finding_info> ).
      insert lines of lt_code_snippets into table ls_content-code_snippets.
    endloop.

    data writer type ref to cl_sxml_string_writer.
    writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
    try.
        call transformation zxslt_code_snippets
              source root = ls_content
              result xml writer.
      catch cx_st_error into data(st_exc).
    endtry.
    r_serialized_data = writer->get_output( ).

  endmethod.

  method get_finding_infos_single.

    data lt_pre_mig_fnd_info like rt_finding_info.
    data lt_post_mig_fnd_info like rt_finding_info.

    select single data_source_id from satc_ac_resulth into @data(lv_object_provider)
      where display_id = @iv_reference_run_id.

    select navigation_data from satc_ac_rstit_ddlv into table @data(lt_navigation_data)
      where display_id    = @iv_reference_run_id and
            exc_validity <> 'E'.

    data(memento_service) = new cl_satc_ac_memento( ).
    loop at lt_navigation_data assigning field-symbol(<lv_navigation_data>).
      if <lv_navigation_data> is initial.
        " This can happen, I don't know why
        continue.
      endif.
      data(navigator) = memento_service->new_navigator_by_memento( exporting i_memento = <lv_navigation_data>-navigation_data ).
      cast cl_satc_ac_navigator_ci( navigator )->get_ci_result( importing e_ci_result = data(ls_ci_result) ).
*      if ls_ci_result-program_name is initial.
*        " No source code finding, we skip this         => Seems not to be true, CodePal Checks have empty program name for example
*        continue.
*      endif.
      if ls_ci_result-sobjname is initial.
        " Let's hope the S4HANA Readiness checks always supply this information
        mo_failure_log->add_failure( is_failure = value #( progname = 'Undefined'
                                                           test     = ls_ci_result-test
                                                           code     = ls_ci_result-code
                                                           line     = ls_ci_result-line
                                                           reason   = 'No subobject information for finding' ) ).
        continue.
      endif.

      data(check_instance) = cl_ci_tests=>get_test_ref( ls_ci_result-test ).
      check_instance->get_detail( exporting p_detail_packed = ls_ci_result-detail
                                  importing p_detail        = data(details) ).

      data(lo_result_node) = check_instance->get_result_node( exporting p_kind = 'T' ).
      lo_result_node->set_srcid( p_info  = ls_ci_result
                                 p_srcid = cl_sca_object_provider_api=>get_ci_src_id_for_obj_provider( lv_object_provider ) ).
      lo_result_node->set_info( ls_ci_result ).

      read table rt_finding_info with key progname = ls_ci_result-sobjname assigning field-symbol(<ls_finding_info>).
      if sy-subrc <> 0.
        " No entry yet, we create one
        insert value #( progname = ls_ci_result-sobjname ) into table rt_finding_info assigning <ls_finding_info>.
      endif.
      insert value #( test     = ls_ci_result-test
                      code     = ls_ci_result-code
                      message  = lo_result_node->if_ci_test~text[ 1 ]-message
                      line     = ls_ci_result-line
                      checksum = ls_ci_result-checksum1 ) into table <ls_finding_info>-finding_details.
    endloop.

  endmethod.

  method extract_with_finding_info.

    data lt_content type zif_nb_extractor=>ty_content.
    data ls_code_snippet like line of lt_content-code_snippets.
    data ls_tadir type tadir.
    data lv_newline(1) type c value cl_abap_char_utilities=>cr_lf.

    " Get source (local)
    if is_finding_info-progname+30(2) = 'CM'.
      " It's a method. The method include in the remote and local system can have different names.
      data ls_method_key type seocpdkey.
      call function 'ZNB_GET_METHOD_NAME_OF_INCLUDE' destination mv_rfcdest
        exporting
          iv_progname           = is_finding_info-progname
        importing
          es_method_key         = ls_method_key
        exceptions
          include_not_found     = 1
          communication_failure = 2
          system_failure        = 3.
      if sy-subrc <> 0 or ls_method_key is initial.
        return.
      endif.
      data method_include type program.
      cl_oo_classname_service=>get_method_include(
        exporting
          mtdkey              = ls_method_key
        receiving
          result              = method_include
        exceptions
          class_not_existing  = 1
          method_not_existing = 2
          others              = 3 ).
      if sy-subrc <> 0.
        return.
      endif.
      data(lt_source_raw_post) = mo_source_reader->read_program( method_include ).
    else.
      lt_source_raw_post = mo_source_reader->read_program( is_finding_info-progname ).
    endif.

    " Get source (rfc)
    data lt_source_raw_pre like lt_source_raw_post.
    call function 'ZNB_GET_SOURCE' destination mv_rfcdest
      exporting
        iv_progname = is_finding_info-progname
      importing
        et_source   = lt_source_raw_pre.
    if lt_source_raw_pre is initial or lt_source_raw_post is initial.
      loop at is_finding_info-finding_details assigning  field-symbol(<ls_finding_detail>).
        mo_failure_log->add_failure( is_failure = value #( progname = is_finding_info-progname
                                                           test     = <ls_finding_detail>-test
                                                           code     = <ls_finding_detail>-code
                                                           line     = <ls_finding_detail>-line
                                                           reason   = 'No source code could be loaded for program' ) ).
      endloop.
      return.
    endif.

    " Determine object information (local)
    call function 'TR_TRANSFORM_TRDIR_TO_TADIR'
      exporting
        iv_trdir_name       = is_finding_info-progname
      importing
        es_tadir_keys       = ls_tadir
      exceptions
        invalid_name_syntax = 1
        others              = 2.
    if sy-subrc <> 0.
      " Should not happen
      loop at is_finding_info-finding_details assigning  <ls_finding_detail>.
        mo_failure_log->add_failure( is_failure = value #( progname = is_finding_info-progname
                                                           test     = <ls_finding_detail>-test
                                                           code     = <ls_finding_detail>-code
                                                           line     = <ls_finding_detail>-line
                                                           reason   = 'No tadir information for main object could be determined' ) ).
      endloop.
      return.
    endif.

    "-----------------------------------------------------------------------------------------------
    " Format for comparison
    "-----------------------------------------------------------------------------------------------
    data(lt_source_post) = format_source_for_comparison( lt_source_raw_post ).
    data(lt_source_pre) = format_source_for_comparison( lt_source_raw_pre ).
    data(lv_index_pre) = 0.
    loop at lt_source_pre assigning field-symbol(<ls_source_pre>).
      data(lv_index_post) = 0.
      lv_index_pre += 1.
      loop at lt_source_post assigning field-symbol(<ls_source_post>) where line = <ls_source_pre>-line.
        " For now we just take the first hit
        if sy-tabix >= lv_index_pre.
          lv_index_post = sy-tabix.
          exit.
        else.
          " Matches in already processed code need to be ignored
        endif.
      endloop.
      if lv_index_post = 0.
        " No match found
        insert value #( line = '"$' ) into lt_source_post index lv_index_pre.
      endif.
      if lv_index_post is not initial.
        do lv_index_post - lv_index_pre times.
          insert value #( line = '"$' ) into lt_source_pre index lv_index_pre.
          lv_index_pre += 1.
        enddo.
      endif.
    endloop.
    do lines( lt_source_post ) - lines( lt_source_pre ) times.
      insert value #( line = '"$' ) into table lt_source_pre.
    enddo.
    "-----------------------------------------------------------------------------------------------

    "-----------------------------------------------------------------------------------------------
    " Extract code snippets
    "-----------------------------------------------------------------------------------------------
    data(lt_finding_detail) = is_finding_info-finding_details.
    sort lt_finding_detail by line ascending.

    loop at lt_finding_detail assigning <ls_finding_detail>.
      " Determine finding line in source
      read table lt_source_pre with key original_line_nr = <ls_finding_detail>-line transporting no fields.
      if sy-subrc <> 0.
        mo_failure_log->add_failure( is_failure = value #( progname = is_finding_info-progname
                                                           test     = <ls_finding_detail>-test
                                                           code     = <ls_finding_detail>-code
                                                           line     = <ls_finding_detail>-line
                                                           reason   = 'Finding line not existing in formatted source' ) ).
        continue.
      endif.
      data(lv_finding_index) = sy-tabix.

      " Currently we demand that the finding line has changed in the post-upgrade system, otherwise we don't continue
      if lt_source_post[ lv_finding_index ]-original_line_nr <> 0.
        mo_failure_log->add_failure( is_failure = value #( progname = is_finding_info-progname
                                                           test     = <ls_finding_detail>-test
                                                           code     = <ls_finding_detail>-code
                                                           line     = <ls_finding_detail>-line
                                                           reason   = 'Finding line unchanged in post-upgrade system' ) ).
        continue.
      endif.

      " Get before snippet
      data(lv_current_index) = lv_finding_index.
      while lt_source_post[ lv_current_index ]-original_line_nr = 0.
        lv_current_index -= 1.
        if lv_current_index = 0.
          " End of source
          exit.
        endif.
      endwhile.
      data(lv_upper_index) = lv_current_index + 1.
      lv_current_index = lv_finding_index.
      while lt_source_post[ lv_current_index ]-original_line_nr = 0.
        lv_current_index += 1.
        if lv_current_index > lines( lt_source_post ).
          " End of source
          exit.
        endif.
      endwhile.
      data(lv_lower_index) = lv_current_index - 1.
      lv_current_index = lv_upper_index.
      do lv_lower_index - lv_upper_index + 1 times.
        data(lv_current_line) = lt_source_pre[ lv_current_index ]-line.
        if ls_code_snippet-before is not initial.
          concatenate ls_code_snippet-before lv_current_line into ls_code_snippet-before separated by lv_newline.
        else.
          concatenate ls_code_snippet-before lv_current_line into ls_code_snippet-before.
        endif.
        lv_current_index += 1.
      enddo.
      data(lv_context_upper_end) = lv_upper_index - 1.

      data(finding_line_in_snippet) = lv_finding_index - lv_upper_index + 1.

      " For now
      if lv_lower_index - lv_upper_index > 10.
        return.
      endif.

      " Get after snippet
      lv_upper_index = lv_lower_index + 1.
      lv_current_index = lv_upper_index.
      if lv_current_index > lines( lt_source_pre ) or lt_source_pre[ lv_current_index ]-original_line_nr <> 0.
        " No after snippet to be found
        lv_lower_index = lv_upper_index.
      else.
        while lt_source_pre[ lv_current_index ]-original_line_nr = 0.
          lv_current_index += 1.
          if lv_current_index > lines( lt_source_pre ).
            " End of source
            exit.
          endif.
        endwhile.
        lv_lower_index = lv_current_index - 1.
        lv_current_index = lv_upper_index.
        do lv_lower_index - lv_upper_index + 1 times.
          lv_current_line = lt_source_post[ lv_current_index ]-line.
          if ls_code_snippet-after is not initial.
            concatenate ls_code_snippet-after lv_current_line into ls_code_snippet-after separated by lv_newline.
          else.
            concatenate ls_code_snippet-after lv_current_line into ls_code_snippet-after.
          endif.
          lv_current_index += 1.
        enddo.
      endif.
      data(lv_context_lower_begin) = lv_lower_index + 1.

      " For now
      if lv_lower_index - lv_upper_index > 10.
        return.
      endif.


      " Get upper context
      data lt_upper_context_pre type string_table.
      data lt_upper_context_post type string_table.
      data lv_upper_context_pre type string.
      data lv_upper_context_post type string.
      data(lv_match_lines_collected) = 0.
      data(lv_lines_collected_pre) = 0.
      data(lv_lines_collected_post) = 0.
      data(lv_work_done_indicator) = abap_false.
      lv_current_index = lv_context_upper_end.
      while lv_work_done_indicator = abap_false.
        if lv_current_index <= 0.
          exit.
        endif.
        data(ls_current_line_pre) = lt_source_pre[ lv_current_index ].
        data(ls_current_line_post) = lt_source_post[ lv_current_index ].
        if ls_current_line_pre-original_line_nr <> 0.
          insert ls_current_line_pre-line into lt_upper_context_pre index 1.
          lv_lines_collected_pre += 1.
        endif.
        if ls_current_line_post-original_line_nr <> 0.
          insert ls_current_line_post-line into lt_upper_context_post index 1.
          lv_lines_collected_post += 1.
        endif.
        if ls_current_line_pre-original_line_nr <> 0 and ls_current_line_post-original_line_nr <> 0.
          lv_match_lines_collected += 1.
        endif.
        lv_current_index -= 1.

        if lv_match_lines_collected = mv_context_lines or lv_lines_collected_pre > 15.
          lv_work_done_indicator = abap_true.
        endif.
      endwhile.
      loop at lt_upper_context_pre assigning field-symbol(<ls_upper_context_pre>).
        if lv_upper_context_pre is not initial.
          concatenate lv_upper_context_pre <ls_upper_context_pre> into lv_upper_context_pre separated by lv_newline.
        else.
          concatenate lv_upper_context_pre <ls_upper_context_pre> into lv_upper_context_pre.
        endif.
      endloop.
      loop at lt_upper_context_post assigning field-symbol(<ls_upper_context_post>).
        if lv_upper_context_post is not initial.
          concatenate lv_upper_context_post <ls_upper_context_post> into lv_upper_context_post separated by lv_newline.
        else.
          concatenate lv_upper_context_post <ls_upper_context_post> into lv_upper_context_post.
        endif.
      endloop.
      if ls_code_snippet-before is not initial.
        concatenate lv_upper_context_pre ls_code_snippet-before into ls_code_snippet-before separated by lv_newline.
      else.
        concatenate lv_upper_context_pre ls_code_snippet-before into ls_code_snippet-before.
      endif.
      if ls_code_snippet-after is not initial.
        concatenate lv_upper_context_post ls_code_snippet-after into ls_code_snippet-after separated by lv_newline.
      else.
        concatenate lv_upper_context_post ls_code_snippet-after into ls_code_snippet-after.
      endif.

      finding_line_in_snippet = finding_line_in_snippet + lines( lt_upper_context_pre ).

      " Get lower context
      data lt_lower_context_pre type string_table.
      data lt_lower_context_post type string_table.
      data lv_lower_context_pre type string.
      data lv_lower_context_post type string.
      lv_match_lines_collected = 0.
      lv_lines_collected_pre = 0.
      lv_lines_collected_post = 0.
      lv_work_done_indicator = abap_false.
      lv_current_index = lv_context_lower_begin.
      while lv_work_done_indicator = abap_false.
        if lv_current_index > lines( lt_source_pre ).
          exit.
        endif.
        ls_current_line_pre = lt_source_pre[ lv_current_index ].
        ls_current_line_post = lt_source_post[ lv_current_index ].
        if ls_current_line_pre-original_line_nr <> 0.
          append ls_current_line_pre-line to lt_lower_context_pre.
          lv_lines_collected_pre += 1.
        endif.
        if ls_current_line_post-original_line_nr <> 0.
          append ls_current_line_post-line to lt_lower_context_post.
          lv_lines_collected_post += 1.
        endif.
        if ls_current_line_pre-original_line_nr <> 0 and ls_current_line_post-original_line_nr <> 0.
          lv_match_lines_collected += 1.
        endif.
        lv_current_index += 1.

        if lv_match_lines_collected = mv_context_lines or lv_lines_collected_pre > 15.
          lv_work_done_indicator = abap_true.
        endif.
      endwhile.
      loop at lt_lower_context_pre assigning field-symbol(<ls_lower_context_pre>).
        if lv_lower_context_pre is not initial.
          concatenate lv_lower_context_pre <ls_lower_context_pre>  into lv_lower_context_pre separated by lv_newline.
        else.
          concatenate lv_lower_context_pre <ls_lower_context_pre> into lv_lower_context_pre.
        endif.
      endloop.
      loop at lt_lower_context_post assigning field-symbol(<ls_lower_context_post>).
        if lv_lower_context_post is not initial.
          concatenate lv_lower_context_post <ls_lower_context_post> into lv_lower_context_post separated by lv_newline.
        else.
          concatenate lv_lower_context_post <ls_lower_context_post> into lv_lower_context_post.
        endif.
      endloop.
      if ls_code_snippet-before is not initial.
        concatenate ls_code_snippet-before lv_lower_context_pre into ls_code_snippet-before separated by lv_newline.
      else.
        concatenate ls_code_snippet-before lv_lower_context_pre into ls_code_snippet-before.
      endif.
      if ls_code_snippet-after is not initial.
        concatenate ls_code_snippet-after lv_lower_context_post into ls_code_snippet-after separated by lv_newline.
      else.
        concatenate ls_code_snippet-after lv_lower_context_post into ls_code_snippet-after.
      endif.

      " Insert snippet into returning table
      if ls_code_snippet-before is not initial or ls_code_snippet-after is not initial.
        ls_code_snippet-code = <ls_finding_detail>-code.
        ls_code_snippet-test = <ls_finding_detail>-test.
        ls_code_snippet-line = finding_line_in_snippet.
        ls_code_snippet-message = <ls_finding_detail>-message.
        ls_code_snippet-progname = is_finding_info-progname.
        ls_code_snippet-objname = ls_tadir-obj_name.
        ls_code_snippet-objtype = ls_tadir-object.
        insert ls_code_snippet into table rt_code_snippets.
      endif.

      clear ls_code_snippet.
      clear lt_upper_context_pre.
      clear lt_upper_context_post.
      clear lv_upper_context_pre.
      clear lv_upper_context_post.
      clear lt_lower_context_pre.
      clear lt_lower_context_post.
      clear lv_lower_context_pre.
      clear lv_lower_context_post.

    endloop.
    "-----------------------------------------------------------------------------------------------

  endmethod.

  method format_source_for_comparison.
    loop at it_source into data(ls_source).
      " Ignore empty lines, *-commented lines and "-commented lines, also remove trailing blanks
      if ls_source is initial.
        continue.
      endif.
      if ls_source(1) = '*'.
        continue.
      endif.
      shift ls_source left deleting leading ' '.
      if ls_source(1) = '"'.
        continue.
      endif.
      " Remove potential comments
      find first occurrence of '"' in ls_source.
      if sy-subrc = 0.
        find first occurrence of |'| in ls_source.
        if sy-subrc = 0.
          " No thanks
          exit.
        endif.
        find first occurrence of |\|| in ls_source.
        if sy-subrc = 0.
          " No thanks
          exit.
        endif.
        data(ls_source_copy) = ls_source.
        data(shift) = 0.
        while ls_source_copy(1) <> '"'.
          shift += 1.
          shift ls_source_copy left.
        endwhile.
        ls_source = ls_source(shift).
      endif.
      " Convert everything to lower case
      translate ls_source to lower case.
      insert value #( original_line_nr = sy-tabix
                      line             = ls_source ) into table rt_compare_source.
    endloop.
  endmethod.

  method get_finding_infos.

    data rfc_message type char120.
    data(lt_finding_info_pre) = get_finding_infos_single( mv_reference_run_id_a ).
    data(lt_finding_info_post) = get_finding_infos_single( mv_reference_run_id_b ).
    rt_finding_info = lt_finding_info_pre.

    " Remove all entries for findings that did not get fixed.
    " No fixed finding => no useful code snippet.

    field-symbols <ls_finding_info_post> like line of lt_finding_info_post.
    loop at rt_finding_info assigning field-symbol(<ls_finding_info_pre>).
      if <ls_finding_info_pre>-progname+30(2) = 'CM'.
        " It's a method. The method include in the remote and local system can have different names.
        data ls_method_key type seocpdkey.
        call function 'ZNB_GET_METHOD_NAME_OF_INCLUDE' destination mv_rfcdest
          exporting
            iv_progname           = <ls_finding_info_pre>-progname
          importing
            es_method_key         = ls_method_key
          exceptions
            include_not_found     = 1
            communication_failure = 2 message rfc_message
            system_failure        = 3 message rfc_message.
        if sy-subrc <> 0 or ls_method_key is initial.
          delete rt_finding_info.
          continue.
        endif.
        data method_include type program.
        cl_oo_classname_service=>get_method_include(
          exporting
            mtdkey              = ls_method_key
          receiving
            result              = method_include
          exceptions
            class_not_existing  = 1
            method_not_existing = 2
            others              = 3 ).
        if sy-subrc <> 0.
          continue.
        endif.
        read table lt_finding_info_post with key progname = method_include assigning <ls_finding_info_post>.
      else.
        read table lt_finding_info_post with key progname = <ls_finding_info_pre>-progname assigning <ls_finding_info_post>.
      endif.
      if sy-subrc = 0.
        loop at <ls_finding_info_pre>-finding_details assigning field-symbol(<ls_finding_detail>).
          read table <ls_finding_info_post>-finding_details with key message = <ls_finding_detail>-message code = <ls_finding_detail>-code checksum = <ls_finding_detail>-checksum transporting no fields.
          if sy-subrc = 0.
            " Finding still exists post migration
            delete <ls_finding_info_pre>-finding_details.
          endif.
        endloop.
        " Delete empty entries
        if <ls_finding_info_pre>-finding_details is initial.
          delete rt_finding_info.
        endif.
      endif.
    endloop.

  endmethod.

endclass.
