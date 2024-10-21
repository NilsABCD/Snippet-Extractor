class zcl_nb_extractor_factory definition
  public
  final
  create public .

  public section.
    "! Creates an extractor that searches for the following pattern: <br><br>
    "! Expression indicating begin of change <br>
    "! * Code before change line 1 (commented out)<br>
    "! * Code before change line 2 (commented out)<br>
    "! * ...<br>
    "! * Code before change line n (commented out)<br>
    "! Code after change line 1 (productive)<br>
    "! Code after change line 2 (productive)<br>
    "! ...<br>
    "! Code after change line n (productive)<br>
    "! Expression indicating end of change <br>
    "!
    "! @parameter iv_begin_of_change_indicator    | RegEx matching begin of change indicator
    "! @parameter iv_end_of_change_indicator      | RegEx matching end of change indicator
    "! @parameter it_program_names                | List of program names for which extraction shall be executed
    "! @parameter iv_context_lines                | Number of context lines to be included in the extract
    class-methods create_extractor_type_a importing iv_begin_of_change_indicator type string
                                                    iv_end_of_change_indicator   type string
                                                    it_program_names             type zif_nb_extractor=>tt_progname
                                                    iv_context_lines             type i
                                          returning value(r_extractor)           type ref to zif_nb_extractor.
    "! Creates an extractor that compares two systems: <br><br>
    "! The code in system A represents the state pre migration. <br>
    "! The code in system B represents the state post migration. <br>
    "! Finding locations are located via reference ATC runs in both systems. <br>
    "!
    "! @parameter iv_reference_run_id_a           | Reference run for pre migration system (remote)
    "! @parameter iv_reference_run_id_b           | Reference run for post migration system (local)
    "! @parameter iv_rfcdest                      | RFC destination of pre migration system
    "! @parameter iv_context_lines                | Number of context lines to be included in the extract
    class-methods create_extractor_type_b importing iv_reference_run_id_a type satc_d_ac_display_id
                                                    iv_reference_run_id_b type satc_d_ac_display_id
                                                    iv_rfcdest            type rfcdest
                                                    iv_context_lines      type i
                                          returning value(r_extractor)    type ref to zif_nb_extractor.

endclass.



class zcl_nb_extractor_factory implementation.

  method create_extractor_type_a.
    r_extractor = new lcl_extractor_type_a( iv_begin_of_change_indicator = iv_begin_of_change_indicator
                                            iv_end_of_change_indicator   = iv_end_of_change_indicator
                                            it_program_names             = it_program_names
                                            iv_context_lines             = iv_context_lines ).
  endmethod.

  method create_extractor_type_b.
    r_extractor = new lcl_extractor_type_b( iv_reference_run_id_a = iv_reference_run_id_a
                                            iv_reference_run_id_b = iv_reference_run_id_b
                                            iv_rfcdest            = iv_rfcdest
                                            iv_context_lines      = iv_context_lines ).
  endmethod.

endclass.
