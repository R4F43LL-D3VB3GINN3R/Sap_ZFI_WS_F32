FUNCTION zfi_ws_f32 .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(BUKRS) TYPE  BKPF-BUKRS
*"     VALUE(MONAT) TYPE  BKPF-MONAT
*"     VALUE(AGKON) TYPE  AGKON
*"     VALUE(BUDAT) TYPE  BKPF-BUDAT
*"     VALUE(WAERS) TYPE  BKPF-WAERS
*"  EXPORTING
*"     VALUE(BELNR) TYPE  BKPF-BELNR
*"     VALUE(MESSAGE) TYPE  BAPI_MSG
*"  TABLES
*"      DOC STRUCTURE  ZDOC_FI
*"----------------------------------------------------------------------
  TABLES: bseg, bkpf.

  DATA: it_document TYPE ztt_documents,
        ls_document TYPE zst_documents.

  TYPES: BEGIN OF ty_wsdl,
           bukrs TYPE bkpf-bukrs,
           monat TYPE bkpf-monat,
           agkon TYPE agkon,
           budat TYPE budat,
           waers TYPE bkpf-waers,
         END OF ty_wsdl.
  DATA: ls_wsdl TYPE ty_wsdl.

  "------------------------------------------------------
  "             RECEBE OS DADOS DO WSDL
  "------------------------------------------------------

  "corpo do wsdl
  ls_wsdl-bukrs = bukrs.
  ls_wsdl-monat = monat.
  ls_wsdl-agkon = agkon.
  ls_wsdl-budat = budat.
  ls_wsdl-waers = waers.

  "items do documento do wsdl
  LOOP AT doc.
    ls_document-belnr = doc-belnr.
    ls_document-buzei = doc-buzei.
    ls_document-gjahr = doc-gjahr.
    APPEND ls_document TO it_document.
  ENDLOOP.

  "------------------------------------------------------
  "        CONVERSÃO PARA DADOS DE ENTRADA
  "------------------------------------------------------

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = monat
    IMPORTING
      output = monat.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = agkon
    IMPORTING
      output = agkon.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = bukrs
    IMPORTING
      output = bukrs.

  LOOP AT it_document INTO ls_document.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_document-belnr
      IMPORTING
        output = ls_document-belnr.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = ls_document-buzei
      IMPORTING
        output = ls_document-buzei.
    MODIFY it_document FROM ls_document.
  ENDLOOP.

  "--------------------------------------------------------
  "  001 - VALIDAÇÃO PARA PROIBIÇÃO DE VALORES A COMPENSAR
  "  002 - RECEBIMENTO DE VALORES A SEREM COMPENSADOS E CHAVE DE LANÇAMENTO
  "--------------------------------------------------------
  LOOP AT it_document INTO ls_document.

    SELECT SINGLE *
    FROM bseg INTO @DATA(ls_temp_bseg)
    WHERE belnr EQ @ls_document-belnr
    AND   buzei EQ @ls_document-buzei
    AND   gjahr EQ @ls_document-gjahr
    AND   bukrs EQ @ls_wsdl-bukrs
    AND ( koart EQ 'D' OR koart = 'K' ).

    "002
    IF sy-subrc EQ 0.
      CASE ls_temp_bseg-shkzg.
        WHEN 'S'.
          ls_document-dmbtr = ls_temp_bseg-wrbtr * 1.
        WHEN 'H'.
          ls_document-dmbtr = ls_temp_bseg-wrbtr.
      ENDCASE.

      ls_document-hkont = ls_temp_bseg-hkont.
      ls_document-bschl = ls_temp_bseg-bschl.
      MODIFY it_document FROM ls_document.
    ENDIF.

    "001
    IF ls_temp_bseg-augbl IS NOT INITIAL.
      message = |Documento { ls_document-belnr } { ls_document-gjahr } { ls_wsdl-bukrs } já se encontra compensado|.
      RETURN.
    ENDIF.

  ENDLOOP.

  "------------------------------------------------------------
  "  003 - VALIDAÇÃO PARA EXISTÊNCIA DE DOCUMENTOS DO TIPO AD
  "------------------------------------------------------------
  LOOP AT it_document INTO ls_document.

    SELECT SINGLE *
    FROM bkpf
    INTO @DATA(ls_temp_bkpf)
    WHERE belnr EQ @ls_document-belnr
    AND   bukrs EQ @ls_wsdl-bukrs
    AND   gjahr EQ @ls_document-gjahr.

    IF sy-subrc EQ 0.

      ls_document-xblnr = ls_temp_bkpf-xblnr.
      ls_document-bldat = ls_temp_bkpf-bldat.
      ls_document-budat = ls_temp_bkpf-budat.
      ls_document-waers = ls_temp_bkpf-waers.
      ls_document-blart = ls_temp_bkpf-blart.

      IF ls_document-waers NE ls_wsdl-waers.
        message = |Documento { ls_document-belnr } { ls_document-gjahr } { ls_wsdl-bukrs } moeda inválida |.
        RETURN.
      ENDIF.

      IF ls_temp_bkpf-blart EQ 'AD'.

        SELECT SINGLE umskz
        FROM bseg
        INTO @DATA(lv_temp_umskz)
        WHERE belnr EQ @ls_document-belnr
        AND buzei   EQ @ls_document-buzei
        AND bukrs   EQ @ls_wsdl-bukrs
        AND gjahr   EQ @ls_document-gjahr
        AND ( koart EQ 'D' OR koart = 'K' ).

        IF sy-subrc EQ 0.
          ls_document-umskz = lv_temp_umskz.
        ENDIF.
      ENDIF.

      MODIFY it_document FROM ls_document.
    ENDIF.
  ENDLOOP.

  "------------------------------------------------------------
  "         004 - COMPENSAÇÃO DE FORNECEDORES
  "------------------------------------------------------------

  "tabelas internas para bapi
  DATA: t_blntab  TYPE STANDARD TABLE OF blntab,
        t_ftclear TYPE STANDARD TABLE OF ftclear,
        t_ftpost  TYPE STANDARD TABLE OF ftpost,
        t_fttax   TYPE STANDARD TABLE OF fttax.

  DATA: ls_blntab  TYPE blntab,
        ls_ftclear TYPE ftclear,
        ls_ftpost  TYPE ftpost,
        ls_fttax   TYPE fttax.

  " definir variáveis para mensagens
  DATA: e_msgid TYPE sy-msgid,
        e_msgno TYPE sy-msgno,
        e_msgty TYPE sy-msgty,
        e_msgv1 TYPE sy-msgv1,
        e_msgv2 TYPE sy-msgv2,
        e_msgv3 TYPE sy-msgv3,
        e_msgv4 TYPE sy-msgv4,
        e_subrc TYPE sy-subrc.

  "--------------------------------------------------
  " inicia a interface de posting

  CALL FUNCTION 'POSTING_INTERFACE_START'
    EXPORTING
      i_client           = sy-mandt
      i_function         = 'C'
      i_mode             = 'N'
      i_user             = sy-uname
    EXCEPTIONS
      client_incorrect   = 1
      function_invalid   = 2
      group_name_missing = 3
      mode_invalid       = 4
      update_invalid     = 5
      OTHERS             = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "--------------------------------------------------
  " preenchimento para estrutura de fornecedores

  SORT it_document BY belnr buzei ASCENDING.
  DATA: lv_last_belnr TYPE belnr_d.
  DATA: doc_itemcount TYPE i VALUE 1.

  LOOP AT it_document INTO ls_document.

    IF lv_last_belnr EQ ls_document-belnr.
      CONTINUE.
    ENDIF.

    CLEAR ls_ftpost.
    ls_ftclear-agkoa  = 'K'.
    ls_ftclear-xnops  = 'X'.
    ls_ftclear-agkon  = ls_wsdl-agkon.
    ls_ftclear-agbuk  = ls_wsdl-bukrs.
    ls_ftclear-selfd  = 'BELNR'.
    ls_ftclear-selvon = ls_document-belnr.
    ls_ftclear-selbis = ls_document-belnr.
    APPEND ls_ftclear TO t_ftclear.

    "--------------------------------------------------
    " documento (compensação)

    CLEAR: ls_ftpost.
    ls_ftpost-stype = 'K'."Header
    ls_ftpost-count = doc_itemcount.  "number of Dynpro
    ADD 1 TO doc_itemcount.

    IF doc_itemcount LT lines( it_document ) - 1.

      ls_ftpost-fnam = 'BKPF-BUDAT'.
      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO ls_ftpost-fval SEPARATED BY '.'.
      APPEND ls_ftpost TO t_ftpost.

      ls_ftpost-fnam = 'BKPF-BLDAT'.
      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO ls_ftpost-fval SEPARATED BY '.'.
      APPEND ls_ftpost TO t_ftpost.

      ls_ftpost-fnam = 'BKPF-BLART'.
      ls_ftpost-fval = 'AB'.
      APPEND ls_ftpost TO t_ftpost.

      ls_ftpost-fnam = 'BKPF-MONAT'.
      ls_ftpost-fval = ls_wsdl-monat.
      APPEND ls_ftpost TO t_ftpost.

      ls_ftpost-fnam = 'BKPF-BUKRS'.
      ls_ftpost-fval = ls_wsdl-bukrs.
      APPEND ls_ftpost TO t_ftpost.

      ls_ftpost-fnam = 'BKPF-WAERS'.
      ls_ftpost-fval = ls_document-waers.
      APPEND ls_ftpost TO t_ftpost.

*      ls_ftpost-fnam  = 'BDC_OKCODE'.
*      ls_ftpost-fval  = '/00'.
*      APPEND ls_ftpost TO t_ftpost.

    ENDIF.

    "--------------------------------------------------
    LOOP AT it_document INTO DATA(ls_document2) WHERE belnr EQ ls_document-belnr.

      CLEAR ls_ftpost.
      ls_ftpost-stype = 'P'.
      ls_ftpost-count = doc_itemcount.
      ls_ftpost-fnam  = 'RF05A-NEWBS'.
      ls_ftpost-fval  = ls_document-bschl.
      APPEND ls_ftpost TO t_ftpost.

      ls_ftpost-count = doc_itemcount.
      ls_ftpost-fnam  = 'RF05A-NEWKO'.   " Vendor code
      ls_ftpost-fval  = ls_wsdl-agkon.
      APPEND ls_ftpost TO t_ftpost.

      DATA lv_wrbtr_char TYPE string.
      lv_wrbtr_char = ls_document-dmbtr.
      TRANSLATE lv_wrbtr_char USING '.,'.
      CONDENSE lv_wrbtr_char NO-GAPS.

      ls_ftpost-count = doc_itemcount.
      ls_ftpost-fnam  = 'BSEG-WRBTR'.
      ls_ftpost-fval  = lv_wrbtr_char.
      APPEND ls_ftpost TO t_ftpost.

      ls_ftpost-count = doc_itemcount.
      ls_ftpost-fnam  = 'BSEG-ZFBDT'.    " Amount
      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO ls_ftpost-fval SEPARATED BY '.'.
      APPEND ls_ftpost TO t_ftpost.

*      ls_ftpost-count = doc_itemcount.
*      ls_ftpost-fnam  = 'BDC_OKCODE'.
*      ls_ftpost-fval  = '/00'.
*      APPEND ls_ftpost TO t_ftpost.

    ENDLOOP.

    ADD 1 TO doc_itemcount.
    lv_last_belnr = ls_document-belnr.

  ENDLOOP.

  "--------------------------------------------------
  " chamar interface de clearing
  CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
    EXPORTING
      i_auglv                    = 'UMBUCHNG'  " transfer posting with clearing
      i_tcode                    = 'FB05'
      i_sgfunct                  = 'C'
    IMPORTING
      e_msgid                    = e_msgid
      e_msgno                    = e_msgno
      e_msgty                    = e_msgty
      e_msgv1                    = e_msgv1
      e_msgv2                    = e_msgv2
      e_msgv3                    = e_msgv3
      e_msgv4                    = e_msgv4
      e_subrc                    = e_subrc
    TABLES
      t_blntab                   = t_blntab
      t_ftclear                  = t_ftclear
      t_ftpost                   = t_ftpost
      t_fttax                    = t_fttax
    EXCEPTIONS
      clearing_procedure_invalid = 1
      clearing_procedure_missing = 2
      table_t041a_empty          = 3
      transaction_code_invalid   = 4
      amount_format_error        = 5
      too_many_line_items        = 6
      company_code_invalid       = 7
      screen_not_found           = 8
      no_authorization           = 9
      OTHERS                     = 10.

  IF sy-subrc <> 0.
    message =  'Erro ao finalizar a interface POSTING_INTERFACE_END'.
  ENDIF.

  " finalizar a interface
  CALL FUNCTION 'POSTING_INTERFACE_END'
    EXPORTING
      i_bdcimmed              = 'X'
    EXCEPTIONS
      session_not_processable = 1
      OTHERS                  = 2.

  IF sy-subrc <> 0.
    message = 'Erro ao finalizar a interface POSTING_INTERFACE_END'.
  ENDIF.

  IF e_subrc = 0.
    COMMIT WORK.
  ELSE.
    DATA lv_message TYPE string.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = e_msgid
        msgnr               = e_msgno
        msgv1               = e_msgv1
        msgv2               = e_msgv2
        msgv3               = e_msgv3
        msgv4               = e_msgv4
      IMPORTING
        message_text_output = lv_message.

    message = lv_message.

  ENDIF.

ENDFUNCTION.
