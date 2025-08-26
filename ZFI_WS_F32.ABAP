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
*  type-pools: slis.
  DATA: l_auglv   TYPE t041a-auglv   VALUE 'UMBUCHNG', "Posting with Clearing
        l_tcode   TYPE sy-tcode      VALUE 'FB05',     "You get an error with any other value
        l_sgfunct TYPE rfipi-sgfunct VALUE 'C'.        "Post immediately


  DATA: lt_blntab  TYPE STANDARD TABLE OF blntab,
        lt_ftclear TYPE STANDARD TABLE OF ftclear,
        ls_ftclear LIKE LINE OF lt_ftclear,
        lt_ftpost  TYPE STANDARD TABLE OF ftpost,
        ls_ftpost  LIKE LINE OF lt_ftpost,
        lt_fttax   TYPE STANDARD TABLE OF fttax.

  DATA: valor   TYPE bseg-wrbtr,
        ireturn TYPE bapiret2,
        itab    TYPE STANDARD TABLE OF bapiret2,
        iret    LIKE LINE OF itab,
        ikoart  LIKE bseg-koart,
        doc_ad  LIKE bkpf-blart,
        ibldat  LIKE sy-datum, "bkpf-bldat,
        ibudat  LIKE bkpf-budat,
        iumskz  LIKE bseg-umskz,
        i       TYPE i,
        ai(2)   TYPE c,
        campo   TYPE string,
        modo(1) TYPE c.

  DATA: i_umskz TYPE bseg-umskz.

*  BAPIRETURN TYPE  BAPIRET2

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


  LOOP AT doc." into wa_doc.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = doc-belnr
      IMPORTING
        output = doc-belnr.

    MODIFY doc.
  ENDLOOP.

*1ª saber se há valores q ficam por compensar
*H  Crédito
*S  Débito

  LOOP AT doc.
    SELECT * FROM bseg INTO bseg
     WHERE belnr EQ doc-belnr AND bukrs EQ bukrs
      AND gjahr EQ doc-gjahr AND ( koart EQ 'D' OR koart = 'K' ).
      IF bseg-shkzg EQ 'S'.

        valor = valor - bseg-wrbtr.
      ELSEIF bseg-shkzg EQ 'H'.
        valor = valor + bseg-wrbtr.
      ENDIF.
      IF bseg-augbl IS NOT INITIAL.
*       bapireturn-type = 'E'.
      CONCATENATE 'Documento' bseg-belnr bseg-gjahr bukrs 'já se encontra compensado'
              INTO message SEPARATED BY space.
      RETURN.
    ENDIF.
    ENDSELECT.
  ENDLOOP.


    "validar se há doc tipo AD
    LOOP AT doc.
      SELECT SINGLE blart FROM bkpf INTO doc_ad
          WHERE belnr EQ doc-belnr AND bukrs EQ bukrs
      AND gjahr EQ doc-gjahr AND blart = 'AD'.
      IF sy-subrc EQ '0'.
        SELECT SINGLE umskz INTO iumskz FROM bseg
          WHERE belnr EQ doc-belnr AND bukrs EQ bukrs
    AND gjahr EQ doc-gjahr AND ( koart EQ 'D' OR koart = 'K' ).
      ENDIF.
    ENDLOOP.
    CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum+2(2) INTO ibldat SEPARATED BY '.'.
    CONCATENATE budat+6(2) budat+4(2) budat+2(2) INTO ibudat SEPARATED BY '.'.

 IF valor > 0 and doc_ad is not initial.
*    IF doc_ad IS NOT INITIAL.  "fazer compensação por bi

      CLEAR dynprotab. REFRESH dynprotab.
      PERFORM carrega_dynprotab USING: 'SAPMF05A'   '0122'  'X',

                                       'BDC_OKCODE' 'SL'   ' ',
                                       'BKPF-BLDAT'  ibldat ' ',
                                       'BKPF-BLART'  'AB'   ' ',
                                       'BKPF-BUKRS'  bukrs  ' ',
                                       'BKPF-BUDAT'  ibudat ' ',
                                       'BKPF-MONAT'  monat  ' ',
                                       'BKPF-WAERS'  waers  ' ',


                                      'SAPMF05A' '0710'  'X',
                                      'BDC_OKCODE'  '=PA' ' ',
                                      'RF05A-AGBUK' bukrs ' ',
                                      'RF05A-AGKON' agkon ' ',
                                      'RF05A-AGUMS' iumskz ' ',
                                      'RF05A-XPOS1(07)' 'X' ' '.



      PERFORM carrega_dynprotab USING:  'SAPMF05A' '0731'  'X',
                                        'BDC_OKCODE'  '=PA' ' '.
      LOOP AT doc.
        i = i + 1.
        ai = i.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ai
          IMPORTING
            output = ai.

        CONCATENATE 'RF05A-SEL01(' ai ')'  INTO campo.
        PERFORM carrega_dynprotab USING: campo doc-belnr ' '.
      ENDLOOP.

      campo = valor.
      TRANSLATE campo USING '.,'.
      PERFORM carrega_dynprotab USING:  'SAPDF05X' '3100'  'X',
                                        'BDC_OKCODE'  '=PI' ' ',
                                        'BDC_CURSOR' 'RF05A-AKOBT' ' ',
                                        'RF05A-ABPOS' '1' ' ',

                                        'SAPDF05X' '3100'  'X',
                                        'RF05A-ABPOS' '1' ' ',
                                        'RF05A-AKOBT' campo ' ',
                                        'BDC_OKCODE'  '=BU' ' ',

                                        'SAPMF05A' '0700'  'X',
                                        'BDC_CURSOR' 'RF05A-AZEI1(01)' ' ',
                                        'BDC_OKCODE'  '=PI' ' ',

                                        'SAPMF05A' '0301'  'X',
                                        'BSEG-ZUONR' '.' ' ',
                                        'BDC_OKCODE'  '=BU' ' '.

      IF modo IS INITIAL. modo = 'N'. ENDIF.
      CALL TRANSACTION 'FB05' USING dynprotab MODE modo

     MESSAGES INTO msg .
      CLEAR dynprotab. REFRESH dynprotab.
      LOOP AT msg WHERE msgtyp EQ 'S' OR msgtyp EQ 'E'.
        MOVE msg-msgv1 TO message.
        IF msg-msgnr EQ '312' AND msg-msgtyp EQ 'S'.
          MOVE msg-msgv1 TO belnr.
          CONCATENATE 'O documento' belnr 'foi registado na empresa' bukrs INTO message SEPARATED BY space.
        ENDIF.
      ENDLOOP.
*    ENDIF.


  ELSEIF  valor >= '-0.99' and valor <= '0.99'.

    CONCATENATE budat+6(2) budat+4(2) budat+2(2) INTO ibudat SEPARATED BY '.'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = agkon
    IMPORTING
      output = agkon.
    CLEAR dynprotab. REFRESH dynprotab.
    PERFORM carrega_dynprotab USING: 'SAPMF05A'   '0131'  'X',

                                     'BDC_OKCODE' '=PA'   ' ',
                                     'RF05A-AGKON' agkon ' ',
*                                       'BKPF-BLART'  'AB'   ' ',
                                     'BKPF-BUKRS'  bukrs  ' ',
                                     'BKPF-BUDAT'  ibudat ' ',
                                     'BKPF-MONAT'  monat  ' ',
                                     'BKPF-WAERS'  waers  ' ',
                                     'RF05A-XNOPS' 'X' ' ',
                                     'RF05A-XPOS1(07)' 'X' ' '.



    PERFORM carrega_dynprotab USING:  'SAPMF05A' '0731'  'X',
                                      'BDC_OKCODE'  '=PA' ' '.
    LOOP AT doc.
      i = i + 1.
      ai = i.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = ai
        IMPORTING
          output = ai.

      CONCATENATE 'RF05A-SEL01(' ai ')'  INTO campo.
      PERFORM carrega_dynprotab USING: campo doc-belnr ' '.
    ENDLOOP.
    valor = valor * -1.
    campo = valor.
    TRANSLATE campo USING '.,'.

    PERFORM carrega_dynprotab USING:   'SAPDF05X'  '3100'  'X',

                                       'BDC_OKCODE'  '=KMD' ' ',
                                       'RF05A-ABPOS'  '1' ' ',

                                       'SAPMF05A'  '0700'  'X',
                                       'BDC_OKCODE'  '/00' ' ',
                                       'RF05A-NEWBS'  '40' ' ',
                                       'RF05A-NEWKO'  '2782000005' ' ',

                                       'SAPMF05A'  '0300'  'X',
                                       'BDC_OKCODE'  '=BS' ' ',
                                       'BSEG-WRBTR'  campo ' ',

                                      'SAPLKACB'  '0002' 'X',
                                       'BDC_OKCODE' '=ENTE' ' ',
                                      'SAPMF05A' '0700'  'X' ,
                                      'BDC_OKCODE'  '=BU' ' '.

    IF modo IS INITIAL. modo = 'N'. ENDIF.
    CALL TRANSACTION 'F-32' USING dynprotab MODE modo
   MESSAGES INTO msg .
    CLEAR dynprotab. REFRESH dynprotab.
    LOOP AT msg WHERE msgtyp EQ 'S' OR msgtyp EQ 'E'.
      MOVE msg-msgv1 TO message.
      IF msg-msgnr EQ '312' AND msg-msgtyp EQ 'S'.
        MOVE msg-msgv1 TO belnr.
        CONCATENATE 'O documento' belnr 'foi registado na empresa' bukrs INTO message SEPARATED BY space.
      ENDIF.
    ENDLOOP.

  ELSE.

    IF modo IS INITIAL. modo = 'N'. ENDIF.
    CALL FUNCTION 'POSTING_INTERFACE_START'
      EXPORTING
        i_client           = sy-mandt
        i_function         = 'C'
*       i_group            = ' '
*       i_holddate         = ' '
*       i_keep             = ' '
        i_mode             = modo "'N'
        i_update           = 'S'
        i_user             = sy-uname
*       i_xbdcc            = ' '
      EXCEPTIONS
        client_incorrect   = 1
        function_invalid   = 2
        group_name_missing = 3
        mode_invalid       = 4
        update_invalid     = 5
        OTHERS             = 6.


    IF sy-subrc <> 0.
      EXIT.
    ENDIF.


*  batch input values
    CLEAR: lt_ftpost.
    REFRESH: lt_ftpost.

    CLEAR: ls_ftpost.
    ls_ftpost-stype = 'K'."Header
    ls_ftpost-count = 1.  "number of Dynpro

    ls_ftpost-fnam = 'BKPF-BLDAT'.
    CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO ls_ftpost-fval SEPARATED BY '.'.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam = 'BKPF-BUDAT'.
    CONCATENATE budat+6(2) budat+4(2) budat(4) INTO ls_ftpost-fval SEPARATED BY '.'.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam = 'BKPF-BLART'.
    ls_ftpost-fval = 'AB'.         "Same type as documents cleared via F-32
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam = 'BKPF-MONAT'.
    ls_ftpost-fval = monat.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam = 'BKPF-BUKRS'.
    ls_ftpost-fval = bukrs.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam = 'BKPF-WAERS'.
    ls_ftpost-fval = waers.
    APPEND ls_ftpost TO lt_ftpost.


*  documents to be cleared
    CLEAR: lt_ftclear, ls_ftclear.
    REFRESH:lt_ftclear.

*  CLEAR: ls_ftclear.

    LOOP AT doc.
      CLEAR i_umskz.
      SELECT SINGLE umskz INTO i_umskz FROM bseg
        WHERE bukrs EQ bukrs AND belnr EQ doc-belnr AND gjahr EQ doc-gjahr
        AND umskz <> ''.
      IF sy-subrc EQ '0'. "razao especial
        ls_ftclear-agums = i_umskz.
      ENDIF.
      ls_ftclear-agkoa = ikoart.  "Account Type - D -Client K - Provider
      ls_ftclear-xnops = 'X'.     "Indicator: Select only open items which are not special G/L?
      ls_ftclear-agbuk = bukrs.   "Example company code
      ls_ftclear-agkon = agkon.   "Filtro pela conta
      ls_ftclear-selfd = 'BELNR'. "Selection Field
      ls_ftclear-selvon = doc-belnr.
      ls_ftclear-selbis = doc-belnr.

      "Considerar o ano do documento
      WRITE doc-gjahr(4) TO ls_ftclear-selvon+10.
      WRITE doc-gjahr(4) TO ls_ftclear-selbis+10.


      APPEND ls_ftclear TO lt_ftclear.
    ENDLOOP.


    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv                    = l_auglv
        i_tcode                    = l_tcode
        i_sgfunct                  = l_sgfunct
        i_no_auth                  = ' '
      TABLES
        t_blntab                   = lt_blntab
        t_ftclear                  = lt_ftclear
        t_ftpost                   = lt_ftpost
        t_fttax                    = lt_fttax
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


    IF sy-msgty EQ 'S'.
      belnr = sy-msgv1.
    ENDIF.
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = sy-msgty
        cl     = sy-msgid
        number = sy-msgno
      IMPORTING
        return = ireturn.

    MOVE: ireturn-message_v1 TO sy-msgv1,
*        ireturn-type TO bapireturn-type,
*        ireturn-id TO bapireturn-id,
*        ireturn-number TO bapireturn-number,
          ireturn-message TO message.
    IF ireturn-type EQ 'S'.
      CONCATENATE message '-' belnr INTO message.
*     move belnr to message.
    ENDIF.

    CALL FUNCTION 'POSTING_INTERFACE_END'
      EXPORTING
        i_bdcimmed              = 'X'
*       i_bdcstrtdt             = no_date
*       i_bdcstrttm             = no_time
      EXCEPTIONS
        session_not_processable = 1
        OTHERS                  = 2.



    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDIF.
ENDFUNCTION.
