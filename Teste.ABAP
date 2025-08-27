*&---------------------------------------------------------------------*
*& Report ZTESTE
*&---------------------------------------------------------------------*
REPORT zteste.

DATA: it_openitems TYPE TABLE OF bapi3008_2 WITH HEADER LINE,
      ls_openitem  TYPE bapi3008_2,
      lv_json      TYPE string,
      it_return    TYPE bapireturn.

"--------------------------------------------------
" obter os open items do fornecedor
CALL FUNCTION 'BAPI_AP_ACC_GETOPENITEMS'
  EXPORTING
    companycode = 'PT02'
    vendor      = '0000100006'
    keydate     = '20250827'
  IMPORTING
    return      = it_return
  TABLES
    lineitems   = it_openitems.

" converter para JSON
CALL METHOD /ui2/cl_json=>serialize
  EXPORTING
    data   = it_openitems
  RECEIVING
    r_json = lv_json.

*cl_demo_output=>display_json( lv_json ).

"--------------------------------------------------
" definir variáveis para mensagens
DATA: e_msgid TYPE sy-msgid,
      e_msgno TYPE sy-msgno,
      e_msgty TYPE sy-msgty,
      e_msgv1 TYPE sy-msgv1,
      e_msgv2 TYPE sy-msgv2,
      e_msgv3 TYPE sy-msgv3,
      e_msgv4 TYPE sy-msgv4,
      e_subrc TYPE sy-subrc.

" definir tabelas internas
DATA: t_blntab  TYPE STANDARD TABLE OF blntab,
      t_ftclear TYPE STANDARD TABLE OF ftclear,
      t_ftpost  TYPE STANDARD TABLE OF ftpost,
      t_fttax   TYPE STANDARD TABLE OF fttax.

DATA: ls_blntab  TYPE blntab,
      ls_ftclear TYPE ftclear,
      ls_ftpost  TYPE ftpost,
      ls_fttax   TYPE fttax.

"--------------------------------------------------
" iniciar a interface de posting
CALL FUNCTION 'POSTING_INTERFACE_START'
  EXPORTING
    i_client           = sy-mandt
    i_function         = 'C'
    i_mode             = 'N'     " sem tela
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

CLEAR ls_ftpost.
ls_ftclear-agkoa  = 'K'.
ls_ftclear-xnops  = 'X'.
ls_ftclear-agkon  = '0000100006'.
ls_ftclear-agbuk  = 'PT02'.
ls_ftclear-selfd  = 'BELNR'.
ls_ftclear-selvon = '0001900076'.
ls_ftclear-selbis = '0001900076'.
WRITE '2024' TO ls_ftclear-selvon+10.
WRITE '2024' TO ls_ftclear-selbis+10.
APPEND ls_ftclear TO t_ftclear.

"--------------------------------------------------
" Item (compensação)
CLEAR: t_ftpost.
REFRESH: t_ftpost.

CLEAR: ls_ftpost.
ls_ftpost-stype = 'K'."Header
ls_ftpost-count = 1.  "number of Dynpro

ls_ftpost-fnam = 'BKPF-BLDAT'.
CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO ls_ftpost-fval SEPARATED BY '.'.
APPEND ls_ftpost TO t_ftpost.

ls_ftpost-fnam = 'BKPF-BUDAT'.
ls_ftpost-fval = '11.10.2025'.
APPEND ls_ftpost TO t_ftpost.

ls_ftpost-fnam = 'BKPF-BLART'.
ls_ftpost-fval = 'KR'.         "Same type as documents cleared via F-32
APPEND ls_ftpost TO t_ftpost.

ls_ftpost-fnam = 'BKPF-MONAT'.
ls_ftpost-fval = '08'.
APPEND ls_ftpost TO t_ftpost.

ls_ftpost-fnam = 'BKPF-BUKRS'.
ls_ftpost-fval = 'PT02'.
APPEND ls_ftpost TO t_ftpost.

ls_ftpost-fnam = 'BKPF-WAERS'.
ls_ftpost-fval = 'EUR'.
APPEND ls_ftpost TO t_ftpost.

*ls_ftpost-fnam = 'BKPF-BKTXT'.
*ls_ftpost-fval = 'RAFAEL TESTE'.
*APPEND ls_ftpost TO t_ftpost.
*
*ls_ftpost-fnam = 'BKPF-XBLNR'.
*ls_ftpost-fval = 'REF1'.
*APPEND ls_ftpost TO t_ftpost.

" Item (transferência para fornecedor)
CLEAR ls_ftpost.
ls_ftpost-stype = 'P'.
ls_ftpost-count = 2.
ls_ftpost-fnam  = 'RF05A-NEWBS'.
ls_ftpost-fval  = '31'.
APPEND ls_ftpost TO t_ftpost.

ls_ftpost-count = 2.
ls_ftpost-fnam  = 'RF05A-NEWKO'.   " Vendor code
ls_ftpost-fval  = '0000100006'.
APPEND ls_ftpost TO t_ftpost.

ls_ftpost-count = 2.
ls_ftpost-fnam  = 'BSEG-WRBTR'.    " Amount
ls_ftpost-fval  = '50'.
APPEND ls_ftpost TO t_ftpost.

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

*" exibir mensagens
*WRITE: / 'MSGID:', e_msgid,
*       'MSGNO:', e_msgno,
*       'MSGTY:', e_msgty.
*WRITE: / 'MSGV1:', e_msgv1,
*       'MSGV2:', e_msgv2,
*       'MSGV3:', e_msgv3,
*       'MSGV4:', e_msgv4.
*WRITE: / 'SUBRC:', e_subrc.

IF sy-subrc <> 0.
  WRITE: / 'Erro ao finalizar a interface POSTING_INTERFACE_END'.
ENDIF.

"--------------------------------------------------
" finalizar a interface
CALL FUNCTION 'POSTING_INTERFACE_END'
  EXPORTING
    i_bdcimmed              = 'X'
  EXCEPTIONS
    session_not_processable = 1
    OTHERS                  = 2.

IF sy-subrc <> 0.
  WRITE: / 'Erro ao finalizar a interface POSTING_INTERFACE_END'.
ENDIF.

"--------------------------------------------------
" se tudo OK, confirmar
IF e_subrc = 0.
  COMMIT WORK.
ENDIF.
