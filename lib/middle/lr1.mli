module Make (Grammar : Intf.GRAMMAR) :
  Intf.LR1 with module Grammar = Grammar
