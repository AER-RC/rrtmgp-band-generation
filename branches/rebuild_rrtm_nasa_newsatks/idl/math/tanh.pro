;+
; NAME:
;    TANH
;
; PURPOSE:
;    Calculates the hyperbolic tan function (tanh).
;
; CATEGORY:
;    Math
;
; CALLING SEQUENCE:
;    Result = TANH(X)
;
; INPUTS:
;    X:   Argument to tanh (may be an array).
;
; OUTPUTS:
;    sinh(X) / cosh(X)
;
; EXAMPLE:
;    IDL> PRINT, TANH(1.)
;         0.761594
;
; MODIFICATION HISTORY:
;    Written by:    Jeremy Bailin
;    12 June 2008   Public release in JBIU
;-
function tanh, x

return, sinh(x)/cosh(x)

end

