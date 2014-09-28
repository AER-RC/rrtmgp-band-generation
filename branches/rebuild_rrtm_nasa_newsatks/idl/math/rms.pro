;+
; NAME:
;    RMS
;
; PURPOSE:
;    Calculates the root-mean-square of a set of values.
;
; CATEGORY:
;    Math
;
; CALLING SEQUENCE:
;    Result = RMS(X)
;
; INPUTS:
;    X:  Array of values.
;
; OUTPUTS:
;    Calculates the square root of the mean of the squares of X.
;
; EXAMPLE:
;    x = 4. * RANDOMN(seed, 10000)
;    rmsx = RMS(x)
;
; MODIFICATION HISTORY:
;    Written by:   Jeremy Bailin
;    12 June 2008  Public release in JBIU
;-
function rms, X

return, sqrt(mean(X^2))

end

