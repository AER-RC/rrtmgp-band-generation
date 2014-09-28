pro bhohls
;
;   HLS color table
;   For use with idl and a set of color indicies from 0-11
;
;     INDEX    Color index          Range  0 - 11
;     HUE      COLOR HUE            RANGE  0 - 255  
;     LIGHT    COLOR LIGHTNESS      RANGE  0 - 1
;     SATUR    COLOR SATURATION     RANGE  0 - 1
;  
;

; HUE   = [ 0,  0, findgen(15)*18]
  HUE   = [ 0,  0, 0, 33, 54, 75, 108, 162, 180, 198, 236, 255]
  LIGHT = [ 0,100, replicate(50,10)]
  SATUR = [ 0,  0,replicate(100,9),60]

print, hue

tvlct, hue, light/100., satur/100.,/hls
end

