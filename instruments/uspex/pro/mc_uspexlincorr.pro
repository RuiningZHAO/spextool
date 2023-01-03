pro mc_uspexlincorr, infile, outfile, LOLIM=lolim, UPLIM=uplim, $
                     NOAMPCOR=noampcor

forward_function myfunct

bias    = '../data/uSpeX_bias.fits'
coefimg = '../data/uSpeX_lincorr.fits'
coefarr = readfits(coefimg)

; Read bias frame

biasdat = float(readfits(bias, hb))
divisor = fxpar(hb,'DIVISOR')
biasdat = temporary(biasdat)/divisor
if keyword_set(NOAMPCOR) then biascor = biasdat else $
   biascor = mc_uspexampcor(biasdat)

sigarr  = float(readfits(infile,h,EXT=1))
pedarr  = float(readfits(infile,h,EXT=2))
divisor = fxpar(h,'DIVISOR')

sigarr  = temporary(sigarr)/divisor 
pedarr  = temporary(pedarr)/divisor 

s       = size(sigarr)
nxpix   = s[1]
nypix   = s[2]

corrimg = fltarr(nxpix,nypix,3)

; Not currently used but can be used to identify bad pixels and set flags/warnings

if not(keyword_set(LOLIM)) then lolim = 0.0
if not(keyword_set(UPLIM)) then uplim = 40000.0

; Input image planes have been corrected for reference pixel drifts with mc_uspexampcor
; and then subtracted from the corrected bias frame. No values should be less than zero.

if keyword_set(NOAMPCOR) then begin
   sigcorr = biascor - sigarr
   pedcorr = biascor - pedarr
endif else begin 
   sigcorr = biascor - mc_uspexampcor(sigarr)
   pedcorr = biascor - mc_uspexampcor(pedarr)
endelse

ximgtool,sigcorr,pedcorr
return


siglccorr = sigcorr
pedlccorr = pedcorr

for j=4,nypix-5 do begin
    for i=4,nxpix-5 do begin
        if pedcorr[i,j] gt 0.0 then pedlccorr[i,j] = pedcorr[i,j]/myfunct(pedcorr[i,j],coefarr[i,j,*]) else pedlccorr[i,j] = 0.0
        if sigcorr[i,j] gt 0.0 then siglccorr[i,j] = sigcorr[i,j]/myfunct(sigcorr[i,j],coefarr[i,j,*]) else siglccorr[i,j] = 0.0
    endfor
endfor

corrimg[*,*,2] = pedlccorr*divisor
corrimg[*,*,1] = siglccorr*divisor
corrimg[*,*,0] = siglccorr - pedlccorr

fxaddpar, h, 'Corrected for non-linearity using coefs in',coefimg

writefits, outfile, corrimg, h

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function myfunct, x, p

scale = 1.0e4
if x le 0.0 then x=0.0 

xs    = x/scale
corr  = (tanh(xs/p[0]))*poly(xs,p[1:*])

return, corr
end
