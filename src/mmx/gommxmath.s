	;; MMX and 3DNow math assembly routines
	;; Author:	 Christian Gosch
	;; Date:	 26.6.2001

global goASM3DNMult2f:function
	;; goASM3DNMult2f(float, float, float, float, &float[2])
goASM3DNMult2f:	
 	push ebx		
	movq mm0,[esp+8]	; Value A | B
;	movq mm1,[esp+16]	; Value C | D
	pfmul mm0,[esp+16]	; A*C, B*D
	mov ebx,[esp+24]
	movq [ebx],mm0	; Store result in pointer to float32[2]
 	pop ebx
	ret
	
	
	