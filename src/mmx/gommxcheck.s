	;; MMX assembly routines
	;; Author:	 Christian Gosch
	;; Date:	 26.6.2001

	;; These snippets are all taken from 
	;; the AMD processor manuals
global goASMCheckMMX:function, goASMCheck3DNOW:function, goASMCheck3DNOWDSP:function, goASMCheck3DNOWEXT:function, goASMMMXBegin:function, goASMMMXEnd:function, goASM3DNBegin:function, goASM3DNEnd:function
goASMCheckMMX:
	push	ebx
	push	edx
	mov	eax,0		; we have no parameters
        pushfd 
	pop	eax 
	mov	ebx,eax 
	xor	eax, 00200000h 
	push	eax 
	popfd 
	pushfd 
	pop	eax 
	cmp	eax,ebx 
	jz	NO_CPUID
	mov	eax,1 
	CPUID 
	test	edx, 800000 
	jz	NO_MM
	mov	eax,1
	pop edx
	pop ebx
NO_CPUID:
NO_MM:
	ret

	;; Taken from the AMD manual
goASMCheck3DNOW:
	push	ebx
	push	edx
	mov	eax,0		; we have no parameters
        pushfd 
	pop	eax 
	mov	ebx,eax 
	xor	eax, 00200000h 
	push	eax 
	popfd 
	pushfd 
	pop	eax 
	cmp	eax,ebx 
	jz	NO_CPUID2
	mov eax,80000000h	;query for extended functions 
	CPUID			;get extended function limit 
	cmp eax,80000000h	;is 8000_0001h supported? 
	jbe NO_EXTENDEDMSR	;if not,3DNow!tech.not supported

	mov	eax,80000001h	;setup ext.function 8000_0001h 
	CPUID			;call the function 
	test	edx,80000000h	;test bit 31 
	jz	NO_EXTENDEDMSR	;3DNow!technology not supported
	mov	eax,1
NO_CPUID2:	
NO_EXTENDEDMSR:
	pop	edx
	pop	ebx
	ret

	
goASMCheck3DNOWDSP:
	push	ebx
	push	edx
	mov	eax,0		; we have no parameters
        pushfd 
	pop	eax 
	mov	ebx,eax 
	xor	eax, 00200000h 
	push	eax 
	popfd 
	pushfd 
	pop	eax 
	cmp	eax,ebx 
	jz	NO_CPUID3
	mov eax,80000000h	;query for extended functions 
	CPUID			;get extended function limit 
	cmp eax,80000000h	;is 8000_0001h supported? 
	jbe NO_EXTENDEDMSR3	;if not,3DNow!tech.not supported

	mov	eax,80000001h	;setup ext.function 8000_0001h 
	CPUID			;call the function 
	test	edx,80000000h	;test bit 31 
	jz	NO_EXTENDEDMSR3	;3DNow!technology not supported

	mov	eax,80000001h
	CPUID
	test	edx,40000000h	;test bit 30
	jz	NO_EXTENDEDMSR3 ;DSP not supported
	
	mov	eax,1
NO_CPUID3:	
NO_EXTENDEDMSR3:
	pop	edx
	pop	ebx
	ret


goASMCheck3DNOWEXT:
	push	ebx
	push	edx
	mov	eax,0		; we have no parameters
        pushfd 
	pop	eax 
	mov	ebx,eax 
	xor	eax, 00200000h 
	push	eax 
	popfd 
	pushfd 
	pop	eax 
	cmp	eax,ebx 
	jz	NO_CPUID4
	mov eax,80000000h	;query for extended functions 
	CPUID			;get extended function limit 
	cmp eax,80000000h	;is 8000_0001h supported? 
	jbe NO_EXTENDEDMSR4	;if not,3DNow!tech.not supported

	mov	eax,80000001h	;setup ext.function 8000_0001h 
	CPUID
	test	edx,00400000h	;test bit 22
	jz	NO_EXTENDEDMSR4 ;extensions not supported
	
	mov	eax,1
NO_CPUID4:	
NO_EXTENDEDMSR4:
	pop	edx
	pop	ebx
	ret
	

goASMMMXBegin:
;; Check if we need to do a FSAVE here!
	ret

goASMMMXEnd:
;; Check if we need to do a FRSTOR here!
;; Use femms for AMD processors!
	emms
	ret

goASM3DNBegin:
	ret

goASM3DNEnd:
	femms
	ret