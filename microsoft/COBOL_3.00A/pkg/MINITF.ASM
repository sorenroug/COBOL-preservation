	name	MINITF
	title	MINITF - Microsoft Fortran Initialization
	subttl	Copyright (C) Micro Focus Ltd 1987

; The only difference between this and the module MINITC is that
; we swap stacks at the start of the MINITF routine. This is because
; the Fortran compiler does not allow the generation of code for split
; DATA and STACK segments. This means that we must use a STACK within
; DGROUP. This is of fixed size 8K.

	public	__NCLINKM	; these are routines called from LCOBOL
	public	__NCLINKI	; from strategic places such as
	public	__NCLINKX	; exit from a program, startup, closedown
	public	__NCLINKXS	; etc.
	public	__NCLINKS
	public	__NCLINK
	public	__astart		
	public	__chkstk
	public	_main		; dummy routine to satisfy ref in LLIBC.LIB

	public	tmpstk

?DF=		1			; this is special for c startup
?PLM = 0
memL = 1

.xlist
CBLRUDAT segment para public 'CBL_DATA'

include cblrudat.equ

CBLRUDAT ends

include	handle.equ
include cmacros.inc
include msdos.inc
include brkctl.inc
.list

	extrn	__cinit:far
	extrn	_exit:far
	extrn	__exit:far

; This module provides an interface for calling Microsoft objects.
; Link your program as follows
;    LINK COBOL_objects+Fortran_objects+MINITF,,,LCOBOL+Fortran_libraries/m;

	page
;===========================================================================
;
;	Segment definitions
;
;	The segment order is essentially the same as in XENIX.
;
;===========================================================================

DOSSEG

createSeg	_TEXT,	code,	byte,	public, CODE,	<>
createSeg	C_ETEXT,etext,	byte,	public, ENDCODE,<>

createSeg	_DATA,	data,	word,	public, DATA,	DGROUP
createSeg	BSS,	data,	word,	public, BSS,	DGROUP
createSeg	STACK,	stack,	para,	stack,	STACK,	DGROUP

defGrp	DGROUP			; define DGROUP


; We must here set up a temporary stack since it is impossible to tell
; the Fortran compiler to allocate separate data and stack segments

_DATA	segment word public 'DATA'

tempstk	db	8192 dup (0)	; 8k temporary stack

_DATA	ends

page

public	__acrtused		; trick to force in startup
	__acrtused = 9876h	; funny value not easily matched in SYMDEB


sBegin	data

extrn	_edata:byte		; end of data (start of bss)
extrn	_end:byte

externW _psp			; psp:0 (paragraph #)
externW __argc
externDP __argv
externDP environ

;	these are used by DOS C memory management (not used in Windows)

globalW _asizds,0			; DS size (in bytes)
globalW _atopsp,0			; top of stack (heap bottom)

labelW	<PUBLIC,_abrktb>		; segment table for brkctl
	dw	?
	dw	DGROUP
	db	(MAXSEG-1) * (size segrec) dup (?)

labelW	<PUBLIC,_abrktbe>
globalW _abrkp,<offset DGROUP:_abrktb>

_ENV	dd	0
_ENVC	dw	0
_ESIZE	dw	0

tmpstk	dw	100 dup (0)		; temporary stack for calling __cinit
endstk	db	?			; as need ss=ds for this routine

MEMERR	DB	"Insufficient memory",0DH,0AH,"$"

sEnd


sBegin	code
	assume	cs:_TEXT,ds:DGROUP,es:DGROUP

oldsp		dw	0
oldss		dw	0

; The following routine is taken partly from the file CRT0.ASM provided
; with the MSC C product and does necessary initialization
; for Microsoft C objects. It will only handle 'L' model objects
; set up with ss not = ds (i.e. compiled with /Awlf).

; Entry: es=cobol global data segment

MINITF	proc	far			; Initialize msc C

	; This code assumes that COBOL is the main program
	; since it only copies one COBOL stack frame to the new stack
	; FIX when Fortran allows split Stack and DATA segments
	cli				; change over stacks
	mov	dx,es			; save CBLRUDAT pointer
	mov	bx,bp			; calculate diff betw sp and bp in bx
	sub	bx,sp
	mov	ax,ss
	mov	ds,ax			; ds = old stack segment
	mov	ax,DGROUP
	mov	es,ax			; es = DGROUP
	mov	ss,ax			; ss = DGROUP
	mov	si,sp			; si = source stack pointer
	mov	cx,bpregz+pgdata+14h	; stack fudge size to copy
	mov	di,8192
	sub	di,cx			; di = start of stack
	mov	sp,di			; set up sp to be new stack pointer
	mov	bp,di
	add	bp,bx			; bp = new stack frame pointer
   rep  movsb				; copy old stack to new stack
	mov	es,dx			; restore CBLRUDAT pointer
	sti

	mov	ax,DGROUP		; ax = msc C data segment
	mov	ds,ax			; ds = ""	""
	mov	es:[cdataseg],ax	; set up for native code access
	mov	ax,sp
	add	ax,bpregz+pgdata+14h	; stack fudge
	mov	[_atopsp],ax		; ss relative stack top
	mov	bx,seg STACK
	sub	bx,DGROUP
	mov	cl,4
	shl	bx,cl
	add	ax,bx
	mov	[_abrktb].sz,ax		; ds relative top of memory
	dec	ax
	mov	[_asizds],ax		; save DS size - 1

	mov	ah,62h
	int	21h			; set up PSP address
	mov	word ptr [_psp],bx

	mov	es,[_psp]
	mov	ax,es:[2ch]		; ax = pointer to environment
	mov	es,ax
	mov	word ptr [_ENV+2],ax
	xor	di,di			; es:di -> environment
	xor	bx,bx
	xor	al,al			; now process environment to C format
	mov	cx,0ffffh		; always find !
MIC10:
  repnz	scasb				; find end of next env string
	inc	bx			; increment string count
	scasb				; end of environment ?
	jnz	MIC10			; no -->

	mov	word ptr [_ESIZE],di	; size of environment
	mov	[_ENVC],bx		; number of environment strings

	shr	bx,1			; allocate space for env vector
	shr	bx,1			; of pointers
	inc	bx			; plus space for terminator
	mov	ah,48h
	int	21h
	jnc	MIC20
	mov	ax,offset MEMERR	; no memory
	jmp	XCABT
MIC20:
	xor	si,si
	push	ds
	mov	word ptr [environ+2],ax	; pointer to environment struct
	mov	bx,[_ENVC]
	or	bx,bx			; any env strings ?
	jz	MIC40			; no -->
	les	di,[_ENV]		; es:di points to env
	mov	cx,[_ESIZE]
	inc	cx
	mov	ds,ax			; ds:si points to alloc'd memory
	xor	ax,ax
MIC30:
	mov	[si],di			; set up ptr to next env string
	mov	[si+2],es		; in env structure
	add	si,4
  repnz	scasb				; find next one
	dec	bx
	jnz	MIC30
	mov	[si],bx			; terminate with nulls
	mov	[si+2],bx
	pop	ds			; ds = DGROUP again

	mov	[oldsp],sp		; save stack for later
	mov	[oldss],ss
	mov	ax,ds			; therefore need local stack
	mov	ss,ax			; temporarily
	mov	sp,offset DGROUP:endstk	; __cinit needs ds = ss
	call	__cinit			; C initialization
	mov	ss,[oldss]		; restore real stack
	mov	sp,[oldsp]

MIC40:
	ret
	

MINITF	endp

__chkstk proc   far
	pop	bx			; pop off return address
	pop	cx
	sub	sp,ax			; assign local stack frame
	push	cx			; push back return address
	push	bx
	ret
__chkstk endp

_main	 proc	far
__astart proc	far
	ret
__astart endp
_main	 endp

page
;------------------------------------------------------------------------
;
;	Fast exit fatal errors - die quick and return (255)

labelNP <PUBLIC,_cintDIV>

labelNP <PUBLIC,_amsg_exit>
	call	__exit			; _exit(255)

;**
;
; name		XCABT -- Ignominious abort
;
; description	This area is entered by direct jump with a message
;		pointer in DS:DX.  It sends the message to the 
;		console via DOS function 9 and then aborts.
; 
XCABT	proc	near
	MOV	AH,9			; print error message
	INT	21H
	MOV	ES,WORD PTR _PSP+2
	MOV	AX,4C01H
	INT	21H
XCABT	endp

sEnd


CSEG	segment	byte public 'CODE'

	assume	cs:CSEG,ds:DGROUP

	extrn	__GETGLOBES:near

installed	db	0

__NCLINKM	proc	near		; Called from LCOBOL on startup

	test	installed,1		; is msc already installed ?
	jnz	nm10			; yes -->
	or	installed,1
	push	ds
	push	bx
	push	cx
	push	dx
	call	__GETGLOBES		; es = COBOL global data segment
	call	MINITF			; initialize msc
	pop	dx
	pop	cx
	pop	bx
	pop	ds
nm10:
	ret

__NCLINKM	endp

__NCLINKS	proc	near		; Called from LCOBOL on closedown

	test	installed,1		; is msc already installed ?
	jz	ns10			; no -->
	push	ds
	push	ax			; push return code
	mov	ax,DGROUP
	mov	ds,ax
	mov	es,word ptr [environ+2]	; free allocated environment
	mov	ah,49h
	int	21h
	call	_exit			; terminate C routines
	pop	ds
ns10:
	ret

__NCLINKS	endp


__NCLINKI  proc near
__NCLINKX  proc near
__NCLINKXS proc near
__NCLINK   proc near
           ret
__NCLINK   endp
__NCLINKXS endp
__NCLINKX  endp
__NCLINKI  endp

CSEG	ends

	end
