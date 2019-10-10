	name	LINITC
	title	LINITC - Lattice C Initialization
	subttl	Copyright (C) Micro Focus Ltd 1987

	public	__NCLINKM	; these are routines called from LCOBOL
	public	__NCLINKI	; from strategic places such as
	public	__NCLINKX	; exit from a program, startup, closedown
	public	__NCLINKXS	; etc.
	public	__NCLINKS
	public	__NCLINK
	public	_MAIN		; dummy routine to satisfy ref in LC.LIB

	.8087

CBLRUDAT segment para public 'CBL_DATA'

	include cblrudat.equ

CBLRUDAT ends

	include	handle.equ

; This module provides an interface for calling Lattice V3.1x objects.
; Link your program as follows
;    LINK COBOL_objects+LINITC+LINITIO+C_objects,,,LCOBOL+LC+C_libraries/m;


;**
;
; Assembly parameters
;
STKMIN	EQU	512			; minimum run-time stack size
TAB	EQU	09H			; tab character
STKRSV	EQU	128			; reserved stack size


	extrn	LINITIO:far


;**
;
; Define data group
;
DGROUP	GROUP	DATA,UDATA,XSTACK
	ASSUME	DS:DGROUP


; The following routine is taken partly from the file C.ASM provided
; with the Lattice C product and does necessary initialization
; for Lattice C objects. It will only handle 'L' model programs
; so to use this and LINITIO you must compile your lattice C programs with
; the -ml option.

; The pool size allocated in this module is by default 64k, but if
; the environment variable POOL is set then this value will be used.
; so to allocate 150k of pool size type the following to your DOS 
; prompt
; 	set pool=150

CSEG	segment byte public 'CODE'

	assume	cs:CSEG,ds:DGROUP

	extrn	__GETGLOBES:near
	extrn	__MFGETENV:near

installed	db	0

__NCLINKM	proc	near		; Called from LCOBOL on startup

	test	installed,1		; is lattice c already installed ?
	jnz	nm10			; yes -->
	or	installed,1
	push	ds
	push	bx
	push	cx
	push	dx
	call	LINITC			; initialize lattice
	call	LINITIO			; initialize lattice IO library
	pop	dx
	pop	cx
	pop	bx
	pop	ds
nm10:
	ret

__NCLINKM	endp

__NCLINKS	proc	near		; Called from LCOBOL on closedown

	test	installed,1		; is lattice c already installed ?
	jz	ns10			; no -->
	push	ds
	mov	ax,dgroup	
	mov	ds,ax
	mov	es,word ptr [environ+2]	; free allocated environment
	mov	ah,49h
	int	21h
	mov	es,word ptr [_MBASE+2]	; free allocated pool memory
	mov	ah,49h
	int	21h
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


; The following routine is taken partly from the file C.ASM provided
; with the Lattice C product and does necessary initialization
; for Lattice C objects. It will only handle 'L' model programs
; so to use this and INITIO you must compile your lattice C programs with
; the -ml option.

; The pool size allocated in this module is by default 64k, but if
; the environment variable POOL is set then this value will be used.
; so to allocate 150k of pool size type the following to your DOS 
; prompt
; 	set pool=150000

LINITC	proc	near			; Initialize lattice C

	mov	ax,DGROUP		; ax = lattice C data segment
	mov	ds,ax			; ds = ""
	call	__GETGLOBES		; es = COBOL global data segment
	mov	es:[cdataseg],ax	; set up for native code access
	mov	ah,30h			; get DOS version
	int	21h
	mov	word ptr [_DOS],ax	; DOS version number

	mov	ax,sp
	mov	[_SP],ax
	mov	[_SS],ss
	add	ax,bpregz+pgdata+14h	; stack fudge
	mov	[_TOP],ax		; ss relative stack top

	mov	ax,seg _PROG		; calculate _PSIZE
	mov	bx,ds
	sub	bx,ax
	mov	cl,4
	rol	ax,cl
	mov	word ptr [_PSIZE],ax
	and	word ptr [_PSIZE],0fff0h
	and	ax,0fh
	mov	word ptr [_PSIZE+2],ax

	mov	ah,62h
	int	21h			; set up PSP address
	mov	word ptr [_PSP+2],bx
	mov	es,bx

	mov	ax,[_TOP]		; calculate _TSIZE
	add	ax,0fh
	mov	cl,4
	shr	ax,cl
	mov	bx,ss
	add	ax,bx			; ax = top of program (stack is last)
	mov	bx,word ptr [_PSP+2]
	sub	ax,bx			; subtract start address (PSP)
	mov	[_TSIZE],ax		; _TSIZE = total size of program

	mov	ax,es:[2ch]		; ax = pointer to environment
	mov	word ptr [_ENV+2],ax
	mov	es,ax
	xor	di,di			; es:di -> environment
	xor	bx,bx
	xor	al,al			; now process environment to C format
	mov	cx,0ffffh		; always find !
LIC10:
  repnz	scasb				; find end of next env string
	inc	bx			; increment string count
	scasb				; end of environment ?
	jnz	LIC10			; no -->

	mov	word ptr [_ESIZE],di	; size of environment
	add	di,2
	and	di,0fffeh		; make even number
	mov	word ptr [_XSIZE],di	; size of extended env
	mov	[_ENVC],bx		; number of environment strings

	shr	bx,1			; allocate space for env vector
	shr	bx,1			; of pointers
	inc	bx			; plus space for terminator
	mov	ah,48h
	int	21h
	jnc	LIC20
	mov	ax,offset MEMERR	; no memory
	jmp	XCABT
LIC20:
	xor	si,si
	push	ds
	mov	word ptr [environ+2],ax	; pointer to environment struct
	mov	bx,[_ENVC]
	or	bx,bx			; any env strings ?
	jz	LIC40			; no -->
	les	di,[_ENV]		; es:di points to env
	mov	cx,[_ESIZE]
	inc	cx
	mov	ds,ax			; ds:si points to alloc'd memory
	xor	ax,ax
LIC30:
	mov	[si],di			; set up ptr to next env string
	mov	[si+2],es		; in env structure
	add	si,4
  repnz	scasb				; find next one
	dec	bx
	jnz	LIC30
	mov	[si],bx			; terminate with nulls
	mov	[si+2],bx
	pop	ds			; ds = DGROUP again

LIC40:					; allocate space for memory pool
	mov	[_STACK],800h		; stack size
	mov	ax,ds
	mov	es,ax
	mov	bx,offset DGROUP:str_pool ; ds:bx -> "POOL"
	mov	di,offset DGROUP:str_poolsz ; es:di -> area to be returned
	mov	cl,10
	call	__MFGETENV		; get env var.
	jc	LIC45			; no such name
	xor	ch,ch			; cl = length of result buffer
	xor	ax,ax			; dx;ax to contain result
	xor	dx,dx
	xor	bx,bx
	mov	si,10			; for multiplication by 10
LIC42:
	mul	si			; size = size * 10
	mov	bl,es:[di]		; size = size + digit
	sub	bl,'0'
	inc	di			; next digit
	add	ax,bx
	adc	dx,0
	loop	LIC42
	mov	si,1024			; result is in nK
	mul	si			; ... so multiply by 1024
	mov	cl,4 			; calculate num paras
	ror	dx,cl			; to allocate
	shr	ax,cl
	or	ax,dx
	mov	poolsize,ax		; set up poolsize

LIC45:
	mov	bx,poolsize		; allocate pool
	mov	ah,48h
	int	21h
	jnc	LIC50
	mov	ax,offset DGROUP:MEMERR	; got memory ?
	jmp	XCABT

LIC50:
	mov	word ptr [_MBASE+2],ax	; base of memory pool
	mov	word ptr [_MNEXT+2],ax	; next memory address
	mov	ax,poolsize		; set up MNEED
	mov	cl,4
	rol	ax,cl
	mov	bx,ax
	and	ah,0fh
	mov	word ptr [_MNEED+2],ax
	mov	word ptr [_MSIZE+2],ax
	and	bx,0fff0h
	mov	word ptr [_MNEED],bx
	mov	word ptr [_MSIZE],bx
	mov	[_STACK],800h		; stack size

;
; initialize 8087 numeric data processor
;
	FNINIT				; reset
	FNSTSW	_NDPSW			; get status
	MOV	AX,100			; this is just for delay
	MOV	DX,AX
	IMUL	DX
	TEST	_NDPSW,0B8BFH		; 8087 will reset all these
	JNZ	LIC_x
	INC	_NDP			; indicate ndp present

LIC_x:
	ret

LINITC	endp

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

_MAIN	proc	near			; dummy _MAIN procedure
	ret
_MAIN	endp

CSEG	ends


_PROG	segment byte public 'PROG'
_PROG	ends


;**
;
;  DGROUP includes the segments named DATA, UDATA, and XSTACK.  The startup
;  routine initializes DS to point to this group, and DS must then be pre-
;  served throughout the program.  The segments within DGROUP are defined
;  as follows:
;
;	DATA   => Contains all static (local and global) initialized items.
;	UDATA  => Contains all static (local and global) uninitialized items.
;	XSTACK => Stack for the startup routine.
;
;  During the startup routine, the initial stack (XSTACK) is replaced with
;  one that has the correct size for program execution.  This size is 
;  determined by examining the command line and the _STACK global item. Then
;  for the S and P memory models, the stack is set up relative to DGROUP (i.e.
;  stack items can addressed via DS).  For the D and L models, the stack 
;  segment stands alone and can be up to 64K bytes.
;
;  The heap (i.e. the space used by the memory allocator) resides above the
;  stack and is also initialized by the startup routine.  Any space not
;  immediately needed for the heap (as defined by _MNEED) is returned to DOS.
;  
;  At the end of the startup routine, memory is organized in the following 
;  sequence:
;
;	-- code --
;	-- DATA  --
;	-- UDATA --
;	-- stack --
;	-- heap --
;
;  FOR PROPER OPERATION OF THE STANDARD MEMORY ALLOCATOR, THIS SEQUENCE IS
;  EXTREMELY IMPORTANT.  IF YOU TAMPER WITH THE STARTUP ROUTINE OR INTRODUCE
;  SEGMENTS AND CLASSES THAT DO NOT FOLLOW LATTICE CONVENTIONS, CHECK THE
;  LOAD MAP CAREFULLY.
;
;**

;**
;
; Initialized data 
;
DATA	SEGMENT PARA PUBLIC 'DATA'
	EXTRN	_STACK:WORD
	EXTRN	_MNEED:DWORD
	PUBLIC	_MODEL,_VER,_TOP,_BASE,_PSP,_MBASE,_MNEXT,_MSIZE,_DSIZE,_PSIZE
	PUBLIC	_ENV,_DOS,_TSIZE,_ESIZE,_XSIZE,_SS,_SP,_NDP,_NDPSW,_NDPCW
	PUBLIC	_FPA,_FPERR,_OSERR,_SIGFPE,_ARGV,_ARGC,_ENVC,environ,_DSP

poolsize	dw	1000h		; pool size in paragraphs
str_pool	db	"POOL",00	; pool environment string
str_poolsz	db	10 dup(' ')	; value of pool environment string

_MODEL	DW	3
_VER	DB	"LC 3.00",0
_DOS	DB	0			; DOS major version number
	DB	0			; DOS minor version number
_SS	DW	0			; stack segment number
_SP	DW	0			; SP reset value
_TOP	DW	0			; top of stack (relative to SS)
_BASE	DW	OFFSET DGROUP:SBASE	; base of stack (relative to DS)
_PSP	DW	0			; program segment prefix pointer
	DW	0
_DSP	DW	0			; data segment pointer
	DW	DGROUP
_MBASE	DW	0			; base of memory pool
	DW	0
_MNEXT	DW	0			; next available memory location
	DW	0
_MSIZE	DW 	0 			; number of bytes left in pool
	DW	0
_TSIZE	DW	0			; total size in paragraphs
_PSIZE	DD	0			; size of program in bytes
_DSIZE	DW	OFFSET DGROUP:SBASE	; size of static data in bytes
	DW	0
_ARGV	DD	0			; argument vector pointer
environ	DD	0			; environment vector pointer
_ARGC	DW	0			; argument count
_ENVC	DW	0			; environment count
_ARG	DD	0			; far pointer to original arg array
_ENV	DD	0			; far pointer to original env array
_ESIZE	DW	0			; environment size in bytes
_XSIZE	DW	0			; extended env size in bytes
_FPA	DQ	0			; floating point accumulator
_FPERR	DW	0			; floating point error code
_NDP	DB	0			; non-zero if 8087 is installed
_NDPSW	DW	0FFFFH			; 8087 status word
_NDPCW	DW	0			; 8087 control word
_OSERR	DW	0			; DOS error code
_SIGFPE	DD	0			; Floating point error signal
STKERR	DB	"Invalid stack size",0DH,0AH,"$"
MEMERR	DB	"Insufficient memory",0DH,0AH,"$"


DATA	ENDS


;**
;
; Uninitialized data
;
UDATA	SEGMENT	PARA PUBLIC 'DATA'
UDATA	ENDS

;**
;
; The stack segment is included to prevent the warning from the
; linker, and also to define the base (lowest address) of the stack.
;
XSTACK	SEGMENT STACK PARA 'DATA'
SBASE	DB	STKRSV DUP (?)
XSTACK	ENDS

	end
