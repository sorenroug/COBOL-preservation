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

; This module provides an interface for calling Lattice V3.2x objects.
; Link your program as follows
;    LINK COBOL_objects+LINITC+LINITIO+C_objects,,,LCOBOL+LC+C_libraries/m;


;**
;
; Assembly parameters
;
STKMIN	EQU	128			; default paragraphs for stack
ENVMIN	EQU	64			; default paragraphs for environment
HEAPMIN	EQU	128			; default paragraphs for heap


	extrn	LINITIO:far


DGROUP	GROUP	DATA,NDATA,UDATA

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
; 	set pool=150

LINITC	proc	near			; Initialize lattice C

	mov	ax,DGROUP		; ax = lattice C data segment
	mov	ds,ax			; ds = ""
	call	__GETGLOBES		; es = COBOL global data segment
	mov	es:[cdataseg],ax	; set up for native code access
	mov	ax,sp
	add	ax,bpregz+pgdata+14h	; stack fudge
	mov	[_TOP],ax		; ss relative stack top
	mov	ah,30h			; get DOS version
	int	21h
	mov	[_DOS],ax		; DOS version number

	mov	ah,62h
	int	21h			; set up PSP address
	mov	word ptr [_PSP+2],bx
	mov	es,bx
;
; Initialize various size items
;
	SUB	_TSIZE,bx		; size of basic block
	SUB	_PSIZE,bx		; size of program section	
	SUB	_DSIZE,SEG DATA		; size of initialized data
	SUB	_NSIZE,SEG NDATA	; size of near data
	SUB	_USIZE,SEG UDATA	; size of uninitialized data
	SUB	_SSIZE,SEG STACK	; size of stack
	SUB	_CSIZE,SEG CX_CONST	; size of constants
	SUB	_FDSIZE,SEG CX_FAR	; size of far initialized data
	SUB	_FBSIZE,SEG CX_BSS	; size of far uninitialized data

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
	mul	si			; ... so multiply by 1024 to get bytes
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
	mov	ax,poolsize		; set up MNEED
	mov	cl,4
	rol	ax,cl
	mov	bx,ax
	and	ah,0fh
	mov	word ptr [_MNEED+2],ax
	and	bx,0fff0h
	mov	word ptr [_MNEED],bx
	mov	bx,poolsize
	mov	word ptr [_MSIZE],bx
	mov	[_STACK],200h		; stack size

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

;**
;
;  The "CODE" segments are followed by those with class "FAR_DATA", which
;  in turn are followed by segments in the "CONST" class.  The following
;  statements are dummy segments that force this ordering when this startup
;  module is linked first.
;
CX_FAR	SEGMENT	PARA 'FAR_DATA'
CX_FAR	ENDS

CX_CONST SEGMENT PARA 'CONST'
CX_CONST ENDS	

	PAGE
;**
;
;  DGROUP always includes the segments named DATA, UDATA, and NDATA.

;  The argument strings and the argument vector are placed at the high-order
;  end of the stack.  This is necessary because the command processor passes
;  and unparsed command line, but C main programs conventionally expect to
;  receive a "vector" of pointers to the command arguments.  The startup
;  routine treats the command line as a sequence of arguments separated by
;  white space.  If an argument contains white space, the entire argument
;  must be enclosed in double quotes.  Within such a quoted argument, a
;  double quote can be "escaped" by preceding it with a backslash.
;
;  The external variable "environ" contains
;  a pointer to this vector, and the pointer is also passed as the third 
;  argument to the main program.  Note that the vector may move as a result
;  of calls to "putenv" or "rmvenv", but when that happens, "environ" is
;  updated.
;
;  The heap (i.e. the space used by the memory allocator) resides above the
;  environment and is also initialized by the startup routine.  Any space not
;  immediately needed for the heap (as defined by _MNEED) is returned to DOS.
;  For the S and P models, the heap size is limited by the FAR_BSS class of
;  data items.  That is, if this class is not empty, then the heap cannot be
;  expanded by getting more space from DOS.
;  
;  At the end of the startup routine, memory is organized in the following 
;  sequence:
;
;	-- code --
;	-- FAR_DATA --
;	-- CONST --
;	-- DATA --
;	-- NDATA --
;	-- UDATA --
;	-- FAR_BSS --
;	-- stack --
;	-- arguments --
;	-- environment --
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
; This segment contains all initialized static data.
;
DATA	SEGMENT PARA PUBLIC 'DATA'
;
; External data items
;
	EXTRN	_STACK:WORD		; stack size needed by user
	EXTRN	_MNEED:DWORD		; heap size needed by user

poolsize	dw	1000h		; pool size in paragraphs
str_pool	db	"POOL",00	; pool environment string
str_poolsz	db	10 dup(' ')	; value of pool environment string

;
; These items provide information about DOS and the memory model.  The 
; _DOS word contains the major version number in its low order byte and
; the minor number in its high order byte.  If you are only interested 
; in the major number, you can reference _DOS as a byte instead of a word.
;
	PUBLIC	_MODEL,_DOS
_MODEL	DW	3			; L model indicator
_DOS	DW	0			; DOS major/minor version number
;
; Information about some objects that are passed by this process's parent.
; The addresses are all represented as far pointers.
;
	PUBLIC	_ENV,_ENVL,_PSP,_CMD
_ENV	DD	0			; address of original environment
_ENVL	DW	0			; size of original environment (bytes)
_PSP	DD	0			; address of program segment prefix
_CMD	DW	80H			; address of original command line
	DW	0
;
; The following item gives the size of the "basic block" for this process.
; The basic block is a contiguous memory area that includes the program,
; data, stack, and heap.  As the heap grows, this value changes.  It is
; administered by the low-level memory allocator (sbrk/rbrk).
;
	PUBLIC	_TSIZE
_TSIZE	DW	SEG HIGH		; size of basic block in paragraphs
; 
; The following item gives the size of the program section, which is the
; area from the Program Segment Prefix to DGROUP.
;
;
	PUBLIC	_PSIZE
_PSIZE	DW	DGROUP
;
; The following items give the base address and size of the CONST section.
;
	PUBLIC	_CBASE,_CSIZE
_CBASE	DW	0			; address of CONST section
	DW	SEG CX_CONST
_CSIZE	DW	DGROUP			; CONST size in paragraphs
;
; The following items give the base address and size of the initialized 
; far data section.
;
	PUBLIC	_FDBASE,_FDSIZE
_FDBASE	DW	0			; address of FAR_DATA section
	DW	SEG CX_FAR
_FDSIZE	DW	SEG CX_CONST		; FAR_DATA size in paragraphs
;
; The following items give the base address and size of the uninitialized
; far data section.
;
	PUBLIC	_FBBASE,_FBSIZE
_FBBASE	DW	0			; address of FAR_BSS section
	DW	SEG CX_BSS
_FBSIZE	DW	SEG STACK		; FAR_BSS size in paragraphs
;
; The following items give the base address and size of the initialized
; data section (i.e. the DATA segment) contained within DGROUP.
;
	PUBLIC	_DBASE,_DSIZE
_DBASE	DW	0			; address of DATA section
	DW	DGROUP
_DSIZE	DW	SEG NDATA		; DATA size in paragraphs
;
; The following items give the base address and size of the near data
; section (i.e. the NDATA segment) contained within DGROUP.
; 
	PUBLIC	_NBASE,_NSIZE
_NBASE	DW	OFFSET DGROUP:NDATA	; address of near data section
	DW	DGROUP
_NSIZE	DW	SEG UDATA		; NDATA size in paragraphs
;
; The following items give the base address and size of the uninitialized
; data section (i.e. the UDATA segment) contained within DGROUP.
; 
	PUBLIC	_UBASE,_USIZE
_UBASE	DW	OFFSET DGROUP:UDATA	; address of uninitialized data
	DW	DGROUP
_USIZE	DW	SEG CX_BSS		; UDATA size in paragraphs
;
; The following items give the base address and size of the stack section.
; This section is a part of the data group for the S and P models, but
; it stands alone for the D and L models.
;
	PUBLIC	_SBASE,_SSIZE,_TOP
_SBASE	DW	0			; address of stack section
	DW	STACK
_SSIZE	DW	SEG HIGH		; STACK size in paragraphs
_TOP	DW	0			; top of stack (relative to SS)
;
; The following items define the environment area.  For the S and P
; models, this area is part of the data group, but it stands alone
; for the D and L models.  As part of the initialization procedure, the
; original environment is copied to this new area, with unused space
; at the end for growth.
;
	PUBLIC	_EBASE,_ESIZE
_EBASE	DD	0			; address of environment section
_ESIZE	DW	0			; size of environment area in bytes
;
; The following items define the heap area.  For the S and P models, this
; area is part of the data group, but it stands alone for the D and L models.
;
	PUBLIC	_MBASE,_MSIZE,_MUSED
_MBASE	DD	0			; address of memory pool           
_MSIZE	DW 	0 			; number of paragraphs in pool
_MUSED	DW	0			; number of paragraphs used
;
; The process arguments are moved to the top of the stack as an array
; of null-terminated strings.  Then an "argument vector" is constructed 
; just below the argument strings.  The vector contains a pointer to each
; successive string.  The end of the vector is marked by a null pointer.
; Also, the last string is followed by a null string.
;
	PUBLIC	_ARG,_ARGV,_ARGC
_ARG	DW	0			; pointer to arg array in stack
	DW	STACK
_ARGV	DW	0			; pointer to arg vector in stack
	DW	STACK
_ARGC	DW	1			; argument count
;
; The environment passed by this process's parent is copied to an area
; just above the stack and below the heap.  The size of this area is
; determined by _ENEED, which should contain a value large enough to
; account for environment growth resulting from calls to "putenv".  If
; _ENEED is 0, then the startup routine computes a reasonable value.
;
	PUBLIC	environ,_ENVC
environ	DD	0			; environment vector pointer
_ENVC	DW	0			; environment count

;
; The following items are used by the floating point subsystem.
;
	PUBLIC	_FPA,_FPERR
	PUBLIC	_NDP,_NDPSW,_NDPCW
	PUBLIC	_SIGFPE
_FPA	DQ	0			; floating point accumulator
_FPERR	DW	0			; floating point error code
_NDP	DB	0			; non-zero if 8087 is installed
_NDPSW	DW	0FFFFH			; 8087 status word
_NDPCW	DW	0			; 8087 control word
_SIGFPE	DD	0			; Floating point error signal
;
; The following item contains the main error code returned by the most
; recent DOS call made through the standard library.  That is, each time
; the standard library calls a DOS service function, it checks the return
; code and places it here.  For DOS 3, the CXSERR routine obtains the
; extended error information and places it in several other items that
; you can find in CXSERR.ASM.
;
; Note that _OSERR, unlike the UNIX error code in errno, is not "sticky".  
; That is, _OSERR gets reset to zero when the DOS service is successful.
;
	PUBLIC	_OSERR
_OSERR	DW	0			; DOS error code

;
; Abort messages for the startup routine
;
ARGERR	DB	"Stack overflow during arg parsing",0DH,0AH,"$"
DOSERR	DB	"Incompatible DOS version",0DH,0AH,"$"
STKERR	DB	"Invalid stack size",0DH,0AH,"$"
MEMERR	DB	"Insufficient memory",0DH,0AH,"$"
ENVERR	DB	"Corrupted environment",0DH,0AH,"$"
HEPERR	DB	"Insufficient memory for specified heap size",0DH,0AH,"$"

DATA	ENDS


;**
;
; This segment contains all data items that are explicitly declared as
; "near".
;
NDATA	SEGMENT	PARA PUBLIC 'DATA'
NDATA	ENDS

;**
;
; Uninitialized data
;
UDATA	SEGMENT	PARA PUBLIC 'DATA'
UDATA	ENDS

;**
;
; This is a dummy segment that establishes the position of the uninitialized
; far data items in the D and L models.
;
CX_BSS	SEGMENT	PARA PUBLIC 'FAR_BSS'
CX_BSS	ENDS

;**
;
; This is the default stack segment, and it also defines the default 
; environment and heap areas.  The size may be larger or smaller after the
; startup routine finishes, because you are allowed to change the stack,
; heap, and environment sizes via the command line or via global variables.
;
STACK	SEGMENT STACK PARA 'STACK'
	DB	(STKMIN+ENVMIN+HEAPMIN)*16 DUP (?)
STACK	ENDS

;**
;
; This is a dummy segment that establishes the position of the uninitialized
; far data items in the S and P models.
;
CX_BSS	SEGMENT	PARA PUBLIC 'FAR_BSS'
CX_BSS	ENDS

;**
;
; This is a dummy segment that defines the high end of the basic block (i.e.
; the last segment in memory).
;
HIGH	SEGMENT	PARA PUBLIC 'HIGH'
HIGH	ENDS

	end
