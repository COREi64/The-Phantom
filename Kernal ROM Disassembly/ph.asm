
; *******************************
; Trilogic Phantom Kernal 1.07
; Overlay for original CBM-Kernal
;
; disassembled and analyzed by
;
; Thomas Salzlechner, 11/2023
;
; ********************************
;

; **** assemble to $2000, origin is $e000

eprom		= $2000
offset		= eprom-$e000

; labels zeropage
linnum		= $14			; temp vars for calculations
index1		= $22			; used by LNKPRG
txttab		= $2b			; start BASIC code
vartab		= $2d			; start BASIC variables	
arytab		= $2f			; start BASIC arrays	
aryend		= $31			; end BASIC array	
fretop		= $33			; start BASIC strings	
memtop		= $37			; highest RAM address	
facho		= $62			; FAC#1	
facmoh		= $63			; FAC#1	
chrget		= $0073			; CHRGET, get next character
chrgot		= $0079			; CHRGOT, get last character
status		= $90			; ST status variable
stkey		= $91			; Flag for STOP and RVS
verck		= $93			; flag 0=load, 1=verify
bsour		= $95			; byte for IEC output
msgflg		= $9d			; 0=direct  $80=program
pcntr 		= $a3			; EOI flag
firt		= $a4			; temporary variable (tape, serial) 
count		= $a5			; temporary variable (tape, serial)
sal		= $ac			; address load lo
sah		= $ad			; address load hi
eal		= $ae			; end address lo
eah		= $af			; end address hi
phblank		= $b4			; $d011 value if screen blanking on
fnlen		= $b7			; length of filename
secadr		= $b9			; secondary address
device		= $ba			; primary device
fnadr		= $bb			; adress of filename lo
mych		= $bf			; serial buffer
stal		= $c1			; start address lo
stah		= $c2			; start address hi
tmp2		= $c3			; temp variable
ndx		= $c6			; # of chars in keyboard buffer
pnt		= $d1			; address of cursor pos lo
pntr		= $d3			; cursor positio in line
qtsw		= $d4			; quote mode
ldtb1		= $d9			; start addresses of screen lines

; labels page 2
keyd		= $0277			; keyboard buffer start
shflag		= $028d			; flag for shift/cbm
keylog		= $028f			; pointer to decoding table
autodn		= $0292			; flag screen scrollin 0=on
phdevice	= $029b			; phantom standard device, bit 7=1 fkeys off
enabl		= $02a1			; NMI enable flag used as temp var

; labels page 3
iload		= $0330			; vector into kernal LOAD routine

; labels BASIC
lnkprg		= $a533			; calc basic lines link adresses
stkrts1		= $a687			;
frmnum		= $ad8a			; evaluate numerical term
chkerr		= $ad99			; show type mismatch error
frmevl		= $ad9e			; evaluate term
frestr		= $b6a3			; check for string
linprt2		= $bdd1			; convert hex to dec, data already in FAC
plsv7		= $e200			; get byte to xr, check for comma
paoc16		= $e25d			; get filename, pointer already in INDEX1

; labels Kernal
csys1		= $e130			; part of SYS command
lp2		= $e5b4			; get char from keyboard buffer
loop3		= $e5cd			; keyboard input
xe617		= $e617			; call into CLP5, get first non space from the right
loop5		= $e632			; get char from screen to A
prt		= $e716			; print character on screen
scnrts		= $eb42			; part of RPT20, select keyboard column 7
ntalk		= $ed09			; IEC TALK
nlistn		= $ed0c			; IEC LISTEN
isour1		= $ed44			; call into ISOUR (ouput byte on IEC bus)
nsecnd		= $edb9			; secondary address after LISTEN
scatn		= $edbe			; pull ATN lo
ntksa		= $edc7			; secondary address after TALK
nciout		= $eddd			; output byte on IEC bus
nuntlk		= $edef			; IEC UNTALK
nunlsn		= $edfe			; IEC UNLISTEN
nacptr		= $ee13			; read byte from IEC bus
acp00a		= $ee1b			; call into NACPTR
clkhi		= $ee85			; pull CLOCK active (low)
clkhi2		= $ee88			; CLOCK low without reading port first
clklo		= $ee8e			; pull CLOCK inactive (high)
clklo2		= $ee91			; CLOCK HIGH without reading port first
datahi		= $ee97			; pull DATA active (low)
datahi2		= $ee9a			; DATA low withou readin port first
datalo		= $eea0			; pull DATA inactive (high)
wims		= $eeb3			; delay for 1 ms
ms1		= $f0bd			; table of OS messages
spmsg		= $f12b			; print message if in direct mode
msg		= $f12f			; print message
xf198		= $f198			; just an RTS
bn31		= $f1b1			; just for branching			
jx150		= $f2f1			; just for branching
ck20		= $f25f			; kust for branching (NOT OUTPUT FILE)
openi		= $f3d5			; open IEC and send filename
ld26		= $f4c4			; entry into LD25 (serial load) 
ld40		= $f4f3			; wait for IEC timeout
ld185		= $f5aa			; end of load, endadress in x/y
ld180		= $f5a9			; end address to X/Y and back ok
ld190		= $f5ae			; end of tape load routine, just for rts
luking		= $f5af			; SEARCHING FOR
sv26		= $f608			; callback into IEC save routine			
break		= $f633			; break error in IEC routine
clsei		= $f642			; close IEC file
clsei1		= $f646			; close IEC file
sv115		= $f68e
ud60		= $f6bc			; set flag for STOP and RVS
error4		= $f704			; file not found
error8		= $f710			; missing file name
error9		= $f713			; illegal device number
zzz		= $f7d0			; pointer to start of tape buffer
cste1		= $f817			; PRESS PLAY and wait for key			
rblk		= $f841			; read block from tape
tstop1		= $f8d6			; part of tape routines
rd80		= $faeb			; part of tape load routine
rd52		= $fb2f			; part of tape load routine
a0int		= $fd02			; check for exrom module
restor		= $fd15			; set standard vectors
udst		= $fe1c			; ora A to ST
prend 		= $febc			; IRQ end, pull registers
kstnam 		= $ffbd			; set filename parameters
kbsout		= $ffd2			; output character
kstop		= $ffe1			; check for STOP key
start		= $fffc			; reset vector

			; start at $e000
			*=$e000
			.offs offset

; load original kernal first (e.g. the VICE kernal file
; then assemble the overlays

			.binary "kernal"	


; hook into sys command, replace call to FRMNUM
; comes back with a call to FRMNUM if not hex

			*=$e12a
			.offs offset
				
csys			jsr hexsys 	; sys parameter

; hook into the save command, replaces call to PLSV
; to get parameters for save

			*=$e156
			.offs offset

csave 			jsr plsv1	; get filename parameters


; get parameters for load/save	

			*=$e1d4
			.offs offset

plsv 			ldy #$00	; load
			.byte $2c	; 
plsv1			ldy #$ff	; save
			jsr devchk	; check/correct device
			iny		; load: 1 save: 0 
			cmp #$01	; if device 1
			bne l1		; 
			dey		; load: 0 save: $ff
l1 			sty secadr	; secondary address
			sta device	; device address
			tya		; 
			beq l2		; if load
			iny		; y=0
			cpy shflag	; check for shift
			bne l3		; 
			dec secadr	; secadr -1
l3			lda #3		; for empty name use
			ldx #<runtb	; filename 0:* 
			ldy #>runtb	; 
l2			jsr kstnam	; set filename parameters
			jsr chrgot	; read last character
			jmp plsv2	; continue

; ***** changes to the sign-on  message

			*=$e49a
			.offs offset

			.null "  phantom v1.07 "


; **** print loading from $xxxx-, but stop printing if start address <$0800

			*=$e4b7
			.offs offset

lodingf			jsr loding	; print loading
			ldy eal		; compare				
			lda eah		; start address in file
			cpy tmp2	; 
			bne lodingf1	; to address 	
			cmp tmp2+1	; given to $FFD5
			beq lodingf2	; if equal just print it
lodingf1		cpx #0		; secondary address 0
			beq lodingf3	; then skip run/sys change
			jmp runsys	; change RUN command to SYS 
lodingf3		jsr prtrvs	; if addresses differnt		
			ldy tmp2	; switch on revers
			sty eal		; and store address
			lda tmp2+1	; from LOAD
			sta eah		; into current address
lodingf2		jsr prtsal	; print start address (rvs if changed)
			lda eah		;
			cmp #8		; if <$0800 skip printing
			bcc lodfscrn	; the "-" since it will corrupt the data
			lda #$2d	; 
			jmp prtdir	; print - if direct mode
lodfscrn		lda #0		; if <$0800				
			sta ndx		; also flush the keyboard buffer
			rts		; back
			
; **** set text color to yellow or all original cbm colors if CTRL is pressed
			
phcolor			lda #4		; check for ctrl key
			bit $dc01	;
			bne phcol1	; if not pressed just set yellow text
			ldy #$0e	; if pressed (with restore) 
			sty $d020	;
			lda #$06	; set original cbm colors
			sta $d021	;
phcol1			sty $0286	; 
			rts		; back
	
; **** 	hook into screen initalisation	
;	to implement STRG-RESTORE (sets original CBM colors) 	
	
			*=$e534
			.offs offset
	
cint1			ldy #$07	; color yellow
			jsr phcolor	; set colors
	
	
; hook into keyboard input for f-keys	
	
			*=$e5e7
			.offs offset
	
lp21			jsr phfkeyck	; phantom f-key check

; **** hook into LP21 (keyboard input), patch shift-r/s

			*=$e5ee
			.offs offset
	
lp21a			jsr phauto	; patched shift-r/s
	
			*=$e5f3
			.offs offset
	
runlop			lda phruntb-1,x	; use new text for shift-r/s
	
; **** hook into keyboard input, handle phantom commands	

			*=$e614
			.offs offset

phkeyb			jmp phwedge	; handle the @/> commands

; **** hook into SCROL (screen scrolling), CBM key stops scrolling

			*=$e938
			.offs offset
	
scr14			ldy #$7f	; keyboard row 7			
scrl41			sty $dc00	;
			lda $dc01	;
			sty $dc00	; continue scan
			cmp #$df	; wait if it was c= key
			beq scrl41	;
			cmp #$fb	; if not CTRL continue
			bne scrl43	;
scrl42			jsr wims	; slow down the scrolling
			jsr wims	;
			dey		; even more
			bne scrl42	;
			sty ndx		; clear keyboard buffer
scrl43			ldx $d6		; current cursor pos line

; **** hook into CLRLIN (clear screen line)

			*=$ea07
			.offs offset
	
clr10			jsr cpatch	; cpatch relocated from $e4da

; **** hook into scnkey (irq keyboard scan)	

			*=$eadd
			.offs offset

ckit2			jmp resetck	; check for soft reset
	

; **** VIC constants, patch in the default colours frame/backround	
	
			*=$ecd9
			.offs offset

			.byte $0c,$0b

; patching runtb to 0:* well and wombat like in the floppy rom :)	
	
			*=$ece7
			.offs offset

runtb			.null "0:*wombat"


; **** hook into NLISTN, send LISTEN to the bus 		
	
			*=$ed0e
			.offs offset

list0			nop		;
			nop		; cancel out rs-232 code
			nop		;
	
			*=$ed24
			.offs offset

list3			nop		; cancel out old serial
			nop		; code
			nop		;
			nop		;
			nop		;
			nop		;
			nop		; 
			jsr resetpar	; and use phantom code

; **** hook into ISOUR (output byte to iec)
	
			*=$ed41
			.offs offset

isour0			jmp phisour	; jumpt to phantom IECOUT		
	
; **** hook into NSECND (sec address for listen) 	

			*=$edbb
			.offs offset

nsecnd2			jsr phisoura	; call phantom ISOURA

; **** hook into NTKSA (sec address for talk) 

			*=$edc9
			.offs offset
	
ntksa2			jsr phisoura	; call phantom ISOURA
	
; **** hook into TKATN (not ATN after talk)	

			*=$edd0
			.offs offset

tkatn0			jsr phatn	; ATN low
			jsr clkhi2	; CLOCK low 

; **** patch for NUNLISN (unlisten)  
	
			*=$ee10
			.offs offset

dlad01			jmp datahi2	; prevents reading pra again 
	
; **** hook into NACPTR (get byte from IEC)	
	
			*=$ee18
			.offs offset
	
nacpt1			jmp phiecin	; call to parallel read routine

; **** switch fkeys on and print dos enabled	
	
			*=$eebb
			.offs offset
				
			nop		; cleared out
				
phenabled		asl phdevice	; trash bit 7
			ldy #$ba	; DOS enabled
			clc		; 
phonoff			ror phdevice	; store 0 into bit 7
			jmp msg		; show messsage

; check for @ or > for DOS wedge (called via stack manipulation) 
	
phcmds			cmp #$40	; @
			beq phcmdok	; or
			cmp #$3e	; >
			beq phcmdok	; continue
			clc		; 
			rts		; else back to EBASIN or $xx14
phcmdok			tya		;	
			pha		; save
			txa		; registers
			pha		;
			ldy #0		;
			sty status	; clear status
			sty facho	; and fac
			jsr getchar	; A=number (C=0) Y=char (C=1)		
			bcs phcmdch	; its character
;			
; **** @## for setting standard device, also a little buggy, e.g. you can issu @8i			
;			
			tax		; save as tens
			jsr getchar	; A=number (C=0) Y=char (C=1)
			bcs phcmdch2	; its character
phcmdlp1		adc #$0a	; add tens
			dex		;
			bpl phcmdlp1	;
			sbc #$09	; adjust again
			tax		; save it
			jsr getchar	; A=number (C=0) Y=char (C=1)
phcmdch2		txa		; restore original value
			and #$0f	; lo nibble
			sta phdevice	; store as device number
			dex		;
			bne phcmdch	; continue
			cpy #$0d	; ENTER		
			beq phcmdnd2	; skip
phcmdch			cpy #"q"	; @Q = DISABLE DOS
			bne phcmddrv	; no continue checking
;			
; **** @q command (kill dos)			
;			
			asl phdevice	; trash bit 7
			sec		; set carry
			ldy #$2d	; DOS KILLED
			jsr phonoff	; switch off and print message
phcmdnd2		jsr chrout	; and <CR>
			bcc phcmdnd	; to end of routine
;			
; **** drive commands from here on, $ and pound open the file, all others the command channel			
; 			
phcmddrv		lda #$6f	; reopen channel 15 
			cpy #"$"	; @$ = directory
			beq phcmdfil	; continue
			cpy #$5c	; @<pound> = alt directory
			bne phcmdcmd	; everything else send as command
phcmdfil		lda #$f0	; open channel 0 for $/pound file
phcmdcmd		sta secadr	; store secondary address
			jsr devchk	; get current device
			sta device	; store as device number
			jsr nlistn	; send LISTEN
			lda secadr	;
			jsr nsecnd	; and secondary address
			tya		; 
			bit status	; check for timeout
			bpl phcmd2	; no continue  
phcmerr			ldy #$38	; device not present
			jsr msg		; print 
			lda device	;
			sta facmoh	; print device #
			jsr linprt2	; hex to dez
			ldy #$3a	;
			jsr msg		; print not present
			bcc phcmdnd	; end
phcmlp1			jsr nciout 	; output the string
			jsr loop5	; next char
phcmd2			cmp #$0d	; until <CR> reached
			bne phcmlp1	; 
			jsr nunlsn	; UNLISTEN
			lda device	;
			jsr nlistn	; send listen		
			lda #$6f	; reopen channell 15
			jsr nsecnd	; secondary address
			bit status	; check for timeout
			bmi phcmerr	; then device not present
			jsr nunlsn	; UNLISTEN
			jsr opstatus	; set device to talk on channel 15
			ldy #3		;
			lda secadr	; check if it was command or dir/pound
			bmi phcmddir1	; dir/pound output
;				
; **** output the status of whatever command was sent except $ and pound				
;				
			jsr chrout	; print <CR>
phcmdlp2		jsr nacptr 	; read byte from bus
phcmdlp4		jsr prt		; print
			cmp #$0d	; 
			bcs phcmd3	; if <CR> or greater
phcmdlp3		jsr nacptr	; if not
			cmp #$0d	; skip to next <CR>
			bne phcmdlp3	; skipping what i do not know 
phcmd3			beq phcmdnd1	; if <CR> end
			cmp #$8d	; if <shift CR>
			bne phcmdlp2	; where do you get <shift CR>, maybe the drivemon
			jsr kstop	; scan for stop
			bne phcmdlp2	; loop
phcmdnd1		jsr nuntlk	; UNTALK
phcmdnd			jmp loop3	; back

; **** show directory or <pound>file

phcmddir		ldy #$02	; overread 2 bytes
phcmddir1		jsr chrout	; print <CR> 
phcmddirlp1		jsr nacptr 	; read byte
			sta facmoh	; store in FAC
			jsr nacptr	; read byte
			ldx status	; check status
			bne phcmddirnd	; end/error
			dey		;
			bne phcmddirlp1	; next byte
			sta facho	; store in FAC
			jsr linprt2	; print number
			lda #" "	; space
phcmddirlp2		ldx status	; check status
			bne phcmddirnd1	; end/error
			jsr prt		; print space or byte read
			jsr nacptr	; read byte
			bne phcmddirlp2	; till zero
			jsr kstop	; check for stop
			bne phcmddir	; next line
phcmddirnd1		jsr chrout	; print <CR>
phcmddirnd		jsr nuntlk	; untalk
			jsr clsei1	; close iec file	
			lda #$6f	;	
			sta secadr	; cmd channel
			jsr opstatus	;
			jsr nacptr	; read byte (status)
			cmp #$30	; check for 0(0,ok,00,00)
			bne phcmdlp4	; if not print error
			beq phcmdlp3	; overread status and end
	
; **** read a char and check for numbers
	
getchar			jsr loop5	; get char
			tay		; into y
			sbc #$2f	; minus $30 (carry is cleared by LOOP5)
			cmp #$0a	; check for numbers (C=0)
			rts		; back	

; **** check for fkeys and STOP	
	
phfkeyck		jsr lp2		; read char 
			bit phdevice	; if fkeys off		
			bmi phfkck1	; skip	
			bit msgflg	; check for direct mode	
			bpl phfkck1	; if not skip
			sbc #$84	; check for f1-f8
			cmp #$08	;
			bcc phfkeyh	; then handle fkeys
phfkck1			eor #$7e	;	
			bne phfkck2	; check for STOP key 
			bit $dc01	;
			bmi phfkck2	;
			sta $c7		; clear RVS	
			sta qtsw	; clear quote mode
			sta ldtb1-1	; screen line addresses
phfkck2			tya		; char back
			rts		; back into input
				
; **** 	handle f-keys (A=f-key number), just runs thru all texts until the
;		right one is found, probably to save the space of storing an offset
				
phfkeyh			ldx qtsw	; quote mode 
			bne phfkck1	; then skip
			stx pntr	; clear cursor pos					
			sta ndx		; # of fkey					
phfkeyhn		ldy #0		;
phfkeyh1		lda fkeytx,x	; get text for fkey
			php		; save flags
			and #$7f	; clear bit 7
			sta keyd,y	; store to buffer				
			inx		;
			iny		; inc pointers
			plp		;
			bpl phfkeyh1	; bit 7 set = end
			dec ndx		; dec fkey numver
			bpl phfkeyhn	; not finished, next
			sty ndx		; store # of keys
			pla		; 
			pla		; clear stack
			jmp loop3	; into keyboard input
	
; **** plsv continued, get filename, device and secondary
	
plsv2			beq plsvnd	; end of string
			eor #$2c	; was it a comma
			beq plsv22	; continue
			jsr frmevl	; evaluate term					
			jsr frestr	; evaluate string
			tax		;
			beq plsv21	; length 0?
			jsr paoc16	; set file name parameters ($22/$23)
plsv21			jsr expcomma	; get comma or else ... end
plsv22			sta secadr	; secondary address 0 
			jsr plsv7	; byte to X, check for comma
			txa		; 				
			beq plsv23	; if not zero
			stx device	; store as device address 
			jsr expcomma	; getc comma or else ... end
			jsr plsv7	; byte to X, check for comma
plsv23			stx secadr	; store secondary address
plsvnd			rts 		; back

; **** end of LOAD routine (from phload from $f4a5 via $0330 from $ffd5)

phloadnd		lda phblank	; get original value for $D011
			beq notoff	; if not zero	
			sta $d011	; store in $D011
notoff			jsr prteal	; print endadress
			bit $dc01	; 
			cli		; check for STOP 
			bmi notbrk	; no, end normally
			jmp break	; break error
notbrk			jmp ld185	; end of load	


; **** read byte from the parallel bus (called from NACPTR) 

phiecin			bit $dc0f	; check CRB 
			bmi phiecinp	; if bit 7=1 do parallel
			jsr clkhi	; clear CLOCK line 
			jmp acp00a	; serial transfer
phiecinp		stx count	; save X
			ldx #$20	; 20 tries
phiecinlp1		bit $dd00	; wait for CLOCK IN high 
			bvc phiecinlp1	; loop
			lda $dd0d	; clear ICR
			jsr datahi	; clear DATA line
			lda #$10	; bit 4 = FLAG
phiecinlp2		dex		; dec counter
			beq phiecinerr	; timeout
			bit $dd0d	; wait for *FLAG
			beq phiecinlp2	; loop
			bit $dd00	;
			bvc phiecinok	; check CLKIN, if active then ok
			lda #$40	; status or $40 (EOI)
			.byte $2c	;
phiecinerr		lda #$02	; status or 2 (TIMEOUT)
			jsr udst	; set status
phiecinok		jsr datalo	; set DATA inactive
			ldx count	; reload X
			lda $dd01	; read data from paralled
			sta firt	; and store
			clc		; 
			cli		;
			rts		; back
;
; **** read data byte from the parallel bus and CLOCK/DATA lines		
;		
phread			lda $dd0d	; wait for *FLAG2
			beq phread	;
			lda $dd01	; read byte
			bit $dd00	; check for CLOCK/DATA
			rts		; back

; **** continuation of LD40, back to parallel or LD40 (with check for eof)
		
chkeof			bcs chkeof1	; C=1 skip and continue with parallel load  
			bit status	; check for eof
			bvs chkeofys	;
			jmp ld40	; back to LD40
chkeofys		jmp ld65	; eof back to end of LD40
chkeof1			jmp backld42	; back into parallel load 	

; **** phantom status/error messages, replacing the CBM ones
	
			* = $f0e2
			.offs offset

			.byte $d9	; shifted Y of press play
			.text "renewe"	; renewed
			.byte $c4	;
			.byte 13	; <crlf> dos killed			
			.text "dos kille"
			.byte $c4	;
			.byte 13,$a3	; <crlf> #						
			.text " not present"	
			.byte $8d	; not present <crlf>
			.byte 13	; <crlf> loading	
			.text "loading"	;
			.byte $a0	;
			.byte 13	; <crlf> saving	
			.text "saving"	;
			.byte $a0	;
			.byte 13	; <crlf> verifying	
			.text "verifying"
			.byte $a0	;
			.byte 13	;
			.text "found"	; <crlf> found
			.byte $a0	;
			.byte 13	; <crlf> ok <crlf>
			.text "ok"	;
			.byte $8d	;			;

; **** hook into GN232, patch out call to RS-232 getbyte				
				
			*= $f150
			.offs offset

gn232a 			nop		; patch
			nop		; out
			nop		; RS-232 code

; **** hook into NBASIN, patch out RS-232 and tape code 

			* = $f175
			.offs offset
	
			bcc bn31	; cancel RS-232/tape code
	
; **** text for dos enabled 	
	
			.text "dos enable"	
			.byte $c4	; text for DOS ENABLED
	
; **** from reset routine	(part 1)
	
phreset1		ldx $dc00	; save PRA for later
			ldy $dc02	; save DDRA for later
			lda #$ff	; set port A
			sta $dc02	; to output
			lda #$fd	; keyboard column1 
			jmp phreset2	; continue
	
; **** text for shift runstop	

			*=$f199
			.offs offset

phruntb 		.text "load"	; load <CR> run: <CR>
runmsg 			.byte 13	; text
			.text "run:"	; for shift-R/S
			.byte 13	;

; check last character input for comma, else do a rts-rts

expcomma		jsr chrgot	; get last character
			eor #$2c	; back if comma	
			beq expcomm1	;
			pla		; else pull address and back
			pla		; into previous caller
expcomm1		rts		;


; **** switch parallel flag off, port to input and set ATN low 

			*=$f1b8
			.offs offset

phatn			bit $dc0f	; check bit 7 of CRB
			bpl phatn2	; if clear skip
			lda #0		; else
			sta $dd03	; set port b to input
phatn2			jmp scatn	; set ATN lo 

; **** switch RVS on
 
prtrvs			lda #$12	; print rvs
			jmp prtdir	; 
	
	
; **** renew command (could be shortened quite a bit by using the CLR routine)
			
			*=$f1db
			.offs offset

			rts		;		
phrenew			lda #$01	; give lnkprg
			tay		; a reason to run by storing
			sta (txttab),y	; non zero into first BASIC link
			jsr lnkprg	; recalc all the links
			lda index1	; end of links is in $22 
			adc #$02	; add 2
			sta vartab	; an store into 
			sta arytab	; all the variable pointers
			sta aryend	;
			lda index1+1	; hi byte in $223
			adc #0		;
			sta vartab+1	; and store
			sta arytab+1	;
			sta aryend+1	;
			lda memtop	; strings from the top
			ldy memtop+1	;	
			sta fretop	; would be much shorter to set $2d/$2e
			sty fretop+1	; and call CLR 
			ldy #$26	; 
			jsr msg		; print RENEWED
			jmp stkrts1	; into end of CLR routine

; on break error while LOADING, print end address then error

			*=$f208
			.offs offset

breakeal		jsr prteal	; print end address
			jmp break	; break error

; **** hook into NCHKIN (open output channel) to cancel out tape and RS-232

			* = $f223
			.offs offset

			bcc ck20	; skip over RS-232/tape code
				
; **** called from NMI, change CTRL-RESTORE to R/S-RESTORE				
				
phstop			jsr ud60	; get status of STOP key			
			lda stkey	; better call it column 7
			cmp #$fb	; CTRL pressed
			bne phstop1	; no, then skip
			lda #$7f	; set STOP pressed
			sta stkey	; translates CTRL-RESTORE 
phstop1			rts		; to R/S-RESTORE me thinks

; **** from RESET routine (N-flag set if shift pressed)
	
			*=$f268
			.offs offset
				
			bcc ck20	; cancel out old tape code
phreset3		stx $dc00	; restore PRA
			sty $dc02	; restore DDRA
			bpl phstop1	; if shift pressed skip module test
			jmp a0int	; else check for modules and back to reset  


; **** hook into NCLOSE, illega device if <3

			*=$f2a7
			.offs offset

 			jmp jx150	; cancel code for tape/rs232 close file

; **** from LODINGF, change RUN:<CR> in keyboard buffer to SYS$xxxx:<CR>

runsys			ldx ndx		; check text in keyboard buffer		
runsyslp1		lda keyd-1,x	; against run: text
			cmp runmsg,x	;
			bne runsysnd	; not found skip
			dex		;
			bne runsyslp1	;
			asl ndx		; 10 characters instead of 5 
runsyslp2		lda sysmsg,x	; copy sys$		
			sta keyd,x	; into keyboard buffer
			inx		;
			cpx #$04	;
			bne runsyslp2	;
			lda eah		; hibyte
			jsr hexasc	; convert to ascii
			sta keyd+4	; into keyboard
			stx keyd+5	; buffer
			tya		; lobyte
			jsr hexasc	; convert to ascii
			sta keyd+6	; store into 
			stx keyd+7	; keyboad buffer
			lda #$3a	; : 
			sta keyd+8	; into keyboard buffer
			lda #13		; <CR>
			sta keyd+9	; into keyboard buffer
runsysnd		jmp lodingf2	; back to output routine
	
; **** from keyboard input	
	
phauto			ldx #$0a	; needs 10 bytes
			sei		; irq off
			lda #0		; reset cursor pos
			sta pntr	; probably to ensur that text is printed
			rts		; on the left side, probably fixes the $DC0E bug
	
; **** hook into NOPEN, illegal device if <3	
	
			*=$f384
			.offs offset

op150			jmp error9	; illegal device error

;
; **** check SYS-parameter for hex entries and convert if necessary  
; **** then jump back into original SYS command
;
hexsys			eor #$24	; check for $ (hex number)
			beq hexsys1	; yes continue
			jmp frmnum	; else back to standard sys via FRMNUM
hexsys1			sta linnum+1	; clear temp storage	
hexsyslp		sta linnum	; store remainder of last operation (clear if start)
			jsr chrget	; get next char
			beq hexsysnd	; Z=1: end of number  
			bcc hexsynm	; its a number
			sbc #$37	; check for 
			cmp #$10	; check for letters a-f
			bcc hexsynm	; use it
			cmp #$5f	; check for the basic token of DEF
			bne hexsyse1	; when entering some hex address with $DEF
			lda #$de	; store $de into lobyte
			sta linnum	; and here is a bug, you can not sys to $xdef other than $0def
			lda #$0f	; and $0f as the rest
hexsynm			and #$0f	; convert to hex
			ldx #$03	; 
hexsyssh		asl linnum	; shift last char into hinibble
			rol linnum+1	; and hinibble into hibyte
			dex				;
			bpl hexsyssh	; 4 shifts
			ora linnum	; add lo nibble
			jmp hexsyslp	; loop
hexsysnd		pla		; done, value is in linnum
			pla		; clear stack
			jmp csys1	; back to original sys
hexsyse1		jmp chkerr	; BASIC type mismatch

; **** check for R/S and arrow (soft reset)
			
resetck			jsr scnrts	; set keyboard row 7	
			lda $dc01	; read key	
			cmp #$7d	; check for R/S and arrow ($80 and $2)
			beq resetck1	; if so do a reset
			jmp (keylog)	; back into keyscan routine	
resetck1		jmp (start)	; reset	
	
; **** detect phantom drive and screen blanking (A=L (load) A=S (save))
; **** return with C=1 fast, C=0 and N=1: not present or Z=1: slow	(1541 or file error)

			*=$f409
			.offs offset

phopen			sta mych	; save S or L
			lda device	; current device
			jsr nlistn	; as listener
			lda #$6f	; reopen channel 15
			jsr nsecnd	; 
			bit status 	;  
			bmi phderr	; not present, back
			lda #"u"	; "U"
			jsr nciout	; 
			lda #"0"	; "0"
			jsr nciout	;
			lda mych	; "S" or "L"
			jsr nciout	; 
			ldy #0		;
			sty phblank	; clear screen blanking
phdlp1			lda (fnadr),y	; send filename
			jsr nciout	;
			iny 		;
			cpy fnlen	;
			bcc phdlp1	; loop
			jsr nunlsn	; UNLISTEN -- sent U0L<filename> or U0S<filename>
			lda device	; current device
			jsr ntalk	; TALK
			lda #$6f	;
			jsr ntksa	; reopen channel 15
			jsr nacptr	; read byte
			tay		; save
phdlp4	 		jsr nacptr	; read bytes
			cmp #$0d	; until <CR>
			bne phdlp4	; 
			jsr nuntlk	; UNTALK
			cpy #"b"	; first character B: enable screen blanking
			beq phdblnk	; Z and C are 1
			cpy #"o"	; first character O: no screen blanking
			beq phdlp2	; Z and C are 1
			cpy #"0"	; none phantom drive will give 00,ok,00,00
phderr			clc		; C=0, no parallel
			rts		; back
phdblnk			lda $d011	; save VIC register
			sta phblank	;
			and #$ef	; screen off
			sta $d011  	;
phdlp2			lda $dd00	;
			ora #$10	; clear CLOCK OUT (high)
			sta $dd00	;
			bmi phdlp2  	; wait for DATA in high
			ldy #0		;
			sty $dd03	; port B to input
			lda #$10	; 
			sta $dd0d	; NMI off for *FLAG2
phdlp3			lda $dd0d	;
			beq phdlp3	; wait for *FLAG2
			lda $dd01	; read byte
			eor mych	; eor with S/L 
			bne phdlp3	; wait for S or L
			rts		; back, C is still 1

; **** clear flag for parallel and reset direction of port B

resetpar		bit $dc0f	; check bit 7 of CRB 
			bpl resetp1	; on bit 7 skip
			lda #$08	; clear bit 7
			sta $dc0f	; of CRB
			lda enabl	; set direction of port B
			sta $dd03	; to stored value
			lda #0		; 	
			sta enabl	; clear temp
resetp1			rts		; back
	
; **** hook into NLOAD routine ($f4a5 via $0330 from $ffd5)

			*=$f4a7			
			.offs offset

ld00			ldx #0		; clear status
			stx status	;
			ldx device	; current device
			cpx #$04	;
			bcs ld01	; > 3 oj
			dex		;
			nop		;
			nop		;
			beq jxtap7	; 1 = ok, continue	
			.byte $d0,$7e	; all others go here 				
			;bne xf436	; somehow doesn't assemble correcty
ld01			ldy fnlen	; length of filename
			bne ld02	;
			jmp error8	; 0 then missing filename error
ld02			ldx secadr	;
			jmp phloadvfy	; phantom load routine				

; **** hook into LD20 (load file)

			*=$f4e8
			.offs offset
	
			nop		;
			nop		; cancel out the
			nop		; original setting
			nop		; of EAL and EAH
			nop		;
			nop		;
			nop		;
			nop		;
				
ld30			jsr lodingf	; print loading from $xxxx-
	
; hook into LD40 (break on LOAD) to print end address	

			*=$f4fe
			.offs offset

			jmp breakeal	; break error, print end address
				
			*=$f512
			.offs offset

; **** verify goes here			
				
			eor (eal),y	; changed from cmp (eal),y
			beq ld41	; ok next byte
			lda #$10	; VERIFY error
			jmp phstatus	; replaces call to UDST

; **** c=1: chkeof will continue with parallel load

ld42			sec		; for xf0ac

; **** load goes here

ld43			sta (eal),y	; store byte
ld41			inc eal  	; increment address
			bne ld44	;
			inc eah 	;
ld44			jmp chkeof	; continune parallel or check status

 			nop		; clear out 1 byte	

; end of LD40 (same but printing of end address)
	
ld65			jsr nuntlk	; UNTALK 
			jsr clseal	; close and print endaddress
			bcc ld180	; end address in X/Y and back
ld90			jmp error4	; file not found 	

; **** changed tape code

jxtap7 			tax		; 
			beq ld102	; device 1 is ok
jxtap71			jmp error9	; illegal device number
ld102			jsr zzz		; pointer to start of tape buffer
			bcc jxtap71	; <$200 then error
			jsr cste1	; PRESS PLAY and wait for key
			bcs ld190	; back
			jsr luking	; SEARCHING FOR
			inc $d020	; inc border color	
			jsr fah		; read block from tape		
			dec $d020	; dec border color
			jsr prfound	; print FOUND
			bcc xf55d	; skip over PHRESET2

; **** from RESET routine (part 2) (A=keyboard column) 
	
phreset2		sta $dc00	; set keyboard column 
			lda $dc01	; read column row
			jmp phreset3	; continue

; **** tape load continued				
				
xf55d			lda $90		; read status

; hook into tape load, change a branch to illegal device

			*=$f56b
			.offs offset

			.byte $d2	;

; **** print loading/verifying, offsets into texts have changed

			*=$f5d2
			.offs offset

loding			ldy #$47	; loading
			lda verck	; load/verify-flag
			beq loding1	; 
			ldy #$58	; verifying
loding1			jmp spmsg	; print message


; **** hook into SV25 (open write file on IEC) part of NSAVE ($f5ed, from $ffd8 via $0332)

			*=$f605
			.offs offset

sv25			jmp phsave	; phantom save

; **** hook into JXTAP8 (cancel out write to tape) 

			*=$f659
			.offs offset

jxtap8 			jmp error9	; illegal device


; **** print end address for load/verify

prteal 			ldy pntr	;
			dey		;
			lda (pnt),y	; get char at cursor
			asl		; 
			clc		;
			eor #$5a	; check for -
			bne prtskip	; no quit
			ldy eal		; lo byte endaddres	
					
; **** print $xxxx if in direct mode					
					
prtsal			lda #$24	; $ character
			jsr prtdir	; print $
			lda eah		; hi byte endadress
			jsr prthex	; print hibyte
			tya		; lobyte
prthex			jsr hexasc	; A-> ascii in A/X
			jsr prtdir	; print hi nibble
			txa		; and lo nibble
prtdir			bit msgflg	; check for direct mode
			bpl prtskip	; if so
			jsr kbsout	; print
prtskip			sei		; mask irqs
			rts		; back
	
; **** part of reset, initialize phantom drive 8 and enabled	
	
phreset0 		lda #$08	; standard device 8, DOS enabled
			sta phdevice	; store
			jmp restor	; kernal init
			
			bit $18		; code rest
			rts
	
; **** hook into SAVING

saving			lda msgflg	; check for direct mode	
			bpl sv115	; if not rts
			ldy #$50	; correct offset of message

; **** changed tape code 
		
			*=$f72b
			.offs offset

fah01			rts		; not changed, just for branch
fah			jsr rblk	; read block from tape		
			bcs fah01	; back		
			ldy #0		;
			sty verck	; load/verify flag to load
			lda ($b2),y	; read byte from tape buffer
			cmp #$05	; if 5 back
			beq fah01	;
			tax		;
			clc		;
			dey		;
			rts		; back

; **** check correct device #, Z=1 load Z=0 save
	
devchk2			bne devchk21	; 
			cmp #$01	; if load device can be 1 
			beq devchk23	;
devchk21		cmp #$08	; device <8 use 8
			bcc devchk22	;
			cmp #$10	; device >15
			bcc devchk23	;
devchk22		lda #$08	; use 8
devchk23		rts		; back

; print found message for tape load
	
prfound			ldy #$63	; print
			jmp spmsg	; found	

; **** replacement for ISOURA, send secondary address with ATN 

phisoura		sei		; irq off
			jsr clklo	; CLOCK inactive
			jsr datahi2	; DATA active	
			stx count	; save X			
			ldx #$58	; counter
			lda $dd0d	; clear irq flags
			lda #$10	; Bit 4 = *FLAG
phisoura1		bit $dd01	; toggle *PC
			dex		; dec counter
			beq phisoura2	; skip parallel if timed out 
			bit $dd0d	; check ICR
			beq phisoura1	; wait for *FLAG on UP
			lda #$88	; setup flag and port 
			sta $dc0f	; for parallel
			jsr parout	; output
phisoura2		ldx count	; restore X

; **** output byte on parallel bus 

phisour			bit $dc0f	; check bit 7 of CRB 
			bmi phisourys	; if set parallel mode
			jmp isour1	; back into serial out		
phisourys		lda $dd01	; read data port
			sta count	; and store
			lda $dd00	; check for
			bmi phisournp	; DATA IN hi then not present
			jsr clkhi2	; set CLOCK low
phisourlp1		lda $dd00	; 
			bpl phisourlp1	; DATA IN low, wait
			bit pcntr	; check EOI flag
			bmi phisour2	;
			jsr clklo2	; set CLOCK hi
phisour2		lda bsour	; char to output
			stx bsour	; save X
			ldx #$20	; counter
			sta $dd01	; write data to port
			lda #$10	; bit 4 = *FLAG
phisourlp2		bit $dd0d	; wait for *FLAG
			bne phisournd	; then branch
			dex		; dec counter
			bne phisourlp2	; else loop
			lda #2		; timeout
			jsr udst	; error
phisournd		ldx bsour	; restore X
			bit $a3		; check eoi flag
			bpl phisournd3	; if set
phisournd2		jsr resetpar	; clear the parallel bus
phisournd3		lda count	; restore 
			sta $dd01	; port B from before
			clc		; 
			cli		; allow irqs
			rts		; back
phisournp		lda #$80	; set DEVICE NOT PRESENT
			jsr udst	; in sstatus
			bmi phisournd2	; and out
	
; **** print carriage return	
	
chrout			lda #$0d	; <CR>
			jmp prt		; print
	
; **** cancel out part of tape code

			*=$f7ea
			.offs offset
	
faf			jmp fah		; replaces a CALL to fah

; **** text for the f-keys

fkeytx			.byte $40	; F1: @8$:* <ENTER> (directory drive 8)
			.text "8$:*"
			.byte $8d
			.text "load"	; F3: load <ENTER>
			.byte $8d
			.text "r"	; F5: r<shift u> <ENTER> (run)
			.byte $75,$3a,$8d
			.byte $40,$8d	; F7: @ <ENTER> (disk status)
			.byte $40	; F2: @9$:* <ENTER> (directory drive 9)
			.text "9$:*"
			.byte $8d		
			.text "save"	; F4: SAVE"	
			.byte $a2
			.text "li"	; F6: li <shift s> : <ENTER> (list)
			.byte $73,$3a,$8d
			.text "sys61916"	
			.byte $8d	; F8: sys61916 <ENTER> (renew)

; **** patch out tape code

			*=$f838
			.offs offset

cste2			jmp tstop1	; to end of tape code

; **** close file and print end address

clseal			jsr clsei	; close iec-file
			jmp prteal	; print end address

; **** set (verify) error and back to end of load routine 	
	
			*=$f864
			.offs offset
	
phstatus		jsr udst	; store status (A)
			jmp ld65	; back to LD40			
	
; **** check/correct device # (part 1) Y=flag 00=load, $ff=save
	
devchk 			lda phdevice	; read phantom device
			and #$0f	; only lower nibble
			cpy #$00	; check if its load
			jmp devchk2	; continue

; **** cancel out rs-232 code in TWRT2	
	
			*=$f88a
			.offs offset

			nop		;
			nop		; patch out RS-232 code
			nop		;
	
; **** cancel out stop-check in TWRT2	

			*=$f8c7
			.offs offset

			nop		;
			nop		; patch out RS-232 code
			nop		;

; **** hook into tape/rs-232 code to skip some bytes   
	
			*=$fadb	
			.offs offset	
		
			bne rd80	; cancel out tape/rs-232
			
; **** save DDRB and set port B to output			

parout			lda $dd03	; save ddrb	
			sta enabl	; to temp 	
			lda #$ff	; set port 	
			sta $dd03	; to output
			rts		; 
		
; **** hook into rs-232 code, skip some bytes, still not sure why it's not just taken out		

			*=$fb20
			.offs offset

xfb20			jmp rd52

; **** set current device as talker on channel 15

opstatus		lda device	; current device
			jsr ntalk	; TALK
			lda secadr	; 
			and #$6f	; reopen channel 15
			jmp ntksa	; set secondary address
	
; **** Parallel LOAD/VERIFY (from $f4a5 via $0330 from $ffd5)  

			*=$fba6
			.offs offset 

phloadvfy		jsr luking	; searching for
			ldy #0		;
			lda ($bb),y	; checkf for name $...
			cmp #"$"	; if not
			bne phlv1	; then do parallel
phlv0			jmp ld26	; load $ slow 
phlv1 			lda #"l"	;
			jsr phopen	; check phantom and open file 
			bcs phlv2	; yes doit
			beq phlv0	; do slow mode if not phantom
			bmi phlv0	; not present
			jmp error4	; file not found
phlv2			jsr phread	; read byte
			bmi phlverr1	; DATA IN high if error 
			sta eal		; store address lo
			jsr phread	; read byte
			bmi phlverr1	; DATA IN high if error
			sta eah		; store address hi
			jsr lodingf	; print loading from $xxxx-
			lda #0		; clear eal
			sta eal		;
			ldx verck	; load/verify flag
			beq phlvld	; to load part
;			
; **** verify command here			
; 			
phlvlp1			lda $dd0d	;
			beq phlvlp1	; wait for *FLAG2
			lda $dd01	; read byte
			bit $dd00	; 
			bmi phlverr1	; end (DATA in hi)
			cmp (eal),y	; compare with memory
			bne phlvverr	; error
			iny		; next byte
			bne phlvlp1	;
			inc eah		; increment hibyte
			bit $dc01	; check for STOP
			bmi phlvlp1	; no, continue
			bpl phlvnd	; error
phlvverr		lda #$10	; verify error 
			sta status	; into status
			bne phlvnd	; error
;			
; **** load command here			
;			
phlvld			lda phblank	; check if screen blanked
			bne phlvlp7	; yes - branch
;			
; **** load without screen blanking			
;			
			sty eal		; eal
phlvlp2			jsr kstop	; check for stop
			beq phlvnd	; error
			jsr phread	; read	
phlverr1		bmi phlverr2	; DATA IN high if error
			ldy #0		;
			jmp ld42	; store byte and increment address
backld42		ldy eal		;
			bne phlvlp2	;
			lda eah		;	
			cmp #$04	;
			bcc phlvlp2	;	
phlvlp3			jsr phread	; read byte
			bmi phlverr2	; DATA IN hight if error
			sta (eal),y	; store byte
			iny 		; next byte
			bne phlvlp3	;
			inc eah		; increment hibyte
phlvlp5			bit $dc01	; check for STOP 
			bpl phlvnd	; error
			lda eah		; hibyte
			beq phlvlp2	; 
			cmp #$d0	; if !$D000
			bne phlvlp3	; next byte
			lda $01		;
			and #$fb	; switch off IO
			tax		;
phlvlp4			jsr phread	; read byte
			bmi phlverr2	; end
			stx $01		; switch off I/O
			sta (eal),y	; store byte under I/O
			txa		;
			ora #4		;
			sta $01		; switch I/O back on
			iny		; 
			bne phlvlp4	; nextt I/O byte
			inc eah		; increment hibyte
			lda eah		;
			cmp #$e0	; from $e000
			beq phlvlp5	; continue normal load
			bit $dc01	; check for STOP
			bmi phlvlp4	; continue
			bpl phlvnd	; error
;			
; **** load for blanked screen, doesn't load under I/O		
; 			
phlvlp7			lda $dd0d	;
			beq phlvlp7	; wait for *FLAG
			lda $dd01	; read byte
			bit $dd00	; check DATA in 
			bmi phlverr2	; if hi end
			sta (eal),y	; store into memory
			iny		; next byte
			bne phlvlp7	; 
			inc eah		; increment hibyte
			bit $dc01	; check for STOP
			bmi phlvlp7	; if not continue
;			
; end for both load routines			
;			
phlvnd 			lda $dd00	;
			and #$ef	; CLOCK OUT low 
			sta $dd00	;
phlvlp6			jsr phread	; read byte 
			bpl phlvlp6	; wait for DATA IN high
phlverr2		asl		; shift
			lda $dd00	;
			and #$ef	; CLOCK out low
			sta $dd00	;
			sty eal		; store index into end address lo
			jmp phloadnd	; end load
	
	
; **** hook into reset routine	
	
			*=$fce7
			.offs offset

			jsr phreset1	; new module check replace call to A0INT
	
			*=$fcf8
			.offs offset
	
			jsr phreset0	; phantom initialization
	

; replacec tape irq vectors with standard vector	
	
			*=$fd9b	
			.offs offset	

			.word $ea31,$ea31
	
; hook into nmi-routine (restore) to eventually skip				
; check for modules (when shift is pressed)				
				
			*=$fe56	
			.offs offset	

			jsr phreset1	; replaces call to A0int	
					
; **** hook into NMI (handling of STOP)	
	
			*=$fe5e	
			.offs offset

			jsr phstop	; handle STOP key

; **** hook into NMI20 (NMI routine) to cancel out RS232-code

			*=$fe72
			.offs offset

			jmp prend	; jump to end of NMI,
				
; **** handle the additional commands 			
				 
phwedge			sty autodn	; screen scrolling on 
			cpy #$01	; if scrolling off then ignore 
			beq phwend	; there seems to be no way that y>0
			bit phdevice	; check for DOS enabled
			bmi phwend	; if not ignore
			pla		;
			tax		; pull registers from stack 
			pla		;
			tay		; check if the call to KBASIN ($FFCF) came 
			pla		; from EBASIN ($E112). of course since
			pha		; only the lo byte is checked, there
			cmp #$14	; might result a bug if called from $xx12
			bne xfe91	; if not from EBASIN skip phantom commands	
			lda #>phcmds-1	; put call to 
			pha		; phantom command
			lda #<phcmds-1	; check 
			pha		; on the stack
xfe91			tya		;
			pha		; push
			txa		; registers back
			pha		;
phwend			ldy autodn	; scrolling flag
			jmp phkeyb+3	; back to keyboard input

; **** convert hex value in A to ascii in A/X
	 
hexasc			pha		; save value
			and #$0f	; lo nibble
			jsr hexasc1	; convert to ascii
			tax		; and store in X
			pla		; original value
			lsr		; shift hi
			lsr		; nibble
			lsr		; into
			lsr		; lo nibble
hexasc1			ora #$30	; convert to number
			cmp #$3a	; if number 
			bcc hexasc2	; then back
			adc #$06	; convert to letter	
hexasc2			rts		; back

; **** relocated CPATCH routine (from $e4da)

cpatch 			lda $0286	; current color
			sta ($f3),y	; store into color ram
			rts		; back
	
	
; **** parallel SAVE routine (from SV25, via $f5ed, $0332, $f5dd, $ffd8) 
	
			*=$fec2
			.offs offset

phsave 			lda #"s"	; 
			jsr phopen	; check phantom/open file
			bcs phsaveys	; yes do it
			beq phsaveno	; serial if no phantom drive
			bmi phsaveno	; serial if not present (error)
			jmp error4	; file not found error
phsaveno		jsr openi	; open IEC file
			jmp sv26	; back into sv25 (serial)
phsaveys		sty sal		; clear sal			
			dec $dd03	; switch port to output
			jsr saving	; print saving
			ldy stal	; start address lo
			lda stah	; start address hi
			sta sah		; store
			bit $dd00	; check DATA IN
			bmi phsavend	; if inactive end (error)
			sty $dd01	; write start address lo
phsavelp1		ldx $dd0d	;
			beq phsavelp1	; wait for *FLAG
			bit $dd00	; check DATA IN
			bmi phsavend	; if inactive end (error)
			sta $dd01	; write start address hi
phsavelp2		ldx $dd0d	;
			beq phsavelp2	; wait for *FLAG2
phsavecnt		lda (sal),y	; read byte from memory
			sta $dd01	; write to parallel port
phsavelp3		bit $dd00	; check for DATA IN lo
			bmi phsavend	; else end (error)
			ldx $dd0d	; check for *FLAG2
			beq phsavelp3	; else loop	
			iny 		; next byte
			bne phsavehi	;
			inc sah		; increment hi address if necessary
phsavehi		sec		; 
			tya		;
			sbc eal		; check for end address
			lda sah		;
			sbc eah		; reached
			bcs phsavend	; if so end
			bit $dc01	; check for STOP
			bmi phsavecnt	; if not pressed do next byte
phsavend		sty sal		; store lo byte last address
			lda $dd00	;
			and #$ef	; set CLOCK OUT
			sta $dd00	; 
			inc $dd03	; port B input
			bit $dd01	; toggle *PC
			jsr phread	; read byte
			asl		; shift
			lda #0		; 
			bit $dc01	; check bit 7
			cli		; irq on
			bmi phsaveok	; ok
			sec		; else error
phsaveok		rts		; back

; **** text for sys command

sysmsg 			.text "sys$"	; text for autostart

; **** jmp to enable dos routine

			*=$fff6
			.offs offset

			jmp phenabled	; enable phantom
			.byte $d5
			
; **** end of source code


	
	
	
	
	
	
	