; main asm source file
; over here will be global attributes, linker data, etc.

Start:
	SEI			; disable interrupts
	CLC : XCE	; switch to native mode
	SEP #$30	; A and XY 8-bit
	STZ $420D	; slow rom access
	STZ $420B	; \ disable any (H)DMA
	STZ $420C	; /
	LDA #$00		; disable joypad, set NMI and V/H count to 0
	STA $4200

	LDA #%10000000	; turn screen off, activate vblank
	STA $2100
	LDA #%11100000	; low byte of green
	STA $2122
	LDA #%00000000	; high byte of green
	STA $2122
	LDA #%00001111	; end vblank, setting brightness to 15
	STA $2100

	LDA #$80		; enable NMI back
	STA $4200

	CLI				; enable interrupts
	STZ $00			; set up RAM space
	LDA #$00

; Loop infinitely
-
	STA $7E002C
	STA $7E002C
	STA $7E002C
	BRL -

SomeStuff:
	INC
	JSR LabelTest

VBlank:
	BRA .type1
.type0:
	;WDM #$00
	LDX $00
	CPX #$01
	BEQ ..decrease
..increase:
	INC
	CMP #$0F
	BNE ..end
	LDX #$01
	STX $00
	BNE ..end
..decrease:
	DEC
	CMP #$00
	BNE ..end
	LDX #$00
	STX $00
..end:
	;AND #$0F
	STA $2100
	BRA .end
.type1:
	LDX $00
	CPX #$01
	BEQ +
	INC
	CMP #$0F
	BNE ++
	LDX #$01
	STX $00
	BNE ++
+
	DEC
	CMP #$00
	BNE ++
	LDX #$00
	STX $00
++
	;AND #$0F
	STA $2100
.end
	RTI


LabelTest:
.localLabel:
	INC
..c:
	CMP #10
	BPL .otherLocal
	BCC ..c
	BRA .localLabel
.otherLocal:
..b:
..c:
	BRA .localLabel
	DEC
	BEQ ..c
	BPL ..b
	ADC #$10
	RTS

db $FF,$FF,$FF,$FF
db 'n','i','c','e', 0
db "even nicer", 0

ArgumentSizeTest:
	ORA ($10,x)
	ORA $32,s
	ORA $10
	ORA [$10]
	ORA #$54
	ORA $9876
	ORA $7EDBCA
	ORA ($10),y
	ORA ($10)
	ORA ($34,s),y
	ORA $10,x
	ORA [$10],y
	ORA $9876,y
	ORA $9876,x
	ORA $7EDCBA,x

; TODO
MoveBlockTest:
	MVP $10,$20
	MVN $20,$10
