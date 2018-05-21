; main asm source file
; over here will be global attributes, linker data, etc.

Start:
	SEI			; disable interrupts
	CLC : XCE	; switch to native mode
	SEP #$30	; A and XY 8-bit
	STZ $420D	; slow rom access
	STZ $420B	; \ disable any (H)DMA
	STZ $420C	; /
	LDA $00		; disable joypad, set NMI and V/H count to 0
	STA $4200

	LDA #%10000000	; turn screen off, activate vblank
	STA $2100
	LDA #%11100000	; low byte of green
	STA $2122
	LDA #%00000000	; high byte of green
	STA $2122
	LDA #%00001111	; end vblank, setting brightness to 15
	STA $2100

	CLI
	STZ $00			; set up RAM space
	LDA #$00

; Loop infinitely
-	BRA -

VBlank:
	WDM #$00
	LDX $00
	CPX #$01
	BEQ .decrease
.increase:
	INC
	CMP #$0F
	BNE .end
	LDX #$01
	STX $00
.decrease:
	DEC
	CMP #$00
	BNE .end
	LDX #$00
	STX $00
.end:
	;AND #$0F
	STA $2100
	RTI


OtherCode:
.localLabel:
	INC
	CMP #10
	BPL .otherLocal
	BRA .localLabel
.otherLocal:
	DEC
	ADC #$10
	RTS

