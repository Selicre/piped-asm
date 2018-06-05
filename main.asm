; main asm source file
; over here will be global attributes, linker data, etc.

Start:
	SEI				; disable interrupts
	CLC : XCE		; switch to native mode
	SEP #$30		; A and XY 8-bit
	STZ $420D		; slow rom access
	STZ $420B		; \ disable any (H)DMA
	STZ $420C		; /
	LDA #$00		; disable joypad, set NMI and V/H count to 0
	STA $4200

	LDA #%10000000	; turn screen off, activate vblank
	STA $2100

	REP #$20		; turn A 16-bit

	LDA.w #Graphics01	; All of this will be in ROM instead of RAM later
	STA $00
	LDA #$0000
	STA $02
	LDA #$0000
	STA $03
	LDA #$4000
	STA $05
	LDY #1
	STY $07
	JSR LoadData
	LDA.w #Palette
	STA $00
	LDA #$0000
	STA $02
	LDA #$0000
	STA $03
	LDA #$0080
	STA $05
	LDY #0
	STY $07
	JSR LoadData
	LDA.w #BGTilemap01
	STA $00
	LDA #$0000
	STA $02
	LDA #$0040
	STA $03
	LDA #$2000
	STA $05
	LDY #1
	STY $07
	JSR LoadData
	SEP #$20		; turn A 8-bit
	
	LDA #%00000010 ; bg mode 1, 8x8 tiles
	STA $2105

	LDA #%01000001	; tilemap at 0x8000, no mirroring
	STA $2107
	LDA #%01001001	; tilemap at 0x9000, no mirroring
	STA $2108
	LDA #%01010001	; tilemap at 0xA000, no mirroring
	STA $2109

	LDA #%00000011	; enable BG1-2
	STA $212C
	LDA #%00000011	; enable BG1-2
	STA $212D

	; Clean memory
	LDX #$00
-	INX
	STZ $00,x
	CPX #$00
	BNE -


	; Set up IRQ to split the screen in two

	REP #$20
	LDA #$01FF
	STA $4209
	LDA.w #SplitIRQ
	STA $80
	SEP #$20

	LDA #%00001111	; end vblank, setting brightness to 15
	STA $2100

	LDA #%10100001	; enable NMI, IRQ & joypad
	STA $4200

	CLI				; enable interrupts
	STA $20
	JMP MainLoop

SplitIRQ:
	PHP
	SEP #$20
	LDA #$20
	STA $210D
	LDA #$00
	STA $210D
	LDA #%00000001 ; bg mode 5, 8x8 tiles
	STA $2105
	PLP
	RTI

MainLoop:
	LDA $10
	BEQ MainLoop
	CLI
	JSR RunFrame
	STZ $10
	BRA MainLoop


RunFrame:
	REP #$30
	LDA $4218
	BIT #$0F00
	BNE +
	STZ $30
+	INC $30
	;BNE ++
	LDA $30
	LSR
	LSR
	LSR
	CMP #$0004
	BMI +
	LDA #$0004
+
	CMP #$FFF8
	BPL +
	LDA #$FFF8
+
	STA $00
	LDA $4218
	BIT #$0100
	BEQ +
	TAX
	LDA $20
	SEC : ADC $00
	STA $20
	TXA
+	BIT #$0200
	BEQ +
	TAX
	LDA $20
	CLC : SBC $00
	STA $20
	TXA
+	BIT #$0400
	BEQ +
	TAX
	LDA $22
	SEC : ADC $00
	STA $22
	TXA
+	BIT #$0800
	BEQ +
	TAX
	LDA $22
	CLC : SBC $00
	STA $22
	TXA
+
++
	INC $24
	SEP #$30
	RTS


VBlank:
	; sync camera scroll values
	LDA $20
	STA $210D
	LDA $21
	STA $210D
	LDA $22
	STA $210E
	LDA $23
	STA $210E
	LDA $24
	STA $210F
	LDA $25
	STA $210F
	LDA $26
	STA $2110
	LDA $27
	STA $2110

	; finish
	LDA #$01
	STA $10
	RTI

; IRQ handler

IRQ:
	CMP $4211	; Dummy read
	JMP ($0080)



; Scratch RAM arguments:
; AA AA AA BB BB SS SS CC
; A: A bus address
; B: B bus address
; S: Size in bytes
; C: 0 - VRAM, 1 - palette

LoadData:
	PHA
	PHY
	PHP
	REP #$20
	SEP #$10
	LDA $00		; A bus address
	STA $4302
	LDY $02		; Bank
	STY $4304
	LDA $05		; Write size
	STA $4305

	LDY $07
	BEQ +
	LDA $02		; B bus address
	STA $2116
	LDY #$80	; Video port control
	STY $2115
	LDY #$01	; Word increment mode
	STY $4300
	LDY #$18	; Destination: VRAM
	STY $4301
	BRA .end

+	LDY $02		; B bus address in CGRAM
	STY $2121
	LDY $03		; B bus address in CGRAM
	STY $2121
	LDY #%00000000
	STY $4300	; 1 byte increment
	LDY #$22		; Destination: CGRAM
	STY $4301
.end:
	LDY #$01	; Turn on DMA
	STY $420B

	PLP
	PLY
	PLA
	RTS

Palette:
	dw $7eee, $7fdd, $0000, $0d71, $13ff, $1e9b, $137f, $03ff
	dw $0000, $0000, $194f, $3e78, $573e, $03ff, $7bde, $7c1f
	dw $0000, $7fdd, $0960, $01a4, $01e8, $022c, $0291, $02f5
	dw $7393, $0000, $0cfb, $2feb, $7393, $0000, $7fdd, $2d7f
	dw $0000, $7fdd, $0000, $0daf, $2e79, $25e0, $2b1c, $0320
	dw $0000, $7fff, $0000, $0320, $0016, $001f, $017f, $029f
	dw $0000, $7fdd, $0000, $2d6b, $3def, $4e73, $6318, $739c
	dw $0000, $7fff, $0000, $0320, $347d, $551e, $65ff, $7b1f

Graphics01:
	incbin "graphics.bin"

BGTilemap01:
	incbin "map4.bin"
	incbin "map3.bin"
