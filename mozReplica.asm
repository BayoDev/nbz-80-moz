; Compilation command: zasm.exe mozCopy.asm --reqcolon --dotnames --z80

;==================================
;       
;      ASSEMBLER VARIABLES
;
;==================================


;==================================
;     NC-Z ROUTINE ADDRESSES
;==================================
serialOut           equ $f970
serialIn            equ $fa09
ASCII               equ $fa10
ASCIIB              equ $fa12
ProcInputChar       equ $f9d1

;==================================
;           ASCII CODES
;==================================
asc_NULL            equ $00
asc_SPACE           equ $20
asc_BACKSPACE       equ $08 
asc_LINEFEED        equ $0A
asc_ETX             equ $03 ; END OF TEXT
asc_CR              equ $0D ; CARRIAGE RETURN
asc_AST             equ $2A ; *
asc_TAB             equ $09 
asc_MINUS           equ $2D ; -
asc_FORWARD_SLASH   equ $2F ; /
asc_LESS            equ $3c ; <
asc_APO             equ $27 ; '
asc_COL             equ $3A ; :

asc_A: equ $41
asc_B: equ $42
asc_C: equ $43
asc_D: equ $44
asc_E: equ $45
asc_F: equ $46
asc_G: equ $47
asc_H: equ $48
asc_I: equ $49
asc_J: equ $4A
asc_K: equ $4B
asc_L: equ $4C
asc_M: equ $4D
asc_N: equ $4E
asc_O: equ $4F
asc_P: equ $50
asc_Q: equ $51
asc_R: equ $52
asc_S: equ $53
asc_T: equ $54
asc_U: equ $55
asc_V: equ $56
asc_W: equ $57
asc_X: equ $58
asc_Y: equ $59
asc_Z: equ $5A

;==================================
;       SAVED REGISTERS OFFSET
;==================================
;
; These are the offsets used for easy access to the registears
; that are saved in a special stack after a breakpoint
;
AF_OFF equ $0D

BC_OFF equ $0C
DE_OFF equ $0B
HL_OFF equ $0A

AF_ALT_OFF equ $09
BC_ALT_OFF equ $08
DE_ALT_OFF equ $07
HL_ALT_OFF equ $06

IX_OFF equ $05 
IY_OFF equ $04

SP_OFF equ $03

R_OFF equ $02
I_OFF equ $01

;==================================
;       STACK POINTER VALUES
;==================================

; Default value of the STACK POINTEr at startup
STACK_POINTER        equ $01FF

; Default value of the ausiliary stack pointer used to
; save the registers after a breakpoint
BR_STACK_POINTER equ $150


;==================================
;         VARIABLES TABLE
;==================================
;   
;   Addresses of various variables
;   that are stored in RAM
;

; (2 bytes) Store a memory address
CURRENT_MEMORY_ADDR_L         equ $0070
CURRENT_MEMORY_ADDR_H         equ $0071

; (2 bytes) Store the program counter where the last breakpoint was called
BR_PC                           equ $0090 ; BREAKPOINT PC

; (2 bytes) Store the monitor stack pointer value to restore after a breakpoint
MO_ST                           equ $0092 ; Monitor SP

; (2 bytes) Store the instruction code replaced by the breakpoint to restore it later
BR_INST                          equ $0094 ; Breakpoint replaced instruction

; (3 bytes) Used to resume execution from the monitor
BR_ADDR_0                         equ $0096 ; This will contain the jump instruction ($C3)
BR_ADDR_1                         equ $0097 ; ADDRESS LOW 
BR_ADDR_2                         equ $0098 ; ADDRESS HIGH

; (7 byte) Used in the "P" and "S" command
INST_BUFFER                     equ $0099

; (2 bytes) Stores the ausiliary stack pointer used to save the registers after a breakpoint
STACK_POINTER_VALUE             equ $0080

; (2 bytes) Used as a temporary storage to help with some operations 
TMP_REG                         equ $0082
; (2 bytes) Used as a temporary storage to help with some operations
TMP_REG2                        equ $0084

; (1 byte) Used in register mode to hold the current register offset to display
CURRENT_REGISTER_OFF            equ $0086

; (2 byte) Stores the last address where the "G"(GO) instruction was issued to use with command "B"
LAST_GO_ADDR                    equ $0087

; (1 byte) Stores the number of register traces left for the multiple steps command
MULTI_STEPS_COUNT               equ $0069

; WHERE THE CODE WILL BE LOCATED IN MEMORY
org $f000
jp init

banner:
.byte  asc_LINEFEED
.byte  asc_CR
.byte  asc_TAB
.ascii "MO-Z MONITOR"
.byte  asc_ETX

;==================================
;
;            MAIN CODE
;
;==================================
init:; <---- First entry

    di
    
    ; Save registers state at startup
    call saveRegisters

    ; Initiate stack pointers
    ld sp,STACK_POINTER
    ld HL,BR_STACK_POINTER
    ld (STACK_POINTER_VALUE),HL

    ; Setup return from rst 38h interrupt
    ld  a, $C3
    ld  ($0038),a
    ld  HL,breakpointReturn
    ld  ($0039),HL

    ; Setup return from NMI (non maskable interrupt)
    ld a,$C3
    ld ($0066),a
    ld HL,breakpointReturn
    ld ($0067),HL

    ; Default multistep to not active
    ld a,$00
    ld (MULTI_STEPS_COUNT),a

    im 1
    ei

main:                   

    ; PRINT BANNER
    ld HL,banner
    call serialStringOut
    call newLine
    .postBanner:
    call newLine

    ; CURSOR
    ld c,asc_AST
    call serialOut

    call echoSerialIn

    ; OPEN MEMORY MODE
    cp asc_O
    jp nz, .endOpenMemoryMode
    .startOpenMemoryMode:

        ; O-
        call newLine
        ld c,asc_O
        call serialOut

        ; "-"
        call outMinus

        ; INPUT ADDRESS XXXX
        call readAddress
        ld hl,BC
        ld (CURRENT_MEMORY_ADDR_L),hl

        ; INPUT ACTION
        call serialIn
        ld a,c

        ; CASE '/'
        cp asc_FORWARD_SLASH
        jp nz,.endOMMprint
        .startOMMprint:

            ld c,asc_FORWARD_SLASH
            call serialOut
            .OMMprintSKIPSLASH:
            ; "-"
            call outMinus

            ; PRINT ADDRESS CONTENT XX
            ld HL,(CURRENT_MEMORY_ADDR_L)
            call ASCII

            ; "-"
            call outMinus

            ; INPUT ACTION
            .OMMprintINPUT:
            call serialIn
            ld a,c


            ; "RETURN"
            cp a,asc_CR
            jp z,.startOMMreturn
            cp a,asc_BACKSPACE
            jp nz,.endOMMreturn
            .startOMMreturn:
                ; OUTPUT 'O'
                jp .startOpenMemoryMode
            .endOMMreturn:

            ; "LINE FEED"
            cp a,asc_LINEFEED
            jp nz, .endOMMlinefeed
            .startOMMlinefeed:
                ld HL,(CURRENT_MEMORY_ADDR_L)
                inc HL
                ld (CURRENT_MEMORY_ADDR_L),HL
            .OMMlinefeedSKIP:
                push hl
                call newLine
                ld b,h
                call ASCIIB
                pop hl
                ld b,l
                call ASCIIB
                jp .startOMMprint
            .endOMMlinefeed:

            ; "<"
            cp a,asc_LESS
            jp nz,.endOMMless
            .startOMMKless:
                ld HL,(CURRENT_MEMORY_ADDR_L)
                dec HL
                ld (CURRENT_MEMORY_ADDR_L),HL
                jp .OMMlinefeedSKIP
            .endOMMless:

            ; "T"
            cp a,asc_T
            jp nz,.endOMMtrace
            .startOMMtrace:

                ld c,asc_T
                call serialOut
                call outMinus

                ld a,$0F
                .OMMtraceLOOP:
                    push af

                    ld HL,(CURRENT_MEMORY_ADDR_L)
                    inc HL
                    ld (CURRENT_MEMORY_ADDR_L),HL

                    call ASCII
                    call outMinus

                    pop af
                    dec a
                    cp $00
                    jp nz,.OMMtraceLOOP
                jp .startOMMlinefeed
            .endOMMtrace:

            ; "Z"
            cp a,asc_Z
            jp nz,.endOMMzero
            .startOMMzero:

                ld c,asc_Z
                call serialOut

                ; XXXX
                call newLine
                ld HL,(CURRENT_MEMORY_ADDR_L)
                ld b,H
                call ASCIIB
                ld b,L
                call ASCIIB
                ; /
                ld c,asc_FORWARD_SLASH
                call serialOut

                ; DECREMENT SO THAT LOOP WORKS
                ld HL,(CURRENT_MEMORY_ADDR_L)
                dec HL
                ld(CURRENT_MEMORY_ADDR_L),HL

                ld a,$10
                .OMMzeroLOOP:
                    push af

                    call outMinus

                    ld HL,(CURRENT_MEMORY_ADDR_L)
                    inc HL
                    ld (HL),$00
                    ld (CURRENT_MEMORY_ADDR_L),HL

                    call ASCII

                    pop af
                    dec a
                    cp $00
                    jp nz,.OMMzeroLOOP
                jp .startOMMlinefeed
            .endOMMzero:

            ; ADD OTHER POSSIBILITIES

            ; IF NONE ARE MET THEN CHANGE MEMORY 

            ; print back first hex value
            ld c,a
            push bc
            call serialOut
            pop bc

            call readHexSkipFirst

            ld a,c

            ld hl, (CURRENT_MEMORY_ADDR_L)
            ld (HL),a

            ld c,asc_MINUS
            call serialOut
            jp .OMMprintINPUT

        
        .endOMMprint:

        ; CASE 'B'
        cp asc_B
        jp nz,.endOMMbreakpoint
        .startOMMbreakpoint:
            ld c,asc_B
            call serialOut

            ld hl,(CURRENT_MEMORY_ADDR_L)
            call setBreakpoint
            
            jp .startOpenMemoryMode
        .endOMMbreakpoint:

        cp asc_C
        jp nz,.endOMMnewBR
        .startOMMnewBR:
            ld c,asc_C
            call serialOut
            
            ld hl,(CURRENT_MEMORY_ADDR_L)
            call setBreakpoint
            ld HL,(BR_PC)
            ld (CURRENT_MEMORY_ADDR_L),HL
            jp .OMMgoSKIP2
        .endOMMnewBR:

        ; CASE 'G'
        cp asc_G
        jp nz,.endOMMgo
        .startOMMgo:
            ld c,asc_G
            call serialOut

            .OMMgoSKIP2:
            ld HL,(CURRENT_MEMORY_ADDR_L)
            ld (LAST_GO_ADDR),HL

            .OMMgoSKIP:
            ld a,$c3
            ld (BR_ADDR_0),a
            ld HL,(CURRENT_MEMORY_ADDR_L)
            ld (BR_ADDR_1),HL
            jp restoreRegisters
        .endOMMgo:

    .endOpenMemoryMode:

    ; REGISTER MODE
    cp asc_R
    jp nz, .endRegisterMode
    .startRegisterMode:
        
        ; "R-"
        call newLine
        ld c,asc_R
        call serialOut
        call outMinus

        ; Load register offset to check if it's empty later
        ld hl,CURRENT_REGISTER_OFF
        ld (HL),$FF

        call echoSerialIn
        ld a,c
        
        cp asc_A
        jp nz, .endRMAF
        .startRMAF:

            ld c,asc_F
            call serialOut

            call echoSerialIn
            ld a,c

            cp asc_APO
            jp nz, .endRMAFapo
            .startRMAFapo:
                ld hl,CURRENT_REGISTER_OFF
                ld (HL),AF_ALT_OFF
            .endRMAFapo:

            cp asc_FORWARD_SLASH
            jp nz, .endRMAFslash
            .startRMAFslash:
                ld hl,CURRENT_REGISTER_OFF
                ld (HL),AF_OFF
            .endRMAFslash:

        .endRMAF:

        cp asc_B
        jp nz, .endRMBCend
        .startRMBCstart:

            ld c,asc_C
            call serialOut

            call echoSerialIn
            ld a,c

            cp asc_APO
            jp nz, .endRMBCapo
            .startRMBCapo:
                ld hl,CURRENT_REGISTER_OFF
                ld (HL),BC_ALT_OFF
            .endRMBCapo:

            cp asc_FORWARD_SLASH
            jp nz, .endRMBCslash
            .startRMBCslash:
                ld hl,CURRENT_REGISTER_OFF
                ld (HL),BC_OFF
            .endRMBCslash:

        .endRMBCend:

        cp asc_D
        jp nz, .endRMDE
        .startRMDE:
            ld c,asc_E
            call serialOut

            call echoSerialIn
            ld a,c

            cp asc_APO
            jp nz, .endRMDEapo
            .startRMDEapo:
                ld hl,CURRENT_REGISTER_OFF
                ld (HL),DE_ALT_OFF
            .endRMDEapo:

            cp asc_FORWARD_SLASH
            jp nz, .endRMDEslash
            .startRMDEslash:
                ld hl,CURRENT_REGISTER_OFF
                ld (HL),DE_OFF
            .endRMDEslash:

        .endRMDE:

        cp asc_H
        jp nz, .endRMHL
        .startRMHL:
            ld c,asc_L
            call serialOut

            call echoSerialIn
            ld a,c

            cp asc_APO
            jp nz, .endRMHLapo
            .startRMHLapo:
                ld hl,CURRENT_REGISTER_OFF
                ld (HL),HL_ALT_OFF
            .endRMHLapo:

            cp asc_FORWARD_SLASH
            jp nz, .endRMHLslash
            .startRHLFslash:
                ld hl,CURRENT_REGISTER_OFF
                ld (HL),HL_OFF
            .endRMHLslash:

        .endRMHL:

        cp asc_I
        jp nz, .endRMI
        .startRMI: 
            call echoSerialIn
            ld a,c

            cp asc_X
            jp nz, .endRMIXapo
            .startRMIXapo:
                ld hl,CURRENT_REGISTER_OFF
                ld (HL),IX_OFF
            .endRMIXapo:

            cp asc_Y
            jp nz, .endRMIYslash
            .startRMIYslash:
                ld hl,CURRENT_REGISTER_OFF
                ld (HL),IY_OFF
            .endRMIYslash:

            cp asc_FORWARD_SLASH
            jp nz, .endRMIslash
            .startRMIslash:
                ld HL,CURRENT_REGISTER_OFF
                ld (HL),I_OFF
            .endRMIslash:
        .endRMI:

        cp asc_S
        jp nz, .endRMSP
        .startRMSP:

            ld c,asc_P
            call serialOut

            call echoSerialIn
            ld a,c

            ld hl,CURRENT_REGISTER_OFF
            ld (HL),SP_OFF

        .endRMSP:

        cp asc_T
        jp nz, .endRMtrace
        .startRMtrace:
            call printRegistersTrace
            jp .startRegisterMode
        .endRMtrace:

        cp asc_Z
        jp nz, .endRMreset
        .startRMreset:
            ld a,$0D
            .RMresetLOOP:

                ; SKIP R register
                cp R_OFF
                jp z,.RMresetSKIP
                cp SP_OFF
                jp z,.RMresetSKIP

                push af
                ld BC,$0000
                call setStoredRegister
                pop af

                .RMresetSKIP:
                dec A
                cp $00
                jp nz,.RMresetLOOP

            jp .startRMtrace
        .endRMreset:

        ; Check if it wasnt right
        ld hl,CURRENT_REGISTER_OFF
        ld a,(HL)
        cp $FF
        jp z,.endRegisterPrint
        .startRegisterPrint:
            ; -XXYY-
            call outMinus
            ld hl,CURRENT_REGISTER_OFF
            ld a,(HL)
            call printStoredRegister
            call outMinus
            ; Input new value
            .regPrintInput:
            call readAddress
            ld hl,CURRENT_REGISTER_OFF
            ld a,(HL)
            call setStoredRegister
            call outMinus
            jp .regPrintInput
        .endRegisterPrint:

    .endRegisterMode:

    ; BREAKPOINT SEQUENCE REPEAT
    cp asc_B
    jp nz, .endBreakRepeat
    .startBreakRepeat:
        ld hl,(BR_PC)
        call setBreakpoint
        ld HL,(LAST_GO_ADDR)
        LD (CURRENT_MEMORY_ADDR_L),HL
        jp .OMMgoSKIP
    .endBreakRepeat:

    ; PROCEED
    cp asc_P
    jp nz, .endProceed
    .startProceed:
        ; Load the program's PC into HL
        ld HL,(BR_PC)
        ; Find out the length of the instruction stored in (PC)
        call getInstructionSize
        ; Copy instruction into buffer
        LD HL,(BR_PC)
        LD DE,INST_BUFFER
        .proceedLOOP:
            
            push af
            push HL

            LD a,(HL)
            LD (DE),a

            pop HL
            pop af

            INC HL
            INC DE
            dec A
            cp $00
            jp nz,.proceedLOOP

        ; Sets jump to next instruction after PC after instruction in the buffer
        ld a,$c3
        ld (DE),a
        INC DE
        ld a,L
        ld (DE),a
        INC DE
        ld a,H
        ld (DE),a
        
        ; Set breakpoint back
        LD HL,(BR_PC)
        call setBreakpoint
        
        ld HL,INST_BUFFER
        ld (CURRENT_MEMORY_ADDR_L),HL
        LD HL,(BR_PC)
        LD (LAST_GO_ADDR),HL

        jp .OMMgoSKIP
    .endProceed:

    ; SINGLE STEP
    cp asc_S
    jp nz,.endSingleStep
    .startSingleStep:
        ; Load the program's PC into HL
        ld HL,(BR_PC)
        ; Find out the length of the instruction stored in (PC)
        call getInstructionSize

        ld HL,(BR_PC)
        .ssLoop:
            inc HL
            dec a
            cp $00
            jp nz,.ssLoop
        
        call setBreakpoint
        ld HL,(BR_PC)
        ld (CURRENT_MEMORY_ADDR_L),HL
        jp .OMMgoSKIP2
    .endSingleStep:

    cp asc_T
    jp nz,.endMultiStep
    .startMultiStep:
        
        call echoSerialIn
        call asciiToHex

        ld a,e
        ld (MULTI_STEPS_COUNT),a

        jp .startSingleStep

    .endMultiStep:

    cp asc_L
    jp nz,.endLowSpeed
    .startLowSpeed:
        ld HL,$0200
        .notCOL:
            ld e,$00
        .lsLOOP:
            call noParitySerialIn
            ld (HL),c
            inc HL
            ld a,c
            cp asc_COL
            jp nz,.notCOL
            ld a,e
            cp $02
            jp z,.lsENDLOOP
            inc e
            jp .lsLOOP
        .lsENDLOOP:
        jp .postBanner
    .endLowSpeed:

    jp .postBanner


;========================================
;
;           AUXILIARY FUNCTIONS
;
;========================================

; Get the length of the instruction
;
; Input: 
;   - HL ~ The address where the firs byte of the instruction is
; Output:
;   - A ~ The length of the instruction
;
getInstructionSize:
    ld A,(HL)
    
    cp $40
    ; Jump if A<$40
    jp c,.topFirstSection
    cp  $C0
    ; Jump if A>=$C0
    jp nc,.bottomFirstSection

    .oneByte:
    ld A,$01
    ret

    .twoByte:
    ld A,$02
    ret

    .threeByte:
    ld A,$03
    ret

    .fourByte:
    ld A,$04
    ret

    .topFirstSection:

        cp $00
        jp z,.oneByte

        and $0F
        cp $00
        jp z,.twoByte
        ld A,(HL)

        and $0F
        cp $01
        jp z,.threeByte
        ld A,(HL)

        cp $22
        jp z,.threeByte

        cp $32
        jp z,.threeByte

        and $0F
        cp $06
        jp z,.threeByte
        ld A,(HL)

        cp $2A
        jp z,.threeByte

        cp $3A
        jp z,.threeByte

        and $0F
        cp $0E
        jp z,.twoByte
        ld A,(HL)

        jp .oneByte

    .bottomFirstSection:
        cp $CB
        jp z,.cbSection
        cp $DD
        jp z,.ddSection
        cp $ED
        jp z,.edSection
        cp $FD
        jp z,.fdSection

        and $0F
        cp $02
        jp z,.threeByte
        ld A,(HL)

        cp $c3
        jp z,.threeByte

        cp $d3
        jp z,.twoByte

        and $0F
        cp $04
        jp z,.threeByte
        ld A,(HL)

        and $0F
        cp $06
        jp z,.twoByte
        ld A,(HL)

        and $0F
        cp $0A
        jp z,.threeByte
        ld A,(HL)

        cp $DB
        jp z,.twoByte

        and $0F
        cp $0C
        jp z,.threeByte
        ld A,(HL)

        cp $CD
        jp z,.threeByte

        and $0F
        cp $0E
        jp z,.twoByte
        ld A,(HL)

        jp .oneByte

    .cbSection:
        jp .twoByte
    .ddSection:
        inc hl
        ld a,(HL)

        cp $CB
        jp z,.fourByte

        cp $21
        jp z,.fourByte

        cp $22
        jp z,.fourByte

        cp $34
        jp z,.threeByte

        cp $35
        jp z,.threeByte

        and $0F
        cp $06
        jp z,.threeByte
        ld A,(HL)

        and $F0
        cp $70
        jp z,.threeByte
        ld A,(HL)

        and $0F
        cp $0E
        jp z,.threeByte
        ld A,(HL)

        cp $2A
        jp z,.fourByte

        jp .twoByte

    .edSection:
        inc hl
        ld a,(HL)

        and $F0
        cp $A0
        jp z,.twoByte
        ld A,(HL)

        and $F0
        cp $B0
        jp z,.twoByte
        ld A,(HL)

        and $0F
        cp $03
        jp z,.fourByte
        ld A,(HL)

        and $0F
        cp $0B
        jp z,.fourByte

        jp .twoByte

    .fdSection:
    
        inc hl
        ld a,(HL)

        cp $CB
        jp z,.fourByte

        cp $21
        jp z,.fourByte

        cp $22
        jp z,.fourByte

        cp $34
        jp z,.threeByte

        cp $35
        jp z,.threeByte

        and $0F
        cp $06
        jp z,.threeByte
        ld A,(HL)

        and $F0
        cp $70
        jp z,.threeByte
        ld A,(HL)

        and $0F
        cp $0E
        jp z,.threeByte
        ld A,(HL)

        cp $2A
        jp z,.fourByte

        jp .twoByte
.endISTsize:

; Output a minus on the serial port "-"
outMinus:
    ; "-"
    ld c,asc_MINUS
    call serialOut
    ret

; Print all of the registers content in the following order
; "-AF-HL-DE-BC-AF'-HL'-DE'-BC'-IX-IY-SP-I"
printRegistersTrace:
    ld a,$0D
    .RMtraceLOOP:

        ; SKIP R register
        cp R_OFF
        jp z,.RMtraceSKIP

        push af
        call outMinus
        pop af
        push af
        call printStoredRegister
        pop af

        .RMtraceSKIP:
        dec A
        cp $00
        jp nz,.RMtraceLOOP
    ret

; Inputs a serial character and echoes it
;
; Output:
;   - C ~ The character that was entered
;
echoSerialIn:
    call serialIn
    push bc
    push af
    call serialOut
    pop af
    pop bc
    ret

; Read two ascii values from serial and convert them to hex
;
; Output:
;   - C ~ The hex value
;
readHex:
    call echoSerialIn
    ld a,c
    cp asc_BACKSPACE
    jp z, .postBanner
    cp asc_CR
    jp z, .postBanner
readHexSkipFirst:
    call asciiToHex
    ld a,e
    rlca
    rlca
    rlca
    rlca
    and $F0
    push af
    call echoSerialIn
    call asciiToHex
    ld a,e
    and $0F
    ld c,a
    pop af
    or c
    ld c,a
    ret

; Read four ascii values from serial and convert them to two hex values
; 
; Output:
;   - BC ~ The two hex values
;
readAddress:
    call readHex
    ld a,c
    push af
    call readHex
    pop af
    ld b,a
    ret

; Output to serial to go to the start of a new line
newLine:
    ld c, asc_LINEFEED
    call serialOut
    ld c,asc_CR
    call serialOut
    ret

; Write the string located at (HL) to the serial output
; stops when the "End of text"(EXT) is met
serialStringOut:
    ; Load character into register A
    ld A, (HL)
    ; Check for end of text
    cp asc_ETX
    jp z,.serialStringOutEnd
    ; Send character
    ld c,a
    call serialOut
    ; Repeat loop
    inc HL
    jp serialStringOut
    .serialStringOutEnd:
    ret

; Inputs a serial byte without the parity bit
;
; Output:
;   - C ~ The data
;
noParitySerialIn:
    call ProcInputChar
    LD C,A
    RET
    ret

; This routine is called when returning from a breakpoint
breakpointReturn:
    ; Save PC
    LD (TMP_REG),HL
    LD (TMP_REG2), A
    POP HL
    DEC HL
    LD (BR_PC),HL

    ; Restore instruction
    LD A,(BR_INST)
    LD (HL),A

    LD HL,(TMP_REG)
    LD A,(TMP_REG2)
    ; Save registers state
    call saveRegisters
    ; Restore SP
    LD SP, (MO_ST)

    ; "B-"
    call newLine
    LD C,asc_B
    call serialOut
    CALL outMinus

    ; "XXXX"
    LD HL,(BR_PC)
    LD B,H
    CALL ASCIIB
    LD B,L
    CALL ASCIIB

    ; Check for multi steps

    ld a,(MULTI_STEPS_COUNT)
    cp $00
    jp z,.notMultiStep

    dec a
    ld (MULTI_STEPS_COUNT),A

    call printRegistersTrace

    ld a,(MULTI_STEPS_COUNT)
    cp $00
    jp z,.postBanner

    jp .startSingleStep

    .notMultiStep:


    ;"-AAFF"
    CALL outMinus
    LD A,AF_OFF
    call printStoredRegister
    jp .postBanner

; Print the content of a register that was saved after a breakpoint
;
; Input:
;   - A ~ The offset of the register to print
;
printStoredRegister:
    call getStoredRegister
    push bc
    call ASCIIB
    pop bc
    ld b,c
    call ASCIIB
    ret

; Set the content of a register that was saved after a breakpoint
;
; Input:
;   - A  ~ The offset of the register
;   - BC ~ The new value of the register
;
setStoredRegister:
    LD HL,(STACK_POINTER_VALUE)
    .setStoredRegisterLOOP: 
        INC HL
        INC HL
        DEC A
        cp $00
        jp nz, .setStoredRegisterLOOP
    LD (HL),C
    DEC HL
    LD (HL),B
    ret

; Get the content of a register that was saved after a breakpoint
;
; Input:
;   - A  ~ The offset of the register
;   - BC ~ The value of the register
;
getStoredRegister:
    ; A offset
    ; BC OUTPUT
    LD HL,(STACK_POINTER_VALUE)
    .getStoredRegisterLOOP: 
        INC HL
        INC HL
        DEC A
        cp $00
        jp nz, .getStoredRegisterLOOP
    LD C,(HL)
    DEC HL
    LD B,(HL)
    ret


; Set a breakpoint
;
; Input:
;   - HL ~ The address where the breakpoint will be set
;
setBreakpoint:

    ; Save previous instruction
    ld a,(HL)
    ld (BR_INST),A

    ; Set interrupt 38h instruction
    ld (HL),$FF

    ret    

; Save the registers state to a new stack
saveRegisters:

    ; Save old sp value into TMP_REG and 
    ; REMEMBER TO INIT (STACK_POINTER_VALUE) !!!!
    ld (TMP_REG),sp
    ld sp,(STACK_POINTER_VALUE)

    ; Save all the registers
    PUSH AF

    PUSH BC
    PUSH DE
    PUSH HL
    EXX
    PUSH AF
    PUSH BC
    PUSH DE
    PUSH HL
    EXX
    PUSH IX 
    PUSH IY

    ; SAVE SP
    ld hl,(TMP_REG)
    PUSH HL

    ; Save IR
    LD A,R
    PUSH AF
    LD A,I
    PUSH AF

    ld (STACK_POINTER_VALUE),sp
    ld sp,(TMP_REG)

    ret


; Restore the register to the last saved values
restoreRegisters:

    ; Save old sp value into TMP_REG and 
    ; REMEMBER TO INIT (STACK_POINTER_VALUE) !!!!
    ld (TMP_REG),sp
    ld sp,(STACK_POINTER_VALUE)

    ; Restore all the registers

    ; Restore IR
    POP AF
    LD I,A
    POP AF
    LD R,A

    ; Restore SP
    POP HL
    LD (TMP_REG),HL

    POP IY
    POP IX
    EXX
    POP HL
    POP DE
    POP BC
    POP AF
    EXX
    POP HL
    POP DE
    POP BC

    POP AF

    ld (STACK_POINTER_VALUE),sp
    ld sp,(TMP_REG)
    jp BR_ADDR_0


; COPIED FROM THE NC-Z DISASSEMBLY FROM 
; https://forum.vcfed.org/index.php?threads%2Ffree-archive-for-all-the-single-board-computers-i-have-worked-on-over-the-years.1209293%2F&fbclid=IwAR3RmjEW2PSuuJrQKGIjNnd5Bq9_CEkcLw41sPZ6MJr9yXIY1EWahW_opWI

asciiToHex:            ; TAKE C AND PUT IN E
            LD      A,C
            CP      $41
            JP      M,.skip
            ADD     A,$F9
    .skip:  AND     $0F
            LD      E,A
            RET

.byte asc_COL
.byte asc_COL
.byte asc_COL