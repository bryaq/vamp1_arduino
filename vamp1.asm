;vamp1.asm
;guest openmusiclabs
;MIDIvampire-I firmware
;
;this is the main file for the midi synth.  it sets up the micro, and takes
;in midi data, and modifies parameters accordingly.  all of the voicing and
;modulation is done in the interrupts, for which there are seperate files.
;
;basic operation:
;
;1. there is no midi stack, data is handled as it arrives.  so there must be
;enough free time leftover after interrupt handling to do midi.  if you get
;more than a byte or 2 backlogged, you will get buffer over-runs.  the usart
;is 3 bytes deep, max.
;
;2. there is a 31.25kHz sample clock that processs the voicing data.  it must
;complete its task in under ~200 clock cycles, or there wont be time for midi
;and the other interrupts.  this has a 1 sample buffer, so 1 interrupt can be
;missed.
;
;3. there is a 244Hz interrupt that does attack, decay, and frequency sweep.
;a second interrupt 90 degrees out of phase does the pwm sweep and fm.
;
;5. note frequencies are stored in a 16b lookup table.
;
;6. there are bit-crush, and feedback effects. the bit-crush is also stored
;in a lookup table.
;
;7. there are 8 different voices.  the first voice is a pwm synth, with
;variable pwm sweep decay, and max pwm value.  this gives an effective
;vcf style cutoff and sweep.  the other 7 voices are 2k signed value lookup
;tables.  these can be swapped out quite easily.
;
;MIDI implementation
;
;cc #	parameter
;----	---------
; 10	voice selection
; 11	attack
; 12	decay
; 13	frequency sweep
; 14	fm depth
; 15	fm rate
; 16	bit-crush
; 17	feedback
; 18	pwm "cutoff"
; 19	pwm decay
; 20	am rate


.include "m328pdef.inc" ; definitions file

; fuse settings
;
; external 8MHz resonator (low power oscillator), slowest startup time
; isp enabled
; brownout @ 1.8V
; no boot reset
; no memory locks
; LOW = 0xFF
; HIGH = 0xD9
; EXTENDED = 0xFE

; pin usage
;
; pb0   = nc
; pb1   = high byte - pwm out, oc1a, t1, output
; pb2   = low byte - pwm out, oc1b, t1, output
; pb3:5 = spi - isp
; pb6:7 = ceramic resonator, 8MHz
; pc0:3 = midi channel select 0:3, input, pullup on
; pc4:5 = nc
; pc6   = reset - isp
; pd0   = midi rx, usart0, pullup on
; pd1:6 = nc
; adc6  = nc
; adc7  = nc
; aref  = nc

; register usage
;
; r0  = multiply result lsb - do not use in main (not backed up)
; r1  = multiply result msb - do not use in main (not backed up)
; r2  = voice 1 frequency lsb
; r3  = voice 1 frequency msb
; r4  = voice 2 frequency lsb
; r5  = voice 2 frequency msb
; r6  = voice 3 frequency lsb
; r7  = voice 3 frequency msb
; r8  = voice 4 frequency lsb
; r9  = voice 4 frequency msb
; r10 = null register
; r11 = am frequency counter
; r12 = voice 1 amplitude
; r13 = voice 2 amplitude
; r14 = voice 3 amplitude
; r15 = voice 4 amplitude
; r16 = main general swap register - main only
; r17 = main general swap register - main only
; r18 = interrupt general swap register - interrupts only
; r19 = interrupt general swap register - interrupts only
; r20 = interrupt general swap register - interrupts only
; r21 = interrupt general swap register - interrupts only
; r22 = am value
; r23 = envelope state
; r24 = trigger
; r25 = current voice
; r26 = interrupt general swap register - interrupts only
; r27 = interrupt general swap register - interrupts only
; r28 = delay table pointer lsb - synth-engine only
; r29 = delay table pointer msb - synth-engine only
; r30 = lookup table address lsb - backed up in interrupts
; r31 = lookup table address msb - backed up in interrupts
; gpior0 = pwm buffer lsb
; gpior1 = pwm buffer msb
; gpior2 = stack for interrupts
; eedr = stack for interrupts
; eearl = stack for interrupts


; sram usage
;
; $0100 - $07ff = delay memory
; $0800 - $08ef = variable storage
; $08f0 - $08ff = stack

.include "vamp1_defs.inc" ; definitions file with sram usage


.org $0000
rjmp init ; initialization
.org OVF1addr
rjmp t1int ; sample playback handler
.org OC0Aaddr
rjmp t0int_a
.org OC0Baddr
rjmp t0int_b


init: ; stuff done only on reset

;move stack pointer to end of sram
ldi r16,high(ramend)
out sph,r16
ldi r16,low(ramend)
out spl,r16

;setup power reduction registers
ldi r16,(1<<prtim2)|(1<<prspi)|(1<<pradc)|(1<<prtwi)
sts prr,r16 ; turn off twi,spi,adc,t2

;initialize io ports
ldi r16,(1<<ddb1)|(1<<ddb2) ; set pwms to output
out ddrb,r16
sbi portd,pd0 ; turn on pullups for usart0 rx
ldi r16,(1<<ddc3)|(1<<ddc2)|(1<<ddc1)|(1<<ddc0) ; turn on pullups for midi channel switches
out portc,r16

;setup t1 for audio generation
;8b phase correct pwm, clear output on match, use both oca and ocb
;ck/1 (31.25khz), top @ $80
ldi r16,(1<<com1a1)|(0<<com1a0)|(1<<com1b1)|(0<<com1b0)|(0<<wgm11)|(0<<wgm10)
sts tccr1a,r16 ; phase and frequency correct, icr1 as top
ldi r16,(1<<wgm13)|(0<<wgm12)|(1<<cs00)
sts tccr1b,r16 ; Fcpu/1, update ocr at bottom, overflow at bottom
ldi r16,$00
sts tccr1c,r16
ldi r16,$00
sts icr1h,r16 ; set icr1 to top, $0080
ldi r16,$80
sts icr1l,r16
ldi r16,$00
sts ocr1ah,r16 ; set ocr1a to midrange, $003f
ldi r16,$3f
sts ocr1al,r16
ldi r16,$00
sts ocr1bh,r16 ; set ocr1b to midrange, $003f
ldi r16,$3f
sts ocr1bl,r16
ldi r16,(1<<toie1) ; turn on overflow interrupt
sts timsk1,r16

;setup t0 for fm, envelopes, cutoff sweep
;oca and ocb every 8000000/256/128 = 244Hz
ldi r16,(1<<wgm01)|(0<<wgm00) ; ctc mode
out tccr0a,r16
ldi r16,(0<<wgm02)|(4<<cs00) ; top = ocra, ck = Fcpu/256
out tccr0b,r16
ldi r16,$7f ; set top to $007f
out ocr0a,r16
ldi r16,$3f ; set mid to $003f
out ocr0b,r16
ldi r16,(1<<ocie0b)|(1<<ocie0a)
sts timsk0,r16 ; turn on top and mid interrupts

;setup usart0 for midi recieve
;8 data bits, 1 stop bit, no parity, 31.25kbps
ldi r16,(1<<rxen0)
sts ucsr0b,r16
ldi r16,(1<<ucsz01)|(1<<ucsz00)
sts ucsr0c,r16
ldi r16,0x0f ; 8MHz/31.25kHz/16 - 1
sts ubrr0l, r16

;fetch midi address
in r16,pinc
andi r16,$0f
sts midi_channel,r16

;initialize variables
;specific purpose registers
clr r10 ; clear the null register
clr r12 ; set all amplitudes to 0
clr r13
clr r14
clr r15
clr r23 ; shut off all voices - ramp
clr r24 ; shut off all triggers
ldi r25,$10 ; initialize voice to pwm
clr r28
ldi r29,$01 ; prep delay pointer

;setup control parameters
ser r16
sts voice1,r16 ; shut off all voices
sts voice2,r16
sts voice3,r16
sts voice4,r16
clr r16
sts midi_state,r16 ; set to data waiting
sts last_byte,r16 ; reset midi recieve buffer

;setup voices
ldi r16,$ff
sts attack,r16 ; set attack
ldi r16,$06
sts decay,r16 ; set decay
clr r16
sts sweep_l,r16 ; set sweep to off
sts sweep_h,r16
ldi r16,$ff
sts bit_crush_l,r16 ; set bit_crush to off
sts bit_crush_h,r16
ldi r16,$80
sts pwm_depth1,r16 ; initialize pwm cutoff to mid
sts pwm_depth2,r16
sts pwm_depth3,r16
sts pwm_depth4,r16
ldi r16,$04
sts freq_decay,r16 ; initialize frequency decay
ldi r16,$28
sts cut_off,r16; initialize cut-off frequency to mid
sts cut_off_rev,r16
sts pwm_start,r16
ldi r16,$ff
sts feedback,r16 ; initialize feedback to off
ldi r16,$00
sts fm_mod_l,r16 ; set fm_mod to off
sts fm_mod_h,r16
ldi r16,$80
sts fm_freq,r16 ; set fm frequency to slowest
clr r16
sts am_freq,r16 ; set am to off

;enable interrupts
ldi r16,$07
out tifr0,r16 ; clear any pending interrupts
out tifr1,r16 ; clear any pending interrupts
sei ; turn on interrupts


main: ; non-interrupt code - process midi data

;check if data is in the rx buffer
lds r16,ucsr0a
sbrs r16,rxc0
rjmp main ; keep checking

;process incoming data
lds r17,udr0 ; store quickly in case another byte is arriving
sbrs r17,$07 ; check if command byte
rjmp data_byte
lds r16,midi_channel
ori r16,$80
cp r17,r16 ; $80 + midi channel
breq note_off
ori r16,$90
cp r17,r16 ; now $90 + midi channel
breq note_on
ori r16,$b0
cp r17,r16 ; now $b0 + midi channel - this only works because of the order
breq control
cpi r17,$f8 ; check if a realtime message
brsh main ; keep same status if realtime message
clr r16 ; if none of the above, set midi state to 0
sts midi_state,r16
rjmp main ; go back to checking

note_off: ; set to note_off state

ldi r16,$02
sts midi_state,r16
rjmp main

note_on: ; set to note_on state

ldi r16,$01
sts midi_state,r16
rjmp main

control: ; set to control state

ldi r16,$04
sts midi_state,r16
rjmp main

data_byte: ; put data in the right place

lds r16,midi_state
sbrc r16,$07 ; check if second_byte in sequence
rjmp second_byte
tst r16 ; check if null state
breq main ; skip if not one of the used states
sts last_byte,r17 ; backup data
ori r16,$80 ; set second_byte bit
sts midi_state,r16
rjmp main ; go back to waiting

second_byte: ; process data

andi r16,$7f ; clear second_byte bit
sts midi_state,r16
sbrc r16,$00
rjmp note_on_process
sbrc r16,$01
rjmp note_off_process
sbrc r16,$02
rjmp control_process
clr r16 ; if none of the above, error
sts midi_state,r16
rjmp main ; return to waiting

note_on_process: ; set notes

tst r17 ; check if note_off velocity (0)
brne note_on_process1 ; do note on if not

;note off process
;every voice is checked in case 2 are on, and only 1 note-off occurs
lds r16,last_byte ; fetch note number
lds r17,voice1 ; fetch voice currently playing
cp r16,r17 ; see if the note off was for that voice
brne note2_off_0 ; turn off that voice if so
ori r23,$03 ; set voice1 to decay
ser r17
sts voice1,r17 ; set voice off

note2_off_0: ; check voice2

lds r17,voice2
cp r16,r17
brne note3_off_0
ori r23,$0c ; set voice2 to decay
ser r17
sts voice2,r17 ; set voice off

note3_off_0:

lds r17,voice3
cp r16,r17
brne note4_off_0
ori r23,$30 ; set voice3 to decay
ser r17
sts voice3,r17 ; set voice off

note4_off_0:

lds r17,voice4
cp r16,r17
brne note_off_done_0
ori r23,$c0 ; set voice4 to decay
ser r17
sts voice4,r17 ; set voice off

note_off_done_0:

rjmp main ; if no matches, do nothing


note_on_process1: ; do note turnon here

cli ; block in case r23 is modded in envelope engine
mov r17,r23 ; make backup of envelope state
lsr r17
or r17,r23 ; check if any voices are off
sei ; unblock - no external state transition from off state
sbrs r17,$00 ; check if voice1 is off
rjmp voice1_on0
sbrs r17,$02 ; check if voice2 is off
rjmp voice2_on0
sbrs r17,$04 ; check if voice3 is off
rjmp voice3_on0
sbrs r17,$06 ; check if voice4 is off
rjmp voice4_on0
cli ; block
mov r17,r23 ; make backup of envelope state
lsr r17
and r17,r23 ; check if any voices are decaying
sei ; unblock - can go from decay to off externally, but thats ok to steal
sbrc r17,$00 ; check if voice1 is decaying
rjmp voice1_on
sbrc r17,$02 ; check if voice2 is decaying
rjmp voice2_on
sbrc r17,$04 ; check if voice3 is decaying
rjmp voice3_on
sbrs r17,$06 ; check if voice4 is decaying
rjmp main ; no notes decaying or off - dont do anything

voice4_on: ; turn on voice4

lds r30,last_byte ; fetch note from lookup table
sts voice4,r30 ; store note number for turnoff sequence
lsl r30 ; adjust for 2byte fetch
ldi r31,$0c
lpm r16,z+ ; load it into the frequency register
lpm r17,z
cli ; block
sts freq4_l,r16
sts freq4_h,r17
lds r16,pwm_start
sts pwm_depth4,r16 ; reset pwm depth
andi r23,$3f
ori r23,$40 ; set voice4 to attack
ori r24,$40 ; set voice4 trigger
sei ; unblock
rjmp main ; go back to waiting

voice1_on: ; turn on voice1

lds r30,last_byte ; fetch note from lookup table
sts voice1,r30 ; store note number for turnoff sequence
lsl r30 ; adjust for 2byte fetch
ldi r31,$0c
lpm r16,z+ ; load it into the frequency register
lpm r17,z
cli
sts freq1_l,r16
sts freq1_h,r17
lds r16,pwm_start
sts pwm_depth1,r16 ; reset pwm depth
andi r23,$fc
ori r23,$01 ; set voice1 to attack
ori r24,$01 ; set voice1 trigger
sei
rjmp main ; go back to waiting

voice2_on: ; turn on voice2

lds r30,last_byte ; fetch note from lookup table
sts voice2,r30 ; store note number for turnoff sequence
lsl r30 ; adjust for 2byte fetch
ldi r31,$0c
lpm r16,z+ ; load it into the frequency register
lpm r17,z
cli
sts freq2_l,r16
sts freq2_h,r17
lds r16,pwm_start
sts pwm_depth2,r16 ; reset pwm depth
andi r23,$f3
ori r23,$04 ; set voice2 to attack
ori r24,$04 ; set voice2 trigger
sei
rjmp main ; go back to waiting

voice3_on: ; turn on voice3

lds r30,last_byte ; fetch note from lookup table
sts voice3,r30 ; store note number for turnoff sequence
lsl r30 ; adjust for 2byte fetch
ldi r31,$0c
lpm r16,z+ ; load it into the frequency register
lpm r17,z
cli
sts freq3_l,r16
sts freq3_h,r17
lds r16,pwm_start
sts pwm_depth3,r16 ; reset pwm depth
andi r23,$cf
ori r23,$10 ; set voice3 to attack
ori r24,$10 ; set voice3 trigger
sei
rjmp main ; go back to waiting

voice1_on0: ; turn on voice1

lds r30,last_byte ; fetch note from lookup table
sts voice1,r30 ; store note number for turnoff sequence
lsl r30 ; adjust for 2byte fetch
ldi r31,$0c
lpm r16,z+ ; load it into the frequency register
lpm r17,z
cli ; block
clr r2 ; sync oscillator if voice off
clr r3
sts freq1_l,r16
sts freq1_h,r17
lds r16,pwm_start
sts pwm_depth1,r16 ; reset pwm depth
andi r23,$fc
ori r23,$01 ; set voice1 to attack
ori r24,$01 ; set voice1 trigger
sei ; unblock
rjmp main ; go back to waiting

voice2_on0: ; turn on voice2

lds r30,last_byte ; fetch note from lookup table
sts voice2,r30 ; store note number for turnoff sequence
lsl r30 ; adjust for 2byte fetch
ldi r31,$0c
lpm r16,z+ ; load it into the frequency register
lpm r17,z
cli
clr r4 ; sync oscillator for voice off
clr r5
sts freq2_l,r16
sts freq2_h,r17
lds r16,pwm_start
sts pwm_depth2,r16 ; reset pwm depth
andi r23,$f3
ori r23,$04 ; set voice2 to attack
ori r24,$04 ; set voice2 trigger
sei
rjmp main ; go back to waiting

voice3_on0: ; turn on voice3

lds r30,last_byte ; fetch note from lookup table
sts voice3,r30 ; store note number for turnoff sequence
lsl r30 ; adjust for 2byte fetch
ldi r31,$0c
lpm r16,z+ ; load it into the frequency register
lpm r17,z
cli
clr r6 ; sync oscillator for voice off
clr r7
sts freq3_l,r16
sts freq3_h,r17
lds r16,pwm_start
sts pwm_depth3,r16 ; reset pwm depth
andi r23,$cf
ori r23,$10 ; set voice3 to attack
ori r24,$10 ; set voice3 trigger
sei
rjmp main ; go back to waiting

voice4_on0: ; turn on voice4

lds r30,last_byte ; fetch note from lookup table
sts voice4,r30 ; store note number for turnoff sequence
lsl r30 ; adjust for 2byte fetch
ldi r31,$0c
lpm r16,z+ ; load it into the frequency register
lpm r17,z
cli
clr r8 ; sync oscillator for voice off
clr r9
sts freq4_l,r16
sts freq4_h,r17
lds r16,pwm_start
sts pwm_depth4,r16 ; reset pwm depth
andi r23,$3f
ori r23,$40 ; set voice4 to attack
ori r24,$40 ; set voice4 trigger
sei
rjmp main ; go back to waiting

note_off_process: ; turn off notes

;every voice is checked in case 2 are on, and only 1 note-off occurs
lds r16,last_byte ; fetch note number
lds r17,voice1 ; fetch voice currently playing
cp r16,r17 ; see if the note off was for that voice
brne note2_off ; turn off that voice if so
ori r23,$03 ; set voice1 to decay
ser r17
sts voice1,r17 ; set voice off

note2_off: ; check voice2

lds r17,voice2
cp r16,r17
brne note3_off
ori r23,$0c ; set voice2 to decay
ser r17
sts voice2,r17 ; set voice off

note3_off:

lds r17,voice3
cp r16,r17
brne note4_off
ori r23,$30 ; set voice3 to decay
ser r17
sts voice3,r17 ; set voice off

note4_off:

lds r17,voice4
cp r16,r17
brne note_off_done
ori r23,$c0 ; set voice4 to decay
ser r17
sts voice4,r17 ; set voice off

note_off_done:

rjmp main ; if no matches, do nothing

control_process: ; check which controller is active

;mov controller number to high register
;check for various control parameters
lds r16,last_byte
cpi r16,$15 ; check if in range of conroller numbers recognized
brlo in_range
rjmp main ; go back if not

in_range: ; controller numbers within range

cpi r16,$0f ; branch the search tree for greater speed
brsh mod15
cpi r16,$0a
brsh mod10
rjmp main ; finish off if not within controller range recognized

mod10: ; check if voice

brne mod11
;voice selection
andi r17,$78 ; mask off upper 4 bits (only goes to 127, so no b7)
subi r17,$f0 ; adjust for table boundary
mov r25,r17
rjmp main

mod11: ; attacks

cpi r16,$0b
brne mod12
neg r17 ; reverse knob direction
subi r17,$80 ; give an offset
brcc mod11a ; check for largest value
ldi r17,$ff ; set to max

mod11a:

sts attack,r17 ; save new attack variable
rjmp main ; go back to waiting

mod12: ; decays

cpi r16,$0c
brne mod13
neg r17 ; reverse knob direction
subi r17,$80 ; give an offset
brcc mod12a ; check for largest value
ldi r17,$ff ; set to max

mod12a:

sts decay,r17 ; save new decay variable
rjmp main ; go back to waiting

mod13: ; frequency sweep

cpi r16,$0d
brne mod14
clr r16
subi r17,$40
sbc r16,r10
sts sweep_l,r17
sts sweep_h,r16
rjmp main ; go back to waiting

mod14: ; fm-depth

cpi r16,$0e
brne mod_none2
lsl r17
clr r16
cli
sts fm_mod_l,r17
sts fm_mod_h,r16
sei

mod_none2: ; none of the above

rjmp main

mod15: ; fm frequency

brne mod16 ; previous operation was cpi r16,$0f
com r17 ; reverse knob direction
subi r17,$7f ; add offset
sts fm_freq,r17
rjmp main ; go back to waiting

mod16: ; bit-crush

cpi r16,$10
brne mod17
mov r30,r17
lsl r30 ; setup for double byte fetch
ldi r31,$0a ; set to lookup table boundary
lpm r16,z+ ; get bit mask
lpm r17,z
sts bit_crush_l,r16 ; store bit mask
sts bit_crush_h,r17
rjmp main ; go back to waiting

mod17: ; feedback

cpi r16,$11
brne mod18
lsl r17
com r17 ; store the inverse for faster calculations in synth-engine
sts feedback,r17
rjmp main ; go back to waiting

mod18: ; pwm cutoff

cpi r16,$12
brne mod19
inc r17
sts cut_off,r17 ; store forward cut_off
ldi r16,$81
sub r16,r17
sts cut_off_rev,r16 ; store revers cut_off
lds r16,freq_decay ; fetch decay rate
tst r16 ; check if not decaying
breq mod18_2
rjmp main ; go back to waiting

mod18_2: ; load current value if not decaying

sts pwm_start,r17 ; set initial to cut_off value
rjmp main

mod19: ; pwm decay

cpi r16,$13
brne mod20
subi r17,$40 ; make positive and negative signals
brne mod19a
sts freq_decay,r17 ; store new decay value (0)
lds r16,cut_off ; fetch cut_off value
sts pwm_start,r16 ; set initial to cut_off value
rjmp main ; go back to waiting

mod19a: ; adjust for non zero values

brcc decay_up
ldi r16,$bf
sub r16,r17 ; invert direction of knob
sts freq_decay,r16 ; store new value
ldi r16,$80
sts pwm_start,r16 ; set initial to high value
rjmp main

decay_up:

ldi r16,$40
sub r16,r17 ; invert direction of knob
sts freq_decay,r16
ldi r16,$01
sts pwm_start,r16 ; set initial to low value
rjmp main ; go back to waiting

mod20: ; am frequency

; cpi $14 already done at beginning
neg r17 ; invert knob direction
subi r17,$80
andi r17,$7f
sts am_freq,r17
rjmp main


t1int: ; do 32khz sound generation
.include "synth-engine.inc"

t0int_a: ; do fm here
.include "frequency-sweep2.inc"

t0int_b: ; do envelopes here
.include "envelope-engine.inc"


.org $0680 ; lookup table for bitcrusher
.include "bit_crush.inc"

.org $0700 ; lookup table for am
.include "sine_256_unsigned_offset.inc"

.org $0780 ; lookup table for note to frequency converter
.include "notelookup_assy.inc"

.org $0800 ; voice2 table
.include "sine_table_2k_signed.inc" ; 8 bit, 2048 sample, sinewave

.org $0c00 ; voice3 table
.include "square.inc" ; 8 bit, 2048 sample, sinewave

.org $1000 ; voice4 table
.include "saw.inc" ; 8 bit, 2048 sample

.org $1400 ; voice5 table
.include "ooh.inc" ; 8 bit, 2048 sample

.org $1800 ; voice6 table
.include "om.inc" ; 8 bit, 2048 sample

.org $1c00 ; voice7 table
.include "aah.inc" ; 8 bit, 2048 sample

.org $2000 ; voice8 table
.include "aay.inc" ; 8 bit, 2048 sample

.org $2400 ; voice9 table
.include "eeh.inc" ; 8 bit, 2048 sample

.org $2800 ; voice10 table
.include "lid1.txt" ; 8 bit, 2048 sample

.org $2c00 ; voice11 table
.include "tube2.txt" ; 8 bit, 2048 sample

.org $3000 ; voice12 table
.include "wv.txt" ; 8 bit, 2048 sample

.org $3400 ; voice13 table
.include "pot1.txt" ; 8 bit, 2048 sample

.org $3800 ; voice14 table
.include "noise3.txt" ; 8 bit, 2048 sample

