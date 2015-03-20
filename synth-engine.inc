; main sound generation routines
; mostly multiples amplitudes and signal samples
in r18,gpior0 ; fetch current pwm 
in r19,gpior1
sts ocr1al,r19 ; send them out to the timer
sts ocr1bl,r18
in r18,sreg ; store sreg
out gpior2,r18 ; io registers are faster than push
out eedr,r30 ; store z pointer
out eearl,r31
clr r18 ; clear accumulation register
clr r19
lds r26,fm_mod_l ; fetch fm_mod
lds r27,fm_mod_h
cpi r25,$10 ; check if doing pwm
brne voice1_process
rjmp pwm_process

voice1_process: ; process voice1

tst r12 ; check if voice1 active
breq voice2_process
movw r31:r30,r3:r2 ; move to lpm register
lds r0,freq1_l ; fetch current frequency
lds r1,freq1_h
add r3,r0 ; add in frequency
adc r2,r1
add r3,r26 ; add in fm_mod
adc r2,r27
lsl r31
rol r30
rol r31
rol r30
rol r31
rol r30
rol r31
andi r31,$07 ; mask off lookup table boundary
add r31,r25 ; adjust for voice table boundary
lpm r21,z ; fetch current sample
mov r20,r12 ; fetch amplitude
mulsu r21,r20
asr r1 ; divide by 4
ror r0
asr r1
ror r0
movw r19:r18,r1:r0 ; accumulate samples (just movw b/c first sample)

voice2_process: ; process voice2

tst r13 ; check if voice2 active
breq voice3_process
movw r31:r30,r5:r4 ; move to lpm register
lds r0,freq2_l ; fetch current frequency
lds r1,freq2_h
add r5,r0 ; add in frequency
adc r4,r1
add r5,r26 ; add in fm_mod
adc r4,r27
lsl r31
rol r30
rol r31
rol r30
rol r31
rol r30
rol r31
andi r31,$07 ; mask off lookup table boundary
add r31,r25 ; adjust for voice table boundary
lpm r21,z ; fetch current sample
mov r20,r13 ; fetch amplitude
mulsu r21,r20
asr r1 ; divide by 4
ror r0
asr r1
ror r0
add r18,r0 ; accumulate samples
adc r19,r1

voice3_process: ; process voice3

tst r14 ; check if voice3 active
breq voice4_process
movw r31:r30,r7:r6 ; move to lpm register
lds r0,freq3_l ; fetch current frequency
lds r1,freq3_h
add r7,r0 ; add in frequency
adc r6,r1
add r7,r26 ; add in fm_mod
adc r6,r27
lsl r31
rol r30
rol r31
rol r30
rol r31
rol r30
rol r31
andi r31,$07 ; mask off lookup table boundary
add r31,r25 ; adjust for voice table boundary
lpm r21,z ; fetch current sample
mov r20,r14 ; fetch amplitude
mulsu r21,r20
asr r1 ; divide by 4
ror r0
asr r1
ror r0
add r18,r0 ; accumulate samples
adc r19,r1

voice4_process: ; process voice4

tst r15 ; check if voice4 active
breq voice_done
movw r31:r30,r9:r8 ; move to lpm register
lds r0,freq4_l ; fetch current frequency
lds r1,freq4_h
add r9,r0 ; add in frequency
adc r8,r1
add r9,r26 ; add in fm_mod
adc r8,r27
lsl r31
rol r30
rol r31
rol r30
rol r31
rol r30
rol r31
andi r31,$07 ; mask off lookup table boundary
add r31,r25 ; adjust for voice table boundary
lpm r21,z ; fetch current sample
mov r20,r15 ; fetch amplitude
mulsu r21,r20
asr r1 ; divide by 4
ror r0
asr r1
ror r0
add r18,r0 ; accumulate samples
adc r19,r1

voice_done:

;do bit crush
lds r20,bit_crush_l ; fetch bit crush
lds r21,bit_crush_h
and r18,r20
and r19,r21

;do delay
ld r21,y ; fetch delayed sample
lds r20,feedback ; fetch inverted feedback
mulsu r19,r20 ; multiply current by inverted feedback
movw r31:r30,r1:r0
mul r18,r20
add r30,r1
adc r31,r10
com r20 ; invert the feedback
mulsu r21,r20 ; multiply by delayed sample
adc r31,r1 ; round negative numbers up and truncate to keep from oscillating at high feedback
st y+,r31
sbrc r29,$03
ldi r29,$01
movw r19:r18,r31:r30

;do am
lds r20,am_freq ; check if doing am
tst r20
breq synth_done
dec r11 ; decrement am counter
brne am_fetch_done
lds r30,am_ptr ; fetch pointer
inc r30 ; increment to next sample
sts am_ptr,r30 ; restore pointer
ldi r31,$0b ; set to lookup table boundary
lpm r22,z ; fetch sine value
mov r11,r20 ; reset counter

am_fetch_done:

mulsu r19,r22 ; multiply by amplitude
movw r31:r30,r1:r0
mul r18,r22
add r30,r1
adc r31,r10
add r18,r30 ; do 50/50 mix of am
adc r19,r31
ror r19 ; only works for nonzero values
ror r18

synth_done: ; finish off

subi r19,$80 ; convert signed to unsigned for pwm output
lsr r19 ; adjust 16b number for 14b output
ror r18
lsr r18
out gpior0,r18 ; save to buffer
out gpior1,r19
in r31,eearl
in r30,eedr ; restore z pointer
in r18,gpior2
out sreg,r18 ; restore sreg
reti ; return from interrupt

pwm_process: ; do pwm instead of lookup tables

;process voice1
tst r12 ; check if voice1 active
breq voice2_pwm
lds r0,freq1_l ; fetch current frequency
lds r1,freq1_h
add r2,r0 ; add in frequency
adc r3,r1
add r2,r26 ; add in fm_mod
adc r3,r27
lds r31,pwm_depth1
cp r3,r31
brsh voice1_add
sub r19,r12 ; subtract amplitude
sbc r18,r10
rjmp voice2_pwm

voice1_add:

add r19,r12 ; add amplitude
adc r18,r10

voice2_pwm: ; process voice2

tst r13 ; check if voice2 active
breq voice3_pwm
lds r0,freq2_l ; fetch current frequency
lds r1,freq2_h
add r4,r0 ; add in frequency
adc r5,r1
add r4,r26 ; add in fm_mod
adc r5,r27
lds r31,pwm_depth2
cp r5,r31
brsh voice2_add
sub r19,r13
sbc r18,r10
rjmp voice3_pwm

voice2_add:

add r19,r13
adc r18,r10

voice3_pwm: ; process voice3

tst r14 ; check if voice3 active
breq voice4_pwm
lds r0,freq3_l ; fetch current frequency
lds r1,freq3_h
add r6,r0 ; add in frequency
adc r7,r1
add r6,r26 ; add in fm_mod
adc r7,r27
lds r31,pwm_depth3
cp r7,r31
brsh voice3_add
sub r19,r14
sbc r18,r10
rjmp voice4_pwm

voice3_add:

add r19,r14
adc r18,r10

voice4_pwm: ; process voice4

tst r15 ; check if voice4 active
breq pwm_done
lds r0,freq4_l ; fetch current frequency
lds r1,freq4_h
add r8,r0 ; add in frequency
adc r9,r1
add r8,r26 ; add in fm_mod
adc r9,r27
lds r31,pwm_depth4
cp r9,r31
brsh voice4_add
sub r19,r15
sbc r18,r10
rjmp pwm_done

voice4_add:

add r19,r15
adc r18,r10

pwm_done:

lsr r18 ; divide by 8
ror r19
ror r18
ror r19
ror r18
ror r19
ror r18
andi r18,$e0 ; mask off unused bits
rjmp voice_done


