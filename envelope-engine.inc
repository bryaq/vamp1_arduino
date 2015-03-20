; envelope generation
; attacks are processed every interrupt
; decays are processed every n interrupts

in r18,sreg ; store sreg
out gpior2,r18
lds r20,attack
lds r19,decay_timer
inc r19 ; increment decay timer
andi r19,$07 ; do decays on 0 and 4, out of phase with pwm sweep
sts decay_timer,r19
breq decays
cpi r19,$04
breq decays

;create attack-only buffer - makes further checks faster
mov r21,r23
lsr r21
eor r21,r23
and r21,r23

;process voice1
sbrs r21,$00 ; check if doing attacks
rjmp voice2_envelope
add r12,r20 ; increment envelope
brcc voice2_envelope ; check if at top
ser r19
mov r12,r19 ; set envelope to full on
subi r23,$ff ; add b01 to set to sustain (b10)

voice2_envelope: ; process voice2

sbrs r21,$02 ; check if doing attacks
rjmp voice3_envelope
add r13,r20 ; increment envelope
brcc voice3_envelope ; check if at top
ser r19
mov r13,r19 ; set envelope to full on
subi r23,$fc ; add b01xx to set to sustain (b10xx)

voice3_envelope: ; process voice3

sbrs r21,$04 ; check if doing attacks
rjmp voice4_envelope
add r14,r20 ; increment envelope
brcc voice4_envelope ; check if at top
ser r19
mov r14,r19 ; set envelope to full on
subi r23,$f0 ; set envelope to sustain

voice4_envelope: ; process voice4

sbrs r21,$06 ; check if doing attacks
rjmp envelope_done
add r15,r20 ; increment envelope
brcc envelope_done ; check if at top
ser r19
mov r15,r19 ; set envelope to full on
subi r23,$c0 ; set envelope to sustain

envelope_done: ; finish off

in r18,gpior2
out sreg,r18 ; restore sreg
reti ; return from interrupt

decays: ; do attack and decay every nth time

lds r21,decay
lds r0,sweep_l
lds r1,sweep_h

;process voice1
sbrs r23,$00 ; check if doing attacks/decay
rjmp voice2_envelope2
sbrc r23,$01 ; check if doing attack
rjmp decay1_process2
add r12,r20 ; increment envelope
brcc voice2_envelope2 ; check if at top
ser r19
mov r12,r19 ; set envelope to full on
subi r23,$ff ; add b01 to set to sustain (b10)
rjmp voice2_envelope2

decay1_process2: ; process decays

lds r26,freq1_l ; fetch frequency
lds r27,freq1_h
add r26,r0 ; do sweep
adc r27,r1
sts freq1_l,r26 ; restore frequency
sts freq1_h,r27
sub r12,r21 ; decrement envelope
brcc voice2_envelope2 ; check if at bottom
clr r12 ; set envelope to full off
andi r23,$fc ; set voice1 ramp to off

voice2_envelope2: ; process voice2

sbrs r23,$02 ; check if doing attacks/decay
rjmp voice3_envelope2
sbrc r23,$03 ; check if doing attacks
rjmp decay2_process2
add r13,r20 ; increment envelope
brcc voice3_envelope2 ; check if at top
ser r19
mov r13,r19 ; set envelope to full on
subi r23,$fc ; add b01xx to set to sustain (b10xx)
rjmp voice3_envelope2

decay2_process2: ; process decays

lds r26,freq2_l ; fetch frequency
lds r27,freq2_h
add r26,r0 ; do sweep
adc r27,r1
sts freq2_l,r26 ; restore frequency
sts freq2_h,r27
sub r13,r21 ; decrement envelope
brcc voice3_envelope2 ; check if at bottom
clr r13 ; set envelope to full off
andi r23,$f3 ; set voice2 ramp to off

voice3_envelope2: ; process voice3

sbrs r23,$04 ; check if doing attacks
rjmp voice4_envelope2
sbrc r23,$05
rjmp decay3_process2
add r14,r20 ; increment envelope
brcc voice4_envelope2 ; check if at top
ser r19
mov r14,r19 ; set envelope to full on
subi r23,$f0 ; set envelope to sustain
rjmp voice4_envelope2

decay3_process2: ; process decays

lds r26,freq3_l ; fetch frequency
lds r27,freq3_h
add r26,r0 ; do sweep
adc r27,r1
sts freq3_l,r26 ; restore frequency
sts freq3_h,r27
sub r14,r21 ; decrement envelope
brcc voice4_envelope2 ; check if at bottom
clr r14 ; set envelope to full off
andi r23,$cf ; set voice3 ramp to off

voice4_envelope2: ; process voice4

sbrs r23,$06 ; check if doing attacks
rjmp envelope_done2
sbrc r23,$07
rjmp decay4_process2
add r15,r20 ; increment envelope
brcc envelope_done2 ; check if at top
ser r19
mov r15,r19 ; set envelope to full on
subi r23,$c0 ; set envelope to sustain
rjmp envelope_done2

decay4_process2: ; process decays

lds r26,freq4_l ; fetch frequency
lds r27,freq4_h
add r26,r0 ; do sweep
adc r27,r1
sts freq4_l,r26 ; restore frequency
sts freq4_h,r27
sub r15,r21 ; decrement envelope
brcc envelope_done2 ; check if at bottom
clr r15 ; set envelope to full off
andi r23,$3f ; set voice4 ramp to off

envelope_done2: ; finish off

in r18,gpior2
out sreg,r18 ; restore sreg
reti ; return from interrupt
