; frequency sweep of oscillators

in r18,sreg ; store sreg
out gpior2,r18

;first do fm
lds r18,fm_mod_l
tst r18
breq fm_done1 ; dont bother doing fm if its off
lds r20,fm_count ; check if time for fm
dec r20
brne fm_done ; skip if not time
lds r19,fm_mod_h
neg r18
adc r19,r10
neg r19
sts fm_mod_l,r18
sts fm_mod_h,r19
lds r20,fm_freq

fm_done: ; finish off

sts fm_count,r20

fm_done1: 

cpi r25,$10 ; check if pwm mode
breq freq_sweep ; finish off if not
in r18,gpior2
out sreg,r18 ; restore sreg
reti ; return from interrupt

freq_sweep: ; do frequency sweep of pwm

lds r18,decay_timer
cpi r18,$02 ; do pwm sweep out of phase with decays
brne trig_done3
lds r19,freq_decay
lds r21,cut_off
tst r19 ; check if doing sweeps
brne flip_check
sts pwm_depth1,r21 ; set pwm depth to cutoff
sts pwm_depth2,r21
sts pwm_depth3,r21
sts pwm_depth4,r21

trig_done3:

in r18,gpior2
out sreg,r18 ; restore sreg
reti ; return from interrupt

flip_check: ; check to see if adding or subtracting

brmi voice1_trig_check2

voice1_trig_check: ; check for voice1 trigger

sbrs r24,$00
rjmp voice2_trig_check
lds r20,pwm_depth1 ; fetch current pwm depth
add r20,r19 ; sweep pwm up
cp r20,r21 ; check if at top
brlo voice1_done
mov r20,r21 ; set pwm depth to max
andi r24,$fc ; clear trigger bit

voice1_done:

sts pwm_depth1,r20 ; restore pwm depth

voice2_trig_check: ; check for voice2 trigger

sbrs r24,$02
rjmp voice3_trig_check
lds r20,pwm_depth2 ; fetch current pwm depth
add r20,r19 ; sweep pwm up
cp r20,r21 ; check if at top
brlo voice2_done
mov r20,r21 ; set pwm depth to max
andi r24,$f3 ; clear trigger bit

voice2_done:

sts pwm_depth2,r20 ; restore pwm depth

voice3_trig_check: ; check for voice2 trigger

sbrs r24,$04
rjmp voice4_trig_check
lds r20,pwm_depth3 ; fetch current pwm depth
add r20,r19 ; sweep pwm up
cp r20,r21 ; check if at top
brlo voice3_done
mov r20,r21 ; set pwm depth to max
andi r24,$cf ; clear trigger bit

voice3_done:

sts pwm_depth3,r20 ; restore pwm depth

voice4_trig_check: ; check for voice2 trigger

sbrs r24,$06
rjmp trig_done
lds r20,pwm_depth4 ; fetch current pwm depth
add r20,r19 ; sweep pwm up
cp r20,r21 ; check if at top
brlo voice4_done
mov r20,r21 ; set pwm depth to max
andi r24,$3f ; clear trigger bit

voice4_done:

sts pwm_depth4,r20 ; restore pwm depth

trig_done:

in r18,gpior2
out sreg,r18 ; restore sreg
reti ; return from interrupt

;down sweep
voice1_trig_check2: ; check for voice1 trigger

lds r21,cut_off_rev ; fetch reversed knob value
sbrs r24,$00
rjmp voice2_trig_check2
lds r20,pwm_depth1 ; fetch current pwm depth
add r20,r19 ; sweep pwm down
brcc voice1_overflow ; check if overflow
cp r20,r21 ; check if at bottom
brsh voice1_done2

voice1_overflow:

mov r20,r21 ; set pwm depth to min
andi r24,$fc ; clear trigger bit

voice1_done2:

sts pwm_depth1,r20 ; restore pwm depth

voice2_trig_check2: ; check for voice2 trigger

sbrs r24,$02
rjmp voice3_trig_check2
lds r20,pwm_depth2 ; fetch current pwm depth
add r20,r19 ; sweep pwm down
brcc voice2_overflow ; check if overflow
cp r20,r21 ; check if at bottom
brsh voice2_done2

voice2_overflow:

mov r20,r21 ; set pwm depth to min
andi r24,$f3 ; clear trigger bit

voice2_done2:

sts pwm_depth2,r20 ; restore pwm depth

voice3_trig_check2: ; check for voice2 trigger

sbrs r24,$04
rjmp voice4_trig_check2
lds r20,pwm_depth3 ; fetch current pwm depth
add r20,r19 ; sweep pwm down
brcc voice3_overflow ; check if overflow
cp r20,r21 ; check if at bottom
brsh voice3_done2

voice3_overflow:

mov r20,r21 ; set pwm depth to min
andi r24,$cf ; clear trigger bit

voice3_done2:

sts pwm_depth3,r20 ; restore pwm depth

voice4_trig_check2: ; check for voice2 trigger

sbrs r24,$06
rjmp trig_done2
lds r20,pwm_depth4 ; fetch current pwm depth
add r20,r19 ; sweep pwm down
brcc voice4_overflow ; check if overflow
cp r20,r21 ; check if at bottom
brsh voice4_done2

voice4_overflow:

mov r20,r21 ; set pwm depth to min
andi r24,$3f ; clear trigger bit

voice4_done2:

sts pwm_depth4,r20 ; restore pwm depth

trig_done2:

in r18,gpior2
out sreg,r18 ; restore sreg
reti ; return from interrupt
