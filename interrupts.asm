.bank 0 slot 0
.org $0038
.section "SMSFramework Video Interrupts" FORCE
SMSFramework_VideoInterruptHandler:
    exx
        call    CallHL
    exx
    ei
    reti
.ends

.bank 0 slot 0
.org $0066
.section "SMSFramework Non-Maskable Interrupts" FORCE
SMSFramework_NMIHandler:
    push    af
        ; Are we initialized, or did this come in while we were booting?
        ld a, (SMSFrameWork_Initialized)
        and a
        jr  z, _SMSFramework_NMIHandler_Restore ; Ignore it if we're not yet initialized.

        ; Pass this on to the mode handler
        push    ix
            call ModeManager_OnNMI
        pop     ix
_SMSFramework_NMIHandler_Restore:
    pop     af
    retn
.ends
