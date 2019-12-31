;==============================================================
; WLA-DX banking setup
;==============================================================
.MEMORYMAP
    DEFAULTSLOT 0

    ; ROM pages are 16K each, and slotted into contiguous memory.
    SLOTSIZE $4000
    SLOT 0 $0000
    SLOT 1 $4000
    SLOT 2 $8000

    ; RAM is 8K, but mirrored.
    SLOTSIZE $2000
    SLOT 3 $C000
    SLOT 4 $E000    ; Mirrored
.ENDME

.ROMBANKMAP
    BANKSTOTAL 1
    BANKSIZE $4000
    BANKS 1
.ENDRO