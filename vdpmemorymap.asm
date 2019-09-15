;==============================================================================
; Defines how the application intends to use the VDP memory.  By having pre-
; selected memory layout, we can make optimizations.
.DEFINE VDP_NAMETABLE_START_LOC     $3800  ; Must be at 2K (0x0800) boundary.
.DEFINE VDP_SAT_START_LOC           $3F00  ; Must be at 256-byte boundary.

;==============================================================================
; Memory Map:
; 
; A typical memory map looks like this:
; 
; Offset    Description
; 0x0000    Tiles 0..255 (typically sprite tiles)
; 0x2000    Tiles 256..448
; 0x3800    Start of Name Table
; 0x3F00    Start of SAT
; 0x3F40    <64-byte unused gap in the SAT>
; 0x3F80    Second half of SAT
;==============================================================================
