;****IMPORTANT****
; This needs to be the first thing the assembler touches,
; so that it can find routines from the SMSFramework.
; Ensure we can get to the SMSFramework.
.INCDIR "../../SMSFramework/"

.include "bankdetails.asm"
.include "interrupts.asm"
.include "vdpmemorymap.asm"
.include "Utils/boot.asm"
.include "Utils/FSM.asm"
.include "Utils/vdp.asm"
.include "Utils/spritechain.asm"
.include "Utils/map.asm"
.include "Utils/input.asm"
.include "Utils/controller.asm"
.include "Utils/macros.asm"
.include "Utils/tile_routines.asm"
.include "Managers/modemanager.asm"
.include "Managers/vdpmanager.asm"
.include "Managers/inputmanager.asm"

;==============================================================
; SDSC tag and SMS rom header
;==============================================================
.SDSCTAG 1.2,"SMS Sample App","Sample application using the SMSFramework","SavagePencil"


.SECTION "Application Main Loop" FREE
; This routine is called by the framework when we're ready to enter
; the main loop.
Application_MainLoop_InitialEntry:
    ei                                  ; Turn on interrupts

Application_MainLoop:    
    call ModeManager_OnUpdate           ; Update for current mode
    call ModeManager_OnRenderPrep       ; Prepare things for rendering
    halt                                ; Wait for VBlank
    jp   Application_MainLoop           
.ENDS

.STRUCT SpriteChain
    Header INSTANCEOF SpriteChainHeader
    YPosEntries INSTANCEOF SAT_YPosEntry 8
    XPosTileEntries INSTANCEOF SAT_XPosTileEntry 8
.ENDST

.MACRO PREP_CHAIN ARGS SRC_CHAIN, NEXT_CHAIN, SIZE, X_POS, Y_POS
    ld      ix, SRC_CHAIN
    ld      a, SIZE
    ld      bc, NEXT_CHAIN
    ld      de, SRC_CHAIN + 8 ;SpriteChain.YPosEntries
    ld      hl, SRC_CHAIN + 8 + SIZE ;SpriteChain.XPosEntries
    call    SpriteChain_Init

    ; Fill it out
    SPRITE_CHAIN_PREP_ENQUEUE_CHAIN_IN_IX SRC_CHAIN + 8, SRC_CHAIN + 8 + SIZE

    ld  b, X_POS
    ld  a, Y_POS

.REPT SIZE
    ld  c, $5F      ; Tile
    SPRITE_CHAIN_ENQUEUE_SPRITE

    ld  c, a
    ld  a, b
    add a, $8
    ld  b, a
    ld  a, c
.ENDR
        

.ENDM


.SECTION "Application Bootstrap" FREE
; This routine sets up an initial state as part of the bootstrapping.
; It should set a mode for the initial program.
Application_Bootstrap:
    ; Setup the VDP
    call    VDPManager_Init

    ; Setup the input manager
    call    InputManager_Init

    ; Set our initial mode
    ld      de, Mode1
    call    ModeManager_Init

; Mode Testing
_Application_Bootstrap_ModeTesting:
    ld      de, Mode2
    call    ModeManager_PushMode

    call    ModeManager_PopMode
    call    ModeManager_OnUpdate

; FSM Testing
_Application_Bootstrap_FSMTesting:
    ld      ix, gMyFSM
    ld      hl, MyFSM_State1
    call    FSM_Init
    call    FSM_OnUpdate
    ld      hl, MyFSM_State4
    call    FSM_ChangeState

; Setup the palette
    ld      b, (PaletteEnd - PaletteBegin) >> 1
    ld      hl, PaletteBegin
-:
    ld      e, (hl)     ; Get entry
    inc     hl
    ld      c, (hl)     ; Get color value
    inc     hl
    push    hl
    call    VDPManager_SetPaletteEntryImmediate
    pop     hl
    djnz    -

; Load the tiles
TileLoader:
    ld      de, TileDataBegin               ; Src data
    ld      bc, TileDataEnd - TileDataBegin ; Length of data
    ld      hl, $0000                       ; Dest tile index
    call    VDP_UploadTileDataToTilePos

; Load 1bpp tile data while remapping only the 1s to a new color (0s go to index 0)
TileLoader1bpp_OneRemap:
    ld      de, TileData_1bpp_Begin                     ; Src data
    ld      bc, TileData_1bpp_End - TileData_1bpp_Begin ; Length of data
    ld      hl, $70                                     ; Dest tile index
    ld      a, 14                                       ; Color index for 1s
    call    Tile_Upload1BPPWithPaletteRemapToTilePos

; Load 1bpp tile data while remapping BOTH the 0s and the 1s.
TileLoader1bpp_TwoRemap:
    ld      hl, $72
    CALC_VRAM_LOC_FOR_TILE_INDEX_IN_HL
    SET_VRAM_WRITE_LOC_FROM_HL

    ; VRAM is set!
    ld      hl, TileData_1bpp_Begin                     ; Src data
    ld      bc, TileData_1bpp_End - TileData_1bpp_Begin ; Length of data
    ld      e, 14                                       ; Color index for 0 values
    ld      d, 7                                        ; Color index for 1 values
    call    Tile_Upload1BPPWithPaletteRemaps_VRAMPtrSet

    ; Alt version that doesn't have to interleave the colors.
    ld      hl, $74
    CALC_VRAM_LOC_FOR_TILE_INDEX_IN_HL
    SET_VRAM_WRITE_LOC_FROM_HL

    ; VRAM is set!
    ld      hl, TileData_1bpp_Begin                     ; Src data
    ld      bc, TileData_1bpp_End - TileData_1bpp_Begin ; Length of data
    PRE_INTERLEAVE_1BPP_REMAP_ENTRIES_TO_E 7, 14

    call    Tile_Upload1BPPWithPaletteRemaps_VRAMPtrSet_ColorsInterleaved

CompositeTiles:
    ; Here's what we're going to do:  Composite two tiles on top of each other
    ; and upload to VRAM.
    ; 
    ; Here's how we're going to do it:
    ; 1. Allocate stack space to hold a 4bpp tile
    ; 2. Inflate a 1bpp tile into RAM, remapping 1s to a new color.
    ; 3. Allocate stack space to hold another 4bpp tile
    ; 4. Inflate a 1bpp tile into RAM, remapping 0s *AND* 1s to new colors.
    ; 5. Composite the first tile on top of the second one, using a mask
    ; 6. Upload the composited tile to VRAM.
    ; 7. Dealloc the stack space used.

    ; Set aside space on the stack for the first 4bpp tile
    ld      hl, -32     ; 1 4bpp tile's worth of bytes!
    add     hl, sp
    ld      sp, hl
    ex      de, hl      ; DE points to base of RAM for tile.

    ; Inflate the first tile into RAM.
    ld      hl, TileData_1bpp_Begin + ( 0 * 8 ) ; Each tile in 1bpp is 8 bytes
    ld      a, 14       ; Palette entry for 1s data.
    ld      b, 8        ; 8 bytes' worth of 1bpp data
    call    Tile_Inflate1BPPto4BPPRAMWithPaletteRemap

    ; Upload this to VRAM so that we can validate that it looks correct.
    push    de
    push    hl
        ld      hl, $0004   ; Compensate for the two pushes.
        add     hl, sp
        ex      de, hl  ; DE = Start of 4bpp data in RAM.
        ld      bc, 32  ; Length of data
        ld      hl, $76 ; Dest tile index
        call    VDP_UploadTileDataToTilePos
    pop     hl
    pop     de

    ; Set aside space on the stack for the second 4bpp tile
    ld      hl, -32     ; 1 4bpp tile's worth of bytes!
    add     hl, sp
    ld      sp, hl
    ex      de, hl      ; DE points to base of RAM for tile.

    ; Inflate the second tile into RAM.
    ld      hl, TileData_1bpp_Begin + ( 1 * 8 ) ; Each tile in 1bpp is 8 bytes
    ld      c, 7        ; Palette entry for 0s data.
    ld      a, 1        ; Palette entry for 1s data.
    ld      b, 8        ; 8 bytes' worth of 1bpp data
    call    Tile_Inflate1BPPto4BPPRAMWithPaletteRemaps

    ; Upload this to VRAM so that we can validate that it looks correct.
    push    de
    push    hl
        ld      hl, $0004   ; Compensate for the two pushes.
        add     hl, sp
        ex      de, hl  ; DE = Start of 4bpp data in RAM.
        ld      bc, 32  ; Length of data
        ld      hl, $77 ; Dest tile index
        call    VDP_UploadTileDataToTilePos
    pop     hl
    pop     de

    ; Create a 1bpp mask from the tile (we have it already in the 1bpp data, but do this to confirm)
    ld      hl, -8
    add     hl, sp
    ld      sp, hl
    ex      de, hl          ; DE holds our dest loc in RAM
    ld      hl, 40
    add     hl, sp          ; HL points to top tile.
    ld      b, 8
    call    Tile_GenerateMaskFromTile_DefaultClearColor

    ; Now composite them using a 1bpp mask.
    ld  hl, $78
    CALC_VRAM_LOC_FOR_TILE_INDEX_IN_HL
    SET_VRAM_WRITE_LOC_FROM_HL

    ; Get bottom tile
    ld      iy, 8       ; Offset 8 for 1bpp mask
    add     iy, sp      ; IY = Pointer to bottom tile
    ; Get top tile
    ld      hl, 40      ; Offset 40 == 8 (1bpp mask) + 32 (bottom tile 4bpp data)
    add     hl, sp
    ex      de, hl      ; DE = Pointer to top tile
    ; Get mask
    ld      hl, 0
    add     hl, sp      ; 1bpp Mask (should be same result as 1bpp src data)
    ld      b, 8        ; Size of Mask
    call    Tile_CompositePlanarTiles_ToVRAM_VRAMPtrSet

    ; Restore our stack pointer
    ld      hl, 64
    add     hl, sp
    ld      sp, hl

; Write a character.
WriteCornerChars:
    ld      hl, (CornerChar)
    ld      d, 0    ; Row
    ld      e, 0    ; Col
    call    VDP_UploadNameTableEntry

    ld      hl, (CornerChar)
    ld      d, 0    ; Row
    ld      e, 31   ; Col
    call    VDP_UploadNameTableEntry

    ld      hl, (CornerChar)
    ld      d, 23   ; Row
    ld      e, 0    ; Col
    call    VDP_UploadNameTableEntry

    ld      hl, (CornerChar)
    ld      d, 23   ; Row
    ld      e, 31   ; Col
    call    VDP_UploadNameTableEntry

    ld      hl, (CornerChar)
    ld      d, 27   ; Row
    ld      e, 31   ; Col
    call    VDP_UploadNameTableEntry

; Render a string based on a dynamic position (calc at runtime)
WriteStringDynamicPos:
    ld      hl, MessageHelloBegin
    ld      d, 2    ; Row
    ld      e, 1    ; Col
    ld      bc, MessageHelloEnd - MessageHelloBegin
    call    VDP_UploadNameTableEntries

; Render a string based on a pre-calculated position
WriteStringPreCalcPos:
    ld      hl, MessageWorldBegin
    VDP_NAMETABLE_CALC_VRAM_ADDRESS_DE 3, 1, VDP_COMMAND_MASK_VRAM_WRITE
    ld      bc, MessageWorldEnd - MessageWorldBegin
    call    VDP_UploadDataToVRAMLoc

; Upload sprites
UploadSprites:
    /*
    ld      hl, MySpriteTable.YPosEntries
    ld      de, MySpriteTable.XPosTileEntries
    ld      b, 2
    call    VDP_UploadSpriteData
    */

    ; The first sprite chain is in ROM.  Create subsequent ones in RAM.
    PREP_CHAIN SpriteChain2, SpriteChain3, 8, 8, $10
    PREP_CHAIN SpriteChain3, SpriteChain4, 8, 8, $18
    PREP_CHAIN SpriteChain4, SpriteChain5, 8, 8, $20
    PREP_CHAIN SpriteChain5, SpriteChain6, 8, 8, $28
    PREP_CHAIN SpriteChain6, SpriteChain7, 8, 8, $30
    PREP_CHAIN SpriteChain7, SpriteChain8, 8, 8, $38
    PREP_CHAIN SpriteChain8, SpriteChain9, 8, 8, $40
    PREP_CHAIN SpriteChain9, $0000, 8, 8, $48

RenderSprite:
    ld      hl, SpriteChain1
    call    SpriteChain_RenderChainSequence

UploadMap:
    ld      ix, MyMapDef
    ld      iy, MyMapRequest
    ld      de, $D000
    call    Map_Flat_LoadDataToBlock

SetJoypads:
    xor     a                               ; 0 == Port 1
    ld      b, CONTROLLER_TYPE_SMS_JOYPAD
    ld      hl, Controller_Joypad_Port1_State
    call    InputManager_SetController

    ld      a, 1                            ; 1 == Port 2
    ld      b, CONTROLLER_TYPE_SMS_JOYPAD
    ld      hl, Controller_Joypad_Port2_State
    call    InputManager_SetController


AllDone:
; Turn on the display, by OR'ing to the current value.
    ld      a, (gVDPManager.Registers.VideoModeControl2)
    or      VDP_REGISTER1_ENABLE_DISPLAY | VDP_REGISTER1_ENABLE_VBLANK
    ld      e, VDP_COMMMAND_MASK_REGISTER1
    call    VDPManager_WriteRegisterImmediate
    ret

.DSTRUCT MyMapRequest INSTANCEOF MapLoadRequest VALUES
    MapXPos .DW 0
    MapYPos .DW 0
    Width   .DB 16
    Height  .DB 2
    Stride  .DW 16
.ENDST

MapData:
;   ---00---  ---01---  ---02---  ---03---  ---04---  ---05---  ---06---  ---07---  ---08---  ---09---  ---0A---  ---0B---  ---0C---  ---0D---  ---0E---  ---0F---  
.db $00, $10, $00, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $10  $00, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

;   ---00---  ---01---  ---02---  ---03---  ---04---  ---05---  ---06---  ---07---  ---08---  ---09---  ---0A---  ---0B---  ---0C---  ---0D---  ---0E---  ---0F---  
.db $00, $10, $00, $11, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $11  $00, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

;   ---00---  ---01---  ---02---  ---03---  ---04---  ---05---  ---06---  ---07---  ---08---  ---09---  ---0A---  ---0B---  ---0C---  ---0D---  ---0E---  ---0F---  
.db $00, $10, $00, $12, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $12  $00, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

;   ---00---  ---01---  ---02---  ---03---  ---04---  ---05---  ---06---  ---07---  ---08---  ---09---  ---0A---  ---0B---  ---0C---  ---0D---  ---0E---  ---0F---  
.db $00, $10, $00, $13, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $12  $00, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

MapDataEnd

.DSTRUCT MyMapDef INSTANCEOF MapDefinition VALUES
    MapWidthInEntries:      .dw $10
    BytesPerEntry:          .db $02
    MapWidthInBytes:        .dw $20
    MapWidthBitShifts:      .db 6
    MapData:                .dw MapData
.ENDST

.DSTRUCT SpriteChain1 INSTANCEOF SpriteChain VALUES

    Header.CurrCount:               .db 8
    Header.MaxCount:                .db 8
    Header.NextChain:               .dw SpriteChain2
    Header.YPosBegin:               .dw SpriteChain1.YPosEntries
    Header.XPosTileBegin:           .dw SpriteChain1.XPosTileEntries

    XPosTileEntries.1.XPos:         .db $08
    YPosEntries.1.YPos:             .db $08
    XPosTileEntries.1.TileIndex:    .db $5F

    XPosTileEntries.2.XPos:         .db $10
    YPosEntries.2.YPos:             .db $08
    XPosTileEntries.2.TileIndex:    .db $5F

    XPosTileEntries.3.XPos:         .db $18
    YPosEntries.3.YPos:             .db $08
    XPosTileEntries.3.TileIndex:    .db $5F

    XPosTileEntries.4.XPos:         .db $20
    YPosEntries.4.YPos:             .db $08
    XPosTileEntries.4.TileIndex:    .db $5F

    XPosTileEntries.5.XPos:         .db $28
    YPosEntries.5.YPos:             .db $08
    XPosTileEntries.5.TileIndex:    .db $5F

    XPosTileEntries.6.XPos:         .db $30
    YPosEntries.6.YPos:             .db $08
    XPosTileEntries.6.TileIndex:    .db $5F

    XPosTileEntries.7.XPos:         .db $38
    YPosEntries.7.YPos:             .db $08
    XPosTileEntries.7.TileIndex:    .db $5F

    XPosTileEntries.8.XPos:         .db $40
    YPosEntries.8.YPos:             .db $08
    XPosTileEntries.8.TileIndex:    .db $5F

.ENDST

.STRUCT SpriteTable
    YPosEntries INSTANCEOF SAT_YPosEntry 2
    XPosTileEntries INSTANCEOF SAT_XPosTileEntry 2
.ENDST

.DSTRUCT MySpriteTable INSTANCEOF SpriteTable VALUES

    XPosTileEntries.1.XPos:         .db $7C
    YPosEntries.1.YPos:             .db $5F
    XPosTileEntries.1.TileIndex:    .db $5F

    YPosEntries.2.YPos:             .db VDP_SAT_STOP_SPRITES_YVALUE ; Sentinel.
.ENDST

.DSTRUCT CornerChar INSTANCEOF NameTableEntry VALUES
    TileIndex:  .db 'J'
    Flags:      .db VDP_NAMETABLE_ENTRY_VFLIP | VDP_NAMETABLE_ENTRY_HFLIP | VDP_NAMETABLE_ENTRY_BGPRIORITY
.ENDST

MessageHelloBegin:  
.DW 'H', 'E', 'L', 'L', 'O'
MessageHelloEnd:

MessageWorldBegin:
.DW 'W', 'O', 'R', 'L', 'D'
MessageWorldEnd

PaletteBegin:
; BG Palette Entry 0 == color 0 (black)
.db VDP_PALETTE_BG_PALETTE_INDEX + 0, $00
; BG Palette Entry 1 == color $3F (white)
.db VDP_PALETTE_BG_PALETTE_INDEX + 1, (3 << VDP_PALETTE_RED_SHIFT) | (3 << VDP_PALETTE_GREEN_SHIFT) | (3 << VDP_PALETTE_BLUE_SHIFT)
; BG Palette Entry 7 == color $30 (blue)
.db VDP_PALETTE_BG_PALETTE_INDEX + 7, (3 << VDP_PALETTE_BLUE_SHIFT)
; BG Palette Entry 14 == color $0C (green)
.db VDP_PALETTE_BG_PALETTE_INDEX + 14, (3 << VDP_PALETTE_GREEN_SHIFT)

; Sprite Palette Entry 0 == color $03 (red).  REMEMBER:  Sprites treat entry 0 as clear :)
.db VDP_PALETTE_SPRITE_PALETTE_INDEX + 0, (3 << VDP_PALETTE_RED_SHIFT)
; Sprite Palette Entry 1 == color $3C (cyan)
.db VDP_PALETTE_SPRITE_PALETTE_INDEX + 1, (3 << VDP_PALETTE_GREEN_SHIFT) | (3 << VDP_PALETTE_BLUE_SHIFT)
PaletteEnd:

TileDataBegin:
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $18,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00
.db $18,$00,$00,$00,$00,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00

.db $6C,$00,$00,$00,$6C,$00,$00,$00,$6C,$00,$00,$00,$00,$00,$00,$00
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $36,$00,$00,$00,$36,$00,$00,$00,$7F,$00,$00,$00,$36,$00,$00,$00
.db $7F,$00,$00,$00,$36,$00,$00,$00,$36,$00,$00,$00,$00,$00,$00,$00

.db $0C,$00,$00,$00,$3F,$00,$00,$00,$68,$00,$00,$00,$3E,$00,$00,$00
.db $0B,$00,$00,$00,$7E,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00

.db $60,$00,$00,$00,$66,$00,$00,$00,$0C,$00,$00,$00,$18,$00,$00,$00
.db $30,$00,$00,$00,$66,$00,$00,$00,$06,$00,$00,$00,$00,$00,$00,$00

.db $38,$00,$00,$00,$6C,$00,$00,$00,$6C,$00,$00,$00,$38,$00,$00,$00
.db $6D,$00,$00,$00,$66,$00,$00,$00,$3B,$00,$00,$00,$00,$00,$00,$00

.db $0C,$00,$00,$00,$18,$00,$00,$00,$30,$00,$00,$00,$00,$00,$00,$00
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $0C,$00,$00,$00,$18,$00,$00,$00,$30,$00,$00,$00,$30,$00,$00,$00
.db $30,$00,$00,$00,$18,$00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00

.db $30,$00,$00,$00,$18,$00,$00,$00,$0C,$00,$00,$00,$0C,$00,$00,$00
.db $0C,$00,$00,$00,$18,$00,$00,$00,$30,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$18,$00,$00,$00,$7E,$00,$00,$00,$3C,$00,$00,$00
.db $7E,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$7E,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.db $00,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$30,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$7E,$00,$00,$00
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.db $00,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$06,$00,$00,$00,$0C,$00,$00,$00,$18,$00,$00,$00
.db $30,$00,$00,$00,$60,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$6E,$00,$00,$00,$7E,$00,$00,$00
.db $76,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $18,$00,$00,$00,$38,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$7E,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$06,$00,$00,$00,$0C,$00,$00,$00
.db $18,$00,$00,$00,$30,$00,$00,$00,$7E,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$06,$00,$00,$00,$1C,$00,$00,$00
.db $06,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $0C,$00,$00,$00,$1C,$00,$00,$00,$3C,$00,$00,$00,$6C,$00,$00,$00
.db $7E,$00,$00,$00,$0C,$00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00

.db $7E,$00,$00,$00,$60,$00,$00,$00,$7C,$00,$00,$00,$06,$00,$00,$00
.db $06,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $1C,$00,$00,$00,$30,$00,$00,$00,$60,$00,$00,$00,$7C,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $7E,$00,$00,$00,$06,$00,$00,$00,$0C,$00,$00,$00,$18,$00,$00,$00
.db $30,$00,$00,$00,$30,$00,$00,$00,$30,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$3E,$00,$00,$00
.db $06,$00,$00,$00,$0C,$00,$00,$00,$38,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00
.db $00,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00
.db $00,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$30,$00,$00,$00

.db $0C,$00,$00,$00,$18,$00,$00,$00,$30,$00,$00,$00,$60,$00,$00,$00
.db $30,$00,$00,$00,$18,$00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$7E,$00,$00,$00,$00,$00,$00,$00
.db $7E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $30,$00,$00,$00,$18,$00,$00,$00,$0C,$00,$00,$00,$06,$00,$00,$00
.db $0C,$00,$00,$00,$18,$00,$00,$00,$30,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$0C,$00,$00,$00,$18,$00,$00,$00
.db $18,$00,$00,$00,$00,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$6E,$00,$00,$00,$6A,$00,$00,$00
.db $6E,$00,$00,$00,$60,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$7E,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00

.db $7C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$7C,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$7C,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00
.db $60,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $78,$00,$00,$00,$6C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$6C,$00,$00,$00,$78,$00,$00,$00,$00,$00,$00,$00

.db $7E,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00,$7C,$00,$00,$00
.db $60,$00,$00,$00,$60,$00,$00,$00,$7E,$00,$00,$00,$00,$00,$00,$00

.db $7E,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00,$7C,$00,$00,$00
.db $60,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$60,$00,$00,$00,$6E,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$7E,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00

.db $7E,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$7E,$00,$00,$00,$00,$00,$00,$00

.db $3E,$00,$00,$00,$0C,$00,$00,$00,$0C,$00,$00,$00,$0C,$00,$00,$00
.db $0C,$00,$00,$00,$6C,$00,$00,$00,$38,$00,$00,$00,$00,$00,$00,$00

.db $66,$00,$00,$00,$6C,$00,$00,$00,$78,$00,$00,$00,$70,$00,$00,$00
.db $78,$00,$00,$00,$6C,$00,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00

.db $60,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00
.db $60,$00,$00,$00,$60,$00,$00,$00,$7E,$00,$00,$00,$00,$00,$00,$00

.db $63,$00,$00,$00,$77,$00,$00,$00,$7F,$00,$00,$00,$6B,$00,$00,$00
.db $6B,$00,$00,$00,$63,$00,$00,$00,$63,$00,$00,$00,$00,$00,$00,$00

.db $66,$00,$00,$00,$66,$00,$00,$00,$76,$00,$00,$00,$7E,$00,$00,$00
.db $6E,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $7C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$7C,$00,$00,$00
.db $60,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00
.db $6A,$00,$00,$00,$6C,$00,$00,$00,$36,$00,$00,$00,$00,$00,$00,$00

.db $7C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$7C,$00,$00,$00
.db $6C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00

.db $3C,$00,$00,$00,$66,$00,$00,$00,$60,$00,$00,$00,$3C,$00,$00,$00
.db $06,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $7E,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00

.db $66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$3C,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00

.db $63,$00,$00,$00,$63,$00,$00,$00,$6B,$00,$00,$00,$6B,$00,$00,$00
.db $7F,$00,$00,$00,$77,$00,$00,$00,$63,$00,$00,$00,$00,$00,$00,$00

.db $66,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$18,$00,$00,$00
.db $3C,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00

.db $66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00

.db $7E,$00,$00,$00,$06,$00,$00,$00,$0C,$00,$00,$00,$18,$00,$00,$00
.db $30,$00,$00,$00,$60,$00,$00,$00,$7E,$00,$00,$00,$00,$00,$00,$00

.db $7C,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00
.db $60,$00,$00,$00,$60,$00,$00,$00,$7C,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$60,$00,$00,$00,$30,$00,$00,$00,$18,$00,$00,$00
.db $0C,$00,$00,$00,$06,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $3E,$00,$00,$00,$06,$00,$00,$00,$06,$00,$00,$00,$06,$00,$00,$00
.db $06,$00,$00,$00,$06,$00,$00,$00,$3E,$00,$00,$00,$00,$00,$00,$00

.db $18,$00,$00,$00,$3C,$00,$00,$00,$66,$00,$00,$00,$42,$00,$00,$00
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$00,$00,$00

.db $1C,$00,$00,$00,$36,$00,$00,$00,$30,$00,$00,$00,$7C,$00,$00,$00
.db $30,$00,$00,$00,$30,$00,$00,$00,$7E,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$3C,$00,$00,$00,$06,$00,$00,$00
.db $3E,$00,$00,$00,$66,$00,$00,$00,$3E,$00,$00,$00,$00,$00,$00,$00

.db $60,$00,$00,$00,$60,$00,$00,$00,$7C,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$7C,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$3C,$00,$00,$00,$66,$00,$00,$00
.db $60,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $06,$00,$00,$00,$06,$00,$00,$00,$3E,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$3E,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$3C,$00,$00,$00,$66,$00,$00,$00
.db $7E,$00,$00,$00,$60,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $1C,$00,$00,$00,$30,$00,$00,$00,$30,$00,$00,$00,$7C,$00,$00,$00
.db $30,$00,$00,$00,$30,$00,$00,$00,$30,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$3E,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$3E,$00,$00,$00,$06,$00,$00,$00,$3C,$00,$00,$00

.db $60,$00,$00,$00,$60,$00,$00,$00,$7C,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00

.db $18,$00,$00,$00,$00,$00,$00,$00,$38,$00,$00,$00,$18,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $18,$00,$00,$00,$00,$00,$00,$00,$38,$00,$00,$00,$18,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$70,$00,$00,$00

.db $60,$00,$00,$00,$60,$00,$00,$00,$66,$00,$00,$00,$6C,$00,$00,$00
.db $78,$00,$00,$00,$6C,$00,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00

.db $38,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$36,$00,$00,$00,$7F,$00,$00,$00
.db $6B,$00,$00,$00,$6B,$00,$00,$00,$63,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$7C,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$3C,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$7C,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$7C,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$3E,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$3E,$00,$00,$00,$06,$00,$00,$00,$07,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$6C,$00,$00,$00,$76,$00,$00,$00
.db $60,$00,$00,$00,$60,$00,$00,$00,$60,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$3E,$00,$00,$00,$60,$00,$00,$00
.db $3C,$00,$00,$00,$06,$00,$00,$00,$7C,$00,$00,$00,$00,$00,$00,$00

.db $30,$00,$00,$00,$30,$00,$00,$00,$7C,$00,$00,$00,$30,$00,$00,$00
.db $30,$00,$00,$00,$30,$00,$00,$00,$1C,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$66,$00,$00,$00,$3E,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$3C,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$63,$00,$00,$00,$6B,$00,$00,$00
.db $6B,$00,$00,$00,$7F,$00,$00,$00,$36,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$00,$3C,$00,$00,$00
.db $18,$00,$00,$00,$3C,$00,$00,$00,$66,$00,$00,$00,$00,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$66,$00,$00,$00,$66,$00,$00,$00
.db $66,$00,$00,$00,$3E,$00,$00,$00,$06,$00,$00,$00,$3C,$00,$00,$00

.db $00,$00,$00,$00,$00,$00,$00,$00,$7E,$00,$00,$00,$0C,$00,$00,$00
.db $18,$00,$00,$00,$30,$00,$00,$00,$7E,$00,$00,$00,$00,$00,$00,$00

.db $0C,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$70,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$0C,$00,$00,$00,$00,$00,$00,$00

.db $18,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$00,$00,$00,$00

.db $30,$00,$00,$00,$18,$00,$00,$00,$18,$00,$00,$00,$0E,$00,$00,$00
.db $18,$00,$00,$00,$18,$00,$00,$00,$30,$00,$00,$00,$00,$00,$00,$00

.db $31,$00,$00,$00,$6B,$00,$00,$00,$46,$00,$00,$00,$00,$00,$00,$00
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

.db $FF,$00,$00,$00,$FF,$00,$00,$00,$FF,$00,$00,$00,$FF,$00,$00,$00
.db $FF,$00,$00,$00,$FF,$00,$00,$00,$FF,$00,$00,$00,$FF,$00,$00,$00
TileDataEnd:

TileData_1bpp_Begin:
.db %00011000
.db %00111000
.db %01011000
.db %00011000
.db %00011000
.db %00011000
.db %00111100
.db %01111110

.db %00000001
.db %00000010
.db %00000100
.db %00001000
.db %00010000
.db %00100000
.db %01000000
.db %10000000

TileData_1bpp_End:

.ENDS

.RAMSECTION "Sprite Chains" SLOT 3
SpriteChain2 INSTANCEOF SpriteChain
SpriteChain3 INSTANCEOF SpriteChain
SpriteChain4 INSTANCEOF SpriteChain
SpriteChain5 INSTANCEOF SpriteChain
SpriteChain6 INSTANCEOF SpriteChain
SpriteChain7 INSTANCEOF SpriteChain
SpriteChain8 INSTANCEOF SpriteChain
SpriteChain9 INSTANCEOF SpriteChain

.ENDS


.SECTION "Mode Manager Test" FREE
.DSTRUCT Mode1 INSTANCEOF ApplicationMode VALUES
    VideoInterruptJumpTarget:   .dw Mode1VideoInterruptHandler
    OnNMI:                      .dw ModeDefaultHandler
    OnActive:                   .dw Mode1ActiveHandler
    OnInactive:                 .dw Mode1InactiveHandler
    OnUpdate:                   .dw Mode1UpdateHandler
    OnRenderPrep:               .dw ModeDefaultHandler
    OnEvent:                    .dw ModeDefaultHandler  
.ENDST
.DSTRUCT Mode2 INSTANCEOF ApplicationMode VALUES
    VideoInterruptJumpTarget:   .dw ModeDefaultHandler
    OnNMI:                      .dw ModeDefaultHandler
    OnActive:                   .dw Mode2ActiveHandler
    OnInactive:                 .dw Mode2InactiveHandler
    OnUpdate:                   .dw ModeDefaultHandler
    OnRenderPrep:               .dw ModeDefaultHandler
    OnEvent:                    .dw ModeDefaultHandler  
.ENDST

ModeDefaultHandler:
    ret

Mode1ActiveHandler:
    ret

Mode1UpdateHandler:
    call    InputManager_OnUpdate
    ret

Mode1InactiveHandler:
    ret

Mode2ActiveHandler:
    ret

Mode2InactiveHandler:
    ret

.ENDS

.RAMSECTION "My FSM" SLOT 3
    gMyFSM INSTANCEOF FSM
.ENDS

.SECTION "FSM Test" FREE
State_NULL:
    and a       ; Clear carry.
    ret

State1_OnEnter:
    ld  hl, MyFSM_State2
    scf         ; Indicate transition
    ret

State2_OnUpdate:
    ld  hl, MyFSM_State3
    scf         ; Indicate transition
    ret

State4_OnEnter:
    ld  hl, MyFSM_State5
    scf         ; Indicate transition
    ret

.DSTRUCT MyFSM_State1 INSTANCEOF State VALUES
    OnUpdate:   .dw State_NULL      
    OnEnter:    .dw State1_OnEnter 
    OnExit:     .dw State_NULL
.ENDST  
.DSTRUCT MyFSM_State2 INSTANCEOF State VALUES
    OnUpdate:   .dw State2_OnUpdate      
    OnEnter:    .dw State_NULL 
    OnExit:     .dw State_NULL
.ENDST
.DSTRUCT MyFSM_State3 INSTANCEOF State VALUES
    OnUpdate:   .dw State_NULL      
    OnEnter:    .dw State_NULL 
    OnExit:     .dw State_NULL
.ENDST
.DSTRUCT MyFSM_State4 INSTANCEOF State VALUES
    OnUpdate:   .dw State_NULL      
    OnEnter:    .dw State4_OnEnter 
    OnExit:     .dw State_NULL
.ENDST
.DSTRUCT MyFSM_State5 INSTANCEOF State VALUES
    OnUpdate:   .dw State_NULL      
    OnEnter:    .dw State_NULL 
    OnExit:     .dw State_NULL
.ENDST

Mode1VideoInterruptHandler:
    ex      af, af'
        in  a, (VDP_STATUS_PORT)    ; Satisfy the interrupt

/*
        ; A little hack to slam the first sprite's X position
        ; Set the VRAM address
        ; Low byte first
        ld      a, (VDP_SAT_START_LOC & $FF) + VDP_SAT_XTABLE_OFFSET    ; Slam to 8-bit
        out     (VDP_CONTROL_PORT), a
        ; High byte + command
        ld      a, (VDP_SAT_START_LOC >> 8) | VDP_COMMAND_MASK_VRAM_WRITE
        out     (VDP_CONTROL_PORT), a

        inc     e
        ld      a, e
        out     (VDP_DATA_PORT), a
*/
    ex      af, af'
    ret

.ENDS