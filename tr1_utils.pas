unit tr1_utils;

{$mode delphi}{$H+}

interface

uses Classes, SysUtils, tr_types{, anysortunit};

type
  TTR1VertexData = record
    data  : array of uint16;
    count : integer;
  end;

  TTR1Room = record
    base       : tr1_room_info;
    roomData   : TTR1VertexData;

    doorsCount : integer;
    doors      : array of tr1_room_door;

    zSector    : integer;
    xSector    : integer;
    sectors    : array of tr1_room_sector;

    intensity  : int16;
    lightsCount : integer;
    light      : array of tr1_room_light;

    meshesCount : integer;
    meshes      : array of tr1_room_staticmesh;

    alterRoom   : int16;
    flags       : uint16;
  end;

  TTR1Level = record
    version : LongWord;

    TextTileCount  : LongWord;
    //PaletteOffset : QWord; // offset in the file
    TextTile      : array of tr1_textile;
    unused        : LongWord;

    RoomCount  : LongWord;
    Rooms      : array of TTR1Room;

    FloorCount : Integer;
    Floor          : array of uint16;
    MeshDataCount  : Integer;
    MeshData       : array of byte;

    MeshPtrCount   : Integer;
    MeshPtr        : array of uint32;

    AnimationCount : Integer;
    Animation      : array of tr1_animation;

    StateChCount   : Integer;
    StateCh        : array of tr_state_change;

    AnimDispCount  : Integer;
    AnimDisp       : array of tr_anim_dispatch;

    AnimCmdCount   : integer;
    AnimCmd        : array of tr_anim_command;

    MeshTreeCount  : integer;
    MeshTree       : array of tr1_meshtree_raw;

    FrameCount     : integer;
    Frame          : array of uint16;

    ModelCount     : integer;
    Model          : array of tr1_model;

    StaticMeshCount : integer;
    StaticMesh      : array of tr_staticmesh;

    ObjTexCount : integer;
    ObjTex      : array of tr1_object_texture;

    SprTexCount : integer;
    SprTex      : array of tr1_sprite_texture;

    SprSeqCount : integer;
    SprSeq      : array of tr1_sprite_sequence;

    CameraCount : integer;
    Camera      : array of tr1_camera;

    SndSrcCount : integer;
    SndSrc      : array of tr1_sound_source;

    BoxCount    : integer;
    Box         : array of tr1_box;

    OverlapCount : integer;
    Overlap      : array of uint16;

    Zone         : array of tr1_zone;

    AnimTexCount : Integer;
    AnimTex      : array of uint16;

    ItemCount    : Integer;
    Item         : array of tr1_item;

    lightmap     : array {[0..32*256-1]} of uint8;
    pallette     : array {[0..255]} of tr_colour3;

    CinFrameCount : integer;
    CinFrame      : array of tr1_cinematic_frame;

    DemoDataCount : integer;
    DemoData      : array of uint8;

    SndMap        : array {[0..255]} of int16;

    SndDetailCount : Integer;
    SndDetail      : array of tr1_sound_details;

    SamplesCount   : integer;
    SamplesData    : array of byte;

    SampleIndexCount : integer;
    SampleIndex      : array of UInt32;
  end;

function ReadLevel(s: TStream; var lvl: TTR1level; aforced: Boolean = false): Boolean; overload;
function ReadLevel(const fn: string; var lvl: TTR1level; aforced: Boolean = false): Boolean; overload;

function WriteLevel(s: TStream; const lvl: TTR1level): Boolean; overload;
function WriteLevel(const fn: string; const lvl: TTR1level): Boolean; overload;

procedure SaveDataToFile(const data; size: integer; const dstFileName: string);

const
  BraidMesh : array [0..5] of array [0..139] of byte = (
    ($08,$00,$07,$00,$13,$00,$80,$01,$00,$00,$06,$00,$0D,$00,$0C,$00,$22,$00,$00,$00,$F6,$FF,$22,$00,$F4,$FF,$0D,$00,$22,$00,$0F,$00,$0D,$00,$00,$00,$00,$00,$F5,$FF,$00,$00,$F2,$FF,$0E,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$54,$2E,$CE,$FE,$ED,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00),
    ($07,$00,$05,$00,$13,$00,$5B,$01,$00,$00,$06,$00,$0B,$00,$09,$00,$23,$00,$FF,$FF,$F7,$FF,$23,$00,$F5,$FF,$0A,$00,$23,$00,$0D,$00,$0B,$00,$00,$00,$FF,$FF,$F5,$FF,$00,$00,$F3,$FF,$0B,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$53,$2E,$CE,$FE,$ED,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00),
    ($06,$00,$05,$00,$12,$00,$39,$01,$00,$00,$06,$00,$09,$00,$08,$00,$22,$00,$00,$00,$F8,$FF,$22,$00,$F7,$FF,$09,$00,$22,$00,$0B,$00,$09,$00,$00,$00,$FF,$FF,$F7,$FF,$00,$00,$F5,$FF,$0A,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$54,$2E,$CE,$FE,$ED,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00),
    ($06,$00,$04,$00,$12,$00,$1E,$01,$00,$00,$06,$00,$08,$00,$06,$00,$23,$00,$00,$00,$F9,$FF,$23,$00,$F8,$FF,$07,$00,$23,$00,$09,$00,$08,$00,$00,$00,$00,$00,$F8,$FF,$00,$00,$F7,$FF,$08,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$53,$2E,$CE,$FE,$ED,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00),
    ($05,$00,$04,$00,$11,$00,$02,$01,$00,$00,$06,$00,$06,$00,$05,$00,$22,$00,$00,$00,$FB,$FF,$22,$00,$FA,$FF,$06,$00,$22,$00,$08,$00,$07,$00,$00,$00,$00,$00,$F9,$FF,$00,$00,$F8,$FF,$07,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$53,$2E,$CE,$FE,$ED,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00),
    ($05,$00,$04,$00,$11,$00,$EE,$00,$00,$00,$06,$00,$05,$00,$04,$00,$23,$00,$00,$00,$FC,$FF,$23,$00,$FB,$FF,$04,$00,$23,$00,$06,$00,$05,$00,$00,$00,$00,$00,$FB,$FF,$00,$00,$FA,$FF,$06,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$54,$2E,$CE,$FE,$EC,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00)
  );

procedure DumpModels(const models: array of tr1_model; modelsCount: Integer);
procedure DumpLevel(const lvl:TTR1Level);

function ReadDemoWAD1(s: TStream; var lvl: TTR1level; aforced: Boolean = false): Boolean; overload;
function ReadDemoWAD1(const fn: string; var lvl: TTR1level; aforced: Boolean = false): Boolean; overload;

function ReadDemoTOM(s: TStream; var lvl: TTR1level; aforced: Boolean = false): Boolean; overload;
function ReadDemoTOM(const fn: string; var lvl: TTR1level; aforced: Boolean = false): Boolean; overload;

const
  TR1_Palette : array [0..256*3-1] of byte = (
 $02,$00,$00,$00,$00,$00,$06,$01,$00,$0C,$03,$01,$12,$07,$01,$18
,$0A,$01,$16,$11,$00,$1A,$14,$03,$1C,$17,$0B,$22,$16,$0B,$28,$1A
,$0E,$29,$1E,$12,$2C,$20,$14,$32,$24,$16,$37,$28,$19,$3A,$30,$1C
,$3E,$34,$1E,$1D,$21,$1D,$16,$19,$16,$13,$14,$13,$10,$10,$10,$0C
,$0C,$0C,$09,$09,$08,$0E,$0B,$05,$14,$0C,$05,$16,$10,$09,$15,$05
,$08,$0F,$03,$06,$1F,$08,$08,$28,$0A,$07,$2E,$0B,$08,$23,$1C,$0E
,$19,$1D,$19,$20,$24,$21,$24,$28,$24,$27,$2C,$28,$2C,$31,$2C,$33
,$33,$33,$39,$39,$39,$3F,$3F,$26,$3F,$3B,$22,$17,$28,$27,$16,$2F
,$31,$21,$33,$33,$2F,$37,$37,$2F,$2F,$23,$2B,$17,$22,$37,$3D,$33
,$27,$25,$29,$0C,$0C,$0C,$0C,$05,$00,$10,$0A,$05,$14,$0F,$0A,$19
,$14,$0F,$1D,$19,$14,$22,$1E,$19,$26,$23,$1D,$2A,$28,$22,$2F,$2D
,$27,$33,$32,$2C,$38,$37,$31,$10,$07,$02,$13,$0A,$03,$17,$0D,$05
,$1A,$10,$07,$1D,$14,$09,$21,$17,$0B,$24,$1A,$0D,$27,$1E,$0E,$01
,$02,$00,$05,$05,$02,$08,$08,$04,$0C,$0B,$07,$10,$0E,$09,$13,$11
,$0B,$17,$14,$0E,$3F,$3F,$00,$14,$00,$00,$1B,$02,$03,$22,$04,$06
,$2A,$06,$08,$31,$08,$0B,$38,$0A,$0E,$0A,$09,$06,$10,$0F,$0A,$15
,$15,$0D,$1B,$1B,$11,$21,$21,$14,$27,$27,$17,$2D,$2D,$1B,$02,$05
,$0C,$04,$09,$0E,$05,$0C,$11,$06,$0F,$13,$08,$13,$16,$09,$16,$19
,$0A,$19,$1B,$1D,$11,$08,$21,$16,$0D,$26,$1B,$12,$2A,$20,$17,$2F
,$25,$1C,$33,$2A,$21,$38,$2F,$25,$3F,$00,$3F,$3F,$00,$3F,$3F,$00
,$3F,$3F,$00,$3F,$3F,$00,$3F,$3F,$00,$3F,$3F,$00,$3F,$3F,$00,$3F
,$3F,$00,$3F,$3F,$00,$3F,$3F,$00,$3F,$3F,$00,$3F,$3F,$00,$3F,$3F
,$00,$3F,$3F,$3B,$17,$39,$32,$12,$34,$2A,$0F,$2F,$21,$0B,$29,$19
,$07,$24,$10,$05,$1F,$08,$00,$28,$20,$11,$1C,$13,$0A,$1C,$1C,$0C
,$00,$00,$00,$07,$05,$02,$0A,$08,$03,$0F,$0C,$05,$11,$0F,$07,$13
,$11,$09,$15,$13,$0C,$18,$15,$0E,$1A,$18,$11,$1C,$1A,$13,$1F,$1D
,$17,$23,$20,$1A,$26,$24,$1E,$29,$27,$22,$2D,$2A,$27,$30,$2D,$2B
,$34,$31,$2F,$37,$34,$33,$3F,$3F,$3F,$3C,$3A,$3A,$35,$32,$31,$2E
,$2A,$28,$2A,$25,$23,$26,$20,$1F,$22,$1C,$1B,$1F,$19,$18,$1E,$17
,$17,$1A,$14,$13,$16,$11,$10,$11,$0E,$0E,$0D,$0B,$0A,$08,$07,$07
,$25,$17,$0A,$2A,$1C,$0D,$2F,$21,$10,$30,$24,$14,$32,$27,$17,$33
,$2A,$1B,$34,$2D,$21,$34,$30,$26,$35,$33,$2B,$3D,$39,$32,$13,$13
,$14,$15,$15,$16,$17,$17,$18,$19,$19,$1A,$1B,$1B,$1C,$1D,$1D,$1E
,$1F,$1F,$20,$21,$21,$22,$23,$23,$24,$25,$25,$26,$27,$27,$28,$29
,$29,$29,$2B,$2B,$2B,$2D,$2D,$2E,$30,$30,$30,$32,$32,$33,$35,$35
,$35,$38,$38,$38,$3A,$3A,$3A,$3D,$3D,$3D,$3F,$3F,$3F,$3F,$3F,$3F
,$05,$06,$00,$05,$09,$00,$08,$0E,$00,$0F,$15,$00,$16,$1C,$00,$1D
,$24,$00,$25,$2B,$00,$2B,$2F,$0D,$31,$33,$1A,$32,$37,$1E,$32,$3B
,$23,$33,$3F,$27,$38,$38,$26,$33,$31,$21,$2E,$2B,$1C,$29,$25,$17
,$25,$1F,$12,$22,$1C,$10,$20,$1A,$0E,$1E,$18,$0C,$1B,$15,$0A,$14
,$10,$09,$11,$0D,$06,$12,$11,$10,$17,$15,$13,$1B,$19,$17,$20,$1E
,$1B,$23,$20,$1D,$25,$23,$1F,$28,$26,$22,$2C,$2A,$26,$30,$2E,$2A
,$06,$06,$05,$06,$09,$09,$09,$0D,$0D,$0C,$10,$10,$0F,$14,$14,$13
,$17,$18,$19,$1D,$1D,$1E,$21,$22,$23,$26,$28,$28,$2A,$2D,$2F,$2F
,$31,$08,$08,$06,$0A,$0A,$08,$0B,$0C,$09,$0D,$0D,$0A,$10,$0F,$0C
,$12,$12,$0E,$15,$14,$10,$18,$17,$12,$1B,$19,$14,$1E,$1D,$17,$06
,$08,$06,$08,$0B,$08,$0A,$0D,$0A,$0D,$0F,$0B,$10,$11,$0D,$12,$14
,$0E,$15,$16,$0F,$19,$1A,$13,$1C,$1E,$16,$1F,$21,$19,$21,$23,$1B
  );

var
  TR1Debug : Boolean = false;

  TR1DefPallette : ptr1_palette = @TR1_Palette[0];

function ModelToWavefrontStr(const model: tr1_model;
  const ptrs: array of uint32;
  const meshdata: array of byte;
  const meshtree: array of tr1_meshtree_raw;
  isWad: Boolean;
  version: Integer;
  scale: double = 1/255): string;

procedure SaveTileToBmp32(const dst: string; const tl: tr_textile8; pal: ptr1_palette = nil);

type
  tr1_object_texture_wad = packed record // 8 bytes
    x,y   : uint8;
    Tile  : uint16; // index into textile list
    unk1  : uint8;  // always $FF
    w     : uint8;  // width  -1
    unk2  : uint8;  // always $FF
    h     : uint8;  // height - 1
  end;

 tr1_animation_wad = packed record    { 32 bytes TR1/2/3 40 bytes TR4 }
    frame_offset        : uint32;  { byte offset into Frames[] (divide by 2 for Frames[i]) }
    frame_rate          : uint8;   { Engine ticks per frame }
    frame_size          : uint8;   { number of int16's in Frames[] used by this animation }
    state_id            : uint16;
    unk                 : array [0..1] of uint8;
    frame_start         : uint16;  { first frame in this animation }
    frame_end           : uint16;  { last frame in this animation (numframes = (End - Start) + 1) }
    next_animation      : uint16;
    next_frame          : uint16;
    num_state_changes   : uint16;
    state_change_offset : uint16;  { offset into StateChanges[] }
    num_anim_commands   : uint16;  { How many of them to use. }
    anim_command        : uint16;  { offset into AnimCommand[] }
  end;


procedure DefaultLvl(var lvl: TTR1Level);

implementation

procedure DefaultLvl(var lvl: TTR1Level);
begin
  lvl.version:=FILEVERSION_TR1;

  if length(lvl.lightmap)=0 then
    SetLEngth(lvl.lightmap, 32*256);
  if length(lvl.pallette)=0 then begin
    SetLength(lvl.pallette, 256);
    move(TR1_Palette[0], lvl.pallette[0], length(lvl.pallette)*sizeof(tr_colour3));
  end;
  if length(lvl.SndMap)=0 then
    SetLength(lvl.SndMap, 256);
end;

procedure DoReadTr1Room1Data(const s:TStream; var room : TTR1Room);
var
  cnt : Integer;
begin
  s.ReadBuffer(room.base, sizeof(room.base));
  room.roomData.count:=s.ReadDWord;
  SetLength(room.roomData.data, room.roomData.count);
  if room.roomData.count>0 then
    s.Read(room.roomData.data[0], room.roomData.count * sizeof(uint16));
  (*
  cnt:=s.ReadWord;
  //writeln('  verticies: ', cnt,' ',sizeof(tr1_vertex_room));
  s.Position:=s.Position+cnt*sizeoF(tr1_vertex_room);

  cnt:=s.ReadWord;
  //writeln('  faces:     ', cnt,' ',sizeof(tr1_face4));
  s.Position:=s.Position+cnt*sizeoF(tr1_face4);

  cnt:=s.ReadWord;
  //writeln('  triangles: ', cnt,' ',sizeof(tr1_face3));
  s.Position:=s.Position+cnt*sizeoF(tr1_face3);

  cnt:=s.ReadWord;
  //writeln('  sprites:   ', cnt,' ',sizeoF(tr1_room_sprite));
  s.Position:=s.Position+cnt*sizeoF(tr1_room_sprite);
  *)
  //writeln('position check: ', s.Position);
  room.doorsCount:=s.ReadWord;
  SetLength(room.doors, room.doorsCount);
  //writeln('  doors:     ', room.doorsCount,' sizeof ',sizeof(tr1_room_door));
  //s.Position:=s.Position+room.doorsCount*sizeoF(tr1_room_door);
  if room.doorsCount>0 then
    s.Read(room.doors[0], room.doorsCount*sizeoF(tr1_room_door) );

  room.zSector:=s.ReadWord;
  room.xSector:=s.ReadWord;
  SetLength(room.sectors, room.zSector*room.xSector);
  if room.zSector*room.xSector>0 then
    s.Read( room.sectors[0], room.zSector*room.xSector*sizeof(tr1_room_sector));
  //s.Position:=s.Position+(room.zSector*room.xSector*sizeof(tr1_room_sector));

  if TR1Debug then
    writeln('  sectors: ', room.zSector,'x',room.xSector,' sizeof ',sizeof(tr1_room_sector));

  room.intensity:=s.ReadWord;
  room.lightsCount:=s.ReadWord;
  SetLength(room.light, room.lightsCount);

  if TR1Debug then
    writeln('  lights:    ', room.lightsCount,' sizeof ',sizeof(tr1_room_light));

  if room.lightsCount>0 then
    s.Read(room.light[0], room.lightsCount*sizeof(tr1_room_light));
    //s.Position:=s.Position+(room.lightsCount*sizeof(tr1_room_light));

  room.meshesCount:=s.ReadWord;

  if TR1Debug then
    writeln('  meshes:    ', room.meshesCount,' sizeof ',sizeof(tr1_room_staticmesh));

  SetLength(room.meshes, room.meshesCount);
  if room.meshesCount>0 then
    s.Read(room.meshes[0], room.meshesCount*sizeof(tr1_room_staticmesh));

  room.alterRoom:=int16(s.ReadWord);
  room.flags:=uint16(s.ReadWord);
end;

// DoReadLevel doesn't catch any exceptions of TStream might send
function DoReadLevel1(const s: TStream; var lvl: TTR1level): Boolean;
var
  i : integer;
  fallback : Int64;
begin
  lvl.TextTileCount:=s.ReadDword;
  SetLength(lvl.TextTile, lvl.TextTileCount);
  if lvl.TextTileCount>0 then
    s.Read(lvl.TextTile[0], lvl.TextTileCount*sizeof(tr_textile8_t));

  lvl.unused:=s.ReadDWord;

  if TR1Debug then
    writeln('unused value = ', lvl.unused);

  lvl.RoomCount:=s.ReadWord;
  SetLength(lvl.Rooms, lvl.RoomCount);
  for i:=0 to lvl.roomCount-1 do begin
    DoReadTr1Room1Data(s, lvl.Rooms[i]);
  end;

  lvl.FloorCount:=s.ReadDWord;
  SetLength(lvl.Floor, lvl.FloorCount);
  if lvl.FloorCount>0 then
    s.Read( lvl.Floor[0], lvl.FloorCount*sizeof(uint16));

  lvl.MeshDataCount:=s.ReadDWord;

  if TR1Debug then
    writeln('mesh data: ', lvl.MeshDataCount);

  SetLength(lvl.MeshData, lvl.MeshDataCount*sizeof(uint16));
  if lvl.MeshDataCount>0 then
    s.Read(lvl.MeshData[0], lvl.MeshDataCount*sizeof(uint16));

  lvl.MeshPtrCount:=s.ReadDWord;

  if TR1Debug then
    writeln('num mesh pointers: ', lvl.MeshPtrCount);

  SetLength(lvl.MeshPtr, lvl.MeshPtrCount);
  if lvl.MeshPtrCount>0 then
    s.Read(lvl.MeshPtr[0], lvl.MeshPtrCount*sizeof(uint32));
  //s.Position:=s.Position+cnt*4; // skipping over pointers

  lvl.AnimationCount:=s.ReadDWord;

  if TR1Debug then
    writeln('num of animations: ', lvl.AnimationCount,' ',sizeof(tr1_animation));

  SetLength(lvl.Animation, lvl.AnimationCount);
  if lvl.AnimationCount>0 then begin
    s.Read(lvl.Animation[0], length(lvl.Animation)*sizeof(tr1_animation));
  end;

  lvl.StateChCount:=s.ReadDWord;

  if TR1Debug then
    writeln('state changes: ', lvl.StateChCount,' ',sizeof(tr_state_change));

  SetLength(lvl.StateCh, lvl.StateChCount);
  if lvl.StateChCount>0 then begin
    s.Read(lvl.StateCh[0], length(lvl.StateCh)*sizeof(tr_state_change));
  end;

  lvl.AnimDispCount:=s.ReadDWord;

  if TR1Debug then
    writeln('anim disp: ', lvl.AnimationCount,' ',sizeof(tr_anim_dispatch));

  SetLength(lvl.AnimDisp, lvl.AnimDispCount);
  if lvl.AnimDispCount>0 then begin
    s.Read(lvl.AnimDisp[0], length(lvl.AnimDisp)*sizeof(tr_anim_dispatch));
  end;

  lvl.AnimCmdCount:=s.ReadDword;

  if TR1Debug then
    writeln('anim cmd: ', lvl.AnimCmdCount,' ',sizeof(tr_anim_command));

  SetLength(lvl.AnimCmd, lvl.AnimCmdCount);
  if lvl.AnimCmdCount>0 then
    s.Read(lvl.AnimCmd[0], lvl.AnimCmdCount*sizeof(tr_anim_command));

  lvl.MeshTreeCount:=s.ReadDWord;

  if TR1Debug then
    writeln('mesh tree: ', lvl.MeshTreeCount,' ',sizeof(tr1_meshtree_raw));

  SetLength(lvl.MeshTree, lvl.MeshTreeCount);
  if lvl.MeshTreeCount>0 then
    s.Read(lvl.MeshTree[0], lvl.MeshTreeCount*sizeof(tr1_meshtree_raw));

  lvl.FrameCount:=s.ReadDWord;

  if TR1Debug then
    writeln('Frame Count: ', lvl.FrameCount);

  SetLength(lvl.Frame, lvl.FrameCount);
  if lvl.FrameCount>0 then
    s.Read(lvl.Frame[0], lvl.FrameCount*sizeof(uint16));

  lvl.ModelCount:=s.ReadDWord;

  if TR1Debug then
    writeln('Movable Count: ', lvl.ModelCount,' ',sizeof(tr1_model));

  SetLength(lvl.Model, lvl.ModelCount);
  if lvl.ModelCount>0 then
    s.Read(lvl.Model[0], lvl.ModelCount*sizeof(tr1_model));

  lvl.StaticMeshCount:=s.ReadDWord;

  if TR1Debug then
    writeln('static meshes: ', lvl.StaticMeshCount,' ',sizeof(tr1_staticmesh));

  SetLength(lvl.StaticMesh, lvl.StaticMeshCount);
  if lvl.StaticMeshCount>0 then
    s.Read(lvl.StaticMesh[0], lvl.StaticMeshCount * sizeof(tr1_staticmesh));

  lvl.ObjTexCount:=s.ReadDword;

  if TR1Debug then
    writeln('object textures: ', lvl.ObjTexCount,' ',sizeof(tr1_object_texture));

  SetLength(lvl.ObjTex, lvl.ObjTexCount);
  if lvl.ObjTexCount>0 then
    s.Read(lvl.ObjTex[0], lvl.ObjTexCount * sizeof(tr1_object_texture));

  lvl.SprTexCount:=s.ReadDword;
  SetLength(lvl.SprTex, lvl.SprTexCount);

  if TR1Debug then
    writeln('sprite textures: ', lvl.SprTexCount,' ',sizeof(tr1_sprite_texture));

  if lvl.ObjTexCount>0 then
    s.Read(lvl.SprTex[0], lvl.SprTexCount * sizeof(tr1_sprite_texture));

  lvl.SprSeqCount:=s.ReadDWord;
  SetLength(lvl.SprSeq, lvl.SprSeqCount);

  if TR1Debug then
    writeln('sprite sequence: ', lvl.SprSeqCount,' ',sizeof(tr1_sprite_sequence));

  if lvl.SprSeqCount>0 then
    s.Read(lvl.SprSeq[0], lvl.SprSeqCount * sizeof(tr1_sprite_sequence));

  lvl.CameraCount:=s.ReadDWord;
  SetLength(lvl.Camera, lvl.CameraCount);

  if TR1Debug then
    writeln('camera: ', lvl.CameraCount,' ',sizeof(tr1_camera));

  if lvl.CameraCount>0 then
    s.Read(lvl.Camera[0], lvl.CameraCount * sizeof(tr1_camera));

  lvl.SndSrcCount:=s.ReadDWord;
  SetLength(lvl.SndSrc, lvl.SndSrcCount);

  if TR1Debug then
    writeln('sound sources: ', lvl.SndSrcCount,' ',sizeof(tr1_sound_source));

  if lvl.SndSrcCount>0 then
    s.Read(lvl.SndSrc[0], lvl.SndSrcCount * sizeof(tr1_sound_source));

  lvl.BoxCount:=s.ReadDWord;
  SetLength(lvl.Box, lvl.BoxCount);

  if TR1Debug then
    writeln('box: ', lvl.BoxCount,' ',sizeof(tr1_box));

  if lvl.BoxCount>0 then
    s.Read(lvl.Box[0], lvl.BoxCount * sizeof(tr1_box));

  lvl.OverlapCount:=s.ReadDWord;

  if TR1Debug then
    writeln('overlap:  ',lvl.OverlapCount);

  SetLength(lvl.Overlap, lvl.OverlapCount);
  if lvl.OverlapCount>0 then
    s.Read(lvl.Overlap[0], lvl.OverlapCount * sizeof(uint16));

  SetLength(lvl.Zone, lvl.BoxCount);
  if lvl.BoxCount>0 then
    s.Read(lvl.Zone[0], lvl.BoxCount * sizeof(tr1_zone));

  lvl.AnimTexCount:=s.ReadDWord;

  if TR1Debug then
    writeln('anim textues: ', lvl.AnimTexCount);

  SetLength(lvl.AnimTex, lvl.AnimTexCount);
  if lvl.AnimTexCount>0 then
    s.Read(lvl.AnimTex[0], lvl.AnimTexCount * sizeof(uint16));

  lvl.ItemCount:=s.ReadDWord;

  if TR1Debug then
    writeln('items: ',lvl.ItemCount,' ',sizeof(tr1_item));

  SetLength(lvl.Item, lvl.ItemCount);
  if lvl.ItemCount>0 then
    s.Read(lvl.Item[0], lvl.ItemCount*sizeof(tr1_item));

    //lightmap     : array {[0..32*256-1]} of uint8;
    //pallette     : array {[0..255]} of tr_colour3;
  SetLength(lvl.lightmap, 32*256);
  s.Read(lvl.lightmap[0], length(lvl.lightmap));
  SetLength(lvl.pallette, 256);
  s.Read(lvl.pallette[0], length(lvl.pallette)*sizeof(tr_colour3));

  fallback:=s.Position;

  lvl.CinFrameCount:=s.ReadWord;
  SetLength(lvl.CinFrame, lvl.CinFrameCount);

  if TR1Debug then
    writeln('cinematic: ',lvl.CinFrameCount,' ',sizeof(tr1_camera));

  if lvl.CinFrameCount>0 then
    s.Read(lvl.CinFrame[0], lvl.CinFrameCount*sizeof(tr1_camera));

  // 142ECA
  lvl.DemoDataCount:=s.ReadWord;
  SetLength(lvl.DemoData, lvl.DemoDataCount);
  if lvl.DemoDataCount>0 then begin
    s.Read(lvl.DemoData[0], lvl.DemoDataCount*sizeof(uint8));
  end;

  if TR1Debug then
    writeln('after demo: ', s.Position,' ', IntToHex(s.Position,8));

  SetLength(lvl.SndMap, 256);
  s.Read(lvl.SndMap[0], length(lvl.SndMap)*sizeof(uint16));

  lvl.SndDetailCount:=s.ReadDWord;
  if (s.Position + sizeof(tr1_sound_details)*lvl.SndDetailCount > s.Size) and (s.Size>0) then begin
    // ok, this seems to be an earlier demo level of TR1
    // it doesn't have any demo data or cin frames, SndMap must also be reread
    s.Position:=fallback;
    lvl.CinFrameCount:=0;
    lvl.DemoDataCount:=0;
    s.Read(lvl.SndMap[0], length(lvl.SndMap)*sizeof(uint16));
    lvl.SndDetailCount:=s.ReadDWord;
  end;

  SetLength(lvl.SndDetail, lvl.SndDetailCount);
  if lvl.SndDetailCount>0 then
    s.Read(lvl.SndDetail[0], sizeof(tr1_sound_details)*lvl.SndDetailCount);

{  if TR1Debug then
    writeln('before samples: ', s.Position,' ', IntToHex(s.Position,8),' wanted = $1430CA');}

  lvl.SamplesCount:=s.ReadDWord;

  if TR1Debug then
    writeln('samples size: ', lvl.SamplesCount);

  SetLength(lvl.SamplesData, lvl.SamplesCount);
  if lvl.SamplesCount>0 then
    s.Read(lvl.SamplesData[0], lvl.SamplesCount);

  writeln('about to read index: ', s.Position,' ',s.Size);
  lvl.SampleIndexCount := s.ReadDWord;
  SetLength(lvl.SampleIndex, lvl.SampleIndexCount);

  if TR1Debug then
    writeln('sample index: ', lvl.SampleIndexCount);

  if lvl.SampleIndexCount> 0 then
    s.Read(lvl.SampleIndex[0], lvl.SampleIndexCount*sizeof(UInt32));

  Result:=true;
end;

function DoReadLevel(s: TStream; var lvl: TTR1level; aforced: Boolean = false) : Boolean;
begin
  if TR1Debug then
    writeln('reading');
  lvl.version:=s.ReadDWord;
  if TR1Debug then
    writeln('version: ',lvl.version,' ',IntToHex(lvl.version,8));

  if (lvl.version=FILEVERSION_TR1) or aforced then
    Result:=DoReadLevel1(s, lvl)
  else
    Result:=false;
end;

function ReadLevel(s: TStream; var lvl: TTR1level; aforced: Boolean = false): Boolean;
begin
  try
    Result:=DoReadLevel(s, lvl, Aforced);
  except
    on e:exception do begin
      Result:=false;
    end;
  end;
end;

function ReadLevel(const fn: string; var lvl: TTR1level; aforced: Boolean): Boolean;
var
  fs : TfileStream;
begin
  try
    fs := TfileStream.Create(fn, fmOpenRead or fmShareDenyNone);;
    try
      Result:=ReadLevel(fs, lvl, aforced);
    finally
      fs.Free;
    end;
  except
    Result:=false;
  end;
end;

procedure DoWriteTr1Room1Data(s: TStream; const room: TTR1Room);
begin
  s.WriteBuffer(room.base, sizeof(room.base));

  s.WriteDWord(room.roomData.count);
  if room.roomData.count>0 then
    s.WriteBuffer(room.roomData.data[0], room.roomData.count*sizeof(uint16));

  //cnt:=s.ReadDWord; //number of data btu16's to follow (=RoomData) (4 bytes)

  //cnt:=s.ReadWord;
  //writeln('  verticies: ', cnt,' ',sizeof(tr1_vertex_room));
  //s.Position:=s.Position+cnt*sizeoF(tr1_vertex_room);

  //cnt:=s.ReadWord;
  //writeln('  faces:     ', cnt,' ',sizeof(tr1_face4));
  //s.Position:=s.Position+cnt*sizeoF(tr1_face4);

  //cnt:=s.ReadWord;
  //writeln('  triangles: ', cnt,' ',sizeof(tr1_face3));
  //s.Position:=s.Position+cnt*sizeoF(tr1_face3);

  //cnt:=s.ReadWord;
  //writeln('  sprites:   ', cnt,' ',sizeoF(tr1_room_sprite));
  //s.Position:=s.Position+cnt*sizeoF(tr1_room_sprite);

  //writeln('position check: ', s.Position);
  s.WriteWord( Word(room.doorsCount));
  if room.doorsCount>0 then
    s.WriteBuffer( room.doors[0], room.doorsCount*sizeoF(tr1_room_door));
  //room.doorsCount:=s.ReadWord;
  //writeln('  doors:     ', room.doorsCount,' sizeof ',sizeof(tr1_room_door));
  //s.Position:=s.Position+room.doorsCount*sizeoF(tr1_room_door);
  s.WriteWord(room.zSector);
  s.WriteWord(room.xSector);
  if room.zSector*room.xSector>0 then
    s.Write( room.sectors[0], room.zSector*room.xSector*sizeof(tr1_room_sector));

  s.WriteWord(room.intensity);
  s.WriteWord(room.lightsCount);
  if room.lightsCount>0 then
    s.Write(room.light[0], room.lightsCount*sizeof(tr1_room_light));

  s.WriteWord(room.meshesCount);
  if room.meshesCount>0 then
    s.Write(room.meshes[0], room.meshesCount*sizeof(tr1_room_staticmesh));

  s.WriteWord(room.alterRoom);
  s.WriteWord(room.flags);
end;

function DoWriteLevel(s: TStream; const lvl: TTR1level): Boolean;
var
  i : integer;
begin
  s.WriteDWord(lvl.version);
  s.WriteDWord(lvl.TextTileCount);
  if lvl.TextTileCount>0 then
    s.Write(lvl.TextTile[0], lvl.TextTileCount*sizeof(tr_textile8_t));
  s.WriteDWord(lvl.unused);

  s.WriteWord(lvl.RoomCount);
  for i:=0 to lvl.roomCount-1 do
    DoWriteTr1Room1Data(s, lvl.Rooms[i]);

  s.WriteDWord(lvl.FloorCount);
  if lvl.FloorCount>0 then
    s.Write( lvl.Floor[0],  lvl.FloorCount*sizeof(uint16));

  s.WriteDWord(lvl.MeshDataCount);
  if lvl.MeshDataCount>0 then
    s.Write(lvl.MeshData[0], lvl.MeshDataCount*sizeof(uint16));

  s.WriteDWord(lvl.MeshPtrCount);
  if lvl.MeshPtrCount>0 then
    s.Write(lvl.MeshPtr[0], lvl.MeshPtrCount*sizeof(uint32));

  s.WriteDWord(lvl.AnimationCount);
  if lvl.AnimationCount>0 then
    s.Write(lvl.Animation[0], length(lvl.Animation)*sizeof(tr1_animation));

  s.WriteDWord(lvl.StateChCount);
  if lvl.StateChCount>0 then
    s.Write(lvl.StateCh[0], length(lvl.StateCh)*sizeof(tr_state_change));

  s.WriteDWord(lvl.AnimDispCount);
  if lvl.AnimDispCount>0 then
    s.Write(lvl.AnimDisp[0], length(lvl.AnimDisp)*sizeof(tr_anim_dispatch));

  s.WriteDWord(lvl.AnimCmdCount);
  if lvl.AnimCmdCount>0 then
    s.Write(lvl.AnimCmd[0], lvl.AnimCmdCount*sizeof(tr_anim_command));

  s.WriteDWord(lvl.MeshTreeCount);
  if lvl.MeshTreeCount>0 then
    s.Write(lvl.MeshTree[0], lvl.MeshTreeCount*sizeof(tr1_meshtree_raw));

  s.WriteDword(lvl.FrameCount);
  if lvl.FrameCount>0 then
    s.Write(lvl.Frame[0], lvl.FrameCount*sizeof(uint16));

  s.WriteDWord(lvl.ModelCount);
  if lvl.ModelCount>0 then
    s.Write(lvl.Model[0], lvl.ModelCount*sizeof(tr1_model));

  s.WriteDWord(lvl.StaticMeshCount);
  if lvl.StaticMeshCount>0 then
    s.Write(lvl.StaticMesh[0], lvl.StaticMeshCount * sizeof(tr1_staticmesh));

  s.WriteDWord(lvl.ObjTexCount);
  if lvl.ObjTexCount>0 then
    s.Write(lvl.ObjTex[0], lvl.ObjTexCount * sizeof(tr1_object_texture));

  s.WriteDWord(lvl.SprTexCount);
  if lvl.ObjTexCount>0 then
    s.Write(lvl.SprTex[0], lvl.SprTexCount * sizeof(tr1_sprite_texture));

  s.WriteDword(lvl.SprSeqCount);
  if lvl.SprSeqCount>0 then
    s.Write(lvl.SprSeq[0], lvl.SprSeqCount * sizeof(tr1_sprite_sequence));

  s.WriteDWord(lvl.CameraCount);
  if lvl.CameraCount>0 then
    s.Write(lvl.Camera[0], lvl.CameraCount * sizeof(tr1_camera));

  s.WriteDWord(lvl.SndSrcCount);
  if lvl.SndSrcCount>0 then
    s.Write(lvl.SndSrc[0], lvl.SndSrcCount * sizeof(tr1_sound_source));

  s.WriteDWord(lvl.BoxCount);
  if lvl.BoxCount>0 then
    s.write(lvl.Box[0], lvl.BoxCount * sizeof(tr1_box));

  s.WriteDWord(lvl.OverlapCount);
  if lvl.OverlapCount>0 then
    s.Write(lvl.Overlap[0], lvl.OverlapCount * sizeof(uint16));

  if lvl.BoxCount>0 then
    s.Write(lvl.Zone[0], lvl.BoxCount * sizeof(tr1_zone));

  s.WriteDWord(lvl.AnimTexCount);
  if lvl.AnimTexCount>0 then
    s.Write(lvl.AnimTex[0], lvl.AnimTexCount * sizeof(uint16));

  s.WriteDword(lvl.ItemCount);
  if lvl.ItemCount>0 then
    s.Write(lvl.Item[0], lvl.ItemCount*sizeof(tr1_item));

  if length(lvl.lightmap)>0 then
    s.Write(lvl.lightmap[0], length(lvl.lightmap));
  if length(lvl.pallette)>0 then
    s.Write(lvl.pallette[0], length(lvl.pallette)*sizeof(tr_colour3));

  s.WriteWord(lvl.CinFrameCount);
  if lvl.CinFrameCount>0 then
    s.Write(lvl.CinFrame[0], lvl.CinFrameCount*sizeof(tr1_camera));

  s.WriteWord(lvl.DemoDataCount);
  if lvl.DemoDataCount>0 then begin
    s.Write(lvl.DemoData[0], lvl.DemoDataCount*sizeof(uint8));
  end;

  if length(lvl.SndMap)>0 then
    s.Write(lvl.SndMap[0], length(lvl.SndMap)*sizeof(uint16));

  s.WriteDWord(lvl.SndDetailCount);
  if lvl.SndDetailCount>0 then
    s.Write(lvl.SndDetail[0], sizeof(tr1_sound_details)*lvl.SndDetailCount);

  s.WriteDWord(lvl.SamplesCount);
  if lvl.SamplesCount>0 then
    s.Write(lvl.SamplesData[0], lvl.SamplesCount);

  s.WriteDword(lvl.SampleIndexCount);
  if lvl.SampleIndexCount> 0 then
    s.Write(lvl.SampleIndex[0], lvl.SampleIndexCount*sizeof(UInt32));

  Result:=true;
end;

function WriteLevel(s: TStream; const lvl: TTR1level): Boolean;
begin
  try
    Result:=DoWriteLevel(s, lvl);
  except
    Result:=false;
  end;
end;

function WriteLevel(const fn: string; const lvl: TTR1level): Boolean;
var
  fs: TFileStream;
begin
  try
    fs:=TFileStream.Create(fn, fmCreate);
    try
      Result:=WriteLevel(fs, lvl);
    finally
      fs.Free;
    end;
  except
    Result:=false;
  end;

end;

procedure SaveDataToFile(const data; size: integer; const dstFileName: string);
var
  fs: TFileStream;
begin
  try
    fs:=TFileStream.Create(dstFileName, fmCreate);
    try
      fs.Write(data, size);
    finally
      fs.Free;
    end;
  except
  end;
end;

procedure DumpModels(const models: array of tr1_model; modelsCount: Integer);
var
  i : integer;
begin
  for i:=0 to modelsCount-1 do begin
    writelN('#',i,' id = ', models[i].object_id);
    writeln('  num meshes: ', models[i].num_meshes);
    writeln('  start mesh: ', models[i].starting_mesh);
    writeln('  tree idx:   ', models[i].mesh_tree_index);
    writeln('  frame ofs:  ', models[i].frame_offset);
    writeln('  anim idx:   ', models[i].animation_index);
  end;
end;

procedure DumpLevel(const lvl: TTR1Level);
begin
  //lvl.ro
end;

procedure DebugData(const prefix: string; num, bytesize, ofs: Integer);
begin
  writeln(prefix,' ',num:8,' bytes=', bytesize:9,' at ', IntToHex(ofs, 8));
end;

procedure AnimWadToRelease(const src: tr1_animation_wad; var dst: tr1_animation);
begin
  FillChar(dst, sizeof(dst), 0);
  dst.frame_offset        := src.frame_offset;
  dst.frame_rate          := src.frame_rate;
  dst.frame_size          := src.frame_size;
  dst.state_id            := src.state_id;
  dst.frame_start         := src.frame_start;
  dst.frame_end           := src.frame_end;
  dst.next_animation      := src.next_animation;
  dst.next_frame          := src.next_frame;
  dst.num_state_changes   := src.num_state_changes;
  dst.state_change_offset := src.state_change_offset;
  dst.num_anim_commands   := src.num_anim_commands;
  dst.anim_command        := src.anim_command;
end;

function ReadDemoWAD1(s: TStream; var lvl: TTR1level; aforced: Boolean): Boolean;
var
  v : LongWord;
  c : LongWord;

  tt  : array of tr1_object_texture_wad;
  aa  : array of tr1_animation_wad;
  i   : integer;
begin
  FillChar(lvl, sizeof(lvl), 0);

  lvl.version:=s.ReadDWord;

  writeln('version: ', lvl.version, ' ', IntTOHex(lvl.version,8),' wad: ', sizeof(tr1_object_texture_wad));
  Result:=true;
  c:=s.ReadDWord;

  //DebugData('num1: ', c, c*8, s.Position);

  SetLength(tt, c);
  s.Read(tt[0], length(tt)*sizeof(tr1_object_texture_wad));

  lvl.ObjTexCount:=c;
  SetLength(lvl.ObjTex, lvl.ObjTexCount);
  for i:=0 to lvl.ObjTexCount-1 do begin
    lvl.ObjTex[i].Attribute:=0;
    lvl.ObjTex[i].Tile:=tt[i].Tile;
    lvl.ObjTex[i].Vertices[0].Xcoordinate := 1;
    lvl.ObjTex[i].Vertices[0].Xpixel      := tt[i].x;
    lvl.ObjTex[i].Vertices[0].Ycoordinate := 1;
    lvl.ObjTex[i].Vertices[0].YPixel      := tt[i].y;

    lvl.ObjTex[i].Vertices[1].Xcoordinate := 255;
    lvl.ObjTex[i].Vertices[1].Xpixel      := tt[i].x+tt[i].w;
    lvl.ObjTex[i].Vertices[1].Ycoordinate := 1;
    lvl.ObjTex[i].Vertices[1].YPixel      := tt[i].y;

    lvl.ObjTex[i].Vertices[2].Xcoordinate := 255;
    lvl.ObjTex[i].Vertices[2].Xpixel      := tt[i].x+tt[i].w;
    lvl.ObjTex[i].Vertices[2].Ycoordinate := 255;
    lvl.ObjTex[i].Vertices[2].YPixel      := tt[i].y+tt[i].h;

    lvl.ObjTex[i].Vertices[3].Xcoordinate := 1;
    lvl.ObjTex[i].Vertices[3].Xpixel      := tt[i].x;
    lvl.ObjTex[i].Vertices[3].Ycoordinate := 255;
    lvl.ObjTex[i].Vertices[3].YPixel      := tt[i].y+tt[i].h;
  end;

  c:=s.ReadDWord; // mesh data? textures?
  if c mod sizeof(tr1_textile)=0 then begin
    lvl.TextTileCount:=c div SizeOf(tr1_textile);
    SetLength(lvl.TextTile, lvl.TextTileCount);
    s.Read(lvl.TextTile[0], lvl.TextTileCount * SizeOf(tr1_textile));
  end else
    s.Position:=s.Position+c;

  lvl.MeshPtrCount:=s.ReadDword; // Mesh Pointers
  SetLength(lvl.MeshPtr, lvl.MeshPtrCount);
  if lvl.MeshPtrCount>0 then
    s.Read(lvl.MeshPtr[0], lvl.MeshPtrCount*4);


  lvl.MeshDataCount:=s.ReadDword; // mesh
  //DebugData('num4: ', c, c*2, s.Position);
  SetLength(lvl.MeshData, lvl.MeshDataCount*2);
  if lvl.MeshDataCount>0 then
    s.Read(lvl.MeshData[0], lvl.MeshDataCount*2);

  c:=s.ReadDword; // AnimChanges?
  SetLength(aa, c);
  if c>0 then begin
    s.Read(aa[0], c*sizeof(tr1_animation_wad) );
    lvl.AnimationCount:=c;
    SetLength(lvl.Animation, c);
    for i:=0 to c - 1 do begin
      AnimWadToRelease( aa[i], lvl.Animation[i]);
    end;
    //lvl.Animation:
  end;
  //DebugData('num5: ', c, c*26, s.Position);
  //s.Position:=s.Position+c*26;

  lvl.StateChCount:=s.ReadDword; // State Changes?
  //DebugData('num6: ', c, c*6, s.Position);
  SetLength(lvl.StateCh, lvl.StateChCount);
  //s.Position:=s.Position+c*6;
  if lvl.StateChCount>0 then
    s.Read(lvl.StateCh[0], lvl.StateChCount*SizeOf(tr_state_change));

  lvl.AnimDispCount:=s.ReadDword; // Anim Dispatches ?
  SetLength(lvl.AnimDisp, lvl.AnimDispCount);
  if lvl.AnimDispCount>0 then
    s.Read(lvl.AnimDisp[0], lvl.AnimDispCount*sizeof(tr_anim_dispatch));
  //DebugData('num7: ', c, c*8, s.Position);
  //s.Position:=s.Position+c*8;

  lvl.AnimCmdCount:=s.ReadDword;  //?
  //DebugData('num8: ', c, c*2, s.Position);
  SetLength(lvl.AnimCmd, lvl.AnimCmdCount);
  if lvl.AnimCmdCount>0 then
    s.Read(lvl.AnimCmd[0], lvl.AnimCmdCount*2);

  //s.Position:=s.Position+c*2;

  lvl.MeshTreeCount:=s.ReadDword;
  //DebugData('num9: ', c, c*4, s.Position);
  SetLength(lvl.MeshTree, lvl.MeshTreeCount);
  if lvl.MeshTreeCount>0 then
    s.Read(lvl.MeshTree[0], lvl.MeshTreeCount*4);
  //s.Position:=s.Position+c*4;

  // it also seems, that C-12 might be correct
  //s.Position:=s.Position+c*12;

  lvl.FrameCount:=s.ReadDword;
  SetLength(lvl.Frame, lvl.FrameCount);
  //DebugData('num10:', c, c*2, s.Position);
  //s.Position:=s.Position+c*2;
  if lvl.FrameCount>0 then
    s.Read(lvl.Frame[0], lvl.FrameCount*2);

  lvl.ModelCount:=s.ReadDword;
  SetLength(lvl.Model, lvl.ModelCount);
  if lvl.ModelCount>0 then
    s.Read(lvl.Model[0], lvl.ModelCount*sizeof(tr1_model));

  //DebugData('num11:', c, c*8, s.Position);
  //writeln('s.size = ', s.size,' ',s.size-s.position,' ',(s.size-s.position)/c:0:2,' ',s.position+c*6);
  //s.Position:=s.Position+c*8;
  //writeln('data left: ', s.size - s.position);

(*
  v:=s.ReadDWord;
  writeln('version: ', v, ' ', IntTOHex(v,8));
  Result:=true;
  c:=s.ReadDWord;
  DebugData('num1: ', c, c*8, s.Position);
  s.Position:=s.Position+c*8;

  c:=s.ReadDWord; // mesh data? textures?
  DebugData('num2: ', c, c, s.Position);
  s.Position:=s.Position+c;

  c:=s.ReadDword; // Mesh pointers?
  DebugData('num3: ', c, c*4, s.Position);
  s.Position:=s.Position+c*4;

  c:=s.ReadDword; // mesh pointers?
  DebugData('num4: ', c, c*2, s.Position);
  s.Position:=s.Position+c*2;

  c:=s.ReadDword; // AnimChanges?
  DebugData('num5: ', c, c*26, s.Position);
  s.Position:=s.Position+c*26;

  c:=s.ReadDword; // State Changes?
  DebugData('num6: ', c, c*6, s.Position);
  s.Position:=s.Position+c*6;

  c:=s.ReadDword; // Anim Dispatches ?
  DebugData('num7: ', c, c*8, s.Position);
  s.Position:=s.Position+c*8;

  c:=s.ReadDword;  //?
  DebugData('num8: ', c, c*2, s.Position);
  s.Position:=s.Position+c*2;

  c:=s.ReadDword;
  DebugData('num9: ', c, c*4, s.Position);
  s.Position:=s.Position+c*4;

  // it also seems, that C-12 might be correct
  //s.Position:=s.Position+c*12;

  c:=s.ReadDword;
  DebugData('num10:', c, c*2, s.Position);
  s.Position:=s.Position+c*2;

  c:=s.ReadDword;
  DebugData('num11:', c, c*18, s.Position);
  writeln('s.size = ', s.size,' ',s.size-s.position,' ',(s.size-s.position)/c:0:2,' ',s.position+c*6);
  s.Position:=s.Position+c*18;

  writeln(s.size - s.position);

  c:=s.ReadDword;
  DebugData('num12: ', c, c*16, s.Position);
  s.Position:=s.Position+c*16;

  writeln(s.size - s.position);
*)
  //c:=s.ReadDword;
  //DebugData('num?: ', c, c*16, s.Position);

  //k:=2;
  //ofs:=s.Position;
  //cc:=c;
  {while k<200 do begin
    writeln('test size: ' , k);

    if s.Position+cc*k+4<s.Size then begin
      s.Position:=s.Position+cc*k;
      c:=s.ReadDword;
      DebugData('num?: ', c, c*4, s.Position);
    end;

  //  ...36  62..80

    k:=k+2;
    s.Position:=ofs;
  end;}
end;

function ReadDemoWAD1(const fn: string; var lvl: TTR1level; aforced: Boolean): Boolean;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    //writeln('reading wad: ', fn);
    Result:=ReadDemoWad1(fs, lvl);
  finally
    fs.Free;
  end;
end;

function ReadDemoTOM(s: TStream; var lvl: TTR1level; aforced: Boolean = false): Boolean; overload;
begin
  Result:=false;
end;

function ReadDemoTOM(const fn: string; var lvl: TTR1level; aforced: Boolean = false): Boolean; overload;
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
    writeln('reading wad: ', fn);
    Result:=ReadDemoWad1(fs, lvl);
  finally
    fs.Free;
  end;
end;

type
  TBasis = record
    x,y,z: single;
  end;

  TBasisStack = record
    count : integer;
    items : array of TBasis;
  end;

procedure BasisInit(out st: TBasisStack);
begin
  st.Count:=0;
  SetLength(st.items,16);
end;

procedure BasisPush(var b: TBasisStack; x,y,z: single);
begin
  if length(b.items)=b.count then begin
    if b.count=0 then SetLength(b.items, 16)
    else Setlength(b.items, b.count*2);
  end;
  b.items[b.count].x:=x;
  b.items[b.count].y:=y;
  b.items[b.count].z:=z;
  inc(b.count);
end;

function BasisPeek(const b: TBasisStack; out x,y,z: single): Boolean; overload;
var
 i : integer;
begin
  Result:=b.count>0;
  if Result then begin
    i:=b.count-1;
    x:=b.items[i].x;
    y:=b.items[i].y;
    z:=b.items[i].z;
  end else begin
    x:=0;
    y:=0;
    z:=0;
  end;
end;

function BasisPop(var b: TBasisStack; out x,y,z: single): Boolean;   overload;
begin
  Result:=b.count>0;
  if Result then begin
    BasisPeek(b, x,y,z);
    dec(b.count);
  end else begin
    x:=0;
    y:=0;
    z:=0;
  end;

{  if length(b.items)=b.count then begin
    if b.count=0 then SetLength(b.items, 16)
    else Setlength(b.items, b.count*2);
  end;
  b.items[b.count].x:=x;
  b.items[b.count].y:=y;
  b.items[b.count].z:=z;}

end;

function BasisPop(var b: TBasisStack): Boolean; overload;
var
  t1,t2,t3: single;
begin
  Result:=BasisPop(b,t1,t2,t3);
end;

procedure Normalize(var x,y,z: single);
var
  l : double;
begin
  l:=sqrt(sqr(x)+sqr(y)+sqr(z));
  if l>0 then begin
    x:=x/l;
    y:=y/l;
    z:=z/l;
  end;
end;

function ModelToWavefrontStr(const model: tr1_model;
  const ptrs: array of uint32;
  const meshdata: array of byte;
  const meshtree: array of tr1_meshtree_raw;
  isWad: Boolean;
  version: Integer;
  scale: double): string;
var
  i   : integer;
  j   : integer;
  ofs : Integer;
  m   : tr1_mesh;
  x,y,z : single;
  s    : string;

  sz   : integer;
  vidx : integer;
  px,py,pz: single;
  dx,dy,dz: single;
  ti : integer;
  tt : integer;
  st : TBasisStack;
  pm : ptr1_meshtree;

  fmt  : string;

const
  fmtRectNorm  = 'f %0:d//%0:d %1:d//%1:d %2:d//%2:d %3:d//%3:d';
  fmtTriagNorm = 'f %0:d//%0:d %1:d//%1:d %2:d//%2:d';
  fmtRect      = 'f %0:d %1:d %2:d %3:d';
  fmtTriag     = 'f %0:d %1:d %2:d';

begin
  s:='';
  vidx:=1;
  ofs:=model.starting_mesh;
  px:=0;
  py:=0;
  pz:=0;

  BasisInit(st);

  ti:=model.mesh_tree_index;
  for i:=0 to model.num_meshes-1 do begin
    if isWad then begin
      if version=$B then
        TR1MayWadSetMeshFromData(meshdata, Ptrs[ofs], m)
      else
        TR1JulWadSetMeshFromData(meshdata, Ptrs[ofs], m);
    end else
      TR1SetMeshFromData( meshdata, ptrs[ ofs ], m );

    writelN('i: ',i,' cnt: ',m.centre.x{/255:0:2},' ',m.centre.y{/255:0:2},' ',m.centre.z{/255:0:2});

    if i>0 then begin
      tt:=model.mesh_tree_index+(i-1)*4;
      pm:=ptr1_meshtree(@meshtree[tt]);

      if (pm.flags and 1>0) then begin
        BasisPop(st);
        BasisPeek(st, px,py,pz);
      end;

      if (pm.flags and 2>0) then BasisPush(st, px,py,pz);

      px:=px+pm^.x;
      py:=py+pm^.y;
      pz:=pz+pm^.z;
    end;

    dx:=px{+m.centre.x}{/255};
    dy:=py{+m.centre.y}{/255};
    dz:=pz{+m.centre.z}{/255};

   {+m.centre.x/255}
   {+m.centre.y/255}
   {+m.centre.z/255}
    s:=s+Format('o mesh_%d',[i])+LineEnding;
    for j:=0 to m.num_vertices-1 do begin
      x:=(m.vertices^[j].x+dx)*scale;
      y:=(m.vertices^[j].y+dy)*scale;
      z:=(m.vertices^[j].z+dz)*scale;
      s:=s+Format('v %.8f %.8f %.8f',[x,y,z])+LineEnding;
    end;

    for j:=0 to m.num_normals-1 do begin
      NormalToSingle(m.normals^[j],x,y,z);
      Normalize(x,y,z);
      //x:=m.normals^[j].x;
      //y:=m.normals^[j].y;
      //z:=m.normals^[j].z;
      s:=s+Format('vn %.8f %.8f %.8f',[x,y,z])+LineEnding;
    end;

    if m.num_textured_rectangles > 0 then begin

      if m.num_normals>0 then fmt:=fmtRectNorm else fmt:=fmtRect;

      s:=s+'# textured rectangles'+LineEnding;
      for j:=0 to m.num_textured_rectangles-1 do begin
        s:=s+Format(fmt
          ,[ m.textured_rectangles^[j].Verticies[0]+vidx
            ,m.textured_rectangles^[j].Verticies[1]+vidx
            ,m.textured_rectangles^[j].Verticies[2]+vidx
            ,m.textured_rectangles^[j].Verticies[3]+vidx
           ])
          +LineEnding;
          {s:=s+Format('f %0:d %1:d %2:d %3:d'
          ,[ m.textured_rectangles^[j].Verticies[0]+vidx
            ,m.textured_rectangles^[j].Verticies[1]+vidx
            ,m.textured_rectangles^[j].Verticies[2]+vidx
            ,m.textured_rectangles^[j].Verticies[3]+vidx
           ])
          +LineEnding;}
          {s:=s+Format('f %3:d %2:d %1:d %0:d'
          ,[ m.textured_rectangles^[j].Verticies[0]+vidx
            ,m.textured_rectangles^[j].Verticies[1]+vidx
            ,m.textured_rectangles^[j].Verticies[2]+vidx
            ,m.textured_rectangles^[j].Verticies[3]+vidx
           ])
          +LineEnding;}
      end;
    end;

    if m.num_textured_triangles>0 then begin


      s:=s+'# textured triangles'+LineEnding;
      if m.num_normals>0 then fmt:=fmtTriagNorm else fmt:=fmtTriag;

      for j:=0 to m.num_textured_triangles-1 do begin
        s:=s+Format( fmt
          ,[ m.textured_triangles^[j].Verticies[0]+vidx
            ,m.textured_triangles^[j].Verticies[1]+vidx
            ,m.textured_triangles^[j].Verticies[2]+vidx
           ])
          +LineEnding;
      end;
    end;

    if m.num_coloured_rectangles>0 then begin
      s:=s+'# colored rectangles'+LineEnding;
      if m.num_normals>0 then fmt:=fmtRectNorm else fmt:=fmtRect;

      for j:=0 to m.num_coloured_rectangles-1 do begin
        s:=s+Format( fmt
          ,[ m.coloured_rectangles^[j].Verticies[0]+vidx
            ,m.coloured_rectangles^[j].Verticies[1]+vidx
            ,m.coloured_rectangles^[j].Verticies[2]+vidx
            ,m.coloured_rectangles^[j].Verticies[3]+vidx
           ])
          +LineEnding;
      end;
    end;

    if m.num_coloured_triangles>0 then begin
      s:=s+'# colored triangles'+LineEnding;
      if m.num_normals>0 then fmt:=fmtTriagNorm else fmt:=fmtTriag;

      for j:=0 to m.num_coloured_triangles-1 do begin
        s:=s+Format( fmt
          ,[ m.coloured_triangles^[j].Verticies[0]+vidx
            ,m.coloured_triangles^[j].Verticies[1]+vidx
            ,m.coloured_triangles^[j].Verticies[2]+vidx
           ])
          +LineEnding;
      end;
    end;

    s:=s+LineEnding+LineEnding;
    inc(vidx, m.num_vertices);
    inc(ofs);
  end;
  Result:=s;
end;

function ColorAdjust24(const cl: tr_colour3): tr_colour3; inline;
begin
  Result.r:=cl.r*4;
  Result.g:=cl.g*4;
  Result.b:=cl.b*4;
end;

function ColorToWin24(const cl: tr_colour3): tr_colour3; inline;
begin
  Result.r:=cl.b;
  Result.g:=cl.g;
  Result.b:=cl.r;
end;

const
  transpcolor : tr_colour4 = (r:0;g:0;b:0;a:0);

function PalIndexToWin32(const pal: ptr1_palette; idx: byte): tr_colour4; inline;
begin
  if idx=0 then
    Result:=transpcolor
  else begin
    Result.alpha:=255;
    Result.clr:=ColorToWin24( ColorAdjust24( pal^[idx] ) );
  end
end;

procedure SaveTileToBmp32(const dst: string; const tl: tr_textile8; pal: ptr1_palette);
var
  fs : TFileStream;
  w,h: integer;
  sz   : integer;
  dofs : integer;

  buf  : array of tr_colour4;
  i,j  : integer;
  k    : integer;
const
  bf : shortstring = 'BM';
begin
  if not Assigned(pal) then pal:=TR1DefPallette;
  fs := TFileStream.Create(dst, fmCreate);
  w:=256;
  h:=256;
  sz:=w * h * sizeof(buf[0]);
  dofs:=14 + 40; // sizeof bmpheader + sizeof (dibheader)
  try
    // bmp header
    fs.Write(bf[1], 2);
    fs.WriteDWord(LongWord(sz+dofs));
    fs.WriteWord(0);
    fs.WriteWord(0);
    fs.WriteDWord(LongWord(dOfs));

    // dib header
    fs.WriteDWord(40);
    fs.WriteDWord(LongWord(w));
    fs.WriteDWord(LongWord(h));
    fs.Writeword(1);

    fs.Writeword(32);
 {  if sizeof(buf[0]) = 4
      then fs.Writeword(32)
      else fs.Writeword(24);}

    fs.WriteDword(0);
    fs.WriteDword(sz); // image size
    fs.WriteDword(0);
    fs.WriteDword(0);
    fs.WriteDword(0);
    fs.WriteDword(0);

    SetLength(buf, w*h);
    {for i:=0 to length(tl.raw)-1 do begin
      buf[i]:=PalIndexToWin32(pal, tl.raw[i]);
    end;}

    k:=0;
    for i:=0 to 255 do begin
      for j:=0 to 255 do begin
        buf[ 256*(255-i) +j]:=PalIndexToWin32(pal, tl.raw[k]);
        inc(k);
      end;
    end;

    fs.Write(buf[0], length(buf)* sizeof(buf[0]));

  finally
    fs.Free;
  end;
end;

end.


