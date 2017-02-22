unit tr1_utils;

{$mode delphi}{$H+}

interface

uses Classes, SysUtils, tr_types;

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

    PaletteCount  : LongWord;
    //PaletteOffset : QWord; // offset in the file
    Palette       : array of tr_textile8_t;
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

function ReadLevel(s: TStream; var lvl: TTR1level): Boolean; overload;
function ReadLevel(const fn: string; var lvl: TTR1level): Boolean; overload;

function WriteLevel(s: TStream; const lvl: TTR1level): Boolean; overload;
function WriteLevel(const fn: string; const lvl: TTR1level): Boolean; overload;

procedure SaveDataToFile(const data; size: integer; const dst: string);

const
  BraidMesh : array [0..5] of array [0..139] of byte = (
    ($08,$00,$07,$00,$13,$00,$80,$01,$00,$00,$06,$00,$0D,$00,$0C,$00,$22,$00,$00,$00,$F6,$FF,$22,$00,$F4,$FF,$0D,$00,$22,$00,$0F,$00,$0D,$00,$00,$00,$00,$00,$F5,$FF,$00,$00,$F2,$FF,$0E,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$54,$2E,$CE,$FE,$ED,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00),
    ($07,$00,$05,$00,$13,$00,$5B,$01,$00,$00,$06,$00,$0B,$00,$09,$00,$23,$00,$FF,$FF,$F7,$FF,$23,$00,$F5,$FF,$0A,$00,$23,$00,$0D,$00,$0B,$00,$00,$00,$FF,$FF,$F5,$FF,$00,$00,$F3,$FF,$0B,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$53,$2E,$CE,$FE,$ED,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00),
    ($06,$00,$05,$00,$12,$00,$39,$01,$00,$00,$06,$00,$09,$00,$08,$00,$22,$00,$00,$00,$F8,$FF,$22,$00,$F7,$FF,$09,$00,$22,$00,$0B,$00,$09,$00,$00,$00,$FF,$FF,$F7,$FF,$00,$00,$F5,$FF,$0A,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$54,$2E,$CE,$FE,$ED,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00),
    ($06,$00,$04,$00,$12,$00,$1E,$01,$00,$00,$06,$00,$08,$00,$06,$00,$23,$00,$00,$00,$F9,$FF,$23,$00,$F8,$FF,$07,$00,$23,$00,$09,$00,$08,$00,$00,$00,$00,$00,$F8,$FF,$00,$00,$F7,$FF,$08,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$53,$2E,$CE,$FE,$ED,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00),
    ($05,$00,$04,$00,$11,$00,$02,$01,$00,$00,$06,$00,$06,$00,$05,$00,$22,$00,$00,$00,$FB,$FF,$22,$00,$FA,$FF,$06,$00,$22,$00,$08,$00,$07,$00,$00,$00,$00,$00,$F9,$FF,$00,$00,$F8,$FF,$07,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$53,$2E,$CE,$FE,$ED,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00),
    ($05,$00,$04,$00,$11,$00,$EE,$00,$00,$00,$06,$00,$05,$00,$04,$00,$23,$00,$00,$00,$FC,$FF,$23,$00,$FB,$FF,$04,$00,$23,$00,$06,$00,$05,$00,$00,$00,$00,$00,$FB,$FF,$00,$00,$FA,$FF,$06,$00,$00,$00,$06,$00,$6B,$26,$C9,$14,$54,$2E,$CE,$FE,$EC,$D3,$EF,$2D,$CE,$DA,$D2,$16,$5E,$2E,$CC,$28,$12,$16,$61,$D4,$C1,$FE,$0E,$D2,$F0,$D3,$6D,$D8,$47,$18,$6D,$D4,$00,$00,$00,$00,$03,$00,$00,$00,$03,$00,$04,$00,$01,$00,$05,$00,$01,$00,$04,$00,$05,$00,$02,$00,$05,$00,$02,$00,$05,$00,$03,$00,$00,$00,$05,$00,$02,$00,$01,$00,$02,$00,$00,$00,$05,$00,$04,$00,$03,$00,$05,$00,$05,$00)
  );

procedure DumpLevel(const lvl:TTR1Level);

implementation

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

  //writeln('  sectors: ', room.zSector,'x',room.xSector,' sizeof ',sizeof(tr1_room_sector));
  room.intensity:=s.ReadWord;
  room.lightsCount:=s.ReadWord;
  SetLength(room.light, room.lightsCount);

  //writeln('  lights:    ', room.lightsCount,' sizeof ',sizeof(tr1_room_light));

  if room.lightsCount>0 then
    s.Read(room.light[0], room.lightsCount*sizeof(tr1_room_light));
    //s.Position:=s.Position+(room.lightsCount*sizeof(tr1_room_light));

  room.meshesCount:=s.ReadWord;
  //writeln('  meshes:    ', room.meshesCount,' sizeof ',sizeof(tr1_room_staticmesh));
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
  lvl.PaletteCount:=s.ReadDword;
  //lvl.PaletteOffset:=s.Position;
  SetLength(lvl.Palette, lvl.PaletteCount);
  if lvl.PaletteCount>0 then
    s.Read(lvl.Palette[0], lvl.PaletteCount*sizeof(tr_textile8_t));

  lvl.unused:=s.ReadDWord;
  //writeln('unused value = ', lvl.unused);

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
  //writeln('mesh data: ', lvl.MeshDataCount);
  SetLength(lvl.MeshData, lvl.MeshDataCount*sizeof(uint16));
  if lvl.MeshDataCount>0 then
    s.Read(lvl.MeshData[0], lvl.MeshDataCount*sizeof(uint16));

  lvl.MeshPtrCount:=s.ReadDWord;
  //writeln('num mesh pointers: ', lvl.MeshPtrCount);
  SetLength(lvl.MeshPtr, lvl.MeshPtrCount);
  if lvl.MeshPtrCount>0 then
    s.Read(lvl.MeshPtr[0], lvl.MeshPtrCount*sizeof(uint32));
  //s.Position:=s.Position+cnt*4; // skipping over pointers

  lvl.AnimationCount:=s.ReadDWord;
  //writeln('num of animations: ', lvl.AnimationCount,' ',sizeof(tr1_animation));
  SetLength(lvl.Animation, lvl.AnimationCount);
  if lvl.AnimationCount>0 then begin
    s.Read(lvl.Animation[0], length(lvl.Animation)*sizeof(tr1_animation));
  end;

  lvl.StateChCount:=s.ReadDWord;
  //writeln('state changes: ', lvl.StateChCount,' ',sizeof(tr_state_change));
  SetLength(lvl.StateCh, lvl.StateChCount);
  if lvl.StateChCount>0 then begin
    s.Read(lvl.StateCh[0], length(lvl.StateCh)*sizeof(tr_state_change));
  end;

  lvl.AnimDispCount:=s.ReadDWord;
  //writeln('anim disp: ', lvl.AnimationCount,' ',sizeof(tr_anim_dispatch));
  SetLength(lvl.AnimDisp, lvl.AnimDispCount);
  if lvl.AnimDispCount>0 then begin
    s.Read(lvl.AnimDisp[0], length(lvl.AnimDisp)*sizeof(tr_anim_dispatch));
  end;

  lvl.AnimCmdCount:=s.ReadDword;
  //writeln('anim cmd: ', lvl.AnimCmdCount,' ',sizeof(tr_anim_command));
  SetLength(lvl.AnimCmd, lvl.AnimCmdCount);
  if lvl.AnimCmdCount>0 then
    s.Read(lvl.AnimCmd[0], lvl.AnimCmdCount*sizeof(tr_anim_command));

  lvl.MeshTreeCount:=s.ReadDWord;
  //writeln('mesh tree: ', lvl.MeshTreeCount,' ',sizeof(tr1_meshtree_raw));
  SetLength(lvl.MeshTree, lvl.MeshTreeCount);
  if lvl.MeshTreeCount>0 then
    s.Read(lvl.MeshTree[0], lvl.MeshTreeCount*sizeof(tr1_meshtree_raw));

  lvl.FrameCount:=s.ReadDWord;
  //writeln('Frame Count: ', lvl.FrameCount);
  SetLength(lvl.Frame, lvl.FrameCount);
  if lvl.FrameCount>0 then
    s.Read(lvl.Frame[0], lvl.FrameCount*sizeof(uint16));

  lvl.ModelCount:=s.ReadDWord;
  //writeln('Movable Count: ', lvl.ModelCount,' ',sizeof(tr1_model));
  SetLength(lvl.Model, lvl.ModelCount);
  if lvl.ModelCount>0 then
    s.Read(lvl.Model[0], lvl.ModelCount*sizeof(tr1_model));

  lvl.StaticMeshCount:=s.ReadDWord;
  //writeln('static meshes: ', lvl.StaticMeshCount,' ',sizeof(tr1_staticmesh));
  SetLength(lvl.StaticMesh, lvl.StaticMeshCount);
  if lvl.StaticMeshCount>0 then
    s.Read(lvl.StaticMesh[0], lvl.StaticMeshCount * sizeof(tr1_staticmesh));

  lvl.ObjTexCount:=s.ReadDword;
  //writeln('object textures: ', lvl.ObjTexCount,' ',sizeof(tr1_object_texture));
  SetLength(lvl.ObjTex, lvl.ObjTexCount);
  if lvl.ObjTexCount>0 then
    s.Read(lvl.ObjTex[0], lvl.ObjTexCount * sizeof(tr1_object_texture));

  lvl.SprTexCount:=s.ReadDword;
  SetLength(lvl.SprTex, lvl.SprTexCount);
  //writeln('sprite textures: ', lvl.SprTexCount,' ',sizeof(tr1_sprite_texture));
  if lvl.ObjTexCount>0 then
    s.Read(lvl.SprTex[0], lvl.SprTexCount * sizeof(tr1_sprite_texture));

  lvl.SprSeqCount:=s.ReadDWord;
  SetLength(lvl.SprSeq, lvl.SprSeqCount);
  //writeln('sprite sequence: ', lvl.SprSeqCount,' ',sizeof(tr1_sprite_sequence));
  if lvl.SprSeqCount>0 then
    s.Read(lvl.SprSeq[0], lvl.SprSeqCount * sizeof(tr1_sprite_sequence));

  lvl.CameraCount:=s.ReadDWord;
  SetLength(lvl.Camera, lvl.CameraCount);
  //writeln('camera: ', lvl.CameraCount,' ',sizeof(tr1_camera));
  if lvl.CameraCount>0 then
    s.Read(lvl.Camera[0], lvl.CameraCount * sizeof(tr1_camera));

  lvl.SndSrcCount:=s.ReadDWord;
  SetLength(lvl.SndSrc, lvl.SndSrcCount);
  //writeln('sound sources: ', lvl.SndSrcCount,' ',sizeof(tr1_sound_source));
  if lvl.SndSrcCount>0 then
    s.Read(lvl.SndSrc[0], lvl.SndSrcCount * sizeof(tr1_sound_source));

  lvl.BoxCount:=s.ReadDWord;
  SetLength(lvl.Box, lvl.BoxCount);
  //writeln('box: ', lvl.BoxCount,' ',sizeof(tr1_box));
  if lvl.BoxCount>0 then
    s.Read(lvl.Box[0], lvl.BoxCount * sizeof(tr1_box));

  lvl.OverlapCount:=s.ReadDWord;
  //writeln('overlap:  ',lvl.OverlapCount);
  SetLength(lvl.Overlap, lvl.OverlapCount);
  if lvl.OverlapCount>0 then
    s.Read(lvl.Overlap[0], lvl.OverlapCount * sizeof(uint16));

  SetLength(lvl.Zone, lvl.BoxCount);
  if lvl.BoxCount>0 then
    s.Read(lvl.Zone[0], lvl.BoxCount * sizeof(tr1_zone));

  lvl.AnimTexCount:=s.ReadDWord;
  //writeln('anim textues: ', lvl.AnimTexCount);
  SetLength(lvl.AnimTex, lvl.AnimTexCount);
  if lvl.AnimTexCount>0 then
    s.Read(lvl.AnimTex[0], lvl.AnimTexCount * sizeof(uint16));

  lvl.ItemCount:=s.ReadDWord;
  //writeln('items: ',lvl.ItemCount,' ',sizeof(tr1_item));
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
  //writeln('cinematic: ',lvl.CinFrameCount,' ',sizeof(tr1_camera));
  if lvl.CinFrameCount>0 then
    s.Read(lvl.CinFrame[0], lvl.CinFrameCount*sizeof(tr1_camera));

  // 142ECA
  lvl.DemoDataCount:=s.ReadWord;
  SetLength(lvl.DemoData, lvl.DemoDataCount);
  if lvl.DemoDataCount>0 then begin
    s.Read(lvl.DemoData[0], lvl.DemoDataCount*sizeof(uint8));
  end;
  //writeln('after demo: ', s.Position,' ', IntToHex(s.Position,8));

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

  //writeln('before samples: ', s.Position,' ', IntToHex(s.Position,8),' wanted = $1430CA');
  lvl.SamplesCount:=s.ReadDWord;
  //writeln('samples size: ', lvl.SamplesCount);
  SetLength(lvl.SamplesData, lvl.SamplesCount);
  if lvl.SamplesCount>0 then
    s.Read(lvl.SamplesData[0], lvl.SamplesCount);

  lvl.SampleIndexCount := s.ReadDWord;
  SetLength(lvl.SampleIndex, lvl.SampleIndexCount);
  //writeln('sample index: ', lvl.SampleIndexCount);
  if lvl.SampleIndexCount> 0 then
    s.Read(lvl.SampleIndex[0], lvl.SampleIndexCount*sizeof(UInt32));

  Result:=true;
end;

function DoReadLevel(s: TStream; var lvl: TTR1level) : Boolean;
begin
  //writeln('reading');
  lvl.version:=s.ReadDWord;
  if lvl.version=FILEVERSION_TR1 then
    Result:=DoReadLevel1(s, lvl)
  else
    Result:=false;
end;

function ReadLevel(s: TStream; var lvl: TTR1level): Boolean;
begin
  try
    Result:=DoReadLevel(s, lvl);
  except
    on e:exception do begin
      Result:=false;
    end;
  end;
end;

function ReadLevel(const fn: string; var lvl: TTR1level): Boolean;
var
  fs : TfileStream;
begin
  try
    fs := TfileStream.Create(fn, fmOpenRead or fmShareDenyNone);;
    try
      Result:=ReadLevel(fs, lvl);
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
  s.WriteDWord(lvl.PaletteCount);
  if lvl.PaletteCount>0 then
    s.Write(lvl.Palette[0], lvl.PaletteCount*sizeof(tr_textile8_t));
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

procedure SaveDataToFile(const data; size: integer; const dst: string);
var
  fs: TFileStream;
begin
  try
    fs:=TFileStream.Create(dst, fmCreate);
    try
      fs.Write(data, size);
    finally
      fs.Free;
    end;
  except
  end;
end;

procedure DumpLevel(const lvl: TTR1Level);
begin
  //lvl.ro
end;

end.

