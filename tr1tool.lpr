program tr1tool;

{$mode delphi}{$H+}

uses
  SysUtils, Classes, tr_types, tr1_utils;

var
  gCommand   : string = '';
  gFileName  : string = '';
  gPaletteFN : string = '';

const
  CMD_VIEW = 'view';

procedure SeparateLvl(const lvl: TTR1Level);
var
  fs : TFileStream;
  i  : integer;
  p  : ptr1_palette;
begin
  for i:=0 to lvl.RoomCount-1 do begin
    fs:=TFilesTream.Create('room'+IntToStr(i)+'.out', fmCreate);
    try
      DoWriteTr1Room1Data(fs, lvl.Rooms[i]);
    finally
      fs.Free;
    end;
  end;

  writeln('floor = ',lvl.FloorCount);
  if lvl.FloorCount>0 then begin
    //writeln('writing floor');
    fs:=TFileStream.create('floordata.out', fmCreate);
    try
      fs.Write(lvl.Floor[0], lvl.FloorCount*2);
    finally
      fs.Free;
    end;
  end;

  writeln('tex = ',lvl.TextTileCount);
  for i:=0 to lvl.TextTileCount-1 do begin
    fs:=TFileStream.Create('tile'+IntToStr(i)+'.out', fmCreate);
    try
      fs.Write(lvl.TextTile[i].pixels[0][0], sizeof(tr1_textile));
      p:=nil;
      if length(lvl.pallette)>0 then p:=@lvl.pallette[0];
      SaveTileToBmp32( 'tile'+IntToStr(i)+'.bmp', lvl.TextTile[i], p);
    finally
      fs.Free;
    end;
  end;

  writeln('objtex = ',lvl.ObjTexCount);
  if lvl.ObjTexCount>0 then begin
    fs:=TFileStream.Create('texobjects.out', fmCreate);
    try
      fs.Write(lvl.ObjTex[0], lvl.ObjTexCount*sizeof(tr1_object_texture));
    finally
      fs.Free;
    end;

  end;

  writeln('objtex = ',lvl.MeshDataCount);
  if lvl.MeshDataCount>0 then
    fs:=TFileStream.Create('meshes.out', fmCreate);
    try
      fs.Write(lvl.MeshData[0], lvl.MeshDataCount*2);
    finally
      fs.Free;
    end;

  fs:=TFileStream.Create('meshesptr.out', fmCreate);
  try
    fs.Write(lvl.MeshPtr[0], lvl.MeshPtrCount*sizeof(lvl.MeshPtr[0]));
  finally
    fs.Free;
  end;

  fs:=TFileStream.Create('meshestree.out', fmCreate);
  try
    fs.Write(lvl.MeshTree[0], lvl.MeshTreeCount*sizeof(lvl.MeshTree[0]));
  finally
    fs.Free;
  end;

  fs:=TFileStream.Create('palette.out', fmCreate);
  try
    if length(lvl.pallette)>0 then
      fs.Write(lvl.pallette[0], length(lvl.pallette)*sizeof(tr_colour3));
  finally
    fs.Free;
  end;

  fs:=TFileStream.Create('frame.out', fmCreate);
  try
    if lvl.FrameCount>0 then
      fs.Write(lvl.Frame[0], lvl.FrameCount*sizeof(uint16));
  finally
    fs.Free;
  end;
end;

procedure DumpModelsToObj(const lvl: TTR1Level; aisWad: Boolean; aspecificModel: integer = -1);
var
  s  : string;
  fs : TfileStream;
  i  : Integer;
begin
  for i:=0 to lvl.ModelCount-1 do begin
    if (aspecificModel>=0) and (i<>aspecificModel) then continue;
    s:=ModelToWavefrontStr( lvl.Model[i], lvl.MeshPtr, lvl.MeshData, lvl.MeshTree, aisWad, lvl.version, i=0);
    s:='# object id '+IntToStr(lvl.Model[i].object_id)+LineEnding
       +LineEnding
       +s;

    fs:=TFileStream.Create('model_'+IntToStr(i)+'.obj', fmCreate);
    try
      fs.Write(s[1], length(s));
    finally
      fs.Free;
    end;
  end;
end;

procedure DumpTexObjects(const lvl: TTR1Level);
var
  i : integer;
  j : integer;
begin
  for i:=0 to lvl.ObjTexCount-1 do begin
    writeln('#',i,' ',IntTOHex(lvl.ObjTex[i].Attribute,4),' ',IntTOHex(lvl.ObjTex[i].Tile,4));
    for j:=0 to length(lvl.ObjTex[i].Vertices)-1 do begin
      writeln('   ',j
        ,' ',lvl.ObjTex[i].Vertices[j].Xcoordinate,' ',lvl.ObjTex[i].Vertices[j].Xpixel
        ,' ',lvl.ObjTex[i].Vertices[j].Ycoordinate,' ',lvl.ObjTex[i].Vertices[j].Ypixel);
    end;
  end;
end;

procedure DumpAnimModel(const lvl: TTR1Level);
var
  i : integer;
  j : integer;
begin
  for i:=0 to lvl.ModelCount-1 do begin
    writeln('model #',i);
    j:=lvl.Model[i].animation_index;
    writeln(' anim idx: ',j);
    writeln(' models frame ofs: ', lvl.Model[i].frame_offset);
    writeln(' anim frame ofs:   ', lvl.Animation[j].frame_offset);
    writeln(' MATCH: ', lvl.Model[i].frame_offset = lvl.Animation[j].frame_offset);
    //if (j=$FFFF) or (j>=lvl.AnimationCount) then continue;
//    lvl.Model[i].frame_offset:=lvl.Animation[j].frame_offset;
  end;
end;

procedure DumpStateChanges(const lvl: TTR1Level);
var
  i : integer;
begin
  for i:=0 to lvl.StateChCount-1 do begin
    writeln('st ch #',i);
    writeln('   stateid: ',lvl.StateCh[i].state_id,' anim disp: ',lvl.StateCh[i].anim_dispatch,'  number of disp: ',lvl.StateCh[i].num_anim_dispatches);
  end;
  for i:=0 to lvl.AnimDispCount-1 do begin
    writeln('disp #',i);
    writeln('   range: [',lvl.AnimDisp[i].low,'..',lvl.AnimDisp[i].high,']');
    writeln('   next anim: ',lvl.AnimDisp[i].next_animation,'/',lvl.AnimDisp[i].next_frame);
  end;
end;

procedure TestFrames(const lvl: TTR1Level);
var
  p : ptr_anim_frame;
  i : integer;
  sz: integer;
begin
  i:=0;
  while i<lvl.FrameCount do begin
    p:=ptr_anim_frame(@lvl.Frame[i]);
    sz:=GetFrameSizeInUint16(lvl.Frame, i);
    if sz+i>lvl.FrameCount then begin
      writeln('TEST FRAMES: FAILED');
      writeln('frame at: ',i, ' (',(i*2),') break through the buffer! wanted: ',sz,' has only: ', lvl.FrameCount-i);
      Exit;
    end;
    inc(i,sz);
  end;
  writeln('TEST FRAMES: SUCCESS!');
end;

procedure TestStates(const lvl: TTR1Level);
var
  i : integer;
  j : integer;
begin
  for i:=0 to lvl.StateChCount-1 do
    for j:=lvl.StateCh[i].anim_dispatch to lvl.StateCh[i].anim_dispatch
                                          +lvl.StateCh[i].num_anim_dispatches-1 do
    begin
      if (j<0) or (j>=lvl.AnimDispCount) then begin
        writeln('TEST STATES: FAILED!');
        writeln('a wrong animation dispatch given: ',lvl.StateCh[i].anim_dispatch,' at state changer ',i,' for disp: ', j);
        Exit;
      end;
    end;
  writeln('TEST STATES: SUCCESS!');
end;

procedure TestDisp(const lvl: TTR1Level);
var
  i : integer;
  j : integer;
  k : integer;
begin
  for i:=0 to lvl.AnimDispCount-1 do begin
    j:=lvl.AnimDisp[i].next_animation;
    if (j<0) or (j>=lvl.AnimationCount) then begin
      writeln('TEST DISP: FAILED!');
      writeln('a wrong animation is given: ',j,' at dispatch ',i);
      Exit;
    end;
    k:=lvl.AnimDisp[i].next_frame;
    if (k<lvl.Animation[j].frame_start) or (k>lvl.Animation[j].frame_end) then begin
      writeln('TEST DISP: FAILED!');
      writeln('a wrong frame is given: ',k,' at dispatch ',i,' for animation ',j);
      Exit;
    end;
  end;
  writeln('TEST DISP: SUCCESS!');
end;

procedure {%H-}TestMeshes(const lvl: TTR1Level);
var
  i : integer;
  j : integer;
  k : integer;
  m : tr1_mesh;
begin
  for i:=0 to lvl.MeshPtrCount-1 do begin
    if (lvl.MeshPtr[i]>=length(lvl.MeshData)) then begin
      writeln('TEST MESH: FAILED!');
      writeln('a wrong mesh pointer is given: ',lvl.MeshPtr[i],' at dispatch ',i);
    end;
    j:=lvl.MeshPtr[i];

    TR1SetMeshFromData( lvl.MeshData, j, m);
    for j:=0 to m.num_textured_rectangles-1 do begin
      if (m.textured_rectangles^[j].Texture>=lvl.ObjTexCount) then begin
        writeln(Format(
          'ERROR: invalid texture at TexRecangle #%0:d of mesh %1:d, value %2:d (%2:x) / %3:d '
            ,[j, i, m.textured_rectangles^[j].Texture, lvl.ObjTexCount]));
      end;
    end;

    for j:=0 to m.num_textured_triangles-1 do begin
      if (m.textured_triangles^[j].Texture>=lvl.ObjTexCount) then begin
        writeln(Format(
          'ERROR: invalid texture at TexTriangle #%0:d of mesh %1:d, value %2:d (%2:x) / %3:d '
            ,[j, i, m.textured_triangles^[j].Texture, lvl.ObjTexCount]));
      end;
    end;

    for j:=0 to m.num_coloured_rectangles-1 do begin
      if (m.coloured_rectangles^[j].Texture>=lvl.ObjTexCount) then begin
        writeln(Format(
          'ERROR: invalid color at ColRecangle #%0:d of mesh %1:d, value %2:d (%2:x) / %3:d '
            ,[j, i, m.coloured_rectangles^[j].Texture, length(lvl.pallette)]));
      end;
    end;

    for j:=0 to m.num_coloured_triangles-1 do begin
      if (m.coloured_triangles^[j].Texture>=lvl.ObjTexCount) then begin
        writeln(Format(
          'ERROR: invalid color at ColTriangle #%0:d of mesh %1:d, value %2:d (%2:x) / %3:d '
            ,[j, i, m.coloured_triangles^[j].Texture, length(lvl.pallette)]));
      end;
    end;

  end;

  writeln('TEST MESH: SUCCESS!');
end;

function isValid(val, max: Word): string;
begin
  if val>=max then Result:='BAD '
  else Result:='GOOD';
end;

procedure ReportMesheTextures(const lvl: TTR1Level);
var
  i : integer;
  j : integer;
  k : integer;
  m : tr1_mesh;
begin
  for i:=0 to lvl.MeshPtrCount-1 do begin
    j:=lvl.MeshPtr[i];

    TR1SetMeshFromData( lvl.MeshData, j, m);
    for j:=0 to m.num_textured_rectangles-1 do begin
        writeln(Format(
          '%4:s: invalid texture at TexRecangle #%0:d of mesh %1:d, value %2:d (%2:x,%5:d) / %3:d '
            ,[j, i, m.textured_rectangles^[j].Texture
              , lvl.ObjTexCount
              , isValid(m.textured_rectangles^[j].Texture,lvl.ObjTexCount)
              , int16(m.textured_rectangles^[j].Texture)
             ]));
    end;

    for j:=0 to m.num_textured_triangles-1 do begin
        writeln(Format(
          '%4:s: invalid texture at TexTriangle #%0:d of mesh %1:d, value %2:d (%2:x,%5:d) / %3:d '
            ,[j, i, m.textured_triangles^[j].Texture, lvl.ObjTexCount
              ,isValid(m.textured_triangles^[j].Texture,lvl.ObjTexCount)
              , int16(m.textured_rectangles^[j].Texture)
              ]));
    end;

    for j:=0 to m.num_coloured_rectangles-1 do begin
        writeln(Format(
          '%4:s: invalid color at ColRecangle #%0:d of mesh %1:d, value %2:d (%2:x) / %3:d '
            ,[j, i, m.coloured_rectangles^[j].Texture, length(lvl.pallette)
              ,isValid(m.coloured_rectangles^[j].Texture,length(lvl.pallette))
              ]));
    end;

    for j:=0 to m.num_coloured_triangles-1 do begin
        writeln(Format(
          '%4:s: invalid color at ColTriangle #%0:d of mesh %1:d, value %2:d (%2:x) / %3:d '
            ,[j, i, m.coloured_triangles^[j].Texture, length(lvl.pallette)
              ,isValid(m.coloured_triangles^[j].Texture,length(lvl.pallette))
              ]));
    end;
  end;

end;


procedure {%H-}ReadStats(const fn: string);
var
  lvl: TTR1Level;
begin
  if not ReadLevel(fn, lvl, true) then begin
    writeln(Format('failed to read the level %s', [fn]));
    Exit;
  end;

  SeparateLvl(lvl);
{  DumpRooms(lvl);
  DumpModels(lvl.Model, lvl.ModelCount);}
  //DumpModelsToObj(lvl, false);
  DumpTexObjects(lvl);
{  DumpAnimation(lvl);

  DumpAnimModel(lvl);
  DumpStateChanges(lvl);}

  TestFrames(lvl);
  TestStates(lvl);
  TestDisp(lvl);
  //TestMeshes(lvl);
  ReportMesheTextures(lvl);
//WriteLevel( ChangeFileExt(fn,'.resaved.phd'), lvl);
end;

procedure ReadWadStats(const fn, tompalfn: string);
var
  lvl : TTR1Level;
  fs  : TFileStream;
  i   : integer;
  pal : TTR1Level;
begin
  ReadDemoWAD1(fn, lvl);

  fs:=TFileStream.create('rawmodels.out', fmCreate);
  try
    fs.Write(lvl.MeshData[0], lvl.MeshDataCount*2);
  finally
    fs.Free;
  end;
  SeparateLvl(lvl);

  DumpModels(lvl.Model, lvl.ModelCount);
  DumpModelsToObj(lvl, true);

  RewriteMeshData(lvl, true);

  NormalizeTextures(lvl);

  DefaultLvl(lvl);
  if (tompalfn<>'') then begin
    ReadDemoTOM(tompalfn, pal);
    Move(pal.pallette[0], lvl.pallette[0], length(lvl.pallette)*sizeof(tr_colour3));
  end;

  DefaultRoom(lvl);
  DefaultItem(lvl);

  //lvl.ModelCount:=1;
  //lvl.Model[0]:=lvl.Model[6];
  //lvl.AnimationCount:=1;
  {for i:=0 to lvl.AnimationCount-1 do begin
    lvl.Animation[i].
  end;}

  WriteLevel( ChangeFileExt(fn,'.converted.phd'), lvl);
end;


procedure {%H-}HackStats(const fn: string);
var
  lvl: TTR1Level;
  i,j: integer;
begin
  if not ReadLevel(fn, lvl, true) then begin
    writeln(Format('failed to read the level %s', [fn]));
    Exit;
  end;

  lvl.FloorCount:=1;
  for i:=0 to lvl.RoomCount-1 do begin
    for j:=0 to length(lvl.Rooms[i].sectors)-1 do begin
      lvl.Rooms[i].sectors[j].box_index:=$FFFF;
      lvl.Rooms[i].sectors[j].fd_index:=0;
    end;
  end;

  DefaultRoom( lvl );
  //lvl.RoomCount:=1;
  //lvl.Rooms[0]:=lvl.Rooms[36];
  //lvl.ItemCount:=1;
  DefaultItem( lvl );
  //lvl.FloorCount:=1;

  WriteLevel( ChangeFileExt(fn,'.hacked.phd'), lvl);
end;

procedure ReadTom(const fn: string);
var
  lvl: TTR1Level;
begin
  ReadDemoTOM(fn, lvl);
end;

procedure PrintHelp;
begin
end;

function SafeParamStr(i: integer; const def: string = ''): string;
begin
  if (i<=0) and (i>ParamCount)
    then Result:=def
    else Result:=ParamStr(i);
end;

procedure ParseParams;
var
  i : integer;
  s : string;
  l : string;
begin
  i:=1;
  while i<=ParamCount do begin
    s:=ParamStr(i);
    l:=AnsiLowerCase(s);
    inc(i);
    if Pos('-',s)=1 then begin
    end else begin
      if gCommand='' then gCommand:=l
      else if gFileName='' then gFileName:=s;
    end;
  end;

  if (gCommand<>'') and (gFileName='') then begin
    gFileName:=gCommand;
    gCommand:='view';
  end;
end;

var
  ext   : string;
  palfn : string;

begin
  if ParamCount=0 then begin
    writeln('please sepcify .phd file exit');
    exit;
  end;
  ParseParams;
  ext:=AnsiLowerCase(ExtractFileExt(gFileName));

  if gCommand=CMD_VIEW then begin
    ReadStats(gFileName)
  end else begin
    TR1Debug:=true;
    if ext='.phd' then
      ReadStats(gFileName)
      //HackStats(fn)
    else if ext='.wad' then begin
      if (ParamCount>1) and (AnsiLowerCase(ExtractFileExt(ParamStr(2)))='.tom') then
        palfn:=ParamStr(2)
      else
        palfn:='';
      ReadWadStats(gFileName, palfn);
    end else if ext='.tom' then
      ReadTom(gFileName);
  end;
end.

