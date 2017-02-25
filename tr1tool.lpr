program tr1tool;

{$mode delphi}{$H+}

uses
  SysUtils, Classes, tr_types, tr1_utils;

procedure SeparateLvl(const lvl: TTR1Level);
var
  fs : TFileStream;
  i  : integer;
  p  : ptr1_palette;
begin
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

  if lvl.ObjTexCount>0 then begin
    fs:=TFileStream.Create('texobjects.out', fmCreate);
    try
      fs.Write(lvl.ObjTex[0], lvl.ObjTexCount*sizeof(tr1_object_texture));
    finally
      fs.Free;
    end;

  end;

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

end;

procedure DumpModelsToObj(const lvl: TTR1Level; aisWad: Boolean);
var
  s  : string;
  fs : TfileStream;
  i  : Integer;
begin
  for i:=0 to lvl.ModelCount-1 do begin
    s:=ModelToWavefrontStr( lvl.Model[i], lvl.MeshPtr, lvl.MeshData, lvl.MeshTree, aisWad, lvl.version);
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

procedure ReadStats(const fn: string);
var
  lvl: TTR1Level;
begin
  if not ReadLevel(fn, lvl, true) then begin
    writeln(Format('failed to read the level %s', [fn]));
    Exit;
  end;

  SeparateLvl(lvl);
  DumpModelsToObj(lvl, false);
  DumpTexObjects(lvl);
end;

procedure ReadWadStats(const fn: string);
var
  lvl: TTR1Level;
  fs : TFileStream;
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

  DefaultLvl(lvl);
  WriteLevel( ChangeFileExt(fn,'.converted.phd'), lvl);
end;


var
  fn  : string;
  ext : string;
begin
  if ParamCount=0 then begin
    writeln('please sepcify .phd file exit');
    exit;
  end;
  fn:=ParamStr(1);
  ext:=AnsiLowerCase(ExtractFileExt(fn));
  writeln('ext: ', ext);
  TR1Debug:=true;
  if ext='.phd' then
    ReadStats(fn)
  else if ext='.wad' then
    ReadWadStats(fn);
end.

