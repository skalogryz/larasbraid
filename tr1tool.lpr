program tr1tool;

{$mode delphi}{$H+}

uses
  SysUtils, Classes, tr_types, tr1_utils;

procedure SeparateLvl(const lvl: TTR1Level);
var
  fs : TFileStream;
begin
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

  fs:=TFileStream.Create('models.out', fmCreate);
  try
    fs.Write(lvl.Model[0], lvl.ModelCount*sizeof(tr1_model));
  finally
    fs.Free;
  end;
end;

procedure DumpModelsToObj(const lvl: TTR1Level);
var
  s  : string;
  fs : TfileStream;
  i  : Integer;
begin
  for i:=0 to lvl.ModelCount-1 do begin
    s:=ModelToWavefrontStr( lvl.Model[i], lvl.MeshPtr, lvl.MeshData );
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

procedure ReadStats(const fn: string);
var
  lvl: TTR1Level;
begin
  if not ReadLevel(fn, lvl, true) then begin
    writeln(Format('failed to read the level %s', [fn]));
    Exit;
  end;

  SeparateLvl(lvl);
  DumpModelsToObj(lvl);
end;

procedure ReadWadStats(const fn: string);
var
  lvl: TTR1Level;
begin
  ReadDemoWAD1(fn, lvl);
  DumpModels(lvl.Model, lvl.ModelCount);
  DumpModelsToObj(lvl);
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

