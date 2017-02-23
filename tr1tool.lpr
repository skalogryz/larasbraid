program tr1tool;

{$mode delphi}{$H+}

uses
  SysUtils, Classes, tr_types, tr1_utils;

procedure ReadStats(const fn: string);
var
  lvl: TTR1Level;
begin
  if not ReadLevel(fn, lvl, true) then begin
    writeln(Format('failed to read the level %s', [fn]));
    Exit;
  end;

end;

procedure ReadWadStats(const fn: string);
var
  lvl: TTR1Level;
begin
  ReadDemoWAD1(fn, lvl);
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

