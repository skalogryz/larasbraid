program tr1upscale;

{$mode delphi}

uses
  Classes, SysUtils,
  tr_types, tr1_utils;

procedure ObjTexToRect(const ot: tr1_object_texture; var r: TRect);
var
  miny,minx: integer;
  maxx,maxy: integer;
  i : integer;
  x,y: integer;
begin
  minx:=-1;
  miny:=-1;
  maxx:=-1;
  maxy:=-1;
  for i:=0 to 3 do begin
    if ot.Vertices[i].Xcoordinate=0 then continue;

    x:=ot.Vertices[i].Xpixel;
    y:=ot.Vertices[i].Ypixel;
    //writeln('x = ',x,' y = ',y);
    if (maxx<0) or (x>maxx) then maxx:=x;
    if (minx<0) or (x<minx) then minx:=x;

    if (maxy<0) or (y>maxy) then maxy:=y;
    if (miny<0) or (y<miny) then miny:=y;
  end;
  //writeln('x = ',minx,' y = ',miny,'; x = ',maxx,' y = ',maxy,' ',ot.Tile);
  r:=Rect(minx,miny,maxx,maxy);
end;

type

  { TTextureStat }

  TTextureStat = class(TObject)
  private
    fScale : Integer;
  public
    TexRect     : TRect;
    TileNum     : Integer;
    TexObjIdx   : array of Integer;
    TexObjCount : integer;
    function isRect(const ar: TRect): Boolean;
    function AddTexObj(aindex: integer): Integer;
  end;

{ TTextureStat }

function TTextureStat.isRect(const ar: TRect): Boolean;
begin
  Result:=(ar.Left=TexRect.Left)
       and(ar.Top=TexRect.Top)
       and(ar.Right=TexRect.Right)
       and(ar.Bottom=TexRect.Bottom)
  ;
end;

function TTextureStat.AddTexObj(aindex: integer): Integer;
begin
  if TexObjCount= length(TexObjIdx) then begin
    if TexObjCount=0 then SetLength(TexObjIdx, 4)
    else SetLEngth(TexObjIdx, TexObjCount*2);
  end;
  TexObjIdx[TexObjCount]:=aindex;
  Result:=TexObjCount;
  inc(TexObjCount);
end;

procedure DoAdjustTiles(var lvl: TTR1Level; ts: TList);
var
  i,j  : integer;
  t    : TTextureStat;
  y,x  : integer;
  xx   : integer;
  src  : array of tr_textile8;
  ln   : array of byte;
  v    : LongWord;
  vb   : Byte;
  w    : integer;
  sc   : integer;
  kx,k : integer;
begin
  writelN('making a source of textiles');
  SetLength(src, lvl.TextTileCount);
  writelN('copying textiles');
  Move(lvl.TextTile[0], src[0], lvl.TextTileCount*sizeof(tr_textile8));

  writelN('clearing out existing textiles');
  FillChar(lvl.TextTile[0], lvl.TextTileCount*sizeof(tr_textile8), 0);

  lvl.TextTileCount:=ts.Count;
  SetLength(lvl.TextTile, ts.Count);

  writelN('setting up the line copy ');
  setLength(ln, 256);
  for i:=0 to lvl.TextTileCount-1 do begin
    t:=TTextureStat(ts[i]);
    writelN('extending texture ', i,' top: ',t.TexRect.Top,' bottom: ',t.TexRect.Bottom);
    y:=0;
    w:=(t.TexRect.Right-t.TexRect.Left+1);
    if w<=64 then sc:=4
    else if w<=128 then sc:=2
    else sc:=1;
    t.fScale:=sc;
    w:=w*sc;
    for j:=t.TexRect.Top to t.TexRect.Bottom do begin
      xx:=0;
      for x:=t.TexRect.Left to t.TexRect.Right do begin
        vb:=src[t.TileNum].pixels[j][x];
        for kx:=1 to sc do begin
          ln[xx]:=vb;
          inc(xx);
        end;
        //for xx:=(x-t.TexRect.Left
        //v:=(vb shl 24) or (vb shl 16) or (vb shl 8) or vb;
        //PLongWord(@ln[xx])^:=v;
        //inc(xx,4);
      end;

      for kx:=1 to sc do begin
        Move(ln[0], lvl.TextTile[i].pixels[y][0], w);
        inc(y);
      end;

      {writeln('y0 ',y+0);
      writeln('y1 ',y+1);
      Move(ln[0], lvl.TextTile[i].pixels[y+1][0], w);
      writeln('y2 ',y+2);
      Move(ln[0], lvl.TextTile[i].pixels[y+2][0], w);
      writeln('y3 ',y+3);
      Move(ln[0], lvl.TextTile[i].pixels[y+3][0], w);
      inc(y,4);}
    end;
  end;

  for i:=0 to lvl.TextTileCount-1 do begin
    t:=TTextureStat(ts[i]);
    for j:=0 to t.TexObjCount-1 do begin
      x:=t.TexObjIdx[j];
      for k:=0 to length(lvl.ObjTex[x].Vertices) do begin
        if lvl.ObjTex[x].Vertices[k].Xcoordinate=0 then continue;

        if lvl.ObjTex[x].Vertices[k].Xpixel = t.TexRect.Left then
          lvl.ObjTex[x].Vertices[k].Xpixel:=0
        else
          lvl.ObjTex[x].Vertices[k].Xpixel:=(t.TexRect.Right-t.TexRect.Left+1)*t.fScale;

        if lvl.ObjTex[x].Vertices[k].Ypixel = t.TexRect.Left
          then lvl.ObjTex[x].Vertices[k].Ypixel:=0
          else lvl.ObjTex[x].Vertices[k].Ypixel:=(t.TexRect.Bottom-t.TexRect.Top+1)*t.fScale;
      end;
    end;
  end;
end;

procedure TextureStats(var lvl: TTR1Level; adjustTiles: Boolean);
var
  i  : integer;
  j  : integer;
  ts : TList;
  t  : TTextureStat;
  rr : TRect;
  rt : TTextureStat;
  tl : integer;
begin
  tS:=TList.Create;
  try
    for i:=0 to lvl.ObjTexCount-1 do begin
      ObjTexToRect(lvl.ObjTex[i], rr);
      tl:=lvl.ObjTex[i].Tile;
      rt:=nil;
      for j:=0 to ts.Count-1 do begin
        t:=TTextureStat(ts[j]);
        if t.TileNum<>tl then continue;
        if t.isRect(rr) then begin
          rt:=t;
          //writeln('Tex Object ', i,' matched to texture! ', j,'  = ', t.TexRect.Left,' ',t.TexRect.Top,' ',t.TexRect.Right,' ',t.TexRect.Right);
          Break;
        end;
      end;
      if not Assigned(rt) then begin
        rt:=TTextureStat.Create;
        rt.TileNum:=tl;
        rt.TexRect:=rr;
        ts.Add(rt);
      end;
      rt.AddTexObj(i);
    end;
    writeln('tiles:       ', lvl.TextTileCount);
    writeln('tex objects: ', lvl.ObjTexCount);
    writeln('textures:    ', ts.Count);

    if adjustTiles then
      DoAdjustTiles(lvl, ts);

  finally
    ts.Free;
  end;

end;



var
  lvl: TTR1Level;
  mi : integer;
  src : string;
  dst : string;



begin
  If paramCount=0 then begin
    writeln('please specify .phd file (Tomb Raider 1 level file)');
    exit;
  end;

  try
    src:=ParamStr(1);
    if not ReadLevel(src,lvl) then
      writeln('something went wrong with reading: ',src)
    else begin
      TextureStats(lvl, true);
      WriteLevel(ChangeFileExt(src,'.upscaled.phd'),  lvl);
    end;
  except
    on e:exception do
      writeln(e.message);
  end;
end.


