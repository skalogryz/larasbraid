program larasbraid;

{$mode delphi}

uses
  Classes, SysUtils,
  tr_types, tr_utils;

procedure DropSoundFiles(const lvl: TTRLevel);
var
  i   : integer;
  ofs : integer;
  sz  : integer;
begin
  for i:=0 to lvl.SampleIndexCount-1 do begin
    ofs:=lvl.SampleIndex[i];
    if i<lvl.SampleIndexCount-1 then
      sz:=lvl.SampleIndex[i+1]
    else
      sz:=lvl.SamplesCount;
    sz:=sz-ofs;
    SaveDataToFile(lvl.SamplesData[ofs], sz, Format('output%d.wav',[i]));
  end;
end;

procedure DumpItems(const lvl: TTRLevel);
var
  i : integer;
begin
  writeln('Items:');
  for i:=0 to lvl.ItemCount-1 do
    writeln(lvl.Item[i].object_id:10,lvl.Item[i].x:8,lvl.Item[i].y:8,lvl.Item[i].z:8);
end;

procedure DumpMeshTree(const lvl: TTRLevel; treeidx: integer);
var
  i : integer;
begin
  i:=treeidx;
  writeln('          @',i,' ',IntToHeX(lvl.MeshTree[i],8)
  ,' ',Integer(lvl.MeshTree[i+1])
  ,' ',Integer(lvl.MeshTree[i+2])
  ,' ',Integer(lvl.MeshTree[i+3])
  ,' ',Integer(lvl.MeshTree[i+4]));
end;

procedure DumpMoveables(const lvl: TTRLevel);
var
  i : integer;
begin
  writeln('Moveables:');
  for i:=0 to lvl.MoveableCount-1 do begin
    writeln('':10,i,' object_id: ', lvl.Moveable[i].object_id);
    writeln('':15,' animidx:  ',lvl.Moveable[i].animation_index);
    writeln('':15,' start:    ',lvl.Moveable[i].starting_mesh);
    writeln('':15,' meshes:   ',lvl.Moveable[i].num_meshes);
    writeln('':15,' treeidx:  ',lvl.Moveable[i].mesh_tree_index,' ',lvl.MeshTreeCount);
    writeln('':15,' frameofs: ',lvl.Moveable[i].frame_offset);
    DumpMeshTree(lvl, lvl.Moveable[i].mesh_tree_index);
  end;
end;

function isBraided(const lvl: TTRLevel): Integer;
var
  i : integer;
begin
  Result:=-1;
  for i:=0 to lvl.MoveableCount-1 do
    if lvl.Moveable[i].object_id=TR1_OBJID_BRAID then begin
      Result:=i;
      Exit;
    end;
end;

function BufToArray(const data: array of byte; size: integer): string;
var
  i : integer;
  j : integer;
begin
  SetLength(Result, size*4-1);
  FillChar(Result[1], length(Result),',');
  j:=1;
  for i:=0 to size-1 do begin
    Result[j]:='$'; inc(j);
    BinToHex(@data[i], @Result[j], 1);
    inc(j,3);
  end;
end;

procedure DumpMoveable(const lvl: TTRLevel; const mv: tr1_moveable);
var
  ofs : integer;
  sz  : integer;
  m   : tr1_mesh;
  i   : integer;
  k   : integer;
begin
  writeln('object_id:  ', mv.object_id);
  writeln('meshes:     ', mv.num_meshes);
  writeln('start mesh: ', mv.starting_mesh,' / ', lvl.MeshPtrCount);
  writeln('tree index: ', mv.mesh_tree_index,' / ', lvl.MeshTreeCount);
  writeln('frame ofs:  ', mv.frame_offset,' / ',lvl.FrameCount);

  ofs:=lvl.MeshPtr[mv.starting_mesh];
  for i:=0 to mv.num_meshes-1 do begin
    writelN('mesh ', i,' at ', ofs);
    sz:=ReadFromData(lvl.MeshData, ofs, m);
    writeln('  mesh size: ', sz);
    writeln('    vert:  ',m.num_vertices,' ',sizeof(tr1_vertex));
    writeln('    norm:  ',m.num_normals,' ',sizeof(tr1_vertex));
    writeln('    light: ',m.num_lights,' ',2);
    writeln('    trect: ',m.num_textured_rectangles,' ',sizeof(tr1_face4) );
    writeln('    crect: ',m.num_coloured_rectangles,' ',sizeof(tr1_face4));
    writeln('    ttria: ',m.num_textured_triangles,' ',sizeof(tr1_face3));
    writeln('    ctria: ',m.num_coloured_triangles,' ',sizeof(tr1_face3));
    inc(ofs, sz);

    MakeMeshStandAlone(m);
    writeln(BufToArray(m._selfdata, length(m._selfdata)) );

  end;


  i:=mv.mesh_tree_index;
  for k:=0 to mv.num_meshes-2 do begin
    writeln('  mesh tree: ', i);
    writeln('    ', IntToHex(lvl.MeshTree[i],8));
    writeln('    ofs : ', lvl.MeshTree[i+1],' ',lvl.MeshTree[i+2],' ',lvl.MeshTree[i+3]);
    inc(i, 4);
  end;
end;

procedure DumpMeshPointers(const lvl: TTRLevel);
var
  i : integer;
begin
  writeln('Mesh data count: ', lvl.MeshDataCount,' bytes = ',lvl.MeshDataCount*2);
  for i:=0 to lvl.MeshPtrCount-1 do begin
    writeln(i,' ',lvl.MeshPtr[i]);
  end;
end;


procedure AddBraid(var lvl: TTRLevel);
var
  i : integer;
  j : integer;
  mp  : integer;
  ofs : integer;
  k : integer;
begin
  i:=lvl.MoveableCount;
  lvl.MoveableCount:=lvl.MoveableCount+1;
  SetLength(lvl.Moveable, lvl.MoveableCount);
  lvl.Moveable[i].object_id:=TR1_OBJID_BRAID;
  lvl.Moveable[i].frame_offset:=lvl.FrameCount*2;
  lvl.Moveable[i].animation_index:=$FFFF;

  mp:=lvl.MeshPtrCount;
  ofs:=length(lvl.MeshData);

  lvl.Moveable[i].num_meshes    := length(BraidMesh);
  lvl.Moveable[i].starting_mesh := mp;

  lvl.MeshPtrCount:=lvl.MeshPtrCount+length(BraidMesh);
  SetLength(lvl.MeshPtr, lvl.MeshPtrCount);

  lvl.MeshDataCount:=lvl.MeshDataCount+(sizeof(BraidMesh) div 2);
  SetLength(lvl.MeshData, lvl.MeshDataCount*2);

  for j:=0 to length(BraidMesh)-1 do begin
    lvl.MeshPtr[mp]:=ofs;

    Move(BraidMesh[j][0], lvl.MeshData[ofs], length(BraidMesh[j]));

    inc(ofs, length(BraidMesh[j]));
    inc(mp);
  end;


  j:=lvl.MeshTreeCount;
  lvl.MeshTreeCount:=lvl.MeshTreeCount+(4*(length(BraidMesh)-1));

  SetLength(lvl.MeshTree, lvl.MeshTreeCount);

  lvl.Moveable[i].mesh_tree_index:=j;

  for k:=0 to length(BraidMesh)-2 do begin
    lvl.MeshTree[j]:=0;
    if k=0 then begin
      lvl.MeshTree[j+1]:=1;
      lvl.MeshTree[j+2]:=1;
    end else begin
      lvl.MeshTree[j+1]:=0;
      lvl.MeshTree[j+2]:=0;
    end;
    lvl.MeshTree[j+3]:=34;
    inc(j,4);
  end;
end;

var
  lvl: TTRLevel;
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

      mi:=isBraided(lvl);
      //DumpMeshPointers(lvl);
      //DumpMoveables(lvl);
      if mi>=0 then begin
        writeln('The level contains braid model. No changes done.');
        //DumpMoveable(lvl, lvl.Moveable[mi]);
      end else begin
        dst:=ChangeFileExt(ParamStr(1), '-braided.phd');
        writeln('The braid model is added. Writting to: ', dst);
        AddBraid(lvl);
        WriteLevel(dst, lvl);
      end;
    end;
  except
    on e:exception do
      writeln(e.message);
  end;
end.

