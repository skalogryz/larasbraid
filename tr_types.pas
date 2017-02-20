unit tr_types;

{$mode objfpc}{$H+}
{$packrecords c}

interface

const
  TR1 = $0000020;

type
  bitu8  = Byte;
  bit8   = ShortInt;
  bitu16 = Word;
  bit16  = SmallInt;
  bitu32 = Longword;
  bit32  = Integer;

  pbit16 = ^bit16;
  pbit32 = ^bit32;

  {/  RGBA colour using bitu8. For palette etc. }
  tr2_colour_s = record
    r : bitu8;  {/<  the red component. }
    g : bitu8;  {/<  the green component. }
    b : bitu8;  {/<  the blue component. }
    a : bitu8;  {/<  the alpha component. }
  end;
  tr2_colour_t = tr2_colour_s;

  {/  RGBA colour using float. For single colours. }
  tr5_colour_s = record
    r : single; {/<  the red component. }
    g : single; {/<  the green component. }
    b : single; {/<  the blue component. }
    a : single; {/<  the alpha component. }
  end;
  tr5_colour_t = tr5_colour_s;
    {/  A vertex with x, y and z coordinates. }

  tr5_vertex = record
    x : single;
    y : single;
    z : single;
  end;

  tr5_vertex_s = tr5_vertex;
  tr5_vertex_t = tr5_vertex_s;
    {/  Definition for a triangle. }
    {/< index into the appropriate list of vertices. }
    {*<  object-texture index or colour index.
                                   * If the triangle is textured, then this is an index into the object-texture list.
                                   * If it's not textured, then the low 8 bit contain the index into the 256 colour palette
                                   * and from TR2 on the high 8 bit contain the index into the 16 bit palette.
                                    }
    {*<  transparency flag & strength of the hilight (TR4-TR5).
                                   * bit0 if set, then alpha channel = intensity (see attribute in tr2_object_texture).<br>
                                   * bit1-7 is the strength of the hilight.
                                    }

      tr4_face3_s = record
          vertices : array[0..2] of bitu16;
          texture : bitu16;
          lighting : bitu16;
        end;
      tr4_face3_t = tr4_face3_s;
    {/  Definition for a rectangle. }
    {/< index into the appropriate list of vertices. }
    {*<  object-texture index or colour index.
                                   * If the rectangle is textured, then this is an index into the object-texture list.
                                   * If it's not textured, then the low 8 bit contain the index into the 256 colour palette
                                   * and from TR2 on the high 8 bit contain the index into the 16 bit palette.
                                    }
    {*<  transparency flag & strength of the hilight (TR4-TR5).
                                   *
                                   * In TR4, objects can exhibit some kind of light reflection when seen from some particular angles.
                                   * - bit0 if set, then alpha channel = intensity (see attribute in tr2_object_texture).
                                   * - bit1-7 is the strength of the hilight.
                                    }

      tr4_face4_s = record
          vertices : array[0..3] of bitu16;
          texture : bitu16;
          lighting : bitu16;
        end;
      tr4_face4_t = tr4_face4_s;


    {*  8-bit texture.
      *
      * Each pixel is an index into the colour palette.
       }
    tr_textile8 = record
      pixels : array[0..255] of array[0..255] of bitu8;
    end;
    tr_textile8_s = tr_textile8; // OpenTomb comatibility
    tr_textile8_t = tr_textile8;

    {*  16-bit texture.
      *
      * Each pixel is a colour with the following format.<br>
      * - 1-bit transparency (0 ::= transparent, 1 ::= opaque) (0x8000)
      * - 5-bit red channel (0x7c00)
      * - 5-bit green channel (0x03e0)
      * - 5-bit blue channel (0x001f)
       }

      tr2_textile16_s = record
          pixels : array[0..255] of array[0..255] of bitu16;
        end;
      tr2_textile16_t = tr2_textile16_s;
    {*  32-bit texture.
      *
      * Each pixel is an ABGR value.
       }

      tr4_textile32_s = record
          pixels : array[0..255] of array[0..255] of bitu32;
        end;
      tr4_textile32_t = tr4_textile32_s;

  {*  Room portal. }
  tr5_room_portal = record
    adjoining_room : bitu16;       {   which room this portal leads to. }
    normal         : tr5_vertex_t; {* which way the portal faces.
                                       * the normal points away from the adjacent room.
                                       * to be seen through, it must point toward the viewpoint.
                                        }
    vertices       : array[0..3] of tr5_vertex_t;  {* the corners of this portal.
                                       * the right-hand rule applies with respect to the normal.
                                       * if the right-hand-rule is not followed, the portal will
                                       * contain visual artifacts instead of a viewport to
                                       * AdjoiningRoom.
                                        }
  end;
  // aliases
  tr5_room_portal_s = tr5_room_portal;
  tr5_room_portal_t = tr5_room_portal_s;
    {*  Room sector.
       }
    { Index into FloorData[] }
    { Index into Boxes[]/Zones[] (-1 if none) }
    { The number of the room below this one (-1 or 255 if none) }
    { Absolute height of floor (multiply by 256 for world coordinates) }
    { The number of the room above this one (-1 or 255 if none) }
    { Absolute height of ceiling (multiply by 256 for world coordinates) }

      tr_room_sector_s = record
          fd_index : bitu16;
          box_index : bitu16;
          room_below : bitu8;
          floor : bit8;
          room_above : bitu8;
          ceiling : bit8;
        end;
      tr_room_sector_t = tr_room_sector_s;
    {*  Room light.
       }
    { world coords }
    { three bytes rgb values }
    { Calculated intensity }
    { Light intensity }
    { Almost always equal to Intensity1 [absent from TR1 data files] }
    { Falloff value 1 }
    { Falloff value 2 [absent from TR1 data files] }
    { same as D3D (i.e. 2 is for spotlight) }
    { always 0xff? }
    { direction }
    { world coords }
    { direction }

      tr5_room_light_s = record
          pos : tr5_vertex_t;
          color : tr2_colour_t;
          intensity : single;
          intensity1 : bitu16;
          intensity2 : bitu16;
          fade1 : bitu32;
          fade2 : bitu32;
          light_type : bitu8;
          unknown : bitu8;
          r_inner : single;
          r_outer : single;
          length : single;
          cutoff : single;
          dir : tr5_vertex_t;
          pos2 : tr5_vertex_t;
          dir2 : tr5_vertex_t;
        end;
      tr5_room_light_t = tr5_room_light_s;
    {*  Room sprite.
       }
    { offset into vertex list }
    { offset into sprite texture list }

      tr_room_sprite_s = record
          vertex : bit16;
          texture : bit16;
        end;
      tr_room_sprite_t = tr_room_sprite_s;
    {*  Room layer (TR5).
       }
    { number of vertices in this layer (4 bytes) }
    { number of rectangles in this layer (2 bytes) }
    { number of triangles in this layer (2 bytes) }
    {  The following 6 floats (4 bytes each) define the bounding box for the layer }

      tr5_room_layer_s = record
          num_vertices : bitu16;
          unknown_l1 : bitu16;
          unknown_l2 : bitu16;
          num_rectangles : bitu16;
          num_triangles : bitu16;
          unknown_l3 : bitu16;
          unknown_l4 : bitu16;
          bounding_box_x1 : single;
          bounding_box_y1 : single;
          bounding_box_z1 : single;
          bounding_box_x2 : single;
          bounding_box_y2 : single;
          bounding_box_z2 : single;
          unknown_l6a : bit16;
          unknown_l6b : bit16;
          unknown_l7a : bit16;
          unknown_l7b : bit16;
          unknown_l8a : bit16;
          unknown_l8b : bit16;
        end;
      tr5_room_layer_t = tr5_room_layer_s;
    {*  Room vertex.
       }
    { where this vertex lies (relative to tr2_room_info::x/z) }
    { A set of flags for special rendering effects [absent from TR1 data files] }
    { 0x8000 something to do with water surface }
    { 0x4000 under water lighting modulation and }
    { movement if viewed from above water surface }
    { 0x2000 water/quicksand surface movement }
    { 0x0010 "normal" }
    { Almost always equal to Lighting1 [absent from TR1 data files] }
    { TR5 --> }
    { vertex color ARGB format (4 bytes) }

      tr5_room_vertex_s = record
          vertex : tr5_vertex_t;
          lighting1 : bit16;
          attributes : bitu16;
          lighting2 : bit16;
          normal : tr5_vertex_t;
          colour : tr5_colour_t;
        end;
      tr5_room_vertex_t = tr5_room_vertex_s;
    {*  Room staticmesh.
       }
    { world coords }
    { high two bits (0xC000) indicate steps of }
    { 90 degrees (e.g. (Rotation >> 14) * 90) }
    { Constant lighting; -1 means use mesh lighting }
    { Like Intensity 1, and almost always the same value [absent from TR1 data files] }
    { which StaticMesh item to draw }
    { extracted from intensity }

      tr2_room_staticmesh_s = record
          pos : tr5_vertex_t;
          rotation : single;
          intensity1 : bit16;
          intensity2 : bit16;
          object_id : bitu16;
          tint : tr5_colour_t;
        end;
      tr2_room_staticmesh_t = tr2_room_staticmesh_s;
    {*  Room.
       }
    {/<  offset of room (world coordinates). }
    {/<  indicates lowest point in room. }
    {/<  indicates highest point in room. }
    { number of layers (pieces) this room (4 bytes) }
    { [NumStaticMeshes]list of static meshes }
    { number of vertices in the following list }
    { [NumVertices] list of vertices (relative coordinates) }
    { number of textured rectangles }
    { [NumRectangles] list of textured rectangles }
    { number of textured triangles }
    { [NumTriangles] list of textured triangles }
    { number of sprites }
    { [NumSprites] list of sprites }
    { number of visibility portals to other rooms }
    { [NumPortals] list of visibility portals }
    { "width" of sector list }
    { "height" of sector list }
    { [NumXsectors * NumZsectors] list of sectors }
    { in this room }
    { This and the next one only affect externally-lit objects }
    { Almost always the same value as AmbientIntensity1 [absent from TR1 data files] }
    { (present only in TR2: 0 is normal, 1 is flickering(?), 2 and 3 are uncertain) }
    { number of point lights in this room }
    { [NumLights] list of point lights }
    { number of static meshes }
    { [NumStaticMeshes]list of static meshes }
    { number of the room that this room can alternate }
    { number of group which is used to switch alternate rooms }
    { with (e.g. empty/filled with water is implemented as an empty room that alternates with a full room) }
    { Flag bits: }
    { 0x0001 - room is filled with water, }
    { 0x0020 - Lara's ponytail gets blown by the wind; }
    { TR1 has only the water flag and the extra unknown flag 0x0100. }
    { TR3 most likely has flags for "is raining", "is snowing", "water is cold", and "is }
    { filled by quicksand", among others. }
    { Water scheme is used with various room options, for example, R and M room flags in TRLE. }
    { Also, it specifies lighting scheme, when 0x4000 vertex attribute is set. }
    { Reverb info is used in TR3-5 and contains index that specifies reverb type. }
    { 0 - Outside, 1 - Small room, 2 - Medium room, 3 - Large room, 4 - Pipe. }
    { Present in TR4 and TR5 only }
    { TR5 only: }

      tr5_room_s = record
          offset : tr5_vertex_t;
          y_bottom : single;
          y_top : single;
          num_layers : bitu32;
          layers : ^tr5_room_layer_t;
          num_vertices : bitu32;
          vertices : ^tr5_room_vertex_t;
          num_rectangles : bitu32;
          rectangles : ^tr4_face4_t;
          num_triangles : bitu32;
          triangles : ^tr4_face3_t;
          num_sprites : bitu32;
          sprites : ^tr_room_sprite_t;
          num_portals : bitu16;
          portals : ^tr5_room_portal_t;
          num_zsectors : bitu16;
          num_xsectors : bitu16;
          sector_list : ^tr_room_sector_t;
          intensity1 : bit16;
          intensity2 : bit16;
          light_mode : bit16;
          num_lights : bitu16;
          lights : ^tr5_room_light_t;
          num_static_meshes : bitu16;
          static_meshes : ^tr2_room_staticmesh_t;
          alternate_room : bit16;
          alternate_group : bit8;
          flags : bitu16;
          water_scheme : bitu8;
          reverb_info : bitu8;
          light_colour : tr5_colour_t;
          room_x : single;
          room_z : single;
          room_y_bottom : single;
          room_y_top : single;
          unknown_r1 : bitu32;
          unknown_r2 : bitu32;
          unknown_r3 : bitu32;
          unknown_r4a : bitu16;
          unknown_r4b : bitu16;
          unknown_r5 : bitu32;
          unknown_r6 : bitu32;
        end;
      tr5_room_t = tr5_room_s;
    {*  Mesh.
       }
    { This is usually close to the mesh's centroid, and appears to be the center of a sphere used for collision testing. }
    { This appears to be the radius of that aforementioned collisional sphere. }
    { number of vertices in this mesh }
    {[NumVertices]; // list of vertices (relative coordinates) }
    { If positive, number of normals in this mesh. }
    { If negative, number of vertex lighting elements (* (-1)) }
    { Engine internal for above }
    {[NumNormals]; // list of normals (if NumNormals is positive) }
    {[-NumNormals]; // list of light values (if NumNormals is negative) }
    { number of textured rectangles in this mesh }
    {[NumTexturedRectangles]; // list of textured rectangles }
    { number of textured triangles in this mesh }
    {[NumTexturedTriangles]; // list of textured triangles }
    { the rest is not present in TR4 }
    { number of coloured rectangles in this mesh }
    {[NumColouredRectangles]; // list of coloured rectangles }
    { number of coloured triangles in this mesh }
    {[NumColouredTriangles]; // list of coloured triangles }

      tr4_mesh_s = record
          centre : tr5_vertex_t;
          collision_size : bit32;
          num_vertices : bit16;
          vertices_count : bitu32;
          vertices : ^tr5_vertex_t;
          num_normals : bit16;
          num_lights : bit16;
          normals : ^tr5_vertex_t;
          lights : ^bit16;
          num_textured_rectangles : bit16;
          textured_rectangles : ^tr4_face4_t;
          num_textured_triangles : bit16;
          textured_triangles : ^tr4_face3_t;
          num_coloured_rectangles : bit16;
          coloured_rectangles : ^tr4_face4_t;
          num_coloured_triangles : bit16;
          coloured_triangles : ^tr4_face3_t;
        end;
      tr4_mesh_t = tr4_mesh_s;
    {*  Staticmesh.
       }
    { 32 bytes }
    { Object Identifier (matched in Items[]) }
    { mesh (offset into MeshPointers[]) }
    { Meaning uncertain; it is usually 2, and is 3 for objects Lara can travel through, }
    { like TR2's skeletons and underwater vegetation }

      tr_staticmesh_s = record
          object_id : bitu32;
          mesh : bitu16;
          visibility_box : array[0..1] of tr5_vertex_t;
          collision_box : array[0..1] of tr5_vertex_t;
          flags : bitu16;
        end;
      tr_staticmesh_t = tr_staticmesh_s;


    {* MeshTree.
     *
     * MeshTree[] is actually groups of four bit32s. The first one is a
     * "flags" word;
     *    bit 1 (0x0002) indicates "put the parent mesh on the mesh stack";
     *    bit 0 (0x0001) indicates "take the top mesh off of the mesh stack and use as the parent mesh"
     * when set, otherwise "use the previous mesh are the parent mesh".
     * When both are present, the bit-0 operation is always done before the bit-1 operation; in effect, read the stack but do not change it.
     * The next three bit32s are X, Y, Z offsets of the mesh's origin from the parent mesh's origin.
      }
    tr5_meshtree = record     { 4 bytes }
      flags  : bitu32;
      offset : tr5_vertex_t;
    end;
    tr5_meshtree_s = tr5_meshtree;
    tr5_meshtree_t = tr5_meshtree_s;
    {*  Frame.
      *
      * Frames indicates how composite meshes are positioned and rotated.
      * They work in conjunction with Animations[] and Bone2[].
      *
      * A given frame has the following format:
      *    short BB1x, BB1y, BB1z           // bounding box (low)
      *    short BB2x, BB2y, BB2z           // bounding box (high)
      *    short OffsetX, OffsetY, OffsetZ  // starting offset for this moveable
      *    (TR1 ONLY: short NumValues       // number of angle sets to follow)
      *    (TR2/3: NumValues is implicitly NumMeshes (from moveable))
      *
      * What follows next is a list of angle sets.  In TR2/3, an angle set can
      * specify either one or three axes of rotation.  If either of the high two
      * bits (0xc000) of the first angle unsigned short are set, it's one axis:
      *  only one  unsigned short,
      *  low 10 bits (0x03ff),
      *  scale is 0x100 == 90 degrees;
      * the high two  bits are interpreted as follows:
      *  0x4000 == X only, 0x8000 == Y only,
      *  0xC000 == Z only.
      *
      * If neither of the high bits are set, it's a three-axis rotation.  The next
      * 10 bits (0x3ff0) are the X rotation, the next 10 (including the following
      * unsigned short) (0x000f, 0xfc00) are the Y rotation,
      * the next 10 (0x03ff) are the Z rotation, same scale as
      * before (0x100 == 90 degrees).
      *
      * Rotations are performed in Y, X, Z order.
      * TR1 ONLY: All angle sets are two words and interpreted like the two-word
      * sets in TR2/3, EXCEPT that the word order is reversed.
      *
       }
    {typedef struct tr_frame_s

        tr5_vertex_t bbox_low;
        tr5_vertex_t bbox_high;
        tr5_vertex_t offset;
        tr5_vertex_array_t rotations;
        bit32 byte_offset;
     tr_frame_t;
    typedef prtl::array < tr_frame_t > tr_frame_array_t; }
    {*  Moveable.
       }
  tr_moveable_s = record         { 18 bytes }
      object_id       : bitu32;  { Item Identifier (matched in Items[]) }
      num_meshes      : bitu16;  { number of meshes in this object }
      starting_mesh   : bitu16;  { stating mesh (offset into MeshPointers[]) }
      mesh_tree_index : bitu32;  { offset into MeshTree[] }
      frame_offset    : bitu32;  { byte offset into Frames[] (divide by 2 for Frames[i]) }
      frame_index     : bitu32;
      animation_index : bitu16;  { offset into Animations[] }
    end;
  tr_moveable_t = tr_moveable_s;
    {*  Item.
       }
    { 24 bytes [TR1: 22 bytes] }
    { Object Identifier (matched in Moveables[], or SpriteSequences[], as appropriate) }
    { which room contains this item }
    { world coords }
    { ((0xc000 >> 14) * 90) degrees }
    { (constant lighting; -1 means use mesh lighting) }
    { Like Intensity1, and almost always with the same value. [absent from TR1 data files] }
    { Object code bit - used for altering entity behaviour. Only in TR4-5. }
    { 0x0100 indicates "initially invisible", 0x3e00 is Activation Mask }
    { 0x3e00 indicates "open" or "activated";  these can be XORed with }
    { related FloorData::FDlist fields (e.g. for switches) }

      tr2_item_s = record
          object_id : bit16;
          room : bit16;
          pos : tr5_vertex_t;
          rotation : single;
          intensity1 : bit16;
          intensity2 : bit16;
          ocb : bit16;
          flags : bitu16;
        end;
      tr2_item_t = tr2_item_s;


  {* Sprite texture. }

  tr_sprite_texture = packed record
    tile        : bitu16;
    x0          : bit16;
    y0          : bit16;
    x1          : bit16;
    y1          : bit16;
    left_side   : bit16;
    top_side    : bit16;
    right_side  : bit16;
    bottom_side : bit16;
  end;

  tr_sprite_texture_s = tr_sprite_texture;
  tr_sprite_texture_t = tr_sprite_texture_s;

  tr1_sprite_texture = packed record { 16 bytes }
    tile        : bitu16;
    x           : bit8;
    y           : bit8;
    width       : bit16;  // actually (Width * 256) + 255
    height      : bit16;  // actually (Height * 256) + 255
    left_side   : bit16;
    top_side    : bit16;
    right_side  : bit16;
    bottom_side : bit16;
  end;

  {* Sprite sequence.  }

  tr_sprite_sequence = record { 8 bytes }
    object_id : bit32;        { Item identifier (matched in Items[]) }
    length    : bit16;        { negative of "how many sprites are in this sequence" }
    offset    : bit16;        { where (in sprite texture list) this sequence starts }
  end;
  tr_sprite_sequence_s = tr_sprite_sequence;
  tr_sprite_sequence_t = tr_sprite_sequence_s;
  tr1_sprite_sequence = tr_sprite_sequence;

    {*  Animation.
      *
      * This describes each individual animation; these may be looped by specifying
      * the next animation to be itself. In TR2 and TR3, one must be careful when
      * parsing frames using the FrameSize value as the size of each frame, since
      * an animation's frame range may extend into the next animation's frame range,
      * and that may have a different FrameSize value.
       }

  tr_animation = record            { 32 bytes TR1/2/3 40 bytes TR4 }
    frame_offset        : bitu32;  { byte offset into Frames[] (divide by 2 for Frames[i]) }
    frame_rate          : bitu8;   { Engine ticks per frame }
    frame_size          : bitu8;   { number of bit16's in Frames[] used by this animation }
    state_id            : bitu16;  { new in TR4 --> }
    speed               : single;  { lateral speed and acceleration. }
    accel               : single;
    speed_lateral       : single;
    accel_lateral       : single;
    frame_start         : bitu16;  { first frame in this animation }
    frame_end           : bitu16;  { last frame in this animation (numframes = (End - Start) + 1) }
    next_animation      : bitu16;
    next_frame          : bitu16;
    num_state_changes   : bitu16;
    state_change_offset : bitu16;  { offset into StateChanges[] }
    num_anim_commands   : bitu16;  { How many of them to use. }
    anim_command        : bitu16;  { offset into AnimCommand[] }
  end;

  tr1_animation = packed record    { 32 bytes TR1/2/3 40 bytes TR4 }
    frame_offset        : bitu32;  { byte offset into Frames[] (divide by 2 for Frames[i]) }
    frame_rate          : bitu8;   { Engine ticks per frame }
    frame_size          : bitu8;   { number of bit16's in Frames[] used by this animation }
    state_id            : bitu16;
    unk                 : array [0..7] of bitu8;
    frame_start         : bitu16;  { first frame in this animation }
    frame_end           : bitu16;  { last frame in this animation (numframes = (End - Start) + 1) }
    next_animation      : bitu16;
    next_frame          : bitu16;
    num_state_changes   : bitu16;
    state_change_offset : bitu16;  { offset into StateChanges[] }
    num_anim_commands   : bitu16;  { How many of them to use. }
    anim_command        : bitu16;  { offset into AnimCommand[] }
  end;
  tr2_animation = tr1_animation;

  tr_animation_s = tr_animation;
  tr_animation_t = tr_animation_s;

  {* State Change.
   *
   * Each one contains the state to change to and which animation dispatches
   * to use; there may be more than one, with each separate one covering a different
   * range of frames.
  }
  tr_state_change = record        { 6 bytes }
    state_id            : bitu16;
    num_anim_dispatches : bitu16; { number of ranges (seems to always be 1..5) }
    anim_dispatch       : bitu16; { Offset into AnimDispatches[] }
  end;

  tr_state_change_s = tr_state_change;
  tr_state_change_t = tr_state_change_s;

  {* Animation Dispatch.
   *
   * This specifies the next animation and frame to use; these are associated
   * with some range of frames. This makes possible such specificity as one
   * animation for left foot forward and another animation for right foot forward.
    }
  tr_anim_dispatch = record   { 8 bytes }
    low            : bit16;   { Lowest frame that uses this range }
    high           : bit16;   { Highest frame (+1?) that uses this range }
    next_animation : bit16;   { Animation to dispatch to }
    next_frame     : bit16;   { Frame offset to dispatch to }
  end;
  tr_anim_dispatch_s = tr_anim_dispatch;
  tr_anim_dispatch_t = tr_anim_dispatch_s;

  tr_anim_command = bit16;

    {*  Animation Command.
      *
      * These are various commands associated with each animation; they are
      * called "Bone1" in some documentation. They are varying numbers of bit16's
      * packed into an array; the first of each set is the opcode, which determines
      * how operand bit16's follow it. Some of them refer to the whole animation
      * (jump and grab points, etc.), while others of them are associated with
      * specific frames (sound, bubbles, etc.).
       }
    {typedef struct         // 2 bytes }
    {    bit16 value; }
    { tr_anim_command_t; }


  {* Box. }
  tr_box = packed record     { 8 bytes [TR1: 20 bytes] In TR1, the first four are bit32's instead of bitu8's, and are not scaled. }
    zmin          : bitu32;  { sectors (* 1024 units) }
    zmax          : bitu32;
    xmin          : bitu32;
    xmax          : bitu32;
    true_floor    : bit16;   { Y value (no scaling) }
    overlap_index : bit16;   { index into Overlaps[]. The high bit is sometimes set; this }
                             { occurs in front of swinging doors and the like. }
  end;
  tr_box_s = tr_box;
  tr_box_t = tr_box_s;

  tr1_box = tr_box;

  {*  SoundSource.
   *
   * This structure contains the details of continuous-sound sources. Although
   * a SoundSource object has a position, it has no room membership; the sound
   * seems to propagate omnidirectionally for about 10 horizontal-grid sizes
   * without regard for the presence of walls.
   }
  tr_sound_source = packed record
    x        : bit32;   { absolute X position of sound source (world coordinates) }
    y        : bit32;   { absolute Y position of sound source (world coordinates) }
    z        : bit32;   { absolute Z position of sound source (world coordinates) }
    sound_id : bitu16;  { internal sound index }
    flags    : bitu16;  { 0x40, 0x80, or 0xc0 }
  end;
  tr_sound_source_s = tr_sound_source;
  tr_sound_source_t = tr_sound_source_s;
  tr1_sound_source = tr_sound_source;

    {*  SoundDetails.
     *
     * SoundDetails (also called SampleInfos in native TR sources) are properties
     * for each sound index from SoundMap. It contains all crucial information
     * that is needed to play certain sample, except offset to raw wave buffer,
     * which is unnecessary, as it is managed internally by DirectSound.
      }
    { 8 bytes }
    { Index into SampleIndices -- NOT USED IN TR4-5!!! }
    { Global sample value }
    { Sound range }
    { Chance to play }
    { Pitch shift }
    { Bits 0-1: Looped flag, bits 2-5: num samples, bits 6-7: UNUSED }
    { Bit 4: UNKNOWN, bit 5: Randomize pitch, bit 6: randomize volume }
    { All other bits in flags_2 are unused. }

      tr_sound_details_s = record
          sample : bitu16;
          volume : bitu16;
          sound_range : bitu16;
          chance : bitu16;
          pitch : bit16;
          num_samples_and_flags_1 : bitu8;
          flags_2 : bitu8;
        end;
      tr_sound_details_t = tr_sound_details_s;
    {*  Object Texture Vertex.
      *
      * It specifies a vertex location in textile coordinates.
      * The Xpixel and Ypixel are the actual coordinates of the vertex's pixel.
      * The Xcoordinate and Ycoordinate values depend on where the other vertices
      * are in the object texture. And if the object texture is used to specify
      * a triangle, then the fourth vertex's values will all be zero.
       }
    { 4 bytes }
    { 1 if Xpixel is the low value, -1 if Xpixel is the high value in the object texture }
    { 1 if Ypixel is the low value, -1 if Ypixel is the high value in the object texture }

      tr4_object_texture_vert_s = record
          xcoordinate : bit8;
          xpixel : bitu8;
          ycoordinate : bit8;
          ypixel : bitu8;
        end;
      tr4_object_texture_vert_t = tr4_object_texture_vert_s;
    {*  Object Texture.
      *
      * These, thee contents of ObjectTextures[], are used for specifying texture
      * mapping for the world geometry and for mesh objects.
       }
    { 38 bytes TR4 - 20 in TR1/2/3 }
    { 0 means that a texture is all-opaque, and that transparency }
    { information is ignored. }
    { 1 means that transparency information is used. In 8-bit colour, }
    { index 0 is the transparent colour, while in 16-bit colour, the }
    { top bit (0x8000) is the alpha channel (1 = opaque, 0 = transparent). }
    { 2 (only in TR3) means that the opacity (alpha) is equal to the intensity; }
    { the brighter the colour, the more opaque it is. The intensity is probably calculated }
    { as the maximum of the individual color values. }
    { index into textile list }
    { TR4 }
    { the four corners of the texture }
    { TR4 }
    { TR4 }
    { TR4 }
    { TR4 }

      tr4_object_texture_s = record
          transparency_flags : bitu16;
          tile_and_flag : bitu16;
          flags : bitu16;
          vertices : array[0..3] of tr4_object_texture_vert_t;
          unknown1 : bitu32;
          unknown2 : bitu32;
          x_size : bitu32;
          y_size : bitu32;
        end;
      tr4_object_texture_t = tr4_object_texture_s;
    {*  Animated Textures.
       }
    {bit16 num_texture_ids;    // Actually, this is the number of texture ID's - 1. }
    {[NumTextureIDs + 1]; // offsets into ObjectTextures[], in animation order. }

      tr_animated_textures_s = record
          texture_ids_count : bit16;
          texture_ids : ^bit16;
        end;
      tr_animated_textures_t = tr_animated_textures_s;
    {[NumAnimatedTextures]; }
    {*  Camera.
       }
    { correlates to Boxes[]? Zones[]? }

  tr_camera = record
    x        : bit32;
    y        : bit32;
    z        : bit32;
    room     : bit16;
    unknown1 : bitu16;
  end;
  tr_camera_s = tr_camera;
  tr_camera_t = tr_camera_s;
  tr1_camera = tr_camera;

    {*  Extra Camera.
       }

      tr4_flyby_camera_s = record
          pos_x : bit32;
          pos_y : bit32;
          pos_z : bit32;
          target_x : bit32;
          target_y : bit32;
          target_z : bit32;
          sequence : bitu8;
          index : bitu8;
          fov : bitu16;
          roll : bitu16;
          timer : bitu16;
          speed : bitu16;
          flags : bitu16;
          room_id : bit32;
        end;
      tr4_flyby_camera_t = tr4_flyby_camera_s;
    {*  AI Object.
       }
    { the objectID from the AI object (AI_FOLLOW is 402) }
    { The trigger flags (button 1-5, first button has value 2) }

      tr4_ai_object_s = record
          object_id : bitu16;
          room : bitu16;
          x : bit32;
          y : bit32;
          z : bit32;
          ocb : bitu16;
          flags : bitu16;
          angle : bit32;
        end;
      tr4_ai_object_t = tr4_ai_object_s;


  {* Cinematic Frame.  }

  tr_cinematic_frame = packed record
    targetx : bit16; {Camera look at position }
    targety : bit16;
    targetz : bit16;
    posx    : bit16; {Camera position }
    posy    : bit16;
    posz    : bit16;
    fov     : bit16;
    roll    : bit16;
  end;
  tr_cinematic_frame_s = tr_cinematic_frame;
  tr_cinematic_frame_t = tr_cinematic_frame_s;
  tr1_cinematic_frame = tr_cinematic_frame;

    {*  Lightmap.
       }

      tr_lightmap_s = record
          map : array[0..(32*256)-1] of bitu8;
        end;
      tr_lightmap_t = tr_lightmap_s;
    {*  Palette.
       }

      tr2_palette_s = record
          colour : array[0..255] of tr2_colour_t;
        end;
      tr2_palette_t = tr2_palette_s;

  tr2_room_info = record    // 16 bytes
    x       : bit32; // X-offset of room (world coordinates)
    z       : bit32; // Z-offset of room (world coordinates)
    yBottom : bit32; // (actually largest value, but indicates lowest point in room)
    yTop    : bit32; // (actually smallest value, but indicates highest point in room)
  end;

  { Vertex structure - this is how vertices are specified, using relative coordinates.
    They are generally formed into lists, such that other entities (such as quads or triangles)
    can refer to them by simply using their index in the list. }
  tr1_vertex = record
    x : bit16;
    y : bit16;
    z : bit16;
  end;
  ptr1_vertex = ^tr1_vertex;
  tr2_vertex = tr1_vertex;
  tr1_vertexarray = array [word] of tr1_vertex;
  ptr1_vertexarray = ^tr1_vertexarray;

  tr1_face4 = packed record
    Verticies : array [0..3] of bitu16;
    Texture   : bitu16;
  end;
  ptr1_face4 = tr1_face4;
  tr1_face4array = array [Word] of tr1_face4;
  ptr1_face4array = ^tr1_face4array;

  tr2_face4 = tr1_face4;

  tr1_face3 = packed record
    Verticies : array [0..2] of bitu16;
    Texture   : bitu16;
  end;
  ptr1_faces3 = tr1_face3;
  tr1_face3array = array [Word] of tr1_face3;
  ptr1_face3array = ^tr1_face3array;

  tr2_face3 = tr1_face3;

  tr1_room_sprite = record
    Vertex    : bit16;
    Texture   : bit16;
  end;
  tr2_room_sprite = tr1_room_sprite;


  {* \Room portal. }
  tr1_room_portal = record
    adjoining_room : bitu16;       {   which room this portal leads to. }
    normal         : tr1_vertex; {* which way the portal faces.
                                       * the normal points away from the adjacent room.
                                       * to be seen through, it must point toward the viewpoint.
                                        }
    vertices       : array[0..3] of tr1_vertex;  {* the corners of this portal.
                                       * the right-hand rule applies with respect to the normal.
                                       * if the right-hand-rule is not followed, the portal will
                                       * contain visual artifacts instead of a viewport to
                                       * AdjoiningRoom.
                                        }
  end;
  tr1_room_door = tr1_room_portal;

  tr1_vertex_room = record
    vertex    : tr1_vertex;
    Lighting1 : bit16;
  end;

  tr1_room_light = packed record  // 18 bytes
    x          : bit32;   // X-position of light, in world coordinates
    y          : bit32;   // Y-position of light, in world coordinates
    z          : bit32;   // Z-position of light, in world coordinates
    Intensity1 : bitu16;  // Light intensity
    Fade1      : bitu32;  // Falloff value 1
  end;

  tr2_room_light = record  // 24 bytes [TR1: 18 bytes]
    x          : bit32;   // X-position of light, in world coordinates
    y          : bit32;   // Y-position of light, in world coordinates
    z          : bit32;   // Z-position of light, in world coordinates
    Intensity1 : bitu16;  // Light intensity
    Intensity2 : bitu16;  // Almost always equal to Intensity1 [absent from TR1 data files]
    Fade1      : bitu32;  // Falloff value 1
    Fade2      : bitu32;  // Falloff value 2 [absent from TR1 data files]
  end;

  tr1_room_sector = record  // 8 bytes
    FDindex   : bitu16;     // Index into FloorData[]
    BoxIndex  : bitu16;     // Index into Boxes[]/Zones[] (-1 if none)
    RoomBelow : bitu8;      // The number of the room below this one (-1 or 255 if none)
    Floor     : bit8;       // Absolute height of floor (multiply by 256 for world coordinates)
    RoomAbove : bitu8;      // The number of the room above this one (-1 or 255 if none)
    Ceiling   : bit8;       // Absolute height of ceiling (multiply by 256 for world coordinates)
  end;

  tr1_room_staticmesh = packed record
    x          : bitu32; // absolute X position in world coordinates
    y          : bitu32; // absolute Y position in world coordinates
    z          : bitu32; // absolute Z position in world coordinates
    Rotation   : bitu16; // high two bits (0xC000) indicate steps of
                         // 90 degrees (e.g. (Rotation >> 14) * 90)
    Intensity1 : bitu16; // Constant lighting; -1 means use mesh lighting
    ObjectID   : bitu16; // which StaticMesh item to draw
  end;

  tr2_room_staticmesh = record
    x          : bitu32; // absolute X position in world coordinates
    y          : bitu32; // absolute Y position in world coordinates
    z          : bitu32; // absolute Z position in world coordinates
    Rotation   : bitu16; // high two bits (0xC000) indicate steps of
                         // 90 degrees (e.g. (Rotation >> 14) * 90)
    Intensity1 : bitu16; // Constant lighting; -1 means use mesh lighting
    Intensity2 : bitu16; // Like Intensity 1, and almost always the same value [absent from TR1 data files]
    ObjectID   : bitu16; // which StaticMesh item to draw
  end;

  {*
   * MeshTree structure
   *
   * MeshTree[] is actually groups of four bit32s. The first one is a
   * "flags" word;
   *    bit 1 (0x0002) indicates "put the parent mesh on the mesh stack";
   *    bit 0 (0x0001) indicates "take the top mesh off of the mesh stack and use as the parent mesh"
   * when set, otherwise "use the previous mesh are the parent mesh".
   * When both are present, the bit-0 operation is always done before the bit-1 operation; in effect, read the stack but do not change it.
   * The next three bit32s are X, Y, Z offsets of the mesh's origin from the parent mesh's origin.
   *}
  tr1_meshtree = record     { 4 bytes }
    flags  : bit32;
    x,y,z  : bit32;
  end;
  ptr1_meshtree = ^tr1_meshtree;
  tr1_meshtree_raw = bitu32;

  {*  Moveable.
 * Moveable structure. This defines a list of contiguous meshes that
 * comprise one object.
 * This structure also points to the hierarchy and offsets of the meshes
 * (MeshTree), and also to the animations used (Animation); these will be
 * described in detail below. If the Animation index is -1, that means that
 * the entity's animations are all generated by the engine; an example is
 * Lara's ponytail. Some movables are really stationary, such as locks and
 * the sky, and some are not rendered, such as "look at me" points to aim
 * the camera at.
 * }
  tr1_moveable = packed record { 18 bytes }
    object_id       : bitu32;  { Item Identifier (matched in Items[]) }
    num_meshes      : bitu16;  { number of meshes in this object }
    starting_mesh   : bitu16;  { stating mesh (offset into MeshPointers[]) }
    mesh_tree_index : bitu32;  { offset into MeshTree[] }
    frame_offset    : bitu32;  { byte offset into Frames[] (divide by 2 for Frames[i]) }
    animation_index : bitu16;  { offset into Animations[] }
  end;

 {*
 * StaticMesh structure. This defines meshes that don't move (e.g. skeletons
 * lying on the floor, spiderwebs, trees, statues, etc.)
 * StaticMeshes have two bounding boxes; it is not clear why they have more than
 * one. One could be the visibililty box, and one could be the collisional
 * box, for instance; the former being used for visibility testing, and the
 * latter for collision testing.
 * }

  tr1_staticmesh = packed record { 32 bytes }
    object_id       : bitu32;  { Object Identifier (matched in Items[]) }
    mesh            : bitu16;  { mesh (offset into MeshPointers[]) }
    visibility_box  : array[0..1] of tr1_vertex; // First index is which one; second index is opposite corners
    collision_box   : array[0..1] of tr1_vertex;
    flags           : bitu16;  { Meaning uncertain; it is usually 2, and is 3 for objects Lara can travel through, }
                               { like TR2's skeletons and underwater vegetation }
  end;
  tr2_staticmesh = tr1_staticmesh;


{*
 * Object-texture vertex structure. It specifies a vertex location in textile coordinates.
 * The Xpixel and Ypixel are the actual coordinates of the vertex's pixel.
 * The Xcoordinate and Ycoordinate values depend on where the other vertices
 * are in the object texture. And if the object texture is used to specify
 * a triangle, then the fourth vertex's values will all be zero.
 *}
  tr1_object_texture_vert = packed record // 4 bytes
    Xcoordinate : bitu8; // 1 if Xpixel is the low value, 255 if Xpixel is the high value in the object texture
    Xpixel      : bitu8;
    Ycoordinate : bitu8; // 1 if Ypixel is the low value, 255 if Ypixel is the high value in the object texture
    Ypixel      : bitu8;
  end;
  tr2_object_texture_vert = tr1_object_texture_vert;

{*
 * Object texture structure.
 * These, thee contents of ObjectTextures[], are used for specifying texture
 * mapping for the world geometry and for mesh objects.
 *}
  tr1_object_texture = packed record // 20 bytes
    Attribute : bitu16;  // 0 means that a texture is all-opaque, and that transparency
                         // information is ignored.
                         // 1 means that transparency information is used. In 8-bit colour,
                         // index 0 is the transparent colour, while in 16-bit colour, the
                         // top bit (0x8000) is the alpha channel (1 = opaque, 0 = transparent).
                         // 2 (only in TR3) means that the opacity (alpha) is equal to the intensity;
                         // the brighter the colour, the more opaque it is. The intensity is probably calculated
                         // as the maximum of the individual color values.
    Tile      : bitu16 ; // index into textile list
    Vertices  : array [0..3] of tr2_object_texture_vert; // the four corners of the texture
  end;
  tr2_object_texture = tr1_object_texture;

  tr1_zone = packed record
    ground_zone1     : bitu16;
    ground_zone2     : bitu16;
    fly_zone         : bitu16;
    ground_zone1_alt : bitu16;
    ground_zone2_alt : bitu16;
    fly_zone_alt     : bitu16;
  end;

  {* Item. }
  { 24 bytes [TR1: 22 bytes] }

  tr1_item = packed record
    object_id  : bit16;       { Object Identifier (matched in Moveables[], or SpriteSequences[], as appropriate) }
    room       : bit16;       { which room contains this item }
    x,y,z      : bit32;       { world coords }
    angle      : bit16;       { ((0xc000 >> 14) * 90) degrees }
    intensity1 : bit16;       { (constant lighting; -1 means use mesh lighting) }
    flags      : bitu16;      { 0x0100 indicates "initially invisible", 0x3e00 is Activation Mask }
                              { 0x3e00 indicates "open" or "activated";  these can be XORed with }
                              { related FloorData::FDlist fields (e.g. for switches) }
  end;

  tr_colour3 = packed record
    r : bitu8;  { the red component. }
    g : bitu8;  { the green component. }
    b : bitu8;  { the blue component. }
  end;

  {*
 * Sound-sample details (SoundDetails)
 *}
  tr1_sound_details = packed record     // 8 bytes
    Sample   : bit16; // (index into SampleIndices)
    Volume   : bit16;
    Unknown1 : bit16; // sound range? (distance at which this sound can be heard?)
    Unknown2 : bit16; // Bits 8-15: priority?, Bits 2-7: number of sound
                           // samples in this group, Bits 0-1: channel number?
  end;


  bit16array = array [word] of bit16;
  pbit16array = ^bit16array;


  tr1_mesh = record
    centre         : tr1_vertex;
    collision_size : bit32;
    num_vertices   : bit16;
    vertices       : ptr1_vertexarray;
    num_normals    : bit16;
    normals        : ptr1_vertexarray;
    num_lights     : bit16;
    lights         : pbit16array;

    num_textured_rectangles : bit16;
    textured_rectangles     : ptr1_face4array;

    num_textured_triangles  : bit16;
    textured_triangles      : ptr1_face3array; // ^tr4_face3_t;

    num_coloured_rectangles : bit16;
    coloured_rectangles     : ptr1_face4array;

    num_coloured_triangles  : bit16;
    coloured_triangles      : ptr1_face3array;

    _selfdata : array of byte; // used only when extracting data from common mesh data
  end;
  ptr1_mesh = ^tr1_mesh;


const
  TR1_OBJID_BRAID = 189;

function ReadFromData(const data: array of byte; offset: Integer; var m: tr1_mesh): Integer;
// copies all the data to the __selfdata
// if __selfdata is not empty, then does nothing
procedure MakeMeshStandAlone(var m: tr1_mesh);

function CalcMeshSize(const m: tr1_mesh): integer;
function WriteMeshToData(const m: tr1_mesh; var data: array of byte; offset: Integer): Integer;

implementation

function ReadFromData(const data: array of byte; offset: Integer; var m: tr1_mesh): Integer;
var
  i : integer;
  n : integer;
begin
  SetLength(m._selfdata,0);

  i:=offset;
  m.centre:=ptr1_vertex(@data[i])^;
  inc(i, sizeof(tr1_vertex));

  m.collision_size:=pbit32(@data[i])^;
  inc(i, sizeof(bit32));

  m.num_vertices:=pbit16(@data[i])^;
  inc(i, sizeof(bit16));

  m.vertices:=@data[i]; // do not copy... now
  inc(i, m.num_vertices*sizeof(tr1_vertex));

  n:=pbit16(@data[i])^;
  inc(i, sizeof(bit16));
  m.normals:=@data[i];
  m.lights:=@data[i];
  if n>0 then begin
    m.num_normals:=n;
    m.num_lights:=0; // no lights, only normals
    inc(i, n*sizeof(tr1_vertex));
  end else begin
    n:=-n;
    m.num_normals:=0;
    m.num_lights:=n;
    inc(i, n*sizeof(bit16));
  end;

  m.num_textured_rectangles:=pbit16(@data[i])^;
  inc(i, sizeof(bit16));
  m.textured_rectangles:=@data[i];
  inc(i, m.num_textured_rectangles*sizeof(tr1_face4));

  m.num_textured_triangles:=pbit16(@data[i])^;
  inc(i, sizeof(bit16));
  m.textured_triangles:=@data[i];
  inc(i, m.num_textured_triangles*sizeof(tr1_face3));

  m.num_coloured_rectangles:=pbit16(@data[i])^;
  inc(i, sizeof(bit16));
  m.coloured_rectangles:=@data[i];
  inc(i, m.num_coloured_rectangles*sizeof(tr1_face4));

  m.num_coloured_triangles:=pbit16(@data[i])^;
  inc(i, sizeof(bit16));
  m.coloured_triangles:=@data[i];
  inc(i, m.num_coloured_triangles*sizeof(tr1_face3));

  Result:=i-offset;
end;

function CalcMeshSize(const m: tr1_mesh): integer;
begin
  Result:=sizeof(tr1_vertex)
      +sizeof(bit32)

      +sizeof(bit16)
      +m.num_vertices*sizeof(tr1_vertex)

      +sizeof(bit16)
      +m.num_normals*sizeof(tr1_vertex)
      +m.num_lights*sizeof(UInt16)

      +sizeof(bit16)
      +m.num_textured_rectangles*sizeof(tr1_face4)
      +sizeof(bit16)
      +m.num_textured_triangles*sizeof(tr1_face3)
      +sizeof(bit16)
      +m.num_coloured_rectangles*sizeof(tr1_face4)
      +sizeof(bit16)
      +m.num_coloured_triangles*sizeof(tr1_face3)
    ;
end;

procedure MakeMeshStandalone(var m: tr1_mesh);
var
  sz: integer;
begin
  if length(m._selfdata)<>0 then exit; // no.. it is already stand-alone
  sz:=CalcMeshSize(m);

  SetLength(m._selfdata, sz);
  WriteMeshToData(m, m._selfdata, 0);
end;

function WriteMeshToData(const m: tr1_mesh; var data: array of byte; offset: Integer): Integer;
var
  i : integer;
  n : integer;
  sz: integer;
begin
  i:=offset;
  ptr1_vertex(@data[i])^:=m.centre;
  inc(i, sizeof(tr1_vertex));

  pbit32(@data[i])^:=m.collision_size;
  inc(i, sizeof(bit32));

  pbit16(@data[i])^:=m.num_vertices;
  inc(i, sizeof(bit16));

  sz:=m.num_vertices*sizeof(tr1_vertex);
  if sz>0 then begin
    move(m.vertices[0], data[i], sz);
    inc(i, sz);
  end;

  if m.num_normals>0 then begin
    pbit16(@data[i])^:=m.num_normals;
    inc(i, sizeof(bit16));

    sz:=sizeof(tr1_vertex)*m.num_normals;
    if sz>0 then begin
      Move( m.normals[0], data[i], sz);
      inc(i, sz);
    end;
  end else begin
    pbit16(@data[i])^:= -m.num_lights;
    inc(i, sizeof(bit16));

    sz:=sizeof(bit16)*m.num_lights;
    if sz>0 then begin
      Move( m.lights[0], data[i], sz);
      inc(i, sz);
    end;
  end;


  pbit16(@data[i])^:=m.num_textured_rectangles;
  inc(i, sizeof(bit16));
  sz:=m.num_textured_rectangles*sizeof(tr1_face4);
  if sz>0 then begin
    Move(m.textured_rectangles[0], data[i], sz);
    inc(i, sz);
  end;

  pbit16(@data[i])^:=m.num_textured_triangles;
  inc(i, sizeof(bit16));
  sz:=m.num_textured_triangles*sizeof(tr1_face3);
  if sz>0 then begin
    Move(m.textured_triangles[0], data[i], sz);
    inc(i, sz);
  end;

  pbit16(@data[i])^:=m.num_coloured_rectangles;
  inc(i, sizeof(bit16));
  sz:=m.num_coloured_rectangles*sizeof(tr1_face4);
  if sz>0 then begin
    Move(m.coloured_rectangles[0], data[i], sz);
    inc(i, sz);
  end;

  pbit16(@data[i])^:=m.num_coloured_triangles;
  inc(i, sizeof(bit16));
  sz:=m.num_coloured_triangles*sizeof(tr1_face3);
  if sz>0 then begin
    Move(m.coloured_triangles[0], data[i], sz);
    inc(i, sz);
  end;

  Result:=i-offset;
end;

end.
