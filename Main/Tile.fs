module Tile

  open Microsoft.Xna.Framework
  open Microsoft.Xna.Framework.Graphics
  open Microsoft.Xna.Framework.Input
  open Microsoft.Xna.Framework.Content

  type Microsoft.Xna.Framework.Game with member this.Self = this
  type Microsoft.Xna.Framework.GameTime with member this.DT = this.ElapsedGameTime.TotalSeconds |> float32

  type Ground = 
    | Grass
    | Sand
    | Water
    | Tree
    | Cement
    with 
      member tile.Rectangle =
        match tile with
        | Grass         -> Rectangle(215, 152, 240-215, 178-152)
        | Sand          -> Rectangle(116, 286, 134-116, 300-286)
        | Water         -> Rectangle(134, 179, 161-134, 205-179)
        | Tree          -> Rectangle(10, 0, (115-10)/4, 28-0)
        | Cement        -> Rectangle(313, 188, 339-313, 214-188)
      static member draw((tileset:Texture2D),(sb:SpriteBatch),(px,py,sx,sy),(t:Ground)) =
        sb.Draw(tileset, Rectangle(px,py,sx,sy), System.Nullable(t.Rectangle), Color.White)
      static member draw((tileset:Texture2D),(sb:SpriteBatch),(p:Vector2),(t:Ground)) =
        sb.Draw(tileset, p, System.Nullable(t.Rectangle), Color.White)

  type Building = 
    | SmallCondo1
    | SmallCondo2
    | SmallCondo3
    | House1
    | House2
    | House3
    | Temple1
    | Temple2
    | Temple3
    with 
      member tile.Rectangle =
        match tile with
        | SmallCondo1   -> Rectangle(508, 561, 577-508, 656-561)
        | SmallCondo2   -> Rectangle(526, 666, 601-526, 782-666)
        | SmallCondo3   -> Rectangle(601, 676, 708-601, 774-676)
        | House1        -> Rectangle(579, 596, 626-579, 651-596)
        | House2        -> Rectangle(543, 423, 597-543, 482-423)
        | House3        -> Rectangle(488, 432, 541-488, 476-432)
        | Temple1       -> Rectangle(532, 812, 616-531, 892-812)
        | Temple2       -> Rectangle(662, 804, 740-662, 892-804)
        | Temple3       -> Rectangle(809, 376, 892-809, 501-376)
      member tile.Scale =
        match tile with
        | SmallCondo1   -> 1.0f
        | SmallCondo2   -> 1.0f
        | SmallCondo3   -> 1.0f
        | House1        -> 1.0f
        | House2        -> 1.0f
        | House3        -> 1.0f
        | Temple1       -> 1.7f
        | Temple2       -> 1.7f
        | Temple3       -> 1.9f
      member tile.Offset =
        match tile with
        | SmallCondo1   -> 0,0
        | SmallCondo2   -> 0,0
        | SmallCondo3   -> 0,0
        | House1        -> 0,0
        | House2        -> 0,0
        | House3        -> 0,0
        | Temple1       -> 5,8
        | Temple2       -> 5,8
        | Temple3       -> 6,9
      static member draw((tileset:Texture2D),(sb:SpriteBatch),(px,py,sx,sy),(t:Building)) =
        let scale = t.Scale
        let ox,oy = t.Offset
        sb.Draw(tileset, 
          Rectangle(px - ox,
                    py - oy,
                    int(float32 sx * t.Scale),
                    int(float32 sy * t.Scale)), System.Nullable(t.Rectangle), Color.White)
      static member draw((tileset:Texture2D),(sb:SpriteBatch),(p:Vector2),(t:Building)) =
        sb.Draw(tileset, p, System.Nullable(t.Rectangle), Color.White)

  type Transportation = 
    | Plane
    | Car
    with 
      member tile.Rectangle =
        match tile with
        | Plane         -> Rectangle(451, 354, 494-451, 393-354)
        | Car           -> Rectangle(544, 363, 554-544, 369-363)
      static member draw((tileset:Texture2D),(sb:SpriteBatch),(px,py,sx,sy),(t:Transportation)) =
        sb.Draw(tileset, Rectangle(px,py,sx,sy), System.Nullable(t.Rectangle), Color.White)
      static member draw((tileset:Texture2D),(sb:SpriteBatch),(p:Vector2),(t:Transportation)) =
        sb.Draw(tileset, p, System.Nullable(t.Rectangle), Color.White)

  type Road = 
    | Two1
    | Two2
    | Two3
    | Two4
    | Curve1
    | Curve2
    | Curve3
    | Curve4
    | Three1
    | Three2
    | Three3
    | Three4
    | Four
    with 
      member tile.Rectangle =
        match tile with
        | Two1         -> Rectangle(0 * 32 * 8, 0 * 32 * 8, 8 * 32, 8 * 32)
        | Two2         -> Rectangle(3 * 32 * 8, 0 * 32 * 8, 8 * 32, 8 * 32)
        | Two3         -> Rectangle(0 * 32 * 8, 0 * 32 * 8, 2 * 32, 8 * 32)
        | Two4         -> Rectangle(3 * 32 * 8, 0 * 32 * 8, 8 * 32, 2 * 32)
        | Curve1       -> Rectangle(1 * 32 * 8, 0 * 32 * 8, 8 * 32, 8 * 32)
        | Curve2       -> Rectangle(0 * 32 * 8, 1 * 32 * 8, 8 * 32, 8 * 32)
        | Curve3       -> Rectangle(2 * 32 * 8, 1 * 32 * 8, 8 * 32, 8 * 32)
        | Curve4       -> Rectangle(3 * 32 * 8, 1 * 32 * 8, 8 * 32, 8 * 32)
        | Three1       -> Rectangle(0 * 32 * 8, 2 * 32 * 8, 8 * 32, 8 * 32)
        | Three2       -> Rectangle(1 * 32 * 8, 2 * 32 * 8, 8 * 32, 8 * 32)
        | Three3       -> Rectangle(2 * 32 * 8, 2 * 32 * 8, 8 * 32, 8 * 32)
        | Three4       -> Rectangle(3 * 32 * 8, 2 * 32 * 8, 8 * 32, 8 * 32)
        | Four         -> Rectangle(2 * 32 * 8, 0 * 32 * 8, 8 * 32, 8 * 32)
      static member draw((tileset:Texture2D),(sb:SpriteBatch),(px,py,sx,sy),(t:Road)) =
        sb.Draw(tileset, Rectangle(px,py,sx,sy), System.Nullable(t.Rectangle), Color.White)
      static member draw((tileset:Texture2D),(sb:SpriteBatch),(p:Vector2),(t:Road)) =
        sb.Draw(tileset, p, System.Nullable(t.Rectangle), Color.White)
