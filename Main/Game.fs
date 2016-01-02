module VirtualCity
  (*
  TODO:
  cars in the city
  *)


  open Microsoft.Xna.Framework
  open Microsoft.Xna.Framework.Graphics
  open Microsoft.Xna.Framework.Input
  open Microsoft.Xna.Framework.Content

  type Microsoft.Xna.Framework.Game with member this.Self = this
  type Microsoft.Xna.Framework.GameTime with member this.DT = this.ElapsedGameTime.TotalSeconds |> float32
  type Ground = Tile.Ground
  type Building = Tile.Building
  type Transportation = Tile.Transportation
  type Road = Tile.Road

  type Simulation(assignment:Choice<System.Func<Vector2, seq<Vector2>, seq<Vector2>>,
                                    System.Func<seq<Vector2>, seq<Vector2 * float32>, seq<seq<Vector2>>>,
                                    System.Func<Vector2, Vector2, seq<Vector2 * Vector2>, seq<Vector2 * Vector2>>,
                                    System.Func<Vector2, seq<Vector2>, seq<Vector2 * Vector2>, seq<seq<Vector2 * Vector2>>>>,
                  fullscreen) =
    inherit Game()


    member val graphics = 
      let g = new GraphicsDeviceManager(base.Self)
      let screen = System.Windows.Forms.Screen.PrimaryScreen.Bounds
      g.PreferredBackBufferWidth <- screen.Width - 128
      g.PreferredBackBufferHeight <- screen.Height - 128
      g.IsFullScreen <- fullscreen
      g with get
    [<DefaultValue>]
    val mutable spriteBatch : SpriteBatch
    [<DefaultValue>]
    val mutable tileSet : Texture2D
    [<DefaultValue>]
    val mutable roadTileSet : Texture2D
    [<DefaultValue>]
    val mutable crosshair : Texture2D
    [<DefaultValue>]
    val mutable consolas : SpriteFont
    [<DefaultValue>]
    val mutable circle : Texture2D

    let map_width,map_height = 96,48
    let random = System.Random()
    let mutable planes = [] : List<Vector2>

    let mutable camera_x, camera_y, zoom = 0.0f,0.0f,10.0f
    let clamp (a:float32) b c = MathHelper.Clamp(a,b,c)

    let map = MapGenerator.generate_map map_width map_height

    let big_markers, small_markers, (numberedMarkers:List<(int*Vector2)*Color>), (numberedFloatMarkers:List<(float32*Vector2)*Color>), circles = 
      let houses : List<Vector2> = 
        [
          for i = 0 to map_width do
            for j = 0 to map_height do
              match map.Buildings.[i].[j] with
              | Some Building.House1 | Some Building.House2 | Some Building.House3
              | Some Building.SmallCondo1 | Some Building.SmallCondo2 | Some Building.SmallCondo3 -> 
                yield Vector2(float32 i,float32 j)
              | _ -> ()
        ]
      let specialBuildings : List<Vector2> =
        [
          for i = 0 to map_width do
            for j = 0 to map_height do
              match map.Buildings.[i].[j] with
              | Some Building.Temple1 | Some Building.Temple2 | Some Building.Temple3 -> 
                yield Vector2(float32 i,float32 j)
              | _ -> ()
        ]
      let roads : List<Vector2 * Vector2> = 
        [
          for (i_a,j_a),(i_b,j_b) in map.Roads do
            yield Vector2(float32 i_a,float32 j_a), Vector2(float32 i_b,float32 j_b)
        ]
      match assignment with
      | Choice1Of4 f ->
        let startingHouse = houses.[random.Next(houses.Length)]
        let mk_pair x y = x,y
        [startingHouse, Color.Red], [], 
        [ for x in f.Invoke(startingHouse, specialBuildings) |> Seq.mapi mk_pair |> List.ofSeq do 
          yield x, Color.White ], [], [startingHouse, 2.0f]
      | Choice2Of4 f ->
        let housesAndDistances : List<Vector2 * float32> = 
          [
            for h in houses do
              let sqr x = x * x
              yield h, sqr(float32(random.NextDouble())) * 10.0f + 5.0f
          ] |> List.sortBy (fun _ -> random.Next()) |> List.take 5
        let mk_pair x y = x,y
        let res = f.Invoke(specialBuildings, housesAndDistances) |> Seq.map (Seq.toList) |> Seq.toList
        [ for h,_ in housesAndDistances -> h, Color.Red ] @ (res |> List.concat |> List.ofSeq |> List.map (fun x -> x,Color.Blue)), 
          [], [], [], housesAndDistances
      | Choice3Of4 f ->
        let startingHouse = houses.[random.Next(houses.Length)]
        let destinationSpecialBuilding = specialBuildings.[random.Next(specialBuildings.Length)]
        let mk_pair x y = x,y
        let res = f.Invoke(startingHouse, destinationSpecialBuilding, roads) |> Seq.toList |> List.mapi mk_pair
        [startingHouse, Color.Red; destinationSpecialBuilding, Color.Blue], 
          [ 
            for i,(p,p') in res do
            let d = p'-p
            for a = 0 to 3 do
            yield p + d * (float32 a / 4.0f),Color.Lerp(Color.Red, Color.Blue, float32 i / float32(res.Length-1))
          ], [], [], [startingHouse, 2.0f]
      | Choice4Of4 f ->
        let startingHouse = houses.[random.Next(houses.Length)]
        let mk_pair x y = x,y
        let specialBuildings = specialBuildings |> List.sortBy (fun _ -> random.Next()) |> List.take 5
        let res = f.Invoke(startingHouse, specialBuildings, roads) |> Seq.map Seq.toList |> Seq.toList
        [startingHouse, Color.Red] @ (specialBuildings |> List.map (fun x -> x,Color.Blue)), 
          [
            for l in res do
            for i,(p,p') in l |> List.mapi mk_pair do
            let d = p'-p
            for a = 0 to 3 do
            yield p + d * (float32 a / 4.0f),Color.Lerp(Color.Red, Color.Blue, float32 i / float32(l.Length-1))
          ], [], [], 
          [startingHouse, 2.0f]

    let full_crossings =
      [| for i = 0 to map_width do
           yield [|
             for j = 0 to map_height do
               yield
                  map.Roads |> Set.contains ((i,j),(i+1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i-1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i,j+1)) && 
                  map.Roads |> Set.contains ((i,j),(i,j-1)) |] |]
    let three_crossings =
      [| for i = 0 to map_width do
           yield [|
             for j = 0 to map_height do
               if map.Roads |> Set.contains ((i,j),(i-1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i+1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i,j+1)) then
                 yield Some Road.Three3
               elif map.Roads |> Set.contains ((i,j),(i-1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i+1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i,j-1)) then
                 yield Some Road.Three2
               elif map.Roads |> Set.contains ((i,j),(i-1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i,j+1)) && 
                  map.Roads |> Set.contains ((i,j),(i,j-1)) then
                 yield Some Road.Three4
               elif map.Roads |> Set.contains ((i,j),(i+1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i,j+1)) && 
                  map.Roads |> Set.contains ((i,j),(i,j-1)) then
                 yield Some Road.Three1
               else
                 yield None |] |]
    let curves =
      [| for i = 0 to map_width do
           yield [|
             for j = 0 to map_height do
               if map.Roads |> Set.contains ((i,j),(i+1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i,j+1)) then
                 yield Some Road.Curve3
               elif map.Roads |> Set.contains ((i,j),(i-1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i,j+1)) then
                 yield Some Road.Curve4
               elif map.Roads |> Set.contains ((i,j),(i-1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i,j-1)) then
                 yield Some Road.Curve2
               elif map.Roads |> Set.contains ((i,j),(i+1,j)) && 
                  map.Roads |> Set.contains ((i,j),(i,j-1)) then
                 yield Some Road.Curve1
               else
                 yield None |] |]

    let short_connectors =
      [| for i = 0 to map_width do
           yield [|
             for j = 0 to map_height do
               if map.Roads |> Set.contains ((i,j),(i+1,j)) then
                 yield Some Road.Two3
               elif map.Roads |> Set.contains ((i,j),(i,j+1)) then
                 yield Some Road.Two4
               else
                 yield None |] |]
      
    override this.Initialize() =
      base.Content.RootDirectory <- "Content"
      this.spriteBatch <- new SpriteBatch(this.GraphicsDevice)
      this.tileSet <- this.Content.Load "tileset.png"
      this.roadTileSet <- this.Content.Load "roads.jpg"
      this.crosshair <- this.Content.Load "crosshair.png"
      this.consolas <- this.Content.Load "numberingFont"
      this.circle <- this.Content.Load "circle.png"

    override this.Update gt =
      let ks = Keyboard.GetState()
      let camera_speed = MathHelper.SmoothStep(300.0f, 10.0f, (zoom - 1.0f) / 4.0f)
      if ks.[Keys.Escape] = KeyState.Down then
        do this.Exit()
      if ks.[Keys.A] = KeyState.Down then
        camera_x <- camera_x - camera_speed * gt.DT * zoom
      if ks.[Keys.D] = KeyState.Down then
        camera_x <- camera_x + camera_speed * gt.DT * zoom
      if ks.[Keys.W] = KeyState.Down then
        camera_y <- camera_y - camera_speed * gt.DT * zoom
      if ks.[Keys.S] = KeyState.Down then
        camera_y <- camera_y + camera_speed * gt.DT * zoom
      if ks.[Keys.Z] = KeyState.Down then
        zoom <- zoom + 10.0f * gt.DT
      if ks.[Keys.X] = KeyState.Down then
        zoom <- zoom - 10.0f * gt.DT
      zoom <- clamp zoom 1.0f 5.0f
      let screen_width,screen_height = float32 this.GraphicsDevice.Viewport.Width, float32 this.GraphicsDevice.Viewport.Height
      camera_x <- clamp camera_x 0.0f ((float32 map_width * 16.0f - screen_width / 4.0f + 16.0f) * zoom)
      camera_y <- clamp camera_y 0.0f ((float32 map_height * 16.0f - screen_height / 4.0f + 16.0f) * zoom)
      base.Update(gt)

    override this.Draw gt =
      do this.GraphicsDevice.Clear(Color.CornflowerBlue)

      let sb = this.spriteBatch
      let tileset = this.tileSet
      let roadTileSet = this.roadTileSet
      do sb.Begin(transformMatrix = System.Nullable(Matrix.CreateTranslation(-camera_x, -camera_y, 0.0f) * Matrix.CreateScale(zoom)))
      for i = 0 to map_width do
        for j = 0 to map_height do
          do Ground.draw(tileset,sb,(i * 16, j * 16, 16, 16),map.Ground.[i].[j])
          if map.Trees.[i].[j] then 
            do Ground.draw(tileset,sb,(i * 16, j * 16, 16, 16),Tile.Tree)
      for i = 0 to map_width do
        for j = 0 to map_height do
          if full_crossings.[i].[j] then
            do Road.draw(roadTileSet,sb,(i * 16 - 2, j * 16 - 2, 4, 4),Road.Four)
          else
            match three_crossings.[i].[j] with
            | Some tile ->
              do Road.draw(roadTileSet,sb,(i * 16 - 2, j * 16 - 2, 4, 4),tile)
            | None -> 
              match curves.[i].[j] with
              | Some tile ->
                do Road.draw(roadTileSet,sb,(i * 16 - 2, j * 16 - 2, 4, 4),tile)
              | None ->
                match short_connectors.[i].[j] with
                | Some tile ->
                  do Road.draw(roadTileSet,sb,(i * 16 - 2, j * 16 - 2, 4, 4),tile)
                | None -> ()
      for ((i,j),(i',j')) in map.Roads do
          if i' = i + 1 then
            do Road.draw(roadTileSet,sb,(i * 16 + 2, j * 16 - 2, 12, 4),Road.Two1)
          elif j' = j + 1 then
            do Road.draw(roadTileSet,sb,(i * 16 - 2, j * 16 + 2, 4, 12),Road.Two2)
      for i = 0 to map_width do
        for j = 0 to map_height do
          match map.Buildings.[i].[j] with
          | Some tile -> 
              do Building.draw(tileset,sb,(i * 16 + 4, j * 16 + 4, 10, 10),tile)
          | None -> ()

      for m,c in big_markers do
        let i,j = int m.X, int m.Y
        for d = 0 to 2 do
          let k = 3
          sb.Draw(this.crosshair, Rectangle(i * 16 - d * k, j * 16 - d * k, 16 + d * 2 * k, 16 + d * 2 * k), c)
      for m,c in small_markers do
        let i,j = int(m.X * 16.0f), int(m.Y * 16.0f)
        sb.Draw(this.crosshair, Rectangle(i - 4, j - 4, 8, 8), c)
      for (n,m),c in numberedMarkers do
        let i,j = int m.X, int m.Y
        for di = -1 to 1 do
          for dj = -1 to 1 do
            let a,b = float32 di, float32 dj
            let a,b = a*0.5f,b*0.5f
            sb.DrawString(this.consolas, string n, 
              Vector2(float32 i * 16.0f + a, float32 j * 16.0f + b), Color.Black, 
              0.0f, Vector2.Zero, MathHelper.SmoothStep(0.5f, 0.3f, (zoom-1.0f)/4.0f), SpriteEffects.None, 0.0f)
        sb.DrawString(this.consolas, string n, 
          Vector2(float32 i * 16.0f, float32 j * 16.0f), c, 
          0.0f, Vector2.Zero, MathHelper.SmoothStep(0.5f, 0.3f, (zoom-1.0f)/4.0f), SpriteEffects.None, 0.0f)
      for (n,m),c in numberedFloatMarkers do
        let i,j = int m.X, int m.Y
        for di = -1 to 1 do
          for dj = -1 to 1 do
            let a,b = float32 di, float32 dj
            let a,b = a*0.5f,b*0.5f
            sb.DrawString(this.consolas, n.ToString("#.0"), 
              Vector2(float32 i * 16.0f + a, float32 j * 16.0f + b), Color.Black, 
              0.0f, Vector2.Zero, MathHelper.SmoothStep(0.5f, 0.3f, (zoom-1.0f)/4.0f), SpriteEffects.None, 0.0f)
        sb.DrawString(this.consolas, n.ToString("#.0"), 
          Vector2(float32 i * 16.0f, float32 j * 16.0f), c, 
          0.0f, Vector2.Zero, MathHelper.SmoothStep(0.5f, 0.3f, (zoom-1.0f)/4.0f), SpriteEffects.None, 0.0f)
      for (p,r) in circles do
        let r = int(r * 16.0f * 2.0f * 1.05f)
        let i,j = int p.X, int p.Y
        sb.Draw(this.circle, Rectangle(i * 16 - r / 2 + 8, j * 16 - r / 2 + 8, r, r), Color.White)
      planes <-
        [
          for p in planes do
            do Transportation.draw(tileset,sb,p,Tile.Plane)
            if p.Y < 600.0f then
              yield p + Vector2.UnitY * gt.DT * 40.0f
          if random.NextDouble() > 0.9999 then
            yield Vector2(float32(random.NextDouble()) * (float32 map_width * 16.0f), -80.0f)
        ]

      do sb.End()

      base.Draw(gt)

  let RunAssignment1 implementation fullscreen = new Simulation(Choice1Of4 implementation, fullscreen)
  let RunAssignment2 implementation fullscreen = new Simulation(Choice2Of4 implementation, fullscreen)
  let RunAssignment3 implementation fullscreen = new Simulation(Choice3Of4 implementation, fullscreen)
  let RunAssignment4 implementation fullscreen = new Simulation(Choice4Of4 implementation, fullscreen)


  let random = System.Random()
  let rec GetInitialValue() =
    let r = random.Next(1, 6)
    if r <= 4 then string r
    else
      if random.NextDouble() <= 0.1 then
        "8==D"
      else
        GetInitialValue()
