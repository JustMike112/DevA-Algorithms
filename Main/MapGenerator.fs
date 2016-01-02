module MapGenerator

type Ground = Tile.Ground
type Building = Tile.Building
type Road = Tile.Road

type Map = 
  {
    Ground    : Ground[][]
    Trees     : bool[][]
    Buildings : Option<Building>[][]
    Roads     : Set<(int*int)*(int*int)>
  }

let generate_map map_width map_height = 
  let random = new System.Random()
  let ground_tiles = 
    let nature_density = Perlin.noise 256 4 (Some 7)
    [|
      for i = 0 to map_width do
        yield 
          [| for j = 0 to map_height do
                yield
                  if nature_density.[i].[j] < 40.0f then Tile.Water
                  elif nature_density.[i].[j] < 45.0f then Tile.Sand
                  elif nature_density.[i].[j] > 70.0f then Tile.Grass
                  else Tile.Cement |]
    |]

  let tree_tiles =
    let tree_random = System.Random()
    [| for i = 0 to map_width do yield [| for j = 0 to map_height do yield ground_tiles.[i].[j] = Tile.Grass && tree_random.NextDouble() > 0.5 |]|]

  let abitation_density = Perlin.noise 256 4 (Some 13)
  let building_tiles = 
    [|
      for i = 0 to map_width do
        yield 
          [| for j = 0 to map_height do
                yield
                  if ground_tiles.[i].[j] = Tile.Cement then
                    let temple_density = int((100.0f - abitation_density.[i].[j]) * 0.5f) |> max 4 |> min 32
                    if i % temple_density = 0 && j % temple_density = 0 then
                      if (i * i - j) % 3 = 0 then
                        Some Tile.Temple1
                      elif (i * i - j) % 3 = 1 then
                        Some Tile.Temple2
                      else 
                        Some Tile.Temple3
                    else
                      if abitation_density.[i].[j] < 70.0f then
                        if (i * i - j) % 3 = 0 then
                          Some Tile.House1
                        elif (i * i - j) % 3 = 1 then
                          Some Tile.House2
                        else
                          Some Tile.House3
                      else
                        if (i * i - j) % 3 = 0 then
                          Some Tile.SmallCondo1
                        elif (i * i - j) % 3 = 1 then
                          Some Tile.SmallCondo2
                        else
                          Some Tile.SmallCondo3
                  else
                    None |]
    |]

  let road_tiles = 
    let road_random = System.Random()
    let nodes = 
      [
        for i = 0 to map_width do 
          for j = 0 to map_height do 
            if building_tiles.[i].[j] <> None then
              yield i,j
      ] |> Set.ofList
    let edges = 
      [
        for i = 0 to map_width do 
          for j = 0 to map_height do 
            yield (i,j),
              [if nodes |> Set.contains (i,j) && nodes |> Set.contains (i+1,j) then
                 yield (i+1,j)
               if nodes |> Set.contains (i,j) && nodes |> Set.contains (i,j+1) then
                 yield (i,j+1)
               if nodes |> Set.contains (i,j) && nodes |> Set.contains (i-1,j) then
                 yield (i-1,j)
               if nodes |> Set.contains (i,j) && nodes |> Set.contains (i,j-1) then
                 yield (i,j-1)]
      ] |> Map.ofList
    let mutable visited = Set.singleton (nodes.MinimumElement)
    let mutable frontier = Set.singleton (nodes.MinimumElement)
    let mutable mst = Set.empty
    while frontier |> Set.isEmpty |> not do
      let old_frontier = frontier
      frontier <- Set.empty
      for node_f in old_frontier |> Set.toList |> List.sortBy (fun _ -> random.Next()) do
        let node_fs = edges.[node_f] |> List.sortBy (fun _ -> random.Next())
        for node_f' in node_fs do
          if visited |> Set.contains node_f' |> not then
            visited <- visited |> Set.add node_f'
            frontier <- frontier |> Set.add node_f'
            mst <- mst |> Set.add (node_f,node_f')
    [
      for n1 in nodes do
        for n2 in edges.[n1] do
          if mst |> Set.contains (n1,n2) || 
            float32(random.NextDouble()) * 400.0f < abitation_density.[fst n1].[snd n1] then
            yield n1,n2
            yield n2,n1
    ] |> Set.ofList

  {
    Ground    = ground_tiles
    Trees     = tree_tiles
    Buildings = building_tiles
    Roads     = road_tiles
  }
