module Perlin

  open Microsoft.Xna.Framework
  open Microsoft.Xna.Framework.Graphics
  open Microsoft.Xna.Framework.Input
  open Microsoft.Xna.Framework.Content

  let noise output_size num_octaves seed = 
    let r = match seed with | Some s -> System.Random(s) | _ -> System.Random()
    let base_size = 64
    let noise = 
      [| for i = 0 to base_size do
          yield [| for j = 0 to base_size do yield float32(100.0 * r.NextDouble()) |] 
      |]
    let lerp x1 x2 a = MathHelper.Lerp(x1,x2,a)
    let bilinear_filter (p:Vector2) =
      let i,j = (p.X |> int) % base_size, (p.Y |> int) % base_size
      let i',j' = (i+1)%base_size, (j+1)%base_size
      let ax,ay = p.X - (i |> float32), p.Y - (j |> float32)
      lerp
        (lerp noise.[i].[j] noise.[i'].[j] ax)
        (lerp noise.[i].[j'] noise.[i'].[j'] ax)
        ay
    let zero = 
      [| for i = 0 to output_size do
          yield [| for j = 0 to output_size do yield 0.0f |] 
      |]
    let add_octave (prev:float32[][]) intensity_factor (sampling_scale:float32) =
      [| for i = 0 to output_size do
          yield [| for j = 0 to output_size do 
                     yield prev.[i].[j] * intensity_factor + 
                            bilinear_filter 
                              (Vector2(float32 i / float32 output_size,float32 j / float32 output_size) * 
                                sampling_scale) |] 
      |]
    let mutable result = zero
    let mutable i = 1.0f
    let mutable s = 1.0f
    for o = 1 to num_octaves do
      result <- add_octave result i s
      i <- i * 0.5f
      s <- s * 2.0f
    result      
