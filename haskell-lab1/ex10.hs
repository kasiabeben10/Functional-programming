--ex10

roots :: (Double, Double, Double) -> (Double, Double)
roots (a,b,c) = let d = sqrt (b*b - 4*a*c)
		    e = 2 * a
		in ( (-b - d) / e, (-b + d) / e )

unitVec2D :: (Double, Double) -> (Double, Double) -- wersor 2d
unitVec2D (a, b) = 
  let d = sqrt( a^2 + b^2 )
  in ( a / d, b / d)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double) -- wersor 3d
unitVec3D (a, b, c) =
  let d = sqrt( a^2 + b^2 + c^2 )
  in ( a / d, b / d, c / d )
