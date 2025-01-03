--ex9

roots :: (Double, Double, Double) -> (Double, Double)
roots (a,b,c) = ((-b - d) / e, (-b - d) / e )
	where d = sqrt (b * b - 4 * a * c)
	      e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x,y) = (x / d, y / d)
	where d = sqrt (x^2 + y^2)