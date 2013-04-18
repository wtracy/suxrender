data Point a = Point a a a

distance :: Floating a => Point (a) -> Point (a) -> a
distance (Point x1 y1 z1) (Point x2 y2 z2) =
  (sqrt x1)

class Volume v where
  contains :: Floating a => Ord a => v a -> Point a -> Bool
  intersects :: v a -> v a -> Bool
  intersects x y = intersects y x

data Sphere a = Sphere (Point a) a

data Ray a = Ray (Point a) (Point a)

instance Volume Ray where
  contains (Ray (Point sx sy sz) (Point vx vy vz)) (Point tx ty tz) = 
  {- exists t such that 
    sx + t*vx = tx 
    sy + t*vy = ty
    sz + t*vz = tz
     -}
    ((ty == sy + t * vy)
      && (tz == sz + t * vz))
    where t = (tx - sx) / vx

instance Volume Sphere where
  contains (Sphere center radius) target = 
    distance center target <= radius
  intersects (Sphere a b) (Sphere c d) = False
  --intersects  (Ray start vector) (Sphere center radius)= False

main = putStrLn ""
