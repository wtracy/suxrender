data Point a = Point a a a
data Vector a = Vector a a a

distance :: Floating a => Point (a) -> Point (a) -> a
distance (Point x1 y1 z1) (Point x2 y2 z2) =
  (sqrt dx * dx + dy * dy + dz * dz)
  where 
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2

unit :: Floating a => Vector(a) -> Vector(a)
unit (Vector x y z) =
  (Vector (x/l) (y/l) (z/l))
  where l = sqrt x * x + y * y + z * z



class Volume v where
  contains :: Floating a => Ord a => v a -> Point a -> Bool
  intersects :: Floating a => Ord a => v a -> v a -> Bool
  intersects x y = intersects y x

data Sphere a = Sphere (Point a) a


data Ray a = Ray (Point a) (Vector a)

instance Volume Ray where
  contains (Ray (Point sx sy sz) (Vector vx vy vz)) (Point tx ty tz) = 
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
  intersects (Sphere aCenter aRadius) (Sphere bCenter bRadius) = 
    (distance aCenter bCenter) <= (aRadius + bRadius)
  --intersects  (Ray start vector) (Sphere center radius)= False

main = putStrLn ""
