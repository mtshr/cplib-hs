module Math.Number
  ( floorCongruentTo,
    ceilCongruentTo,
  )
where

-- | Computes a maximum integer @x@ equal to or smaller than @n@ that is congruent to @x = r mod m@.
--
-- >>> floorCongruentTo (-8) 2 3
-- -10
floorCongruentTo ::
  (Integral a) =>
  -- | An upper bound
  a ->
  -- | A remainder
  a ->
  -- | A modulus
  a ->
  a
floorCongruentTo n r m
  | m <= 0 = error "Modulus must be positive."
  | x >= y = n - (x - y)
  | otherwise = n - x - (m - y)
  where
    x = mod n m
    y = mod r m

-- | Computes a minimum integer @x@ equal to or greater than @n@ that is congruent to @x = r mod m@.
--
-- >>> ceilCongruentTo (-8) 2 3
-- -7
ceilCongruentTo ::
  (Integral a) =>
  -- | A lower bound
  a ->
  -- | A remainder
  a ->
  -- | A modulus
  a ->
  a
ceilCongruentTo n r m
  | m <= 0 = error "Modulus must be positive."
  | x <= y = n + (y - x)
  | otherwise = n + (m - x) + y
  where
    x = mod n m
    y = mod r m
