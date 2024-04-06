module Math.Number
  ( factorize,
    sigma0,
    sigma1,
    moebius,
    totient,
    floorCongruentTo,
    ceilCongruentTo,
  )
where

import Data.Semigroup (Semigroup (..))

multiplicative :: (Integral a, Monoid b) => (a -> Word -> b) -> a -> b
multiplicative f n
  | n <= 0 = error "The integer must be positive."
  | otherwise =
      case takeFactor n 2 0 of
        (m, 0) -> multiplicative' f m 3
        (m, e) -> f 2 e <> multiplicative' f m 3

multiplicative' :: (Integral a, Monoid b) => (a -> Word -> b) -> a -> a -> b
multiplicative' _ 1 _ = mempty
multiplicative' f n p
  | p * p > n = f n 1
  | n `mod` p == 0 =
      let (m, e) = takeFactor (n `quot` p) p 1
       in f p e <> multiplicative' f m (p + 2)
  | otherwise = multiplicative' f n (p + 2)

takeFactor :: (Integral a, Num b) => a -> a -> b -> (a, b)
takeFactor n p e =
  if n `mod` p == 0
    then
      takeFactor (n `quot` p) p (e + 1)
    else (n, e)

-- | Factorize an integer.
--
-- >>> factorize 1
-- []
--
-- >>> factorize 12
-- [(2,2),(3,1)]
--
-- >>> factorize 123456789
-- [(3,2),(3607,1),(3803,1)]
factorize :: (Integral a) => a -> [(a, Word)]
factorize =
  multiplicative
    ( ((: []) .)
        . (,)
    )

-- | The number of divisors function.
-- Computes the number of divisors of the given integer.
--
-- >>> sigma0 1
-- 1
--
-- >>> sigma0 12
-- 6
--
-- >>> sigma0 123456789
-- 12
sigma0 :: (Integral a) => a -> Word
sigma0 =
  multiplicativeProduct (\(_, e) -> e + 1)

-- | The sum of divisors function.
-- Computes the sum of divisors of the given integer.
--
-- >>> sigma1 1
-- 1
--
-- >>> sigma1 12
-- 28
--
-- >>> sigma1 123456789
-- 178422816
sigma1 :: (Integral a) => a -> a
sigma1 =
  multiplicativeProduct (\(p, e) -> let PartialMatrix a b = stimes e PartialMatrix p 1 in a + b)

-- | Represents [[a, b], [0, 1]]
data PartialMatrix a = PartialMatrix a a

instance (Integral a) => Semigroup (PartialMatrix a) where
  (<>) (PartialMatrix a b) (PartialMatrix c d) = PartialMatrix (a * c) (a * d + b)

-- | Möbius function.
--
-- >>> moebius 1
-- 1
--
-- >>> moebius 105
-- -1
--
-- >>> moebius 123456789
-- 0
moebius :: (Integral a) => a -> a
moebius =
  f 1 . factorize
  where
    f m [] = m
    f m ((_, e) : xs)
      | e >= 2 = 0
      | otherwise = f (-m) xs -- e <= 0 is unreachable

-- | Euler's totient function.
--
-- >>> totient 1
-- 1
--
-- >>> totient 123456789
-- 82260072
totient :: (Integral a) => a -> a
totient =
  multiplicativeProduct (\(p, e) -> p ^ (e - 1) * (p - 1))

multiplicativeProduct :: (Integral a, Integral b) => ((a, Word) -> b) -> a -> b
multiplicativeProduct f =
  product
    . fmap
      f
    . factorize

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
