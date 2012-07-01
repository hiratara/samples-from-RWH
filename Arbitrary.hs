import Test.QuickCheck

data Ternary = Yes | No | Unknown deriving (Eq, Show)

-- instance Arbitrary Ternary where
--     arbitrary = elements [Yes, No, Unknown]

instance Arbitrary Ternary where
    arbitrary = do
      n <- choose (0, 2) :: Gen Int
      return $ case n of
                 0 -> Yes
                 1 -> No
                 _ -> Unknown

