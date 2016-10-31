class (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)
    (a1, b1) `mappend` (a2, b2) = (a1 `mappend` a2, b1 `mappend` b2)
