# Contravariant Pattern

```
component = do
  stateOut /\ stateIn <- state 0
  let
    modifier = sampleOf stateOut stateIn
  pure $ do
    button [on click (modifier $< (_ + 1))] $ text "Increment"
    button [on click (modifier $< (_ - 1))] $ text "Decrement"
    div' do
      text "Count: "
      textOut $ show <$> stateOut
```

```
cvoidRight :: forall a b f. Contravariant f => a -> f a -> f b
cvoidRight x = cmap (const x)

infixl 4 voidRight as >$

cvoidLeft :: forall a b f. Contravariant f => f a -> a -> f b
cvoidLeft f x = const x >$< f

infixl 4 cvoidLeft as $<
```

```
dividing = fromFunc \a -> [
    send divided1 $ a / 2
  , send divided2 $ a * 2
  ]
```

```
capp :: a -> f b -> f (a -> b) ?

capp a f = cmap (_ $ a) f   -- cmap だけでいける
```
