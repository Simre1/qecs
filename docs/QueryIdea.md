## Try 1

```haskell
query $ want moving <> ignore frozen <> set (position %~ (+ velocity))
```

Unlikely to work nicely

## Try 2

Need HKT components!


```haskell
data Position f = Position (f Double) (f Double)
data Velocity f = Position (f Double) (f Double)

class Component (c :: * -> *) where
    componentPure :: (forall a. f a) -> c f

data QV a = QV

get :: f QV

query $ do
    want @Moving
    ignore @Frozen
    Velocity dx dy <- get
    Position x y <- get
    put $ Position (x + dx) (y + dy)
```

Awkard when modifying values. Would need expression datatypes and query datatypes

## Try 3

```haskell
query $ with @Moving $ without @Frozen $ 
    let Position x y = get @Position
        Velocity dx dy = get @Velocity
    in QueryResult (Position (x + dx, y + dy)) ()
    
    position %~ (+ velocity)
```

Queries are now expressions where we can magically get components

## Try 4

Use standard `cmap (a -> b)` queries ala apecs where the function needs to be quoted.