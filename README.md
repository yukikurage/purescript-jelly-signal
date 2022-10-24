# Static Signal

The `Signal a` in `purescript-signal` can be thought of as a combination of `Event a` and the initial value `a`.

```
Signal a = (Event a, a) = ([Time, a], a)
```

This library is based on the idea of delaying initialization by making `a` an `Effect a`.

```
Signal a = (Event a, Effect a) = ([Time, a], Effect a)
```

This could make many Signal-related operations pure.
