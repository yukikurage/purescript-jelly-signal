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

## As a FRP

This can also be seen as replacing the classic FRP Behaivor with Signal.

At least on the Web, we rarely use "true" continuous values (e.g., y = x^2), but usually only memoize discrete values.

Thus, the replacement of Behavior with Signal adds the benefit of being able to monitor value changes to Behavior.
