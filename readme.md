# hascon

## Explanation

Refer to constructors of datatypes using Symbols, 
and reify them as strings. If the Symbol does not
refer to an actual constructor, you get a compile-time
error.

## Example

```Haskell
{-# language
    DataKinds
  , TemplateHaskell
  , TypeApplications
  , TypeFamilies
  , UndecidableInstances
#-}

data Foo = Foo1 | Foo2
mkHasCon ''Foo

-- compiles just fine
foo1 :: String
foo1 = con @Foo @"Foo1"

-- compiles with error:
--
-- Constructor "Foo3" does not exist for Type Foo
foo3 :: String
foo3 = con @Foo @"Foo3"
```
