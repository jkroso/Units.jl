# Units

Inspired by [Unitful.jl](https://github.com/ajkeller34/Unitful.jl) I've been experimenting with ways of supporting units in Julia.

With Unitful when you define a new dimension type your quantities are not actually instances of that type. So it's hard to extend the behavior of those objects. It's doable I'm sure but not obvious. With this package you define new Units by simply subtyping `Dimension`. So if you want to do something like change the way this type is displayed you just do the obvious thing and define `show(::IO, ::MyDimension)`. See [Money.jl](./Money.jl) for an example. Creating abstract or concrete derived types is also equally obvious `km/hr <: Length/Time` thanks to `UnionAll` types.
