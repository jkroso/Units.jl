# Units.jl

This unit library is designed for ultimate usability. Everything should just work as you expect.

## Installation

```julia
pkg> add https://github.com/jkroso/Kip.jl
julia> using Kip
julia> @use "github.com/jkroso/Units.jl" m s hr Speed Length Time;
julia> Length/Time == Speed
true
julia> m/s <: Speed
true
julia> 1m/s isa Speed
true
julia> convert(km/hr, 1m/s)
3.6km/hr
```
