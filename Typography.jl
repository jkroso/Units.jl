@use "." Length basefactor @abbreviate Unit
@use "./Imperial.jl" inch

struct Point <: Length
  value::Number
end
@abbreviate pt Point

const POINT_BASEFACTOR = basefactor(inch)/72
basefactor(::Type{Point}) = POINT_BASEFACTOR
Base.show(io::IO, p::pt) = invoke(show, Tuple{IO,Unit}, io, round(p))

struct Pixel <: Length
  value::Number
end
@abbreviate px Pixel

const PIXEL_BASEFACTOR = basefactor(inch)/96
basefactor(::Type{Pixel}) = PIXEL_BASEFACTOR
Base.show(io::IO, p::px) = invoke(show, Tuple{IO,Unit}, io, round(p))
