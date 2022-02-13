@use "." m Length basefactor @abbreviate Unit
@use "./Imperial.jl" inch

struct Point <: Length
  value::Number
end
@abbreviate pt Point

const POINT_BASEFACTOR = basefactor(inch)/72
basefactor(::Type{Point}) = POINT_BASEFACTOR
Base.show(io::IO, p::pt) = invoke(show, Tuple{IO,Unit}, io, round(p))
