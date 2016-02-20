"""
Convert two `Unit`s to use the same magnitude while tolerating differences
in the number of dimensions

```julia
promote_magnitude(1km², 2mm) == (1e6mm², 2mm)
```
"""
function promote_magnitude end

# map magnitudes to their standard name
const prefix = Dict(1 => :da,
                    2 => :h,
                    3 => :k,
                    6 => :M,
                    9 => :G,
                   12 => :T,
                   15 => :P,
                   18 => :E,
                   21 => :Z,
                   24 => :Y,
                   -1 => :d,
                   -2 => :c,
                   -3 => :m,
                   -6 => :μ,
                   -9 => :n,
                  -12 => :p,
                  -15 => :f,
                  -18 => :a,
                  -21 => :z,
                  -24 => :y)
const exponent = ['¹','²','³','⁴','⁵','⁶','⁷','⁸','⁹']

abstract Unit <: Real
abstract Size{dimensions} <: Unit

typealias Length Size{1}
typealias Area Size{2}
typealias Volume Size{3}

immutable Meter{d,magnitude} <: Size{d}
  value::Real
end

Base.size{d}(::Meter{d}) = (d,)

typealias km  Meter{1, 3}
typealias m   Meter{1, 0}
typealias cm  Meter{1,-2}
typealias mm  Meter{1,-3}
typealias km² Meter{2, 3}
typealias m²  Meter{2, 0}
typealias cm² Meter{2, -2}
typealias mm² Meter{2, -3}
typealias km³ Meter{3, 3}
typealias m³  Meter{3, 0}
typealias litre Meter{3,-1}
typealias cm³ Meter{3, -2}
typealias mm³ Meter{3, -3}

# support the `2cm` syntax
Base.(:*){T<:Unit}(n::Real, ::Type{T}) = T(n)
# support `m^2` syntax
Base.(:^){m,d}(::Type{Meter{d,m}}, n::Real) = Meter{n,m}(1)
Base.(:*){m,da,db}(::Type{Meter{da,m}}, ::Type{Meter{db,m}}) = Meter{(da + db),m}(1)
# support `3m^2` syntax
Base.(:*)(n::Real, m::Meter) = typeof(m)(m.value * n)
Base.(:*)(m::Meter, n::Real) = n * m

# support Base.promote(1mm, 2m) == (1mm, 2000mm)
Base.promote_rule{d,m1,m2}(::Type{Meter{d,m1}},::Type{Meter{d,m2}}) = Meter{d,min(m1,m2)}
Base.convert{d,m1,m2}(::Type{Meter{d,m2}}, s::Meter{d,m1}) = Meter{d,m2}(magnify(s.value, m1 - m2))

"""
Scale `n` by `m` orders of magnitude. Always returns a `Float`
"""
magnify(n::Real, m::Integer) = n * 10.0 ^ m

# Define math functions
for sym in (:+, :-)
  @eval Base.$sym{T<:Meter}(a::T, b::T) = T($sym(a.value, b.value))
  @eval Base.$sym{d,m1,m2}(a::Meter{d,m1}, b::Meter{d,m2}) = $sym(promote(a, b)...)
end

promote_magnitude{amag,bmag,da,db}(a::Meter{da,amag}, b::Meter{db,bmag}) = begin
  avalue, bvalue, outmag = (bmag > amag
  ? (a.value, b.value * (10 ^ (bmag - amag)), amag)
  : (a.value * (10 ^ (amag - bmag)), b.value, bmag))
  Meter{da,outmag}(avalue), Meter{db,outmag}(bvalue)
end

# * and / also effect the dimension count
for sym in (:*, :/)
  @eval Base.$sym{d1,d2,m1,m2}(a::Meter{d1,m1}, b::Meter{d2,m2}) = $sym(promote_magnitude(a, b)...)
  @eval Base.$sym{d1,d2,m}(a::Meter{d1,m}, b::Meter{d2,m}) = begin
    Meter{$(sym == :* ? :+ : :-)(d1, d2),m}($sym(a.value, b.value))
  end
end

Base.show{d,mag}(io::IO, m::Meter{d,mag}) = begin
  print_shortest(io, m.value)
  print(io, get(prefix, mag, ""), 'm', d > 1 ? exponent[d] : "")
end
