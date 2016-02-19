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

# Define math functions
for sym in (:+, :-)
  @eval function Base.$sym{d}(a::Meter{d}, b::Meter{d})
    mag, aval, bval = normalize(a, b)
    Meter{d,mag}($sym(aval, bval))
  end
end

# * and / also effect the dimension count
for sym in (:*, :/)
  @eval function Base.$sym{da,db}(a::Meter{da},b::Meter{db})
    mag, aval, bval = normalize(a, b)
    dimensions = $(sym == :* ? :+ : :-)(da, db)
    Meter{dimensions,mag}($sym(aval, bval))
  end
end

"""
Determine a common magnitude and adjust both values to that magnitude
"""
normalize{amag,bmag,da,db}(a::Meter{da,amag}, b::Meter{db,bmag}) = begin
  minmag = min(amag, bmag)
  maxmag = max(amag, bmag)
  difference = maxmag - minmag
  if amag == minmag
    minmag, a.value, b.value * (10 ^ difference)
  else
    minmag, a.value * (10 ^ difference), b.value
  end
end

Base.show{d,mag}(io::IO, m::Meter{d,mag}) =
  print(io, m.value, get(prefix, mag, ""), 'm', d > 1 ? exponent[d] : "")
