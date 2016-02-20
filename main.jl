"""
Convert two `Unit`s to use the same magnitude while tolerating differences
in the number of dimensions

```julia
promote_magnitude(1km², 2mm) == (1e6mm², 2mm)
```
"""
function promote_magnitude end

"""
Print the value without its unit
"""
function show_value end

"""
Print the shorthand notation for a given unit type
"""
function show_unit end

"""
`basefactor(km) == 1000`
"""
function basefactor end

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

show_value(io::IO, u::Unit) = begin
  isinteger(u.value) && return print_shortest(io, convert(Int, u.value))
  isa(u.value, Rational) && return print(io, u.value)
  print_shortest(io, u.value)
end
Base.show(io::IO, t::Unit) = (show_value(io, t); show_unit(io, typeof(t)))

abstract Size{dimensions} <: Unit

typealias Length Size{1}
typealias Area Size{2}
typealias Volume Size{3}

immutable Meter{d,magnitude} <: Size{d}
  value::Real
end

basefactor{d,m}(::Type{Meter{d,m}}) = Rational(10) ^ m
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

show_unit{d,mag}(io::IO, ::Type{Meter{d,mag}}) =
  print(io, get(prefix, mag, ""), 'm', d > 1 ? exponent[d] : "")

const time_factors = Dict(-1000 => :ms,
                          1 => :s,
                          60 => :minute,
                          3600 => :hr)

immutable Time{factor} <: Unit
  value::Real
end

show_unit{f}(io::IO, ::Type{Time{f}}) = print(io, time_factors[f])
basefactor{f}(::Type{Time{f}}) = f

for (factor,name) in time_factors
  @eval typealias $name Time{$factor}
end

# support Base.promote(1s, 2hr) == (1s, 3600s)
Base.promote_rule{f1,f2}(::Type{Time{f1}},::Type{Time{f2}}) = Time{min(f1,f2)}
Base.convert{f1,f2}(Out::Type{Time{f2}}, s::Time{f1}) = Out(s.value)

for sym in (:+, :-)
  @eval Base.$sym{T<:Time}(a::T, b::T) = T($sym(a.value, b.value))
  @eval Base.$sym{fa,fb}(a::Time{fa}, b::Time{fb}) = $sym(promote(a,b)...)
end

immutable Ratio{Num,Den} <: Unit
  value::Real
end

show_unit{Num,Den}(io, ::Type{Ratio{Num,Den}}) = begin
  show_unit(io, Num)
  print(io, '/')
  show_unit(io, Den)
end

typealias Speed{s<:Size,t<:Time} Ratio{s,t}

# enable `1m/s` and `m/s` syntax
Base.(:/){T<:Unit}(a::Unit, b::Type{T}) = Ratio{typeof(a),T}(a.value)
Base.(:/){Num<:Unit,Den<:Unit}(a::Type{Num}, b::Type{Den}) = Ratio{Num,Den}

Base.promote_rule{NA,DA,NB,DB}(a::Type{Ratio{NA,DA}}, b::Type{Ratio{NB,DB}}) =
  Ratio{promote_rule(NA,NB), promote_rule(DA,DB)}
Base.convert{N1,D1,N2,D2}(T::Type{Ratio{N2,D2}}, r::Ratio{N1,D1}) =
  T(r.value * basefactor(N1)//basefactor(N2) * basefactor(D2)//basefactor(D1))

for sym in (:+, :-)
  @eval Base.$sym{T<:Ratio}(a::T, b::T) = T($sym(a.value, b.value))
  @eval Base.$sym(a::Ratio, b::Ratio) = $sym(promote(a,b)...)
end
