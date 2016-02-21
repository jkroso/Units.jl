"""
Convert two `Unit`s to use the same magnitude while tolerating differences
in the number of dimensions

```julia
promote_magnitude(1km², 2mm) == (1e6mm², 2mm)
```
"""
function promote_magnitude end

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
  isa(u.value, Rational) && return print_shortest(io, convert(AbstractFloat, u.value))
  print_shortest(io, u.value)
end
Base.show(io::IO, t::Unit) = (show_value(io, t); show_unit(io, typeof(t)))

# basic math
for sym in (:+, :-)
  @eval Base.$sym{T<:Unit}(a::T, b::T) = T($sym(a.value, b.value))
  @eval Base.$sym{A<:Unit,B<:Unit}(a::A, b::B) = begin
    T = promote_type(A, B)
    T($sym(convert(T, a).value, convert(T, b).value))
  end
end

abstract Size{dimensions} <: Unit

typealias Length Size{1}
typealias Area Size{2}
typealias Volume Size{3}

const imperial_units = Dict(1609344//1000 => :mile,
                            9144//10000 => :yard,
                            3048//10000 => :ft,
                            254//10000 => :inch)

immutable ImperialSize{basefactor, d} <: Size{d}
  value::Real
end

show_unit{d,f}(io::IO, ::Type{ImperialSize{f,d}}) =
  print(io, imperial_units[f], d > 1 ? exponent[d] : "")

basefactor{f,d}(s::Type{ImperialSize{f,d}}) = f

Base.promote_rule{f1,f2,d}(::Type{ImperialSize{f1,d}},::Type{ImperialSize{f2,d}}) =
  ImperialSize{min(f1,f2),d}
Base.convert{f1,f2,d}(T::Type{ImperialSize{f2,d}}, s::ImperialSize{f1,d}) =
  T(s.value * basefactor(typeof(s))//basefactor(T))

for (factor, name) in imperial_units
  @eval typealias $name ImperialSize{$factor, 1}
  @eval typealias $(symbol(name, '²')) ImperialSize{$factor, 2}
  @eval typealias $(symbol(name, '³')) ImperialSize{$factor, 3}
end

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
Scale `n` by `m` orders of magnitude
"""
magnify(n::Real, m::Integer) = n * Rational(10) ^ m

# enable combining imperial and metric
Base.promote_rule{f,m,d}(::Type{ImperialSize{f,d}}, ::Type{Meter{d,m}}) = Meter{1,0}
Base.promote_rule{f,m,d}(::Type{Meter{d,m}}, ::Type{ImperialSize{f,d}}) = Meter{1,0}
Base.convert{f,m,d}(T::Type{Meter{d,m}}, s::ImperialSize{f,d}) = T(s.value * f)

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
                          3600 => :hr,
                          86400 => :day,
                          604800 => :week)

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
Base.convert{f1,f2}(T::Type{Time{f2}}, s::Time{f1}) =
  T(s.value * basefactor(typeof(s))//basefactor(T))

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

# enable promote(1m/s, 2km/hr) == (1m/s, 7200m/s)
Base.promote_rule{NA,DA,NB,DB}(a::Type{Ratio{NA,DA}}, b::Type{Ratio{NB,DB}}) =
  Ratio{promote_rule(NA,NB), promote_rule(DA,DB)}
Base.convert{N1,D1,N2,D2}(T::Type{Ratio{N2,D2}}, r::Ratio{N1,D1}) =
  T(r.value * basefactor(N1)//basefactor(N2) * basefactor(D2)//basefactor(D1))
