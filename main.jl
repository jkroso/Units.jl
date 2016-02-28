"""
Convert two `Unit`s to use the same magnitude while tolerating differences
in the number of dimensions

```julia
promote_magnitude(1km², 2mm) == (1e6mm², 2mm)
```
"""
function promote_magnitude end

"""
Returns the shorthand notation for a given unit type
"""
function abbr end

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

Base.show(io::IO, t::Unit) = begin
  print_shortest(io, convert(AbstractFloat, t.value))
  write(io, abbr(typeof(t)))
end

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

immutable ImperialSize{basefactor, d} <: Size{d} value::Real end

for (factor, name) in imperial_units
  @eval typealias $name ImperialSize{$factor, 1}
  @eval typealias $(symbol(name, '²')) ImperialSize{$factor, 2}
  @eval typealias $(symbol(name, '³')) ImperialSize{$factor, 3}
end

abbr{d,f}(::Type{ImperialSize{f,d}}) = string(imperial_units[f], d > 1 ? exponent[d] : "")
basefactor{f,d}(s::Type{ImperialSize{f,d}}) = f

Base.promote_rule{f1,f2,d}(::Type{ImperialSize{f1,d}},::Type{ImperialSize{f2,d}}) =
  ImperialSize{min(f1,f2),d}
Base.convert{f1,f2,d}(T::Type{ImperialSize{f2,d}}, s::ImperialSize{f1,d}) =
  T(s.value * basefactor(typeof(s))//basefactor(T))

immutable Meter{d,magnitude} <: Size{d} value::Real end
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

abbr{d,m}(::Type{Meter{d,m}}) = string(get(prefix, m, ""), 'm', d > 1 ? exponent[d] : "")
basefactor{d,m}(::Type{Meter{d,m}}) = Rational(10) ^ m

# support `2cm`
Base.(:*){T<:Unit}(n::Real, ::Type{T}) = T(n)
# support `m^2`
Base.(:^){m,d}(::Type{Meter{d,m}}, n::Integer) = Meter{n,m}
Base.(:*){m,da,db}(::Type{Meter{da,m}}, ::Type{Meter{db,m}}) = Meter{(da + db),m}
# support `3 * 1cm` and `1cm / 3`
for sym in (:*, :/)
  @eval Base.$sym{T<:Unit}(n::Real, u::T) = T($sym(u.value, n))
  @eval Base.$sym{T<:Unit}(u::T, n::Real) = $sym(n, u)
end

# support Base.promote(1mm, 2m) == (1mm, 2000mm)
Base.promote_rule{d,m1,m2}(::Type{Meter{d,m1}},::Type{Meter{d,m2}}) = Meter{d,min(m1,m2)}
Base.convert{d,m1,m2}(::Type{Meter{d,m2}}, s::Meter{d,m1}) =
  Meter{d,m2}(s.value * Rational(10) ^ (m1 - m2))

# enable combining imperial and metric
Base.promote_rule{f,m,d}(::Type{ImperialSize{f,d}}, ::Type{Meter{d,m}}) = Meter{d,0}
Base.promote_rule{f,m,d}(::Type{Meter{d,m}}, ::Type{ImperialSize{f,d}}) = Meter{d,0}
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

Base.(:^){d,m}(u::Meter{d,m}, n::Integer) = Meter{d * n, m}(u.value)

const time_factors = Dict(-1000 => :ms,
                          1 => :s,
                          60 => :minute,
                          3600 => :hr,
                          86400 => :day,
                          604800 => :week)

immutable Time{factor} <: Unit value::Real end
for (factor,name) in time_factors
  @eval typealias $name Time{$factor}
end

abbr{f}(::Type{Time{f}}) = string(time_factors[f])
basefactor{f}(::Type{Time{f}}) = f

# support Base.promote(1s, 2hr) == (1s, 3600s)
Base.promote_rule{f1,f2}(::Type{Time{f1}},::Type{Time{f2}}) = Time{min(f1,f2)}
Base.convert{f1,f2}(T::Type{Time{f2}}, s::Time{f1}) =
  T(s.value * basefactor(typeof(s))//basefactor(T))

immutable Ratio{Num,Den} <: Unit value::Real end
typealias Speed{s<:Size,t<:Time} Ratio{s,t}
typealias Acceleration{s<:Size,t<:Time} Ratio{Speed{s,t},t}
typealias Jerk{s<:Size,t<:Time} Ratio{Acceleration{s,t},t}

abbr{Num,Den}(::Type{Ratio{Num,Den}}) = string(abbr(Num), '/', abbr(Den))

# enable `1m/s` and `m/s` syntax
Base.(:/){T<:Unit}(a::Unit, b::Type{T}) = Ratio{typeof(a),T}(a.value)
Base.(:/){Num<:Unit,Den<:Unit}(a::Type{Num}, b::Type{Den}) = Ratio{Num,Den}

# enable promote(1m/s, 2km/hr) == (1m/s, 7200m/s)
Base.promote_rule{NA,DA,NB,DB}(a::Type{Ratio{NA,DA}}, b::Type{Ratio{NB,DB}}) =
  Ratio{promote_rule(NA,NB), promote_rule(DA,DB)}
Base.convert{N1,D1,N2,D2}(T::Type{Ratio{N2,D2}}, r::Ratio{N1,D1}) =
  T(r.value * basefactor(N1)//basefactor(N2) * basefactor(D2)//basefactor(D1))

abstract Angle <: Unit
immutable Degree <: Angle value::Real end
immutable Radian <: Angle value::Real end
typealias ° Degree
typealias rad Radian
basefactor(::Type{Degree}) = π/180
basefactor(::Type{Radian}) = 1
abbr(::Type{Degree}) = "°"
abbr(::Type{Radian}) = "rad"
Base.promote_rule{A<:Angle,B<:Angle}(::Type{A}, ::Type{B}) = Radian
Base.convert(::Type{Radian}, d::Degree) = Radian(d.value * basefactor(Degree))
Base.convert(::Type{Degree}, r::Radian) = Degree(r.value / basefactor(Degree))

abstract Temperature{factor} <: Unit
immutable Kelvin{f} <: Temperature value::Real end
immutable Celsius{f} <: Temperature value::Real end
immutable Fahrenheit{f} <: Temperature value::Real end
typealias K Kelvin{0}
typealias °C Celsius{0}
typealias °F Fahrenheit{0}
abbr{m}(::Type{Kelvin{m}}) = string(get(prefix, m, ""), "K")
abbr{m}(::Type{Celsius{m}}) = string(get(prefix, m, ""), "°C")
abbr{m}(::Type{Fahrenheit{m}}) = string(get(prefix, m, ""), "°F")
basefactor{m}(::Type{Kelvin{m}}) = 1
basefactor{m}(::Type{Celsius{m}}) = 1       # with a fixed offset
basefactor{m}(::Type{Fahrenheit{m}}) = 5//9 #
baseoffset{m}(::Type{Kelvin{m}}) = 0
baseoffset{m}(::Type{Fahrenheit{m}}) = 459.67
baseoffset{m}(::Type{Celsius{m}}) = 273.15
Base.promote_rule{A<:Temperature,B<:Temperature}(::Type{A}, ::Type{B}) = Kelvin{0}
Base.convert{K<:Kelvin,T<:Union{Celsius,Fahrenheit}}(::Type{K}, t::T) =
  K((t.value + baseoffset(T)) * basefactor(T))
