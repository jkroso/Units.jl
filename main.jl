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
`basefactor(m) == 1`
"""
function basefactor(x) 1 end

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

abstract type Unit <: Number end

Base.show(io::IO, t::Unit) = begin
  print_shortest(io, convert(AbstractFloat, t.value))
  write(io, abbr(typeof(t)))
end

Base.:(==){U<:Unit}(a::U,b::U) = convert(Real, a) == convert(Real, b)

Base.convert{N<:Real,U<:Unit}(::Type{N}, u::U) = convert(N, u.value * basefactor(U))
Base.convert{U<:Unit}(::Type{U}, n::Real) = U(n)
Base.promote_rule{U<:Unit,N<:Real}(::Type{U}, ::Type{N}) = U

# support `2cm`
Base.:*{T<:Unit}(n::Real, ::Type{T}) = T(n)

# support `3 * 1cm` etc..
for op in (:*, :/, :+, :-)
  @eval Base.$op{T<:Unit}(n::Real, u::T) = T($op(u.value, n))
  @eval Base.$op{T<:Unit}(u::T, n::Real) = T($op(u.value, n))
end

# support 1cm + 1mm etc..
for sym in (:+, :-, :*, :/)
  @eval Base.$sym{T<:Unit}(a::T, b::T) = T($sym(a.value, b.value))
  @eval Base.$sym{A<:Unit,B<:Unit}(a::A, b::B) = begin
    T = promote_type(A, B)
    T($sym(convert(T, a).value, convert(T, b).value))
  end
end

struct Ratio{Num,Den} <: Unit value::Real end

abbr{Num,Den}(::Type{Ratio{Num,Den}}) = string(abbr(Num), '/', abbr(Den))

# enable `1m/s` and `m/s` syntax
Base.:/{T<:Unit}(a::Unit, b::Type{T}) = Ratio{typeof(a),T}(a.value)
Base.:/{Num<:Unit,Den<:Unit}(a::Type{Num}, b::Type{Den}) = Ratio{Num,Den}

# enable promote(1m/s, 2km/hr) == (1m/s, 7200m/s)
Base.promote_rule{NA,DA,NB,DB}(a::Type{Ratio{NA,DA}}, b::Type{Ratio{NB,DB}}) =
  Ratio{promote_rule(NA,NB), promote_rule(DA,DB)}
Base.convert{N1,D1,N2,D2}(T::Type{Ratio{N2,D2}}, r::Ratio{N1,D1}) =
  T(r.value * basefactor(N1)//basefactor(N2) * basefactor(D2)//basefactor(D1))

abstract type Size{dimensions} <: Unit end
const Length = Size{1}
const Area = Size{2}
const Volume = Size{3}

const imperial_units = Dict(1609344//1000 => :mile,
                            9144//10000 => :yard,
                            3048//10000 => :ft,
                            254//10000 => :inch)

struct ImperialSize{basefactor, d} <: Size{d} value::Real end

for (factor, name) in imperial_units
  @eval const $name = ImperialSize{$factor, 1}
  @eval const $(Symbol(name, '²')) = ImperialSize{$factor, 2}
  @eval const $(Symbol(name, '³')) = ImperialSize{$factor, 3}
end

abbr{d,f}(::Type{ImperialSize{f,d}}) = string(imperial_units[f], d > 1 ? exponent[Int(d)] : "")
basefactor{f,d}(s::Type{ImperialSize{f,d}}) = f

Base.promote_rule{f1,f2,d}(::Type{ImperialSize{f1,d}},::Type{ImperialSize{f2,d}}) =
  ImperialSize{min(f1,f2),d}
Base.convert{f1,f2,d}(T::Type{ImperialSize{f2,d}}, s::ImperialSize{f1,d}) =
  T(s.value * basefactor(typeof(s))/basefactor(T))

struct Meter{d,magnitude} <: Size{d} value::Real end
const km    = Meter{Rational(1), 3}
const m     = Meter{Rational(1), 0}
const cm    = Meter{Rational(1),-2}
const mm    = Meter{Rational(1),-3}
const km²   = Meter{Rational(2), 3}
const m²    = Meter{Rational(2), 0}
const cm²   = Meter{Rational(2), -2}
const mm²   = Meter{Rational(2), -3}
const km³   = Meter{Rational(3), 3}
const m³    = Meter{Rational(3), 0}
const litre = Meter{Rational(3),-1}
const cm³   = Meter{Rational(3), -2}
const mm³   = Meter{Rational(3), -3}

abbr{d,m}(::Type{Meter{d,m}}) = string(get(prefix, m, ""), 'm', d > 1 ? exponent[Int(d)] : "")
basefactor{d,m}(::Type{Meter{d,m}}) = (Rational(10) ^ m) ^ d

# support `m^2`
Base.:^{m,d}(::Type{Meter{d,m}}, n::Integer) = Meter{n,m}
Base.:*{m,da,db}(::Type{Meter{da,m}}, ::Type{Meter{db,m}}) = Meter{(da + db),m}
Base.sqrt{d,m}(s::Meter{d,m}) = Meter{d/2,m}(sqrt(s.value))

# support Base.promote(1mm, 2m) == (1mm, 2000mm)
Base.promote_rule{d,m1,m2}(::Type{Meter{d,m1}},::Type{Meter{d,m2}}) = Meter{d,min(m1,m2)}
Base.convert{d,m1,m2}(T::Type{Meter{d,m2}}, s::Meter{d,m1}) =
  T(s.value * basefactor(typeof(s))/basefactor(T))

# enable combining imperial and metric
Base.promote_rule{f,m,d}(::Type{ImperialSize{f,d}}, ::Type{Meter{d,m}}) = Meter{d,0}
Base.promote_rule{f,m,d}(::Type{Meter{d,m}}, ::Type{ImperialSize{f,d}}) = Meter{d,0}
Base.convert{f,m,d}(T::Type{Meter{d,m}}, s::ImperialSize{f,d}) = T(s.value * f)

promote_magnitude{amag,bmag,da,db}(a::Meter{da,amag}, b::Meter{db,bmag}) = begin
  minmag = min(amag, bmag)
  convert(Meter{da,minmag}, a), convert(Meter{db,minmag}, b)
end

# * and / also effect the dimension count
for sym in (:*, :/)
  @eval Base.$sym{d1,d2,m1,m2}(a::Meter{d1,m1}, b::Meter{d2,m2}) = $sym(promote_magnitude(a, b)...)
  @eval Base.$sym{d1,d2,m}(a::Meter{d1,m}, b::Meter{d2,m}) = begin
    Meter{$(sym == :* ? :+ : :-)(d1, d2),m}($sym(a.value, b.value))
  end
end

Base.:^{d,m}(u::Meter{d,m}, n::Integer) = Meter{d * n, m}(u.value ^ n)

const time_factors = Dict(-1000 => :ms,
                          1 => :s,
                          60 => :minute,
                          3600 => :hr,
                          86400 => :day,
                          604800 => :week)

struct Time{factor} <: Unit value::Real end
for (factor,name) in time_factors
  @eval const $name = Time{$factor}
end

abbr{f}(::Type{Time{f}}) = string(time_factors[f])
basefactor{f}(::Type{Time{f}}) = f

# support Base.promote(1s, 2hr) == (1s, 3600s)
Base.promote_rule{f1,f2}(::Type{Time{f1}},::Type{Time{f2}}) = Time{min(f1,f2)}
Base.convert{f1,f2}(T::Type{Time{f2}}, s::Time{f1}) =
  T(s.value * basefactor(typeof(s))/basefactor(T))

const Speed{s<:Size,t<:Time} = Ratio{s,t}
const Acceleration{s<:Size,t<:Time} = Ratio{Speed{s,t},t}
const Jerk{s<:Size,t<:Time} = Ratio{Acceleration{s,t},t}

abstract type Angle <: Unit end
struct Degree <: Angle value::Real end
struct Radian <: Angle value::Real end
const ° = Degree
const rad = Radian
basefactor(::Type{Degree}) = π/180
abbr(::Type{Degree}) = "°"
abbr(::Type{Radian}) = "rad"
Base.promote_rule{A<:Angle,B<:Angle}(::Type{A}, ::Type{B}) = Radian
Base.convert(::Type{Radian}, d::Degree) = Radian(d.value * basefactor(Degree))
Base.convert(::Type{Degree}, r::Radian) = Degree(r.value / basefactor(Degree))

for sym in (:sin,:cos,:tan)
  @eval Base.$sym(n::Angle) = $sym(convert(Radian, n).value)
end

abstract type Temperature{factor} <: Unit end
struct Kelvin{f} <: Temperature{f} value::Real end
struct Celsius{f} <: Temperature{f} value::Real end
struct Fahrenheit{f} <: Temperature{f} value::Real end
const K = Kelvin{0}
const °C = Celsius{0}
const °F = Fahrenheit{0}
abbr{m}(::Type{Kelvin{m}}) = string(get(prefix, m, ""), "K")
abbr{m}(::Type{Celsius{m}}) = string(get(prefix, m, ""), "°C")
abbr{m}(::Type{Fahrenheit{m}}) = string(get(prefix, m, ""), "°F")
basefactor{m}(::Type{Fahrenheit{m}}) = 5//9 #
baseoffset{m}(::Type{Kelvin{m}}) = 0
baseoffset{m}(::Type{Fahrenheit{m}}) = 459.67
baseoffset{m}(::Type{Celsius{m}}) = 273.15
Base.promote_rule{A<:Temperature,B<:Temperature}(::Type{A}, ::Type{B}) = Kelvin{0}
Base.convert{K<:Kelvin,T<:Union{Celsius,Fahrenheit}}(::Type{K}, t::T) =
  K((t.value + baseoffset(T)) * basefactor(T))

abstract type Weight{magnitude} <: Unit end
struct Gram{m} <: Weight{m} value::Real end
const g = Gram{0}
const kg = Gram{3}
const ton = Gram{6}
abbr{m}(::Type{Gram{m}}) = string(get(prefix, m, ""), "g")
abbr(::Type{Gram{6}}) = "ton"
basefactor{m}(::Type{Gram{m}}) = 10^m
Base.promote_rule{a,b}(::Type{Gram{a}}, ::Type{Gram{b}}) = Gram{min(a,b)}
Base.convert{A<:Gram}(::Type{A}, b::Gram) = A(b.value * (basefactor(typeof(b))/basefactor(A)))
