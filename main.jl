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

const exponents = Vector{Char}("¹²³⁴⁵⁶⁷⁸⁹")

"Units are meaningful numbers"
abstract type Unit <: Number end

"Dimensions are the base units which others are derived from"
abstract type Dimension <: Unit end

abstract type Length <: Dimension end
abstract type Mass <: Dimension end
abstract type Angle <: Dimension end
abstract type Temperature <: Dimension end

abstract type DerivedUnit <: Unit end

"Represents units like m/s"
struct Ratio{Num<:Unit,Den<:Unit} <: DerivedUnit value::Num end

"Represents units like m²"
struct Exponent{dimensions,D<:Dimension} <: DerivedUnit value::Real end

"""
Returns the shorthand notation for a given unit type
"""
function abbr end

"""
Get the magnitude of a Dimension

`magnitude(m) = 0`
`magnitude(km) = 3`
"""
magnitude(::Type{<:Dimension}) = 1

"""
Compute the factor required to convert a Unit of any magnitude
to one with a magnitude of 1

`basefactor(km) == 1000`
`basefactor(m) == 1`
"""
basefactor(::Type{T}) where T<:Dimension = Rational(10)^magnitude(T) # Rational allows 10^-1
basefactor(::Type{Exponent{d,D}}) where {d,D} = basefactor(D)^d

"Convert to the most precise type possible"
precise(n::Number) = n
precise(n::AbstractFloat) = rationalize(n)

abbr(::Type{Ratio{A,B}}) where {A,B} = string(abbr(A), '/', abbr(B))
abbr(::Type{Exponent{n,T}}) where {n,T} = string(abbr(T), exponents[Int(n)])

dimensions(::Type{T}) where T<:Unit = 1
dimensions(::Type{Exponent{n,T}}) where {n,T} = n

dimension(::Type{Exponent{n,T}}) where {n,T} = T
dimension(::Type{T}) where T<:Unit = T

Base.show(io::IO, t::Unit) = begin
  Base.print_shortest(io, Float64(t.value))
  write(io, abbr(typeof(t)))
end

Base.show(io::IO, r::Ratio{N,D}) where {N,D} = begin
  show(io, r.value)
  write(io, '/', abbr(D))
end

# convert(Real, 1km) == 1000
Base.convert(::Type{N}, u::U) where {N<:Real,U<:Unit} = convert(N, precise(u.value) * basefactor(U))
# convert(km, 1) == 1km
Base.convert(::Type{U}, n::Real) where U<:Unit = U(n)

const Area = Exponent{2,L} where L<:Length
const Volume = Exponent{3,L} where L<:Length

struct Meter{magnitude} <: Length value::Real end

# define mm, km etc...
for mag in [3,0,-2,-3,-6]
  name = Symbol(get(prefix, mag, ""), :m)
  @eval const $name = Meter{$mag}
  @eval const $(Symbol(name, '²')) = Area{$name}
  @eval const $(Symbol(name, '³')) = Volume{$name}
end
const litre = Volume{Meter{-1}}

abbr(M::Type{<:Meter}) = string(get(prefix, magnitude(M), ""), 'm')
magnitude(::Type{Meter{m}}) where m = m

# 2cm == Meter{-2}(2)
Base.:*(n::Real, ::Type{T}) where T<:Unit = T(n)

# 3 * 1cm == 3cm
for op in (:*, :/, :+, :-)
  @eval Base.$op(n::Real, u::T) where T<:Unit = T($op(precise(u.value), precise(n)))
  @eval Base.$op(u::T, n::Real) where T<:Unit = T($op(precise(u.value), precise(n)))
end

# 1cm + 1mm == 11mm && 1cm - 1mm == 9mm
for sym in (:+, :-)
  @eval Base.$sym(a::T, b::T) where T<:Unit = T($sym(precise(a.value), precise(b.value)))
  @eval Base.$sym(a::A, b::B) where {A<:Unit,B<:Unit} = $sym(promote(a, b)...)
  # -(1m) == -1m
  @eval Base.$sym(a::T) where T<:Unit = T($sym(a.value))
end

# m/s == Ratio{m,s}
Base.:/(A::Type{<:Dimension}, B::Type{<:Dimension}) = Ratio{A,B}
# 1m/s == Ratio{m,s}(1) && 1m/s^2 == Ratio{m,s^2}
Base.:/(a::A, b::Type{B}) where {A<:Dimension,B<:Unit} = Ratio{A,B}(a)
# 1s/5s == 0.2
Base.:/(a::T, b::T) where T<:Dimension = precise(a.value)/precise(b.value)
# 1m²/2m² == 0.5
Base.:/(a::Exponent{da,T}, b::Exponent{db,T}) where {da,db,T} = begin
  d = da - db
  v = precise(a.value)/precise(b.value)
  d == 0 ? v : d == 1 ? T(v) : Exponent{d,T}(v)
end
# 1m/5s == 0.2m/s
Base.:/(a::A, b::B) where {A<:Dimension,B<:Dimension} = Ratio{A,B}(precise(a.value)/precise(b.value))
# 1.1s²/1m² == 1.1s²/m²
Base.:/(a::A, b::B) where {A<:Exponent,B<:Exponent} = Ratio{A,B}(precise(a.value)/precise(b.value))
# 1.1s/1m² == 1.1s/m²
Base.:/(a::A, b::B) where {A<:Dimension,B<:Exponent} = Ratio{A,B}(precise(a.value)/precise(b.value))
# 1.1s²/1m == 1.1s²/m
Base.:/(a::A, b::B) where {A<:Exponent,B<:Dimension} = Ratio{A,B}(precise(a.value)/precise(b.value))
# 1m²/2m == 0.5m
Base.:/(a::Unit, b::Unit) = convert(Exponent, a) / convert(Exponent, b)

# 5s * (1m/s) == 5m
Base.:*(a::Unit, b::Ratio{<:Unit,B}) where B<:Unit =
  b.value * convert(B, a).value
Base.:*(a::Ratio, b::Unit) = b * a

# promote(1m/s, 2km/hr) == (1m/s, 7200m/s)
Base.promote_rule(a::Type{Ratio{NA,DA}}, b::Type{Ratio{NB,DB}}) where {NA,DA,NB,DB} =
  Ratio{promote_type(NA,NB), promote_type(DA,DB)}
Base.convert(T::Type{Ratio{N2,D2}}, r::Ratio{N1,D1}) where {N1,D1,N2,D2} =
  T(precise(r.value) * basefactor(N1)/basefactor(N2) * basefactor(D2)/basefactor(D1))

# m^2 == m²
Base.:^(::Type{U}, n::Integer) where U<:Unit = Exponent{n,U}
# m²^2 == Exponent{4,m}
Base.:^(::Type{Exponent{d,T}}, n::Integer) where {d,T} = Exponent{d*n,T}
# (1m²)^2 == 1m⁴
Base.:^(u::Exponent{d,T}, n::Integer) where {d,T} = Exponent{d*n,T}(precise(u.value) ^ n)

# m * m == m²
Base.:*(::Type{Exponent{da,T}}, ::Type{Exponent{db,T}}) where {da,db,T<:Unit} = Exponent{da+db,T}
# m * m² == m³
Base.:*(::Type{A}, ::Type{B}) where {A<:Unit,B<:Unit} = convert(Exponent, A) * convert(Exponent, B)
# 1m¹ * 2m¹ == 2m²
Base.:*(a::Exponent{da,T}, b::Exponent{db,T}) where {da,db,T} = Exponent{da+db, T}(a.value * b.value)
# (1mm²) * (2cm^1) => 20mm³
Base.:*(a::Exponent{da,TA}, b::Exponent{db,TB}) where {da,db,TA,TB} = begin
  T = promote_type(TA, TB)
  av = precise(a.value) * basefactor(TA)/basefactor(T)
  bv = precise(b.value) * basefactor(TB)/basefactor(T)
  Exponent{da + db, T}(av * bv)
end

# 1m * 2m => 2m²
Base.:*(a::Unit, b::Unit) = convert(Exponent, a) * convert(Exponent, b)
Base.convert(::Type{Exponent}, d::Dimension) = Exponent{1,typeof(d)}(d.value)
Base.convert(::Type{Exponent}, u::Exponent) = u

# promote(1cm², 1m²) == (1cm, 100cm)
Base.promote_rule(::Type{Exponent{d,TA}}, ::Type{Exponent{d,TB}}) where {d,TA,TB} =
  Exponent{d,promote_type(TA,TB)}

# convert(m³, 1000_000_000mm³) == 1m³
Base.convert(T::Type{Exponent{n,TA}}, b::B) where {n,TA,TB,B<:Exponent{n,TB}} =
  T(precise(b.value) * basefactor(B)/basefactor(T))

# promote(1mm, 2m) == (1mm, 2000mm)
Base.promote_rule(::Type{Meter{m1}},::Type{Meter{m2}}) where {m1,m2} = Meter{min(m1,m2)}
Base.convert(T::Type{<:Meter}, s::Meter{m}) where m =
  T(precise(s.value) * (10^m)/basefactor(T))

Base.sqrt(s::Exponent{d,T}) where {d,T} = begin
  n = Int(d/2)
  n == 1 ? T(sqrt(s.value)) : Exponent{n,T}(sqrt(s.value))
end

const time_factors = Dict(-1000 => :ms,
                          1 => :s,
                          60 => :minute,
                          3600 => :hr,
                          86400 => :day,
                          604800 => :week)

struct Time{factor} <: Dimension value::Real end
for (factor,name) in time_factors
  @eval const $name = Time{$factor}
end

abbr(::Type{Time{f}}) where f = string(time_factors[f])
basefactor(::Type{Time{f}}) where f = f

# promote(1s, 1hr) == (1s, 3600s)
Base.promote_rule(::Type{Time{f1}},::Type{Time{f2}}) where {f1,f2} = Time{min(f1,f2)}
Base.convert(T::Type{Time{f2}}, s::Time{f1}) where {f1,f2} =
  T(precise(s.value) * basefactor(typeof(s))/basefactor(T))

const Speed = Ratio{<:Length,<:Time}
const Acceleration = Ratio{<:Speed,<:Time}
const Jerk = Ratio{<:Acceleration,<:Time}

struct Degree <: Angle value::Real end
struct Radian <: Angle value::Real end
const ° = Degree
const rad = Radian

basefactor(::Type{Degree}) = π/180
abbr(::Type{Degree}) = "°"
abbr(::Type{Radian}) = "rad"
Base.promote_rule(::Type{<:Angle}, ::Type{<:Angle}) = Radian
Base.convert(::Type{Radian}, d::Degree) = Radian(precise(d.value) * basefactor(Degree))
Base.convert(::Type{Degree}, r::Radian) = Degree(precise(r.value) / basefactor(Degree))

for sym in (:sin,:cos,:tan)
  @eval Base.$sym(n::Angle) = $sym(convert(Radian, n).value)
end

struct Kelvin <: Temperature value::Real end
struct Celsius <: Temperature value::Real end
struct Fahrenheit <: Temperature value::Real end
const K = Kelvin
const °C = Celsius
const °F = Fahrenheit
abbr(::Type{Kelvin}) = "K"
abbr(::Type{Celsius}) = "°C"
abbr(::Type{Fahrenheit}) = "°F"
basefactor(::Type{Fahrenheit}) = 5//9
baseoffset(::Type{Kelvin}) = 0//1
baseoffset(::Type{Fahrenheit}) = rationalize(459.67)
baseoffset(::Type{Celsius}) = rationalize(273.15)
Base.promote_rule(::Type{<:Temperature}, ::Type{<:Temperature}) = Kelvin
Base.convert(::Type{Kelvin}, t::T) where T<:Union{Celsius,Fahrenheit} =
  Kelvin((precise(t.value) + baseoffset(T)) * basefactor(T))

struct Gram{m} <: Mass value::Real end
const g = Gram{0}
const kg = Gram{3}
const ton = Gram{6}
magnitude(::Type{Gram{m}}) where m = m
abbr(::Type{T}) where T<:Gram = string(get(prefix, magnitude(T), ""), "g")
abbr(::Type{Gram{6}}) = "ton"
Base.promote_rule{a,b}(::Type{Gram{a}}, ::Type{Gram{b}}) = Gram{min(a,b)}
Base.convert{A<:Gram}(::Type{A}, b::Gram) = A(precise(b.value) * (basefactor(typeof(b))/basefactor(A)))

const Pressure = Ratio{<:Mass,<:Area}

for λ ∈ (:<, :>, :!=, :(==))
  @eval begin
    # 1g < 2g == true
    Base.$λ(a::T,b::T) where T<:Unit = $λ(a.value, b.value)
    # 1kg < 2g == false
    Base.$λ(a::Unit,b::Unit) = $λ(promote(a,b)...)
    # 1kg < 2 == false
    Base.$λ(a::Real,b::Unit) = $λ(a, convert(Real, b))
    # 1 < 1kg == true
    Base.$λ(a::Unit,b::Real) = $λ(convert(Real, a), b)
  end
end
