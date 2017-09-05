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

"Represents percentages like 15%"
struct Percent <: Number value::Rational end
Percent(n::AbstractFloat) = Percent(rationalize(n))
Base.:-(a::Number, p::Percent) = a/(1 + p.value)
Base.:+(a::Number, p::Percent) = a*(1 + p.value)
(p::Percent)(n::Number) = n * p.value
(p::Percent)(n::Real) = precise(n) * p.value
Base.show(io::IO, p::Percent) = begin
  Base.print_shortest(io, Float64(p.value*100))
  write(io, '%')
end

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

"""
Like basefactor but for instead of comparing with the base type it
compares with a specific type
"""
function conversion_factor end

# conversion_factor(m³, mm³) == 1//1000_000_000
conversion_factor(::Type{A}, ::Type{B}) where {A<:Dimension,B<:Dimension} = begin
  @assert typejoin(A, B) != Dimension "can't convert $B to $A"
  basefactor(B)/basefactor(A)
end

# conversion_factor(m^2,cm^2) == 1//10_000
# conversion_factor(hr^-1,s^-1) == 3600//1
conversion_factor(::Type{A}, ::Type{B}) where {d1,d2,TA,TB,A<:Exponent{d1,TA},B<:Exponent{d2,TB}} = begin
  @assert typejoin(TA, TB) != Dimension "can't convert $TB to $TA"
  @assert d1 == d2 "$A needs to have the same dimension count as $B"
  basefactor(TB)^d2/basefactor(TA)^d1
end

# conversion_factor(m/s, km/hr) == 5//18
conversion_factor(::Type{Ratio{N2,D2}}, ::Type{Ratio{N1,D1}}) where {N1,D1,N2,D2} = begin
  conversion_factor(N2, N1)/conversion_factor(D2, D1)
end

"Convert to the most precise type possible"
precise(n::Number) = n
precise(n::AbstractFloat) = rationalize(n)

value(r::Ratio) = value(r.value)
value(u::Unit) = precise(u.value)

"Convert derived dimensions into plain dimensions where possible"
simplify(::Type{Exponent{1,T}}) where T = T
simplify(::Type{Exponent{0,T}}) where T = Real
simplify(::Type{Ratio{T,T}}) where T = Real
simplify(::Type{Ratio{A,B}}) where {A,B} = Ratio{simplify(A), simplify(B)}
simplify(::Type{T}) where T<:Dimension = T
simplify(::Type{T}) where T<:Exponent = T

abbr(::Type{Ratio{A,B}}) where {A,B} = string(abbr(A), '/', abbr(B))
abbr(::Type{Exponent{n,T}}) where {n,T} = string(abbr(T), exponents[Int(n)])

Base.exponent(::Type{T}) where T<:Unit = 1
Base.exponent(::Type{Exponent{n,T}}) where {n,T} = n

"Get a units abstract dimension type"
dimension(::Type{Exponent{n,T}}) where {n,T} = Exponent{n,<:dimension(T)}
dimension(::Type{Ratio{A,B}}) where {A,B} = Ratio{<:dimension(A), <:dimension(B)}
dimension(::Type{T}) where T<:Dimension = supertype(T) == Dimension ? abstract_type(T) : supertype(T)
# dimension(m²) == Exponent{2,<:Length}
# dimension(m/s) == Ratio{<:Length,<:Time}
# dimension(m) == Length
# dimension(s) == Time

abstract_type(T::UnionAll) = T
abstract_type(T::DataType) = length(T.parameters) == 0 ? T : T.name.wrapper

Base.show(io::IO, t::Unit) = begin
  Base.print_shortest(io, Float64(t.value))
  write(io, abbr(typeof(t)))
end

Base.show(io::IO, r::Ratio{N,D}) where {N,D} = begin
  show(io, r.value)
  write(io, '/', abbr(D))
end

# convert(Real, 1km) == 1000
Base.convert(::Type{N}, u::U) where {N<:Real,U<:Unit} = convert(N, u.value * basefactor(U))
# convert(km, 1) == 1km
Base.convert(::Type{U}, n::Real) where U<:Unit = U(n)
# convert(km, 1000m) == 1km
Base.convert(::Type{B}, a::A) where {A<:Unit,B<:Unit} = B(value(a) * conversion_factor(B, A))

# 2cm == Meter{-2}(2)
Base.:*(n::Real, ::Type{T}) where T<:Unit = T(n)

# 3 * 1cm == 3cm
for op in (:*, :/, :+, :-)
  @eval Base.$op(n::Real, u::T) where T<:Unit = T($op(value(u), precise(n)))
  @eval Base.$op(u::T, n::Real) where T<:Unit = T($op(value(u), precise(n)))
end

# 1cm + 1mm == 11mm && 1cm - 1mm == 9mm
for op in (:+, :-)
  @eval Base.$op(a::T, b::T) where T<:Unit = T($op(value(a), value(b)))
end

# -(1m) == -1m
Base.:-(a::T) where T<:Unit = T(-(value(a)))

# 2m²/1m² == 2
# 1s/5s == 0.2
Base.:/(a::A, b::A) where A<:Unit = value(a)/value(b)
# m/s == Ratio{m,s}
# s/m² == Ratio{s,m^2}
Base.:/(A::Type{<:Unit}, B::Type{<:Unit}) = simplify(Ratio{A,B})

# 1m/s == Ratio{m,s}(1)
# 1m/s^2 == Ratio{m,s^2}(1)
# 1m²/s^2 == Ratio{m²,s^2}(1)
Base.:/(a::A, b::Type{B}) where {A<:Unit,B<:Unit} = Ratio{A,B}(a)
# 1m²/200cm² == 50 && 1m³/200cm² == 5000cm
# (2m^4)/(2m²) == 1m²
# (1.1s^2)/1m² == (1.1s^2)/m²
Base.:/(a::Exponent{da,TA}, b::Exponent{db,TB}) where {da,db,TA<:Dimension,TB<:Dimension} = begin
  if typejoin(TA, TB) == Dimension # different dimensions
    (typeof(a)/typeof(b))(value(a)/value(b))
  else
    T = promote_type(TA, TB)
    av = value(convert(Exponent{da,T}, a))
    bv = value(convert(Exponent{db,T}, b))
    simplify(Exponent{da-db,T})(av/bv)
  end
end
# 1m/5s == 0.2m/s
# 1.1s/1m² == 1.1s/m²
# (1.1s^2)/1m == 1.1s^2/m && 1m²/2m == 0.5m
Base.:/(a::Unit, b::Unit) = convert(Exponent, a) / convert(Exponent, b)

# promote(1m/s, 9km/hr) == (1m/s, 2.5m/s)
Base.promote_rule(a::Type{Ratio{NA,DA}}, b::Type{Ratio{NB,DB}}) where {NA,DA,NB,DB} =
  Ratio{promote_type(NA,NB), promote_type(DA,DB)}

# m^2 == m²
Base.:^(::Type{U}, n::Integer) where U<:Unit = Exponent{n,U}
# m²^2 == m^4
Base.:^(::Type{Exponent{d,T}}, n::Integer) where {d,T} = Exponent{d*n,T}
# (1m²)^2 == 1m^4
Base.:^(u::Exponent{d,T}, n::Integer) where {d,T} = Exponent{d*n,T}(value(u) ^ n)

# 1m * 2m == 2m²
Base.:*(a::Unit, b::Unit) = convert(Exponent, a) * convert(Exponent, b)
# 5s * (1m/s) == 5m
Base.:*(a::Unit, b::Ratio{<:Unit,B}) where B<:Unit =
  b.value * convert(B, a).value
Base.:*(a::Ratio, b::Unit) = b * a
# m * m == m² && m * cm == cm²
Base.:*(::Type{A}, ::Type{B}) where {A<:Dimension,B<:Dimension} = Exponent{2,promote_type(A,B)}
# m^1 * m² == m³
Base.:*(::Type{Exponent{da,T}}, ::Type{Exponent{db,T}}) where {da,db,T<:Unit} = Exponent{da+db,T}
# m * m² == m³
Base.:*(::Type{A}, ::Type{B}) where {A<:Dimension,B<:Exponent} = Exponent{1,A} * B
Base.:*(::Type{B}, ::Type{A}) where {A<:Dimension,B<:Exponent} = Exponent{1,A} * B
# 1m² * 2m² == 2m^4
Base.:*(a::Exponent{da,T}, b::Exponent{db,T}) where {da,db,T} = Exponent{da+db,T}(value(a) * value(b))
# 1mm² * 2cm^1 == 20mm³
Base.:*(a::Exponent{da,TA}, b::Exponent{db,TB}) where {da,db,TA,TB} = begin
  T = promote_type(TA, TB)
  av = value(convert(Exponent{da,T}, a))
  bv = value(convert(Exponent{db,T}, b))
  Exponent{da + db, T}(av * bv)
end

Base.convert(::Type{Exponent}, d::Dimension) = Exponent{1,typeof(d)}(d.value)
Base.convert(::Type{Exponent}, u::Exponent) = u

# convert(m³, 1000_000_000mm³) == 1m³
# promote(1cm², 1m²) == (1cm², 10_000cm²)
Base.promote_rule(::Type{Exponent{d,TA}}, ::Type{Exponent{d,TB}}) where {d,TA,TB} =
  Exponent{d,promote_type(TA,TB)}

# sqrt(100m^2) == 10m
Base.sqrt(s::Exponent{d,T}) where {d,T} = begin
  n = Int(d/2)
  n == 1 ? T(sqrt(value(s))) : Exponent{n,T}(sqrt(value(s)))
end

struct Meter{magnitude} <: Length value::Real end

abbr(M::Type{<:Meter}) = string(get(prefix, magnitude(M), ""), 'm')
magnitude(::Type{Meter{m}}) where m = m
Base.promote_rule(::Type{Meter{m1}},::Type{Meter{m2}}) where {m1,m2} = Meter{min(m1,m2)}
# promote(1mm, 2m) == (1mm, 2000mm)

struct Time{factor} <: Dimension value::Real end

const time_factors = Dict{Rational,Symbol}(-1000 => :ms,
                                            1 => :s,
                                            60 => :minute,
                                            3600 => :hr,
                                            86400 => :day,
                                            604800 => :week)

abbr(::Type{Time{f}}) where f = String(time_factors[f])
basefactor(::Type{Time{f}}) where f = f

# promote(1s, 1hr) == (1s, 3600s)
Base.promote_rule(::Type{Time{f1}},::Type{Time{f2}}) where {f1,f2} = Time{min(f1,f2)}

struct Degree <: Angle value::Real end
struct Radian <: Angle value::Real end
basefactor(::Type{Degree}) = π/180
abbr(::Type{Degree}) = "°"
abbr(::Type{Radian}) = "rad"
Base.promote_rule(::Type{<:Angle}, ::Type{<:Angle}) = Radian
Base.convert(::Type{Radian}, d::Degree) = Radian(d.value * basefactor(Degree))
Base.convert(::Type{Degree}, r::Radian) = Degree(r.value / basefactor(Degree))
# convert(Degree, 1rad) == 57.29577951308232°

# sin(20°) == sind(20)
for op in (:sin,:cos,:tan)
  @eval Base.$op(n::Angle) = $op(convert(Radian, n))
  @eval Base.$op(n::Radian) = $op(n.value)
end

struct Kelvin <: Temperature value::Real end
struct Celsius <: Temperature value::Real end
struct Fahrenheit <: Temperature value::Real end
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
magnitude(::Type{Gram{m}}) where m = m
abbr(::Type{T}) where T<:Gram = string(get(prefix, magnitude(T), ""), "g")
abbr(::Type{Gram{6}}) = "ton"
Base.promote_rule{a,b}(::Type{Gram{a}}, ::Type{Gram{b}}) = Gram{min(a,b)}

for λ ∈ (:<, :>, :!=, :(==))
  @eval begin
    # 1g < 2g
    Base.$λ(a::T,b::T) where T<:Unit = $λ(value(a), value(b))
    # 1kg > 2g
    Base.$λ(a::Unit,b::Unit) = $λ(promote(a,b)...)
    # 1kg > 2
    Base.$λ(a::Real,b::Unit) = $λ(a, convert(Real, b))
    # 1 < 1kg
    Base.$λ(a::Unit,b::Real) = $λ(convert(Real, a), b)
  end
end

@eval macro $:export(e)
  quote
    export $(esc(e.args[1]))
    $(esc(Expr(:const, e)))
  end
end

@export Area = Exponent{2,<:Length}
@export Volume = Exponent{3,<:Length}
@export Pressure = Ratio{<:Mass,<:Area}
@export Speed = Ratio{<:Length,<:Time}
@export Acceleration = Ratio{<:Speed,<:Time}
@export Jerk = Ratio{<:Acceleration,<:Time}

for (factor,name) in time_factors
  @eval @export $name = Time{$factor}
end

# define mm, km etc...
for mag in (3, 0, -2, -3, -6, -9)
  name = Symbol(get(prefix, mag, ""), 'm')
  @eval @export $name = Meter{$mag}
  @eval @export $(Symbol(name, '²')) = Area{$name}
  @eval @export $(Symbol(name, '³')) = Volume{$name}
end
@export litre = Volume{Meter{-1}}

for mag in (3, 0, -3, -6, -9)
  name = Symbol(get(prefix, mag, ""), 'g')
  @eval @export $name = Gram{$mag}
end
@export ton = Gram{6}

@export K = Kelvin
@export °C = Celsius
@export °F = Fahrenheit
@export ° = Degree
@export rad = Radian

export Length, Mass, Time, Angle, Temperature, Ratio, Exponent
