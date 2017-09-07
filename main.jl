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

"Represents units like m²"
struct Exponent{dimensions,D<:Dimension} <: DerivedUnit value::Real end

"Represents units like m/s and N·m"
struct Combination{D<:Tuple{Vararg{Exponent}}} <: DerivedUnit value::Real end

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

"Convert to the most precise type possible"
precise(n::Number) = n
precise(n::AbstractFloat) = rationalize(n)

value(u::Unit) = precise(u.value)

"Convert derived dimensions into plain dimensions where possible"
simplify(::Type{Exponent{1,T}}) where T = T
simplify(::Type{Exponent{0,T}}) where T = Real
simplify(::Type{T}) where T<:Exponent = T
simplify(::Type{T}) where T<:Dimension = T

abbr(::Type{Exponent{n,T}}) where {n,T} = string(abbr(T), exponents[Int(n)])
# abbr(Combination{Tuple{m²,hr^-1}}) == "m²/hr"
# abbr(Combination{Tuple{m²,hr^1}}) == "m²·hr"
abbr(::Type{C}) where C<:Combination = begin
  str = sprint(abbr_params, params(C))
  str[chr2ind(str, 2):end]
end

abbr_params(io, params) =
  for T ∈ params
    d, ET = T.parameters
    d == 0 && continue
    print(io, d > 0 ? '·' : '/', abbr(ET))
    d = abs(d)
    d > 1 && print(io, exponents[d])
  end

# handle units with custom printing
Base.show(io::IO, c::Combination) = begin
  p = params(typeof(c))
  first, rest = (p[1], p[2:end])
  show(io, simplify(first)(value(c)))
  abbr_params(io, rest)
end

Base.show(io::IO, t::Unit) = begin
  Base.print_shortest(io, Float64(t.value))
  write(io, abbr(typeof(t)))
end

Base.abs(::Type{Exponent{n,T}}) where {n,T} = Exponent{abs(n), T}
Base.exponent(::Type{T}) where T<:Unit = 1
Base.exponent(::Type{E}) where E<:Exponent =
  if E isa UnionAll
    E.body.parameters[1]
  else
    E.parameters[1]
  end

"""
Get a units abstract type

```julia
abstract_unit(m²) == Area
abstract_unit(m) == Length
```
"""
abstract_unit(::Type{Exponent{n,T}}) where {n,T} = Exponent{n,<:abstract_unit(T)}
abstract_unit(::Type{C}) where C<:Combination = Combination{Tuple{map(abstract_unit, params(C))...}}
abstract_unit(::Type{T}) where T<:Dimension = supertype(T) == Dimension ? abstract_type(T) : supertype(T)
# abstract_unit(m²) == Exponent{2,<:Length}
# abstract_unit(m/s) == Combination{Tuple{Exponent{1,<:Length},Exponent{-1,<:Time}}}
# abstract_unit(m) == Length
# abstract_unit(s) == Time

"""
Find the abstract type a unit was derived from

```julia
dimension(m²) == Length
dimension(m) == Length
```
"""
dimension(::Type{T}) where T<:Dimension = abstract_unit(T)
dimension(::Type{T}) where T<:Exponent = begin
  if T isa UnionAll
    T.body.parameters[2].ub
  else
    abstract_unit(T.parameters[2])
  end
end
# dimension(Length^1) == Length
# dimension(Meter{0}^1) == Length

"Convert to a UnionAll if its a parametric DataType"
abstract_type(T::UnionAll) = T
abstract_type(T::DataType) = length(T.parameters) == 0 ? T : T.name.wrapper

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

# m/s == Combination{Tuple{m^1,s^-1}}
# s/m² == Combination{Tuple{s^1,m^-2}}
# s^2/m² == Combination{Tuple{s^2,m^-2}}
Base.:/(A::Type{<:Unit}, B::Type{<:Unit}) = simplify(Combination{Tuple{verbose(A),negate(B)}})
Base.:/(A::Type{<:Combination}, B::Type{<:Unit}) = simplify(A + Combination{Tuple{negate(B)}})
Base.:/(A::Type{<:Unit}, B::Type{<:Combination}) = simplify(Combination{Tuple{verbose(A)}} + B)

# 1m/s == (m/s)(1)
# 1m/s^2 == (m/s^2)(1)
# 1m²/s^2 == (m²/s^2)(1)
Base.:/(a::A, b::Type{B}) where {A<:Unit,B<:Unit} = (A/B)(a.value)

verbose(::Type{E}) where E<:Exponent = unionall(E)
verbose(::Type{D}) where D<:Dimension = unionall(Exponent{1,D})
negate(::Type{D}) where D<:Dimension = unionall(Exponent{-1,D})
negate(::Type{E}) where E<:Exponent =
  if E isa UnionAll
    d, var = E.body.parameters
    UnionAll(var, Exponent{-d, var})
  else
    d, T = E.parameters
    Exponent{-d, T}
  end

unionall(E::Type{Exponent{n,T}}) where {n,T} = T isa DataType && T.abstract ? Exponent{n,<:T} : E
# m^2 == m²
Base.:^(::Type{U}, n::Integer) where U<:Unit = unionall(Exponent{n,U})
# m²^2 == m^4
Base.:^(::Type{Exponent{d,T}}, n::Integer) where {d,T} = unionall(Exponent{d*n,T})
# (1m²)^2 == 1m^4
Base.:^(u::Exponent{d,T}, n::Integer) where {d,T} = Exponent{d*n,T}(value(u) ^ n)

# m * m == m² && m * cm == cm²
Base.:*(::Type{A}, ::Type{B}) where {A<:Dimension,B<:Dimension} = Exponent{2,promote_type(A,B)}
# m^1 * m² == m³
Base.:*(::Type{Exponent{da,T}}, ::Type{Exponent{db,T}}) where {da,db,T<:Unit} = Exponent{da+db,T}
# m * m² == m³
Base.:*(::Type{A}, ::Type{B}) where {A<:Dimension,B<:Exponent} = Exponent{1,A} * B
Base.:*(::Type{B}, ::Type{A}) where {A<:Dimension,B<:Exponent} = Exponent{1,A} * B
# 1m² * 2m² == 2m^4

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

const time_factors = Dict{Rational,Symbol}(60 => :minute,
                                           3600 => :hr,
                                           86400 => :day,
                                           604800 => :week)
# abbr(Time{-1000_000_000_000//1}) == "ps"
abbr(::Type{Time{f}}) where f =
  String(get(time_factors, f, string(get(prefix, -round(Int, log(10, abs(f))), ""), 's')))
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

params(::Type{Combination{T}}) where T<:Tuple = T.parameters
params(::Type{E}) where E<:Exponent = Core.svec(E)
params(::Type{D}) where D<:Dimension = Core.svec(Exponent{1,D})
# simplify(Combination{Tuple{m^2,s^0}}) == m^2
simplify(::Type{Combination{Tuple{T}}}) where T = simplify(T)
simplify(::Type{C}) where C<:Combination = begin
  p = collect(Iterators.filter(E->exponent(E) != 0, params(C)))
  length(p) == 0 && return Real
  length(p) == 1 && return simplify(p[1])
  Combination{Tuple{p...}}
end

Base.convert(::Type{Combination}, x::Exponent) = Combination{Tuple{typeof(x)}}(x.value)
Base.convert(::Type{Combination}, x::Dimension) = Combination{Tuple{Exponent{1,typeof(x)}}}(x.value)
# promote(1m/s, 9km/hr) == (1m/s, 2.5m/s)
Base.promote_rule(::Type{A}, ::Type{B}) where {A<:Combination,B<:Combination} = begin
  Combination{Tuple{map(promote_type, params(A), params(B))...}}
end

# conversion_factor(m²/hr, cm²/s) == 9//25
conversion_factor(::Type{A}, ::Type{B}) where {A<:Combination,B<:Combination} = begin
  pa, pb = (params(A), params(B))
  @assert length(pa) == length(pb) "$B is not equivelent to $A"
  foldl((value, p)->value * conversion_factor(p[1], p[2]), 1, zip(pa, pb))
end

for op in (:*, :/)
  @eval Base.$op(a::Unit, b::Unit) = $op(Combination(a), Combination(b))
  @eval Base.$op(a::Unit, b::Combination) = $op(Combination(a), b)
  @eval Base.$op(a::Combination, b::Unit) = $op(a, Combination(b))
  @eval Base.$op(a::A, b::B) where {A<:Combination, B<:Combination} = begin
    T = $(op == :* ? :+ : :-)(A, B)
    params_a, params_b = params(A), params(B)
    factor = 1
    for i in 1:min(length(params_a), length(params_b))
      EA = params_a[i]
      D = dimension(EA)
      i = findfirst(E->dimension(E) == D, params_b)
      i > 0 || continue
      EB = params_b[i]
      d1,T1 = EA.parameters
      d2,T2 = EB.parameters
      T0 = promote_type(T1, T2)
      ba = basefactor(EA)/basefactor(Exponent{d1,T0})
      bb = basefactor(EB)/basefactor(Exponent{d2,T0})
      factor *= ba * bb
    end
    T($op(value(a), value(b)) * factor)
  end
end
# 5s * (1m/s) == 5m
# 1m * 2m == 2m²
# 1minute * (1m/s) == 60m
# 3g * (1000m/kg) == 3m
# 1000_000_000mm³ * (2.5ton/m³) == 2.5ton
# Combination{Tuple{s^1}}(12) * Combination{Tuple{km^1, minute^-1}}(1) == (1//5)km
# 1mm² * 2cm^1 == 20mm³
# 2m²/1m² == 2
# 1s/5s == 0.2
# 1m/5s == 0.2m/s
# 1.1s/1m² == 1.1s/m²
# (1.1s^2)/1m == 1.1s^2/m && 1m²/2m == 0.5m
# 1m²/200cm² == 50 && 1m³/200cm² == 5000cm
# (2m^4)/(2m²) == 1m²
# (1.1s^2)/1m² == (1.1s^2)/m²

# Combination{Tuple{s^1}} * Combination{Tuple{s^2}} == s^2
# Combination{Tuple{s^2}}-Combination{Tuple{m^2}} == Combination{Tuple{s^2,m^-2}}
for op in (:+, :-, :*, :/)
  @eval Base.$op(::Type{A}, ::Type{B}) where {A<:Combination, B<:Combination} = begin
    params_a = params(A)
    params_b = params(B)
    dimensions = union(map(dimension, params_a), map(dimension, params_b))
    exprs = map(dimensions) do D
      ia = findfirst(E->dimension(E) == D, params_a)
      ib = findfirst(E->dimension(E) == D, params_b)
      if ib == 0
        params_a[ia]
      elseif ia == 0
        $(op == :- ? negate : identity)(params_b[ib])
      else
        d1,TA = params_a[ia].parameters
        d2,TB = params_b[ib].parameters
        Exponent{$op(d1,d2), promote_type(TA, TB)}
      end
    end
    simplify(Combination{Tuple{exprs...}})
  end
end

@eval macro $:export(e)
  quote
    export $(esc(e.args[1]))
    $(esc(Expr(:const, e)))
  end
end

@export Area = Length^2
@export Volume = Length^3
@export Pressure = Mass/Area
@export Speed = Length/Time
@export Acceleration = Speed/Time
@export Jerk = Acceleration/Time

# convert(s, 1ns) == 1e-9s
for (factor,name) in time_factors
  @eval @export $name = Time{$factor}
end
for mag in (-3, -6, -9, -12)
  name = Symbol(get(prefix, mag, ""), 's')
  @eval @export $name = Time{$(Rational(10)^mag)}
end
@export s = Time{1//1}

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

export Length, Mass, Time, Angle, Temperature
