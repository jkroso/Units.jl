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

"Base units are the primitive types all others are derived from"
abstract type BaseUnit <: Unit end

abstract type SIUnit{D} <: BaseUnit end

baremodule SIUnits
  import Base.@enum
  @enum SIUnit Time Length Mass Current Temperature Substance Luminosity Angle
end

for u in instances(SIUnits.SIUnit)
  @eval const $(Symbol(u)) = SIUnit{$u}
end

abstract type DerivedUnit <: Unit end

sort_params(s) = begin
  out = Vector{Any}(undef, length(s))
  for i in 1:length(s)
    out[i] = s[i]
  end
  out
  sort!(out, by=siunit)
end
siunit(T::Type{BaseUnit}) = 99
siunit(T::Type{Unit}) = 100
siunit(T::Type{SIUnit{S}}) where S = S
siunit(T::Type{<:DerivedUnit}) = siunit(supertype(T))

"Represents units like m²"
struct Exponent{dimensions,D<:BaseUnit} <: DerivedUnit value::Number end

"Represents units like m/s and N·m"
struct Combination{D<:Tuple{Vararg{Exponent}}, magnitude} <: DerivedUnit
  value::Number
end

"""
Enables you to treat derived units as if they were base units and provides the
ability to define a scale factor which is necessary for representing units like
`Wh`
"""
struct SealedUnit{U<:Combination, scale} <: BaseUnit
  value::Number
end

scale(::Type{SealedUnit{U,s}}) where {U,s} = s
scale(::Type{SealedUnit{U,s}}, s2) where {U,s} = SealedUnit{U, s2}
scale(::Type{U}, s) where U<:Combination = SealedUnit{U, s}

"Represents percentages like 15%"
struct Percent <: Number value::Rational end
Percent(n::AbstractFloat) = Percent(rationalize(n))
Base.:-(a::Number, p::Percent) = a/(1 + p.value/100)
Base.:+(a::Number, p::Percent) = a*(1 + p.value/100)
Base.:*(a::Percent, b::Percent) = Percent((a.value/100) * (b.value/100))
Base.:+(a::Percent, b::Percent) = Percent(a.value + b.value)
Base.:-(a::Percent, b::Percent) = Percent(a.value - b.value)
for op in (:*, :/)
  @eval Base.$op(a::Number, b::Percent) = $op(a, b.value/100)
  @eval Base.$op(a::Percent, b::Number) = $op(a.value/100, b)
end
Base.:/(a::Percent, b::Percent) = Percent(a.value / b.value)
(p::Percent)(n::Real) = precise(n) * (p.value/100)
Base.show(io::IO, p::Percent) = print(io, Float64(p.value), '%')
Base.convert(::Type{T}, p::Percent) where T <: Real = p isa T ? p : convert(T, p.value)
Base.convert(::Type{Percent}, n::Real) = Percent(100n)
Base.convert(::Type{Percent}, n::Percent) = n
Base.promote_rule(::Type{Percent}, ::Type{B}) where B<:Real = Rational

"""
Returns the shorthand notation for a given unit type
"""
function abbr end
abbr(::Type{SealedUnit{U,s}}) where {U,s} = "$(abbr(U)) × $(scale(S))"
abbr(::Type{Exponent{n,T}}) where {n,T} = begin
  n == 1 && return abbr(T)
  n == 0 && return string(abbr(T), '⁰')
  n > 1 && return string(abbr(T), exponents[Int(n)])
  string(abbr(T), '⁻', exponents[Int(abs(n))])
end
abbr(::Type{C}) where C<:Combination = begin
  str = sprint(abbr_params, params(C))
  str[nextind(str, 0, 2):end]
end
abbr_params(io, params) =
  for T in params
    d, ET = T.parameters
    d == 0 && continue
    print(io, d > 0 ? '·' : '/', abbr(ET))
    d = abs(d)
    d > 1 && print(io, exponents[d])
  end
# abbr(Combination{Tuple{m²,hr^-1},0}) == "m²/hr"
# abbr(Combination{Tuple{m²,hr^1},0}) == "m²·hr"

"""
Get/set the magnitude of a Unit

`magnitude(km) = 3`
`magnitude(km) == magnitude(m, 3)`
"""
magnitude(::Type{<:BaseUnit}) = 0
magnitude(::Type{<:Combination{T,m}}) where {T,m} = m
magnitude(u::UnionAll)= u.body.parameters[2]
magnitude(::Type{<:Combination{T,_}}, m) where {T,_} = Combination{T,m}
magnitude(::Type{T}, m) where T<:Exponent = Combination{Tuple{T},m}
magnitude(::Type{SealedUnit{U,s}}, m) where {U,s} = SealedUnit{magnitude(U, m),s}
magnitude(::Type{SealedUnit{U,s}}) where {U,s} = magnitude(U)

"""
Compute the factor required to convert a Unit of any magnitude
to one with a magnitude of 1

`basefactor(km) == 1000`
`basefactor(m) == 1`
"""
basefactor(::Type{T}) where T<:BaseUnit = (10//1)^magnitude(T) # Rational allows 10^-1
basefactor(::Type{Exponent{d,D}}) where {d,D} = basefactor(D)^d

"""
Like basefactor but instead of comparing with the base type it
compares with a specific type
"""
function conversion_factor end

# conversion_factor(m³, mm³) == 1//1000_000_000
conversion_factor(::Type{A}, ::Type{B}) where {A<:BaseUnit,B<:BaseUnit} = begin
  @assert typejoin(A, B) != BaseUnit "can't convert $B to $A"
  basefactor(B)/basefactor(A)
end
# conversion_factor(m^2,cm^2) == 1//10_000
# conversion_factor(hr^-1,s^-1) == 3600//1
conversion_factor(::Type{A}, ::Type{B}) where {d1,d2,TA,TB,A<:Exponent{d1,TA},B<:Exponent{d2,TB}} = begin
  @assert typejoin(TA, TB) != BaseUnit "can't convert $TB to $TA"
  @assert d1 == d2 "$A needs to have the same exponent count as $B"
  basefactor(B)/basefactor(A)
end
# conversion_factor(m²/hr, cm²/s) == 9//25
conversion_factor(::Type{A}, ::Type{B}) where {A<:Combination,B<:Combination} = begin
  pa, pb = (params(A), params(B))
  @assert length(pa) == length(pb) "$B is not equivelent to $A"
  f = foldl((value, p)->value * conversion_factor(p[1], p[2]), zip(pa, pb), init=1)
  ma, mb = magnitude(A), magnitude(B)
  ma == mb && return f
  f * (10//1)^-(ma-mb)
end
# conversion_factor(mW, W) == 1000
# conversion_factor(W, mW) == 1//1000
# conversion_factor(kW, MW) == 1000
# conversion_factor(W, W) == 1

conversion_factor(A::Type{<:Unit},::Type{SealedUnit{U,s}}) where {U,s} = conversion_factor(A, U)*s
conversion_factor(A::Type{SealedUnit{U,s}},B::Type{<:Unit}) where {U,s} = conversion_factor(U, B)/s
conversion_factor(::Type{SealedUnit{A,a}},::Type{SealedUnit{B,b}}) where {A,B,a,b} = conversion_factor(A, B)*(b/a)

"Convert to the most precise type possible"
precise(n::Number) = n
precise(n::AbstractFloat) = rationalize(n)

value(u::Unit) = precise(u.value)

"Convert derived dimensions into plain dimensions where possible"
simplify(::Type{Exponent{1,T}}) where T = T
simplify(::Type{Exponent{0,T}}) where T = Real
simplify(::Type{T}) where T<:Exponent = T
simplify(::Type{T}) where T<:BaseUnit = T

Base.show(io::IO, c::Combination) = begin
  write(io, seperate(c.value), abbr(typeof(c)))
  nothing
end

Base.round(u::Unit) = typeof(u)(round(u.value))

"Formats long numbers with commas seperating it into chunks"
seperate(n::Number; kwargs...) = seperate(string(convert(isinteger(n) ? Int : Float64, n)), kwargs...)
seperate(str::String, sep = ",", k = 3) = begin
  parts = split(str, '.')
  str = parts[1]
  n = length(str)
  groups = (str[max(x-k+1, 1):x] for x in reverse(n:-k:1))
  whole_part = join(groups, sep)
  length(parts) == 1 ? whole_part : join([whole_part,  parts[2]], '.')
end

Base.show(io::IO, e::Exponent{d,U}) where {d,U} = begin
  write(io, seperate(e.value))
  if hasmethod(abbr, Tuple{Type{typeof(e)}})
    write(io, abbr(typeof(e)))
  elseif d < 0
    write(io, '/', abbr(U))
    d < -1 && write(io, exponents[Int(abs(d))])
  else
    write(io, abbr(U))
    d > 1 && write(io, exponents[Int(d)])
  end
end

Base.show(io::IO, t::Unit) = begin
  write(io, seperate(t.value))
  write(io, abbr(typeof(t)))
  nothing
end

Base.abs(::Type{Exponent{n,T}}) where {n,T} = Exponent{abs(n), T}
Base.exponent(::Type{T}) where T<:Unit = 1
Base.exponent(::Type{E}) where E<:Exponent = parameters(E)[1]

"""
Get a units abstract type

```julia
abstract_unit(m²) == Area
abstract_unit(m) == Length
```
"""
abstract_unit(::Type{Exponent{n,T}}) where {n,T} = Exponent{n,<:abstract_unit(T)}
abstract_unit(::Type{<:Combination{T,n}}) where {T,n} = Combination{<:Tuple{map(abstract_unit, T.parameters)...}, n}
abstract_unit(U::Type{SIUnit{T}}) where T = U
abstract_unit(::Type{T}) where T<:BaseUnit = begin
  supertype(T) == BaseUnit ? abstract_type(T) : abstract_unit(supertype(T))
end
abstract_unit(U::UnionAll) = U
# abstract_unit(m²) == Exponent{2,<:Length}
# abstract_unit(m/s) == Combination{<:Tuple{Exponent{1,<:Length},Exponent{-1,<:Time}},0}
# abstract_unit(m) == Length
# abstract_unit(s) == Time

"""
Find the BaseUnit a unit was derived from

```julia
baseunit(m²) == Length
baseunit(m) == Length
```
"""
baseunit(u::Unit) = baseunit(typeof(u))
baseunit(::Type{SIUnit{T}}) where T = SIUnit{T}
baseunit(::Type{T}) where T<:BaseUnit = supertype(T) == BaseUnit ? abstract_type(T) : baseunit(supertype(T))
baseunit(::Type{T}) where T<:Exponent = begin
  if T isa UnionAll
    T.body.parameters[2].ub
  else
    baseunit(T.parameters[2])
  end
end
# baseunit(Length^1) == Length
# baseunit(Meter{0}^1) == Length
baseunit(::Type{C}) where C<:Combination = begin
  Combination{Tuple{map(T->Exponent{exponent(T), baseunit(T)}, params(C))...}, magnitude(C)}
end
# baseunit(m/s) == Combination{Tuple{Exponent{1,Length},Exponent{-1,Time}}, 0}

"Convert to a UnionAll if its a parametric DataType"
abstract_type(T::UnionAll) = T
abstract_type(T::DataType) = length(T.parameters) == 0 ? T : T.name.wrapper

# convert(Real, 1km) == 1000
Base.convert(::Type{N}, u::U) where {N<:Real,U<:Unit} = convert(N, u.value * basefactor(U))
# convert(km, 1) == 1km
Base.convert(::Type{U}, n::Real) where U<:Unit = U(n)
# convert(km, 1000m) == 1km
# convert(day^-1, 1/year) == (1/365.2425)day^-1
Base.convert(::Type{B}, a::A) where {A<:Unit,B<:Unit} = B(value(a) * conversion_factor(B, A))
Base.convert(::Type{Combination}, b::Unit) = tocombination(typeof(b))(b.value)

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

# 1/m == (m^-1)(1)
Base.:/(a::Number, B::Type{<:Unit}) = simplify(Combination{Tuple{inv(B)},0})(a)

toexponent(::Type{E}) where E<:Exponent = unionall(E)
toexponent(::Type{D}) where D<:BaseUnit = unionall(Exponent{1,D})
tocombination(::Type{A}) where A<:Combination = A
tocombination(::Type{A}) where A<:Unit = Combination{Tuple{toexponent(A)},0}
Base.inv(::Type{D}) where D<:BaseUnit = unionall(Exponent{-1,D})
Base.inv(::Type{E}) where E<:Exponent =
  if E isa UnionAll
    d, var = E.body.parameters
    UnionAll(var, Exponent{-d, var})
  else
    d, T = E.parameters
    Exponent{-d, T}
  end
# Exponent{1,Second{1//1}} <: Exponent{1,<:Time}
unionall(E::Type{Exponent{n,T}}) where {n,T} = (T isa DataType && T.abstract) || T isa UnionAll ? Exponent{n,<:T} : E
unionall(U::UnionAll) = U

# m^2 == m²
Base.:^(::Type{U}, n::Integer) where U<:Unit = unionall(Exponent{n,U})
# m²^2 == m^4
Base.:^(::Type{Exponent{d,T}}, n::Integer) where {d,T} = unionall(Exponent{d*n,T})
# (1m²)^2 == 1m^4
Base.:^(u::Exponent{d,T}, n::Integer) where {d,T} = Exponent{d*n,T}(value(u) ^ n)


Base.convert(::Type{Exponent}, d::BaseUnit) = Exponent{1,typeof(d)}(d.value)
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

struct Second{factor} <: Time value::Real end

const time_factors = Dict{Rational,Symbol}(60 => :minute,
                                           3600 => :hr,
                                           86400 => :day,
                                           604800 => :week,
                                           2629746 => :month,
                                           31556952 => :year)

# abbr(Second{-1000_000_000_000//1}) == "ps"
abbr(::Type{Second{f}}) where f =
  String(get(time_factors, f, string(get(prefix, -round(Int, log(10, abs(f))), ""), 's')))
basefactor(::Type{Second{f}}) where f = f
# promote(1s, 1hr) == (1s, 3600s)
Base.promote_rule(::Type{Second{f1}},::Type{Second{f2}}) where {f1,f2} = Second{min(f1,f2)}

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
Base.promote_rule(::Type{Gram{a}}, ::Type{Gram{b}}) where {a,b} = Gram{min(a,b)}

for λ ∈ (:<, :>, :(==), :isless)
  @eval begin
    # 1g < 2g
    Base.$λ(a::T,b::T) where T<:Unit = $λ(value(a), value(b))
    # 1kg > 2g
    Base.$λ(a::Unit,b::Unit) = begin
      @assert baseunit(a) == baseunit(b)
      $λ(promote(a,b)...)
    end
    # 1kg > 2
    Base.$λ(a::Real,b::Unit) = $λ(a, convert(Real, b))
    # 1 < 1kg
    Base.$λ(a::Unit,b::Real) = $λ(convert(Real, a), b)
  end
end

params(U::UnionAll) = sort_params(U.body.parameters[1].ub.parameters)
params(::Type{Combination{T,m}}) where {T<:Tuple,m} = sort_params(T.parameters)
params(::Type{E}) where E<:Exponent = sort_params(Core.svec(E))
params(::Type{D}) where D<:BaseUnit = sort_params(Core.svec(Exponent{1,D}))
# simplify(Combination{Tuple{m^2,s^0},0}) == m^2
simplify(::Type{Combination{Tuple{T}, m}}) where {T,m} = simplify(T)
simplify(::Type{C}) where C<:Combination = begin
  p = collect(Iterators.filter(E->exponent(E) != 0, params(C)))
  length(p) == 0 && return Real
  length(p) == 1 && return simplify(p[1])
  Combination{Tuple{p...}, magnitude(C)}
end

# promote(1m/s, 9km/hr) == (1m/s, 2.5m/s)
Base.promote_rule(::Type{A}, ::Type{B}) where {A<:Combination,B<:Combination} = begin
  Combination{Tuple{map(promote_type, params(A), params(B))...}, min(magnitude(A), magnitude(B))}
end
# (60m/s) / (1°/minute) == 3600m/°
# (60m/s) / (1/minute) == 3600m
for op in (:*, :/)
  @eval Base.$op(a::Unit, b::Unit) = $op(convert(Combination, a), convert(Combination, b))
  @eval Base.$op(a::Unit, b::Combination) = $op(convert(Combination, a), b)
  @eval Base.$op(a::Combination, b::Unit) = $op(a, convert(Combination, b))
  @eval Base.$op(a::A, b::B) where {A<:Combination, B<:Combination} = begin
    T = $op(A, B)
    params_a, params_b = (params(A), params(B))
    value_a, value_b = (value(a), value(b))
    for EA in params_a
      D = baseunit(EA)
      i = findfirst(E->baseunit(E) == D, params_b)
      i == nothing && continue
      EB = params_b[i]
      da,TA = EA.parameters
      db,TB = EB.parameters
      T0 = promote_type(TA, TB)
      value_a *= basefactor(EA)/basefactor(Exponent{da,T0})
      value_b *= basefactor(EB)/basefactor(Exponent{db,T0})
    end
    T($op(value_a, value_b))
  end
end
# 1m/s == (m/s)(1)
# 1m/s^2 == (m/s^2)(1)
# 1m²/s^2 == (m²/s^2)(1)
# m * m == m² && m * cm == cm²
# m^1 * m² == m³
# m * m² == m³
# 1m² * 2m² == 2m^4
# 5s * (1m/s) == 5m
# 1m * 2m == 2m²
# 1minute * (1m/s) == 60m
# 3g * (1000m/kg) == 3m
# 1000_000_000mm³ * (2.5ton/m³) == 2.5ton
# Combination{Tuple{s^1},0}(12) * Combination{Tuple{km^1, minute^-1},0}(1) == (1//5)km
# 1mm² * 2cm^1 == 20mm³
# 2m²/1m² == 2
# 1s/5s == 1//5
# 1m/5s == 0.2m/s
# 1.1s/1m² == 1.1s/m²
# (1.1s^2)/1m == 1.1s^2/m && 1m²/2m == 0.5m
# 1m²/200cm² == 50 && 1m³/200cm² == 5000cm
# (2m^4)/(2m²) == 1m²
# (1.1s^2)/1m² == (1.1s^2)/m²
# 1kg/m * 1cm == 0.01kg
# m/s == Combination{Tuple{m^1,s^-1},0}
# s/m² == Combination{Tuple{s^1,m^-2},0}
# s^2/m² == Combination{Tuple{s^2,m^-2},0}
# Combination{Tuple{s^1},0} * Combination{Tuple{s^2},0} == s^3
# Combination{Tuple{s^2},0}-Combination{Tuple{m^2},0} == Combination{Tuple{s^2,m^-2},0}
for op in (:+, :-, :*, :/)
  @eval Base.$op(::Type{A}, ::Type{B}) where {A<:Unit, B<:Unit} = $op(tocombination(A), tocombination(B))
  @eval Base.$op(::Type{A}, ::Type{B}) where {A<:Combination, B<:Combination} = begin
    params_a = params(A)
    params_b = params(B)
    dimensions = union(map(baseunit, params_a), map(baseunit, params_b))
    units = map(dimensions) do D
      ia = findfirst(E->baseunit(E) == D, params_a)
      ib = findfirst(E->baseunit(E) == D, params_b)
      if ib == nothing
        params_a[ia]
      elseif ia == nothing
        $(op == :- || op == :/ ? inv : identity)(params_b[ib])
      else
        d1,TA = parameters(params_a[ia])
        d2,TB = parameters(params_b[ib])
        d = $(op == :* ? (+) : (-))(d1, d2)
        if !isabstract(TA) && !isabstract(TB)
          Exponent{d, promote_type(TA,TB)}
        else
          Exponent{d, <:ub(TA)}
        end
      end
    end
    combine(units, magnitude(A))
  end
  @eval Base.$op(a::A, ::Type{B}) where {A<:Unit,B<:Unit} = $op(A,B)(a.value)
  @eval Base.$op(::Type{A}, b::B) where {A<:Unit,B<:Unit} = $op(A,B)(b.value)
end

combine(types, m) = begin
  p = sort_params(collect(Iterators.filter(E->exponent(E) != 0, types)))
  length(p) == 0 && return Real
  length(p) == 1 && return simplify(magnitude(p[1], m))
  if any(isabstract, p)
    Combination{<:Tuple{p...}, m}
  else
    Combination{Tuple{p...}, m}
  end
end

parameters(D::DataType) = D.parameters
parameters(U::UnionAll) = parameters(U.body)
ub(D::DataType) = D
ub(D::TypeVar) = D.ub
ub(D::UnionAll) = D.body
isabstract(::Union{TypeVar,UnionAll}) = true
isabstract(D::DataType) = D.hasfreetypevars

@eval macro $:export(e)
  quote
    export $(esc(e.args[1]))
    $(esc(Expr(:const, e)))
  end
end

@export Area = Length^2
@export Volume = Length^3
@export Pressure = Mass/Area
# m/s <: Speed
@export Speed = Length/Time
# m/s^2 <: Acceleration
@export Acceleration = Speed/Time
@export Jerk = Acceleration/Time

# convert(s, 1ns) == 1e-9s
for (factor,name) in time_factors
  @eval @export $name = Second{$factor}
end
for mag in (-3, -6, -9, -12)
  name = Symbol(get(prefix, mag, ""), 's')
  @eval @export $name = Second{$((10//1)^mag)}
end
@export s = Second{1//1}

# define mm, km etc...
for mag in (3, 0, -2, -3, -6, -9)
  name = Symbol(get(prefix, mag, ""), 'm')
  @eval @export $name = Meter{$mag}
  @eval @export $(Symbol(name, '²')) = Area{$name}
  @eval @export $(Symbol(name, '³')) = Volume{$name}
end
@export litre = Volume{Meter{-1}}
@export hectare = Area{Meter{2}}

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

struct Ampere <: Current value::Real end
abbr(::Type{Ampere}) = "A"

@export Amp = Ampere
@export Joule = kg*m²/s^2
@export J = Joule
@export MJ = magnitude(J, 6)
@export Newton = kg*m/s^2
@export KN = magnitude(Newton, 3)
@export Watt = J/s
@export W = Watt
@export mW = magnitude(W, -3)
@export kW = magnitude(W, 3)
@export MW = magnitude(W, 6)
@export Wh = scale(J, 3600)
@export kWh = magnitude(Wh, 3)
@export MWh = magnitude(Wh, 6)

abbr(::Type{Wh}) = "Wh"
abbr(::Type{kWh}) = "kWh"
abbr(::Type{MWh}) = "MWh"
abbr(::Type{J}) = "J"
abbr(::Type{MJ}) = "MJ"

@export Pascal = Newton/m²
@export Hertz = inv(s)
@export Coulomb = Amp*s
@export Volt = Joule/Coulomb
@export V = Volt
@export Ohm = Volt/Amp
@export gravity = 9.80665m/s^2 # http://physics.nist.gov/cgi-bin/cuu/Value?gn

export Length, Mass, Time, Angle, Temperature
