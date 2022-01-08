@use "github.com/jkroso/Prospects.jl" ismethod

const Units = @__MODULE__()
macro defunit(typ, short_name)
  if typ isa Symbol
    type_name = typ
    super = :($Units.BaseUnit)
  else
    type_name = typ.args[1]
    super = esc(typ.args[2])
  end
  T = esc(type_name)
  if Meta.isexpr(short_name, :call, 3) && short_name.args[1] == :*
    @assert Meta.isexpr(short_name.args[2], :hcat) || Meta.isexpr(short_name.args[2], :vect)
    abbrev = short_name.args[3]
    magnitudes = short_name.args[2].args
  else
    abbrev = short_name
    magnitudes = Any[]
  end
  magnitudes = map(magnitudes) do m
    for (n, sym) in prefix
      sym == m && return :(const $(esc(Symbol(m, abbrev))) = $T{$n})
    end
    error("unknown prefix $m")
  end
  quote
    Base.@__doc__ struct $T{magnitude} <: $super value::Number end
    $Units.magnitude(::Type{$T{m}}) where m = m
    $Units.abbr(::Type{$T{m}}) where m = string(get(prefix, m, ""), $(string(abbrev)))
    Base.promote_rule(::Type{$T{m1}},::Type{$T{m2}}) where {m1,m2} = $T{min(m1,m2)}
    const $(esc(abbrev)) = $T{0}
    $(magnitudes...)
  end
end

@eval macro $:export(e)
  quote
    export $(esc(e.args[1]))
    $(esc(Expr(:const, e)))
  end
end

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
  @eval export $(Symbol(u))
end

abstract type DerivedUnit <: Unit end

elt(a,b) = begin
  same_sign = exponent(a) < 0 == exponent(b) < 0
  same_sign ? sort_value(a) < sort_value(b) : exponent(a) < exponent(b)
end
sort_params(s) = begin
  out = Vector{Any}(undef, length(s))
  for i in 1:length(s)
    out[i] = s[i]
  end
  out
  sort!(out, lt=elt, rev=true)
end
sort_value(T::Type{BaseUnit}) = 99
sort_value(T::Type{Unit}) = 100
sort_value(T::Type{SIUnit{S}}) where S = S
sort_value(T::Type{<:DerivedUnit}) = sort_value(supertype(T))

"Represents units like m²"
struct Exponent{dimensions,D<:BaseUnit} <: DerivedUnit value::Number end

"Represents units like m/s and N·m"
struct Combination{D<:Tuple{Vararg{Exponent}}, magnitude} <: DerivedUnit
  value::Number
end

params(U::UnionAll) = sort_params(U.body.parameters[1].ub.parameters)
params(::Type{Combination{T,m}}) where {T<:Tuple,m} = sort_params(T.parameters)
params(::Type{E}) where E<:Exponent = sort_params(Core.svec(E))
params(::Type{D}) where D<:BaseUnit = sort_params(Core.svec(Exponent{1,D}))

Base.promote_rule(::Type{A}, ::Type{B}) where {A<:Combination,B<:Combination} =
  Combination{Tuple{map(promote_type, params(A), params(B))...}, min(magnitude(A), magnitude(B))}

"""
Enables you to treat derived units as if they were base units and provides the
ability to define a scale factor which is necessary for representing units like
`Wh`
"""
struct SealedUnit{U<:Combination, scale} <: BaseUnit
  value::Number
end

seal(u::U) where U<:Unit = SealedUnit{tocombination(U),1}(u.value)
scale(::Type{SealedUnit{U,s}}) where {U,s} = s
scale(::Type{SealedUnit{U,s}}, s2) where {U,s} = SealedUnit{U, s2}
scale(::Type{U}, s) where U<:Combination = SealedUnit{U, s}

Base.div(a::Unit, b::Unit) = Int(floor(a/b))
Base.rem(a::Unit, b::Unit) = a-(floor(a/b)*b)
Base.floor(a::Unit) = typeof(a)(floor(a.value))
Base.ceil(a::Unit) = typeof(a)(ceil(a.value))
Base.round(A::Type{<:Unit}, a::Unit) = round(convert(A, a))
Base.abs(::Type{Exponent{n,T}}) where {n,T} = Exponent{abs(n), T}
Base.exponent(::Type{T}) where T<:Unit = 1
Base.exponent(::Type{E}) where E<:Exponent = parameters(E)[1]
Base.round(u::Unit; kwargs...) = typeof(u)(round(u.value; kwargs...))

"""
Returns the shorthand notation for a given unit type
"""
function abbr end
abbr(::Type{SealedUnit{U,s}}) where {U,s} = s == 1 ? abbr(U) : "$(abbr(U)) × $s"
abbr(::Type{Exponent{n,T}}) where {n,T} = begin
  n == 1 && return abbr(T)
  n == 0 && return string(abbr(T), '⁰')
  n > 1 && return string(abbr(T), exponents[Int(n)])
  string(abbr(T), '⁻', exponents[Int(abs(n))])
end
abbr(::Type{C}) where C<:Combination = begin
  p, m = parameters(C)
  if m == 0
    units = [p.parameters[i] for i in 1:length(p.parameters)]
    str = sprint(abbr_params, units)
    str[nextind(str, 0, 2):end]
  else
    string(get(prefix, m, "e$m")) * abbr(magnitude(C, 0))
  end
end
abbr_params(io, params) =
  for T in params
    d, ET = T.parameters
    d == 0 && continue
    print(io, d > 0 ? '·' : '/', abbr(ET))
    d = abs(d)
    d > 1 && print(io, exponents[d])
  end

"""
Get/set the magnitude of a Unit

`magnitude(km) = 3`
`magnitude(km) == magnitude(m, 3)`
"""
magnitude(::Any) = 0
magnitude(E::Type{<:Exponent}) = begin
  if E isa UnionAll
    magnitude(E.body.parameters[2])
  else
    magnitude(E.parameters[2])
  end
end
magnitude(T::Type{<:Combination}) = begin
  if T isa UnionAll
    T.body.parameters[2]
  else
    T.parameters[2]
  end
end
magnitude(u::UnionAll) = magnitude(u.body)
magnitude(u::TypeVar)= magnitude(u.ub)

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

conversion_factor(A::Type{<:Unit},::Type{SealedUnit{U,s}}) where {U,s} = conversion_factor(A, U)*s
conversion_factor(A::Type{SealedUnit{U,s}},B::Type{<:Unit}) where {U,s} = conversion_factor(U, B)/s
conversion_factor(::Type{SealedUnit{A,a}},::Type{SealedUnit{B,b}}) where {A,B,a,b} = conversion_factor(A, B)*(b/a)
conversion_factor(::Type{A}, ::Type{B}) where {A<:BaseUnit,B<:BaseUnit} = begin
  @assert typejoin(A, B) != BaseUnit "can't convert $B to $A"
  basefactor(B)/basefactor(A)
end
conversion_factor(::Type{A}, ::Type{B}) where {d1,d2,TA,TB,A<:Exponent{d1,TA},B<:Exponent{d2,TB}} = begin
  @assert typejoin(TA, TB) != BaseUnit "can't convert $TB to $TA"
  @assert d1 == d2 "$A needs to have the same exponent count as $B"
  basefactor(B)/basefactor(A)
end
conversion_factor(::Type{A}, ::Type{B}) where {A<:Combination,B<:Combination} = begin
  pa, pb = (params(A), params(B))
  @assert length(pa) == length(pb) "$B is not equivelent to $A"
  f = foldl((value, p)->value * conversion_factor(p[1], p[2]), zip(pa, pb), init=1)
  ma, mb = magnitude(A), magnitude(B)
  ma == mb && return f
  f * (10//1)^-(ma-mb)
end

"Convert to the most precise type possible"
precise(n::Number) = n
precise(n::AbstractFloat) = rationalize(n)
value(u::Unit) = precise(u.value)

"Convert derived dimensions into plain dimensions where possible"
simplify(::Type{Exponent{1,T}}) where T = T
simplify(::Type{Exponent{0,T}}) where T = Real
simplify(::Type{T}) where T<:Exponent = T
simplify(::Type{T}) where T<:BaseUnit = T
simplify(::Type{Combination{Tuple{T}, m}}) where {T,m} = simplify(T)
simplify(::Type{C}) where C<:Combination = begin
  p = collect(Iterators.filter(E->exponent(E) != 0, params(C)))
  length(p) == 0 && return Real
  length(p) == 1 && return simplify(p[1])
  Combination{Tuple{p...}, magnitude(C)}
end

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

Base.show(io::IO, t::Unit) = (write(io, seperate(t.value), abbr(typeof(t))); nothing)
Base.show(io::IO, t::Combination) = begin
  T = typeof(t)
  p, m = T.parameters
  if m != 0 || ismethod(abbr, (T,))
    write(io, seperate(t.value), abbr(T))
  else
    units = [p.parameters[i] for i in 1:length(p.parameters)]
    show(io, simplify(units[1])(value(t)))
    abbr_params(io, units[2:end])
  end
  nothing
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

"""
Get a units abstract type

```julia
abstract_unit(m²) == Area
abstract_unit(m) == Length
```
"""
abstract_unit(U::UnionAll) = U
abstract_unit(::Type{Exponent{n,T}}) where {n,T} = Exponent{n,<:abstract_unit(T)}
abstract_unit(::Type{<:Combination{T,n}}) where {T,n} = Combination{<:Tuple{map(abstract_unit, T.parameters)...}, n}
abstract_unit(U::Type{SIUnit{T}}) where T = U
abstract_unit(::Type{T}) where T<:BaseUnit =
  supertype(T) == BaseUnit ? abstract_type(T) : abstract_unit(supertype(T))

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
baseunit(::Type{T}) where T<:Exponent =
  if T isa UnionAll
    T.body.parameters[2].ub
  else
    baseunit(T.parameters[2])
  end
baseunit(::Type{C}) where C<:Combination =
  Combination{Tuple{map(T->Exponent{exponent(T), baseunit(T)}, params(C))...}, magnitude(C)}

"Convert to a UnionAll if its a parametric DataType"
abstract_type(T::UnionAll) = T
abstract_type(T::DataType) = length(T.parameters) == 0 ? T : T.name.wrapper

Base.convert(::Type{N}, u::U) where {N<:Real,U<:Unit} = convert(N, u.value * basefactor(U))
Base.convert(::Type{U}, n::Real) where U<:Unit = U(n)
Base.convert(::Type{B}, a::A) where {A<:Unit,B<:Unit} = B(value(a) * conversion_factor(B, A))
Base.convert(::Type{Combination}, b::Unit) = tocombination(typeof(b))(b.value)
Base.convert(::Type{Exponent}, d::BaseUnit) = Exponent{1,typeof(d)}(d.value)
Base.convert(::Type{Exponent}, u::Exponent) = u
Base.:*(n::Real, ::Type{T}) where T<:Unit = T(n)

for op in (:*, :/, :+, :-)
  @eval Base.$op(n::Real, u::T) where T<:Unit = T($op(value(u), precise(n)))
  @eval Base.$op(u::T, n::Real) where T<:Unit = T($op(value(u), precise(n)))
end

for op in (:+, :-)
  @eval Base.$op(a::T, b::T) where T<:Unit = T($op(value(a), value(b)))
end

Base.:-(a::T) where T<:Unit = T(-(value(a)))
Base.:/(a::Number, B::Type{<:Unit}) = simplify(Combination{Tuple{inv(B)},0})(a)
Base.:^(::Type{U}, n::Integer) where U<:Unit = unionall(Exponent{n,U})
Base.:^(::Type{Exponent{d,T}}, n::Integer) where {d,T} = unionall(Exponent{d*n,T})
Base.:^(u::Exponent{d,T}, n::Integer) where {d,T} = Exponent{d*n,T}(value(u) ^ n)
Base.sqrt(s::Exponent{d,T}) where {d,T} = begin
  n = Int(d/2)
  n == 1 ? T(sqrt(value(s))) : Exponent{n,T}(sqrt(value(s)))
end

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
Base.inv(::Type{C}) where C<:Combination = combine(map(inv, params(C)), magnitude(C))
unionall(E::Type{Exponent{n,T}}) where {n,T} = (T isa DataType && isabstracttype(T)) || T isa UnionAll ? Exponent{n,<:T} : E
unionall(U::UnionAll) = U

Base.promote_rule(::Type{Exponent{d,TA}}, ::Type{Exponent{d,TB}}) where {d,TA,TB} =
  Exponent{d,promote_type(TA,TB)}

@defunit Meter <: Length [μ n m c k]m
@defunit Gram <: Mass [k]g
abbr(::Type{Gram{6}}) = "ton"

struct Second{factor} <: Time value::Real end
const time_factors = Dict{Rational,Symbol}(60 => :minute,
                                           3600 => :hr,
                                           86400 => :day,
                                           604800 => :week,
                                           2629746 => :month,
                                           31556952 => :year)
abbr(::Type{Second{f}}) where f =
  String(get(time_factors, f, string(get(prefix, magnitude(f), ""), 's')))
magnitude(n::Real) = n == 0 ? 0 : round(Int, n > 0 ? log10(n) : -log10(abs(n)))
basefactor(::Type{Second{f}}) where f = f
Base.promote_rule(::Type{Second{f1}},::Type{Second{f2}}) where {f1,f2} = Second{min(f1,f2)}

struct Degree <: Angle value::Real end
struct Radian <: Angle value::Real end
basefactor(::Type{Degree}) = π/180
abbr(::Type{Degree}) = "°"
abbr(::Type{Radian}) = "rad"
Base.promote_rule(::Type{<:Angle}, ::Type{<:Angle}) = Radian
Base.convert(::Type{Radian}, d::Degree) = Radian(d.value * basefactor(Degree))
Base.convert(::Type{Degree}, r::Radian) = Degree(r.value / basefactor(Degree))

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

for λ ∈ (:<, :>, :(==), :isless)
  @eval begin
    Base.$λ(a::Real,b::Unit) = $λ(a, convert(Real, b))
    Base.$λ(a::Unit,b::Real) = $λ(convert(Real, a), b)
    Base.$λ(a::T,b::T) where T<:Unit = $λ(value(a), value(b))
    Base.$λ(a::Unit,b::Unit) = begin
      @assert baseunit(a) == baseunit(b)
      $λ(promote(a,b)...)
    end
  end
end

scaled_value(x) = value(x)
scaled_value(x::Combination{T,m}) where {T,m} = value(x) * 10^(m//1)

for op in (:*, :/)
  @eval Base.$op(a::Unit, b::Unit) = $op(convert(Combination, a), convert(Combination, b))
  @eval Base.$op(a::Unit, b::Combination) = $op(convert(Combination, a), b)
  @eval Base.$op(a::Combination, b::Unit) = $op(a, convert(Combination, b))
  @eval Base.$op(a::A, b::B) where {A<:Combination, B<:Combination} = begin
    T = $op(A, B)
    params_a, params_b = (params(A), params(B))
    value_a, value_b = (scaled_value(a), scaled_value(b))
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
isabstract(D::DataType) = !isempty(D.parameters) && any(x->x isa TypeVar, D.parameters)

@export Area = Length^2
@export Volume = Length^3
@export Pressure = Mass/Area
@export Density = Mass/Volume
@export Speed = Length/Time
@export Acceleration = Speed/Time
@export Jerk = Acceleration/Time

for (factor,name) in time_factors
  @eval @export $name = Second{$factor}
end

for mag in (-3, -6, -9, -12)
  name = Symbol(get(prefix, mag, ""), 's')
  # be nice to lesser computers
  @eval @export $name = Second{$(Base.Sys.WORD_SIZE < 64 && mag == -12 ? 1//1000000000000 : (10//1)^mag)}
end
@export s = Second{1//1}
Base.sleep(duration::Second) = sleep(convert(s, duration).value)

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

@export A = Ampere
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
@export bar = scale(Pascal, 100_000)
@export mbar = magnitude(bar, -3)
abbr(::Type{bar}) = "bar"
abbr(::Type{mbar}) = "mbar"

@export Hertz = inv(s)
@export Hz = Hertz
abbr(::Type{Hz}) = "Hz"
@export Coulomb = A*s
@export Volt = Joule/Coulomb
@export V = Volt
@export Ohm = Volt/A
@export gravity = 9.80665m/s^2 # http://physics.nist.gov/cgi-bin/cuu/Value?gn

abbr(::Type{W}) = "W"
abbr(::Type{V}) = "V"

abstract type Data <: BaseUnit end
@defunit Byte <: Data [k M G T]b
@defunit Bit <: Data [k M G T]bit
Base.convert(B::Type{Bit{m}}, b::Byte) where m = B(8*convert(Real, b)/basefactor(B))
Base.convert(B::Type{Byte{m}}, b::Bit) where m = B((convert(Real, b)/8)/basefactor(B))

struct Lumen <: Luminosity value::Real end
abbr(::Type{Lumen}) = "lm"
@export lm = Lumen
@export lx = Lumen/m²

"Represents scaled numbers like percent, and ppm"
abstract type Magnitude{m} <: Real end
Base.isinteger(m::Magnitude) = isinteger(convert(Float64, m))
magnitude(::Magnitude{m}) where m = m
magnitude(::Type{<:Magnitude{m}}) where m = m
value(m::Magnitude) = m.value * 10^(magnitude(m)//1)

"Represents percentages like 15%"
struct Percent <: Magnitude{-2} value::Real end
Percent(n::AbstractFloat) = Percent(rationalize(n))
struct ppm <: Magnitude{-6} value::Real end
struct ppb <: Magnitude{-9} value::Real end
struct ppt <: Magnitude{-12} value::Real end

Base.:*(a::Real, ::Type{M}) where M<:Magnitude = M(a)
Base.:*(a::M, b::M) where M<:Magnitude = M(value(a) * value(b))
Base.:+(a::M, b::M) where M<:Magnitude = M(a.value + b.value)
Base.:-(a::M, b::M) where M<:Magnitude = M(a.value - b.value)
for op in (:*, :/, :+, :-)
  @eval Base.$op(a::Real, b::Magnitude) = $op(a, value(b))
  @eval Base.$op(a::Magnitude, b::Real) = $op(value(a), b)
end
Base.:/(a::M, b::M) where M<:Magnitude = M(a.value / b.value)
Base.:/(a::Magnitude, B::Type{<:Unit}) = magnitude(inv(tocombination(B)), magnitude(a))(a.value)
Base.:/(M::Type{<:Magnitude}, B::Type{<:Unit}) = magnitude(inv(tocombination(B)), magnitude(M))

(p::Magnitude)(n::Real) = precise(n) * value(p)

abbr(::Type{M}) where M<:Magnitude = String(nameof(M))
abbr(::Type{Percent}) = "%"

Base.show(io::IO, p::Magnitude) = print(io, p.value, abbr(typeof(p)))
Base.show(io::IO, p::Percent) = print(io, Float64(p.value), abbr(typeof(p)))
Base.abs(m::M) where M<:Magnitude = M(abs(m.value))
Base.convert(::Type{T}, p::Magnitude) where T <: Real = p isa T ? p : convert(T, value(p))
Base.convert(::Type{M}, n::Real) where {m,M<:Magnitude{m}} = M(n/m)
Base.convert(::Type{M}, n::M) where M<:Magnitude = n
Base.promote_rule(::Type{Magnitude}, ::Type{B}) where B<:Real = Rational
Base.round(u::M; kwargs...) where M<:Magnitude = M(round(u.value; kwargs...))
