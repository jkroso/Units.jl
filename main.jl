@use "./utils.jl" get_param set_param seperate Magnitude exponents prefixs
@use "github.com/FluxML/MacroTools.jl" => MacroTools @capture
@use "github.com/jkroso/Prospects.jl" group mapcat

abstract type Unit <: Number end
abstract type Dimension <: Unit end
struct Exponent{dimension<:Unit, n} <: Unit
  value::Number
end
abstract type AbstractCombination{dimensions<:Tuple{Vararg{Exponent}}} <: Unit end
abstract type DerivedUnit{magnitude, dimensions} <: AbstractCombination{dimensions} end

"""
Used to represent composite types like `kg*m^2/s^3`. The seperation of dimensions from units
allows it to also be used to represent dimensionless units like `cm/m` while also keeping
track of the magnitude of each dimension. Which is useful in units like `kW*hr`
"""
struct Combination{dimensions<:Tuple{Vararg{Exponent}}, units<:Tuple{Vararg{Exponent}}} <: AbstractCombination{dimensions}
  value::Number
end

abstract type Time <: Dimension end
abstract type Length <: Dimension end
abstract type Mass <: Dimension end
abstract type Current <: Dimension end
abstract type Temperature <: Dimension end
abstract type Substance <: Dimension end
abstract type Luminosity <: Dimension end

"Get the concrete units for a given dimension"
get_units(D::Type{<:Dimension}, c::Combination) = get_units(D, typeof(c))
get_units(D::Type{<:Dimension}, C::Type{<:Combination}) = begin
  units = subunits(C)
  map(simplify, units[findall(x->abstract_dimension(x) <: D, units)])
end

function short_name end

const Units = @__MODULE__()
macro defunit(type, short_name)
  @capture type (name_ <: super_)|name_
  if isnothing(super) super = :($Units.Dimension) end
  T = esc(name)
  if Meta.isexpr(short_name, :call, 3) && short_name.args[1] == :*
    @assert Meta.isexpr(short_name.args[2], :hcat) || Meta.isexpr(short_name.args[2], :vect)
    abbrev = short_name.args[3]
    magnitudes = short_name.args[2].args
  else
    abbrev = short_name
    magnitudes = Any[]
  end
  quote
    Base.@__doc__ struct $(esc(:($name{magnitude} <: $super))) value::Number end
    $Units.short_name(::Type{<:$T}) = $(string(abbrev))
    $Units.scaler(::Type{$T{m}}) where m = m
    Base.promote_rule(::Type{$T{m1}}, ::Type{$T{m2}}) where {m1,m2} = $T{m1<m2 ? m1 : m2}
    const $(esc(abbrev)) = $T{Magnitude(0)}
    $(map(magnitudes) do m
      for (n, sym) in prefixs
        sym == m && return :(const $(esc(Symbol(m, abbrev))) = $T{Magnitude($n)})
      end
      error("unknown prefix $m")
    end...)
  end
end

macro abbreviate(name, def)
  quote
    const $(esc(name)) = $(esc(def))
    $Units.abbr(::Type{$(esc(name))}) = $(string(name))
  end
end

parameters(D::DataType) = D.parameters
parameters(U::UnionAll) = parameters(U.body)
parameters(V::TypeVar) = parameters(V.ub)
ub(D::DataType) = D
ub(D::TypeVar) = D.ub
ub(D::UnionAll) = D.body
isabstract(::Union{TypeVar,UnionAll}) = true
isabstract(D::DataType) = isabstracttype(D) || (!isempty(D.parameters) && any(x->x isa TypeVar, D.parameters))
wrap(T, e) = isabstract(T) ? Exponent{D, e} where D<:ub(T) : Exponent{T,e}

Base.inv(::Type{Exponent{d,n}}) where {d,n} = wrap(d, -n)
Base.inv(E::Type{<:Exponent}) = wrap(get_param(E, 1), -get_param(E, 2))
Base.inv(::Type{D}) where D<:Dimension = wrap(D, -1)
Base.inv(C::Type{<:Combination}) = map_combo(inv, C)
Base.:^(D::Type{<:Unit}, e) = wrap(D, e)
Base.:^(::Type{Exponent{d, e1}}, e2) where {d,e1} = wrap(d, e1 * e2)
Base.:^(E::Type{<:Exponent}, e) = wrap(get_param(E, 1), get_param(E, 2) * e)
Base.:^(C::Type{<:Combination}, e) = map_combo(d->d^e, C)
tounionall(U::UnionAll) = U
tounionall(U::DataType) = U.name.wrapper
Base.:/(U::Type{<:Unit}, n::Real) = begin
  D = tounionall(unwrap(U))
  m1 = convert(Magnitude, scaler(U))
  m2 = convert(Magnitude, n^(1//power(U)))
  simplify(wrap(D{m1/m2}, power(U)))
end
Base.:*(U::Type{<:Unit}, n::Real) = begin
  D = tounionall(unwrap(U))
  m1 = convert(Magnitude, scaler(U))
  m2 = convert(Magnitude, n^(1//power(U)))
  simplify(wrap(D{m1*m2}, power(U)))
end

map_combo(f, C::Type{<:Combination}) = begin
  dims, units = parameters(get_param(C, 1)), parameters(get_param(C, 2))
  Combination{Tuple{map(f, dims)...}, Tuple{map(f, units)...}}
end

Base.promote_rule(::Type{Exponent{d1,e}}, ::Type{Exponent{d2,e}}) where {d1,d2,e} = wrap(promote_type(d1, d2), e)
Base.promote_rule(A::Type{<:Combination}, B::Type{<:Unit}) = simplify(promote_rule(A, to_combo(B)))
Base.promote_rule(A::Type{<:Exponent}, B::Type{<:Dimension}) = promote_rule(A, wrap(B, 1))
Base.promote_rule(::Type{A}, ::Type{B}) where {A<:Combination,B<:Combination} = begin
  dims_a, dims_b = get_param(A, 1), get_param(B, 1)
  @assert dims_a == dims_b "dimension mismatch"
  Combination{dims_a, Tuple{map(promote_type, subunits(A), subunits(B))...}}
end

abbr(m::Magnitude) = string(get(prefixs, m.value, ""))
scaler(U::Type{<:Exponent{d}}) where d = scaler(d)
scaler(U::Type{<:Unit}) = 1
prefix(T::Type{<:Dimension}) = abbr(scaler(T))

abbr(T::Type{<:Dimension}) = prefix(T) * short_name(T)
abbr(::Type{Exponent{d,n}}) where {d,n} = begin
  n == 1 && return abbr(d)
  n >= 0 && return string(abbr(d), exponents[Int(n)+1])
  string(abbr(d), '⁻', exponents[Int(abs(n))+1])
end
abbr(T::Type{<:AbstractCombination}) = begin
  negative, positive = group(u->power(u) < 0, subunits(T))
  str = join(map(abbr, positive), '·')
  isempty(negative) && return str
  str * '/' * join(map(abbr ∘ inv, negative), '·')
end
abbr(::Type{D}) where {m,_,D<:DerivedUnit{m, _}} = abbr(m) * short_name(D)

Base.:-(n::Unit) = typeof(n)(-n.value)
Base.:*(n::Real, ::Type{T}) where T<:Unit = T(n)
Base.:/(n::Real, ::Type{T}) where T<:Unit = inv(T)(n)
Base.:^(u::Unit, n::Integer) = (typeof(u)^n)(u.value^n)
Base.convert(::Type{U}, n::Real) where U<:Unit = U(n)
Base.convert(::Type{N}, u::U) where {N<:Real,U<:Unit} = convert(N, u.value * basefactor(U))
Base.convert(::Type{U}, n::Unit) where U<:Unit = U(n.value * conversion_factor(typeof(n), U))
Base.convert(::Type{Combination}, d::D) where D<:Unit = to_combo(D)(d.value)
Base.show(io::IO, t::Unit) = (write(io, seperate(t.value), abbr(typeof(t))); nothing)
Base.isinteger(u::Unit) = isinteger(u.value)
Base.isapprox(a::Unit, b::Unit) = isapprox(map(x->x.value, promote(a, b))...)
Base.div(a::Unit, b::Unit) = Int(floor(a/b))
Base.rem(a::Unit, b::Unit) = a-(floor(a/b)*b)
Base.floor(a::Unit) = typeof(a)(floor(a.value))
Base.ceil(a::Unit) = typeof(a)(ceil(a.value))
Base.round(A::Type{<:Unit}, a::Unit) = round(convert(A, a))
Base.round(u::Unit; kwargs...) = typeof(u)(round(u.value; kwargs...))
Base.sqrt(s::Exponent{T,d}) where {d,T} = begin
  n = Int(d/2)
  n == 1 ? T(sqrt(s.value)) : wrap(T, n)(sqrt(s.value))
end

for λ in (:*, :/, :+, :-)
  @eval Base.$λ(n::Real, u::T) where T<:Unit = T($λ(n, u.value))
  @eval Base.$λ(u::T, n::Real) where T<:Unit = T($λ(u.value, n))
end

for λ in (:+, :-)
  @eval Base.$λ(a::T, b::T) where T<:Unit = T($λ(a.value, b.value))
  @eval Base.$λ(a::Unit, b::Unit) = $λ(promote(a, b)...)
end

for λ in (:<, :>, :(==), :isless)
  @eval Base.$λ(a::Real,b::Unit) = $λ(a, convert(Real, b))
  @eval Base.$λ(a::Unit,b::Real) = $λ(convert(Real, a), b)
  @eval Base.$λ(a::T,b::T) where T<:Unit = $λ(a.value, b.value)
  @eval Base.$λ(a::Unit,b::Unit) = begin
    @assert dimension(typeof(a)) == dimension(typeof(b))
    $λ(promote(a, b)...)
  end
end

Base.convert(::Type{T}, n::D) where {D<:Dimension, T<:Dimension} = T(n.value * conversion_factor(D, T))
basefactor(D::Type{<:Dimension}) = scaler(D)
basefactor(D::Type{<:DerivedUnit}) = scaler(D)
basefactor(E::Type{Exponent{d,e}}) where {d,e} = basefactor(d)^e
basefactor(C::Type{<:Combination}) = begin
  units = parameters(get_param(C, 2))
  mapreduce(basefactor, *, units, init=1)
end
conversion_factor(A::Type{<:Unit}, B::Type{<:Unit}) = conversion_factor(to_combo(A), to_combo(B))
conversion_factor(::Type{A}, ::Type{B}) where {A<:Dimension,B<:Dimension} = basefactor(A)/basefactor(B)
conversion_factor(::Type{A}, ::Type{B}) where {A<:Exponent,B<:Exponent} = basefactor(A)/basefactor(B)
conversion_factor(A::Type{<:AbstractCombination{DA}}, B::Type{<:AbstractCombination{DB}}) where {DA,DB} = begin
  (units_a, scaler_a), (units_b, scaler_b) = simple_units(A), simple_units(B)
  mapreduce(conversion_factor, *, units_a, units_b, init=scaler_a/scaler_b)
end

"unpack derived units, dedupe dimensions, and keep track of the resulting scale difference"
simple_units(C::Type{<:AbstractCombination}) = begin
  units, scale = flatten_units(C)
  dims = map(abstract_dimension, units)
  nulls = units[findall(==(NullDimension), dims)]
  scale *= mapreduce(scaler, *, nulls, init=1)
  output = map(unique!(filter(!=(NullDimension), dims))) do d
    like_units = units[findall(==(d), dims)]
    T = promote_type(map(unwrap, like_units)...)
    out = Exponent{T, mapreduce(power, +, like_units)}
    out, mapreduce(u->conversion_factor(u, Exponent{T,power(u)}), *, like_units)
  end
  filter!(!ispointless, map(first, output)), mapreduce(last, *, output) * scale
end

typebelow(child, stop) = begin
  while supertype(child).name.wrapper != stop; child = supertype(child) end
  child
end
abstract_dimension(E::Type{<:Exponent}) = abstract_dimension(unwrap(E))
abstract_dimension(D::Type{<:Dimension}) = typebelow(D, Dimension)
abstract_dimension(U::Type{<:AbstractCombination}) = typebelow(U, AbstractCombination)
abstract_dimension(U::Type{<:DerivedUnit}) = U.name.wrapper

dimension(D::Type{<:Dimension}) = wrap(abstract_dimension(D), 1)
dimension(D::Type{<:Exponent}) = wrap(abstract_dimension(unwrap(D)),power(D))
dimension(C::Type{<:Combination}) = begin
  dims = map(dimension, get_param(C, 1).parameters)
  AbstractCombination{dimensions} where dimensions<:Tuple{dims...}
end
unwrap(E::Type{<:Exponent}) = ub(get_param(E, 1))
unwrap(D::Type{<:Dimension}) = D
unwrap(D::Type{<:DerivedUnit}) = D

to_combo(D::Type{<:AbstractCombination}) = D
to_combo(D::Type{<:DerivedUnit}) = Combination{Tuple{dimensions(D)...}, Tuple{D^1}}
to_combo(D::Type{<:Dimension}) = to_combo(D^1)
to_combo(D::Type{<:Exponent}) = begin
  d, e = get_param(D, 1), get_param(D, 2)
  if isabstract(d)
    AbstractCombination{Dimensions} where Dimensions<:Tuple{D}
  else
    Combination{Tuple{dimension(D)}, Tuple{D}}
  end
end

subunits(::Type{Exponent{D,n}}) where {D<:DerivedUnit,n} = DataType[wrap(unwrap(x), power(x) * n) for x in subunits(D)]
subunits(D::Type{<:Exponent}) = DataType[D]
subunits(C::Type{<:Combination}) = parameters(parameters(C)[2])
subunits(D::Type{<:AbstractCombination}) = parameters(parameters(D)[1])
subunits(D::Type{<:DerivedUnit{m,units}}) where {m,units} = parameters(units)

flatten_units(C::Type{<:AbstractCombination}) where u = accum_units(map(flatten_units, parameters(get_param(C, 1))), 1)
flatten_units(::Type{<:Combination{d,u}}) where {d,u} = accum_units(map(flatten_units, parameters(u)), 1)
flatten_units(::Type{<:DerivedUnit{m,u}}) where {m,u} = accum_units(map(flatten_units, parameters(u)), m)
flatten_units(::Type{<:Exponent{d,e}}) where {d,e} = begin
  units, scale = flatten_units(d)
  map(u->wrap(unwrap(u),power(u)*e), units), scale
end
flatten_units(D::Type{<:Dimension}) = Any[D], 1
accum_units(results, scale) = begin
  reduce(results, init=(AnyType[],scale)) do out, (units, scale)
    push!(out[1], units...), out[2] * scale
  end
end

const AnyType = Union{UnionAll,DataType}

dimensions(D::Type{<:Dimension}) = AnyType[wrap(D, 1)]
dimensions(E::Type{<:Exponent{D,e}}) where {D<:Unit,e} = map(d->wrap(unwrap(d), power(d) * e), dimensions(D))
dimensions(C::Type{<:Combination}) = parameters(parameters(C)[1])
dimensions(C::Type{<:DerivedUnit{m,units}}) where {m,units} = map(dimension, units.parameters)
ispointless(d) = power(d) == 0
power(E::Type{<:Exponent}) = get_param(E, 2)
power(::Type{<:Unit}) = 1

"Reduces a complex unit to a simpler but equivelent one"
simplify(T::Type{<:Unit}) = T
simplify(E::Type{Exponent{d,n}}) where {d,n} = n == 1 ? d : E
simplify(C::Type{<:Combination}) = begin
  dims = Any[x for x in get_param(C, 1).parameters if !ispointless(x)]
  units = Any[x for x in get_param(C, 2).parameters if !ispointless(x)]
  isempty(units) && return Real
  length(units) == 1 && return simplify(units[1])
  Combination{Tuple{dims...}, Tuple{units...}}
end

"deduplicate units in a combination while also keeping track of the scale difference between input and output types"
prune(T::Type{<:Unit}) = T, 1
prune(::Type{Combination{Ds, units}}) where {Ds, units} = begin
  dims = map(abstract_dimension, units.parameters)
  isempty(dims) && return Combination{Tuple{}, Tuple{}}, 1
  output = map(unique(dims)) do d
    like_units = units.parameters[findall(==(d), dims)]
    T = promote_type(map(unwrap, like_units)...)
    out = Exponent{T, mapreduce(power, +, like_units)}
    out, mapreduce(u->conversion_factor(u, Exponent{T,power(u)}), *, like_units)
  end
  Combination{Ds, Tuple{filter!(!ispointless, map(first, output))...}}, mapreduce(last, *, output)
end

"Takes a Unit and a value and produces an instance of the simplest version of that type"
pruned(T, v) = begin
  T, scaler = prune(T)
  simplify(T)(v * scaler)
end

"""
Unpack derived units within a combination. Returns a simplified type along with a scaler which
is usefull if you a converting an instance of the input type to the simplified type
"""
unpack(::Type{Combination{Ds, units}}) where {Ds, units} = begin
  subs = mapreduce(subunits, vcat, units.parameters, init=Any[])
  isempty(subs) && return Combination{Tuple{}, Tuple{}}, 1
  abstract_dims = map(abstract_dimension, subs)
  output = map(unique(abstract_dims)) do d
    like_units = subs[findall(==(d), abstract_dims)]
    T = promote_type(map(unwrap, like_units)...)
    out = Exponent{T, mapreduce(power, +, like_units)}
    out, mapreduce(u->conversion_factor(u, Exponent{T,power(u)}), *, like_units)
  end
  simplify(Combination{Ds, Tuple{map(first, output)...}}), mapreduce(last, *, output)
end
unpacked(c::Combination) = begin
  T, scaler = unpack(typeof(c))
  T(c.value * scaler)
end

for λ in (:*, :/)
  @eval Base.$λ(a::A, ::Type{B}) where {A<:Unit,B<:Unit} = pruned($λ(A,B), a.value)
  @eval Base.$λ(::Type{A}, b::B) where {A<:Unit,B<:Unit} = pruned($λ(A,B), b.value)
  @eval Base.$λ(::Type{A}, ::Type{B}) where {A<:Unit,B<:Unit} = $λ(to_combo(A), to_combo(B))
  @eval Base.$λ(A::Type{<:DerivedUnit}, B::Type{<:DerivedUnit}) = $λ(to_combo(A), to_combo(B))
  @eval Base.$λ(A::Type{<:DerivedUnit}, B::Type{<:AbstractCombination}) = $λ(to_combo(A), B)
  @eval Base.$λ(A::Type{<:AbstractCombination}, B::Type{<:DerivedUnit}) = $λ(A, to_combo(B))
  @eval Base.$λ(A::Type{<:AbstractCombination}, B::Type{<:AbstractCombination}) = begin
    dims_a, dims_b = parameters(get_param(A, 1)), parameters(get_param(B, 1))
    Das, Dbs = map(abstract_dimension, dims_a), map(abstract_dimension, dims_b)
    Ds = map(union(Das, Dbs)) do D
      ia, ib = findfirst(==(D), Das), findfirst(==(D), Dbs)
      isnothing(ia) && return $(λ == :/ ? inv : identity)(dims_b[ib])
      isnothing(ib) && return dims_a[ia]
      wrap(D, $(λ == :* ? :+ : :-)(power(dims_a[ia]), power(dims_b[ib])))
    end
    AbstractCombination{Dimensions} where Dimensions<:Tuple{filter!(!ispointless, Ds)...}
  end
  @eval Base.$λ(A::Type{<:Combination}, B::Type{<:Combination}) = begin
    units_a, units_b = subunits(A), subunits(B)
    naked_a, naked_b = map(unwrap, units_a), map(unwrap, units_b)
    units = filter!(!ispointless, map(union(naked_a, naked_b)) do u
      ia,ib = findfirst(==(u), naked_a), findfirst(==(u), naked_b)
      isnothing(ia) && return $(λ == :/ ? inv : identity)(units_b[ib])
      isnothing(ib) && return units_a[ia]
      wrap(u, $(λ == :* ? :+ : :-)(power(units_a[ia]), power(units_b[ib])))
    end)
    dims = mapcat(dimensions, units)
    wrapped = map(unique!(map(abstract_dimension, dims))) do AD
      e = sum((power(u) for u in dims if abstract_dimension(u) == AD))
      wrap(AD, e)
    end
    combine(filter!(!ispointless, wrapped), units)
  end
  @eval Base.$λ(a::Unit, b::Unit) = $λ(convert(Combination, a), convert(Combination, b))
  @eval Base.$λ(a::Unit, b::Combination) = $λ(convert(Combination, a), b)
  @eval Base.$λ(a::Combination, b::Unit) = $λ(a, convert(Combination, b))
  @eval Base.$λ(a::A, b::B) where {A<:Combination, B<:Combination} = begin
    (UA, scaler_a), (UB, scaler_b) = prune(A), prune(B)
    pruned($λ(UA, UB), $λ(a.value * scaler_a, b.value * scaler_b))
  end
end

combine(dims, units) = begin
  length(units) == 1 && return simplify(units[1])
  Combination{Tuple{dims...}, Tuple{units...}}
end

const Energy = Mass * Length^2 / Time^2
const Power = Energy/Time
const Area = Length^2
const Volume = Length^3
const Pressure = Mass/Area
const Density = Mass/Volume
const Speed = Length/Time
const Acceleration = Speed/Time
const Jerk = Acceleration/Time
const Snap = Jerk/Time
const Crackle = Snap/Time
const Pop = Crackle/Time

@defunit Meter <: Length [μ c m k]m
const m² = m^2
const m³ = m^3
@abbreviate litre m³/1e3
@abbreviate ml litre/1e3
@abbreviate μl litre/1e6
@abbreviate hectare Area{Meter{Magnitude(2)}}
@defunit Gram <: Mass [μ m k]g
@abbreviate ton Gram*1e6
@defunit Ampere <: Current [m]A
@defunit Lumen <: Luminosity lm
@abbreviate lx lm/m²
@defunit Second <: Time [n m]s
abbr(S::Type{Second{m}}) where m = begin
  m isa Magnitude && return abbr(m) * "s"
  m == 1 && return "s"
  m == 60 && return "minute"
  m == 3600 && return "hr"
  m == 3600*24 && return "day"
  m == 3600*24*7 && return "week"
  m == 3600*24*365 && return "yr"
  m == 3600*24*365/12 && return "month"
  error("unknown magnitude $m")
end
scaler(::Type{Second{m}}) where m = exponentiable(m)
exponentiable(m::Magnitude) = m
exponentiable(m::Integer) = m//1
exponentiable(m) = m
const minute = Second{60}
const hr = Second{3600}
const day = Second{convert(Int, 24scaler(hr))}
const week = Second{convert(Int, 7scaler(day))}
const yr = Second{convert(Int, 365scaler(day))}
const month = Second{convert(Int, scaler(yr)/12)}
Base.sleep(duration::Second) = sleep(convert(Real, duration))

@defunit Kelvin <: Temperature K
@defunit Celsius <: Temperature °C
@defunit Fahrenheit <: Temperature °F
basefactor(::Type{Fahrenheit{m}}) where m = 5//9 * m
baseoffset(::Type{Kelvin{m}}) where m = 0//1
baseoffset(::Type{Fahrenheit{m}}) where m = rationalize(459.67)
baseoffset(::Type{Celsius{m}}) where m = rationalize(273.15)
Base.promote_rule(::Type{<:Temperature}, ::Type{<:Temperature}) = Kelvin
Base.convert(K::Type{<:Kelvin}, t::T) where T<:Union{Celsius,Fahrenheit} =
  K((t.value + baseoffset(T)) * basefactor(T))

abstract type Data <: Dimension end
@defunit Byte <: Data [k M G T]b
@defunit Bit <: Data [k M G T]bit
Base.convert(B::Type{Bit{m}}, b::Byte) where m = B(8convert(Real, b)/basefactor(B))
Base.convert(B::Type{Byte{m}}, b::Bit) where m = B((convert(Real, b)/8)/basefactor(B))

macro deriveunit(name, units, abbrev)
  N = esc(name)
  if Meta.isexpr(abbrev, :call)
    short = abbrev.args[3]
    magnitudes = abbrev.args[2].args
  else
    short = abbrev
    magnitudes = []
  end
  quote
    subunits, scale = flatten_units($(esc(units)))
    @assert scale == 1 "derived units should be defined with no scale"
    Base.@__doc__ struct $N{magnitude} <: DerivedUnit{magnitude, Tuple{subunits...}}
      value::Number
    end
    $Units.short_name(::Type{$N{m}}) where m = $(string(short))
    $Units.scaler(::Type{$N{m}}) where m = m
    Base.promote_rule(::Type{$N{m1}}, ::Type{$N{m2}}) where {m1,m2} = $N{m1<m2 ? m1 : m2}
    const $(esc(short)) = $N{Magnitude(0)}
    $(map(magnitudes) do m
      for (n, sym) in prefixs
        sym == m && return :(const $(esc(Symbol(m, short))) = $N{Magnitude($n)})
      end
      error("unknown prefix $m")
    end...)
  end
end

@deriveunit Joule kg*m²/s^2 [k M]J
@deriveunit Watt kg*m²/s^3 [m k M]W
@abbreviate Wh W*hr
@abbreviate kWh kW*hr
@deriveunit Newton kg*m/s^2 [k]N
@deriveunit Pascal N/m² [k M]Pa
@abbreviate bar Pascal*1e5
@abbreviate mbar bar/1e3
@deriveunit Coulomb A*s C
@deriveunit Volt J/C V
@abbreviate kV Volt*1e3
@deriveunit Ohm V/A Ω
@abbreviate Hz inv(s)
@abbreviate kHz inv(ms)
@abbreviate MHz inv(ns)

const gravity = 9.80665m/s^2 # http://physics.nist.gov/cgi-bin/cuu/Value?gn

abstract type NullDimension <: Dimension end
to_combo(U::Type{<:NullDimension}) = Combination{Tuple{},Tuple{wrap(U, 1)}}
dimensions(::Type{<:NullDimension}) = Any[]
dimension(::Type{<:NullDimension}) = wrap(NullDimension, 1)
abstract_dimension(::Type{<:NullDimension}) = NullDimension

struct ScalingUnit{magnitude} <: NullDimension
  value::Number
end
scaler(::Type{ScalingUnit{m}}) where m = m
const Percent = ScalingUnit/1e2
abbr(::Type{Percent}) = "%"
const Permille = ScalingUnit/1e3
abbr(::Type{Permille}) = "‰"
@abbreviate ppm ScalingUnit/1e6
@abbreviate ppb ScalingUnit/1e9

abstract type Angle <: NullDimension end
baseunit(::Type{Angle}) = Radian
struct Degree <: Angle value::Number end
@abbreviate ° Degree
struct Radian <: Angle value::Number end
@abbreviate rad Radian
basefactor(::Type{Degree}) = π/180
Base.promote_rule(::Type{<:Angle}, ::Type{<:Angle}) = Radian
Base.convert(::Type{Radian}, d::Degree) = Radian(d.value * basefactor(Degree))
Base.convert(::Type{Degree}, r::Radian) = Degree(r.value / basefactor(Degree))
for λ in (:sin,:cos,:tan)
  @eval Base.$λ(n::Angle) = $λ(convert(Radian, n))
  @eval Base.$λ(n::Radian) = $λ(n.value)
end

scale(x::Unit) = begin
  T = typeof(x)
  trailing_zeros = findfirst(!=(0), digits(round(Int, x.value); base=10)) - 1
  m = Magnitude(trailing_zeros ÷ 3 * 3)
  (T*m)(x.value/m)
end
