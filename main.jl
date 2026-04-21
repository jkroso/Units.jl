@use "./utils.jl" get_param set_param seperate Magnitude exponents prefixs LogNumber
@use "github.com/jkroso/Prospects.jl" group mapcat
@use MacroTools: @capture
@use Dates

abstract type Unit <: Number end
abstract type Dimension <: Unit end
struct Exponent{dimension<:Unit, n, T<:Number} <: Unit
  value::T
end
Exponent{D, n}(v::T) where {D<:Unit, n, T<:Number} = Exponent{D, n, T}(v)
abstract type AbstractCombination{dimensions<:Tuple{Vararg{Exponent}}} <: Unit end
abstract type DerivedUnit{magnitude, dimensions} <: AbstractCombination{dimensions} end

"""
Used to represent composite types like `kg*m^2/s^3`. The seperation of dimensions from units
allows it to also be used to represent dimensionless units like `cm/m` while also keeping
track of the magnitude of each dimension. Which is useful in units like `kW*hr`
"""
struct Combination{dimensions<:Tuple{Vararg{Exponent}}, units<:Tuple{Vararg{Exponent}}, T<:Number} <: AbstractCombination{dimensions}
  value::T
end
Combination{D, U}(v::T) where {D, U, T<:Number} = Combination{D, U, T}(v)

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
    magnitudes = short_name.args[2].args
    short_name = short_name.args[3]
  else
    magnitudes = Any[]
  end
  (abbrev, superscript) = match(r"([^⁽]+)(⁽[²³]+⁾)?", string(short_name)).captures
  abbrev = String(abbrev)
  superscript = isnothing(superscript) ? Int[] : Int[x-175 for x in Char[superscript...][2:end-1]]
  quote
    Base.@__doc__ $(esc(:(struct $name{magnitude, T<:Number} <: $super
      value::T
    end)))
    $T{m}(v::V) where {m, V<:Number} = $T{m, V}(v)
    $Units.short_name(::Type{<:$T}) = $abbrev
    $Units.scaler(::Type{<:$T{m}}) where m = m
    Base.promote_rule(A::Type{<:$T}, B::Type{<:$T}) = begin
      m_a, m_b = $Units.get_param(A, 1), $Units.get_param(B, 1)
      m = m_a < m_b ? m_a : m_b
      V = $Units.combine_valtype($Units.valtype_of(A), $Units.valtype_of(B))
      isnothing(V) ? $T{m} : $T{m, V}
    end
    const $(esc(Symbol(abbrev))) = $T{Magnitude(0)}
    $(map(superscript) do s
      :(const $(esc(Symbol(abbrev, exponents[s]))) = $T{Magnitude(0)}^$(s-1))
    end...)
    $(map(magnitudes) do m
      for (n, sym) in prefixs
        sym != m && continue
        return quote
          const $(esc(Symbol(m, abbrev))) = $T{Magnitude($n)}
          $(map(superscript) do s
            :(const $(esc(Symbol(m, abbrev, exponents[s]))) = $T{Magnitude($n)}^$(s-1))
          end...)
        end
      end
      error("unknown prefix $m")
    end...)
  end
end

macro abbreviate(name, def)
  quote
    const $(esc(name)) = $(esc(def))
    $Units.abbr(::Type{<:$(esc(name))}) = $(string(name))
  end
end

parameters(D::DataType) = D.parameters
parameters(U::UnionAll) = parameters(U.body)
parameters(V::TypeVar) = parameters(V.ub)
ub(D::DataType) = D
ub(D::TypeVar) = D.ub
ub(D::UnionAll) = D
innermost(U::UnionAll) = innermost(U.body)
innermost(D::DataType) = D
# True if the last type parameter of D is the struct's value field type
# (i.e. D is parameterized by the value's type)
parameterized_by_value(D::DataType) = begin
  n = length(D.parameters)
  (n == 0 || fieldcount(D) == 0) && return false
  fieldtype(D, 1) === D.parameters[n]
end
parameterized_by_value(T::Type) = parameterized_by_value(innermost(T))

# Strip the value-type parameter (last one) from a concrete unit type,
# so the constructor can re-infer T from the value it's given
@generated naked(::Type{T}) where T = begin
  D = innermost(T)
  if parameterized_by_value(D)
    n = length(D.parameters)
    stripped = D.name.wrapper{D.parameters[1:n-1]...}
    return :($stripped)
  end
  return :($T)
end

# Given a unit type and an actual value type, return a unit type with
# that value type bound. If T is already concrete in its last param,
# leave it alone.
@generated with_valtype(::Type{T}, ::Type{V}) where {T, V} = begin
  D = innermost(T)
  if parameterized_by_value(D)
    n = length(D.parameters)
    last = D.parameters[n]
    if last isa TypeVar
      bound = D.name.wrapper{D.parameters[1:n-1]..., V}
      return :($bound)
    end
  end
  return :($T)
end

# Extract the bound value-type param (nothing if still a TypeVar)
@generated valtype_of(::Type{T}) where T = begin
  D = innermost(T)
  if parameterized_by_value(D)
    n = length(D.parameters)
    last = D.parameters[n]
    return last isa TypeVar ? :(nothing) : :($last)
  end
  return :(nothing)
end

# Combine two candidate value types from promotion inputs
combine_valtype(::Nothing, ::Nothing) = nothing
combine_valtype(a::Type, ::Nothing) = a
combine_valtype(::Nothing, b::Type) = b
combine_valtype(a::Type, b::Type) = promote_type(a, b)
# "abstract" for our unit system: any non-value type parameter is free
isabstract_impl(D::DataType) = begin
  isabstracttype(D) && return true
  n = length(D.parameters)
  n == 0 && return false
  stop = fieldcount(D) > 0 ? n - 1 : n
  for i in 1:stop
    D.parameters[i] isa TypeVar && return true
  end
  false
end
isabstract_impl(U::UnionAll) = isabstract_impl(innermost(U))
isabstract_impl(T::TypeVar) = true
isabstract(T::TypeVar) = true
isabstract(U::UnionAll) = isabstract_impl(innermost(U))
@generated isabstract(::Type{T}) where T = :($(isabstract_impl(T)))
@generated wrap_concrete(::Type{T}, ::Val{e}) where {T, e} = :(Exponent{$(naked(T)), $e})
wrap(T::Type, e) = isabstract(T) ? (Exponent{D, e} where D<:ub(T)) : wrap_concrete(T, Val(e))
wrap(T::TypeVar, e) = Exponent{D, e} where D<:ub(T)

Base.inv(::Type{<:Exponent{d,n}}) where {d,n} = wrap(d, -n)
Base.inv(E::Type{<:Exponent}) = wrap(get_param(E, 1), -get_param(E, 2))
Base.inv(::Type{D}) where D<:Dimension = wrap(D, -1)
Base.inv(C::Type{<:Combination}) = map_combo(inv, C)
Base.inv(u::Unit) = inv(u.value)inv(typeof(u))
Base.:^(D::Type{<:Unit}, e) = wrap(D, e)
Base.:^(::Type{<:Exponent{d, e1}}, e2) where {d,e1} = wrap(d, e1 * e2)
Base.:^(E::Type{<:Exponent}, e) = wrap(get_param(E, 1), get_param(E, 2) * e)
Base.:^(C::Type{<:Combination}, e) = map_combo(d->d^e, C)
tounionall(U::UnionAll) = tounionall(innermost(U))
tounionall(U::DataType) = U.name.wrapper
Base.:/(U::Type{<:Unit}, n::Real) = begin
  D = tounionall(unwrap(U))
  m1 = convert(LogNumber, scaler(U))
  m2 = convert(LogNumber, n^(1//power(U)))
  simplify(wrap(D{m1/m2}, power(U)))
end
Base.:*(U::Type{<:Unit}, n::Real) = begin
  D = tounionall(unwrap(U))
  m1 = convert(LogNumber, scaler(U))
  m2 = convert(LogNumber, n^(1//power(U)))
  simplify(wrap(D{m1*m2}, power(U)))
end

map_combo(f, C::Type{<:Combination}) = begin
  dims, units = parameters(get_param(C, 1)), parameters(get_param(C, 2))
  Combination{Tuple{map(f, dims)...}, Tuple{map(f, units)...}}
end

Base.promote_rule(::Type{<:Exponent{d1,e}}, ::Type{<:Exponent{d2,e}}) where {d1,d2,e} = begin
  E = wrap(promote_type(d1, d2), e)
  V = combine_valtype(valtype_of(Exponent{d1,e}), valtype_of(Exponent{d2,e}))
  isnothing(V) ? E : with_valtype(E, V)
end
Base.promote_rule(A::Type{<:Exponent}, B::Type{<:Exponent}) = begin
  da, ea = unwrap(A), power(A)
  db, eb = unwrap(B), power(B)
  ea == eb || error("dimension mismatch")
  E = wrap(promote_type(da, db), ea)
  V = combine_valtype(valtype_of(A), valtype_of(B))
  isnothing(V) ? E : with_valtype(E, V)
end
Base.promote_rule(A::Type{<:Combination}, B::Type{<:Unit}) = simplify(promote_rule(A, to_combo(B)))
Base.promote_rule(A::Type{<:Exponent}, B::Type{<:Dimension}) = promote_rule(A, wrap(B, 1))
Base.promote_rule(::Type{A}, ::Type{B}) where {A<:Combination,B<:Combination} = begin
  dims_a, dims_b = sorted_dims(A), sorted_dims(B)
  @assert dims_a == dims_b "dimension mismatch"
  C = Combination{Tuple{dims_a...}, Tuple{map(promote_type, sorted_subunits(A), sorted_subunits(B))...}}
  V = combine_valtype(valtype_of(A), valtype_of(B))
  isnothing(V) ? C : with_valtype(C, V)
end
sorted_dims(a) = sort!(collect(dimensions(a)), by=string ∘ abstract_dimension)
sorted_subunits(a) = sort!(collect(subunits(a)), by=string ∘ abstract_dimension)

abbr(_) = ""
abbr(m::Magnitude) = string(get(prefixs, m.value, ""))
scaler(U::Type{<:Exponent{d}}) where d = scaler(d)
scaler(U::Type{<:DerivedUnit{d}}) where d = d
scaler(U::Type{<:Unit}) = 1
prefix(T::Type{<:Dimension}) = abbr(scaler(T))

abbr(T::Type{<:Dimension}) = prefix(T) * short_name(T)
abbr(::Type{<:Exponent{d,n}}) where {d,n} = begin
  n == 1 && return abbr(d)
  n >= 0 && return string(abbr(d), exponents[Int(n)+1])
  string(abbr(d), '⁻', exponents[Int(abs(n))+1])
end
abbr(E::Type{<:Exponent}) = begin
  d, n = unwrap(E), power(E)
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

Base.:-(n::Unit) = naked(typeof(n))(-n.value)
Base.:*(n::Real, ::Type{T}) where T<:Unit = naked(T)(n)
Base.:/(n::Real, ::Type{T}) where T<:Unit = naked(inv(T))(n)
Base.:^(u::Unit, n::Integer) = (typeof(u)^n)(u.value^n)
Base.convert(::Type{U}, n::Real) where U<:Unit = U(n)
Base.convert(::Type{N}, u::U) where {N<:Real,U<:Unit} = convert(N, u.value * basefactor(U))
# Helper that's pure w.r.t. its type arguments: computes (factor, output_type, inverted?)
Base.@assume_effects :foldable convert_plan(::Type{U}, ::Type{N}) where {U<:Unit, N<:Unit} = begin
  ad = map(dimension, sorted_dims(N))
  bd = map(dimension, sorted_dims(U))
  if ad == bd
    factor = conversion_factor(N, U)
    out = with_valtype(U, promote_type(fieldtype(N, 1), typeof(factor)))
    (factor, out, false)
  elseif map(inv, ad) == bd
    Ninv = inv(N)
    factor = conversion_factor(Ninv, U)
    out = with_valtype(U, promote_type(fieldtype(Ninv, 1), typeof(factor)))
    (factor, out, true)
  else
    (nothing, U, nothing)
  end
end

Base.convert(::Type{U}, n::N) where {U<:Unit, N<:Unit} = begin
  factor, out, inverted = convert_plan(U, N)
  inverted === nothing && error("$(abbr(U)) not dimensionally compatable with $(abbr(N))")
  if inverted
    ni = inv(n)
    out(ni.value * factor)
  else
    out(n.value * factor)
  end
end
Base.convert(::Type{Combination}, d::D) where D<:Unit = to_combo(D)(d.value)
Base.show(io::IO, t::Unit) = (write(io, seperate(t.value), abbr(typeof(t))); nothing)
Base.conj(x::Unit) = x
Base.isinteger(u::Unit) = isinteger(u.value)
Base.abs(u::Unit) = abs(u.value)typeof(u)
Base.isapprox(a::Unit, b::Unit) = isapprox(map(x->x.value, promote(a, b))...)
Base.div(a::Unit, b::Unit) = Int(floor(a/b))
Base.div(a::Unit, b::Real) = naked(typeof(a))(div(a.value, b))
Base.rem(a::Unit, b::Unit) = a-(floor(a/b)*b)
Base.rem(a::Unit, b::Real) = naked(typeof(a))(rem(a.value, b))
Base.floor(a::Unit) = naked(typeof(a))(floor(a.value))
Base.ceil(a::Unit) = naked(typeof(a))(ceil(a.value))
Base.round(A::Type{<:Unit}, a::Unit) = round(convert(A, a))
Base.round(u::Unit; kwargs...) = naked(typeof(u))(round(u.value; kwargs...))
Base.sqrt(s::Exponent{T,d}) where {d,T} = begin
  n = Int(d/2)
  n == 1 ? T(sqrt(s.value)) : wrap(T, n)(sqrt(s.value))
end

for λ in (:*, :/, :+, :-)
  @eval Base.$λ(n::Real, u::Unit) = naked(typeof(u))($λ(n, u.value))
  @eval Base.$λ(u::Unit, n::Real) = naked(typeof(u))($λ(u.value, n))
end

for λ in (:+, :-)
  @eval Base.$λ(a::T, b::T) where T<:Unit = naked(T)($λ(a.value, b.value))
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

Base.convert(::Type{T}, n::D) where {D<:Dimension, T<:Dimension} = begin
  n isa T && return n
  val = n.value * conversion_factor(D, T)
  with_valtype(T, typeof(val))(val)
end

basefactor(D::Type{<:Dimension}) = scaler(D)
basefactor(D::Type{<:DerivedUnit}) = scaler(D)
basefactor(E::Type{<:Exponent{d,e}}) where {d,e} = basefactor(d)^e
basefactor(E::Type{<:Exponent}) = basefactor(unwrap(E))^power(E)
basefactor(C::Type{<:Combination}) = begin
  units = parameters(get_param(C, 2))
  mapreduce(basefactor, *, units, init=1)
end
conversion_factor(A::Type{<:Unit}, B::Type{<:Unit}) = conversion_factor(to_combo(A), to_combo(B))
conversion_factor(::Type{A}, ::Type{B}) where {A<:Dimension,B<:Dimension} = basefactor(A)/basefactor(B)
conversion_factor(::Type{<:Exponent{da,ea}}, ::Type{<:Exponent{db,eb}}) where {da,db,ea,eb} = error("dimension mismatch")
conversion_factor(::Type{<:Exponent{da,e}}, ::Type{<:Exponent{db,e}}) where {da,db,e} = (basefactor(da)/basefactor(db))^e
conversion_factor(A::Type{<:Exponent}, B::Type{<:Exponent}) = begin
  da, ea = unwrap(A), power(A)
  db, eb = unwrap(B), power(B)
  ea == eb || error("dimension mismatch")
  (basefactor(da)/basefactor(db))^ea
end
conversion_factor(A::Type{<:AbstractCombination{DA}}, B::Type{<:AbstractCombination{DB}}) where {DA,DB} = begin
  (units_a, scaler_a), (units_b, scaler_b) = simple_units(A), simple_units(B)
  sort!(units_a, by=string ∘ abstract_dimension)
  sort!(units_b, by=string ∘ abstract_dimension)
  mapreduce(conversion_factor, *, units_a, units_b, init=scaler_a/scaler_b)
end

abstract type NullDimension <: Dimension end
to_combo(U::Type{<:NullDimension}) = Combination{Tuple{},Tuple{wrap(U, 1)}}
dimensions(::Type{<:NullDimension}) = Any[]
dimension(::Type{<:NullDimension}) = wrap(NullDimension, 1)
abstract_dimension(::Type{<:NullDimension}) = NullDimension

"unpack derived units, dedupe dimensions, and keep track of the resulting scale difference"
simple_units(x) = [Exponent{x,1}], 1
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
abstract_dimension(U::Type{<:DerivedUnit}) = innermost(U).name.wrapper

dimension(D::Type{<:Dimension}) = wrap(abstract_dimension(D), 1)
dimension(D::Type{<:Exponent}) = wrap(abstract_dimension(unwrap(D)),power(D))
dimension(C::Type{<:Combination}) = begin
  dims = map(dimension, get_param(C, 1).parameters)
  sort!(dims, by=string ∘ abstract_dimension)
  AbstractCombination{dimensions} where dimensions<:Tuple{dims...}
end
dimension(C::Type{<:DerivedUnit{m, units}}) where {m, units} = begin
  dims = map(dimension, units.parameters)
  sort!(dims, by=string ∘ abstract_dimension)
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

subunits(::Type{<:Exponent{D,n}}) where {D<:DerivedUnit,n} = AnyType[wrap(unwrap(x), power(x) * n) for x in subunits(D)]
subunits(D::Type{<:Exponent}) = AnyType[D]
subunits(C::Type{<:Combination}) = parameters(parameters(C)[2])
subunits(D::Type{<:AbstractCombination}) = parameters(parameters(D)[1])
subunits(D::Type{<:DerivedUnit{m,units}}) where {m,units} = parameters(units)

flatten_units(C::Type{<:AbstractCombination}) = accum_units(map(flatten_units, parameters(get_param(C, 1))), 1)
flatten_units(::Type{<:Combination{d,u}}) where {d,u} = accum_units(map(flatten_units, parameters(u)), 1)
flatten_units(::Type{<:DerivedUnit{m,u}}) where {m,u} = accum_units(map(flatten_units, parameters(u)), m)
flatten_units(::Type{<:Exponent{d,e}}) where {d,e} = begin
  units, scale = flatten_units(d)
  map(u->wrap(unwrap(u),power(u)*e), units), scale
end
flatten_units(E::Type{<:Exponent}) = begin
  d, e = unwrap(E), power(E)
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
dimensions(E::Type{<:Exponent}) = begin
  D = unwrap(E)
  e = power(E)
  map(d->wrap(unwrap(d), power(d) * e), dimensions(D))
end
dimensions(C::Type{<:Combination}) = parameters(parameters(C)[1])
dimensions(C::Type{<:DerivedUnit{m,units}}) where {m,units} = map(dimension, units.parameters)
ispointless(d) = power(d) == 0
power(E::Type{<:Exponent}) = get_param(E, 2)
power(::Type{<:Unit}) = 1

"Reduces a complex unit to a simpler but equivelent one"
simplify(T::Type{<:Unit}) = T
simplify(E::Type{<:Exponent{d,n}}) where {d,n} = n == 1 ? d : E
simplify(E::Type{<:Exponent}) = power(E) == 1 ? unwrap(E) : E
simplify(C::Type{<:Combination}) = begin
  dims = Any[x for x in get_param(C, 1).parameters if !ispointless(x)]
  units = Any[x for x in get_param(C, 2).parameters if !ispointless(x)]
  isempty(units) && return Real
  length(units) == 1 && return simplify(units[1])
  Combination{Tuple{dims...}, Tuple{units...}}
end

"deduplicate units in a combination while also keeping track of the scale difference between input and output types"
prune(T::Type{<:Unit}) = T, 1
prune(::Type{<:Combination{Ds, units}}) where {Ds, units} = begin
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
  val = v * scaler
  naked(simplify(T))(val)
end

unpacked(c::Combination) = begin
  units, scale = simple_units(typeof(c))
  simplify(Combination{Tuple{map(dimension, units)...}, Tuple{units...}})(c.value * scale)
end

combine(dims, units) = begin
  length(units) == 1 && return simplify(units[1])
  Combination{Tuple{dims...}, Tuple{units...}}
end

combine_types(op, A::Type{<:AbstractCombination}, B::Type{<:AbstractCombination}) = begin
  dims_a, dims_b = parameters(get_param(A, 1)), parameters(get_param(B, 1))
  Das, Dbs = map(abstract_dimension, dims_a), map(abstract_dimension, dims_b)
  Ds = map(union(Das, Dbs)) do D
    ia, ib = findfirst(==(D), Das), findfirst(==(D), Dbs)
    isnothing(ia) && return op == :/ ? inv(dims_b[ib]) : dims_b[ib]
    isnothing(ib) && return dims_a[ia]
    wrap(D, op == :* ? power(dims_a[ia]) + power(dims_b[ib]) : power(dims_a[ia]) - power(dims_b[ib]))
  end
  AbstractCombination{Dimensions} where Dimensions<:Tuple{filter!(!ispointless, Ds)...}
end
combine_types(op, A::Type{<:Combination}, B::Type{<:Combination}) = begin
  units_a, units_b = subunits(A), subunits(B)
  naked_a, naked_b = map(unwrap, units_a), map(unwrap, units_b)
  units = filter!(!ispointless, map(union(naked_a, naked_b)) do u
    ia,ib = findfirst(==(u), naked_a), findfirst(==(u), naked_b)
    isnothing(ia) && return op == :/ ? inv(units_b[ib]) : units_b[ib]
    isnothing(ib) && return units_a[ia]
    wrap(u, op == :* ? power(units_a[ia]) + power(units_b[ib]) : power(units_a[ia]) - power(units_b[ib]))
  end)
  dims = mapcat(dimensions, units)
  wrapped = map(unique!(map(abstract_dimension, dims))) do AD
    e = sum((power(u) for u in dims if abstract_dimension(u) == AD))
    wrap(AD, e)
  end
  combine(filter!(!ispointless, wrapped), units)
end

# Pure type-level arithmetic: compute the output type for op(A, B) at compile-time.
# Returns a type, never an Expr — safe to call inside @generated bodies.
combo_op(op, A::Type, B::Type) = begin
  A <: DerivedUnit && return combo_op(op, to_combo(A), B)
  B <: DerivedUnit && return combo_op(op, A, to_combo(B))
  if A <: Combination && B <: Combination
    combine_types(op, A, B)
  elseif A <: AbstractCombination && B <: AbstractCombination
    combine_types(op, A, B)
  else
    CA = (A <: Combination || A <: AbstractCombination) ? A : to_combo(A)
    CB = (B <: Combination || B <: AbstractCombination) ? B : to_combo(B)
    combo_op(op, CA, CB)
  end
end

for λ in (:*, :/)
  op = QuoteNode(λ)
  @eval @generated Base.$λ(::Type{A}, ::Type{B}) where {A<:Unit, B<:Unit} = :($(combo_op($op, A, B)))
end

# Helper that @generated bodies use to build the per-call expression
function build_combo_expr(op::Symbol, UA, UB, scaler_a, scaler_b)
  T = getfield(Base, op)(UA, UB)
  Tp, scaler = prune(T)
  out = naked(simplify(Tp))
  inner = Expr(:call, op, :(a.value * $scaler_a), :(b.value * $scaler_b))
  :($out($inner * $scaler))
end

# Then: the value-level arithmetic. Each body precomputes the output type and scalers at
# macro expansion, leaving only the numeric work for runtime.
for λ in (:*, :/)
  op = QuoteNode(λ)
  @eval @generated function Base.$λ(a::A, ::Type{B}) where {A<:Unit,B<:Unit}
    T = $λ(A, B)
    Tp, scaler = prune(T)
    out = naked(simplify(Tp))
    :($out(a.value * $scaler))
  end
  @eval @generated function Base.$λ(::Type{A}, b::B) where {A<:Unit,B<:Unit}
    T = $λ(A, B)
    Tp, scaler = prune(T)
    out = naked(simplify(Tp))
    :($out(b.value * $scaler))
  end
  @eval @generated function Base.$λ(a::A, b::B) where {A<:Unit, B<:Unit}
    CA, CB = to_combo(A), to_combo(B)
    UA, scaler_a = prune(CA)
    UB, scaler_b = prune(CB)
    build_combo_expr($op, UA, UB, scaler_a, scaler_b)
  end
  @eval @generated function Base.$λ(a::A, b::B) where {A<:Unit, B<:Combination}
    CA = to_combo(A)
    UA, scaler_a = prune(CA)
    UB, scaler_b = prune(B)
    build_combo_expr($op, UA, UB, scaler_a, scaler_b)
  end
  @eval @generated function Base.$λ(a::A, b::B) where {A<:Combination, B<:Unit}
    CB = to_combo(B)
    UA, scaler_a = prune(A)
    UB, scaler_b = prune(CB)
    build_combo_expr($op, UA, UB, scaler_a, scaler_b)
  end
  @eval @generated function Base.$λ(a::A, b::B) where {A<:Combination, B<:Combination}
    UA, scaler_a = prune(A)
    UB, scaler_b = prune(B)
    build_combo_expr($op, UA, UB, scaler_a, scaler_b)
  end
end

const Energy = Mass * Length^2 / Time^2
const Power = Energy/Time
const Area = Length^2
const Volume = Length^3
const Density = Mass/Volume
const Force = Mass*Length/Time^2
const Pressure = Force/Area
const Speed = Length/Time
const Acceleration = Speed/Time
const Jerk = Acceleration/Time
const Snap = Jerk/Time
const Crackle = Snap/Time
const Pop = Crackle/Time

@defunit Meter <: Length [μ c m k]m⁽²³⁾
@abbreviate litre m³/1e3
@abbreviate ml litre/1e3
@abbreviate μl litre/1e6
@abbreviate hectare Area{Meter{Magnitude(2)}}
@defunit Gram <: Mass [μ m k]g
@abbreviate ton Gram*1e6
@defunit Mole <: Substance n
const Nₐ = 6.02214076*10^23/n
@defunit Ampere <: Current [m]A
@defunit Lumen <: Luminosity lm
@abbreviate lx lm/m²
# define second manually because it needs a special scaler definition
struct Second{magnitude, T<:Number} <: Time value::T end
Second{m}(v::V) where {m, V<:Number} = Second{m, V}(v)
short_name(::Type{<:Second}) = "s"
scaler(::Type{<:Second{m}}) where m = exponentiable(m)
exponentiable(m::Magnitude) = m
exponentiable(m::Integer) = m//1
exponentiable(m) = m
Base.promote_rule(A::Type{<:Second}, B::Type{<:Second}) = begin
  m_a, m_b = get_param(A, 1), get_param(B, 1)
  m = m_a < m_b ? m_a : m_b
  V = combine_valtype(valtype_of(A), valtype_of(B))
  isnothing(V) ? Second{m} : Second{m, V}
end
const s = Second{Magnitude(0)}
const ns = Second{Magnitude(-9)}
const ms = Second{Magnitude(-3)}
abbr(S::Type{<:Second{m}}) where m = begin
  m isa Magnitude && return abbr(m) * "s"
  error("unknown magnitude $m")
end
@abbreviate minute Second{60}
@abbreviate hr Second{3600}
@abbreviate day Second{convert(Int, 24scaler(hr))}
@abbreviate week Second{convert(Int, 7scaler(day))}
@abbreviate yr Second{convert(Int, 365scaler(day))}
@abbreviate month Second{convert(Int, scaler(yr)/12)}
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
@defunit Byte <: Data [k M G T]B
@defunit Bit <: Data [k M G T]b
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
    subunits, scale = simple_units($(esc(units)))
    @assert scale == 1 "derived units should be defined with no scale"
    Base.@__doc__ struct $N{magnitude, T<:Number} <: DerivedUnit{magnitude, Tuple{subunits...}}
      value::T
    end
    $N{m}(v::V) where {m, V<:Number} = $N{m, V}(v)
    $Units.short_name(::Type{<:$N{m}}) where m = $(string(short))
    $Units.scaler(::Type{<:$N{m}}) where m = m
    Base.promote_rule(A::Type{<:$N}, B::Type{<:$N}) = begin
      m_a, m_b = $Units.get_param(A, 1), $Units.get_param(B, 1)
      m = m_a < m_b ? m_a : m_b
      V = $Units.combine_valtype($Units.valtype_of(A), $Units.valtype_of(B))
      isnothing(V) ? $N{m} : $N{m, V}
    end
    const $(esc(short)) = $N{Magnitude(0)}
    $(map(magnitudes) do m
      for (n, sym) in prefixs
        sym == m && return :(const $(esc(Symbol(m, short))) = $N{Magnitude($n)})
      end
      error("unknown prefix $m")
    end...)
  end
end

macro scaledunit(name, unit)
  type_name = string(name, "Unit")
  N = esc(Symbol(type_name))
  x = Core.eval(__module__, unit)
  subunits, scale = simple_units(typeof(x))
  scale *= x.value
  quote
    Base.@__doc__ struct $N{scale, T<:Number} <: DerivedUnit{scale, Tuple{$(subunits...)}} value::T end
    $N{s}(v::V) where {s, V<:Number} = $N{s, V}(v)
    $Units.short_name(::Type{<:$N{s}}) where s = $(string(name))
    $Units.scaler(::Type{<:$N{s}}) where s = s
    const $(esc(name)) = $N{$scale}
  end
end

@deriveunit Joule kg*m²/s^2 [k M]J
@deriveunit Watt kg*m²/s^3 [m k M]W
@abbreviate Wh W*hr
@abbreviate kWh kW*hr
@deriveunit Newton kg*m/s^2 [k]N
@deriveunit Pascal N/m² [k M]Pa
const gravity = 9.80665m/s^2 # http://physics.nist.gov/cgi-bin/cuu/Value?gn
# @scaledunit kgf 1kg*gravity ruins precompilation
# @abbreviate tonf kgf*1e3
@abbreviate bar Pascal*1e5
@abbreviate mbar bar/1e3
@deriveunit Coulomb A*s C
@deriveunit Volt J/C V
@abbreviate kV Volt*1e3
@deriveunit Ohm V/A Ω
@abbreviate Hz inv(s)
@abbreviate kHz inv(ms)
@abbreviate MHz inv(ns)

struct ScalingUnit{magnitude, T<:Number} <: NullDimension
  value::T
end
ScalingUnit{m}(v::T) where {m, T<:Number} = ScalingUnit{m, T}(v)
scaler(::Type{<:ScalingUnit{m}}) where m = m
scaler(s::ScalingUnit) = s.value*scaler(typeof(s))
Base.promote_rule(A::Type{<:ScalingUnit}, B::Type{<:ScalingUnit}) = begin
  m_a, m_b = get_param(A, 1), get_param(B, 1)
  m = m_a < m_b ? m_a : m_b
  V = combine_valtype(valtype_of(A), valtype_of(B))
  isnothing(V) ? ScalingUnit{m} : ScalingUnit{m, V}
end
const Percent = ScalingUnit/1e2
abbr(::Type{Percent}) = "%"
const Permille = ScalingUnit/1e3
abbr(::Type{Permille}) = "‰"
@abbreviate ppm ScalingUnit/1e6
@abbreviate ppb ScalingUnit/1e9

Base.:-(a::Real, b::ScalingUnit) = a - a*scaler(b)
Base.:-(a::Unit, b::ScalingUnit) = a - a*scaler(b)
Base.:+(a::Real, b::ScalingUnit) = a + a*scaler(b)
Base.:+(a::Unit, b::ScalingUnit) = a + a*scaler(b)

abstract type Angle <: NullDimension end
baseunit(::Type{Angle}) = Radian
struct Degree{T<:Number} <: Angle value::T end
@abbreviate ° Degree
struct Radian{T<:Number} <: Angle value::T end
@abbreviate rad Radian
basefactor(::Type{<:Degree}) = π/180
Base.promote_rule(::Type{<:Angle}, ::Type{<:Angle}) = Radian
Base.convert(::Type{<:Radian}, d::Degree) = Radian(d.value * basefactor(Degree))
Base.convert(::Type{<:Degree}, r::Radian) = Degree(r.value / basefactor(Degree))
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

abstract type TimeOfDay{name} end
const am = TimeOfDay{:am}
const pm = TimeOfDay{:pm}
Base.:*(t::Integer, ::Type{TimeOfDay{:am}}) = t>12 ? Dates.Time(0,t) : Dates.Time(t)
Base.:*(t::Integer, ::Type{TimeOfDay{:pm}}) = Dates.Time(t+12)
Base.:*(t::UnitRange, ::Type{TimeOfDay{:am}}) = Dates.Time(t.start, t.stop)
Base.:*(t::UnitRange, ::Type{TimeOfDay{:pm}}) = Dates.Time(t.start+12, t.stop)
Base.:(:)(t::Int, time::Dates.Time) = begin
  m = Dates.minute(time)
  Dates.Time(t, m == 0 ? Dates.hour(time) : m)
end
Base.show(io::IO, ::MIME"text/plain", ns::Dates.Nanosecond) = show(io, Dates.canonicalize(ns))
Base.show(io::IO, ::MIME"text/plain", c::Dates.CompoundPeriod) = show(io, MIME("text/plain"), convert(Dates.Nanosecond, c))
Base.promote_rule(T::Type{<:Dates.Period}, ::Type{<:Time}) = T
Base.convert(T::Type{<:Dates.Period}, t::Time) = convert(T, Dates.Second(convert(Float64, t)))
Base.:-(a::Dates.Period, b::Time) = -(promote(a,b)...)
Base.:+(a::Dates.Period, b::Time) = +(promote(a,b)...)
Base.:-(a::Time, b::Dates.Period) = -(promote(a,b)...)
Base.:+(a::Time, b::Dates.Period) = +(promote(a,b)...)
