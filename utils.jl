"Formats long numbers with commas seperating it into chunks"
seperate(n::Number; kwargs...) = seperate(string(convert(isinteger(n) ? Int : Float64, n)), kwargs...)
seperate(str::String, sep = ",", k = 3) = begin
  parts = split(str, '.')
  str = parts[1]
  n = length(str)
  groups = (str[max(x-k+1, 1):x] for x in reverse(n:-k:1))
  whole_part = join(groups, sep)
  startswith(whole_part, "-,") && (whole_part = "-" * whole_part[3:end])
  length(parts) == 1 ? whole_part : join([whole_part,  parts[2]], '.')
end

set_param(T::UnionAll, i, x) = set_param(T.body, i, x)
set_param(T::DataType, i, x) = begin
  params = collect(Any, T.parameters)
  params[i] = x
  foldr(params, init=T.name.wrapper{params...}) do param,T
    param isa TypeVar ? UnionAll(param, T) : T
  end
end

get_param(T::UnionAll, i) = get_param(T.body, i)
get_param(T::DataType, i) = T.parameters[i]

modify_param(f, T::UnionAll, i) = modify_param(f, T.body, i)
modify_param(f, T::DataType, i) = set_param(T, i, f(get_param(T, i)))

get_body(x::UnionAll) = get_body(x.body)
get_body(x::DataType) = x

abstract type LogNumber <: Real end

struct Magnitude <: LogNumber
  value::Int8
end
Base.convert(::Type{N}, m::Magnitude) where N<:Number = convert(N, Rational(10)^m.value)
Base.convert(::Type{Magnitude}, m::Magnitude) = m
Base.convert(::Type{Magnitude}, n::Real) = Magnitude(floor(Int8, log10(n)))
Base.promote_rule(::Type{N}, ::Type{LogNumber}) where N<:Number = N
Base.promote_rule(::Type{<:Integer}, ::Type{LogNumber}) = Rational
Base.promote_rule(::Type{<:LogNumber}, ::Type{<:LogNumber}) = ScaledMagnitude
Base.:/(a::Magnitude, b::Magnitude) = Magnitude(a.value - b.value)
Base.:*(a::Magnitude, b::Magnitude) = Magnitude(a.value + b.value)
Base.:<(a::Magnitude, b::Magnitude) = a.value < b.value
Base.:-(a::Magnitude) = Magnitude(-a.value)
Base.:-(a::Magnitude, b::Magnitude) = Magnitude(a.value - b.value)
Base.:+(a::Magnitude, b::Magnitude) = Magnitude(a.value + b.value)
Base.:^(m::Magnitude, n::Integer) = Magnitude(m.value*n)
Base.inv(m::Magnitude) = Magnitude(-m.value)
const exponents = Vector{Char}("⁰¹²³⁴⁵⁶⁷⁸⁹")
superscript(n::Real) = string(n < 0 ? "⁻" : "", exponents[(+).(1, reverse!(digits(abs(n))))]...)
Base.show(io::IO, m::Magnitude) = write(io, "10$(superscript(m.value))")

struct ScaledMagnitude{T} <: LogNumber
  magnitude::Int8
  scaler::T
end
Base.show(io::IO, m::ScaledMagnitude) = print(io, float(m.scaler), "×10", superscript(m.magnitude))
Base.getproperty(s::ScaledMagnitude, f::Symbol) = f == :value ? convert(Number, s) : getfield(s, f)
Base.convert(::Type{N}, m::ScaledMagnitude) where N<:Number = convert(N, Rational(10)^m.value*m.scaler)
Base.convert(::Type{ScaledMagnitude}, m::Magnitude) = ScaledMagnitude(m.value, 1)
Base.convert(::Type{LogNumber}, m::Magnitude) = m
Base.convert(::Type{LogNumber}, m::ScaledMagnitude) = m
Base.convert(::Type{ScaledMagnitude}, n::ScaledMagnitude) = n
Base.convert(::Type{ScaledMagnitude}, n::Real) = begin
  mag = floor(Int8, log10(n))
  scale = n/Rational(10)^mag
  ScaledMagnitude(mag, isinteger(scale) ? convert(Integer, scale) : scale)
end
Base.:*(m::ScaledMagnitude, n::Real) = m*convert(ScaledMagnitude, n)
Base.:*(a::ScaledMagnitude, b::ScaledMagnitude) = ScaledMagnitude(a.magnitude+b.magnitude, a.scaler*b.scaler)
Base.:/(a::ScaledMagnitude, b::ScaledMagnitude) = ScaledMagnitude(a.magnitude-b.magnitude, a.scaler/b.scaler)
Base.:-(a::ScaledMagnitude) = ScaledMagnitude(a.magnitude, -a.scaler)
Base.inv(m::ScaledMagnitude) = ScaledMagnitude(-m.scaler)

Base.convert(::Type{LogNumber}, n::Real) = begin
  magnitude = log10(n)
  mag = if round(Int8, magnitude) ≈ magnitude
    round(Int8, magnitude)
  else
    floor(Int8, magnitude)
  end
  scale = n/Rational(10)^mag
  if scale ≈ 1
    Magnitude(mag)
  else
    s = round(Int, scale)
    ScaledMagnitude(mag, s ≈ scale ? s : scale)
  end
end

"map magnitudes to their standard name"
const prefixs = Dict(1 => :da,
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
