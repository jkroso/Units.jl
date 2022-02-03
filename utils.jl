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

struct Magnitude <: Real value::Int8 end
Base.convert(::Type{N}, m::Magnitude) where N<:Number = convert(N, Rational(10)^m.value)
Base.promote_rule(::Type{N}, ::Type{Magnitude}) where N<:Number = N
Base.promote_rule(::Type{<:Integer}, ::Type{Magnitude}) = Rational
Base.:/(a::Magnitude, b::Magnitude) = Magnitude(a.value - b.value)
Base.:*(a::Magnitude, b::Magnitude) = Magnitude(a.value + b.value)
Base.:<(a::Magnitude, b::Magnitude) = a.value < b.value
Base.:-(a::Magnitude) = Magnitude(-a.value)
Base.:-(a::Magnitude, b::Magnitude) = Magnitude(a.value - b.value)
Base.:+(a::Magnitude, b::Magnitude) = Magnitude(a.value + b.value)
Base.:^(m::Magnitude, n::Integer) = Rational(10)^(m.value*n)
const exponents = Vector{Char}("⁰¹²³⁴⁵⁶⁷⁸⁹")
Base.show(io::IO, m::Magnitude) = begin
  s = join((exponents[d+1] for d in reverse!(digits(abs(m.value)))))
  write(io, "10$(m.value < 0 ? "⁻" : "")$s")
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
