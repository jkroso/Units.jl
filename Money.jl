@require "." Dimension basefactor abbr Ratio Time precise
@require "github.com/jkroso/Request.jl" GET

const rates = let
  data = parse(GET("http://api.fixer.io/latest?base=USD"))["rates"]
  push!(convert(Dict{Symbol,Float32}, data), :USD=>1)
end

abstract type Money <: Dimension end
struct Dollar{nation} <: Money value::Real end

# sprint(show, 1NZD) == "1.00 NZD"
Base.show(io::IO, d::Dollar) = @printf(io, "%0.2f %s", d.value, abbr(typeof(d)))

abbr(::Type{Dollar{c}}) where c = string(c)
basefactor(::Type{Dollar{abbr}}) where abbr = rates[abbr]
Base.promote_rule(::Type{<:Dollar}, ::Type{<:Dollar}) = Dollar{:USD}
Base.convert(::Type{A}, b::Dollar) where A<:Dollar =
  A(precise(b.value) * basefactor(A)/basefactor(typeof(b)))

const Wage = Ratio{<:Money,<:Time}
const USD = Dollar{:USD}
const NZD = Dollar{:NZD}
