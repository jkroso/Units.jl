@require "." Unit basefactor abbr Ratio Time
@require "github.com/JuliaMath/FixedPointDecimals.jl" FixedDecimal
@require "github.com/jkroso/Request.jl" GET

const rates = let
  data = parse(GET("http://api.fixer.io/latest?base=USD"))["rates"]
  push!(convert(Dict{Symbol,Float32}, data), :USD=>1)
end

abstract type Money <: Unit end
struct Dollar{nation} <: Money
  value::FixedDecimal{Int,2}
end

abbr{c}(::Type{Dollar{c}}) = string(c)
basefactor{abbr}(::Type{Dollar{abbr}}) = rates[abbr]
Base.promote_rule{a,b}(::Type{Dollar{a}}, ::Type{Dollar{b}}) = Dollar{:USD}
Base.convert{A<:Dollar}(::Type{A}, b::Dollar) = A(b.value * basefactor(A)/basefactor(typeof(b)))

const Wage = Ratio{<:Money,<:Time}
const USD = Dollar{:USD}
const NZD = Dollar{:NZD}
