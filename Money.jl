@require "github.com/jkroso/Request.jl" GET
@require "." abbr basefactor BaseUnit exports...

const rates = let
  data = parse(GET("http://api.fixer.io/latest?base=USD"))["rates"]
  Dict{Symbol,Rational}((Symbol(k)=>1/rationalize(v) for (k,v) ∈ data)..., :USD=>1)
end

abstract type Money <: BaseUnit end
struct Dollar{nation} <: Money value::Real end

# sprint(show, 1NZD) == "1.00 NZD"
Base.show(io::IO, d::Dollar) = @printf(io, "%0.2f %s", d.value, abbr(typeof(d)))

abbr(::Type{Dollar{c}}) where c = string(c)
basefactor(::Type{Dollar{abbr}}) where abbr = rates[abbr]
Base.promote_rule(::Type{<:Dollar}, ::Type{<:Dollar}) = Dollar{:USD}

const Wage = Money/Time

for sym in [:USD :NZD :AUD :JPY :EUR :GBP :CNY]
  @eval const $sym = Dollar{$(QuoteNode(sym))}
end
