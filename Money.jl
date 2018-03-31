@require "github.com/jkroso/parse-json.jl"
@require "." abbr basefactor BaseUnit exports...

const rates = let
  file = joinpath(@__DIR__(), "rates.json")
  fstat = stat(file)
  if !ispath(fstat) || Dates.unix2datetime(fstat.mtime) < Dates.today()
    @require "github.com/jkroso/Request.jl" GET
    ispath(fstat) && rm(file)
    write(file, GET("http://api.fixer.io/latest?base=USD"))
  end
  data = parse(MIME("application/json"), read(file))["rates"]
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
