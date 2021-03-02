@use "github.com/JuliaLang/Downloads.jl" download
@use "github.com/jkroso/parse-json.jl"
@use "." abbr basefactor BaseUnit seperate exports...
import Dates: unix2datetime, today
import Printf.@printf

struct Money{nation} <: BaseUnit
  value::Real
end

const symbols = let
  file = joinpath(@__DIR__(), "symbols.json")
  data = parse(MIME("application/json"), read(file))
  Dict{Symbol,Symbol}((Symbol(k)=>Symbol(v["symbol"]) for (k,v) ∈ data)...)
end

const rates = let
  file = joinpath(@__DIR__(), "rates.json")
  fstat = stat(file)
  if !ispath(fstat) || unix2datetime(fstat.mtime) < today()
    ispath(fstat) && rm(file)
    download("https://api.exchangerate.host/latest?base=USD", file)
  end
  data = parse(MIME("application/json"), read(file))["rates"]
  Dict{Symbol,Rational}((Symbol(k)=>1/rationalize(v) for (k,v) ∈ data)...)
end

Base.show(io::IO, d::Money{code}) where code = begin
  n = seperate(sprint((io, x)->@printf(io, "%0.2f", x), d.value))
  if symbols[code] == :$
    write(io, "$n $code")
  else
    write(io, "$(symbols[code])$n")
  end
end

abbr(::Type{Money{c}}) where c = string(c)
basefactor(::Type{Money{abbr}}) where abbr = rates[abbr]
Base.promote_rule(::Type{<:Money}, ::Type{<:Money}) = Money{:USD}

const Wage = Money/Time

for sym in [:USD :NZD :AUD :JPY :EUR :GBP]
  @eval const $sym = Money{$(QuoteNode(sym))}
  symbols[sym] == :$ || @eval const $(Symbol(symbols[sym])) = $sym
end
