@use "github.com/JuliaLang/Downloads.jl" download
@use "github.com/jkroso/parse-json.jl"
@use "." abbr scaler Dimension Time get_units
@use "./utils" seperate
import Dates: unix2datetime, today
import Printf.@printf

abstract type Asset <: Dimension end
struct Money{nation} <: Asset
  value::Real
end

const Wage = Asset/Time

const symbols = let
  file = joinpath(@__DIR__(), "symbols.json")
  data = parse(MIME("application/json"), read(file))
  Dict{Symbol,Symbol}((Symbol(k)=>Symbol(v["symbol"]) for (k,v) in data)...)
end

const rates = let
  file = joinpath(@__DIR__(), "rates.json")
  fstat = stat(file)
  if !ispath(fstat) || unix2datetime(fstat.mtime) < today()
    ispath(fstat) && rm(file)
    download("https://api.exchangerate.host/latest?base=USD", file)
  end
  data = parse(MIME("application/json"), read(file))["rates"]
  Dict{Symbol,Rational}((Symbol(k)=>1/rationalize(v) for (k,v) in data)...)
end

Base.show(io::IO, d::Money{code}) where code = begin
  n = seperate(sprint((io, x)->@printf(io, "%0.2f", x), d.value))
  if symbols[code] == :$
    write(io, "$n $code")
  else
    write(io, "$(symbols[code])$n")
  end
end

Base.show(io::IO, wage::Wage) = begin
  A = get_units(Asset, wage)[1]
  show(io, A(wage.value))
  write(io, '/', abbr(inv(get_units(Time, wage)[1])))
  nothing
end

abbr(::Type{Money{c}}) where c = string(c)
scaler(::Type{Money{abbr}}) where abbr = rates[abbr]
Base.promote_rule(::Type{<:Money}, ::Type{<:Money}) = Money{:USD}

for sym in [:USD :NZD :AUD :JPY :EUR :GBP]
  @eval const $sym = Money{$(QuoteNode(sym))}
  symbols[sym] == :$ || @eval const $(Symbol(symbols[sym])) = $sym
end
