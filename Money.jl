@use "github.com/jkroso/JSON.jl/read"
@use "." abbr scaler Dimension Time get_units
@use "./utils" seperate
@use Printf: @printf
@use FixedPointDecimals: FixedDecimal

const MoneyValue = FixedDecimal{Int128, 12}

abstract type Asset <: Dimension end
struct Money{nation} <: Asset
  value::MoneyValue
  Money{n}(v::Real) where n = new{n}(round(MoneyValue, v))
end

const Wage = Asset/Time

const symbols = let
  file = joinpath(@__DIR__(), "symbols.json")
  data = parse(MIME("application/json"), read(file))
  Dict{Symbol,Symbol}((Symbol(k)=>Symbol(v["symbol"]) for (k,v) in data)...)
end

"""
    exchange_rate(from::Symbol, to::Symbol)

Rate of `to` per 1 unit of `from`, as a `Real` (a `Rational` keeps conversion exact).

Units.jl ships no exchange-rate data. Rates are a property of a moment and often of a
consumer's own record-keeping — an accounting system reporting a prior quarter needs the
rate it filed with, not today's — so guessing one is worse than refusing. A consumer
supplies rates by adding a method:

    Units.Money.exchange_rate(from::Symbol, to::Symbol) = my_rate_lookup(from, to)

Until one does, any operation that would cross currencies errors instead of silently
converting.
"""
function exchange_rate end

exchange_rate(from, to) = error("""
  Units.Money has no exchange-rate source, so it cannot convert $from → $to.
  Add a method to supply one:

      Units.Money.exchange_rate(from::Symbol, to::Symbol) = ...
  """)

Base.show(io::IO, d::Money{code}) where code = begin
  n = seperate(sprint((io, x)->@printf(io, "%0.2f", x), d.value))
  if symbols[code] == :$
    write(io, "$n $code")
  else
    write(io, "$(symbols[code])$n")
  end
end

abbr(::Type{Money{c}}) where c = string(c)
scaler(::Type{Money{c}}) where c = exchange_rate(c, :USD)
Base.promote_rule(::Type{<:Money}, ::Type{<:Money}) = Money{:USD}

Base.show(io::IO, wage::Wage) = begin
  A = get_units(Asset, wage)[1]
  show(io, A(wage.value))
  write(io, '/', abbr(inv(get_units(Time, wage)[1])))
  nothing
end

for sym in [:USD :NZD :AUD :JPY :EUR :GBP]
  @eval const $sym = Money{$(QuoteNode(sym))}
  symbols[sym] == :$ || @eval const $(Symbol(symbols[sym])) = $sym
end
