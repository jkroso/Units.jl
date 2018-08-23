@require "github.com/jkroso/parse-json.jl"
@require "github.com/jkroso/Request.jl" GET
@require "." abbr basefactor BaseUnit exports...
import Dates: unix2datetime, today
import Printf.@printf

const rates = let
  file = joinpath(@__DIR__(), "rates.json")
  fstat = stat(file)
  if !ispath(fstat) || unix2datetime(fstat.mtime) < today()
    ispath(fstat) && rm(file)
    write(file, GET("http://data.fixer.io/api/latest?access_key=e4778c003b3cc59118912e5bd266b9ff"))
  end
  data = parse(MIME("application/json"), read(file))["rates"]
  Dict{Symbol,Rational}((Symbol(k)=>1/rationalize(v) for (k,v) ∈ data)...)
end

abstract type Money <: BaseUnit end
struct Dollar{nation} <: Money value::Real end

# sprint(show, 1NZD) == "1.00 NZD"
Base.show(io::IO, d::Dollar) =
  write(io, "$(seperate(sprint((io, x)->@printf(io, "%0.2f", x), d.value))) $(abbr(typeof(d)))")

seperate(value::Number; kwargs...) = seperate(string(convert(Float64, value)), kwargs...)
seperate(value::Integer; kwargs...) = seperate(string(value), kwargs...)
seperate(str::String, sep = ",", k = 3) = begin
  parts = split(str, '.')
  str = parts[1]
  n = length(str)
  groups = (str[max(x-k+1, 1):x] for x in reverse(n:-k:1))
  length(parts) == 1 && return join(groups, sep)
  join([join(groups, sep), parts[2]], '.')
end

abbr(::Type{Dollar{c}}) where c = string(c)
basefactor(::Type{Dollar{abbr}}) where abbr = rates[abbr]
Base.promote_rule(::Type{<:Dollar}, ::Type{<:Dollar}) = Dollar{:USD}

const Wage = Money/Time

for sym in [:USD :NZD :AUD :JPY :EUR :GBP :CNY]
  @eval const $sym = Dollar{$(QuoteNode(sym))}
end
