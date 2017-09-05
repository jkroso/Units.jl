@require "." Length Exponent basefactor abbr Meter m Area Volume precise

const imperial_units = Dict(1609344//1000 => :mile,
                            9144//10000 => :yard,
                            3048//10000 => :ft,
                            254//10000 => :inch)

struct ImperialLength{basefactor} <: Length value::Real end

for (factor, name) in imperial_units
  @eval const $name = ImperialLength{$factor}
  @eval const $(Symbol(name, '²')) = Area{$name}
  @eval const $(Symbol(name, '³')) = Volume{$name}
end

basefactor(::Type{ImperialLength{f}}) where f = f
# abbr(inch) == "inch"
abbr(::Type{ImperialLength{f}}) where f = string(imperial_units[f])

# promote(1inch, 1ft) == (1inch, 12inch)
Base.promote_rule(::Type{ImperialLength{f1}},::Type{ImperialLength{f2}}) where {f1,f2} =
  ImperialLength{min(f1,f2)}

# promote(1ft, 1m) == ((381//1250)m, 1m)
Base.promote_rule(::Type{<:ImperialLength}, ::Type{<:Meter}) = m
