@require "." basefactor abbr Meter @export exports...

const imperial_units = Dict(1609344//1000 => :mile,
                            9144//10000 => :yard,
                            3048//10000 => :ft,
                            254//10000 => :inch)

struct ImperialLength{basefactor} <: Length value::Real end

for (factor, name) in imperial_units
  @eval @export $name = ImperialLength{$factor}
  @eval @export $(Symbol(name, '²')) = Area{$name}
  @eval @export $(Symbol(name, '³')) = Volume{$name}
end

# convert(m², 1acre) == 4046.8564224m²
@export acre = Area{ImperialLength{sqrt(43560) * (381//1250)}}

basefactor(::Type{ImperialLength{f}}) where f = f

# abbr(inch) == "inch"
abbr(::Type{ImperialLength{f}}) where f = string(imperial_units[f])
abbr(::Type{acre}) = "acre"

# promote(1inch, 1ft) == (1inch, 12inch)
Base.promote_rule(::Type{ImperialLength{f1}},::Type{ImperialLength{f2}}) where {f1,f2} =
  ImperialLength{min(f1,f2)}

# promote(1ft, 1m) == ((381//1250)m, 1m)
Base.promote_rule(::Type{<:ImperialLength}, ::Type{<:Meter}) = m
