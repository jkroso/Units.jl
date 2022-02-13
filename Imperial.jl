@use "." abbr Meter Length Area Volume m scaler

const acre_side = rationalize(sqrt(4046.8564224))

const imperial_units = Dict{Rational,Symbol}(
  1609344//1000 => :mile,
  9144//10000 => :yard,
  3048//10000 => :ft,
  254//10000 => :inch,
  acre_side => :acre_side)

struct ImperialLength{basefactor} <: Length value::Real end

for (factor, name) in imperial_units
  name == :acre_side && continue
  @eval const $name = ImperialLength{$factor}
  @eval const $(Symbol(name, '²')) = Area{$name}
  @eval const $(Symbol(name, '³')) = Volume{$name}
end

const acre = Area{ImperialLength{acre_side}}

scaler(::Type{ImperialLength{f}}) where f = f

abbr(::Type{ImperialLength{f}}) where f = string(imperial_units[f])
abbr(::Type{acre}) = "acre"

Base.promote_rule(::Type{ImperialLength{f1}},::Type{ImperialLength{f2}}) where {f1,f2} =
  ImperialLength{min(f1,f2)}

Base.promote_rule(::Type{<:ImperialLength}, ::Type{<:Meter}) = m
