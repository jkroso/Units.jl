@use "." => Units abbr Meter Length Area Volume m scaler Mass kg g @scaledunit @abbreviate gravity s litre

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

@abbreviate acre Area{ImperialLength{acre_side}}

scaler(::Type{ImperialLength{f}}) where f = f
abbr(::Type{ImperialLength{f}}) where f = string(imperial_units[f])

Base.promote_rule(::Type{ImperialLength{a}},::Type{ImperialLength{b}}) where {a,b} = ImperialLength{min(a,b)}
Base.promote_rule(::Type{<:ImperialLength}, ::Type{<:Meter}) = m

const L = litre
const mL = L/1000
@abbreviate tsp Volume{ImperialLength{cbrt(convert(m^3, 4.92892159375mL).value)}}
@abbreviate tbsp Volume{ImperialLength{cbrt(convert(m^3, 14.78676478125mL).value)}}
@abbreviate cup Volume{ImperialLength{cbrt(convert(m^3, 236.5882365mL).value)}}
@abbreviate pint Volume{ImperialLength{cbrt(convert(m^3, 473.176473mL).value)}}
@abbreviate quart Volume{ImperialLength{cbrt(convert(m^3, 0.946352946L).value)}}
@abbreviate gallon Volume{ImperialLength{cbrt(convert(m^3, 3.785411784L).value)}}
@abbreviate barrel Volume{ImperialLength{cbrt(convert(m^3, 158.987294928L).value)}}
@abbreviate fl_oz Volume{ImperialLength{cbrt(convert(m^3, 28.41306mL).value)}}

struct ImperialMass{basefactor} <: Mass value::Real end
@abbreviate lb ImperialMass{rationalize(0.45359237)*1000}
Base.promote_rule(::Type{ImperialMass{a}},::Type{ImperialMass{b}}) where {a,b} = ImperialMass{min(a,b)}
Base.promote_rule(::Type{<:ImperialMass}, ::Type{M}) where M<:Mass = M
scaler(::Type{ImperialMass{f}}) where f = f
@abbreviate oz ImperialMass{scaler(lb)/16}

@scaledunit slug 32.174049lb
@scaledunit lbf 1lb*convert(ft/s^2, gravity)
@abbreviate psi lbf/inch²
