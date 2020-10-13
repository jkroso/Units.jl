@use "." conversion_factor abbr abstract_unit baseunit simplify Combination Exponent Meter Gram Second Degree exports...
@use "github.com/jkroso/Rutherford.jl/test.jl" @test

@test abbr(Combination{Tuple{m²,hr^-1},0}) == "m²/hr"
@test abbr(Combination{Tuple{m²,hr^1},0}) == "m²·hr"
@test conversion_factor(m³, mm³) == 1//1000_000_000
@test conversion_factor(m^2,cm^2) == 1//10_000
@test conversion_factor(hr^-1,s^-1) == 3600//1
@test conversion_factor(m²/hr, cm²/s) == 9//25
@test conversion_factor(mW, W) == 1000
@test conversion_factor(W, mW) == 1//1000
@test conversion_factor(kW, MW) == 1000
@test conversion_factor(W, W) == 1
@test abstract_unit(m²) == Exponent{2,<:Length}
@test abstract_unit(m/s) == Combination{<:Tuple{Exponent{1,<:Length},Exponent{-1,<:Time}},0}
@test abstract_unit(m) == Length
@test abstract_unit(s) == Time
@test baseunit(Length^1) == Length
@test baseunit(Meter{0}^1) == Length
@test baseunit(m/s) == Combination{Tuple{Exponent{1,Length},Exponent{-1,Time}}, 0}
@test convert(Real, 1km) == 1000
@test convert(km, 1) == 1km
@test convert(km, 1000m) == 1km
@test convert(day^-1, 1/year) == (1/365.2425)day^-1
@test 2cm == Meter{-2}(2)
@test 3 * 1cm == 3cm
@test 1cm + 1mm == 11mm && 1cm - 1mm == 9mm
@test -(1m) == -1m
@test 1/m == (m^-1)(1)
@test Exponent{1,Second{1//1}} <: Exponent{1,<:Time}
@test m^2 == m²
@test m²^2 == m^4
@test (1m²)^2 == 1m^4
@test convert(m³, 1000_000_000mm³) == 1m³
@test promote(1cm², 1m²) == (1cm², 10_000cm²)
@test sqrt(100m^2) == 10m
@test promote(1mm, 2m) == (1mm, 2000mm)
@test abbr(Second{-1000_000_000_000//1}) == "ps"
@test promote(1s, 1hr) == (1s, 3600s)
@test convert(Degree, 1rad) == 57.29577951308232°
@test sin(20°) == sind(20)
@test 1g < 2g
@test 1kg > 2g
@test 1kg > 2
@test 1 < 1kg
@test simplify(Combination{Tuple{m^2,s^0},0}) == m^2
@test promote(1m/s, 9km/hr) == (1m/s, 2.5m/s)
@test (60m/s) / (1°/minute) == 3600m/°
@test (60m/s) / (1/minute) == 3600m
@test 1m/s == (m/s)(1)
@test 1m/s^2 == (m/s^2)(1)
@test 1m²/s^2 == (m²/s^2)(1)
@test m * m == m² && m * cm == cm²
@test m^1 * m² == m³
@test m * m² == m³
@test 1m² * 2m² == 2m^4
@test 5s * (1m/s) == 5m
@test 1m * 2m == 2m²
@test 1minute * (1m/s) == 60m
@test 3g * (1000m/kg) == 3m
@test 1000_000_000mm³ * (2.5ton/m³) == 2.5ton
@test Combination{Tuple{s^1},0}(12) * Combination{Tuple{km^1, minute^-1},0}(1) == (1//5)km
@test 1mm² * 2cm^1 == 20mm³
@test 2m²/1m² == 2
@test 1s/5s == 1//5
@test 1m/5s == 0.2m/s
@test 1.1s/1m² == 1.1s/m²
@test (1.1s^2)/1m == 1.1s^2/m && 1m²/2m == 0.5m
@test 1m²/200cm² == 50 && 1m³/200cm² == 5000cm
@test (2m^4)/(2m²) == 1m²
@test (1.1s^2)/1m² == (1.1s^2)/m²
@test 1kg/m * 1cm == 0.01kg
@test m/s == Combination{Tuple{m^1,s^-1},0}
@test s/m² == Combination{Tuple{s^1,m^-2},0}
@test s^2/m² == Combination{Tuple{s^2,m^-2},0}
@test Combination{Tuple{s^1},0} * Combination{Tuple{s^2},0} == s^3
@test Combination{Tuple{s^2},0}-Combination{Tuple{m^2},0} == Combination{Tuple{s^2,m^-2},0}
@test m/s <: Speed
@test m/s^2 <: Acceleration
@test convert(s, 1ns) == 1e-9s
