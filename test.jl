@use "." dimension abstract_dimension conversion_factor abbr Combination AbstractCombination Exponent Meter Gram Second hr m mm cm km m² s g kg Length Time day yr minute ton Speed Acceleration ns basefactor
@use "github.com/jkroso/Rutherford.jl/test.jl" @test testset
@use "./utils" Magnitude ScaledMagnitude

@test abbr(AbstractCombination{Tuple{m²,hr^-1}}) == "m²/hr"
@test abbr(AbstractCombination{Tuple{m²,hr^1}}) == "m²·hr"
@test abbr(m²*hr) == "m²·hr"
@test abbr(m²/hr) == "m²/hr"
@test m/1e3 == mm
@test m^3/1e9 == mm^3
@test mm^3*1e9 == m^3
@test mm*1e3 == m
@test conversion_factor(m^3, mm^3) == 1000_000_000
@test conversion_factor(mm^3, m^3) == 1//1000_000_000
@test conversion_factor(m^2,cm^2) == 10_000
@test conversion_factor(hr^-1,s^-1) == 1//3600
@test conversion_factor(cm^2/s, m^2/hr) == 9//25
@test isapprox(1/5km/minute, inv(5minute/km))
@test dimension(m²) == Exponent{<:Length,2}
@test dimension(m/s) == AbstractCombination{<:Tuple{Exponent{<:Length,1},Exponent{<:Time,-1}}}
@test dimension(m) == Length^1
@test dimension(s) == Time^1
@test abstract_dimension(Length^1) == Length
@test abstract_dimension(Meter{0}^1) == Length
@test convert(Real, 1km) == 1000
@test 1km == 1km
@test convert(km, 1) == 1km
@test convert(km, 1000m) == 1km
@test convert(day^-1, 1/yr) == (1//365)day^-1
@test 2cm == Meter{Magnitude(-2)}(2)
@test 3 * 1cm == 3cm
@test 1cm + 1mm == 11mm && 1cm - 1mm == 9mm
@test -(1m) == -1m
@test 1/m == (m^-1)(1)
@test s^1 <: Time^1
@test m^2 == m²
@test m²^2 == m^4
@test (1m²)^2 == 1m^4
@test (2m²)^2 == (2m²)*(2m²)
@test convert(m^3, 1000_000_000mm^3) == 1m^3
@test promote(1cm^2, 1m²) == (1cm^2, 10_000cm^2)
@test sqrt(100m^2) == 10m
@test promote(1mm, 2m) == (1mm, 2000mm)
@test abbr(Second{Magnitude(-12)}) == "ps"
@test promote(1s, 1hr) == (1s, 3600s)
@test 1g < 2g
@test 1kg > 2g
@test 1kg > 2
@test 1 < 1kg
@test isless(1kg, 2kg)
@test m^2*s^0 == m^2
@test promote(1m/s, 9km/hr) == (1m/s, 2.5m/s)
@test 1/5yr ≈ 0.2yr
@test (60m/s) / (1/minute) == 3600m
@test 1m/s == (m/s)(1)
@test 1m/s^2 == (m/s^2)(1)
@test 1m²/s^2 == (m²/s^2)(1)
@test m * m == m²
@test m * cm == Combination{Tuple{Exponent{<:Length, 2}}, Tuple{m^1, cm^1}}
@test m^1 * m² == m^3
@test m * m² == m^3
@test 1m² * 2m² == 2m^4
@test 5s * (1m/s) == 5m
@test 1m * 2m == 2m²
@test 1minute * (1m/s) == 60m
@test 3g * (1000m/kg) == 3m
@test 1000_000_000mm^3 * (2.5ton/m^3) == 2.5ton
@test 1mm^2 * 2cm^1 == 20mm^3
@test 2m²/1m² == 2
@test 1s/5s ≈ 1//5
@test 1m/5s ≈ 0.2m/s
@test 1.1s/1m² == 1.1s/m²
@test (1.1s^2)/1m == 1.1s^2/m && 1m²/2m == 0.5m
@test 1m²/200cm^2 == 50 && 1m^3/200cm^2 == 5000cm
@test (2m^4)/(2m²) == 1m²
@test (1.1s^2)/1m² == (1.1s^2)/m²
@test 1kg/m * 1cm == (1//100)kg
@test m/s <: AbstractCombination{<:Tuple{Length^1,Time^-1}}
@test s/m² <: AbstractCombination{<:Tuple{Time^1,Length^-2}}
@test s^2/m² <: AbstractCombination{<:Tuple{Time^2,Length^-2}}
@test m/s <: Speed
@test m/s^2 <: Acceleration
@test (km/hr)^2 == (km^2/hr^2)
@test (12km/hr)^2 == (12^2)*(km^2/hr^2)
@test (km/hr)^-2 == (km^-2/hr^-2)
@test convert(s, 1ns) ≈ 1e-9s
@test 1m ÷ 49mm == 20
@test 1000mm ÷ 49 == 20mm
@test 1m % 49mm == 20mm
@test round(900.9mm) == 901mm
@test floor(900.1mm) == 900mm
@test ceil(900.1mm) == 901mm
@test round(m, 900mm) == 1m
@test round(9.6742mm, digits=2) == 9.67mm
@test basefactor(m*s) == 1
@test convert(km/hr, inv(5minute/km)) == 12km/hr
@test convert(km/hr, 5minute/km) == 12km/hr

@use "." kWh W mW kW MW J hr kJ K V A kV
testset("DerivedUnit") do
  @test basefactor(kV*A) == 1000
  @test conversion_factor(kV*A, kW) == 1
  @test conversion_factor(mW, W) == 1//1000
  @test conversion_factor(W, mW) == 1000
  @test conversion_factor(kW, MW) == 1//1000
  @test conversion_factor(W, W) == 1
  @test 1kW/(200W/m²) == 5m²
  @test convert(J, 2W*hr) == 7200J
  @test convert(W*hr, 3600J*4) == 4W*hr
  @test convert(kg*m^2/s^2, 1W*hr) == 3600kg*m^2/s^2
  @test convert(W*hr, 3600kg*m^2/s^2) == 1W*hr
  @test convert(kJ, 3600kg*m^2/s^2) ≈ 3.6kJ
  @test 1kW * (1K/kW) == 1K
  @test convert(kWh, 3600J) ≈ 0.001kWh
  @test 1kW*5m/kW == 5m
end

@use "./Money.jl" Wage AUD USD
testset("Money") do
  @test AUD/hr <: Wage
  @test 1.5AUD/hr isa Wage
  @test string(1AUD) == "1.00 AUD"
  @test string(1.5AUD/hr) == "1.50 AUD/hr"
  @test string(1.527AUD/hr) == "1.53 AUD/hr"
  @test convert(USD, 100AUD) != 100USD
  @test 1.2AUD%1AUD ≈ 0.2AUD
  @test 1.2AUD%1 ≈ 0.2AUD
  @test 1.2AUD÷1AUD == 1
  @test 1.2AUD÷1 == 1AUD
end

@use "." kb kbit bit
testset("Data") do
  @test convert(kb, 8000bit) == 1kb
  @test convert(kbit, 1kb) == 8kbit
end

@use "." ms ns s
testset("Sleep") do
  @test 0.8 < @elapsed(sleep(1s)) < 1.2
  @test 0.3 < @elapsed(sleep(500ms)) < 0.7
  @test 0.001 < @elapsed(sleep(5000ns)) < 0.01
end

@use "." Percent month yr ° rad
testset("dimensionless units") do
  @test convert(Percent/month, 15Percent/yr) == 1.25Percent/month
  @test round(Percent(10.547), digits=2) == Percent(10.55)
  @test Percent/yr == Combination{Tuple{Time^-1}, Tuple{Percent^1,yr^-1}}
  @test 10Percent/yr == 10Combination{Tuple{Time^-1}, Tuple{Percent^1,yr^-1}}
  @test 10Percent/yr * 5yr == 50Percent
  @test convert(m, 20m*(10Percent)) == 2m
  @test 100m + 100m*(10Percent) == 110m
  @test 100AUD - 10Percent == 90AUD
  @test 100 - 10Percent == 90
  @test 100AUD + 10Percent == 110AUD
  @test 100 + 10Percent == 110

  testset("ScaledMagnitude") do
    sm = ScaledMagnitude(Int8(3), 2.5)  # Represents 2.5 × 10^3
    @test convert(Float64, inv(sm)) ≈ 1/convert(Float64, sm)
    @test convert(Float64, inv(sm)) ≈ 1/(2.5 * 10^3)
  end

  testset("Angles") do
    @test (60m/s) / (1°/minute) == 3600m/°
    @test convert(°, 1rad) == 57.29577951308232°
    @test sin(90°) == 1
    @test sin(0°) == 0
    @test sin(45°) ≈ sind(45)
  end
end

@use "./Imperial.jl" acre inch ft lb stone
testset("imperial") do
  @test convert(m², 1acre) ≈ 4046.8564224m²
  @test abbr(inch) == "inch"
  @test convert(ft, 12inch) == 1ft
  @test convert(m, 1ft) ≈ (381//1250)m

  testset("Imperial mass units") do
    @test convert(kg, 1lb) ≈ 0.45359237kg
    @test convert(lb, 1stone) == 14lb
    @test convert(kg, 1stone) ≈ 6.35029318kg
  end
end

import Dates: Hour, Minute
@use "." pm am
testset("time shorthand") do
  @test 5pm - (7:30am) == Hour(9)+Minute(30)
  @test Minute(30) - 1minute == Minute(29)
  @test Minute(30) - 60s == Minute(29)
end
