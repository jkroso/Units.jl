include("main.jl")

@test promote(1, 2°C) == (1°C,2°C)
@test promote(1mm,2) == (1mm,2mm)
@test sin(20°) == sind(20)
@test 1mm - 3 == -2mm
@test promote(1s, 2hr) == (1s,7200s)
@test convert(Degree, 1rad) == 57.29577951308232°
