include("main.jl")

@test sin(20°) == sind(20)
@test 1mm - 3 == -2mm
@test promote(1s, 2hr) == (1s,7200s)
@test convert(Degree, 1rad) == 57.29577951308232°
@test sqrt(100m^2) == 10m
