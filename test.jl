include("main.jl")

promote(1, 2°C) == (1°C,2°C)
[promote(1mm,2)...] == [1mm,2mm]
sin(20°) == sind(20)
