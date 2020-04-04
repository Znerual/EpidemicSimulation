

module konstanten
implicit none
    
real, parameter, dimension(6) :: coupling = (/ 0e0, 0e0, 0e0, 1e0, 0e0, 0e0 /) !gives the coupling strength depending on the state 
real, parameter, dimension(6) :: mass = (/ 1e0, 1.5e0, 1.5e0, 1e5, 1.5e0,1e0 /) !to change the movement bevaviour
real, dimension(2) :: dragg = (/-1e-2, -1e-2/) !to slow the movement, when changed to a positive value, the movement gets faster
integer, parameter :: tpd = 24, ticks_before_infectious = 4 * tpd, ticks_before_sick = 8 * tpd, ticks_before_immune = 14 * tpd!tpd ... ticks per day
real, parameter :: transmission_probability = 1e-1 /tpd , no_symptoms_probabilty = 2e-2 / tpd, transmission_radius = 1e0
real, parameter :: x_max = 100e0, y_max = 100e0
real(KIND=8), parameter, dimension(2):: max_speed = (/x_max / 3e0_8, y_max / 3e0_8 /)
integer(KIND=1), parameter :: HEALTHY = 1, INFECTED = 2, INFECTIOUS =3, SICK = 4, NO_SYMPTOMS = 5, IMMUNE = 6 

end module