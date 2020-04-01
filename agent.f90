#define HEALTHY 1
#define SICK 2
#define NO_SYMPTOMS 3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   agentTools has all the tools for one agent     !
!   it holds the data structure agent, as well     !
!   as functions for movement and infection.       !
!   changing the transmission function or para-    !
!   meters should be done in this module.          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module agentTools
    use ifport
    implicit none
    
    real, parameter, dimension(3) :: coupling = (/ 0e0, 0e0, 1e0 /) !gives the coupling strength depending on the state 
    real, parameter, dimension(3) :: mass = (/ 1e0, 1.5e0, 1e5 /) !to change the movement bevaviour
    real, parameter :: dragg = -1e-1 !to slow the movement, when changed to a positive value, the movement gets faster
    real :: time_step = 1e1
    real, parameter :: transmission_probability = 1e-1, no_symptoms_probabilty = 2e-2, transmission_radius = 1e0
    real, parameter, private :: x_max = 100e0, y_max = 100e0
    type :: agent
        real, dimension(2) :: position
        real, dimension(2) :: velocity
        integer :: state !1 means not infected
    end type agent
    
    contains
    function getPairForce(a1, a2) !iterated over a2
        type(agent), intent(in) :: a1
        type(agent), intent(in) :: a2
        real, dimension(2) :: getPairForce     
        getPairForce = (a1%position - a2%position) *  coupling(a1%state) * coupling(a2%state) / (abs(a1%position - a2%position)**3)
    end function getPairForce
    
    subroutine initAgent(a)
        type(agent) :: a
        real :: x ,y
        call RANDOM_NUMBER(x)
        call RANDOM_NUMBER(y)
        
        a%position = (/x * x_max, y*y_max/)
        a%velocity = (/ 0e0, 0e0/)
        a%state = 1
    end subroutine initAgent
    
    subroutine transmission(a1, a2) !iterate over a2, switch a1 and a2 to take care that a2 can be infected too
        real :: p
        type(agent) :: a1, a2 
        if (a1%state /= HEALTHY .or. a2%state == HEALTHY) return !already infected
        !Billiardpotential and transmission (if distance is < transmission_radius than the probability of transmission is transmission_probability)
        if (sqrt(sum((a1%position - a2%position)**2)) < transmission_radius) then
            call RANDOM_NUMBER(p)
            if (p < transmission_probability) then !infection occured
                call RANDOM_NUMBER(p)
                if (p < no_symptoms_probabilty) then
                    a1%state = NO_SYMPTOMS
                else
                    a1%state = SICK
                end if
            end if
        end if
    end subroutine
    
    subroutine updatePosition(a, f)
        type(agent) :: a
        real, dimension(2), intent(in) :: f
        real, dimension(2) :: pos
        a%velocity = a%velocity + (f / mass(a%state) + dragg) * time_step
        pos = a%position + a%velocity * time_step
        if (pos(1) < 0 &
            .or. pos(2) < 0 &
            .or. pos(1) > x_max &
            .or. pos(2) > y_max) & !To check boundary conditions
        a%velocity = -a%velocity
        a%position = a%position + a%velocity * time_step
    end subroutine updatePosition
    
end module agentTools