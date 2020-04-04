

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   agentTools has all the tools for one agent     !
!   it holds the data structure agent, as well     !
!   as functions for movement and infection.       !
!   changing the transmission function or para-    !
!   meters should be done in this module.          !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module agentTools
    use ifport
    use konstanten
    implicit none
    
    
    type :: agent
        real(KIND=8), dimension(2) :: position
        real(KIND=8), dimension(2) :: velocity =(/0e0, 0e0 /)
        integer(KIND=1) :: state = HEALTHY! from -127 to 128
        integer(KIND=2), private :: time_tick = 1
    end type agent
    
    contains
    
    
    function getPairForce(a1, a2) !iterated over a2
        type(agent), intent(in) :: a1
        type(agent), intent(in) :: a2
        real(KIND=8), dimension(2) :: getPairForce     
        getPairForce = (a1%position - a2%position) *  (coupling(a1%state) + coupling(a2%state)) / (sqrt(sum((a1%position - a2%position)**2))**4.32193) !This factor comes from the Mathematica Script in the folder
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
    
    subroutine tick(a) !Probability to die should be implemented here (this could consider an age parameter as well)
        type(agent) :: a
        real :: p
        select case (a%state)
        case(HEALTHY, IMMUNE)
            return
        case(INFECTED)
            if (a%time_tick > ticks_before_infectious) then
                call RANDOM_NUMBER(p)
                if (p < no_symptoms_probabilty) then
                    a%state = NO_SYMPTOMS
                else
                    a%state = INFECTIOUS
                end if
                a%time_tick = 1
                return
            end if
        case(INFECTIOUS)
            if (a%time_tick > ticks_before_sick) then
                a%state = SICK
                a%time_tick = 1
                return
            end if
        case(SICK)
            if (a%time_tick > ticks_before_immune) then
                a%state = IMMUNE
                a%time_tick = 1
                return
            end if
        case(NO_SYMPTOMS)
            if (a%time_tick > ticks_before_immune + ticks_before_sick) then
                a%state = IMMUNE
                a%time_tick = 1
                return
            end if
        end select
        a%time_tick = a%time_tick + 1
            
    end subroutine
    
    subroutine transmission(a1, a2) !iterate over a2, switch a1 and a2 to take care that a2 can be infected too
        real :: p
        type(agent) :: a1, a2 
        if (a1%state /= HEALTHY .or. a2%state == HEALTHY .or. a2%state == IMMUNE .or. a2%state == INFECTED) return !already infected or immune
        !Billiardpotential and transmission (if distance is < transmission_radius than the probability of transmission is transmission_probability)
        if (sqrt(sum((a1%position - a2%position)**2)) < transmission_radius) then
            call RANDOM_NUMBER(p)
            if (p < transmission_probability) then !infection occured
                a1%state = INFECTED
            end if
        end if
    end subroutine
    
    subroutine updatePosition(a, f)
        type(agent) :: a
        real(KIND=8), dimension(2), intent(in) :: f
        real(KIND=8), dimension(2) :: pos
        a%velocity = a%velocity + ((f * 1e0/mass(a%state)) + dragg)
        if (a%velocity(1) > max_speed(1))  a%velocity(1) = max_speed(1)
        if (a%velocity(2) > max_speed(2))  a%velocity(2) = max_speed(2)
        pos = a%position + a%velocity
        if (pos(1) < 0 .or. pos(1) > x_max) a%velocity(1) = -a%velocity(1)
        if (pos(2) < 0 .or. pos(2) > y_max) a%velocity(2) = -a%velocity(2)
        
        a%position = a%position + a%velocity
    end subroutine updatePosition
    
end module agentTools