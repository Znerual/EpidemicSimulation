!  EpidemicSimulation.f90 
!
!  FUNCTIONS:
!  EpidemicSimulation - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: EpidemicSimulation
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program EpidemicSimulation
    use agentTools
    implicit none

    ! Variables
    type(agent) :: a1,a2
    
    call random_seed()
    call initAgent(a1)
    call initAgent(a2)
    ! Body of EpidemicSimulation
    a2%state = 2
    call  transmission(a1,a2)
    print *, 'Hello World', a1%position, a2%position, a1%state

    end program EpidemicSimulation

