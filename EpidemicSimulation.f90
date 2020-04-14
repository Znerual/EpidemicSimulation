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
    use list_module
    implicit none

    ! Variables
    type(agent) :: a1,a2
    type(list) :: l
    call random_seed()
    call agent_init(a1)
    call agent_init(a2)

    ! Body of EpidemicSimulation
   
    call  agent_transmission(a1,a2)
    print *, 'Hello World', a1%position, a2%position, a1%state

    end program EpidemicSimulation

