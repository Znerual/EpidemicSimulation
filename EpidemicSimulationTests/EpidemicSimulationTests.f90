!  EpidemicSimulationTests.f90 
!
!  FUNCTIONS:
!  EpidemicSimulationTests - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: EpidemicSimulationTests
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program EpidemicSimulationTests
    use unit_test
    use agentTools
    implicit none

    ! Variables
    type(test_suite_type) :: test_suite_a
    type(agent), dimension(100) :: a
    integer :: i
    call initAgent(a1)
    ! example with default suite
    call test_suite_init('Test', test_suite_a)
    call test_case_create('Agent Initialisieren - Positionstests', test_suite_a)
    call initAgent(a(1))
    do i= 2,size(a)
        call initAgent(a(i))
        call assert_great_then(sum((a(i)%position - a(i-1)%position)**2),1e-3,__FILE__,__LINE__, suite=test_suite_a)
        call assert_great_then(x_max - a(i)%position(1), 0 ,__FILE__,__LINE__,suite=test_suite_a)
        call assert_great_then(y_max - a(i)%position(2), 0 ,__FILE__,__LINE__,suite=test_suite_a)
        call assert_great_then(a(i)%position(1), 0 ,__FILE__,__LINE__,suite=test_suite_a)
        call assert_great_then(a(i)%position(2), 0 ,__FILE__,__LINE__,suite=test_suite_a)
    end do
    call test_case_create('Agent wurde infiziert', test_suite_a)
    a(1)%state = 
    ! By sending macros __FILE__ and __LINE__, report will print the file and line number where assertion fails.
    call assert_approximate(1.0, 2.0, __FILE__, __LINE__) ! line 14
    call assert_equal(1,2, __FILE__, __LINE__)
    call assert_equal(a1%state, 1, __FILE__, __LINE__)
    ! report the complete suite
    call test_suite_report()

    call test_suite_final()




    ! Body of EpicSimulationTest





    ! Body of EpidemicSimulationTests
    print *, 'Hello World'

    end program EpidemicSimulationTests

