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
    type(test_suite_type) :: specific_suite
    type(agent) :: a1 
    call initAgent(a1)
    ! example with default suite
    call test_suite_init('Test')
    call test_case_create('Test 1')

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

