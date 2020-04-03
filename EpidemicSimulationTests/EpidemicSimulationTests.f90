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
    real n
    ! example with default suite
    call test_suite_init('agentToolsTest', test_suite_a)


    call test_case_create('Agent Initialisieren - Positionstests - Zahlen sind zufällig', test_suite_a)
    call initAgent(a(1))
    do i= 2,size(a)
        call initAgent(a(i))
        call assert_great_than(sum((a(i)%position - a(i-1)%position)**2),1e-3,__FILE__,__LINE__, test_suite_a)
        call assert_great_than(x_max - a(i)%position(1), 0e0 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(y_max - a(i)%position(2), 0e0 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(a(i)%position(1), 0e0 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(a(i)%position(2), 0e0 ,__FILE__,__LINE__,test_suite_a)
    end do


    call test_case_create('Agent wurde infiziert - transmission', test_suite_a)
    a(1)%state = cHEALTHY()
    a(2)%state = cINFECTIOUS()
    a(1)%position = (/ 1e0,1e0 /)
    a(2)%position = (/ 1.01e0, 1.01e0 /)
    n = 0
    do i = 0, tpd * 1000
        call transmission(a(1), a(2))
        if (a(1)%state == cINFECTED()) then
            a(1)%state = cHEALTHY()
            n = n +1
        end if
    end do
    call assert_approximate(n, transmission_probability * 1000,  __FILE__, __LINE__, 1e0, test_suite_a)


    call test_case_create("Makro Function test", test_suite_a)
    call assert_equal(cHEALTHY(), 1_1,  __FILE__, __LINE__, test_suite_a)
    call assert_equal(cINFECTED(), 2_1,  __FILE__, __LINE__, test_suite_a)
    call assert_equal(cSICK(), 4_1,  __FILE__, __LINE__, test_suite_a)


    call test_case_create("Force test", test_suite_a)
    a(1)%position = (/ 1e0,1e0 /)
    a(2)%position = (/ 1.01e0, 1.01e0 /)
    a(1)%state = cHEALTHY()
    a(2)%state = cINFECTIOUS()
    call assert_equal(sum((getPairForce(a(1), a(2)))**2), 0e0_8, __FILE__, __LINE__, test_suite_a)
    a(1)%state = cSICK()
    a(2)%state = cINFECTIOUS()
    call assert_great_than(sum((getPairForce(a(1), a(2)))**2), 0e0_8, __FILE__, __LINE__, test_suite_a)
    a(1)%state = cHEALTHY()
    a(2)%state = cINFECTIOUS()
    call assert_equal(sum((getPairForce(a(1), a(2)))**2), 0e0_8, __FILE__, __LINE__, test_suite_a)


    ! report the complete suite
    call test_suite_report(suite=test_suite_a)

    call test_suite_final(suite=test_suite_a)


    end program EpidemicSimulationTests

