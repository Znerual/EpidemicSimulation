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
    use modell_module
    use konstanten
    use list_module
    implicit none

    ! Variables
    type(test_suite_type) :: test_suite_a, test_suite_modell, test_suite_liste
    type(agent), target, dimension(100) :: a
    
    
    
   
    ! example with default suite
    call agentTests()
    !call listTests()
    !call modellTests()
    !read(*,*) !To not close the console
    end program EpidemicSimulationTests

subroutine agentTests()
    use konstanten
    use agentTools
    use unit_test
    implicit none
        type(agent), target, dimension(100) :: a
        type(test_suite_type) :: test_suite_a
        real(KIND=8), dimension(2) :: f
        real n
        real(KIND=8) :: t1, t2
        integer :: i,j
        call test_suite_init('agentToolsTest', test_suite_a)


        call test_case_create('Agent Initialisieren - Positionstests - Zahlen sind zufällig', test_suite_a)
        call agent_init(a(1))
        do i= 2,size(a)
            call agent_init(a(i))
            call assert_great_than(sum((a(i)%position - a(i-1)%position)**2),1e-3_8,__FILE__,__LINE__, test_suite_a)
            call assert_great_than(x_max - a(i)%position(1), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
            call assert_great_than(y_max - a(i)%position(2), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
            call assert_great_than(a(i)%position(1), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
            call assert_great_than(a(i)%position(2), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        end do


        call test_case_create('Agent wurde infiziert - agent_transmission', test_suite_a)
        a(1)%state = HEALTHY
        a(2)%state = INFECTIOUS
        a(1)%position = (/ 1e0,1e0 /)
        a(2)%position = (/ 1.01e0, 1.01e0 /)
        n = 0
        do i = 0, tpd * 1000
            call agent_transmission(a(1), a(2))
            if (a(1)%state == INFECTED) then
                a(1)%state = HEALTHY
                n = n +1
            end if
        end do
        call assert_approximate(n, transmission_probability * 1000,  __FILE__, __LINE__, 1e0, test_suite_a)


        call test_case_create("Makro Function test", test_suite_a)
        call assert_equal(HEALTHY, 1_1,  __FILE__, __LINE__, test_suite_a)
        call assert_equal(INFECTED, 2_1,  __FILE__, __LINE__, test_suite_a)
        call assert_equal(SICK, 4_1,  __FILE__, __LINE__, test_suite_a)


        call test_case_create("Force test", test_suite_a)
        a(1)%position = (/ 1e0,1e0 /)
        a(2)%position = (/ 1.01e0, 1.01e0 /)
        a(1)%state = HEALTHY
        a(2)%state = INFECTIOUS
        call assert_equal(sum((agent_pairForce(a(1), a(2)))**2), 0e0_8, __FILE__, __LINE__, test_suite_a)
        a(1)%state = SICK
        a(2)%state = INFECTIOUS
        call assert_great_than(sum((agent_pairForce(a(1), a(2)))**2), 0e0_8, __FILE__, __LINE__, test_suite_a)
        a(1)%state = HEALTHY
        a(2)%state = INFECTIOUS
        call assert_equal(sum((agent_pairForce(a(1), a(2)))**2), 0e0_8, __FILE__, __LINE__, test_suite_a)
        a(1)%position = (/ 1e0,1e0 /)
        a(2)%position = (/ 2e0, 1e0 /)
        a(1)%state = SICK
        a(2)%state = HEALTHY
        a(3)%position = (/ 1e0,2e0 /)
        a(4)%position = (/ 1e0, 1e0 /)
        a(3)%state = SICK
        a(4)%state = HEALTHY
        call assert_equal(sum((agent_pairForce(a(1), a(2)))**2), sum((agent_pairForce(a(3), a(4)))**2), __FILE__, __LINE__, test_suite_a)
        a(1)%position = (/ 1e0,1e0 /)
        a(2)%position = (/ 3e0, 1e0 /)
        a(1)%state = SICK
        a(2)%state = HEALTHY
        t1 = sqrt(sum((agent_pairForce(a(1), a(2))**2)))
        call assert_approximate(t1,1e-1_8, __FILE__, __LINE__,1e-4_8, test_suite_a)
    
    
    
    
        call test_case_create("tick test", test_suite_a)
        a(1)%state = HEALTHY
        do i = 0, 1000
            call agent_tick(a(1))
        end do
        call assert_equal(a(1)%state, HEALTHY, __FILE__, __LINE__, test_suite_a)
        a(1)%state = INFECTED
        do i = 0, ticks_before_infectious
            call agent_tick(a(1))
        end do
        call assert_equal(a(1)%state, INFECTIOUS, __FILE__, __LINE__, test_suite_a)
        do i = 0, ticks_before_sick
            call agent_tick(a(1))
        end do
        call assert_equal(a(1)%state, SICK, __FILE__, __LINE__, test_suite_a)
        do i = 0, ticks_before_immune
            call agent_tick(a(1))
        end do
        call assert_equal(a(1)%state, IMMUNE, __FILE__, __LINE__, test_suite_a)
        n = 0
        a(1)%state = INFECTED
        do i = 0, tpd * 1000
            do j = 0, ticks_before_infectious
            call agent_tick(a(1))
            end do
            if (a(1)%state == NO_SYMPTOMS) then
                a(1)%state = INFECTED
                n = n +1
            end if
        end do
        call assert_approximate(n, no_symptoms_probabilty * 1000,  __FILE__, __LINE__, 1e0, test_suite_a)

    
    
        call test_case_create("positional update test", test_suite_a)
        f = (/ -2e0, 3e0 /)
        a(1)%position = (/1e0, 1e0 /)
        do i = 0, 100
            call agent_updatePosition(a(1),f)
        end do
        call assert_great_than(x_max - a(1)%position(1), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(y_max - a(1)%position(2), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(a(1)%position(1), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(a(1)%position(2), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        dragg = (/0e0,0e0/)
        f = (/ 1e0, 0e0 /)
        a(1)%position = (/1e0, 1e0/)
        a(1)%velocity = (/0e0, 0e0 /)
        a(1)%state = HEALTHY
        n = 0e0
        t1 =0e0
        t2 = 1e0
        do i = 1, 10
            call agent_updatePosition(a(1),f)
            t1 = t1 + 1
            t2 = t2 + t1 
        end do
        call assert_equal(a(1)%position, (/t2, 1e0_8/), __FILE__, __LINE__, test_suite_a)
        f = (/ 2e0, 3e0 /)
        a(1)%position = (/1e0, 1e0 /)
        do i = 0, 100
            call agent_updatePosition(a(1),f)
        end do
        call assert_great_than(x_max - a(1)%position(1), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(y_max - a(1)%position(2), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(a(1)%position(1), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(a(1)%position(2), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
    
    
        f = (/ -2e0, -3e0 /)
        a(1)%position = (/0e0, 0e0 /)
        do i = 0, 100
            call agent_updatePosition(a(1),f)
        end do
        call assert_great_than(x_max - a(1)%position(1), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(y_max - a(1)%position(2), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(a(1)%position(1), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(a(1)%position(2), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        f = (/ 2e0, 3e0 /)
        a(1)%position = (/x_max, y_max /)
        do i = 0, 100
            call agent_updatePosition(a(1),f)
        end do
        call assert_great_than(x_max - a(1)%position(1), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(y_max - a(1)%position(2), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(a(1)%position(1), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call assert_great_than(a(1)%position(2), 0e0_8 ,__FILE__,__LINE__,test_suite_a)
        call test_suite_report(suite=test_suite_a)

        call test_suite_final(suite=test_suite_a)
    end subroutine agentTests
   
    subroutine listTests
        use agentTools
        use list_module
        use unit_test
        implicit none
        type(test_suite_type) :: test_suite_liste
        type(agent), target, dimension(100) :: a
        type(list) :: l
        integer :: i
        
        !Initialice the agents
        do i = 1, 100
            call agent_init(a(i))
        end do
        !Test the list module
        call test_suite_init('Listen Test', test_suite_liste)
        call test_case_create('List Initialisieren', test_suite_liste)
        do i = 1, 100
            call add_element(a(i), l)
        end do  
        call assert_equal(agent_toString(get_agent(l)), agent_toString(a(100)), __FILE__,__LINE__, test_suite_liste)
        call assert_equal(l%size, 100, __FILE__,__LINE__, test_suite_liste)
        call test_case_create('Liste vergleichen', test_suite_liste)
        do i = 0, 99
            if( l%error) then
                print*,__LINE__ ,  l%error_string, " bei i= ", i
                exit
            end if
            call assert_equal(agent_toString(get_agent(l)), agent_toString(a(100-i)), __FILE__ ,i, test_suite_liste)
            call next_element(l)
            if( l%error) then
                print*,__LINE__ ,  l%error_string, " bei i= ", i
                exit
            end if
        end do
        call assert_true(l%EOL, __FILE__,__LINE__, test_suite_liste)
        call rewind_list(l)
        call test_case_create('Element einzeln löschen', test_suite_liste)
        call next_element(l)
        do i = 0 , 98
            call delete_current_element(l)
            if (l%error) print*,__LINE__ ,  l%error_string, " bei i= ", i
        end do
        call assert_equal(agent_toString(get_agent(l)), agent_toString(a(100)), __FILE__,__LINE__, test_suite_liste)
        call rewind_list(l)
        do i = 1, 3
            call add_element(a(100 - i), l)
        end do
        call test_case_create('Liste erneut vergleichen nach löschen', test_suite_liste)
        call rewind_list(l)
        do i = 97, 100
            call assert_equal(agent_toString(get_agent(l)), agent_toString(a(i)), __FILE__ ,i, test_suite_liste)
            call next_element(l)
            if( l%error) then
                print*,__LINE__ ,  l%error_string, " bei i= ", i
                !exit
            end if
        end do
        call test_case_create('Alle Elemente löschen', test_suite_liste)
        call free_all_element(l)
        call assert_equal(l%size, 0, __FILE__,__LINE__, test_suite_liste)
        if( l%error) then
            print*, l%error_string, " mit size ", l%size
        end if
        call assert_true(.not. associated(l%first), __FILE__,__LINE__, test_suite_liste)
        call test_suite_report(suite=test_suite_liste)
        call test_suite_final(suite=test_suite_liste)
    end subroutine listTests
    
    
    
    subroutine modellTests
        use unit_test
        use modell_module
        implicit none
     !Test the modell
        type(modell) :: m
        type(test_suite_type) :: test_suite_modell
        type(agent), dimension(:), pointer :: a_m   
        integer(kind=4) :: n_agents
        
        call test_suite_init('Modell Test', test_suite_modell)
        call test_case_create('Modell Initialisieren - grid', test_suite_modell)
        m%n_per_grid = 5
        m%n_agents = 15
        call modell_init()
        call assert_equal(m%n_grid_x, 3,__FILE__,__LINE__, test_suite_modell)
        call assert_equal(m%n_grid_y, 3,__FILE__,__LINE__, test_suite_modell)
    
        call modell_information(agents=a_m, num_agents=n_agents)
        call assert_equal(size(a_m),n_agents ,__FILE__,__LINE__, test_suite_modell)
        call modell_finish(m)
        call test_suite_report(suite=test_suite_modell)
    
        call test_suite_final(suite=test_suite_modell)
    end subroutine modellTests
     