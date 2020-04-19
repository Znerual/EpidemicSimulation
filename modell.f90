module modell_module
    use agentTools
    use list_module
    use konstanten
    implicit none
    
    private
    public modell_init
    public modell_tick
    public modell_information_agents
    public modell_information_grid
    public modell_finish
    
    type, public :: modell
        type(agent), dimension(:), pointer:: a !List of agents
        type(list), dimension(:,:), pointer, private :: grid 
        type(list), dimension(:,:,:), pointer, private :: overlap_grid
        integer(KIND=4) :: n_agents = 4000_4, n_grid_x, n_grid_y,n_per_grid = 50
        character(len=1024) :: error_string
        character(len=64) :: warning_string
        logical :: error
        logical :: warning
    end type modell
    
    interface modell_init
        module procedure init_default
        module procedure init_n_agents
    end interface
    
    interface modell_tick
        module procedure tick_default
    end interface
    
    
      
    interface modell_finish
        module procedure finish_default
    end interface
    
    
    
    integer :: i, j, k !For loops
    
    contains
    subroutine init_default(m)
        type(modell) :: m
        m%n_grid_x = sqrt((1e0 *m%n_agents) / m%n_per_grid + 1.0e0)
        m%n_grid_y = sqrt((1e0 *m%n_agents) / m%n_per_grid + 1.0e0)
        call init_internal(m)
    end subroutine init_default
    subroutine init_n_agents(n_ag, m)
        integer(KIND=4), intent(in) :: n_ag
        type(modell) :: m
        m%n_agents = n_ag      
        m%n_grid_x = sqrt((1e0 *m%n_agents) / m%n_per_grid + 1.0e0)
        m%n_grid_y = sqrt((1e0 *m%n_agents) / m%n_per_grid + 1.0e0)
        call init_internal(m)
    end subroutine init_n_agents
    subroutine init_internal(m)
        type(modell) :: m
        allocate(m%a(m%n_agents))
        allocate(m%grid(m%n_grid_x, m%n_grid_y))
        allocate(m%overlap_grid(m%n_grid_x + 1,m%n_grid_y + 1,3)) ! +1 to save the edge terms too
        
        do i = 1, m%n_agents
            call agent_init(m%a(i))
            call setAgentInGrid(m%a(i), m)
            
        end do
        m%error = .false.
        m%warning = .false.
        m%warning_string = ''
        m%error_string = ''
    end subroutine init_internal
    subroutine modell_information_agents(agents, num_agents, m)
            type(agent), dimension(:), pointer :: agents
            integer(KIND=4) :: num_agents
            type(modell) :: m
            agents => m%a
            num_agents = m%n_agents
    end subroutine modell_information_agents
    subroutine modell_information_grid(input_grid, input_overlap_grid, input_m)
            type(list), dimension(:,:), pointer :: input_grid
            type(list), dimension(:,:,:), pointer :: input_overlap_grid
            type(modell) :: input_m
            input_grid => input_m%grid
            input_overlap_grid => input_m%overlap_grid
        end subroutine modell_information_grid
    subroutine setAgentInGrid(a1, m)
        type(agent), intent(in) :: a1
        type(modell) :: m
        real(KIND=8) :: width_x, width_y, delta_x, delta_y
        real(KIND=8), parameter :: b1 = 1e0_8/6e0_8, b2 = 1e0_8/3e0_8, b3= 2e0_8/3e0_8, b4= 5e0_8/6e0_8 !Borders for grids
        width_x = (1e0_8 * x_max) / m%n_grid_x !TODO Transfer the x_max into the modell type
        width_y = (1e0_8 * y_max) / m%n_grid_y
        
        j = a1%position(1) / width_x +1
        k = a1%position(2) / width_y +1
        delta_x = (a1%position(1) - j * width_x) / width_x !Find the position inside one grid cell and normalice it, so that 1 means right border and 0 left border
        delta_y = (a1%position(2) - k * width_y) / width_y
        !Find the right place for the element, either inside the main m%grid or inside the overlapping m%grid
        if (delta_x < b2) then
            !inside left most
            if (delta_y < b2) then
                call add_element(a1, m%overlap_grid(k, j, 2)) !topmost left corner
                return
            else if (delta_y > b3) then
                call add_element(a1, m%overlap_grid(k + 1, j, 2)) ! bottom left corner (4)
                return
            end if
            if (delta_x < b1 .and. (delta_y > b2 .and. delta_y < b3)) then !left edge
                call add_element(a1, m%overlap_grid(k, j, 3))
                return
            else
                call add_element(a1, m%grid(k, j)) !center m%grid
                return
            end if
        end if
        if (delta_x < b3) then
            if  (delta_y > b4) then !bottom edge (5)
                call add_element(a1, m%overlap_grid(k + 1,j,1))  
                return 
            else if (delta_y < b1) then !top edge
                call add_element(a1, m%overlap_grid(k,j,1)) 
                return
            else !in the center, main grid
                call add_element(a1, m%grid(k,j))
                return
            end if
        end if
        if (delta_x > b3) then
            if (delta_y < b2) then !top right corner (8)
                call add_element(a1, m%overlap_grid(k,j + 1,2)) 
                return 
            else if (delta_y > b3) then !bottom right corner (6)
                call add_element(a1, m%overlap_grid(k + 1,j + 1,2)) 
                return 
            end if
            if (delta_x > b4 .and. (delta_y > b2 .and. delta_y < b3)) then !right edge (7)
                call add_element(a1, m%overlap_grid(k,j +1 ,3)) 
                return 
            else
                call add_element(a1, m%grid(k,j)) !center grid
                return
            end if
        end if
        m%error = .true.
        write(m%error_string, '(A)') 'setAgentinGrid error: couldn t place agent in grid' 
    end subroutine setAgentInGrid
    subroutine tick_default()
        print*, "Still to do"
    end subroutine tick_default
    
    subroutine finish_default(m)
        type(modell) :: m
        
        do i = 1, m%n_grid_x
            do j = 1, m%n_grid_y
                call free_all_element(m%grid(i,j))
                if (m%grid(i,j)%error) then
                    m%error = .true.
                    write(m%error_string, '(A)') "finish default error " // trim(m%grid(i,j)%error_string) // " occured."
                end if
                do k = 1, 3
                    call free_all_element(m%overlap_grid(i,j,k))
                    if (m%overlap_grid(i,j,k)%error) then
                         m%error = .true.
                         write(m%error_string, '(A)') "finish default error " // trim(m%overlap_grid(i,j,k)%error_string) // " occured."
                    end  if
                end do
            end do
        end do
        deallocate(m%a)
        deallocate(m%grid)
        deallocate(m%overlap_grid)
    end subroutine finish_default
end module modell_module