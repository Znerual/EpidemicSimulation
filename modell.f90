module modell_module
    use agentTools
    use list_module
    use konstanten
    implicit none
    
    private
    public modell_init
    public modell_tick
    public modell_information
    public modell_finish
    
    interface modell_init
        module procedure init_default
        module procedure init_n_agents
    end interface
    
    interface modell_tick
        module procedure tick_default
    end interface
    
    interface modell_information
        module procedure information_agents
    end interface
    
    interface modell_finish
        module procedure finish_default
    end interface
    
    type, public :: modell
        type(agent), dimension(:), allocatable, target :: a !List of agents
        type(list), dimension(:,:), allocatable, private :: grid 
        type(list), dimension(:,:,:), allocatable, private :: overlap_grid
        integer(KIND=4) :: n_agents = 4000_4, n_grid_x, n_grid_y,n_per_grid = 50
        character(len=64) :: error_string
        character(len=64) :: warning_string
        logical :: error
        logical :: warning
    end type modell
    
    integer :: i, j, k !For loops
    
    contains
    subroutine init_default(m)
        type(model) :: m
        m%n_grid_x = sqrt((1e0 *m%n_agents) / m%n_per_grid + 1.0e0)
        m%n_grid_y = sqrt((1e0 *m%n_agents) / m%n_per_grid + 1.0e0)
        call init_internal(m)
    end subroutine init_default
    subroutine init_n_agents(n_ag, m)
        integer(KIND=4), intent(in) :: n_ag
        type(model) :: m
        m%n_agents = n_ag      
        m%n_grid_x = sqrt((1e0 *m%n_agents) / m%n_per_grid + 1.0e0)
        m%n_grid_y = sqrt((1e0 *m%n_agents) / m%n_per_grid + 1.0e0)
        call init_internal(m)
    end subroutine init_n_agents
    subroutine init_internal(m)
        type(model) :: m
        allocate(m%a(m%n_agents))
        allocate(m%grid(m%n_grid_x, m%n_grid_y))
        allocate(m%overlap_grid(m%n_grid_x + 1,m%n_grid_y + 1,3)) ! +1 to save the edge terms too
        
        do i = 1, m%n_agents
            call agent_init(a(i))
            call setAgentInGrid(a(i), m%grid, m%overlap_grid)
            
        end do
        m%error = .false.
        m%warning = .false.
        m%warning_string = ''
        m%error_string = ''
    end subroutine init_internal
    
    subroutine setAgentInGrid(a1, gr, o_gr, m)
        type(agent), intent(in) :: a1
        type(agent), dimension(:,:) :: gr
        type(agent), dimension(:,:,:) :: o_gr
        type(model) :: m
        real(KIND=8) :: width_x, width_y, delta_x, delta_y
        real(KIND=8), parameter :: b1 = 1e0_8/6e0_8, b2 = 1e0_8/3e0_8, b3= 2e0_8/3e0_8, b4= 5e0_8/6e0_8 !Borders for grids
        width_x = (1e0_8 * x_max) / m%n_grid_x !TODO Transfer the x_max into the modell type
        width_y = (1e0_8 * y_max) / m%n_grid_y
        
        j = a(i)%position(1) / width_x
        k = a(i)%position(2) / width_y
        delta_x = (a(i)%position(1) - j * width_x) / width_x !Find the position inside one grid cell and normalice it, so that 1 means right border and 0 left border
        delta_y = (a(i)%position(2) - k * width_y) / width_y
        !Find the right place for the element, either inside the main grid or inside the overlapping grid
        if (delta_x < b2) then
            !inside left most
            if (delta_y < b2) then
                call add_element(a(i), overlap_grid(k, j, 2))
                continue
            end if
            if (delta_y > b3) then
                call add_element(a(i), overlap_grid(k, j, 4)) ! not finished
            end if
        end if
    end subroutine setAgentInGrid
    subroutine tick_default()
        print*, "Still to do"
    end subroutine tick_default
    
    subroutine information_agents(agents, num_agents, m)
        type(agent), dimension(:), pointer :: agents
        integer(KIND=4) :: num_agents
        type(model) :: m
        agents => m%a
        num_agents = m%n_agents
    end subroutine information_agents
    
    subroutine finish_default(m)
        type(model) :: m
        deallocate(m%a)
        do i = 1, m%n_grid_x
            do j = 1, m%n_grid_y
                free_all_elements(m%grid(i,j))
                if (m%grid(i,j)%error) then
                    m%error = .true.
                    write(m%error_string, '(A)') "finish default error ", m%grid(i,j)%error_string, " occured.
                end if
                do k = 1, 3
                    free_all_elements(m%overlap_grid(i,j,k))
                    if (m%overlap_grid(i,j,k)%error) then
                         m%error = .true.
                         write(m%error_string, '(A)') "finish default error ", m%overlap_grid(i,j,k)%error_string, " occured."
                    end  if
                end do
            end do
        end do
        deallocate(grid)
        deallocate(grid)
        deallocate(overlap_grid)
    end subroutine finish_default
end module modell_module