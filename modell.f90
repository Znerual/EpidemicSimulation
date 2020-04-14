module modell
    use agentTools
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
    
    type(agent), dimension(:), allocatable, target :: a !List of agents
    type(agent), dimension(:,:), allocatable, private :: grid 
    type(agent), dimension(:,:,:), allocatable, private :: overlap_grid
    integer(KIND=4) :: n_agents = 4000_4, n_grid_x, n_grid_y,n_per_grid = 50
    integer :: i, j, k !For loops
    character(len=64) :: error_string
    character(len=64) :: warning_string
    logical :: error
    logical :: warning
    contains
    subroutine init_default()        
        n_grid_x = sqrt((1e0 *n_agents) / n_per_grid + 1.0e0)
        n_grid_y = sqrt((1e0 *n_agents) / n_per_grid + 1.0e0)
        call init_internal()
    end subroutine init_default
    subroutine init_n_agents(n_ag)
        integer(KIND=4), intent(in) :: n_ag
        n_agents = n_ag      
        n_grid_x = sqrt((1e0 *n_agents) / n_per_grid + 1.0e0)
        n_grid_y = sqrt((1e0 *n_agents) / n_per_grid + 1.0e0)
        call init_internal()
    end subroutine init_n_agents
    subroutine init_internal()        
        allocate(a(n_agents))
        allocate(grid(n_grid_x, n_grid_y))
        allocate(overlap_grid(n_grid_x + 1,n_grid_y + 1,3)) ! +1 to save the edge terms too
        
        do i = 1, n_agents
            call agent_init(a(i))
            call setAgentInGrid(a(i), grid, overlap_grid)
            
        end do
        error = .false.
        warning = .false.
        warning_string = ''
        error_string = ''
    end subroutine init_internal
    
    subroutine setAgentInGrid(a1, gr, o_gr)
        type(agent), intent(in) :: a1
        type(agent), dimension(:,:) :: gr
        type(agent), dimension(:,:,:) :: o_gr
        real(KIND=8) :: width_x, width_y, delta_x, delta_y
        real(KIND=8), parameter :: b1 = 1e0_8/6e0_8, b2 = 1e0_8/3e0_8, b3= 2e0_8/3e0_8, b4= 5e0_8/6e0_8 !Borders for grids
        width_x = (1e0_8 * x_max) / n_grid_x
        width_y = (1e0_8 * y_max) / n_grid_y
        
        j = a(i)%position(1) / width_x
        k = a(i)%position(2) / width_y
        delta_x = (a(i)%position(1) - j * width_x) / width_x !Find the position inside one grid cell and normalice it, so that 1 means right border and 0 left border
        delta_y = (a(i)%position(2) - k * width_y) / width_y
        !Find the right place for the element, either inside the main grid or inside the overlapping grid
        if (delta_x < b2) then
            !inside left most
            if (delta_y < b2) then
                overlap_grid(k, j, 2) = a(i)
                continue
            end if
            if (delta_y > b3) then
                overlap_grid(k, j, 4) = a(i) ! not finished
            end if
        end if
    end subroutine setAgentInGrid
    subroutine tick_default()
        print*, "Still to do"
    end subroutine tick_default
    
    subroutine information_agents(agents, num_agents)
        type(agent), dimension(:), pointer :: agents
        integer(KIND=4) :: num_agents
        agents => a
        num_agents = n_agents
    end subroutine information_agents
    
    subroutine finish_default()
        deallocate(a)
        deallocate(grid)
        deallocate(overlap_grid)
        if (error .eqv. .true.) print*, "Error: ", error_string, " occured."
        if (warning .eqv. .true.) print*, "Warning: ", warning_string, " occured."
    end subroutine finish_default
end module modell