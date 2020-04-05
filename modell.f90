module modell
    use agentTools
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
    type(agent), dimension(:,:, :), allocatable, private :: overlap_grid
    integer(KIND=4) :: n_agents = 4000_4, n_grid_x, n_grid_y,n_per_grid = 50
    integer :: i, j, k !For loops
    character(64) :: error_string
    character(64) :: warning_string
    logical :: error
    logical :: warning
    contains
    subroutine init_default()
        allocate(a(n_agents))
        n_grid_x = sqrt((1e0 *n_agents) / n_per_grid + 1.0e0)
        n_grid_y = sqrt((1e0 *n_agents) / n_per_grid + 1.0e0)
        allocate(grid(n_grid_x, n_grid_y))
        allocate(overlap_grid(n_grid_x,n_grid_y,5))
        error = .false.
        warning = .false.
        warning_string = ''
        error_string = ''
    end subroutine
    subroutine init_n_agents(n_ag)
        integer(KIND=4), intent(in) :: n_ag
        n_agents = n_ag
        allocate(a(n_agents))
        n_grid_x = sqrt((1e0 *n_agents) / n_per_grid + 1.0e0)
        n_grid_y = sqrt((1e0 *n_agents) / n_per_grid + 1.0e0)
        allocate(grid(n_grid_x, n_grid_y))
        allocate(overlap_grid(n_grid_x,n_grid_y,5))
        error = .false.
        warning = .false.
        warning_string = ''
        error_string = ''
        
    end subroutine
    subroutine tick_default()
    
    end subroutine
    
    subroutine information_agents(agents, num_agents)
        type(agent), dimension(:), pointer :: agents
        integer(KIND=4) :: num_agents
        agents => a
        num_agents = n_agents
    end subroutine
    
    subroutine finish_default()
        deallocate(a)
        deallocate(grid)
        deallocate(overlap_grid)
        if (error) print*, "Error: ", error_string, " occured."
        if (warning) print*, "Warning: ", warning_string, " occured."
    end subroutine
end module modell