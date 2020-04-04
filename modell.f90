module modell
    use agentTools
    implicit none
    
    public init
    public tick
    public information
    public finish
    
    interface init
        module procedure init_default
        module procedure init_n_agents
    end interface
    
    interface tick
        module procedure tick_default
    end interface
    
    interface information
        module procedure information_default
    end interface
    
    interface finish
        module procedure finish_default
    end interface
    
    type(agent), dimension(:), allocatable, private :: a !List of agents
    integer(KIND=4) :: n_agents = 100_4, n_grid_x, n_grid_y
    integer :: i, j, k !For loops
    character(64) :: error_string
    character(64) :: warning_string
    logical :: error
    logical :: warning
    contains
    subroutine init_default()
    
    end subroutine
    subroutine init_n_agents(n_ag)
        integer(KIND=4), intent(in) :: n_ag
        n_agents = n_ag
        allocate(a(n_agents))
    end subroutine
    subroutine tick_default()
    
    end subroutine
    
    subroutine information_default()
    
    end subroutine
    
    subroutine finish_default()
    
    end subroutine
end module modell