module list_module
    use agentTools
    implicit none
    type :: node
        type(agent), pointer :: ag
        type(node), pointer :: next => null()
    end type
    type, public :: list
        type(node), pointer :: first => null()
        type(node), pointer :: current => null()
        type(node), pointer :: last => null()
        integer :: size = 0
        logical :: EOL, error
        character(len=128) :: error_string
    end type
    
    private
    public add_element
    public delete_current_element
    public delete_first_element
    public next_element
    public last_element
    public first_element
    public get_element
    public free_all_element
    public get_agent
    public rewind_list
    
    interface add_element
        module procedure add_agent
    end interface
    
    interface delete_current_element
        module procedure delete_current
    end interface
    
    interface delete_first_element
        module procedure delete_first_element
    end interface
    interface next_element
        module procedure get_next
    end interface
    
    interface last_element
        module procedure get_last
    end interface
    interface first_element
        module procedure get_first
    end interface
    
    interface get_element
        module procedure get
    end interface
    
    interface free_all_element
        module procedure free_all
    end interface
    
    interface get_agent
        module procedure get_agent
    end interface
    
    interface rewind_list
        module procedure rewind_list
    end interface
    
    
    contains
    subroutine add_agent(a, l)
        implicit none
        type(agent), target, intent(in) :: a
        type(list) :: l
        type(node), pointer :: new_node, temp_node
        
        allocate(new_node)
        new_node%ag => a
        if(.not. associated(l%first)) then
            l%first => new_node
            l%current => new_node
            l%last => new_node
        else
            temp_node => l%first
            l%first => new_node
            new_node%next => temp_node
            l%current => l%first
        end if
        l%size = l%size + 1
    end subroutine add_agent
    
    subroutine delete_current(l)
        type(list) :: l
        type(node), pointer :: tmp, tmp_old, tmp_f
        integer :: i
        if(.not. associated(l%current)) then 
            l%error = .true.
            l%EOL = .true.
            write(l%error_string, '(A)') 'List-error: current node not allocated'
            return
        end if
        if (l%size == 1 .or. associated(l%current, l%first) .and. l%size >= 1) then !Only one element in list or delete the first element
            tmp_f => l%first%next
            deallocate(l%first)
            
            l%size = l%size - 1
            if (l%size <= 0) l%EOL =  .true.
            l%first => tmp_f
            l%current => l%first
            return
        end if
        tmp_old => l%first
        tmp => l%first%next
        do i = 1, l%size
            if (associated(l%current, tmp)) then
                if(.not. associated(tmp%next)) then !Last element deleted
                    l%current => tmp_old
                    l%last => tmp_old
                    l%EOL = .true.
                    deallocate(tmp)
                    l%size = l%size - 1
                    return
                end if
                if (associated(l%first, tmp)) then
                    l%first => tmp%next
                    deallocate(l%first)
                    l%size = l%size - 1
                    l%current => l%first
                    return
                end if
                tmp_old%next => tmp%next
                l%current => tmp%next
                deallocate(tmp)
                l%size = l%size - 1
                return
            end if
            tmp => tmp%next
            tmp_old => tmp
        end do
        l%error =.true.
        write(l%error_string, '(A)') 'List-error: did not delete a element with delete current element'
    end subroutine delete_current
    
    subroutine delete_first_element(l)
        type(list) ::l
        type(node), pointer :: tmp
        if (.not. associated(l%first)) then
            l%error =.true.
            l%EOL = .true.
            write(l%error_string, '(A)') 'List-error: delete first element no first element'
            return
        end if
        
        tmp => l%first
        deallocate(l%first)
        l%first => tmp%next
        
        if (l%size <= 0) then
            l%EOL = .true.
        else
            l%size = l%size - 1
        end if
        
        
    end subroutine
    subroutine get_next(l)
        type(list) :: l
        if(l%EOL) then
            l%error = .true.
            write(l%error_string, '(A)')'List-error: EOL reached, no next element'
            return
        end if
        if (.not. associated(l%current)) then
            l%error = .true.
            write(l%error_string, '(A)')'List-error: get_next no current element'
            return
        end if
        l%current => l%current%next
        if (.not. associated(l%current%next)) l%EOL = .true. !comparetwo pointers with associated
    end subroutine get_next
    
    
    subroutine get_first(l)
        type(list) :: l
        l%current => l%first
    end subroutine
    
    subroutine get_last(l)
        type(list) :: l
        l%current => l%last
    end subroutine
    
    function get(l)
        type(list) :: l
        type(node), pointer :: get
        get => l%current
    end function get
    
    subroutine free_all(l)
        type(list) :: l
        call rewind_list(l)
        do while(l%EOL .eqv. .false.)
            call delete_current_element(l)
        end do
        if (l%size /= 0) then
            l%error = .true.
            write(l%error_string, '(A)') 'List-error: List not completely deleted'
        end if
    end subroutine free_all
    function get_agent(l)
        type(list) :: l
        type(agent) :: get_agent
        if (.not. associated(l%current)) then
            l%error = .true.
            write(l%error_string, '(A)') 'List-error: get_agent no current element'
            return
        end if
        get_agent = l%current%ag
    end function get_agent
    subroutine rewind_list(l)
        type(list) :: l
        l%EOL = .false.
        l%error_string = ''
        l%error = .false.
        l%current => l%first
    end subroutine
end module list_module