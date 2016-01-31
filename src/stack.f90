!
!  iterator.f90
!  This file is part of FIAT.
!
!  Copyright 2016 Christopher MacMackin <cmacmackin@gmail.com>
!  
!  This program is free software; you can redistribute it and/or modify
!  it under the terms of the GNU Lesser General Public License as
!  published by the Free Software Foundation; either version 3 of the 
!  License, or (at your option) any later version.
!  
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Lesser General Public License for more details.
!  
!  You should have received a copy of the GNU Lesser General Public
!  License along with this program; if not, write to the Free Software
!  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
!  MA 02110-1301, USA.
!  

module stack_mod
  use ordered_mod only: ordered_type
  use abstract_container_mod only: container_type
  use linked_node_mod only: linked_node
  private
  implicit none
  
  type, extends(ordered_type), public :: stack_type
    private
    class(container_type), allocatable :: container
    type(linked_node), pointer :: head => null()
    type(linked_node), pointer :: iter_pos => head
    integer :: num_nodes = 0
  contains
    procedure :: has_next => stack_has_next
    procedure :: next => stack_next
    procedure :: reset => stack_reset
    procedure :: copy => stack_copy
    procedure :: size => stack_size
    procedure :: push => stack_push
    procedure :: pop => stack_pop
    procedure :: peek => stack_peek
    procedure :: clear => stack_clear
    procedure, private :: concat => stack_concat
    procedure, private :: move_head => stack_move_head
    final :: stack_final
  end type stack_type

  interface stack_type
    module procedure :: constructor
  end interface stack_type
    
contains
  
  function constructor(container) result(new)
    class(container_type), intent(in) :: container
    type(stack_type) :: new
    allocate(new%container, mold=container)
  end function constructor
  
  elemental logical function stack_has_next(this)
    class(stack_type), intent(in) :: this
    stack_has_next = associated(this%iter_pos)
  end function stack_has_next
  
  function stack_next(this)
    class(stack_type), intent(inout) :: this
    class(container_type), allocatable :: stack_next
    if (.not. this%has_next()) then
      write(stderr,*) "ERROR: Bottom of stack reached."
#ifdef __GFORTRAN__
      call backtrace
#endif
      stop
    end if
    allocate(stack_next, source=this%iter_pos%get_contents())
    if (this%iter_pos%has_next()) then
      this%iter_pos => this%iter_pos%get_next()
    else
      this%iter_pos => null()
    end if
  end function stack_next
  
  subroutine stack_reset(this)
    class(stack_type), intent(inout) :: this
    this%iter_pos => this%head
  end subroutine stack_reset
  
  elemental function stack_copy(this)
    class(stack_type), intent(in) :: this
    class(stack_type), allocatable :: stack_copy
    type(linked_node), pointer :: node1, node2 => null()
    stack_copy = this
    allocate(node1, source=this%head)
    stack_copy%head => node1
    allocate(node2, source=node1%get_next())
    do while (associated(node2))
      node1%set_next(node2)
      nullify(node1)
      move_alloc(node2, node1)
      allocate(node2, source=node1%get_next())
    end do
  end function stack_copy
  
  elemental integer function stack_size(this)
    class(stack_type), intent(in) :: this
    stack_size = this%num_nodes
  end function stack_size
  
  subroutine stack_push(this, item)
    class(stack_type), intent(inout) :: this
    class(*), intent(in) :: item
    type(linked_node), pointer :: newnode
    type(container_type), allocatable :: newcont
    allocate(newnode)
    allocate(newcont, source=this%container)
    call newcont%set(item)
    call newnode%set_contents(newcont)
    call newnode%set_next(this%head)
    this%head => newnode
    this%num_nodes = this%num_nodes + 1
  end subroutine stack_push
  
  function stack_pop(this) result(item)
    class(stack_type), intent(inout) :: this
    class(container_type), allocatable :: item
    type(linked_node), pointer :: tmp
    move_alloc(this%peek(), item)
    tmp => this%head
    this%head => this%head%get_next()
    deallocate(tmp)
    this%num_nodes = this%num_nodes - 1
  end function stack_pop
  
  subroutine stack_clear(this)
    class(stack_type), intent(out) :: this
    continue
  end subroutine clear
  
  pure function stack_peek(this) result(item)
    class(stack_type), intent(in) :: this
    class(container_type), allocatable :: item
    move_alloc(this%head%get_contents(), item)
  end function stack_peek
  
  elemental function stack_concat(lhs, rhs)
    class(stack_type), intent(in) :: lhs, rhs
    type(stack_type) :: stack_concat
    type(stack_type) :: tmp
    type(linked_node), pointer :: tail
    stack_concat%head => lhs%copy()%move_head()
    if (stack_concat%size() == 0) then
      stack_concat%head => rhs%copy()%move_head()
    else
      tail => stack_concat%head
      do while(tail%has_next)
        tail => tail%get_next()
      end do
      tail%set_next(rhs%copy()%move_head())
    end if
    nullify(tail)
  end function stack_concat

  function stack_move_head(this)
    class(stack_type), intent(inout) :: this
    type(linked_node), pointer :: move_head
    move_head => this%head
    nullify(this%head)
  end function move_head
  
  elemental subroutine stack_final(this)
    class(stack_type), intent(inout) :: this
    nullify(this%iter_pos)
    this%head%unset_next(.true.)
    deallocate(this%head)
  end subroutine stack_final

end module stack_mod
