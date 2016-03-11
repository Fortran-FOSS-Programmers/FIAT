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

!~ module stack_mod
!~   use iso_fortran_env, only: stderr => error_unit
!~   use iterator_mod, only: iterator
!~   use ordered_mod, only: ordered
!~   use abstract_container_mod, only: container
!~   use linked_node_mod, only: linked_node
!~   implicit none
!~   private
  
!~   type, extends(ordered), public :: stack
!~     private
!~     class(container), allocatable :: container_obj
!~     type(linked_node), pointer :: head => null()
!~     type(linked_node), pointer :: iter_pos => null()
!~     integer :: num_nodes = 0
!~   contains
!~     procedure :: has_next => stack_has_next
!~     procedure :: next => stack_next
!~     procedure :: reset => stack_reset
!~     procedure :: copy => stack_copy
!~     procedure :: size => stack_size
!~     procedure :: push => stack_push
!~     procedure :: pop => stack_pop
!~     procedure :: peek => stack_peek
!~     procedure :: clear => stack_clear
!~     procedure, private :: stack_assign
!~     generic :: assignment(=) =>  stack_assign
!~     procedure, private :: concat => stack_concat
!~     procedure, private :: move_head => stack_move_head
!~     final :: stack_final
!~   end type stack

!~   interface stack
!~     module procedure :: constructor
!~   end interface stack
    
!~ contains
  
!~   function constructor(container_obj) result(new)
!~     class(container), intent(in) :: container_obj
!~     type(stack) :: new
!~     allocate(new%container_obj, mold=container_obj)
!~     new%iter_pos => new%head
!~   end function constructor
  
!~   elemental logical function stack_has_next(this)
!~     class(stack), intent(in) :: this
!~     stack_has_next = associated(this%iter_pos)
!~   end function stack_has_next
  
!~   function stack_next(this)
!~     class(stack), intent(inout) :: this
!~     class(container), allocatable :: stack_next
!~     if (.not. this%has_next()) then
!~       write(stderr,*) "ERROR: Bottom of stack reached."
!~ #ifdef __GFORTRAN__
!~       call backtrace
!~ #endif
!~       stop
!~     end if
!~     allocate(stack_next, source=this%iter_pos%get_contents())
!~     if (this%iter_pos%has_next()) then
!~       this%iter_pos => this%iter_pos%get_next()
!~     else
!~       this%iter_pos => null()
!~     end if
!~   end function stack_next
  
!~   subroutine stack_reset(this)
!~     class(stack), intent(inout) :: this
!~     this%iter_pos => this%head
!~   end subroutine stack_reset
  
!~   function stack_copy(this)
!~     class(stack), intent(in) :: this
!~     class(iterator), allocatable :: stack_copy
!~     class(stack), allocatable :: tmp
!~     type(linked_node), pointer :: node1, node2 => null()
!~     allocate(tmp, source=this)
!~     allocate(node1, source=this%head)
!~     tmp%head => node1
!~     allocate(node2, source=node1%get_next())
!~     do while (associated(node2))
!~       call node1%set_next(node2)
!~       nullify(node1)
!~       node1 => node2
!~       nullify(node2)
!~       allocate(node2, source=node1%get_next())
!~     end do
!~     call move_alloc(tmp, stack_copy)
!~   end function stack_copy
  
!~   subroutine stack_assign(lhs, rhs)
!~     class(stack), intent(out) :: lhs
!~     class(stack), intent(in) :: rhs
!~     class(iterator), allocatable :: copy
!~     lhs%num_nodes = rhs%num_nodes
!~     lhs%container_obj = rhs%container_obj
!~     if (lhs%num_nodes > 0) then
!~       call move_alloc(rhs%copy(), copy)
!~       select type(copy)
!~         class is(stack)
!~           lhs%head => copy%head
!~           lhs%iter_pos => copy%iter_pos
!~           if (associated(copy%head)) nullify(copy%head)
!~           if (associated(copy%iter_pos)) nullify(copy%iter_pos)
!~       end select
!~     end if
!~   end subroutine stack_assign
  
!~   integer function stack_size(this)
!~     class(stack), intent(in) :: this
!~     stack_size = this%num_nodes
!~   end function stack_size
  
!~   subroutine stack_push(this, item)
!~     class(stack), intent(inout) :: this
!~     class(*), intent(in) :: item
!~     type(linked_node), pointer :: newnode
!~     class(container), allocatable :: newcont
!~     allocate(newnode)
!~     allocate(newcont, source=this%container_obj)
!~     call newcont%set(item)
!~     call newnode%set_contents(newcont)
!~     call newnode%set_next(this%head)
!~     this%head => newnode
!~     this%num_nodes = this%num_nodes + 1
!~   end subroutine stack_push
  
!~   function stack_pop(this) result(item)
!~     class(stack), intent(inout) :: this
!~     class(container), allocatable :: item
!~     type(linked_node), pointer :: tmp
!~     item = this%peek()
!~     tmp => this%head
!~     this%head => this%head%get_next()
!~     deallocate(tmp)
!~     this%num_nodes = this%num_nodes - 1
!~   end function stack_pop
  
!~   subroutine stack_clear(this)
!~     class(stack), intent(inout) :: this
!~   contains
!~     subroutine blank_stack(s)
!~       class(stack), intent(out) :: s
!~     end subroutine blank_stack
!~   end subroutine stack_clear
  
!~   function stack_peek(this) result(item)
!~     class(stack), intent(in) :: this
!~     class(container), allocatable :: item
!~     item = this%head%get_contents()
!~   end function stack_peek
  
!~   function stack_concat(lhs, rhs)
!~     class(stack), intent(in) :: lhs, rhs
!~     class(ordered), allocatable :: stack_concat
!~     type(stack), allocatable :: tmp_concat
!~     type(stack) :: tmp_stack
!~     type(linked_node), pointer :: tail
!~     tmp_stack = lhs%copy()
!~     tmp_concat%head => tmp%move_head()
!~     if (tmp_concat%size() == 0) then
!~       tmp_stack = rhs%copy()
!~       tmp_concat%head => tmp_stack%move_head()
!~     else
!~       tail => tmp_concat%head
!~       do while(tail%has_next())
!~         tail => tail%get_next()
!~       end do
!~       tmp_stack = rhs%copy()
!~       call tail%set_next(tmp_stack%move_head())
!~     end if
!~     nullify(tail)
!~     call move_alloc(tmp_concat, stack_concat)
!~   end function stack_concat

!~   function stack_move_head(this) result(move_head)
!~     class(stack), intent(inout) :: this
!~     type(linked_node), pointer :: move_head
!~     move_head => this%head
!~     nullify(this%head)
!~   end function stack_move_head
  
!~   subroutine stack_final(this)
!~     type(stack), intent(inout) :: this
!~     nullify(this%iter_pos)
!~     call this%head%unset_next(.true.)
!~     deallocate(this%head)
!~   end subroutine stack_final

!~ end module stack_mod
