!
!  ordered.f90
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
  

module ordered_mod
  use iterable_mod only: iterable_type
  use countable_mod only: countable_type
  use abstract_container_mod only: container_type
  implicit none
  private

  type, extends(countable_type), abstract, public :: ordered_type
  contains
    procedure(push_sub), deferred :: push
    procedure(cont_func), deferred :: pop
    procedure(cont_func), deferred :: peek
    procedure(blank_sub), deferred :: clear
    procedure, private :: array_extend
    procedure, private :: iterable_extend
    procedure(concat_func), private, deferred :: concat
    generic :: extend => array_extend, iterable_extend
    generic :: operator(//) => concat
  end type ordered_type

  abstract interface
    subroutine push_sub(this, item)
      class(ordered_type), intent(inout) :: this
      class(*), intent(in) :: item
    end subroutine push_sub
    function cont_func(this)
      class(ordered_type), intent(inout) :: this
      type(container_type), allocatable :: cont_func
    end function cont_func
    subroutine blank_sub(this)
      class(ordered_type), intent(inout) :: this
    end subroutine blank_sub
    function concat_func(lhs, rhs)
      class(ordered_type), intent(in) :: lhs, rhs
      class(ordered_type) :: concat_func
    end function concat_func
  end interface

contains

  subroutine array_extend(this, items)
    class(iterable), intent(inout) :: this
    class(*), dimension(*), intent(in) :: items
    integer :: i
    do i = 1, size(items)
      this%push(items(i))
    end do
  end subroutine stack_array_extend
  
  subroutine iterable_extend(this, items)
    class(iterable), intent(inout) :: this
    class(iterable), intent(inout) :: this
    type(linked_node) :: tail
    call items%reset()
    do while (items%has_next())
      this%push(items%next())
    end do
    call items%reset()
  end subroutine stack_iterable_extend

end module ordered_mod
