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
  use iterator_mod, only: iterator
  use countable_mod, only: countable
  use abstract_container_mod, only: container
  implicit none
  private

  type, extends(countable), abstract, public :: ordered
  contains
    procedure(push_sub), deferred :: push
    procedure(pop_func), deferred :: pop
    procedure(peek_func), deferred :: peek
    procedure(blank_sub), deferred :: clear
    procedure, private :: array_extend
    procedure, private :: iterator_extend
!~     procedure(concat_func), private, deferred :: concat
    generic :: extend => array_extend, iterator_extend
!~     generic :: operator(//) => concat
  end type ordered

  abstract interface
    subroutine push_sub(this, item)
      import ordered
      class(ordered), intent(inout) :: this
      class(*), intent(in) :: item
    end subroutine push_sub
    function pop_func(this)
      import ordered
      import container
      class(ordered), intent(inout) :: this
      class(container), allocatable :: pop_func
    end function pop_func
    function peek_func(this)
      import ordered
      import container
      class(ordered), intent(in) :: this
      class(container), allocatable :: peek_func
    end function peek_func
    subroutine blank_sub(this)
      import ordered
      class(ordered), intent(inout) :: this
    end subroutine blank_sub
    function concat_func(lhs, rhs)
      import ordered
      class(ordered), intent(in) :: lhs, rhs
      class(ordered), allocatable :: concat_func
    end function concat_func
  end interface

contains

  subroutine array_extend(this, items)
    class(ordered), intent(inout) :: this
    !FIXME: I've switched this from dimension(*) to dimension(:) because gfortran does not yet support dimension(*) for unlimited polymorphic variables. Add logic to allow dimension(*) for compilers supporting it, maybe.
    class(*), dimension(:), intent(in) :: items
    integer :: i
    do i = 1, size(items)
      call this%push(items(i))
    end do
  end subroutine array_extend
  
  subroutine iterator_extend(this, items)
    class(ordered), intent(inout) :: this
    class(iterator), intent(inout) :: items
    call items%reset()
    do while (items%has_next())
      call this%push(items%next())
    end do
    call items%reset()
  end subroutine iterator_extend

end module ordered_mod
