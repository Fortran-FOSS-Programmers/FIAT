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

module iterator_mod
  use abstract_container_mod, only: container
  implicit none
  private

  type, abstract, public :: iterator
  contains
    procedure(has_func), deferred :: has_next
    procedure(next_func), deferred :: next
    procedure(empty_sub), deferred :: reset
    procedure(copy_func), deferred :: copy
  end type iterator

  abstract interface
    elemental function has_func(this)
      import iterator
      class(iterator), intent(in) :: this
      logical :: has_func
    end function has_func
    function next_func(this)
      import iterator
      import container
      class(iterator), intent(inout) :: this
      class(container), allocatable :: next_func
    end function next_func
    subroutine empty_sub(this)
      import iterator
      class(iterator), intent(inout) :: this
    end subroutine empty_sub
    function copy_func(this)
      import iterator
      class(iterator), intent(in) :: this
      class(iterator), allocatable :: copy_func
    end function copy_func
  end interface
end module iterator_mod
