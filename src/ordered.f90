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
    procedure(array_sub), private, deferred :: array_extend
    procedure(iter_sub), private, deferred :: iterable_extend
    procedure(concat_func), private, deferred :: concat
    procedure, private :: assign
    generic :: extend => array_extend, iterable_extend
    generic :: operator(//) => concat
    generic :: assignment(=) => assign
  end type ordered_type

  abstract interface
    subroutine push_sub(content)
      class(*), intent(in) :: content
    end subroutine push_sub
    function cont_func()
      type(container_type), allocatable :: cont_func
    end function cont_func
    subroutine blank_sub()
    end subroutine blank_sub
    subroutine array_sub(contents)
      class(*), dimension(:), intent(in) :: contents
    end subroutine array_sub
    subroutine iter_sub(contents)
      class(iterable_type), intent(in) :: contents
    end subroutine iter_sub
    function concat_func(lhs, rhs)
      class(ordered_type), intent(in) :: lhs, rhs
      class(ordered_type), allocatable :: concat_func
    end function concat_func
  end interface

contains

  subroutine assign(lhs, rhs)
    class(ordered_type), intent(out) :: lhs
    class(ordered_type), intent(in) :: rhs
    call rhs%reset()
    do while(rhs%has_next)
      rhs%push(rhs%next)
    end do
    call rhs%reset()
  end subroutine assign

end module ordered_mod
