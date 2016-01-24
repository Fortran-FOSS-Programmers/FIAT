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
  use abstract_container_mod only: container_type
  implicit none
  private

  type, abstract, public :: iterator_type
  contains
    procedure(has_func), deferred :: has_next
    procedure(next_func), deferred :: next
    procedure(empty_sub), deferred :: reset
  end type iterator_type

  abstract interface
    function has_func()
      logical :: has_func
    end function has_func
    function next_func
      type(container_type), allocatable :: next_func
    end function has_func
    subroutine empty_sub()
    end subroutine empty_sub
  end interface
end module iterator_mod
