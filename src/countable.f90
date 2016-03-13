!
!  countable.f90
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

module countable_mod
  !* Author: Chris MacMackin
  !  Date: February 2016
  !  License: LGPLv3
  !
  ! Provides the [[countable]] abstract type. For a type to be
  ! countable, the number of individual pieces of data stored within
  ! must be known in advance.
  !
  use iterable_mod, only: iterable
  implicit none
  private

  type, extends(iterable), abstract, public :: countable
  !* Author: Chris MacMackin
  !  Date: February 2016
  !  License: LGPLv3
  !
  ! An abstract data type which can be iterated, for which the number of
  ! pieces of items of data stored within is known.
  !
  contains
    procedure(size_func), deferred :: size
      !! Return the number of items stored within this object
  end type countable

  abstract interface
    pure function size_func(this)
      import :: countable
      class(countable), intent(in) :: this
      integer :: size_func
        !! The number of items stored in this object.
    end function size_func
  end interface
  
end module countable_mod
