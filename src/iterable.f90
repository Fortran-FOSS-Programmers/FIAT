!
!  iteratable.f90
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

module iterable_mod
  !* Author: Chris MacMackin
  !  Date: February 2016
  !  License: LGPLv3
  !
  ! Provides the [[iterable]] abstract type. All of FIATs more complex
  ! public data structures are descendants of this type.
  !
  use abstract_container_mod, only: container
  use iterator_mod, only: iterator
  implicit none
  private
  
  type, abstract, public :: iterable
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! An abstract type which can return and [[iterator]] object
    ! representing its contents. All of the more complex public data 
    ! structures in FIAT are descendants of this one.
    !
  contains
    procedure(iterator_return), deferred :: iter
      !! Return an [[iterator]] object with the contents of the iterable 
    procedure(container_return), deferred :: contents_type
      !! Return a [[container]] object with the dynamic type of that
      !! used in this iterable
  end type iterable
  
  abstract interface
    pure function iterator_return(this)
      import :: iterable
      import :: iterator
      class(iterable), intent(in) :: this
      type(iterator) :: iterator_return
        !! An [[iterator]] with the contents of this object
    end function iterator_return
    
    pure function container_return(this)
      import :: iterable
      import :: container
      class(iterable), intent(in) :: this
      class(container), allocatable :: container_return
        !! A container of the dynamic type used in this object
    end function container_return
  end interface

end module iterable_mod
