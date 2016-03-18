!
!  map.f90
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

module map_mod
  !* Author: Chris MacMackin
  !  Date: March 2016
  !  License: LGPLv3
  !
  ! Provides an abstract type for a standard map data structures. These 
  ! are data structures which consist of key-value pairs, with only one
  ! value per key. It is similar to the 
  ! [dictionary](https://docs.python.org/2/library/stdtypes.html#mapping-types-dict) 
  ! type in Python.
  !
  use abstract_container_mod, only: container
  use dictionary_mod, only: dictionary
  implicit none
  private

  type, public, extends(dictionary), abstract :: map
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! An abstract type for the standard map data structures. These are 
    ! data structures which consist of key-value pairs, with only one 
    ! value per key. It has similar functionality to the 
    ! [dictionary](https://docs.python.org/2/library/stdtypes.html#mapping-types-dict) 
    ! type in Python.
    !
  contains
    procedure(get_func), deferred :: get
      !! Returns the value associated with the specified key.
    procedure(update_sub), deferred :: update
      !! Adds any key-value pairs in the second map not already present.
      !! Updates the value for all keys in this dictionary also present 
      !! in the second dictionary.
  end type map

  abstract interface
    pure function get_func(this, key)
      import :: map
      import :: container
      class(map), intent(in) :: this
      class(*), intent(in) :: key
        !! The key whose associated value is to be returned
      class(container), allocatable :: get_func
        !! The value associated with the specified key
    end function get_func
    
    pure subroutine update_sub(this, other)
      import :: map
      class(map), intent(inout) :: this
      class(map), intent(in) :: other
        !! A map whose key-value pairs will be added to this one,
        !! overwriting as necessary.
    end subroutine update_sub
  end interface

end module map_mod
