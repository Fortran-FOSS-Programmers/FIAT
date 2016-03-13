!
!  multimap.f90
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

module multimap_mod
  !* Author: Chris MacMackin
  !  Date: March 2016
  !  License: LGPLv3
  !
  ! Provides an abstract type for a multiple map data structure. These 
  ! are data structures which consist of key-value pairs, potentially
  ! holding multiple values per key.
  !
  use dictionary_mod, only: dictionary
  use array_list_mod, only: array_list
  use map_mod, only: map
  implicit none
  private

  type, public, extends(dictionary), abstract :: multimap
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! An abstract type for the standard map data structures. These are 
    ! data structures which consist of key-value pairs, potentially 
    ! holding multiple values per key.
    !
  contains
    procedure(get_func), deferred :: get
      !! Returns a list of values associated with the specified key.
    procedure(get_count_func), deferred :: get_count
      !! Returns the number of values associated with the specified key.
    procedure(update_map_sub), deferred, private :: update_map
      !! Adds any key-value pairs in the standard map to this one. If a
      !! key does not already exist in this dictionary then it is 
      !! created and assigned the corresponding value. Otherwise, the
      !! value in the other dictionary is just added to the values 
      !! associated with the key in this one.
    procedure(update_multimap_sub), deferred, private :: update_multimap
      !! Adds any key-value pairs in the second multipmap to this one.
      !! If a key does not already exist in this dictionary then it is
      !! created and assigned the corresponding values. Otherwise, the
      !! values in the other dictionary add just added to the values 
      !! associated with the key in this one.
    generic :: update => update_map, update_multimap
      !! Update this multimap's contents with those of another 
      !! (multi)map
    procedure(remove_val_sub), deferred :: remove_value
      !! Remove the specified value from the specified key
  end type multimap

  abstract interface
    pure function get_func(this, key)
      import :: multimap
      import :: array_list
      class(multimap), intent(in) :: this
      class(*), intent(in) :: key
        !! The key whose associated values are to be returned
      type(array_list) :: get_func
        !! The values associated with the specified key
    end function get_func

    pure function get_count_func(this, key)
      import :: multimap
      class(multimap), intent(in) :: this
      class(*), intent(in) :: key
        !! The key whose associated values are to be counted
      integer :: get_func
        !! The number of values associated with the specified key
    end function get_count_func
    
    pure subroutine update_multimap_sub(this, other)
      import :: multimap
      class(multimap), intent(inout) :: this
      class(multimap), intent(in) :: other
        !! A multimap whose key-value pairs will be added to this one,
        !! adding values to any keys which already exist in this one.
    end subroutine update_multimap_sub
    
    pure subroutine update_map_sub(this, other)
      import :: multimap
      import :: map
      class(multimap), intent(inout) :: this
      class(map), intent(in) :: other
        !! A map whose key-value pairs will be added to this one,
        !! appending the value to any keys which already exist in this
        !! one.
    end subroutine update_map_sub
    
    pure subroutine remove_val_sub(this, key, val)
      import :: multimap
      class(multimap), intent(inout) :: this
      class(*), intent(in) :: key
        !! A key from which to remove a value
      class(*), intent(in) :: val
        !! A value which will be removed from `key`, if `key` has such
        !! a value.
    end subroutine remove_val_sub 
  end interface

end module multimap_mod
