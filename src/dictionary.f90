!
!  dictionary.f90
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

module dictionary_mod
  !* Author: Chris MacMackin
  !  Date: March 2016
  !  License: LGPLv3
  !
  ! Provides an abstract type for mapping data structures. These are
  ! data structures which consist of key-value pairs.
  !
  use abstract_container_mod, only: container
  use data_set_mod, only: data_set
  use array_list_mod, only: array_list
  implicit none
  private
  
  type, public, extends(data_set), abstract :: dictionary
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! An abstract type for mapping data structures. These are data 
    ! structures which consist of key-value pairs. Examples of such
    ! structures are 
    ! [dictionaries](https://docs.python.org/2/library/stdtypes.html#mapping-types-dict) 
    ! in Python or a
    ! [hash](https://en.wikibooks.org/wiki/Perl_Programming/Hash_Variables)
    ! variable in Perl.
    !
  contains
    procedure(keys_func), deferred :: keys
      !! Returns a list of the keys in this dictionary
    procedure(values_func), deferred :: values
      !! Returns a list of the values stored in this dictionary, in the
      !! same order as the corresponding keys are returned when
      !! [[dictionary:keys]] is called
    procedure(set_func), deferred :: set
      !! Sets the given key to the give value
    procedure(type_func), deferred :: key_type
      !! Returns a [[container]] of the dynamics type used to store keys
    procedure(type_func), deferred :: value_type
      !! Returns a [[container]] of dynamics the type used to store 
      !! values
  end type dictionary

  abstract interface
    pure function keys_func(this)
      import :: dictionary
      import :: array_list
      class(dictionary), intent(in) :: this
      type(array_list) :: keys_func
        !! A list containing all of the keys in this dictionary
    end function keys_func

    pure function values_func(this)
      import :: dictionary
      import :: array_list
      class(dictionary), intent(in) :: this
      type(array_list) :: values_func
        !! A list containing all of the values in this dictionary,
        !! stored in the same order as their corresponding key would
        !! be were [[dictionary:keys]] called
    end function values_func
    
    function set_func(this, key, val)
      import :: dictionary
      class(dictionary), intent(inout) :: this
      class(*), intent(in) :: key
        !! The key whose value is to be set
      class(*), intent(in) :: val
        !! The value to be assigned to the specified key
    end function set_func
    
    pure function type_func(this)
      import :: dictionary
      import :: container
      class(dictionary), intent(in) :: this
      class(container), allocatable :: type_func
        !! A container of the dynamic type used to store this 
        !! dictionary's keys or values.
    end function type_func
  end interface

end module dictionary_mod
