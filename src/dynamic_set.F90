!
!  dynamic_set.f90
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

module dynamic_set_mod
  !* Author: Chris MacMackin
  !  Date: March 2016
  !  License: LGPLv3
  !
  ! Provides an abstract type for a dynamic set data structures. This
  ! data structure is similar to a normal [[data_set]] structure, but
  ! new items can be added to the set. It is similar to the 
  ! [set](https://docs.python.org/2/library/stdtypes.html#set) type in
  ! Python.
  !
  use abstract_container_mod, only: container
  use iterable_mod, only: iterable
  use data_set_mod, only: data_set

  type, public, abstract, extends(data_set) :: dynamic_set
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! An abstract data type for dynamic sets. These are much like
    ! mathematical sets, but differ from the parent [[data_set]] type
    ! in that items can be added to or removed from the set. To
    ! accomplish this, various additional methods are available. This
    ! data type is similar to the 
    ! [set](https://docs.python.org/2/library/stdtypes.html#set) type in
    ! Python.
    !
  contains
    procedure(single_sub), deferred, private :: add_single
      !! Places the item in the set, if not already present
    procedure(multiple_sub), deferred, private :: add_multiple
      !! Places each item in the array into the set, if not already 
      !! present
    generic :: add => add_single, add_multiple
    procedure(iter_sub), deferred :: add_iter
      !! Places each item in the iterable into the set, if not already
      !! present.
    procedure(single_sub), deferred, private :: remove_single
      !! Removes the item from the set, if present
    procedure(multiple_sub), deferred, private :: remove_multiple
      !! Removes each item in the array from the set, if present
    generic :: remove => remove_single, remove_multiple
    procedure(iter_sub), deferred :: remove_iter
      !! Removes each item in the iterable from the set, if present
    procedure(pop_func), deferred :: pop
      !! Removes a random item from the set and returns it
    procedure(clear_sub), deferred :: clear
      !! Removes all items from the set
  end type dynamic_set

  abstract interface
    pure subroutine single_sub(this, item)
      import :: dynamic_set
      class(dynamic_set), intent(inout) :: this
      class(*), intent(in) :: item
        !! An item to add or remove from the set
    end subroutine single_sub
    
    pure subroutine multiple_sub(this, items)
      import :: dynamic_set
      class(dynamic_set), intent(inout) :: this
#ifdef __GFORTRAN__
      class(*), dimension(:), intent(in) :: items
#else
      class(*), dimension(*), intent(in) :: items
#endif
        !! An array containing items to add or remove from the set
    end subroutine multiple_sub
    
    pure subroutine iter_sub(this, items)
      import :: dynamic_set
      import :: iterable
      class(dynamic_set), intent(inout) :: this
      class(iterable), intent(in) :: items
        !! An iterable containing items to add or remove from the set
    end subroutine iter_sub
    
    function pop_func(this)
      import :: dynamic_set
      import :: container
      class(dynamic_set), intent(inout) :: this
      class(container), allocatable :: pop_func
        !! A random item which has been removed from the set
    end function pop_func
    
    pure subroutine clear_sub(this)
      import :: dynamic_set
      class(dynamic_set), intent(inout) :: this
    end subroutine clear_sub
  end interface

end module dynamic_set_mod



