!
!  multiset.f90
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

module multiset_mod
  !* Author: Chris MacMackin
  !  Date: March 2016
  !  License: LGPLv3
  !
  ! Provides an abstract type for a multi-set data structure. These 
  ! behave like a normal set, except they keep count of the number of
  ! times which an item has been added.
  !
  use dynamic_set_mod, only: dynamic_set
  implicit none
  private
  
  type, public, extends(dynamic_set), abstract :: multiset
    !* Author: Chris MacMackin
    !  Date: March 2016
    !  License: LGPLv3
    !
    ! Provides an abstract type for a multi-set data structure. These 
    ! behave like a normal set, except they keep count of the number of
    ! times which an item has been added. When an item is removed, the
    ! count will be decremented by 1. The item will only become absent
    ! from the list ([[data_set:has]] returns `.false.`) when the count
    ! reaches zero.
    !
  contains
    procedure(get_func), deferred :: get
      !! Returns the number of times this item is present in the set.
  end type multiset

  abstract interface
    pure function get_func(this, item)
      import :: multiset
      class(multiset), intent(in) :: this
      class(*), intent(in) :: item
        !! The item whose presence in the set is being checked
      integer :: get_func
        !! The number of times this item appears in the set
    end function get_func
  end interface

end module multiset_mod
