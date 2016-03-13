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
  !* Author: Chris MacMackin
  !  Date: March 2016
  !  License: LGPLv3
  !
  ! Provides the [[iterator]] data type which can be used to access a
  ! collection of data.
  !
  use abstract_container_mod, only: container
  implicit none
  private

  type, public :: iterator
  !* Author: Chris MacMackin
  !  Date: March 2016
  !
  ! A data type which provides a collection of data to the user. Objects
  ! of this type are returned using the [[iterable:iter]] method of
  ! FIAT's other data types. The contents of the iterator are set to be
  ! the same as the iterable at the time when the `iter` method was 
  ! called. If new items are later added to the iterable object, this
  ! will not be reflected in the iterator object.
  !
  !##Example
  ! If `list_obj` is some sort of [[list]] which contains character
  ! strings, then the following would print all strings held in the
  ! list.
  !```fortran
  !iterator_obj = list_obj%iter()
  !do while(iterator_obj%has_next())
  !    string = iterator_obj%next()
  !    write(*,*) string
  !end do
  !```
  !
  contains
    procedure :: has_next
    procedure :: next
    procedure :: reset
    procedure :: contents_type
  end type iterator

contains

  elemental function has_next(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns `.true.` if there are any remaining objects through which
    ! to iterate, and `.false.` otherwise.
    !
    class(iterator), intent(in) :: this
    logical :: has_next
      !! Whether there are additional items to iterate through
  end function has_next

  function next(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns the next item stored in the iterator. If there are no
    ! more items present then an unallocated [[container]] is returned.
    !
    class(iterator), intent(inout) :: this
    class(container), allocatable :: next
      !! The next item held in the iterator, if present. Otherwise
      !! an unallocated container.
  end function next

  subroutine reset(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Resets the position of the iterator to the start, so it is as
    ! though the [[iterator:next]] routine has never been called.
    !
    class(iterator), intent(inout) :: this
  end subroutine reset
  
  pure function contents_type(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns a container with the dynamic type of that used to hold
    ! the contents of this iterator
    !
    class(iterator), intent(in) :: this
    class(container), allocatable :: contents_type
      !! A container with the dynamic type of that used to hold the
      !! contents of the iterator.
  end function contents_type
  

end module iterator_mod
