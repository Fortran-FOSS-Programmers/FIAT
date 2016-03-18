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
    private
    class(container), allocatable, dimension(:) :: contents
    integer :: location = 1
    logical :: filled = .false.
  contains
    procedure :: has_next
    procedure :: next
    procedure :: reset
    procedure :: contents_type
  end type iterator

  interface iterator
    module procedure constructor
  end interface

contains
  
  pure function constructor(contents) result(new)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Creates an iterator from an array of containers containing
    ! the data to be returned upon iteration. The data is returned
    ! starting with the first element of the array and ending with
    ! the last.
    !
    class(container), dimension(:), intent(in) :: contents
    type(iterator) :: new
    allocate(new%contents(size(contents)), source=contents)
    new%filled = .true.
  end function constructor

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
    has_next = this%location <= size(this%contents)
  end function has_next

  function next(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns the next item stored in the iterator. If there are no
    ! more items present then an empty [[container]] is returned. If
    ! there are no contents stored in this iterator then it returns
    ! an unallocated [[container]].
    !
    class(iterator), intent(inout) :: this
    class(container), allocatable :: next
      !! The next item held in the iterator, if present. Otherwise
      !! an unallocated container.
    if (.not. this%filled) return
    if (this%location > size(this%contents)) then
      allocate(next, mold=this%contents(1))
      return
    end if
    allocate(next,source=this%contents(this%location))
    this%location = this%location + 1
  end function next

  subroutine reset(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Resets the position of the iterator to the start, so it is as
    ! though the [[iterator(type):next(bound)]] routine has never been
    ! called.
    !
    class(iterator), intent(inout) :: this
    this%location = 1
  end subroutine reset
  
  pure function contents_type(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns a container with the dynamic type of that used to hold
    ! the contents of this iterator
    !
    class(iterator), intent(in) :: this
    class(container), pointer :: contents_type
      !! A container with the dynamic type of that used to hold the
      !! contents of the iterator. It is a pointer as pointer assignment
      !! is the easiest way to hold its "value" in an abstract variable.
    if (.not. this%filled) return
    allocate(contents_type,mold=this%contents(1))
  end function contents_type
  

end module iterator_mod
