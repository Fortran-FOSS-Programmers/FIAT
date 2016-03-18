!
!  ordered.f90
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
  

module ordered_mod
  !* Author: Chris MacMackin
  !  Date: February 2016
  !  License: LGPLv3
  !
  ! Provides the [[ordered]] abstract type. This is a data structure for
  ! which the order of the elements is known. One of the effects of this
  ! is that order in which items are retrieved is determined by the 
  ! order in which they are added to the data structure.
  !
  use iterator_mod, only: iterator
  use iterable_mod, only: iterable
  use countable_mod, only: countable
  use abstract_container_mod, only: container
  implicit none
  private

  type, extends(countable), abstract, public :: ordered
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! An abstract type which is an ancestor for any data structure in
    ! which items are stored in a particular order. This means that 
    ! the order in which items are placed in the structure will 
    ! determine the order in which they are retrieved. Examples of data
    ! structures descending from this one are a [[stack]], [[queue]], or
    ! [[list]].
    !
  contains
    procedure(push_sub), deferred :: push
      !! Place a new item in the data structure
    procedure(pop_func), deferred :: pop
      !! Remove and return the next item from the data structure
    procedure(peek_func), deferred :: peek
      !! Return, but do not remove, the next item in the data structure
    procedure(blank_sub), deferred :: clear
      !! Remove all contents from the data structure
    procedure(logical_return), nopass, deferred :: is_fifo
      !! Indicates whether this is a first in first out or last in last
      !! out data type.
    procedure, private :: array_extend
      !! Add ([[ordered:push]]) the elements of an array to this data
      !! structure
    procedure, private :: iterator_extend
      !! Add ([[ordered::push]]) the contents of an [[iterator]] to 
      !! this data structure.
    generic :: extend => array_extend, iterator_extend
      !! Place multiple new items in the data structure
    procedure(concat_func), private, deferred :: concat
      !! Join this object with another [[ordered]] object, returning
      !! the result. The contents of the returned object are ordered
      !! such that applying [[ordered:pop]] until the structure is
      !! empty would provide items in the same order as calling 
      !! [[ordered:pop]] until the first object is empty and then until
      !! the second object is empty.
    generic :: operator(//) => concat
      !! Overloads the concatenation operator to join this object with
      !! another [[ordered]] object, returning the result. The contents
      !! of the returned object are ordered such that applying 
      !! [[ordered:pop]] until the structure is empty would provide 
      !! items in the same order as calling [[ordered:pop]] until the 
      !! first object is empty and then until the second object is 
      !! empty.
  end type ordered

  abstract interface
    pure subroutine push_sub(this, item)
      import :: ordered
      class(ordered), intent(inout) :: this
      class(*), intent(in) :: item
        !! Contents to be added to this data structure
    end subroutine push_sub
    
    function pop_func(this)
      import :: ordered
      import :: container
      class(ordered), intent(inout) :: this
      class(container), allocatable :: pop_func
        !! The next item in the data structure, which has been removed
    end function pop_func
    
    pure function peek_func(this)
      import :: ordered
      import :: container
      class(ordered), intent(in) :: this
      class(container), allocatable :: peek_func
        !! The next item in the data structure
    end function peek_func
    
    pure subroutine blank_sub(this)
      import ordered
      class(ordered), intent(inout) :: this
    end subroutine blank_sub
    
    pure function logical_return()
      import :: ordered
      logical :: logical_return
        !! True if first in first out structure, false if last in 
        !! first out
    end function logical_return
    
    pure function concat_func(lhs, rhs)
      import :: ordered
      class(ordered), intent(in) :: lhs !! This object
      class(ordered), intent(in) :: rhs !! The object being concatenated to this one
      class(ordered), allocatable :: concat_func
        !! The result of the concatenation
    end function concat_func
  end interface

contains

  subroutine array_extend(this, items)
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! Adds the elements of an array to this object.
    ! @Bug `gfortran` does not yet support dimension(*) for unlimited 
    ! polymorphic variables, so I have had to switch to using 
    ! `dimension(:)` when compiling with it. This means that only
    ! 1D arrays are accepted with `gfortran`.
    !
    class(ordered), intent(inout) :: this
#ifdef __GFORTRAN__
    class(*), dimension(:), intent(in) :: items
#else
    class(*), dimension(*), intent(in) :: items
#endif
      !! The items to be added to this data structure
    integer :: i
    do i = 1, size(items)
      call this%push(items(i))
    end do
  end subroutine array_extend
  
  subroutine iterator_extend(this, items)
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! Adds the contents of an [[iterable]] object to this data structure.
    !
    class(ordered), intent(inout) :: this
    class(iterable), intent(inout) :: items
      !! The iterable whose contents are to be added to this data
      !! structure.
    type(iterator) :: iter_items
    iter_items = items%iter()
    do while (iter_items%has_next())
      call this%push(iter_items%next())
    end do
  end subroutine iterator_extend

end module ordered_mod
