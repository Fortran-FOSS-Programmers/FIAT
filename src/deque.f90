!
!  dequeue.f90
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

module deque_mod
  !* Author: Chris MacMackin
  !  Date: March 2016
  !  License: LGPLv3
  !
  ! Provides a doubled ended queue abstract data type. This is a first
  ! in first out data structure which can be added to or taken from at
  ! either end.
  !
  use queue_mod, only: queue
  use abstract_container_mod, only: container
  implicit none
  private
  
  type, abstract, extends(queue), public :: deque
  !* Author: Chris MacMackin
  !  Date: March 2016
  !
  ! An abstract data type representing the double ended queue data
  ! structure. Rather than just pushing items to one end (the "left 
  ! end") and popping them from the other (the "right end"), items can
  ! be pushed or popped to/from either the right or the left.
  !
  contains
    procedure(push_sub), deferred :: pushleft
      !! Add an item to the left end of the data structure (equivalent
      !! to [[ordered:push]] on a queue)
    procedure(push_sub), deferred :: pushright
      !! Add an item to the right end of the data structure
    procedure(pop_func), deferred :: popleft
      !! Remove and return the item from the left end of the data
      !! structure
    procedure(pop_func), deferred :: popright
      !! Remove and return the item from the right end of the data
      !! structure (equivalent to [[ordered:pop]] on a queue)
    procedure(peek_func), deferred :: peekleft
      !! Return, but do not remove, the item at the left end of the data
      !! structure
    procedure(peek_func), deferred :: peekright
      !! Return, but do not remove, the item at the left end of the data
      !! structure (equivalent to [[ordered:peek]] on a queue)
  end type deque

  abstract interface
    pure subroutine push_sub(this, item)
      import :: deque
      class(deque), intent(inout) :: this
      class(*), intent(in) :: item !! The value to be added to the list
    end subroutine push_sub
    
    function pop_func(this)
      import :: deque
      import :: container
      class(deque), intent(inout) :: this
      class(container), allocatable :: pop_func
        !! The next value, which has just been removed
    end function pop_func
    
    pure function peek_func(this)
      import :: deque
      import :: container
      class(deque), intent(in) :: this
      class(container), allocatable :: peek_func
        !! The next value, which is not removed
    end function peek_func
  end interface

end module deque_mod
