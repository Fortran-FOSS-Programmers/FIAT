!
!  queue.f90
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

module queue_mod
  !* Author: Chris MacMackin
  !  Date: February 2016
  !  License: LGPLv3
  !
  ! Provides an abstract data type representing a queue (first in first 
  ! out) data structure.
  !
  use ordered_mod, only: ordered
  implicit none
  private
  
  type, public, extends(ordered), abstract :: queue
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! An abstract data type representing the queue structure. This is 
    ! largely a placeholder type, in case some methods specific to queues
    ! are added at a later time. However, it does implement the 
    ! [[ordered:is_fifo]] method.
    !
    private
  contains
    procedure, nopass :: is_fifo => queue_is_fifo
      !! Returns true, as queues are a first in first out data type.
  end type queue

contains

  pure logical function queue_is_fifo()
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! Returns `.true.`, indicating that queues are a "first in first 
    ! out" data structure.
    !
    queue_is_fifo = .true.
  end function queue_is_fifo

end module queue_mod
