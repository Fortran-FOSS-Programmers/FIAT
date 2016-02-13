!
!  node.f90
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

module node_mod
  !* Author: Chris MacMackin
  !  Date: February 2016
  !  License: LGPLv3
  ! 
  ! Provides a [[node]] data type, for holding some sort of contents.
  ! While a node itself is unlikely to be very useful, various type
  ! extensions are also made which are then used to build more complex
  ! data structures.
  !
  ! It is not anticipated that the node type, or any types extending it,
  ! will be handled directly by end users of FIAT; they are meant for
  ! internal use within this package.
  !
  use abstract_container_mod, only: container
  implicit none
  private
  
  type, public :: node
    !* Author: Chris MacMackin
    !  Date: February 2016
    ! 
    ! An object which contains a container that can be set to have
    ! arbitrary contents. While of limited use on its own, various
    ! derived types exist which are extensions of this one exist and
    ! are used to build more complex data structures.
    !
    ! It is not anticipated that the node type, or any types extending it,
    ! will be handled directly by end users of FIAT; they are meant for
    ! internal use within this package.
    !
    private
    class(container), allocatable :: contents 
      !! Contains the value held in this node.
  contains
    procedure, non_overridable :: has_contents 
      !! Evaluates whether contents have been assigned to node.
    procedure, non_overridable :: get_contents
      !! Returns the value stored in the node.
    procedure, non_overridable :: set_contents
      !! Sets the value to be stored in the node.
    procedure, non_overridable :: unset_contents
      !! Removes the record of any value stored in the node.
  end type node

contains

  elemental logical function has_contents(this)
    !* Author: Chris MacMackin
    !  Date: February 2016
    ! 
    ! Returns whether or not a values has been assigned to the node.
    !
    class(node), intent(in) :: this
    has_contents = allocated(this%contents)
  end function has_contents
  
  pure function get_contents(this)
    !* Author: Chris MacMackin
    !  Date: February 2016
    ! 
    ! An accessor returning a [[container]] object storing the value
    ! placed in the node. If the contents have not been set then an
    ! unallocated container is returned.
    !
    class(node), intent(in) :: this
    class(container), allocatable :: get_contents !! The stored in the node.
    if (this%has_contents()) allocate(get_contents, source=this%contents)
  end function get_contents
  
  subroutine set_contents(this, contents)
    !* Author: Chris MacMackin
    !  Date: February 2016
    ! 
    ! Places a new value into storage within the node. This value must
    ! already be heald within an allocatable [[container]]. The actual
    ! argument will be deallocated after the subroutine call, as its
    ! allocation is moved to the contents of the node.
    !
    class(node), intent(inout) :: this
    class(container), intent(inout), allocatable :: contents
      !* The new value to be stored in this node. The actual argument
      ! will be deallocated during the process of assigning it to the
      ! node.
    call this%unset_contents()
    call move_alloc(contents, this%contents)
  end subroutine set_contents
  
  subroutine unset_contents(this)
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! Deallocates the value stored within the  node.
    !
    class(node), intent(inout) :: this
    if (this%has_contents()) deallocate(this%contents)
  end subroutine unset_contents

end module node_mod
