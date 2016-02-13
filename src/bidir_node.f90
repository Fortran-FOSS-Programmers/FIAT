!
!  bidir_node.f90
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

module bidir_node_mod
  !* Author: Chris MacMackin
  !  Date: February 2016
  !  License: LGPLv3
  !
  ! Implements a node which contains a pointer to two other (the next 
  ! and previous) nodes, allowing a chain to be formed. This can be used
  ! to, for example, build a doubly-linked lists.
  ! 
  ! It is not anticipated that the bidir_node type, or any types 
  ! extending it, will be handled directly by end users of FIAT; they 
  ! are meant for internal use within this package.
  !
  use linked_node_mod, only: linked_node
  implicit none
  private
  
  type, extends(linked_node), public :: bidir_node
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! A node which, in addition to holding a value, points at two other
    ! (the previous and next) bidir_node objects or objects of a
    ! descendent type. This type can be built up into a chain, allowing
    ! a doubly-linked list to be formed.
    ! 
    ! It is not anticipated that the bidir_node type, or any types 
    ! extending it, will be handled directly by end users of FIAT; they 
    ! are meant for internal use within this package. As such, care must
    ! be taken when using certain methods (see below) to avoid memory
    ! leaks or segfaults.
    !
    private
    class(bidir_node), pointer :: prev => null()
  contains
    procedure :: has_prev
    procedure :: get_prev
    procedure :: set_prev
    procedure :: unset_prev
  end type bidir_node

contains

  elemental logical function has_prev(this)
    !* Author: Chris MacMackin
    !  Date: February 2016
    !  
    ! Returns whether or not this node points to a previous one, forming
    ! a chain in the backwards direction.
    !
    class(bidir_node), intent(in) :: this
    has_prev = associated(this%prev)
  end function has_prev
  
  function get_prev(this)
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! Returns a pointer to the previous node in the chain. If this node
    ! does not point at a previous one one, then a null pointer is
    ! returned.
    !
    class(bidir_node), intent(in) :: this
    class(bidir_node), pointer :: get_prev
    if (this%has_prev()) then
      get_prev => this%prev
    else
      get_prev => null()
    end if
  end function get_prev
  
  subroutine set_prev(this, new_prev, deallocate_old)
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! Sets the pointer to the previous node in the chain. If this node
    ! already points to a previous one, the pointer will, by default, be
    ! nullified. This may result in a memory leak. Optionally, by
    ! setting `deallocate_old=.true.`, the previous node (and all nodes
    ! it points to) can be deallocated. This may result in a segfault if
    ! another part of the program tries to access the former previous
    ! node. The new previous node will not automatically be set to have
    ! this one as the next, with the same rules applied to deallocation.
    !
    class(bidir_node), intent(inout) :: this
    class(bidir_node), pointer, intent(in) :: new_prev
      !! The node which will now be previous in the chain.
    logical, optional, intent(in) :: deallocate_old
      !! Whether to deallocate (rather than just nullify) any existing
      !! previous nodes in the chain. Defaults to `.false.`.
    call this%unset_prev(deallocate_old)
    this%prev => new_prev
  end subroutine set_prev
  
  subroutine unset_prev(this, deallocate_old)
    !* Author: Chris MacMackin
    !  Date: February 2016
    ! 
    ! Unsets the pointer to the previous node in the chain, severing it.
    ! By default, the pointer is only nullified. This may result in a
    ! memory leak. Optionally, by setting `deallocate_old=.true.`, the
    ! previous node (and all previous nodes it points to) can be
    ! deallocated. This may result in a segfault if another part of the
    ! program tries to access the former previous node.
    !
    class(bidir_node), intent(inout) :: this
    logical, optional, intent(in) :: deallocate_old
      !! Whether to deallocate (rather than just nullify) any existing
      !! p nodes in the chain. Defaults to `.false.`.
    if (.not. this%has_prev()) return
    if (present(deallocate_old)) then
      if (deallocate_old) then
        call this%prev%unset_prev(.true.)
        deallocate(this%prev)
        return
      end if
    end if
    nullify(this%prev)
  end subroutine unset_prev

end module bidir_node_mod
