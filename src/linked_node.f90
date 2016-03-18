!
!  linked_node.f90
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

module linked_node_mod
  !* Author: Chris MacMackin
  !  Date: February 2016
  !  License: LGPLv3
  !
  ! Implements a node which contains a pointer to another (the next) 
  ! node, allowing a chain to be formed. This can be used to, for
  ! example, build linked lists.
  ! 
  ! It is not anticipated that the linked_node type, or any types 
  ! extending it, will be handled directly by end users of FIAT; they 
  ! are meant for internal use within this package.
  !
  use node_mod, only: node
  implicit none
  private
  
  type, extends(node), public :: linked_node
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! A node which, in addition to holding a value, points at another
    ! (the next) linked_node or descendent type. This type can be built
    ! up into a chain, allowing a linked list to be formed.
    ! 
    ! It is not anticipated that the linked_node type, or any types 
    ! extending it, will be handled directly by end users of FIAT; they 
    ! are meant for internal use within this package. As such, care must
    ! be taken when using certain methods (see below) to avoid memory
    ! leaks or segfaults.
    !
    private
    class(linked_node), pointer :: next => null() !! The next node in the chain.
  contains
    procedure :: has_next
      !! Checks whether this node points to another one 
    procedure :: get_next
      !! Returns the next node in the chain if it exists.
    procedure :: set_next
      !! Sets the next node in the chain.
    procedure :: unset_next
      !! Sets this node not to point at any others, severing the chain.
  end type linked_node

contains

  elemental logical function has_next(this)
    !* Author: Chris MacMackin
    !  Date: February 2016
    !  
    ! Returns whether or not this node points at another one, forming
    ! a chain.
    !
    class(linked_node), intent(in) :: this
    has_next = associated(this%next)
  end function has_next
  
  function get_next(this)
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! Returns a pointer to the node which this ones points to, i.e. the
    ! next node in the chain. If this node does not point at another 
    ! one, then a null pointer is returned.
    !
    class(linked_node), intent(in) :: this
    class(linked_node), pointer :: get_next
    if (this%has_next()) then
      get_next => this%next
    else
      get_next => null()
    end if
  end function get_next
  
  subroutine set_next(this, new_next, deallocate_old)
    !* Author: Chris MacMackin
    !  Date: February 2016
    !
    ! Sets the node which this one points to (i.e. sets the next node in
    ! the chain). If this node already points to another one, the 
    ! pointer will, by default, be nullified. This may result in a
    ! memory leak. Optionally, by setting `deallocate_old=.true.`, the
    ! next node (and all nodes it points to) can be deallocated. This
    ! may result in a segfault if another part of the program tries to
    ! access the former next node.
    !
    class(linked_node), intent(inout) :: this
    class(linked_node), pointer, intent(in) :: new_next
      !! The node which will now be next in the chain.
    logical, optional, intent(in) :: deallocate_old
      !! Whether to deallocate (rather than just nullify) any existing
      !! subsequent nodes in the chain. Defaults to `.false.`.
    call this%unset_next(deallocate_old)
    this%next => new_next
  end subroutine set_next
  
  subroutine unset_next(this, deallocate_old)
    !* Author: Chris MacMackin
    !  Date: February 2016
    ! 
    ! Unsets the pointer to the next node in the chain, severing it.
    ! By default, the pointer is only nullified. This may result in a
    ! memory leak. Optionally, by setting `deallocate_old=.true.`, the
    ! next node (and all nodes it points to) can be deallocated. This
    ! may result in a segfault if another part of the program tries to
    ! access the former next node.
    !
    class(linked_node), intent(inout) :: this
    logical, optional, intent(in) :: deallocate_old
      !! Whether to deallocate (rather than just nullify) any existing
      !! subsequent nodes in the chain. Defaults to `.false.`.
    if (.not. this%has_next()) return
    if (present(deallocate_old)) then
      if (deallocate_old) then
        call this%next%unset_next(.true.)
        deallocate(this%next)
        return
      end if
    end if
    nullify(this%next)
  end subroutine unset_next

end module linked_node_mod
