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
  use node_mod only: node_type
  implicit none
  private
  
  type, extends(node_type), public :: linked_node
    private
    class(linked_node), pointer :: next => null()
  contains
    procedure, non_overridable :: has_next
    procedure, non_overridable :: get_next
    procedure, non_overridable :: set_next
    procedure, non_overridable :: unset_next
  end type linked_node

contains

  elemental logical function has_next(this)
    class(linked_node), intent(in) :: this
    has_next = associated(this%next)
  end function has_next
  
  elemental function get_next(this)
    class(linked_node), intent(in) :: this
    class(linked_node), pointer :: get_next
    if (this%has_next()) then
      get_next => this%next
    else
      get_next => null()
    end if
  end function get_next
  
  subroutine set_next(this, new_next, nullify_old)
    class(linked_node), intent(inout) :: this
    class(linked_node), pointer, intent(in) :: new_next
    logical, optional, intent(in) :: nullify_old
    call this%unset_next(nullify_old)
    this%next => new_next
  end subroutine set_next
  
  subroutine unset_next(this, nullify_old)
    class(linked_node), intent(inout) :: this
    logical, optional, intent(in) :: nullify_old
    if (.not. this%has_next()) return
    if (present(nullify_old)) then
      if (nullify_old) this%next%unset_next(.true.)
    end if
    nullify(this%next)
  end subroutine unset_next

end module linked_node_mod
