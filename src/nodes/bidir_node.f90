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
  use linked_node_mod only: linked_node
  implicit none
  private
  
  type, extends(linked_node), public :: bidir_node
    private
    class(bidir_node), pointer :: prev => null()
  contains
    procedure, non_overridable :: has_prev
    procedure, non_overridable :: get_prev
    procedure, non_overridable :: set_prev
    procedure, non_overridable :: unset_prev
  end type bidir_node

contains

  elemental logical function has_prev(this)
    class(bidir_node), intent(in) :: this
    has_prev = associated(this%prev)
  end function has_prev
  
  elemental function get_prev(this)
    class(bidir_node), intent(in) :: this
    class(bidir_node), pointer :: get_prev
    if (this%has_prev()) then
      get_prev => this%prev
    else
      get_prev => null()
    end if
  end function get_prev
  
  subroutine set_prev(this, new_prev, nullify_old)
    class(bidir_node), intent(inout) :: this
    class(bidir_node), pointer, intent(in) :: new_prev
    logical, optional, intent(in) :: nullify_old
    call this%unset_prev(nullify_old)
    this%prev => new_prev
  end subroutine set_prev
  
  subroutine unset_prev(this, nullify_old)
    class(bidir_node), intent(inout) :: this
    logical, optional, intent(in) :: nullify_old
    if (.not. this%has_prev()) return
    if (present(nullify_old)) then
      if (nullify_old) this%prev%unset_prev(.true.)
    end if
    nullify(this%prev)
  end subroutine unset_prev

end module bidir_node_mod
