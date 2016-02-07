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
  use abstract_container_mod, only: container
  implicit none
  private
  
  type, public :: node
    private
    class(container), allocatable :: contents
  contains
    procedure, non_overridable :: has_contents
    procedure, non_overridable :: get_contents
    procedure, non_overridable :: set_contents
    procedure, non_overridable :: unset_contents
  end type node

contains

  elemental logical function has_contents(this)
    class(node), intent(in) :: this
    has_contents = allocated(this%contents)
  end function has_contents
  
  pure function get_contents(this)
    class(node), intent(in) :: this
    class(container), allocatable :: get_contents
    if (this%has_contents()) allocate(get_contents, source=this%contents)
  end function get_contents
  
  subroutine set_contents(this, contents)
    class(node), intent(inout) :: this
    class(container), intent(inout), allocatable :: contents
    call this%unset_contents()
    call move_alloc(contents, this%contents)
  end subroutine set_contents
  
  subroutine unset_contents(this)
    class(node), intent(inout) :: this
    if (this%has_contents()) deallocate(this%contents)
  end subroutine unset_contents

end module node_mod
