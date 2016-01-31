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
  use abstract_container_mode only: container_type
  implicit none
  private
  
  type, public :: node_type
    private
    class(container_type), allocatable :: contents
  contains
    procedure, non_overridable :: has_contents
    procedure, non_overridable :: get_contents
    procedure, non_overridable :: set_contents
    procedure, non_overridable :: unset_contents
  end type node_type

contains

  elemental logical function has_contents(this)
    class(node_type), intent(in) :: this
    has_contents = allocated(this%contents)
  end function has_contents
  
  elemental function get_contents(this)
    class(node_type), intent(in) :: this
    class(container_type), allocatable :: get_contents
    if (this%has_contents()) allocate(get_contents, source=this%contents)
  end function get_contents
  
  subroutine set_contents(this, contents)
    class(node_type), intent(inout) :: this
    class(container_type), intent(in) :: contents
    call this%unset_contents()
    move_alloc(contents, this%contents)
  end subroutine set_contents
  
  subroutine unset_contents(this)
    class(node_type), intent(inout) :: this
    if (this%has_contents()) deallocate(this%contents)
  end subroutine unset_contents

end module node_mod
