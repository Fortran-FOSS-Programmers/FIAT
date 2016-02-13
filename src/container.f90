!  container_mod.f90
!  
!  Copyright 2015 Christopher MacMackin <cmacmackin@gmail.com>
!  
!  This program is free software; you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation; either version 2 of the License, or
!  (at your option) any later version.
!  
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!  
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
!  MA 02110-1301, USA.
!  
!  
 

module container_mod
  !! Author: Chris MacMackin
  !! Date: December 2015
  !! License: LGPLv3
  !!
  !! Provides implementations of the [[container]] abstract
  !! derived type for all of the intrinsic variable types.

  use abstract_container_mod, only: container
  use iso_fortran_env, only: i1 => int8, i2 => int16, i4 => int32, &
                             i8 => int64, r4 => real32, r8 => real64, &
                             r16 => real128
  implicit none
  private

  type, extends(container) ::  int_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the defualt integer type.
  contains
    private
    procedure   ::  typeguard => int_guard
  end type int_container

  type, extends(container) ::  int1_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the 1 byte integer type.
  contains
    private
    procedure   ::  typeguard => int1_guard
  end type int1_container

  type, extends(container) ::  int2_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the 2 byte integer type.
  contains
    private
    procedure   ::  typeguard => int2_guard
  end type int2_container

  type, extends(container) ::  int4_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the 4 byte integer type.
  contains
    private
    procedure   ::  typeguard => int4_guard
  end type int4_container

  type, extends(container) ::  int8_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the 8 byte integer type.
  contains
    private
    procedure   ::  typeguard => int8_guard
  end type int8_container

  type, extends(container) ::  real_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the defualt real type.
  contains
    private
    procedure   ::  typeguard => real_guard
  end type real_container
  
  type, extends(container) ::  real4_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the 4 byte real type.
  contains
    private
    procedure   ::  typeguard => real4_guard
  end type real4_container

  type, extends(container) ::  real8_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the 8 byte real type.
  contains
    private
    procedure   ::  typeguard => real8_guard
  end type real8_container

  type, extends(container) ::  real16_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the 16 byte real type.
  contains
    private
    procedure   ::  typeguard => real16_guard
  end type real16_container

  type, extends(container) ::  complex_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the defualt complex type.
  contains
    private
    procedure   ::  typeguard => complex_guard
  end type complex_container
  
  type, extends(container) ::  complex4_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the 4 byte complex type.
  contains
    private
    procedure   ::  typeguard => complex4_guard
  end type complex4_container

  type, extends(container) ::  complex8_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the 8 byte complex type.
  contains
    private
    procedure   ::  typeguard => complex8_guard
  end type complex8_container

  type, extends(container) ::  complex16_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the 16 byte complex type.
  contains
    private
    procedure   ::  typeguard => complex16_guard
  end type complex16_container

  type, extends(container) ::  logical_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the defualt logical type.
  contains
    private
    procedure   ::  typeguard => logical_guard
  end type logical_container
  
  type, extends(container) ::  character_container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! A container for holding the defualt character type.
  contains
    private
    procedure   ::  typeguard => character_guard
  end type character_container
  
  public :: character_container, complex_container, complex4_container, &
            complex8_container, complex16_container, int_container,     &
            int1_container, int2_container, int4_container,             &
            int8_container, logical_container, real_container,          &
            real4_container, real8_container, real16_container
  
contains

  logical function int_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(int_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(integer)
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function int_guard

  logical function int1_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(int1_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(integer(i1))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function int1_guard

  logical function int2_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(int2_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(integer(i2))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function int2_guard

  logical function int4_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(int4_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(integer(i4))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function int4_guard

  logical function int8_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(int8_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(integer(i8))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function int8_guard

  logical function real_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(real_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(real)
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function real_guard

  logical function real4_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(real4_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(real(r4))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function real4_guard

  logical function real8_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(real8_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(real(r8))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function real8_guard

  logical function real16_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(real16_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(real(r16))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function real16_guard
  
  logical function complex_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(complex_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(complex)
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function complex_guard

  logical function complex4_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(complex4_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(complex(r4))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function complex4_guard

  logical function complex8_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(complex8_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(complex(r8))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function complex8_guard

  logical function complex16_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(complex16_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(complex(r16))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function complex16_guard

  logical function logical_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(logical_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(logical)
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function logical_guard

  logical function character_guard(this, lhs) result(ret)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the container's contents to the variable on the left
    !! hand side of the equals sign if it is of the correct type.
    class(character_container), intent(in) ::  this
    class(*), intent(inout) ::  lhs
    select type(lhs)
      type is(character(len=*))
        lhs = transfer(this%contents(), lhs)
        ret = .true.
      class default
        ret = .false.
    end select
  end function character_guard

end module container_mod
