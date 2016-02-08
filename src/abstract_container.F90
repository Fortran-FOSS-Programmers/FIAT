!
!  abstract_container_mod.f90
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

module abstract_container_mod
  !! Author: Chris MacMackin
  !! Date: December 2015
  !! License: GPLv3
  !!
  !! Provides an abstract container derived type which can be used 
  !! as a sort of unlimited polymorphic entity whose contents are
  !! retrievable with type-guards. Different subclasses are created
  !! in order to hold different data-types. See [[container]] for 
  !! instructions on creating concrete subclasses. See [[container_mod]]
  !! for subclasses containing the built-in data-types.

  use iso_fortran_env, only: stderr => error_unit, i1 => int8
  implicit none
  private

  type, abstract ::   container
    !! Author: Chris MacMackin
    !! Date: December 2015
    !! Display: Public
    !!          Private
    !!
    !! An abstract derived type which contains data. This type can be
    !! used for a sort of unlimited polymorphism. It is extended to
    !! create different classes capable of holding particular 
    !! data-types. Extensions must implement the procedure 
    !! [[container:typeguard]] in order to provide the ability to
    !! transfer data out of the container and into a variable. Assuming
    !! that you are creating a concrete class called 
    !! `example_container`, this should be implemented as follows:
    !!
    !!```fortran
    !! module example_container_mod
    !! 
    !!   use abstract_container_mod
    !!   implicit none
    !!   private
    !! 
    !!   type example
    !!     integer, public :: i
    !!   end type example
    !! 
    !!   type, extends(container) :: example_container
    !!   contains
    !!     private
    !!     procedure :: typeguard => example_guard
    !!   end type example_container
    !! 
    !! contains
    !! 
    !!   logical function example_guard(this, lhs) result(ret)
    !!     class(example_container), intent(in) :: this
    !!     class(*), intent(inout) :: lhs
    !!     select type(lhs)
    !!       type is(example)
    !!         lhs = transfer(this%contents(), lhs)
    !!         ret = .true.
    !!       class default
    !!         ret = .false.
    !!     end select
    !!   end function example_guard
    !! 
    !! end module example_container_mod
    !!```
    private
    integer(i1), dimension(:), allocatable :: storage
      !! Variable in which to place data contents
    logical ::  filled = .false.
      !! `.true.` if container is set, `.false.` otherwise
  contains
    private
    procedure(guard), deferred :: typeguard
      !! Performs the actual transfer of the container's contents to 
      !! another variable.
    procedure, public :: contents
      !! Retrieves the contents of the container, in the form of an
      !! integer array.
    procedure, public :: set
      !! Sets the contents of the container.
    procedure, pass(rhs) :: assign_container
      !! Assigns container contents to another variable.
    procedure :: is_equal
      !! Check whether two containers have the same contents.
    generic, public :: assignment(=) => assign_container
    generic, public :: operator(==) => is_equal
  end type container

  abstract interface
    logical function guard(this, lhs)
      import container
      class(container), intent(in) ::  this
      class(*), intent(inout) ::  lhs
        !! The variable which the container contents are to be 
        !! transferred to.
    end function guard
  end interface
  
  public    ::  container
  
contains
  
  subroutine assign_container(lhs, rhs)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Transfers the contents of the container to another variable.
    !! If the other variable is another container of the same type
    !! then the contents will be transferred. If the other variable is
    !! the same type as the contents of the container (as determined
    !! by the [[container:typeguard]] routine provided for that 
    !! concrete type extension) then it will be given the value held by
    !! the container. Otherwise, an error message will be printed and 
    !! the program stopped. If compiled with `gfortran` then a backtrace
    !! will also be printed. In the event that the container was never
    !! set to a value, then this also constitutes an error.
    class(*), intent(inout) ::  lhs
      !! The variable which the container contents will be assigned to.
    class(container), intent(in)  ::  rhs
      !! The container variable.
    !-------------------------------------------------------------------
    select type(lhs)
      class is(container)
        if (same_type_as(lhs, rhs)) then
          lhs%storage = rhs%storage
          return
        else
          write(stderr,*) "ERROR: Can not assign to a different container subclass"
#ifdef __GFORTRAN__
          call backtrace
#endif
          stop
        end if
      class default
        if (rhs%filled) then
          if (rhs%typeguard(lhs)) return
          write(stderr,*) "ERROR: Can not assign this container's contents to given variable"
#ifdef __GFORTRAN__
          call backtrace
#endif
          stop
        else
          write(stderr,*) "ERROR: Container is empty."
#ifdef __GFORTRAN__
          call backtrace
#endif
          stop
        end if
    end select
  end subroutine assign_container

  pure function contents(this)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Returns the contents, encoded as a character array, of the 
    !! container.
    class(container), intent(in)   ::  this
    integer(i1), dimension(:), allocatable  ::  contents
    contents = this%storage
  end function contents

  subroutine set(this, content)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Sets the contents of the storage array to value passed. The type
    !! of the variable provided must be the same as the container
    !! variable is designed to accept (as determined by the
    !! concrete type implementation of the [[container:typeguard]]
    !! method in the extension) or be of the same type of container.
    !! Otherwise an error message will be printed and the program will 
    !! exit. If `gfortran` was used to compile then a backtrace will
    !! also be printed.
    !!
    !! @Warning During the initial phase of writing unit tests for the 
    !! containers, I found that when content is class(container) then
    !! ~5GB of memory would end up being allocated at line 204. After
    !! various experiments which involved moving the code around, it
    !! stopped doing this when I changed it back. I have no idea why
    !! it works now and suspect that this is a bug with gfortran 5.3.0.
    class(container), intent(out)  ::  this
    class(*), intent(in)    ::  content
      !! The value to be placed in the container
    class(*), allocatable   ::  tmp
    allocate(tmp, source=content)
    if (.not. allocated(this%storage)) allocate(this%storage(1))
    if (same_type_as(this, content)) then
      select type(content)
        class is(container)
          if (content%filled) then
            this%filled = .true.
            this%storage = content%storage
          else
            this%filled = .false.
            if (allocated(this%storage)) deallocate(this%storage)
          endif
          return
      end select
    else if (this%typeguard(tmp)) then
      this%filled = .true.
      this%storage = transfer(content, this%storage)
    else
      write(stderr,*) "ERROR: Can not assign given variable to this container"
#ifdef __GFORTRAN__
      call backtrace
#endif
      stop
    end if
  end subroutine set

  elemental logical function is_equal(lhs, rhs)
    !! Author: Chris MacMackin
    !! Date: December 2015
    !!
    !! Checks whether two containers are of the same type and are
    !! storing the same contents.
    class(container), intent(in) :: lhs, rhs
    if (.not.same_type_as(lhs, rhs)) then
      is_equal = .false.
      return
    end if
    if ((.not.lhs%filled).and.(.not.rhs%filled)) then
      is_equal = .true.
      return
    end if
    if (lhs%filled.neqv.rhs%filled) then
      is_equal = .false.
      return
    end if
    is_equal = (size(lhs%storage) == size(rhs%storage) .and. &
                all(lhs%storage == rhs%storage))
  end function is_equal

end module abstract_container_mod
