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
  !! License: LGPLv3
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

  type, public, abstract ::   container
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
    procedure, public :: is_filled
      !! Returns whether contents have been assigned to the container
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

    pure function test_func(item)
      !* An abstract interface for a function which tests a
      !  [[container]] object in some way
      import :: container
      class(container), intent(in) :: item
        !! The item which is being evaluated
      logical :: test_func
        !! Whether the item passes the test or not
    end function test_func
    
    pure function addition_func(item1, item2)
      !* Performs an addition operation on two [[container]] objects,
      !  returning the result in a container.
      import :: container
      class(container), intent(in) :: item1
        !! One of the items in the addition
      class(container), intent(in) :: item2
        !! The other item in the addition
      class(container), allocatable :: addition_func
        !! The sum, `item1 + item2`
    end function addition_func
    
    pure function subtraction_func(item1, item2)
      !! An abstract interface for a procedure finding the difference
      !! between two items, `item1 - item2`. Note that a procedure may
      !! satisfy both this abstract interface and [[comparison_func]].
      import :: container
      class(container), intent(in) :: item1
        !! The item which the other is subtracted from
      class(container), intent(in) :: item2
        !! The item subtracted from the other
      real :: subtraction_func
        !! A real number, the absolute value of which represents the
        !! magnitude of the difference between `item1` and `item2`.
    end function subtraction_func

    pure function comparison_func(item1, item2)
      !* An abstract interface for a procedure comparing two
      !  [[container]] objects. Note that a procedure may satisfy both
      !! this abstract interface and [[subtraction_func]].
      import :: container
      class(container), intent(in) :: item1
        !! The first item in the comparison
      class(container), intent(in) :: item2
        !! The second item in the comparison
      real :: comparison_func
        !! negative if `item1 < item2`, 0 if `item1 == item2`, positive 
        !! if `item1 > item2`
    end function comparison_func
  
    subroutine action_sub(item)
      !* An abstract interface for a procedure which will act on each
      !  item in a list.
      import :: container
      class(container), intent(inout) :: item
        !! A container object which is will be modified in some way
    end subroutine action_sub
  end interface
  
  public :: test_func, addition_func, subtraction_func, &
            comparison_func,  action_sub
  
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
          if (rhs%filled) then
            lhs%storage = rhs%storage
            lhs%filled = .true.
          else if (lhs%filled) then
            deallocate(lhs%storage)
            lhs%filled = .false.
          end if
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
  
  elemental logical function is_filled(this)
    !! Author: Chris MacMackin
    !! Date: March 2016
    !!
    !! Returns `.true.` if a value has been assigned to the container,
    !! `.false.` otherwise.
    class(container), intent(in) :: this
    is_filled = this%filled
  end function is_filled

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
    !! ~5GB of memory would end up being allocated when allocating tmp.
    !! After various experiments, I found that changing where tmp is
    !! allocated, so that this is only done if it is not being allocated
    !! to another container type, stopped this from happening. However,
    !! I'm still not clear on exactly what the cause of the bug is 
    !! (similar things occasionally happened when DEallocating a 
    !! container) and suspect its origin is a compiler bug. As such, I'm
    !! keeping this note here for information in case the issue ever
    !! arises again.
    !!
    class(container), intent(out)  ::  this
    class(*), intent(in)    ::  content
      !! The value to be placed in the container
    class(*), allocatable   ::  tmp
    if (.not. allocated(this%storage)) allocate(this%storage(1))
    if (same_type_as(this, content)) then
      select type(content)
        class is(container)
          if (content%filled) then
            this%filled = .true.
            this%storage = content%storage
          else
            this%filled = .false.
            deallocate(this%storage)
          endif
          return
      end select
    end if
    allocate(tmp, source=content)
    if (this%typeguard(tmp)) then
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
