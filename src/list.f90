!
!  list.f90
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

module list_mod
  !* Author: Chris MacMackin
  !  Date: March 2016
  !  License: LGPLv3
  !
  ! Provides an abstract data type representing a list. This interface
  ! provides a derived type with many of the same abilities as lists in
  ! higher-level languages such as Python.
  !
  use deque_mod, only: deque
  use abstract_container_mod, only: container, test_func, &
                                    addition_func, subtraction_func, &
                                    comparison_func,  action_sub
  implicit none
  private
  
  type, abstract, extends(deque), public :: list
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! An abstract data type representing a list. This is a fully dynamic
    ! means of storing data of a single type and comes with many advanced
    ! type-bound procedures for manipulating said data. This derived type
    ! aims to provide many of the same features found in lists in 
    ! higher-level languages such as Python
    !
  contains
    procedure(append_sub), deferred :: append
      !! Add an item to the end of the list
    procedure(get_func), deferred :: get
      !! Get the item at the specified index
    procedure(set_single_sub), deferred, private :: set_single
      !! Set the specified element to the specified value
    procedure(set_multiple_sub), deferred, private :: set_multiple
      !! Set the specified elements to the specified values
    generic :: set => set_single, set_multiple
      !! Set the value of one or more elements in the list
    procedure(get_index_func), deferred :: get_index
      !! Get the index of the first occurrence of this item in the list
    procedure(get_last_index_func), deferred :: get_last_index
      !! Get the index of the last occurrence of this item in the list
    procedure(get_indices_func), deferred :: get_indices
      !! Get the indices of all occurrences of this item in the list
    procedure(slice_func), deferred :: slice
      !! Returns a list containing the items with indices in the
      !! specified range
    procedure(foreach_sub), deferred :: foreach
      !! Perform the provided procedure on each element of the list
    procedure(insert_sub), deferred :: insert
      !! Add the item to the specified position in the list, moving
      !! all succeeding elements forward by one position
    procedure(remove_sub), deferred :: remove
      !! Remove the first occurrence of the specified item from the list
    procedure(remove_last_sub), deferred :: remove_last
      !! Remove the last occurrence of the specified item from the list
    procedure(remove_all_sub), deferred :: remove_all
      !! Remove the all occurrences of the specified item from the list
    procedure(delete_single_sub), deferred, private :: delete_single
      !! Remove the item from the list at the specified index
    procedure(delete_multiple_sub), deferred, private :: delete_multiple
      !! Remove the items from the list at the specified indices
    procedure(delete_slice_sub), deferred, private :: delete_slice
      !! Remove the items from the list within the specified slice
    generic :: delete => delete_single, delete_multiple, delete_slice
      !! Remove one or more elements from the list
    procedure(has_func), deferred :: has
      !! Returns `.true.` if the specified item is present in the list.
    procedure(sort_sub), deferred :: sort
      !! Sorts the list in place using the provided comparison procedure
    procedure(min_func), deferred :: min
      !! Returns the smallest item in the list as determined using the
      !! provided comparison procedure
    procedure(max_func), deferred :: max
      !! Returns the largest item in the list as determined using the
      !! provided comparison procedure
    procedure(nearest_func), deferred :: nearest
      !! Returns the item in the list for which the provided subtraction
      !! procedure returns the smallest absolute value
    procedure(sum_func), deferred :: sum
      !! Returns an item representing the sum as determined by
      !! iteratively applying the provided addition procedure to all
      !! elements in the list
    procedure(filter_func), deferred :: filter
      !! Returns a list containing all elements from this list for which
      !! the provided test procedure returns `.true.`
    procedure(to_array_func), deferred :: to_array
      !! Returns an array of containers holding the contents of this
      !! list.
  end type list

  abstract interface
    subroutine append_sub(this, item)
      import :: list
      class(list), intent(inout) :: this
      class(*), intent(in) :: item !! Item to be appended to list
    end subroutine append_sub
  
    pure function get_func(this, element)
      import :: list
      import :: container
      class(list), intent(in) :: this
      integer, intent(in) :: element
        !! The index (starting from 1) of the element to return
      class(container), allocatable :: get_func
        !! The item with index `element`
    end function get_func
    
    subroutine set_single_sub(this, element, item)
      import :: list
      class(list), intent(inout) :: this
      integer, intent(in) :: element
        !! The index (starting from 1) of the element whose value is to be set.
      class(*), intent(in) :: item
        !! The value to store in the specified element
    end subroutine set_single_sub
    
    subroutine set_multiple_sub(this, elements, items)
      import :: list
      class(list), intent(inout) :: this
      integer, dimension(:), intent(in) :: elements
        !! The indices (starting from 1) of the elements whose values 
        !! are to be set.
      class(*), dimension(:), intent(in) :: items
        !! The values to be stored in the specified elements. Each item
        !! is placed in the element specified by the integer in the
        !! corresponding position in the array `elements`.
    end subroutine set_multiple_sub
    
    pure function get_index_func(this, item)
      import :: list
      class(list), intent(in) :: this
      class(*), intent(in) :: item
      integer :: get_index_func
        !! Position of the first occurrence of `item` in list
    end function get_index_func
    
    pure function get_last_index_func(this, item)
      import :: list
      class(list), intent(in) :: this
      class(*), intent(in) :: item
      integer :: get_last_index_func
        !! Position of the last occurrence of `item` in list
    end function get_last_index_func
    
    pure function get_indices_func(this, item)
      import :: list
      class(list), intent(in) :: this
      class(*), intent(in) :: item
      integer, dimension(:), allocatable :: get_indices_func
        !! Positions of the all occurrences of `item` in list
    end function get_indices_func
    
    function slice_func(this, start_element, end_element)
      import :: list
      class(list), intent(in) :: this
      integer, intent(in) :: start_element
        !! The index of the first element in the slice to be returned
      integer, intent(in) :: end_element
        !! The index of the last element in the slice to be returned
      class(list), allocatable :: slice_func
        !! A list containing the elements within the slice.
    end function slice_func
    
    subroutine foreach_sub(this, action)
      import :: list
      import :: action_sub
      class(list), intent(inout) :: this
      procedure(action_sub) :: action
        !! A procedure to act on each element of the list
    end subroutine foreach_sub
    
    subroutine insert_sub(this, position, item)
      import :: list
      class(list), intent(inout) :: this
      integer, intent(in) :: position
        !! The location at which the new element will be placed
      class(*), intent(in) :: item
        !! The value to be placed in the list
    end subroutine insert_sub
    
    subroutine remove_sub(this, item)
      import :: list
      class(list), intent(inout) :: this
      class(*), intent(in) :: item
        !! An item, the first occurrence of which will be removed from
        !! the list
    end subroutine remove_sub
    
    subroutine remove_last_sub(this, item)
      import :: list
      class(list), intent(inout) :: this
      class(*), intent(in) :: item
        !! An item, the last occurrence of which will be removed from
        !! the list
    end subroutine remove_last_sub
    
    subroutine remove_all_sub(this, item)
      import :: list
      class(list), intent(inout) :: this
      class(*), intent(in) :: item
        !! An item, all occurrences of which will be removed from the
        !! list
    end subroutine remove_all_sub
    
    subroutine delete_single_sub(this, element)
      import :: list
      class(list), intent(inout) :: this
      integer, intent(in) :: element
        !! The position of the element to be deleted from the list
    end subroutine delete_single_sub
    
    subroutine delete_multiple_sub(this, element)
      import :: list
      class(list), intent(inout) :: this
      integer, dimension(:), intent(in) :: element
        !! The positions of the elements to be deleted from the list
    end subroutine delete_multiple_sub
    
    subroutine delete_slice_sub(this, start_element, end_element)
      import :: list
      class(list), intent(inout) :: this
      integer, intent(in) :: start_element
        !! Index of the first element in the slice to be deleted
      integer, intent(in) :: end_element
        !! Index of the last element in the slice to be deleted
    end subroutine delete_slice_sub
    
    elemental function has_func(this, item)
      import :: list
      class(list), intent(in) :: this
      class(*), intent(in) :: item
        !! A value whose presence in the list is being checked for
      logical :: has_func
        !! `.true.` if `item` is present in list, `.false.` otherwise
    end function has_func
        
    subroutine sort_sub(this, comparison)
      import :: list
      import :: comparison_func
      class(list), intent(inout) :: this
      procedure(comparison_func) :: comparison
        !! A procedure which evaluates whether a [[container]] object is
        !! less than, equal to, or greater than another
    end subroutine sort_sub
    
    pure function min_func(this, comparison)
      import :: list
      import :: comparison_func
      import :: container
      class(list), intent(in) :: this
      procedure(comparison_func) :: comparison
        !! A procedure which evaluates whether a [[container]] object is
        !! less than, equal to, or greater than another    
      class(container), allocatable :: min_func
        !! The smallest item in the list, as determined by the
        !! `comparison` function
    end function min_func
 
    pure function max_func(this, comparison)
      import :: list
      import :: comparison_func
      import :: container
      class(list), intent(in) :: this
      procedure(comparison_func) :: comparison
        !! A procedure which evaluates whether a [[container]] object is
        !! less than, equal to, or greater than another    
      class(container), allocatable :: max_func
        !! The largest item in the list, as determined by the
        !! `comparison` function
    end function max_func
    
    pure function nearest_func(this, item, subtraction)
      import :: list
      import :: container
      import :: subtraction_func
      class(list), intent(in) :: this
      class(*), intent(in) :: item
        !! The value which those in the list are being compared to
      procedure(subtraction_func) :: subtraction
        !! A function determining the magnitude of the difference
        !! between two items
      class(container), allocatable :: nearest_func
        !! The value from the list which, when passed to `subtraction`
        !! with `item` as the other argument, returns the smallest value
    end function nearest_func
    
    pure function sum_func(this, addition)
      import :: list
      import :: container
      import :: addition_func
      class(list), intent(in) :: this
      procedure(addition_func) :: addition
        !! A procedure performing addition between two [[container]]
        !! objects and returning the result in another container
      class(container), allocatable :: sum_func
        !! A container holding the sum of all of the items held within
        !! this list
    end function sum_func

    pure function filter_func(this, test)
      import :: list
      import :: test_func
      class(list), intent(in) :: this
      procedure(test_func) :: test
        !! A test for which the values that pass will be returned in a
        !! new list
      class(list), allocatable :: filter_func
        !! Contains those items in this list for which `test` returns
        !! `.true.`
    end function filter_func

    pure function to_array_func(this)
      import :: list
      import :: container
      class(list), intent(in) :: this
      class(container), dimension(:), allocatable :: to_array_func
        !! An array of [[container]] objects holding the contents of
        !! this list
    end function to_array_func
  end interface
  
end module list_mod
