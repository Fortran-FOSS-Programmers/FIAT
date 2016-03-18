!
!  array_list.f90
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

module array_list_mod
  !* Author: Chris MacMackin
  !  Date: March 2016
  !  License: LGPLv3
  !
  ! Provides a concrete implementation of the [[list]] abstract data
  ! type. This implementation, the [[array_list]], stores values in an
  ! array and thus fast to read from.
  !
  use list_mod, only: list
  use abstract_container_mod, only: container, test_func, &
                                    addition_func, subtraction_func, &
                                    comparison_func,  action_sub
  use iterator_mod, only: iterator
  use ordered_mod, only: ordered
  implicit none
  private
  
  type, public, extends(list) :: array_list
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! A concrete implementation of the [[list]] abstract data type.
    ! This implementation stores list contents in an array. It is fast
    ! to read from, but it is slow to insert elements into the middle
    ! of the list or to add new items to the end (if doing so requires
    ! the storage array to be expanded).
    !
    ! Note that this list is 1-indexed and that all slices are 
    ! inclusive. In this regard, they are similar to arrays and strings
    ! in Fortran.
    !
    private
    class(container), allocatable, dimension(:) :: contents
    integer :: length
  contains
    procedure :: iter => array_list_iter
    procedure :: contents_type => array_list_contents_type
    procedure :: size => array_list_size
    procedure :: push => array_list_push
    procedure :: pop => array_list_pop
    procedure :: peek => array_list_peek
    procedure, private :: concat => array_list_concat
    procedure :: clear => array_list_clear
    procedure :: pushleft => array_list_push
    procedure :: pushright => array_list_append
    procedure :: popleft => array_list_popleft
    procedure :: popright => array_list_pop
    procedure :: peekleft => array_list_peekleft
    procedure :: peekright => array_list_peek
    procedure :: append => array_list_append
    procedure :: get => array_list_get
    procedure, private :: set_single  => array_list_set_single
    procedure, private :: set_multiple => array_list_set_multiple
    procedure :: get_index => array_list_get_index
    procedure :: get_last_index => array_list_get_last_index
    procedure :: get_indices => array_list_get_indices
    procedure :: slice => array_list_slice
    procedure :: foreach => array_list_foreach
    procedure :: insert => array_list_insert
    procedure :: remove => array_list_remove
    procedure :: remove_last => array_list_remove_last
    procedure :: remove_all => array_list_remove_all
    procedure, private :: delete_single => array_list_delete_single
    procedure, private :: delete_multiple => array_list_delete_multiple
    procedure, private :: delete_slice => array_list_delete_slice
    procedure :: has => array_list_has
    procedure :: sort => array_list_sort
    procedure :: min => array_list_min
    procedure :: max => array_list_max
    procedure :: nearest => array_list_nearest
    procedure :: sum => array_list_sum
    procedure :: filter => array_list_filter
    procedure :: to_array => array_list_to_array
  end type array_list

contains

  pure type(iterator) function array_list_iter(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns an [[iterator]] containing the contents of this list
    ! as they were at the time this method was called.
    !
    class(array_list), intent(in) :: this
  end function array_list_iter
  
  pure function array_list_contents_type(this) result(cont)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns a container with the dynamic type of the contents of this
    ! list.
    !
    class(array_list), intent(in) :: this
    class(container), allocatable :: cont
  end function array_list_contents_type

  pure integer function array_list_size(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns the number of items in the list
    !
    class(array_list), intent(in) :: this
  end function array_list_size

  pure subroutine array_list_push(this, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Adds an item to the beginning of the list.
    !
    class(array_list), intent(inout) :: this
    class(*), intent(in) :: item
      !! The value to place at the start of the list.
  end subroutine array_list_push

  function array_list_pop(this) result(pop)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Removes the item at the end of the list and returns it.
    !
    class(array_list), intent(inout) :: this
    class(container), allocatable :: pop
      !! The value from the end of the list
  end function array_list_pop

  pure function array_list_peek(this) result(peek)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !  
    ! Returns the item at the end of the list, without removing it.
    !
    class(array_list), intent(in) :: this
    class(container), allocatable :: peek
      !! The element at the end of the list
  end function array_list_peek
  
  pure function array_list_concat(lhs, rhs) result(concat)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Join this list with an [[ordered]] object, returning the result. The 
    ! contents of the returned object are ordered such that applying 
    ! [[ordered:pop]] until it is empty would return items in the same order as 
    ! calling [[ordered:pop]] until the list is empty and then until the second 
    ! object is empty.
    !
    class(array_list), intent(in) :: lhs !! The list
    class(ordered), intent(in) :: rhs !! The object being concatenated to the list
    class(ordered), allocatable :: concat
     !! The concatenated object. Will have dynamic type [[array_list]]. 
  end function array_list_concat

  pure subroutine array_list_clear(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Removes all items from this list, but does not change its container type.
    !
    class(array_list), intent(inout) :: this
  end subroutine array_list_clear

  function array_list_popleft(this) result(pop)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Removes the item at the start of the list and returns it.
    !
    class(array_list), intent(inout) :: this
    class(container), allocatable :: pop
      !! The item just removed from the start of the list
  end function array_list_popleft
  
  pure function array_list_peekleft(this) result(peek)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns the item at the start of the list.
    !
    class(array_list), intent(in) :: this
    class(container), allocatable :: peek
      !! The item at the start of the list
  end function array_list_peekleft
  
  pure subroutine array_list_append(this, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Adds the provided item to the end of the list.
    !
    class(array_list), intent(inout) :: this
    class(*), intent(in) :: item !! Item to be appended to list
  end subroutine array_list_append

  pure function array_list_get(this, element)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns the item at the specified location in the list. Returns
    ! an unallocated container if that location has not been set.
    !
    class(array_list), intent(in) :: this
    integer, intent(in) :: element
      !! The index (starting from 1) of the element to return
    class(container), allocatable :: array_list_get
      !! The item with index `element`. Unallocated if no such element
      !! is present.
  end function array_list_get
  
  subroutine array_list_set_single(this, element, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Places the value of the provided element in the specified location
    ! in the array. Crashes if this element is not already defined.
    !
    class(array_list), intent(inout) :: this
    integer, intent(in) :: element
      !! The index (starting from 1) of the element whose value is to be set.
    class(*), intent(in) :: item
      !! The value to store in the specified element
  end subroutine array_list_set_single
  
  subroutine array_list_set_multiple(this, elements, items)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Places the values of the provided array of items in the locations
    ! specified by the corresponding indices in the in the array of
    ! elements.
    !
    class(array_list), intent(inout) :: this
    integer, dimension(:), intent(in) :: elements
      !! The indices (starting from 1) of the elements whose values 
      !! are to be set.
    class(*), dimension(:), intent(in) :: items
      !! The values to be stored in the specified elements. Each item
      !! is placed in the element specified by the integer in the
      !! corresponding position in the array `elements`.
  end subroutine array_list_set_multiple
  
  pure function array_list_get_index(this, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns the index of the first occurrence of the item in the list.
    ! If there are no occurrences, then returns -1.
    !
    class(array_list), intent(in) :: this
    class(*), intent(in) :: item
    integer :: array_list_get_index
      !! Position of the first occurrence of `item` in list
  end function array_list_get_index
  
  pure function array_list_get_last_index(this, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns the index of the last occurrence of the item in the list.
    ! If there are no occurrences, then returns -1.
    !
    class(array_list), intent(in) :: this
    class(*), intent(in) :: item
    integer :: array_list_get_last_index
      !! Position of the last occurrence of `item` in list. -1 if `item`
      !! not present
  end function array_list_get_last_index
  
  pure function array_list_get_indices(this, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns an array containing the indices of all occurrences of the
    ! item in the list. If there are no occurrences, then returns an
    ! unallocated array.
    !
    class(array_list), intent(in) :: this
    class(*), intent(in) :: item
    integer, dimension(:), allocatable :: array_list_get_indices
      !! Positions of the all occurrences of `item` in list. Unallocated
      !! if `item` not present.
  end function array_list_get_indices
  
  function array_list_slice(this, start_element, end_element)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns an [[array_list]] instance containing all items in this
    ! list within the specified slice, inclusive. Crashes if one of
    ! the indices is outside of the list's bounds.
    !
    class(array_list), intent(in) :: this
    integer, intent(in) :: start_element
      !! The index of the first element in the slice to be returned
    integer, intent(in) :: end_element
      !! The index of the last element in the slice to be returned
    class(list), allocatable :: array_list_slice
      !! An array_list containing the elements within the slice.
  end function array_list_slice
  
  subroutine array_list_foreach(this, action)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Performs the specified action on each item in the list, in place.
    !
    class(array_list), intent(inout) :: this
    procedure(action_sub) :: action
      !! A procedure to act on each element of the list
  end subroutine array_list_foreach
  
  subroutine array_list_insert(this, position, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Places the provided item into the list at the position specified,
    ! moving all succeeding items forward by one. The position must be
    ! one which is already filled or the length of the list plus one.
    !
    class(array_list), intent(inout) :: this
    integer, intent(in) :: position
      !! The location at which the new element will be placed
    class(*), intent(in) :: item
      !! The value to be placed in the list
  end subroutine array_list_insert
  
  subroutine array_list_remove(this, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Remove the first occurrence of the specified item from the list,
    ! moving all succeeding items back by one position. No action is
    ! taken if the item is not present.
    !
    class(array_list), intent(inout) :: this
    class(*), intent(in) :: item
      !! An item, the first occurrence of which will be removed from
      !! the list
  end subroutine array_list_remove
  
  subroutine array_list_remove_last(this, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Remove the last occurrence of the specified item from the list,
    ! moving all succeeding items back by one position. No action is
    ! taken if the item is not present.
    !
    class(array_list), intent(inout) :: this
    class(*), intent(in) :: item
      !! An item, the last occurrence of which will be removed from
      !! the list
  end subroutine array_list_remove_last
  
  subroutine array_list_remove_all(this, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Remove all occurrences of the specified item from the list,
    ! moving all succeeding items back in position. No action is taken
    ! if the item is not present.
    !
    class(array_list), intent(inout) :: this
    class(*), intent(in) :: item
      !! An item, all occurrences of which will be removed from the
      !! list
  end subroutine array_list_remove_all
  
  subroutine array_list_delete_single(this, element)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Removes the element at the specified position in the list, moving
    ! all succeeding items back by one position. Crashes if the element
    ! has not been set.
    !
    class(array_list), intent(inout) :: this
    integer, intent(in) :: element
      !! The position of the element to be deleted from the list
  end subroutine array_list_delete_single
  
  subroutine array_list_delete_multiple(this, element)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Removes the elements at the positions specified by the indices in
    ! the array `elements` from the list. The elements removed are those
    ! at the specified location just before the call was made. Once all
    ! removals have been performed, the remaining elements well be moved
    ! backwards in position as necessary. Crashes if an element has not
    ! been set.
    !
    class(array_list), intent(inout) :: this
    integer, dimension(:), intent(in) :: element
      !! The positions of the elements to be deleted from the list
  end subroutine array_list_delete_multiple
  
  subroutine array_list_delete_slice(this, start_element, end_element)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Removes the element within the specified slice of the list, moving
    ! all succeeding items back by the number of items deleted. The 
    ! slice contains the element at the start index to the end index,
    ! inclusive. Crashes if part of the slice has not been set.
    !
    class(array_list), intent(inout) :: this
    integer, intent(in) :: start_element
      !! Index of the first element in the slice to be deleted
    integer, intent(in) :: end_element
      !! Index of the last element in the slice to be deleted
  end subroutine array_list_delete_slice
  
  elemental function array_list_has(this, item)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Indicates whether there are any copies of the specified item
    ! present in the list.
    !
    class(array_list), intent(in) :: this
    class(*), intent(in) :: item
      !! A value whose presence in the list is being checked for
    logical :: array_list_has
      !! `.true.` if `item` is present in list, `.false.` otherwise
  end function array_list_has
      
  subroutine array_list_sort(this, comparison)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Sorts the list, in place, so that all items are in ascending order
    ! according the the provided comparison function.
    !
    class(array_list), intent(inout) :: this
    procedure(comparison_func) :: comparison
      !! A procedure which evaluates whether a [[container]] object is
      !! less than, equal to, or greater than another
  end subroutine array_list_sort
  
  pure function array_list_min(this, comparison)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns the smallest item contained in the list, as determined
    ! using the provided comparison function.
    !
    class(array_list), intent(in) :: this
    procedure(comparison_func) :: comparison
      !! A procedure which evaluates whether a [[container]] object is
      !! less than, equal to, or greater than another    
    class(container), allocatable :: array_list_min
      !! The smallest item in the list, as determined by the
      !! `comparison` function
  end function array_list_min

  pure function array_list_max(this, comparison)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns the largest item contained in the list, as determined
    ! using the provided comparison function.
    !
    class(array_list), intent(in) :: this
    procedure(comparison_func) :: comparison
      !! A procedure which evaluates whether a [[container]] object is
      !! less than, equal to, or greater than another    
    class(container), allocatable :: array_list_max
      !! The largest item in the list, as determined by the
      !! `comparison` function
  end function array_list_max
  
  pure function array_list_nearest(this, item, subtraction)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns the value contained in the list for which the subtraction
    ! function returns the smallest absolute real number when comparing
    ! with the specified `item`.
    !
    class(array_list), intent(in) :: this
    class(*), intent(in) :: item
      !! The value which those in the list are being compared to
    procedure(subtraction_func) :: subtraction
      !! A function determining the magnitude of the difference
      !! between two items
    class(container), allocatable :: array_list_nearest
      !! The value from the list which, when passed to `subtraction`
      !! with `item` as the other argument, returns the smallest value
  end function array_list_nearest
  
  pure function array_list_sum(this, addition)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns a [[container]] representing the sum of all items 
    ! contained in the list. This sum is calculated by repeatedly
    ! applying the addition procedure to the list's contents.
    !
    class(array_list), intent(in) :: this
    procedure(addition_func) :: addition
      !! A procedure performing addition between two [[container]]
      !! objects and returning the result in another container
    class(container), allocatable :: array_list_sum
      !! A container holding the sum of all of the items held within
      !! this list
  end function array_list_sum

  pure function array_list_filter(this, test)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns a new list containing only those items held in this list
    ! which pass the provided test.
    !
    class(array_list), intent(in) :: this
    procedure(test_func) :: test
      !! A test for which the values that pass will be returned in a
      !! new list
    class(list), allocatable :: array_list_filter
      !! Contains those items in this list for which `test` returns
      !! `.true.`
  end function array_list_filter

  pure function array_list_to_array(this)
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! Returns copies of all items held in this list, stored within an
    ! array of [[container]] objects.
    !  
    class(array_list), intent(in) :: this
    class(container), dimension(:), allocatable :: array_list_to_array
      !! An array of [[container]] objects holding the contents of
      !! this list
  end function array_list_to_array

end module array_list_mod
