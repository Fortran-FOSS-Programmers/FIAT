!
!  data_set.f90
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

module data_set_mod
  !* Author: Chris MacMackin
  !  Date: March 2016
  !  License: LGPLv3
  !
  ! Provides an abstract type for the set data structure. Such sets
  ! support similar operations to their mathematical counterparts.
  ! This class, and its descendents, have a comparable set of methods
  ! to those in the Python 
  ! [frozenset](https://docs.python.org/2/library/stdtypes.html#frozenset)
  ! type.
  !
  use abstract_container_mod, only: container, test_func, &
                                    addition_func, subtraction_func, &
                                    comparison_func,  action_sub
  use countable_mod, only: countable
  use array_list_mod, only: array_list
  implicit none
  private

  type, public, extends(countable), abstract :: data_set
    !* Author: Chris MacMackin
    !  Date: March 2016
    !
    ! An abstract type for set data structures. These sets support a 
    ! similar selection of operations as do their mathematical
    ! counterparts. The collection of methods for this type waas inspired
    ! by those available for the
    ! [frozenset](https://docs.python.org/2/library/stdtypes.html#frozenset)
    ! type in Python.
    !
  contains
    procedure(union_func), deferred :: union
      !! Returns a set containing the items held in this set and the
      !! passed set.
    generic :: operator(+) => union
      !! Returns the union of the two sets
    procedure(intersect_func), deferred :: intersection
      !! Returns a set containing only the items present in both this
      !! set and the argument
    procedure(diff_func), deferred :: difference
      !! Returns a set containing the item present in this set but not
      !! present in the argument
    generic :: operator(-) => difference
        !! Returns the difference between the two sets
    procedure(pure_diff_func), deferred :: symmetric_difference
      !! Returns a set containing those items present either only in 
      !! this set or only in the argument
    procedure(relation_func), deferred :: is_disjoint
      !! True if none of the items in this set are present in the 
      !! argument
    procedure(relation_func), deferred :: is_subset
      !! True if every element in this set is also in the argument
    procedure(relation_func), deferred :: is_proper_subset
      !! True if every element in this set is also in the argument and
      !! this set is not equal to the argument
    procedure(relation_func), deferred :: is_superset
      !! True if every element in the argument is also present in this
      !! set.
    procedure(relation_func), deferred :: is_proper_superset
      !! True if every element in the argument is also present in this
      !! set and the argument is not equal to this set.
    procedure(relation_func), deferred :: is_equal
      !! True if this set and the argument contain only the same items.
    generic :: operator(<=) => is_subset
      !! True if every element in this set is also in the argument
    generic :: operator(<) => is_proper_subset
      !! True if every element in this set is also in the argument and
      !! this set is not equal to the argument
    generic :: operator(>=) => is_superset
      !! True if every element in the argument is also present in this
      !! set.
    generic :: operator(>) => is_proper_superset
      !! True if every element in the argument is also present in this
      !! set and the argument is not equal to this set.
    generic :: operator(==) => is_equal
      !! True if this set and the argument contain only the same items.
    procedure(has_func), deferred :: has
      !! True if the argument is present in the set
    procedure(empty_func), deferred :: is_empty
      !! True if the set contains no items
    procedure(peek_func), deferred :: peek
      !! Returns an item, at random, from the set
    procedure(filter_func), deferred :: filter
      !! Returns a set containing all elements from this set for which
      !! the provided test procedure returns `.true.`
    procedure(enum_func), deferred :: enumerate
      !! Returns an [array_list] containing all of the items present
      !! in this set.
    procedure(min_func), deferred :: min
      !! Returns the smallest item in the set as determined using the
      !! provided comparison procedure
    procedure(max_func), deferred :: max
      !! Returns the largest item in the set as determined using the
      !! provided comparison procedure
    procedure(nearest_func), deferred :: nearest
      !! Returns the item in the set for which the provided subtraction
      !! procedure returns the smallest absolute value
    procedure(sum_func), deferred :: sum
      !! Returns an item representing the sum as determined by
      !! iteratively applying the provided addition procedure to all
      !! items in the set
  end type data_set

  abstract interface
    pure function union_func(this, other)
      import :: data_set
      class(data_set), intent(in) :: this, other !! A second set
      class(data_set), allocatable :: union_func
        !! A set containing all elements found in this one and the other
    end function union_func
    
    pure function intersect_func(this, other)
      import :: data_set
      class(data_set), intent(in) :: this, other !! A second set
      class(data_set), allocatable :: union_func
        !! A set containing all elements found both in this one and 
        !! the other
    end function intersect_func
    
    pure function diff_func(this, other)
      import :: data_set
      class(data_set), intent(in) :: this, other !! A second set
      class(data_set), allocatable :: diff_func
        !! A set containing elements found in this one and not the other
    end function diff_func
    
    pure function pure_diff_func(this, other)
      import :: data_set
      class(data_set), intent(in) :: this, other !! A second set
      class(data_set), allocatable :: pure_diff_func
        !! A set containing elements found in one, but not both, of this
        !! set or the other set
    end function pure_diff_func
    
    pure function relation_func(this, other)
      import :: data_set
      class(data_set), intent(in) :: this, other !! A second set
      logical :: relation_func
        !! Whether the relationship between `this` and `other` is true
    end function relation_func
    
    pure function has_func(this, item)
      import :: data_set
      class(data_set), intent(in) :: this
      class(*), intent(in) :: item
        !! An item which may be contained in the set
      logical :: has_func
        !! Whether `item` is present in this set
    end function has_func
    
    pure function empty_func(this)
      import :: data_set
      class(data_set), intent(in) :: this
      logical :: empty_func
        !! `.true.` if this set contains no items
    end function empty_func
    
    pure function peek_func(this)
      import :: data_set
      import :: container
      class(data_set), intent(in) :: this
      class(container), allocatable :: peek_func
        !! An item contained within the set. Is unallocated if the set
        !! is empty.
    end function peek_func
    
    pure function filter_func(this, test)
      import :: data_set
      import :: test_func
      class(data_set), intent(in) :: this
      procedure(test_func) :: test
        !! A test for which the values that pass will be returned in a
        !! new list
      class(data_set), allocatable :: filter_func
        !! Contains those items in this set for which `test` returns
        !! `.true.`
    end function filter_func

    pure function enum_func(this)
      import :: data_set
      import :: array_list
      class(data_set), intent(in) :: this
      type(array_list) :: enum_func
        !! A list containing copies of all of the items in this set
    end function enum_func
    
    pure function min_func(this, comparison)
      import :: data_set
      import :: comparison_func
      import :: container
      class(data_set), intent(in) :: this
      procedure(comparison_func) :: comparison
        !! A procedure which evaluates whether a [[container]] object is
        !! less than, equal to, or greater than another    
      class(container), allocatable :: min_func
        !! The smallest item in the set, as determined by the
        !! `comparison` function
    end function min_func
 
    pure function max_func(this, comparison)
      import :: data_set
      import :: comparison_func
      import :: container
      class(data_set), intent(in) :: this
      procedure(comparison_func) :: comparison
        !! A procedure which evaluates whether a [[container]] object is
        !! less than, equal to, or greater than another    
      class(container), allocatable :: max_func
        !! The largest item in the set, as determined by the
        !! `comparison` function
    end function max_func
    
    pure function nearest_func(this, item, subtraction)
      import :: data_set
      import :: container
      import :: subtraction_func
      class(data_set), intent(in) :: this
      class(*), intent(in) :: item
        !! The value which those in the set are being compared to
      procedure(subtraction_func) :: subtraction
        !! A function determining the magnitude of the difference
        !! between two items
      class(container), allocatable :: nearest_func
        !! The value from the set which, when passed to `subtraction`
        !! with `item` as the other argument, returns the smallest value
    end function nearest_func
    
    pure function sum_func(this, addition)
      import :: data_set
      import :: container
      import :: addition_func
      class(data_set), intent(in) :: this
      procedure(addition_func) :: addition
        !! A procedure performing addition between two [[container]]
        !! objects and returning the result in another container
      class(container), allocatable :: sum_func
        !! A container holding the sum of all of the items held within
        !! this set
    end function sum_func
  end interface
  
end module data_set_mod
