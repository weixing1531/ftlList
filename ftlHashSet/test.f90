! Copyright (c) 2016, 2017  Robert R¨¹ger
!
! This file is part of of the Fortran Template Library.
!
! The Fortran Template Library is free software: you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! The Fortran Template Library is distributed in the hope that it will be
! useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
! General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License along
! with the Fortran Template Library.  If not, see <http://www.gnu.org/licenses/>.

program main
	use ftlHashSetMod
	type(ftlHashSet) :: um
	type(ftlHashSetIterator) :: it

	call um%New(10)
	call um%Insert(42)
	call um%Insert(1)
	call um%Insert(2)
	call um%Insert(3)
	call um%Insert(4)
	call um%Insert(5)
	call um%Insert(6)
	call um%Insert(7)
	call um%Insert(8)
	call um%Insert(9)
	call um%Insert(10)
	call um%Insert(11)
	call um%Insert(12)
	call um%Insert(13)
	call um%Insert(123.00)

	call um%WriteHashSet()

	write(*,*)11.in.um

	write(*,*)um%BucketCount()
	write(*,*)um%Size()

	it=um%Begin()

	do while(it/=um%End())
		call WriteNode(it%GetValue())
		call it%Inc()
	end do

	it=um%Find(10)
	call WriteNode(it%GetValue())

	call um%Erase(123.00)
	call um%Insert(99)
	call um%WriteHashSet()
end program