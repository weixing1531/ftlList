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
	use ftlHashMapMod
	type(ftlHashMap) :: um
	type(ftlHashMapIterator) :: it

	call um%New(10)
	call um%Set('foo ', 42)
	call um%Set('bar ', 1)
	call um%Set('test', 2)
	call um%Set('blub', 3)
	call um%Set('jipi', 4)
	call um%Set('fort', 5)
	call um%Set('ran ', 6)
	call um%Set('is m', 7)
	call um%Set('y fa', 8)
	call um%Set('vour', 9)
	call um%Set('ite ', 10)
	call um%Set('lang', 11)
	call um%Set('not ', 12)
	call um%Set('rly!', 13)
	call um%Set(123.00, 123.00)

	call um%WriteHashMap()

	write(*,*)'not '.in.um

	write(*,*)um%BucketCount()
	write(*,*)um%Size()

	it=um%Begin()

	do while(it/=um%End())
		call WriteNode(it%GetValue())
		call it%Inc()
	end do

	it=um%Find('lang')
	call WriteNode(it%GetValue())

	call WriteNode(um%Get(123.00))

	call um%Erase(123.00)
	call um%Set('lang', 99)
	call um%WriteHashMap()
end program