! Copyright (c) 2016, 2017  Robert Rüger
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

module ftlHashModule
	use ftlKindsModule
	use iso_fortran_env
	implicit none
	private

	public :: ftlHash
!	interface ftlHash !通用过程名
!		module procedure ftlHashInt32
!		module procedure ftlHashInt64
!		module procedure ftlHashReal32
!		module procedure ftlHashReal64
!		module procedure ftlHashCharacterString
!		module procedure ftlHashLogical
!	end interface
contains
! Note: These hash functions are probably pretty bad. I didn't really test any of this ...
pure integer function ftlHash(i) result(hash)
	class(*), intent(in) :: i

	select type(p=>i)
		type is(integer(INT32))
			hash=ftlHashInt32(p)
		type is(integer(INT64))
			hash=ftlHashInt64(p)
		type is(real(REAL32))
			hash=ftlHashReal32(p)
		type is(real(REAL64))
			hash=ftlHashReal64(p)
		type is(Character(*))
			hash=ftlHashCharacterString(p)
		type is(Logical)
			hash=ftlHashLogical(p)
	end select
end function

pure integer function ftlHashInt32(i) result(hash)
	integer(INT32), intent(in) :: i
	integer :: tmp

	tmp = int(i)

	if (tmp >= 0) then
		hash = tmp
	else
		hash = (huge(tmp) + tmp) + 1
	endif
end function

pure integer function ftlHashInt64(i) result(hash)
	integer(INT64), intent(in) :: i
	integer :: tmp

	tmp = int(i)

	if (tmp >= 0) then
		hash = tmp
	else
		hash = (huge(tmp) + tmp) + 1
	endif
end function

pure integer function ftlHashReal32(r) result(hash)
	real(REAL32), intent(in) :: r
	! TODO: handle +inf, -inf and NaN. didn't test this ...
	real(REAL32) :: rPosZero
	character(len=32) :: str

	rPosZero = r
	if (rPosZero == 0.0) rPosZero = 0.0 ! this turns a potential -0.0 into +0.0 (they also compare equal with ==)
	write (str,*) rPosZero !浮点数转换为字符串
	hash = ftlHashCharacterString(trim(str))
end function

pure integer function ftlHashReal64(r) result(hash)
	real(REAL64), intent(in) :: r
	! TODO: handle +inf, -inf and NaN. didn't test this ...
	real(REAL64) :: rPosZero
	character(len=32) :: str

	rPosZero = r
	if (rPosZero == 0.0) rPosZero = 0.0 ! this turns a potential -0.0 into +0.0 (they also compare equal with ==)
	write (str,*) rPosZero !浮点数转换为字符串
	hash = ftlHashCharacterString(trim(str))
end function

pure integer function ftlHashCharacterString(str) result(hash)
	character(len=*), intent(in) :: str
	integer :: i

	hash = 1299709

	do i = 1, len(str)
		hash = -101 * hash + ichar(str(i:i))
	enddo

	hash = abs(hash)
end function

pure integer function ftlHashLogical(l) result(hash)
	logical, intent(in) :: l

	if (l) then !1真0假
		hash = 1
	else
		hash = 0
	endif
end function
end module
