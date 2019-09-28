! Copyright (c) 2016, 2017  Robert R��ger
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

module ftlHashSetMod
	use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT,INT32,INT64,Real32,Real64 !����
	use ftlKindsModule
	use ftlHashModule
	implicit none

	private
	! ====== Type of the ftlHashSet container itself ============================================================================
	type :: BucketEntry !Ͱ
		type(DataEntry), pointer :: first => null() !����ͷ�ڵ�
	end type

	type :: DataEntry !�������ڵ�
		type(DataEntry), pointer :: next => null() !��ָ��
		class(*), allocatable :: value !Vֵ
	end type
	!ftlHashSet����=>type(BucketEntry)����buckets=>type(DataEntry)����ͷ�ڵ�first=>type(DataEntry)�ڵ�
	type, public :: ftlHashSet !ftlHashSet����
		private

		integer :: psize = 0 !Ԫ�ظ���
		real(FTL_KREAL) :: maxLoadFactor = 1.0 !����������ÿ��Ͱƽ�������Ԫ�صĸ���
		type(BucketEntry), allocatable :: buckets(:) !Ͱ һά��̬����
	contains
		private

		procedure :: NewDefault
		procedure :: NewCopyOther
		generic , public :: New => NewDefault, NewCopyOther !���췽��
		generic , public :: assignment(=) => NewCopyOther
		procedure, public :: Delete
		final :: Finalizer
		procedure :: BeginHashSet
		generic , public :: Begin => BeginHashSet !��ϣ��ͷ
		procedure :: EndHashSet
		generic , public :: End => EndHashSet !��ϣ��β
		procedure, public :: Empty !�ж��Ƿ�Ϊ��
		procedure :: SizeHashSet
		generic , public :: Size => SizeHashSet !Ԫ�ظ���
		procedure :: InsertStandard
		generic , public :: Insert => InsertStandard !����Ԫ��
		procedure :: HasStandard
		generic , public :: Has => HasStandard !�ж��Ƿ����
		procedure, pass(rhs) :: OperatorInStandard !ע�� �ı���ʵ��������λ��
		generic , public :: operator(.in.) => OperatorInStandard !���������
		procedure, public :: FindStandard
		generic , public :: Find => FindStandard !����Vֵ��Ӧ������
		procedure :: EraseValue
		procedure :: EraseSingleIterator
		procedure :: EraseIteratorPair
		generic , public :: Erase => EraseValue, EraseSingleIterator, EraseIteratorPair !ɾ��Ԫ��
		procedure, public :: Clear !�������Ԫ��
		procedure, public :: BucketCount !Ͱ�ĸ���
		procedure, public :: BucketSize !ĳ��Ͱ��������
		procedure :: BucketStandard
		generic , public :: Bucket => BucketStandard !���ݹ�ϣ����ֵ���෵��Ͱλ��
		procedure, public :: LoadFactor
		procedure, public :: SetMaxLoadFactor
		procedure, public :: GetMaxLoadFactor
		procedure, public :: Rehash
		procedure, public :: Reserve
		procedure, public :: WriteHashSet
#ifdef FTL_TEMPLATE_TYPE_IS_FTLSTRING
		procedure :: Insert_SPECIALIZATION_ftlString
		generic , public :: Insert => Insert_SPECIALIZATION_ftlString
		procedure :: Has_SPECIALIZATION_ftlString
		generic , public :: Has => Has_SPECIALIZATION_ftlString
		procedure, pass(rhs) :: OperatorIn_SPECIALIZATION_ftlString
		generic , public :: operator(.in.) => OperatorIn_SPECIALIZATION_ftlString
		procedure :: Find_SPECIALIZATION_ftlString
		generic , public :: Find => Find_SPECIALIZATION_ftlString
		procedure :: EraseValue_SPECIALIZATION_ftlString
		generic , public :: Erase => EraseValue_SPECIALIZATION_ftlString
		procedure :: Bucket_SPECIALIZATION_ftlString
		generic , public :: Bucket => Bucket_SPECIALIZATION_ftlString
#endif
	end type

	public :: Begin
	interface Begin
		module procedure BeginHashSet
	end interface

	public :: End
	interface End
		module procedure EndHashSet
	end interface

	public :: Size
	interface Size
		module procedure SizeHashSet
	end interface

	public :: WriteNode !����
	public :: EqualClass !����
	! ====== Type of an iterator over a ftlHashSet container =========================================================================
	type, public :: ftlHashSetIterator !������
		private

		type(BucketEntry) , pointer :: buckets(:) => null() !Ͱ ����ָ��
		integer :: bIdx = 0 !Ԫ������Ͱ��λ��
		type(DataEntry) , pointer :: entr => null() !Ԫ�����������ڵ�
		class(*), pointer :: value => null() ! �޸�Ϊ˽�б���
	contains
		private

		procedure :: NewItDefault
		procedure :: NewItCopyOther
		generic , public :: New => NewItDefault, NewItCopyOther !���췽��
		procedure, public :: Inc !��ָ��ֻ������
		procedure, public :: GetValue !����ʵ������
	end type

	public :: operator(==)
	interface operator(==)
		module procedure EqualOther

	end interface

	public :: operator(/=)
	interface operator(/=)
		module procedure UnequalOther
	end interface
contains
! ====== Implementation of ftlHashSet methods ====================================================================================
subroutine NewDefault(self, n) !���췽��
	class(ftlHashSet), intent(out) :: self
	integer , intent(in) :: n

	allocate(self%buckets(n)) !n��Ͱ
end subroutine
!
impure elemental subroutine NewCopyOther(self, other)
	class(ftlHashSet), intent(out), target :: self
	class(ftlHashSet), intent(in) , target :: other
	class(DataEntry), pointer :: getter, putter
	integer :: b

	self%psize = other%psize
	self%maxLoadFactor = other%maxLoadFactor

	if (allocated(other%buckets)) then
		allocate(self%buckets(size(other%buckets)))

		do b = 1, size(self%buckets) !����Ͱ
			! nothing to do for empty buckets
			if (.not.associated(other%buckets(b)%first)) cycle !��Ͱ����
			getter => other%buckets(b)%first !Ͱͷ�ڵ�
			allocate(self%buckets(b)%first)
			putter => self%buckets(b)%first
			putter%value = getter%value !����ͷ�ڵ�����

			do while (associated(getter%next)) !�����ڵ�
				getter => getter%next !��һ�ڵ�
				allocate(putter%next)
				putter => putter%next
				putter%value = getter%value !���ƽڵ�����
			enddo
		enddo
	endif
end subroutine

elemental subroutine Delete(self)
	class(ftlHashSet), intent(inout), target :: self

	call self%Clear()
	if (allocated(self%buckets)) deallocate(self%buckets) !�ͷ������ڴ�
	self%maxLoadFactor = 1.0
end subroutine
!
elemental subroutine Finalizer(self) !��������
	type(ftlHashSet), intent(inout) :: self

	call self%Delete()
end subroutine
! =============> Iterators:
type(ftlHashSetIterator) function BeginHashSet(self) result(Begin) !��Ԫ�ص�����
	class(ftlHashSet), intent(in), target :: self

	Begin%buckets => self%buckets !ָ��ָ��һά���� Ͱ�ı���
	Begin%bIdx = 1 !Ԫ������Ͱ��λ��

	do while (Begin%bIdx <= size(Begin%buckets)) !����Ͱ
		if (associated(Begin%buckets(Begin%bIdx)%first)) then !���ǿ�����
			Begin%entr => Begin%buckets(Begin%bIdx)%first !Ԫ����������ͷ�ڵ�
			Begin%value => Begin%entr%value !�ڶ���valueΪVֵ
			return !�ҵ���
		endif

		Begin%bIdx = Begin%bIdx + 1
	enddo
end function

type(ftlHashSetIterator) function EndHashSet(self) result(End) !βԪ�ص�����
	class(ftlHashSet), intent(in), target :: self

	End%buckets => self%buckets
	End%bIdx = size(self%buckets) + 1 !Ͱ����+1
end function
! =============> Capacity:
pure logical function Empty(self) !�ж��Ƿ�Ϊ��
	class(ftlHashSet), intent(in) :: self

	Empty = (self%psize == 0)
end function

pure integer function SizeHashSet(self) result(size) !Ԫ�ظ���
	class(ftlHashSet), intent(in) :: self

	size = self%psize
end function
! =============> Element access:
subroutine InsertStandard(self, value) !����Ԫ��
	class(ftlHashSet), intent(inout), target :: self
	class(*) , intent(in) :: value
	integer :: b
	class(DataEntry), pointer :: entr

	b = self%Bucket(value) !��ϣ����ֵ����ȷ��Ͱλ��

	if (.not.associated(self%buckets(b)%first)) then !ͷ�ڵ�Ϊ������
		allocate(self%buckets(b)%first) !�����ڴ�
		self%buckets(b)%first%value = value !���ýڵ�����
	else !ͷ�ڵ�Ϊ�ǿ����� λ���ѱ�ռ
		entr => self%buckets(b)%first !ͷ�ڵ�

		do while (.true.) !�����ڵ�
			!if (value == entr%value) then !ֱ�ӽ�Vֵ���� HashMap��Ԫ��Vֵû����ͬ��
			if (EqualClass(value,entr%value)) then !ֱ�ӽ�Vֵ���� HashMap��Ԫ��Vֵû����ͬ��
				return !�ҵ���
			endif
			!û���ҵ�
			if (associated(entr%next)) then !������ֹһ��Ԫ��
				entr => entr%next !��һ�ڵ������
			else !�������нڵ㶼û���ҵ�
				allocate(entr%next) !�����½ڵ�
				entr%next%value = value !�����½ڵ�����
				exit
			endif
		enddo
	endif

	self%psize = self%psize + 1
	if (self%LoadFactor() > self%maxLoadFactor) call self%Rehash(2 * self%BucketCount()) !Ͱ������һ��
end subroutine
!�ж�Ԫ���Ƿ����
logical function HasStandard(self, value) result(Has)
	class(ftlHashSet), intent(in), target :: self
	class(*) , intent(in) :: value
	type(DataEntry), pointer :: entr

	entr => self%buckets(self%Bucket(value))%first !Ͱ����������ͷ�ڵ�

	do while (associated(entr)) !�����ڵ�
		!if (value == entr%value) then
		if (EqualClass(value,entr%value)) then
			Has = .true.
			return !�ҵ���
		endif
		!û�ҵ�
		entr => entr%next !��һ�ڵ������
	end do
	!��������û�ҵ�
	Has = .false.
end function
!�ж�Ԫ���Ƿ���rhs��
logical function OperatorInStandard(lhs, rhs) result(in)
	class(*) , intent(in) :: lhs !Vֵ
	class(ftlHashSet), intent(in), target :: rhs !ʵ������self

	in = rhs%Has(lhs)
end function
!����Vֵ��Ӧ������
type(ftlHashSetIterator) function FindStandard(self, value) result(Find)
	class(ftlHashSet), intent(in), target :: self
	class(*) , intent(in) :: value

	Find%buckets => self%buckets
	Find%bidx = self%Bucket(value) !Ͱλ��
	Find%entr => Find%buckets(Find%bidx)%first !ͷ�ڵ�

	do while (associated(Find%entr)) !�����ڵ�
		!if (value == Find%entr%value) then
		if (EqualClass(value,Find%entr%value)) then
			Find%value => Find%entr%value
			return !�ҵ���
		endif
		!û�ҵ�
		Find%entr => Find%entr%next !��һ�ڵ������
	end do
	! Not found? Let's make the End() iterator ...
	Find%bIdx = size(self%buckets) + 1 !û�ҵ�
	nullify(Find%entr) !���ؿ�ָ��
end function
! =============> Modifiers:
subroutine EraseValue(self, value) !ɾ��Vֵ��ӦԪ��
	class(ftlHashSet), intent(inout) :: self
	class(*) , intent(in) :: value
	type(ftlHashSetIterator) :: it

	it = self%Find(value) !���Ҹýڵ�
	if (it /= self%End()) call self%EraseSingleIterator(it) !�ҵ��˲�ɾ��Ԫ��
end subroutine
!
subroutine EraseSingleIterator(self, it) !ɾ����������ӦԪ��
	class(ftlHashSet) , intent(inout), target :: self
	type(ftlHashSetIterator), intent(inout) :: it
	type(DataEntry), pointer :: entr

	if (.not.associated(it%buckets,self%buckets)) return !Ͱ����Ӧ

	if (associated(it%entr, self%buckets(it%bIdx)%first)) then !λ����ͷ�ڵ�
		self%buckets(it%bIdx)%first => it%entr%next !ͷ�ڵ㻻Ϊ��һ�ڵ�
	else !λ�ò���ͷ�ڵ�
		entr => self%buckets(it%bIdx)%first !ͷ�ڵ�

		do while (.not.associated(entr%next,it%entr)) !�����ڵ�
			entr => entr%next !��һ�ڵ�
		enddo
		!��ʱentrλ��Ϊit��ǰһλ��
		entr%next => it%entr%next !�Ͽ�it������
	endif

	deallocate(it%entr) !�������ͷ��ڴ�
	self%psize = self%psize - 1
end subroutine
!ɾ��first��last-1�Ľڵ�
subroutine EraseIteratorPair(self, first, last) !ɾ��[first, last-1]Ԫ��
	class(ftlHashSet) , intent(inout) :: self
	type(ftlHashSetIterator), intent(in) :: first, last
	type(ftlHashSetIterator) :: walker, deleter

	walker = first

	do while (walker /= last)
		deleter = walker
		call walker%Inc() !��һ�ڵ�
		call self%EraseSingleIterator(deleter)
	end do
end subroutine

elemental subroutine Clear(self) !�������Ԫ�� ������Ͱ���ڴ�
	class(ftlHashSet), intent(inout), target :: self
	integer :: b
	type(DataEntry), pointer :: walker, deleter

	if (allocated(self%buckets)) then
		do b = 1, size(self%buckets) !����Ͱ
			if (.not.associated(self%buckets(b)%first)) cycle !��ͰΪ������������
			walker => self%buckets(b)%first !Ͱ����ͷ�ڵ�
			deleter => walker

			do while (associated(walker%next)) !��������
				walker => walker%next !��һ�ڵ�
				deallocate(deleter) !�ͷ���һ�ڵ��ڴ�
				deleter => walker
			enddo

			deallocate(deleter) !�ͷ����ڵ��ڴ�
		enddo
	endif

	self%psize = 0
end subroutine
! =============> Buckets:
pure integer function BucketCount(self) !����Ͱ����  ����������
	class(ftlHashSet), intent(in) :: self

	BucketCount = size(self%buckets)
end function
! ���ظ�Ͱ����Ԫ�ظ���
integer function BucketSize(self, n)
	class(ftlHashSet), intent(in), target :: self
	integer , intent(in) :: n !Ͱ�����
	type(DataEntry), pointer :: entr

	entr => self%buckets(n)%first !Ͱ����ͷ�ڵ�

	if (.not.associated(entr)) then !������
		BucketSize = 0
	else
		BucketSize = 1

		do while (associated(entr%next)) !��������
			BucketSize = BucketSize + 1
			entr => entr%next
		enddo
	endif
end function
!���ݹ�ϣ����ֵ���෵��Ͱλ��
pure integer function BucketStandard(self, key) result(Bucket)
	class(ftlHashSet), intent(in) :: self
	class(*) , intent(in) :: key

	Bucket = mod(ftlHash(key), size(self%buckets)) + 1 !��ΧΪ[1,self%buckets)]
end function
! =============> Hash policy:
pure real(FTL_KREAL) function LoadFactor(self) !Ԫ�ظ�����Ͱ����֮��
	class(ftlHashSet), intent(in) :: self

	LoadFactor = real(self%psize, FTL_KREAL) / real(self%BucketCount(), FTL_KREAL)
end function
!
pure real(FTL_KREAL) function GetMaxLoadFactor(self)
	class(ftlHashSet), intent(in) :: self

	GetMaxLoadFactor = self%maxLoadFactor
end function
!
subroutine SetMaxLoadFactor(self, maxLoadFactor)
	class(ftlHashSet), intent(inout) :: self
	real(FTL_KREAL) , intent(in) :: maxLoadFactor

	if (maxLoadFactor > 0.0) then
		self%maxLoadFactor = maxLoadFactor
		if (self%LoadFactor() > self%maxLoadFactor) call self%Rehash(2 * self%BucketCount())
	endif
end subroutine
!�����Ź�ϣ�� Ͱ����������n��
subroutine Rehash(self, n)
	class(ftlHashSet), intent(inout), target :: self
	integer , intent(in) :: n
	type(BucketEntry), allocatable, target :: newbuckets(:)
	type(DataEntry), pointer :: eorg, tmp, enew
	integer :: borg, bnew

	if (n <= 0) return
	allocate(newbuckets(n))

	do borg = 1, size(self%buckets) !����ԭͰ
		eorg => self%buckets(borg)%first !ԭ��Ͱͷ�ڵ�

		do while (associated(eorg)) !�����ڵ�
			tmp => eorg !ԭ��Ͱ��ǰ�ڵ�
			eorg => eorg%next !ԭ��Ͱ��һ�ڵ�
			nullify(tmp%next) !��ǰ�ڵ�Ͽ�����
			bnew = mod(ftlHash(tmp%value), n) + 1 ![1,n] ��Ͱλ��

			if (.not.associated(newbuckets(bnew)%first)) then !������ͷ�ڵ�Ϊ��
				newbuckets(bnew)%first => tmp !��Ϊ�µ�ͷ�ڵ�
			else !������ͷ�ڵ�Ϊ�ǿ�
				enew => newbuckets(bnew)%first !������ͷ�ڵ�

				do while (associated(enew%next)) !����������
					enew => enew%next !��һ�ڵ�
				enddo
				!��ʱenewΪβ�ڵ�
				enew%next => tmp !������β�����ӽڵ�
			endif
		enddo
	enddo

	call move_alloc(newbuckets, self%buckets) !����
end subroutine

subroutine Reserve(self, n)
	class(ftlHashSet), intent(inout) :: self
	integer , intent(in) :: n

	if (n > self%BucketCount() * self%maxLoadFactor) call self%Rehash(ceiling(n / self%maxLoadFactor))
end subroutine
#ifdef FTL_TEMPLATE_TYPE_IS_FTLSTRING
! =============> Template specialization for ftlString as a value type:
! For all methods that accept a value, we define alternatives that accept a normal Fortran string instead of an ftlString.
! This is mostly so that we can provide a nicer interface, where you can do
!
! call Insert('my value')
!
! instead of
!
! call Insert(ftlString('my value'))
!
! It's a small thing, but no reason not to do it ...
subroutine Insert_SPECIALIZATION_ftlString(self, value)
	class(ftlHashSet), intent(inout) :: self
	character(len=*) , intent(in) :: value

	call self%Insert(ftlString(value))
end subroutine

logical function Has_SPECIALIZATION_ftlString(self, value) result(Has)
	class(ftlHashSet), intent(in) :: self
	character(len=*) , intent(in) :: value

	Has = self%Has(ftlString(value))
end function

logical function OperatorIn_SPECIALIZATION_ftlString(lhs, rhs) result(in)
	character(len=*) , intent(in) :: lhs
	class(ftlHashSet), intent(in), target :: rhs

	in = rhs%Has(ftlString(lhs))
end function

type(ftlHashSetIterator) &
function Find_SPECIALIZATION_ftlString(self, value) result(Find)
	class(ftlHashSet), intent(in) :: self
	character(len=*) , intent(in) :: value

	Find = self%Find(ftlString(value))
end function

subroutine EraseValue_SPECIALIZATION_ftlString(self, value)
	class(ftlHashSet), intent(inout) :: self
	character(len=*) , intent(in) :: value

	call self%EraseValue(ftlString(value))
end subroutine

integer function Bucket_SPECIALIZATION_ftlString(self, value) result(Bucket)
	class(ftlHashSet), intent(in) :: self
	character(len=*) , intent(in) :: value

	Bucket = self%Bucket(ftlString(value))
end function
#endif
! ====== Implementation of ftlHashSetIterator methods ============================================================================
subroutine NewItDefault(self)
	class(ftlHashSetIterator), intent(out) :: self
	! Nothing to do here: intent(out) already resets everything
end subroutine
!
subroutine NewItCopyOther(self, other)
	class(ftlHashSetIterator), intent(out) :: self
	class(ftlHashSetIterator), intent(in) :: other

	self%buckets => other%buckets !Ͱ�ı���
	self%bIdx = other%bIdx !Ԫ������Ͱ��λ��
	self%entr => other%entr !Ԫ�����������ڵ�
	if (associated(self%entr)) self%value => self%entr%value !�ǿսڵ�
end subroutine
! =============> Arithmetic operations:
subroutine Inc(self) !��һ�ڵ�
	class(ftlHashSetIterator), intent(inout) :: self

	if (associated(self%entr%next)) then !�����ڵ�����һ�ڵ�
		self%entr => self%entr%next !��һ�ڵ�
		self%value => self%entr%value
	else !�����ڵ�����һ�ڵ� ���ڵ��ڸ�Ͱ���һ���ڵ�
		do while (.true.) !����Ͱ
			self%bIdx = self%bIdx + 1 !��һ��Ͱ

			if (self%bIdx > size(self%buckets)) then !ͰβEnd() �ڵ�self%entrΪβͰ��β�ڵ�
				nullify(self%entr)
				nullify(self%value)
				return
			endif
			!����βͰ
			if (associated(self%buckets(self%bIdx)%first)) then !ͷ�ڵ�ǿ�
				self%entr => self%buckets(self%bIdx)%first !��һ��Ͱ����ͷ�ڵ�
				self%value => self%entr%value
				return
			endif
		enddo
	endif
end subroutine
! =============> Logical operations:
pure logical function EqualOther(self, other)
	class(ftlHashSetIterator), intent(in) :: self
	class(ftlHashSetIterator), intent(in) :: other

	EqualOther = &
	associated(self%entr, other%entr) .or. & !����Ͱβ
	(associated(self%buckets, other%buckets) .and. (self%bIdx == size(self%buckets) + 1) .and. (self%bIdx == other%bIdx) ) !ͰβEnd()
end function
!
pure logical function UnequalOther(self, other)
	class(ftlHashSetIterator), intent(in) :: self
	class(ftlHashSetIterator), intent(in) :: other

	UnequalOther = .not.EqualOther(self, other)
end function
!�������� ���޶�̬������������ж�
pure logical function EqualClass(self, other)
	class(*), intent(in) :: self
	class(*), intent(in) :: other
	EqualClass=.false. !��ֵΪ��

	select type(self)
		type is(integer(INT32))
			select type(other)
				type is(integer(INT32))
					EqualClass=(self==other)
			end select
		type is(integer(INT64))
			select type(other)
				type is(integer(INT64))
					EqualClass=(self==other)
			end select
		type is(real(Real32))
			select type(other)
				type is(real(Real32))
					EqualClass=(abs(self-other)<1.0e-5)
			end select
		type is(real(Real64))
			select type(other)
				type is(real(Real64))
					EqualClass=(abs(self-other)<1.0d-5)
			end select
		type is(Character(*))
			select type(other)
				type is(Character(*))
					EqualClass=(self==other)
			end select
		type is(Logical)
			select type(other)
				type is(Logical)
					EqualClass=(self.eqv.other)
			end select
	end select
end function
!��ӡԪ��ֵ ģ�鷽��
subroutine WriteNode(val, name, unit,IsNewLine)
	class(*), intent(in) :: val
	character(*), intent(in), optional :: name !Ԫ������
	integer, intent(in), optional :: unit !����ļ��� Ĭ��Ϊ��Ļ
	logical, intent(in), optional :: IsNewLine !Ĭ���滻�� �ٲ�����

	character(len=80) :: string
	integer :: lunit

	lunit = OUTPUT_UNIT !Ĭ�����Ϊ��Ļ
	if(present(unit)) lunit = unit

	if(present(name))then
		write(lunit,'(a)',advance='no') name // '='
	end if
	!���޶�̬���ñ�����select type
	select type(v_p => val)
		type is(integer(INT32))
			write(lunit,'(i0)',advance='no') v_p
		type is(integer(INT64))
			write(lunit,'(i0)',advance='no') v_p
		type is(real(Real32)) !fmt=*��advance='no'����ͬʱ����
			write(string,fmt=*) v_p
			write(lunit,'(a)',advance='no') trim(adjustl(string))
		type is(real(Real64)) !˫����
			write(string,fmt=*) v_p
			write(lunit,'(a)',advance='no') trim(adjustl(string))
		type is(complex(Real32))
			write(string,fmt=*) v_p !����ת��Ϊ�ַ���
			write(lunit,'(a)',advance='no') trim(adjustl(string)) !ɾ����β�ո�
		type is(complex(Real64))
			write(string,fmt=*) v_p !����ת��Ϊ�ַ���
			write(lunit,'(a)',advance='no') trim(adjustl(string)) !ɾ����β�ո�
		type is(logical)
			if (v_p) then
				write(lunit,'(a)',advance='no') 'true'
			else
				write(lunit,'(a)',advance='no') 'false'
			end if
		type is(character(*))
			write(lunit,'(3a)',advance='no') '"', v_p, '"'
		class default !δ֪����
			write(lunit,'(a)',advance='no') '*'
	end select

	if(present(IsNewLine).and.(.not.IsNewLine))then
		!������
	else
		write(lunit,*) !����
	end if
end subroutine WriteNode
!��ӡHashSet���нڵ�
subroutine WriteHashSet(self, unit)
	class(ftlHashSet), intent(in) :: self
	integer, intent(in), optional :: unit !����ļ��� Ĭ��Ϊ��Ļ
	type(ftlHashSetIterator) :: it

	integer :: lunit !ʵ������ļ���
	integer :: i

	lunit = OUTPUT_UNIT !Ĭ�����Ϊ��Ļ
	if(present(unit)) lunit = unit

	it=self%Begin() !��Ԫ��λ��

	do while(it/=self%End()) !�����ǿսڵ�
		if(associated(it%entr,it%buckets(it%bIdx)%first))then !��ǰ�ڵ�ΪͰ������ͷ�ڵ�
			write(lunit,'(a,i0,a)',advance='no') 'buckets(',it%bIdx,'):' !��ӡͰ���
			write(lunit,'(a)',advance='no') '[' !������
			i=0 !��ǰ�����ڵ����¼���
		end if

		i=i+1
		call WriteNode(it%value,unit=lunit,IsNewLine=.false.) !������
		!β��㲻��Ҫ�������
		if(i<self%BucketSize(it%bIdx)) then !��ǰ�ڵ㲻��Ͱ������β�ڵ�
			write(lunit,'(a)',advance='no') ',' !Ԫ�ؼ��Ϊ����
		else !��ǰ�ڵ�ΪͰ������β�ڵ�
			write(lunit,'(a)',advance='no') ']' !������
			write(lunit,*) !����
		end if

		call it%Inc() !��һ�ڵ�
	end do
end subroutine WriteHashSet
!����ʵ������ ���ص�������Ӧ�ڵ�Ԫ������
pure function GetValue(self) result(value)
	class(ftlHashSetIterator), intent(in) :: self
	class(*), allocatable :: value

	if(associated(self%value))value=self%value
end function GetValue
end module
