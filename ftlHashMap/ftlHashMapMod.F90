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

module ftlHashMapMod
	use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT,INT32,INT64,Real32,Real64 !增加
	use ftlKindsModule
	use ftlHashModule
	implicit none

	private
	! ====== Type of the ftlHashMap container itself ============================================================================
	type :: BucketEntry !桶
		type(DataEntry), pointer :: first => null() !链表头节点
	end type

	type :: DataEntry !单链表节点
		type(DataEntry), pointer :: next => null() !后指针
		class(*) ,allocatable :: key !键K值
		class(*) ,allocatable :: value  !映射V值
	end type
	!ftlHashMap类型=>type(BucketEntry)数组buckets=>type(DataEntry)链表头节点first=>type(DataEntry)节点
	type, public :: ftlHashMap !ftlHashMap类型
		private

		integer :: psize = 0 !元素个数
		real(FTL_KREAL) :: maxLoadFactor = 1.0 !载入因子是每个桶平均保存的元素的个数
		type(BucketEntry), allocatable :: buckets(:) !桶 一维动态数组
	contains
		private
		!HashMap类对外接口：New,=,Delete,Begin,End,Empty,Size,Set,Get,Has,.in.,Find,Erase,Bucket
		procedure :: NewDefault
		procedure :: NewCopyOther
		generic , public :: New => NewDefault, NewCopyOther !构造方法
		generic , public :: assignment(=) => NewCopyOther
		procedure, public :: Delete
		final :: Finalizer !析构方法
		procedure :: BeginHashMap
		generic , public :: Begin => BeginHashMap !哈希表头
		procedure :: EndHashMap
		generic , public :: End => EndHashMap !哈希表尾
		procedure, public :: Empty !判断是否为空
		procedure :: SizeHashMap
		generic , public :: Size => SizeHashMap !元素个数
		procedure :: SetStandard
		generic , public :: Set => SetStandard !增加元素
		procedure :: GetStandard
		generic , public :: Get => GetStandard !获得元素V值
		procedure :: HasStandard
		generic , public :: Has => HasStandard !判断是否存在
		procedure, pass(rhs) :: OperatorInStandard !注意 改变了实例对象传入位置
		generic , public :: operator(.in.) => OperatorInStandard !重载运算符
		procedure, public :: FindStandard
		generic , public :: Find => FindStandard !返回K值对应迭代器
		procedure :: EraseKey
		procedure :: EraseSingleIterator
		procedure :: EraseIteratorPair
		generic , public :: Erase => EraseKey, EraseSingleIterator, EraseIteratorPair !删除元素
		procedure, public :: Clear !清除所有元素
		procedure, public :: BucketCount !桶的个数
		procedure, public :: BucketSize !某个桶链表长度
		procedure :: BucketStandard
		generic , public :: Bucket => BucketStandard !根据哈希函数值求余返回桶位置
		procedure, public :: LoadFactor
		procedure, public :: SetMaxLoadFactor
		procedure, public :: GetMaxLoadFactor
		procedure, public :: Rehash
		procedure, public :: Reserve
		procedure, public :: WriteHashMap !打印所有节点
#ifdef FTL_TEMPLATE_KEYTYPE_IS_FTLSTRING
		procedure :: Set_SPECIALIZATION_ftlString
		generic , public :: Set => Set_SPECIALIZATION_ftlString
		procedure :: Get_SPECIALIZATION_ftlString
		generic , public :: Get => Get_SPECIALIZATION_ftlString
		procedure :: Has_SPECIALIZATION_ftlString
		generic , public :: Has => Has_SPECIALIZATION_ftlString
		procedure, pass(rhs) :: OperatorIn_SPECIALIZATION_ftlString
		generic , public :: operator(.in.) => OperatorIn_SPECIALIZATION_ftlString
		procedure :: Find_SPECIALIZATION_ftlString
		generic , public :: Find => Find_SPECIALIZATION_ftlString
		procedure :: EraseKey_SPECIALIZATION_ftlString
		generic , public :: Erase => EraseKey_SPECIALIZATION_ftlString
		procedure :: Bucket_SPECIALIZATION_ftlString
		generic , public :: Bucket => Bucket_SPECIALIZATION_ftlString
#endif
	end type

	public :: Begin
	interface Begin
		module procedure BeginHashMap
	end interface

	public :: End
	interface End
		module procedure EndHashMap
	end interface

	public :: Size
	interface Size
		module procedure SizeHashMap
	end interface
	! ====== Type of an iterator over a ftlHashMap container =========================================================================
	type, public :: ftlHashMapIterator !迭代器
		private

		type(BucketEntry) , pointer :: buckets(:) => null() !桶 数组指针
		integer :: bIdx = 0 !元素所在桶的位置
		type(DataEntry) , pointer :: entr => null() !元素所在链表节点
		class(*) , pointer :: value => null() !元素映射V值  修改为私有变量
	contains
		private

		procedure :: NewItDefault
		procedure :: NewItCopyOther
		generic , public :: New => NewItDefault, NewItCopyOther !构造方法
		procedure, public :: Key !获得迭代器键K值
		procedure, public :: Inc !后指针只有向下
		procedure, public :: GetValue !新增实例方法
	end type

	public :: operator(==)
	interface operator(==)
		module procedure EqualOther
	end interface

	public :: operator(/=)
	interface operator(/=)
		module procedure UnequalOther
	end interface

	public :: EqualClass !新增
	public :: WriteNode !新增
contains
! ====== Implementation of ftlHashMap methods ====================================================================================
subroutine NewDefault(self, n) !构造方法
	class(ftlHashMap), intent(out) :: self
	integer , intent(in) :: n

	allocate(self%buckets(n)) !n个桶
end subroutine
!
impure elemental subroutine NewCopyOther(self, other)
	class(ftlHashMap), intent(out), target :: self
	class(ftlHashMap), intent(in) , target :: other
	class(DataEntry), pointer :: getter, putter
	integer :: b

	self%psize = other%psize
	self%maxLoadFactor = other%maxLoadFactor

	if (allocated(other%buckets)) then
		allocate(self%buckets(size(other%buckets)))

		do b = 1, size(self%buckets) !遍历桶
			! nothing to do for empty buckets
			if (.not.associated(other%buckets(b)%first)) cycle !空桶跳过
			getter => other%buckets(b)%first !桶头节点
			allocate(self%buckets(b)%first)
			putter => self%buckets(b)%first
			putter%key = getter%key !复制头节点数据
			putter%value = getter%value

			do while (associated(getter%next)) !遍历节点
				getter => getter%next !下一节点
				allocate(putter%next)
				putter => putter%next
				putter%key = getter%key !复制节点数据
				putter%value = getter%value
			enddo
		enddo
	endif
end subroutine

elemental subroutine Delete(self)
	class(ftlHashMap), intent(inout), target :: self

	call self%Clear()
	if (allocated(self%buckets)) deallocate(self%buckets) !释放数组内存
	self%maxLoadFactor = 1.0
end subroutine
!
elemental subroutine Finalizer(self) !析构方法
	type(ftlHashMap), intent(inout) :: self

	call self%Delete()
end subroutine
! =============> Iterators:
type(ftlHashMapIterator) function BeginHashMap(self) result(Begin) !首元素迭代器
	class(ftlHashMap), intent(in), target :: self

	Begin%buckets => self%buckets !指针指向一维数组 桶的别名
	Begin%bIdx = 1 !元素所在桶的位置

	do while (Begin%bIdx <= size(Begin%buckets)) !遍历桶
		if (associated(Begin%buckets(Begin%bIdx)%first)) then !不是空链表
			Begin%entr => Begin%buckets(Begin%bIdx)%first !元素所在链表头节点
			Begin%value => Begin%entr%value !第二个value为映射V值
			return !找到了
		endif

		Begin%bIdx = Begin%bIdx + 1
	enddo
end function

type(ftlHashMapIterator) function EndHashMap(self) result(End) !尾元素迭代器
	class(ftlHashMap), intent(in), target :: self

	End%buckets => self%buckets
	End%bIdx = size(self%buckets) + 1 !桶个数+1
end function
! =============> Capacity:
pure logical function Empty(self) !判断是否为空
	class(ftlHashMap), intent(in) :: self

	Empty = (self%psize == 0)
end function

pure integer function SizeHashMap(self) result(size) !元素个数
	class(ftlHashMap), intent(in) :: self

	size = self%psize
end function
! =============> Element access:
subroutine SetStandard(self, key, value) !增加元素
	class(ftlHashMap), intent(inout), target :: self
	class(*) , intent(in) :: key
	class(*) , intent(in) :: value
	integer :: b
	class(DataEntry), pointer :: entr

	b = self%Bucket(key) !哈希函数值求余确定桶位置

	if (.not.associated(self%buckets(b)%first)) then !头节点为空链表
		allocate(self%buckets(b)%first) !分配内存
		self%buckets(b)%first%key = key !设置节点数据
		self%buckets(b)%first%value = value
	else !头节点为非空链表 位置已被占
		entr => self%buckets(b)%first !头节点

		do while (.true.) !遍历节点
			!if (key == entr%key) then !K值冲突以最后的V值为准
			if (EqualClass(key , entr%key)) then !K值冲突以最后的V值为准
				entr%value = value !直接将K值相同的对应V值覆盖
				return !找到了
			endif
			!没有找到
			if (associated(entr%next)) then !链表不止一个元素
				entr => entr%next !下一节点继续找
			else !链表所有节点都没有找到
				allocate(entr%next) !增加新节点
				entr%next%key = key !设置新节点数据
				entr%next%value = value
				exit
			endif
		enddo
	endif

	self%psize = self%psize + 1
	if (self%LoadFactor() > self%maxLoadFactor) call self%Rehash(2 * self%BucketCount()) !桶数扩大一倍
end subroutine

function GetStandard(self, key) result(value) !获得元素V值
	class(ftlHashMap), intent(in), target :: self
	class(*) , intent(in) :: key
	class(*) , pointer :: value
	type(DataEntry), pointer :: entr

	entr => self%buckets(self%Bucket(key))%first !桶所在链表的头节点

	do while (associated(entr)) !遍历节点
		!if (key == entr%key) then
		if (EqualClass(key , entr%key)) then
			value => entr%value
			return !找到了
		endif
		!没找到
		entr => entr%next !下一节点继续找
	end do
	!遍历后还是没找到
	nullify(value) !返回空指针
end function
!判断元素是否存在
logical function HasStandard(self, key) result(Has)
	class(ftlHashMap), intent(in), target :: self
	class(*) , intent(in) :: key
	type(DataEntry), pointer :: entr

	entr => self%buckets(self%Bucket(key))%first !桶所在链表的头节点

	do while (associated(entr)) !遍历节点
		!if (key == entr%key) then
		if (EqualClass(key , entr%key)) then
			Has = .true.
			return !找到了
		endif
		!没找到
		entr => entr%next !下一节点继续找
	end do
	!遍历后还是没找到
	Has = .false.
end function
!判断元素是否在rhs中
logical function OperatorInStandard(lhs, rhs) result(in)
	class(*) , intent(in) :: lhs !K值
	class(ftlHashMap), intent(in), target :: rhs !实例对象self

	in = rhs%Has(lhs)
end function
!返回K值对应迭代器
type(ftlHashMapIterator) function FindStandard(self, key) result(Find)
	class(ftlHashMap), intent(in), target :: self
	class(*) , intent(in) :: key

	Find%buckets => self%buckets
	Find%bidx = self%Bucket(key) !桶位置
	Find%entr => Find%buckets(Find%bidx)%first !头节点

	do while (associated(Find%entr)) !遍历节点
		!if (key == Find%entr%key) then
		if (EqualClass(key , Find%entr%key)) then
			Find%value => Find%entr%value
			return !找到了
		endif
		!没找到
		Find%entr => Find%entr%next !下一节点继续找
	end do
	! Not found? Let's make the End() iterator ...
	Find%bIdx = size(self%buckets) + 1 !没找到
	nullify(Find%entr) !返回空指针
end function
! =============> Modifiers:
subroutine EraseKey(self, key) !删除K值对应元素
	class(ftlHashMap), intent(inout) :: self
	class(*) , intent(in) :: key
	type(ftlHashMapIterator) :: it

	it = self%Find(key) !查找该节点
	if (it /= self%End()) call self%EraseSingleIterator(it) !找到了并删除元素
end subroutine
!
subroutine EraseSingleIterator(self, it) !删除迭代器对应元素
	class(ftlHashMap) , intent(inout), target :: self
	type(ftlHashMapIterator), intent(inout) :: it
	type(DataEntry), pointer :: entr

	if (.not.associated(it%buckets,self%buckets)) return !桶不对应

	if (associated(it%entr, self%buckets(it%bIdx)%first)) then !位置在头节点
		self%buckets(it%bIdx)%first => it%entr%next !头节点换为下一节点
	else !位置不在头节点
		entr => self%buckets(it%bIdx)%first !头节点

		do while (.not.associated(entr%next,it%entr)) !遍历节点
			entr => entr%next !下一节点
		enddo
		!此时entr位置为it的前一位置
		entr%next => it%entr%next !断开it的链接
	endif

	deallocate(it%entr) !迭代器释放内存
	self%psize = self%psize - 1
end subroutine
!删除first到last-1的节点
subroutine EraseIteratorPair(self, first, last) !删除[first, last-1]元素
	class(ftlHashMap) , intent(inout) :: self
	type(ftlHashMapIterator), intent(in) :: first, last
	type(ftlHashMapIterator) :: walker, deleter

	walker = first

	do while (walker /= last)
		deleter = walker
		call walker%Inc() !下一节点
		call self%EraseSingleIterator(deleter)
	end do
end subroutine

elemental subroutine Clear(self) !清除所有元素 但保留桶的内存
	class(ftlHashMap), intent(inout), target :: self
	integer :: b
	type(DataEntry), pointer :: walker, deleter

	if (allocated(self%buckets)) then
		do b = 1, size(self%buckets) !遍历桶
			if (.not.associated(self%buckets(b)%first)) cycle !若桶为空链表则跳过
			walker => self%buckets(b)%first !桶链表头节点
			deleter => walker

			do while (associated(walker%next)) !遍历链表
				walker => walker%next !下一节点
				deallocate(deleter) !释放上一节点内存
				deleter => walker
			enddo

			deallocate(deleter) !释放最后节点内存
		enddo
	endif

	self%psize = 0
end subroutine
! =============> Buckets:
pure integer function BucketCount(self) !返回桶个数  即链表条数
	class(ftlHashMap), intent(in) :: self

	BucketCount = size(self%buckets)
end function
! 返回各桶链表元素个数
integer function BucketSize(self, n)
	class(ftlHashMap), intent(in), target :: self
	integer , intent(in) :: n !桶的序号
	type(DataEntry), pointer :: entr

	entr => self%buckets(n)%first !桶链表头节点

	if (.not.associated(entr)) then !空链表
		BucketSize = 0
	else !非空链表
		BucketSize = 1

		do while (associated(entr%next)) !遍历链表
			BucketSize = BucketSize + 1
			entr => entr%next !下一节点
		enddo
	endif
end function
!根据哈希函数值求余返回桶位置
pure integer function BucketStandard(self, key) result(Bucket)
	class(ftlHashMap), intent(in) :: self
	class(*) , intent(in) :: key

	Bucket = mod(ftlHash(key), size(self%buckets)) + 1 !范围为[1,self%buckets)]
end function
! =============> Hash policy:
pure real(FTL_KREAL) function LoadFactor(self) !元素个数与桶个数之比
	class(ftlHashMap), intent(in) :: self

	LoadFactor = real(self%psize, FTL_KREAL) / real(self%BucketCount(), FTL_KREAL)
end function
!
pure real(FTL_KREAL) function GetMaxLoadFactor(self)
	class(ftlHashMap), intent(in) :: self

	GetMaxLoadFactor = self%maxLoadFactor
end function
!
subroutine SetMaxLoadFactor(self, maxLoadFactor)
	class(ftlHashMap), intent(inout) :: self
	real(FTL_KREAL) , intent(in) :: maxLoadFactor

	if (maxLoadFactor > 0.0) then
		self%maxLoadFactor = maxLoadFactor
		if (self%LoadFactor() > self%maxLoadFactor) call self%Rehash(2 * self%BucketCount()) !桶个数扩大一倍
	endif
end subroutine
!重新排哈希表 桶个数调整到n个
subroutine Rehash(self, n)
	class(ftlHashMap), intent(inout), target :: self
	integer , intent(in) :: n
	type(BucketEntry), allocatable, target :: newbuckets(:)
	type(DataEntry), pointer :: eorg, tmp, enew
	integer :: borg, bnew

	if (n <= 0) return
	allocate(newbuckets(n))

	do borg = 1, size(self%buckets) !遍历原桶
		eorg => self%buckets(borg)%first !原各桶头节点

		do while (associated(eorg)) !遍历节点
			tmp => eorg !原各桶当前节点
			eorg => eorg%next !原各桶下一节点
			nullify(tmp%next) !当前节点断开关联
			bnew = mod(ftlHash(tmp%key), n) + 1 ![1,n] 新桶位置

			if (.not.associated(newbuckets(bnew)%first)) then !新链表头节点为空
				newbuckets(bnew)%first => tmp !作为新的头节点
			else !新链表头节点为非空
				enew => newbuckets(bnew)%first !新链表头节点

				do while (associated(enew%next)) !遍历新链表
					enew => enew%next !下一节点
				enddo
				!此时enew为尾节点
				enew%next => tmp !新链表尾部增加节点
			endif
		enddo
	enddo

	call move_alloc(newbuckets, self%buckets) !拷贝
end subroutine

subroutine Reserve(self, n)
	class(ftlHashMap), intent(inout) :: self
	integer , intent(in) :: n

	if (n > self%BucketCount() * self%maxLoadFactor) call self%Rehash(ceiling(n / self%maxLoadFactor))
end subroutine
#ifdef FTL_TEMPLATE_KEYTYPE_IS_FTLSTRING
! =============> Template specialization for ftlString as a key type:
! For all methods that accept a key, we define alternatives that accept a normal Fortran string instead of an ftlString.
! This is mostly so that we can provide a nicer interface, where you can do
!
! call Set('my key', value)
!
! instead of
!
! call Set(ftlString('my key'), value)
!
! It's a small thing, but no reason not to do it ...
subroutine Set_SPECIALIZATION_ftlString(self, key, value)
	class(ftlHashMap), intent(inout) :: self
	character(len=*) , intent(in) :: key
	class(*) , intent(in) :: value

	call self%Set(ftlString(key), value)
end subroutine

function Get_SPECIALIZATION_ftlString(self, key) result(value)
	class(ftlHashMap), intent(in) :: self
	character(len=*) , intent(in) :: key
	class(*), pointer :: value

	value => self%Get(ftlString(key))
end function

logical function Has_SPECIALIZATION_ftlString(self, key) result(Has)
	class(ftlHashMap), intent(in) :: self
	character(len=*) , intent(in) :: key

	Has = self%Has(ftlString(key))
end function

logical function OperatorIn_SPECIALIZATION_ftlString(lhs, rhs) result(in)
	character(len=*) , intent(in) :: lhs
	class(ftlHashMap), intent(in), target :: rhs

	in = rhs%Has(ftlString(lhs))
end function

type(ftlHashMapIterator) &
function Find_SPECIALIZATION_ftlString(self, key) result(Find)
	class(ftlHashMap), intent(in) :: self
	character(len=*) , intent(in) :: key

	Find = self%Find(ftlString(key))
end function

subroutine EraseKey_SPECIALIZATION_ftlString(self, key)
	class(ftlHashMap), intent(inout) :: self
	character(len=*) , intent(in) :: key

	call self%EraseKey(ftlString(key))
end subroutine

integer function Bucket_SPECIALIZATION_ftlString(self, key) result(Bucket)
	class(ftlHashMap), intent(in) :: self
	character(len=*) , intent(in) :: key

	Bucket = self%Bucket(ftlString(key))
end function
#endif
! ====== Implementation of ftlHashMapIterator methods ============================================================================
subroutine NewItDefault(self)
	class(ftlHashMapIterator), intent(out) :: self
	! Nothing to do here: intent(out) already resets everything
end subroutine
!
subroutine NewItCopyOther(self, other)
	class(ftlHashMapIterator), intent(out) :: self
	class(ftlHashMapIterator), intent(in) :: other

	self%buckets => other%buckets !桶的别名
	self%bIdx = other%bIdx !元素所在桶的位置
	self%entr => other%entr !元素所在链表节点
	if (associated(self%entr)) self%value => self%entr%value !非空节点
end subroutine
! Getter method for the key of the entry an iterator is pointing to. We have to make this a method so that the
! key can not be changed through the iterator. This would seriously mess up the hash map ...
!
function Key(self) result(Res)!返回迭代器K值
	class(ftlHashMapIterator), intent(in) :: self
	class(*) ,allocatable :: Res

	if (associated(self%entr)) Res = self%entr%key
end function
! =============> Arithmetic operations:
subroutine Inc(self) !下一节点
	class(ftlHashMapIterator), intent(inout) :: self

	if (associated(self%entr%next)) then !链表节点有下一节点
		self%entr => self%entr%next !下一节点
		self%value => self%entr%value
	else !链表节点无下一节点 即节点在该桶最后一个节点

		do while (.true.) !遍历桶
			self%bIdx = self%bIdx + 1 !下一个桶

			if (self%bIdx > size(self%buckets)) then !桶尾End() 节点self%entr为尾桶的尾节点
				nullify(self%entr)
				nullify(self%value)
				return
			endif
			!不在尾桶
			if (associated(self%buckets(self%bIdx)%first)) then !头节点非空
				self%entr => self%buckets(self%bIdx)%first !下一个桶链表头节点
				self%value => self%entr%value
				return
			endif
		enddo
	endif
end subroutine
! =============> Logical operations:
pure logical function EqualOther(self, other)
	class(ftlHashMapIterator), intent(in) :: self
	class(ftlHashMapIterator), intent(in) :: other

	EqualOther = &
	associated(self%entr, other%entr) .or. & !不在桶尾
	(associated(self%buckets, other%buckets) .and. (self%bIdx == size(self%buckets) + 1) .and. (self%bIdx == other%bIdx) ) !桶尾End()
end function
!
pure logical function UnequalOther(self, other)
	class(ftlHashMapIterator), intent(in) :: self
	class(ftlHashMapIterator), intent(in) :: other

	UnequalOther = .not.EqualOther(self, other)
end function
!新增方法 无限多态类型数据相等判断
pure logical function EqualClass(self, other)
	class(*), intent(in) :: self
	class(*), intent(in) :: other
	EqualClass=.false. !初值为假

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
!打印元素值 模块方法
subroutine WriteNode(val, name, unit,IsNewLine)
	class(*), intent(in) :: val
	character(*), intent(in), optional :: name !元素名称
	integer, intent(in), optional :: unit !输出文件号 默认为屏幕
	logical, intent(in), optional :: IsNewLine !默认真换行 假不换行

	character(len=80) :: string
	integer :: lunit

	lunit = OUTPUT_UNIT !默认输出为屏幕
	if(present(unit)) lunit = unit

	if(present(name))then
		write(lunit,'(a)',advance='no') name // '='
	end if
	!无限多态引用必须用select type
	select type(v_p => val)
		type is(integer(INT32))
			write(lunit,'(i0)',advance='no') v_p
		type is(integer(INT64))
			write(lunit,'(i0)',advance='no') v_p
		type is(real(Real32)) !fmt=*与advance='no'不能同时出现
			write(string,fmt=*) v_p
			write(lunit,'(a)',advance='no') trim(adjustl(string))
		type is(real(Real64)) !双精度
			write(string,fmt=*) v_p
			write(lunit,'(a)',advance='no') trim(adjustl(string))
		type is(complex(Real32))
			write(string,fmt=*) v_p !复数转换为字符串
			write(lunit,'(a)',advance='no') trim(adjustl(string)) !删除首尾空格
		type is(complex(Real64))
			write(string,fmt=*) v_p !复数转换为字符串
			write(lunit,'(a)',advance='no') trim(adjustl(string)) !删除首尾空格
		type is(logical)
			if (v_p) then
				write(lunit,'(a)',advance='no') 'true'
			else
				write(lunit,'(a)',advance='no') 'false'
			end if
		type is(character(*))
			write(lunit,'(3a)',advance='no') '"', v_p, '"'
		class default !未知类型
			write(lunit,'(a)',advance='no') '*'
	end select

	if(present(IsNewLine).and.(.not.IsNewLine))then
		!不换行
	else
		write(lunit,*) !换行
	end if
end subroutine WriteNode
!打印HashMap所有节点
subroutine WriteHashMap(self, unit)
	class(ftlHashMap), intent(in) :: self
	integer, intent(in), optional :: unit !输出文件号 默认为屏幕
	type(ftlHashMapIterator) :: it

	integer :: lunit !实际输出文件号
	integer :: i

	lunit = OUTPUT_UNIT !默认输出为屏幕
	if(present(unit)) lunit = unit

	it=self%Begin() !首元素位置

	do while(it/=self%End()) !遍历非空节点
		if(associated(it%entr,it%buckets(it%bIdx)%first))then !当前节点为桶链表的头节点
			write(lunit,'(a,i0,a)',advance='no') 'buckets(',it%bIdx,'):' !打印桶序号
			write(lunit,'(a)',advance='no') '[' !不换行
			i=0 !当前链表节点重新计数
		end if

		i=i+1
		call WriteNode(it%value,unit=lunit,IsNewLine=.false.) !不换行
		!尾结点不需要间隔逗号
		if(i<self%BucketSize(it%bIdx)) then !当前节点不是桶链表的尾节点
			write(lunit,'(a)',advance='no') ',' !元素间隔为逗号
		else !当前节点为桶链表的尾节点
			write(lunit,'(a)',advance='no') ']' !不换行
			write(lunit,*) !换行
		end if

		call it%Inc() !下一节点
	end do
end subroutine WriteHashMap
!增加实例方法 返回迭代器对应节点元素数据
pure function GetValue(self) result(value)
	class(ftlHashMapIterator), intent(in) :: self
	class(*), allocatable :: value

	if(associated(self%value))value=self%value
end function GetValue
end module