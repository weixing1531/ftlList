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

module ftlSingleListMod
	use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
	implicit none
	!模块对外接口包括Begin,End,Size,ftlSwap,ftlMove,operator(==),operator(/=),WriteNode(增加方法)
	private
	! ====== Type of the ftlSingleList container itself ====================================================================================
	type :: ListNode !父类 单向链表节点  哨兵节点或数据节点
		class(ListNode), pointer :: next => null() !后指针 类指针 可指向父类ListNode及其子类
	end type

	type, extends(ListNode) :: DataNode !单向链表节点的子类 数据节点 在子程InsertNodeAfter中使用
		class(*), allocatable :: data !参考FCCF代码 存放数据节点的无限多态数据
	end type

	type, extends(ListNode) :: SLNode !单向链表节点的子类 哨兵节点 本模块关键 用于确定尾节点位置
		class(ListNode), pointer :: prev => null() !前指针 类指针 可指向父类ListNode及其子类
	end type
	!                                End             Begin                                            End
	!环状单向链表的节点排列顺序为：哨兵节点(无元素)－头节点(第一个元素)－...－尾节点(最后一个元素)－哨兵节点(无元素)－...
	type, public :: ftlSingleList !环状单向链表
		private

		integer :: psize = 0 !链表的元素个数 私有变量
		type(SLNode) :: sentinel !父类 哨兵节点（无data不保存数据） 私有变量 修改
		class(*), pointer :: front => null() !头节点元素数值 修改为私有变量
		class(*), pointer :: back => null()  !尾节点元素数值 修改为私有变量
	contains
		private
		!链表类对外接口：New,Delete,Begin,End,Size,Empty,=,Insert(增加方法),Erase(增加方法),PushFront,PopFront,PushBack,Resize,Clear
		!链表类新增对外接口：Get,Chang,FindIt,WriteList,GetFront,GetBack
		procedure :: NewDefault
		procedure :: NewCopyOther
		procedure :: NewFill
		procedure :: NewFromArray
		procedure :: NewFromftlSingleListIteratorPair
		generic , public :: New => NewDefault, NewCopyOther, NewFill, NewFromArray, NewFromftlSingleListIteratorPair !构造方法

		procedure, public :: Delete !消毁链表
		final :: Finalizer !析构方法

		procedure, public :: Begin => BeginList !返回迭代器 链表头节点位置 p%Begin()
		procedure, public :: End => EndList     !返回迭代器 链表哨兵节点 即尾节点位置+1 p%End()

		procedure :: SizeList
		generic , public :: Size => SizeList !链表元素个数 p%Size()

		procedure, public :: Empty !是否链表为空

		procedure :: AssignOther
		procedure :: AssignArray
		generic , public :: assignment(=) => AssignOther, AssignArray !链表赋值号重载
		procedure :: InsertSingle
		procedure :: InsertFill
		procedure :: InsertArray
		procedure :: InsertftlSingleListIteratorPair
		procedure :: InsertVal !新增方法 按照序号前插元素
		generic , public :: Insert => InsertSingle, InsertFill, InsertArray, InsertftlSingleListIteratorPair, InsertVal !只有InsertVal前插 其余为后插

		procedure :: EraseSingle
		procedure :: EraseftlSingleListIteratorPair
		procedure :: EraseVal !新增方法 按照序号删除元素
		generic , public :: Erase => EraseSingle, EraseftlSingleListIteratorPair, EraseVal !删除链表元素

		procedure, public :: PushFront !链表头部压入新元素
		procedure, public :: PopFront  !链表头部压出元素
		procedure, public :: PushBack  !链表尾部压入新元素

		procedure, public :: Resize !调整链表元素个数
		procedure, public :: Clear  !清除链表所有节点

		procedure :: FixValuePtrs   !确定链表的头值与尾值

		procedure, public :: WriteList !增加实例方法 打印链表
		procedure, public :: FindIt    !增加实例方法 返回第index个元素的迭代器
		procedure, public :: Get       !增加实例方法 按照序号获取元素
		procedure, public :: Change    !增加实例方法 按照序号改变元素
		procedure, public :: GetFront  !增加实例方法 返回头值
		procedure, public :: GetBack   !增加实例方法 返回尾值
	end type
	! 不绑定也能使用
	public :: Begin !Begin(self) 返回迭代器
	interface Begin
		module procedure BeginList
	end interface
	! 不绑定也能使用
	public :: End !End(self) 返回迭代器
	interface End
		module procedure EndList
	end interface
	! 不绑定也能使用
	public :: Size !Size(self)
	interface Size
		module procedure SizeList
	end interface
	! 交换链表
	public :: ftlSwap
	interface ftlSwap
		module procedure SwapList
	end interface
	! 移动链表
	public :: ftlMove
	interface ftlMove
		module procedure ftlMoveList
	end interface

	public :: WriteNode !增加模块方法 打印节点
	! ====== Type of an ftlSingleListIterator over a ftlSingleList container ============================================================================
	type, public :: ftlSingleListIterator !迭代器：节点+数据  相当于节点指针
		private

		class(ListNode) , pointer :: node => null() !类指针  可指向父类ListNode及其子类 即哨兵节点或数据节点
		class(*), pointer :: value => null()        !节点元素数值 修改为私有变量
	contains
		private
		!迭代器类对外接口：New,Inc,Dec,GetValue
		procedure :: NewItDefault
		procedure :: NewItCopyOther
		generic , public :: New => NewItDefault, NewItCopyOther !构造方法

		procedure, public :: Inc !下一个节点
		procedure, public :: Dec !上一个节点 只用于哨兵节点转到尾节点

		procedure, public :: GetValue !增加实例方法 返回迭代器对应节点元素数据
	end type

	public :: operator(==) !迭代器相等
	interface operator(==)
		module procedure EqualOther
	end interface

	public :: operator(/=) !迭代器不相等
	interface operator(/=)
		module procedure UnequalOther
	end interface

contains
! ====== Implementation of ftlSingleList methods =======================================================================================
subroutine NewDefault(self)
	class(ftlSingleList), intent(out), target :: self
	!链表初始化只有哨兵节点
	self%sentinel%next => self%sentinel !后指针指向哨兵节点自身
	self%sentinel%prev => self%sentinel !前指针指向哨兵节点自身
end subroutine
!
impure elemental subroutine NewCopyOther(self, other)
	class(ftlSingleList), intent(inout) :: self
	!type(ftlSingleList), intent(in) :: other
	class(ftlSingleList), intent(in) :: other !改动

	type(ftlSingleListIterator) :: it !迭代器

	call self%New()

	it = other%Begin() !头节点

	do while (it /= other%End())
		call self%PushBack(it%value) !尾部压入
		call it%Inc() !下一个节点
	enddo
end subroutine
!n个元素的初始值均设为val
subroutine NewFill(self, n, val)
	class(ftlSingleList), intent(inout) :: self
	integer , intent(in) :: n
	class(*) , intent(in), optional :: val

	integer :: i

	call self%New()

	if (present(val)) then
		do i = 1, n
			call self%PushBack(val) !尾部压入
		enddo
	else
		stop 'TODO: Implement ftlSingleList%NewFill without val'
	endif
end subroutine
!array必须为一维数组
subroutine NewFromArray(self, array)
	class(ftlSingleList), intent(inout) :: self
	class(*) , intent(in) :: array(:)
	integer :: i

	call self%New()

	do i = 1, size(array)
		call self%PushBack(array(i)) !尾部压入
	enddo
end subroutine
!由迭代器构造链表
subroutine NewFromftlSingleListIteratorPair(self, first, last)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator), intent(in) :: first !迭代器
	type(ftlSingleListIterator), intent(in) :: last  !迭代器

	call self%New()
	call self%Insert(self%Begin(), first, last)
end subroutine
!销毁链表
impure elemental subroutine Delete(self)
	class(ftlSingleList), intent(inout), target :: self
	class(ListNode), pointer :: walker, deletor

	walker => self%sentinel%next !头节点

	do while (associated(walker) .and. .not.associated(walker,self%sentinel))
		deletor => walker
		walker => walker%next
		deallocate(deletor) !删除该节点并释放其内存
	enddo

	self%psize = 0 !元素个数置0
	nullify(self%sentinel%prev) !前指针置空
	nullify(self%sentinel%next) !后指针置空
	nullify(self%front)
	nullify(self%back)
end subroutine
!析构方法
impure elemental subroutine Finalizer(self)
	type(ftlSingleList), intent(inout) :: self

	call self%Delete()
end subroutine
! =============> ftlSingleListIterators:
type(ftlSingleListIterator) function BeginList(self) result(Begin) !迭代器 链表的头节点
	class(ftlSingleList), intent(in), target :: self

	Begin%node => self%sentinel%next !头节点 迭代器也可以指向数据节点(子类DataNode)

	select type (node => Begin%node) !node为别名
		type is (DataNode) !数据节点
		Begin%value => node%data
	end select
end function

type(ftlSingleListIterator) function EndList(self) result(End) !迭代器 链表的节点结束标志（也是哨兵节点无data）
	class(ftlSingleList), intent(in), target :: self

	End%node => self%sentinel !类定义中class(ListNode)多态指针的原因 迭代器也可以指向哨兵节点(子类SLNode)
end function
! =============> Capacity:
pure integer function SizeList(self) result(Size) !链表的元素个数
	class(ftlSingleList), intent(in) :: self

	Size = self%psize
end function

pure logical function Empty(self) !链表是否为空
	class(ftlSingleList), intent(in) :: self

	Empty = (self%psize == 0)
end function
! =============> Modifiers:
! TODO: implement using existing list nodes instead of copy construction
!
impure elemental subroutine AssignOther(self, other) !链表赋值
	class(ftlSingleList), intent(inout) :: self
	!type(ftlSingleList), intent(in) :: other
	class(ftlSingleList), intent(in) :: other !改动

#if defined(NAGFOR)
	! For some reason NAGFOR needs a special implementation here. I suspect that this is a compiler bug. The normal
	! implementation, as in %NewCopyOther doesn't work, because the ftlSingleListIterator into other never becomes equal to other%End(),
	! causing an infinite loop until we run out of memory.
	! For some reason this seems to be related to this method being called through the assignment statement, because this doesn't
	! happen if %NewCopyOther is called directly, or %AssignOther is made public and called directly. I don't really understand
	! this, it's either a NAGFOR bug or I'm totally not understanding something about user defined assignment in Fortran ...
	type(ftlSingleListIterator) :: it !迭代器
	integer :: i

	call self%New()
	i = 1 !头节点从1开始
	it = other%Begin()

	do while (i <= other%Size())
		call self%PushBack(it%value)
		i = i + 1
		call it%Inc() !下一个节点
	enddo
#else
	call self%New(other)
#endif
end subroutine
!由一维数组赋值给链表
subroutine AssignArray(self, array)
	class(ftlSingleList), intent(inout) :: self
	class(*) , intent(in) :: array(:)

	call self%New(array)
end subroutine
!链表迭代器position位置之后插入1个元素,其值为val
subroutine InsertSingle(self, position, val)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: position !迭代器
	class(*) , intent(in) :: val

	call self%InsertFill(position, 1, val)
end subroutine
!链表迭代器position位置之后插入n个元素,其值均为val
subroutine InsertFill(self, position, n, val)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: position !迭代器
	integer , intent(in) :: n
	class(*) , intent(in) :: val
	integer :: i

	do i = 1, n
		call InsertNodeAfter(position%node, val) !后插 InsertNodeAfter第一个参数是多态节点类指针 修改
	enddo

	self%psize = self%psize + n !链表元素个数改变
	call self%FixValuePtrs() !确定链表的头值与尾值
end subroutine
!链表迭代器position位置之后插入数组array
subroutine InsertArray(self, position, array)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: position
	class(*) , intent(in) :: array(:)
	integer :: i

	do i = 1, size(array)
		call InsertNodeAfter(position%node, array(i)) !后插 InsertNodeAfter第一个参数是多态节点类指针 修改
	enddo

	self%psize = self%psize + size(array) !链表元素个数改变
	call self%FixValuePtrs() !确定链表的头值与尾值
end subroutine
!链表迭代器position位置之后插入迭代器 从first到last-1
subroutine InsertftlSingleListIteratorPair(self, position, first, last)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: position
	type(ftlSingleListIterator), intent(in) :: first
	type(ftlSingleListIterator), intent(in) :: last
	type(ftlSingleListIterator) :: it

	it = first

	do while (it /= last)
		call InsertNodeAfter(position%node, it%value) !后插 InsertNodeAfter第一个参数是多态节点类指针 修改
		self%psize = self%psize + 1 !链表元素个数改变
		call it%Inc() !下一个节点
	enddo

	call self%FixValuePtrs() !确定链表的头值与尾值
end subroutine
!头部压入
subroutine PushFront(self, val)
	class(ftlSingleList), intent(inout), target :: self
	class(*) , intent(in) :: val
	type(ftlSingleListIterator) :: it !新增

	it=self%End() !新增
	!注意不能直接在self%sentinel位置插入 因为它的类型为type(SLNode) 不是多态 所以迂回处理
	call InsertNodeAfter(it%node, val) !后插 InsertNodeAfter第一个参数是多态节点类指针 修改
	self%psize = self%psize + 1 !链表元素个数改变
	call self%FixValuePtrs() !确定链表的头值与尾值
end subroutine
!头部压出
function PopFront(self) result(Res)
	class(ftlSingleList), intent(inout), target :: self
	class(ListNode), pointer :: oldfirst
	class(*), allocatable :: Res

	oldfirst => self%sentinel%next !头节点（第一个元素对应的节点）
#ifdef FTL_TEMPLATE_TYPE_PROVIDES_FTLMOVE
	call ftlMove(self%front, Res) !适用于派生类型
#else
	Res = self%front !适用于自带类型
#endif
	self%psize = self%psize - 1 !链表元素个数改变
	self%sentinel%next => oldfirst%next !新增
	deallocate(oldfirst) !释放该节点内存
	call self%FixValuePtrs() !确定链表的头值与尾值
end function
!尾部压入
subroutine PushBack(self, val)
	class(ftlSingleList), intent(inout) :: self
	class(*) , intent(in) :: val

  call InsertNodeAfter(self%sentinel%prev, val) !后插 InsertNodeAfter第一个参数是多态节点类指针
	self%psize = self%psize + 1 !链表元素个数改变
	call self%FixValuePtrs() !确定链表的头值与尾值
end subroutine
!删除迭代器position下一节点
subroutine EraseSingle(self, position)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: position
	class(ListNode), pointer :: temp !新增

	temp => position%node%next !新增 要被删除的节点
	position%node%next => temp%next !新增 断开被删除的节点

	if(associated(temp,self%sentinel%prev))self%sentinel%prev => position%node !temp为尾节点 新增
	deallocate(temp) !释放该节点内存 新增
	self%psize = self%psize - 1 !链表元素个数改变
	call self%FixValuePtrs() !确定链表的头值与尾值
end subroutine
!
subroutine EraseftlSingleListIteratorPair(self, first, last) !从first+1到last-1删除元素
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator), intent(in) :: first !修改
	type(ftlSingleListIterator), intent(in) :: last
	type(ftlSingleListIterator) :: walker !修改

	walker = first !新增
	call walker%Inc() !下一个节点 新增  这里处理与双向链表不同

	do while (walker /= last)
		call walker%Inc() !下一个节点
		call self%EraseSingle(first) !总是删除first的下一节点
	enddo
end subroutine
!交换链表 模块方法
subroutine SwapList(self, other)
	type(ftlSingleList), intent(inout), target :: self
	type(ftlSingleList), intent(inout), target :: other
	integer :: tmpSize
	type(SLNode) :: tmpNode !修改

	! fix pointers from data nodes to the sentinels
	self%sentinel%prev%next => other%sentinel
	other%sentinel%prev%next => self%sentinel
	! exchange sentinels themselves
	!交换哨兵节点和元素个数
	tmpNode = self%sentinel
	tmpSize = self%psize

	self%sentinel = other%sentinel
	self%psize = other%psize

	other%sentinel = tmpNode
	other%psize = tmpSize

	! fix front/back pointers for both lists
	call self%FixValuePtrs()  !确定链表的头值与尾值
	call other%FixValuePtrs() !确定链表的头值与尾值
end subroutine

subroutine Resize(self, n, val)
	class(ftlSingleList), intent(inout) :: self
	integer , intent(in) :: n
	class(*) , intent(in) , optional :: val
	type(ftlSingleListIterator) :: it
	integer :: i

	if (n == self%psize) then
		return !子程序不进行任何操作
	else if (n < self%psize) then
		it = self%Begin() !头节点 i=1

		do i = 2, n
			call it%Inc() !下一个节点
		enddo
		!此时it位置为n
		!这里删除了一句
		call self%Erase(it,self%End()) !删除n+1到尾节点的多余尾巴
	else ! n > self%psize
		do i = 1, n - self%psize
			call InsertNodeAfter(self%sentinel%prev, val) !后插 （尾节点之后）补充不足部分 修改
		enddo
	endif

	self%psize = n !链表元素个数改变
	call self%FixValuePtrs() !确定链表的头值与尾值
end subroutine
!清除所有节点
subroutine Clear(self)
	class(ftlSingleList), intent(inout) :: self

	call self%New()
end subroutine
! =============> FTL methods:
subroutine ftlMoveList(src, dest) !src旧毁,dest新生
	type(ftlSingleList), intent(inout) :: src
	type(ftlSingleList), intent(out) , target :: dest

	dest%psize = src%psize
	dest%sentinel = src%sentinel
	!这里删除了一句
	if (associated(dest%sentinel%prev)) dest%sentinel%prev%next => dest%sentinel
	call dest%FixValuePtrs() !确定链表的头值与尾值
	nullify(src%sentinel%prev)
	nullify(src%sentinel%next)
	nullify(src%front)
	nullify(src%back)
	src%psize = 0
end subroutine
! =============> Internal methods:
subroutine InsertNodeAfter(afternode, val) !在afternode位置后插入元素
	class(ListNode), pointer, intent(inout) :: afternode !注意是节点类多态指针
	class(*) , intent(in) , optional :: val
	class(ListNode), pointer :: oldnext, newnext

	oldnext => afternode%next !下一个节点
	allocate(DataNode::afternode%next) !新数据节点分配子类内存
	newnext => afternode%next
	!这里删除了一句
	newnext%next => oldnext
	!若afternode为尾节点 则oldnext为哨兵节点
	select type(oldnext) !新增 本程序关键
		type is(SLNode) !哨兵节点
			oldnext%prev => newnext !只有哨兵节点有前指针
	end select

	if (present(val)) then
		select type (newnext)
			type is (DataNode) ! always true
			newnext%data = val
		end select
	endif
end subroutine

subroutine FixValuePtrs(self)
	class(ftlSingleList), intent(inout) :: self

	if (self%psize == 0) then !空链表
		nullify(self%front,self%back)
	else !非空链表
		select type (first => self%sentinel%next) !头节点 数据节点
			type is (DataNode)
			self%front => first%data
		end select

		select type (last => self%sentinel%prev) !尾节点 数据节点
			type is (DataNode)
			self%back => last%data
		end select
	endif
end subroutine
! ====== Implementation of ftlSingleListIterator methods ===============================================================================
subroutine NewItDefault(self)
	class(ftlSingleListIterator), intent(out) :: self
	! Nothing to do here: intent(out) already resets everything
end subroutine
!
subroutine NewItCopyOther(self, other)
	class(ftlSingleListIterator), intent(out) :: self
	class(ftlSingleListIterator), intent(in) :: other

	self%node => other%node

	select type (node => self%node) !node为别名
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
! =============> Arithmetic operations:
subroutine Inc(self) !下一个节点
	class(ftlSingleListIterator), intent(inout) :: self

	self%node => self%node%next !下一个节点

	select type (node => self%node) !node为别名
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
!
subroutine Dec(self) !上一个节点 只用于哨兵节点转到尾节点
	class(ftlSingleListIterator), intent(inout) :: self

	!self%node => self%node%prev !Error: 'prev' at (1) is not a member of the 'listnode' structure
	select type(p => self%node)
		type is(SLNode) !哨兵节点
		!p => p%prev !上一个节点 这样会报错
		!Error: Different types in pointer assignment at (1); attempted assignment of CLASS(listnode) to TYPE(slnode)
		self%node => p%prev !上一个节点
	end select

	select type (node => self%node) !node为别名
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
! =============> Logical operations:
pure logical function EqualOther(self, other) !迭代器相等
	class(ftlSingleListIterator), intent(in) :: self
	class(ftlSingleListIterator), intent(in) :: other

	EqualOther = associated(self%node,other%node) !指针关联
end function
!
pure logical function UnequalOther(self, other) !迭代器不相等
	class(ftlSingleListIterator), intent(in) :: self
	class(ftlSingleListIterator), intent(in) :: other

	UnequalOther = .not.associated(self%node,other%node) !指针不关联
end function
!打印链表所有元素值
recursive subroutine WriteList(self, unit)
	class(ftlSingleList), intent(in) :: self
	integer, intent(in), optional :: unit !输出文件号 默认为屏幕
	type(ftlSingleListIterator) :: it

	integer :: lunit !实际输出文件号
	integer :: i

	lunit = OUTPUT_UNIT !默认输出为屏幕
	if(present(unit)) lunit = unit

	write(lunit,'(a)',advance='no') '[' !不换行

	it=self%Begin() !首元素位置
	i=0 !节点计数

	do while(it/=self%End()) !不是哨兵节点
		i=i+1
		call WriteNode(it%value,unit=lunit,IsNewLine=.false.) !不换行
		!尾结点不需要间隔逗号
		if(i<self%psize) write(lunit,'(a)',advance='no') ',' !元素间隔为逗号
		call it%Inc() !下一节点
	end do

	write(lunit,'(a)',advance='no') ']' !不换行
	write(lunit,*) !换行
end subroutine WriteList
!查找索引元素值 只能用call WriteNode(o%Get(7))显示
function Get(self, index) result(value)
	class(ftlSingleList), intent(in) :: self
	integer, intent(in) :: index  !位置序号
	type(ftlSingleListIterator) :: it
	class(*), allocatable :: value

	it=self%FindIt(index) !第index个元素的迭代器 修改
	value = it%value !拷贝元素值
end function Get
!更改索引元素值
subroutine Change(self, index, value)
	class(ftlSingleList), intent(inout) :: self
	integer, intent(in) :: index
	class(*), intent(in) :: value
	type(ftlSingleListIterator) :: it

	it=self%FindIt(index) !第index个元素的迭代器 修改

	select type (node => it%node) !node为别名
		type is (DataNode) ! always true
		node%data = value
	end select

	call self%FixValuePtrs() !确定链表的头值与尾值
end subroutine Change
!索引位置前插入新元素
subroutine InsertVal(self, index, value)
	class(ftlSingleList), intent(inout) :: self
	integer, intent(in) :: index !位置序号
	class(*), intent(in) :: value
	type(ftlSingleListIterator) :: it

	it=self%FindIt(index,.true.) !第index个元素前一个位置的迭代器
	call self%InsertFill(it, 1, value)
end subroutine InsertVal
!索引位置删除元素
subroutine EraseVal(self, index)
	class(ftlSingleList), intent(inout) :: self
	integer, intent(in) :: index !位置序号
	type(ftlSingleListIterator) :: it

	it=self%FindIt(index,.true.) !第index个元素前一个位置的迭代器
	call self%EraseSingle(it)
end subroutine EraseVal
!返回第index个元素前一个位置的迭代器 self%FindIt(2)等价于self%Begin()
type(ftlSingleListIterator) function FindIt(self, index, IsBefore)  !迭代器
	class(ftlSingleList), intent(in) :: self
	integer, intent(in) :: index !位置序号
	logical, optional, intent(in) :: IsBefore !真为第index个元素前一位置 假为当前位置 新增
	integer :: i

	if (index>self%psize .or. index<1) then
		stop "Index is not in range [1,psize]."
	end if
	!只能从头开始搜索
	FindIt=self%End()   !哨兵节点 修改 函数返回前一位置

	if(present(IsBefore) .and. IsBefore)then !函数返回前一位置

	else !函数返回当前位置
		if(index==self%psize)then !尾节点
			call FindIt%Dec() !上一节点 哨兵节点转到尾节点
			return
		end if

		call FindIt%Inc() !下一节点即头节点 修改 函数返回当前位置
	end if

	do i=1,index-1 !循环index-1次
		call FindIt%Inc() !下一节点
	end do
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
		type is(integer)
			write(lunit,'(i0)',advance='no') v_p
		type is(real) !fmt=*与advance='no'不能同时出现
			write(string,fmt=*) v_p
			write(lunit,'(a)',advance='no') trim(adjustl(string))
		type is(real(kind(0d0))) !双精度
			write(string,fmt=*) v_p
			write(lunit,'(a)',advance='no') trim(adjustl(string))
		type is(complex)
			write(string,fmt=*) v_p !复数转换为字符串
			write(lunit,'(a)',advance='no') trim(adjustl(string)) !删除首尾空格
		type is(complex(kind(0d0)))
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
		type is(ftlSingleList) !元素是另一个链表
			call v_p%WriteList(lunit)
		class default !未知类型
			write(lunit,'(a)',advance='no') '*'
	end select

	if(present(IsNewLine).and.(.not.IsNewLine))then
		!不换行
	else
		write(lunit,*) !换行
	end if
end subroutine WriteNode
!增加实例方法 返回头值
pure function GetFront(self) result(value)
	class(ftlSingleList), intent(in) :: self
	class(*), allocatable :: value

	if(associated(self%front))value=self%front
end function GetFront
!增加实例方法 返回尾值
pure function GetBack(self) result(value)
	class(ftlSingleList), intent(in) :: self
	class(*), allocatable :: value

	if(associated(self%back))value=self%back
end function GetBack
!增加实例方法 返回迭代器对应节点元素数据
pure function GetValue(self) result(value)
	class(ftlSingleListIterator), intent(in) :: self
	class(*), allocatable :: value

	if(associated(self%value))value=self%value
end function GetValue
end module