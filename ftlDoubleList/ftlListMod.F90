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

module ftlListMod
	use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
	implicit none
	!ģ�����ӿڰ���Begin,End,Size,ftlSwap,ftlMove,operator(==),operator(/=),WriteNode(���ӷ���)
	private
	! ====== Type of the ftlList container itself ====================================================================================
	type :: ListNode !���� ˫�������ڵ�  �ڱ��ڵ�����ݽڵ�
		class(ListNode), pointer :: prev => null() !ǰָ�� ��ָ�� ��ָ����ListNode��������
		class(ListNode), pointer :: next => null() !��ָ�� ��ָ��
	end type

	type, extends(ListNode) :: DataNode !˫�������ڵ������ ���ݽڵ� ���ӳ���InsertNodeBefore��InsertNodeAfter��ʹ��
		class(*), allocatable :: data !�ο�FCCF���� ������ݽڵ�����޶�̬����
	end type
	!                                End             Begin                                            End
	!��״˫�������Ľڵ�����˳��Ϊ���ڱ��ڵ�(��Ԫ��)��ͷ�ڵ�(��һ��Ԫ��)��...��β�ڵ�(���һ��Ԫ��)���ڱ��ڵ�(��Ԫ��)��...
	type, public :: ftlList !��״˫������
		private

		integer :: psize = 0 !������Ԫ�ظ��� ˽�б���
		type(ListNode) :: sentinel !���� �ڱ��ڵ㣨��data���������ݣ� ˽�б���
		class(*), pointer :: front => null() !ͷ�ڵ�Ԫ����ֵ �޸�Ϊ˽�б���
		class(*), pointer :: back => null()  !β�ڵ�Ԫ����ֵ �޸�Ϊ˽�б���
	contains
		private
		!���������ӿڣ�New,Delete,Begin,End,Size,Empty,=,Insert(���ӷ���),Erase(���ӷ���),PushFront,PopFront,PushBack,PopBack,Resize,Clear
		!��������������ӿڣ�Get,Chang,FindIt,WriteList,GetFront,GetBack
		procedure :: NewDefault
		procedure :: NewCopyOther
		procedure :: NewFill
		procedure :: NewFromArray
		procedure :: NewFromftlListIteratorPair
		generic , public :: New => NewDefault, NewCopyOther, NewFill, NewFromArray, NewFromftlListIteratorPair !���췽��

		procedure, public :: Delete !��������
		final :: Finalizer !��������

		procedure, public :: Begin => BeginList !���ص����� ����ͷ�ڵ�λ�� p%Begin()
		procedure, public :: End => EndList     !���ص����� �����ڱ��ڵ� ��β�ڵ�λ��+1 p%End()

		procedure :: SizeList
		generic , public :: Size => SizeList !����Ԫ�ظ��� p%Size()

		procedure, public :: Empty !�Ƿ�����Ϊ��

		procedure :: AssignOther
		procedure :: AssignArray
		generic , public :: assignment(=) => AssignOther, AssignArray !������ֵ������
		procedure :: InsertSingle
		procedure :: InsertFill
		procedure :: InsertArray
		procedure :: InsertftlListIteratorPair
		procedure :: InsertVal !�������� �������ǰ��Ԫ��
		generic , public :: Insert => InsertSingle, InsertFill, InsertArray, InsertftlListIteratorPair, InsertVal !��ǰ��������Ԫ��

		procedure :: EraseSingle
		procedure :: EraseftlListIteratorPair
		procedure :: EraseVal !�������� �������ɾ��Ԫ��
		generic , public :: Erase => EraseSingle, EraseftlListIteratorPair, EraseVal !ɾ������Ԫ��

		procedure, public :: PushFront !����ͷ��ѹ����Ԫ��
		procedure, public :: PopFront  !����ͷ��ѹ��Ԫ��
		procedure, public :: PushBack  !����β��ѹ����Ԫ��
		procedure, public :: PopBack   !����β��ѹ��Ԫ��

		procedure, public :: Resize !��������Ԫ�ظ���
		procedure, public :: Clear  !����������нڵ�

		procedure :: FixValuePtrs   !ȷ��������ͷֵ��βֵ

		procedure, public :: WriteList !����ʵ������ ��ӡ����
		procedure, public :: FindIt    !����ʵ������ ���ص�index��Ԫ�صĵ�����
		procedure, public :: Get       !����ʵ������ ������Ż�ȡԪ��
		procedure, public :: Change    !����ʵ������ ������Ÿı�Ԫ��
		procedure, public :: GetFront  !����ʵ������ ����ͷֵ
		procedure, public :: GetBack   !����ʵ������ ����βֵ
	end type
	! ����Ҳ��ʹ��
	public :: Begin !Begin(self) ���ص�����
	interface Begin
		module procedure BeginList
	end interface
	! ����Ҳ��ʹ��
	public :: End !End(self) ���ص�����
	interface End
		module procedure EndList
	end interface
	! ����Ҳ��ʹ��
	public :: Size !Size(self)
	interface Size
		module procedure SizeList
	end interface
	! ��������
	public :: ftlSwap
	interface ftlSwap
		module procedure SwapList
	end interface
	! �ƶ�����
	public :: ftlMove
	interface ftlMove
		module procedure ftlMoveList
	end interface

	public :: WriteNode !����ģ�鷽�� ��ӡ�ڵ�
	! ====== Type of an ftlListIterator over a ftlList container ============================================================================
	type, public :: ftlListIterator !���������ڵ�+����  �൱�ڽڵ�ָ��
		private

		class(ListNode) , pointer :: node => null() !��ָ��  ��ָ����ListNode�������� ���ڱ��ڵ�����ݽڵ�
		class(*), pointer :: value => null()        !�ڵ�Ԫ����ֵ �޸�Ϊ˽�б���
	contains
		private
		!�����������ӿڣ�New,Inc,Dec,GetValue
		procedure :: NewItDefault
		procedure :: NewItCopyOther
		generic , public :: New => NewItDefault, NewItCopyOther !���췽��

		procedure, public :: Inc !��һ���ڵ�
		procedure, public :: Dec !��һ���ڵ�

		procedure, public :: GetValue !����ʵ������ ���ص�������Ӧ�ڵ�Ԫ������
	end type

	public :: operator(==) !���������
	interface operator(==)
		module procedure EqualOther
	end interface

	public :: operator(/=) !�����������
	interface operator(/=)
		module procedure UnequalOther
	end interface

contains
! ====== Implementation of ftlList methods =======================================================================================
subroutine NewDefault(self)
	class(ftlList), intent(out), target :: self
	!������ʼ��ֻ���ڱ��ڵ�
	self%sentinel%next => self%sentinel !��ָ��ָ���ڱ��ڵ�����
	self%sentinel%prev => self%sentinel !ǰָ��ָ���ڱ��ڵ�����
end subroutine
!
impure elemental subroutine NewCopyOther(self, other)
	class(ftlList), intent(inout) :: self
	!type(ftlList), intent(in) :: other
	class(ftlList), intent(in) :: other !�Ķ�

	type(ftlListIterator) :: it !������

	call self%New()

	it = other%Begin() !ͷ�ڵ�

	do while (it /= other%End())
		call self%PushBack(it%value) !β��ѹ��
		call it%Inc() !��һ���ڵ�
	enddo
end subroutine
!n��Ԫ�صĳ�ʼֵ����Ϊval
subroutine NewFill(self, n, val)
	class(ftlList), intent(inout) :: self
	integer , intent(in) :: n
	class(*) , intent(in), optional :: val

	integer :: i

	call self%New()

	if (present(val)) then
		do i = 1, n
			call self%PushBack(val) !β��ѹ��
		enddo
	else
		stop 'TODO: Implement ftlList%NewFill without val'
	endif
end subroutine
!array����Ϊһά����
subroutine NewFromArray(self, array)
	class(ftlList), intent(inout) :: self
	class(*) , intent(in) :: array(:)
	integer :: i

	call self%New()

	do i = 1, size(array)
		call self%PushBack(array(i)) !β��ѹ��
	enddo
end subroutine
!�ɵ�������������
subroutine NewFromftlListIteratorPair(self, first, last)
	class(ftlList) , intent(inout) :: self
	type(ftlListIterator), intent(in) :: first !������
	type(ftlListIterator), intent(in) :: last  !������

	call self%New()
	call self%Insert(self%Begin(), first, last)
end subroutine
!��������
impure elemental subroutine Delete(self)
	class(ftlList), intent(inout), target :: self
	class(ListNode), pointer :: walker, deletor

	walker => self%sentinel%next !ͷ�ڵ�

	do while (associated(walker) .and. .not.associated(walker,self%sentinel))
		deletor => walker
		walker => walker%next
		deallocate(deletor) !ɾ���ýڵ㲢�ͷ����ڴ�
	enddo

	self%psize = 0 !Ԫ�ظ�����0
	nullify(self%sentinel%prev) !ǰָ���ÿ�
	nullify(self%sentinel%next) !��ָ���ÿ�
	nullify(self%front)
	nullify(self%back)
end subroutine
!��������
impure elemental subroutine Finalizer(self)
	type(ftlList), intent(inout) :: self

	call self%Delete()
end subroutine
! =============> ftlListIterators:
type(ftlListIterator) function BeginList(self) result(Begin) !������ ������ͷ�ڵ�
	class(ftlList), intent(in), target :: self

	Begin%node => self%sentinel%next !ͷ�ڵ� ������Ҳ����ָ�����ݽڵ�(����DataNode)

	select type (node => Begin%node) !nodeΪ����
		type is (DataNode) !���ݽڵ�
		Begin%value => node%data
	end select
end function

type(ftlListIterator) function EndList(self) result(End) !������ �����Ľڵ������־��Ҳ���ڱ��ڵ���data��
	class(ftlList), intent(in), target :: self

	End%node => self%sentinel !�ඨ����class(ListNode)��ָ̬���ԭ�� ������Ҳ����ָ���ڱ��ڵ�(����ListNode)
end function
! =============> Capacity:
pure integer function SizeList(self) result(Size) !������Ԫ�ظ���
	class(ftlList), intent(in) :: self

	Size = self%psize
end function

pure logical function Empty(self) !�����Ƿ�Ϊ��
	class(ftlList), intent(in) :: self

	Empty = (self%psize == 0)
end function
! =============> Modifiers:
! TODO: implement using existing list nodes instead of copy construction
!
impure elemental subroutine AssignOther(self, other) !������ֵ
	class(ftlList), intent(inout) :: self
	!type(ftlList), intent(in) :: other
	class(ftlList), intent(in) :: other !�Ķ�

#if defined(NAGFOR)
	! For some reason NAGFOR needs a special implementation here. I suspect that this is a compiler bug. The normal
	! implementation, as in %NewCopyOther doesn't work, because the ftlListIterator into other never becomes equal to other%End(),
	! causing an infinite loop until we run out of memory.
	! For some reason this seems to be related to this method being called through the assignment statement, because this doesn't
	! happen if %NewCopyOther is called directly, or %AssignOther is made public and called directly. I don't really understand
	! this, it's either a NAGFOR bug or I'm totally not understanding something about user defined assignment in Fortran ...
	type(ftlListIterator) :: it !������
	integer :: i

	call self%New()
	i = 1 !ͷ�ڵ��1��ʼ
	it = other%Begin()

	do while (i <= other%Size())
		call self%PushBack(it%value)
		i = i + 1
		call it%Inc() !��һ���ڵ�
	enddo
#else
	call self%New(other)
#endif
end subroutine
!��һά���鸳ֵ������
subroutine AssignArray(self, array)
	class(ftlList), intent(inout) :: self
	class(*) , intent(in) :: array(:)

	call self%New(array)
end subroutine
!����������positionλ��֮ǰ����1��Ԫ��,��ֵΪval
subroutine InsertSingle(self, position, val)
	class(ftlList) , intent(inout) :: self
	type(ftlListIterator) :: position !������
	class(*) , intent(in) :: val

	call self%InsertFill(position, 1, val)
end subroutine
!����������positionλ��֮ǰ����n��Ԫ��,��ֵ��Ϊval
subroutine InsertFill(self, position, n, val)
	class(ftlList) , intent(inout) :: self
	type(ftlListIterator) :: position !������
	integer , intent(in) :: n
	class(*) , intent(in) :: val
	integer :: i

	do i = 1, n
		call InsertNodeBefore(position%node, val) !ǰ�� InsertNodeBefore��һ�������Ƕ�̬�ڵ���ָ��
	enddo

	self%psize = self%psize + n !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!����������positionλ��֮ǰ��������array
subroutine InsertArray(self, position, array)
	class(ftlList) , intent(inout) :: self
	type(ftlListIterator) :: position
	class(*) , intent(in) :: array(:)
	integer :: i

	do i = 1, size(array)
		call InsertNodeBefore(position%node, array(i)) !ǰ�� InsertNodeBefore��һ�������Ƕ�̬�ڵ���ָ��
	enddo

	self%psize = self%psize + size(array) !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!����������positionλ��֮ǰ��������� ��first��last-1
subroutine InsertftlListIteratorPair(self, position, first, last)
	class(ftlList) , intent(inout) :: self
	type(ftlListIterator) :: position
	type(ftlListIterator), intent(in) :: first
	type(ftlListIterator), intent(in) :: last
	type(ftlListIterator) :: it

	it = first

	do while (it /= last)
		call InsertNodeBefore(position%node, it%value) !ǰ�� InsertNodeBefore��һ�������Ƕ�̬�ڵ���ָ��
		self%psize = self%psize + 1 !����Ԫ�ظ����ı�
		call it%Inc() !��һ���ڵ�
	enddo

	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!ͷ��ѹ��
subroutine PushFront(self, val)
	class(ftlList), intent(inout), target :: self
	class(*) , intent(in) :: val

	call InsertNodeBefore(self%sentinel%next, val) !ǰ�� InsertNodeBefore��һ�������Ƕ�̬�ڵ���ָ��
	self%psize = self%psize + 1 !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!ͷ��ѹ��
function PopFront(self) result(Res)
	class(ftlList), intent(inout), target :: self
	class(ListNode), pointer :: oldfirst
	class(*), allocatable :: Res

	oldfirst => self%sentinel%next !ͷ�ڵ㣨��һ��Ԫ�ض�Ӧ�Ľڵ㣩
#ifdef FTL_TEMPLATE_TYPE_PROVIDES_FTLMOVE
	call ftlMove(self%front, Res) !��������������
#else
	Res = self%front !�������Դ�����
#endif
	self%psize = self%psize - 1 !����Ԫ�ظ����ı�
	call UnlinkNode(oldfirst) !��״˫��������ɾ���ýڵ�
	deallocate(oldfirst) !�ͷŸýڵ��ڴ�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end function
!β��ѹ��
subroutine PushBack(self, val)
	class(ftlList), intent(inout) :: self
	class(*) , intent(in) :: val

	call InsertNodeAfter(self%sentinel%prev, val) !��� InsertNodeAfter��һ�������Ƕ�̬�ڵ���ָ��
	self%psize = self%psize + 1 !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!β��ѹ��
function PopBack(self) result(Res)
	class(ftlList), intent(inout), target :: self
	class(ListNode), pointer :: oldlast
	class(*), allocatable :: Res

	oldlast => self%sentinel%prev !β�ڵ㣨���һ��Ԫ�ض�Ӧ�Ľڵ㣩
#ifdef FTL_TEMPLATE_TYPE_PROVIDES_FTLMOVE
	call ftlMove(self%back, Res) !��������������
#else
	Res = self%back !�������Դ�����
#endif
	self%psize = self%psize - 1 !����Ԫ�ظ����ı�
	call UnlinkNode(oldlast) !��״˫��������ɾ���ýڵ�
	deallocate(oldlast) !�ͷŸýڵ��ڴ�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end function
!ɾ��������position�ڵ�
subroutine EraseSingle(self, position)
	class(ftlList) , intent(inout) :: self
	type(ftlListIterator) :: position

	call UnlinkNode(position%node) !��״˫��������ɾ���ýڵ�
	deallocate(position%node) !�ͷŸýڵ��ڴ�
	self%psize = self%psize - 1 !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!
subroutine EraseftlListIteratorPair(self, first, last) !��first��last-1ɾ��Ԫ��
	class(ftlList) , intent(inout) :: self
	type(ftlListIterator) :: first !ע����ý�����first�Ѹı�
	type(ftlListIterator), intent(in) :: last
	type(ftlListIterator) :: deletor

	associate(walker => first) !walker��first�ı���
		do while (walker /= last)
			deletor = walker
			call walker%Inc() !��һ���ڵ�
			call self%EraseSingle(deletor) !ɾ��deletor�ڵ�
		enddo
	end associate
end subroutine
!�������� ģ�鷽��
subroutine SwapList(self, other)
	type(ftlList), intent(inout), target :: self
	type(ftlList), intent(inout), target :: other
	integer :: tmpSize
	type(ListNode) :: tmpNode

	! fix pointers from data nodes to the sentinels

	self%sentinel%prev%next => other%sentinel
	self%sentinel%next%prev => other%sentinel

	other%sentinel%prev%next => self%sentinel
	other%sentinel%next%prev => self%sentinel

	! exchange sentinels themselves
	!�����ڱ��ڵ��Ԫ�ظ���
	tmpNode = self%sentinel
	tmpSize = self%psize

	self%sentinel = other%sentinel
	self%psize = other%psize

	other%sentinel = tmpNode
	other%psize = tmpSize

	! fix front/back pointers for both lists

	call self%FixValuePtrs()  !ȷ��������ͷֵ��βֵ
	call other%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine

subroutine Resize(self, n, val)
	class(ftlList), intent(inout) :: self
	integer , intent(in) :: n
	class(*) , intent(in) , optional :: val
	type(ftlListIterator) :: it
	integer :: i

	if (n == self%psize) then
		return !�ӳ��򲻽����κβ���
	else if (n < self%psize) then
		it = self%Begin() !ͷ�ڵ� i=1

		do i = 2, n
			call it%Inc() !��һ���ڵ�
		enddo
		!��ʱλ��Ϊn
		call it%Inc() !��һ���ڵ� ��ʱλ��Ϊn+1
		call self%Erase(it,self%End()) !ɾ������β��
	else ! n > self%psize
		do i = 1, n - self%psize
			call InsertNodeAfter(self%sentinel%prev, val) !��� ��β�ڵ�֮�󣩲��䲻�㲿��
		enddo
	endif

	self%psize = n !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!������нڵ�
subroutine Clear(self)
	class(ftlList), intent(inout) :: self

	call self%New()
end subroutine
! =============> FTL methods:
subroutine ftlMoveList(src, dest) !src�ɻ�,dest����
	type(ftlList), intent(inout) :: src
	type(ftlList), intent(out) , target :: dest

	dest%psize = src%psize
	dest%sentinel = src%sentinel
	if (associated(dest%sentinel%next)) dest%sentinel%next%prev => dest%sentinel
	if (associated(dest%sentinel%prev)) dest%sentinel%prev%next => dest%sentinel
	call dest%FixValuePtrs() !ȷ��������ͷֵ��βֵ
	nullify(src%sentinel%prev)
	nullify(src%sentinel%next)
	nullify(src%front)
	nullify(src%back)
	src%psize = 0
end subroutine
! =============> Internal methods:
subroutine InsertNodeBefore(beforenode, val) !��beforenodeλ��ǰ����Ԫ��
	class(ListNode), pointer, intent(inout) :: beforenode !ע���ǽڵ����ָ̬��
	class(*) , intent(in) , optional :: val
	class(ListNode), pointer :: oldprev, newprev

	oldprev => beforenode%prev !��һ���ڵ�
	allocate(DataNode::beforenode%prev) !�����ݽڵ���������ڴ�
	newprev => beforenode%prev
	newprev%next => beforenode
	newprev%prev => oldprev
	oldprev%next => newprev

	if (present(val)) then
		select type (newprev)
			type is (DataNode) ! always true
			newprev%data = val
		end select
	endif
end subroutine

subroutine InsertNodeAfter(afternode, val) !��afternodeλ�ú����Ԫ��
	class(ListNode), pointer, intent(inout) :: afternode !ע���ǽڵ����ָ̬��
	class(*) , intent(in) , optional :: val
	class(ListNode), pointer :: oldnext, newnext

	oldnext => afternode%next !��һ���ڵ�
	allocate(DataNode::afternode%next) !�����ݽڵ���������ڴ�
	newnext => afternode%next
	newnext%prev => afternode
	newnext%next => oldnext
	oldnext%prev => newnext

	if (present(val)) then
		select type (newnext)
			type is (DataNode) ! always true
			newnext%data = val
		end select
	endif
end subroutine
!��״˫�������жϿ��ýڵ� �����ͷ��ڴ�
subroutine UnlinkNode(node)
	class(ListNode), intent(inout) :: node !ע���ǽڵ����ָ̬��

	node%next%prev => node%prev
	node%prev%next => node%next
end subroutine
!ȷ��������ͷֵ��βֵ
subroutine FixValuePtrs(self)
	class(ftlList), intent(inout) :: self

	if (self%psize == 0) then !������
		nullify(self%front,self%back)
	else !�ǿ�����
		select type (first => self%sentinel%next) !ͷ�ڵ� ���ݽڵ�
			type is (DataNode)
			self%front => first%data
		end select

		select type (last => self%sentinel%prev) !β�ڵ� ���ݽڵ�
			type is (DataNode)
			self%back => last%data
		end select
	endif
end subroutine
! ====== Implementation of ftlListIterator methods ===============================================================================
subroutine NewItDefault(self)
	class(ftlListIterator), intent(out) :: self
	! Nothing to do here: intent(out) already resets everything
end subroutine
!
subroutine NewItCopyOther(self, other)
	class(ftlListIterator), intent(out) :: self
	class(ftlListIterator), intent(in) :: other

	self%node => other%node

	select type (node => self%node) !nodeΪ����
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
! =============> Arithmetic operations:
subroutine Inc(self) !��һ���ڵ�
	class(ftlListIterator), intent(inout) :: self

	self%node => self%node%next !��һ���ڵ�

	select type (node => self%node) !nodeΪ����
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
!
subroutine Dec(self) !��һ���ڵ�
	class(ftlListIterator), intent(inout) :: self

	self%node => self%node%prev !��һ���ڵ�

	select type (node => self%node) !nodeΪ����
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
! =============> Logical operations:
pure logical function EqualOther(self, other) !���������
	class(ftlListIterator), intent(in) :: self
	class(ftlListIterator), intent(in) :: other

	EqualOther = associated(self%node,other%node) !ָ�����
end function
!
pure logical function UnequalOther(self, other) !�����������
	class(ftlListIterator), intent(in) :: self
	class(ftlListIterator), intent(in) :: other

	UnequalOther = .not.associated(self%node,other%node) !ָ�벻����
end function
!��ӡ��������Ԫ��ֵ
recursive subroutine WriteList(self, unit)
	class(ftlList), intent(in) :: self
	integer, intent(in), optional :: unit !����ļ��� Ĭ��Ϊ��Ļ
	type(ftlListIterator) :: it

	integer :: lunit !ʵ������ļ���
	integer :: i

	lunit = OUTPUT_UNIT !Ĭ�����Ϊ��Ļ
	if(present(unit)) lunit = unit

	write(lunit,'(a)',advance='no') '[' !������

	it=self%Begin() !��Ԫ��λ��
	i=0 !�ڵ����

	do while(it/=self%End()) !�����ڱ��ڵ�
		i=i+1
		call WriteNode(it%value,unit=lunit,IsNewLine=.false.) !������
		!β��㲻��Ҫ�������
		if(i<self%psize) write(lunit,'(a)',advance='no') ',' !Ԫ�ؼ��Ϊ����
		call it%Inc() !��һ�ڵ�
	end do

	write(lunit,'(a)',advance='no') ']' !������
	write(lunit,*) !����
end subroutine WriteList
!��������Ԫ��ֵ ֻ����call WriteNode(o%Get(7))��ʾ
function Get(self, index) result(value)
	class(ftlList), intent(in) :: self
	integer, intent(in)   :: index  !λ�����
	type(ftlListIterator) :: it
	class(*), allocatable :: value

	it=self%FindIt(index) !��index��Ԫ�صĵ�����
	value = it%value !����Ԫ��ֵ
end function Get
!��������Ԫ��ֵ
subroutine Change(self, index, value)
	class(ftlList), intent(inout) :: self
	integer, intent(in)  :: index
	class(*), intent(in) :: value
	type(ftlListIterator) :: it

	it=self%FindIt(index) !��index��Ԫ�صĵ�����

	select type (node => it%node) !nodeΪ����
		type is (DataNode) ! always true
		node%data = value
	end select

	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine Change
!����λ��ǰ������Ԫ��
subroutine InsertVal(self, index, value)
	class(ftlList), intent(inout) :: self
	integer, intent(in)  :: index !λ�����
	class(*), intent(in) :: value
	type(ftlListIterator) :: it

	it=self%FindIt(index) !��index��Ԫ�صĵ�����
	call self%InsertFill(it, 1, value)
end subroutine InsertVal
!����λ��ɾ��Ԫ��
subroutine EraseVal(self, index)
	class(ftlList), intent(inout) :: self
	integer, intent(in)  :: index !λ�����
	type(ftlListIterator) :: it

	it=self%FindIt(index) !��index��Ԫ�صĵ�����
	call self%EraseSingle(it)
end subroutine EraseVal
!���ص�index��Ԫ�صĵ����� self%FindIt(1)�ȼ���self%Begin()
type(ftlListIterator) function FindIt(self, index)  !������
	class(ftlList), intent(in) :: self
	integer, intent(in)  :: index !λ�����
	integer :: i

	if (index>self%psize .or. index<1) then
		stop "Index is not in range [1,psize]."
	end if
	!����index��Сѡ����������
	if (index<=shiftr(self%psize,1)+1) then !��indexС ��ͷ��ʼ����
		FindIt=self%Begin() !��Ԫ��λ��

		do i=1,index-1 !ѭ��index-1��
			call FindIt%Inc() !��һ�ڵ�
		end do
	else !��index�� ��β��ʼ����
		FindIt=self%End() !�ڱ��ڵ�

		do i=index,self%psize !ѭ��self%psize-index+1��
			call FindIt%Dec() !��һ�ڵ�
		end do
	end if
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
		type is(integer)
			write(lunit,'(i0)',advance='no') v_p
		type is(real) !fmt=*��advance='no'����ͬʱ����
			write(string,fmt=*) v_p
			write(lunit,'(a)',advance='no') trim(adjustl(string))
		type is(real(kind(0d0))) !˫����
			write(string,fmt=*) v_p
			write(lunit,'(a)',advance='no') trim(adjustl(string))
		type is(complex)
			write(string,fmt=*) v_p !����ת��Ϊ�ַ���
			write(lunit,'(a)',advance='no') trim(adjustl(string)) !ɾ����β�ո�
		type is(complex(kind(0d0)))
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
		type is(ftlList) !Ԫ������һ������
			call v_p%WriteList(lunit)
		class default !δ֪����
			write(lunit,'(a)',advance='no') '*'
	end select

	if(present(IsNewLine).and.(.not.IsNewLine))then
		!������
	else
		write(lunit,*) !����
	end if
end subroutine WriteNode
!����ʵ������ ����ͷֵ
pure function GetFront(self) result(value)
	class(ftlList), intent(in) :: self
	class(*), allocatable :: value

	value=self%front
end function GetFront
!����ʵ������ ����βֵ
pure function GetBack(self) result(value)
	class(ftlList), intent(in) :: self
	class(*), allocatable :: value

	value=self%back
end function GetBack
!����ʵ������ ���ص�������Ӧ�ڵ�Ԫ������
pure function GetValue(self) result(value)
	class(ftlListIterator), intent(in) :: self
	class(*), allocatable :: value

	value=self%value
end function GetValue
end module