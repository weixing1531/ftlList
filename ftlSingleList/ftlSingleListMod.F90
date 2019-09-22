module ftlSingleListMod
	use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT
	implicit none
	!ģ�����ӿڰ���Begin,End,Size,ftlSwap,ftlMove,operator(==),operator(/=),WriteNode(���ӷ���)
	private
	! ====== Type of the ftlSingleList container itself ====================================================================================
	type :: ListNode !���� ���������ڵ�  �ڱ��ڵ�����ݽڵ�
		class(ListNode), pointer :: next => null() !��ָ�� ��ָ�� ��ָ����ListNode��������
	end type

	type, extends(ListNode) :: DataNode !���������ڵ������ ���ݽڵ� ���ӳ�InsertNodeAfter��ʹ��
		class(*), allocatable :: data !�ο�FCCF���� ������ݽڵ�����޶�̬����
	end type

	type, extends(ListNode) :: SLNode !���������ڵ������ �ڱ��ڵ� ��ģ��ؼ� ����ȷ��β�ڵ�λ��
		class(ListNode), pointer :: prev => null() !ǰָ�� ��ָ�� ��ָ����ListNode��������
	end type
	!                                End             Begin                                            End
	!��״���������Ľڵ�����˳��Ϊ���ڱ��ڵ�(��Ԫ��)��ͷ�ڵ�(��һ��Ԫ��)��...��β�ڵ�(���һ��Ԫ��)���ڱ��ڵ�(��Ԫ��)��...
	type, public :: ftlSingleList !��״��������
		private

		integer :: psize = 0 !������Ԫ�ظ��� ˽�б���
		type(SLNode) :: sentinel !���� �ڱ��ڵ㣨��data���������ݣ� ˽�б��� �޸�
		class(*), pointer :: front => null() !ͷ�ڵ�Ԫ����ֵ �޸�Ϊ˽�б���
		class(*), pointer :: back => null()  !β�ڵ�Ԫ����ֵ �޸�Ϊ˽�б���
	contains
		private
		!���������ӿڣ�New,Delete,Begin,End,Size,Empty,=,Insert(���ӷ���),Erase(���ӷ���),PushFront,PopFront,PushBack,Resize,Clear
		!��������������ӿڣ�Get,Chang,FindIt,WriteList,GetFront,GetBack
		procedure :: NewDefault
		procedure :: NewCopyOther
		procedure :: NewFill
		procedure :: NewFromArray
		procedure :: NewFromftlSingleListIteratorPair
		generic , public :: New => NewDefault, NewCopyOther, NewFill, NewFromArray, NewFromftlSingleListIteratorPair !���췽��

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
		procedure :: InsertftlSingleListIteratorPair
		procedure :: InsertVal !�������� �������ǰ��Ԫ��
		generic , public :: Insert => InsertSingle, InsertFill, InsertArray, InsertftlSingleListIteratorPair, InsertVal !��ǰ��������Ԫ��

		procedure :: EraseSingle
		procedure :: EraseftlSingleListIteratorPair
		procedure :: EraseVal !�������� �������ɾ��Ԫ��
		generic , public :: Erase => EraseSingle, EraseftlSingleListIteratorPair, EraseVal !ɾ������Ԫ��

		procedure, public :: PushFront !����ͷ��ѹ����Ԫ��
		procedure, public :: PopFront  !����ͷ��ѹ��Ԫ��
		procedure, public :: PushBack  !����β��ѹ����Ԫ��
		!procedure, public :: PopBack   !����β��ѹ��Ԫ�� �������ݽڵ�û��ǰָ��

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
	! ====== Type of an ftlSingleListIterator over a ftlSingleList container ============================================================================
	type, public :: ftlSingleListIterator !���������ڵ�+����  �൱�ڽڵ�ָ��
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
		procedure, public :: Dec !��һ���ڵ� ֻ�����ڱ��ڵ�ת��β�ڵ�

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
! ====== Implementation of ftlSingleList methods =======================================================================================
subroutine NewDefault(self)
	class(ftlSingleList), intent(out), target :: self
	!������ʼ��ֻ���ڱ��ڵ�
	self%sentinel%next => self%sentinel !��ָ��ָ���ڱ��ڵ�����
	self%sentinel%prev => self%sentinel !ǰָ��ָ���ڱ��ڵ�����
end subroutine
!
impure elemental subroutine NewCopyOther(self, other)
	class(ftlSingleList), intent(inout) :: self
	!type(ftlSingleList), intent(in) :: other
	class(ftlSingleList), intent(in) :: other !�Ķ�

	type(ftlSingleListIterator) :: it !������

	call self%New()

	it = other%Begin() !ͷ�ڵ�

	do while (it /= other%End())
		call self%PushBack(it%value) !β��ѹ��
		call it%Inc() !��һ���ڵ�
	enddo
end subroutine
!n��Ԫ�صĳ�ʼֵ����Ϊval
subroutine NewFill(self, n, val)
	class(ftlSingleList), intent(inout) :: self
	integer , intent(in) :: n
	class(*) , intent(in), optional :: val

	integer :: i

	call self%New()

	if (present(val)) then
		do i = 1, n
			call self%PushBack(val) !β��ѹ��
		enddo
	else
		stop 'TODO: Implement ftlSingleList%NewFill without val'
	endif
end subroutine
!array����Ϊһά����
subroutine NewFromArray(self, array)
	class(ftlSingleList), intent(inout) :: self
	class(*) , intent(in) :: array(:)
	integer :: i

	call self%New()

	do i = 1, size(array)
		call self%PushBack(array(i)) !β��ѹ��
	enddo
end subroutine
!�ɵ�������������
subroutine NewFromftlSingleListIteratorPair(self, first, last)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator), intent(in) :: first !������
	type(ftlSingleListIterator), intent(in) :: last  !������

	call self%New()
	call self%Insert(self%Begin(), first, last)
end subroutine
!��������
impure elemental subroutine Delete(self)
	class(ftlSingleList), intent(inout), target :: self
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
	type(ftlSingleList), intent(inout) :: self

	call self%Delete()
end subroutine
! =============> ftlSingleListIterators:
type(ftlSingleListIterator) function BeginList(self) result(Begin) !������ ������ͷ�ڵ�
	class(ftlSingleList), intent(in), target :: self

	Begin%node => self%sentinel%next !ͷ�ڵ� ������Ҳ����ָ�����ݽڵ�(����DataNode)

	select type (node => Begin%node) !nodeΪ����
		type is (DataNode) !���ݽڵ�
		Begin%value => node%data
	end select
end function

type(ftlSingleListIterator) function EndList(self) result(End) !������ �����Ľڵ������־��Ҳ���ڱ��ڵ���data��
	class(ftlSingleList), intent(in), target :: self

	End%node => self%sentinel !�ඨ����class(ListNode)��ָ̬���ԭ�� ������Ҳ����ָ���ڱ��ڵ�(����SLNode)
end function
! =============> Capacity:
pure integer function SizeList(self) result(Size) !������Ԫ�ظ���
	class(ftlSingleList), intent(in) :: self

	Size = self%psize
end function

pure logical function Empty(self) !�����Ƿ�Ϊ��
	class(ftlSingleList), intent(in) :: self

	Empty = (self%psize == 0)
end function
! =============> Modifiers:
! TODO: implement using existing list nodes instead of copy construction
!
impure elemental subroutine AssignOther(self, other) !������ֵ
	class(ftlSingleList), intent(inout) :: self
	!type(ftlSingleList), intent(in) :: other
	class(ftlSingleList), intent(in) :: other !�Ķ�

#if defined(NAGFOR)
	! For some reason NAGFOR needs a special implementation here. I suspect that this is a compiler bug. The normal
	! implementation, as in %NewCopyOther doesn't work, because the ftlSingleListIterator into other never becomes equal to other%End(),
	! causing an infinite loop until we run out of memory.
	! For some reason this seems to be related to this method being called through the assignment statement, because this doesn't
	! happen if %NewCopyOther is called directly, or %AssignOther is made public and called directly. I don't really understand
	! this, it's either a NAGFOR bug or I'm totally not understanding something about user defined assignment in Fortran ...
	type(ftlSingleListIterator) :: it !������
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
	class(ftlSingleList), intent(inout) :: self
	class(*) , intent(in) :: array(:)

	call self%New(array)
end subroutine
!����������positionλ��֮�����1��Ԫ��,��ֵΪval
subroutine InsertSingle(self, position, val)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: position !������
	class(*) , intent(in) :: val

	call self%InsertFill(position, 1, val)
end subroutine
!����������positionλ��֮�����n��Ԫ��,��ֵ��Ϊval
subroutine InsertFill(self, position, n, val)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: position !������
	integer , intent(in) :: n
	class(*) , intent(in) :: val
	integer :: i

	do i = 1, n
		call InsertNodeAfter(position%node, val) !��� InsertNodeAfter��һ�������Ƕ�̬�ڵ���ָ�� �޸�
	enddo

	self%psize = self%psize + n !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!����������positionλ��֮���������array
subroutine InsertArray(self, position, array)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: position
	class(*) , intent(in) :: array(:)
	integer :: i

	do i = 1, size(array)
		call InsertNodeAfter(position%node, array(i)) !��� InsertNodeAfter��һ�������Ƕ�̬�ڵ���ָ�� �޸�
	enddo

	self%psize = self%psize + size(array) !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!����������positionλ��֮���������� ��first��last-1
subroutine InsertftlSingleListIteratorPair(self, position, first, last)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: position
	type(ftlSingleListIterator), intent(in) :: first
	type(ftlSingleListIterator), intent(in) :: last
	type(ftlSingleListIterator) :: it

	it = first

	do while (it /= last)
		call InsertNodeAfter(position%node, it%value) !��� InsertNodeAfter��һ�������Ƕ�̬�ڵ���ָ�� �޸�
		self%psize = self%psize + 1 !����Ԫ�ظ����ı�
		call it%Inc() !��һ���ڵ�
	enddo

	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!ͷ��ѹ��
subroutine PushFront(self, val)
	class(ftlSingleList), intent(inout), target :: self
	class(*) , intent(in) :: val
	type(ftlSingleListIterator) :: it !����

	it=self%End() !����
	!ע�ⲻ��ֱ����self%sentinelλ�ò��� ��Ϊ��������Ϊtype(SLNode) ���Ƕ�̬ �����ػش���
	call InsertNodeAfter(it%node, val) !��� InsertNodeAfter��һ�������Ƕ�̬�ڵ���ָ�� �޸�
	self%psize = self%psize + 1 !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!ͷ��ѹ��
function PopFront(self) result(Res)
	class(ftlSingleList), intent(inout), target :: self
	class(ListNode), pointer :: oldfirst
	class(*), allocatable :: Res

	oldfirst => self%sentinel%next !ͷ�ڵ㣨��һ��Ԫ�ض�Ӧ�Ľڵ㣩
#ifdef FTL_TEMPLATE_TYPE_PROVIDES_FTLMOVE
	call ftlMove(self%front, Res) !��������������
#else
	Res = self%front !�������Դ�����
#endif
	self%psize = self%psize - 1 !����Ԫ�ظ����ı�
	self%sentinel%next => oldfirst%next !����
	deallocate(oldfirst) !�ͷŸýڵ��ڴ�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end function
!β��ѹ��
subroutine PushBack(self, val)
	class(ftlSingleList), intent(inout) :: self
	class(*) , intent(in) :: val

  call InsertNodeAfter(self%sentinel%prev, val) !��� InsertNodeAfter��һ�������Ƕ�̬�ڵ���ָ��
	self%psize = self%psize + 1 !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!ɾ��������position��һ�ڵ�
subroutine EraseSingle(self, position)
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: position
	class(ListNode), pointer :: temp !����

	temp => position%node%next !����
	position%node%next => temp%next !����

	if(associated(temp,self%sentinel%prev))self%sentinel%prev => position%node !tempΪβ�ڵ� ����
	deallocate(temp) !�ͷŸýڵ��ڴ� ����
	self%psize = self%psize - 1 !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!
subroutine EraseftlSingleListIteratorPair(self, first, last) !��first+1��last-1ɾ��Ԫ��
	class(ftlSingleList) , intent(inout) :: self
	type(ftlSingleListIterator) :: first !ע����ý�����first�Ѹı�
	type(ftlSingleListIterator), intent(in) :: last
	type(ftlSingleListIterator) :: deletor

	deletor=first !�޸� deletorλ���ǹ̶���

	associate(walker => first) !walker��first�ı���
		call walker%Inc() !��һ���ڵ� ����  ���ﴦ����˫��������ͬ

		do while (walker /= last)
			call walker%Inc() !��һ���ڵ�
			call self%EraseSingle(deletor) !ɾ��deletor�ڵ�
		enddo
	end associate
end subroutine
!�������� ģ�鷽��
subroutine SwapList(self, other)
	type(ftlSingleList), intent(inout), target :: self
	type(ftlSingleList), intent(inout), target :: other
	integer :: tmpSize
	type(SLNode) :: tmpNode !�޸�

	! fix pointers from data nodes to the sentinels
	self%sentinel%prev%next => other%sentinel
	other%sentinel%prev%next => self%sentinel
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
	class(ftlSingleList), intent(inout) :: self
	integer , intent(in) :: n
	class(*) , intent(in) , optional :: val
	type(ftlSingleListIterator) :: it
	integer :: i

	if (n == self%psize) then
		return !�ӳ��򲻽����κβ���
	else if (n < self%psize) then
		it = self%Begin() !ͷ�ڵ� i=1

		do i = 2, n
			call it%Inc() !��һ���ڵ�
		enddo
		!��ʱitλ��Ϊn
		!����ɾ����һ��
		call self%Erase(it,self%End()) !ɾ��n+1��β�ڵ�Ķ���β��
	else ! n > self%psize
		do i = 1, n - self%psize
			call InsertNodeAfter(self%sentinel%prev, val) !��� ��β�ڵ�֮�󣩲��䲻�㲿�� �޸�
		enddo
	endif

	self%psize = n !����Ԫ�ظ����ı�
	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine
!������нڵ�
subroutine Clear(self)
	class(ftlSingleList), intent(inout) :: self

	call self%New()
end subroutine
! =============> FTL methods:
subroutine ftlMoveList(src, dest) !src�ɻ�,dest����
	type(ftlSingleList), intent(inout) :: src
	type(ftlSingleList), intent(out) , target :: dest

	dest%psize = src%psize
	dest%sentinel = src%sentinel
	!����ɾ����һ��
	if (associated(dest%sentinel%prev)) dest%sentinel%prev%next => dest%sentinel
	call dest%FixValuePtrs() !ȷ��������ͷֵ��βֵ
	nullify(src%sentinel%prev)
	nullify(src%sentinel%next)
	nullify(src%front)
	nullify(src%back)
	src%psize = 0
end subroutine
! =============> Internal methods:
subroutine InsertNodeAfter(afternode, val) !��afternodeλ�ú����Ԫ��
	class(ListNode), pointer, intent(inout) :: afternode !ע���ǽڵ����ָ̬��
	class(*) , intent(in) , optional :: val
	class(ListNode), pointer :: oldnext, newnext

	oldnext => afternode%next !��һ���ڵ�
	allocate(DataNode::afternode%next) !�����ݽڵ���������ڴ�
	newnext => afternode%next
	!����ɾ����һ��
	newnext%next => oldnext
	!��afternodeΪβ�ڵ� ��oldnextΪ�ڱ��ڵ�
	select type(oldnext) !���� ������ؼ�
		type is(SLNode) !�ڱ��ڵ�
			oldnext%prev => newnext !ֻ���ڱ��ڵ���ǰָ��
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

	select type (node => self%node) !nodeΪ����
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
! =============> Arithmetic operations:
subroutine Inc(self) !��һ���ڵ�
	class(ftlSingleListIterator), intent(inout) :: self

	self%node => self%node%next !��һ���ڵ�

	select type (node => self%node) !nodeΪ����
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
!
subroutine Dec(self) !��һ���ڵ� ֻ�����ڱ��ڵ�ת��β�ڵ�
	class(ftlSingleListIterator), intent(inout) :: self

	!self%node => self%node%prev !Error: 'prev' at (1) is not a member of the 'listnode' structure
	select type(p => self%node)
		type is(SLNode) !�ڱ��ڵ�
		!p => p%prev !��һ���ڵ� �����ᱨ��
		!Error: Different types in pointer assignment at (1); attempted assignment of CLASS(listnode) to TYPE(slnode)
		self%node => p%prev !��һ���ڵ�
	end select

	select type (node => self%node) !nodeΪ����
		type is (DataNode)
		self%value => node%data
	end select
end subroutine
! =============> Logical operations:
pure logical function EqualOther(self, other) !���������
	class(ftlSingleListIterator), intent(in) :: self
	class(ftlSingleListIterator), intent(in) :: other

	EqualOther = associated(self%node,other%node) !ָ�����
end function
!
pure logical function UnequalOther(self, other) !�����������
	class(ftlSingleListIterator), intent(in) :: self
	class(ftlSingleListIterator), intent(in) :: other

	UnequalOther = .not.associated(self%node,other%node) !ָ�벻����
end function
!��ӡ��������Ԫ��ֵ
recursive subroutine WriteList(self, unit)
	class(ftlSingleList), intent(in) :: self
	integer, intent(in), optional :: unit !����ļ��� Ĭ��Ϊ��Ļ
	type(ftlSingleListIterator) :: it

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
	class(ftlSingleList), intent(in) :: self
	integer, intent(in)   :: index  !λ�����
	type(ftlSingleListIterator) :: it
	class(*), allocatable :: value

	it=self%FindIt(index) !��index��Ԫ�صĵ����� �޸�
	value = it%value !����Ԫ��ֵ
end function Get
!��������Ԫ��ֵ
subroutine Change(self, index, value)
	class(ftlSingleList), intent(inout) :: self
	integer, intent(in)  :: index
	class(*), intent(in) :: value
	type(ftlSingleListIterator) :: it

	it=self%FindIt(index) !��index��Ԫ�صĵ����� �޸�

	select type (node => it%node) !nodeΪ����
		type is (DataNode) ! always true
		node%data = value
	end select

	call self%FixValuePtrs() !ȷ��������ͷֵ��βֵ
end subroutine Change
!����λ��ǰ������Ԫ��
subroutine InsertVal(self, index, value)
	class(ftlSingleList), intent(inout) :: self
	integer, intent(in)  :: index !λ�����
	class(*), intent(in) :: value
	type(ftlSingleListIterator) :: it

	it=self%FindIt(index,.true.) !��index��Ԫ��ǰһ��λ�õĵ�����
	call self%InsertFill(it, 1, value)
end subroutine InsertVal
!����λ��ɾ��Ԫ��
subroutine EraseVal(self, index)
	class(ftlSingleList), intent(inout) :: self
	integer, intent(in)  :: index !λ�����
	type(ftlSingleListIterator) :: it

	it=self%FindIt(index,.true.) !��index��Ԫ��ǰһ��λ�õĵ�����
	call self%EraseSingle(it)
end subroutine EraseVal
!���ص�index��Ԫ��ǰһ��λ�õĵ����� self%FindIt(2)�ȼ���self%Begin()
type(ftlSingleListIterator) function FindIt(self, index, IsBefore)  !������
	class(ftlSingleList), intent(in) :: self
	integer, intent(in)  :: index !λ�����
	logical, optional, intent(in)  :: IsBefore !��Ϊ��index��Ԫ��ǰһλ�� ��Ϊ��ǰλ�� ����
	integer :: i

	if (index>self%psize .or. index<1) then
		stop "Index is not in range [1,psize]."
	end if
	!ֻ�ܴ�ͷ��ʼ����
	if(present(IsBefore) .and. IsBefore)then
		FindIt=self%End()   !�ڱ��ڵ� �޸� ��������ǰһλ��
	else
		FindIt=self%Begin() !ͷ�ڵ� �޸� �������ص�ǰλ��
	end if

	do i=1,index-1 !ѭ��index-1��
		call FindIt%Inc() !��һ�ڵ�
	end do
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
		type is(ftlSingleList) !Ԫ������һ������
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
	class(ftlSingleList), intent(in) :: self
	class(*), allocatable :: value

	value=self%front
end function GetFront
!����ʵ������ ����βֵ
pure function GetBack(self) result(value)
	class(ftlSingleList), intent(in) :: self
	class(*), allocatable :: value

	value=self%back
end function GetBack
!����ʵ������ ���ص�������Ӧ�ڵ�Ԫ������
pure function GetValue(self) result(value)
	class(ftlSingleListIterator), intent(in) :: self
	class(*), allocatable :: value

	value=self%value
end function GetValue
end module