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

program main
	use ftlListMod
	implicit none

	type(ftlList) :: o !无限多态双向循环链表
	type(ftlListIterator) :: it !链表迭代器
	integer :: i
	real(kind=4) :: r
	character(len=:),allocatable :: c !递延长度字符串

	call o%New([5,13,41,97,17,10,88]) !构造方法

	write(*,*)o%Size() !元素个数
	call o%WriteList() !打印链表
	call o%PushBack("123") !尾部压入元素
	call o%WriteList() !打印链表

	select type(p=>o%GetBack()) !o%GetBack()为链表尾节点元素
		type is(character(*))
			c=p !无限多态类型赋值字符串类型
	end select

	write(*,*)c

	call WriteNode(o%PopBack(),"Node") !尾部压出元素并打印该元素
	call o%WriteList() !打印链表
	call o%PushFront(0.123) !头部压入元素
	call o%WriteList() !打印链表

	select type(p=>o%GetFront()) !o%GetFront()为链表头节点元素
		type is(real(kind=4))
			r=p !无限多态类型赋值单精度整数类型
	end select

	write(*,*)r

	call WriteNode(o%PopFront(),"Node") !头部压出元素并打印该元素
	call o%WriteList() !打印链表
	call o%Change(2,55) !第2个元素值更改为55
	call o%WriteList() !打印链表
	call o%Erase(7) !删除第7个元素
	call o%WriteList() !打印链表
	call o%Insert(2,33) !第2个元素前插入新元素33
	call o%WriteList() !打印链表
	call WriteNode(o%Get(6),"Node") !获取第6个元素值并打印该元素

	select type(p=>o%Get(6))
		type is(integer)
			i=p !无限多态类型赋值整数类型
	end select

	write(*,*)i

	write(*,*)"以下正向遍历链表"
	it=o%Begin() !链表头节点

	do while(it/=o%End())
		call WriteNode(it%GetValue(),"Node") !打印节点元素
		call it%Inc() !下一节点
	end do

	write(*,*)"以下反向遍历链表"
	it=o%End() !链表哨兵节点
	call it%Dec() !上一节点即尾节点

	do while(it/=o%End())
		call WriteNode(it%GetValue(),"Node") !打印节点元素
		call it%Dec() !上一节点
	end do

	read(*,*)
end program
