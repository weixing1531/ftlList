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

program main
	use ftlSingleListMod
	implicit none

	type(ftlSingleList) :: o !���޶�̬˫��ѭ������
	type(ftlSingleListIterator) :: it !����������
	integer :: i
	real(kind=4) :: r
	character(len=:),allocatable :: c !���ӳ����ַ���

	call o%New([5,13,41,97,17,10,88]) !���췽��

	write(*,*)o%Size() !Ԫ�ظ���
	call o%WriteList() !��ӡ����
	call o%Resize(8,(0.0d0,0.0d0)) !�������� ���ӵĽڵ�Ϊ����
	call o%WriteList() !��ӡ����

	call o%PushBack("123") !β��ѹ��Ԫ��
	call o%WriteList() !��ӡ����

	select type(p=>o%GetBack()) !o%GetBack()Ϊ����β�ڵ�Ԫ��
		type is(character(*))
			c=p !���޶�̬���͸�ֵ�ַ�������
	end select

	write(*,*)c

	call o%PushFront(0.123) !ͷ��ѹ��Ԫ��
	call o%WriteList() !��ӡ����

	select type(p=>o%GetFront()) !o%GetFront()Ϊ����ͷ�ڵ�Ԫ��
		type is(real(kind=4))
			r=p !���޶�̬���͸�ֵ��������������
	end select

	write(*,*)r

	call WriteNode(o%PopFront(),"Node") !ͷ��ѹ��Ԫ�ز���ӡ��Ԫ��
	call o%WriteList() !��ӡ����
	call o%Change(2,55) !��2��Ԫ��ֵ����Ϊ55
	call o%WriteList() !��ӡ����
	call o%Erase(3) !ɾ����3��Ԫ��
	call o%WriteList() !��ӡ����
	call o%Insert(2,33) !��2��Ԫ��ǰ������Ԫ��33
	call o%WriteList() !��ӡ����
	call WriteNode(o%Get(3),"Node") !��ȡ��6��Ԫ��ֵ����ӡ��Ԫ��

	select type(p=>o%Get(3))
		type is(integer)
			i=p !���޶�̬���͸�ֵ��������
	end select

	write(*,*)i

	write(*,*)"���������������"
	it=o%Begin() !����ͷ�ڵ�

	do while(it/=o%End())
		call WriteNode(it%GetValue(),"Node") !��ӡ�ڵ�Ԫ��
		call it%Inc() !��һ�ڵ�
	end do

	it=o%End() !�����ڱ��ڵ�
	call it%Dec() !��һ�ڵ㼴β�ڵ�
	call WriteNode(it%GetValue(),"Node") !��ӡ�ڵ�Ԫ��

	it=o%FindIt(5) !�����ڱ��ڵ�
	call WriteNode(it%GetValue(),"Node") !��ӡ�ڵ�Ԫ��

	read(*,*)
end program