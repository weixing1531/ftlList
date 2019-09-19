# ftlList
A Fortran List Module using unlimited polymorphic.

本模块在GitHub上大牛Robert Rüger项目Fortran Template Library (FTL)的基础上进行了如下修改：
1.用无限多态class(*)代替预处理宏进行通用类型操作
2.ftlList类增加了Set, Get, Change, WriteList, EraseVal, InsertVal, FindIt, GetFront, GetBack等实例方法，并将实例变量front, back设为私有
3.ftlListIterator类增加了GetValue实例方法，并将实例变量value设为私有
4.增加了模块方法WriteNode
