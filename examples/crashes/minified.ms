
# Testing weird source code input
class B{next:int}class A{next:B constructor(self){self.next=B() self.next.next=5}}type X int type Y X hhh:Y?=5 take_int=fn(in:int){print in}take_int(hhh or 8) x:A?=A() print x print (A()).next.next type none int?null:none = nil take_bool=fn(in:bool) {print in}class C{a:int}m:C?=nil print (m or C()).a type A bool? k:A?=nil take_bool(get (k or nil) or false)
