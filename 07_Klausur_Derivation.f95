module derivation
	implicit none
	type diff
		real::value,deriv
	end type

	interface operator (+)
		module procedure add
	end interface operator (+)

	interface operator (-)
		module procedure sub
	end interface operator (-)

	interface operator (*)
		module procedure mul
	end interface operator (*)

	interface operator (/)
		module procedure div
	end interface operator (/)

	contains

	function add(diff1,diff2)
		type(diff),intent(in)::diff1,diff2
		type(diff)::add
		add%value=diff1%value+diff2%value
		add%deriv=diff1%deriv+diff2%deriv
	end function add

	function sub(diff1,diff2)
		type(diff),intent(in)::diff1,diff2
		type(diff)::sub
		sub%value=diff1%value-diff2%value
		sub%deriv=diff1%deriv-diff2%deriv
	end function sub

	function mul(diff1,diff2)
		type(diff),intent(in)::diff1,diff2
		type(diff)::mul
		mul%value=diff1%value*diff2%value
		mul%deriv=diff1%deriv*diff2%value+diff1%value*diff2%deriv
	end function mul

	function div(diff1,diff2)
		type(diff),intent(in)::diff1,diff2
		type(diff)::div
		div%value=diff1%value/diff2%value
		div%deriv=(diff1%deriv-(diff1%value/diff2%value)*diff2%deriv)/diff2%value
	end function div

	function diffcon(c)
		real::c
		type(diff)::diffcon
		diffcon%value=c
		diffcon%deriv=0
	end function diffcon

	function diffvar(x)
		real::x
		type(diff)::diffvar
		diffvar%value=x
		diffvar%deriv=1
	end function diffvar

	subroutine put(diff1)
		type(diff)::diff1
		write(*,*) 'f(x)=',diff1%value
		write(*,*) 'fp(x)=',diff1%deriv
		write(*,*) ''
	end subroutine

	function f(x)
		real::x
		type(diff)::f
		f=(diffvar(x)+diffcon(4.0))/(diffvar(x)*diffvar(x)+diffcon(5.0))
	end function f

end module derivation

program main
	use derivation
	implicit none
	real::a,b,h
	do 
	read(*,*) a,b,h
		if (a<=b.and.h>0) exit
	end do
	do while (a<=b)
		call put(f(a))
		a=a+h
	end do
end program main
