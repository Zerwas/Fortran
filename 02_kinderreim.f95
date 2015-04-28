module children
	implicit none

	type child
		character(len=10)::name
		integer::age
		type(child),pointer::nextChild=>null()
	end type child
	
	type(child),pointer::firstChild=>null()

	private
	public :: PUT_CYCLE
	contains

	subroutine PUT_CYCLE(fst)
		type(child),pointer::fst,current
		current=fst
		if (associated(current)) then
			while (associated(current%nextChild,fst)) do
				write(*,*) current%name
			enddo
		endif
end module children

program kinderreim
	write(*,*) 'test'
end program kinderreim
