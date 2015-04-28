module children
	implicit none

	type child
		character(len=10)::name
		integer::age
		type(child),pointer::nextChild
	end type child
	
	type(child),pointer::firstChild=>null()

	contains

	subroutine PUT_CYCLE(fst)
		type(child),pointer::fst,current
		current=>fst
		write(*,*) 'Remaining children:'
		if (associated(current)) then
			!loop over children until you are back at the first child
			do
				write(*,*) current%name
				current=>current%nextChild
				if (associated(current,fst)) exit
			enddo
		endif
	end subroutine PUT_CYCLE

	subroutine INIT_CYCLE()
		firstChild=>null()
	end subroutine INIT_CYCLE

	logical function LAST_ONE()
		!is there only one child in the cycle?
		LAST_ONE=associated(firstChild).and.(associated(firstChild,firstChild%nextChild))
	end function LAST_ONE

	function BUILD_CYCLE()
		type(child),pointer::BUILD_CYCLE,new,tmp=>null()
		integer :: io_error
   		integer :: age,n
		character(len=10)::name
		open(unit=20,file='KREIM.DAT',status='old',action='read',iostat=io_error)
		if (io_error == 0) then
			do n = 1, 15
				!read next child
				read(20,*,iostat=io_error) name,age
				!end of file?
				if (io_error/=0) then 
					exit
				endif
				allocate(new)
				new%age=age
				new%name=name
				!append child to list
				if (associated(tmp)) then
					tmp%nextChild=>new
				else
					firstChild=>new
				endif
				tmp=>new
				write(*,*) name,age
			end do
			!connect the last child with the first child
			tmp%nextChild=>firstChild
			BUILD_CYCLE=>firstChild
		else
			write(*,*) 'Error ',io_error
		end if
		close(unit=20)
	end function BUILD_CYCLE

	subroutine DEL_NEXT(elem)
		type(child),pointer::elem,tmp
		!scip child to be deleted
		tmp=>elem%nextChild
		elem%nextChild=>tmp%nextChild
		!set first child
		firstChild=>elem
		!free memory
		deallocate(tmp)
	end subroutine DEL_NEXT

end module children

program kinderreim
	use children
	implicit none
	type(child),pointer::current
	integer::n
	call INIT_CYCLE()
	current=>BUILD_CYCLE()
	do while (.not.LAST_ONE())
		!go 20 children foreward (stop at the 21st)
		do n=1,20
			current=>current%nextChild
		enddo
		!go (age-1) of current child many steps forward
		do n=1,current%age-1
			current=>current%nextChild
		enddo
		call PUT_CYCLE(current)
		write(*,*) 'This round ',current%nextChild%name,'has to leave.'
		call DEL_NEXT(current)
		!ryhme starts again at the child after deleted child
		current=>current%nextChild
	enddo
	call PUT_CYCLE(current)
	write(*,*) current%name,'won.'
end program kinderreim
