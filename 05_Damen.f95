module place
	implicit none
	type dame
		integer::posX,posY
	end type

	contains

	recursive function placeQueens(n,queens) result (solutions)
		integer::solutions,n
		type(dame),Dimension(:)::queens
	end function placeQueens

end module place

program queens
	use place
	implicit none
	integer::n
	
	do
		read(*,*) n
		if (n<1) exit
	end do
	!------ Phase 1 ------
	!read files
end program queens
!recursive function t result x
!	integer::x
!end function t
