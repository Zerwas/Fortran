module place
	implicit none
	type dame
		integer::posX,posY
	end type

	contains

	recursive function placeQueens(n,queens) result (solutions)
		integer::solutions,n,i,placed
		type(dame),Dimension(:)::queens
		type(dame),Dimension(:),allocatable::newqueens
		type(dame)::newqueen
		placed=size(queens)
		newqueen%posX=placed+1
		allocate(newqueens(placed+1))
		newqueens(1:placed)=queens
		solutions=0
		do i=1,n
			newqueen%posY=i
			newqueens(placed+1)=newqueen
			if (consistent(newqueens)) then
				if (placed+1==n) then
					call drawqueens(newqueens)
					solutions=solutions+1
				else
					solutions=solutions+placeQueens(n,newqueens)
				end if
				
			end if
		end do
	end function placeQueens

	logical function consistent(queens)
		type(dame),Dimension(:)::queens
		integer::i,j,dX,dY
		consistent=.True.
		do i=1,size(queens)
			do j=i+1,size(queens)
				dX=abs(queens(i)%posX-queens(j)%posX)
				dY=abs(queens(i)%posY-queens(j)%posY)
				if (dX==0.or.dY==0.or.dX==dY) consistent=.False.
			end do
		end do
	end function consistent

	subroutine drawqueens(queens)
		type(dame),Dimension(:)::queens
		integer::x,y,i
		character(len=size(queens))::line
		do y=1,size(queens)
			line=''
			do x=1,size(queens)
				line(x:)='.'
				do i=1,size(queens)
					if (queens(i)%posY==y.and.queens(i)%posX==x) line(x:)='Q'
				end do
			end do
			write(*,*) line
		end do
		write(*,*) ''
	end subroutine drawqueens
end module place

program queens
	use place
	implicit none
	integer::n,solutions
	type(dame),Dimension(0)::init
	
	do
		read(*,*) n
		if (n<1) exit
		solutions=placeQueens(n,init)
		Write(*,*) "Number of Solutions: ",solutions
	end do
	!------ Phase 1 ------
	!read files
end program queens
!recursive function t result x
!	integer::x
!end function t
