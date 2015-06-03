module insertmerge
	implicit none

	type filecomp
		integer::content,filenumber
	end type filecomp

	interface operator (<=)
		module procedure leq
	end interface operator (<=)

	contains

	logical function leq(a,b)
		type(filecomp),intent(in)::a,b
		leq=a%content<=b%content	
	end function leq

	subroutine sort(list)
		type(filecomp),dimension(:),intent(inout)::list
		integer::sep
		do sep=size(list)-1,1
			call insert(list(sep:size(list)),list(sep))
		end do
	end subroutine sort

	subroutine insert(list,elem)
		type(filecomp),dimension(:),intent(inout)::list
		type(filecomp),intent(in)::elem
		integer::i
		type(filecomp)::tmp
		i=2
		!create new element so its not overwritten
		tmp%content=elem%content
		tmp%filenumber=elem%filenumber
		do while (i<=size(list) .and. list(i) <= tmp)
			list(i-1)=list(i)
			i=i+1
		end do
		list(i-1)=tmp
	end subroutine insert

end module insertmerge


program mergefiles
	use insertmerge
	implicit none
	type(filecomp),dimension(:),allocatable::Arbeitsfeld
	type(filecomp)::tmp
	integer::n,i,io_error
	character(2)::str
	read(*,*) n
	allocate(Arbeitsfeld(n))
	!------ Phase 1 ------
	!read files
	do i=1,n
		write(str,fmt='(I2.2)') i
		write(*,*) 'erfass'//str//'.dat'
		open(unit=21+i,file='erfass'//str//'.dat',action='read')
		tmp%filenumber=21+i
		read(21+i,*) tmp%content
		Arbeitsfeld(i)=tmp
	end do
	!create outputfile
	open(unit=22+n,file='ziel.dat',action='write')
	!------ Phase 2 ------
	call sort(Arbeitsfeld)
	!------ Phase 3 ------
	i=1
	do io_error=1,n
		write(*,*) Arbeitsfeld(io_error)%filenumber,Arbeitsfeld(io_error)%content
	end do
	do while (i<=n)
		write(22+n,*) Arbeitsfeld(i)%content
		read(Arbeitsfeld(i)%filenumber,*,iostat=io_error) Arbeitsfeld(i)%content
		!end of file?
		if (io_error/=0) then
			i=i+1
		else
			call insert(Arbeitsfeld(i:n),Arbeitsfeld(i))
		end if
	end do
end program mergefiles


