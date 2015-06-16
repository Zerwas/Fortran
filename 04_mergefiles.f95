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
		do sep=size(list)-1,1,-1
			call insert(list(sep:size(list)),list(sep))
		end do
	end subroutine sort

	subroutine insert(list,elem)
		type(filecomp),dimension(:),intent(inout)::list
		type(filecomp),intent(in)::elem
		integer::i,l,r
		type(filecomp)::tmp
		!create new element so its not overwritten
		tmp%content=elem%content
		tmp%filenumber=elem%filenumber
		i=2
		l=1
		r=size(list)
		!binary search
		do while (l/=r)
			if (list((l+r+1)/2)<=tmp) then
				l=(l+r+1)/2
			else
				r=(l+r+1)/2-1
			end if
		end do
		
		do while (i<=l)
			list(i-1)=list(i)
			i=i+1
		end do
		list(l)=tmp
	end subroutine insert

end module insertmerge


program mergefiles
	use insertmerge
	implicit none
	type(filecomp),dimension(:),allocatable::Arbeitsfeld
	type(filecomp)::tmp
	integer::n,i,io_error,h,h2
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
	!close files
	do i=1,n+1
		close(21+i)
	end do
	!test whether ziel.dat is sorted
	open(22,file='ziel.dat',action='read')
	read(22,*,iostat=io_error) h
	do while(io_error==0)
		read(22,*,iostat=io_error) h2
		if (h>h2) write(*,*) 'error'
	end do
	close(22)
end program mergefiles


