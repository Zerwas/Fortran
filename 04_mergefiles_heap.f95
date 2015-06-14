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

	subroutine Build_Heap(list)
		type(filecomp),dimension(:),intent(inout)::list
		integer::levels,level,parent,parentId,i
		!determine number of levels in heap
		i=1
		levels=0
		do while (i<size(list))
			i=i*2
			levels=levels+1
		end do
		write(*,*) levels
		parentId=1
		do level=levels,1,-1
			do parent=0,level
				call Heapify(list,level,parent,parentId)
				parentId=parentId+1
			end do
		end do
	end subroutine Build_Heap

	subroutine Heapify(list,level,parent,parentId)
		type(filecomp),dimension(:),intent(inout)::list
		integer::parentId,leftChildId,level,parent
		type(filecomp)::tmp
		leftChildId=2*parentId
		!tausche rechtes Kind falls es existiert und größer ist als das linke Kind und Parent
 		if (leftChildId+1<size(list) .and. list(parentId)<=list(leftChildId+1) .and. list(leftChildId)<=list(leftChildId+1)) then
			!create new element so its not overwritten
			tmp%content=list(parentId)%content
			tmp%filenumber=list(parentId)%filenumber
			list(parentId)=list(leftChildId+1)
			list(leftChildId+1)=tmp
 			call sink(list,level+1,parent+1,leftChildId+1)
		end if

 		!tausche linkes Kind falls es existiert und größer ist
		if (leftChildId<size(list) .and. list(parentId)<=list(leftChildId)) then
			tmp%content=list(parentId)%content
			tmp%filenumber=list(parentId)%filenumber
			list(parentId)=list(leftChildId)
			list(leftChildId)=tmp
			call sink(list,level+1,parent,leftChildId)
		end if
	end subroutine Heapify

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
	call Build_Heap(Arbeitsfeld)

	!------ Phase 3 ------
	i=1
	do while (i<=n)
		write(22+n,*) Arbeitsfeld(i)%content
		read(Arbeitsfeld(i)%filenumber,*,iostat=io_error) Arbeitsfeld(i)%content
		!end of file?
		if (io_error/=0) then
			i=i+1
		else
			call Heapyfi(Arbeitsfeld(1:n-i+1),0,0,1)
		end if
	end do
end program mergefiles


