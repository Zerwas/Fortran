module queumod
	implicit none

	type node
		integer::content
		type(node),pointer::next
	end type node
	
	type queue
		type(node),pointer::head=>null(),tail=>null()
		integer::length=0
	end type queue

	contains

	logical function empty(q)
		type(queue)::q
		empty=associated(q%head)
	end function empty

	subroutine enqueue(q,content)
		type(queue)::q
		type(node),pointer::new
		integer::content
		allocate(new)
		new%content=content
		if (q%length==0) then
			q%head=>new
			q%tail=>new
		else
			q%tail%next=>new
			q%tail=>new
		endif
		q%length=q%length+1
	end subroutine enqueue

	subroutine dequeue(q)
		type(queue)::q
		type(node),pointer::tmp
		if (q%length==0) then
			write(*,*) 'Die Warteschlange ist leer!'
		else
			!remove first element
			q%length=q%length-1
			tmp=>q%head
			q%head=>q%head%next
			!necessary?
			if (q%length==0) then 
				q%tail=>null()
			endif
			deallocate(tmp)
		endif
	end subroutine dequeue

	subroutine PUT(q)
		type(queue)::q
		type(node),pointer::tmp
		integer::n
		if (q%length==0) then
			write(*,*) 'Die Warteschlange ist leer!'
		else
			tmp=>q%head
			write(*,fmt='(I3)',advance='no') tmp%content
			tmp=>tmp%next
			do n=2,q%length
				write(*,fmt='(A1,I3)',advance='no') ',',tmp%content
				tmp=>tmp%next
			enddo
			!newline
			write(*,*)
		endif
	end subroutine PUT

end module queumod

program warteschlange
	use queumod
	implicit none
	type(queue),dimension(:),allocatable::queues
	integer::n,time,j
	real::w,h,content
	logical::output
	call RANDOM_SEED()
	!initialise queues
	write(*,*) 'Anzahl der Kassen:'
	read(*,*) n
	allocate(queues(n))
	write(*,*) 'Wahrscheinlichkeit für neuen Kunden:'
	read(*,*) w
	!start simulation
	do time=1,12*3600
		output=.false.
		call RANDOM_NUMBER(h)
		if (h<w) then
			output=.true.
			!add customer to shortest queue
			call RANDOM_NUMBER(content)
			!DIM=1 so return value of minloc is integer not array of integers
			call enqueue(queues(minloc(queues (:) %length,DIM=1)),floor(content*291+10))
		endif
		!serve customers
		do j=1,n
			if (queues(j)%length>0) then
				queues(j)%head%content=queues(j)%head%content-1
				if (queues(j)%head%content==0) then
					output=.true.
					call dequeue(queues(j))					
				endif
			endif
		enddo
		if (output) then
			!print current state
			write(*,fmt='(A14,I2,A1,I2,A1,I2)') 'Aktuelle Zeit:',time/3600,':',mod(time,3600)/60,':',mod(time,60)
			do j=1,n
				write(*,fmt='(A14,I2)') 'Warteschlange:',j
				call PUT(queues(j))
			enddo
		endif
	enddo
end program warteschlange


