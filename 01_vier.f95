module game
  implicit none

  contains
  subroutine displayadv(f,m,n)
    use Grafikmod
    integer, dimension(:,:)::f
    integer::n,m,i,j,k,ID
    REAL, DIMENSION(1:40):: XC, YC
    REAL, PARAMETER :: Tau = 6.283185307
    do k=1,40
      XC(k)=-0.5+SIN(Tau*k/40)/2
      YC(k)=-0.5+COS(Tau*k/40)/2
    enddo
    ID= PG_OPEN(DEVICE = '/XWINDOW')
    !CALL PG_SET_LINE_WIDTH(LW= 4)
    CALL PG_ENVIRONMENT(xmin= REAL(0), xmax= REAL(n), ymin= REAL(0), ymax= REAL(m), JUST= 1, AXIS= 2) !Axis=-2?
    do j=m,1,-1
      do i=1,n
        if (f(j,i)==1) then
          CALL PG_SET_COLOR_INDEX(2) !0 to 15
          CALL PG_PUT_POLYGON(40,i+XC,j+YC)
        end if
        if (f(j,i)==-1) then
          CALL PG_SET_COLOR_INDEX(1) !0 to 15
          CALL PG_PUT_POLYGON(40,i+XC,j+YC)
        end if
      end do
    end do
    CALL PG_CLOSE()
    !deallocate(f)!?
  end subroutine displayadv

  subroutine display(f,m,n)
    integer, dimension(:,:)::f
    integer::n,m,i,j
    character(len=n)::l,border
!    character,dimension(-1:1)::sign=(/'o',' ','x'/)
    do i=1,n
      border(i:)='-'
    end do
    write(*,*) '-'//border//'-'
    l=''
    do j=m,1,-1
      do i=1,n
        if (f(j,i)==1) then
!write(*,fmt='(A1,A1)',advance='no') '|',sign(f(j,i))
          l(i:)='X'
        end if
        if (f(j,i)==-1) then
          l(i:)='O'
        end if
      end do
!write(*,*) '|'
      write(*,*) '|'//l//'|'
      l=''
    end do
    write(*,*) '-'//border//'-'
  end subroutine display

  integer function Gewinn_Nr(f,m,n)
    integer, dimension(:,:)::f
    integer::i,j,summ,m,n
    Gewinn_Nr=0
    !horizontal
    do j=1,n
!abs(sum(f(j,1:4)))
      do i=1,m-3
        if (abs(sum(f(j,i:i+3)))==4) then
          Gewinn_Nr=f(j,i)
        end if
      end do
    end do
    !vertical
    do i=1,m
      do j=1,n-3
        if (abs(sum(f(j:j+3,i)))==4) then
          Gewinn_Nr=f(j,i)
        end if
      end do
    end do
    !diagonal
    do i=1,m-3
      do j=1,n-3
        summ=f(j,i)+f(j+1,i+1)+f(j+2,i+2)+f(j+3,i+3)
!abs(sum((/(f(j+k,i+k),k=0,3)/)))
        if (summ==4) then
          Gewinn_Nr=1
        end if
        if (summ==-4) then
          Gewinn_Nr=-1
        end if
      end do
    end do
    !deallocate(f)?
  end function Gewinn_Nr
end module game

program vier
  use game
  implicit none
  integer::n,m,i,j,pl,column
  integer, dimension(:,:),allocatable:: field
  integer, dimension(:),allocatable:: columns
  logical:: wrongnumber
  
  do
    write(*,*) 'Enter Field size'
    read(*,*) m,n
    if (n<0.or.m<0) then
      exit
    endif
    allocate(field(m,n))
    allocate(columns(n))
    !init field
    do i=1,n
      columns(i)=0
      do j=1,m
        field(j,i)=0
      end do
    end do
    pl=-1
    do
      !get move
      call display(field,m,n)
      call displayadv(field,m,n)
      wrongnumber=.True.
      write(*,*) 'Turn of player ',pl,'.'
      do while (wrongnumber)
        read(*,*) column
        if (column>n.or.column<1) then
          write(*,*) 'Number not in field.'
        else
          wrongnumber=columns(column)==m
        endif
      enddo
      !make move
      columns(column)=columns(column)+1
      field(columns(column),column)=pl
      pl=-pl
      !game over?
      if (sum(columns)==n*m) then
        exit
      endif
      if (Gewinn_Nr(field,m,n)/=0) then
        exit
      endif
    enddo
    !display end msg
    call display(field,m,n)
    !call displayadv(field,m,n)
    if (Gewinn_Nr(field,m,n)==0) then
      write(*,*) 'Draw.'
    else
      write(*,fmt='(A6,1X,I2,1X,A4)') 'Player',Gewinn_Nr(field,m,n),'won.'
    endif
    deallocate(field)
    deallocate(columns)
  enddo
  
end program vier
