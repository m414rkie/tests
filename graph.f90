program graph

implicit none
	character*15		:: filename
	real				:: f, g, x, y, xx, yy
	real				:: lowx, highx, lowy, highy, dx, dy
	real, parameter		:: pi = acos(-1.0)
	integer				:: i, j
	integer				:: numx, numy
	character*1			:: flag, parsed
	
	
! Equations that can be graphed
f(x) = pi*(x**2)/(1.0-x)

g(x,y) = (5*x**2 + y**2)**(0.5)

! Formatting statement. 
50 format ("graphof",1a1,".dat")	

! Initial user input
15 write(*,*) "Enter F to graph f(x) or G to graph g(x,y)"
read(*,*) flag

! Input parsing
call chartoup(flag,parsed)

! error checking
if ((parsed .ne. "F") .and. (parsed .ne. "G")) then
	write(*,*) "Input not recognized."
	goto 15
end if

! User input continued
if (parsed .eq. "F") then
	
	write(*,*) "Input lower bound of x:"
	read(*,*) lowx
	write(*,*) "Input upper bound of x:"
	read(*,*) highx
	write(*,*) "Input stepsize:"
	read(*,*) dx
	
	numx = ceiling((highx-lowx)/dx)
	
else if (parsed .eq. "G") then
	
	write(*,*) "Input lower bound of x:"
	read(*,*) lowx
	write(*,*) "Input upper bound of x:"
	read(*,*) highx
	write(*,*) "Input stepsize in x:"
	read(*,*) dx
	write(*,*) "Input lower bound of y:"
	read(*,*) lowy
	write(*,*) "Input upper bound of y:"
	read(*,*) highy
	write(*,*) "Input stepsize in y:"
	read(*,*) dy

	numx = ceiling((highx-lowx)/dx)
	numy = ceiling((highy-lowy)/dy)
	
end if

! Calculations
if (parsed .eq. "F") then
	
	write(filename,50) trim(parsed)
	
	open(unit=15,file=filename,status="replace",position="append")
	
	do i = 1, numx, 1
		
		xx = (real(i)*dx + lowx)
		
		write(15,*) xx, f(xx)

	end do

	close(15)

else if (parsed .eq. "G") then
	
	write(filename,50) trim(parsed)
	
	open(unit=15,file=filename,status="replace",position="append")
	
	do i = 1, numx, 1
		
		xx = (real(i)*dx + lowx)
		
		do j = 1, numy, 1
		
			yy = (real(j)*dy + lowy)	
		
			write(15,*) xx, yy, g(xx,yy)
		
		end do

	end do

	close(15)
	
end if
	
	
end program

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine chartoup(stringin,stringout)

! converts text input to upper case

implicit none
	character(*)					:: stringin
	character(len(stringin))		:: stringout
	integer							:: i, j

do i = 1, len(stringin), 1
	j = iachar(stringin(i:i))
		if(j .ge. iachar("a") .and. j .le. iachar("z")) then
			stringout(i:i) = achar(iachar(stringin(i:i))-32)
		else
			stringout(i:i) = stringin(i:i)
		end if
end do

end subroutine