program triangulo
implicit none
integer :: i, n
real*8  :: vertic, v2, L, h, lh, hh
real*8,dimension(2) :: ini,now
n  = 20000
L  = 10.0
lh = 0.5*L
h  = 0.57*lh
hh = 0.5*h
ini(1)= lh
ini(2)= hh

open(unit=11,file="triangulo.dat",status="replace",action="write")
do i=1,n
 call random_number (vertic)
 vertic = vertic*3.0
 write(*,*)vertic
 if (vertic <= 1.0) then
  now(1)= 0.5*(ini(1))
  now(2)= 0.5*(ini(2))
 else if (vertic <= 2.0.and.vertic > 1.0) then
  now(1)= 0.5*abs(l + ini(1))
  now(2)= 0.5*(ini(2))
 else
  now(1)= 0.5*abs(lh + ini(1))
  now(2)= 0.5*abs(h  + ini(2))
 endif
 write(11,*)now(1),now(2)
 ini = now
enddo
close(11)

end
