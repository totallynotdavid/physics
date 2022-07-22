program dejadepedirelcodigoplis
  implicit none
  real :: T(9), a(9), logT(9), loga(9)
  integer :: i
  T = (/.241, .615, 1., 1.88, 11.86, 29.5, 84., 165., 248. /)
  a = (/ .387, .723, 1., 1.523, 5.202, 9.539, 19.18, 30.06, 39.44 /)
  logT= log(T)
  loga= log(a)
  open(file="TerceraKepler.txt", unit=7)
  do i=1,9
    write(7,*) logT(i), loga(i)
  end do
end program