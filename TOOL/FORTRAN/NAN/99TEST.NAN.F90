! https://gcc.gnu.org/onlinedocs/gcc-4.7.4/gfortran/ISNAN.html
program test_nan
  implicit none
  real :: x
  x = -1.0
  x = sqrt(x)
  if (isnan(x)) stop '"x" is a NaN'
end program test_nan

