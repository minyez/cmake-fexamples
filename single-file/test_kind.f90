program test_kind
  implicit none
  integer :: i, ir, jr
  integer,parameter :: k15 = selected_real_kind(15)
  real(k15) :: fk15 = 1.0_k15
  i = kind(1.0d0)
  ir = range(1.0d0)
  jr = range(fk15)
  print*, i, ir, k15, jr
end program test_kind
