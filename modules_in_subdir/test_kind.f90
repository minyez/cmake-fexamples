program test_kind
  use mykind
  implicit none
  type (kind_collect) :: kc
  print*, "double kind:", kc%kind_double
  print*, "  real kind:", kc%kind_real
  stop
end program test_kind
