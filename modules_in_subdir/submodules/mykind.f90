module mykind
  implicit none
  type kind_collect
    integer :: kind_double = kind(1.0d0)
    integer :: kind_real = kind(1.0e0)
  end type
end module mykind
