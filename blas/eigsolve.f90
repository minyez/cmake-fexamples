module eigsolve
contains
subroutine syeig(jobz, uplo, n, a, lda, w)
  character,intent(in) :: jobz,uplo
  integer,intent(in)   :: n, lda
  real(8),intent(inout) :: a(lda,*)
  real(8),intent(out) :: w(n)

  real(8),allocatable :: work(:)
  real(8),allocatable :: rwork(:)

  integer :: lwork,info
  external :: ilaenv,dsyev
  integer :: ilaenv

  info = 0
  lwork = ilaenv(1,"dsytrd",uplo,n,-1,-1,-1)
  if(lwork.le.1) then
    lwork=max(1,3*n-1)
  else
    lwork=n*(2+lwork)
  endif
  allocate(work(lwork),rwork(3*n-2))
  call dsyev(jobz, uplo, n, a, lda, w, work, lwork, rwork, info)
  deallocate(work,rwork)
  if(info.ne.0)then
    write(*,"(A20)") "dsyev error in gesolve"
    stop 1
  endif
end subroutine

subroutine solve(trans, a, b)
  character,intent(in) :: trans
  real(8),intent(inout) :: b(:,:)
  real(8),intent(inout) :: a(:,:)

  integer :: m, n, lda, nrhs, ldb, info
  integer,allocatable :: ipiv(:)
  external :: dgetrf,dgetrs
  info = 0
  m = size(a,1)
  n = size(a,2)
  lda = m
  ldb = size(b,1)
  nrhs = size(b,2)
  ! factorize
  allocate(ipiv(min(m,n)))
  call dgetrf(m, n, a, lda, ipiv, info)
  if(info.ne.0)then
    deallocate(ipiv)
    write(*,"(A20)") "dgetrf error in gesolve"
    stop 1
  endif
  ! inverse
  call dgetrs(trans, n, nrhs, awork, lda, ipiv, b, ldb, info)
  deallocate(ipiv)
  if(info.ne.0)then
    write(*,"(A20)") "dgetrs error in gesolve"
    stop 1
  endif
end subroutine

end module eigsolve
