program ftest_kmeans_valid_monotone_inertia
   use mod_kinds
   use mod_kmeans, only : kmeans, new_kmeans
   use mod_random_seed, only : fix_random_seed
   implicit none
   integer(i64), parameter :: n=300, d=3, k=3, SEED=777_i64
   real(r64),    allocatable :: X(:,:), inert(:)
   type(kmeans)  :: km
   integer(i64)       :: m, nstep=10

   allocate(X(n,d), inert(nstep))
   call fix_random_seed(SEED); call random_number(X)

   do m = 1, nstep
      km = new_kmeans(k, random_state=SEED, max_iter=m)
      call km%fit(X)
      inert(m) = km%best_score          ! ← 既存 public 変数／getter を想定
   end do

   if (any( inert(2:) > inert(:nstep-1) )) error stop 99
end program
