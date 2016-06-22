!  combo_prb.f90  24 July 2000
!
program combo_prb
!
!*******************************************************************************
!
!! COMBO_PRB calls the COMBO tests.
!
  character ( len = 8 ) date
  character ( len = 10 ) time
!
  call date_and_time ( date, time )

  write ( *, * ) ' '
  write ( *, * ) 'COMBO_PRB'
  write ( *, * ) '  Tests for the COMBO combinatorial routines.'
  write ( *, * ) ' '
  write ( *, * ) '  Today''s date: ', date
  write ( *, * ) '  Today''s time: ', time
 
  call test01
  call test02
  call test03
  call test04
  call test05
  call test06
  call test07
  call test08
  call test09
  call test094
  call test095
  call test10

  call test11
  call test12
  call test13
  call test14
  call test15
  call test16
  call test17
  call test18
  call test185
  call test186
  call test19
  call test20

  call test21
  call test22
  call test23
  call test24
  call test25
  call test26
  call test27
  call test28
  call test29
  call test30

  call test31
  call test32
  call test33

  write ( *, * ) ' '
  write ( *, * ) 'COMBO_PRB'
  write ( *, * ) '  Normal end of the COMBO tests.'

  stop
end
subroutine test01
!
!*******************************************************************************
!
!! TEST01 tests BAL_SEQ_ENUM;
!! TEST01 tests BAL_SEQ_RANK;
!! TEST01 tests BAL_SEQ_SUCCESSOR;
!! TEST01 tests BAL_SEQ_UNRANK.
!
  integer, parameter :: n = 5
!
  integer nseq
  integer rank
  integer rankold
  integer t(2*n)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST01'
  write ( *, * ) '  Balanced sequences:'
  write ( *, * ) ' '
  write ( *, * ) '  BAL_SEQ_ENUM enumerates,'
  write ( *, * ) '  BAL_SEQ_RANK ranks,'
  write ( *, * ) '  BAL_SEQ_SUCCESSOR lists,'
  write ( *, * ) '  BAL_SEQ_UNRANK unranks.'
!
!  Enumerate.
!
  call bal_seq_enum ( n, nseq )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of balanced sequences is ', nseq
  write ( *, * ) ' '
!
!  List.
!
  rank = -1

  do

    rankold = rank

    call bal_seq_successor ( n, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(4x,i3,2x,10i2)' ) rank, t(1:2*n)

  end do
!
!  Unrank.
!
  rank = nseq / 2

  call bal_seq_unrank ( rank, n, t )

  write ( *, * ) ' '
  write ( *, * ) '  The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(4x,10i2)' ) t(1:2*n)
!
!  Rank.
!
  call bal_seq_rank ( n, t, rank )

  call ivec_print ( 2*n, t, '  Element to be ranked:' )

  write ( *, * ) ' '
  write ( *, * ) '  Computed rank: ', rank

  return
end
subroutine test02
!
!*******************************************************************************
!
!! TEST02 tests BAL_SEQ_TO_TABLEAU;
!! TEST02 tests TABLEAU_TO_BAL_SEQ.
!
  integer, parameter :: n = 4
!
  integer i
  integer rank
  integer t(2*n)
  integer tab(2,n)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST02'
  write ( *, * ) '  BAL_SEQ_TO_TABLEAU converts a balanced'
  write ( *, * ) '    sequence to a tableau;'
  write ( *, * ) '  TABLEAU_TO_BAL_SEQ converts a tableau'
  write ( *, * ) '    to a balanced sequence.'
!
!  Pick a "random" balanced sequence.
!
  rank = 7

  call bal_seq_unrank ( rank, n, t )

  write ( *, * ) ' '
  write ( *, * ) '  "Random" balanced sequence:'
  write ( *, * ) ' '
  write ( *, '(4x,8i2)' ) t(1:2*n)
!
!  Convert to a tableau.
!
  call bal_seq_to_tableau ( n, t, tab )

  write ( *, * ) ' '
  write ( *, * ) '  Corresponding tableau'
  write ( *, * ) ' '
  write ( *, '(4x,4i2)' ) tab(1,1:n)
  write ( *, '(4x,4i2)' ) tab(2,1:n)
!
!  Convert to a balanced sequence.
!
  call tableau_to_bal_seq ( n, tab, t )

  call ivec_print ( 2*n, t, '  Corresponding balanced sequence:' )

  return
end
subroutine test03
!
!*******************************************************************************
!
!! TEST03 tests BELL_NUMBERS.
!
  integer, parameter :: m = 10
!
  integer b(0:m)
  integer i
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST03'
  write ( *, * ) '  BELL_NUMBERS computes Bell numbers.'
  write ( *, * ) ' '

  call bell_numbers ( m, b )

  do i = 0, m
    write ( *, '(2i8)' ) i, b(i)
  end do

  return
end
subroutine test04
!
!*******************************************************************************
!
!! TEST04 tests BINOMIAL.
!
  integer binomial
  integer i
  integer j
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST04'
  write ( *, * ) '  BINOMIAL computes binomial coefficients.'

  do i = -1, 5
    do j = -1, 5
      write ( *, '(3i8)' ) i, j, binomial ( i, j )
    end do
  end do

  return
end
subroutine test05
!
!*******************************************************************************
!
!! TEST05 tests CYCLE_TO_PERM;
!! TEST05 tests PERM_TO_CYCLE.
!
  integer, parameter :: n = 7
!
  integer i
  integer j
  integer jlo
  integer index(n)
  integer ncycle
  integer nperm
  integer p(n)
  integer rank
  integer t(n)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST05'
  write ( *, * ) '  CYCLE_TO_PERM converts a permutation from'
  write ( *, * ) '    cycle to array form;'
  write ( *, * ) '  PERM_TO_CYCLE converts a permutation from'
  write ( *, * ) '    array to cycle form.'
!
!  Enumerate.
!
  call perm_enum ( n, nperm )
!
!  Choose a "random" RGF.
!
  rank = nperm / 2

  call perm_lex_unrank ( rank, n, p )

  write ( *, * ) ' '
  write ( *, * ) '  "Random" permutation:'
  write ( *, * ) ' '
  write ( *, '(4x,8i2)' ) p(1:n)
!
!  Convert the permutation to cycle form.
!
  call perm_to_cycle ( n, p, ncycle, t, index )

  write ( *, * ) ' '
  write ( *, * ) '  Corresponding cycle form:'
  write ( *, * ) '  Number of cycles is ', ncycle
  write ( *, * ) ' '
  jlo = 0
  do i = 1, ncycle
    write ( *, '(4x,20i4)' ) t(jlo+1:jlo+index(i))
    jlo = jlo + index(i)
  end do
!
!  Convert the set partition back to an RGF.
!
  call cycle_to_perm ( n, ncycle, t, index, p )

  write ( *, * ) ' '
  write ( *, * ) '  Corresponding permutation:'
  write ( *, * ) ' '
  write ( *, '(4x,8i2)' ) p(1:n)

  return
end
subroutine test06
!
!*******************************************************************************
!
!! TEST06 tests DIST_ENUM;
!! TEST06 tests DIST_NEX.
!
  integer, parameter :: k = 3
!
  integer i
  integer idist
  integer m
  logical more
  integer num_dist
  integer q(k)
!
  m = 5
  more = .false.

  call dist_enum ( k, m, num_dist )

  write ( *, * ) ' '
  write ( *, * ) 'TEST06'
  write ( *, * ) '  For a distribution of M indistinguishable'
  write ( *, * ) '  objects among K distinguishable slots:'
  write ( *, * ) ' '
  write ( *, * ) '  DIST_ENUM enumerates them;'
  write ( *, * ) '  DIST_NEX produces the "next" one.'
  write ( *, * ) ' '
  write ( *, * ) '  Number of:'
  write ( *, * ) '    indistinguishable objects = ', m
  write ( *, * ) '    distinguishable slots =     ', k
  write ( *, * ) '    distributions is            ', num_dist
  write ( *, * ) ' '

  idist = 0

  do

    call dist_nex ( k, m, q, more )

    if ( .not. more ) then
      exit
    end if

    idist = idist + 1
    write ( *, '(4x,6i5)' ) idist, q(1:k)

  end do

  return
end
subroutine test07
!
!*******************************************************************************
!
!! TEST07 tests GRAY_CODE_ENUM;
!! TEST07 tests GRAY_CODE_RANK;
!! TEST07 tests GRAY_CODE_SUCCESSOR;
!! TEST07 tests GRAY_CODE_UNRANK.
!
  integer, parameter :: n = 5
!
  integer j
  integer ngray
  integer rank
  integer rankold
  integer t(n)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST07'
  write ( *, * ) '  Gray codes:'
  write ( *, * ) ' '
  write ( *, * ) '  GRAY_CODE_ENUM enumerates,'
  write ( *, * ) '  GRAY_CODE_RANK ranks,'
  write ( *, * ) '  GRAY_CODE_SUCCESSOR lists,'
  write ( *, * ) '  GRAY_CODE_UNRANK unranks.'
!
!  Enumerate.
!
  call gray_code_enum ( n, ngray )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of Gray code elements is ', ngray
  write ( *, * ) ' '
!
!  List
!
  rank = -1

  do

    rankold = rank

    call gray_code_successor ( n, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(4x,6i5)' ) rank, t(1:n)
  
  end do
!
!  Unrank.
!
  rank = ngray / 2

  call gray_code_unrank ( rank, n, t )

  write ( *, * ) ' '
  write ( *, * ) '  The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(4x,6i5)' ) t(1:n)
!
!  Rank.
!
  call gray_code_rank ( n, t, rank )

  call ivec_print ( n, t, '  Element to be ranked:' )

  write ( *, * ) ' '
  write ( *, * ) '  Computed rank: ', rank

  return
end
subroutine test08
!
!*******************************************************************************
!
!! TEST08 tests IVEC_SEARCH_BINARY_A;
!! TEST08 tests IVEC_SORT_INSERT_A.
!
  integer, parameter :: n = 10
!
  integer a(n)
  integer b
  integer i
  integer index
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST08'
  write ( *, * ) '  Integer vectors:'
  write ( *, * ) ' '
  write ( *, * ) '  IVEC_SORT_INSERT_A ascending sorts;'
  write ( *, * ) '  IVEC_SEARCH_BINARY_A searches a ascending sorted vector.'

  a(1:n) = (/ 6, 7, 1, 0, 4, 3, 2, 1, 5, 8 /)

  call ivec_print ( n, a, '  Before ascending sort:' )

  call ivec_sort_insert_a ( n, a )

  call ivec_print ( n, a, '  After ascending sort:' )

  b = 5

  write ( *, * ) ' '
  write ( *, * ) '  Now search for an instance of the value ', b

  call ivec_search_binary_a ( n, a, b, index )

  write ( *, * ) ' '
  if ( index == 0 ) then
    write ( *, * ) '  The value does not occur.'
  else
    write ( *, * ) '  The value occurs at index = ', index
  end if

  return
end
subroutine test09
!
!*******************************************************************************
!
!! TEST09 tests IVEC_SEARCH_BINARY_D;
!! TEST09 tests IVEC_SORT_INSERT_D.
!
  integer, parameter :: n = 10
!
  integer a(n)
  integer b
  integer i
  integer index
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST09'
  write ( *, * ) '  Integer vectors:'
  write ( *, * ) ' '
  write ( *, * ) '  IVEC_SORT_INSERT_D descending sorts;'
  write ( *, * ) '  IVEC_SEARCH_BINARY_D searches a descending sorted vector.'

  a(1:n) = (/ 6, 7, 1, 0, 4, 3, 2, 1, 5, 8 /)

  call ivec_print ( n, a, '  Before descending sort:' )

  call ivec_sort_insert_d ( n, a )

  call ivec_print ( n, a, '  After descending sort:' )

  b = 5

  write ( *, * ) ' '
  write ( *, * ) '  Now search for an instance of the value ', b

  call ivec_search_binary_d ( n, a, b, index )

  write ( *, * ) ' '
  if ( index == 0 ) then
    write ( *, * ) '  The value does not occur.'
  else
    write ( *, * ) '  The value occurs at index = ', index
  end if

  return
end
subroutine test094
!
!*******************************************************************************
!
!! TEST094 tests KNAPSACK_REORDER.
!! TEST094 tests KNAPSACK_01.
!
  integer, parameter :: n = 5
!
  integer i
  real :: mass
  real :: mass_limit = 26.0
  real, dimension ( n ) :: p = (/ 24.0, 13.0, 23.0, 15.0, 16.0 /)
  real :: profit
  real, dimension ( n ) :: w = (/ 12.0,  7.0, 11.0,  8.0,  9.0 /)
  real, dimension ( n ) :: x
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST094'
  write ( *, * ) '  KNAPSACK_REORDER reorders the knapsack data.'
  write ( *, * ) '  KNAPSACK_01 solves the 0/1 knapsack problem.'

  write ( *, * ) ' '
  write ( *, * ) '  Object, Profit, Mass, "Profit Density"'
  write ( *, * ) ' '
  do i = 1, n
    write ( *, '(i6,3f7.3)' ) i, p(i), w(i), p(i)/w(i)
  end do

  call knapsack_reorder ( n, p, w )

  write ( *, * ) ' '
  write ( *, * ) '  After reordering by Profit Density:'
  write ( *, * ) ' '
  write ( *, * ) '  Object, Profit, Mass, "Profit Density"'
  write ( *, * ) ' '
  do i = 1, n
    write ( *, '(i6,3f7.3)' ) i, p(i), w(i), p(i)/w(i)
  end do

  write ( *, * ) ' '
  write ( *, * ) '  Total mass restriction is ', mass_limit

  call knapsack_01 ( n, mass_limit, p, w, x, mass, profit )

  write ( *, * ) ' '
  write ( *, * ) '  Object, Density, Choice, Profit, Mass'
  write ( *, * ) ' '
  do i = 1, n
    write ( *, '(i6,f7.3,f7.3,2f7.3)' ) i, p(i)/w(i), x(i), &
      x(i) * p(i), x(i) * w(i)
  end do

  write ( *, * ) ' '
  write ( *, '(a,2f7.3)' ) '  Total:            ', profit, mass

  return
end
subroutine test095
!
!*******************************************************************************
!
!! TEST095 tests KNAPSACK_REORDER.
!! TEST095 tests KNAPSACK_RATIONAL.
!
  integer, parameter :: n = 5
!
  integer i
  real :: mass
  real :: mass_limit = 26.0
  real, dimension ( n ) :: p = (/ 24.0, 13.0, 23.0, 15.0, 16.0 /)
  real :: profit
  integer rank(n)
  real, dimension ( n ) :: w = (/ 12.0,  7.0, 11.0,  8.0,  9.0 /)
  real, dimension ( n ) :: x
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST095'
  write ( *, * ) '  KNAPSACK_REORDER reorders the knapsack data.'
  write ( *, * ) '  KNAPSACK_RATIONAL solves the rational knapsack problem.'

  write ( *, * ) ' '
  write ( *, * ) '  Object, Profit, Mass, "Profit Density"'
  write ( *, * ) ' '
  do i = 1, n
    write ( *, '(i6,3f7.3)' ) i, p(i), w(i), p(i)/w(i)
  end do

  call knapsack_reorder ( n, p, w )

  write ( *, * ) ' '
  write ( *, * ) '  After reordering by Profit Density:'
  write ( *, * ) ' '
  write ( *, * ) '  Object, Profit, Mass, "Profit Density"'
  write ( *, * ) ' '
  do i = 1, n
    write ( *, '(i6,3f7.3)' ) i, p(i), w(i), p(i)/w(i)
  end do

  write ( *, * ) ' '
  write ( *, * ) '  Total mass restriction is ', mass_limit

  call knapsack_rational ( n, mass_limit, p, w, x, mass, profit )

  write ( *, * ) ' '
  write ( *, * ) '  Object, Density, Choice, Profit, Mass'
  write ( *, * ) ' '
  do i = 1, n
    write ( *, '(i6,f7.3,f7.3,2f7.3)' ) i, p(i)/w(i), x(i), &
      x(i) * p(i), x(i) * w(i)
  end do

  write ( *, * ) ' '
  write ( *, '(a,2f7.3)' ) '  Total:            ', profit, mass

  return
end
subroutine test10
!
!*******************************************************************************
!
!! TEST10 tests KSUBSET_COLEX_RANK;
!! TEST10 tests KSUBSET_COLEX_SUCCESSOR;
!! TEST10 tests KSUBSET_COLEX_UNRANK;
!! TEST10 tests KSUBSET_ENUM.
!
  integer, parameter :: k = 3
!
  integer j
  integer n
  integer nksub
  integer rank
  integer rankold
  integer t(k)
!
  n = 5

  write ( *, * ) ' '
  write ( *, * ) 'TEST10'
  write ( *, * ) '  K-subsets of an N set,'
  write ( *, * ) '  using the colexicographic ordering:'
  write ( *, * ) ' '
  write ( *, * ) '  KSUBSET_COLEX_RANK ranks,'
  write ( *, * ) '  KSUBSET_COLEX_SUCCESSOR lists,'
  write ( *, * ) '  KSUBSET_COLEX_UNRANK unranks.'
  write ( *, * ) '  KSUBSET_ENUM enumerates,'
!
!  Enumerate.
!
  call ksubset_enum ( k, n, nksub )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of K subsets is ', nksub
  write ( *, * ) ' '
!
!  List
!
  rank = -1

  do

    rankold = rank

    call ksubset_colex_successor ( k, n, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(4x,6i5)' ) rank, t(1:k)

  end do
!
!  Unrank.
!
  rank = nksub / 2

  call ksubset_colex_unrank ( rank, k, n, t )

  write ( *, * ) ' '
  write ( *, * ) '  The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(4x,6i5)' ) t(1:k)
!
!  Rank.
!
  call ksubset_colex_rank ( k, n, t, rank )

  write ( *, * ) ' '
  write ( *, * ) '  The rank of the element:'
  write ( *, * ) ' '
  write ( *, '(4x,6i5)' ) t(1:k)
  write ( *, * ) ' '
  write ( *, * ) '  is computed as ', rank

  return
end
subroutine test11
!
!*******************************************************************************
!
!! TEST11 tests KSUBSET_ENUM;
!! TEST11 tests KSUBSET_LEX_RANK;
!! TEST11 tests KSUBSET_LEX_SUCCESSOR;
!! TEST11 tests KSUBSET_LEX_UNRANK.
!
  integer, parameter :: k = 3
!
  integer j
  integer n
  integer nksub
  integer rank
  integer rankold
  integer t(k)
!
  n = 5

  write ( *, * ) ' '
  write ( *, * ) 'TEST11'
  write ( *, * ) '  K-subsets of an N set,'
  write ( *, * ) '  using the lexicographic ordering:'
  write ( *, * ) ' '
  write ( *, * ) '  KSUBSET_ENUM enumerates,'
  write ( *, * ) '  KSUBSET_LEX_RANK ranks,'
  write ( *, * ) '  KSUBSET_LEX_SUCCESSOR lists,'
  write ( *, * ) '  KSUBSET_LEX_UNRANK unranks.'
!
!  Enumerate.
!
  call ksubset_enum ( k, n, nksub )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of K subsets is ', nksub
  write ( *, * ) ' '
!
!  List
!
  rank = -1

  do

    rankold = rank

    call ksubset_lex_successor ( k, n, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:k)

  end do
!
!  Unrank.
!
  rank = nksub / 2

  call ksubset_lex_unrank ( rank, k, n, t )

  write ( *, * ) ' '
  write ( *, * ) '  The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(6i5)' ) t(1:k)
!
!  Rank.
!
  call ksubset_lex_rank ( k, n, t, rank )

  write ( *, * ) ' '
  write ( *, * ) '  The rank of the element:'
  write ( *, * ) ' '
  write ( *, '(6i5)' ) t(1:k)
  write ( *, * ) ' '
  write ( *, * ) '  is computed as ', rank

  return
end
subroutine test12
!
!*******************************************************************************
!
!! TEST12 tests KSUBSET_ENUM;
!! TEST12 tests KSUBSET_REVDOOR_RANK;
!! TEST12 tests KSUBSET_REVDOOR_SUCCESSOR;
!! TEST12 tests KSUBSET_REVDOOR_UNRANK.
!
  integer, parameter :: k = 3
!
  integer j
  integer n
  integer nksub
  integer rank
  integer rankold
  integer t(k)
!
  n = 5

  write ( *, * ) ' '
  write ( *, * ) 'TEST12'
  write ( *, * ) '  K-subsets of an N set,'
  write ( *, * ) '  using the revolving door ordering:'
  write ( *, * ) ' '
  write ( *, * ) '  KSUBSET_ENUM enumerates,'
  write ( *, * ) '  KSUBSET_REVDOOR_RANK ranks,'
  write ( *, * ) '  KSUBSET_REVDOOR_SUCCESSOR lists,'
  write ( *, * ) '  KSUBSET_REVDOOR_UNRANK unranks.'
!
!  Enumerate.
!
  call ksubset_enum ( k, n, nksub )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of K subsets is ', nksub
  write ( *, * ) ' '
!
!  List
!
  rank = -1

  do

    rankold = rank

    call ksubset_revdoor_successor ( k, n, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:k)

  end do
!
!  Unrank.
!
  rank = nksub / 2
 
  call ksubset_revdoor_unrank ( rank, k, n, t )

  write ( *, * ) ' '
  write ( *, * ) '  The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(6i5)' ) t(1:k)
!
!  Rank.
!
  call ksubset_revdoor_rank ( k, n, t, rank )

  write ( *, * ) ' '
  write ( *, * ) '  The rank of the element:'
  write ( *, * ) ' '
  write ( *, '(6i5)' ) t(1:k)
  write ( *, * ) ' '
  write ( *, * ) '  is computed as ', rank

  return
end
subroutine test13
!
!*******************************************************************************
!
!! TEST13 tests MARRIAGE.
!
  integer, parameter :: n = 5
!
  integer fiancee(n)
  integer i
  integer next(n)
  integer prefer(n,n)
  integer rank(n,n)
!
  prefer(1,1) = 2
  prefer(1,2) = 5
  prefer(1,3) = 1
  prefer(1,4) = 3
  prefer(1,5) = 4

  prefer(2,1) = 1
  prefer(2,2) = 2
  prefer(2,3) = 3
  prefer(2,4) = 4
  prefer(2,5) = 5

  prefer(3,1) = 2
  prefer(3,2) = 3
  prefer(3,3) = 5
  prefer(3,4) = 4
  prefer(3,5) = 1

  prefer(4,1) = 1
  prefer(4,2) = 3
  prefer(4,3) = 2
  prefer(4,4) = 4
  prefer(4,5) = 5

  prefer(5,1) = 5
  prefer(5,2) = 3
  prefer(5,3) = 2
  prefer(5,4) = 1
  prefer(5,5) = 4
!
!  RANK(W,M) is the index of man M on woman W's list.
!
  rank(1,1) = 2
  rank(1,2) = 4
  rank(1,3) = 5
  rank(1,4) = 3
  rank(1,5) = 1

  rank(2,1) = 4
  rank(2,2) = 3
  rank(2,3) = 5
  rank(2,4) = 1
  rank(2,5) = 2

  rank(3,1) = 1
  rank(3,2) = 3
  rank(3,3) = 4
  rank(3,4) = 2
  rank(3,5) = 5

  rank(4,1) = 4
  rank(4,2) = 2
  rank(4,3) = 1
  rank(4,4) = 3
  rank(4,5) = 5

  rank(5,1) = 5
  rank(5,2) = 2
  rank(5,3) = 3
  rank(5,4) = 1
  rank(5,5) = 4

  write ( *, * ) ' '
  write ( *, * ) 'TEST13'
  write ( *, * ) '  MARRIAGE arranges a set of stable marriages'
  write ( *, * ) '  given a set of preferences.'

  call marriage ( n, prefer, rank, fiancee, next )

  write ( *, * ) ' '
  write ( *, * ) '  Man, Wife''s rank, Wife'
  write ( *, * ) ' '
  do i = 1, n
    write ( *, '(3i8)' ) i, next(i), prefer(i,next(i))
  end do

  write ( *, * ) ' '
  write ( *, * ) '  Woman, Husband''s rank, Husband'
  write ( *, * ) ' '
  do i = 1, n
    write ( *, '(3i8)' ) i, rank(i,fiancee(i)), fiancee(i)
  end do

  write ( *, * ) ' '
  write ( *, * ) '  Correct result:'
  write ( *, * ) ' '
  write ( *, * ) '  M:W 1  2  3  4  5' 
  write ( *, * ) '   1  +  .  .  .  .'
  write ( *, * ) '   2  .  .  .  +  .'
  write ( *, * ) '   3  .  .  .  .  +'
  write ( *, * ) '   4  .  .  +  .  .'
  write ( *, * ) '   5  .  +  .  .  .'

  return
end
subroutine test14
!
!*******************************************************************************
!
!! TEST14 tests MOUNTAIN.
!
  integer, parameter :: n = 5
!
  integer i
  integer mxy
  integer row(0:2*n)
  integer x
  integer y
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST14'
  write ( *, * ) '  MOUNTAIN computes mountain numbers.'
  write ( *, * ) ' '
  write ( *, * ) '  X  Y  MXY'
  write ( *, * ) ' '

  do y = 0, n
    do x = 0, 2*n
      call mountain ( n, x, y, mxy )
      row(x) = mxy
    end do
    write ( *, '(i2,3x,11i4)' ) y, row(0:2*n) 
  end do

  return
end
subroutine test15
!
!*******************************************************************************
!
!! TEST15 tests NPART_ENUM;
!! TEST15 tests NPART_RSF_LEX_RANK;
!! TEST15 tests NPART_RSF_LEX_SUCCESSOR;
!! TEST15 tests NPART_RSF_LEX_UNRANK.
!
  integer, parameter :: npart = 3
!
  integer j
  integer n
  integer npartitions
  integer rank
  integer rankold
  integer t(npart)
!
  n = 12

  write ( *, * ) ' '
  write ( *, * ) 'TEST15'
  write ( *, * ) '  Partitions of N with NPART parts'
  write ( *, * ) '  in reverse standard form:'
  write ( *, * ) ' '
  write ( *, * ) '  NPART_ENUM enumerates,'
  write ( *, * ) '  NPART_RSF_LEX_RANK ranks,'
  write ( *, * ) '  NPART_RSF_LEX_SUCCESSOR lists;'
  write ( *, * ) '  NPART_RSF_LEX_UNRANK unranks.'
!
!  Enumerate.
!
  call npart_enum ( n, npart, npartitions )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  and NPART = ', npart
  write ( *, * ) '  the number of partitions is ', npartitions
  write ( *, * ) ' '
!
!  List.
!
  rank = -1

  do

    rankold = rank

    call npart_rsf_lex_successor ( n, npart, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:npart)

  end do
!
!  Unrank.
!
  rank = npartitions / 3

  call npart_rsf_lex_unrank ( rank, n, npart, t )

  write ( *, * ) ' '
  write ( *, * ) '  The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(6i5)' ) t(1:npart)
!
!  Rank.
!
  call npart_rsf_lex_rank ( n, npart, t, rank )

  write ( *, * ) ' '
  write ( *, * ) '  The rank of the element:'
  write ( *, * ) ' '
  write ( *, '(6i5)' ) t(1:npart)
  write ( *, * ) ' '
  write ( *, * ) '  is computed as ', rank

  return
end
subroutine test16
!
!*******************************************************************************
!
!! TEST16 tests NPART_ENUM;
!! TEST16 tests NPART_SF_SUCCESSOR;
!
  integer, parameter :: npart = 3
!
  integer j
  integer n
  integer npartitions
  integer rank
  integer rankold
  integer t(npart)
!
  n = 12

  write ( *, * ) ' '
  write ( *, * ) 'TEST16'
  write ( *, * ) '  Partitions of N with NPART parts'
  write ( *, * ) '  in standard form:'
  write ( *, * ) ' '
  write ( *, * ) '  NPART_ENUM enumerates,'
  write ( *, * ) '  NPART_SF_LEX_SUCCESSOR lists.'
!
!  Enumerate.
!
  call npart_enum ( n, npart, npartitions )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  and NPART = ', npart
  write ( *, * ) '  the number of partitions is ', npartitions
  write ( *, * ) ' '
!
!  List.
!
  rank = -1

  do

    rankold = rank

    call npart_sf_lex_successor ( n, npart, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:npart)

  end do

  return
end
subroutine test17
!
!*******************************************************************************
!
!! TEST17 tests NPART_TABLE;
!! TEST17 tests PART_TABLE.
!
  integer, parameter :: maxn = 10
  integer, parameter :: maxpart = 5
!
  integer i
  integer j
  integer p(0:maxn,0:maxpart)
  integer p2(0:maxn)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST17'
  write ( *, * ) '  NPART_TABLE tabulates partitions'
  write ( *, * ) '    of N with NPART parts;'
  write ( *, * ) '  PART_TABLE tabulates partitions of N.'

  call npart_table ( maxn, maxpart, maxn, p )

  call part_table ( maxn, p2 )

  write ( *, * ) ' '
  write ( *, * ) '    I P(I)  P(I,0) P(I,1) P(I,2) P(I,3) P(I,4) P(I,5)'
  write ( *, * ) ' '

  do i = 0, maxn
    write ( *, '(11i5)' ) i, p2(i), p(i,0:maxpart)
  end do

  return
end
subroutine test18
!
!*******************************************************************************
!
!! TEST18 tests PART_ENUM;
!! TEST18 tests PART_SUCCESSOR.
!
  integer, parameter :: n = 8
!
  integer j
  integer npart
  integer npartitions
  integer rank
  integer rankold
  integer t(n)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST18'
  write ( *, * ) '  PART_SUCCESSOR produces partitions of N,'
  write ( *, * ) '  PART_ENUM enumerates.'
  write ( *, * ) ' '
  write ( *, * ) '  Partitions of N = ', n
!
!  Enumerate.
!
  call part_enum ( n, npartitions )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of partitions is ', npartitions
  write ( *, * ) ' '
!
!  List.
!
  rank = -1

  do

    rankold = rank

    call part_successor ( n, npart, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(i3,3x,10i3)' ) rank, t(1:npart)

  end do

  return
end
subroutine test185
!
!*******************************************************************************
!
!! TEST185 tests PART_SUCCESSOR.
!! TEST185 tests PART_SF_CONJUGATE.
!
  integer, parameter :: n = 8
!
  integer b(n)
  integer j
  integer npart
  integer npartb
  integer npartitions
  integer rank
  integer rankold
  integer t(n)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST185'
  write ( *, * ) '  PART_SUCCESSOR produces partitions of N,'
  write ( *, * ) '  PART_SF_CONJUGATE produces the conjugate of a partition.'
  write ( *, * ) ' '
  write ( *, * ) '  Partitions of N = ', n
!
!  List.
!
  rank = -1

  do

    rankold = rank

    call part_successor ( n, npart, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(i3,3x,10i3)' ) rank, t(1:npart)
    call part_sf_conjugate ( n, npart, t, npartb, b )
    write ( *, '(a4,2x,10i3)' ) 'Con:', b(1:npartb)

  end do

  return
end
subroutine test186
!
!*******************************************************************************
!
!! TEST186 tests PART_SF_MAJORIZE.
!
  integer, parameter :: n = 8
!
  integer, parameter, dimension ( n ) :: a = (/ 2, 2, 2, 1, 1, 0, 0, 0 /)
  integer, parameter, dimension ( n ) :: b = (/ 3, 1, 1, 1, 1, 1, 0, 0 /)
  integer, parameter, dimension ( n ) :: c = (/ 2, 2, 1, 1, 1, 1, 0, 0 /)
  integer, parameter :: nparta = 5
  integer, parameter :: npartb = 6
  integer, parameter :: npartc = 6
  integer result
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST186'
  write ( *, * ) '  PART_SF_MAJORIZE determines if one partition'
  write ( *, * ) '  majorizes another.'
  write ( *, * ) ' '
  write ( *, * ) '  Partitions of N = ', n
  write ( *, * ) ' '
  write ( *, '(a3,2x,10i3)' ) 'A:', a(1:nparta)
  write ( *, '(a3,2x,10i3)' ) 'B:', b(1:npartb)
  write ( *, '(a3,2x,10i3)' ) 'C:', c(1:npartc)

  call part_sf_majorize ( n, nparta, a, npartb, b, result )
  write ( *, * ) ' '
  write ( *, * ) '  A compare B: ', result
  call part_sf_majorize ( n, npartb, b, npartc, c, result )
  write ( *, * ) '  B compare C: ', result
  call part_sf_majorize ( n, npartc, c, nparta, a, result )
  write ( *, * ) '  C compare A: ', result
  call part_sf_majorize ( n, npartc, c, npartc, c, result )
  write ( *, * ) '  C compare C: ', result

  return
end
subroutine test19
!
!*******************************************************************************
!
!! TEST19 tests PARTN_ENUM;
!! TEST19 tests PARTN_SUCCESSOR.
!! TEST19 tests PART_SF_CONJUGATE.
!
  integer, parameter :: n = 11
!
  integer b(n)
  integer j
  integer nmax
  integer npart
  integer npart2
  integer npartitions
  integer rank
  integer rankold
  integer t(n)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST19'
  write ( *, * ) '  Partitions of N with maximum element NMAX:'
  write ( *, * ) ' '
  write ( *, * ) '  PARTN_SUCCESSOR lists;'
  write ( *, * ) '  PARTN_ENUM enumerates.'

  nmax = 4
!
!  Enumerate.
!
  call partn_enum ( n, nmax, npartitions )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  and NMAX = ', nmax
  write ( *, * ) '  the number of partitions is ', npartitions
  write ( *, * ) ' '
!
!  List.
!
  rank = -1

  do

    rankold = rank

    call partn_successor ( n, nmax, npart, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(i3,3x,15i3)' ) rank, t(1:npart)

  end do
!
!  List conjugates.
!
  write ( *, * ) ' '
  write ( *, * ) 'Repeat, but list RSF conjugated partitions.'
  write ( *, * ) ' '
  rank = -1

  do

    rankold = rank

    call partn_successor ( n, nmax, npart, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    call part_sf_conjugate ( n, npart, t, npart2, b )
    call ivec_reverse ( npart2, b )
    write ( *, '(i3,3x,15i3)' ) rank, b(1:npart2)

  end do

  return
end
subroutine test20
!
!*******************************************************************************
!
!! TEST20 tests PERM_INV;
!! TEST20 tests PERM_MUL.
!
  integer, parameter :: n = 4
!
  integer j
  integer nperm
  integer p(n)
  integer q(n)
  integer r(n)
  integer rank
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST20'
  write ( *, * ) '  Permutations of the integers:'
  write ( *, * ) ' '
  write ( *, * ) '  PERM_INV computes an inverse permutation,'
  write ( *, * ) '  PERM_MUL multiplies two permutations.'
!
!  Enumerate.
!
  call perm_enum ( n, nperm )
!
!  Unrank.
!
  rank = nperm / 2

  call perm_lex_unrank ( rank, n, p )

  write ( *, * ) ' '
  write ( *, * ) '  The permutation P is '
  write ( *, * ) ' '
  write ( *, * ) p(1:n)
!
!  Invert.
!
  call perm_inv ( n, p, q )

  write ( *, * ) ' '
  write ( *, * ) '  The inverse permutation Q is '
  write ( *, * ) ' '
  write ( *, * ) q(1:n)
!
!  Multiply.
!
  call perm_mul ( n, p, q, r )

  write ( *, * ) ' '
  write ( *, * ) '  The product R = P * Q is '
  write ( *, * ) ' '
  write ( *, * ) r(1:n)

  return
end
subroutine test21
!
!*******************************************************************************
!
!! TEST21 tests PERM_ENUM;
!! TEST21 tests PERM_LEX_RANK;
!! TEST21 tests PERM_LEX_SUCCESSOR;
!! TEST21 tests PERM_LEX_UNRANK.
!
  integer, parameter :: n = 4
!
  integer j
  integer nperm
  integer pi(n)
  integer rank
  integer rankold
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST21'
  write ( *, * ) '  Permutations of the integers,'
  write ( *, * ) '  using the lexicographic ordering:'
  write ( *, * ) ' '
  write ( *, * ) '  PERM_ENUM enumerates,'
  write ( *, * ) '  PERM_LEX_RANK ranks,'
  write ( *, * ) '  PERM_LEX_SUCCESSOR lists,'
  write ( *, * ) '  PERM_LEX_UNRANK unranks.'
!
!  Enumerate.
!
  call perm_enum ( n, nperm )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of permutations is ', nperm
  write ( *, * ) ' '
!
!  List
!
  rank = -1

  do

    rankold = rank

    call perm_lex_successor ( n, pi, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, pi(1:n)

  end do
!
!  Unrank.
!
  rank = nperm / 2

  call perm_lex_unrank ( rank, n, pi )

  write ( *, * ) ' '
  write ( *, * ) 'The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(6i5)' ) pi(1:n)
!
!  Rank.
!
  call perm_lex_rank ( n, pi, rank )

  write ( *, * ) ' '
  write ( *, * ) 'The rank of the element:'
  write ( *, * ) ' '
  write ( *, '(6i5)' ) pi(1:n)
  write ( *, * ) ' '
  write ( *, * ) 'is computed as ', rank

  return
end
subroutine test22
!
!*******************************************************************************
!
!! TEST22 tests PERM_TJ_ENUM;
!! TEST22 tests PERM_TJ_RANK;
!! TEST22 tests PERM_TJ_SUCCESSOR;
!! TEST22 tests PERM_TJ_UNRANK.
!
  integer, parameter :: n = 4
!
  integer j
  integer nperm
  integer pi(n)
  integer rank
  integer rankold
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST22'
  write ( *, * ) '  Permutations of the integers'
  write ( *, * ) '  using the Trotter-Johnson ordering:'
  write ( *, * ) ' '
  write ( *, * ) '  PERM_ENUM enumerates,'
  write ( *, * ) '  PERM_TJ_RANK ranks,'
  write ( *, * ) '  PERM_TJ_SUCCESSOR lists,'
  write ( *, * ) '  PERM_TJ_UNRANK unranks.'
!
!  Enumerate.
!
  call perm_enum ( n, nperm )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of permutations is ', nperm
  write ( *, * ) ' '
!
!  List
!
  rank = -1

  do

    rankold = rank

    call perm_tj_successor ( n, pi, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, pi(1:n)

  end do
!
!  Unrank.
!
  rank = nperm / 2

  call perm_tj_unrank ( rank, n, pi )

  write ( *, * ) ' '
  write ( *, * ) 'The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(6i5)' ) pi(1:n)
!
!  Rank.
!
  call perm_tj_rank ( n, pi, rank )

  write ( *, * ) ' '
  write ( *, * ) 'The rank of the element:'
  write ( *, * ) ' '
  write ( *, '(6i5)' ) pi(1:n)
  write ( *, * ) ' '
  write ( *, * ) 'is computed as ', rank

  return
end
subroutine test23
!
!*******************************************************************************
!
!! TEST23 tests PRUEFER_ENUM;
!! TEST23 tests PRUEFER_RANK;
!! TEST23 tests PRUEFER_SUCCESSOR;
!! TEST23 tests PRUEFER_UNRANK.
!
  integer, parameter :: n = 4
!
  integer j
  integer ncode
  integer p(n-2)
  integer rank
  integer rankold
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST21'
  write ( *, * ) '  Pruefer codes:'
  write ( *, * ) ' '
  write ( *, * ) '  PRUEFER_ENUM enumerates,'
  write ( *, * ) '  PRUEFER_RANK ranks,'
  write ( *, * ) '  PRUEFER_SUCCESSOR lists,'
  write ( *, * ) '  PRUEFER_UNRANK unranks.'
!
!  Enumerate.
!
  call pruefer_enum ( n, ncode )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of Pruefer codes is ', ncode
  write ( *, * ) ' '
!
!  List
!
  rank = -1

  do

    rankold = rank

    call pruefer_successor ( n, p, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, p(1:n-2)

  end do
!
!  Unrank.
!
  rank = ncode / 2

  call pruefer_unrank ( rank, n, p )

  write ( *, * ) ' '
  write ( *, * ) 'The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(6i5)' ) p(1:n-2)
!
!  Rank.
!
  call pruefer_rank ( n, p, rank )

  write ( *, * ) ' '
  write ( *, * ) 'The rank of the element:'
  write ( *, * ) ' '
  write ( *, '(6i5)' ) p(1:n-2)
  write ( *, * ) ' '
  write ( *, * ) 'is computed as ', rank

  return
end
subroutine test24
!
!*******************************************************************************
!
!! TEST24 tests PRUEFER_TO_TREE;
!! TEST24 tests TREE_TO_PRUEFER.
!
  integer, parameter :: n = 5
!
  integer j
  integer p(n-2)
  integer rank
  integer t(2,n-1)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST24'
  write ( *, * ) '  PRUEFER_TO_TREE converts a Pruefer code to'
  write ( *, * ) '    a tree;'
  write ( *, * ) '  TREE_TO_PRUEFER converts a tree to a Pruefer'
  write ( *, * ) '    code.'
!
!  Pick a "random" Pruefer code.
!
  rank = 7
  call pruefer_unrank ( rank, n, p )

  write ( *, * ) ' '
  write ( *, * ) '  "Random" Pruefer code:'
  write ( *, '(6i5)' ) p(1:n-2)
!
!  Convert the Pruefer code to a tree.
!
  call pruefer_to_tree ( n, p, t )

  write ( *, * ) ' '
  write ( *, * ) '  Edge list for the corresponding tree:'
  write ( *, * ) ' '
  do j = 1, n-1
    write ( *, '(6i5)' ) j, t(1,j), t(2,j)
  end do
!
!  Convert the tree to a Pruefer code.
!
  call tree_to_pruefer ( n, t, p )

  write ( *, * ) ' '
  write ( *, * ) '  Corresponding Pruefer code:'
  write ( *, '(6i5)' ) p(1:n-2)

  return
end
subroutine test25
!
!*******************************************************************************
!
!! TEST25 tests QUEENS;
!! TEST25 tests BACKTRACK.
!
  integer, parameter :: n = 8
  integer, parameter :: maxstack = n * n
!
  integer i
  integer iarray(n)
  integer indx
  integer istack(maxstack)
  integer k
  integer nstack
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST25'
  write ( *, * ) '  QUEENS produces nonattacking queens'
  write ( *, * ) '    on a chessboard.'
  write ( *, * ) '  BACKTRACK supervises a backtrack search.'
  write ( *, * ) ' '

  indx = 0

  do

    call backtrack ( n, iarray, indx, k, nstack, istack, maxstack )

    if ( indx == 1 ) then

      write ( *, '(19i4)' ) iarray(1:n)

    else if ( indx == 2 ) then

      call queens ( n, iarray, k, nstack, istack, maxstack )

    else

      exit

    end if

  end do

  return
end
subroutine test26
!
!*******************************************************************************
!
!! TEST26 tests RGF_G_ENUM.
!
  integer, parameter :: MMAX = 8
!
  integer d(0:MMAX,0:MMAX)
  integer i
  integer j
  integer m
!
  m = 6

  write ( *, * ) ' '
  write ( *, * ) 'TEST26'
  write ( *, * ) '  RGF_G_ENUM enumerates generalized restricted'
  write ( *, * ) '    growth functions.'
  write ( *, * ) ' '

  call rgf_g_enum ( m, MMAX, d )

  do i = 0, m
    write ( *, '(7i6)' ) d(i,0:m-i)
  end do

  return
end
subroutine test27
!
!*******************************************************************************
!
!! TEST27 tests RGF_ENUM;
!! TEST27 tests RGF_RANK;
!! TEST27 tests RGF_SUCCESSOR;
!! TEST27 tests RGF_UNRANK.
!
  integer, parameter :: m = 4
!
  integer f(m)
  integer j
  integer nrgf
  integer rank
  integer rankold
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST27'
  write ( *, * ) '  Restricted growth functions:'
  write ( *, * ) ' '
  write ( *, * ) '  RGF_ENUM enumerates,'
  write ( *, * ) '  RGF_RANK ranks,'
  write ( *, * ) '  RGF_SUCCESSOR lists;'
  write ( *, * ) '  RGF_UNRANK unranks.'
!
!  Enumerate.
!
  call rgf_enum ( m, nrgf )

  write ( *, * ) ' '
  write ( *, * ) '  For M = ', m
  write ( *, * ) '  the number of RGF''s is ', nrgf
  write ( *, * ) ' '
!
!  List.
!
  rank = -1

  do

    rankold = rank

    call rgf_successor ( m, f, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, f(1:m)

  end do
!
!  Unrank.
!
  rank = nrgf / 2

  call rgf_unrank ( rank, m, f )

  write ( *, * ) ' '
  write ( *, * ) 'The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(6i5)' ) f(1:m)
!
!  Rank.
!
  call rgf_rank ( m, f, rank )

  write ( *, * ) ' '
  write ( *, * ) 'The rank of the element:'
  write ( *, * ) ' '
  write ( *, '(6i5)' ) f(1:m)
  write ( *, * ) ' '
  write ( *, * ) 'is computed as ', rank
  return
end
subroutine test28
!
!*******************************************************************************
!
!! TEST28 tests RGF_TO_SETPART;
!! TEST28 tests SETPART_TO_RGF.
!
  integer, parameter :: m = 8
!
  integer i
  integer j
  integer jlo
  integer f(m)
  integer index(m)
  integer nsub
  integer rank
  integer s(m)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST28'
  write ( *, * ) '  RGF_TO_SETPART converts a balanced'
  write ( *, * ) '    sequence to a restricted growth function;'
  write ( *, * ) '  SETPART_TO_RGF converts a restricted growth'
  write ( *, * ) '  function to a balanced sequence.'
!
!  Choose a "random" RGF.
!
  rank = 7
  call rgf_unrank ( rank, m, f )

  write ( *, * ) ' '
  write ( *, * ) '"Random" restricted growth function:'
  write ( *, * ) ' '
  write ( *, '(8i2)' ) f(1:m)
!
!  Convert the RGF to a set partition.
!
  call rgf_to_setpart ( m, f, nsub, s, index )

  write ( *, * ) ' '
  write ( *, * ) 'Corresponding set partition'
  write ( *, * ) ' '
  jlo = 1
  do i = 1, nsub
    write ( *, * ) s(jlo:index(i))
    jlo = index(i) + 1
  end do
!
!  Convert the set partition back to an RGF.
!
  call setpart_to_rgf ( m, nsub, s, index, f )

  write ( *, * ) ' '
  write ( *, * ) 'Corresponding restricted growth function:'
  write ( *, * ) ' '
  write ( *, '(8i2)' ) f(1:m)

  return
end
subroutine test29
!
!*******************************************************************************
!
!! TEST29 tests STIRLING_NUMBERS1.
!
  integer, parameter :: maxm = 6
  integer, parameter :: maxn = 6
!
  integer i
  integer j
  integer s(0:maxm,0:maxn)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST29'
  write ( *, * ) '  STIRLING_NUMBERS1 computes a table of Stirling'
  write ( *, * ) '    numbers of the first kind.'

  call stirling_numbers1 ( maxm, maxn, s )

  write ( *, * ) ' '
  write ( *, * ) '    I S(I,0) S(I,1) S(I,2) S(I,3) S(I,4) S(I,5)'
  write ( *, * ) ' '

  do i = 0, maxm
    write ( *, '(11i5)' ) i, s(i,0:maxn)
  end do

  return
end
subroutine test30
!
!*******************************************************************************
!
!! TEST30 tests STIRLING_NUMBERS2.
!
  integer, parameter :: maxm = 6
  integer, parameter :: maxn = 6
!
  integer i
  integer j
  integer s(0:maxm,0:maxn)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST30'
  write ( *, * ) '  STIRLING_NUMBERS2 computes a table of Stirling'
  write ( *, * ) '    numbers of the second kind.'

  call stirling_numbers2 ( maxm, maxn, s )

  write ( *, * ) ' '
  write ( *, * ) '    I S(I,0) S(I,1) S(I,2) S(I,3) S(I,4) S(I,5)'
  write ( *, * ) ' '

  do i = 0, maxm
    write ( *, '(11i5)' ) i, s(i,0:maxn)
  end do

  return
end
subroutine test31
!
!*******************************************************************************
!
!! TEST31 tests SUBSET_COLEX_RANK;
!! TEST31 tests SUBSET_COLEX_SUCCESSOR;
!! TEST31 tests SUBSET_COLEX_UNRANK;
!! TEST31 tests SUBSET_ENUM.
!
  integer, parameter :: n = 5
!
  integer j
  integer nsub
  integer rank
  integer rankold
  integer t(n)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST31'
  write ( *, * ) '  All subsets of a set,'
  write ( *, * ) '  using the colexicographic ordering:'
  write ( *, * ) ' '
  write ( *, * ) '  SUBSET_COLEX_RANK ranks,'
  write ( *, * ) '  SUBSET_COLEX_SUCCESSOR lists,'
  write ( *, * ) '  SUBSET_COLEX_UNRANK unranks.'
  write ( *, * ) '  SUBSET_ENUM enumerates.'
!
!  Enumerate.
!
  call subset_enum ( n, nsub )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of subsets is ', nsub
  write ( *, * ) ' '
!
!  List
!
  rank = -1

  do

    rankold = rank

    call subset_colex_successor ( n, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:n)
 
  end do
!
!  Unrank.
!
  rank = nsub / 2

  call subset_colex_unrank ( rank, n, t )

  write ( *, * ) ' '
  write ( *, * ) 'The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(6i5)' ) t(1:n)
!
!  Rank.
!
  call subset_colex_rank ( n, t, rank )

  write ( *, * ) ' '
  write ( *, * ) 'The rank of the element:'
  write ( *, * ) ' '
  write ( *, '(6i5)' ) t(1:n)
  write ( *, * ) ' '
  write ( *, * ) 'is computed as ', rank

  return
end
subroutine test32
!
!*******************************************************************************
!
!! TEST32 tests SUBSET_ENUM;
!! TEST32 tests SUBSET_LEX_RANK;
!! TEST32 tests SUBSET_LEX_SUCCESSOR;
!! TEST32 tests SUBSET_LEX_UNRANK.
!
  integer, parameter :: n = 5
!
  integer j
  integer nsub
  integer rank
  integer rankold
  integer t(n)
!
  write ( *, * ) ' '
  write ( *, * ) 'TEST32'
  write ( *, * ) '  All subsets of a set,'
  write ( *, * ) '  using the lexicographic ordering:'
  write ( *, * ) ' '
  write ( *, * ) '  SUBSET_ENUM enumerates,'
  write ( *, * ) '  SUBSET_LEX_RANK ranks,'
  write ( *, * ) '  SUBSET_LEX_SUCCESSOR lists,'
  write ( *, * ) '  SUBSET_LEX_UNRANK unranks.'
!
!  Enumerate.
!
  call subset_enum ( n, nsub )

  write ( *, * ) ' '
  write ( *, * ) '  For N = ', n
  write ( *, * ) '  the number of subsets is ', nsub
  write ( *, * ) ' '
!
!  List
!
  rank = -1

  do

    rankold = rank

    call subset_lex_successor ( n, t, rank )

    if ( rank <= rankold ) then
      exit
    end if

    write ( *, '(6i5)' ) rank, t(1:n)

  end do
!
!  Unrank.
!
  rank = nsub / 2

  call subset_lex_unrank ( rank, n, t )

  write ( *, * ) ' '
  write ( *, * ) 'The element of rank ', rank
  write ( *, * ) ' '
  write ( *, '(6i5)' ) t(1:n)
!
!  Rank.
!
  call subset_lex_rank ( n, t, rank )

  write ( *, * ) ' '
  write ( *, * ) 'The rank of the element:'
  write ( *, * ) ' '
  write ( *, '(6i5)' ) t(1:n)
  write ( *, * ) ' '
  write ( *, * ) 'is computed as ', rank

  return
end
subroutine test33
!
!*******************************************************************************
!
!! TEST33 tests SUBSETSUM_SWAP.
!
  integer, parameter :: n = 7
!
  integer a(n)
  integer i
  integer index(n)
  integer sum
  integer sumopt
!
  sumopt = 17

  write ( *, * ) ' '
  write ( *, * ) 'TEST33'
  write ( *, * ) '  SUBSETSUM_SWAP seeks a solution of the subset'
  write ( *, * ) '  sum problem using pair swapping.'
  write ( *, * ) ' '
  write ( *, * ) '  The desired sum is ', sumopt

  a(1) = 12
  a(2) = 8
  a(3) = 11
  a(4) = 30
  a(5) = 8
  a(6) = 3
  a(7) = 7
  
  call subsetsum_swap ( n, a, sumopt, index, sum )

  write ( *, * ) ' '
  write ( *, * ) '  A(I), INDEX(I)'
  write ( *, * ) ' '

  do i = 1, n
    write ( *, '(2i5)' ) a(i), index(i)
  end do

  write ( *, * ) ' '
  write ( *, * ) '  The achieved sum is ', sum

  return
end
