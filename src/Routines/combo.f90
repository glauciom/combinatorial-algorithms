subroutine backtrack ( l, iarray, indx, k, nstack, stack, maxstack )
!
!*******************************************************************************
!
!! BACKTRACK supervises a backtrack search.
!
!
!  Discussion:
!
!    The routine builds a vector, one element at a time, which is
!    required to satisfy some condition.
!
!    At any time, the partial vector may be discovered to be
!    unsatisfactory, but the routine records information about where the
!    last arbitrary choice was made, so that the search can be
!    carried out efficiently, rather than starting out all over again.
!
!  Reference:
!
!    A Nijenhuis and H Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Modified:
!
!    28 April 1999
!
!  Parameters:
!
!    Input/output, integer L, the length of the completed candidate vector.
!
!    Input/output, integer IARRAY(L), the candidate vector.
!
!    Input/output, integer INDX.
!
!    On input, set INDX = 0 to start a search.
!
!    On output:
!
!    1, a complete output vector has been determined.
!    2, candidates are needed.
!    3, no more possible vectors exist.
!
!    Input/output, integer K, the current length of the candidate vector.
!
!    Input/output, integer NSTACK, the current length of the stack.
!
!    Output, integer STACK(MAXSTACK), a list of more candidates for
!    positions 1 through K.
!
!    Input, integer MAXSTACK, the maximum length of the stack.
!
  integer l
  integer maxstack
!
  integer iarray(l)
  integer indx
  integer k
  integer nstack
  integer stack(maxstack)
!
!  If this is the first call, request a candidate for position 1.
!
  if ( indx == 0 ) then
    k = 1
    nstack = 0
    indx = 2
    return
  end if
!
!  Examine the stack.
!
  do

    nstack = nstack - 1
!
!  If there are candidates for position K, take the first available
!  one off the stack, and increment K.
!
!  This may cause K to reach the desired value of L, in which case
!  we need to signal the user that a complete set of candidates
!  is being returned.
!
    if ( stack(nstack+1) /= 0 ) then

      iarray(k) = stack(nstack)
      stack(nstack) = stack(nstack+1) - 1

      if ( k /= l ) then
        k = k + 1
        indx = 2
      else
        indx = 1
      end if

      exit
!
!  If there are no candidates for position K, then decrement K.
!  If K is still positive, repeat the examination of the stack.
!
    else

      k = k - 1

      if ( k <= 0 ) then
        indx = 3
        exit
      end if

    end if

  end do

  return
end
subroutine bal_seq_check ( n, t, ierror )
!
!*******************************************************************************
!
!! BAL_SEQ_CHECK checks a balanced sequence.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of 0's (and 1's) in the sequence.
!    N must be positive.
!
!    Input, integer T(2*N), a balanced sequence.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N is not positive.
!    I, the I-th entry of T is illegal.
!    2*N+1, there are not the same number of 1's as 0's.
!
  integer n
!
  integer i
  integer ierror
  integer ones
  integer t(2*n)
  integer zeros
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if

  ones = 0
  zeros = 0

  do i = 1, 2*n

    if ( t(i) == 0 ) then
      zeros = zeros + 1
    else if ( t(i) == 1 ) then
      ones = ones + 1
    else
      ierror = i
      return
    end if

    if ( ones > zeros ) then
      ierror = 1
      return
    end if

  end do

  if ( ones /= zeros ) then
    ierror = 2 * n + 1
  end if

  return
end
subroutine bal_seq_enum ( n, nseq )
!
!*******************************************************************************
!
!! BAL_SEQ_ENUM enumerates the balanced sequences.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 95.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of 0's (and 1's) in the sequence.
!    N must be nonnegative.
!
!    Output, integer NSEQ, the number of balanced sequences.
!
  integer binomial
  integer n
  integer nseq
!
  nseq = binomial ( 2*n, n ) / ( n + 1 )

  return
end
subroutine bal_seq_rank ( n, t, rank )
!
!*******************************************************************************
!
!! BAL_SEQ_RANK ranks a balanced sequence.
!
!
!  Reference:
!
!    Algorithm 3.23,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 99.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of 0's (and 1's) in the sequence.
!    N must be positive.
!
!    Input, integer T(2*N), a balanced sequence.
!
!    Output, integer RANK, the rank of the balanced sequence.
!
  integer n
!
  integer ierror
  integer mxy
  integer rank
  integer t(2*n)
  integer x
  integer y
!
!  Check.
!
  call bal_seq_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'BAL_SEQ_RANK - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
  y = 0
  rank = 0

  do x = 1, 2*n-1

    if ( t(x) == 0 ) then
      y = y + 1
    else
      call mountain ( n, x, y + 1, mxy )
      rank = rank + mxy
      y = y - 1
    end if

  end do

  return
end
subroutine bal_seq_successor ( n, t, rank )
!
!*******************************************************************************
!
!! BAL_SEQ_SUCCESSOR computes the lexical balanced sequence successor.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of 0's (and 1's) in the sequence.
!    N must be positive.
!
!    Input/output, integer T(2*N), on input, a balanced sequence,
!    and on output, its lexical successor.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer n
!
  integer i
  integer ierror
  integer j
  integer open
  integer open_index
  integer rank
  integer slot
  integer slot_index
  integer slot_ones
  integer t(2*n)
!
!  Return the first element.
!
  if ( rank == -1 ) then
    t(1:n) = 0
    t(n+1:2*n) = 1
    rank = 0
    return
  end if
!
!  Check.
!
  call bal_seq_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'BAL_SEQ_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  After the I-th 0 there is a 'slot' with the capacity to
!  hold between 0 and I ones.  
!
!  The first element of the sequence has all the 1's cowering
!  behind the N-th 0.
!
!  We seek to move a 1 to the left, and to do it lexically,
!  we will move a 1 to the rightmost slot that is under capacity.
!
!  Find the slot.
!
  slot = 0
  slot_index = 0
  slot_ones = 0

  open = 0
  open_index = 0

  do i = 1, 2*n

    if ( t(i) == 0 ) then

      if ( slot > 0 ) then
        if ( slot > slot_ones ) then
          open = slot
          open_index = slot_index
        end if
      end if

      slot = slot + 1
      slot_index = i

!     slot_ones = 0

    else
      slot_ones = slot_ones + 1
    end if

  end do
!
!  If OPEN is not 0, then preserve the string up to the OPEN-th 0,
!  preserve the 1's that follow, but then write a 1, then 
!  all the remaining 0's and all the remaining 1's.
!
  if ( open /= 0 ) then

    j = open_index + 1

    do while ( t(j) == 1 ) 
      j = j + 1
    end do

    t(j) = 1

    do i = open+1, n
      j = j + 1
      t(j) = 0
    end do

    t(j+1:2*n) = 1
!
!  If OPEN is 0, the last element was input. 
!  Return the first one.
!
  else

    t(1:n) = 0
    t(n+1:2*n) = 1
    rank = 0
    return

  end if

  rank = rank + 1

  return
end
subroutine bal_seq_to_tableau ( n, t, tab )
!
!*******************************************************************************
!
!! BAL_SEQ_TO_TABLEAU converts a balanced sequence to a 2 by N tableau.
!
!
!  Reference:
!
!    Algorithm 3.26,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 102.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of 0's (and 1's) in the sequence.
!    N must be positive.
!
!    Input, integer T(2*N), a balanced sequence.
!
!    Output, integer TAB(2,N), a 2 by N tableau.
!
  integer n
!
  integer c(2)
  integer i
  integer ierror
  integer r
  integer t(2*n)
  integer tab(2,n)
!
!  Check.
!
  call bal_seq_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'BAL_SEQ_TO_TABLEAU - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  c(1) = 0
  c(2) = 0
  do i = 1, 2 * n
    r = t(i) + 1
    c(r) = c(r) + 1
    tab(r,c(r)) = i
  end do

  return
end
subroutine bal_seq_unrank ( rank, n, t )
!
!*******************************************************************************
!
!! BAL_SEQ_UNRANK unranks a balanced sequence.
!
!
!  Reference:
!
!    Algorithm 3.24,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 100.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the balanced sequence.
!
!    Input, integer N, the number of 0's (and 1's) in the sequence.
!    N must be positive.
!
!    Output, integer T(2*N), a balanced sequence.
!
  integer n
!
  integer low
  integer m
  integer nseq
  integer rank
  integer t(2*n)
  integer x
  integer y
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'BAL_SEQ_UNRANK - Fatal error!'
    write ( *, * ) '  Input N is illegal.'
    stop
  end if

  call bal_seq_enum ( n, nseq )

  if ( rank < 0 .or. rank > nseq ) then
    write ( *, * ) ' '
    write ( *, * ) 'BAL_SEQ_UNRANK - Fatal error!'
    write ( *, * ) '  Input rank is illegal.'
    stop
  end if

  y = 0
  low = 0

  do x = 1, 2 * n

    call mountain ( n, x, y + 1, m )

    if ( rank <= low + m - 1 ) then
      y = y + 1
      t(x) = 0
    else
      low = low + m
      y = y - 1
      t(x) = 1
    end if

  end do

  return
end
subroutine bell_numbers ( m, b )
!
!*******************************************************************************
!
!! BELL_NUMBERS computes the Bell numbers.
!
!
!  Note:
!
!    There are B(M) restricted growth functions of length M.
!
!    There are B(M) partitions of a set of M objects.
!
!    B(M) is the sum of the Stirling numbers of the second kind,
!    S(M,N), for N = 0 to M.
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 81.
!
!  Modified:
!
!    18 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, indicates how many Bell numbers are to compute.
!    M must be nonnegative.
!
!    Output, integer B(0:M), the first M+1 Bell numbers.
!
  integer m
!
  integer b(0:m)
  integer binomial
  integer i
  integer j
!
  b(0) = 1
  do j = 1, m
    b(j) = 0
    do i = 0, j - 1
      b(j) = b(j) + binomial ( j - 1, i ) * b(i)
    end do
  end do

  return
end
function binomial ( n, k )
!
!*******************************************************************************
!
!! BINOMIAL computes the binomial coefficient C(N,K).
!
!
!  Formula:
!
!    BINOMIAL(N,K) = C(N,K) = N! / ( K! * (N-K)! )
!
!  Reference:
!
!    M L Wolfson and H V Wright,
!    Combinatorial of M Things Taken N at a Time,
!    ACM algorithm 160,
!    Communications of the ACM,
!    April, 1963.
!
!  Modified:
!
!    17 January 1999
!
!  Parameters:
!
!    Input, integer N, K, are the values of N and K.
!
!    Output, integer BINOMIAL, the number of combinations of N
!    things taken K at a time.
!
  integer binomial
  integer i
  integer icnk
  integer k
  integer mn
  integer mx
  integer n
!
  mn = min ( k, n - k )

  if ( mn < 0 ) then

    icnk = 0

  else if ( mn == 0 ) then

    icnk = 1

  else

    mx = max ( k, n - k )
    icnk = mx + 1

    do i = 2, mn
      icnk = ( icnk * ( mx + i ) ) / i
    end do

  end if

  binomial = icnk

  return
end
subroutine combin ( n, k, cnk )
!
!*******************************************************************************
!
!! COMBIN computes the combinatorial coefficient C(N,K).
!
!
!  Method:
!
!    Real arithmetic is used, and C(N,K) is computed directly, via
!    Gamma functions, rather than recursively.
!
!  Definition:
!
!    C(N,K) is the number of distinct combinations of K objects
!    chosen from a set of N distinct objects.  A combination is
!    like a set, in that order does not matter.
!
!  Examples:
!
!    The number of combinations of 2 things chosen from 5 is 10.
!
!    C(5,2) = ( 5 * 4 * 3 * 2 * 1 ) / ( ( 3 * 2 * 1 ) * ( 2 * 1 ) ) = 10.
!
!    The actual combinations may be represented as:
!
!      (1,2), (1,3), (1,4), (1,5), (2,3),
!      (2,4), (2,5), (3,4), (3,5), (4,5).
!
!  Formula:
!
!    C(N,K) = N! / ( (N-K)! * K! )
!
!  Modified:
!
!    16 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the value of N.
!
!    Input, integer K, the value of K.
!
!    Output, real CNK, the value of C(N,K)
!
  real arg
  real cnk
  real fack
  real facn
  real facnmk
  real gamma_log
  integer k
  integer n
!
  if ( n < 0 ) then

    cnk = 0.0E+00

  else if ( k == 0 ) then

    cnk = 1.0E+00

  else if ( k == 1 ) then

    cnk = real ( n )

  else if ( k > 1 .and. k < n-1 ) then

    arg = real ( n + 1 )
    facn = gamma_log ( arg )

    arg = real ( k + 1 )
    fack = gamma_log ( arg )

    arg = real ( n - k + 1 )
    facnmk = gamma_log ( arg )

    cnk = anint ( exp ( facn - fack - facnmk ) )

  else if ( k == n-1 ) then

    cnk = real ( n )

  else if ( k == n ) then

    cnk = 1.0E+00

  else

    cnk = 0.0E+00

  end if

  return
end
subroutine cycle_check ( n, ncycle, t, index, ierror )
!
!*******************************************************************************
!
!! CYCLE_CHECK checks a permutation in cycle form.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of items permuted.
!    N must be positive.
!
!    Input, integer NCYCLE, the number of cycles.
!    1 <= NCYCLE <= N.
!
!    Input, integer T(N), INDEX(NCYCLE), describes the permutation
!    as a collection of NCYCLE cycles.  The first cycle is
!    T(1) -> T(2) -> ... -> T(INDEX(1)) -> T(1).
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N is less than 1.
!    -2, NCYCLE is less than 1 or greater than N.
!    -3, an entry of INDEX is illegal.
!    -4, the entries of INDEX do not add up to N.
!    I, entry I of T is illegal.
!
  integer n
  integer ncycle
!
  integer i
  integer ierror
  integer ifind
  integer index(ncycle)
  integer iseek
  integer t(n)
!
!  N must be at least 1.
!
  if ( n < 1 ) then
    ierror = -1
    return
  end if
!
!  1 <= NCYCLE <= N.
!
  if ( ncycle < 1 .or. ncycle > n ) then
    ierror = -2
    return
  end if
!
!  1 <= INDEX(I) <= N.
!
  do i = 1, ncycle
    if ( index(i) < 1 .or. index(i) > n ) then
      ierror = -3
      return
    end if
  end do
!
!  The INDEX(I)'s sum to N.
!
  if ( sum ( index(1:ncycle) ) /= n ) then
    ierror = -4
  end if
!
!  1 <= T(I) <= N.
!
  do i = 1, n
    if ( t(i) < 1 .or. t(i) > n ) then
      ierror = i
      return
    end if
  end do
!
!  Verify that every value from 1 to N occurs in T.
!
  do iseek = 1, n

    ifind = 0

    do i = 1, n
      if ( t(i) == iseek ) then
        ifind = i
        exit
      end if
    end do

    if ( ifind == 0 ) then
      ierror = iseek
      return
    end if

  end do

  return
end
subroutine cycle_to_perm ( n, ncycle, t, index, p )
!
!*******************************************************************************
!
!! CYCLE_TO_PERM converts a permutation from cycle to array form.
!
!
!  Reference:
!
!    Algorithm 6.3,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 200.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of items permuted.
!    N must be positive.
!
!    Input, integer NCYCLE, the number of cycles.
!    1 <= NCYCLE <= N.
!
!    Input, integer T(N), INDEX(NCYCLE), describes the permutation
!    as a collection of NCYCLE cycles.  The first cycle is
!    T(1) -> T(2) -> ... -> T(INDEX(1)) -> T(1).
!
!    Output, integer P(N), describes the permutation using a
!    single array.  For each index I, I -> P(I).
!
  integer n
  integer ncycle
!
  integer i
  integer ierror
  integer index(ncycle)
  integer j
  integer jhi
  integer jlo
  integer p(n)
  integer t(n)
!
!  Check.
!
  call cycle_check ( n, ncycle, t, index, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'CYCLE_TO_PERM - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  jhi = 0

  do i = 1, ncycle

    jlo = jhi + 1
    jhi = jhi + index(i)

    do j = jlo, jhi

      if ( j < jhi ) then
        p(t(j)) = t(j+1)
      else
        p(t(j)) = t(jlo)
      end if

    end do

  end do

  return
end
subroutine dist_enum ( k, m, num_dist )
!
!*******************************************************************************
!
!! DIST_ENUM returns the number of distributions of indistinguishable objects.
!
!
!  Modified:
!
!    27 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of distinguishable "slots".
!
!    Input, integer M, the number of indistinguishable objects.
!
!    Output, integer NUM_DIST, the number of distributions of M indistinguishable
!    objects about K distinguishable slots.
!
  real cnk
  integer k
  integer m
  integer num_dist

  call combin ( m + k - 1, m, cnk )

  num_dist = nint ( cnk )
  
  return
end
subroutine dist_nex ( k, m, q, more )
!
!*******************************************************************************
!
!! DIST_NEX returns the next distribution of indistinguishable objects.
!
!
!  Example:
!
!    K = 3, M = 5
!
!    0           0           5
!    0           1           4
!    0           2           3
!    0           3           2
!    0           4           1
!    0           5           0
!    1           0           4
!    1           1           3
!    1           2           2
!    1           3           1
!    1           4           0
!    2           0           3
!    2           1           2
!    2           2           1
!    2           3           0
!    3           0           2
!    3           1           1
!    3           2           0
!    4           0           1
!    4           1           0
!    5           0           0
!
!  Reference:
!
!    Robert Fenichel,
!    ACM Algorithm 329,
!    Distribution of Indistinguishable Objects into Distinguishable Slots,
!    Communications of the ACM,
!    Volume 11, page 430, June 1968.
!
!  Modified:
!
!    26 June 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of distinguishable "slots".
!
!    Input, integer M, the number of indistinguishable objects.
!
!    Input/output, integer Q(K); for slot I, Q(I) is the number of objects
!    in that slot.
!
!    Input/output, logical MORE, used to start and stop the computation.
!
!    On first call, the user should set MORE = .FALSE, to signal the 
!    routine that this is a startup.  The routine will set MORE = .TRUE.,
!    and return the first distribution.  If the user calls again, the
!    routine will try to find the next distribution.  On each call, the
!    next distribution is found, and MORE is .TRUE., but finally, when
!    there is no next distribution, MORE is returned as .FALSE.
!
  integer k
!
  integer i
  integer, save :: leftmost = 1
  integer m
  logical more
  integer q(k)
!
  if ( .not. more ) then

    more = .true.

    q(1:k-1) = 0
    q(k) = m

    leftmost = k + 1

  else if ( q(1) == m ) then

    more = .false.

    q(1:k-1) = 0
    q(k) = m

    leftmost = k + 1

  else if ( leftmost < k+1 ) then

    leftmost = leftmost - 1
    q(k) = q(leftmost) - 1
    q(leftmost) = 0
    q(leftmost-1) = q(leftmost-1) + 1
    if ( q(k) /= 0 ) then
      leftmost = k + 1
    end if

  else

    if ( q(k) == 1 ) then
      leftmost = k
    end if

    q(k) = q(k) - 1
    q(k-1) = q(k-1) + 1

  end if

  return
end
subroutine edge_check ( n_node, n_edge, t, ierror )
!
!*******************************************************************************
!
!! EDGE_CHECK checks a graph stored by edges.
!
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N_NODE, the number of nodes in the graph.
!    N_NODE must be positive.
!
!    Input, integer N_EDGE, the number of edges in the graph.
!    N_EDGE must be positive.
!
!    Input, integer T(2,N_EDGE), describes the edges of the tree
!    as pairs of nodes.
!
!    Output, integer IERROR, error flag.
!    -1, N_NODE is not positive.
!    -2, N_EDGE is not positive.
!    0, no error.
!    I, edge T(1,I), T(2,I) is illegal.
!
  integer n_edge
  integer n_node
!
  integer i
  integer ierror
  integer j
  integer j2
  integer t(2,n_edge)
!
  ierror = 0

  if ( n_node < 1 ) then
    ierror = -1
    return
  end if

  if ( n_edge < 1 ) then
    ierror = -2
    return
  end if
!
!  Every edge must join two legal nodes.
!
  do i = 1, 2
    do j = 1, n_edge
      if ( t(i,j) < 1 .or. t(i,j) > n_node ) then
        ierror = i
        return
      end if
    end do
  end do
!
!  Every edge must join distinct nodes.
!
  do j = 1, n_edge
    if ( t(1,j) == t(2,j) ) then
      ierror = i
      return
    end if
  end do
!
!  Every edge must be distinct.
!
  do j = 1, n_edge-1
    do j2 = j+1, n_edge
      if ( t(1,j) == t(1,j2) .and. t(2,j) == t(2,j2) ) then
        ierror = j2
        return
      else if ( t(1,j) == t(2,j2) .and. t(2,j) == t(1,j2) ) then
        ierror = j2
        return
      end if
    end do
  end do

  return
end
subroutine edge_degree ( n_node, n_edge, t, d )
!
!*******************************************************************************
!
!! EDGE_DEGREE returns the degree of the nodes of a graph stored by edges.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N_NODE, the number of nodes in the graph.
!    N_NODE must be positive.
!
!    Input, integer N_EDGE, the number of edges in the graph.
!    N_EDGE must be positive.
!
!    Input, integer T(2,N_EDGE), describes the edges of the tree
!    as pairs of nodes.
!
!    Output, integer D(N_NODE), the degree of each node.
!
  integer n_edge
  integer n_node
!
  integer d(n_node)
  integer i
  integer ierror
  integer j
  integer t(2,n_edge)
!
!  Check.
!
  call edge_check ( n_node, n_edge, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'EDGE_DEGREE - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Compute the degree of each node.
!
  d(1:n_node) = 0

  do j = 1, n_edge
    d(t(1,j)) = d(t(1,j)) + 1
    d(t(2,j)) = d(t(2,j)) + 1
  end do

  return
end
subroutine edge_enum ( n_node, nedge )
!
!*******************************************************************************
!
!! EDGE_ENUM enumerates the maximum number of edges in a graph on N_NODE nodes.
!
!
!  Modified:
!
!    24 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N_NODE, the number of nodes in the graph.
!    N_NODE must be positive.
!
!    Output, integer NEDGE, the maximum number of edges in a graph 
!    on N_NODE nodes.
!
  integer n_node
  integer nedge
!
  nedge = ( n_node * ( n_node - 1 ) ) / 2

  return
end
function factorial ( n )
!
!*******************************************************************************
!
!! FACTORIAL computes the factorial N!
!
!
!  Formula:
!
!    FACTORIAL( N ) = PRODUCT ( 1 <= I <= N ) I
!
!  Modified:
!
!    16 January 1999
!
!  Parameters:
!
!    Input, integer N, the argument of the factorial function.
!    If N is less than 1, FACTORIAL is returned as 1.
!
!    Output, integer FACTORIAL, the factorial of N.
!
  integer factorial
  integer i
  integer n
!
  factorial = 1

  do i = 1, n
    factorial = factorial * i
  end do

  return
end
function fall ( x, n )
!
!*******************************************************************************
!
!! FALL computes the falling factorial function [X]_N.
!
!
!  Discussion:
!
!    The number of "injections" or 1-to-1 mappings from
!    a set of N elements to a set of M elements is [M]_N.
!
!    The number of permutations of N objects out of M is [M}_N.
!
!    The Stirling numbers of the first kind can be used
!    to convert a falling factorial into a polynomial, as follows:
!
!      [X]_N = S^0_N + S^1_N * X + S^2_N * X^2 + ... + S^N_N X^N.
!
!  Formula:
!
!    [X}_N = X * ( X - 1 ) * ( X - 2 ) * ... * ( X - N + 1 ).
!
!  Modified:
!
!    24 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer X, the argument of the falling factorial function.
!
!    Input, integer N, the order of the falling factorial function.
!    If N = 0, FALL = 1, if N = 1, FALL = X.  Note that if N is
!    negative, a "rising" factorial will be computed.
!
!    Output, integer FALL, the falling factorial function.
!
  integer arg
  integer fall
  integer i
  integer n
  integer x
!
  fall = 1

  arg = x

  if ( n > 0 ) then

    do i = 1, n
      fall = fall * arg
      arg = arg - 1
    end do

  else if ( n < 0 ) then

    do i = -1, n
      fall = fall * arg
      arg = arg + 1
    end do

  end if

  return
end
function gamma_log ( x )
!
!*******************************************************************************
!
!! GAMMA_LOG calculates the natural logarithm of GAMMA ( X ) for positive X.
!
!
!  Discussion:
!
!    Computation is based on an algorithm outlined in references 1 and 2.
!    The program uses rational functions that theoretically approximate
!    LOG(GAMMA(X)) to at least 18 significant decimal digits.  The
!    approximation for X > 12 is from reference 3, while approximations
!    for X < 12.0E+00 are similar to those in reference 1, but are unpublished.
!    The accuracy achieved depends on the arithmetic system, the compiler,
!    intrinsic functions, and proper selection of the machine-dependent
!    constants.
!
!  Modified:
!
!    16 June 1999
!
!  Authors:
!
!    W. J. Cody and L. Stoltz
!    Argonne National Laboratory
!
!  References:
!
!    # 1)
!    W. J. Cody and K. E. Hillstrom,
!    Chebyshev Approximations for the Natural Logarithm of the Gamma Function,
!    Math. Comp.
!    Volume 21, 1967, pages 198-203.
!
!    # 2)
!    K. E. Hillstrom,
!    ANL/AMD Program ANLC366S, DGAMMA/DLGAMA,
!    May 1969.
!
!    # 3)
!    Hart, Et. Al.,
!    Computer Approximations,
!    Wiley and sons, New York, 1968.
!
!  Parameters:
!
!    Input, real X, the argument of the Gamma function.  X must be positive.
!
!    Output, real GAMMA_LOG, the logarithm of the Gamma function of X.
!    If X <= 0.0, or if overflow would occur, the program returns the
!    value XINF, the largest representable floating point number.
!
!*******************************************************************************
!
!  Explanation of machine-dependent constants
!
!  BETA   - radix for the floating-point representation.
!
!  MAXEXP - the smallest positive power of BETA that overflows.
!
!  XBIG   - largest argument for which LN(GAMMA(X)) is representable
!           in the machine, i.e., the solution to the equation
!             LN(GAMMA(XBIG)) = BETA**MAXEXP.
!
!  XINF   - largest machine representable floating-point number;
!           approximately BETA**MAXEXP.
!
!  EPS    - The smallest positive floating-point number such that
!           1.0+EPS .GT. 1.0E+00
!
!  FRTBIG - Rough estimate of the fourth root of XBIG
!
!
!  Approximate values for some important machines are:
!
!                            BETA      MAXEXP         XBIG
!
!  CRAY-1        (S.P.)        2        8191       9.62E+2461
!  Cyber 180/855
!    under NOS   (S.P.)        2        1070       1.72E+319
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)        2         128       4.08E+36
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)        2        1024       2.55D+305
!  IBM 3033      (D.P.)       16          63       4.29D+73
!  VAX D-Format  (D.P.)        2         127       2.05D+36
!  VAX G-Format  (D.P.)        2        1023       1.28D+305
!
!
!                            XINF        EPS        FRTBIG
!
!  CRAY-1        (S.P.)   5.45E+2465   7.11E-15    3.13E+615
!  Cyber 180/855
!    under NOS   (S.P.)   1.26E+322    3.55E-15    6.44E+79
!  IEEE (IBM/XT,
!    SUN, etc.)  (S.P.)   3.40E+38     1.19E-7     1.42E+9
!  IEEE (IBM/XT,
!    SUN, etc.)  (D.P.)   1.79D+308    2.22D-16    2.25D+76
!  IBM 3033      (D.P.)   7.23D+75     2.22D-16    2.56D+18
!  VAX D-Format  (D.P.)   1.70D+38     1.39D-17    1.20D+9
!  VAX G-Format  (D.P.)   8.98D+307    1.11D-16    1.89D+76
!
  real, parameter :: d1 = - 5.772156649015328605195174E-01
  real, parameter :: d2 =   4.227843350984671393993777E-01
  real, parameter :: d4 =   1.791759469228055000094023E+00
  real, parameter :: EPS = 1.19E-07
  real, parameter :: FRTBIG = 1.42E+09
  real, parameter :: PNT68 = 0.6796875E+00
  real, parameter :: SQRTPI = 0.9189385332046727417803297E+00
  real, parameter :: XBIG = 4.08E+36
  real, parameter :: XINF = 3.401E+38
!
  real, parameter, dimension ( 7 ) :: c = (/ &
    -1.910444077728E-03, &
     8.4171387781295E-04, &
    -5.952379913043012E-04, &
     7.93650793500350248E-04, &
    -2.777777777777681622553E-03, &
     8.333333333333333331554247E-02, &
     5.7083835261E-03 /)
  real corr
  integer i
  real gamma_log
  real, parameter, dimension ( 8 ) :: p1 = (/ &
    4.945235359296727046734888e+00, &
    2.018112620856775083915565e+02, &
    2.290838373831346393026739e+03, &
    1.131967205903380828685045e+04, &
    2.855724635671635335736389e+04, &
    3.848496228443793359990269e+04, &
    2.637748787624195437963534e+04, &
    7.225813979700288197698961e+03 /)
  real, parameter, dimension ( 8 ) :: p2 = (/ &
    4.974607845568932035012064e+00, &
    5.424138599891070494101986e+02, &
    1.550693864978364947665077e+04, &
    1.847932904445632425417223e+05, &
    1.088204769468828767498470e+06, &
    3.338152967987029735917223e+06, &
    5.106661678927352456275255e+06, &
    3.074109054850539556250927e+06 /)
  real, parameter, dimension ( 8 ) :: p4 = (/ &
    1.474502166059939948905062e+04, &
    2.426813369486704502836312e+06, &
    1.214755574045093227939592e+08, &
    2.663432449630976949898078e+09, &
    2.940378956634553899906876e+10, &
    1.702665737765398868392998e+11, &
    4.926125793377430887588120e+11, &
    5.606251856223951465078242e+11 /)
  real, parameter, dimension ( 8 ) :: q1 = (/ &
    6.748212550303777196073036e+01, &
    1.113332393857199323513008e+03, &
    7.738757056935398733233834e+03, &
    2.763987074403340708898585e+04, &
    5.499310206226157329794414e+04, &
    6.161122180066002127833352e+04, &
    3.635127591501940507276287e+04, &
    8.785536302431013170870835e+03 /)
  real, parameter, dimension ( 8 ) :: q2 = (/ &
    1.830328399370592604055942e+02, &
    7.765049321445005871323047e+03, &
    1.331903827966074194402448e+05, &
    1.136705821321969608938755e+06, &
    5.267964117437946917577538e+06, &
    1.346701454311101692290052e+07, &
    1.782736530353274213975932e+07, &
    9.533095591844353613395747e+06 /)
  real, parameter, dimension ( 8 ) :: q4 = (/ &
    2.690530175870899333379843e+03, &
    6.393885654300092398984238e+05, &
    4.135599930241388052042842e+07, &
    1.120872109616147941376570e+09, &
    1.488613728678813811542398e+10, &
    1.016803586272438228077304e+11, &
    3.417476345507377132798597e+11, &
    4.463158187419713286462081e+11 /)
  real res
  real x
  real xden
  real xm1
  real xm2
  real xm4
  real xnum
  real xsq
!
!  Return immediately if the argument is out of range.
!
  if ( x <= 0.0E+00 .or. x > XBIG ) then
    gamma_log = XINF
    return
  end if

  if ( x <= EPS ) then

    res = - log ( x )

  else if ( x <= 1.5 ) then

    if ( x < PNT68 ) then
      corr = - log ( x )
      xm1 = x
    else
      corr = 0.0E+00
      xm1 = ( x - 0.5E+00 ) - 0.5E+00
    end if

    if ( x <= 0.5E+00 .or. x >= PNT68 ) then

      xden = 1.0E+00
      xnum = 0.0E+00

      do i = 1, 8
        xnum = xnum * xm1 + p1(i)
        xden = xden * xm1 + q1(i)
      end do

      res = corr + ( xm1 * ( d1 + xm1 * ( xnum / xden ) ) )

    else

      xm2 = ( x - 0.5E+00 ) - 0.5E+00
      xden = 1.0E+00
      xnum = 0.0E+00
      do i = 1, 8
        xnum = xnum * xm2 + p2(i)
        xden = xden * xm2 + q2(i)
      end do

      res = corr + xm2 * ( d2 + xm2 * ( xnum / xden ) )

    end if

  else if ( x <= 4.0E+00 ) then

    xm2 = x - 2.0E+00
    xden = 1.0E+00
    xnum = 0.0E+00
    do i = 1, 8
      xnum = xnum * xm2 + p2(i)
      xden = xden * xm2 + q2(i)
    end do

    res = xm2 * ( d2 + xm2 * ( xnum / xden ) )

  else if ( x <= 12.0E+00 ) then

    xm4 = x - 4.0E+00
    xden = - 1.0E+00
    xnum = 0.0E+00
    do i = 1, 8
      xnum = xnum * xm4 + p4(i)
      xden = xden * xm4 + q4(i)
    end do

    res = d4 + xm4 * ( xnum / xden )

  else

    res = 0.0E+00

    if ( x <= FRTBIG ) then

      res = c(7)
      xsq = x * x

      do i = 1, 6
        res = res / xsq + c(i)
      end do

    end if

    res = res / x
    corr = log ( x )
    res = res + SQRTPI - 0.5E+00 * corr
    res = res + x * ( corr - 1.0E+00 )

  end if

  gamma_log = res

  return
end
subroutine gray_code_check ( n, t, ierror )
!
!*******************************************************************************
!
!! GRAY_CODE_CHECK checks a Gray code element.
!
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of digits in each element.
!    N must be positive.
!
!    Input, integer T(N), an element of the Gray code.
!    Each entry T(I) is either 0 or 1.
!
!    Output, integer IERROR, error flag.
!    0, no error, T represents a Gray code element.
!    -1, N is not positive.
!    I, error, T(I) is an illegal value for a Gray code element.
!
  integer n
!
  integer i
  integer ierror
  integer t(n)
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if

  do i = 1, n

    if ( t(i) /= 0 .and. t(i) /= 1 ) then
      ierror = i
      return
    end if

  end do
  
  return
end
subroutine gray_code_enum ( n, ngray )
!
!*******************************************************************************
!
!! GRAY_CODE_ENUM enumerates the Gray codes on N digits.
!
!
!  Modified:
!
!    22 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of digits in each element.
!    N must be nonnegative.
!
!    Output, integer NGRAY, the number of distinct elements.
!
  integer n
  integer ngray
!
  ngray = 2**n

  return
end
subroutine gray_code_rank ( n, t, rank )
!
!*******************************************************************************
!
!! GRAY_CODE_RANK computes the rank of a Gray code element.
!
!
!  Reference:
!
!    Algorithm 2.4,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 41.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of digits in each element.
!    N must be positive.
!
!    Input, integer T(N), an element of the Gray code.
!    Each entry T(I) is either 0 or 1.
!
!    Output, integer RANK, the rank of the element.
!
  integer n
!
  integer b
  integer i
  integer ierror
  integer rank
  integer t(n)
!
!  Check.
!
  call gray_code_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRAY_CODE_RANK - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  rank = 0
  b = 0

  do i = n-1, 0, -1

    if ( t(n-i) /= 0 ) then
      b = 1 - b
    end if

    if ( b == 1 ) then
      rank = rank + 2**i
    end if

  end do
  
  return
end
subroutine gray_code_successor ( n, t, rank )
!
!*******************************************************************************
!
!! GRAY_CODE_SUCCESSOR computes the binary reflected Gray code successor.
!
!
!  Example:
!
!    000, 001, 011, 010, 110, 111, 101, 100, 
!    after which the sequence repeats.
!
!  Discussion:
!
!    In the original code, the successor of the element that has an
!    initial 1 followed by N-1 zeroes is undefined.  In this version,
!    the successor is the element with N zeroes.
!
!  Reference:
!
!    Algorithm 2.3,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 39.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of digits in each element.
!    N must be positive.
!
!    Input/output, integer T(N).  
!
!    On input, T contains an element of the Gray code, that is,
!    each entry T(I) is either 0 or 1.
!
!    On output, T contains the successor to the input value; this
!    is an element of the Gray code, which differs from the input
!    value in a single position.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer n
!
  integer i
  integer ierror
  integer rank
  integer t(n)
  integer weight
!
!  Return the first element.
!
  if ( rank == -1 ) then

    t(1:n) = 0
    rank = 0
    return

  end if
!
!  Check.
!
  call gray_code_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRAY_CODE_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  call subset_weight ( n, t, weight )

  if ( mod ( weight, 2 ) == 0 ) then

    if ( t(n) == 0 ) then
      t(n) = 1
    else
      t(n) = 0
    end if

    rank = rank + 1
    return

  else

    do i = n, 2, -1
      if ( t(i) == 1 ) then
        if ( t(i-1) == 0 ) then
          t(i-1) = 1
        else
          t(i-1) = 0
        end if
        rank = rank + 1
        return
      end if
    end do
!
!  The final element was input.
!  Return the first element.
!
    t(1:n) = 0
    rank = 0

  end if

  return
end
subroutine gray_code_unrank ( rank, n, t )
!
!*******************************************************************************
!
!! GRAY_CODE_UNRANK computes the Gray code element of given rank.
!
!
!  Reference:
!
!    Algorithm 2.5,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 42.
!
!  Modified:
!
!    22 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the element.
!    0 <= RANK <= 2**N.
!
!    Input, integer N, the number of digits in each element.
!    N must be positive.
!
!    Output, integer T(N), the element of the Gray code which has
!    the given rank.
!
  integer n
!
  integer b
  integer bprime
  integer i
  integer ngray
  integer rank
  integer rank_copy
  integer t(n)
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRAY_CODE_UNRANK - Fatal error!'
    write ( *, * ) '  Input N is illegal.'
    stop
  end if

  call gray_code_enum ( n, ngray )

  if ( rank < 0 .or. rank > ngray ) then
    write ( *, * ) ' '
    write ( *, * ) 'GRAY_CODE_UNRANK - Fatal error!'
    write ( *, * ) '  Input rank is illegal.'
    stop
  end if

  rank_copy = rank
  t(1:n) = 0
  bprime = 0

  do i = n-1, 0, -1

    b = rank_copy / 2**i

    if ( b /= bprime ) then
      t(n-i) = 1
    end if

    bprime = b
    rank_copy = rank_copy - b * 2**i

  end do

  return
end
subroutine i_swap ( i, j )
!
!*******************************************************************************
!
!! I_SWAP switches two integer values.
!
!
!  Modified:
!
!    30 November 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, integer I, J.  On output, the values of I and
!    J have been interchanged.
!
  integer i
  integer j
  integer k
!
  k = i
  i = j
  j = k

  return
end
subroutine ivec_backtrack ( n, x, indx, k, nstack, stack, maxstack, ncan )
!
!*******************************************************************************
!
!! IVEC_BACKTRACK supervises a backtrack search for an integer vector.
!
!
!  Discussion:
!
!    The routine tries to construct an integer vector one index at a time,
!    using possible candidates as supplied by the user.
!
!    At any time, the partially constructed vector may be discovered to be
!    unsatisfactory, but the routine records information about where the
!    last arbitrary choice was made, so that the search can be
!    carried out efficiently, rather than starting out all over again.
!
!    First, call the routine with INDX = 0 so it can initialize itself.
!
!    Now, on each return from the routine, if INDX is:
!      1, you've just been handed a complete candidate vector;
!         Admire it, analyze it, do what you like.
!      2, please determine suitable candidates for position X(K).
!         Return the number of candidates in NCAN(K), adding each
!         candidate to the end of STACK, and increasing NSTACK.
!      3, you're done.  Stop calling the routine;
!
!  Reference:
!
!    A Nijenhuis and H Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Modified:
!
!    24 July 2000
!
!  Parameters:
!
!    Input, integer N, the number of positions to be filled in the vector.
!
!    Input/output, integer X(N), the partial or complete candidate vector.
!
!    Input/output, integer INDX, a communication flag.
!    On input,
!      0 to start a search.
!    On output:
!      1, a complete output vector has been determined and returned in X(1:N);
!      2, candidates are needed for position X(K);
!      3, no more possible vectors exist.
!
!    Output, integer K, if INDX=2, the current vector index being considered.
!
!    Input/output, integer NSTACK, the current length of the stack.
!
!    Input, integer STACK(MAXSTACK), a list of all current candidates for
!    all positions 1 through K.
!
!    Input, integer MAXSTACK, the maximum length of the stack.
!
!    Input/output, integer NCAN(N), lists the current number of candidates for
!    positions 1 through K.
!
  integer n
  integer maxstack
!
  integer indx
  integer k
  integer ncan(n)
  integer nstack
  integer stack(maxstack)
  integer x(n)
!
!  If this is the first call, request a candidate for position 1.
!
  if ( indx == 0 ) then
    k = 1
    nstack = 0
    indx = 2
    return
  end if
!
!  Examine the stack.
!
  do
!
!  If there are candidates for position K, take the first available
!  one off the stack, and increment K.
!
!  This may cause K to reach the desired value of N, in which case
!  we need to signal the user that a complete set of candidates
!  is being returned.
!
    if ( ncan(k) > 0 ) then

      x(k) = stack(nstack)
      nstack = nstack - 1

      ncan(k) = ncan(k) - 1

      if ( k /= n ) then
        k = k + 1
        indx = 2
      else
        indx = 1
      end if

      exit
!
!  If there are no candidates for position K, then decrement K.
!  If K is still positive, repeat the examination of the stack.
!
    else

      k = k - 1

      if ( k <= 0 ) then
        indx = 3
        exit
      end if

    end if

  end do

  return
end
subroutine ivec_part1 ( n, npart, x )
!
!*******************************************************************************
!
!! IVEC_PART1 partitions an integer N into NPART parts.
!
!
!  Example:
!
!    Input:
!
!      N = 17, NPART = 5
!
!    Output:
!
!      X = ( 13, 1, 1, 1, 1 ).
!
!  Modified:
!
!    18 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.  N
!    may be positive, zero, or negative.
!
!    Input, integer NPART, the number of entries in the array.
!    1 <= NPART <= N.
!
!    Output, integer X(NPART), the partition of N.  The entries of
!    X add up to N.  X(1) = N + 1 - NPART, and all other entries
!    are equal to 1.
!
  integer npart
!
  integer i
  integer n
  integer x(npart)
!
  if ( npart < 1 .or. npart > n ) then
    write ( *, * ) ' '
    write ( *, * ) 'IVEC_PART1 - Fatal error!'
    write ( *, * ) '  The input value of NPART is illegal.'
    stop
  end if

  x(1) = n + 1 - npart
  x(2:npart) = 1

  return
end
subroutine ivec_part2 ( n, npart, x )
!
!*******************************************************************************
!
!! IVEC_PART2 partitions an integer N into NPART nearly equal parts.
!
!
!  Example:
!
!    Input:
!
!      N = 17, NPART = 5
!
!    Output:
!
!      X = ( 4, 4, 3, 3, 3 ).
!
!  Modified:
!
!    18 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.  N
!    may be positive, zero, or negative.
!
!    Input, integer NPART, the number of entries in the array.
!    1 <= NPART 
!
!    Output, integer X(NPART), the partition of N.  The entries of
!    X add up to N.  The entries of X are either all equal, or
!    differ by at most 1.  The entries of X all have the same sign
!    as N, and the "largest" entries occur first.
!
  integer npart
!
  integer i
  integer j
  integer n
  integer x(npart)
!
  if ( npart < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'IVEC_PART2 - Fatal error!'
    write ( *, * ) '  The input value of NPART is illegal.'
    stop
  end if

  x(1:npart) = 0

  if ( n > 0 ) then

    j = 1
    do i = 1, n
      x(j) = x(j) + 1
      j = j + 1
      if ( j > npart ) then
        j = 1
      end if
    end do

  else if ( n < 0 ) then

    j = 1
    do i = n, - 1
      x(j) = x(j) - 1
      j = j + 1
      if ( j > npart ) then
        j = 1
      end if
    end do

  end if

  return
end
subroutine ivec_print ( n, a, title )
!
!*******************************************************************************
!
!! IVEC_PRINT prints an integer vector.
!
!
!  Modified:
!
!    16 December 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of components of the vector.
!
!    Input, integer A(N), the vector to be printed.
!
!    Input, character ( len = * ) TITLE, a title to be printed first.
!    TITLE may be blank.
!
  integer n
!
  integer a(n)
  integer i
  character ( len = * ) title
!
  if ( title /= ' ' ) then
    write ( *, * ) ' '
    write ( *, '(a)' ) trim ( title )
  end if

  write ( *, * ) ' '
  do i = 1, n
    write ( *, '(i6,i10)' ) i, a(i)
  end do

  return
end
subroutine ivec_reverse ( n, x )
!
!*******************************************************************************
!
!! IVEC_REVERSE reverses the elements of an integer vector.
!
!
!  Example:
!
!    Input:
!
!      N = 5, X = ( 11, 12, 13, 14, 15 ).
!
!    Output:
!
!      X = ( 15, 14, 13, 12, 11 ).
!
!  Modified:
!
!    25 October 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries in the array.
!
!    Input/output, integer X(N), the array to be reversed.
!
  integer n
!
  integer i
  integer x(n)
!
  do i = 1, n/2
    call i_swap ( x(i), x(n+1-i) )
  end do

  return
end
subroutine ivec_search_binary_a ( n, a, b, indx )
!
!*******************************************************************************
!
!! IVEC_SEARCH_BINARY_A searches the ascending sorted vector A for the value B.
!
!
!  Discussion:
!
!    Binary search is used.
!
!  Reference:
!
!    Algorithm 1.9,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 26.
!
!  Modified:
!
!    16 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of elements in the vector.
!
!    Input, integer A(N), the array to be searched.  A must
!    be sorted in ascending order.
!
!    Input, integer B, the value to be searched for.
!
!    Output, integer INDX, the result of the search.
!    0, B does not occur in A.
!    I, A(I) = B.
!
  integer n
!
  integer a(n)
  integer b
  integer high
  integer indx
  integer low
  integer mid
!
  indx = 0

  low = 1
  high = n

  do while ( low <= high )

    mid = ( low + high ) / 2

    if ( a(mid) == b ) then
      indx = mid
      exit
    else if ( a(mid) < b ) then
      low = mid + 1
    else if ( a(mid) > b ) then
      high = mid - 1
    end if

  end do

  return
end
subroutine ivec_search_binary_d ( n, a, b, indx )
!
!*******************************************************************************
!
!! IVEC_SEARCH_BINARY_D searches the descending sorted vector A for the value B.
!
!
!  Discussion:
!
!    Binary search is used.
!
!  Reference:
!
!    Algorithm 1.9,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 26.
!
!  Modified:
!
!    16 April 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of elements in the vector.
!
!    Input, integer A(N), the array to be searched.  A must
!    be sorted in descending order.
!
!    Input, integer B, the value to be searched for.
!
!    Output, integer INDX, the result of the search.
!    0, B does not occur in A.
!    I, A(I) = B.
!
  integer n
!
  integer a(n)
  integer b
  integer high
  integer indx
  integer low
  integer mid
!
  indx = 0

  low = 1
  high = n

  do while ( low <= high )

    mid = ( low + high ) / 2

    if ( a(mid) == b ) then
      indx = mid
      exit
    else if ( a(mid) > b ) then
      low = mid + 1
    else if ( a(mid) < b ) then
      high = mid - 1
    end if

  end do

  return
end
subroutine ivec_sort_insert_a ( n, a )
!
!*******************************************************************************
!
!! IVEC_SORT_INSERT_A uses an ascending insertion sort on an integer vector.
!
!
!  Reference:
!
!    Algorithm 1.1,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 11.
!
!  Modified:
!
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of items in the vector.
!    N must be positive.
!
!    Input/output, integer A(N).
!
!    On input, A contains data to be sorted.
!    On output, the entries of A have been sorted in ascending order.
!
  integer n
!
  integer a(n)
  integer i
  integer j
  integer x
!
  do i = 2, n

    x = a(i)

    j = i - 1

    do while ( j >= 1 )

      if ( a(j) <= x ) then
        exit
      end if

      a(j+1) = a(j)
      j = j - 1

    end do

    a(j+1) = x

  end do

  return
end
subroutine ivec_sort_insert_d ( n, a )
!
!*******************************************************************************
!
!! IVEC_SORT_INSERT_D uses a descending insertion sort on an integer vector.
!
!
!  Reference:
!
!    Algorithm 1.1,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 11.
!
!  Modified:
!
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of items in the vector.
!    N must be positive.
!
!    Input/output, integer A(N).
!
!    On input, A contains data to be sorted.
!    On output, the entries of A have been sorted in descending order.
!
  integer n
!
  integer a(n)
  integer i
  integer j
  integer x
!
  do i = 2, n

    x = a(i)

    j = i - 1

    do while ( j >= 1 )

      if ( a(j) >= x ) then
        exit
      end if

      a(j+1) = a(j)
      j = j - 1

    end do

    a(j+1) = x

  end do

  return
end
subroutine knapsack_01 ( n, mass_limit, p, w, x, mass, profit )
!
!*******************************************************************************
!
!! KNAPSACK_01 solves the 0/1 knapsack problem.
!
!
!  Description:
!
!    The 0/1 knapsack problem is as follows:
!
!      Given:
!        a set of N objects,
!        a profit P(I) and weight W(I) associated with each object,
!        and a weight limit MASS_LIMIT,
!      Determine:
!        a set of choices X(I) which are 0 or 1, that maximizes the profit
!          P = Sum ( 1 <= I <= N ) P(I) * X(I)
!        subject to the constraint
!          Sum ( 1 <= I <= N ) W(I) * X(I) <= MASS_LIMIT.
!
!    This routine assumes that the objects have already been sorted
!    in order of decreasing "profit density", P(I)/W(I).
!
!  Reference:
!
!    Algorithm 4.9,
!    Donald Kreher and Douglas Stinson,
!    Combinatorial Algorithms,
!    CRC Press, 1999, page 125.
!
!  Modified:
!
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of objects.
!
!    Input, real MASS_LIMIT, the weight limit of the chosen objects.
!
!    Input/output, real P(N), the "profit" or value of each object.
!    P is assumed to be nonnegative.
!
!    Input/output, real W(N), the "weight" or cost of each object.
!    W is assumed to be  nonnegative.
!
!    Output, real X(N), the choice function for the objects.
!    0, the object was not taken.
!    1, the object was taken.
!
!    Output, real MASS, the total mass of the objects taken.
!
!    Output, real PROFIT, the total profit of the objects taken.
!
  integer, parameter :: maxstack = 100
!
  integer n
!
  integer i
  integer indx
  integer j
  integer k
  real mass
  real mass_1
  real mass_2
  real mass_best
  real mass_limit
  real mass_remaining
  integer ncan(n)
  integer nstack
  real p(n)
  real profit
  real profit_1
  real profit_2
  real profit_best
  real stack(maxstack)
  real w(n)
  real x(n)
  real x_best(n)
!
  nstack = 0
!
!  Initialize the "best so far" data.
!
  x_best(1:n) = 0.0E+00
  profit_best = 0.0E+00
  mass_best = 0
!
!  Begin the backtracking solution.
!
  indx = 0

  do

    call rvec_backtrack ( n, x, indx, k, nstack, stack, maxstack, ncan )
!
!  Got a new candidate.  Compare it to the best so far.
!
    if ( indx == 1 ) then

      profit = dot_product ( p, x )
      mass = dot_product ( w, x )

      if ( profit > profit_best .or. &
         ( profit == profit_best .and. mass < mass_best ) ) then
        profit_best = profit
        mass_best = mass
        x_best(1:n) = x(1:n)
      end if
!
!  Need candidates for X(K).
!
!  X(K) = 1 is possible if:
!
!    * adding W(K) to our mass doesn't put us over our mass limit;
!    * and adding P(K) to our current profit, and taking the best we
!      could get using rational X for the remainder would put us over
!      our current best.
!
!  X(K) = 0 is always possible.
!
    else if ( indx == 2 ) then

      ncan(k) = 0

      mass_1 = dot_product ( w(1:k-1), x(1:k-1) ) + w(k)

      if ( mass_1 <= mass_limit ) then

        mass_remaining = mass_limit - mass_1

        profit_1 = dot_product ( p(1:k-1), x(1:k-1) ) + p(k)

        if ( k < n ) then
          call knapsack_rational ( n-k, mass_remaining, p(k+1), w(k+1), &
            x(k+1), mass_2, profit_2 )
        else
          profit_2 = 0.0E+00
        end if

        if ( profit_1 + profit_2 > profit_best ) then
          if ( nstack >= maxstack ) then
            write ( *, * ) ' '
            write ( *, * ) 'KNAPSACK - Fatal error!'
            write ( *, * ) '  Exceeded stack space.'
            return
          end if
          ncan(k) = ncan(k) + 1
          nstack = nstack + 1
          stack(nstack) = 1.0E+00
        end if

      end if

      if ( nstack >= maxstack ) then
        write ( *, * ) ' '
        write ( *, * ) 'KNAPSACK - Fatal error!'
        write ( *, * ) '  Exceeded stack space.'
        return
      end if

      ncan(k) = ncan(k) + 1
      nstack = nstack + 1
      stack(nstack) = 0.0E+00
!
!  Done.  Return the best solution.
!
    else

      profit = profit_best
      mass = mass_best
      x(1:n) = x_best(1:n)
      exit

    end if

  end do

  return
end
subroutine knapsack_rational ( n, mass_limit, p, w, x, mass, profit )
!
!*******************************************************************************
!
!! KNAPSACK_RATIONAL solves the rational knapsack problem.
!
!
!  Description:
!
!    The rational knapsack problem is a generalization of the 0/1 knapsack
!    problem.  It is mainly used to derive a bounding function for the
!    0/1 knapsack problem.
!
!    The 0/1 knapsack problem is as follows:
!
!      Given:
!        a set of N objects,
!        a profit P(I) and weight W(I) associated with each object,
!        and a weight limit MASS_LIMIT,
!      Determine:
!        a set of choices X(I) which are 0 or 1, that maximizes the profit
!          P = Sum ( 1 <= I <= N ) P(I) * X(I)
!        subject to the constraint
!          Sum ( 1 <= I <= N ) W(I) * X(I) <= MASS_LIMIT.
!
!    By contrast, the rational knapsack problem allows the values X(I)
!    to be any value between 0 and 1.  A solution for the rational knapsack
!    problem is known.  Arrange the objects in order of their "profit density"
!    ratios P(I)/W(I), and then take in order as many of these as you can.
!    If you still have "room" in the weight constraint, then you should
!    take the maximal fraction of the very next object, which will complete
!    your weight limit, and maximize your profit.
!
!    If should be obvious that, given the same data, a solution for
!    the rational knapsack problem will always have a profit that is
!    at least as high as for the 0/1 problem.  Since the rational knapsack
!    maximum profit is easily computed, this makes it a useful bounding
!    function.
!
!    Note that this routine assumes that the objects have already been
!    arranged in order of the "profit density".
!
!  Reference:
!
!    Algorithm 4.8,
!    Donald Kreher and Douglas Stinson,
!    Combinatorial Algorithms,
!    CRC Press, 1999, page 124.
!
!  Modified:
!
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of objects.
!
!    Input, real MASS_LIMIT, the weight limit of the chosen objects.
!
!    Input, real P(N), the "profit" or value of each object.
!    The entries of P are assumed to be nonnegative.
!
!    Input, real W(N), the "weight" or cost of each object.
!    The entries of W are assumed to be nonnegative.
!
!    Output, real X(N), the choice function for the objects.
!    0.0, the object was not taken.
!    1.0, the object was taken.
!    R, where 0 < R < 1, a fractional amount of the object was taken.
!
!    Output, real MASS, the total mass of the objects taken.
!
!    Output, real PROFIT, the total profit of the objects taken.
!
  integer n
!
  integer i
  real mass
  real mass_limit
  real p(n)
  real profit
  real w(n)
  real x(n)
!
  mass = 0.0E+00
  profit = 0.0E+00

  do i = 1, n

    if ( mass >= mass_limit ) then
      x(i) = 0.0E+00
    else if ( mass + w(i) <= mass_limit ) then
      x(i) = 1.0E+00
      mass = mass + w(i)
      profit = profit + p(i)
    else
      x(i) = ( mass_limit - mass ) / w(i) 
      mass = mass_limit
      profit = profit + p(i) * x(i)
    end if

  end do

  return
end
subroutine knapsack_reorder ( n, p, w )
!
!*******************************************************************************
!
!! KNAPSACK_REORDER reorders the knapsack data by "profit density".
!
!
!  Description:
!
!    This routine must be called to rearrange the data before calling
!    routines that handle a knapsack problem.
!
!    The "profit density" for object I is defined as P(I)/W(I).
!
!  Reference:
!
!    Algorithm 4.8,
!    Donald Kreher and Douglas Stinson,
!    Combinatorial Algorithms,
!    CRC Press, 1999, page 124.
!
!  Modified:
!
!    24 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of objects.
!
!    Input/output, real P(N), the "profit" or value of each object.
!
!    Input/output, real W(N), the "weight" or cost of each object.
!
  integer n
!
  integer i
  integer j
  real p(n)
  real w(n)
!
!  Rearrange the objects in order of "profit density".
!
  do i = 1, n
    do j = i+1, n
      if ( p(j) * w(i) > p(i) * w(j) ) then
        call r_swap ( p(i), p(j) )
        call r_swap ( w(i), w(j) )
      end if
    end do
  end do

  return
end
subroutine ksubset_colex_check ( k, n, t, ierror )
!
!*******************************************************************************
!
!! KSUBSET_COLEX_CHECK checks a K subset in colex form.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input, integer T(K), describes a K subset.  T(I) is the I-th 
!    element of the K subset.  The elements must be listed in 
!    DESCENDING order.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N is not positive.
!    -2, K is not positive.
!    I, entry I is illegal.
!
  integer k
!
  integer i
  integer ierror
  integer n
  integer t(k)
  integer tmax
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if

  if ( k < 1 .or. k > n ) then
    ierror = -2
    return
  end if

  tmax = n + 1

  do i = 1, k

    if ( t(i) <= 0 .or. t(i) >= tmax ) then
      ierror = i
      return
    end if

    tmax = t(i)

  end do

  return
end
subroutine ksubset_colex_rank ( k, n, t, rank )
!
!*******************************************************************************
!
!! KSUBSET_COLEX_RANK computes the colex rank of a K subset.
!
!
!  Reference:
!
!    Algorithm 2.9,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 46.
!
!  Modified:
!
!    15 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input, integer T(K), describes a K subset.  T(I) is the I-th 
!    element of the K subset.  The elements must be listed in DESCENDING order.
!
!    Output, integer RANK, the rank of the subset.
!
  integer k
!
  integer binomial
  integer i
  integer ierror
  integer n
  integer rank
  integer t(k)
!
!  Check.
!
  call ksubset_colex_check ( k, n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_COLEX_CHECK - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  rank = 0

  do i = 1, k
    rank = rank + binomial ( t(i) - 1, k + 1 - i )
  end do

  return
end
subroutine ksubset_colex_successor ( k, n, t, rank )
!
!*******************************************************************************
!
!! KSUBSET_COLEX_SUCCESSOR computes the K subset colex successor.
!
!
!  Discussion:
!
!    In the original code, there is a last element with no successor.
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input/output, integer T(K), describes a K subset.  T(I) is the
!    I-th element.  The elements must be listed in DESCENDING order.
!
!    On input, T describes a K subset.
!
!    On output, T describes the next K subset in the ordering.
!    If the input T was the last in the ordering, then the output T
!    will be the first.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer k
!
  integer i
  integer ierror
  integer n
  integer rank
  integer t(k)
!
!  Return the first element.
!
  if ( rank == -1 ) then
    do i = 1, k
      t(i) = k + 1 - i
    end do
    rank = 0
    return
  end if
!
!  Check.
!
  call ksubset_colex_check ( k, n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_COLEX_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  do i = k-1, 1, -1
    if ( t(k+1-i) + 1 < t(k-i) ) then
      t(k+1-i) = t(k+1-i) + 1
      rank = rank + 1
      return
    end if
  end do

  if ( t(1) < n ) then
    t(1) = t(1) + 1
    do i = 1, k - 1
      t(k+1-i) = i
    end do
    rank = rank + 1
    return
  end if
!
!  The last K subset was input.
!  Return the first one.
!
  do i = 1, k
    t(i) = k + 1 - i
  end do

  rank = 0

  return
end
subroutine ksubset_colex_unrank ( rank, k, n, t )
!
!*******************************************************************************
!
!! KSUBSET_COLEX_UNRANK computes the K subset of given colex rank.
!
!
!  Reference:
!
!    Algorithm 2.10,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 47.
!
!  Modified:
!
!    15 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the K subset.
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Output, integer T(K), describes the K subset of the given rank.  
!    T(I) is the I-th element.  The elements must be listed in
!    DESCENDING order.
!
  integer k
!
  integer binomial
  integer i
  integer n
  integer nksub
  integer rank
  integer rank_copy
  integer t(k)
  integer x
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_COLEX_UNRANK - Fatal error!'
    write ( *, * ) '  Input N is illegal.'
    stop
  end if

  if ( k < 1 .or. k > n ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_COLEX_UNRANK - Fatal error!'
    write ( *, * ) '  Input K is illegal.'
    stop
  end if

  call ksubset_enum ( k, n, nksub )

  if ( rank < 0 .or. rank > nksub ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_COLEX_UNRANK - Fatal error!'
    write ( *, * ) '  Input rank is illegal.'
    stop
  end if
!
  rank_copy = rank

  x = n

  do i = 1, k

    do while ( binomial ( x, k + 1 - i ) > rank_copy ) 
      x = x - 1
    end do

    t(i) = x + 1
    rank_copy = rank_copy - binomial ( x, k + 1 - i )

  end do

  return
end
subroutine ksubset_enum ( k, n, nksub )
!
!*******************************************************************************
!
!! KSUBSET_ENUM enumerates the K element subsets of an N set.
!
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of elements each K subset must have.
!    0 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    0 <= N.
!
!    Output, integer NKSUB, the number of distinct elements.
!
  integer binomial
  integer k
  integer n
  integer nksub
!
  nksub = binomial ( n, k )

  return
end
subroutine ksubset_lex_check ( k, n, t, ierror )
!
!*******************************************************************************
!
!! KSUBSET_LEX_CHECK checks a K subset in lex form.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input, integer T(K), describes a K subset.  T(I) is the I-th 
!    element of the K subset.  The elements must be listed in 
!    DESCENDING order.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N is illegal.
!    -2, K is illegal.
!    I, entry I is illegal.
!
  integer k
!
  integer i
  integer ierror
  integer n
  integer t(k)
  integer tmin
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if

  if ( k < 1 .or. k > n ) then
    ierror = -2
    return
  end if

  tmin = 0

  do i = 1, k

    if ( t(i) <= tmin .or. t(i) > n ) then
      ierror = i
      return
    end if

    tmin = t(i)

  end do

  return
end
subroutine ksubset_lex_rank ( k, n, t, rank )
!
!*******************************************************************************
!
!! KSUBSET_LEX_RANK computes the lexicographic rank of a K subset.
!
!
!  Reference:
!
!    Algorithm 2.7,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 44.
!
!  Modified:
!
!    14 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input, integer T(K), describes a K subset.  T(I) is the I-th
!    element.  The elements must be listed in ascending order.
!
!    Output, integer RANK, the rank of the K subset.
!
  integer k
!
  integer binomial
  integer i
  integer ierror
  integer j
  integer n
  integer rank
  integer t(k)
  integer tim1
!
!  Check.
!
  call ksubset_lex_check ( k, n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_LEX_RANK - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    stop
  end if

  rank = 0

  do i = 1, k

    if ( i == 1 ) then
      tim1 = 0
    else
      tim1 = t(i-1)
    end if

    if ( tim1 + 1 <= t(i) - 1 ) then
      do j = tim1 + 1, t(i) - 1
        rank = rank + binomial ( n - j, k - i )
      end do
    end if

  end do

  return
end
subroutine ksubset_lex_successor ( k, n, t, rank )
!
!*******************************************************************************
!
!! KSUBSET_LEX_SUCCESSOR computes the K subset lexicographic successor.
!
!
!  Discussion:
!
!    In the original code, there is a last element with no successor.
!
!  Reference:
!
!    Algorithm 2.6,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 43.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input/output, integer T(K), describes a K subset.  T(I) is the I-th
!    element.  The elements must be listed in ascending order.
!
!    On input, T describes a K subset.
!
!    On output, T describes the next K subset in the ordering.
!    If the input T was the last in the ordering, then the output T
!    will be the first.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer k
!
  integer i
  integer ierror
  integer isave
  integer j
  integer n
  integer rank
  integer t(k)
!
!  Return the first element.
!
  if ( rank == -1 ) then
    do i = 1, k
      t(i) = i
    end do
    rank = 0
    return
  end if
!
!  Check.
!
  call ksubset_lex_check ( k, n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_LEX_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  isave = 0

  do i = k, 1, -1
    if ( t(i) /= n - k + i ) then
      isave = i
      exit
    end if
  end do
!
!  The last K subset was input.
!  Return the first one.
!
  if ( isave == 0 ) then

    do i = 1, k
      t(i) = i
    end do

    rank = 0

  else

    do j = k, isave, -1
      t(j) = t(isave) + 1 + j - isave
    end do

    rank = rank + 1

  end if

  return
end
subroutine ksubset_lex_unrank ( rank, k, n, t )
!
!*******************************************************************************
!
!! KSUBSET_LEX_UNRANK computes the K subset of given lexicographic rank.
!
!
!  Reference:
!
!    Algorithm 2.8,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 44.
!
!  Modified:
!
!    15 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the K subset.
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Output, integer T(K), describes the K subset of the given rank.  
!    T(I) is the I-th element.  The elements must be listed in
!    ascending order.
!
  integer k
!
  integer binomial
  integer i
  integer n
  integer nksub
  integer rank
  integer rank_copy
  integer t(k)
  integer x
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_LEX_RANK - Fatal error!'
    write ( *, * ) '  Input N is illegal.'
    stop
  end if

  if ( k < 1 .or. k > n ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_LEX_RANK - Fatal error!'
    write ( *, * ) '  Input K is illegal.'
    stop
  end if

  call ksubset_enum ( k, n, nksub )

  if ( rank < 0 .or. rank > nksub ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_LEX_UNRANK - Fatal error!'
    write ( *, * ) '  Input rank is illegal.'
    stop
  end if
!
  rank_copy = rank

  x = 1

  do i = 1, k

    do while ( binomial ( n - x, k - i ) <= rank_copy ) 
      rank_copy = rank_copy - binomial ( n - x, k - i )
      x = x + 1
    end do

    t(i) = x
    x = x + 1

  end do

  return
end
subroutine ksubset_revdoor_rank ( k, n, t, rank )
!
!*******************************************************************************
!
!! KSUBSET_REVDOOR_RANK computes the revolving door rank of a K subset.
!
!
!  Reference:
!
!    Algorithm 2.11,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 51.
!
!  Modified:
!
!    24 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input, integer T(K), describes a K subset.  T(I) is the I-th
!    element.  The elements must be listed in ascending order.
!
!    Output, integer RANK, the rank of the K subset.
!
  integer k
!
  integer binomial
  integer i
  integer ierror
  integer n
  integer rank
  integer s
  integer t(k)
!
!  Check.
!
  call ksubset_lex_check ( k, n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_REVDOOR_RANK - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
  if ( mod ( k, 2 ) == 0 ) then

    rank = 0

  else

    rank = - 1

  end if

  s = 1

  do i = k, 1, -1
    rank = rank + s * binomial ( t(i), i )
    s = - s
  end do

  return
end
subroutine ksubset_revdoor_successor ( k, n, t, rank )
!
!*******************************************************************************
!
!! KSUBSET_REVDOOR_SUCCESSOR computes the K subset revolving door successor.
!
!
!  Reference:
!
!    A Nijenhuis and H Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!    Algorithm 2.13,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 52.
!
!  Note:
!
!    After numerous attempts to implement the algorithm published in 
!    Kreher and Simpson, the Nijenhuis and Wilf version was implemented
!    instead.  The K and S algorithm is supposedly based on the N and W one.
!
!  Modified:
!
!    31 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input/output, integer T(K), describes a K subset.  T(I) is the
!    I-th element.  The elements must be listed in ascending order.
!
!    On input, T describes a K subset.
!
!    On output, T describes the next K subset in the ordering.
!    If the input T was the last in the ordering, then the output T
!    will be the first.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer k
!
  integer i
  integer ierror
  integer j
  integer n
  integer rank
  integer t(k)
!
!  Return the first element.
!
  if ( rank == - 1 ) then
    do i = 1, k
      t(i) = i
    end do
    rank = 0
    return
  end if
!
!  Check.
!
  call ksubset_lex_check ( k, n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_REVDOOR_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  j = 0

  do

    if ( j > 0 .or. mod ( k, 2 ) == 0 ) then
 
      j = j + 1
 
      if ( j > k ) then
        t(k) = k
        rank = 0
        return
      end if
 
      if ( t(j) /= j ) then
 
        t(j) = t(j) - 1
 
        if ( j /= 1 ) then
          t(j-1) = j - 1
        end if
 
        rank = rank + 1
        return
 
      end if
 
    end if
 
    j = j + 1

    if ( j < k ) then
      if ( t(j) /= t(j+1) - 1 ) then
        exit
      end if
    else
      if ( t(j) /= n ) then
        exit
      end if
    end if

  end do

  t(j) = t(j) + 1
 
  if ( j /= 1 ) then
    t(j-1) = t(j) - 1
  end if
 
  rank = rank + 1

  return
end
subroutine ksubset_revdoor_unrank ( rank, k, n, t )
!
!*******************************************************************************
!
!! KSUBSET_REVDOOR_UNRANK computes the K subset of given revolving door rank.
!
!
!  Reference:
!
!    Algorithm 2.12,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 51.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the K subset.
!
!    Input, integer K, the number of elements each K subset must have.
!    1 <= K <= N.
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Output, integer T(K), describes the K subset of the given rank.  
!    T(I) is the I-th element.  The elements must be listed in
!    ascending order.
!
  integer k
!
  integer binomial
  integer i
  integer n
  integer nksub
  integer rank
  integer rank_copy
  integer t(k)
  integer x
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_REVDOOR_UNRANK - Fatal error!'
    write ( *, * ) '  Input N is illegal.'
    stop
  end if

  if ( k < 1 .or. k > n ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_REVDOOR_UNRANK - Fatal error!'
    write ( *, * ) '  Input K is illegal.'
    stop
  end if

  call ksubset_enum ( k, n, nksub )

  if ( rank < 0 .or. rank > nksub ) then
    write ( *, * ) ' '
    write ( *, * ) 'KSUBSET_REVDOOR_UNRANK - Fatal error!'
    write ( *, * ) '  Input rank is illegal.'
    stop
  end if

  rank_copy = rank

  x = n

  do i = k, 1, -1

    do while ( binomial ( x, i ) > rank_copy ) 
      x = x - 1
    end do

    t(i) = x + 1
    rank_copy = binomial ( x + 1, i ) - rank_copy - 1

  end do

  return
end
subroutine marriage ( n, prefer, rank, fiancee, next )
!
!*******************************************************************************
!
!! MARRIAGE finds a stable set of marriages for given preferences.
!
!
!  Discussion:
!
!    Given a set of N men and N women who must be married in pairs,
!    and information defining the relative rankings that each person
!    assigns to the candidates of the opposite sex, this routine finds
!    a stable set of marriages for them.
!
!    A stable set of marriages is a pairing of the men and women with
!    the stability property: if M1 marries W1 and M2 marries W2, then
!    it is never the case that M1 and W2 would both prefer to be married
!    to each other.
!
!    An important application of stable marriage algorithms occurs in
!    the annual matching of medical residents to hospitals.
!
!  Reference:
!
!    Robert Sedgewick,
!    Algorithms,
!    Addison-Wesley, 1983.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of pairs of men and women.
!
!    Input, integer PREFER(N,N); for man I, the value of PREFER(I,J)
!    represents his J-th preference for a wife.
!
!    Input, integer RANK(N,N); for woman I, the value of RANK(I,J)
!    represents her ranking of man number J.  A value of 1 for RANK(I,J)
!    means woman I ranks man J most preferable, while a value of N
!    would mean she ranked him least preferable.
!
!    Output, integer FIANCEE(N); for woman I, FIANCEE(I) is the man
!    to whom she is now engaged.
!
!    Output, integer NEXT(N); for man I, NEXT(I) is his preference
!    ranking for the woman to whom he is now engaged.  A value of 1 represents
!    his first choice, a value of N his last.
!
  integer n
!
  integer fiancee(n)
  integer i
  integer m
  integer next(n)
  integer prefer(n,n)
  integer rank(n,n)
  integer w
!
!  For man I, NEXT(I) is the woman I has most recently proposed to,
!  and hence NEXT(I)+1 is the next one to try.
!
  next(1:n) = 0
!
!  For woman I, FIANCEE(I) is the man she has agree to marry,
!  or 0 if she has not agreed to any man yet.
!
  fiancee(1:n) = 0
!
!  Start with an unengaged man, and end with an engaged woman.
!
  do i = 1, n

    m = i

    do

      next(m) = next(m) + 1

      w = prefer(m,next(m))

      if ( fiancee(w) == 0 ) then
        fiancee(w) = m
        exit
      end if

      if ( rank (w,m) < rank(w,fiancee(w)) ) then
        call i_swap ( fiancee(w), m )
      end if

    end do

  end do

  return
end
subroutine mountain ( n, x, y, mxy )
!
!*******************************************************************************
!
!! MOUNTAIN enumerates the mountains.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 98.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, ...
!    N must be positive.
!
!    Input, integer X, Y, ...
!    0 <= X <= 2 * N, 
!
!    Output, integer MXY, the value of the "mountain function"
!    M ( N, X, Y ), which is the number of all mountain ranges from
!    (X,Y) to (2*N,0) which do not drop below sea level.
!
  integer a
  integer b
  integer binomial
  integer c
  integer mxy
  integer n
  integer x
  integer y
!
!  Check.
!
  if ( n <= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'MOUNTAIN - Fatal error!'
    write ( *, * ) '  N <= 0.'
    write ( *, * ) '  N = ', n
    stop
  else if ( x < 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'MOUNTAIN - Fatal error!'
    write ( *, * ) '  X < 0.'
    write ( *, * ) '  X = ', x
    stop
  else if ( x > 2 * n ) then
    write ( *, * ) ' '
    write ( *, * ) 'MOUNTAIN - Fatal error!'
    write ( *, * ) '  X > 2 * N.'
    write ( *, * ) '  X = ', x
    write ( *, * ) '  N = ', n
    stop
  end if
!
!  Special cases.
!
  if ( y < 0 ) then
    mxy = 0
    return
  end if

  if ( x + y > 2 * n ) then
    mxy = 0
    return
  end if

  if ( mod ( x + y, 2 ) == 1 ) then
    mxy = 0
    return
  end if

  a = 2 * n - x
  b = n - ( x + y ) / 2
  c = n - 1 - ( x + y ) / 2

  mxy = binomial ( a, b ) - binomial ( a, c )

  return
end
subroutine npart_enum ( n, npart, npartitions )
!
!*******************************************************************************
!
!! NPART_ENUM enumerates the number of partitions of N with NPART parts.
!
!
!  Reference:
!
!    Algorithm 3.6,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 74.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    Normally N must be positive, but for this routine any
!    N is allowed.
!
!    Input, integer NPART, the number of parts of the partition.  
!    Normally, 1 <= NPART <= N is required,
!    but for this routine any value of NPART is allowed.
!
!    Output, integer NPARTITIONS is the number of partitions of N
!    with NPART parts.
!
  integer n
!
  integer npart
  integer npartitions
  integer p(0:n,0:n)
!
  if ( n <= 0 ) then

    npartitions = 0

  else if ( npart <= 0 .or. npart > n ) then

    npartitions = 0

  else

    call npart_table ( n, npart, n, p )

    npartitions = p(n,npart)

  end if

  return
end
subroutine npart_rsf_lex_rank ( n, npart, a, rank )
!
!*******************************************************************************
!
!! NPART_RSF_LEX_RANK computes the lex rank of an RSF NPART partition.
!
!
!  Reference:
!
!    Algorithm 3.8,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 77.
!
!  Modified:
!
!    27 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input, integer NPART, the number of parts of the partition.  
!    1 <= NPART <= N.
!
!    Input, integer A(NPART), contains the partition.
!    A(1) through A(NPART) contain the nonzero integers which
!    sum to N.
!
!    Output, integer RANK, the rank of the partition.
!
  integer n
  integer npart
!
  integer a(npart)
  integer b(npart)
  integer i
  integer ierror
  integer ncopy
  integer npartcopy
  integer p(0:n,0:npart)
  integer rank
!
!  Check.
!
  call part_rsf_check ( n, npart, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'NPART_RSF_LEX_RANK - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Get the table of partitions of N with NPART parts.
!
  call npart_table ( n, npart, n, p )
!
!  Copy the partition "backwards".
!
  do i = 1, npart
    b(i) = a(npart+1-i)
  end do

  rank = 0
  ncopy = n
  npartcopy = npart

  do while ( ncopy > 0 .and. npartcopy > 0 ) 

    if ( b(npartcopy) == 1 ) then

      ncopy = ncopy - 1
      npartcopy = npartcopy - 1

    else

      do i = 1, npartcopy
        b(i) = b(i) - 1
      end do
      rank = rank + p(ncopy-1,npartcopy-1)
      ncopy = ncopy - npartcopy

    end if

  end do

  return
end
subroutine npart_rsf_lex_successor ( n, npart, a, rank )
!
!*******************************************************************************
!
!! NPART_RSF_LEX_SUCCESSOR computes the RSF lex successor for NPART partitions.
!
!
!  Reference:
!
!    Algorithm 3.7,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 76.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be at least 1.
!
!    Input, integer NPART, the number of parts of the partition.
!    1 <= NPART <= N.
!
!    Input/output, integer A(NPART), contains the partition.
!    A(1) through A(NPART) contain the nonzero integers which
!    sum to N.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer npart
!
  integer a(npart)
  integer d
  integer i
  integer ierror
  integer j
  integer n
  integer rank
!
!  Return the first element.
!
  if ( rank == -1 ) then

    if ( npart < 1 ) then
      write ( *, * ) ' '
      write ( *, * ) 'NPART_RSF_LEX_SUCCESSOR - Fatal error!'
      write ( *, * ) '  NPART < 1.'
      stop
    end if
 
    a(1:npart-1) = 1
    a(npart) = n - ( npart - 1 )

    rank = 0
    return

  end if
!
!  Check.
!
  call part_rsf_check ( n, npart, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'NPART_RSF_LEX_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Find the first index I for which A(NPART) > A(NPART+1-I) + 1.
!
  i = 2

  do
  
    if ( i > npart ) then
      exit
    end if

    if ( a(npart) > a(npart+1-i) + 1 ) then
      exit
    end if

    i = i + 1

  end do
!
!  If no such index, we've reached the end of the line.
!
  if ( i == npart + 1 ) then

    a(1:npart-1) = 1
    a(npart) = n - ( npart - 1 )

    rank = 0
    return
!
!  Otherwise, increment A(NPART+1-I), and adjust other entries.
!
  else

    a(npart+1-i) = a(npart+1-i) + 1
    d = - 1

    do j = i - 1, 2, -1
      d = d + a(npart+1-j) - a(npart+1-i)
      a(npart+1-j) = a(npart+1-i)
    end do

    a(npart) = a(npart) + d

  end if

  rank = rank + 1

  return
end
subroutine npart_rsf_lex_unrank ( rank, n, npart, a )
!
!*******************************************************************************
!
!! NPART_RSF_LEX_UNRANK unranks an RSF NPART partition in the lex ordering.
!
!
!  Reference:
!
!    Algorithm 3.9,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 78.
!
!  Modified:
!
!    03 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the partition.
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input, integer NPART, the number of parts of the partition.  
!    1 <= NPART <= N.
!
!    Output, integer A(NPART), contains the partition.
!    A(1) through A(NPART) contain the nonzero integers which
!    sum to N.
!
  integer n
  integer npart
!
  integer a(npart)
  integer i
  integer ncopy
  integer npartcopy
  integer npartitions
  integer p(0:n,0:npart)
  integer rank
  integer rankcopy
!
!  Check.
!
  if ( n <= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'NPART_RSF_LEX_UNRANK - Fatal error!'
    write ( *, * ) '  The input N is illegal.'
    stop
  end if

  if ( npart < 1 .or. npart > n ) then
    write ( *, * ) ' '
    write ( *, * ) 'NPART_RSF_LEX_UNRANK - Fatal error!'
    write ( *, * ) '  The input NPART is illegal.'
    stop
  end if

  call npart_enum ( n, npart, npartitions )

  if ( rank < 0 .or. rank > npartitions ) then
    write ( *, * ) ' '
    write ( *, * ) 'NPART_RSF_LEX_UNRANK - Fatal error!'
    write ( *, * ) '  The input rank is illegal.'
    stop
  end if
!
!  Get the table of partitions of N with NPART parts.
!
  call npart_table ( n, npart, n, p )

  a(1:npart) = 0

  rankcopy = rank
  ncopy = n
  npartcopy = npart

  do while ( ncopy > 0 )

    if ( rankcopy < p(ncopy-1,npartcopy-1) ) then
      a(npart+1-npartcopy) = a(npart+1-npartcopy) + 1
      ncopy = ncopy - 1
      npartcopy = npartcopy - 1
    else
      do i = 1, npartcopy
        a(npart+1-i) = a(npart+1-i) + 1
      end do
      rankcopy = rankcopy - p(ncopy-1,npartcopy-1)
      ncopy = ncopy - npartcopy
    end if

  end do

  return
end
subroutine npart_sf_lex_successor ( n, npart, a, rank )
!
!*******************************************************************************
!
!! NPART_SF_LEX_SUCCESSOR computes SF NPART partition.
!
!
!  Reference:
!
!    Algorithm 3.4,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 70.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input, integer NPART, the number of parts of the partition.
!    1 <= NPART <= N.
!
!    Input/output, integer A(NPART), contains the partition.
!    A(1) through A(NPART) contain the nonzero integers which
!    sum to N.  The values in A must be in DESCENDING order.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer npart
!
  integer a(npart)
  integer i
  integer ierror
  integer indx
  integer n
  integer rank
  integer temp
!
!  Return the first element.
!
  if ( rank == -1 ) then
    call ivec_part2 ( n, npart, a )
    rank = 0
    return
  end if
!
!  Check.
!
  call part_sf_check ( n, npart, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'NPART_SF_LEX_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Find the last entry that is 2 or more.
!
  do i = npart, 1, - 1
    if ( a(i) > 1 ) then
      indx = i
      exit
    end if
  end do
!
!  As long as the last nonunit occurs after the first position,
!  have it donate 1 to the left.
!
  if ( indx > 1 ) then

    a(indx) = a(indx) - 1
    a(indx-1) = a(indx-1) + 1
    indx = indx - 1

    do

      if ( indx <= 1 ) then
        exit
      end if

      if ( a(indx) <= a(indx-1) ) then
        exit
      end if

      call i_swap ( a(indx), a(indx-1) )
      indx = indx - 1

    end do
!
!  Sum the tail.
!
    temp = sum ( a(indx+1:npart) )
!
!  Partition the tail sum equally over the tail.
!
    call ivec_part2 ( temp, npart - indx, a(indx+1) )

    rank = rank + 1
!
!  If A(2) through A(NPART) are 1, then this is the last element.
!  Return the first one.
!
  else   
     
    call ivec_part2 ( n, npart, a )
    rank = 0

  end if

  return
end
subroutine npart_table ( n, npart, nmax, p )
!
!*******************************************************************************
!
!! NPART_TABLE tabulates the number of partitions of N having NPART parts.
!
!
!  Reference:
!
!    Algorithm 3.5,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 72.
!
!  Modified:
!
!    17 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input, integer NPART, the number of parts of the partition.
!    1 <= NPART <= N.
!
!    Input, integer NMAX, the leading dimension of P.
!
!    Output, integer P(0:NMAX,0:NPART), P(I,J) is the number of 
!    partitions of I having J parts.
!
  integer nmax
  integer npart
!
  integer i
  integer j
  integer n
  integer p(0:nmax,0:npart)
!
  p(0,0) = 1
  p(1:n,0) = 0

  do i = 1, n
    do j = 1, npart
      if ( i < j ) then
        p(i,j) = 0
      else if ( i < 2 * j ) then
        p(i,j) = p(i-1,j-1)
      else
        p(i,j) = p(i-1,j-1) + p(i-j,j)
      end if
    end do
  end do

  return
end
subroutine part_enum ( n, npartitions )
!
!*******************************************************************************
!
!! PART_ENUM enumerates the number of partitions of N.
!
!
!  Reference:
!
!    Algorithm 3.6,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 74.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    Normally N must be positive, but for this routine any
!    N is allowed.
!
!    Output, integer NPARTITIONS is the number of partitions of N.
!
  integer n
!
  integer npartitions
  integer p(0:n)
!
  if ( n < 0 ) then

    npartitions = 0

  else

    call part_table ( n, p )

    npartitions = p(n)

  end if

  return
end
subroutine part_rsf_check ( n, npart, a, ierror )
!
!*******************************************************************************
!
!! PART_RSF_CHECK checks a reverse standard form partition of an integer.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    24 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input, integer NPART, the number of parts of the partition.  
!    1 <= NPART <= N.
!
!    Input, integer A(NPART), contains the partition.
!    A(1) through A(NPART) contain the nonzero integers which
!    sum to N.  The entries must be in ASCENDING order.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N is illegal.
!    -2, NPART is illegal.
!    -3, the entries do not add up to N.
!    I, the I-th entry of A is illegal.
!
  integer npart
!
  integer a(npart)
  integer i
  integer ierror
  integer n
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if

  if ( npart < 1 .or. npart > n ) then
    ierror = -2
    return
  end if
!
!  Every entry must lie between 1 and N.
!
  do i = 1, npart
    if ( a(i) < 1 .or. a(i) > n ) then
      ierror = i
      return
    end if
  end do
!
!  The entries must be in ascending order.
!
  do i = 2, npart
    if ( a(i) < a(i-1) ) then
      ierror = i
      return
    end if
  end do
!
!  The entries must add up to N.
!
  if ( sum ( a(1:npart) ) /= n ) then
    ierror = -3
  end if

  return
end
subroutine part_sf_check ( n, npart, a, ierror )
!
!*******************************************************************************
!
!! PART_SF_CHECK checks a standard form partition of an integer.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input, integer NPART, the number of parts of the partition.  
!    1 <= NPART <= N.
!
!    Input, integer A(NPART), contains the partition.
!    A(1) through A(NPART) contain the nonzero integers which
!    sum to N.  The entries must be in DESCENDING order.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N is illegal.
!    -2, NPART is illegal.
!    -3, the entries do not add up to N.
!    I, the I-th entry of A is illegal.
!
  integer npart
!
  integer a(npart)
  integer i
  integer ierror
  integer n
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if

  if ( npart < 1 .or. npart > n ) then
    ierror = -2
    return
  end if
!
!  Every entry must lie between 1 and N.
!
  do i = 1, npart
    if ( a(i) < 1 .or. a(i) > n ) then
      ierror = i
      return
    end if
  end do
!
!  The entries must be in descending order.
!
  do i = 2, npart
    if ( a(i) > a(i-1) ) then
      ierror = i
      return
    end if
  end do
!
!  The entries must add up to N.
!
  if ( sum ( a(1:npart) ) /= n ) then
    ierror = -3
  end if

  return
end
subroutine part_sf_conjugate ( n, npart, a, npart2, b )
!
!*******************************************************************************
!
!! PART_SF_CONJUGATE computes the conjugate of a partition.
!
!
!  Reference:
!
!    Algorithm 3.2,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 69.
!
!  Modified:
!
!    17 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input, integer NPART, the number of parts of the partition.
!    1 <= NPART <= N.
!
!    Input, integer A(N), contains the partition.
!    A(1) through A(NPART) contain the nonzero integers which
!    sum to N.
!
!    Output, integer NPART2, the number of parts of the conjugate partition.
!
!    Output, integer B(N), contains the conjugate partition.
!
  integer n
!
  integer a(n)
  integer b(n)
  integer i
  integer ierror
  integer j
  integer npart
  integer npart2
!
!  Check.
!
  call part_sf_check ( n, npart, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PART_SF_CONJUGATE - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  npart2 = a(1)
  b(1:npart2) = 0

  do i = 1, npart
    do j = 1, a(i)
      b(j) = b(j) + 1
    end do
  end do

  return
end
subroutine part_sf_majorize ( n, nparta, a, npartb, b, result )
!
!*******************************************************************************
!
!! PART_SF_MAJORIZE determines if partition A majorizes partition B.
!
!
!  Discussion:
!
!    The partitions must be in standard form.
!
!    If A, with NPARTA parts, and B, with NPARTB parts, are both partitions
!    of the same positive integer N, then we say that A majorizes B if,
!    for every index K from 1 to N, it is true that
!
!      sum ( 1 <= I <= K ) A(I) >= sum ( 1 <= I <= K ) B(I)
!
!    where entries of A beyond index NPARTA, and of B beyond BPARTB
!    are assumed to be 0.  We say that A strictly majorizes B if
!    A majorizes B, and for at least one index K the inequality is strict.
!
!    For any two partitions of N, it is possible that A majorizes B,
!    B majorizes A, both partitions majorize each other (in which case
!    they are equal), or that neither majorizes the other.
!
!  Reference:
!
!    J H van Lint and R M Wilson,
!    A Course in Combinatorics,
!    Cambridge, 1996, page 148.
!
!  Modified:
!
!    05 July 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input, integer NPARTA, the number of parts in partition A.
!    1 <= NPARTA <= N.
!
!    Input, integer A(NPARTA), contains partition A in standard form.
!    A(1) through A(NPARTA) contain nonzero integers which sum to N.
!
!    Input, integer NPARTB, the number of parts in partition B.
!    1 <= NPARTB <= N.
!
!    Input, integer B(NPARTB), contains partition B in standard form.
!    B(1) through B(NPARTB) contain nonzero integers which sum to N.
!
!    Output, integer RESULT, the result of the comparison.
!    -1, A < B, (A is strictly majorized by B),
!     0, A = B, (A and B are identical),
!     1, A > B, (A strictly majorizes B),
!     2, A and B are incomparable.
!
  integer nparta
  integer npartb
!
  integer a(nparta)
  integer b(npartb)
  integer i
  integer ierror
  integer n
  integer result
  integer suma
  integer sumb
!
!  Check.
!
  call part_sf_check ( n, nparta, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PART_SF_MAJORIZE - Fatal error!'
    write ( *, * ) '  The input array A is illegal.'
    stop
  end if

  call part_sf_check ( n, npartb, b, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PART_SF_MAJORIZE - Fatal error!'
    write ( *, * ) '  The input array B is illegal.'
    stop
  end if

  result = 0
  suma = 0
  sumb = 0

  do i = 1, min ( nparta, npartb )

    if ( i <= nparta ) then
      suma = suma + a(i)
    end if

    if ( i <= npartb ) then
      sumb = sumb + b(i)
    end if

    if ( result == -1 ) then

      if ( suma > sumb ) then
        result = -2
        return
      end if

    else if ( result == 0 ) then

      if ( suma < sumb ) then
        result = -1
      else if ( suma > sumb ) then
        result = +1
      end if

    else if ( result == + 1 ) then

      if ( suma < sumb ) then
        result = -2
        return
      end if

    end if

  end do

  return
end
subroutine part_successor ( n, npart, a, rank )
!
!*******************************************************************************
!
!! PART_SUCCESSOR computes the lexicographic partition successor.
!
!
!  Discussion:
!
!    PART_SUCCESSOR is "inspired by" the GenPartitions algorithm,
!    but instead of relying on recursion, generates the partitions
!    one at a time.
!
!  Reference:
!
!    Algorithm 3.1,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 68.
!
!  Modified:
!
!    24 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input/output, integer NPART, the number of parts of the partition.
!    1 <= NPART <= N.
!
!    Input/output, integer A(N), contains the partition.
!    A(1) through A(NPART) contain the nonzero integers which
!    sum to N.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer n
!
  integer a(n)
  integer asum
  integer i
  integer ierror
  integer ihi
  integer ii
  integer npart
  integer rank
!
!  Return the first element.
!
  if ( rank == -1 ) then
    a(1:n) = 1
    npart = n
    rank = 0
    return
  end if
!
!  Check.
!
  call part_sf_check ( n, npart, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PART_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    stop
  end if
!
!  If possible, increment the first intermediate position that
!  is less than its left hand neighbor, and has at least one
!  right hand neighbor.
!
  ihi = npart - 1

  do i = ihi, 2, -1

    if ( a(i-1) > a(i) ) then
      asum = sum ( a(i+1:npart) ) - 1
      a(i) = a(i) + 1
      a(i+1:npart) = 0
      npart = i + asum
      a(i+1:npart) = 1
      rank = rank + 1
      return
    end if

  end do
!
!  A) there are two or more parts
!  Increment the first, replace the rest by 1's.
!
  if ( npart >= 2 ) then
    a(1) = a(1) + 1
    a(2:npart) = 0
    npart = n - a(1) + 1
    a(2:npart) = 1
    rank = rank + 1
!
!  B) there's only one part.
!  We've reached the last item.
!  Return the first one.
!
  else if ( npart == 1 ) then
    a(1:n) = 1
    npart = n
    rank = 0
  end if

  return
end
subroutine part_table ( n, p )
!
!*******************************************************************************
!
!! PART_TABLE tabulates the number of partitions of N.
!
!
!  Reference:
!
!    Algorithm 3.6,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 74.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Output, integer P(0:N), P(I) is the number of partitions of I.
!
  integer n
!
  integer i
  integer j
  integer p(0:n)
  integer psum
  integer sign
  integer w
  integer wprime
!
  p(0) = 1
  p(1) = 1

  do i = 2, n

    sign = 1
    psum = 0
    w = 1
    j = 1
    wprime = w + j

    do while ( w < n ) 

      if ( i - w >= 0 ) then
        if ( sign == 1 ) then
          psum = psum + p(i-w)
        else
          psum = psum - p(i-w)
        end if
      end if

      if ( wprime <= i ) then

        if ( sign == 1 ) then
          psum = psum + p(i-wprime)
        else
          psum = psum - p(i-wprime)
        end if

      end if

      w = w + 3 * j + 1
      j = j + 1
      wprime = w + j
      sign = - sign

    end do

    p(i) = psum

  end do

  return
end
subroutine partn_sf_check ( n, nmax, npart, a, ierror )
!
!*******************************************************************************
!
!! PARTN_SF_CHECK checks an SF partition of an integer with largest entry NMAX.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    26 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input, integer NMAX, the value of the largest entry.
!    1 <= NMAX <= N.
!
!    Input, integer NPART, the number of parts of the partition.  
!    1 <= NPART <= N.
!
!    Input, integer A(NPART), contains the partition.
!    A(1) through A(NPART) contain the nonzero integers which
!    sum to N.  The entries must be in DESCENDING order.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N is illegal.
!    -2, NMAX is illegal.
!    -3, NPART is illegal.
!    -3, the entries do not add up to N.
!    I, the I-th entry of A is illegal.
!
  integer npart
!
  integer a(npart)
  integer asum
  integer i
  integer ierror
  integer n
  integer nmax
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if

  if ( nmax < 1 .or. nmax > n ) then
    ierror = -2
    return
  end if

  if ( npart < 1 .or. npart > n ) then
    ierror = -3
    return
  end if
!
!  Entry 1 must be NMAX.
!
  if ( a(1) /= nmax ) then
    ierror = 1
    return
  end if
!
!  Every entry must lie between 1 and N.
!
  do i = 1, npart
    if ( a(i) < 1 .or. a(i) > n ) then
      ierror = i
      return
    end if
  end do
!
!  The entries must be in descending order.
!
  do i = 2, npart
    if ( a(i) > a(i-1) ) then
      ierror = i
      return
    end if
  end do
!
!  The entries must add up to N.
!
  asum = 0
  do i = 1, npart
    asum = asum + a(i)
    if ( asum > n ) then
      ierror = i
      return
    end if
  end do

  if ( asum /= n ) then
    ierror = -3
  end if

  return
end
subroutine partn_enum ( n, nmax, npartitions )
!
!*******************************************************************************
!
!! PARTN_ENUM enumerates the partitions of N with maximum element NMAX.
!
!
!  Reference:
!
!    Algorithm 3.6,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 74.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    Normally N must be positive, but for this routine any
!    N is allowed.
!
!    Input, integer NMAX, the maximum element in the partition. 
!    Normally, 1 <= NMAX <= N is required,
!    but for this routine any value of NMAX is allowed.
!
!    Output, integer NPARTITIONS is the number of partitions of N
!    with maximum element NMAX.
!
  integer, parameter :: NBIG = 25
!
  integer n
  integer nmax
  integer npartitions
  integer p(0:NBIG,0:NBIG)
!
  if ( n <= 0 ) then

    npartitions = 0

  else if ( nmax <= 0 .or. nmax > n ) then

    npartitions = 0

  else

    call npart_table ( n, nmax, NBIG, p )

    npartitions = p(n,nmax)

  end if

  return
end
subroutine partn_successor ( n, nmax, npart, a, rank )
!
!*******************************************************************************
!
!! PARTN_SUCCESSOR computes partitions whose largest part is NMAX.
!
!
!  Reference:
!
!    Algorithm 3.3,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 70.
!
!  Modified:
!
!    26 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the integer to be partitioned.
!    N must be positive.
!
!    Input, integer NMAX, the maximum size of any part of the
!    partition.  1 <= NMAX <= N.
!
!    Input/output, integer NPART, the number of parts of the partition.
!    1 <= NPART <= N.
!
!    Input/output, integer A(N), contains the partition.
!    A(1) through A(NPART) contain the nonzero integers which
!    sum to N.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer n
!
  integer a(n)
  integer i
  integer ierror
  integer index
  integer nmax
  integer npart
  integer rank
  integer temp
!
!  Return the first element.
!
  if ( rank == -1 ) then
    a(1) = nmax
    npart = n + 1 - nmax
    a(2:npart) = 1
    rank = 0
    return
  end if
!
!  Check.
!
  call partn_sf_check ( n, nmax, npart, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PARTN_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  If there are at least two parts, and the next to last is not NMAX,
!  then rob the last part and pay the next to the last part.
!  Then, if the next to last part is too big, swap it leftwards.
!
  if ( npart > 1 ) then

    if ( a(npart-1) < nmax ) then

      a(npart) = a(npart) - 1
      a(npart-1) = a(npart-1) + 1
      index = npart - 1

      do

        if ( index <= 1 ) then
          exit
        end if

        if ( a(index-1) >= a(index) ) then
          exit
        end if

        call i_swap ( a(index-1), a(index) )
        index = index - 1

      end do
!
!  Sum the tail.
!
      temp = sum ( a(index+1:npart) )
!
!  Spread the sum as 1's.
!
      npart = index + temp
      a(index+1:npart) = 1
      rank = rank + 1
      return

    end if
!
!  Otherwise, we've reached the last item.
!  Return the first one.
!
  else

    npart = n + 1 - nmax
    a(1) = nmax
    a(2:npart) = 1
    rank = 0
    return

  end if

  return
end
subroutine perm_check ( n, p, ierror )
!
!*******************************************************************************
!
!! PERM_CHECK checks a representation of a permutation.
!
!
!  Discussion:
!
!    The routine is given N and P, a vector of length N.  
!    P is a legal represention of a permutation of the integers from 
!    1 to N if and only if every integer from 1 to N occurs 
!    as a value of P(I) for some I between 1 and N.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Input, integer P(N), the array to check.
!
!    Output, integer IERROR, error flag.
!    0, P is a valid permutation.
!    -1, N is illegal.
!    I, P is not a valid permutation; value I is missing
!    from the range of P.
!
  integer n
!
  integer i
  integer ierror
  integer ifind
  integer iseek
  integer p(n)
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if

  do i = 1, n
    if ( p(i) < 1 .or. p(i) > n ) then
      ierror = i
      return
    end if
  end do

  do iseek = 1, n

    ifind = 0

    do i = 1, n
      if ( p(i) == iseek ) then
        ifind = i
        exit
      end if
    end do

    if ( ifind == 0 ) then
      ierror = iseek
      return
    end if

  end do

  return
end
subroutine perm_enum ( n, nperm )
!
!*******************************************************************************
!
!! PERM_ENUM enumerates the permutations on N digits.
!
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values being permuted.
!    N must be nonnegative.
!
!    Output, integer NPERM, the number of distinct elements.
!
  integer factorial
  integer n
  integer nperm
!
  nperm = factorial ( n )

  return
end
subroutine perm_inv ( n, p, pinv  )
!
!*******************************************************************************
!
!! PERM_INV computes the inverse of a permutation.
!
!
!  Reference:
!
!    Algorithm 6.2,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 199.
!
!  Modified:
!
!    22 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Input, integer P(N), describes the permutation.
!    P(I) is the item which is permuted into the I-th place
!    by the permutation.
!
!    Output, integer PINV(N), the inverse permutation.
!
  integer n
!
  integer i
  integer ierror
  integer p(n)
  integer pinv(n)
!
!  Check.
!
  call perm_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_INV - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  do i = 1, n
    pinv(p(i)) = i
  end do

  return
end
subroutine perm_lex_rank ( n, p, rank )
!
!*******************************************************************************
!
!! PERM_LEX_RANK computes the lexicographic rank of a permutation.
!
!
!  Sins:
!
!    The original code altered the input permutation.  The current
!    code uses an internal array, which limits the problem size.
!
!  Reference:
!
!    Algorithm 2.15,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 55.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Input, integer P(N), describes the permutation.
!    P(I) is the item which is permuted into the I-th place
!    by the permutation.
!
!    Output, integer RANK, the rank of the permutation.
!
  integer n
!
  integer factorial
  integer i
  integer ierror
  integer j
  integer p(n)
  integer pcopy(n)
  integer rank
!
!  Check.
!
  call perm_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_LEX_RANK - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  rank = 0
  pcopy(1:n) = p(1:n)

  do j = 1, n

    rank = rank + ( pcopy(j) - 1 ) * factorial ( n - j )

    do i = j + 1, n
      if ( pcopy(i) > pcopy(j) ) then
        pcopy(i) = pcopy(i) - 1
      end if
    end do

  end do

  return
end
subroutine perm_lex_successor ( n, p, rank )
!
!*******************************************************************************
!
!! PERM_LEX_SUCCESSOR computes the lexicographic permutation successor.
!
!
!  Reference:
!
!    Algorithm 2.14,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 53.
!
!  Example:
!
!    RANK  Permutation
!
!       0  1 2 3 4
!       1  1 2 4 3
!       2  1 3 2 4
!       3  1 3 4 2
!       4  1 4 2 3
!       5  1 4 3 2
!       6  2 1 3 4
!       ...
!      23  4 3 2 1
!
!  Modified:
!
!    01 February 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Input/output, integer P(N), describes the permutation.
!    P(I) is the item which is permuted into the I-th place
!    by the permutation.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer n
!
  integer i
  integer ierror
  integer j
  integer p(n)
  integer rank
!
!  Return the first element.
!
  if ( rank == -1 ) then

    do i = 1, n
      p(i) = i
    end do

    rank = 0
    return

  end if
!
!  Check.
!
  call perm_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_LEX_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Seek I, the highest index for which the next element is bigger.
!
  i = n - 1

  do

    if ( i <= 0 ) then
      exit
    end if

    if ( p(i) <= p(i+1) ) then
      exit
    end if

    i = i - 1

  end do
!
!  If no I could be found, then we have reach the final permutation,
!  N, N-1, ..., 2, 1.  Time to start over again.
!
  if ( i == 0 ) then

    do i = 1, n
      p(i) = i
    end do

    rank = 0

  else
!
!  Otherwise, look for the the highest index after I whose element
!  is bigger than I's.  We know that I+1 is one such value, so the
!  loop will never fail.
!
    j = n 
    do while ( p(j) < p(i) ) 
      j = j - 1
    end do
!
!  Interchange elements I and J.
!
    call i_swap ( p(j), p(i) )
!
!  Reverse the elements from I+1 to N.
!
    call ivec_reverse ( n - i, p(i+1) )

    rank = rank + 1

  end if

  return
end
subroutine perm_lex_unrank ( rank, n, p )
!
!*******************************************************************************
!
!! PERM_LEX_UNRANK computes the permutation of given lexicographic rank.
!
!
!  Reference:
!
!    Algorithm 2.16,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 56.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the permutation.
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Output, integer P(N), describes the permutation.
!
  integer n
!
  integer d
  integer factorial
  integer i
  integer j
  integer nperm
  integer p(n)
  integer rank
  integer rankcopy
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_LEX_UNRANK - Fatal error!'
    write ( *, * ) '  Input N is illegal.'
    stop
  end if

  call perm_enum ( n, nperm )

  if ( rank < 0 .or. rank > nperm ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_LEX_UNRANK - Fatal error!'
    write ( *, * ) '  Input rank is illegal.'
    stop
  end if

  rankcopy = rank

  p(n) = 1

  do j = 1, n - 1

    d = mod ( rankcopy, factorial ( j + 1 ) ) / factorial ( j )
    rankcopy = rankcopy - d * factorial ( j )
    p(n-j) = d + 1

    do i = n - j + 1, n

      if ( p(i) > d ) then
        p(i) = p(i) + 1
      end if

    end do

  end do

  return
end
subroutine perm_mul ( n, p, q, r  )
!
!*******************************************************************************
!
!! PERM_MUL computes the product of two permutations.
!
!
!  Reference:
!
!    Algorithm 6.1,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 199.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Input, integer P(N), Q(N), describes the permutation factors.
!
!    Output, integer R(N), the product permutation P * Q.
!    R(I) = P(Q(I)).
!
  integer n
!
  integer i
  integer ierror
  integer p(n)
  integer q(n)
  integer r(n)
  integer s(n)
!
!  Check.
!
  call perm_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_MUL - Fatal error!'
    write ( *, * ) '  The first input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  call perm_check ( n, q, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_MUL - Fatal error!'
    write ( *, * ) '  The second input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Use a temporary vector for the result, to avoid problems if
!  some arguments are actually identified.
!
  s(1:n) = p(q(1:n))

  r(1:n) = s(1:n)

  return
end
subroutine perm_parity ( n, p, parity )
!
!*******************************************************************************
!
!! PERM_PARITY computes the parity of a permutation.
!
!
!  Sins:
!
!    The routine requires the use of a temporary array.
!
!  Definition:
!
!    A permutation is called "even" or "odd", depending on whether
!    it is equivalent to an even or odd number of pairwise 
!    transpositions.  This is known as the "parity" of the 
!    permutation.
!
!    The "sign" of a permutation is +1 if it has even parity,
!    and -1 if it has odd parity.
!
!  Reference:
!
!    Algorithm 2.19,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 63.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Input, integer P(N), describes the permutation.
!    P(I) is the item which is permuted into the I-th place
!    by the permutation.
!
!    Output, integer PARITY, the parity of the permutation.
!    0, the permutation has even parity.
!    1, the permutation has odd parity.
!
  integer, parameter :: MAXN = 25
!
  integer n
!
  integer a(MAXN)
  integer c
  integer i
  integer ierror
  integer j
  integer parity
  integer p(n)
!
!  Check.
!
  call perm_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_PARITY - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  a(1:n) = 0

  c = 0

  do j = 1, n

    if ( a(j) == 0 ) then

      c = c + 1
      a(j) = 1
      i = j

      do while ( p(i) /= j ) 
        i = p(i)
        a(i) = 1
      end do

    end if

  end do

  parity = mod ( n - c, 2 )

  return
end
subroutine perm_tj_rank ( n, p, rank )
!
!*******************************************************************************
!
!! PERM_TJ_RANK computes the Trotter-Johnson rank of a permutation.
!
!
!  Reference:
!
!    Algorithm 2.17,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 60.
!
!  Modified:
!
!    16 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Input, integer P(N), describes the permutation.
!    P(I) is the item which is permuted into the I-th place
!    by the permutation.
!
!    Output, integer RANK, the rank of the permutation.
!
  integer n
!
  integer i
  integer ierror
  integer j
  integer k
  integer p(n)
  integer rank
!
!  Check.
!
  call perm_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_TJ_RANK - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  rank = 0

  do j = 2, n

    k = 1
    i = 1

    do while ( p(i) /= j ) 
      if ( p(i) < j ) then
        k = k + 1
      end if
      i = i + 1
    end do
    
    if ( mod ( rank, 2 ) == 0 ) then
      rank = j * rank + j - k
    else
      rank = j * rank + k - 1
    end if

  end do

  return
end
subroutine perm_tj_successor ( n, p, rank )
!
!*******************************************************************************
!
!! PERM_TJ_SUCCESSOR computes the Trotter-Johnson permutation successor.
!
!
!  Reference:
!
!    Algorithm 2.20,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 63.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Input/output, integer P(N), describes the permutation.
!    P(I) is the item which is permuted into the I-th place
!    by the permutation.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer n
!
  integer d
  logical done
  integer i
  integer ierror
  integer m
  integer p(n)
  integer par
  integer q(n)
  integer rank
  integer st
!
!  Return the first element.
!
  if ( rank == -1 ) then

    do i = 1, n
      p(i) = i
    end do

    rank = 0
    return

  end if
!
!  Check.
!
  call perm_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_TJ_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  st = 0
  q(1:n) = p(1:n)
  done = .false.
  m = n

  do while ( m > 1 .and. .not. done ) 

    d = 1
    do while ( q(d) /= m ) 
      d = d + 1 
    end do

    do i = d, m - 1
      q(i) = q(i+1)
    end do

    call perm_parity ( m-1, q, par )

    if ( par == 1 ) then

      if ( d == m ) then
        m = m - 1
      else
        call i_swap ( p(st+d), p(st+d+1) )
        done = .true.
      end if

    else

      if ( d == 1 ) then
        m = m - 1
        st = st + 1
      else
        call i_swap ( p(st+d), p(st+d-1) )
        done = .true.
      end if

    end if

  end do
!
!  Last element was input.  Return first one.
!
  if ( m == 1 ) then
    do i = 1, n
      p(i) = i
    end do
    rank = 0
    return
  end if

  rank = rank + 1

  return
end
subroutine perm_tj_unrank ( rank, n, p )
!
!*******************************************************************************
!
!! PERM_TJ_UNRANK computes the permutation of given Trotter-Johnson rank.
!
!
!  Reference:
!
!    Algorithm 2.18,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 61.
!
!  Modified:
!
!    17 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the permutation.
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Output, integer P(N), describes the permutation.
!
  integer n
!
  integer factorial
  integer i
  integer j
  integer k
  integer jhi
  integer nperm
  integer p(n)
  integer rank
  integer r1
  integer r2
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_TJ_UNRANK - Fatal error!'
    write ( *, * ) '  Input N is illegal.'
    stop
  end if

  call perm_enum ( n, nperm )

  if ( rank < 0 .or. rank > nperm ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_TJ_UNRANK - Fatal error!'
    write ( *, * ) '  Input rank is illegal.'
    stop
  end if

  p(1) = 1
  r2 = 0

  do j = 2, n
!
!  Replace this ratio of factorials!
!
    r1 = ( rank * factorial ( j ) ) / factorial ( n )
    k = r1 - j * r2

    if ( mod ( r2, 2 ) == 0 ) then
      jhi = j - k
    else
      jhi = k + 1
    end if

    do i = j - 1, jhi, -1
      p(i+1) = p(i)
    end do
    p(jhi) = j

    r2 = r1

  end do

  return
end
subroutine perm_to_cycle ( n, p, ncycle, t, index )
!
!*******************************************************************************
!
!! PERM_TO_CYCLE converts a permutation from array to cycle form.
!
!
!  Reference:
!
!    Algorithm 6.4,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 200.
!
!  Modified:
!
!    03 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values being permuted.
!    N must be positive.
!
!    Input, integer P(N), describes the permutation using a
!    single array.  For each index I, I -> P(I).
!
!    Output, integer NCYCLE, the number of cycles.
!    1 <= NCYCLE <= N.
!
!    Output, integer T(N), INDEX(N), describes the permutation
!    as a collection of NCYCLE cycles.  The first cycle is
!    T(1) -> T(2) -> ... -> T(INDEX(1)) -> T(1).
!
  integer n
  integer ncycle
!
  integer i
  integer ierror
  integer index(n)
  integer j
  integer nset
  integer p(n)
  integer t(n)
!
!  Check.
!
  call perm_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PERM_TO_CYCLE - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Initialize.
!
  ncycle = 0
  index(1:n) = 0
  t(1:n) = 0
  nset = 0
!
!  Find the next unused entry.      
!
  do i = 1, n

    if ( p(i) > 0 ) then

      ncycle = ncycle + 1
      index(ncycle) = 1

      nset = nset + 1
      t(nset) = p(i)
      p(i) = - p(i)

      do

        j = t(nset)

        if ( p(j) < 0 ) then
          exit
        end if

        index(ncycle) = index(ncycle) + 1

        nset = nset + 1
        t(nset) = p(j)
        p(j) = - p(j)

      end do

    end if

  end do
!
!  If no unused entries remain, we are done.
!  Restore the sign of the permutation and return.
!
  p(1:n) = - p(1:n)

  return
end
subroutine pruefer_check ( n, p, ierror )
!
!*******************************************************************************
!
!! PRUEFER_CHECK checks a Pruefer code.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of nodes in the tree.
!    N must be at least 3.
!
!    Input, integer P(N-2), the Pruefer code for the tree.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N is less than 3.
!    J, the element P(J) is illegal.
!
  integer n
!
  integer i
  integer ierror
  integer p(n-2)
!
  ierror = 0

  if ( n < 3 ) then
    ierror = -1
    return
  end if

  do i = 1, n-2
    if ( p(i) < 1 .or. p(i) > n ) then
      ierror = i
      return
    end if
  end do

  return
end
subroutine pruefer_enum ( n, ncode )
!
!*******************************************************************************
!
!! PRUEFER_ENUM enumerates the Pruefer codes on N-2 digits.
!
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of digits in the code, plus 2.
!    N must be at least 3.
!
!    Output, integer NCODE, the number of distinct elements.
!
  integer n
  integer ncode
!
  ncode = n**( n - 2 )

  return
end
subroutine pruefer_rank ( n, p, rank )
!
!*******************************************************************************
!
!! PRUEFER_RANK ranks a Pruefer code.
!
!
!  Reference:
!
!    Algorithm 3.20,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 93.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of nodes in the tree.
!    N must be at least 3.
!
!    Input, integer P(N-2), the Pruefer code for the tree.
!
!    Output, integer RANK, the rank of the Pruefer code.
!
  integer n
!
  integer i
  integer ierror
  integer k
  integer p(n-2)
  integer rank
!
!  Check.
!
  call pruefer_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PRUEFER_RANK - Fatal error!'
    write ( *, * ) '  Input array is illegal.'
    write ( *, * ) '  Error code = ', ierror
    stop
  end if

  rank = 0
  k = 1
  do i = n-2, 1, -1
    rank = rank + k * ( p(i) - 1 )
    k = k * n
  end do

  return
end
subroutine pruefer_successor ( n, p, rank )
!
!*******************************************************************************
!
!! PRUEFER_SUCCESSOR computes the lexical Pruefer sequence successor.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of nodes in the tree.
!    N must be at least 3.
!
!    Input, integer P(N-2), on input, the Pruefer code for a tree.
!    and on output, its lexical successor.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer n
!
  integer i
  integer ierror
  integer j
  integer rank
  integer p(n-2)
!
!  Return the first element.
!
  if ( rank == -1 ) then
    p(1:n-2) = 1
    rank = 0
    return
  end if
!
!  Check.
!
  call pruefer_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PRUEFER_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  j = n - 2

  do

    if ( p(j) /= n ) then
      exit
    end if

    j = j - 1

    if ( j <= 0 ) then
      exit
    end if

  end do

  if ( j /= 0 ) then
    p(j) = p(j) + 1
    p(j+1:n-2) = 1
    rank = rank + 1
  else
    p(1:n-2) = 1
    rank = 0
  end if

  return
end
subroutine pruefer_to_tree ( n, p, t )
!
!*******************************************************************************
!
!! PRUEFER_TO_TREE converts a Pruefer code to a tree.
!
!
!  Sins:
!
!    The original code attempts to tack on an extra entry to P.
!
!  Reference:
!
!    Algorithm 3.19,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 92.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of nodes in the tree.
!    N must be at least 3.
!
!    Input, integer P(N-2), the Pruefer code for the tree.
!
!    Output, integer T(2,N-1), describes the edges of the tree
!    as pairs of nodes.
!
  integer n
!
  integer d(n)
  integer i
  integer ierror
  integer j
  integer p(n-2)
  integer t(2,n-1)
  integer x
  integer y
!
!  Check.
!
  call pruefer_check ( n, p, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PRUEFER_TO_TREE - Fatal error!'
    write ( *, * ) '  The input array is illegal!'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Initialize the tree to 0.
!
  t(1:2,1:n-1) = 0
  d(1:n) = 1

  do i = 1, n-2
    d(p(i)) = d(p(i)) + 1
  end do

  do i = 1, n - 1

    x = n
    do while ( d(x) /= 1 )
      x = x - 1
    end do

    if ( i == n-1 ) then
      y = 1
    else
      y = p(i)
    end if

    d(x) = d(x) - 1
    d(y) = d(y) - 1
    t(1,i) = x
    t(2,i) = y

  end do

  return
end
subroutine pruefer_unrank ( rank, n, p )
!
!*******************************************************************************
!
!! PRUEFER_UNRANK unranks a Pruefer code.
!
!
!  Reference:
!
!    Algorithm 3.21,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 93.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer RANK, the rank of the Pruefer code.
!
!    Input, integer N, the number of nodes in the tree.
!    N must be at least 3.
!
!    Output, integer P(N-2), the Pruefer code for the tree.
!
  integer n
!
  integer i
  integer ncode
  integer p(n-2)
  integer rank
  integer rankcopy
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'PRUEFER_UNRANK - Fatal error!'
    write ( *, * ) '  Input N is illegal.'
    stop
  end if

  call pruefer_enum ( n, ncode )

  if ( rank < 0 .or. rank > ncode ) then
    write ( *, * ) ' '
    write ( *, * ) 'PRUEFER_UNRANK - Fatal error!'
    write ( *, * ) '  Input rank is illegal.'
    stop
  end if

  rankcopy = rank

  do i = n-2, 1, -1
    p(i) = mod ( rankcopy, n ) + 1
    rankcopy = ( rankcopy - p(i) + 1 ) / n
  end do

  return
end
subroutine queens ( n, iarray, k, nstack, istack, maxstack )
!
!*******************************************************************************
!
!! QUEENS finds possible positions for the K-th nonattacking queen.
!
!
!  Description:
!
!    The chessboard is N by N, and is being filled one column at a time,
!    with a tentative solution to the nonattacking queen problem.  So
!    far, K-1 rows have been chosen, and we now need to provide a list
!    of all possible rows that might be used in column K.
!
!  Modified:
!
!    05 July 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the total number of queens to place, and the length
!    of a side of the chessboard.
!
!    Input, integer IARRAY(N).  The first K-1 entries of IARRAY
!    record the rows into which queens have already been placed.
!
!    Input, integer K, the column for which we need possible row positions
!    for the next queen.
!
!    Input/output, integer NSTACK, the current length of stack.  On
!    output, this needs to be updated.
!
!    Workspace, integer ISTACK(MAXSTACK).  On output, we have added
!    the candidates, and the number of candidates, to the end of the
!    stack.
!
!    Input, integer MAXSTACK, maximum dimension of ISTACK.
!
  integer n
  integer maxstack
!
  logical diag
  integer iarray(n)
  integer irow
  integer istack(maxstack)
  integer jcol
  integer k
  integer ncan
  integer nstack
  logical row
!
  ncan = 0

  do irow = 1, n
!
!  If row IROW has already been used, that's it.
!
    row = .FALSE.

    do jcol = 1, k-1
      if ( iarray(jcol) == irow ) then
        row = .TRUE.
      end if
    end do

    if ( .not. row ) then

      diag = .FALSE.

      do jcol = 1, k-1

        if ( irow == iarray(jcol) + k - jcol .or. &
             irow == iarray(jcol) - ( k - jcol ) ) then

          diag = .TRUE.

        end if

      end do

      if ( .not. diag ) then
        ncan = ncan + 1
        nstack = nstack + 1
        istack(nstack) = irow
      end if

    end if

  end do

  nstack = nstack + 1
  istack(nstack) = ncan

  return
end
subroutine r_swap ( x, y )
!
!*******************************************************************************
!
!! R_SWAP swaps two real values.
!
!
!  Modified:
!
!    01 May 2000
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input/output, real X, Y.  On output, the values of X and
!    Y have been interchanged.
!
  real x
  real y
  real z
!
  z = x
  x = y
  y = z

  return
end
subroutine rgf_check ( m, f, ierror )
!
!*******************************************************************************
!
!! RGF_CHECK checks a restricted growth function.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the domain of the RGF is the integers from 1 to M.
!    M must be positive.
!
!    Input, integer F(M), the restricted growth function.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, M is illegal.
!    I, entry I of the restricted growth function is illegal.
!
  integer m
!
  integer f(m)
  integer fmax
  integer i
  integer ierror
!
  if ( m <= 0 ) then
    ierror = -1
    return
  end if

  fmax = 0
  do i = 1, m
    if ( f(i) <= 0 .or. f(i) > fmax + 1 ) then
      ierror = i
      return
    end if
    fmax = max ( fmax, f(i) )
  end do

  return
end
subroutine rgf_enum ( m, nrgf )
!
!*******************************************************************************
!
!! RGF_ENUM enumerates the restricted growth functions on M.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 81.
!
!  Modified:
!
!    03 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the domain of the RGF is the integers from 1 to M.
!    M must be positive.  However, for the enumeration routine only,
!    it is legal to call with any value of M.
!
!    Output, integer NRGF, the number of restricted growth functions.
!
  integer m
!
  integer b(0:m)
  integer binomial
  integer i
  integer j
  integer nrgf
!
  if ( m < 0 ) then

    nrgf = 0

  else if ( m == 0 ) then

    nrgf = 1

  else

    b(0) = 1
    do j = 1, m
      b(j) = 0
      do i = 0, j - 1
        b(j) = b(j) + binomial ( j - 1, i ) * b(i)
      end do
    end do

    nrgf = b(m)

  end if

  return
end
subroutine rgf_g_enum ( m, mmax, d )
!
!*******************************************************************************
!
!! RGF_G_ENUM enumerates the generalized restricted growth functions.
!
!
!  Example:
!
!    M = 6
!
!    D =  1    1    1    1    1    1    1
!         1    2    3    4    5    6    0
!         2    5   10   17   26    0    0
!         5   15   37   77    0    0    0
!        15   52  151    0    0    0    0
!        52  203    0    0    0    0    0
!       203    0    0    0    0    0    0
!
!  Reference:
!
!    Algorithm 3.14,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 85.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, indicates how many rows and columns are to
!    be computed.  M must be nonnegative.
!
!    Input, integer MMAX, the value used to allocate space for the
!    D array.  MMAX must be at least M.
!
!    Output, integer D(0:MMAX,0:MMAX), the first M+1 rows and M+1 columns of
!    the table of the number of generalized restricted growth functions.
!    D(I,J) is the number of GRGF's of length I with restriction
!    parameter J.
!
  integer mmax
!
  integer d(0:mmax,0:mmax)
  integer i
  integer j
  integer m
!
  d(0,0:m) = 1

  do i = 1, m
    do j = 0, m
      if ( j <= m - i ) then
        d(i,j) = j * d(i-1,j) + d(i-1,j+1)
      else
        d(i,j) = 0
      end if
    end do
  end do

  return
end
subroutine rgf_rank ( m, f, rank )
!
!*******************************************************************************
!
!! RGF_RANK ranks a restricted growth function.
!
!
!  Reference:
!
!    Algorithm 3.15,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 86.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the domain of the RGF is the integers from 1 to M.
!    M must be positive.
!
!    Input, integer F(M), the restricted growth function.
!
!    Output, integer RANK, the rank of the restricted growth function.
!
  integer m
!
  integer d(0:m,0:m)
  integer f(m)
  integer i
  integer ierror
  integer j
  integer rank
!
!  Check.
!
  call rgf_check ( m, f, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'RGF_RANK - Fatal error!'
    write ( *, * ) '  The input array is illegal!'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Get the generalized restricted growth function table.
!
  call rgf_g_enum ( m, m, d )

  rank = 0
  j = 1
  do i = 2, m
    rank = rank + ( f(i) - 1 ) * d(m-i,j)
    j = max ( j, f(i) )
  end do

  return
end
subroutine rgf_successor ( m, f, rank )
!
!*******************************************************************************
!
!! RGF_SUCCESSOR generates the next restricted growth function.
!
!
!  Reference:
!
!    Algorithm 3.13,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 84.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the domain of the RGF is the integers from 1 to M.
!    M must be positive.
!
!    Input/output, integer F(M), the restricted growth function.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer m
!
  integer f(m)
  integer fmax
  integer i
  integer ierror
  integer j
  integer rank
!
!  Return the first element.
!
  if ( rank == -1 ) then
    f(1:m) = 1
    rank = 0
    return
  end if
!
!  Check.
!
  call rgf_check ( m, f, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'RGF_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal!'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Find the first position from the right which can be incremented.
!
  do i = m, 2, -1

    fmax = 1
    do j = 2, i - 1
      fmax = max ( fmax, f(j) )
    end do
!
!  Increment the function at this position, and set later entries to 1.
!
    if ( f(i) /= fmax + 1 ) then
      f(i) = f(i) + 1
      f(i+1:m) = 1
      rank = rank + 1
      return
    end if

  end do
!
!  The final element was input.
!  Return the first element.
!
  f(1:m) = 1
  rank = 0

  return
end
subroutine rgf_to_setpart ( m, f, nsub, s, index )
!
!*******************************************************************************
!
!! RGF_TO_SETPART converts a restricted growth function to a set partition.
!
!
!  Reference:
!
!    Algorithm 3.12,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 83.
!
!  Modified:
!
!    19 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the domain of the RGF is the integers from 1 to M.
!    M must be positive.
!
!    Input, integer F(M), the restricted growth function.
!
!    Output, integer NSUB, the number of nonempty subsets into
!    which the set is partitioned.  
!
!    Output, integer INDEX(M), lists the location in S of the last
!    element of each subset.  Thus, the elements of subset 1
!    are S(1) through S(INDEX(1)), the elements of subset 2
!    are S(INDEX(1)+1) through S(INDEX(2)) and so on.
!
!    Output, integer S(M), describes the partition of a set of
!    M objects into NSUB nonempty subsets.  If element I of the
!    superset belongs to subset J, then S(I) = J.
!
  integer m
  integer nsub
!
  integer f(m)
  integer i
  integer ierror
  integer index(m)
  integer j
  integer k
  integer s(m)
!
!  Check.
!
  call rgf_check ( m, f, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'RGF_TO_SETPART - Fatal error!'
    write ( *, * ) '  The input array is illegal!'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Determine the number of subsets.
!
  nsub = f(1)
  do j = 2, m
    nsub = max ( nsub, f(j) )
  end do
! 
!  Initialize.
!
  s(1:m) = 0
  index(1:nsub) = 0
!
!  For each subset I, collect the indices of F which have value I.
!  These are the elements of the I-th subset.
!
  k = 0
  do i = 1, nsub
    do j = 1, m
      if ( f(j) == i ) then
        k = k + 1
        s(k) = j
      end if
    end do
    index(i) = k
  end do

  return
end
subroutine rgf_unrank ( rank, m, f )
!
!*******************************************************************************
!
!! RGF_UNRANK returns the restricted growth function of a given rank.
!
!
!  Reference:
!
!    Algorithm 3.16,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 87.
!
!  Modified:
!
!    03 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the restricted growth function.
!
!    Input, integer M, the domain of the RGF is the integers from 1 to M.
!    M must be positive.
!
!    Output, integer F(M), the restricted growth function.
!
  integer m
!
  integer d(0:m,0:m)
  integer f(m)
  integer i
  integer j
  integer nrgf
  integer rank
  integer rank_copy
!
!  Check.
!
  if ( m < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'RGF_UNRANK - Fatal error!'
    write ( *, * ) '  Input M is illegal.'
    stop
  end if

  call rgf_enum ( m, nrgf )

  if ( rank < 0 .or. rank > nrgf ) then
    write ( *, * ) ' '
    write ( *, * ) 'RGF_UNRANK - Fatal error!'
    write ( *, * ) '  The input rank is illegal.'
    stop
  end if

  call rgf_g_enum ( m, m, d )

  rank_copy = rank
  j = 1
  f(1) = 1

  do i = 2, m

    if ( j * d(m-i,j) <= rank_copy ) then
      f(i) = j + 1
      rank_copy = rank_copy - j * d(m-i,j)
      j = j + 1
    else
      f(i) = 1 + ( rank_copy / d(m-i,j) )
      rank_copy = mod ( rank_copy, d(m-i,j) )
    end if

  end do

  return
end
subroutine rvec_backtrack ( n, x, indx, k, nstack, stack, maxstack, ncan )
!
!*******************************************************************************
!
!! RVEC_BACKTRACK supervises a backtrack search for a real vector.
!
!
!  Discussion:
!
!    The routine tries to construct a real vector one index at a time,
!    using possible candidates as supplied by the user.
!
!    At any time, the partially constructed vector may be discovered to be
!    unsatisfactory, but the routine records information about where the
!    last arbitrary choice was made, so that the search can be
!    carried out efficiently, rather than starting out all over again.
!
!    First, call the routine with INDX = 0 so it can initialize itself.
!
!    Now, on each return from the routine, if INDX is:
!      1, you've just been handed a complete candidate vector;
!         Admire it, analyze it, do what you like.
!      2, please determine suitable candidates for position X(K).
!         Return the number of candidates in NCAN(K), adding each
!         candidate to the end of STACK, and increasing NSTACK.
!      3, you're done.  Stop calling the routine;
!
!  Reference:
!
!    A Nijenhuis and H Wilf,
!    Combinatorial Algorithms,
!    Academic Press, 1978, second edition,
!    ISBN 0-12-519260-6.
!
!  Modified:
!
!    24 July 2000
!
!  Parameters:
!
!    Input, integer N, the number of positions to be filled in the vector.
!
!    Input/output, real X(N), the partial or complete candidate vector.
!
!    Input/output, integer INDX, a communication flag.
!    On input,
!      0 to start a search.
!    On output:
!      1, a complete output vector has been determined and returned in X(1:N);
!      2, candidates are needed for position X(K);
!      3, no more possible vectors exist.
!
!    Output, integer K, if INDX=2, the current vector index being considered.
!
!    Input/output, integer NSTACK, the current length of the stack.
!
!    Input, real STACK(MAXSTACK), a list of all current candidates for
!    all positions 1 through K.
!
!    Input, integer MAXSTACK, the maximum length of the stack.
!
!    Input/output, integer NCAN(N), lists the current number of candidates for
!    positions 1 through K.
!
  integer n
  integer maxstack
!
  integer indx
  integer k
  integer ncan(n)
  integer nstack
  real stack(maxstack)
  real x(n)
!
!  If this is the first call, request a candidate for position 1.
!
  if ( indx == 0 ) then
    k = 1
    nstack = 0
    indx = 2
    return
  end if
!
!  Examine the stack.
!
  do
!
!  If there are candidates for position K, take the first available
!  one off the stack, and increment K.
!
!  This may cause K to reach the desired value of N, in which case
!  we need to signal the user that a complete set of candidates
!  is being returned.
!
    if ( ncan(k) > 0 ) then

      x(k) = stack(nstack)
      nstack = nstack - 1

      ncan(k) = ncan(k) - 1

      if ( k /= n ) then
        k = k + 1
        indx = 2
      else
        indx = 1
      end if

      exit
!
!  If there are no candidates for position K, then decrement K.
!  If K is still positive, repeat the examination of the stack.
!
    else

      k = k - 1

      if ( k <= 0 ) then
        indx = 3
        exit
      end if

    end if

  end do

  return
end
subroutine setpart_check ( m, nsub, s, index, ierror )
!
!*******************************************************************************
!
!! SETPART_CHECK checks a set partition.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the number of elements of the set.
!    M must be positive.
!
!    Input, integer NSUB, the number of nonempty subsets into
!    which the set is partitioned.  1 <= NSUB <= M.
!
!    Input, integer INDEX(NSUB), lists the location in S of the last
!    element of each subset.  Thus, the elements of subset 1
!    are S(1) through S(INDEX(1)), the elements of subset 2
!    are S(INDEX(1)+1) through S(INDEX(2)) and so on.
!
!    Input, integer S(M), contains the integers from 1 to M,
!    grouped into subsets as described by INDEX.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -I, the I-th element of INDEX is illegal.
!    +I, the I-th element of S is illegal.
!
  integer m
  integer nsub
!
  integer i
  integer ierror
  integer imin
  integer index(nsub)
  integer j
  integer s(m)
!
!  Check INDEX.
!
  imin = 0
  do i = 1, nsub
    if ( index(i) <= imin .or. index(i) > m ) then
      ierror = -i
      return
    end if
    imin = index(i)
  end do
!
!  Check the elements of S.
!
  do i = 1, m

    if ( s(i) <= 0 .or. s(i) > m ) then
      ierror = i
      return
    end if

    do j = 1, i - 1
      if ( s(j) == s(i) ) then
        ierror = i
        return
      end if
    end do

  end do

  return
end
subroutine setpart_enum ( m, npart )
!
!*******************************************************************************
!
!! SETPART_ENUM enumerates the partitions of a set of M elements.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 81.
!
!  Modified:
!
!    03 April 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the number of elements in the set.
!    M must be positive.  However, for the enumeration routine only,
!    it is legal to call with any value of M.
!
!    Output, integer NPART, the number of partitions of the set.
!
  integer m
!
  integer b(0:m)
  integer binomial
  integer i
  integer j
  integer npart
!
  if ( m < 0 ) then

    npart = 0

  else if ( m == 0 ) then

    npart = 1

  else

    b(0) = 1
    do j = 1, m
      b(j) = 0
      do i = 0, j - 1
        b(j) = b(j) + binomial ( j - 1, i ) * b(i)
      end do
    end do

    npart = b(m)

  end if

  return
end
subroutine setpart_to_rgf ( m, nsub, s, index, f )
!
!*******************************************************************************
!
!! SETPART_TO_RGF converts a set partition to a restricted growth function.
!
!
!  Sins:
!
!    The algorithm in the reference is unneccessarily convoluted.
!    Well, I think so.
!
!  Reference:
!
!    Algorithm 3.11,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 83.
!
!  Modified:
!
!    19 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the number of elements of the set.
!    M must be positive.
!
!    Input, integer NSUB, the number of nonempty subsets into
!    which the set is partitioned.  1 <= NSUB <= M.
!
!    Input, integer INDEX(NSUB), lists the location in S of the last
!    element of each subset.  Thus, the elements of subset 1
!    are S(1) through S(INDEX(1)), the elements of subset 2
!    are S(INDEX(1)+1) through S(INDEX(2)) and so on.
!
!    Input, integer S(M), contains the integers from 1 to M,
!    grouped into subsets as described by INDEX.
!
!    Output, integer F(M), the restricted growth function from
!    M to NSUB.
!
  integer m
  integer nsub
!
  integer f(m)
  integer i
  integer ierror
  integer index(nsub)
  integer k
  integer khi
  integer klo
  integer s(m)
!
!  Check.
!
  call setpart_check ( m, nsub, s, index, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SETPART_TO_RGF - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    stop
  end if

  khi = 0
  do i = 1, nsub
    klo = khi + 1
    khi = index(i)
    do k = klo, khi
      f(s(k)) = i
    end do
  end do

  return
end
subroutine stirling_numbers1 ( m, n, s )
!
!*******************************************************************************
!
!! STIRLING_NUMBERS1 computes Stirling numbers of the first kind.
!
!
!  Reference:
!
!    Algorithm 3.17,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 89.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the maximum row to compute.
!    M must be nonnegative.
!
!    Input, integer N, the maximum column to compute.
!    N must be nonnegative.
!
!    Output, integer S(0:M,0:N), the first M+1 rows and N+1 columns
!    of the table of Stirling numbers of the first kind.
!
  integer m
  integer n
!
  integer i
  integer j
  integer s(0:m,0:n)
!
  s(0,0) = 1
  s(1:m,0) = 0
!
!  This loop may be extraneous.
!
  do i = 0, min ( m, n-1 )
    s(i,i+1) = 0
  end do

  do i = 1, m
    do j = 1, n
      if ( j <= i ) then
        s(i,j) = s(i-1,j-1) - ( i - 1 ) * s(i-1,j)
      else
        s(i,j) = 0
      end if
    end do
  end do

  return
end
subroutine stirling_numbers2 ( m, n, s )
!
!*******************************************************************************
!
!! STIRLING_NUMBERS2 computes Stirling numbers of the second kind.
!
!
!  Sins:
!
!    The reference has a typographical error, referring to
!    S(I-J,J-1) instead of S(I-1,J-1).
!
!  Reference:
!
!    Algorithm 3.10,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 81.
!
!  Modified:
!
!    19 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer M, the maximum row to compute.
!    M must be nonnegative.
!
!    Input, integer N, the maximum column to compute.
!    N must be nonnegative.
!
!    Output, integer S(0:M,0:N), the first M+1 rows and N+1 columns
!    of the table of Stirling numbers of the second kind.
!
  integer m
  integer n
!
  integer i
  integer j
  integer s(0:m,0:n)
!
  s(0,0) = 1
  s(1:m,0) = 0
!
!  This loop may be extraneous.
!
  do i = 0, min ( m, n-1 )
    s(i,i+1) = 0
  end do

  do i = 1, m
    do j = 1, n
      if ( j <= i ) then
        s(i,j) = j * s(i-1,j) + s(i-1,j-1)
      else
        s(i,j) = 0
      end if
    end do
  end do

  return
end
subroutine subset_colex_rank ( n, t, rank )
!
!*******************************************************************************
!
!! SUBSET_COLEX_RANK computes the colexicographic rank of a subset.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    11 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of items in the master set.
!    N must be positive.
!
!    Input, integer T(N), the subset.  If T(I) = 0, item I is not in
!    the subset; if T(I) = 1, item I is in the subset.
!
!    Output, integer RANK, the rank of the subset.
!
  integer n
!
  integer i
  integer ierror
  integer rank
  integer t(n)
!
!  Check.
!
  call subset_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_COLEX_RANK - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  rank = 0

  do i = n, 1, -1

    if ( t(i) == 1 ) then
      rank = rank + 2**(n-i)
    end if

  end do

  return
end
subroutine subset_colex_successor ( n, t, rank )
!
!*******************************************************************************
!
!! SUBSET_COLEX_SUCCESSOR computes the subset colexicographic successor.
!
!
!  Discussion:
!
!    In the original code, there is a last element with no successor.
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 46.
!
!  Modified:
!
!    22 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input/output, integer T(N), describes a subset.  T(I) is 0 if
!    the I-th element of the master set is not in the subset, and is
!    1 if the I-th element is part of the subset.
!
!    On input, T describes a subset.
!
!    On output, T describes the next subset in the ordering.
!    If the input T was the last in the ordering, then the output T
!    will be the first.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer n
!
  integer i
  integer ierror
  integer rank
  integer t(n)
!
!  Return the first element.
!
  if ( rank == -1 ) then
    t(1:n) = 0
    rank = 0
    return
  end if
!
!  Check.
!
  call subset_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_COLEX_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  do i = 1, n

    if ( t(i) == 0 ) then
      t(i) = 1
      rank = rank + 1
      return
    else
      t(i) = 0
    end if

  end do

  rank = 0

  return
end
subroutine subset_colex_unrank ( rank, n, t )
!
!*******************************************************************************
!
!! SUBSET_COLEX_UNRANK computes the subset of given colexicographic rank.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    11 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the subset.
!
!    Input, integer N, the number of items in the master set.
!    N must be positive.
!
!    Output, integer T(N), the subsetof the given rank.  If T(I) = 0, 
!    item I is not in the subset; if T(I) = 1, item I is in the subset.
!
  integer n
!
  integer i
  integer nsub
  integer rank
  integer rank_copy
  integer t(n)
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_COLEX_UNRANK - Fatal error!'
    write ( *, * ) '  Input N is illegal.'
    stop
  end if

  call subset_enum ( n, nsub )

  if ( rank < 0 .or. rank > nsub ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_COLEX_UNRANK - Fatal error!'
    write ( *, * ) '  Input rank is illegal.'
    stop
  end if

  rank_copy = rank

  do i = 1, n
    if ( mod ( rank_copy, 2 ) == 1 ) then
      t(i) = 1
    else
      t(i) = 0
    end if

    rank_copy = rank_copy / 2

  end do

  return
end
subroutine subset_check ( n, t, ierror )
!
!*******************************************************************************
!
!! SUBSET_CHECK checks a subset.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    20 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input, integer T(N), the subset.  If T(I) = 0, item I is not in
!    the subset; if T(I) = 1, item I is in the subset.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N is illegal.
!    I, entry I is illegal.
!
  integer n
!
  integer i
  integer ierror
  integer t(n)
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if

  do i = 1, n

    if ( t(i) /= 0 .and. t(i) /= 1 ) then
      ierror = i
      return
    end if

  end do

  return
end
subroutine subset_complement ( n, a, b )
!
!*******************************************************************************
!
!! SUBSET_COMPLEMENT computes the complement of a set.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the master set, of which A is
!    a subset.  N must be positive.
!
!    Input, integer A(N), a subset of the master set.
!    A(I) = 0 if the I-th element is in the subset A, and is
!    1 otherwise.
!
!    Output, integer B(N), the complement of A.
!
  integer n
!
  integer a(n)
  integer b(n)
  integer i
  integer ierror
!
!  Check.
!
  call subset_check ( n, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_COMPLEMENT - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  b(1:n) = 1 - a(1:n)

  return
end
subroutine subset_distance ( n, t1, t2, dist )
!
!*******************************************************************************
!
!! SUBSET_DISTANCE computes the Hamming distance between two sets.
!
!
!  Discussion:
!
!    The sets T1 and T2 are assumed to be subsets of a set of N elements.
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 35.
!
!  Modified:
!
!    12 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the master set, of which T1 and
!    T2 are subsets.  N must be positive.
!
!    Input, integer T1(N), T2(N), two subsets of the master set.
!    T1(I) = 0 if the I-th element is in the subset T1, and is
!    1 otherwise; T2 is defined similarly.
!
!    Output, integer DIST, the Hamming distance between T1 and T2,
!    defined as the number of elements of the master set which are
!    in either T1 or T2 but not both.
!
  integer n
!
  integer dist
  integer i
  integer ierror
  integer t1(n)
  integer t2(n)
!
!  Check.
!
  call subset_check ( n, t1, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_DISTANCE - Fatal error!'
    write ( *, * ) '  The first input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  call subset_check ( n, t2, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_DISTANCE - Fatal error!'
    write ( *, * ) '  The second input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  dist = 0

  do i = 1, n

    if ( ( t1(i) == 0 .and. t2(i) /= 0 ) .or. &
         ( t1(i) /= 0 .and. t2(i) == 0 ) ) then
      dist = dist + 1
    end if

  end do

  return
end
subroutine subset_enum ( n, nsub )
!
!*******************************************************************************
!
!! SUBSET_ENUM enumerates the subsets of a set with N elements.
!
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of elements in the set.
!    N must be at least 0.
!
!    Output, integer NSUB, the number of distinct elements.
!
  integer n
  integer nsub
!
  nsub = 2**n

  return
end
subroutine subset_intersect ( n, a, b, c )
!
!*******************************************************************************
!
!! SUBSET_INTERSECT computes the intersection of two sets.
!
!
!  Reference:
!
!    Algorithm 1.7,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 21.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the master set, of which A and
!    B are subsets.  N must be positive.
!
!    Input, integer A(N), B(N), two subsets of the master set.
!    A(I) = 0 if the I-th element is in the subset A, and is
!    1 otherwise; B is defined similarly.
!
!    Output, integer C(N), the intersection of A and B.
!
  integer n
!
  integer a(n)
  integer b(n)
  integer c(n)
  integer i
  integer ierror
!
!  Check.
!
  call subset_check ( n, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_INTERSECT - Fatal error!'
    write ( *, * ) '  The first input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  call subset_check ( n, b, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_INTERSECT - Fatal error!'
    write ( *, * ) '  The second input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  c(1:n) = a(1:n) * b(1:n)

  return
end
subroutine subset_lex_rank ( n, t, rank )
!
!*******************************************************************************
!
!! SUBSET_LEX_RANK computes the lexicographic rank of a subset.
!
!
!  Reference:
!
!    Algorithm 2.1,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 34.
!
!  Modified:
!
!    11 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of items in the master set.
!    N must be positive.
!
!    Input, integer T(N), the subset.  If T(I) = 0, item I is not in
!    the subset; if T(I) = 1, item I is in the subset.
!
!    Output, integer RANK, the rank of the subset.
!
  integer n
!
  integer i
  integer ierror
  integer rank
  integer t(n)
!
!  Check.
!
  call subset_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_LEX_RANK - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  rank = 0

  do i = 1, n

    if ( t(i) == 1 ) then
      rank = rank + 2**( n - i )
    end if

  end do

  return
end
subroutine subset_lex_successor ( n, t, rank )
!
!*******************************************************************************
!
!! SUBSET_LEX_SUCCESSOR computes the subset lexicographic successor.
!
!
!  Discussion:
!
!    In the original code, there is a last element with no successor.
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 43.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of elements in the master set.
!    N must be positive.
!
!    Input/output, integer T(N), describes a subset.  T(I) is 0 if
!    the I-th element of the master set is not in the subset, and is
!    1 if the I-th element is part of the subset.
!
!    On input, T describes a subset.
!
!    On output, T describes the next subset in the ordering.
!    If the input T was the last in the ordering, then the output T
!    will be the first.
!
!    Input/output, integer RANK, the rank.
!
!    If RANK = -1 on input, then the routine understands that this is
!    the first call, and that the user wishes the routine to supply
!    the first element in the ordering, which has RANK = 0.  
!
!    In general, the input value of RANK is increased by 1 for output,
!    unless the very last element of the ordering was input, in which
!    case the output value of RANK is 0.
!
  integer n
!
  integer i
  integer ierror
  integer rank
  integer t(n)
!
!  Return the first element.
!
  if ( rank == -1 ) then
    t(1:n) = 0
    rank = 0
    return
  end if
!
!  Check.
!
  call subset_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_LEX_SUCCESSOR - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  do i = n, 1, -1

    if ( t(i) == 0 ) then
      t(i) = 1
      rank = rank + 1
      return
    else
      t(i) = 0
    end if

  end do

  rank = 0

  return
end
subroutine subset_lex_unrank ( rank, n, t )
!
!*******************************************************************************
!
!! SUBSET_LEX_UNRANK computes the subset of given lexicographic rank.
!
!
!  Reference:
!
!    Algorithm 2.2,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 34.
!
!  Modified:
!
!    11 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer RANK, the rank of the subset.
!
!    Input, integer N, the number of items in the master set.
!    N must be positive.
!
!    Output, integer T(N), the subset of the given rank.  If T(I) = 0, 
!    item I is not in the subset; if T(I) = 1, item I is in the subset.
!
  integer n
!
  integer i
  integer nsub
  integer rank
  integer rank_copy
  integer t(n)
!
!  Check.
!
  if ( n < 1 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_LEX_UNRANK - Fatal error!'
    write ( *, * ) '  Input N is illegal.'
    stop
  end if

  call subset_enum ( n, nsub )

  if ( rank < 0 .or. rank > nsub ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_LEX_UNRANK - Fatal error!'
    write ( *, * ) '  Input rank is illegal.'
    stop
  end if

  rank_copy = rank

  do i = n, 1, -1

    if ( mod ( rank_copy, 2 ) == 1 ) then
      t(i) = 1
    else
      t(i) = 0
    end if

    rank_copy = rank_copy / 2

  end do

  return
end
subroutine subset_union ( n, a, b, c )
!
!*******************************************************************************
!
!! SUBSET_UNION computes the union of two sets.
!
!
!  Reference:
!
!    Algorithm 1.6,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 20.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the master set, of which A and
!    B are subsets.  N must be positive.
!
!    Input, integer A(N), B(N), two subsets of the master set.
!    A(I) = 0 if the I-th element is in the subset A, and is
!    1 otherwise; B is defined similarly.
!
!    Output, integer C(N), the union of A and B.
!
  integer n
!
  integer a(n)
  integer b(n)
  integer c(n)
  integer i
  integer ierror
!
!  Check.
!
  call subset_check ( n, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_UNION - Fatal error!'
    write ( *, * ) '  The first input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  call subset_check ( n, b, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_UNION - Fatal error!'
    write ( *, * ) '  The second input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  do i = 1, n
    c(i) = max ( a(i), b(i) )
  end do

  return
end
subroutine subset_weight ( n, t, weight )
!
!*******************************************************************************
!
!! SUBSET_WEIGHT computes the Hamming weight of a set.
!
!
!  Discussion:
!
!    The Hamming weight is simply the number of elements in the set.
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 38.
!
!  Modified:
!
!    12 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the master set, of which T
!    is a subset.  N must be positive.
!
!    Input, integer T(N), defines the subset T.
!    T(I) is 1 if I is an element of T, and 0 otherwise.
!
!    Output, integer WEIGHT, the Hamming weight of the subset T.
!
  integer n
!
  integer i
  integer ierror
  integer t(n)
  integer weight
!
!  Check.
!
  call subset_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_WEIGHT - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  weight = sum ( t(1:n) )

  return
end
subroutine subset_xor ( n, a, b, c )
!
!*******************************************************************************
!
!! SUBSET_XOR computes the symmetric difference of two sets.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the order of the master set, of which A and
!    B are subsets.  N must be positive.
!
!    Input, integer A(N), B(N), two subsets of the master set.
!    A(I) = 0 if the I-th element is in the subset A, and is
!    1 otherwise; B is defined similarly.
!
!    Output, integer C(N), the symmetric difference of A and B.
!
  integer n
!
  integer a(n)
  integer b(n)
  integer c(n)
  integer i
  integer ierror
!
!  Check.
!
  call subset_check ( n, a, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_XOR - Fatal error!'
    write ( *, * ) '  The first input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  call subset_check ( n, b, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'SUBSET_XOR - Fatal error!'
    write ( *, * ) '  The second input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  do i = 1, n
    c(i) = max ( a(i), b(i) ) - min ( a(i), b(i) )
  end do

  return
end
subroutine subsetsum_swap ( n, a, sum_opt, index, sum_got )
!
!*******************************************************************************
!
!! SUBSETSUM_SWAP seeks a solution of the subset sum problem by swapping.
!
!
!  Discussion:
!
!    Given a collection of N not necessarily distinct positive integers A(I), 
!    and a positive integer SUM_OPT, select a subset of the values so that
!    their sum is as close as possible to SUM_OPT without exceeding it.
!
!  Algorithm:
!
!    Start with no values selected, and SUM_GOT = 0.
!
!    Consider each element A(I):
!
!      If A(I) is not selected and SUM_GOT + A(I) <= SUM_OPT, select A(I).
!
!      If A(I) is still not selected, and there is a selected A(J)
!      such that SUM_GOT < SUM_GOT + A(I) - A(J), select A(I) and deselect A(J).
!
!      If no items were selected on this sweep, exit.  Otherwise repeat
!      the search.
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 301.
!
!  Modified:
!
!    13 March 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of values.  N must be positive.
!
!    Input/output, integer A(N), a collection of positive values.
!    On output, A has been sorted into descending order.
!
!    Input, integer SUM_OPT, the desired sum.
!
!    Output, integer INDEX(N); INDEX(I) is 1 if A(I) is part of the
!    sum, and 0 otherwise.
!
!    Output, integer SUM_GOT, the sum of the selected elements.
!
  integer n
!
  integer a(n)
  integer i
  integer index(n)
  integer j
  integer nmove
  integer sum_got
  integer sum_opt
!
!  Initialize.
!
  sum_got = 0
  index(1:n) = 0
!
!  Sort into descending order.
!
  call ivec_sort_insert_d ( n, a )

  do

    nmove = 0

    do i = 1, n

      if ( index(i) == 0 ) then

        if ( sum_got + a(i) <= sum_opt ) then
          index(i) = 1
          sum_got = sum_got + a(i)
          nmove = nmove + 1
          cycle
        end if

      end if

      if ( index(i) == 0 ) then

        do j = 1, n
 
          if ( index(j) == 1 ) then

            if ( sum_got + a(i) - a(j) > sum_got .and. &
              sum_got + a(i) - a(j) <= sum_opt ) then
              index(j) = 0
              index(i) = 1
              nmove = nmove + 2
              sum_got = sum_got + a(i) - a(j)
              exit
            end if

          end if

        end do

      end if

    end do

    if ( nmove <= 0 ) then
      exit
    end if

  end do

  return
end
subroutine tableau_check ( n, tab, ierror )
!
!*******************************************************************************
!
!! TABLEAU_CHECK checks a 2 by N tableau.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of columns in the tableau.
!    N must be positive.
!
!    Input, integer TAB(2,N), a 2 by N tableau.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N was illegal.
!    (I-1)*N+J, the (I,J) entry of TAB is illegal.
!
  integer n
!
  integer i
  integer ierror
  integer j
  integer tab(2,n)
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if
!
!  The entries must be between 0 and 2*N.
!
  do i = 1, 2
    do j = 1, n
      if ( tab(i,j) < 1 .or. tab(i,j) > 2 * n ) then
        ierror = ( i - 1 ) * n + j
        return
      end if
    end do 
  end do
!
!  The entries must be increasing to the right.
!
  do i = 1, 2
    do j = 2, n
      if ( tab(i,j) <= tab(i,j-1) ) then
        ierror = ( i - 1 ) * n + j
        return
      end if
    end do
  end do
!
!  The entries must be increasing down.
!
  i = 2
  do j = 1, n
    if ( tab(i,j) <= tab(i-1,j) ) then
      ierror = ( i - 1 ) * n + j
      return
    end if
  end do

  return
end
subroutine tableau_enum ( n, ntab )
!
!*******************************************************************************
!
!! TABLEAU_ENUM enumerates the 2 by N standard tableaus.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 95.
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of columns in the tableau.
!    N must be nonnegative.
!
!    Output, integer NTAB, the number of 2 by N standard tableaus.
!
  integer binomial
  integer n
  integer ntab
!
  ntab = binomial ( 2*n, n ) / ( n + 1 )

  return
end
subroutine tableau_to_bal_seq ( n, tab, t )
!
!*******************************************************************************
!
!! TABLEAU_TO_BAL_SEQ converts a 2 by N tableau to a balanced sequence.
!
!
!  Reference:
!
!    Algorithm 3.25,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 102.
!
!  Modified:
!
!    21 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of 0's (and 1's) in the sequence.
!    N must be positive.
!
!    Input, integer TAB(2,N), a 2 by N tableau.
!
!    Output, integer T(2*N), a balanced sequence.
!
  integer n
!
  integer i
  integer ierror
  integer j
  integer t(2*n)
  integer tab(2,n)
!
!  Check.
!
  call tableau_check ( n, tab, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'TABLEAU_TO_BAL_SEQ - Fatal error!'
    write ( *, * ) '  The input array is illegal.'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if

  do i = 1, 2
    do j = 1, n
      t(tab(i,j)) = i - 1
    end do
  end do

  return
end
subroutine tree_check ( n, t, ierror )
!
!*******************************************************************************
!
!! TREE_CHECK checks a tree.
!
!
!  Reference:
!
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of nodes in the tree.
!    N must be positive.
!
!    Input, integer T(2,N-1), describes the edges of the tree
!    as pairs of nodes.
!
!    Output, integer IERROR, error flag.
!    0, no error.
!    -1, N was illegal.
!    J, the edge T(1,J) to T(2,J) is illegal.
!
  integer n
!
  integer d(n)
  integer i
  integer ierror
  integer j
  integer k
  integer t(2,n-1)
  integer x
  integer y
!
  ierror = 0

  if ( n < 1 ) then
    ierror = -1
    return
  end if

  do i = 1, 2
    do j = 1, n - 1
      if ( t(i,j) < 1 .or. t(i,j) > n ) then
        ierror = j
        return
      end if
    end do
  end do
!
!  Compute the degree of each node.
!
  call edge_degree ( n, n-1, t, d )
!
!  Delete a node of degree 1, N-1 times.
!
  do k = 1, n - 1

    x = 1

    do while ( d(x) /= 1 ) 
      x = x + 1
      if ( x > n ) then
        ierror = -1
        return
      end if
    end do
!
!  Find its neighbor.
!
    j = 1

    do

      if ( t(1,j) == x ) then
        y = t(2,j)
        exit
      end if

      if ( t(2,j) == x ) then
        y = t(1,j)
        exit
      end if

      j = j + 1

      if ( j > n ) then
        ierror = -1
        return
      end if

    end do
!
!  Delete the edge.
!
    t(1,j) = - t(1,j)
    t(2,j) = - t(2,j)

    d(x) = d(x) - 1
    d(y) = d(y) - 1

  end do

  t(1:2,1:n-1) = - t(1:2,1:n-1)

  return
end
subroutine tree_enum ( n, ntree )
!
!*******************************************************************************
!
!! TREE_ENUM enumerates the trees on N nodes.
!
!
!  Modified:
!
!    23 January 1999
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of nodes in each tree.
!    N must normally be at least 3, but for this routine,
!    any value of N is allowed.
!
!    Output, integer NTREE, the number of distinct elements.
!
  integer n
  integer ntree
!
  if ( n < 1 ) then
    ntree = 0
  else if ( n == 1 ) then
    ntree = 1
  else if ( n == 2 ) then
    ntree = 1
  else
    ntree = n**( n - 2 )
  end if

  return
end
subroutine tree_to_pruefer ( n, t, p )
!
!*******************************************************************************
!
!! TREE_TO_PRUEFER converts a tree to a Pruefer code.
!
!
!  Reference:
!
!    Algorithm 3.18,
!    Donald Kreher and Douglas Simpson,
!    Combinatorial Algorithms,
!    CRC Press, 1998, page 91.
!
!  Modified:
!
!    28 February 2001
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of nodes in the tree.
!    N must be positive.
!
!    Input, integer T(2,N-1), describes the edges of the tree
!    as pairs of nodes.
!
!    Output, integer P(N-2), the Pruefer code for the tree.
!
  integer n
!
  integer d(n)
  integer ierror
  integer j
  integer k
  integer p(n-2)
  integer t(2,n-1)
  integer x
  integer y
!
!  Check.
!
  call tree_check ( n, t, ierror )

  if ( ierror /= 0 ) then
    write ( *, * ) ' '
    write ( *, * ) 'TREE_TO_PRUEFER - Fatal error!'
    write ( *, * ) '  The input array is illegal!'
    write ( *, * ) '  IERROR = ', ierror
    stop
  end if
!
!  Compute the degree of each node.
!
  call edge_degree ( n, n-1, t, d )
 
  do j = 1, n - 2
!
!  Find a node of degree 1.
!
    x = n
    do while ( d(x) /= 1 )
      x = x - 1
    end do
!
!  Find its neighbor.
!
    k = 1

    do

      if ( t(1,k) == x ) then
        y = t(2,k)
        exit
      end if

      if ( t(2,k) == x ) then
        y = t(1,k)
        exit
      end if

      k = k + 1

    end do
!
!  Store the neighbor.
!
    p(j) = y
!
!  Delete the edge from the tree.
!
    d(x) = d(x) - 1
    d(y) = d(y) - 1

    t(1,k) = - t(1,k)
    t(2,k) = - t(2,k)

  end do
!
!  Restore the original tree.
!
  t(1:2,1:n-1) = - t(1:2,1:n-1)

  return
end
