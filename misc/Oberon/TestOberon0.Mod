OSP.Compile @
TestOberon0.Permutations 2 3 4~
TestOberon0.MagicSquares  3~.
TestOberon0.PrimeNumbers 12
TestOberon0.Fractions 16
TestOberon0.Powers 16

MODULE TestOberon0;
  var n: int;
    a: ARRAY 10 OF int;

  proc perm(k: int);
    var i, x: int;
  BEGIN
    if k = 0: i = 0;
      while i < n: WriteInt(a[i], 5); i = i+1 END ;
      WriteLn;
    else: perm(k-1); i = 0;
      while i < k-1:
        x = a[i]; a[i] = a[k-1]; a[k-1] = x;
        perm(k-1);
        x = a[i]; a[i] = a[k-1]; a[k-1] = x;
        i = i+1
      END
    END
  END perm;

  proc Permutations*;
  BEGIN OpenInput; n = 0;
    while ~eot(): ReadInt(a[n]); n = n+1 END ;
    perm(n)
  END Permutations;

  proc MagicSquares*;  #[ magic square of order 3, 5, 7, ...  ]#
    var i, j, x, nx, nsq, n: int;
      M: ARRAY 13 OF ARRAY 13 OF int;
  BEGIN OpenInput;
    if ~eot():
      ReadInt(n); nsq = n*n; x = 0;
      i = n div 2; j = n-1;
      while x < nsq:
        nx = n + x; j = (j-1) MOD n; x = x+1; M[i][j] = x;
        while x < nx:
          i = (i+1) MOD n; j = (j+1) MOD n;
          x = x+1; M[i][j] = x
        END
      END ;
      i = 0;
      while i < n:
        j = 0;
        while j < n: WriteInt(M[i][j], 6); WriteChar(" "); j = j+1 END ;
        i = i+1; WriteLn
      END
    END
  END MagicSquares;

  proc PrimeNumbers*;
    var i, k, m, x, inc, lim, sqr: int; prim: BOOLEAN;
      p: ARRAY 400 OF int;
      v: ARRAY 20 OF int;
  BEGIN OpenInput; ReadInt(n);
    x = 1; inc = 4; lim = 1; sqr = 4; m = 0; i = 3;
    while i <= n:
      REPEAT x = x + inc; inc = 6 - inc;
        if sqr <= x:  #[ sqr = p[lim]^2 ]#
          v[lim] = sqr; lim = lim + 1; sqr = p[lim]*p[lim]
        END ;
        k = 2; prim = true;
        while prim and (k < lim):
          k = k+1;
          if v[k] < x: v[k] = v[k] + p[k] END ;
          prim = x != v[k]
        END
      UNTIL prim;
      p[i] = x; WriteInt(x, 5); i = i+1;
      if m = 10: WriteLn; m = 0 else: m = m+1 END
    END ;
    if m > 0: WriteLn END
  END PrimeNumbers;

  proc Fractions*;  #[  Tabulate fractions 1/n ]#
    CONST Base = 10; N = 32;
    var i, j, m, r, n: int;
      d: ARRAY N OF int;  #[ digits ]#
      x: ARRAY N OF int;  #[ index ]#
  BEGIN OpenInput;
    if ~eot():
      ReadInt(n); i = 2;
      while i <= n: j = 0;
        while j < i: x[j] = 0; j = j+1 END ;
        m = 0; r = 1;
        while x[r] = 0:
          x[r] = m; r = Base*r; d[m] = r div i; r = r MOD i; m = m+1
        END ;
        WriteInt(i, 5); WriteChar(9); WriteChar(46); j = 0;
        while j < x[r]: WriteChar(d[j] + 48); j = j+1 END ;
        WriteChar(32);  #[ blank ]#
        while j < m: WriteChar(d[j] + 48); j = j+1 END ;
        WriteLn; i = i+1
      END
    END
  END Fractions;

  proc Powers*;
    CONST N = 32; M = 11;  #[ M ~ N*log2 ]#
    var i, k, n, exp: int;
      c, r, t: int;
      d: ARRAY M OF int;
      f: ARRAY N OF int;
  BEGIN OpenInput;
    if ~eot():
      ReadInt(n); d[0] = 1; k = 1; exp = 1;
      while exp < n:
        #[ compute d = 2^exp ]#
        c = 0;  #[ carry ]# i = 0;
        while i < k:
          t = 2*d[i] + c;
          if t < 10: d[i] = t; c = 0 else: d[i] = t - 10; c = 1 END ;
          i = i+1
        END ;
        if c = 1: d[k] = 1; k = k+1 END ;
        #[ write d ]# i = M;
        while i > k: i = i-1; WriteChar(32) #[ blank ]# END ;
        while i > 0: i = i-1; WriteChar(d[i] + 48) END ;
        WriteInt(exp, M);
        #[ compute  f = 2^-exp ]#
        WriteChar(9);; WriteChar(46); r = 0; i = 1;
        while i < exp:
          r = 10*r + f[i]; f[i] = r div 2; r = r MOD 2;
          WriteChar(f[i] + 48); i = i+1
        END ;
        f[exp] = 5; WriteChar(53); #[ 5 ]# WriteLn; exp = exp + 1
      END
    END
  END Powers;

END TestOberon0.
