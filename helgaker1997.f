      real*8 function helgaker1997_7(corr1,corr2,LL)
        implicit none
        ! extrapolation of electronic correlation energy: following 
        ! T. Helgaker, W. Klopper, H. Koch, and J. Noga, 
        ! The Journal of Chemical Physics 106, 9639 (1997).
        ! Equation (7)
        ! input : corr1: the correlation energy of small basis set
        ! input : corr2: the correlation energy of large basis set
        ! input : LL: the `cardinal number' of large basis set
        real*8 :: corr1, corr2
        integer :: LL
        real*8 :: L
        L=LL
        helgaker1997_7=(corr2*L**3-corr1*(L-1)**3)
     &       /(      L**3-      (L-1)**3)
        return
      end function helgaker1997_7
