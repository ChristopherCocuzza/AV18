C **********************************************************************
	SUBROUTINE WJC1 (q,u,w,vt,vs)
C
C  Deuteron wavefunction from WJC-1 (Gross et al.) NN potential model.
C  Gross & Stadler, Phys. Rev. C78, 014005 (2008).
C
C  Note: q in 1/fm, wfns in fm^3/2.
C
C  Wave function normalization
C    \int dq q^2 (u^2+w^2+vt^2+vs^2) + V' term = 1
C    so that wave functions themselves normalized to ~105%
C    => renormalize to 1 for structure functions
C
C **********************************************************************
        IMPLICIT NONE
        INTEGER id,nq
        PARAMETER (nq=60)
        REAL*8  q,u,w,vt,vs,DQDVAL,
     &		qgrid(nq),ugrid(nq),wgrid(nq),vtgrid(nq),vsgrid(nq),dum
        LOGICAL init /.FALSE./
        REAL*8  pi,hcM,hcG
	SAVE

        pi = 4*DATAN(1.D0)
        hcM = 197.327D0		! GeV.fm conversion factor
        hcG = 0.197327D0	! MeV.fm conversion factor

        IF (init) GO TO 999     ! Data already read
C...Read data from file
	OPEN (10, FORM='FORMATTED',
     &	      FILE='/u/home/wmelnitc/Work/EMC/D/Wfn/wjc-1.dat',
     &	      STATUS='OLD')

C...Momentum space [qgrid in MeV, ugrid in GeV^-3/2]
        DO id=1,nq
          READ (10,*) qgrid(id),
     &		      ugrid(id), wgrid(id), vtgrid(id), vsgrid(id)
          qgrid(id) = qgrid(id) / hcM		! MeV => 1/fm
          ugrid(id)  = ugrid(id)  * hcG**1.5D0	! GeV^-3/2 => fm^3/2
          wgrid(id)  = wgrid(id)  * hcG**1.5D0
          vtgrid(id) = vtgrid(id) * hcG**1.5D0
          vsgrid(id) = vsgrid(id) * hcG**1.5D0
        ENDDO
	PRINT *, '... WJC-1 model read...'
	init = .TRUE.

C...Evaluate wavefunction
c 999	u  = DQDVAL (q,nq,qgrid,ugrid,.FALSE.)
c	w  = DQDVAL (q,nq,qgrid,wgrid,.FALSE.)
c	vt = DQDVAL (q,nq,qgrid,vtgrid,.FALSE.)
c	vs = DQDVAL (q,nq,qgrid,vsgrid,.FALSE.)
 999	CALL Pinterp (qgrid,ugrid,nq,q,u,dum,1)
	CALL Pinterp (qgrid,wgrid,nq,q,w,dum,1)
	CALL Pinterp (qgrid,vtgrid,nq,q,vt,dum,1)
	CALL Pinterp (qgrid,vsgrid,nq,q,vs,dum,1)

        RETURN
        END


C **********************************************************************
	SUBROUTINE WJC2 (q,u,w,vt,vs)
C
C  Deuteron wavefunction from WJC-2 (Gross et al.) NN potential model.
C  Gross & Stadler, Phys. Rev. C78, 014005 (2008).
C
C  Note: q in 1/fm, wfns in fm^3/2.
C
C  Wave function normalization
C    \int dq q^2 (u^2+w^2+vt^2+vs^2) + V' term = 1
C    so that wave functions themselves normalized to ~102%
C    => renormalize to 1 for structure functions
C
C **********************************************************************
        IMPLICIT NONE
        INTEGER id,nq
        PARAMETER (nq=60)
        REAL*8  q,u,w,vt,vs,
     &		qgrid(nq),ugrid(nq),wgrid(nq),vtgrid(nq),vsgrid(nq),dum
        LOGICAL init /.FALSE./
        REAL*8  pi,hcM,hcG
	SAVE

        pi = 4*DATAN(1.D0)
        hcM = 197.327D0		! GeV.fm conversion factor
        hcG = 0.197327D0	! MeV.fm conversion factor

        IF (init) GO TO 999     ! Data already read
C...Read data from file
	OPEN (10, FORM='FORMATTED',
     &	      FILE='/u/home/wmelnitc/Work/EMC/D/Wfn/wjc-2.dat',
     &	      STATUS='OLD')

C...Momentum space [qgrid in MeV, ugrid in GeV^-3/2]
        DO id=1,nq
          READ (10,*) qgrid(id),
     &		      ugrid(id), wgrid(id), vtgrid(id), vsgrid(id)
          qgrid(id) = qgrid(id) / hcM		! MeV => 1/fm
          ugrid(id)  = ugrid(id)  * hcG**1.5D0	! GeV^-3/2 => fm^3/2
          wgrid(id)  = wgrid(id)  * hcG**1.5D0
          vtgrid(id) = vtgrid(id) * hcG**1.5D0
          vsgrid(id) = vsgrid(id) * hcG**1.5D0
        ENDDO
	PRINT *, '... WJC-2 model read...'
	init = .TRUE.

C...Evaluate wavefunction
c 999	u  = DQDVAL (q,nq,qgrid,ugrid,.FALSE.)
c	w  = DQDVAL (q,nq,qgrid,wgrid,.FALSE.)
c	vt = DQDVAL (q,nq,qgrid,vtgrid,.FALSE.)
c	vs = DQDVAL (q,nq,qgrid,vsgrid,.FALSE.)
 999	CALL Pinterp (qgrid,ugrid,nq,q,u,dum,1)
	CALL Pinterp (qgrid,wgrid,nq,q,w,dum,1)
	CALL Pinterp (qgrid,vtgrid,nq,q,vt,dum,1)
	CALL Pinterp (qgrid,vsgrid,nq,q,vs,dum,1)

        RETURN
        END
